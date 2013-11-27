import System.Process (runCommand, waitForProcess)
import System.Posix.Files (createNamedPipe)
import System.Directory (removeFile, doesFileExist)
import System.Environment (getArgs)
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Log.Logger
import System.Log.Handler.Syslog
import Control.Exception
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void, when)

-- If the paths to watch exist before-hand, nuke them
-- has a race condition
safe_remove :: FilePath -> IO ()
safe_remove path = do
  exists <- doesFileExist path
  when exists $ removeFile path

-- Get a single log line, output to syslog
get_line ltype fd = do
  line <- hGetLine fd
  noticeM ltype line

-- Create a single pipe, listen for log input
mk_pipe :: (String,String) -> IO ()
mk_pipe (ltype,path) = do
  safe_remove path
  createNamedPipe path 0644
  fd <- openFile path ReadMode
  hSetBuffering fd LineBuffering
  void $ forkIO $ forever $ do
    is_eof <- hIsEOF fd
    if is_eof then threadDelay 1000000 else get_line ltype fd

-- Cycle through available pipes to create them
mk_pipes :: [(String,String)] -> IO ()
mk_pipes (pipe:pipes) = do
  mk_pipe pipe
  mk_pipes pipes
mk_pipes [] = return ()

-- Utility function to map "foo:/path" to a tuple ("foo", "/path")
is_colon x = x == ':'
get_logname path = (ltype, p) where (ltype, (_:p)) = break is_colon path

-- Create a syslog handle and parse command line arguments and start nginx
main :: IO ()
main = do
  mainlog <- openlog "nginxpipe" [PID] DAEMON NOTICE
  updateGlobalLogger rootLoggerName (setHandlers [mainlog])
  updateGlobalLogger rootLoggerName (setLevel NOTICE)
  noticeM "nginxpipe" "starting up"
  args <- getArgs
  mk_pipes $ map get_logname args
  noticeM "nginxpipe" "starting nginx"
  ph <- runCommand "nginx"
  exit_code <- waitForProcess ph
  noticeM "nginxpipe" $ "nginx stopped with code: " ++ show exit_code
