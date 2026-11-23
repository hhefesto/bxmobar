-- The monitors' reference: one of xmobar 0.50's monitors, started as
-- xmobar starts it, reading files that bwrap has put in their places
-- (tests/gen/GenMon.hs runs it so). Before each of the monitor's outputs
-- the files are set to the next step's, so a monitor that compares two
-- readings (MultiCpu, DynNetwork) sees them change between its runs.
--
--   monref SPEC CUR STEP_0 STEP_1 ...
--
-- SPEC is a Monitors value as a config writes it (MultiCpu ["-t", ".."] 2);
-- CUR is the directory whose files bwrap bound (proc_stat to /proc/stat,
-- ...); each STEP_i a directory with the same file names. It prints the
-- outputs, one show'n String a line.
module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (BlockedIndefinitelyOnMVar (..), catch)
import Control.Monad (foldM_, forM_, when)
import System.Environment (lookupEnv)
import Xmobar.System.StatFS (FileSystemStats (..), getFileSystemStats)
import System.Environment (getArgs)
import Install (install)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import System.Posix.IO (OpenFileFlags (..), OpenMode (WriteOnly), closeFd, defaultFileFlags, fdWrite, openFd)
import Xmobar.Plugins.Monitors (Monitors)
import Xmobar.Run.Exec (start)
import Xmobar.App.Config (defaultConfig)
import Xmobar.Text.Output (format)

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, _ : b) -> a : splitOn c b
  (a, []) -> [a]

main :: IO ()
main = do
  spec : cur : steps <- getArgs
  let mon = read spec :: Monitors
  -- FILL=path:bytes,...: files of that many bytes written first (in a
  -- tmpfs bwrap made), and each path's statfs printed first, as DiskU
  -- reads it: total, available and used bytes
  -- PLAIN=PREFIX: each output after PREFIX, as xmobar -T writes the line
  -- (Text.Output.format at defaultConfig, Plain), for tests/gen/GenE2E.hs
  plain <- lookupEnv "PLAIN"
  let shown o = maybe o (\p -> format defaultConfig (p ++ o)) plain
  fill <- maybe [] (splitOn ',') <$> lookupEnv "FILL"
  forM_ fill $ \pf -> do
    let (p, n) = break (== ':') pf
    writeFile (p </> "filler") (replicate (read (drop 1 n)) 'x')
  forM_ fill $ \pf -> do
    st <- getFileSystemStats (takeWhile (/= ':') pf)
    print (maybe [0, 0, 0] (\f -> [fsStatByteCount f, fsStatBytesAvailable f, fsStatBytesUsed f]) st)
  install cur (head steps)
  out <- newEmptyMVar
  _ <- forkIO (start mon (putMVar out))
  -- a monitor that failed ends (runM's handle is around its loop): the bar
  -- keeps its last text, so each step after shows that again
  let next prev = takeMVar out `catch` \BlockedIndefinitelyOnMVar -> pure prev
  foldM_ (\prev (i, s) -> do
    when (i > 0) (install cur s)
    o <- next prev
    print (shown o)
    hFlush stdout
    pure o) "" (zip [0 :: Int ..] steps)
