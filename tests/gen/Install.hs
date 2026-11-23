-- A step's files put in place for the references (MonRef, NetRef): each
-- file of the step written over the file of the same name in CUR (the same
-- inode, so what bwrap bound there sees it), through a bare descriptor:
-- GHC's own handles would lock the inode the monitor reads.
module Install (install) where

import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as B
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import System.Posix.IO (OpenFileFlags (..), OpenMode (WriteOnly), closeFd, defaultFileFlags, fdWrite, openFd)

install :: FilePath -> FilePath -> IO ()
install cur step = do
  fs <- listDirectory step
  forM_ fs $ \f -> do
    dir <- doesDirectoryExist (step </> f)
    if dir
      then install (cur </> f) (step </> f)
      else do
        s <- B.readFile (step </> f)
        fd <- openFd (cur </> f) WriteOnly defaultFileFlags {trunc = True}
        _ <- fdWrite fd (B.unpack s)
        closeFd fd
