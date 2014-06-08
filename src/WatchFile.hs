module WatchFile (watch) where


import System.Directory (getModificationTime)
import System.Cmd (system)
import System.IO (hFlush, stdout)
import Control.Monad (when)
import Control.Concurrent (threadDelay)

-- Execute io-action when watching file modified.
watch :: FilePath -> IO a -> IO a
watch filename action = do
  action
  lastModified <- getModificationTime filename
  watch' lastModified
    where
      watch' lastModified = do
                          current <- getModificationTime filename
                          when (current /= lastModified) performAction
                          -- Sleep while n micro seconds
                          threadDelay (1 * 1000 * 1000)
                          watch' current
      performAction = do
        putStrLn . replicate 60 $ '-'
        hFlush stdout
        action
        return ()
