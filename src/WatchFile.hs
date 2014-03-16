module WatchFile where


import System.Directory (getModificationTime)
import System.Cmd (system)
import System.IO (hFlush, stdout)
import Control.Monad (when)
import Control.Concurrent (threadDelay)

-- Execute io-action when watching file modified.
watch :: FilePath -> IO a -> IO ()
watch filename action = do
  action
  last_modified <- getModificationTime filename
  watch' last_modified
    where
      watch' last_modified = do
                          current <- getModificationTime filename
                          when (current /= last_modified) performAction
                          -- Sleep while n micro seconds
                          threadDelay (1 * 1000 * 1000)
                          watch' current
      performAction = do
        putStrLn . replicate 60 $ '-'
        hFlush stdout
        action
        return ()
