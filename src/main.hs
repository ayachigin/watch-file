module Main where


import System.Process (system)
import System.Exit (ExitCode)
import System.Environment (getArgs)


import WatchFile (watch)


main :: IO ()
main = do
  args <- getArgs
  let filename = head args
      command = tail args
  watch filename (system . unwords $ command)
