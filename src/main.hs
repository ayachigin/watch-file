module Main where


import System.Cmd (system)
import System.Exit (ExitCode)
import System.Environment (getArgs)


import WatchFile (watch)


main :: IO ExitCode
main = do
  args <- getArgs
  let filename = head args
      command = tail args
  watch filename (system . unwords $ command)
