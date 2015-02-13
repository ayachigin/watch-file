module WatchFile (watch) where

import Control.Monad (filterM)
import Prelude hiding (FilePath)
import System.FilePath
import System.Directory (getDirectoryContents,
                         getModificationTime,
                         doesDirectoryExist,
                         doesFileExist,
                         canonicalizePath)
import System.IO (hFlush, stdout)
import Control.Monad (when)
import Control.Concurrent (threadDelay)
import Data.Time.Clock (UTCTime)

type Files = [(String, UTCTime)]

-- | Execute io-action when watching file modified.
watch :: FilePath -> IO a -> IO ()
watch path action = do
  action
  paths <- getFilesRecursively path
  files <- fromFilesPaths paths
  watchFiles files action


getFilesRecursively :: FilePath -> IO [FilePath]
getFilesRecursively x = filterFiles =<< getFilesRecursively' x

getFilesRecursively' :: FilePath -> IO [FilePath]
getFilesRecursively' fs = do
  isD <- doesDirectoryExist fs
  if isD then do
    (files, dirs) <- partitionM doesFileExist =<< getFiles fs
    putStrLn $ "dirs: " ++ show  dirs
    files2 <- fmap concat $ mapM getFilesRecursively' dirs
    mapM canonicalizePath (files ++ files2)
  else
    return [fs]

filterFiles :: [FilePath] -> IO [FilePath]
filterFiles = filterM doesFileExist

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f ls = go ls [] []
  where
    go [] as bs = return (reverse as, reverse bs)
    go (x:xs) as bs = do
      r <- f x
      if r then
        go xs (x:as) bs
      else
        go xs as (x:bs)
  
getFiles :: FilePath -> IO [FilePath]
getFiles fs = getFiles' fs
  where
    getFiles' fs = do
      isD <- doesDirectoryExist fs
      if isD then do
        files <- getDirectoryContents fs
        return $ map (\x -> normalise (fs++"/") ++ x) $
                 filter (\x -> x /= "." && x /= "..") files
      else
        return []

fromFilesPaths :: [FilePath] -> IO Files
fromFilesPaths [] = return []
fromFilesPaths (f:fs) = do
  t <- getModificationTime f
  fss <- fromFilesPaths fs
  return $ (f, t): fss


watchFiles :: Files -> IO a -> IO ()
watchFiles files action = do
  currentTimes <- mapM (\(f, _) -> getModificationTime f) files
  when (anyFilesModified currentTimes) (action >> return ())
  threadDelay (1 * 1000 * 1000)
  watchFiles (newFiles currentTimes) action
    where
      anyFilesModified cts = any (\ ((_, t1), t2) -> t1 /= t2) $ zip files cts
      newFiles cts = map (\ ((f, _), t) -> (f, t)) $ zip files cts
