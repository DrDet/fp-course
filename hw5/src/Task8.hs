{-# LANGUAGE RankNTypes #-}

module Task8
  (
    replaceFileExtensions
  , getFullContent
  , deleteSubDir
  , FS(..)
  ) where

import           Control.Monad         (join)
import           Data.Function         ((&))
import           Data.List             (deleteBy)
import           Lens.Micro            ((%~), (^.), (^..))
import           System.FilePath.Posix (replaceExtension)
import           Task6                 (children, _File)
import           Task7                 (FS (..), ls)

replaceFileExtensions :: String -> FS -> FS
replaceFileExtensions ext = children %~ (map replaceExt)
    where
      replaceExt = _File %~ (flip replaceExtension ext)

getFullContent :: FS -> [FilePath]
getFullContent fs = (join curContent) ++ childrenContent
  where
    curContent      = fs ^.. ls
    childrenContent = join $ map getFullContent (fs ^. children)

deleteSubDir :: FS -> FilePath -> FS
deleteSubDir fs targetSubDir = fs & children %~ deleter
    where
      deleter :: [FS] -> [FS]
      deleter l = deleteBy predicate (Dir "" []) l
      predicate _ cur =
        case cur of
          Dir curName [] -> curName == targetSubDir
          _              -> False
