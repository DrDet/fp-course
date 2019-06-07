{-# LANGUAGE RankNTypes #-}

module Task7
  (
    cd
  , ls
  , file
  , FS(..)
  ) where

import           Data.List  (find)
import           Lens.Micro (SimpleGetter, Traversal', to)
import           Task6      (FS (..))

-- Applicative f => (FS -> f FS) -> FS -> f FS
cd :: FilePath -> Traversal' FS FS
cd targetPath mf curDir@Dir{ contents = l } =
  case e of
    Just subDir@(Dir _ _) -> mf subDir
    _                     -> pure curDir
    where
      e = find ((targetPath ==) . name) l
cd _ _ fileName = pure fileName

ls :: SimpleGetter FS [FilePath]
ls = to $ \x -> case x of
            Dir _ _ -> map name (contents x)
            _       -> []

file :: FilePath -> Traversal' FS FilePath
file targetFileName mf curDir@Dir{ contents = l } =
  case e of
    Just (File fileName) -> File <$> mf fileName
    _                    -> pure curDir
    where
      e = find ((targetFileName ==) . name) l
file _ _ fileName = pure fileName
