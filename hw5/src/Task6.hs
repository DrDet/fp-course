module Task6
  (
    FS(..)
  , getFSTree
  , label
  , children
  , _Dir
  , _File
  ) where

import           Control.Monad         (when)
import           Lens.Micro            (Lens', Traversal', lens)
import           System.Directory      (doesDirectoryExist, doesFileExist,
                                        listDirectory)
import           System.FilePath.Posix (takeFileName, (</>))

data FS
  = Dir
  { name     :: FilePath  -- название папки, не полный путь
  , contents :: [FS]
  }
  | File
  { name     :: FilePath  -- название файла, не полный путь
  }
  deriving Show

getFSTree :: FilePath -> IO FS
getFSTree root = do
  isFile <- doesFileExist root
  if isFile then return $ File { name = takeFileName root }
      else do
            isDir <- doesDirectoryExist root
            when (not isDir) $ error $ "File " ++ root ++ " doesn't exist"
            content   <- listDirectory root
            children' <- mapM (getFSTree . (root </>)) content
            return $ Dir { name = takeFileName root, contents = children' }

label :: Lens' FS FilePath
label = lens name (\x v -> x { name = v })

children :: Lens' FS [FS]
children = lens getter setter
        where
          getter x =
            case x of
              Dir _ l -> l
              File _  -> []
          setter x l' =
            case x of
              Dir _ _ -> x { contents = l' }
              file    -> file

-- Applicative f => ((FilePath, [FS]) -> f (FilePath, [FS])) -> FS -> f FS
_Dir :: Traversal' FS (FilePath, [FS])
_Dir mf (Dir path l) = (\p -> Dir (fst p) (snd p)) <$> mf (path, l)
_Dir _ file          = pure file

_File :: Traversal' FS FilePath
_File mf (File path) = File <$> mf path
_File _ dir          = pure dir
