module Main where

import           Control.Monad.Reader (runReaderT)
import           Data.IORef           (newIORef, readIORef)
import           Data.List.Index      (indexed)
import           Data.Map.Strict      (fromList)
import           Interpreter          (Env (..), interpret)
import           Parser               (bashProgram)
import           System.Environment   (getArgs)
import           Text.Megaparsec      (runParser)

main :: IO ()
main = do
  fileName:args <- getArgs
  input      <- readFile fileName
  let argVarsList = map (\p -> (show $ fst p, snd p)) (indexed args)
  envVarsRef <- newIORef $ fromList argVarsList
  let res = runParser bashProgram fileName input
  case res of
    Right ast -> do
      runReaderT (interpret ast) (Env envVarsRef)
      x <- readIORef envVarsRef
      print x
    _         -> putStrLn "Error occurred while parsing"
