module Main where

import           Control.Monad.Reader (runReaderT)
import           Interpreter          (Env (..), interpret)
import           Data.IORef           --(newIORef)
import           Parser               (bashProgram)
import           System.Environment   (getArgs)
import           Text.Megaparsec      (runParser)
import           Data.List.Index      (indexed)
import           Data.Map.Strict      (fromList)

main :: IO ()
main = do
  fileName:args <- getArgs
  input      <- readFile fileName
  let argVarsList = map (\p -> (show $ fst p, snd p)) (indexed args)
  envVarsRef <- newIORef $ fromList argVarsList
  let res = runParser bashProgram fileName input
  case res of
    Right ast -> runReaderT (interpret ast) (Env envVarsRef)
    _         -> putStrLn "Error occurred while parsing\n"
  x <- readIORef envVarsRef
  print x
