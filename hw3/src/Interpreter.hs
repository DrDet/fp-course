module Interpreter
  (
    Env(..)
  , interpret
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT (..), ask)
import           Data.IORef             (IORef, readIORef, writeIORef)
import           Data.Map.Strict        (Map, insert, lookup)
import           Data.Maybe             (fromMaybe)
import           Parser                 (Expr (..), Sequence (..),
                                         Statement (..), Variable (..))
import           Prelude                hiding (lookup)

newtype Env = Env
  { vars :: IORef (Map String String)
  }

interpret :: Sequence -> ReaderT Env IO ()
interpret (Sequence []) = return ()
interpret (Sequence (st:ast)) = do
  env <- ask
  interpretStatement env st
  interpret (Sequence ast)

interpretStatement :: Env
                   -> Statement
                   -> ReaderT Env IO ()
interpretStatement (Env envVarsRef)
                   (Assignment (Variable varName) exprList) = do
  envVars <- liftIO $ readIORef envVarsRef
  let newVal = expandExprList envVars exprList
  liftIO $ writeIORef envVarsRef (insert varName newVal envVars)


expandExprList :: Map String String -> [Expr] -> String
expandExprList _ [] = ""
expandExprList envVars (expr:rest) = expanded ++ expandExprList envVars rest
  where
    expanded = case expr of
      (VariableAccess (Variable varName))    -> fromMaybe "" el
        where el = lookup varName envVars
      (SingleQuotedText s)                   -> s
