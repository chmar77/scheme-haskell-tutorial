{-# LANGUAGE ExistentialQuantification #-}

module Type where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Monad.Error
import System.IO
import Data.IORef

data LispVal =
    Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func { params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env }
  | IOFunc ([LispVal] -> IOThrowsError LispVal)
  | Port Handle

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

data LispError =
    NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

type ThrowsError = Either LispError

type Env = IORef [(String, IORef LispVal)]

type IOThrowsError = ErrorT LispError IO