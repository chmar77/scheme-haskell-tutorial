{-# LANGUAGE ExistentialQuantification #-}

module Unpacker where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Monad.Error
import System.IO
import Data.IORef
import Type

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

-- unpackNum :: LispVal -> Integer
-- unpackNum (Number n) = n
-- unpackNum (String n) = 
--     let parsed = reads n :: [(Integer, String)] in 
--         if null parsed 
--             then 0
--             else fst $ parsed !! 0
-- unpackNum (List [n]) = unpackNum n
-- unpackNum _ = 0
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
    let parsed = reads n
    in  if null parsed
            then throwError $ TypeMismatch "number" $ String n
            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool   s) = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
    do
            unpacked1 <- unpacker arg1
            unpacked2 <- unpacker arg2
            return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM
        (unpackEquals arg1 arg2)
        [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList