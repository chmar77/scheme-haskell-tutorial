{-# LANGUAGE ExistentialQuantification #-}

module Primitive where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Monad.Error
import System.IO
import Data.IORef
import Type
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
    [ ("+"        , numericBinop (+))
    , ("-"        , numericBinop (-))
    , ("*"        , numericBinop (*))
    , ("/"        , numericBinop div)
    , ("mod"      , numericBinop mod)
    , ("quotient" , numericBinop quot)
    , ("remainder", numericBinop rem)
    , ("="        , numBoolBinop (==))
    , ("<"        , numBoolBinop (<))
    , (">"        , numBoolBinop (>))
    , ("/="       , numBoolBinop (/=))
    , (">="       , numBoolBinop (>=))
    , ("<="       , numBoolBinop (<=))
    , ("&&"       , boolBoolBinop (&&))
    , ("||"       , boolBoolBinop (||))
    , ("string=?" , strBoolBinop (==))
    , ("string<?" , strBoolBinop (<))
    , ("string>?" , strBoolBinop (>))
    , ("string<=?", strBoolBinop (<=))
    , ("string>=?", strBoolBinop (>=))
    , ("car"      , car)
    , ("cdr"      , cdr)
    , ("cons"     , cons)
    , ("eq?"      , eqv)
    , ("eqv?"     , eqv)
    , ("equal?"   , equal)
    ]

primitiveBindings :: IO Env
primitiveBindings = 
    nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                                ++ map (makeFunc PrimitiveFunc) primitives)
        where makeFunc constructor (var, func) = (var, constructor func)
        
car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)        ] = return x
car [DottedList (x:xs) _] = return x
car [badArg             ] = throwError $ TypeMismatch "pair" badArg
car badArgList            = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)        ] = return $ List xs
cdr [DottedList [_   ] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg             ] = throwError $ TypeMismatch "pair" badArg
cdr badArgList            = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []            ] = return $ List [x1]
cons [x , List xs            ] = return $ List $ x : xs
cons [x , DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2                 ] = return $ DottedList [x1] x2
cons badArgList                = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool   arg1), (Bool arg2)  ] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom   arg1), (Atom arg2)  ] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] =
    eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] =
    return
        $  Bool
        $  (length arg1 == length arg2)
        && (all eqvPair $ zip arg1 arg2)
    where
    eqvPair (x1, x2) = case eqv [x1, x2] of
        Left  err        -> False
        Right (Bool val) -> val
eqv [_, _]     = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

-- numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
-- numericBinop op params = Number $ foldl1 op $ map unpackNum params
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
    then throwError $ NumArgs 2 args
    else do
        left  <- unpacker $ args !! 0
        right <- unpacker $ args !! 1
        return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

