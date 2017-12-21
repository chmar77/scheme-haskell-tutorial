{-# LANGUAGE ExistentialQuantification #-}

module SeventySeven
    ( run 
    ) where

import System.Environment (getArgs)
import Parser (readExpr)
import Evaluator (eval)

run :: IO ()
run = getArgs >>= print . eval . readExpr . head