module SeventySeven where

import Repl (runRepl, runOne)
import System.Environment (getArgs)

run :: IO ()
run = do 
        args <- getArgs
        if null args then runRepl else runOne $ args