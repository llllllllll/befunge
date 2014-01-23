-- |
-- Module      : Befunge
-- Copyright   : Joe Jevnik 2013
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- A befunge interpreter.

{-# LANGUAGE CPP #-}

module Main where

import Befunge.Data
import Befunge.Operations
import Befunge.Parser

import Control.Arrow      (first,second)
import Control.Concurrent (forkIO)
import Control.Monad      (void,liftM,(<=<))
import Data.Array.MArray  (readArray)
import Data.Char          (isDigit)
import System.Exit        (exitSuccess)
import System.Environment (getArgs)


-- | Mail loop, handles errors and reads the playfield.
readAll :: Either StateError State -> IO (Either StateError State)
readAll (Left (DivByZeroError (r,c))) =
    error $ "ERROR at (" ++ show r ++ "," ++ show c
              ++ "): Attempted to divide by zero."
readAll (Left (InvalidInputError ch (r,c))) =
    error $ "ERROR at (" ++ show r ++ "," ++ show c ++ "): Invalid input: "
              ++ show ch ++ "."
readAll t@(Left Terminate) = return t
readAll (Right st) = readNext st >>= readAll . incPointer

-- | Reads the next 'State' or any 'StateError's that may have occured.
readNext :: State -> IO (Either StateError State)
readNext st@(State{isString = True}) = readArray (playfield st) (loc st)
                                       >>= \c -> return $ Right
                                                 $ if c == charToWord '"'
                                                     then st {isString = False}
                                                     else sPush (wordToInt c) st
readNext st = readArray (playfield st) (loc st)
              >>= \c -> parseCommand (wordToChar c) st

-- | Reads a symbol and applies the proper function.
parseCommand :: Char -> State -> IO (Either StateError State)
parseCommand '+' = return . Right . sAdd
parseCommand '-' = return . Right . sSub
parseCommand '*' = return . Right . sMul
parseCommand '/' = return         . sDiv
parseCommand '%' = return         . sMod
parseCommand '!' = return . Right . sNot
parseCommand '`' = return . Right . sGT
parseCommand '>' = return . Right . pRight
parseCommand '<' = return . Right . pLeft
parseCommand '^' = return . Right . pUp
parseCommand 'v' = return . Right . pDown
parseCommand '?' = liftM    Right . pRand
parseCommand '_' = return . Right . pCheckLeft
parseCommand '|' = return . Right . pCheckUp
parseCommand '"' = return . Right . (\st -> st { isString = True })
parseCommand ':' = return . Right . sDup
parseCommand '\\'= return . Right . sSwap
parseCommand '$' = return . Right . sPop
parseCommand '.' = liftM    Right . sPrintInt
parseCommand ',' = liftM    Right . sPrintChar
parseCommand '#' = return         . incPointer . Right
parseCommand 'p' = liftM    Right . fPut
parseCommand 'g' = liftM    Right . fGet
parseCommand '&' =                  sInputInt
parseCommand '~' = liftM    Right . sInputChar
parseCommand ' ' = return . Right . id
parseCommand '@' = return         . const (Left Terminate)
parseCommand n
    | isDigit n  = return . Right . sPush (read [n])
    | otherwise  = return . Left  . InvalidInputError n . loc

-- | Increments the pointer of the 'State' based on the 'Direction'.
incPointer :: Either StateError State -> Either StateError State
incPointer e@(Left _) = e
incPointer (Right st@(State {dir = PUp})) =
    Right st { loc = case loc st of
                         (0,c) -> (24,c)
                         l     -> first (flip (-) 1) l }
incPointer (Right st@(State {dir = PDown})) =
    Right st { loc = case loc st of
                         (24,c) -> (0,c)
                         l      -> first (+ 1) l }
incPointer (Right st@(State {dir = PLeft})) =
    Right st { loc = case loc st of
                         (r,0) -> (r,79)
                         l     -> second (flip (-) 1) l }
incPointer (Right st@(State {dir = PRight})) =
    Right st { loc = case loc st of
                         (r,79) -> (r,0)
                         l      -> second (+ 1) l }

main :: IO ()
main = getArgs
       >>= \as -> case as of
                    []  -> putStrLn "Usage: runbefunge [FILE]"
                    [n] | n `elem` ["-h","--help"]
                            -> putStrLn helpString
                        | n `elem` ["-v","--version"]
                            -> putStrLn "runbefunge version 1.0\nBy Joe Jevnik"
                        | otherwise -> void $ stateFromFile n
                                       >>= readAll . Right
                    --("-c":ps) -> map (forkIO $ readAll . Right <=< stateFromFile) ps
                    _ -> mapM_ (readAll . Right <=< stateFromFile) as

helpString :: String
helpString =
    "Usage:\n\n    runbefunge [SOURCE-FILE]\n\nTo run a befunge-93 program, \
run the interpreter like so:\n\n    runbefunge my_prog.bf\n\nwhere \
my_prog.bf is the path to the program you wish to run.\n\nAlternativly, you \
may run multiple files at once by passing them in the order\nin which you \
would like them to be evaluated, for example:\n\n    runbefunge prog1.bf \
prog2.bf\n\nwould run prog1.bf until its null character is reached and then \
begin to run prog2.bf. For this reason, it will not evaluate any programs \
after reaching a program that does not contain a '@' (terminating character). \
"
