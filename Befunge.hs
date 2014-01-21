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

module Main where

import Befunge.Data
import Befunge.Operations
import Befunge.Parser

import Control.Arrow      ((***))
import Control.Monad      (void)
import Data.Array.MArray  (readArray)
import System.Exit        (exitSuccess)
import System.Environment (getArgs)
import Data.Tuple

readAll :: Either StateError State -> IO (Either StateError State)
readAll (Left (DivByZeroError (r,c))) =
    error $ "ERROR at (" ++ show r ++ "," ++ show c
              ++ "): Attempted to divide by zero."
readAll (Left (OutOfBoundsError (r,c) (r',c'))) =
    error $ "ERROR at (" ++ show r ++ "," ++ show c
              ++ "): Attempted to put or get out of bounds at index: ("
              ++ show r' ++ "," ++ show c' ++ ")."
readAll (Left (InvalidInputError ch (r,c))) =
    error $ "ERROR at (" ++ show r ++ "," ++ show c ++ "): Invalid input: "
              ++ show ch ++ "."
readAll (Right st) = readNext (incPointer st) >>= readAll

-- | Reads the next 'State' or any 'StateError's that may have occured.
readNext st@(State{isString = True}) = readArray (playfield st) (loc st)
                                       >>= \c -> return $ Right
                                                 $ if c == charToWord '"'
                                                     then st {isString = False}
                                                     else sPush c st
readNext st = readArray (playfield st) (loc st)
              >>= \c -> parseCommand (wordToChar c) st

-- | Reads a symbol and applies the proper function.
parseCommand :: Char -> State -> IO (Either StateError State)
parseCommand '+' st = return $ sAdd st
parseCommand '-' st = return $ sSub st
parseCommand '*' st = return $ sMul st
parseCommand '/' st = return $ sDiv st
parseCommand '%' st = return $ sMod st
parseCommand '!' st = return $ sNot st
parseCommand '`' st = return $ sGT  st
parseCommand '>' st = return $ Right . pRight $ st
parseCommand '<' st = return $ Right . pLeft  $ st
parseCommand '^' st = return $ Right . pUp    $ st
parseCommand 'v' st = return $ Right . pDown  $ st
parseCommand '?' st = pRand st
parseCommand '_' st = return $ pCheckLeft st
parseCommand '|' st = return $ pCheckUp st
parseCommand '"' st = return $ Right . (\st -> st { isString = True }) $ st
parseCommand ':' st = return $ sDup st
parseCommand '\\'st = return $ sSwap st
parseCommand '$' st = return $ sPop st
parseCommand '.' st = sPrintInt st
parseCommand ',' st = sPrintChar st
parseCommand '#' st = return $ Right . incPointer $ st
parseCommand 'p' st = fPut st
parseCommand 'g' st = fGet st
parseCommand '&' st = sInputInt st
parseCommand '~' st = sInputChar st
parseCommand ' ' st = return $ Right . id $ st
parseCommand '@' st = exitSuccess
parseCommand n st
    | n >= '0' && n <= '9' = return $ Right $ sPush (read [n]) st
    | otherwise = return $ Left $ InvalidInputError n (loc st)

-- | Increments the pointer of the 'State' based on the 'Direction'.
incPointer :: State -> State
incPointer st@(State {dir = PUp}) =
    st { loc = case loc st of
                   (0,c) -> (24,c)
                   l     -> (flip (-) 1 *** id) l }
incPointer st@(State {dir = PDown}) =
    st { loc = case loc st of
                   (24,c) -> (0,c)
                   l      -> ((+) 1 *** id) l }
incPointer st@(State {dir = PLeft}) =
    st { loc = case loc st of
                   (r,0) -> (r,79)
                   l     -> (id *** flip (-) 1) l }
incPointer st@(State {dir = PRight}) =
    st { loc = case loc st of
                   (r,79) -> (r,0)
                   l      -> (id *** (+) 1) l }

main :: IO ()
main = getArgs
       >>= \as -> if null as
                    then putStrLn "Usage: runbefunge [FILE]"
                    else void $ stateFromFile (head as) >>= readAll . Right
