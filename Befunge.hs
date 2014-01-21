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

module Befunge where

import Befunge.Data
import Befunge.Operations
import Befunge.Parser

import Control.Arrow      ((***))
import Control.Monad      (void)
import Data.Array.MArray  (readArray)
import System.Exit        (exitSuccess)
import System.Environment (getArgs)

readAll :: Either StateError State -> IO (Either StateError State)
readAll (Left EmptyStackError  ) =
    error "ERROR: Attempted to read from an empty stack."
readAll (Left DivByZeroError   ) =
    error "ERROR: Attempted to divide by zero."
readAll (Left OutOfBoundsError ) =
    error "ERROR: Attempted to put or get out of bounds."
readAll (Left InvalidInputError) =
    error "ERROR: Invalid input."
readAll (Right st)               = readNext (incPointer st) >>= readAll

-- | Reads the next 'State' or any 'StateError's that may have occured.
readNext st@(State{isString = True}) = (case stack st of
                                            [] -> return ()
                                            (n:ns) -> print n) >>
                                       readArray (playfield st) (loc st)
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
    | n >= '0' && n <= '9' = return $ Right $ sPush (charToWord n) st
    | otherwise = return $ Left InvalidInputError

-- | Increments the pointer of the 'State' based on the 'Direction'.
incPointer :: State -> State
incPointer st@(State {dir = PUp})    = st { loc = (id *** flip (-) 1) $ loc st }
incPointer st@(State {dir = PDown})  = st { loc = (id *** (+) 1) $ loc st }
incPointer st@(State {dir = PLeft})  = st { loc = (flip (-) 1 *** id) $ loc st }
incPointer st@(State {dir = PRight}) = st { loc = ((+) 1 *** id) $ loc st}

main :: IO ()
main = getArgs
       >>= \as -> if null as
                    then putStrLn "Usage: runbefunge [FILE]"
                    else void $ stateFromFile (head as) >>= readAll . Right
