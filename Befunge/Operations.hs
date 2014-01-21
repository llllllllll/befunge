-- |
-- Module      : Befunge.Operations
-- Copyright   : Joe Jevnik 2013
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- All operations for my befunge-93 interpreter.
-- Stack operations are prefixed with an s. I.E. to push to the stack use sPush
-- or to add onto the stack, use sAdd.
-- Pointer operations are prefixed with p. I.E. to set the pointer right, use
-- pRight or to set it down use pDown.
-- Functions that act on the playfield are prefixed f. fPut and fGet.

module Befunge.Operations
    ( sPush      -- :: Word8 -> State -> State
    , sAdd       -- :: State -> Either StateError State
    , sSub       -- :: State -> Either StateError State
    , sMul       -- :: State -> Either StateError State
    , sDiv       -- :: State -> Either StateError State
    , sMod       -- :: State -> Either StateError State
    , sGT        -- :: State -> Either StateError State
    , sSwap      -- :: State -> Either StateError State
    , sNot       -- :: State -> Either StateError State
    , sPop       -- :: State -> Either StateError State
    , sDup       -- :: State -> Either StateError State
    , pUp        -- :: State -> State
    , pDown      -- :: State -> State
    , pLeft      -- :: State -> State
    , pRight     -- :: State -> State
    , pCheckLeft -- :: State -> Either StateError State
    , pCheckUp   -- :: State -> Either StateError State
    , pRand      -- :: State -> IO State
    , sPrintInt  -- :: State -> IO (Either StateError State)
    , sPrintChar -- :: State -> IO (Either StateError State)
    , sInputInt  -- :: State -> IO (Either StateError State)
    , sInputChar -- :: State -> IO State
    , fPut       -- :: State -> IO (Either StateError State)
    , fGet       -- :: State -> IO (Either StateError State)
    )  where

import Befunge.Data

import Control.Applicative ((<$>))
import Data.Array.MArray
import System.Random       (randomR,getStdRandom)
import Text.Read           (readMaybe)

-- -----------------------------------------------------------------------------
--

-- | Pushes a char or number litteral onto the stack.
-- "0-9a-zA-Z"
sPush :: Word8 -> State -> State
sPush n st = st { stack = n : stack st }

-- -----------------------------------------------------------------------------
-- Binary operations.

-- | Adds the top 2 elements on the stack pushing the result.
-- '+'
sAdd :: State -> Either StateError State
sAdd st@(State {stack = [] }) = Right st { stack = [0] }
sAdd st@(State {stack = [_]}) = Right st
sAdd st = let [b,a] = take 2 $ stack st
          in Right st { stack = a + b : (drop 2 $ stack st) }

-- | Subtracts the top 2 elements on the stack pushing the result.
-- '-'
sSub :: State -> Either StateError State
sSub st@(State {stack = [] }) = Right st { stack = [0] }
sSub st@(State {stack = [_]}) = Right st
sSub st = let [b,a] = take 2 $ stack st
          in Right st { stack = b - a : (drop 2 $ stack st) }

-- | Multiplies the top 2 elements on the stack pushing the result.
-- '*'
sMul :: State -> Either StateError State
sMul st@(State {stack = [] }) = Right st { stack = [0] }
sMul st@(State {stack = [_]}) = Right st { stack = [0] }
sMul st = let [b,a] = take 2 $ stack st
          in Right st { stack = b * a : (drop 2 $ stack st) }

-- | Divides the top 2 elements on the stack pushing the result.
-- '/'
sDiv :: State -> Either StateError State
sDiv (State {stack = [] }) = Left DivByZeroError
sDiv (State {stack = [_]}) = Left DivByZeroError
sDiv st = let [b,a] = take 2 $ stack st
          in if b == 0
               then Left DivByZeroError
               else Right st { stack = b `div` a : (drop 2 $ stack st) }

-- | Mods the top 2 elements on the stack pushing the result.
-- '%'
sMod :: State -> Either StateError State
sMod (State {stack = [] }) = Left DivByZeroError
sMod (State {stack = [_]}) = Left DivByZeroError
sMod st = let [b,a] = take 2 $ stack st
          in if b == 0
               then Left DivByZeroError
               else Right st { stack = b `mod` a : (drop 2 $ stack st) }

-- | Pushes 1 if b > a otherwise pushes 0.
-- '`'
sGT  :: State -> Either StateError State
sGT  st@(State {stack = [] }) = Right st
sGT  st@(State {stack = [n]}) = Right st { stack = if n > 0
                                                     then [1]
                                                     else [0] }
sGT  st = let [b,a] = take 2 $ stack st
          in Right st { stack = (if b > a
                                   then 1
                                   else 0) : (drop 2 $ stack st) }

-- | Pops a and b and swaps them.
-- '\'
sSwap :: State -> Either StateError State
sSwap st@(State {stack = [] }) = Right st
sSwap st@(State {stack = [_]}) = Right st
sSwap st = let [b,a] = take 2 $ stack st
           in Right st { stack = a : b : (drop 2 $ stack st) }

-- -----------------------------------------------------------------------------
-- Unary operations.

-- | Pops the top value and pushes 1 if it is 0, or 0 otherwise.
-- '!'
sNot :: State -> Either StateError State
sNot st@(State {stack = []}) = Right st { stack = [1] }
sNot st = let a = head $ stack st
          in Right st { stack = (if a == 0
                                   then 1
                                   else 0) : (tail $ stack st) }

-- | Pops a value from the stack and discards it.
-- '$'
sPop :: State -> Either StateError State
sPop st@(State {stack = []}) = Right st
sPop st = Right st { stack = tail $ stack st }

-- | Duplicates the top value on the stack.
-- ':'
sDup :: State -> Either StateError State
sDup st@(State {stack = []}) = Right st
sDup st = Right st { stack = (head $ stack st) : stack st }

-- -----------------------------------------------------------------------------
-- Pointer operations.

-- | Start moving up.
-- '^'
pUp :: State -> State
pUp = pSetDir PUp

-- | Start moving down.
-- 'v'
pDown :: State -> State
pDown = pSetDir PDown

-- | Start moving left.
-- '<'
pLeft :: State -> State
pLeft = pSetDir PLeft

-- | Start moving right.
-- '>'
pRight :: State -> State
pRight = pSetDir PRight

-- | Helper function to shorten the other pSets.
pSetDir :: PDirection -> State -> State
pSetDir pDir st = st { dir = pDir }

-- | Pop a value, start moving left if it is non-zero, otherwise move right.
-- '_'
pCheckLeft :: State -> Either StateError State
pCheckLeft st@(State {stack = []}) = Right $ pRight st
pCheckLeft st = let a = head . stack $ st
                in Right st { stack = tail . stack $ st
                            , dir   = if a == 0
                                        then PRight
                                        else PLeft }

-- | Pop a value, start moving up if it is non-zero, otherwise move down.
-- '|'
pCheckUp :: State -> Either StateError State
pCheckUp st@(State {stack = []}) = Right $ pDown st
pCheckUp st = let a = head . stack $ st
                in Right st { stack = tail . stack $ st
                            , dir   = if a == 0
                                        then PDown
                                        else PLeft }

-- | Start moving in a random direction.
-- '?'
pRand :: State -> IO (Either StateError State)
pRand st = getDir <$> getStdRandom (randomR (0 :: Int,3 :: Int))
           >>= return . Right . flip pSetDir st
  where
      getDir 0 = PUp
      getDir 1 = PDown
      getDir 2 = PLeft
      getDir 3 = PRight

-- -----------------------------------------------------------------------------
-- IO.

-- | Pops the top value and prints it as an Int (Word8).
-- '.'
sPrintInt :: State -> IO (Either StateError State)
sPrintInt st@(State {stack = []}) = putStr "0" >> return (Right st)
sPrintInt st = putStr (show . head . stack $ st) >> return (sPop st)

-- | Pops the top value and prints it as a Char.
-- ','
sPrintChar :: State -> IO (Either StateError State)
sPrintChar st@(State {stack = []}) = putStr "\0" >> return (Right st)
sPrintChar st = putStr ([wordToChar . head . stack $ st])
                >> return (sPop st)

-- | Ask a user to input an integer value mod 256.
-- '&'
sInputInt :: State -> IO (Either StateError State)
sInputInt st = readMaybe . (\c -> [c]) <$> getChar >>= \n ->
               case n of
                   Nothing -> return $ Left InvalidInputError
                   Just v  -> return $ Right st { stack = v : stack st }

-- | Ask a user to input a Char and push its ASCII value.
-- '~'
sInputChar :: State -> IO (Either StateError State)
sInputChar st = getChar
                >>= \c -> return $ Right st { stack = charToWord c : stack st }

-- -----------------------------------------------------------------------------
-- Playfield.

-- | Put a Char into the playfield.
-- 'p'
fPut :: State -> IO (Either StateError State)
fPut (State {stack = []   }) = return $ Left EmptyStackError
fPut (State {stack = [_]  }) = return $ Left EmptyStackError
fPut (State {stack = [_,_]}) = return $ Left EmptyStackError
fPut st = let [y,x,v] = take 3 $ stack st
          in if x > 79 || y > 24
               then return $ Left OutOfBoundsError
               else writeArray (playfield st) (x,y) v >> return (Right st)

-- | Get a Char out of the playfield.
-- 'g'
fGet :: State -> IO (Either StateError State)
fGet (State {stack = [] }) = return $ Left EmptyStackError
fGet (State {stack = [_]}) = return $ Left EmptyStackError
fGet st = let [y,x] = take 2 $ stack st
          in if x > 79 || y > 24
               then return $ Left OutOfBoundsError
               else readArray (playfield st) (x,y)
                        >>= \v -> return (Right st { stack = v : stack st })
