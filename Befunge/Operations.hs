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
    , sAdd       -- :: State -> State
    , sSub       -- :: State -> State
    , sMul       -- :: State -> State
    , sDiv       -- :: State -> Either StateError State
    , sMod       -- :: State -> Either StateError State
    , sGT        -- :: State -> State
    , sSwap      -- :: State -> State
    , sNot       -- :: State -> State
    , sPop       -- :: State -> State
    , sDup       -- :: State -> State
    , pUp        -- :: State -> State
    , pDown      -- :: State -> State
    , pLeft      -- :: State -> State
    , pRight     -- :: State -> State
    , pCheckLeft -- :: State -> State
    , pCheckUp   -- :: State -> State
    , pRand      -- :: State -> IO State
    , sPrintInt  -- :: State -> IO State
    , sPrintChar -- :: State -> IO State
    , sInputInt  -- :: State -> IO (Either StateError State)
    , sInputChar -- :: State -> IO State
    , fPut       -- :: State -> IO  State
    , fGet       -- :: State -> IO State
    )  where

import Befunge.Data

import Control.Applicative ((<$>))
import Control.Monad       (liftM)
import Data.Array.MArray   (writeArray,readArray)
import System.Random       (randomR,getStdRandom)
import Text.Read           (readMaybe)

-- -----------------------------------------------------------------------------
--

-- | Pushes a char or number litteral onto the stack.
-- "0-9a-zA-Z"
sPush :: Int32 -> State -> State
sPush n st = st { stack = n : stack st }

-- -----------------------------------------------------------------------------
-- Binary operations.

-- | Adds the top 2 elements on the stack pushing the result.
-- '+'
sAdd :: State -> State
sAdd st@(State {stack = [] }) = st { stack = [0]   }
sAdd st@(State {stack = [n]}) = st { stack = [0,n] }
sAdd st = let [b,a] = take 2 $ stack st
          in st { stack = a + b : drop 2 (stack st) }

-- | Subtracts the top 2 elements on the stack pushing the result.
-- '-'
sSub :: State -> State
sSub st@(State {stack = [] }) = st { stack = [0]   }
sSub st@(State {stack = [n]}) = st { stack = [n,n] }
sSub st = let [b,a] = take 2 $ stack st
          in st { stack = b - a : drop 2 (stack st) }

-- | Multiplies the top 2 elements on the stack pushing the result.
-- '*'
sMul :: State -> State
sMul st@(State {stack = [] }) = st { stack = [0] }
sMul st@(State {stack = [_]}) = st { stack = [0] }
sMul st = let [b,a] = take 2 $ stack st
          in st { stack = b * a : drop 2 (stack st) }

-- | Divides the top 2 elements on the stack pushing the result.
-- '/'
sDiv :: State -> Either StateError State
sDiv st@(State {stack = [] }) = Left $ DivByZeroError (loc st)
sDiv st@(State {stack = [_]}) = Left $ DivByZeroError (loc st)
sDiv st = let [b,a] = take 2 $ stack st
          in if b == 0
               then Left (DivByZeroError (loc st))
               else Right st { stack = b `div` a : drop 2 (stack st) }

-- | Mods the top 2 elements on the stack pushing the result.
-- '%'
sMod :: State -> Either StateError State
sMod st@(State {stack = [] }) = Left $ DivByZeroError (loc st)
sMod st@(State {stack = [_]}) = Left $ DivByZeroError (loc st)
sMod st = let [b,a] = take 2 $ stack st
          in if b == 0
               then Left $ DivByZeroError (loc st)
               else Right st { stack = b `mod` a : drop 2 ( stack st) }

-- | Pushes 1 if b > a otherwise pushes 0.
-- '`'
sGT  :: State -> State
sGT  st@(State {stack = [] }) = st { stack = [0] }
sGT  st@(State {stack = [n]}) = st { stack = if 0 > n
                                               then [1]
                                               else [0] }
sGT  st = let [b,a] = take 2 $ stack st
          in st { stack = (if a > b
                             then 1
                             else 0) : drop 2 (stack st) }

-- | Pops a and b and swaps them.
-- '\'
sSwap :: State -> State
sSwap st@(State {stack = [] }) =  st
sSwap st@(State {stack = [n]}) = st { stack = [0,n] }
sSwap st = let [b,a] = take 2 $ stack st
           in st { stack = a : b : drop 2 (stack st) }

-- -----------------------------------------------------------------------------
-- Unary operations.

-- | Pops the top value and pushes 1 if it is 0, or 0 otherwise.
-- '!'
sNot :: State -> State
sNot st@(State {stack = []}) = st { stack = [1] }
sNot st = let a = head $ stack st
          in st { stack = (if a == 0
                             then 1
                             else 0) : tail (stack st) }

-- | Pops a value from the stack and discards it.
-- '$'
sPop :: State -> State
sPop st = st { stack = drop 1 $ stack st }

-- | Duplicates the top value on the stack.
-- ':'
sDup :: State -> State
sDup st@(State {stack = []}) = st { stack = [0] }
sDup st = st { stack = head (stack st) : stack st }

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
pCheckLeft :: State -> State
pCheckLeft st@(State {stack = []}) = pRight st
pCheckLeft st = let a = head . stack $ st
                in st { stack = tail . stack $ st
                      , dir   = if a == 0
                                  then PRight
                                  else PLeft }

-- | Pop a value, start moving up if it is non-zero, otherwise move down.
-- '|'
pCheckUp :: State -> State
pCheckUp st@(State {stack = []}) = pDown st
pCheckUp st = let a = head . stack $ st
                in st { stack = tail . stack $ st
                      , dir   = if a == 0
                                  then PDown
                                  else PLeft }

-- | Start moving in a random direction.
-- '?'
pRand :: State -> IO State
pRand st = liftM (`pSetDir` st)
           (getDir <$> getStdRandom (randomR (0 :: Int,3 :: Int)))
  where
      getDir 0 = PUp
      getDir 1 = PDown
      getDir 2 = PLeft
      getDir 3 = PRight

-- -----------------------------------------------------------------------------
-- IO.

-- | Pops the top value and prints it as an Int (Word8).
-- '.'
sPrintInt :: State -> IO State
sPrintInt st@(State {stack = []}) = putStr "0" >> return st
sPrintInt st = putStr (show . head . stack $ st) >> return (sPop st)

-- | Pops the top value and prints it as a Char.
-- ','
sPrintChar :: State -> IO State
sPrintChar st@(State {stack = []}) = putStr "\0" >> return st
sPrintChar st = putStr [intToChar . head . stack $ st]
                >> return (sPop st)

-- | Ask a user to input an integer value mod 256.
-- '&'
sInputInt :: State -> IO (Either StateError State)
sInputInt st = readMaybe . (:[]) <$> getChar >>= \n ->
               case n of
                   Nothing -> return $ Left $ InvalidInputError
                              (maybe ' ' intToChar n) (loc st)
                   Just v  -> return $ Right st { stack = v : stack st }

-- | Ask a user to input a Char and push its ASCII value.
-- '~'
sInputChar :: State -> IO State
sInputChar st = getChar >>= \c -> return st { stack = charToInt c : stack st }

-- -----------------------------------------------------------------------------
-- Playfield.

-- | Put a Char into the playfield.
-- 'p'
fPut :: State -> IO State
fPut st@(State {stack = []   }) = fPut' st 0 0 0
fPut st@(State {stack = [y]  }) = fPut' st y 0 0
fPut st@(State {stack = [y,x]}) = fPut' st y x 0
fPut st = let [y,x,v] = take 3 $ stack st
          in fPut' st y x $ intToWord v

-- | Helper function to fPut.
fPut' :: State -> Int32 -> Int32 -> Word8 -> IO State
fPut' st y x v = if x > 79 || x < 0 || y > 24 || y < 0
               then return st { stack = drop 3 $ stack st }
               else writeArray (playfield st)
                        (intToWord y,intToWord x) v
                        >> return st { stack = drop 3 $ stack st }

-- | Get a Char out of the playfield.
-- 'g'
fGet :: State -> IO State
fGet st@(State {stack = [] }) = fGet' st 0 0
fGet st@(State {stack = [y]}) = fGet' st y 0
fGet st = let [y,x] = take 2 $ stack st
          in fGet' st y x

-- | Helper function to fGet.
fGet' :: State -> Int32 -> Int32 -> IO State
fGet' st y x = if x > 79 || x < 0 || y > 24 || y < 0
               then return st { stack = drop 2 $ stack st }
               else readArray (playfield st) (intToWord y,intToWord x)
                        >>= \v -> return st { stack = wordToInt v
                                                      : drop 2 (stack st) }
