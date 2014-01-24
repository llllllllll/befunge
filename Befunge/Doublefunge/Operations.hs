-- |
-- Module      : Doublefunge.Operations
-- Copyright   : Joe Jevnik 2013
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- All operations for my double interpreter.
-- Stack operations are prefixed with an s. I.E. to push to the stack use sPush
-- or to add onto the stack, use sAdd.
-- Pointer operations are prefixed with p. I.E. to set the pointer right, use
-- pRight or to set it down use pDown.
-- Functions that act on the playfield are prefixed f. fPut and fGet.

module Befunge.Doublefunge.Operations
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
    , pHorzIf    -- :: State -> State
    , pVertIf    -- :: State -> State
    , pRand      -- :: State -> IO State
    , sPrintInt  -- :: State -> IO State
    , sPrintChar -- :: State -> IO State
    , sInputInt  -- :: State -> IO (Either StateError State)
    , sInputChar -- :: State -> IO State
    , fPut       -- :: State -> IO  State
    , fGet       -- :: State -> IO State
    )  where

import Befunge.Doublefunge.Data

import Control.Applicative ((<$>))
import Control.Monad       (liftM)
import Data.Array.MArray   (writeArray,readArray)
import System.Random       (randomR,getStdRandom)
import Text.Read           (readMaybe)

-- -----------------------------------------------------------------------------
--

-- | Pushes a number litteral onto the stack.
-- "0-9"
sPush :: Int32 -> State -> State
sPush n st = st { stack = n : stack st }

-- -----------------------------------------------------------------------------
-- Binary operations.

-- | Adds the top 2 elements on the stack pushing the result.
-- '+'
sAdd :: State -> State
sAdd st@(State {stack = [] })      = st { stack = [0]   }
sAdd st@(State {stack = [n]})      = st { stack = [n] }
sAdd st@(State {stack = (b:a:rs)}) = st { stack = a + b : rs }

-- | Subtracts the top 2 elements on the stack pushing the result.
-- '-'
sSub :: State -> State
sSub st@(State {stack = [] })      = st { stack = [0]   }
sSub st@(State {stack = [n]})      = st { stack = [-n] }
sSub st@(State {stack = (b:a:rs)}) = st { stack = a - b : rs }

-- | Multiplies the top 2 elements on the stack pushing the result.
-- '*'
sMul :: State -> State
sMul st@(State {stack = [] })      = st { stack = [0] }
sMul st@(State {stack = [_]})      = st { stack = [0] }
sMul st@(State {stack = (b:a:rs)}) = st { stack = a * b : rs }

-- | Divides the top 2 elements on the stack pushing the result.
-- '/'
sDiv :: State -> Either StateError State
sDiv st@(State {stack = [] })      = throwDivByZeroError st
sDiv st@(State {stack = [_]})      = throwDivByZeroError st
sDiv st@(State {stack = (0:_:_)})  = throwDivByZeroError st
sDiv st@(State {stack = (b:a:rs)}) = Right st { stack = a `div` b : rs }

-- | Mods the top 2 elements on the stack pushing the result.
-- '%'
sMod :: State -> Either StateError State
sMod st@(State {stack = [] })      = throwDivByZeroError st
sMod st@(State {stack = [_]})      = throwDivByZeroError st
sMod st@(State {stack = (0:_:_)})  = throwDivByZeroError st
sMod st@(State {stack = (b:a:rs)}) = Right st { stack = a `mod` b : rs }

-- | Pushes 1 if b > a otherwise pushes 0.
-- '`'
sGT  :: State -> State
sGT  st@(State {stack = [] }) = st { stack = [0] }
sGT  st@(State {stack = [n]}) = st { stack = if 0 > n
                                               then [1]
                                               else [0] }
sGT  st@(State {stack = (b:a:rs)}) = st { stack = (if a > b
                                                     then 1
                                                     else 0) : rs }

-- | Pops a and b and swaps them.
-- '\'
sSwap :: State -> State
sSwap st@(State {stack = [] }) =  st
sSwap st@(State {stack = [n]}) = st { stack = [0,n] }
sSwap st@(State {stack = (b:a:rs)}) = st { stack = a : b : rs }

-- -----------------------------------------------------------------------------
-- Unary operations.

-- | Pops the top value and pushes 1 if it is 0, or 0 otherwise.
-- '!'
sNot :: State -> State
sNot st@(State {stack = []})     = st { stack = [1] }
sNot st@(State {stack = (a:rs)}) = st { stack = (if a == 0
                                                   then 1
                                                   else 0) : rs }

-- | Pops a value from the stack and discards it.
-- '$'
sPop :: State -> State
sPop st = st { stack = drop 1 $ stack st }

-- | Duplicates the top value on the stack.
-- ':'
sDup :: State -> State
sDup st@(State {stack = []})       = st { stack = [0] }
sDup st@(State {stack = rs@(a:_)}) = st { stack = a : rs }

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
pSetDir dir st@(State{currPointer = Primary}) = st { pDir = dir }
pSetDir dir st                                = st { sDir = dir }

-- | Pop a value, start moving left if it is non-zero, otherwise move right.
-- '_'
pHorzIf :: State -> State
pHorzIf st@(State {stack = []}) = pRight st
pHorzIf st@(State {currPointer = Primary,stack = (0:rs)}) = st { stack = rs
                                                               , pDir   = PRight
                                                               }
pHorzIf st@(State {stack = (0:rs)}) =                       st { stack = rs
                                                               , sDir   = PRight
                                                               }
pHorzIf st@(State {currPointer = Primary,stack = (_:rs)}) = st { stack = rs
                                                               , pDir   = PLeft
                                                               }
pHorzIf st@(State {stack = (_:rs)}) =                       st { stack = rs
                                                               , sDir   = PLeft
                                                               }

-- | Pop a value, start moving up if it is non-zero, otherwise move down.
-- '|'
pVertIf :: State -> State
pVertIf st@(State {stack = []})     = pDown st
pVertIf st@(State {currPointer = Primary,stack = (0:rs)}) = st { stack = rs
                                                               , pDir   = PDown
                                                               }
pVertIf st@(State {stack = (0:rs)}) =                       st { stack = rs
                                                               , pDir   = PDown
                                                               }
pVertIf st@(State {currPointer = Primary,stack = (_:rs)}) = st { stack = rs
                                                               , pDir   = PUp }
pVertIf st@(State {stack = (_:rs)}) =                       st { stack = rs
                                                               , sDir   = PUp }

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
sPrintInt st@(State {stack = []})    = putStr "0" >> return st
sPrintInt st@(State {stack = (a:_)}) = putStr (show a) >> return (sPop st)

-- | Pops the top value and prints it as a Char.
-- ','
sPrintChar :: State -> IO State
sPrintChar st@(State {stack = []})    = putStr "\0" >> return st
sPrintChar st@(State {stack = (a:_)}) = putStr [intToChar a] >> return (sPop st)

-- | Ask a user to input an integer value mod 256.
-- '&'
sInputInt :: State -> IO (Either StateError State)
sInputInt st = readMaybe . (:[]) <$> getChar >>= \n ->
               case n of
                   Nothing -> return $ Left $ InvalidInputError
                              (maybe ' ' intToChar n) $ case currPointer st of
                                                            Primary -> pLoc st
                                                            _       -> sLoc st
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
fPut st@(State {stack = []   })     = fPut' st 0 0 0
fPut st@(State {stack = [y]  })     = fPut' st y 0 0
fPut st@(State {stack = [y,x]})     = fPut' st y x 0
fPut st@(State {stack = (y:x:v:_)}) = fPut' st y x $ intToWord v

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
fGet st@(State {stack = [] })     = fGet' st 0 0
fGet st@(State {stack = [y]})     = fGet' st y 0
fGet st@(State {stack = (y:x:_)}) = fGet' st y x

-- | Helper function to fGet.
fGet' :: State -> Int32 -> Int32 -> IO State
fGet' st y x = if x > 79 || x < 0 || y > 24 || y < 0
               then return st { stack = drop 2 $ stack st }
               else readArray (playfield st) (intToWord y,intToWord x)
                        >>= \v -> return st { stack = wordToInt v
                                                      : drop 2 (stack st) }

-- -----------------------------------------------------------------------------
-- Errors.

-- | Throws a DivByZeroError.
throwDivByZeroError st@(State{currPointer = Primary}) = Left $ DivByZeroError
                                                        $ pLoc st
throwDivByZeroError st = Left . DivByZeroError . sLoc $ st
