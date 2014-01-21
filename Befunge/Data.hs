-- |
-- Module      : Befunge.Data
-- Copyright   : Joe Jevnik 2013
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- All data types for my befunge-93 interpreter.

module Befunge.Data
    ( Word8
    , State(..)
    , newState        -- :: IO State
    , newStateFromArr -- :: IOUArray (Int,Int) Word8 -> IO State
    , StateError(..)  -- Instances: Show
    , PDirection(..)
    , wordToChar      -- :: Word8 -> Char
    , charToWord      -- :: Char -> Word8
    ) where

import Data.Array.IO (IOArray,newArray)
import Data.Word     (Word8)
import Unsafe.Coerce

-- | The program state.
data State = State { loc       :: (Word8,Word8)
                   , dir       :: PDirection
                   , playfield :: IOArray (Word8,Word8) Word8
                   , stack     :: [Word8]
                   , isString  :: Bool
                   }

instance Show State where
    show st = show (loc st) ++ "\n" ++ show (dir st) ++ "\n" ++ show (stack st)
              ++ "\n" ++ show (isString st)

-- | A new state with a playfield of all spaces.
newState :: IO State
newState = newArray ((0,0),(24,79)) 32  >>= \arr ->
           return State { loc       = (0,0)
                        , dir       = PRight
                        , playfield = arr
                        , stack     = []
                        , isString  = False
                        }

-- | A new state with a supplied playfield.
newStateFromArr :: IOArray (Word8,Word8) Word8 -> IO State
newStateFromArr arr = return State { loc       = (0,0)
                                   , dir       = PRight
                                   , playfield = arr
                                   , stack     = []
                                   , isString  = False
                                   }

-- | The various types of errors that can be thrown.
data StateError = DivByZeroError   (Word8,Word8)
                | OutOfBoundsError (Word8,Word8) (Word8,Word8)
                | InvalidInputError Char (Word8,Word8) deriving Show

-- | A pointer direction.
data PDirection = PUp | PDown | PLeft | PRight deriving Show

-- | Converts a Word8 to the corrosponding ASCII Char.
wordToChar :: Word8 -> Char
wordToChar = unsafeCoerce

-- | Converts a Char to a Word8.
charToWord :: Char -> Word8
charToWord = toEnum . fromEnum
