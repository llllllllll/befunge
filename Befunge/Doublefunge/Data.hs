-- |
-- Module      : Doublefunge.Data
-- Copyright   : Joe Jevnik 2013
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- All data types for my doublefunge interpreter.

module Befunge.Doublefunge.Data
    ( Word8
    , Int32
    , State(..)
    , showState       -- :: State -> IO String
    , newState        -- :: IO State
    , newStateFromArr -- :: IOUArray (Int,Int) Word8 -> IO State
    , StateError(..)  -- Instances: Show
    , PDirection(..)
    , Pointer(..)
    , wordToChar      -- :: Word8 -> Char
    , charToWord      -- :: Char -> Word8
    , wordToInt       -- :: Word8 -> Int32
    , intToWord       -- :: Int32 -> Word8
    , charToInt       -- :: Char -> Int32
    , intToChar       -- :: Int32 -> Char
    ) where

import Data.Array.IO (IOArray,newArray,getElems)
import Data.Word     (Word8)
import Data.Int      (Int32)
import Unsafe.Coerce (unsafeCoerce)

-- | The program state.
data State = State { pLoc        :: (Word8,Word8)
                   , sLoc        :: (Word8,Word8)
                   , pDir        :: PDirection
                   , sDir        :: PDirection
                   , playfield   :: IOArray (Word8,Word8) Word8
                   , stack       :: [Int32]
                   , pIsString   :: Bool
                   , sIsString   :: Bool
                   , currPointer :: Pointer
                   }

instance Show State where
    show st = show (pLoc st) ++ '\n' : show (sLoc st) ++ '\n' : show (pDir st)
              ++ '\n' : show (sDir st) ++ '\n' : show (stack st)
              ++ '\n' : show (pIsString st) ++ '\n' : show (sIsString st)

-- | Pretty prints a 'State', wrapped in the IO monad due to the IOUArray.
showState :: State -> IO String
showState st = getElems (playfield st)
               >>= \es -> return (show st ++ '\n':insert 80 '\n'
                                           (map wordToChar es))
  where
      insert :: Int -> a -> [a] -> [a]
      insert w y ns = zip ns (cycle [1..w]) >>= (\(m,k) -> if k == w
                                                             then [m,y]
                                                             else [m])

-- | A new 'State' with a playfield of all spaces.
newState :: IO State
newState = newArray ((0,0),(24,79)) 32 >>= newStateFromArr

-- | A new 'State' with a supplied playfield.
newStateFromArr :: IOArray (Word8,Word8) Word8 -> IO State
newStateFromArr arr = return State { pLoc        = (0,0)
                                   , sLoc        = (24,79)
                                   , pDir        = PRight
                                   , sDir        = PLeft
                                   , playfield   = arr
                                   , stack       = []
                                   , pIsString   = False
                                   , sIsString   = False
                                   , currPointer = Primary
                                   }

-- | The various types of errors that can be thrown.
data StateError = DivByZeroError         (Word8,Word8)
                | InvalidInputError Char (Word8,Word8)
                | Terminate -- ^ Represents that the terminating char was hit

-- | A pointer direction.
data PDirection = PUp    -- ^ Indicates that the pointer is going up.
                | PDown  -- ^ Indicates that the pointer is going down.
                | PLeft  -- ^ Indicates that the pointer is going left.
                | PRight -- ^ Indicates that the pointer is going right.
                  deriving Show

-- | The two pointers in play.
data Pointer = Primary   -- ^ The primary pointer, starting at (0,0).
             | Secondary -- ^ The secondary pointer, starting at (24,79).

-- | Converts a 'Word8' to the corrosponding ASCII 'Char'.
wordToChar :: Word8 -> Char
wordToChar = unsafeCoerce

-- | Converts a 'Char' to a 'Word8'.
charToWord :: Char -> Word8
charToWord = toEnum . fromEnum

-- | Converts a 'Word8' to 'Int32'.
wordToInt :: Word8 -> Int32
wordToInt = unsafeCoerce

-- | Converts an 'Int32' to 'Word8' mod 256.
intToWord :: Int32 -> Word8
intToWord = unsafeCoerce . flip mod 256

-- | Converts an 'Int32' to a 'Word8' mod 256.
intToChar :: Int32 -> Char
intToChar = unsafeCoerce . flip mod 256

-- | Converts a 'Char' to an 'Int32'.
charToInt :: Char -> Int32
charToInt = unsafeCoerce
