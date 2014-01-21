-- |
-- Module      : Befunge.Parser
-- Copyright   : Joe Jevnik 2013
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- Parsing functions for the befunge interpreter.

module Befunge.Parser where

import Befunge.Data
import Befunge.Operations

import Control.Applicative ((<$>))
import Data.Array.IO       (IOUArray,newArray,writeArray)

-- | Reads a new state from a string.
stateFromSource :: String -> IO State
stateFromSource cs = newArray ((0,0),(24,79)) 32
                     >>= iter 0 0 cs
                     >>= newStateFromArr
  where
      iter _ _ [] arr        = return arr
      iter _ y ('\n':cs) arr = iter 0 (y + 1) cs arr
      iter x y (c:cs) arr    = writeArray arr (y,x) (charToWord c) >>
                               iter (x + 1) y cs arr

-- | Reads a new state from a file.
stateFromFile :: FilePath -> IO State
stateFromFile fl = readFile fl >>= stateFromSource
