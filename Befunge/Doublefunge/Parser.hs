-- |
-- Module      : Doublefunge.Parser
-- Copyright   : Joe Jevnik 2013
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- Parsing functions for the doublefunge interpreter.

module Befunge.Doublefunge.Parser where

import Befunge.Doublefunge.Data
import Befunge.Doublefunge.Operations

import Control.Applicative ((<$>))
import Control.Arrow       (second)
import Control.Monad       ((<=<))
import Data.Array.IO       (IOUArray,newArray,writeArray)

-- | Reads a new state from a string.
stateFromSource :: String -> IO State
stateFromSource cs = newArray ((0,0),(24,79)) 32
                     >>= \arr -> iter arr 0 0
                                 (concatMap (extend 80) $ take 25 $ lines cs)
                     >>= newStateFromArr
  where
      extend n cs         = take n $ cs ++ repeat ' '
      iter arr _ _ []     = return arr
      iter arr 80 y cs    = iter arr 0 (y + 1) cs
      iter arr x y (c:cs) = writeArray arr (y,x) (charToWord c)
                            >> iter arr (x + 1) y cs

-- | Reads a new 'State' from a file.
stateFromFile :: FilePath -> IO State
stateFromFile = stateFromSource <=< readFile
