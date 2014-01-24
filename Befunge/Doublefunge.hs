-- |
-- Module      : Doublefunge
-- Copyright   : Joe Jevnik 2013
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- A doublefunge interpreter.

module Befunge.Doublefunge
    ( runDoublefunge -- :: [String] -> IO ()
    ) where

import Befunge.Doublefunge.Data
import Befunge.Doublefunge.Operations
import Befunge.Doublefunge.Parser

import Control.Arrow       (first,second)
import Control.Applicative ((<$>))
import Control.Concurrent  (MVar,forkFinally,putMVar,newEmptyMVar)
import Control.Monad       (void,liftM,(<=<))
import Data.Array.MArray   (readArray)
import Data.Char           (isDigit)
import System.Exit         (exitSuccess)
import System.Environment  (getArgs)

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
readNext st@(State{pIsString = True,sIsString = True}) = do
    pc <- readArray (playfield st) (pLoc st)
    sc <- readArray (playfield st) (sLoc st)
    let st' = case (wordToChar pc,wordToChar sc) of
                  ('"','"') -> st { pIsString = False
                                  , sIsString = False }
                  ('"',_)   -> sPush (wordToInt sc) st { pIsString = False }
                  (_,'"')   -> sPush (wordToInt pc) st { sIsString = False }
                  (_,_)     -> sPush (wordToInt sc) $ sPush (wordToInt pc) st
    return $ Right st'
readNext st@(State{pIsString = True}) = do
    pc <- wordToChar <$> readArray (playfield st) (pLoc st)
    sc <- wordToChar <$> readArray (playfield st) (sLoc st)
    if pc == '"'
      then parseCommand sc $ st { pIsString = False
                                , currPointer = Secondary }
      else parseCommand sc $ sPush (charToInt pc) st { currPointer = Secondary }
readNext st@(State{sIsString = True}) = do
    pc <- wordToChar <$> readArray (playfield st) (pLoc st)
    sc <- wordToChar <$> readArray (playfield st) (sLoc st)
    if sc == '"'
      then parseCommand pc $ st { sIsString = False
                                , currPointer = Primary }
      else parseCommand pc st
               >>= (\a -> case a of
                              e@(Left _) -> return e
                              Right st' -> return $ Right $ sPush (charToInt sc)
                                           st' { currPointer = Primary })
readNext st = do
    pc <- readArray (playfield st) (pLoc st)
    sc <- readArray (playfield st) (sLoc st)
    parseCommand (wordToChar pc) (st { currPointer = Primary })
              >>= \st' -> case st' of
                              e@(Left _) -> return e
                              Right s -> parseCommand (wordToChar sc)
                                         (s { currPointer = Secondary })

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
parseCommand '_' = return . Right . pHorzIf
parseCommand '|' = return . Right . pVertIf
parseCommand '"' = return . Right
                   . \st -> case currPointer st of
                                Primary -> st { pIsString = True }
                                _       -> st { sIsString = True }
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
    | otherwise  = return . Left  . InvalidInputError n
                   . \st -> case currPointer st of
                                Primary -> pLoc st
                                _       -> sLoc st

-- | Increments the pointers of the 'State' based on the 'Direction's.
-- Accounts for wrapping.
incPointer :: Either StateError State -> Either StateError State
incPointer e@(Left _) = e
incPointer (Right st) = Right . incPointerP . incPointerS $ st

-- | Increments the 'Primary' pointer.
incPointerP :: State -> State
incPointerP st@(State {pDir = PUp,pLoc = (0,c)}) =
    st { pLoc = (24,c) }
incPointerP st@(State {pDir = PUp}) =
    st { pLoc = first (flip (-) 1) $ pLoc st }
incPointerP st@(State {pDir = PDown,pLoc = (24,c)}) =
    st { pLoc = (0,c) }
incPointerP st@(State {pDir = PDown}) =
    st { pLoc = first (+ 1) $ pLoc st }
incPointerP st@(State {pDir = PLeft,pLoc = (r,0)}) =
    st { pLoc = (r,79) }
incPointerP st@(State {pDir = PLeft}) =
    st { pLoc = second (flip (-) 1) $ pLoc st }
incPointerP st@(State {pDir = PRight,pLoc = (r,79)}) =
    st { pLoc = (r,0) }
incPointerP st@(State {pDir = PRight}) =
    st { pLoc = second (+ 1) $ pLoc st }

-- | Increments the 'Secondary' pointer.
incPointerS :: State -> State
incPointerS st@(State {sDir = PUp,sLoc = (0,c)}) =
    st { sLoc = (24,c) }
incPointerS st@(State {sDir = PUp}) =
    st { sLoc = first (flip (-) 1) $ sLoc st }
incPointerS st@(State {sDir = PDown,sLoc = (24,c)}) =
    st { sLoc = (0,c) }
incPointerS st@(State {sDir = PDown}) =
    st { sLoc = first (+ 1) $ sLoc st }
incPointerS st@(State {sDir = PLeft,sLoc = (r,0)}) =
    st { sLoc = (r,79) }
incPointerS st@(State {sDir = PLeft}) =
    st { sLoc = second (flip (-) 1) $ sLoc st }
incPointerS st@(State {sDir = PRight,sLoc = (r,79)}) =
    st { sLoc = (r,0) }
incPointerS st@(State {sDir = PRight}) =
    st { sLoc = second (+ 1) $ sLoc st }

runDoublefunge :: [String] -> IO ()
runDoublefunge []        = error "runDoublefunge recieved []"
runDoublefunge [p]       = void $ stateFromFile p >>= readAll . Right
runDoublefunge ("-c":ps) = mapM_ forkDoublefunge ps
runDoublefunge ps        = mapM_ (readAll . Right <=< stateFromFile) ps

-- | Forks a befunge program into its own thread using forkFinally and MVar ()
-- writing to hold the main thread open until all the children have terminated.
forkDoublefunge :: FilePath -> IO (MVar ())
forkDoublefunge b = do
    lock <- newEmptyMVar
    forkFinally (readAll . Right <=< stateFromFile $ b)
                    (const $ putMVar lock ())
    return lock
