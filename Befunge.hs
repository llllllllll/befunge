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

{-# LANGUAGE CPP #-}

module Main where

import Befunge.Doublefunge (runDoublefunge)
import Befunge.Data
import Befunge.Operations
import Befunge.Parser

import Control.Arrow      (first,second)
import Control.Concurrent (MVar,forkFinally,putMVar,newEmptyMVar)
import Control.Monad      (void,liftM,(<=<))
import Data.Array.MArray  (readArray)
import Data.Char          (isDigit)
import System.Exit        (exitSuccess)
import System.Environment (getArgs)


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
readNext st@(State{isString = True}) = readArray (playfield st) (loc st)
                                       >>= \c -> return $ Right
                                                 $ if c == charToWord '"'
                                                     then st {isString = False}
                                                     else sPush (wordToInt c) st
readNext st = readArray (playfield st) (loc st)
              >>= \c -> parseCommand (wordToChar c) st

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
parseCommand '"' = return . Right . (\st -> st { isString = True })
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
    | otherwise  = return . Left  . InvalidInputError n . loc

-- | Increments the pointer of the 'State' based on the 'Direction'.
-- Accounts for wrapping.
incPointer :: Either StateError State -> Either StateError State
incPointer e@(Left _) = e
incPointer (Right st@(State {dir = PUp,loc = (0,c)})) =
    Right st { loc = (24,c) }
incPointer (Right st@(State {dir = PUp})) =
    Right st { loc = first (flip (-) 1) $ loc st }
incPointer (Right st@(State {dir = PDown,loc = (24,c)})) =
    Right st { loc = (0,c) }
incPointer (Right st@(State {dir = PDown})) =
    Right st { loc = first (+ 1) $ loc st }
incPointer (Right st@(State {dir = PLeft,loc = (r,0)})) =
    Right st { loc = (r,79) }
incPointer (Right st@(State {dir = PLeft})) =
    Right st { loc = second (flip (-) 1) $ loc st }
incPointer (Right st@(State {dir = PRight,loc = (r,79)})) =
    Right st { loc = (r,0) }
incPointer (Right st@(State {dir = PRight})) =
    Right st { loc = second (+ 1) $ loc st }

main :: IO ()
main = getArgs >>= runBefunge

-- | Parses the args.
runBefunge :: [String] -> IO ()
runBefunge [] = putStrLn "Usage: runbefunge [FILE]"
runBefunge [p]
    | p `elem` ["-h","--help"]    = putStrLn helpString
    | p `elem` ["-v","--version"] = putStrLn versionString
    | otherwise = void $ stateFromFile p >>= readAll . Right
runBefunge as
    | all (`notElem` ["-cd","-dc","-c","-d"]) as
        = mapM_ (readAll . Right <=< stateFromFile) as
    | all (`notElem` ["-cd","-dc","-d"]) as && "-c" `elem` as
        = mapM_ forkBefunge as
    | any (`elem` ["-cd","-dc"]) as = runDoublefunge $ "-c"
                                      : filter (`notElem` ["-cd","-dc"]) as
    | "-d" `elem` as = runDoublefunge $ filter (/= "-d") as

-- | Forks a befunge program into its own thread using forkFinally and MVar ()
-- writing to hold the main thread open until all the children have terminated.
forkBefunge :: FilePath -> IO (MVar ())
forkBefunge b = do
    lock <- newEmptyMVar
    forkFinally (readAll . Right <=< stateFromFile $ b)
                    (const $ putMVar lock ())
    return lock

-- | The 'String' to be printed when the user invokes '-v' or '--version'.
versionString :: String
versionString =
    "runbefunge 1.0.0 (24.1.2013)\n\
Copyright (C) 2013 Joe Jevnik.\n\
This is free software; see the source for copying conditions.  There is NO\n\
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE."

-- | The 'String' to be printed when the user invokes '-h' or '--help'.
helpString :: String
helpString =
    "Usage:\n\n\
    runbefunge [OPTION]... [SOURCE-FILE]...\n\n\
To run a befunge-93 program, run the interpreter like so:\n\n\
    runbefunge my_prog.bf\n\n\
where my_prog.bf is the path to the program you wish to run.\n\n\
Alternativly, you may run multiple files at once by passing them in the order\n\
in which you would like them to be evaluated, for example:\n\n\
    runbefunge prog1.bf prog2.bf\n\n\
would run prog1.bf until its null character is reached and then begin to run\n\
prog2.bf. For this reason, it will not evaluate any programs after reaching a\n\
program that does not contain a '@' (terminating character).\n\n\
One can also run a set of befunge-93 programs concurrently by passing the '-c'\
\nflag before a list like so:\n\n\
    runbefunge -c prog1.bf prog2.bf\n\n\
WARNING: You may get butchered output as they will both print to stdout\n\
together; however, only one will consume from stdin.\n\n\
This interpreter also will interpret doublefunge when passed the '-d' flag.\
For\nexample:\n\n\
    runbefunge -d my_prog.df\n\n\
will interpret my_prog.df. Doublefunge is like befunge, but also has a second\n\
instruction pointer that starts at (24,79) pointing left. This is called the\n\
secondary pointer. The primary pointer always executes its command first, \
before\nthe secondary, but otherwise behaves the same as the primary pointer. \
Each\npointer has a string mode that is independant of the other, meaning that \
if the\nprimary pointer enters string mode, the secondary pointer will still \
read its\ncommands normally, unless of course the secondary pointer is also in \
string\nmode."
