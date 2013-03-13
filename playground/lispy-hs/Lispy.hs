{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, MultiParamTypeClasses,
  ViewPatterns, OverloadedStrings, FlexibleInstances #-}


-- "Compile" means CompileToBytecode

module Lispy (main) where

--import Control.Exception
--import System.Timeout
import System.Environment

import qualified Data.Char as Char
import Data.Text as Text
import qualified Data.Text.IO
--import Data.Ratio
import Data.List as List
import Data.Vector as Vector
import Data.Map.Strict as Map
import Data.Set as Set
import Data.Foldable as Foldable
import Data.Monoid
import Data.Sequence as Seq
--import Data.Maybe
--import Data.Generics.Uniplate.Data
--import Data.Data
--import Data.Attoparsec.Text as P
import qualified Text.Parsec as P
--import Text.Parsec.Text () --for the instance
import Control.Applicative

import Lispy.Types
import Lispy.Parse
import Lispy.CompileToBytecode
import Lispy.Interpret
import Lispy.Show



{-
testEval :: Text -> Text -> IO ()
testEval expr result = do
  try (timeout (evaluate $ do --Maybe
    Right (Program [ast]) <- P.parseOnly parseLispy expr
-}
-- better to work on the stack machine.

--main :: IO ()
--main = print (P.parseOnly parseLispy "abc (d  e 34) (())")

repn :: Int -> (a -> a) -> a -> a
repn 0 f a = a
repn n f a = repn (n-1) f (f a)

run :: Int -> LispyState -> IO ()
run n state
  | n > 300 = putStrLn "Took too long; giving up."
  | otherwise = do
  putStrLn ("\n\n\nStep " List.++ show n)
  putStr (showsStateStack state "")
  run (n+1) (singleStep state)


main :: IO ()
main = do
  args <- getArgs
  let filename = case args of
        [] -> "test.scm"
        [name] -> name
        _ -> error "too many command line arguments"
  inputText <- Data.Text.IO.readFile filename
  let parsed = doParse parseSexp filename inputText
    --"abc (d  e 34) (())"
    --"(lambda (x y z) (x y (x z)))"
  let compiled = case parsed of
                  Right ast ->
                    compile builtinFunctionTextToVarIdx ast
  print parsed
  print compiled

  run 0 (startProgram compiled)

  --putStr (showsStateStack (repn 12 singleStep (startProgram compiled)) "")
  --putStr (showsStateStack (repn 300 singleStep (startProgram compiled)) "")
  --putStr (showProgramBytecode (programBytecode compiled))
