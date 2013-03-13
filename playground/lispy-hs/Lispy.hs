
module Lispy (main) where

--import Control.Exception
--import System.Timeout
import System.Environment

--import qualified Data.Char as Char
--import Data.Text as Text
import qualified Data.Text.IO
--import Data.Ratio
import Data.List as List
--import Data.Vector as Vector
--import Data.Map.Strict as Map
--import Data.Set as Set
--import Data.Foldable as Foldable
--import Data.Monoid as Monoid
--import Data.Sequence as Seq
--import Data.Maybe
--import Control.Applicative

import Lispy.Types
import Lispy.Parse
import Lispy.CompileToBytecode
import Lispy.Interpret
import Lispy.Show

repn :: Int -> (a -> a) -> a -> a
repn 0 _ a = a
repn n f a = repn (n-1) f (f a)

run :: Int -> LispyState -> IO ()
run n state
  | n > 300 = putStrLn "Took too long; giving up."
  | otherwise = do
  putStrLn ("\n\n\nStep " List.++ show n)
  putStr (showStateStack state)
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

  --putStr (showStateStack (repn 12 singleStep (startProgram compiled)))
  --putStr (showStateStack (repn 300 singleStep (startProgram compiled)))

