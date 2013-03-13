{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, MultiParamTypeClasses,
  ViewPatterns, OverloadedStrings, FlexibleInstances #-}

module Lispy.Interpret (startProgram, singleStep) where

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
--import MonadLib as M

import Lispy.Types

------------------- INTERPRETING -------------------

-- "pure" as in "no side effects"
pureBuiltinFunction :: BuiltinFunction -> [RuntimeValue] -> RuntimeValue
pureBuiltinFunction Plus [AtomValue a, AtomValue b] = AtomValue (a + b)
pureBuiltinFunction Minus [AtomValue a, AtomValue b] = AtomValue (a - b)
pureBuiltinFunction Times [AtomValue a, AtomValue b] = AtomValue (a * b)
pureBuiltinFunction Negate [AtomValue a] = AtomValue (negate a)
pureBuiltinFunction LessThan [AtomValue a, AtomValue b] = AtomValue (toIntBool (a < b))
pureBuiltinFunction LessEqual [AtomValue a, AtomValue b] = AtomValue (toIntBool (a <= b))
pureBuiltinFunction GreaterThan [AtomValue a, AtomValue b] = AtomValue (toIntBool (a > b))
pureBuiltinFunction GreaterEqual [AtomValue a, AtomValue b] = AtomValue (toIntBool (a >= b))
pureBuiltinFunction Equal [AtomValue a, AtomValue b] = AtomValue (toIntBool (a == b))
pureBuiltinFunction NotEqual [AtomValue a, AtomValue b] = AtomValue (toIntBool (a /= b))
pureBuiltinFunction And [AtomValue a, AtomValue b] = AtomValue (toIntBool (fromIntBool a && fromIntBool b))
pureBuiltinFunction Or [AtomValue a, AtomValue b] = AtomValue (toIntBool (fromIntBool a || fromIntBool b))
pureBuiltinFunction Not [AtomValue a] = AtomValue (toIntBool (not (fromIntBool a)))
pureBuiltinFunction _ _ = error "wrong number of arguments to builtin function"

startProgram :: CompiledProgram -> LispyState
startProgram program = LispyState
  program
  (LispyStack (LispyStackFrame 0 builtinsComputedValues) Nothing)
  mempty
  0

singleStep :: LispyState -> LispyState
singleStep state@(LispyState
              program@(CompiledProgram bytecode _ _)
              stack@(LispyStack
                frame@(LispyStackFrame instructionPointer computedValues)
                parent)
              pendingValues
              nextPendingValueIdx) =
  let
    dePendValue :: RuntimeValue -> RuntimeValue
    dePendValue (PendingValue idx) = case Map.lookup idx pendingValues of
      Nothing -> error "Used a value in a letrec before it was defined"
      Just val -> dePendValue val
    dePendValue val = val

    call :: Maybe VarIdx -> VarIdx -> Vector VarIdx -> LispyState -> LispyState
    call resultElseTail func args stat = let
      computedValues = (lsfComputedValues (lsFrame (lsStack stat)))
      funcVal = computedValues Map.! func
      argVals = fmap (computedValues Map.!) args
      in case dePendValue funcVal of
        FunctionValue initialFrame params -> let
            argEnv = Map.fromList (Vector.toList (Vector.zip params argVals))
            newStackFrame = initialFrame{lsfComputedValues =
                Map.union argEnv (lsfComputedValues initialFrame)}
          in stat{
            lsStack = case resultElseTail of
              Nothing -> --tail call
                LispyStack newStackFrame (lsParent (lsStack stat))
              Just result -> --non-tail
                LispyStack newStackFrame (Just (result, lsStack stat))
            }
        BuiltinFunctionValue bf ->
          callBuiltinFunction resultElseTail bf args stat
        _ -> error "runtime error: calling a non-function"

    callBuiltinFunction :: Maybe VarIdx -> BuiltinFunction -> Vector VarIdx -> LispyState -> LispyState
    callBuiltinFunction resultElseTail bf args stat = case bf of
      _ -> let
        computedValues = (lsfComputedValues (lsFrame (lsStack stat)))
        val = pureBuiltinFunction bf
          (Vector.toList (fmap (dePendValue . (computedValues Map.!)) args))
        in case resultElseTail of
          Nothing -> --tail call
            ret val stat
          Just result -> --non-tail
            bindValue result val stat

    updateStackFrame :: (LispyStackFrame -> LispyStackFrame)
                     -> LispyState -> LispyState
    updateStackFrame f stat = stat{ lsStack = let stac = lsStack stat in
      stac{ lsFrame = f (lsFrame stac) } }

    bindValue :: VarIdx -> RuntimeValue -> LispyState -> LispyState
    bindValue result value stat = let
      frame = lsFrame (lsStack stat)
      computedValues = lsfComputedValues frame
      instructionPointer = lsfInstructionPointer frame
      in case
        Map.insertLookupWithKey (\_ newValue _ -> newValue)
          result value computedValues of
      (oldVal, newComputedValues) -> let
        newState = updateStackFrame (\fr -> fr{
            lsfComputedValues = newComputedValues,
            lsfInstructionPointer = instructionPointer+1
          }) stat
        in case oldVal of
          Just (PendingValue pendingValueIdx) ->
            newState{
              lsPendingValues = Map.insert pendingValueIdx value pendingValues }
          _ -> newState

    goto :: InstructionPointer -> LispyState -> LispyState
    goto absoluteDest st = updateStackFrame (\fr -> fr{
        lsfInstructionPointer = absoluteDest
      }) st

    ret :: RuntimeValue -> LispyState -> LispyState
    ret retVal stat =
      case lsParent (lsStack stat) of
        Nothing -> error ("returning from the main program: "
                         List.++ show retVal)
        Just (retValDest, newStack) ->
          bindValue retValDest retVal (state{lsStack = newStack})
  in
  case snd (bytecode Vector.! instructionPointer) of
    CALL result func args -> call (Just result) func args state
    TAILCALL func args -> call Nothing func args state
    LITERAL result value -> bindValue result (AtomValue value) state
    NAME result origName -> bindValue result (computedValues Map.! origName) state
    RETURN origName -> ret (computedValues Map.! origName) state
    MAKE_CLOSURE result vars dest params -> case
      Set.foldl'
        (\(nextPending, newComputedValues, varsInClosure)
          varInClosure ->
           case Map.lookup varInClosure computedValues of
             Just val -> (nextPending, newComputedValues,
               Map.insert varInClosure val varsInClosure)
             Nothing -> let
               newPending = PendingValue nextPending
               nextPending' = nextPending+1
               newComputedValues' = Map.insert varInClosure
                 newPending newComputedValues
               in (nextPending', newComputedValues',
               Map.insert varInClosure newPending varsInClosure))
        (nextPendingValueIdx, computedValues, mempty)
        vars
      of (nextPending, newComputedValues, varsInClosure) -> let
            value = FunctionValue
              (LispyStackFrame (instructionPointer+1+dest) varsInClosure)
              params
            -- Note: the bindValue happens *after* putting in
            -- newComputedValues.  This lets bindValue detect
            -- whether the current closure is pending (self-recursive
            -- functions) so that it will define it in pendingValues.
            in bindValue result value
              (updateStackFrame (\fr ->
                fr{ lsfComputedValues = newComputedValues }) state
              ){
                lsNextPendingValueIdx = nextPending
              }
    GOTO dest -> goto (instructionPointer+1+dest) state
    GOTO_IF_NOT dest cond ->
      --TODO bool type rather than "zero is false"
      if (computedValues Map.! cond) == AtomValue 0
      then goto (instructionPointer+1+dest) state
      else goto (instructionPointer+1) state

