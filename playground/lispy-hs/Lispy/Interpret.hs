
module Lispy.Interpret (startProgram, singleStep) where

--import Data.Text as Text
import Data.List as List
import Data.Vector as Vector
import Data.Map.Strict as Map
import Data.Set as Set
--import Data.Foldable as Foldable
import Data.Monoid
--import Data.Sequence as Seq
--import Data.Maybe
--import Control.Applicative

import Lispy.Types
import Lispy.Show (showRuntimeValue)

------------------- INTERPRETING -------------------


varargPairs :: [RuntimeValue] -> Maybe [(RuntimeValue, RuntimeValue)]
varargPairs (a:b:rest) = fmap ((a, b) :) (varargPairs rest)
varargPairs [] = Just []
varargPairs (_:[]) = Nothing

--nothingToNil :: Maybe RuntimeValue -> RuntimeValue
maybeRuntimeValue :: (a -> RuntimeValue) -> Maybe a -> RuntimeValue
maybeRuntimeValue _ Nothing = NilValue
maybeRuntimeValue f (Just a) = f a

-- "pure" as in "no side effects"
pureBuiltinFunction :: Builtin -> [RuntimeValue] -> RuntimeValue
pureBuiltinFunction Plus [AtomValue a, AtomValue b] = AtomValue (a + b)
pureBuiltinFunction Minus [AtomValue a, AtomValue b] = AtomValue (a - b)
pureBuiltinFunction Times [AtomValue a, AtomValue b] = AtomValue (a * b)
pureBuiltinFunction Negate [AtomValue a] = AtomValue (negate a)
pureBuiltinFunction LessThan [a, b] = truthValue (a < b)
pureBuiltinFunction LessEqual [a, b] = truthValue (a <= b)
pureBuiltinFunction GreaterThan [a, b] = truthValue (a > b)
pureBuiltinFunction GreaterEqual [a, b] = truthValue (a >= b)
pureBuiltinFunction Equal [a, b] = truthValue (a == b)
pureBuiltinFunction NotEqual [a, b] = truthValue (a /= b)
-- We use the Lua convention of returning the first truthy value
-- that makes the and/or operator have the correct truthiness.
pureBuiltinFunction And [a, b] = if isTruthy a then b else a
pureBuiltinFunction Or [a, b] = if isTruthy a then a else b
pureBuiltinFunction Not [a] = truthValue (not (isTruthy a))
pureBuiltinFunction TableFromSequence vals =
    ImmTableValue (Map.fromList (List.zip (fmap AtomValue [0..]) vals))
pureBuiltinFunction TableFromPairs vals =
  case varargPairs vals of
    Nothing -> error "Odd number of arguments to table-from-pairs"
    Just pairs -> ImmTableValue (Map.fromList pairs)
pureBuiltinFunction TableSize [ImmTableValue m] = AtomValue (Map.size m)
pureBuiltinFunction TableViewKey [ImmTableValue m, k] =
  ImmTableViewValue (createMapIterator k m)
pureBuiltinFunction TableUnView [ImmTableViewValue i] =
  ImmTableValue (mapIteratorGetMap i)
pureBuiltinFunction TableViewMin [ImmTableValue m] =
  maybeRuntimeValue ImmTableViewValue (mapMinIterator m)
pureBuiltinFunction TableViewMax [ImmTableValue m] =
  maybeRuntimeValue ImmTableViewValue (mapMaxIterator m)
pureBuiltinFunction TableViewNext [ImmTableViewValue i] =
  maybeRuntimeValue ImmTableViewValue (mapIteratorNext i)
pureBuiltinFunction TableViewPrev [ImmTableViewValue i] =
  maybeRuntimeValue ImmTableViewValue (mapIteratorPrev i)
pureBuiltinFunction TableViewSet [ImmTableViewValue i, v] =
  ImmTableViewValue (mapIteratorSet i v)
pureBuiltinFunction TableViewDelete [ImmTableViewValue i] =
  ImmTableViewValue (mapIteratorDelete i)
pureBuiltinFunction TableViewGetKey [ImmTableViewValue i] = mapIteratorGetKey i
pureBuiltinFunction TableViewGetValue [ImmTableViewValue i] =
  maybeRuntimeValue id (mapIteratorGetValue i)
pureBuiltinFunction TableViewElemExists [ImmTableViewValue i] =
  truthValue (mapIteratorExists i)
pureBuiltinFunction _ _ =
  error "wrong number of arguments to builtin function (or bug non-function builtin)"

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
      if not (isTruthy (computedValues Map.! cond))
      then goto (instructionPointer+1+dest) state
      else goto (instructionPointer+1) state


dePendValue :: LispyState -> RuntimeValue -> RuntimeValue
dePendValue state (PendingValue idx) =
  case Map.lookup idx (lsPendingValues state) of
    Nothing -> error "Used a value in a letrec before it was defined"
    Just val -> dePendValue state val
dePendValue _ val = val

call :: Maybe VarIdx -> VarIdx -> Vector VarIdx -> LispyState -> LispyState
call resultElseTail func args state = let
  computedValues = (lsfComputedValues (lsFrame (lsStack state)))
  funcVal = computedValues Map.! func
  argVals = fmap (computedValues Map.!) args
  in case dePendValue state funcVal of
    FunctionValue initialFrame params -> let
        argEnv = Map.fromList (Vector.toList (Vector.zip params argVals))
        newStackFrame = initialFrame{lsfComputedValues =
            Map.union argEnv (lsfComputedValues initialFrame)}
      in state{
        lsStack = case resultElseTail of
          Nothing -> --tail call
            LispyStack newStackFrame (lsParent (lsStack state))
          Just result -> --non-tail
            LispyStack newStackFrame (Just (result, lsStack state))
        }
    BuiltinFunctionValue bf ->
      callBuiltinFunction resultElseTail bf args state
    _ -> error "runtime error: calling a non-function"

callBuiltinFunction :: Maybe VarIdx -> Builtin -> Vector VarIdx -> LispyState -> LispyState
callBuiltinFunction resultElseTail bf args state = case bf of
  _ -> let
    computedValues = (lsfComputedValues (lsFrame (lsStack state)))
    val = pureBuiltinFunction bf
      (Vector.toList (fmap (dePendValue state . (computedValues Map.!)) args))
    in case resultElseTail of
      Nothing -> --tail call
        ret val state
      Just result -> --non-tail
        bindValue result val state

updateStackFrame :: (LispyStackFrame -> LispyStackFrame)
                 -> LispyState -> LispyState
updateStackFrame f state = state{ lsStack = let stack = lsStack state in
  stack{ lsFrame = f (lsFrame stack) } }

bindValue :: VarIdx -> RuntimeValue -> LispyState -> LispyState
bindValue result value state = let
  frame = lsFrame (lsStack state)
  computedValues = lsfComputedValues frame
  instructionPointer = lsfInstructionPointer frame
  in case
    Map.insertLookupWithKey (\_ newValue _ -> newValue)
      result value computedValues of
  (oldVal, newComputedValues) -> let
    newState = updateStackFrame (\fr -> fr{
        lsfComputedValues = newComputedValues,
        lsfInstructionPointer = instructionPointer+1
      }) state
    in case oldVal of
      Just (PendingValue pendingValueIdx) ->
        newState{
          lsPendingValues = Map.insert pendingValueIdx value
            (lsPendingValues state)
          }
      _ -> newState

goto :: InstructionPointer -> LispyState -> LispyState
goto absoluteDest state = updateStackFrame (\fr -> fr{
    lsfInstructionPointer = absoluteDest
  }) state

ret :: RuntimeValue -> LispyState -> LispyState
ret retVal state =
  case lsParent (lsStack state) of
    Nothing -> error ("returning from the main program: "
      List.++ showRuntimeValue state retVal)
    Just (retValDest, newStack) ->
      bindValue retValDest retVal (state{lsStack = newStack})
