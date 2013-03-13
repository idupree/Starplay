
module Lispy.Show where

--import qualified Data.Char as Char
import Data.Text as Text
import Data.List as List
import Data.Vector as Vector
import Data.Map.Strict as Map
import Data.Set as Set
import Data.Foldable as Foldable
import Data.Monoid
--import Data.Sequence as Seq
--import Data.Maybe
--import Control.Applicative

import Lispy.Types

---------------------- SHOWING ----------------------

instance (Show a) => Show (Located a) where
  showsPrec prec (L _ val) = showsPrec prec val

instance Show AST where
  showsPrec _ (ASTNumber _ num) = shows num
  showsPrec _ (ASTIdentifier _ ident) = showString (Text.unpack ident)
  showsPrec _ (ASTList _ members) =
    if Vector.null members
    then showString "()"
    else
    showChar '(' .
    Vector.foldr1
      (\s rest -> s . showChar ' ' . rest)
      (fmap (shows . unL) members) .
    showChar ')'

instance Show CompiledProgram where
  showsPrec _ (CompiledProgram bytecode (L progSource _) astsByIdx) =
    showString (Text.unpack (sourceText progSource)) .
    showChar '\n' .
    appEndo (foldMap
      (\(bytecodeIdx, (astidx, instr)) -> Endo (
        let
          motherInstructionDesc = showsVarIdx astsByIdx astidx
          bytecodeIdxDesc = shows bytecodeIdx
        in
        showString (List.replicate
          (max 0 (3 - List.length (bytecodeIdxDesc ""))) ' ') .
        shows bytecodeIdx .
        showString "  " .
        motherInstructionDesc .
        showString (List.replicate
          (max 1 (22 - List.length (motherInstructionDesc ""))) ' ') .
        showsBytecodeInstruction astsByIdx bytecodeIdx instr . showChar '\n'
      ))
      (Vector.indexed bytecode))

showsVarIdx :: Vector (Located AST) -> VarIdx -> ShowS
showsVarIdx astsByIdx idx =
  shows idx .
  showChar '(' .
  ( case Map.lookup idx builtinVarIdxToText of
      Just builtinName ->
        showString "builtin " .
        showString (Text.unpack builtinName)
      Nothing ->
        case astsByIdx Vector.! idx of
          L source ast ->showsASTConciseSummary ast .
            showChar ':' . shows (sourceLine (sourceBegin source)) .
            showChar ':' . shows (sourceColumn (sourceBegin source))
  ) .
  showChar ')'

showsASTConciseSummary :: AST -> ShowS
showsASTConciseSummary ast = case ast of
  ASTNumber _ num -> shows num
  ASTIdentifier _ ident -> showString (Text.unpack ident)
  ASTList _ members -> case Vector.headM members of
    Just (L _ ast') -> showChar '(' . showsASTConciseSummary ast' .
      if Vector.length members > 1 then showString "…)" else showChar ')'
    Nothing -> showString "()"

showsBytecodeInstruction :: Vector (Located AST) -> Int -> BytecodeInstruction -> ShowS
showsBytecodeInstruction astsByIdx bytecodeIdx bytecode = let
    var = showsVarIdx astsByIdx
    relDest dest = showChar '+' . shows dest . showChar '(' .
      shows (bytecodeIdx + 1 + dest) . showChar ')'
  in case bytecode of
  CALL result func args ->
    showString "CALL " .
    var result .
    showString " = " . var func .
    appEndo (foldMap (\idx -> Endo (showChar ' ' . var idx)) args)
  TAILCALL func args ->
    showString "TAILCALL " .
    var func .
    appEndo (foldMap (\idx -> Endo (showChar ' ' . var idx)) args)
  LITERAL result value ->
    showString "LITERAL " .
    var result .
    showString " = " .
    shows value
  NAME result origName ->
    showString "NAME " .
    var result .
    showString " = " .
    var origName
  RETURN origName ->
    showString "RETURN " .
    var origName
  MAKE_CLOSURE result vars dest params ->
    showString "MAKE_CLOSURE " .
    var result .
    showString " =" .
    ( if Set.null vars
      then id
      else
      showString " [" .
      List.foldr1
        (\s rest -> s . showChar ' ' . rest)
        (fmap var (Set.toList vars)) .
      showChar ']'
    ) .
    showChar ' ' .
    relDest dest .
    appEndo (foldMap (\idx -> Endo (showChar ' ' . var idx)) params)
  GOTO dest ->
    showString "GOTO " . relDest dest
  GOTO_IF_NOT dest cond ->
    showString "GOTO_IF_NOT " . relDest dest .
    showChar ' ' .
    var cond

{-
showsProgramBytecode :: Vector (ASTIdx, BytecodeInstruction) -> ShowS
showsProgramBytecode = appEndo . foldMap
  (\(_, instr) -> Endo (shows instr . showChar '\n'))
showProgramBytecode :: Vector (ASTIdx, BytecodeInstruction) -> String
showProgramBytecode = flip showsProgramBytecode ""
-}


showsStackFrame :: LispyState -> LispyStackFrame -> ShowS
showsStackFrame state (LispyStackFrame instructionPointer computedValues) =
  let program = lsCompiledProgram state in
  showString "Instruction pointer: " .
  shows instructionPointer .
  let (astidx, code) = (programBytecode program) Vector.! instructionPointer in
  showString "  " .
  showsVarIdx (programASTsByIdx program) astidx .
  showString "  " .
  showsBytecodeInstruction (programASTsByIdx program) instructionPointer code .
  appEndo (foldMap (\(idx, val) -> Endo (
    showString "\n\t" .
    showsVarIdx (programASTsByIdx program) idx .
    showString " = " .
    showsRuntimeValue state val
    )) (Map.toList computedValues)) .
  showChar '\n'

showsStack :: LispyState -> LispyStack -> ShowS
showsStack state (LispyStack frame parent) =
  let program = lsCompiledProgram state in
  showsStackFrame state frame .
  case parent of
    Just (retValDest, nextStack) ->
      showString "  will return value to " .
      showsVarIdx (programASTsByIdx program) retValDest .
      showChar '\n' .
      showsStack state nextStack
    Nothing -> id

showsStateStack :: LispyState -> ShowS
showsStateStack state@(LispyState _ stack _ _) = showsStack state stack

showStateStack :: LispyState -> String
showStateStack ls = showsStateStack ls ""


showsMap :: (k -> ShowS) -> (v -> ShowS) -> Map k v -> ShowS
showsMap kf vf m = case Map.toList m of
  [] -> showString "{}"
  (p1:ps) -> let showPair (k, v) = kf k . showString ": " . vf v in
    showChar '{' .
    showPair p1 .
    appEndo (foldMap (\p -> Endo (showString ", " . showPair p)) ps) .
    showChar '}'

-- How do you serialize a closure?  By its ASTIdx?  And then you can
-- map that back to the code... by choosing the first one that matches
-- that?  Sounds good.  Because we don't trust random pointers into
-- our bytecode, or the compiler is nondeterministic
-- (well, with this bytecode, that'd be harmless, but I gather Lua's is riskier).

showsRuntimeValue :: LispyState -> RuntimeValue -> ShowS
showsRuntimeValue _ NilValue = showString "nil"
showsRuntimeValue _ TrueValue = showString "true"
showsRuntimeValue _ (AtomValue n) = shows n
showsRuntimeValue s (ImmTableValue m) =
  showsMap (showsRuntimeValue s) (showsRuntimeValue s) m
showsRuntimeValue s (ImmTableViewValue i) =
  showsRuntimeValue s (mapIteratorGetKey i) .
  showChar '@' .
  showsMap (showsRuntimeValue s) (showsRuntimeValue s) (mapIteratorGetMap i)
showsRuntimeValue _ (BuiltinFunctionValue bf) =
  showString (Text.unpack (builtinDataToText Map.! bf))
showsRuntimeValue state (FunctionValue
    (LispyStackFrame instructionPointer computedValues) _) =
  let program = lsCompiledProgram state in
  showString "<closure: " .
  shows instructionPointer .
  let (astidx, _) = (programBytecode program) Vector.! instructionPointer in
  showString " (" .
  showsVarIdx (programASTsByIdx program) astidx .
  showString ") " .
  showsMap (showsVarIdx (programASTsByIdx program))
           (showsRuntimeValue state)
           computedValues .
  showChar '>'
showsRuntimeValue state (PendingValue pv) =
  case Map.lookup pv (lsPendingValues state) of
    Nothing -> showString "<pending " . shows pv . showChar '>'
    Just v -> let
        -- avoid emitting infinite text for loops
        sanitizedState = state{lsPendingValues =
          Map.delete pv (lsPendingValues state) }
      in
      showString "<via pending " .
      shows pv .
      showString ": " .
      showsRuntimeValue sanitizedState v .
      showChar '>'

showRuntimeValue :: LispyState -> RuntimeValue -> String
showRuntimeValue p v = showsRuntimeValue p v ""


