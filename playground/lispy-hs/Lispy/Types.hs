{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, MultiParamTypeClasses,
  ViewPatterns, OverloadedStrings, FlexibleInstances #-}

module Lispy.Types (module Lispy.Types, module Text.Parsec) where

import Data.Text as Text
--import Data.List as List
import Data.Vector as Vector
import Data.Map.Strict as Map
import Data.Set as Set
--import Data.Foldable as Foldable
--import Data.Monoid as Monoid
--import Data.Sequence as Seq
--import Data.Maybe
--import Control.Applicative

import Text.Parsec (SourcePos, sourceLine, sourceColumn)


-- |
-- bcResultName is a binding for the result of the call, permitting
-- later code to use the result value.
--
-- bcRelativeDestination is relative to the next instruction, e.g.
-- "GOTO 0" is equivalent to a NOP.  For MAKE_CLOSURE, it points to
-- the first instruction of the created function.
data BytecodeInstruction
  = CALL { bcResultName :: VarIdx,
               bcFunc :: VarIdx, bcArgs :: Vector VarIdx }
  | TAILCALL { bcFunc :: VarIdx, bcArgs :: Vector VarIdx }
  | LITERAL { bcResultName :: VarIdx, bcLiteralValue :: AtomicValue }
  | NAME { bcResultName :: VarIdx, bcOriginalName :: VarIdx }
  | RETURN { bcOriginalName :: VarIdx }
  | MAKE_CLOSURE { bcResultName :: VarIdx, bcVarsInClosure :: Set VarIdx,
                   bcRelativeDestination :: Int, bcParams :: Vector VarIdx }
  | GOTO { bcRelativeDestination :: Int }
  | GOTO_IF_NOT { bcRelativeDestination :: Int, bcConditionName :: VarIdx }
  --UNTIL
  deriving (Eq, Ord, Show)
type VarIdx = ASTIdx
type AtomicValue = LispyNum
type LispyNum = Int

type ASTIdx = Int
data SourceLocInfo = SourceLocInfo
  { sourceText :: !Text
  , sourceBegin, sourceEnd :: !Text.Parsec.SourcePos
  }
  deriving (Eq, Ord, Show)
data Located a = L { sourceLocInfo :: !SourceLocInfo, unL :: !a }
  deriving (Eq, Ord, Functor)
data AST
  = ASTNumber { astIdx :: !ASTIdx, astNumber :: !LispyNum }
  | ASTIdentifier { astIdx :: !ASTIdx, astIdentifier :: !Text }
  | ASTList { astIdx :: !ASTIdx, astList :: !(Vector (Located AST)) }
lASTIdx :: Located AST -> ASTIdx
lASTIdx (L _ ast) = astIdx ast

data CompiledProgram = CompiledProgram
  { programBytecode :: !(Vector (ASTIdx, BytecodeInstruction))
  , programAST :: !(Located AST)
  , programASTsByIdx :: !(Vector (Located AST))
  }

-- | Used in CompileToBytecode.hs
data CompileScope = CompileScope
  { compileScopeEnv :: Map Text VarIdx
  -- | when compileScopeIsTailPosition, the returned code
  -- is expected to return or tailcall, never to let the
  -- end of the returned code-block be reached.
  , compileScopeIsTailPosition :: Bool   --Maybe ASTIdx
  }


-- | Builtins: used in main and in Interpret.hs

-- Every builtin function must have worst-case running time
-- that is very short.  Constant or log(n) time are generally
-- acceptable.
data BuiltinFunction = Plus | Minus | Times | Negate
  | LessThan | LessEqual | GreaterThan | GreaterEqual
  | Equal | NotEqual | And | Or | Not
  deriving (Eq, Ord, Bounded, Enum, Show, Read)

-- TODO something less arbitrary than just giving each builtin
-- a high index that probably won't conflict with things.
builtinFunctionVarIdxs :: [(VarIdx, BuiltinFunction)]
builtinFunctionVarIdxs = fmap (\bf -> (negate (fromEnum bf + 40000000), bf))
                                      [minBound..maxBound]
builtinsComputedValues :: StackFrameComputedValues
builtinsComputedValues = fmap BuiltinFunctionValue
                              (Map.fromList builtinFunctionVarIdxs)
builtinFunctionNames :: [(Text, BuiltinFunction)]
builtinFunctionNames =
  [("+", Plus)
  ,("-", Minus)
  ,("*", Times)
  ,("negate", Negate)
  ,("<", LessThan)
  ,("<=", LessEqual)
  ,(">", GreaterThan)
  ,(">=", GreaterEqual)
  ,("=", Equal)
  ,("not=", NotEqual)
  ,("and", And)
  ,("or", Or)
  ,("not", Not)
  ]
builtinFunctionVarIdxToData :: Map VarIdx BuiltinFunction
builtinFunctionVarIdxToData = Map.fromList builtinFunctionVarIdxs
builtinFunctionDataToVarIdx :: Map BuiltinFunction VarIdx
builtinFunctionDataToVarIdx = Map.fromList
                      (fmap (\(x,y)->(y,x)) builtinFunctionVarIdxs)
builtinFunctionTextToVarIdx :: Map Text VarIdx
builtinFunctionTextToVarIdx = fmap (builtinFunctionDataToVarIdx Map.!)
                                (Map.fromList builtinFunctionNames)
builtinFunctionVarIdxToText :: Map VarIdx Text
builtinFunctionVarIdxToText =
  Map.fromList (fmap (\(x,y)->(y,x))
    (Map.toList builtinFunctionTextToVarIdx))


-- | Since we only have one value type in our script language currently
-- (ints), we allow boolean operators by having them operate on ints.
toIntBool :: Bool -> Int
toIntBool True = 1
toIntBool False = 0

fromIntBool :: Int -> Bool
fromIntBool 0 = False
fromIntBool _ = True

-- | Used in Interpret.hs
type StackFrameComputedValues = Map VarIdx RuntimeValue
type InstructionPointer = Int
type PendingValueIdx = Int

data RuntimeValue
  = AtomValue AtomicValue
  | BuiltinFunctionValue BuiltinFunction
  -- | The instruction pointer points to the beginning of the
  -- function, and the computed values are anything in the
  -- function's closure.
  | FunctionValue LispyStackFrame (Vector VarIdx {-params-})
  -- | This is created when a closure in a letrec
  -- closes over a value that has not yet finished being
  -- defined/evaluated.
  -- TODO investigate compiling letrecs to a clever use
  -- of the Y combinator instead (requiring no runtime
  -- support, but making recursive references be values
  -- with different identities than the function they refer to).
  -- I think I'd use Tarjan's SCC-finding algorithm.
  -- For mutual recursion, I think I'd pass to the Y combinator
  -- a function that takes an int parameter "which function are we
  -- recursing to".
  | PendingValue PendingValueIdx
  deriving (Eq, Ord, Show)

data LispyStackFrame = LispyStackFrame
  { lsfInstructionPointer :: InstructionPointer
  , lsfComputedValues :: StackFrameComputedValues
  }
  deriving (Eq, Ord, Show)

data LispyStack = LispyStack
  { lsFrame :: LispyStackFrame
  , lsParent :: Maybe (VarIdx, LispyStack)
  }
  deriving (Eq, Ord, Show)

data LispyState = LispyState
  { lsCompiledProgram :: CompiledProgram
  , lsStack :: LispyStack
  -- | Values are never removed from this map (unless we prove
  -- that there is no longer a PendingValue value with that index
  -- sitting around anywhere).
  , lsPendingValues :: Map PendingValueIdx RuntimeValue
  , lsNextPendingValueIdx :: PendingValueIdx
  }






