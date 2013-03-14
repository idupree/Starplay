{-# LANGUAGE DeriveFunctor, OverloadedStrings #-}

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
import Data.Int

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
  | LITERAL { bcResultName :: VarIdx, bcLiteralValue :: LispyNum }
  | NAME { bcResultName :: VarIdx, bcOriginalName :: VarIdx }
  | RETURN { bcOriginalName :: VarIdx }
  | MAKE_CLOSURE { bcResultName :: VarIdx, bcVarsInClosure :: Set VarIdx,
                   bcRelativeDestination :: Int, bcParams :: Vector VarIdx }
  | GOTO { bcRelativeDestination :: Int }
  | GOTO_IF_NOT { bcRelativeDestination :: Int, bcConditionName :: VarIdx }
  --UNTIL
  deriving (Eq, Ord, Show)
type VarIdx = ASTIdx

-- Currently we ignore int overflow and just let the numbers twos-complement
-- wrap.  Luckily, in Haskell, twos-complement sized int types are
-- modulo rather than C's unspecified behavior on signed overflow:
-- http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-Int.html
type LispyNum = Int64

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
--
-- TableFromSequence and TableFromPairs can take O(the number
-- of arguments you pass).  Which is a compile-time constant.
-- Function calling also takes O(number of arguments).  Probably
-- we should just set an arbitrary limit on max number of arguments,
-- like 32 or 100.
data Builtin = Plus | Minus | Times | Negate
  | LessThan | LessEqual | GreaterThan | GreaterEqual
  | Equal | NotEqual | And | Or | Not
  -- Currently, Nil is false and every other value is true.
  -- Currently, even builtins like "nil" that should be builtin
  -- constants are functions instead
  | Nil | Truth
  | EmptyTable
  | TableFromSequence -- ^ (table-from-seq a b c) gives table {0:a, 1:b, 2:c}
  | TableFromPairs -- ^ (table-from-pairs 4 a 2 b 1 c) gives table {1:c, 2:b, 4:a}
  | TableSize -- ^ table -> number
  | TableViewKey -- ^ table, key -> iterator
  | TableUnView -- ^ iterator -> table
  | TableViewMin -- ^ table -> maybe iterator
  | TableViewMax -- ^ table -> maybe iterator
  | TableViewNext -- ^ iterator -> maybe iterator
  | TableViewPrev -- ^ iterator -> maybe iterator
  | TableViewSet -- ^ iterator -> value -> iterator
  | TableViewDelete -- ^ iterator -> iterator
  | TableViewGetKey -- ^ iterator -> value
  | TableViewGetValue -- ^ iterator -> maybe value
  | TableViewElemExists -- ^ iterator -> boolean
  -- Data.Map.split is the only log(n) function we can't create
  -- out of the above building blocks and keep it log(n), but
  -- how useful is it?  Why doesn't Data.Map have a join function
  -- that can re-join after a split in log(n) (or does union
  -- have that behavior and it's just not documented)?
  deriving (Eq, Ord, Bounded, Enum, Show, Read)

-- TODO something less arbitrary than just giving each builtin
-- a high index that probably won't conflict with things.
builtinVarIdxs :: [(VarIdx, Builtin)]
builtinVarIdxs = fmap (\bf -> (negate (fromEnum bf + 40000000), bf))
                                      [minBound..maxBound]
builtinNames :: [(Text, Builtin)]
builtinNames =
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
  ,("nil", Nil)
  ,("true", Truth)
  ,("empty-table", EmptyTable)
  ,("table-sequence", TableFromSequence)
  ,("table", TableFromPairs)
  ,("table-size", TableSize)
  ,("table-view-key", TableViewKey)
  ,("table-unview", TableUnView)
  ,("table-view-min", TableViewMin)
  ,("table-view-max", TableViewMax)
  ,("table-view-next", TableViewNext)
  ,("table-view-prev", TableViewPrev)
  ,("table-view-set", TableViewSet)
  ,("table-view-delete", TableViewDelete)
  ,("table-view-get-key", TableViewGetKey)
  ,("table-view-get-value", TableViewGetValue)
  ,("table-view-elem-exists", TableViewElemExists)
  ]
builtinVarIdxToData :: Map VarIdx Builtin
builtinVarIdxToData = Map.fromList builtinVarIdxs
builtinDataToVarIdx :: Map Builtin VarIdx
builtinDataToVarIdx = Map.fromList (fmap (\(x,y)->(y,x)) builtinVarIdxs)
builtinTextToData :: Map Text Builtin
builtinTextToData = Map.fromList builtinNames
builtinDataToText :: Map Builtin Text
builtinDataToText = Map.fromList (fmap (\(x,y)->(y,x)) builtinNames)
builtinTextToVarIdx :: Map Text VarIdx
builtinTextToVarIdx = fmap (builtinDataToVarIdx Map.!) (Map.fromList builtinNames)
builtinVarIdxToText :: Map VarIdx Text
builtinVarIdxToText = Map.fromList (fmap (\(x,y)->(y,x)) (Map.toList builtinTextToVarIdx))


builtinsComputedValues :: StackFrameComputedValues
builtinsComputedValues = fmap b (Map.fromList builtinVarIdxs)
  where
    b Nil = NilValue
    b Truth = TrueValue
    b EmptyTable = ImmTableValue Map.empty
    b f = BuiltinFunctionValue f

-- | Used in Interpret.hs
type StackFrameComputedValues = Map VarIdx RuntimeValue
type InstructionPointer = Int
type PendingValueIdx = Int


isTruthy :: RuntimeValue -> Bool
isTruthy NilValue = False
isTruthy (PendingValue _) = error "bug: isTruthy on PendingValue"
isTruthy _ = True
truthValue :: Bool -> RuntimeValue
truthValue False = NilValue
truthValue True = TrueValue

-- | Haskell's Map API does not support all the O(1)* iteration operations
-- that such a tree can support, but O(log n) is close enough that we can
-- pretend.
--
-- The iterator can point to a nonexistent key, to allow insertion there.
-- Should this be the case?
--
-- Iterator equality/ordering is unspecified but deterministic
-- between iterators into different maps. TODO is this the best choice?
--
-- (*) actually amortized O(1), worst-case O(log n), I believe.
data MapIterator k v = MapIterator !k !(Map k v) deriving (Show)

instance (Eq k) => Eq (MapIterator k v) where
  (MapIterator k1 _) == (MapIterator k2 _) = (k1 == k2)
instance (Ord k) => Ord (MapIterator k v) where
  compare (MapIterator k1 _) (MapIterator k2 _) = compare k1 k2

createMapIterator :: (Ord k) => k -> Map k v -> MapIterator k v
createMapIterator k m = MapIterator k m

mapIteratorGetMap :: (Ord k) => MapIterator k v -> Map k v
mapIteratorGetMap (MapIterator _ m) = m

mapMinIterator :: (Ord k) => Map k v -> Maybe (MapIterator k v)
mapMinIterator m =
  fmap (\((k, _), _) -> MapIterator k m) (Map.minViewWithKey m)

mapMaxIterator :: (Ord k) => Map k v -> Maybe (MapIterator k v)
mapMaxIterator m =
  fmap (\((k, _), _) -> MapIterator k m) (Map.maxViewWithKey m)

mapIteratorNext :: (Ord k) => MapIterator k v -> Maybe (MapIterator k v)
mapIteratorNext (MapIterator k m) =
  fmap (\(k', _) -> MapIterator k' m) (Map.lookupGT k m)

mapIteratorPrev :: (Ord k) => MapIterator k v -> Maybe (MapIterator k v)
mapIteratorPrev (MapIterator k m) =
  fmap (\(k', _) -> MapIterator k' m) (Map.lookupLT k m)

mapIteratorSet :: (Ord k) => MapIterator k v -> v -> MapIterator k v
mapIteratorSet (MapIterator k m) v =
  (MapIterator k (Map.insert k v m))

mapIteratorDelete :: (Ord k) => MapIterator k v -> MapIterator k v
mapIteratorDelete (MapIterator k m) =
  (MapIterator k (Map.delete k m))

mapIteratorGetKey :: (Ord k) => MapIterator k v -> k
mapIteratorGetKey (MapIterator k _) = k

mapIteratorGetValue :: (Ord k) => MapIterator k v -> Maybe v
mapIteratorGetValue (MapIterator k m) = Map.lookup k m

mapIteratorExists :: (Ord k) => MapIterator k v -> Bool
mapIteratorExists (MapIterator k m) = Map.member k m

data RuntimeValue
  = NilValue
  | TrueValue
  | NumberValue !LispyNum
  -- | Tables, as in Lua, are used for both sequences and maps.
  -- Unlike in Lua, our tables are ordered by key.  They are
  -- balanced binary search trees.  Also unlike in Lua, they
  -- are purely functional ("Imm": immutable) currently.
  -- Creating a new version of an existing table with an extra
  -- or removed key is O(log n) average and worst case.
  --
  -- Being ordered is very convenient: it means that iterating a table
  -- whose keys are integers will just automatically go in the right order.
  -- Worst-case log(n) is also excellent.  The only downside is that
  -- the constant factor costs of binary search trees are not so great.
  --
  -- NOTE: if you put a recursive closure in a table
  -- so it has PendingValue closure members then the ordering/identity
  -- is somewhat arbitrary.  But using function values as table
  -- keys is silly anyway: anything deterministic seems fine.
  | ImmTableValue !(Map RuntimeValue RuntimeValue)
  | ImmTableViewValue !(MapIterator RuntimeValue RuntimeValue)
  | BuiltinFunctionValue !Builtin
  -- | The instruction pointer points to the beginning of the
  -- function, and the computed values are anything in the
  -- function's closure.
  | FunctionValue !LispyStackFrame !(Vector VarIdx {-params-})
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
  | PendingValue !PendingValueIdx
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






