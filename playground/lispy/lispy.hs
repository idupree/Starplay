{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, MultiParamTypeClasses,
  ViewPatterns, OverloadedStrings, FlexibleInstances #-}

-- TODO LambdaCase is 7.6+

--module Lispy where
module Main where

--import Control.Exception
--import System.Timeout

import qualified Data.Char as Char
import Data.Text as Text
--import Data.Ratio
import Data.List as List
import Data.Vector as Vector
import Data.Map.Strict as Map
import Data.Set as Set
import Data.Foldable as Foldable
import Data.Monoid
import Data.Sequence as Seq
--import Data.Generics.Uniplate.Data
--import Data.Data
--import Data.Attoparsec.Text as P
import qualified Text.Parsec as P
--import Text.Parsec.Text () --for the instance
import Control.Applicative
--import MonadLib as M

-- Haddock it :DD
-- or literate haskell hrm

{-
testEval :: Text -> Text -> IO ()
testEval expr result = do
  try (timeout (evaluate $ do --Maybe
    Right (Program [ast]) <- P.parseOnly parseLispy expr
-}
-- better to work on the stack machine.

--main :: IO ()
--main = print (P.parseOnly parseLispy "abc (d  e 34) (())")

main :: IO ()
main = let
  parsed = doParseLispyProgram "test str"
    --"abc (d  e 34) (())"
    "3 1 2"
  compiled = case parsed of Right ast -> compile ast
  in do
  print parsed
  putStr (showProgramBytecode (programBytecode compiled))


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
                   bcRelativeDestination :: Int }
  | GOTO { bcRelativeDestination :: Int }
  | GOTO_IF_NOT { bcRelativeDestination :: Int, bcConditionName :: VarIdx }
  --UNTIL
  deriving (Eq, Ord, Show)
type VarIdx = ASTIdx
type AtomicValue = LispyNum


-- | TODO: excludes negative scoped vars used (non-closure parameter uses)
scopedVarsUsed :: (Foldable f) => f BytecodeInstruction -> Set VarIdx
scopedVarsUsed = foldMap (\instr -> case instr of
    CALL _ func args      -> mappend (Set.singleton func)
                                     (foldMap Set.singleton args)
    TAILCALL func args    -> mappend (Set.singleton func)
                                     (foldMap Set.singleton args)
    LITERAL _ _           -> mempty
    NAME _ originalName   -> Set.singleton originalName
    RETURN originalName   -> Set.singleton originalName
    MAKE_CLOSURE _ vars _ -> vars
    GOTO _                -> mempty
    GOTO_IF_NOT _ cond    -> Set.singleton cond
  )

type ASTIdx = Int
data SourceLocInfo = SourceLocInfo
  { sourceText :: !Text
  , sourceBegin, sourceEnd :: !P.SourcePos
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


data CompiledProgram = CompiledProgram
  { programBytecode :: !(Vector (ASTIdx, BytecodeInstruction))
  , programAST :: !(Located AST)
  , programASTsByIdx :: !(Vector (Located AST))
  }
--instance Show CompiledProgram where
showsProgramBytecode :: Vector (ASTIdx, BytecodeInstruction) -> (String -> String)
showsProgramBytecode = appEndo . foldMap
  (\(_, instr) -> Endo (shows instr . showChar '\n'))
showProgramBytecode :: Vector (ASTIdx, BytecodeInstruction) -> String
showProgramBytecode = flip showsProgramBytecode ""
  

-- how does let or arg-binding work
-- current stack frame astidx indexed
-- 'call' can do special stuff

indexAST :: Located AST -> Vector (Located AST)
indexAST astToIndex = let
  makeASTIdxMap :: Located AST -> Map ASTIdx (Located AST)
  makeASTIdxMap lAST@(L _ (ASTList idx children)) =
    Map.insert idx lAST (foldMap makeASTIdxMap children)
  makeASTIdxMap lAST@(L _ ast) =
    Map.singleton (astIdx ast) lAST
  astIdxMap = makeASTIdxMap astToIndex
  in Vector.generate (Map.size astIdxMap) (astIdxMap Map.!)

-- This implementation could be faster because Seq has O(1) length.
seqToVector :: Seq a -> Vector a
seqToVector s = Vector.fromList (Foldable.toList s)

compile :: Located AST -> CompiledProgram
compile ast = let
  instrs = compile' (CompileScope Map.empty True) ast
  in CompiledProgram
    (seqToVector instrs)
    ast
    (indexAST ast)

data CompileScope = CompileScope
  { compileScopeEnv :: Map Text VarIdx
  -- | when compileScopeIsTailPosition, the returned code
  -- is expected to return or tailcall, never to let the
  -- end of the returned code-block be reached.
  , compileScopeIsTailPosition :: Bool   --Maybe ASTIdx
  }

-- We use 'Seq' (Data.Sequence) because it has O(1) cons and snoc
-- and length, and O(log n) concatenation.
compile' :: CompileScope -> Located AST -> Seq (ASTIdx, BytecodeInstruction)
compile' scope (L _ ast) = let
  instr i = Seq.singleton (astIdx ast, i)
  in 
  case ast of
  ASTNumber _ v -> mconcat [
    instr (LITERAL (astIdx ast) v),
    if compileScopeIsTailPosition scope
    then instr (RETURN (astIdx ast))
    else mempty
    ]
  ASTIdentifier _ v ->
    case Map.lookup v (compileScopeEnv scope) of
      -- TODO show src loc in error too
      Nothing -> error ("undeclared identifier " List.++ Text.unpack v)
      Just varIdx ->
        if compileScopeIsTailPosition scope
        then instr (RETURN varIdx)
        else instr (NAME (astIdx ast) varIdx)
  ASTList _ list -> case Vector.toList list of
    [] -> error "() as a nil list literal not presently supported"
    (func : args) -> case unL func of
      ASTIdentifier _ "if" -> case args of
        --[cond, then_] ->
        [cond, then_, else_] -> let
          condCode = compile' scope{compileScopeIsTailPosition=False} cond
          thenCode = compile' scope then_
          elseCode = compile' scope else_
          elseCode2 = if compileScopeIsTailPosition scope
            then elseCode
            else mconcat [
              elseCode,
              instr (NAME (astIdx ast) (lASTIdx else_))
              ]
          thenCode2 = if compileScopeIsTailPosition scope
            then thenCode
            else mconcat [
              thenCode,
              instr (NAME (astIdx ast) (lASTIdx then_)),
              instr (GOTO (Seq.length elseCode2))
              ]
          in mconcat [
            condCode,
            instr (GOTO_IF_NOT (Seq.length thenCode2) (lASTIdx cond)),
            thenCode2,
            elseCode2
            ]
        _ -> error "syntax error: 'if' must match (if cond then else)"
      ASTIdentifier _ "letrec" -> let
        letrecSyntaxMsg =
          "syntax error: 'letrec' must match (letrec ((var expr)...) result)"
        in case args of
        [L _ (ASTList _ bindings), result] -> let
          parseBinding :: Located AST -> (Text, Located AST)
          parseBinding (L _ (ASTList _
            (Vector.toList -> [L _ (ASTIdentifier _ varname), expr])))
            = (varname, expr)
          parseBinding _ = error letrecSyntaxMsg
          parsedBindings = fmap parseBinding bindings
          bindingEnv = Map.fromList (Vector.toList (
            (fmap (\ (varname, expr) -> (varname, lASTIdx expr)) parsedBindings)))
          resultScope = scope { compileScopeEnv =
            (Map.union bindingEnv (compileScopeEnv scope)) }
          bindingsCode = foldMap (compile' resultScope . snd) parsedBindings
          resultCode = compile' resultScope result
          in mconcat [
            bindingsCode,
            resultCode,
            if compileScopeIsTailPosition scope
            then mempty
            else instr (NAME (astIdx ast) (lASTIdx result))
            ]
        _ -> error letrecSyntaxMsg
      ASTIdentifier _ "lambda" -> case args of
        [L _ (ASTList _ params), body] -> let
          bindingEnv = Map.fromList (Vector.toList (
            fmap (\ (paramNum, L _ (ASTIdentifier _ varname))
                      -> (varname, -paramNum - 1))
            (Vector.indexed params)))
          bodyScope = scope {
            compileScopeEnv = (Map.union bindingEnv (compileScopeEnv scope)),
            compileScopeIsTailPosition = True
            }
          bodyCode = compile' bodyScope body
          bodyClosureUses = Set.intersection
            (Set.fromList (Map.elems (compileScopeEnv scope)))
            (scopedVarsUsed (fmap snd bodyCode))
          in mconcat [
            instr (MAKE_CLOSURE (astIdx ast) bodyClosureUses 1),
            instr (GOTO (Seq.length bodyCode)),
            bodyCode
            ]
        _ -> error "syntax error: 'lambda' must match (lambda (var...) body)"
      ASTIdentifier _ "do" -> case List.reverse args of
        [] -> mempty
        (last_ : (List.reverse -> init_)) -> mconcat [
          foldMap (compile' scope{compileScopeIsTailPosition=False}) init_,
          compile' scope last_,
          if compileScopeIsTailPosition scope
          then mempty
          else instr (NAME (astIdx ast) (lASTIdx last_))
          ]
      _ -> let
        funcCode = compile' scope{compileScopeIsTailPosition=False} func
        argsCode = foldMap (compile' scope{compileScopeIsTailPosition=False}) args
        call = if compileScopeIsTailPosition scope
          then TAILCALL
          else CALL (astIdx ast)
        in mconcat [
          funcCode,
          argsCode,
          instr (call (lASTIdx func) (Vector.fromList (fmap lASTIdx args)))
          ]


-- TODO using ByteString instead of Text would improve the asymptotic
-- complexity (Text doesn't have O(1) length or slicing)
-- but then we need to parse UTF-8 chars with parsec.
data LocText = LocText {-#UNPACK#-}!Int !Text
instance (Monad m) => P.Stream LocText m Char where
  uncons (LocText loc text) = return (fmap (fmap (LocText (loc+1))) (Text.uncons text))
  {-# INLINE uncons #-}

type LispyNum = Int
type LispyParser = P.Parsec LocText ASTIdx



schemeIdentifierChar :: LispyParser Char
schemeIdentifierChar = P.satisfy (\c ->
    {-Attoparsec has inClass-}
    Set.member c (Set.fromList "-!$%&*+.\\/:<=>?@^_~")
    || Char.isAlphaNum c
  )

parseNaturalNumber :: LispyParser Int
parseNaturalNumber = go 0
  where
    go n = do
      d <- P.digit
      let n' = n * 10 + (Char.digitToInt d)
      P.option n' (go n')

parseNumber :: LispyParser LispyNum
parseNumber =
  fmap negate (P.char '-' >> parseNaturalNumber)
  <|> parseNaturalNumber

parserNextASTIdx :: LispyParser ASTIdx
parserNextASTIdx = do
  idx <- P.getState
  P.putState (idx + 1)
  return idx

parseAtom :: LispyParser AST
parseAtom =
  P.choice (List.map P.try
  [ do
      num <- parseNumber P.<?> "number"
      idx <- parserNextASTIdx
      return (ASTNumber idx num)
  , do
      ident <- (P.many1 schemeIdentifierChar) P.<?> "identifier"
      idx <- parserNextASTIdx
      return (ASTIdentifier idx (Text.pack ident))
  ])
{-
  [ fmap (\rat -> Number (LispyNum rat True)) rational
  , fmap (\str -> Ident str) $ many1 schemeIdentifierChar
  , fmap (const Void) $ string "#void"
  , fmap (const (Boolean True)) $ string "#true"
  , fmap (const (Boolean False)) $ string "#false"
  , fmap (const UnboundVariable) $ string "#unbound-variable"
  -- warn/fail if it's not a known builtin function?
  , fmap (\str -> BuiltinFunction str) $ P.char '#' >> many1 schemeIdentifierChar
  ]-}

parseList :: LispyParser AST
parseList = do
  _ <- P.char '('
  idx <- parserNextASTIdx
  asts <- P.many parseSexp
  _ <- P.char ')'
  return (ASTList idx (Vector.fromList asts))

parseSexp :: LispyParser (Located AST)
parseSexp = parseWithLocation (parseAtom <|> parseList) P.<?> "s-expression"

parseWithLocation :: LispyParser a -> LispyParser (Located a)
parseWithLocation parser = do
  P.skipMany P.space
  beginLoc <- P.getPosition
  LocText beginCharIdx beginText <- P.getInput
  
  parsed <- parser
  
  endLoc <- P.getPosition
  LocText endCharIdx _ <- P.getInput
  let info = SourceLocInfo
        (Text.take (endCharIdx - beginCharIdx) beginText)
        beginLoc
        endLoc
  
  P.skipMany P.space
  return (L info parsed)

parseLispyProgram :: LispyParser (Located AST)
parseLispyProgram = do
  idx <- parserNextASTIdx
  fmap (fmap (ASTList idx . Vector.fromList))
    (parseWithLocation (P.many parseSexp))

doParseLispyProgram :: P.SourceName -> Text -> Either P.ParseError (Located AST)
doParseLispyProgram sourceName text =
  P.runParser parseLispyProgram 0 sourceName (LocText 0 text)


{-





data LispyNum = LispyNum { numVal :: Rational, numIsExact :: Bool }
  deriving (Eq, Ord, Typeable, Data)
instance Num LispyNum where
  LispyNum v1 e1 + LispyNum v2 e2 = LispyNum (v1 + v2) (e1 && e2)
  LispyNum v1 e1 - LispyNum v2 e2 = LispyNum (v1 - v2) (e1 && e2)
  LispyNum v1 e1 * LispyNum v2 e2 = LispyNum (v1 * v2) (e1 && e2)
  negate (LispyNum v1 e1) = LispyNum (negate v1) (e1)
  abs (LispyNum v1 e1) = LispyNum (abs v1) (e1)
  signum (LispyNum v1 e1) = LispyNum (signum v1) (e1)
  fromInteger i = LispyNum (fromInteger i) True



type Ident = String

builtins :: Map Ident Atom --or AST?
--builtins = Map.empty
builtins = Map.union fns vals
  where
  fns = Map.mapWithKey (\i _ -> BuiltinFunction i) builtinFunctions
  vals = Map.fromList $
    [ ("#void", Void)
    , ("#true", Boolean True)
    , ("#false", Boolean False)
    , ("#unbound-variable", UnboundVariable) ]

data PathToCodeLiteral = PathToCodeLiteral Ident [Int]
getCodeLiteralE :: Env -> PathToCodeLiteral -> AST
getCodeLiteralE env (PathToCodeLiteral i path) = case Map.lookup i env of
  Just ast -> getCodeLiteralP path ast
  Nothing -> error "invalid path envname!"

getCodeLiteralP :: [Int] -> AST -> AST
getCodeLiteralP [] ast = ast
getCodeLiteralP (idx:is) ast = getCodeLiteralP is (getChildNumber idx ast)

getChildNumber :: Int -> AST -> AST
getChildNumber i ast = children ast !! i

--data Stack = 


type Env = Map Ident AST
-- this monad could also 'tick' and when it runs
-- out of ticks, return.  except that moment won't
-- be serializable?
type EvalM = ReaderT Env CanFail

evalToNumber :: AST -> EvalM LispyNum
evalToNumber = undefined
evalToBool :: AST -> EvalM Bool
evalToBool = undefined
evalToAtom :: AST -> EvalM Atom --I am lazy with implementing =
evalToAtom = undefined
eval :: AST -> EvalM AST --TODO return free var env needed?
eval = undefined

-- BuiltinFunction vs BuiltinSpecialForm

data BuiltinFunction = Plus | Minus | Times | Negate
  | LessThan | LessEqual | GreaterThan | GreaterEqual
  | Equal | NotEqual | And | Or | Not
  
data BuiltinSpecialForm = Lambda | If | Let | Loop | While | For
  | Goto | Label | Tailcall | Return | Do | Set


data Atom
    = Void
    | Number LispyNum
    | Ident Ident
    | Boolean Bool
    | UnboundVariable
    | BuiltinSyntax BuiltinSpecialForm
    | BuiltinFunction BuiltinFunction --{ atomName :: String }--, atomFnVal :: AST -> AST }
  deriving (Eq, Ord, Data, Typeable)--, Show, Read)
--instance Eq Atom where
--  Void == Void = True
--  Number a b == Number a b = a

{-
--hmm
noL :: a -> Located a
noL = L NoSrcLoc
data SrcLoc = SrcLoc {
  slBegin :: Int, -- ^ index of first character
  slEnd :: Int, -- ^ one-past-the-end of represented text
  slProg :: Text -- ^ the whole input text that the indices index
  } | NoSrcLoc --maybe useful to have list of all generating fns, or most recent
  deriving (Data, Typeable)
data Located a = L { lLoc :: SrcLoc, unL :: a } deriving (Functor, Data, Typeable)
type LAST = Located AST
type LAtom = Located Atom
-}

data Lambda = Lambda 
ifCondIdx = 0
ifThenIdx = 1
ifElseIdx = 2
viewLambda ::

--tail position (proper tail calls etc)

-- a single program for all your forces, I guess
-- (but, per player? hrm. maybe you can switch. or share??)

newtype ASTIdx = ASTIdx Int
  deriving (Eq, Ord, Data, Typeable)
newtype IdentIdx = IdentIdx Int
  deriving (Eq, Ord, Data, Typeable)
type LocalVarIdx = ASTIdx --pointing to the AST node that binds it
type GlobalVarIdx = IdentIdx --we uniquify strings used in the program
--data BiMap a b = BiMap (Map a b) (Map b a)
--whoops hmm they're not all bi, and, efficiency
data BiIdxMap enum b = BiMap (Vector b) (Map b enum)

-- turn idents into IdentIdx
-- turn IdentIdx into ScopedVarIdx (unique over whole program for each same-use)
data StackFrame = StackFrame
  -- | sfFuncBody is the root node of the function body;
  -- (+ x (+ y 3)) in (fn (x y) (+ x (+ y 3)))
  { sfFuncBody :: ASTIdx
  -- | sfNextStepToEval is some node within (possibly equal to) sfFuncBody.
  -- If equal to sfFuncBody, we are close to done computing the function's
  -- value (we are about to do the last step).
  , sfNextStepToEval :: ASTIdx
  -- | For closures, sfAlreadyComputed starts nonempty.
  --
  -- It is permitted to remove vals from it if they're not
  -- required to compute the rest of the fn, but not required
  -- (not even for lexical temporaries and for sub-scopes? hmm...)
  , sfAlreadyComputed :: Map LocalVarIdx AtomicValue
  --, sfReturnTo :: Maybe (StackFrame, LocalVarIdx {-where to put the return value-}
  --ah but we can not move on until return, and sfNextStepToEval will say where
  }

-- canonicalize: compresses identidxs to only used ones at runtime ?

data LispyState = LispyState
  { lsGlobals :: Map GlobalVarIdx AnyValue
  , lsStack :: [StackFrame]
  }

singleStep :: LispyState -> LispyState
singleStep state@(LispyState _ []) = state
singleStep state@(LispyState globals stack@(
    frame@(StackFrame funcBody nextStepToEval alreadyComputed)
    : frames
  )) = case nextStepToEval of
    Literal atom -> atom
    FnCall (Vector.null -> True) -> error "bug: fncall with no function"
    FnCall asts = let fn = Vector.head asts; args = Vector.tail asts in
      case fn of
        BuiltinSyntax If ->
        _ | Just functionVal <- Map.lookup fn alreadyComputed ->
          callSemiAtomic functionVal
  where
    callSemiAtomic :: SemiAtomicValue -> Vector ASTIdx -> LispyState
    callSemiAtomic SemiAtomicValue (ReallyAtomic atom) = callAtomic
    callSemiAtomic (ReferenceToGlobal globalVar) =
      case Map.lookup globalVar globals of
        Nothing -> error "undefined global var called as a function"
        Just (AnyAtomicValue atomic) -> callAtomic atomic
        Just (AnyCompositeValue (LispyClosure ast closureBindings)) ->
          LispyState globals (
            (StackFrame (fnBody ast) (fnEntrance ast) closureBindings)
            : stack)
    
    callAtomic :: AtomicValue -> Vector ASTIdx -> LispyState
    callAtomic (ClosurelessFunction ast) =
          LispyState globals (
            (StackFrame (fnBody ast) (fnEntrance ast) Map.empty)
            : stack)
    callAtomic (BuiltinFunction f) = callBuiltinFunction f

-- | fnEntrance takes the AST node of a lambda expression and returns
-- the AST node of the first evaluation step that must be done as part of
-- evaluating that lambda function.  For example, given
--   (fn (x) (+ x (+ x x)))
-- This plus  ^  is the first expression in the function that must be
-- evaluated.  We treat all functions/operators as just another value
-- that must be evaluated as function-evaluation-time (we are a Lisp-1),
-- but all builtin syntax ('fn', 'if', etc.) is treated specially
-- (we are not really a lisp: we do not have macros).
fnEntrance :: AST -> AST
fnEntrance (FnCall (Vector.toList -> [Literal (BuiltinSyntax Fn), params, _])) =
  sastNextASTNodeToExecuteAfterExecutingThisOne params

-- | fnBody takes a lambda and returns the body node;
-- (fn (x) (+ x x)) --> (+ x x)
-- I only support lambdas with a single body node.  Imperative functions
-- with more than one body node can be trivially emulated with "do"
-- ("begin" from Scheme):
-- (fn (x) (doStuff) (+ x x)) could be (fn (x) (do (doStuff) (+ x x))).
--
-- Implementing translations like this is not wholly trivial.  We use
-- AST node indices to represent the instruction pointer.  This shallow
-- approach makes it unnecessarily hard to represent single builtins
-- that do multiple things.  For example, (while (< x 3) (set x (+ x 1)))
-- might be represented in a bytecode:
-- (syntax: pseudocode in SSA form)
-- 1: LABEL begin
-- 2: %a <- GET x
-- 3: %b <- LESS_THAN %a 3
-- 4: GOTO_IF_NOT end %b
-- 5: %c <- GET x
-- 6: %d <- PLUS %c 1
-- 7: SET x %d
-- 8: GOTO begin
-- 9: LABEL end
-- Both code locations "1" and "4" are worth distinguishing, but the AST
-- does not let us do that!  The condition is "3" ("2" is a sub part of
-- evaluating the condition), and the body is "7" (5-7 if including
-- sub-expressions).  We could say that "1" corresponds to the whole AST
-- and "4" to the "while" identifier node, or vice versa, but that's just
-- a hack that happens to work.  It might not work for a more complex
-- operator such as a C-style-"for" expression.
--
-- labels can be truly a label instead of a code location: yeah
-- non-conditional gotos could be an annotation of "next instruction ptr" instead: nah


data InstructionPointer =
data InstructionPointerMode = Entering |

-- If bytecode and AST are made isomorphic then we can just *have* the bytecode
-- trivially in Haskell.
-- They don't quite correspond, though.
-- We can artificially make every AST node have at least one bytecode node
-- (that could be a NOP) if we wanted.  If it was in a meaningful place, that
-- might be useful.
-- There's no reason to make bytecode transformable to AST because it can just
-- keep references into the original AST.

-- (In terms of actually using Lua code, upvalues are probably the trickiest
--  to adapt to us)
-- and nonmutability so snapshots are trivial

-- ok debuggability will be less well demonstrated than steppability,
-- a future-work thing that this framework allows.
-- similar for space limit

-- program: letrec, e.g.
{-
 (plusone (lambda (x) (+ x 1))
 (factorial (lambda (x) (if (= x 0) 1 (* x (factorial (- x 1))))))
 (one 1)  ;haha
-}
-- and then the body is whatever you want to run
-- hum, init time, literals can have code.
-- maybe it really is letrec - the last thing is just the last thing
-- Also is prepended with (+ #+) and such: all the builtin functions


--hmm we could make all literals always be in all envs

-- The compiled program contains a sequence of bytecodes
-- as well as references to the AST nodes they come from.
-- Intermediates: hm
-- Current system is a vector/AST idx (equivalent for this)
-- duplicated for each stack frame
-- RET
-- TAILCALL
-- CALL
-- if+tailcall
-- pushframe
-- goto
-- MAKE_CLOSURE (Set VarIdx) ASTIdx
--
-- PUSHFRAME (Map VarIdx{in new frame} VarIdx{in this frame})
-- REPLACEFRAME (Map VarIdx{in new frame} VarIdx{in this frame})
-- POPFRAME
-- ret...
-- serialize as the state is a
-- PUSHFRAME (map...) bytecodeloc
-- oh wait that does not encode the whole stack :/
-- well i mean i can just serialize it
-- but it would be nice if it were a valid program somehow.
-- hmm with multiple recursion i'm not going to be able to
-- serialize it into anything nice looking with #val.
-- though it is doable without recursion.
-- So; security of passing around this serialization:
-- it could point weird places in the bytecode or have loops
-- or have variables defined that shouldn't be or undefined
-- that should be.
-- I *think* everything here is checked and finite, but
-- rarely tested conditions are risky.  A fuzz tester would
-- be useful.
-- Single-steppable and visualizable
--
-- Wait, PUSHFRAME can't know the destination's arg names.
-- BECAUSE IT MIGHT BE A BUILTIN!

-- You can name something anytime it's created, store that
-- name in the heap next to the data, & also store as a key
-- in a globals.
-- Can we efficiently do escape analysis with it? Hmm. Not easily;
-- we could have a reference to a table that we don't know whether
-- it's local? hm er.  each composite created could have a reference
-- to the stack frame it was created in, and vice versa, and if it's
-- put into a composite (including _GLOBALS) that is not within this
-- stack frame then its scope is lifted to that frame (or it's made
-- global maybe)  (or if the stack frame returns).
-- So I think we can basically do this all in the basic lua framework
type BytecodeIdx = Int

-- making TAILCALL: perhaps a postprocessing pass?
-- top-down, tail if:
-- * if root node
-- * if parent is tail 'do' and we are its last child
-- * if parent is tail 'if' and we are its second or third child
-- * if parent is lambda

-- goto can be to the ast idx at first, then replaced

--TAIL: end with RETURN or TAILCALL

-- use Data.Sequence or difflists

--er void, true, false can all just be reserved varnames
--oh wait that made the localization harder
--but so does 'if' then

-- ASTIdx of return value? n/a? ok

--getting the ASTIdx of result value right
--compileTail :: CompileScope -> SituatedAST -> CompiledProgram
--compileNonTail :: CompileScope -> SituatedAST -> CompiledProgram

-- I'll just inefficiently have tail do/if bits in non-tail position
-- rebind their result vals.  It makes debugger-ing potentially nicer

fnBody :: AST -> AST
fnBody (FnCall (Vector.toList -> [Literal (BuiltinSyntax Fn), _, body])) = body

--data FnASTView = FnASTView 
--viewFnAST :: AST -> 

-- Error reporting is currently done by throwing an exception.
-- For example, evaluating (= 1) does not pass enough arguments
-- to the equality-checking operator.  It might lead to indexing
-- a vector with an out-of-bounds index (which throws an exception
-- in Haskell).
--
-- A serious implementation would check for these errors and
-- give a user-friendly message.  It's not theoretically difficult
-- to do so; I omit it here.

-- An idiomatic Haskell program might use a monad to track state,
-- rather than pass and return LispyState explicitly.  I choose
-- to pass it explicitly here for explicitness.

-- | callBuiltinFunction assumes that all its parameters have been
-- evaluated.  This is guaranteed by the preprocessing that writes
-- sastNextASTNodeToExecuteAfterExecutingThisOne (TODO explain)
callBuiltinFunction :: BuiltinFunction -> Vector ASTIdx -> LispyState -> LispyState
callBuiltinFunction builtinFunction params state@(LispyState globals stack@(
    frame@(StackFrame funcBody nextStepToEval alreadyComputed)
    : frames
  )) = let
      arg :: Int -> AtomicValue
      arg i = Map.lookup (params Vector.! i) alreadyComputed
      num :: ASTIdx -> LispyNum
      num = case arg i of Number n -> n
      binaryNumericOp :: (LispyNum -> LispyNum -> LispyNum) -> LispyState
      binaryNumericOp op = LispyState globals (op (num 1) (num 2))
    case builtinFunction of
  Plus -> binaryNumeric (+)
  LispyState globals
Plus | Minus | Times | Negate
  | LessThan | LessEqual | GreaterThan | GreaterEqual
  | Equal | NotEqual | And | Or | Not
builtinFunctions :: Map Ident ([AST] -> EvalM Atom)
builtinFunctions = Map.fromList $
  --[ ("+", \case [_,a,b] -> Number <$> ((+) <$> num a <*> num b); ast -> arityFail ast)
  [ ("+", binary Number (+) num)
  , ("-", binary Number (-) num)
  , ("*", binary Number (*) num)
  , ("negate", unary (Number . negate) num)

  , ("<", binary Boolean (<) num)
  , (">", binary Boolean (>) num)
  , ("<=", binary Boolean (<=) num)
  , (">=", binary Boolean (>=) num)

  , ("and", binary Boolean (&&) bool)
  , ("or", binary Boolean (||) bool)
  , ("not", unary (Boolean . not) bool)

  , ("=", binary Boolean (==) evalToAtom)
  , ("not=", binary Boolean (/=) evalToAtom)
  {-  Blargh these are now keywords
      We could change that by treating 1st arg specially but aah
  , ("if", \case
      [_, cond, t, f] -> do
        b <- bool cond
        return $! if b then t else f
      ast -> arityFail ast)
      -}
  , ("array", undefined)
  ]
{-  [ easy "+" $ \ast -> do [_, Number a, Number b] <- ast; return $ Number $ a+b
  , easy "-" $ \ast -> do [_, Number a, Number b] <- ast; return $ Number $ a-b
  , easy "*" $ \ast -> do [_, Number a, Number b] <- ast; return $ Number $ a*b
  --, easy "#mod" $ \ast -> do [_, Number a, Number b] <- ast; return $ Number $ a3-b
  , easy "negate" $ \ast -> do [_, Number a] <- ast; return $ Number $ negate a

  , easy "<" $ \ast -> do [_, Number a, Number b] <- ast; return $ Boolean $ a<b
  , easy ">" $ \ast -> do [_, Number a, Number b] <- ast; return $ Boolean $ a>b
  , easy "<=" $ \ast -> do [_, Number a, Number b] <- ast; return $ Boolean $ a<=b
  , easy ">=" $ \ast -> do [_, Number a, Number b] <- ast; return $ Boolean $ a>=b

  , easy "and" $ \ast -> do [_, Boolean a, Boolean b] <- ast; return $ Boolean $ a&&b
  , easy "or" $ \ast -> do [_, Boolean a, Boolean b] <- ast; return $ Boolean $ a||b
  , easy "not" $ \ast -> do [_, Boolean a] <- ast; return $ Boolean $ not a
  ]-}
  where
    --easy name f = ("#"++name, \ast -> f ast <|> arityFail ast)
    arityFail ast = inBase $ Err ("invalid arguments to builtin: " ++ showLList (fmap show ast))
    num = evalToNumber
    bool = evalToBool
    unary op extr = \case [_,a] -> (Literal . op) <$> extr a; ast -> arityFail ast
    binary constr op extr = \case [_,a,b] -> (Literal . constr) <$> (op <$> extr a <*> extr b); ast -> arityFail ast


-- map keys in-language take keys by-value, by fiat!
--   #copy


-- Map GlobalVarIdx AnyValue

-- tuple vs array? asymptotics?

data AnyValue
  = AnyCompositeValue CompositeValue
  | AnyAtomicValue AtomicValue
  deriving (Eq, Ord, Data, Typeable)
  
-- so either we have separate  data FrozenValue
-- or our mutation operations take arbitrarily long lvalue paths.
-- hm
-- or both
-- also why the copying? oh well :P
data FrozenValue
  = FrozenLispyArray (Vector FrozenValue)
  | FrozenLispyMap (Map FrozenValue FrozenValue)
  | FrozenClosure ASTIdx (Map LocalVarIdx FrozenValue)
-- I see no excuse to forbid it. (famous last words.)
--  | FrozenClosure ASTIdx (Map LocalVarIdx FrozenValue {-^closure-})
----  | FrozenClosure AST (Map IdentIdx FrozenValue {-^closure-})--oops not normalized re alpha renaming
  | FrozenAtom AtomicValue
  deriving (Eq, Ord, Data, Typeable)

-- | O(n) -- TODO fix O(C^n) worst case!
-- Whoops worst case is
-- @a = [@b, @b]
-- @b = [@c, @c]
-- @c = [@d, @d]
-- [...]
-- ...now, it is possible to fix the worst case by
-- temporarily memoizing globalvaridx frozenvalues
-- and sharing the results -- but then *doing* anything with it,
-- like printing it or comparing it to itself, would still
-- take exponential time!
-- Also we need to be resilient to just plain loops.

-- Is there any reasonable way to pause in the middle of freezing something?
-- Or will we have to get it to O(n) and then accept that?
-- But we should allow 10000 mem elements, and be interruptible
-- by physics-feedback every 0.1s, and be only 1-10kHz
-- which means that one op on 10k elements...
-- (10k/1kHz) = 10s   (10k/10kHz) = 1s
-- which is too long a feedback time
-- and if we allow interruption for free we let them waste time
--   (and this would indeed be a common user programming error)
-- and it's unreasonable to ask the user not to freeze values I think.
-- HOWEVER
-- freezing's constant factor is probably smaller than 200 processor cycles
-- for large values
-- also, large numbers of global-variable-names is unlikely in in-control
-- robos
--
-- How long does even doing the graph traversal to detect loops
-- and number of things take?  We can separately have a set
-- (map ident->refcount really) of direct refs with each global at only
-- constant cost, but that doesn't help the worst case where every
-- member is a reference! I don't think we can have a transitive set
-- (nor map ident->refcount) because it'd take too long to update.
--
-- And the graph traversal is up to O(n) in program memory!
-- A way to do it:
--   Queue GlobalVarIdx --todo
--   Map GlobalVarIdx FrozenValue --done
--   --currently in progress:
--   RevList FrozenValue       --CurrentSequencesValues
--     er perhaps that should be a stack
--     lol this is complex
--     i suppose primitives could be made to do it in script lang
--     but: slow & also complex.
--     so there'd need to be a way to incrementally build a Frozen*
--     and then freeze it in constant time
--
-- also er! sort() on long arrays!!
-- it is extra crazy to try this goal on them. hmm.
-- suppose our worst processor time is 100ns*n*log_2(n) with n=32768
-- then operation time is 100ns*32768*15 = 1.5us*32768 = 1.5ms*32.768 =
-- about 50 ms = 1/20 second! -for just one robot program.  Hrm.
-- ...of course, interpreted code probably gets 10x perf penalty or so.
-- Also hrm: at 10kHz, you can't do *anything* with 10k things in under
-- a second. Hrm.
--
-- So a ~1 page program (lines 246-302 of playground.coffee) is 310 words
-- incl comments, 186 words excl comments.
-- word approx= token
-- amount of time to traverse the whole program text.
-- RoboWar 50 ops per tick vs 10Hz ticks would be 0.5kHz, interesting
-- So I am proposing 1kHz approx= RoboWar 100 processor, 10kHz --> 1000
-- Ok
--
-- Green threads might be acceptable in that a 50ms/robot time cost to *save*
-- might be okay. Hrm.
-- so
-- I want it to be serializable at least as often as every 0.1s
-- which at 10kHz is every 1000 operations
--
-- Oh hmm, if I know the duration of a coming long-running operation,
-- I can put off running it until it's about to be finished.
-- Downside: time travel or duplication(sending the game to another person)
-- can repeat the operation more than it should be.  Upside: simpler code
-- But... I don't even know how to know the duration for structural_lessthan
-- of two structures!

-- structural_lessthan
-- so that it can work on e.g. 10m < 10s  :-P

-- IO device: radio (aka: yelling :)

-- for web thing, identity matters
-- so
-- number things by order of first encountered in a depth first search
-- (with cycles detected to prevent infiniteness, but after the first
--  go through them, they won't add any "first encountered", so it's fine)
-- I think that's O(good)

-- TODO make primitives to add a thing to a frozen Map/Table
-- nonmutatingly, then implement this in userspace
-- sort() is just add everything to a table then extract it?
-- or hmm it will be frozen: but it can be mapped to itself, and then
-- the val taken.
freezeLispyValue :: Map GlobalVarIdx AnyValue -> AnyValue -> FrozenValue
freezeLispyValue _ (AnyAtomicValue v) = FrozenAtom v
freezeLispyValue env (AnyCompositeValue cv) = case cv of
  --LispyArray arr -> FrozenLispyArray (Vector.map (freezeSemiAtomicValue env) arr)
  LispyMap m -> FrozenLispyMap (Map.map (freezeSemiAtomicValue env) m)
  LispyClosure i c -> FrozenClosure i (Map.map
                          (freezeLispyValue env . (env Map.!)) c)

-- | O(n)
freezeSemiAtomicValue :: Map GlobalVarIdx AnyValue -> SemiAtomicValue -> FrozenValue
freezeSemiAtomicValue _ (ReallyAtomic v) = FrozenAtom v
freezeSemiAtomicValue env (ReferenceToGlobal g) =
  freezeLispyValue env (env Map.! g)

-- ??? that can't be right. unfreezing something e.g.
--    @someKey = @someMap.last.key
--    @someKey[3][4] = 7
--alts:
-- explicit unfreezing op
-- can't unfreeze, things implicitly freeze for comparisons with it
--   (actually, deep comparisons should implicitly freeze anyway!)
-- allow an explicit Reference GlobalVarIdx to be used in map keys
--   or allow retrieving the original reference (but it has changed vars now?!)
--   or allow changes in the mutable value to redo the map (?!
--      -- when there is a conflict, what happens? require multimap?
--      then what orders are the result vals in?  sorted lol?
--      bimultimap actually makes a bit of sense)
--thawLispyValue :: FrozenValue -> (AnyValue, Map GlobalVarIdx AnyValue)
--
-- also when re-serializing, what about map keys that have been extracted
-- and passed around a lot - do they get duplicated a bunch? or are they
-- CSE'd into a global!?

-- so I can make arrays be lua-like (use maps for now anyhow).
-- and iterators be a stack (map) of interior ptrs which technically makes
-- a full iteration be n log(log n) I think
-- but in haskell I don't have access to those interior ptrs! but I do
-- have lazy toList
-- Also what about mutation using iterators.  Does it just re look up
-- the key? :/ I guess? but that feels wrong
-- or are the values mutable but the keystructure not? that sounds
-- really weird, & which mutations are shared would be unpredictable
-- Python just silently does the easy thing here:
{-
>>> v = ['a', 'b']
>>> for i, s in enumerate(v): s = s + 'z'
... 
>>> v
['a', 'b']
>>> for i, s in enumerate(v): v[i] = s + 'z'
... 
>>> v
['az', 'bz']
>>> 
-}
data CompositeValue
--  = LispyArray (Vector SemiAtomicValue)
  -- hmm this is wrong: the keys have to be *recursively* atomic,
  -- e.g. an array of arrays of numbers.
  = LispyMap (Map FrozenValue SemiAtomicValue)
  | LispyClosure ASTIdx (Map LocalVarIdx GlobalVarIdx)
--  | LispyFunction ASTIdx (Map LocalVarIdx AtomicValue {-^closure-})
  deriving (Eq, Ord, Data, Typeable)

--data AtomicVariable
--data LocalVariableValue
data SemiAtomicValue
  = ReallyAtomic AtomicValue
  | ReferenceToGlobal GlobalVarIdx
  deriving (Eq, Ord, Data, Typeable)

data AtomicValue
  = Void
  | Number LispyNum
  | Boolean Bool
  | BuiltinFunction BuiltinFunction
  | ClosurelessFunction ASTIdx
  | Symbol ____
--  | ImplicitReference GlobalVarIdx
  deriving (Eq, Ord, Data, Typeable)

data ProgramData = ProgramData
  { programSourceText :: Text
  , programAST :: AST
  , programIndexedASTs :: Vector AST
data SituatedAST = SituatedAST
  { sastAST :: AST
  , sastIdx :: ASTIdx
  , sastParentIdx :: Maybe ASTIdx
  , sastChildIdxs :: Vector ASTIdx
  -- |
  -- In
  --   (fn (x y) (+ x (+ y 3)))
  --   01  23 4  56 7 89 a b
  -- with the (hexadecimal) digits underneath the expression
  -- naming each AST node,
  -- 0 -> somewhere in the outside scope (0's effect is
  -- creating a closure).
  -- 1 -> 0   (or 1 -> Nothing)
  -- 2 -> 6   (arbitrarily put here the info of where to enter the function)
  -- 3 -> Nothing  (parameters aren't expressions)
  -- 4 -> Nothing  (parameters aren't expressions)
  -- 5 -> Nothing  (after evaluating this we return)
  -- 6 -> 7
  -- 7 -> 9
  -- 9 -> a
  -- a -> b
  -- b -> 8
  -- 8 -> 5
  , sastNextASTNodeToExecuteAfterExecutingThisOne :: Maybe ASTIdx
  }

data AST
    = FnCall (Vector AST)
--  #ignore: strict in args, ret #void    | Program [AST]
--  #do: strict in args, ret last one    | Imperative [AST]
--    | EnvAST Env AST
    | Literal Atom

{-
    | Lambda { lambdaParams :: [Ident], lambdaBody :: AST }
    | If { ifCond :: AST, ifThen :: AST, ifElse :: AST }
    | Let { letBindings :: [(Ident, AST)], letBody :: AST }
    | Loop { loopBody :: AST }
    | While { whileCondition :: AST, whileBody :: AST }
    | For { forInit :: Maybe (Maybe Ident, AST), forCondition :: AST,
            forIncr :: AST, forBody :: AST }
    | Goto { gotoTarget :: Ident } -- when can the target be bound? hmm.
    | Label { labelName :: Ident }
    | TailCall (Vector AST)
    | Return AST
    | Do (Vector AST)
  deriving (Data, Typeable)
-}

-- annotate break & continue with the loop they're break/conting
-- annotate return with the function it's returning from
-- (as ASTIdx)
-- ASTRef = (ASTIdx, AST)
-- parent ??

data ASTNodeEvalInfo
  = EvalFnCall (Vector Atom)
{-
data AST
    = List [AST]
    | Program [AST]
    | Imperative [AST]
    | EnvAST Env AST
    | Literal Atom
    | Lambda { lambdaParams :: [Ident], lambdaBody :: AST }
    | Array [AST]
    | Dict [(AST, AST)]
  deriving (Data, Typeable)
-}
-- this is the non-original-formatting-preserving version
-- also.. Strings?!
-- also a pretty-printing line could do intelligent line-breaking better.
showLList :: [String] -> String
showLList xs = "("++List.intercalate " " xs++")"
instance Show Atom where
  show Void = "#void"
  show (Number (LispyNum val exact)) = showLList [op,
              show $ numerator val, show $ denominator val]
    where op = if exact then "#/" else "#/?"
  show (Ident i) = i
  show (Boolean b) = if b then "#true" else "#false"
  show (UnboundVariable) = "#unbound-variable"
  show (BuiltinFunction name) = name
-- perhaps after-pass to remove # from any ident whose scope
-- doesn't require it to be #ed

instance Show AST where
  show (List as) = showLList (fmap show as)
  show (Program as) = List.intercalate "\n" (fmap show as)
  show (Imperative as) = showLList ("#do" : fmap show as)
  show (Literal a) = show a
  show (Lambda params body) = showLList $ ["#fn", (showLList params), show body]
  show (Array as) = showLList ("#array" : fmap show as)
  --show (EnvAST env a) = 
--instance (Show a) => Show (Located a) where
--  show (L _ a) = show a --hm

freeVarsIn :: AST -> Set Ident
freeVarsIn = para $ \ast (Set.unions -> vars) ->
  case ast of
    Lambda params _ -> Set.difference vars (Set.fromList params)
    Literal (Ident i) -> Set.insert i vars
    _ -> vars

bindFreeVars :: Env -> AST -> AST
bindFreeVars env ast =
  let free = freeVarsIn ast
  in if Set.null free
    then ast
    else EnvAST (Map.intersection env
                  (Map.fromSet (const ()) free))
                ast

schemeIdentifierChar :: P.Parser Char
schemeIdentifierChar = P.satisfy (\c -> inClass "-!$%&*+.\\/:<=>?@^_~" c
                                        || Char.isAlphaNum c)
--P.takeWhile1 for Text return

data TokenVal = LParen | RParen | Ident Text | HashIdent Text
data Token = Token
  { tokenVal :: TokenVal
  , tokenBegin :: P.SourcePos
  , tokenEnd :: P.SourcePos
  }
tokenize :: P.Parser Text


parseAtom :: P.Parser Atom
parseAtom = skipSpace >> P.choice
  [ fmap (\rat -> Number (LispyNum rat True)) rational
  , fmap (\str -> Ident str) $ many1 schemeIdentifierChar
  , fmap (const Void) $ string "#void"
  , fmap (const (Boolean True)) $ string "#true"
  , fmap (const (Boolean False)) $ string "#false"
  , fmap (const UnboundVariable) $ string "#unbound-variable"
  -- warn/fail if it's not a known builtin function?
  , fmap (\str -> BuiltinFunction str) $ P.char '#' >> many1 schemeIdentifierChar
  ]

parseSexp :: P.Parser AST
parseSexp = fmap List parseList <|> fmap Literal parseAtom

parseList :: P.Parser [AST]
parseList = do
  _ <- P.char '('
  _ <- P.skipMany P.space
  asts <- P.sepBy parseSexp (P.skipMany1 P.space)
  _ <- P.skipMany P.space
  _ <- P.char ')'
  return asts

parseProgram :: P.Parser [AST]
parseProgram = P.sepBy parseSexp (P.skipMany1 P.space)

parseLispy :: P.Parser AST
parseLispy = fmap Program parseProgram



data CanFail a = Err String | Success a deriving (Show, Functor)
instance Monad CanFail where
  return = Success
  (Success a) >>= f = f a
  (Err e) >>= _ = Err e
  fail = Err
instance Applicative CanFail where
  pure = Success
  (Success f) <*> (Success a) = Success (f a)
  (Err e) <*> _ = Err e
  _ <*> (Err e) = Err e
instance Alternative CanFail where
  empty = Err "[none]"
  (Err _) <|> m = m
  m <|> _ = m
instance M.BaseM CanFail CanFail where inBase = id




-- should env contain identity information?
-- or stateful vars should?
-- or equality checking
-- or hm @varname meaning look up in global env...
-- a way to modify the global env ?
-- Each time a stateful thing is created
-- (an array, which needs env binding)
-- put it with ID in the global env?
-- What about lambda args - can they be set! ?

-- Let's try: instead of EnvAST, anything that might need it
-- is named and added to the global environment.
{-
-- Each function e
data PathToCodeInstr = PathToCodeInstr {-GlobalEnv-} [Int]
data FunctionEvaluation = FunctionEvaluation
  Ident -- ^ name; every fn has a global name
  -- because they are a composite type.  Or
  -- should closures not have code copied
  -- and instead be referred-to extra?
  Map Int Var --[Var]
  -- ^ boxes, possibly strictly evaled, to put
  -- data in that we're evaluating.
  -- Bytecodelike they can be numbered in sequence;
  -- or they could be the [Int] tree-path each.
  -- (+ (* 1 2) 3) would have
  -- box 0: result of * 1 2
  -- box 1: result of +
  -- ..or, more stringently,
  -- box 0: 1
  -- box 1: 2
  -- box 2: * result
  -- box 3: 3
  -- box 4: + result
  -- if:
  -- (if cond (+ 1 2) (* 3 4))
  -- box 1: cond
  -- box 2: 1
  -- box 3: 2
  -- box 4: +
  -- box 5: 3
  -- box 6: 4
  -- box 7: *
  -- box 8: if                         (theta?)
  --
  -- ...actually all of these have a separate
  -- box to eval e.g. * to a function, then a box
  -- for each regular arg, then a box for the result.
  --
  -- while: (loop infinite; for i=; while; ?)
  -- (loop (+ 1 2) (fireMissiles))
  -- box 0: 1
  -- box 1: 2
  -- box 2: +
  -- box 3: fireMissiles val
  -- box 4: fireMissiles result
  -- then upon restart of loop, all boxes under loop are invalidated
  --
  -- So that closures aren't wrecked:
  -- At the moment that (fn (..) ..) is evaluated, any variables
  -- it actually closes over are taken from their boxes, named & made
  -- global, and the boxes replaced with a global Ident reference to them.
  -- Besides, (fn) creates a composite type so it will create at least
  -- one global for itself.
  --
  -- So what is a FunctionEvaluation? Also stack? Is it global?
  -- It's one of the execution pointers? It has a name?
  --
  -- f1: (+ (* 1 (if true (+ 1 1) 4)) 3) compiles to:
  --   er assuming correct strictness inference? hmm.
  --   what about passing around fns.  Is returning function 'if' allowed?
  --   ...Let's claim it's a special form and that's disallowed.
  --   Then we don't have to bind + and * here first to check them.
  --   Er hmm what.
  -- 0 <- +
  -- 1 <- *
  -- 2 <- 1
  -- 3 <- if
  -- 4 <- true
  -- 5 <- lazy (+ 1 1)
  -- 6 <- lazy 4
  -- 7 <- [enter if] 0 <- ^4
  --      [3 4 5 6]  1 <- (magic-choosing) ^5 [enter f1:(+ 1 1) aka f1:5]
  --                                                  0 <- +
  --        <-----<        <---returns val&           1 <- 1
  --                           control to             2 <- 1
  --                           f1:4;if:1---------<    3 <- [enter #+ [0 1 2], get 2]
  -- 8 <- enter #* 1 2 [1 2 7], get 2
  -- 9 <- 3
  -- 10 <- enter #+ 2 3 [0 8 9], get 5   <---<and return
  --
  --(loop (+ 1 2) (fireMissiles))
  -- 0 <- +
  -- 1 <- 1
  -- 2 <- 2
  -- 3 <- enter [0 1 2], get 3
  -- 4 <- fireMissiles
  -- 5 <- enter fireMissiles, probably get something
  -- 6 <- goto 0   [going back always erases the intervening boxes]
  --
  --(while (cond) (+ 1 2) (fireMissiles))
  -- 0 <- while
  -- 1 <- cond
  -- 2 <- enter [1]
  -- 3 <- [if 2 then ret to pre-11 else ret regularly to post-3; if hierarchical
  --                             numbering then it's step in vs step over]
  -- 4 <- +
  -- 5 <- 1
  -- 6 <- 2
  -- 7 <- enter [4 5 6], get 3
  -- 8 <- fireMissiles
  -- 9 <- enter fireMissiles, probably get something
  -- 10 <- goto 1   [going back always erases the intervening boxes]
  -- 11 <- ret #void
  --
  -- bytecode is invalid if e.g. box 11 uses box 5.
  -- that would be obvious, i suppose, if we used hierarchical numbering.
  -- otherwise it is just a thing.
  -- numbers can be converted back to code location,
  -- for showing the user and for serialization.
  
  -- Tree diff
  
  -- Should be serializable in a way someone might write.
  -- Not that we'd normally feel like doing that.  But it should be.
  -- So: (#lazy expr) or I suppose it's (#lazylet var val code).
  -- And hmm... goto?  Not strictly necessary in the absence of side
  -- effects but/er/hmm.  Even constructing a closure or array has
  -- side effects now, I guess, (though if purely functional they could
  -- be deduplicated, which only has asymptotic GC problems in more
  -- ridiculous code than Lasercake will probably have).
  --
  -- Each expr [with a nontrivial result] [that will be reused later]
  -- gets wrapped in (#at val expr)
  -- 
  -- I wonder if thing can be shown something like http://www.telescopictext.com/
  
  -- laziness is a boundary like fn is. fn, if.
-}
---}
