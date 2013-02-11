{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, MultiParamTypeClasses,
  ViewPatterns, LambdaCase #-}

module Lispy where

import Data.Text as Text
import Data.Ratio
import Data.List as List
import Data.Map.Strict as Map
import Data.Set as Set
import Data.Generics.Uniplate.Data
import Data.Data
import Data.Attoparsec.Text as P
import Control.Applicative
import MonadLib as M

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

data PathToCodeLiteral = PathToCodeLiteral Ident [Int]
getCodeLiteralE :: Env -> PathToCodeLiteral -> AST
getCodeLiteralE env (PathToCodeLiteral i path) = Map.lookup i env

getCodeLiteralP :: [Int] -> AST -> AST
getCodeLiteralP [] ast = ast
getCodeLiteralP (idx:is) ast = getCodeLiteralP is (getChildNumber idx ast)

getChildNumber :: Int -> AST -> AST
getChildNumber i ast = children ast !! i

data Stack = 


type Env = Map Ident LAST
-- this monad could also 'tick' and when it runs
-- out of ticks, return.  except that moment won't
-- be serializable?
type EvalM = ReaderT Env CanFail

evalToNumber :: LAST -> EvalM LispyNum
evalToNumber = undefined
evalToBool :: LAST -> EvalM Bool
evalToBool = undefined
evalToAtom :: LAST -> EvalM Atom --I am lazy with implementing =
evalToAtom = undefined
eval :: LAST -> EvalM LAST --TODO return free var env needed?
eval = undefined

builtinFunctions :: Map Ident ([LAST] -> EvalM LAST)
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
  
  , ("if", \case
      [_, cond, t, f] -> do
        b <- bool cond
        return $! if b then t else f
      ast -> arityFail ast)
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
    unary op extr = \case [_,a] -> (noL . Literal . op) <$> extr a; ast -> arityFail ast
    binary constr op extr = \case [_,a,b] -> (noL . Literal . constr) <$> (op <$> extr a <*> extr b); ast -> arityFail ast

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

data Atom
    = Void
    | Number LispyNum
    | Ident Ident
    | Boolean Bool
    | UnboundVariable
    | BuiltinFunction { atomName :: String }--, atomFnVal :: LAST -> LAST }
  deriving (Eq, Ord, Data, Typeable)--, Show, Read)
--instance Eq Atom where
--  Void == Void = True
--  Number a b == Number a b = a

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

data AST
    = List [LAST]
    | Program [LAST]
    | Imperative [LAST]
    | EnvAST Env LAST
    | Literal Atom
    | Lambda { lambdaParams :: [Ident], lambdaBody :: LAST }
    | Array [LAST]
    | Dict [(LAST, LAST)]
  deriving (Data, Typeable)

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
  show (List as) = showLList (fmap (show.unL) as)
  show (Program as) = List.intercalate "\n" (fmap (show.unL) as)
  show (Imperative as) = showLList ("#do" : fmap (show.unL) as)
  show (Literal a) = show a
  show (Lambda params body) = showLList $ ["#fn", (showLList params), show (unL body)]
  show (Array as) = showLList ("#array" : fmap (show.unL) as)
  show (EnvAST env a) = 
instance (Show a) => Show (Located a) where
  show (L _ a) = show a --hm

freeVarsIn :: AST -> Set Ident
freeVarsIn = para $ \ast (Set.unions -> vars) ->
  case ast of
    Lambda params _ -> Set.difference vars (Set.fromList params)
    Literal (Ident i) -> Set.insert i vars
    _ -> vars

bindFreeVars :: Env -> LAST -> LAST
bindFreeVars env lAST@(L l ast) =
  let free = freeVarsIn ast
  in if Set.null free
    then lAST
    else L l $ EnvAST (Map.intersection env
                        (Map.fromSet (const ()) free))
                      lAST

schemeIdentifierChar :: P.Parser Char
schemeIdentifierChar = P.satisfy (\c -> inClass "-!$%&*+.\\/:<=>?@^_~" c
                                        || isAlphaNum c)
--P.takeWhile1 for Text return

parseAtom :: P.Parser Atom
parseAtom = skipSpace >> P.choice
  [ fmap (\rat -> LispyNum rat True) rational
  , fmap (\str -> Ident str) $ many1 schemeIdentifierChar
  , fmap (const Void) $ string "#void"
  , fmap (const (Boolean True)) $ string "#true"
  , fmap (const (Boolean False)) $ string "#false"
  , fmap (const UnboundVariable) $ string "#unbound-variable"
  -- warn/fail if it's not a known builtin function?
  , fmap (\str -> BuiltinFunction str) $ P.char '#' >> many1 schemeIdentifierChar
  ]

parseSexp :: P.Parser AST
parseSexp = parseList <|> parseAtom

parseList :: P.Parser [AST]
parseList = do
  P.char '('
  asts <- P.sepBy parseSexp (P.skipMany1 P.space)
  P.char ')'
  return (asts
  

parseLispy :: Text -> P.Result AST
parseLispy = undefined



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
