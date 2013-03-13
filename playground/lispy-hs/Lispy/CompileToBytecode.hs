{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, MultiParamTypeClasses,
  ViewPatterns, OverloadedStrings, FlexibleInstances #-}

module Lispy.CompileToBytecode where

import Data.Text as Text
import Data.List as List
import Data.Vector as Vector
import Data.Map.Strict as Map
import Data.Set as Set
import Data.Foldable as Foldable
import Data.Monoid
import Data.Sequence as Seq
--import Data.Maybe
--import Control.Applicative

import Lispy.Types



scopedVarsUsed :: (Foldable f) => f BytecodeInstruction -> Set VarIdx
scopedVarsUsed = foldMap (\instr -> case instr of
    CALL _ func args        -> mappend (Set.singleton func)
                                       (foldMap Set.singleton args)
    TAILCALL func args      -> mappend (Set.singleton func)
                                       (foldMap Set.singleton args)
    LITERAL _ _             -> mempty
    NAME _ originalName     -> Set.singleton originalName
    RETURN originalName     -> Set.singleton originalName
    MAKE_CLOSURE _ vars _ _ -> vars
    GOTO _                  -> mempty
    GOTO_IF_NOT _ cond      -> Set.singleton cond
  )


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

-------------------- COMPILING ---------------------

compile :: Map Text VarIdx -> Located AST -> CompiledProgram
compile builtins ast = let
  instrs = compile' (CompileScope builtins True) ast
  in CompiledProgram
    (seqToVector instrs)
    ast
    (indexAST ast)

-- We use the Data.Sequence.Seq list data structure
-- because it has O(1) cons and snoc and length,
-- and O(log n) concatenation.
--
-- A diff list that carried its length as an int
-- would probably have a much better constant factor speed wise.
-- Diff lists are also trivial to make in any language that
-- has lambdas, whereas Seq is a purely functional data structure
-- that is not as widely implemented.
-- (Alternatively, an imperative implementation could adapt this
-- code to an imperative style.)
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
          parseBinding :: Located AST -> (VarIdx, Text, Located AST)
          parseBinding (L _ (ASTList _
            (Vector.toList -> [L _ (ASTIdentifier idx varname), expr])))
            = (idx, varname, expr)
          parseBinding _ = error letrecSyntaxMsg
          parsedBindings = fmap parseBinding bindings
          bindingEnv = Map.fromList (Vector.toList (
            (fmap (\ (idx, varname, _expr) -> (varname, idx)) parsedBindings)))
          resultScope = scope { compileScopeEnv =
            (Map.union bindingEnv (compileScopeEnv scope)) }
          bindingsCode = foldMap (\(idx, _varname, expr) -> mconcat [
              compile' resultScope{compileScopeIsTailPosition=False} expr,
              instr (NAME idx (lASTIdx expr))
            ]) parsedBindings
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
        [L _ (ASTList _ paramsAST), body] -> let
          params = fmap (\(L _ (ASTIdentifier idx varname)) -> (varname, idx)) paramsAST
          bindingEnv = Map.fromList (Vector.toList params)
          bodyScope = scope {
            compileScopeEnv = (Map.union bindingEnv (compileScopeEnv scope)),
            compileScopeIsTailPosition = True
            }
          bodyCode = compile' bodyScope body
          bodyClosureUses = Set.intersection
            (Set.fromList (Map.elems (compileScopeEnv scope)))
            (scopedVarsUsed (fmap snd bodyCode))
          in mconcat [
            instr (MAKE_CLOSURE (astIdx ast) bodyClosureUses 1 (fmap snd params)),
              if compileScopeIsTailPosition scope
              then instr (RETURN (astIdx ast))
              else instr (GOTO (Seq.length bodyCode)),
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
