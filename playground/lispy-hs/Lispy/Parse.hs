{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, MultiParamTypeClasses,
  ViewPatterns, OverloadedStrings, FlexibleInstances #-}

module Lispy.Parse (parseSexp, parseLispyProgram, doParse) where

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

------------------ PARSING -----------------------

-- TODO using ByteString instead of Text would improve the asymptotic
-- complexity (Text doesn't have O(1) length or slicing)
-- but then we need to parse UTF-8 chars with parsec.
data LocText = LocText {-#UNPACK#-}!Int !Text
instance (Monad m) => P.Stream LocText m Char where
  uncons (LocText loc text) = return (fmap (fmap (LocText (loc+1))) (Text.uncons text))
  {-# INLINE uncons #-}

type LispyParser = P.Parsec LocText ASTIdx

parseComment :: LispyParser ()
parseComment = (P.char ';' >> P.skipMany (P.noneOf "\n\r")) P.<?> "comment"

parseWhitespaceAndComments :: LispyParser ()
parseWhitespaceAndComments =
  P.skipMany (P.skipMany1 P.space <|> parseComment)

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
parseSexp = P.between parseWhitespaceAndComments parseWhitespaceAndComments
  (parseWithLocation (parseAtom <|> parseList) P.<?> "s-expression")

parseWithLocation :: LispyParser a -> LispyParser (Located a)
parseWithLocation parser = do
  beginLoc <- P.getPosition
  LocText beginCharIdx beginText <- P.getInput

  parsed <- parser

  endLoc <- P.getPosition
  LocText endCharIdx _ <- P.getInput
  let info = SourceLocInfo
        (Text.take (endCharIdx - beginCharIdx) beginText)
        beginLoc
        endLoc

  return (L info parsed)

parseLispyProgram :: LispyParser (Located AST)
parseLispyProgram = do
  idx <- parserNextASTIdx
  fmap (fmap (ASTList idx . Vector.fromList))
    (parseWithLocation (P.many parseSexp))

doParse :: LispyParser a -> P.SourceName -> Text -> Either P.ParseError a
doParse parser sourceName text = let
    fullParser = do
      result <- parser
      P.eof
      return result
  in
  P.runParser fullParser 0 sourceName (LocText 0 text)




