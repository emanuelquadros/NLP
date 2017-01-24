module Nombank (parseNom, lemma, Nom) where

import Text.Parsec (many, many1, skipMany1, digit, char, noneOf)
import Text.ParserCombinators.Parsec.Prim (Parser, (<|>), try)
import Text.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as Token


--- This a sample Nombank entry:
---
---    wsj/17/wsj_1734.mrg 3 49 equity 01 44:2-ARG2 49:0-rel
---
--- where each field is separated by a whitespace.

data Nom = Nom { file :: String,
                 tree :: Int,
                 token :: Int,
                 lemma :: String,
                 sense :: Int,
                 pieces :: [Piece]
               } deriving (Eq, Show)

data Piece = Piece { pointers :: Pointers,
                     label :: String
                   } deriving (Eq, Show)

data Pointers = Pointer { ptoken :: Int,
                          level :: Int
                        }
              | ConcatPointer [Pointers]
              | PointerChain [Pointers] deriving (Eq, Show)
-------------------------------------------------

--- lexer, just to make the code more compact
lexer = Token.makeTokenParser emptyDef

lexeme = Token.lexeme lexer
----------------------------------------------

parseNom :: Parser Nom
parseNom = do
  file <- lexeme parseString
  tree <- read <$> lexeme (many1 digit)
  token <- read <$> lexeme (many1 digit)
  lemma <- lexeme parseString
  sense <- read <$> lexeme (many1 digit)
  labels <- many parsePieces
  return (Nom file tree token lemma sense labels)

parseString :: Parser String
parseString = do
  s <- many1 $ noneOf " \n"
  return s

parsePieces :: Parser Piece
parsePieces = do
  ps <- parsePointers
  label <- lexeme parseString
  return (Piece ps label)

pointer :: Parser Pointers
pointer = do
  ptoken <- read <$> many1 digit
  char ':'
  level <- read <$> many1 digit
  return (Pointer ptoken level)

parsePointers :: Parser Pointers
parsePointers = do
  p <- try (pointer <* (char '-'))
          <|> try parseConcat
          <|> parseChain
  return p

parseConcat :: Parser Pointers
parseConcat = do
  ps <- many1 (pointer <* (try (char ',') <|> char '-'))
  return (ConcatPointer ps)
  
parseChain :: Parser Pointers
parseChain = do
  ps <- many1 (pointer <* (try (char '*') <|> char '-'))
  return (PointerChain ps)
