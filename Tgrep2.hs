module Tgrep2 (parsePattern, Tpattern(..), Relation(..), Operator(..)) where

import Text.Parsec (eof, many1, many)
import Text.Parsec.Char (alphaNum, spaces, char)
import Text.Parsec.Combinator (between)
import Text.ParserCombinators.ReadP (manyTill)
import Text.ParserCombinators.Parsec.Prim (Parser, try, (<|>))
import Text.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as Token

data Tpattern = Node { label :: String }
              | Tpattern { label :: String,
                           relations :: [Relation]
                         }
                deriving (Show)

data Relation = Conj Relation Relation
              | Disj Relation Relation
              | Relation Operator Tpattern
                deriving (Show)

data Operator = ParentOf | ChildOf
                 deriving (Show, Eq)

languageDef = emptyDef { Token.reservedOpNames = ["<", ">"] }

lexer = Token.makeTokenParser languageDef
reservedOp = Token.reservedOp lexer

parsePattern :: Parser Tpattern
parsePattern = do
  spaces
  pattern <- try (fmap Head parseNode <* (spaces >> eof))
             <|> do {
                   node <- parseNode;
                   spaces;
                   op <- parseOperator;
                   subpattern <- parsePattern;
                   return (Tpattern node op subpattern)
                 }
  return pattern

parseNode :: Parser Node
parseNode = do
  node <- many1 alphaNum
  return node

parseOperator :: Parser Operator
parseOperator = do
  (reservedOp "<" >> return ParentOf)
  <|> (reservedOp ">" >> return ChildOf)
