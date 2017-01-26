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

data Operator = ParentOf | ChildOf | SisterOf
                 deriving (Show, Eq)

languageDef = emptyDef { Token.reservedOpNames = ["<", ">", "$"] }

lexer = Token.makeTokenParser languageDef
reservedOp = Token.reservedOp lexer

parens = between (char '(') (char ')')
--brackets = between (char '[') (char ']')

parsePattern :: Parser Tpattern
parsePattern = do
  spaces
  pattern <- parens parsePattern
             <|> try (fmap Node parseNode <* (spaces >> eof))
             <|> do {
                   node <- parseNode;
                   spaces;
                   relations <- many parseRelation;
                   return (Tpattern node relations)
                 }
  return pattern

parseRelation :: Parser Relation
parseRelation = do
  spaces
  op <- parseOperator
  spaces
  subpattern <- parens parsePattern <|> fmap Node parseNode
  return (Relation op subpattern)

parseNode :: Parser String
parseNode = do
  node <- many1 alphaNum
  return node

parseOperator :: Parser Operator
parseOperator = do
  (reservedOp "<" >> return ParentOf)
  <|> (reservedOp ">" >> return ChildOf)
