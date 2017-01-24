-- Partial rewrite of Eric Kow's original code at
-- https://hackage.haskell.org/package/penn-treebank

-- Original license statement:
-- BSD3 licensed
-- Eric Kow <E.Y.Kow@brighton.ac.uk>

module PennTreebank (parseTree) where

import Data.Tree
import Text.ParserCombinators.Parsec hiding (spaces, try)
import Debug.Trace
import Text.Parsec hiding (try)
import Text.Parsec.Prim
import Data.Functor.Identity
import Text.Parsec.Token

println msg = trace (show msg) $ return ()

seeNext :: Int -> ParsecT String u Identity ()
seeNext n = do
  s <- getParserState
  let out = take n (stateInput s)
  println out

spaces :: Parser ()
spaces = skipMany1 space

leaf :: a -> Tree a
leaf x = Node x []

parseString :: Parser String
parseString =
 do s <- many1 $ noneOf "() \t\v\n"
    return $ case s of
              "-LRB-" -> "("
              "-RRB-" -> ")"
              _       -> s

parseNode' :: Parser (Tree String)
parseNode' = do
  n <- parseString
  many space
  kids <- sepEndBy1 parseTree (many space)
  return $ Node n kids

parseNode :: Parser (Tree String)
parseNode = choice [try (between (char '(') (char ')') parseNode'),
                    try (between (string "((") (string "))") parseNode'),
                    between start end parseTree]
    where
      start = sequence [char '(', space]
      end = choice [try (sequence [char ')', (eof >> return ' ')]),
                    sequence [char ')', space]]

parseLeaf :: Parser (Tree String)
parseLeaf = leaf `fmap` parseString

parseTree :: Parser (Tree String)
parseTree = parseLeaf <|> parseNode
