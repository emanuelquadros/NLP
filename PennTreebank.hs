-- BSD3 licensed
-- Eric Kow <E.Y.Kow@brighton.ac.uk>

module NLP.PennTreebank (parseTree) where

import Data.Tree
import Text.ParserCombinators.Parsec hiding (spaces)

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

parseNode :: Parser (Tree String)
parseNode = do char '('
               n <- parseString
               many space
               kids <- sepEndBy1 parseTree (many space)
               char ')'
               return $  Node n kids

parseLeaf :: Parser (Tree String)
parseLeaf = leaf `fmap` parseString

parseTree :: Parser (Tree String)
parseTree = parseLeaf <|> parseNode
