module NLP.PennTreebank.Query (searchPattern) where

import NLP.Tgrep2 (parsePattern, Tpattern(..))
import NLP.PennTreebank (parseTree)
import Data.String.Utils (strip)
import Data.Tree (Tree(..))
import Data.Tree.Zipper (isLeaf, fromTree)
import Data.List (elemIndex, intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Text.Parsec (parse, many)
import Text.Parsec.Error (ParseError)
import System.IO (FilePath, hGetContents, hClose, openFile, IOMode( ReadMode ) )
import Utils.Dir (getFiles, lastN)
import Control.DeepSeq (deepseq)

data Result = Result { file :: String
                     , treeNumber :: Int
                     , subtree :: Tree String
                     } deriving (Show)

data LabeledTree = LTree { fileid :: String
                         , pos :: Int
                         , tree :: Tree String
                         } deriving (Show)


searchPattern :: String -> FilePath -> IO [LabeledTree]
searchPattern str basedir = do
  let p = parse parsePattern "" str
  forest <- buildForest basedir
  case p of
    Right (Head node) -> queryHead (Head node) forest
    Left err -> error ("Check the syntax of your query.\n" ++ (show err))

queryHead :: Tpattern -> [LabeledTree] -> IO [LabeledTree]
queryHead pattern@(Head node) ltrees = do
    return (filter (hasNode node) ltrees)

hasNode :: String -> LabeledTree -> Bool
hasNode node ltree = any (elem node)
                     (filter nonTerminal (subTrees (tree ltree)))

nonTerminal :: Tree a -> Bool
nonTerminal = not . isLeaf . fromTree

-- Wrapper around the parser, calling it as many times as possible on
-- a string, and returning a list.
parseText :: FilePath -> String -> [Tree String]
parseText path text = case parse (many parseTree) "" (strip text) of
                        Left err -> error $ "Input:\n" ++ show path ++
                                    "\nError:\n" ++ show err
                        Right result -> result

buildForest :: FilePath -> IO [LabeledTree]
buildForest basedir = do
  trees <- mapM parseFile =<< getFiles basedir
  return (concat trees)

-- get all subtrees of a tree, where `subtree-of'
-- is taken to be a reflexive relation.
subTrees :: Eq a => Tree a -> [Tree a]
subTrees t
    | [] == (subForest t) = [t]
    | otherwise = t:foldl1 (++) (map subTrees kids)
    where
      kids = subForest t

-- Send the content of a file to parseText
parseFile :: FilePath -> IO [LabeledTree]
parseFile f = do
  h <- openFile f ReadMode
  contents <- hGetContents h
  deepseq contents (hClose h)

  let ltrees = parseText f (strip contents)

  -- We need to return trees labeled with a file id and their position in the
  -- file.
  let labelTree = \e -> LTree { fileid = getFileId f
                              , pos = fromMaybe 0 (elemIndex e ltrees)
                              , tree = e}
  
  return $ map labelTree ltrees

-- helper function to get just an ID from a filepath
getFileId :: FilePath -> String
getFileId path = intercalate "/" (lastN 3 (splitOn "/" path))
