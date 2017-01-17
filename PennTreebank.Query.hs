module NLP.PennTreebank.Query (searchPattern) where

import Tgrep2 (parsePattern, Tpattern(..), Relation(..), Operator(..))
import NLP.PennTreebank (parseTree)
import Data.String.Utils (strip)
import Data.Tree (Tree(rootLabel, subForest))
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

type LTreeFilter = [LabeledTree] -> [LabeledTree]

searchPattern :: String -> FilePath -> IO [LabeledTree]
searchPattern str basedir = do
  forest <- buildForest basedir
  let lforest = concat (map labeledSubTrees forest)
  case (parse parsePattern "" str) of
    Right pattern -> do
      return ((patternFilter pattern) lforest)
    Left err -> error ("Check the syntax of your query.\n" ++ (show err))

labeledSubTrees :: LabeledTree -> [LabeledTree]
labeledSubTrees ltree = map labelTree (subTrees (tree ltree))
    where labelTree = \t -> LTree { fileid = fileid ltree,
                                    pos = pos ltree,
                                    tree = t }

patternFilter :: Tpattern -> LTreeFilter
patternFilter (Node label) = filter (\lt -> rootLabel (tree lt) == label)
patternFilter (Tpattern label relations) =
    foldl1 (.) (map (relationFilter (Node label)) relations)

relationFilter :: Tpattern -> Relation -> LTreeFilter
relationFilter node (Relation op pattern) =
    case op of
      ParentOf -> filter (\lt -> not $ null
                                 ((patternFilter pattern) (labeledSubTrees lt)))
                  . (patternFilter node)

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
-- is not taken to be a reflexive relation.
subTrees :: Eq a => Tree a -> [Tree a]
subTrees t = foldl (++) [] (map subTrees' (subForest t))
    where subTrees' sub = sub:foldl (++) [] (map subTrees' kids)
              where kids = filter (not . isLeaf . fromTree) (subForest sub)

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
