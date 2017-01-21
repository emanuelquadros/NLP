module NLP.PennTreebank.Query (searchPattern) where

import Tgrep2 (parsePattern, Tpattern(..), Relation(..), Operator(..))
import NLP.PennTreebank (parseTree)
import Data.String.Utils (strip)
import Data.Tree (Tree(rootLabel, subForest))
import Data.Tree.Zipper (isLeaf, fromTree, toTree)
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
                         , tokenIndex :: Int
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
labeledSubTrees ltree = propagateLabels ltree (subTrees (tree ltree))

propagateLabels :: LabeledTree -> [Tree String] -> [LabeledTree]
propagateLabels ltree sub = map (labelSubTree ltree) sub

-- The trickiest part is labeling subtrees according to where they appear inside
-- the mother tree. This label refers to the position of the first terminal
-- of the subtree in relation to all terminal elements in the mother tree.
labelSubTree :: LabeledTree -> Tree String -> LabeledTree
labelSubTree ltree tr = LTree { fileid = fileid ltree,
                                pos = pos ltree,
                                tokenIndex =
                                    findTerminal (firstTerminal tr) (tree ltree),
                                tree = tr }

findTerminal :: Eq a => Tree a -> Tree a -> Int
findTerminal term nt = fromMaybe 0 (elemIndex term (terminals nt))

-- This helper will return the first terminal element from a tree/subtree.
firstTerminal :: Eq a => Tree a -> Tree a
firstTerminal = head . terminals

terminals :: Eq a => Tree a -> [Tree a]
terminals = (filter isTerminal) . subTrees
                      
patternFilter :: Tpattern -> LTreeFilter
patternFilter (Node label) = filter (\lt -> rootLabel (tree lt) == label)
patternFilter (Tpattern label relations) =
    foldl1 (.) (map (relationFilter (Node label)) relations)

relationFilter :: Tpattern -> Relation -> LTreeFilter
relationFilter node (Relation op pattern) =
    case op of
      -- immediate dominance filters
      ParentOf -> hasChild pattern . patternFilter node
      ChildOf -> hasChild node . patternFilter pattern

      -- dominance
--      AncestorOf -> undefined
--      DescendantOf -> undefined

--- Filters --------------------------------------------------------------------

hasChild :: Tpattern -> LTreeFilter
hasChild pattern =
    filter (\lt -> not $ null $ (patternFilter pattern) (lSubForest lt))

-- This filter must return subtrees that have a certain pattern (a node or a
-- relation) among their descendants.
hasDescendant :: Tpattern -> LTreeFilter
hasDescendant pattern =
    filter (\lt -> not $ null $ (patternFilter pattern) (labeledSubTrees lt))

--------------------------------------------------------------------------------

-- Calls the Data.Tree subForest fuction, but propagates the labels from the
-- main tree.
lSubForest :: LabeledTree -> [LabeledTree]
lSubForest ltree = propagateLabels ltree (subForest (tree ltree))

isTerminal :: Tree a -> Bool
isTerminal = isLeaf . fromTree

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
    where subTrees' sub = sub:foldl (++) [] (map subTrees' (subForest sub))

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
                              , tokenIndex = 0
                              , tree = e}
  
  return $ map labelTree ltrees

-- helper function to get just an ID from a filepath
getFileId :: FilePath -> String
getFileId path = intercalate "/" (lastN 3 (splitOn "/" path))
