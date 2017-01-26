module PennTreebank.Query (searchPattern, LabeledTree(..), getTree) where

import Tgrep2 (parsePattern, Tpattern(..), Relation(..), Operator(..))
import PennTreebank (parseTree)
import Data.String.Utils (strip)
import Data.Tree (Tree(rootLabel, subForest), Forest)
import Data.Tree.Zipper (isLeaf, fromTree, toTree, forest, children, next, tree,
                         nextSpace, nextTree, label, firstChild, TreePos(..),
                         Full, Empty)
import Data.List (elemIndex, intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Text.Parsec (parse, many)
import Text.Parsec.Error (ParseError)
import System.IO (FilePath, hGetContents, hClose, openFile, IOMode( ReadMode ) )
import Utils.Dir (getFiles, lastN)
import Control.DeepSeq (deepseq)

data LabeledTree = LTree { fileid :: String
                         , treeIndex :: Int
                         , tokenIndex :: Int
                         , treepos :: TreePos Full String
                         }
                   deriving (Show)

type LTreeFilter = [LabeledTree] -> [LabeledTree]
type Terminal = TreePos Full String
type Subtree = TreePos Full String

getTree :: LabeledTree -> TreePos Full String
getTree = treepos

searchPattern :: String -> FilePath -> IO [LabeledTree]
searchPattern str basedir = do
  forest <- buildForest basedir
  let lforest = concat (map labeledSubTrees forest)
  case (parse parsePattern "" str) of
    Right pattern -> do
      return ((patternFilter pattern) lforest)
    Left err -> error ("Check the syntax of your query.\n" ++ (show err))

labeledSubTrees :: LabeledTree -> [LabeledTree]
labeledSubTrees ltree = propagateLabels ltree (subTrees (treepos ltree))

propagateLabels :: LabeledTree -> [TreePos Full String] -> [LabeledTree]
propagateLabels ltree sub = map (labelSubTree ltree) sub

labelSubTree :: LabeledTree -> Subtree -> LabeledTree
labelSubTree ltree tr = LTree { fileid = fileid ltree,
                                treeIndex = treeIndex ltree,
                                tokenIndex =
                                    findTerminal (firstTerminal tr)
                                                     (treepos ltree),
                                treepos = tr }

findTerminal :: Terminal -> Subtree -> Int
findTerminal term nt = fromMaybe 0 (elemIndex term (terminals nt))

firstTerminal :: Subtree -> Terminal
firstTerminal = head . terminals

patternFilter :: Tpattern -> LTreeFilter
patternFilter (Node l) = filter (\lt -> l == label (treepos lt))
patternFilter (Tpattern label relations) =
    foldl1 (.) (map (relationFilter (Node label)) relations)

relationFilter :: Tpattern -> Relation -> LTreeFilter
relationFilter node (Relation op pattern) =
    case op of
      -- immediate dominance filters
      ParentOf -> hasChild pattern . patternFilter node
      ChildOf -> hasChild node . patternFilter pattern

     -- SisterOf -> 

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

-- Gets all subtrees and propagates labels from the  main tree.
lSubForest :: LabeledTree -> [LabeledTree]
lSubForest ltree = propagateLabels ltree (posForest (treepos ltree))

-- Wrapper around the parser, calling it as many times as possible on
-- a string, and returning a list.
-- The filename argument is for debugging purposes only.
parseText :: FilePath -> String -> [TreePos Full String]
parseText path text = case parse (many parseTree) "" (strip text) of
                        Left err -> error $ "Input:\n" ++ show path ++
                                    "\nError:\n" ++ show err
                        Right result -> map fromTree result

buildForest :: FilePath -> IO [LabeledTree]
buildForest basedir = do
  trees <- mapM parseFile =<< getFiles basedir
  return (concat trees)

-- get all subtrees of a tree, where `subtree-of'
-- is not taken to be a reflexive relation.
subTrees :: TreePos Full String -> [Subtree]
subTrees t = foldl (++) [] (map subTrees' (posForest t))
    where subTrees' sub = sub:foldl (++) [] (map subTrees' kids)
              where kids = filter (not . isLeaf) (posForest sub)
                           
terminals :: Subtree -> [Terminal]
terminals t
    | isLeaf t = [t]
    | otherwise = foldl (++) [] (map terminals (posForest t))

-- Returns the location of all the trees found at a given location.
posForest :: TreePos Full String -> [TreePos Full String]
posForest start = case firstChild start of
                    Just pos -> pos:go pos
                    Nothing -> []
    where go pt = case next pt of
                    Just t -> t:go t
                    Nothing -> []

-- Send the content of a file to parseText
parseFile :: FilePath -> IO [LabeledTree]
parseFile f = do
  h <- openFile f ReadMode
  contents <- hGetContents h
  deepseq contents (hClose h)

  let trees = parseText f (strip contents)
  return $ map (labelTree f trees) trees

labelTree :: FilePath -> [Subtree] -> Subtree -> LabeledTree
labelTree file trees tr = LTree { fileid = getFileId file,
                                  treeIndex = fromMaybe 0 (elemIndex tr trees),
                                  tokenIndex = 0,
                                  treepos = tr }
                                              
-- helper function to get just an ID from a filepath
getFileId :: FilePath -> String
getFileId path = intercalate "/" (lastN 3 (splitOn "/" path))
