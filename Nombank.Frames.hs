{-# LANGUAGE OverloadedStrings #-}

module Nombank.Frames (getRoles) where

import Prelude hiding (readFile)
import Text.XML (readFile, def)
import Text.XML.Cursor (descendant, element, attributeIs, ($//), ($/),
                        attribute, fromDocument, child, Cursor)
import Data.Maybe (fromMaybe)
import Data.Text (pack, unpack)

    
type Lemma = String
type Sense = Int
type Name = String

data Role = RoleT (Int, String, String) | Role (Int, String)
          deriving (Show)

-- The return type of a search contains a frame with a description of the
-- roles of the participants in a Nombank proposition.
newtype Frame = Frame (Lemma, Name, [Role])
    deriving (Show)

frames = "/home/manu/Dropbox/research/utils/NomBank/frames/"

getRoles :: Lemma -> Sense -> IO Frame
getRoles l s = do
  cursor <- readFrame l frames
  let lemma = unpack (head (cursor $/ attribute "lemma"))
              
  let roleset = descendant cursor
                >>= attributeIs "id" (pack (l ++ ".0" ++ (show s)))
  let name = unpack (head (attribute "name" =<< roleset))
  let roles = roleset >>= descendant >>= element "role"
  
  return $ Frame (lemma, name, map getInfo roles)

getInfo :: Cursor -> Role
getInfo role
    | (not . null) vntheta = RoleT (n, descr, unpack (head (vntheta)))
    | otherwise = Role (n, descr)
    where n = read (unpack (head (attribute "n" role)))
          descr = unpack (head (attribute "descr" role))
          vntheta = role $// attribute "vntheta"

readFrame :: Lemma -> FilePath -> IO Cursor
readFrame lemma filepath = do
  doc <- readFile def (filepath ++ lemma ++ ".xml")
  return $ fromDocument doc
