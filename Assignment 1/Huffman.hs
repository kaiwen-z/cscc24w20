module Huffman where

import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import           LeftistHeap (PQueue)
import qualified LeftistHeap as PQueue

import           HuffmanDef

-- Given a list of booleans, decode the huffman tree
-- into a string i.e. a list of chars
decode :: HuffmanTree -> [Bool] -> [Char]
decode (Leaf value char) [] = [char]
decode htree (b:bs) = decode htree (b:bs)
  where
    decode (Branch value left right) [] = []
    decode (Leaf value char) bs = char : decode htree bs
    decode htree (False:bs) = decode (getleft htree) bs
    decode htree (True:bs) = decode (getright htree) bs

-- Helper function to get the left tree
getleft :: HuffmanTree -> HuffmanTree
getleft (Branch value left right) = left

-- Helper function to get the right tree
getright :: HuffmanTree -> HuffmanTree
getright (Branch value left right) = right

-- Given a list of character frequencies, construct a
-- corrosponding huffman tree
huffmanTree :: [(Char, Int)] -> HuffmanTree
huffmanTree (x:xs) = prepareQueue (initQ (x:xs))

-- Populate a PQueue with all of the letter frequencies
prepareQueue :: PQueue Int HuffmanTree -> HuffmanTree
prepareQueue pq = buildTree (getq pq)

-- Build the huffman tree
buildTree :: [HuffmanTree] -> HuffmanTree
buildTree [htree] = htree
buildTree [htree1, htree2] = mergeTree htree1 htree2
buildTree (a:b:xs) = buildTree ((mergeTree a b) : xs)

-- Extract all items from the PQueue into an easier managed
-- list of leaf nodes.
getq :: PQueue Int HuffmanTree -> [HuffmanTree]
getq pq =
  case qvalue of
      Nothing -> []
      Just (_, (Leaf _ _)) ->
        let ltree = qvalue
          in (getTree ltree) : (getq (getNewQ ltree))
      Just (_, (Branch _ (Leaf _ _) (Leaf _ _))) ->
        let btree = qvalue
          in (getTree btree) : (getq (getNewQ btree))
  where qvalue = PQueue.extractMin pq

-- Get the new queue returned by extractMin
getNewQ :: Maybe (PQueue Int HuffmanTree, HuffmanTree) -> PQueue Int HuffmanTree
getNewQ (Just (newQ, (Leaf val char))) = newQ
getNewQ (Just (newQ, (Branch val htree1 htree2))) = newQ

-- Get the huffman tree objects returned by extractMin
getTree :: Maybe (PQueue Int HuffmanTree, HuffmanTree) -> HuffmanTree
getTree (Just (newQ, (Leaf val char))) = (Leaf val char)
getTree (Just (newQ, (Branch val htree1 htree2))) = (Branch val htree1 htree2)

-- init a new pqueue of leaf
initQ :: [(Char, Int)] -> PQueue Int HuffmanTree
initQ [] = PQueue.empty
initQ (x:xs) = PQueue.insert (snd x) (Leaf (snd x) (fst x)) (initQ xs)

-- makes new branch and merge trees
mergeTree :: HuffmanTree -> HuffmanTree -> HuffmanTree
mergeTree  t1 t2
  | (getFreq t1 <= getFreq t2) = (Branch (getFreq t1 + getFreq t2) t1 t2)
  | (getFreq t1 > getFreq t2) = (Branch (getFreq t1 + getFreq t2) t2 t1)


-- Build dict of chars to their path to the leaf they are in
buildDict :: HuffmanTree -> Map Char [Bool]
buildDict htree = Map.fromList (getDList htree (getChars htree))

-- for all chars in list, get their bools
getDList :: HuffmanTree -> [Char] -> [(Char,[Bool])]
getDList htree [] = []
getDList htree (c:cs) = (c,(getBools htree c)) : getDList htree cs

-- for a char get the list of bool from the root node
getBools :: HuffmanTree -> Char -> [Bool]
getBools (Leaf val char) c = []
getBools (Branch val left right) c
  | (False == (containsChar left c)) = True : getBools right c
  | otherwise = False : getBools left c

-- get list of chars in htree
getChars :: HuffmanTree -> [Char]
getChars (Leaf val char) = [char]
getChars (Branch val left right) = getChars left ++ getChars right

-- check if subtree contains char
containsChar :: HuffmanTree -> Char -> Bool
containsChar (Leaf val char) c
  | char == c = True
  | otherwise = False
containsChar (Branch val htree1 htree2) c = 
  containsChar htree1 c || containsChar htree2 c

-- Encode
encode :: HuffmanTree -> [Char] -> [Bool]
encode tree cs = concatMap (\c -> dict ! c) cs
  where
    dict = buildDict tree
