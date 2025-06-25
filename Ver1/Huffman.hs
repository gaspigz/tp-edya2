module Huffman where

import Data.Map as DM (Map, fromList)
import Heap

{-
Integrantes grupo:
- 
- 
- 

-}

-- Bits y códigos

data Bit = Zero | One deriving (Eq, Show)

type Code = [Bit]

-- Árbol de codificación

data HTree = Leaf Char Int
           | Node HTree HTree Int
           deriving Show

weight :: HTree -> Int
weight (Leaf _ w)   = w
weight (Node _ _ w) = w

-- Diccionarios de frecuencias y códigos

type FreqMap = Map Char Int

type CodeMap = Map Char Code


-- Ejercicio 1


-- Ejercicio 2

buildFreqMap :: String -> FreqMap
buildFreqMap = undefined

-- Ejercicio 3

buildHTree :: FreqMap -> HTree
buildHTree = undefined

-- Ejercicio 4

buildCodeMap :: HTree -> CodeMap
buildCodeMap = undefined

-- Ejercicio 5

encode :: CodeMap -> String -> Code
encode = undefined

-- Ejercicio 6

decode :: HTree -> Code -> String
decode = undefined

-- Ejercicio 7

engFM :: FreqMap
engFM = fromList [
    ('a', 691),
    ('b', 126),
    ('c', 235),
    ('d', 360),
    ('e', 1074),
    ('f', 188),
    ('g', 170),
    ('h', 515),
    ('i', 589),
    ('j', 13),
    ('k', 65),
    ('l', 340),
    ('m', 203),
    ('n', 571),
    ('o', 635),
    ('p', 163),
    ('q', 8),
    ('r', 506),
    ('s', 535),
    ('t', 766),
    ('u', 233),
    ('v', 83),
    ('w', 200),
    ('x', 13),
    ('y', 167),
    ('z', 6),
    (' ', 1370),
    (',', 84),
    ('.', 89)
    ]
