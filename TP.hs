import Heap
import qualified Data.Map as Map



-- Definimos el tipo de dato que nos permite representar árboles de Huffman

data HTree = Leaf Char Int | Node HTree HTree Int deriving Show


-- PREGUNTAR CUANDO DOS HTree SON IGUALES!

-- 1. Dar una instancia de la clase Ord para el tipo HTree. Para ello, primero debe dar una instancia de la clase Eq

instance Eq HTree where
  (==) (Leaf c1 w1) (Leaf c2 w2)           = (c1 == c2) && (w1 == w2)
  (==) (Leaf _ _) (Node _ _ _)             = False
  (==) (Node _ _ _) (Leaf _ _)             = False
  (==) (Node ht1 ht2 w1) (Node ht3 ht4 w2) = (ht1 == ht3) && (ht2 == ht4) && (w1 == w2)

  (/=) ht1 ht2 = not (ht1 == ht2)


instance Ord HTree where
  (<=) (Leaf _ w1) (Leaf _ w2)           = (w1 <= w2)
  (<=) (Leaf _ w1) (Node _ _ w2)         = (w1 <= w2)
  (<=) (Node _ _ w1) (Leaf _ w2)         = (w1 <= w2)
  (<=) (Node _ _ w1) (Node _ _ w2)       = (w1 <= w2)

  (<) (Leaf _ w1) (Leaf _ w2)           = (w1 < w2)
  (<) (Leaf _ w1) (Node _ _ w2)         = (w1 < w2)
  (<) (Node _ _ w1) (Leaf _ w2)         = (w1 < w2)
  (<) (Node _ _ w1) (Node _ _ w2)       = (w1 < w2)

  (>) ht1 ht2 = not (ht1 <= ht2)

  (>=) ht1 ht2 = not (ht1 < ht2)

  (max) ht1 ht2 = if (ht1 <= ht2) then ht2 else ht1

  (min) ht1 ht2 = if (ht1 <= ht2) then ht1 else ht2
  

{- 2. Definir una función buildFreqMap :: String -> FreqMap que dado un string, compute la cantidad de apariciones
  de cada uno de sus caracteres. Sugerencia: la función insertWith de Data.Map puede ser de utilidad.
-}

type FreqMap = Map.Map Char Int

build_FreqMap_aux :: FreqMap -> String -> FreqMap
build_FreqMap_aux m "" = m
build_FreqMap_aux m (x:xs) = build_FreqMap_aux (Map.insertWith (+) x 1 m) xs

build_FreqMap :: String -> FreqMap
build_FreqMap str = build_FreqMap_aux Map.empty str


{- 3. Definir una función buildHTree :: FreqMap -> HTree que compute un árbol de codificación óptimo para los
  símbolos y su cantidad de apariciones dados, siguiendo el algoritmo descripto anteriormente. Para implementarlo
  eficientemente, use un heap para tener acceso eficiente a los árboles de menor peso. En el módulo Heap se provee
  una implementación de min-heaps basada en leftist heaps, como se vio en clase.
-}

map_to_HTree_list :: [(Char,Int)] -> [HTree]
map_to_HTree_list [] tree         = tree
map_to_HTree_list ((c,w):xs) tree = [(Leaf c w)] ++ tree

trans :: [HTree] -> Heap HTree -> Heap HTree
trans [] heap     = heap
trans (x:xs) heap = trans xs (merge (N 1 x E E) heap)

suma_pesos :: HTree -> HTree -> Int
suma_pesos (Leaf _ w1) (Leaf _ w2)     = w1+w2
suma_pesos (Leaf _ w1) (Node _ _ w2)   = w1+w2
suma_pesos (Node _ _ w1) (Leaf _ w2)   = w1+w2
suma_pesos (Node _ _ w1) (Node _ _ w2) = w1+w2

merge_HTree :: HTree -> HTree -> HTree
merge_HTree ht1 ht2 = (Node ht1 ht2 (suma_pesos ht1 ht2))

build_HTree_aux :: Heap HTree -> HTree -> HTree
build_HTree_aux heap min_htree
    | (isEmpty heap) = min_htree
    | otherwise      = let
                          min_el    = findmin heap
                          rest_heap = deleteMin heap
                          new_htree = merge min_htree min_el
                          new_heap  = insert rest_heap new_htree
                       in
                        build_FreqMap_aux (deleteMin new_heap) (findMin new_heap)

build_HTree :: FreqMap -> HTree
build_HTree m  
  | (m == Map.empty) = undefined 
  | otherwise = let 
                  heap = trans (Map.fromList m) E 
                in 
                  build_HTree_aux (deleteMin heap) (findMin heap)





