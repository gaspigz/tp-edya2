import qualified Heap as Heap
import qualified Data.Map as Map


-- Definimos el tipo de dato que nos permite representar árboles de Huffman

data HTree = Leaf Char Int | Node HTree HTree Int deriving Show


-- 1. Dar una instancia de la clase Ord para el tipo HTree. Para ello, primero debe dar una instancia de la clase Eq

instance Eq HTree where
  (==) (Leaf c1 w1) (Leaf c2 w2)           = (w1 == w2)
  (==) (Leaf _ w1) (Node _ _ w2)           = (w1 == w2)
  (==) (Node _ _ w1) (Leaf _ w2)           = (w1 == w2)
  (==) (Node ht1 ht2 w1) (Node ht3 ht4 w2) = (w1 == w2)

  -- (/=) ht1 ht2 = not (ht1 == ht2)


instance Ord HTree where
  (<=) (Leaf _ w1) (Leaf _ w2)           = (w1 <= w2)
  (<=) (Leaf _ w1) (Node _ _ w2)         = (w1 <= w2)
  (<=) (Node _ _ w1) (Leaf _ w2)         = (w1 <= w2)
  (<=) (Node _ _ w1) (Node _ _ w2)       = (w1 <= w2)

  --(<) (Leaf _ w1) (Leaf _ w2)           = (w1 < w2)
  --(<) (Leaf _ w1) (Node _ _ w2)         = (w1 < w2)
  --(<) (Node _ _ w1) (Leaf _ w2)         = (w1 < w2)
  --(<) (Node _ _ w1) (Node _ _ w2)       = (w1 < w2)

  --(>) ht1 ht2 = not (ht1 <= ht2)

  --(>=) ht1 ht2 = not (ht1 < ht2)

  --(max) ht1 ht2 = if (ht1 <= ht2) then ht2 else ht1

  --(min) ht1 ht2 = if (ht1 <= ht2) then ht1 else ht2


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

suma_pesos :: HTree -> HTree -> Int
suma_pesos (Leaf _ w1) (Leaf _ w2)     = w1+w2
suma_pesos (Leaf _ w1) (Node _ _ w2)   = w1+w2
suma_pesos (Node _ _ w1) (Leaf _ w2)   = w1+w2
suma_pesos (Node _ _ w1) (Node _ _ w2) = w1+w2


merge_HTree :: HTree -> HTree -> HTree
merge_HTree ht1 ht2 = (Node ht1 ht2 (suma_pesos ht1 ht2))


map_to_HTree_list :: [(Char,Int)] -> [HTree] -> [HTree]
map_to_HTree_list [] tree         = tree
map_to_HTree_list ((c,w):xs) tree = map_to_HTree_list xs ([(Leaf c w)] ++ tree)


trans :: [HTree] -> Heap.Heap HTree -> Heap.Heap HTree
trans [] heap     = heap
trans (x:xs) heap = trans xs (Heap.merge (Heap.insert x Heap.empty) heap)


build_HTree_aux :: Heap.Heap HTree -> HTree -> HTree
build_HTree_aux heap min_htree
    | (Heap.isEmpty heap) = min_htree
    | otherwise           = let
                              min_el    = Heap.findMin heap
                              rest_heap = Heap.deleteMin heap
                              new_htree = merge_HTree min_htree min_el
                              new_heap  = (Heap.insert new_htree rest_heap)
                            in
                              build_HTree_aux (Heap.deleteMin new_heap) (Heap.findMin new_heap)


build_HTree :: FreqMap -> HTree
build_HTree m
  | (m == Map.empty) = error "Mapa vacío" 
  | otherwise = let 
                  list_kv = map_to_HTree_list (Map.toList m) []
                  heap    = trans list_kv Heap.empty
                in 
                  build_HTree_aux (Heap.deleteMin heap) (Heap.findMin heap)


-- Creamos un tipo de dato para representar un bit
data Bit = Zero | One deriving Show

-- Asignamos un sinonimo del tipo de dato [Bit] para representar codigos
type Code = [Bit] 

type CodeMap = Map.Map Char Code


{- 4. Definir una función buildCodeMap :: HTree -> CodeMap que dado un árbol de codificación construya un 
   diccionario de códigos de caracteres.
-}

buildCodeMap_aux :: HTree -> Code -> [(Char,Code)] -> [(Char,Code)]
buildCodeMap_aux (Leaf x _) code codes_list       = (x, code):codes_list
buildCodeMap_aux (Node htl htr _) code codes_list = (buildCodeMap_aux htl (code ++ [Zero]) codes_list) ++ 
                                                    (buildCodeMap_aux htr (code ++ [One]) codes_list)

buildCodeMap :: HTree -> CodeMap
buildCodeMap (Leaf c _)       = Map.fromList [(c,[Zero])]
buildCodeMap (Node htl htr _) = Map.fromList ((buildCodeMap_aux htl [Zero] [])++(buildCodeMap_aux htr [One] []))


{- 5. Definir una función encode::CodeMap -> String -> Code que codifique un string, dado un diccionario de códigos -}

encode :: CodeMap -> String -> Code
encode code_map []     = []
encode code_map (x:xs) = case (Map.lookup x code_map) of
                          (Just v) -> v ++ (encode code_map xs)
                          Nothing  -> error "Invalid character for this map"


{- 6. Definir una función decode :: HTree -> Code -> String que decodifique un código, dado el árbol con el que se codificó -}

parse_one :: HTree -> Code -> (Char, Code)
parse_one (Leaf x _) [] = (x,[])
parse_one (Leaf x _) xs = (x, xs)
parse_one (Node htl htr _) [] = error "Invalid code for this Huffman Tree"
parse_one (Node htl htr _) (x:xs) = case x of
                                      Zero -> parse_one htl xs
                                      One  -> parse_one htr xs

decode :: HTree -> Code -> String
decode htree []     = ""
decode htree xs     = let 
                        (char,rest_code) = parse_one htree xs
                      in
                        char:decode htree rest_code



-- 7.

engFM :: FreqMap
engFM = Map.fromList [
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

-- a) Obtenga un árbol de codificación para engFM.
htree_engfm   = build_HTree engFM
codemap_engfm = buildCodeMap htree_engfm

-- b)
strings_ingles :: [String]
strings_ingles = ["whisper", "cloud", "bounce", "flame", "mirror", "crash", "juggle", "silent", "rocket", "thread"]

codeMapEngFMTrad :: Map.Map Char Code
codeMapEngFMTrad = Map.fromList [
  (' ', [Zero, Zero, Zero, Zero, Zero]),
  ('e', [Zero, Zero, Zero, Zero, One]),
  ('t', [Zero, Zero, Zero, One, Zero]),
  ('a', [Zero, Zero, Zero, One, One]),
  ('o', [Zero, Zero, One, Zero, Zero]),
  ('h', [Zero, Zero, One, Zero, One]),
  ('s', [Zero, Zero, One, One, Zero]),
  ('i', [Zero, Zero, One, One, One]),
  ('n', [Zero, One, Zero, Zero, Zero]),
  ('r', [Zero, One, Zero, Zero, One]),
  ('d', [Zero, One, Zero, One, Zero]),
  ('l', [Zero, One, Zero, One, One]),
  ('c', [Zero, One, One, Zero, Zero]),
  ('u', [Zero, One, One, Zero, One]),
  ('m', [Zero, One, One, One, Zero]),
  ('w', [Zero, One, One, One, One]),
  ('f', [One, Zero, Zero, Zero, Zero]),
  ('g', [One, Zero, Zero, Zero, One]),
  ('y', [One, Zero, Zero, One, Zero]),
  ('p', [One, Zero, Zero, One, One]),
  ('b', [One, Zero, One, Zero, Zero]),
  ('v', [One, Zero, One, Zero, One]),
  ('k', [One, Zero, One, One, Zero]),
  ('j', [One, Zero, One, One, One]),
  ('x', [One, One, Zero, Zero, Zero]),
  ('q', [One, One, Zero, Zero, One]),
  ('z', [One, One, Zero, One, Zero]),
  (',', [One, One, Zero, One, One]),
  ('.', [One, One, One, Zero, Zero])
  ]

encodeEngFMConLen xs = length (encode codemap_engfm xs)
longitudesConHuffman = map encodeEngFMConLen strings_ingles
sumaLongHuffman = sum longitudesConHuffman


encodeTradicional :: String -> Code
encodeTradicional [] = []
encodeTradicional (x:xs) = case Map.lookup x codeMapEngFMTrad of
                            Just code -> code ++ encodeTradicional xs
                            Nothing -> error "La letra no esa contemplada"
                                                      
encodeEngFMTradConLen xs = length (encodeTradicional xs)
longitudesConTradicional = map encodeEngFMTradConLen strings_ingles
sumaLongTradicional = sum longitudesConTradicional

{- 

-}
