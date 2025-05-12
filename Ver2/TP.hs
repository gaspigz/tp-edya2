import qualified Heap as Heap
import qualified Data.Map as Map


-- Definimos el tipo de dato que nos permite representar árboles de Huffman

data HTree = Leaf Char Int | Node HTree HTree Int deriving Show

{- 1. Dar una instancia de la clase Ord para el tipo HTree. 
   Para ello, primero debe dar una instancia de la clase Eq

  En este caso consideramos que dos HTree son iguales si 
  tienen el mismo peso. Decidimos esto ya que no es de gran 
  importancia en nuestro tp la igualdad, nos importa el orden
  y como para establecer relaciones de mayor y menor usamos 
  el peso, nos conviene que la igualdad sea definida en 
  base al peso.
-}

instance Eq HTree where
  (==) (Leaf c1 w1) (Leaf c2 w2)           = (w1 == w2)
  (==) (Leaf _ w1) (Node _ _ w2)           = (w1 == w2)
  (==) (Node _ _ w1) (Leaf _ w2)           = (w1 == w2)
  (==) (Node ht1 ht2 w1) (Node ht3 ht4 w2) = (w1 == w2)


instance Ord HTree where
  (<=) (Leaf _ w1) (Leaf _ w2)           = (w1 <= w2)
  (<=) (Leaf _ w1) (Node _ _ w2)         = (w1 <= w2)
  (<=) (Node _ _ w1) (Leaf _ w2)         = (w1 <= w2)
  (<=) (Node _ _ w1) (Node _ _ w2)       = (w1 <= w2)


{- 2. Definir una función buildFreqMap :: String -> FreqMap 
  que dado un string, compute la cantidad de apariciones de 
  cada uno de sus caracteres. Sugerencia: la función 
  insertWith de Data.Map puede ser de utilidad.
-}

type FreqMap = Map.Map Char Int


build_FreqMap_aux :: FreqMap -> String -> FreqMap
build_FreqMap_aux m "" = m
build_FreqMap_aux m (x:xs) = build_FreqMap_aux (Map.insertWith (+) x 1 m) xs

build_FreqMap :: String -> FreqMap
build_FreqMap str = build_FreqMap_aux Map.empty str


{- 3. Definir una función buildHTree :: FreqMap -> HTree que 
  compute un árbol de codificación óptimo para los símbolos 
  y su cantidad de apariciones dados, siguiendo el algoritmo 
  descripto anteriormente. Para implementarlo eficientemente,
  use un heap para tener acceso eficiente a los árboles de 
  menor peso. En el módulo Heap se provee una implementación 
  de min-heaps basada en leftist heaps, como se vio en clase.
-}

-- weight: Recibe un árbol de Huffman y devuelve su peso
weight :: HTree -> Int
weight (Leaf _ w)   = w
weight (Node _ _ w) = w


-- merge_HTree: Recibe 2 árboles de Huffman y devuelve un árbol
-- de Huffman que tiene como hijos a los árboles ingresados 
-- y su peso es la suma de los pesos de los hijos
merge_HTree :: HTree -> HTree -> HTree
merge_HTree ht1 ht2 = (Node ht1 ht2 ((weight ht1)+(weight ht2)))


-- make_HLeaf: Recibe un caracter c y la frecuencia w de dicho 
-- caracter en un string y devuelve una hoja de un árbol 
-- de Huffman que tiene como raíz c y peso w.
make_HLeaf :: (Char,Int) -> HTree
make_HLeaf (c,w) = Leaf c w


-- insert_HT_in_heap: Recibe un árbol de Huffman y un heap, 
-- e inserta el árbol de Huffman en el heap
insert_HT_in_heap :: HTree -> Heap.Heap HTree -> Heap.Heap HTree
insert_HT_in_heap ht heap = (Heap.merge (Heap.insert ht Heap.empty) heap)


-- build_HTree_aux: Esta función recibe un min_heap de árboles
-- de Huffman, un árbol de Huffman y devuelve un árbol de 
-- Huffman con codificación óptima. 
--
-- Obs: El árbol de Huffman que se recibe debe tener un peso
-- menor o igual a los pesos de todos los árboles de 
-- Huffman del heap
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


-- build_HTree: Recibe un mapa de frecuencias y devuelve
-- un árbol de codificación óptimo para el mapa de 
-- frecuencias ingresado 
build_HTree :: FreqMap -> HTree
build_HTree m
  | (m == Map.empty) = error "Empty map" 
  | otherwise = let  
                  leafs_list = [make_HLeaf x | x <- (Map.toList m)]
                  heap       = foldr insert_HT_in_heap Heap.empty leafs_list
                in 
                  build_HTree_aux (Heap.deleteMin heap) (Heap.findMin heap)


-- Creamos un tipo de dato para representar un bit
data Bit = Zero | One deriving Show

-- Asignamos un sinonimo del tipo de dato [Bit] para 
-- representar codigos
type Code = [Bit] 

type CodeMap = Map.Map Char Code


{- 4. Definir una función buildCodeMap :: HTree -> CodeMap 
  que dado un árbol de codificación construya un diccionario 
  de códigos de caracteres.
-}


buildCodeMap_aux :: HTree -> Code -> [(Char,Code)] -> [(Char,Code)]
buildCodeMap_aux (Leaf x _) code codes_list       = (x, reverse code):codes_list
buildCodeMap_aux (Node htl htr _) code codes_list = (buildCodeMap_aux htl (Zero:code) codes_list) ++ 
                                                    (buildCodeMap_aux htr (One:code) codes_list)


-- buildCodeMap: Recibe un árbol de codificación y devuelve
-- un diccionario de códigos de caracteres
buildCodeMap :: HTree -> CodeMap
buildCodeMap (Leaf c _)       = Map.fromList [(c,[Zero])]
buildCodeMap (Node htl htr _) = Map.fromList ((buildCodeMap_aux htl [Zero] [])++
                                              (buildCodeMap_aux htr [One] []))


{- 5. Definir una función encode::CodeMap -> String -> Code 
  que codifique un string, dado un diccionario de códigos 
-}

-- encode: Recibe un diccionario de códigos de caracteres
-- y un string, y devuelve el string codificado
encode :: CodeMap -> String -> Code
encode code_map []     = []
encode code_map (x:xs) = case (Map.lookup x code_map) of
                          (Just v) -> v ++ (encode code_map xs)
                          Nothing  -> error "Character not found"


{- 6. Definir una función decode :: HTree -> Code -> String que decodifique un código, dado el árbol con el que se codificó -}

-- parse_one: Esta función recibe un árbol de Huffman y
-- un código y devuelve el primer caracter decodificado
-- del código y el resto del código sin decodificar
parse_one :: HTree -> Code -> (Char, Code)
parse_one (Leaf x _) xs = (x, xs)
parse_one (Node htl htr _) [] = error "Invalid code for this Huffman Tree"
parse_one (Node htl htr _) (x:xs) = case x of
                                      Zero -> parse_one htl xs
                                      One  -> parse_one htr xs

-- decode: Recibe un código y el árbol de Huffman con
-- el que fue codificado el código, y devuelve el 
-- string decodificado
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

str_1 = "robert was a man of huge appetites and simple tastes. he had a laugh that shook the halls and a temper that could turn the air to ice. his hands were strong, his voice louder than any trumpet. he liked his wine, he liked his meat, he liked his women. he ruled as he fought, with passion and little patience. men followed him because he was strong, because he believed in them, because he could make them believe in themselves. that was robert, and that was the reason he had taken the throne. but strength fades, and crowns are heavy things."

str_2 = "harry looked around the room. the walls were covered in moving pictures, people waving and smiling. books were stacked in every corner, some old, some new. an owl sat on the edge of a desk, calm and alert. ron was talking about spells, about how his brother had once turned a teacup into a turtle. hermione was already reading, her eyes scanning line after line with determination. everything felt strange and wonderful. the castle held secrets, the kind that whispered through stone halls and echoed in candlelight. magic was real, and harry knew his life had changed in ways beyond imagining."

str_3 = "lucy walked deeper into the forest. the snow crunched under her feet, and the trees stood tall and silent. a lamp stood in the clearing, casting soft light across the ground. she felt a chill, but not from the cold. something was different, something was waiting. then she saw him, a faun with a scarf and an umbrella, looking surprised and a little afraid. he bowed politely and spoke in a gentle voice. the world beyond the wardrobe was quiet and full of wonder. lucy knew she had found something special, something hidden, a place where stories waited to be lived."

str_4 = "frodo stood at the edge of the road, listening to the wind. the trees whispered above him, and the sky was pale with morning light. a sense of adventure stirred in his chest, soft and uncertain. sam waited beside him, quiet and steady. the path ahead curved into the hills, fading behind green fields and dark woods. the journey was real now, not just talk or dreams. he thought of the shire, of quiet nights and warm meals. but something greater called to him, something distant and deep. with careful steps, they began to walk, leaving comfort behind for the unknown."

str_5 = "winter was coming, and the air carried a silence that felt old. the trees stood still, their branches bare against the sky. jon watched the snow fall, slow and endless. the cold touched his skin, sharp and clear. men moved through the yard, their breath rising in soft clouds. he thought of his father, of words spoken with weight and care. honor, duty, and family. these things shaped him like stone shapes the river. he was not a boy now, not in this place. the wall stood behind him, vast and unbroken. and beyond it, the world waited without mercy."

str_6 = "the train moved through the countryside, and the sky turned soft with clouds. inside the compartment, harry listened as ron spoke about classes, ghosts, and spells gone wrong. the castle waited ahead, full of secrets and wonder. a frog jumped across the seat, and laughter followed. hermione appeared with a book in hand, already deep in thought. the world harry had known felt far away, small and quiet. here, magic was real, and every face held a story. the wind touched the windows, and the sound of wheels filled the silence. hogwarts was not just a school, it was a beginning."

str_7 = "peter stepped forward through the tall trees, his boots pressing into the snow. the forest stretched wide around him, silent and full of light. he could hear the distant sound of water, soft and steady. susan followed close behind, her eyes on the branches above. there was peace in the air, but also a warning, quiet and cold. they had left the wardrobe behind, and now each step felt like a page turning in a story. the world was strange, yet something about it felt true. narnia was waking, and with it came both beauty and danger hidden in every shadow."

str_8 = "in the beginning god created the heaven and the earth. the earth was without form and void, and darkness was upon the face of the deep. and the spirit of god moved upon the face of the waters. god saw the light, that it was good. he divided the light from the darkness. he called the light day, and the darkness he called night. the evening and the morning were the first day. god made the firmament, and the waters were divided. the dry land appeared, and it was called earth. the grass grew, and the trees brought forth fruit."

str_9 = "the night was dark and full of whispers, the wind moved through the trees like a voice lost in time, jon snow stood watch at the top of the wall, his breath rising in pale clouds, the cold bit at his skin but he did not move, he listened to the silence beyond, the vast and wild unknown that stretched past the ice, he thought of the men who had gone out and not returned, he thought of the stories told in low voices, of things with eyes that watched and waited, and he felt the weight of the black cloak on his shoulders"

str_10 = "the sun was setting behind the hills, casting long shadows across the land. eddard stark stood at the edge of the castle, looking out over winterfell. his mind was heavy with the weight of the coming days. there were things he had to do, things he had promised to his father. but honor was a hard thing to follow, and the path ahead was unclear. the wind rustled through the trees, and he could hear the distant call of a wolf. it was a sound that always brought him comfort, but today it seemed to carry a warning."


strings_list :: [String]
strings_list = [str_1, str_2, str_3, str_4, str_5, str_6, str_7, str_8, str_9, str_10]

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

encode_EngFM_with_lenght xs = length (encode codemap_engfm xs)
lenght_list_Huffman         = map encode_EngFM_with_lenght strings_list
code_lenght_Huffman         = sum lenght_list_Huffman


encode_traditional :: String -> Code
encode_traditional [] = []
encode_traditional (x:xs) = case Map.lookup x codeMapEngFMTrad of
                                Just code -> code ++ encode_traditional xs
                                Nothing   -> error "Character not contempled"
                                                      
encode_EngFM_trad_with_lenght xs = length (encode_traditional xs)
lenght_list_traditional          = map encode_EngFM_trad_with_lenght strings_list
code_lenght_traditional          = sum lenght_list_traditional

{-  Conclusión: Al usar la codificación de Huffman logramos ahorrar una gran cantidad 
    de bits. Además mientras más largo sea el texto, más bits se ahorraran.

    Por ejemplo en este trabajo se definieron 10 strings con fragmentos de libros varios,
    y vemos que usando Huffman se ahorran 4319 bits, que aproximadamente son 539 bytes. Quizas
    puede parecer un número bajo, pero si con fragmentos de libros podemos lograr esto, con 
    textos más largos podríamos obtener incluso un mejor resultado.
-}