module ListSeq where

import Seq
import Par ((|||))

lg n | n <= 1 = 0
     | otherwise = 1 + lg (n `div` 2)

instance Seq [] where
    emptyS     = []
    singletonS x = [x]
    lengthS    = length
    nthS xs i  = xs !! i
    tabulateS f n = map f [0..n-1]
    mapS          = map
    filterS       = filter
    appendS       = (++) 
    takeS xs i     = take i xs
    dropS xs i     = drop i xs
    showtS [] = EMPTY
    showtS [x] = ELT x
    showtS    xs  = let mid = length xs `div` 2 
                        (l,r) = takeS xs mid ||| dropS xs mid
                    in NODE l r     
    showlS [] = NIL
    showlS (x:xs) = CONS x xs
    joinS = concat --  concat [[1, 2, 3], [4, 5], [6], []] -> [1,2,3,4,5,6]
    reduceS f b [] = b
    reduceS f b [x] = f b x
    reduceS f b s = let 
                            n = lengthS s
                            maxPotDos = 2 ^ lg (n-1) -- lg: Returns floor of log base 2. (Hoogle)
                            (l,r) = reduceS f b (takeS s maxPotDos) ||| reduceS f b (dropS s maxPotDos)
                    in f l r
    scanS f b [] = ([], b)
    scanS f b [x]  = ([b], f b x)
    scanS f b s = let
                    n = lengthS s
                    medio = div n 2
                    contraccion = tabulateS (\ i -> f (nthS s (2*i)) (nthS s ((2*i)+1)))  medio 
                    (s', end) = scanS f b contraccion
                    expansion = tabulateS (\i -> if even i then (nthS s' (div i 2)) else (f (nthS s' (div i 2)) (nthS contraccion (i-1)))) n
                  in
                    (expansion, end)
    fromList xs = xs