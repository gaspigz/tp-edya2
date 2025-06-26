module ListSeq where

import Seq
import Par ((|||))

lg n | n <= 1 = 0
     | otherwise = 1 + lg (n `div` 2)

scanAux1 :: (a -> a -> a) -> [a] -> Int -> Int -> a
scanAux1 f s i n = if 
                    ((2*i) >= n-1)
                  then 
                    nthS s (2*i)
                  else
                    f (nthS s (2*i)) (nthS s ((2*i)+1))

scanAux2 :: (a -> a -> a) -> [a] -> [a] -> Int -> a
scanAux2 f s s' i = if 
                     (even i)
                    then
                     (nthS s (div i 2))
                    else
                     f (nthS s (div i 2)) (nthS s' (i-1))

tabulateAux :: (Int -> a) -> Int -> Int -> [a]
tabulateAux f n 0 = (f n):[]
tabulateAux f n i = let
                      (x,rest) = (f (n-i)) ||| (tabulateAux f n (i-1))
                    in
                      (x:rest)

reduceAux :: (a -> a -> a) -> [a] -> [a]
reduceAux _ [] = []
reduceAux _ [x] = [x]
reduceAux f (x:(x':xs)) = let
                  (y,ys) = (f x x') ||| (reduceAux f xs)
                 in
                  (y:ys)



instance Seq [] where
    emptyS = []

    singletonS x = [x]

    lengthS [] = 0
    lengthS (x:xs) = 1 + (lengthS xs)

    nthS xs i  = xs !! i

    tabulateS f n = tabulateAux f n n

    filterS p []     = []
    filterS p (x:xs) = let
                      (x',xs') = (p x) ||| (filterS p xs)
                     in
                      if x' then (x:xs') else xs'

    appendS [] ys     = ys
    appendS (x:xs) ys = x:(xs++ys)

    takeS xs 0     = []
    takeS [] n     = []
    takeS (x:xs) n = x:(takeS xs (n-1))

    dropS xs 0     = xs
    dropS [] n     = []
    dropS (x:xs) n = (dropS xs (n-1))
    

    showtS []  = EMPTY
    showtS [x] = ELT x
    showtS xs  = let 
                   mid   = length xs `div` 2 
                   (l,r) = takeS xs mid ||| dropS xs mid
                 in NODE l r

    showlS []     = NIL
    showlS (x:xs) = CONS x xs

    joinS [] = []
    joinS ([]:xss) = joinS xss
    joinS ((x:xs):xss) = x:(joinS (xs:xss))

    reduceS f b []  = b
    reduceS f b [x] = f b x
    reduceS f b xs  = let 
                        ys = reduceAux f xs
                      in
                        reduceS f b ys
    
--    scanS f b []   = ([], b)
--    scanS f b [x]  = ([b], f b x)
--    scanS f b s    = let
--                       n           = lengthS s
--                       medio       = div n 2
--                       contraccion = tabulateS (scanAux1 f s n) (medio+1)
--                       (s', end)   = scanS f b contraccion
--                       expansion   = tabulateS (scanAux2 f s' contraccion) n
--                     in
--                       (expansion, end)
--
--    fromList xs = xs