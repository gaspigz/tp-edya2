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


instance Seq [] where
    emptyS = []

    singletonS x = [x]

    lengthS xs = length xs

    nthS xs i  = xs !! i

    -- Versión 2
    -- tabulateS f n = [f x | x <- [0..n-1]]

    tabulateS f n = map f [0..n-1]

    mapS f xs = map f xs

    filterS p xs = filter p xs

    appendS xs ys = (xs ++ ys)

    takeS xs i = take i xs

    dropS xs i = drop i xs

    showtS []  = EMPTY
    showtS [x] = ELT x
    showtS xs  = let 
                   mid   = length xs `div` 2 
                   (l,r) = takeS xs mid ||| dropS xs mid
                 in NODE l r     

    showlS []     = NIL
    showlS (x:xs) = CONS x xs

    joinS xss = concat xss

    reduceS f b []  = b
    reduceS f b [x] = f b x
    reduceS f b s   = let 
                        n         = lengthS s
                        maxPotDos = 2 ^ lg (n-1)
                        (l,r)     = (reduceS f b (takeS s maxPotDos)) ||| (reduceS f b (dropS s maxPotDos))
                      in f l r


    scanS f b []   = ([], b)
    scanS f b [x]  = ([b], f b x)
    scanS f b s    = let
                       n           = lengthS s
                       medio       = div n 2
                       contraccion = tabulateS (scanAux1 f s n) (medio+1)
                       (s', end)   = scanS f b contraccion
                       expansion   = tabulateS (scanAux2 f s' contraccion) n
                     in
                       (expansion, end)

    fromList xs = xs