module ListSeq where

import Seq
import Par ((|||))

lg n | n <= 1 = 0
     | otherwise = 1 + lg (n `div` 2)



scanAux :: (a -> a -> a) -> [a] -> [a] -> Int -> Int -> [a]
scanAux f xs xs' 0 n = if 
                        n == 0
                       then 
                        [] 
                       else let
                              (y,ys) = (nthS xs' 0) ||| (scanAux f xs xs' 1 n)
                            in
                              (y:ys)
scanAux f xs xs' i n = if 
                        (i == n) 
                       then 
                        [] 
                       else
                            let
                              (y,y')   = (nthS xs 0) ||| (nthS xs' 0)
                              (ys,ys') = (dropS xs 1) ||| (dropS xs' 1)
                              
                              (y'',ys'') = if 
                                            (even i)
                                           then
                                            (id y') ||| (scanAux f ys xs' (i+1) n)
                                           else
                                            (f y' y) ||| (scanAux f ys ys' (i+1) n)

                            in
                              (y'':ys'')


tabulateAux f n i
                | (i < 0)   = error "invalid index"
                | (i == 0)  = []
                | otherwise = let
                                (x,rest) = (f (n-i)) ||| (tabulateAux f n (i-1))
                              in
                                (x:rest)



opPairs :: (a -> a -> a) -> [a] -> [a]
opPairs _ []          = []
opPairs _ [x]         = [x]
opPairs f (x:(x':xs)) = let
                          (y,ys) = (f x x') ||| (opPairs f xs)
                        in
                          (y:ys)








instance Seq [] where
    emptyS = []


    singletonS x = [x]


    lengthS []     = 0
    lengthS (x:xs) = 1 + (lengthS xs)


    nthS xs i  = xs !! i


    tabulateS f n = tabulateAux f n n


    mapS f []     = []
    mapS f (x:xs) = let
                      (x',xs') = (f x) ||| (mapS f xs)
                    in
                      (x':xs')


    filterS p []     = []
    filterS p (x:xs) = let
                      (x',xs') = (p x) ||| (filterS p xs)
                     in
                      if x' then (x:xs') else xs'


    appendS [] ys     = ys
    appendS (x:xs) ys = x:(xs++ys)

    takeS [] n 
              | (n < 0)  = error "invalid idx"
              | (n >= 0) = []
    takeS (x:xs) n
                  | (n < 0)   = error "invalid idx"
                  | (n == 0)  = []
                  | otherwise = x:(takeS xs (n-1))


    dropS [] n 
              | (n < 0)  = error "invalid idx"
              | (n >= 0) = []
    dropS (x:xs) n
                  | (n < 0)   = error "invalid idx"
                  | (n == 0)  = (x:xs)
                  | otherwise = (dropS xs (n-1))
    

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
                        ys = opPairs f xs
                      in
                        reduceS f b ys


    scanS f b []   = ([], b)
    scanS f b [x]  = ([b], f b x)
    scanS f b xs   = let
                       (cont, n) = (opPairs f xs) ||| (length xs)
                       (xs', r)  = scanS f b cont
                       exp       = scanAux f xs xs' 0 n
                     in
                       (exp, r)

    fromList xs = xs