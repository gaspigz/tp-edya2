module ArrSeq where

import Seq
import qualified Arr as A
import Arr ((!))
import Par ((|||))

lg n | n <= 1 = 0
     | otherwise = 1 + lg (n `div` 2)
reduceSAux :: (a -> a -> a) -> A.Arr a -> a
reduceSAux f xs =  case (lengthS xs) of 
    1 -> nthS xs 0 
    n -> let 
            maxPotDos = 2 ^ lg (n-1)
            (ys, zs) = reduceSAux f (takeS xs maxPotDos) ||| reduceSAux f (dropS xs maxPotDos)  
            in f ys zs

instance Seq A.Arr where
    emptyS     = A.empty
    singletonS x = A.fromList [x] 
    lengthS      =  A.length
    nthS xs i  = xs ! i 
    tabulateS  =  A.tabulate
    fromList  = A.fromList
    mapS   f xs      = tabulateS (\i -> f (nthS xs i)) (lengthS xs)
    appendS xs ys = let n = lengthS xs
                        m = lengthS ys
                    in tabulateS (\ i -> if i < n then nthS xs i else nthS ys (i - n)) (n + m)  
    takeS xs i     = A.subArray 0 i xs
    dropS xs i     = A.subArray i (lengthS xs - i) xs
    filterS   p xs    = let
                             n = lengthS xs
                        in fromList [x | i <- [0..n-1], let x = xs ! i, p x]


    showtS xs = case (lengthS xs) of
            0 -> EMPTY
            1 -> ELT (nthS xs 0) 
            n -> let 
                    (l,r) = takeS xs ( n `div` 2) ||| dropS xs ( n `div` 2)
                 in NODE l r 


    showlS xs = case (lengthS xs) of
             0 -> NIL
             n -> let 
                    (l',r) = takeS xs 1 ||| dropS xs 1
                    l = nthS l' 0
                 in CONS l r 


    joinS = A.flatten 

    reduceS f b xs = case (lengthS xs) of 
            0 -> b
            n -> f b (reduceSAux f xs) 

    

    scanS f b xs = case (lengthS xs) of 
        0 -> (A.empty, b)
        1 -> (singletonS b, f b (nthS xs 0)) 
        n -> let  
                medio = div n 2
                contraccion = tabulateS (\ i -> f (nthS xs (2*i)) (nthS xs ((2*i)+1)) ) medio 
                (xs', end) = scanS f b contraccion
                expansion = tabulateS (\i -> if even i then (nthS xs' (div i 2)) else (f (nthS xs' (div i 2)) (nthS contraccion (i-1)))) n
             in
               (expansion, end)
