module ArrSeq where

import Seq
import qualified Arr as A
import Arr ((!))
import Par ((|||))

lg n | n <= 1 = 0
     | otherwise = 1 + lg (n `div` 2)

reduceSAux :: (a -> a -> a) -> A.Arr a -> a
reduceSAux f arr =  case (lengthS arr) of 
                            1 -> nthS arr 0 
                            n -> let 
                                   maxPotDos = 2 ^ lg (n-1)
                                   (arr1, arr2) = (reduceSAux f (takeS arr maxPotDos)) ||| 
                                                  (reduceSAux f (dropS arr maxPotDos))
                                 in
                                   f arr1 arr2

scanAux1 :: (a -> a -> a) -> A.Arr a -> Int -> a
scanAux1 f arr i = f (nthS arr (2*i)) (nthS arr ((2*i)+1))

scanAux2 :: (a -> a -> a) -> A.Arr a -> A.Arr a -> Int -> a
scanAux2 f arr arr' i = if 
                         (even i)
                        then
                         (nthS arr (div i 2))
                        else
                         f (nthS arr (div i 2)) (nthS arr' (i-1))


instance Seq A.Arr where
    emptyS = A.empty

    singletonS x = A.fromList [x] 

    lengthS arr =  A.length arr

    nthS arr i  = arr ! i 

    tabulateS f n =  A.tabulate f n

    fromList xs = A.fromList xs

    mapS f arr = tabulateS (\i -> f (nthS arr i)) (lengthS arr)

    appendS arr1 arr2 = let 
                          n = lengthS arr1
                          m = lengthS arr2
                        in 
                          tabulateS (\ i -> if i < n then nthS arr1 i else nthS arr2 (i - n)) (n + m)  

    takeS arr i     = A.subArray 0 i arr

    dropS arr i     = A.subArray i (lengthS arr - i) arr

    filterS p arr = let
                     n = lengthS arr
                   in 
                     fromList [x | i <- [0..n-1], let x = nthS arr i, p x]

    showtS arr = case (lengthS arr) of
                            0 -> EMPTY
                            1 -> ELT (nthS arr 0) 
                            n -> let 
                                   (l,r) = (takeS arr ( n `div` 2)) ||| (dropS arr ( n `div` 2))
                                 in NODE l r 


    showlS arr = case (lengthS arr) of
                            0 -> NIL
                            n -> let 
                                  (l,r) = (nthS arr 0) ||| (dropS arr 1)
                                 in CONS l r 


    joinS arrr = A.flatten arrr

    reduceS f b arr = case (lengthS arr) of 
                            0 -> b
                            n -> f b (reduceSAux f arr) 

    

    scanS f b arr = case (lengthS arr) of 
                            0 -> (A.empty, b)
                            1 -> (singletonS b, f b (nthS arr 0)) 
                            n -> let  
                                   medio = div n 2
                                   contraccion = tabulateS (scanAux1 f arr) medio 
                                   (arr', end) = scanS f b contraccion
                                   expansion = tabulateS (scanAux2 f arr' contraccion) n
                                 in
                                   (expansion, end)
