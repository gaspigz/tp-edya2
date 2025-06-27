module ArrSeq where

import Seq
import qualified Arr as A
import Arr ((!))
import Par ((|||))

lg n | n <= 1 = 0
     | otherwise = 1 + lg (n `div` 2)


auxC :: (a -> a -> a) -> A.Arr a -> Int -> Int -> Int -> a
auxC f arr n newSize i = if 
                           (i == newSize-1) && (odd n)
                         then
                           (nthS arr (n-1))
                         else
                           f (nthS arr (2*i)) (nthS arr ((2*i) + 1))
                        


contr :: (a -> a -> a) -> A.Arr a -> A.Arr a
contr f arr = case (lengthS arr) of
                             0 -> emptyS
                             1 -> arr
                             n -> let
                                    newSize = (div (n+1) 2)
                                  in
                                    tabulateS (auxC f arr n newSize) newSize


auxE :: (a -> a -> a) -> A.Arr a -> A.Arr a -> Int -> a
auxE f arr arr' i = if 
                      (even i)
                    then
                      (nthS arr (div i 2))
                    else
                      f (nthS arr (div i 2)) (nthS arr' (i-1))


expand :: (a -> a -> a) -> A.Arr a -> A.Arr a -> Int -> A.Arr a
expand f arr arr' n = tabulateS (auxE f arr arr') n


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


    filterS p arr = case (lengthS arr) of
                              0 -> emptyS
                              n -> let
                                     (l,r) = (takeS arr (div n 2)) ||| (dropS arr (div n 2))
                                     (l',r') = (filterS p l) ||| (filterS p r)
                                   in
                                     (appendS l' r')
      

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
                            1 -> f b (nthS arr 0)
                            n -> let
                                   arr' = contr f arr
                                 in
                                   reduceS f b arr'


    -- !! PENSAR SI EL SCAN CUMPLE CON LA ESPECIFICACIÓN DE LOS COSTOS
    scanS f b arr = case (lengthS arr) of 
                            0 -> (A.empty, b)
                            1 -> (singletonS b, f b (nthS arr 0)) 
                            n -> let  
                                   contraction = contr f arr
                                   (arr', end) = scanS f b contraction
                                   expansion = expand f arr' arr n
                                 in
                                   (expansion, end)
