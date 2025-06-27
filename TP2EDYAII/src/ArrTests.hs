module ArrTests where

import Test.HUnit
import Seq
import Arr        (Arr)
import ArrSeq

s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11 :: Arr Int
s12, s14 :: Arr [Char]
s13 :: Arr [Int]
s0  = fromList []
s1  = fromList [4]
s2  = fromList [5,1]
s3  = fromList [6,3,4]
s4  = fromList [7,4,15,4,20,20,-8,17,4]
s5  = fromList [5,-2,-2,-4,-7,5,8,3,4,9,6,1,-5]
s6  = fromList [2,-3,3,2,7,6,9,5,9,12]
s7  = fromList [-1,0,4,5,6,7,8,9,10,11,12,13,14,15]
s8  = fromList [43,-13,66,-77,83,-98,-45,0,100,-12,55]
s9  = fromList [-4,-100,-99,-25,32,41,0,14,10,2,3,1]
s10 = fromList [-20,-40,-15,-80,2,-100,-44,-33,-10,4,3]
s11 = fromList [-40,30,70,100,-10,-90,-5,-6,-100]
s12 = fromList ["a","b","c","d","e","f","g","h","i","j"]
s13 = fromList [[1],[2],[3],[4],[5],[6],[7]]
s14 = fromList ["k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]


-- Tests lengthS
testLengthS0 :: Test
testLengthS0 = 
  TestCase $ assertEqual "length s0" 0 (lengthS s0)

testLengthS1 :: Test
testLengthS1 = 
  TestCase $ assertEqual "length s1" 1 (lengthS s1)

testLengthS5 :: Test
testLengthS5 = 
  TestCase $ assertEqual "length s5" 13 (lengthS s5)


-- Test singletonS
testSingletonS :: Test
testSingletonS = 
  TestCase $ assertEqual "singleton 42" (fromList [42]) (singletonS 42)


-- Test tabulateS
testTabulate1 :: Test
testTabulate1 = 
  TestCase $ assertEqual "tabulate (+1)" (fromList [1,2,3]) (tabulateS (+1) 3)

testTabulate2 :: Test
testTabulate2 = 
  TestCase $ assertEqual "tabulate (*2)" (fromList [0,2,4,6]) (tabulateS (*2) 4)

testTabulate3 :: Test
testTabulate3 = 
  TestCase $ assertEqual "tabulate const 7" (fromList [7,7,7]) (tabulateS (const 7) 3)

testTabulate4 :: Test
testTabulate4 = 
  TestCase $ assertEqual "tabulate negate" (fromList [0,-1,-2]) (tabulateS negate 3)

testTabulate5 :: Test
testTabulate5 = 
  TestCase $ assertEqual "tabulate square" (fromList [0,1,4,9]) (tabulateS (\i -> i*i) 4)


-- Test mapS
testMap0 :: Test
testMap0 = 
  TestCase $ assertEqual "map (*2) s0" (fromList []) (mapS (*2) s0)

testMap1 :: Test
testMap1 = 
  TestCase $ assertEqual "map (*2) s2" (fromList [10,2]) (mapS (*2) s2)

testMap2 :: Test
testMap2 = 
  TestCase $ assertEqual "map (+3) s3" (fromList [9,6,7]) (mapS (+3) s3)

testMap3 :: Test
testMap3 = 
  TestCase $ assertEqual "map (^2) s4" (fromList [49,16,225,16,400,400,64,289,16]) (mapS (^2) s4)

testMap4 :: Test
testMap4 = 
  TestCase $ assertEqual "map abs s5" (fromList [5,2,2,4,7,5,8,3,4,9,6,1,5]) (mapS abs s5)

testMap5 :: Test
testMap5 = 
  TestCase $ assertEqual "map even s6" (fromList [True,False,False,True,False,True,False,False,False,True]) (mapS even s6)

testMap6 :: Test
testMap6 = 
  TestCase $ assertEqual "map odd s7" (fromList [True,False,False,True,False,True,False,True,False,True,False,True,False,True]) (mapS odd s7)

testMap7 :: Test
testMap7 = 
  TestCase $ assertEqual "map (>0) s8" (fromList [True,False,True,False,True,False,False,False,True,False,True]) (mapS (>0) s8)

testMap8 :: Test
testMap8 = 
  TestCase $ assertEqual "map (++"!") s12" (fromList ["a!","b!","c!","d!","e!","f!","g!","h!","i!","j!"]) (mapS (++ "!") s12)

testMap9 :: Test
testMap9 = 
  TestCase $ assertEqual "map (map (+1)) s13" (fromList [[2],[3],[4],[5],[6],[7],[8]]) (mapS (map (+1)) s13)

testMap10 :: Test
testMap10 = 
  TestCase $ assertEqual "map (++"1") s14" 
    (fromList ["k1","l1","m1","n1","o1","p1","q1","r1","s1","t1","u1","v1","w1","x1","y1","z1"]) 
    (mapS (++ "1") s14)


-- Test filterS
testFilter1 :: Test
testFilter1 = 
  TestCase $ assertEqual "filter even s7" (fromList [0,4,6,8,10,12,14]) (filterS even s7)

testFilter2 :: Test
testFilter2 = 
  TestCase $ assertEqual "filter (<0) s4" (fromList [-8]) (filterS (<0) s4)

testFilter3 :: Test
testFilter3 = 
  TestCase $ assertEqual "filter (>10) s6" (fromList [12]) (filterS (>10) s6)

testFilter4 :: Test
testFilter4 = 
  TestCase $ assertEqual "error filter test 4" (fromList [0,6,9,12,15]) (filterS (\x -> x `mod` 3 == 0) s7)

testFilter5 :: Test
testFilter5 = 
  TestCase $ assertEqual "filter (>50) s8" (fromList [66,83,100,55]) (filterS (>50) s8)

testFilter6 :: Test
testFilter6 = 
  TestCase $ assertEqual "filter (odd . abs) s5" (fromList [5,-7,5,3,9,1,-5]) (filterS (odd . abs) s5)

testFilter7 :: Test
testFilter7 = 
  TestCase $ assertEqual "filter (length > 1) s12" (fromList []) (filterS (\x -> length x > 1) s12)

testFilter8 :: Test
testFilter8 = 
  TestCase $ assertEqual "filter (< -50) s9" (fromList [-100,-99]) (filterS (< (-50)) s9)

testFilter9 :: Test
testFilter9 = 
  TestCase $ assertEqual "filter (l -> length l > 1) s13" (fromList []) (filterS (\l -> length l > 1) s13)

testFilter10 :: Test
testFilter10 = 
  TestCase $ assertEqual "filter (>30) s11" (fromList [70,100]) (filterS (>30) s11)


-- Test appendS

testAppend1 :: Test
testAppend1 = 
  TestCase $ assertEqual "append s1 s2" (fromList [4,5,1]) (appendS s1 s2)

testAppend2 :: Test
testAppend2 = 
  TestCase $ assertEqual "append s12 s14" 
    (fromList ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"]) 
    (appendS s12 s14)

testAppend3 :: Test
testAppend3 = 
  TestCase $ assertEqual "append [] s3" s3 (appendS s0 s3)

testAppend4 :: Test
testAppend4 = 
  TestCase $ assertEqual "append s13 []" s13 (appendS s13 s0)

testAppend5 :: Test
testAppend5 = 
  TestCase $ assertEqual "append s4 s5" (fromList ([7,4,15,4,20,20,-8,17,4] ++ [5,-2,-2,-4,-7,5,8,3,4,9,6,1,-5])) (appendS s4 s5)


-- Test takeS

testTakeS0 :: Test
testTakeS0 = 
  TestCase $ assertEqual "take 0 s5" (fromList []) (takeS s5 0)

testTakeS1 :: Test
testTakeS1 = 
  TestCase $ assertEqual "take full s5" s5 (takeS s5 13)

testTakeS2 :: Test
testTakeS2 = 
  TestCase $ assertEqual "take 5 s6" (fromList [2,-3,3,2,7]) (takeS s6 5)


-- Test dropS

testDropS0 :: Test
testDropS0 = 
  TestCase $ assertEqual "drop 0 s5" s5 (dropS s5 0)

testDropS1 :: Test
testDropS1 = 
  TestCase $ assertEqual "drop full s5" (fromList []) (dropS s5 13)

testDropS2 :: Test
testDropS2 = 
  TestCase $ assertEqual "drop 5 s6" (fromList [6,9,5,9,12]) (dropS s6 5)


-- Test joinS

testJoinS0 :: Test
testJoinS0 = 
  TestCase $ assertEqual "join [[1],[2],[3]]" (fromList [1,2,3]) 
                         (joinS (fromList [fromList [1], fromList [2], fromList [3]]))

testJoinS1 :: Test
testJoinS1 = 
  TestCase $ assertEqual "join [[1,2],[3,4]]" (fromList [1,2,3,4]) 
                         (joinS (fromList [fromList [1,2], fromList [3,4]]))


-- Test reduceS
testReduceS0 :: Test
testReduceS0 = 
  TestCase $ assertEqual "sum s4" 83 (reduceS (+) 0 s4)

testReduceS1 :: Test
testReduceS1 = 
  TestCase $ assertEqual "sum s5" 21 (reduceS (+) 0 s5)

testReduceS2 :: Test
testReduceS2 = 
  TestCase $ assertEqual "prod s6" (-7348320) (reduceS (*) 1 s6)

testReduceS3 :: Test
testReduceS3 = 
  TestCase $ assertEqual "prod s7" 0 (reduceS (*) 1 s7)

testReduceS4 :: Test
testReduceS4 = 
  TestCase $ assertEqual "max s8" 100 (reduceS max (-999) s8)

testReduceS5 :: Test
testReduceS5 = 
  TestCase $ assertEqual "min s9" (-100) (reduceS min 999 s9)

testReduceS6 :: Test
testReduceS6 = 
  TestCase $ assertEqual "sum s1" 4 (reduceS (+) 0 s1)

testReduceS7 :: Test
testReduceS7 = 
  TestCase $ assertEqual "max s2" 5 (reduceS max (-1000) s2)

testReduceS8 :: Test
testReduceS8 = 
  TestCase $ assertEqual "sum s0" 0 (reduceS (+) 0 s0)

testReduceS9 :: Test
testReduceS9 = 
  TestCase $ assertEqual "min s10" (-100) (reduceS min 999 s10)


-- Test scanS

testScanS0 :: Test
testScanS0 = 
  TestCase $ assertEqual "scan (+) 0 s2" (fromList [0,5], 6) (scanS (+) 0 s2)

testScanS1 :: Test
testScanS1 = 
  TestCase $ assertEqual "scan (*) 1 s2" (fromList [1,5], 5) (scanS (*) 1 s2)

testScanS2 :: Test
testScanS2 = 
  TestCase $ assertEqual "scan max (-999) s3" (fromList [-999,6,6], 6) (scanS max (-999) s3)

testScanS3 :: Test
testScanS3 = 
  TestCase $ assertEqual "scan min 999 s3" (fromList [999,6,3], 3) (scanS min 999 s3)

testScanS4 :: Test
testScanS4 = 
  TestCase $ assertEqual "scan (+) 0 s1" (fromList [0], 4) (scanS (+) 0 s1)

testScanS5 :: Test
testScanS5 = 
  TestCase $ assertEqual "scan (*) 1 s1" (fromList [1], 4) (scanS (*) 1 s1)

testScanS6 :: Test
testScanS6 = 
  TestCase $ assertEqual "scan (+) 0 s0" (fromList [], 0) (scanS (+) 0 s0)

testScanS7 :: Test
testScanS7 = 
  TestCase $ assertEqual "scan (*) 1 s0" (fromList [], 1) (scanS (*) 1 s0)

testScanS8 :: Test
testScanS8 = 
  TestCase $ assertEqual "scan (+) 1 s2" (fromList [1,6], 7) (scanS (+) 1 s2)

testScanS9 :: Test
testScanS9 = 
  TestCase $ assertEqual "scan (*) 2 s2" (fromList [2,10], 10) (scanS (*) 2 s2)


-- Test catédra

testMapEmptySeq :: Test
testMapEmptySeq = 
  TestCase $ assertEqual "Error on empty sequence map"
                         s0 (mapS (+1) s0)

testMapNonEmptySeq :: Test
testMapNonEmptySeq = 
  TestCase $ assertEqual "Error on non-empty sequence map"
                         (fromList [7,4,5]) (mapS (+1) s3)

testReduceSumSeq0 :: Test
testReduceSumSeq0 = 
  TestCase $ assertEqual "Error reducing empty sequence"
                         0 (reduceS (+) 0 s0)

testReduceSumSeq3 :: Test
testReduceSumSeq3 = 
  TestCase $ assertEqual "Error reducing sequence of length 3"
                         13 (reduceS (+) 0 s3)

testScanSumSeq0 :: Test
testScanSumSeq0 = 
  TestCase $ assertEqual "Error on empty sequence scan"
                         (emptyS, 0) (scanS (+) 0 s0)

testScanSumSeq3 :: Test
testScanSumSeq3 = 
  TestCase $ assertEqual "Error on scan for sequence of length 3"
                         (fromList[0,6,9], 13) (scanS (+) 0 s3)

testsArray = 
  [
    testMapEmptySeq,
    testMapNonEmptySeq,
    testLengthEmptySeq,
    testLengthNonEmptySeq,
    testReduceSumSeq0,
    testReduceSumSeq3,
    testScanSumSeq0,
    testScanSumSeq3
  ]


main :: IO Counts
main = runTestTT $ TestList testsArray
