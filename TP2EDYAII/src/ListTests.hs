module ListTests where

import Test.HUnit
import Seq
import ListSeq

-- Función para ver el orden de reducción del reduceS
fview :: String -> String -> String
fview s s' = "(" ++ s ++ "+" ++ s' ++ ")"

s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11 :: [Int]
s12, s14 :: [[Char]]
s13 :: [[Int]]
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

-- Tests length

testLengthS1 :: Test
testLengthS1 =
  TestCase $ assertEqual "Error in length of s1"
    1
    (lengthS s1)

testLengthS2 :: Test
testLengthS2 =
  TestCase $ assertEqual "Error in length of s2"
    2
    (lengthS s2)

testLengthS3 :: Test
testLengthS3 =
  TestCase $ assertEqual "Error in length of s3"
    3
    (lengthS s3)

testLengthS4 :: Test
testLengthS4 =
  TestCase $ assertEqual "Error in length of s4"
    9
    (lengthS s4)

testLengthS5 :: Test
testLengthS5 =
  TestCase $ assertEqual "Error in length of s5"
    13
    (lengthS s5)

testLengthEmptySeq :: Test
testLengthEmptySeq = 
  TestCase $ assertEqual "Error on empty sequence length"
                         0 (lengthS s0)

testLengthNonEmptySeq :: Test
testLengthNonEmptySeq = 
  TestCase $ assertEqual "Error on non-empty sequence length"
                         2 (lengthS s2)


-- Tests tabulate

testTabulate1 :: Test
testTabulate1 =
  TestCase $ assertEqual "Error in tabulate with (+1)"
    [1,2,3,4,5]
    (tabulateS (+1) 5)

testTabulate2 :: Test
testTabulate2 =
  TestCase $ assertEqual "Error in tabulate with (*2)"
    [0,2,4,6,8,10]
    (tabulateS (*2) 6)

testTabulate3 :: Test
testTabulate3 =
  TestCase $ assertEqual "Error in tabulate with constant"
    [7,7,7]
    (tabulateS (const 7) 3)

testTabulate4 :: Test
testTabulate4 =
  TestCase $ assertEqual "Error in tabulate with negate"
    [0,-1,-2,-3]
    (tabulateS (\i -> -i) 4)

testTabulate5 :: Test
testTabulate5 =
  TestCase $ assertEqual "Error in tabulate with square"
    [0,1,4,9,16]
    (tabulateS (\i -> i*i) 5)



-- Tests map

testMapEmptySeq :: Test
testMapEmptySeq = 
  TestCase $ assertEqual "Error on empty sequence map"
                         s0 (mapS (+1) s0)

testMapS1 :: Test
testMapS1 =
  TestCase $ assertEqual "Error in map (*2) on s1"
    (fromList [8])
    (mapS (*2) s1)

testMapS2 :: Test
testMapS2 =
  TestCase $ assertEqual "Error in map (+10) on s2"
    (fromList [15,11])
    (mapS (+10) s2)

testMapS3 :: Test
testMapS3 =
  TestCase $ assertEqual "Error in map (^2) on s3"
    (fromList [36,9,16])
    (mapS (^2) s3)

testMapS4 :: Test
testMapS4 =
  TestCase $ assertEqual "Error in map negate on s4"
    (fromList [-7,-4,-15,-4,-20,-20,8,-17,-4])
    (mapS negate s4)

testMapS5 :: Test
testMapS5 =
  TestCase $ assertEqual "Error in map abs on s5"
    (fromList [5,2,2,4,7,5,8,3,4,9,6,1,5])
    (mapS abs s5)

testMapS6 :: Test
testMapS6 =
  TestCase $ assertEqual "Error in map even on s6"
    (fromList [True, False, False, True, False, True, False, False, False, True])
    (mapS even s6)

testMapS7 :: Test
testMapS7 =
  TestCase $ assertEqual "Error in map odd on s7"
    (fromList [True,False,False,True,False,True,False,True,False,True,False,True,False,True])
    (mapS odd s7)

testMapS8 :: Test
testMapS8 =
  TestCase $ assertEqual "Error in map (>0) on s8"
    (fromList [True,False,True,False,True,False,False,False,True,False,True])
    (mapS (>0) s8)

testMapS12 :: Test
testMapS12 =
  TestCase $ assertEqual "Error in map (++\"1\") on s12"
    (fromList ["a1","b1","c1","d1","e1","f1","g1","h1","i1","j1"])
    (mapS (++ "1") s12)

testMapS13 :: Test
testMapS13 =
  TestCase $ assertEqual "Error in map (map (+1)) on s13"
    (fromList [[2],[3],[4],[5],[6],[7],[8]])
    (mapS (map (+1)) s13)



-- Test filter

testFilterEvenS7 :: Test
testFilterEvenS7 =
  TestCase $ assertEqual "Error in filter even on s7"
    (fromList [0,4,6,8,10,12,14])
    (filterS even s7)

testFilterNegativeS4 :: Test
testFilterNegativeS4 =
  TestCase $ assertEqual "Error in filter (<0) on s4"
    (fromList [-8])
    (filterS (<0) s4)

testFilterGT10S6 :: Test
testFilterGT10S6 =
  TestCase $ assertEqual "Error in filter (>10) on s6"
    (fromList [12])
    (filterS (>10) s6)

testFilterDiv3S7 :: Test
testFilterDiv3S7 =
  TestCase $ assertEqual "Error in filter divisible by 3 on s7"
    (fromList [0,6,9,12,15])
    (filterS (\x -> x `mod` 3 == 0) s7)

testFilterGT50S8 :: Test
testFilterGT50S8 =
  TestCase $ assertEqual "Error in filter (>50) on s8"
    (fromList [66,83,100,55])
    (filterS (>50) s8)

testFilterAbsOddS5 :: Test
testFilterAbsOddS5 =
  TestCase $ assertEqual "Error in filter (odd . abs) on s5"
    (fromList [5,-7,5,3,9,1,-5])
    (filterS (odd . abs) s5)

testFilterLongStrS12 :: Test
testFilterLongStrS12 =
  TestCase $ assertEqual "Error in filter length > 1 on s12"
    (fromList [])
    (filterS (\x -> length x > 1) s12)

testFilterUpperCharS12 :: Test
testFilterUpperCharS12 =
  TestCase $ assertEqual "Error in filter isUpper on s12"
    (fromList [])
    (filterS (\c -> case c of [ch] -> ch >= 'A' && ch <= 'Z'; _ -> False) s12)

testFilterMinorsS9 :: Test
testFilterMinorsS9 =
  TestCase $ assertEqual "Error in filter (< -50) on s9"
    (fromList [-100, -99])
    (filterS (< (-50)) s9)

testFilterListLenGT1S13 :: Test
testFilterListLenGT1S13 =
  TestCase $ assertEqual "Error in filter (length > 1) on s13"
    (fromList [])
    (filterS (\l -> length l > 1) s13)



-- Tests append
testAppendListInt :: Test
testAppendListInt =
  TestCase $ assertEqual "Error in append on [Int] sequences"
  [6,3,4,-1,0,4,5,6,7,8,9,10,11,12,13,14,15]
  (appendS s3 s7)

testAppendListChar :: Test
testAppendListChar =
  TestCase $ assertEqual "Error in append on [Int] sequences"
  ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k","l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]
  (appendS s12 s14)






-- Test reduce

-- Usando suma

testReduceSumSeq0 :: Test
testReduceSumSeq0 = 
  TestCase $ assertEqual "Error reducing empty sequence"
                         0 (reduceS (+) 0 s0)

testReduceSumSeq3 :: Test
testReduceSumSeq3 = 
  TestCase $ assertEqual "Error reducing sequence of length 3"
                         13 (reduceS (+) 0 s3)

testReduceSumS4 :: Test
testReduceSumS4 =
  TestCase $ assertEqual "Error in reduce sum on s4"
    83
    (reduceS (+) 0 s4)

testReduceSumS5 :: Test
testReduceSumS5 =
  TestCase $ assertEqual "Error in reduce sum on s5"
    21
    (reduceS (+) 0 s5)


-- Usando Multiplicación

testReduceProdS6 :: Test
testReduceProdS6 =
  TestCase $ assertEqual "Error in reduce product on s6"
    (-7348320)
    (reduceS (*) 1 s6)

testReduceProdS7 :: Test
testReduceProdS7 =
  TestCase $ assertEqual "Error in reduce product on s7"
    0
    (reduceS (*) 1 s7)


-- Usando Mínimo y Máximo

testReduceMaxS8 :: Test
testReduceMaxS8 =
  TestCase $ assertEqual "Error in reduce max on s8"
    100
    (reduceS max (-999) s8)

testReduceMinS9 :: Test
testReduceMinS9 =
  TestCase $ assertEqual "Error in reduce min on s9"
    (-100)
    (reduceS min 999 s9)

testReduceMinS10 :: Test
testReduceMinS10 =
  TestCase $ assertEqual "Error in reduce min on s10"
    (-100)
    (reduceS min 999 s10)

testReduceMaxS11 :: Test
testReduceMaxS11 =
  TestCase $ assertEqual "Error in reduce max on s11"
    100
    (reduceS max (-999) s11)


-- Usando Append

testReduceAppendS12 :: Test
testReduceAppendS12 =
  TestCase $ assertEqual "Error in reduce append on s12"
    "abcdefghij"
    (reduceS (++) "" s12)

testReduceAppendS13 :: Test
testReduceAppendS13 =
  TestCase $ assertEqual "Error in reduce append on s13"
    [1,2,3,4,5,6,7]
    (reduceS (++) [] s13)





-- Test scan

-- Usando Suma
testScanSumSeq0 :: Test
testScanSumSeq0 = 
  TestCase $ assertEqual "Error on empty sequence scan"
                         (emptyS, 0) (scanS (+) 0 s0)

testScanSumSeq3 :: Test
testScanSumSeq3 = 
  TestCase $ assertEqual "Error on scan for sequence of length 3"
                         (fromList[0,6,9], 13) (scanS (+) 0 s3)

testScanSumSeq4 :: Test
testScanSumSeq4 =
  TestCase $ assertEqual "Error on scan sum with 10 elems"
    (fromList [0, 7, 11, 26, 30, 50, 70, 62, 79], 83)
    (scanS (+) 0 s4)

testScanSumSeq5 :: Test
testScanSumSeq5 =
  TestCase $ assertEqual "Error on scan sum with 14 elems"
    (fromList [0, 5, 3, 1, -3, -10, -5, 3, 6, 10, 19, 25, 26], 21)
    (scanS (+) 0 s5)



-- Usando multiplicación
testScanProdSeq6 :: Test
testScanProdSeq6 =
  TestCase $ assertEqual "Error on scan product with 11 elems"
    (fromList [1, 2, -6, -18, -36, -252, -1512, -13608, -68040, -612360], -7348320)
    (scanS (*) 1 s6)

testScanProdSeq7 :: Test
testScanProdSeq7 =
  TestCase $ assertEqual "Error on scan product with 15 elems"
    (fromList [1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], 0)
    (scanS (*) 1 s7)



-- Usando máximo y mínimo
testScanMaxSeq8 :: Test
testScanMaxSeq8 =
  TestCase $ assertEqual "Error on scan max with 12 elems"
    (fromList [-50, 43, 43, 66, 66, 83, 83, 83, 83, 100, 100], 100)
    (scanS max (-50) s8)

testScanMaxSeq9 :: Test
testScanMaxSeq9 =
  TestCase $ assertEqual "Error on scan max with 10 elems"
    (fromList [-100, -40, 30, 70, 100, 100, 100, 100, 100], 100)
    (scanS max (-100) s11)

testScanMinSeq10 :: Test
testScanMinSeq10 =
  TestCase $ assertEqual "Error on scan min with 13 elems"
    (fromList [0, -4, -100, -100, -100, -100, -100, -100, -100, -100, -100, -100], -100)
    (scanS min 0 s9)

testScanMinSeq11 :: Test
testScanMinSeq11 =
  TestCase $ assertEqual "Error on scan min with 12 elems"
    (fromList [0, -20, -40, -40, -80, -80, -100, -100, -100, -100, -100], -100)
    (scanS min 0 s10)



-- Usando Append
testScanAppendSeq12 :: Test
testScanAppendSeq12 =
  TestCase $ assertEqual "Error on scan append with strings"
    (fromList ["", "a", "ab", "abc", "abcd", "abcde", "abcdef", "abcdefg", "abcdefgh", "abcdefghi"], "abcdefghij")
    (scanS (++) "" s12)

testScanAppendSeq13 :: Test
testScanAppendSeq13 =
  TestCase $ assertEqual "Error on scan append with list of ints"
    (fromList [[], [1], [1,2], [1,2,3], [1,2,3,4], [1,2,3,4,5], [1,2,3,4,5,6]], [1,2,3,4,5,6,7])
    (scanS (++) [] s13)




testsLists = 
  [
    -- Test Length
    testLengthEmptySeq,
    testLengthNonEmptySeq,
    testLengthS1,
    testLengthS2,
    testLengthS3,
    testLengthS4,
    testLengthS5,

    -- Test Tabulate
    testTabulate1,
    testTabulate2,
    testTabulate3,
    testTabulate4,
    testTabulate5,

    -- Test Map
    testMapEmptySeq,
    testMapS1,
    testMapS2,
    testMapS3,
    testMapS4,
    testMapS5,
    testMapS6,
    testMapS7,
    testMapS8,
    testMapS12,
    testMapS13,


    -- Test Filter
    testFilterEvenS7,
    testFilterNegativeS4,
    testFilterGT10S6,
    testFilterDiv3S7,
    testFilterGT50S8,
    testFilterAbsOddS5,
    testFilterLongStrS12,
    testFilterUpperCharS12,
    testFilterMinorsS9,
    testFilterListLenGT1S13,


    -- Test Append
    testAppendListInt,
    testAppendListChar,


    -- Test Reduce
    testReduceSumSeq0,
    testReduceSumSeq3,
    testReduceSumS4,
    testReduceSumS5,
    testReduceProdS6,
    testReduceProdS7,
    testReduceMaxS8,
    testReduceMinS9,
    testReduceMinS10,
    testReduceMaxS11,
    testReduceAppendS12,
    testReduceAppendS13,

    -- Test Scan
    testScanSumSeq0,
    testScanSumSeq3,
    testScanSumSeq4,
    testScanSumSeq5,
    testScanProdSeq6,
    testScanProdSeq7,
    testScanMaxSeq8,
    testScanMaxSeq9,
    testScanMinSeq10,
    testScanMinSeq11,
    testScanAppendSeq12,
    testScanAppendSeq13
  ]


main :: IO Counts
main = runTestTT $ TestList testsLists
