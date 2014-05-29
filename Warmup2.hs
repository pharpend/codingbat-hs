-- Warmup 2 from CodingBat

module Warmup2 where

import Helper

-- |Given a string and a non-negative int n, return a larger string
-- that is n copies of the original string.
stringTimes :: String -> Int -> String
stringTimes s n = concat $ replicate n s

-- |Given a string and a non-negative int n, we'll say that the front
-- of the string is the first 3 chars, or whatever is there if the
-- string is less than length 3. Return n copies of the front;
frontTimes :: String -> Int -> String
frontTimes s n = concat $ replicate n $ take 3 s

-- |Given a string, return a new string made of every other char
-- starting with the first, so "Hello" yields "Hlo".
stringBits :: String -> String
stringBits s = [s !! i | i <- [0.. (length s-1)], even i]


-- |Given a non-empty string like "Code" return a string like
-- "CCoCodCode".
stringSplosion :: String -> String
stringSplosion "" = ""
stringSplosion s  = (stringSplosion $ init s) ++ s

-- |Given a string, return the count of the number of times that a
-- substring length 2 appears in the string and also as the last 2
-- chars of the string, so "hixxxhi" yields 1 (we won't count the end
-- substring).
last2 :: String -> Int
last2 s
  | 2 >= length s         = 0
  | take 2 s == rtake 2 s = 1 + (last2 $ drop 1 s)
  | otherwise             = last2 $ drop 1 s

-- |Given an array of ints, return the number of 9's in the array.
arrayCount9 :: [Int] -> Int
arrayCount9 xs = length [i | i <- xs, 9 == i]

-- |Given an array of ints, return true if one of the first 4 elements
-- in the array is a 9. The array length may be less than 4.
arrayFront9 :: [Int] -> Bool
arrayFront9 = elem 9 . take 4

-- |Given an array of ints, return true if .. 1, 2, 3, .. appears in
-- the array somewhere.
array123 :: [Int] -> Bool
array123 []         = False
array123 (1:2:3:_)  = True
array123 xs         = array123 $ tail xs

-- |Given 2 strings, a and b, return the number of the positions where
-- they contain the same length 2 substring. So "xxcaazz" and "xxbaaz"
-- yields 3, since the "xx", "aa", and "az" substrings appear in the
-- same place in both strings.
stringMatch :: String -> String -> Int
stringMatch a b
  | 2 > (length a) || 2 > (length b)  = 0
  | (take 2 a) == (take 2 b)          = 1 + stringMatch (drop 1 a) (drop 1 b)
  | otherwise                         = stringMatch (drop 1 a) (drop 1 b)

-- |Given a string, return a version where all the "x" have been
-- removed. Except an "x" at the very start or end should not be
-- removed.
stringX :: String -> String
stringX s
  | 2 >= length s     = s
  | otherwise         = [head s] ++ [i | i <- ss, 'x' /= i] ++ [last s]
  where ss = init $ tail s

-- |Given a string, return a string made of the chars at indexes 0,1,
-- 4,5, 8,9 ... so "kittens" yields "kien".
altPairs :: String -> String
altPairs s = [s !! i | i <- [0..(length s)], (0 == (i-1) `mod` 4) || (0 == i `mod` 4)]

-- |Suppose the string "yak" is unlucky. Given a string, return a
-- version where all the "yak" are removed, but the "a" can be any
-- char. The "yak" strings will not overlap.
stringYak :: String -> String
stringYak s
  | 3 > length s      = s
  | "yak" == take 3 s = stringYak $ drop 3 s
  | otherwise         = (++) [head s] $ stringYak $ drop 1 s

-- |Given an array of ints, return the number of times that two 6's
-- are next to each other in the array. Also count instances where the
-- second "6" is actually a 7.
array667 :: [Int] -> Int
array667 []       = 0
array667 (6:6:x)  = 1 + array667 (6:x)
array667 (6:7:x)  = 1 + array667 (7:x)
array667 s        = array667 $ tail s

-- |Given an array of ints, we'll say that a triple is a value
-- appearing 3 times in a row in the array. Return true if the array
-- does not contain any triples.
noTriples :: [Int] -> Bool
noTriples s
  | 3 > (length s)          = True
  | and $ map (== a) [b, c] = False
  | otherwise               = True && (noTriples $ tail s)
  where [a, b, c] = take 3 s

-- |Given an array of ints, return true if it contains a 2, 7, 1
-- pattern -- a value, followed by the value plus 5, followed by the
-- value minus 1. Additionally the 271 counts even if the "1" differs
-- by 2 or less from the correct value.
has271 :: [Int] -> Bool
has271 s
  | 3 > length s                      = False
  | (5 == (b-a)) && (2 >= adiff a c)  = True
  | otherwise                         = False || (has271 $ tail s)
  where adiff a b = abs $ a - b
        [a, b, c] = take 3 s
