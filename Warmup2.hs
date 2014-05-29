-- Warmup 2 from CodingBat

module Warmup2 where

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
stringBits s = map (s !!) $ filter even [0 .. (length s - 1)]

-- |Given a non-empty string like "Code" return a string like
-- "CCoCodCode".
stringSplosion :: String -> String
stringSplosion "" = ""
stringSplosion s = (stringSplosion $ init s) ++ s

-- |Given a string, return the count of the number of times that a
-- substring length 2 appears in the string and also as the last 2
-- chars of the string, so "hixxxhi" yields 1 (we won't count the end
-- substring).
last2 :: String -> Int
last2 = undefined
