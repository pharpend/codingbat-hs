-- CodingBat Warmup 1 solutions, in Haskell

module Warmup1 where

-- |The parameter weekday is True if it is a weekday, and the
-- parameter vacation is True if we are on vacation. We sleep in if it
-- is not a weekday or we're on vacation. Return True if we sleep in.
sleepIn :: Bool -> Bool -> Bool
sleepIn weekday vacation = (not weekday) || vacation


-- |We have two monkeys, a and b, and the parameters a_smile and
-- b_smile indicate if each is smiling. We are in trouble if they are
-- both smiling or if neither of them is smiling. Return True if we
-- are in trouble.
monkeyTrouble :: Bool -> Bool -> Bool
monkeyTrouble = (==)

-- |Given two int values, return their sum. Unless the two values are
-- the same, then return double their sum.
sumDouble :: Int -> Int -> Int
sumDouble a b
  | a == b    = 2 * (a + b)
  | otherwise = a + b

-- |Given an int n, return the absolute difference between n and 21,
-- except return double the absolute difference if n is over 21.
diff21 :: Int -> Int
diff21 n
  | 21 < n    = 2 * (n - 21)
  | otherwise = 21 - n

-- |We have a loud talking parrot. The "hour" parameter is the current
-- hour time in the range 0..23. We are in trouble if the parrot is
-- talking and the hour is before 7 or after 20. Return True if we are
-- in trouble.
parrotTrouble :: Bool -> Int -> Bool
parrotTrouble talking hour = talking && 7>hour && 20<hour

-- |Given 2 ints, a and b, return True if one if them is 10 or if
-- their sum is 10.
makes10 :: Int -> Int -> Bool
makes10 a b = (10 == a) || (10 == b) || (10 == (a+b))

-- |Given an int n, return True if it is within 10 of 100 or
-- 200. Note: abs(num) computes the absolute value of a number.
nearHundred :: Int -> Bool
nearHundred x = (10 >= adiff 100 x) || (10 >= adiff 200 x)
  where adiff x y = abs $ x - y

-- |Given 2 int values, return True if one is negative and one is
-- positive. Except if the parameter "negative" is True, then return
-- True only if both are negative.
posNeg :: Int -> Int -> Bool -> Bool
posNeg a b neg
  | neg       = (0 > a) && (0 > b)
  | otherwise = (0 > a) /= (0 > b)

-- |Given a string, return a new string where "not " has been added to
-- the front. However, if the string already begins with "not", return
-- the string unchanged.
notString :: String -> String
notString s
  | 3 > length s        = s
  | "not" == (take 3 s) = s
  | otherwise           = "not " ++ s

-- |Given a non-empty string and an int n, return a new string where
-- the char at index n has been removed. The value of n will be a
-- valid index of a char in the original string (i.e. n will be in the
-- range 0..len(str)-1 inclusive).
missingChar :: String -> Int -> String
missingChar s n = (take n s) ++ (drop (n+1) s)

-- |Given a string, return a new string where the first and last chars
-- have been exchanged.
frontBack :: String -> String
frontBack s
  | 2 >= length s = s
  | otherwise     = [last s] ++ (drop 1 $ init s) ++ [head s]

-- |Given a string, we'll say that the front is the first 3 chars of
-- the string. If the string length is less than 3, the front is
-- whatever is there. Return a new string which is 3 copies of the
-- front.
front3 :: String -> String
front3 = concat . replicate 3 . take 3
