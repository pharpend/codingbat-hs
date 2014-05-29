-- |Logic1 - basic logic problems

module Logic1 where

-- |When squirrels get together for a party, they like to have
-- cigars. A squirrel party is successful when the number of cigars is
-- between 40 and 60, inclusive. Unless it is the weekend, in which
-- case there is no upper bound on the number of cigars. Return true
-- if the party with the given values is successful, or false
-- otherwise.
cigarParty :: Int -> Bool -> Bool
cigarParty cigs wknd
  | wknd      = 40 <= cigs
  | otherwise = cigs `elem` [40..60]

-- |You and your date are trying to get a table at a restaurant. The
-- parameter "you" is the stylishness of your clothes, in the range
-- 0..10, and "date" is the stylishness of your date's clothes. The
-- result getting the table is encoded as an int value with 0=no,
-- 1=maybe, 2=yes. If either of you is very stylish, 8 or more, then
-- the result is 2 (yes). With the exception that if either of you has
-- style of 2 or less, then the result is 0 (no). Otherwise the result
-- is 1 (maybe).
dateFashion :: Int -> Int -> Int
dateFashion you date
  | 2 >= you  = 0
  | 2 >= date = 0
  | 8 <= you  = 2
  | 8 <= date = 2
  | otherwise = 1

-- |The squirrels in Palo Alto spend most of the day playing. In
-- particular, they play if the temperature is between 60 and 90
-- (inclusive). Unless it is summer, then the upper limit is 100
-- instead of 90. Given an int temperature and a boolean isSummer,
-- return true if the squirrels play and false otherwise.
squirrelPlay :: Int -> Bool -> Bool
squirrelPlay temp summer
  | summer    = temp `elem` [60..100]
  | otherwise = temp `elem` [60..90]

-- |You are driving a little too fast, and a police officer stops
-- you. Write code to compute the result, encoded as an int value:
-- 0=no ticket, 1=small ticket, 2=big ticket. If speed is 60 or less,
-- the result is 0. If speed is between 61 and 80 inclusive, the
-- result is 1. If speed is 81 or more, the result is 2. Unless it is
-- your birthday -- on that day, your speed can be 5 higher in all
-- cases.
caughtSpeeding :: Int -> Bool -> Int
caughtSpeeding speed bday
  | bday        = caughtSpeeding (speed - 5) False
  | speed <= 60 = 0
  | speed <= 80 = 1
  | otherwise   = 2

-- |Given 2 ints, a and b, return their sum. However, sums in the
-- range 10..19 inclusive, are forbidden, so in that case just return
-- 20.
sortaSum :: Int -> Int -> Int
sortaSum a b
  | sum `elem` [10..19] = 20
  | otherwise           = sum
  where sum = a + b

-- |Given a day of the week encoded as 0=Sun, 1=Mon, 2=Tue, ...6=Sat,
-- and a boolean indicating if we are on vacation, return a string of
-- the form "7:00" indicating when the alarm clock should
-- ring. Weekdays, the alarm should be "7:00" and on the weekend it
-- should be "10:00". Unless we are on vacation -- then on weekdays it
-- should be "10:00" and weekends it should be "off".
alarmClock :: Int -> Bool -> String
alarmClock day vacation
  | vacation && weekday = "10:00"
  | vacation && weekend = "off"
  | weekday             = "7:00"
  | otherwise           = "10:00"
  where weekday = day `elem` [1..5]
        weekend = day `elem` [0, 6]

-- |The number 6 is a truly great number. Given two int values, a and
-- b, return true if either one is 6. Or if their sum or difference is
-- 6. Note: the function Math.abs(num) computes the absolute value of
-- a number.
love6 :: Int -> Int -> Bool
love6 6 _ = True
love6 _ 6 = True
love6 x y = (6 == x + y) || (6 == (abs $ x - y))

-- |Given a number n, return true if n is in the range 1..10,
-- inclusive. Unless "outsideMode" is true, in which case return true
-- if the number is less or equal to 1, or greater or equal to 10.
in1to10 :: Int -> Bool -> Bool
in1to10 x False = x `elem` [1..10]
in1to10 x True  = not $ in1to10 x False

-- |We'll say a number is special if it is a multiple of 11 or if it
-- is one more than a multiple of 11. Return true if the given
-- non-negative number is special.
specialEleven :: Int -> Bool
specialEleven = (`elem` [0, 1]) . (`mod` 11) 

-- |Return true if the given non-negative number is 1 or 2 more than a
-- multiple of 20.
more20 :: Int -> Bool
more20 = (`elem` [1,2]) . (`mod` 20)

-- |Return true if the given non-negative number is a multiple of 3 or
-- 5, but not both.
old35 :: Int -> Bool
old35 n = (0 == (n `mod` 3)) /= (0 == (n `mod` 5))

-- |Return true if the given non-negative number is 1 or 2 less than a
-- multiple of 20. So for example 38 and 39 return true, but 40
-- returns false.
less20 :: Int -> Bool
less20 = (`elem` [18,19]) . (`mod` 20)

-- |Given a non-negative number "num", return true if num is within 2
-- of a multiple of 10. Note: (a % b) is the remainder of dividing a
-- by b, so (7 % 5) is 2.
nearTen :: Int -> Bool
nearTen = (<= 4) . abs . (`mod` 10) . (+ 2)
