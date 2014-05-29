-- |String 1 - Basic string problems. See http://codingbat.com/java/String-1 for details.

module String1 where

import Helper

-- |Given a string name, e.g. "Bob", return a greeting of the form
-- "Hello Bob!".
helloName :: String -> String
helloName = ("Hello " ++)

-- |Given two strings, a and b, return the result of putting them
-- together in the order abba, e.g. "Hi" and "Bye" returns
-- "HiByeByeHi".
makeAbba :: String -> String -> String
makeAbba a b = a ++ b ++ b ++ a

-- |The web is built with HTML strings like "<i>Yay</i>" which draws
-- Yay as italic text. In this example, the "i" tag makes <i> and </i>
-- which surround the word "Yay". Given tag and word strings, create
-- the HTML string with tags around the word, e.g. "<i>Yay</i>".
makeTags :: String -> String -> String
makeTags tag str = "<" ++ tag ++ ">" ++ str ++ "</" ++ tag ++ ">"

-- |Given an "out" string length 4, such as "<<>>", and a word, return
-- a new string where the word is in the middle of the out string,
-- e.g. "<<word>>". Note: use str.substring(i, j) to extract the
-- String starting at index i and going up to but not including index
-- j.
makeOutWord :: String -> String -> String
makeOutWord out word = (take 2 out) ++ word ++ (rtake 2 out)

-- |Given a string, return a new string made of 3 copies of the last 2
-- chars of the original string. The string length will be at least 2.
extraEnd :: String -> String
extraEnd = concat . replicate 3 . rtake 2

-- |Given a string, return the string made of its first two chars, so
-- the String "Hello" yields "He". If the string is shorter than
-- length 2, return whatever there is, so "X" yields "X", and the
-- empty string "" yields the empty string "". Note that str.length()
-- returns the length of a string.
firstTwo :: String -> String
firstTwo = take 2

-- |Given a string of even length, return the first half. So the
-- string "WooHoo" yields "Woo".
-- firstHalf :: String -> String
-- firstHalf s
--   | 0 /= ((length s) `mod` 2) = error "Even number of characters, dumbass."
--   | otherwise                 = take ((length s) / 2) s
-- 
-- So, actually, this isn't possible in Haskell, due to the type
-- safety - integers are not closed under division, so you can't
-- faithfully take half the length of the string.
-- 
-- Strictly speaking, it is possible, but you have to do some weird
-- coercion tricks, and those are not type-safe, and should be
-- avoided. In essence, this is a shitty problem, so don't try it.

-- |Given a string, return a version without the first and last char,
-- so "Hello" yields "ell". The string length will be at least 2.
withoutEnd :: String -> String
withoutEnd = init . tail

-- |Given 2 strings, a and b, return a string of the form
-- short+long+short, with the shorter string on the outside and the
-- longer string on the inside. The strings will not be the same
-- length, but they may be empty (length 0).
comboString :: String -> String -> String 
comboString x y
  | length x <= length y  = x ++ y ++ x
  | otherwise             = y ++ x ++ y

-- |Given 2 strings, return their concatenation, except omit the first
-- char of each. The strings will be at least length 1.
nonStart :: String -> String -> String
nonStart x y = (tail x) ++ (tail y)

-- ... okay, these are all really easy, moving on to the next section.
