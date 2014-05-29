-- |Helper functions

module Helper where

rdrop n = reverse . drop n . reverse
rtake n = reverse . take n . reverse
