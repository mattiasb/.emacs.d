{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import Data.List

int :: Integer -> Integer
str :: String -> String
ints :: [Integer] -> [Integer]
strs :: [String] -> [String]

strings :: String -> String
strings  = (unlines . map string . lines)

integers :: String -> String
integers = (unlines . map (show . int . read) . lines)

string :: String -> String
string  = str

integer :: String -> String
integer = show . int . read

main = interact format

---------------------------

-- One of integer(s) or string(s)
format = integer

int = (+ 1)
str = id
ints = map int
strs = map str
