{\rtf1\ansi\ansicpg1252\cocoartf2758
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\paperw11900\paperh16840\margl1440\margr1440\vieww33400\viewh21000\viewkind0
\pard\tx566\tx1133\tx1700\tx2267\tx2834\tx3401\tx3968\tx4535\tx5102\tx5669\tx6236\tx6803\pardirnatural\partightenfactor0

\f0\fs24 \cf0 import Data.Char\
\
------------------------------------------------------\
-- A.1 h - Implementing the given math function in Haskell\
------------------------------------------------------\
\
\
-- |\
--    seqexp_func is a helper function to sum the sequential exponents\
--    e.g. seqexp_func 2 will give 2^0 + 2^1 + 2^2 = 7\
seqexp :: Integer -> Integer\
seqexp a = sum [a ^ i | i <- [0 .. a]]\
\
\
-- |\
--    h is the implementation of the mathematical function to multiply\
--    the sum of sequential exponents of sequential bases\
--    e.g. h 2 will give (seqexp 0)*(seqexp 1)*(seqexp 2) = 14\
h :: Integer -> Integer\
h a = product [seqexp i | i <- [0 .. (abs a)]]\
\
-- | FOR PRACTICE: implemented with recursion\
--\
-- seqexp :: Integer -> Integer -> Integer\
-- seqexp a b\
--    | b <=0 = 1\
--    | b > 0 = (seqexp a (b-1)) + a^b\
--\
-- h :: Integer -> Integer\
-- h a\
--    | a == 0 = 1\
--    | (abs a) > 0 = (seqexp (abs a) (abs a)) * (h ((abs a)-1))\
\
------------------------------------------------------\
-- A.2 ae - Implementing common character counter\
------------------------------------------------------\
\
-- |\
--    The function ae takes two input strings and returns their similarity.\
--    If strings have different lengths, return -1.\
--    otherwise we pass the strings into a helper function\
--    that counts common characters.\
ae :: String -> String -> Int\
ae s1 s2\
  | length s1 /= length s2 = -1\
  | otherwise = countCommonChars s1 s2 0 0\
  where\
    countCommonChars :: String -> String -> Int -> Int -> Int\
    countCommonChars [] [] _ count = count\
    countCommonChars (a : as) (b : bs) i count\
      | a == b = countCommonChars as bs (i + 1) (count + 1)\
      | otherwise = countCommonChars as bs (i + 1) count\
\
------------------------------------------------------\
-- A.3 ozfw - Parse Octal Sequence then Convert to Decimal\
------------------------------------------------------\
\
-- |\
--    parseOctal removes everything from an input string except 0-7\
parseOctal :: String -> String\
parseOctal [] = ""\
parseOctal (x : xs)\
  | isDigit x && x /= '8' && x /= '9' = [x] ++ (parseOctal xs)\
  | otherwise = parseOctal xs\
\
-- |\
--    convertOctToDec reads string to Int and converts octal to decimal\
--    Note to self: read casts it to Int. x needs to be a [Char]\
--    because read only contains String\
--    null xs is the same as checking if xs is empty.\
convertOctToDec :: String -> Int\
convertOctToDec [] = -1\
convertOctToDec (x : xs)\
  | not (null xs) = (read [x] :: Int) * 8 ^ (length xs) + convertOctToDec xs\
  | null xs = read [x] :: Int\
\
-- |\
--    ozfw extracts the octal digits using parseOctal, then passes it as a\
--    param to convertOctToDec. See the other functions for description. If input string is empty, then -1 is returned because it is not possible for there to be a negative result as the hyphen would have been parsed out when parseOctal was called\
ozfw :: String -> (String, Int)\
ozfw str = (parseOctal str, convertOctToDec (parseOctal str))\
\
------------------------------------------------------\
-- A.4 verflechte - Intertwine lists\
------------------------------------------------------\
\
-- |\
--    verflechte takes two Int lists and returns an Int list that is an\
--    alternatively merged list. Base cases are if list1 is empty and list2\
--    is not, then return list2, and vice versa. Otherwise, concatenate the\
--    first Int in list1, then the second Int in list2, and call the function\
--    on the remaining lists.\
verflechte :: [Int] -> [Int] -> [Int]\
verflechte [] list2 = list2\
verflechte list1 [] = list1\
verflechte (a : as) (b : bs) = [a] ++ [b] ++ verflechte as bs\
\
------------------------------------------------------\
-- A.5 Comment function descriptions and understand what they do\
------------------------------------------------------\
\
-- |\
--    See each function comments for A.5\
\
------------------------------------------------------\
-- A.6 Tests - run test_all in ghci\
------------------------------------------------------\
\
test_h :: IO ()\
test_h\
  | (h 0 == 1)\
      && (h 1 == 2)\
      && (h (-1) == 2)\
      && (h 2 == 14)\
      && (h (-2) == 14)\
      && (h 3 == 560)\
      && (h (-3) == 560)\
      && (h 4 == 190960) =\
      print "All tests for h passed"\
  | otherwise = print "One or more tests for h failed"\
\
test_ae :: IO ()\
test_ae\
  | (ae "Fahrrad" "Dreirad" == 3)\
      && (ae "Funken" "funken" == 5)\
      && (ae "1001" "1111" == 2)\
      && (ae "Fakt" "Fake" == 3)\
      && (ae "Funktional" "Objektorientiert" == -1)\
      && (ae "Maggie" "Wang" == -1)\
      && (ae "Hello" "Mello" == 4)\
      && (ae "2398477568" "2398477567" == 9)\
      && (ae "bruder" "mutter" == 2) =\
      print "All tests for ae passed"\
  | otherwise = print "One or more tests for ae failed"\
\
test_ozfw :: IO ()\
test_ozfw\
  | (ozfw "abc89098cab" == ("0", 0))\
      && (ozfw "9aB*3K8m,4H!n59" == ("345", 229))\
      && (ozfw "9a00B*3K8m,4H!n59" == ("00345", 229))\
      && (ozfw "" == ("", -1))\
      && (ozfw "999999999" == ("", -1))\
      && (ozfw "abcxyz" == ("", -1)) =\
      print "All tests for ozfw passed"\
  | otherwise = print "One or more tests for ozfw failed"\
\
test_verflechte :: IO ()\
test_verflechte\
  | (verflechte [1, 2, 3] [4, 5, 6] == [1, 4, 2, 5, 3, 6])\
      && (verflechte [1, 2, 3] [4] == [1, 4, 2, 3])\
      && (verflechte [1, 2] [3, 4, 5, 6] == [1, 3, 2, 4, 5, 6])\
      && null (verflechte [] [])\
      && (verflechte [1, 2, 3] [4] == [1, 4, 2, 3])\
      && (verflechte [4] [] == [4])\
      && (verflechte [1, 1, 1, 1, 1] [2, 2, 2, 2] == [1, 2, 1, 2, 1, 2, 1, 2, 1])\
      && (verflechte [] [1, 2, 3] == [1, 2, 3]) =\
      print "All tests for verflechte passed"\
  | otherwise = print "One or more tests for verflechte failed"\
\
test_all :: IO ()\
test_all = do\
  test_h\
  test_ae\
  test_ozfw\
  test_verflechte}