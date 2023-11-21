import Data.Char

------------------------------------------------------
-- A.1 h - Implementing the given math function in Haskell
------------------------------------------------------

-- |
--    seqexp_func is a helper function to sum the sequential exponents
--    e.g. seqexp_func 2 will give 2^0 + 2^1 + 2^2 = 7, so the inner part of the function
seqexp :: Integer -> Integer
seqexp a = sum [a ^ i | i <- [0 .. a]]

-- |
--    h is the implementation of the mathematical function to multiply
--    the sum of sequential exponents of sequential bases
--    e.g. h 2 will give (seqexp 0)*(seqexp 1)*(seqexp 2) = 14
h :: Integer -> Integer
h a = product [seqexp i | i <- [0 .. (abs a)]]


------------------------------------------------------
-- A.2 ae - Implementing common character counter
------------------------------------------------------

-- |
--    The function ae takes two input strings and returns their similarity.
--    If strings have different lengths, return -1.
--    otherwise we pass the strings into a helper function
--    that counts common characters.
--    The function ae takes two input strings and two input int returns their similarity.
--    Base case Stings are empty return count
--    Frage (as bs)
ae :: String -> String -> Int
ae s1 s2
  | length s1 /= length s2 = -1
  | otherwise = countCommonChars s1 s2 0 0
  where
    countCommonChars :: String -> String -> Int -> Int -> Int
    countCommonChars [] [] _ count = count
    countCommonChars (a : as) (b : bs) i count
      | a == b = countCommonChars as bs (i + 1) (count + 1)
      | otherwise = countCommonChars as bs (i + 1) count

------------------------------------------------------
-- A.3 parseOctal - Parse Octal Sequence
------------------------------------------------------

-- |
--    parseOctal removes everything from an input string except 0-7
parseOctal :: String -> String
parseOctal [] = ""
parseOctal (x : xs)
  | isDigit x && x /= '8' && x /= '9' = [x] ++ (parseOctal xs)
  | otherwise = parseOctal xs


------------------------------------------------------
-- A.4 verflechte - Intertwine lists
------------------------------------------------------

-- |
--    verflechte takes two Int lists and returns an Int list that is an
--    alternatively merged list. Base cases are if list1 is empty and list2
--    is not, then return list2, and vice versa. Otherwise, concatenate the
--    first Int in list1, then the second Int in list2, and call the function
--    on the remaining lists.
verflechte :: [Int] -> [Int] -> [Int]
verflechte [] list2 = list2
verflechte list1 [] = list1
verflechte (a : as) (b : bs) = [a] ++ [b] ++ verflechte as bs


------------------------------------------------------
-- A.6 Tests - run test_all in ghci
------------------------------------------------------

test_h :: IO ()
test_h
  | (h 0 == 1)
      && (h 1 == 2)
      && (h (-1) == 2)
      && (h 2 == 14)
      && (h (-2) == 14)
      && (h 3 == 560)
      && (h (-3) == 560)
      && (h 4 == 190960) =
      print "All tests for h passed"
  | otherwise = print "One or more tests for h failed"

test_ae :: IO ()
test_ae
  | (ae "Fahrrad" "Dreirad" == 3)
      && (ae "Funken" "funken" == 5)
      && (ae "1001" "1111" == 2)
      && (ae "Fakt" "Fake" == 3)
      && (ae "Funktional" "Objektorientiert" == -1)
      && (ae "Moritz" "Schumacher" == -1)
      && (ae "Hello" "Mello" == 4)
      && (ae "2398477568" "2398477567" == 9)
      && (ae "bruder" "mutter" == 2) =
      print "All tests for ae passed"
  | otherwise = print "One or more tests for ae failed"
  

test_verflechte :: IO ()
test_verflechte
  | (verflechte [1, 2, 3] [4, 5, 6] == [1, 4, 2, 5, 3, 6])
      && (verflechte [1, 2, 3] [4] == [1, 4, 2, 3])
      && (verflechte [1, 2] [3, 4, 5, 6] == [1, 3, 2, 4, 5, 6])
      && null (verflechte [] [])
      && (verflechte [1, 2, 3] [4] == [1, 4, 2, 3])
      && (verflechte [4] [] == [4])
      && (verflechte [1, 1, 1, 1, 1] [2, 2, 2, 2] == [1, 2, 1, 2, 1, 2, 1, 2, 1])
      && (verflechte [] [1, 2, 3] == [1, 2, 3]) =
      print "All tests for verflechte passed"
  | otherwise = print "One or more tests for verflechte failed"

test_all :: IO ()
test_all = do
  test_h
  test_ae
  test_ozfw
  test_verflechte
