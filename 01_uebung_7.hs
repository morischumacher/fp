{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# HLINT ignore "Use print" #-}
{-# LANGUAGE DatatypeContexts #-}
------------------------------------------------------
-- Copy pasting previous declarations
------------------------------------------------------

data Zahl = N -- N f¨ur ‘Null’
    | P Zahl -- P f¨ur ‘plus eins’
    | M Zahl -- M f¨ur ‘minus eins’

instance Enum Zahl where
    toEnum :: Int -> Zahl
    toEnum n
        | n == 0 = N
        | n > 0 = P (toEnum (n-1))
        | n < 0 = M (toEnum (n+1)) -- or otherwise

    fromEnum :: Zahl -> Int
    fromEnum N = 0
    fromEnum (P n) = 1 + fromEnum n
    fromEnum (M n) = (-1) + fromEnum n

instance Eq Zahl where
    (==) :: Zahl -> Zahl -> Bool
    n == m = fromEnum n == fromEnum m

instance Ord Zahl where
    (<=) :: Zahl -> Zahl -> Bool
    n <= m = fromEnum n <= fromEnum m

instance Num Zahl where
    (+) :: Zahl -> Zahl -> Zahl
    a + (P n) = P a + n
    a + (M n) = M a + n
    a + N = a

    (*) :: Zahl -> Zahl -> Zahl
    n * m = sum $ replicate (fromEnum n) m

    abs :: Zahl -> Zahl
    abs = toEnum . abs . fromEnum

    signum :: Zahl -> Zahl
    signum = toEnum . signum . fromEnum

    fromInteger :: Integer -> Zahl
    fromInteger = toEnum . fromInteger

    (-) :: Zahl -> Zahl -> Zahl
    a - (P n) = M a - n
    a - (M n) = P a - n
    a - N = a

instance Show Zahl where
    show :: Zahl -> String
    show = show . fromEnum

instance Read Zahl where
    readsPrec :: Int -> ReadS Zahl
    readsPrec i s = map (\(z,r) -> (toEnum z, r)) (readsPrec i s :: [(Int, String)])

data Variable = A | B | C | R | S | T | X | Y | Z deriving (Eq, Show)

data ArithAusdruck = K Zahl -- K f ̈ur Konstante
    | V Variable -- V f ̈ur Variable
    | Ps ArithAusdruck ArithAusdruck -- Ps f ̈ur plus
    | Ml ArithAusdruck ArithAusdruck -- Ml f ̈ur mal
    | Ms ArithAusdruck ArithAusdruck -- Ms f ̈ur minus
    | Abs ArithAusdruck -- Abs f ̈ur Absolutbetrag

type Zustand = Variable -> Zahl -- Total definierte Abbildungen
type Wert = Integer

type Wahrheitswert = Bool
data LogAusdruck = LK Wahrheitswert -- LK f ̈ur logische Konstante
    | Und LogAusdruck LogAusdruck -- Logische Konjunktion
    | Oder LogAusdruck LogAusdruck -- Logische Disjunktion
    | Nicht LogAusdruck -- Logische Negation
    | Gleich ArithAusdruck ArithAusdruck -- Wertegleichheit zweier
    -- arithm. Ausdr ̈ucke
    | Kleiner ArithAusdruck ArithAusdruck -- Wert des ersten Ausdrucks
    -- kleiner Wert des zweiten

aaw :: ArithAusdruck -> Zustand -> Wert
aaw (K k) _ = toInteger (fromEnum k)
aaw (V v) z = toInteger (fromEnum (z v))
aaw (Ps v1 v2) z = aaw v1 z + aaw v2 z
aaw (Ml v1 v2) z = aaw v1 z * aaw v2 z
aaw (Ms v1 v2) z = aaw v1 z - aaw v2 z
aaw (Abs v) z = abs (aaw v z)

ala :: LogAusdruck -> Zustand -> Wahrheitswert -- ala f ̈ur Auswertung log. Ausdr ̈ucke
ala (LK w) _ = w
ala (Und l1 l2) z = ala l1 z && ala l2 z
ala (Oder l1 l2) z = ala l1 z || ala l2 z
ala (Nicht l1) z = not (ala l1 z)
ala (Gleich a1 a2) z = aaw a1 z == aaw a2 z
ala (Kleiner a1 a2) z = aaw a1 z < aaw a2 z


------------------------------------------------------
-- A.1
------------------------------------------------------
-- copied from the Ubung description
data Anweisung
  = Zw
      { ls :: Variable,
        rs :: ArithAusdruck
      }
  | Skip
  | If Bedingung Block Block
  | While Bedingung Block

type Bedingung = LogAusdruck

type Block = [Anweisung]

type Programm = Block

----------
-- A1.1
----------

{-|
    updateZustand
    parameters: Variable, Value to be assigned, and Zustand to be updated
    outputs: Zustand
    function: The function updates the variable within the state with the value, if it has changed from previous state. If not, nothing changes.
-}
updateZustand :: Variable -> Wert -> Zustand -> Zustand
updateZustand var val state v
    | v == var = fromInteger val
    | otherwise = state v

{-|
    int
    parameters: Programm, Zustand
    outputs: List of Zustand
    function: The function receives a program, which is a list of block expressions and the current stae. For each expression block in Programm, int() will execute the block and store the state each time it is updated. The output is an aggregation of all the state changes.
-}
int :: Programm -> Zustand -> [Zustand]
int [] z = []
int (Skip:zws) z = z : int zws z
-- if it matches with Zw variable expr:rest of list, zustand, then recusufly call int with the rest and the new zustand which is: update zustand with variable v, computed expr in zustamd z throught fúnction aaw, and zustand z. 
int (Zw v expr:zws) z = z : int zws (updateZustand v (aaw expr z) z)
--split into if/else block and rest (zws)
-- if ala cond == true add thenBlock and rest of the program with state z
-- otherwise add elseBlock and rest of the program with sate z
int (If cond thenBlock elseBlock : zws) z
    | ala cond z = int (thenBlock++zws) z
    | otherwise = int (elseBlock++zws) z
int (While cond thenBlock : zws) z
    -- when condtion is true (enering the while loop) add thenbranch, and cocatinate with the "While cond thenBlock" prepened to the rest zws
    | ala cond z = take 100 ( int (thenBlock ++ While cond thenBlock : zws) z)
    | otherwise = int zws z

----------
-- A1.2
----------
type Nat0 = Int
type Anfangszustand = Zustand
type Index = Nat0

{-|
    inspiziere
    parameters: Programm, Anfangszustand, Index
    outputs: Maybe Zustand
    function: The function receives a program, which is a list of block expressions, the current state, and an index. Inspiziere will try to retrieve element at the specific index. If there is a result, then it will return the result as a Zustand. If it retrieves anything else other than a valid Zustand, it will return Nothing. 
-}
inspiziere :: Programm -> Anfangszustand -> Index -> Maybe Zustand
-- if program startstate and index is empty, return nothing. !!What about when only one of these is empty!!
inspiziere [] _ _ = Nothing
inspiziere prog state0 i
    | i < 0 ||  i >= length result = Nothing
    -- Just cast he value in a mayv´be context, get the value of list result at index i (while result is the interpreted program from int)
    | otherwise = Just (result !! i)
    where
        result = int prog state0

----------
-- A1.3
----------
type Von = Index
type Bis = Index

{-|
    inspiziere'
    parameters: Programm, Anfangszustand, Von, Bis
    outputs: Maybe [Zustand]
    function: The function receives a program, which is a list of block expressions, the current state, a start index, and an end index. Inspiziere' will retrieve a sublist of Zustands from Von to Bis inidices. Some cases:
        1. If Von or Bis is outside the range of indices, then it will return Nothing
        2. If Programm is empty, it will return Nothing
        3. If Von to Bis is invalid, i.e. negative range, then it will return Nothing
-}
inspiziere' :: Programm -> Anfangszustand -> Von -> Bis -> Maybe [Zustand]
-- call the helper with the interpreted program [Zustand], von, bis
inspiziere' prog state0 von bis = helper (int prog state0) von bis
    where
        helper :: [Zustand] -> Von -> Bis ->  Maybe [Zustand]
        helper result von bis
            | von < 0 || bis < 0 || von >= length result || von > bis = Nothing
            | null result = Just result
            --take n list: returns n first elements of the list
            --drop n lsit: returns the list starting with element n
            -- equivalent with Just (drop von (take bis result))
            | otherwise = Just $ drop von (take bis result)

----------
-- A1.4
----------
{-|
    inspiziere2
    parameters: Programm, Anfangszustand, Index
    outputs: Maybe Zustand
    function: The function does the same as inspiziere but will only return variables which are significant.
-}
inspiziere2 :: Programm -> Anfangszustand -> Index -> String
inspiziere2 [] _ _ = "No solutions"
inspiziere2 prog state0 i
    | i < 0 ||  i >= length result = "Invalid Index"
    --take the state i from state list we got from (int) and filter out the sigVariables
    | otherwise = showZustand sigVariables (result !! i)
    where
        result = int prog state0

{-|
    inspiziere2'
    parameters: Programm, Anfangszustand, Von, Bis
    outputs: Maybe [Zustand]
    function: The function receives a program, which is a list of block expressions, the current state, a start index, and an end index. Inspiziere' will retrieve a sublist of Zustands from Von to Bis inidices and only the significant variables of these. Some cases:
        1. If Von or Bis is outside the range of indices, then it will return Nothing
        2. If Programm is empty, it will return empty list
        3. If Von to Bis is invalid, i.e. negative range, then it will return Nothing
-}
inspiziere2' :: Programm -> Anfangszustand -> Von -> Bis -> String
inspiziere2' prog state0 von bis = do
    -- call helper with zustand list from (int), von bis
    let  z_list = case helper (int prog state0) von bis of
                    -- cast the result of helper in a when its just a, otherwise (mainly nothing) an empty list
                    Just a -> a
                    otherwise -> []
    if not (null z_list)
        -- concatMap: function in Haskell that applies a function to each element of a list and concatenates the results.
        -- (\v -> showZustand sigVariables v): apply for each element v (showZustand sigVariables v), result is a list of strings
        -- concatMap concartinates the list of strings to one list -> z_list
        then concatMap (\v -> showZustand sigVariables v) z_list
        else ""
    where
        -- call with zustand list of (int)
        helper :: [Zustand] -> Von -> Bis ->  Maybe [Zustand]
        helper result von bis
            -- if index out of bound give nothing
            | von < 0 || bis < 0 || von >= length result || von > bis = Nothing
            -- if given result list is empty (null) give just object of empty list
            | null result = Just result
            --take n list: returns n first elements of the list
            --drop n lsit: returns the list starting with element n
            -- equivalent with Just (drop von (take bis result))
            | otherwise = Just $ drop von (take bis result)

----------
-- A1.5
----------
{-|
    inspiziere3
    parameters: Programm, Anfangszustand, Index
    outputs: Maybe Zustand
    function: The function does the same as inspiziere3 (significant variables from the state i) but will show the significant variables from the first state as well.
-}
inspiziere3 :: Programm -> Anfangszustand -> Index -> String
inspiziere3 [] _ _ = "No solutions"
inspiziere3 prog state0 i
    | i < 0 ||  i >= length result = "Invalid Index"
    | otherwise = "Initial state: \n" ++ showZustand sigVariables state0 ++
                    "\n" ++ "Final state: \n" ++ showZustand sigVariables (result !! i)
    where
        result = int prog state0


{-|
    inspiziere3'
    parameters: Programm, Anfangszustand, Von, Bis
    outputs: Maybe [Zustand]
    function: The function receives a program, which is a list of block expressions, the current state, a start index, and an end index. Inspiziere3' will retrieve the sigVariables of start state von, as well as the sigVariables of end state bis. Some cases:
        1. If Von or Bis is outside the range of indices, then it will return Nothing
        2. If Programm is empty, it will return empty list
        3. If Von to Bis is invalid, i.e. negative range, then it will return Nothing
-}
inspiziere3' :: Programm -> Anfangszustand -> Von -> Bis -> String
inspiziere3' prog state0 von bis = do
    -- we generale state list z_list by calling ghe helper function with the (int) states list
    let  z_list = case helper (int prog state0) von bis of
                    Just a -> a
                    otherwise -> []
    if not (null z_list)
        then 
            let [last] = init (drop 1 z_list) in 
                "Initial state: \n" ++ showZustand sigVariables state0 ++
                "\n" ++ "Final state: \n" ++ showZustand sigVariables last
        else ""
    where
        helper :: [Zustand] -> Von -> Bis ->  Maybe [Zustand]
        helper result von bis
            | von < 0 || bis < 0 || von >= length result || von > bis = Nothing
            | null result = Just result
            -- take n list: returns n first elements of the list
            -- drop n lsit: returns the list starting with element n
            -- equivalent with Just (drop von (take bis result))
            | otherwise = Just $ drop von (take bis result)

----------
-- A1.6
----------

{-|
    a) 
    
    new show function improved by code from suggested solutions and allows for 
    specifying which variables to show for inspiziere2, inspiziere2', inspiziere3, inspiziere3'
-}

-- takes in a list of significant variables and a state, and returns the string to print.
-- if no list is specified, it will by default display all.
showZustand :: [Variable] -> Zustand -> String
showZustand v_list var = do
    let variables = case v_list of
                        [] -> [A,B,C,R,S,T,X,Y,Z]
                        otherwise -> v_list
    concatMap (\v -> show v ++ ": " ++ show (var v) ++ "\t") variables ++ "\n"

-- old show function submitted with past assignments
-- showZustand :: Zustand -> String
-- showZustand var = 
--         "A: " ++ show (var A) ++ "\t" ++
--         "B: " ++ show (var B) ++ "\t" ++
--         "C: " ++ show (var C) ++ "\n"++
--         "R: " ++ show (var R) ++ "\t" ++
--         "S: " ++ show (var S) ++ "\t" ++
--         "T: " ++ show (var T) ++ "\n"++
--         "X: " ++ show (var X) ++ "\t" ++
--         "Y: " ++ show (var Y) ++ "\t" ++
--         "Z: " ++ show (var Z)

-- takes a list of Zustands and prints them out together with their index number
showIntResults :: [Zustand] -> String
showIntResultSo [] = ""
showIntResults z_list = helper z_list 0
    where
        helper :: [Zustand] -> Int -> String
        helper [] _ = ""
        helper (z:zs) i = "--------------\ni=" ++ show i ++ "\n"
            ++ showZustand [] z
            ++ helper zs (i+1)

{-|
    b) Implementation analysis:

    int
    - Two edge cases of the interpreter include accepting an empty program, an infinite while loop condition. In the case of an empty program, it will return nothing. For the scenario where the while loop is infinite, the while block is set to `take` an arbitrary 100 of the first elements of the list to prevent overflow.
    - An assumption that int() makes is that a valid initial state will be passed in. In our test cases, we set the initial state to 0 for all variables.

    inspiziere
    - The edge cases of inspiziere include an invalid Index, empty program, and variable initialization. An invalid Index will result in Nothing (outputted as No Solutions in the console). The empty program case results in no solutions as well, and we make the same assumption that int() makes which is that a valid initial state will be passed in. In our test cases, we set the initial state to 0 for all variables.

    inspiziere'
    - The edge cases of inspiziere' include invalid Indices, empty program, and variable initialization. An invalid set of Indices will result in Nothing (outputted as No Solutions in the console). The empty program case results in no solutions as well, and we make the assumption that int() makes is that a valid initial state will be passed in. In our test cases, we set the initial state to 0 for all variables.

    inspiziere2, inspiziere3
    - The edge cases of inspiziere2, and inspiziere3 include an invalid Index, empty program, and variable initialization. An invalid Index will result in an empty string. The empty program case results in an empty string as well, and we make the same assumption that int() makes which is that a valid initial state will be passed in. In our test cases, we set the initial state to 0 for all variables.

    inspiziere2', inspiziere3'
    - The edge cases of inspiziere2' and inspiziere3' include invalid Indices, empty program, and variable initialization. An invalid set of Indices will result in Nothing (outputted as No Solutions in the console). The empty program case results in no solutions as well, and we make the assumption that int() makes is that a valid initial state will be passed in. In our test cases, we set the initial state to 0 for all variables.
-}



------------------------------------------------------
-- A.2
------------------------------------------------------
{-|
    Design Overview:

    MAIN ALGORITHM
    --------------------
    - getAllSolutions : 
        1. generates all base permutations
        2. filters out invalid permutations
        3. builds the pyramid for each permutation, then 
        4. filiters out solutions that don't match the puzzle pattern

    newtypes: 
    - Layer: list of Integers. Each row in a pyramid
    - Pyramid: the list of layers in a pyramid shape (Highest layer is first object in pyramid)
    - Solutions: a list of all possible Pyramids that satisfy the puzzle

    Relevant functions:
    - getPyramidBase: gets depth/height of Pyramid
    - isValidLayer: checks if a given layer matches the pattern in the corresponding puzzle layer
    - isValidSolution: checks if each layer in a pyramid matches the pattern in each layer of the corresponding puzzle 
    - getValidSolutions: filters out solutions that don't match puzzle pattern
    - basePerms: generates all possible permutations of base layer
    - calculatePyramid: builds the pyramid by summing up diagonals 

    Helper functions:
    - concatLayers: allows layer unpacking and concatenation for Pyramid
    - concatPyramid: allows layer unpacking and concatenation for Solutions
    - instance Show Layer
    - instance Show Pyramid
    - instance Show Solutions

    -e.g.  Pyramid
    [
        [120],
        [56,64],
        [26,30,34],
        [13,13,17,17],
        [7,6,7,10,7],
        [2,5,1,6,4,3]
    ]
     -e.g.  puzzle
    [
        [0],
        [0,64],
        [0,0,0],
        [0,13,0,0],
        [0,0,7,0,0],
        [2,0,0,0,4,0]
    ]

    Answers to assignment questions:

    a) We thought about implementing a tree but since this was more efficient as a bottom-up approach, we felt it was necessary to be able to access elements horizontally. For example, pattern checking with the pre-filled puzzles would be much less efficient if the top block were the root node and the base layer was composed of leaf nodes. It would require O(len(layer)+len(layer-1)). Instead using lists for the layers allow O(len(layer)) efficiency. The functions and algorithm are described above and allow for any number of solutions.
    
    b) Possible edge cases:
    - empty pre-filled puzzle - we account for this in getAllSolutions. "No solutions" is outputted
    - invalid pre-filled puzzle - we assume that puzzles are valid pyramid shapes
    - non-positive values for puzzle - gives no solutions as no permutation of the base would match
    - more than one solution - our implementation generates a list of possible solutions

-}

-- new types for Pyramid problem
newtype Layer = L [Integer]
newtype Pyramid = Py [Layer]
newtype Solutions = So [Pyramid]

-- allows Integer concatenation to build Layer
concatLayers :: Layer -> Layer -> Layer
-- concatination of the two layers (integerlists)
concatLayers (L a) (L b) = L (a++b)

-- allows Layer concatenation to build Pyramid 
concatPyramid :: Pyramid -> Pyramid -> Pyramid
-- concatination of the two pyramids (layerlist)
concatPyramid (Py a) (Py b) = Py (a++b)

-- get base = height of pyramid
getPyramidBase :: Pyramid -> Int
-- Basecase: Py is empty
getPyramidBase (Py []) = 0
--recursivly call getPyramidBase with the not lowest layer and add 1
getPyramidBase (Py (layer:rest)) = 1 + getPyramidBase (Py rest)

-- filters out elements that don't match the puzzle pattern for pyramid matching
-- gets a computed solutions object and the given puzzle pyramid and provides a new solutions object only with pyramids which match to the pyramid 
getValidSolutions :: Solutions -> Pyramid -> Solutions
-- (So list) deconstructs the Solutions newtype, extracting the list of pyramids
-- a is the given puzzle pyramid
-- we construct a new Soutions object by calling isValidSolution for each element of the input pyramids list (list) and the given puzzle pyramid a, when it match put it into the new Solultions object
getValidSolutions (So list) a = So [x | x <- list, isValidSolution x a]

layerLength :: Layer -> Int
-- (L l) deconstructs the Layer newtype, extracting the list of integer
layerLength (L l) = length l

-- check that the list matches the puzzle pattern, so layer size increases always by one
isValidPyramid :: Pyramid -> Bool
isValidPyramid (Py []) = True
isValidPyramid (Py pyr) = helper (Py pyr) 1
    where
        helper :: Pyramid -> Int -> Bool
        --if only one layer, this should ne lenght 1
        helper (Py [p]) i = layerLength p == i
        -- check if the atm highest layer matches with the current length i and recursifly check if the next layer matchs with i+1
        helper (Py (p:ps)) i = layerLength p == i && helper (Py ps) (i+1)

-- check that the list (pyramid) matches the puzzle pattern, so the numbers in the puzzels matchs
-- gets two pyramids and returns if the pyramid
isValidSolution :: Pyramid -> Pyramid -> Bool
isValidSolution (Py []) (Py []) = True
isValidSolution (Py (l:ls)) (Py (a:as))
    -- if the the atm highest layer (l) is valid in terms of (a) recursifly call isValidSolution with the next layers
    | isValidLayer l a = isValidSolution (Py ls) (Py as)
    | otherwise = False

-- check that the list (layer) matches the puzzle pattern
isValidLayer :: Layer -> Layer -> Bool
isValidLayer (L []) (L []) = True
isValidLayer (L (l:ls)) (L (a:as))
    -- check if a is given by the puzzle (not zero) if yes compare and recursifly call for the rest of the layer
    | a /=0 = a==l && isValidLayer (L ls) (L as)
    | otherwise = isValidLayer (L ls) (L as)




## Explanation 
Think of it in 2 parts:
1. basePerms
2. helper

### helper:

We'll start with helper because it's the trivial case
  - "How can i start the list with each number in the list?"
If I have a list with 0 element, this gives [].
If I have a list with 1 element, this gives [x].
If I have a list with 2 elements [0,1], this gives [[0,1],[1,0]].
If I have a list with 3 elements [1,2,3], this gives [1,2,3], [2,1,3], [3,1,2], basically taking each element to the front and pushing the other elements down.

A breakdown of that:
In our OUTPUT = []
Level [1,2,3]
    - will give us OUTPUT = [[1,2,3], pending] and then recurse on [2,3]
    Level [2,3]
        - will give us OUTPUT = [[2,3], pending] and then recurse on [3]
        Level [3]
        - will give us OUTPUT = [[3],[]]
    - so now we go back up to Level [2,3] where [y:x:ys | (y:ys) <- helper xs] = [[3]], we can form the rest of the list.

        first element is [2,3], x = 2 and xs = [3]
        second element is [3,2], x = 2 and y = 3 and ys = []

- so now we go back up to LEvel [1,2,3] where [y:x:ys | (y:ys) <- helper xs] = [[2,3],[3,2]], we can form the rest of the list.

    first element is [1,2,3], x = 1 and xs = [2,3]
    second element is [2,1,3], x = 1 and y = 2 and ys = 3 //wrong
    third element is [3,1,2], x = 1 and y = 3 and ys = 2

With the 3 element case, I can generalize 

### basePerms:
- takes the first element and splits it into the head and the rest. it permutes the rest, then after getting a list of all the permutations of the rest of the body, it appends each one to the head

For example:

xs = [1,2,3] 
helper xs = [1,2,3], [2,1,3], [3,1,2]

Let's only look at [1,2,3] now
(z:ys) <- helper xs splits it into (1:[2,3])
[2,3] gets passed back into basePerms and will return RESULT = [[2,3], [3,2]]
then z:zs appends 1 to the head of each item in RESULT

.. und so weit for  [2,1,3], [3,1,2].




-- generate list of permutations of base
-- parameters: a list of Integers from [1..n] where n is the width of the lowest layer of the pyramid
basePerms :: Layer -> [Layer]
basePerms (L []) = [L []]
basePerms xs = [ L (z:zs) | L (z:ys) <- helper xs, L zs <- basePerms (L ys)]
    where
        helper :: Layer -> [Layer]
        helper (L []) = []
        helper (L (x:xs)) = L (x:xs) : [L (y:x:ys) | L (y:ys) <- helper (L xs)]



        

-- calculate solution from each permutation
-- parameters: a permutation (one list)
-- returns: solution (list of lists of integers)
calculatePyramid :: Layer -> Pyramid
calculatePyramid (L []) = Py []
-- concatinate the actual layer ((Py [L list])) and the recursifly next layers (getNextLayer (L list))
calculatePyramid (L list) = concatPyramid (calculatePyramid (getNextLayer (L list))) (Py [L list])
    where
        getNextLayer :: Layer -> Layer
        getNextLayer (L []) = L []
        getNextLayer (L [x]) = L []
        -- concat l0+l1 and recursilfy l1 with ls
        getNextLayer (L (l0:l1:ls)) = concatLayers (L [l0+l1]) (getNextLayer (L (l1:ls)))

{-| 
    functoni: getAllSolutions
    parameters: Pyramid (list of Layers)
    outputs: Maybe Solutions
    function: returns a list of Solutions (list of lists). Could be empty 
    if there is no solution for the pyramid
-}
getAllSolutions :: Pyramid-> Maybe Solutions
getAllSolutions puzzle
    | height == 0 = Just (So [])
    | not (isValidPyramid puzzle) = Nothing
    | otherwise = do
        let perms = basePerms (L [1..(toInteger height)])
        Just (So [calculatePyramid p | p <- perms, isValidSolution (calculatePyramid p) puzzle])
    where 
        height = getPyramidBase puzzle

getOneSolution :: Pyramid-> Maybe Pyramid
getOneSolution puzzle
    | height == 0 = Just (Py [])
    | not (isValidPyramid puzzle) = Nothing
    | otherwise = do
        let perms = basePerms (L [1..(toInteger height)])
        let  sols = take 1 [calculatePyramid p | p <- perms, isValidSolution (calculatePyramid p) puzzle]
        case sols of 
            [] -> Nothing
            a -> Just (a !! 0)
    where 
        height = getPyramidBase puzzle






instance Show Layer where
    show (L lyr) = "| " ++ concatWithString " | " (map show lyr) ++ " |"
        where
            concatWithString :: String -> [String] -> String
            concatWithString _ [] = ""
            concatWithString _ [x] = x
            concatWithString str (x:y:rest) = x ++ str ++ concatWithString str (y:rest)

instance Show Pyramid where
    show (Py pyr) = concatWithString "\n" (map show pyr) ++ "\n"
        where
            concatWithString :: String -> [String] -> String
            concatWithString _ [] = ""
            concatWithString _ [x] = x
            concatWithString str (x:y:rest) = x ++ str ++ concatWithString str (y:rest)

instance Show Solutions where
    show (So []) = "No solutions\n"
    show (So sol) = concatWithString "\n" (map show sol) 0
        where
            concatWithString :: String -> [String] -> Integer -> String
            concatWithString _ [] _ = ""
            concatWithString _ [x] i = "Solution " ++ show i ++ "\n-------------------------\n" ++ x
            concatWithString str (x:y:rest) i = "Solution " ++ show i ++ "\n-------------------------\n" ++ x ++ str ++ concatWithString str (y:rest) (i+1)

------------------------------------------------------
-- A.4 Tests - run test_all in ghci

{-| 
    possible functions:
    -- test_all
    -- test_int
    -- test_inspiziere
    -- test_inspiziere'
    -- test_inspiziere2
    -- test_inspiziere2'
    -- test_inspiziere3
    -- test_inspiziere3'
    -- test_pyramid
    -- test_tims_pyramid - the one on tuwel
-}

------------------------------------------------------

testProgram :: Programm
testProgram = [ Zw A (K (P (P N))),
    While (Kleiner (V B) (K (P (P (P N))))) [ Zw B (Ps (V B) (K (P N))) ], Skip]

testInfiniteProgram :: Programm
testInfiniteProgram = [While (LK (3 ==3)) [ Zw A (Ps (V A) (K (P N))) ]]

emptyProgram :: Programm
emptyProgram = []

initialState :: Zustand
initialState _ = N

sigVariables :: [Variable]
sigVariables = [A, C, R]

case_break :: String
case_break = "-------------"

test_break :: String
test_break=">\n>\n>\n"

tst_pyr_empty :: Pyramid
tst_pyr_empty = Py []

tst_pyr_invalid :: Pyramid
tst_pyr_invalid = Py [
    L [0, 0],
    L [0,0]]

tst_pyr_small :: Pyramid
tst_pyr_small = Py [
    L [0],
    L [0,0], 
    L [1,0,0]]

tst_pyr_neg :: Pyramid
tst_pyr_neg = Py [
    L [0],
    L [0,0], 
    L [-1,0,0]]

tst_pyr_actual :: Pyramid
tst_pyr_actual = Py [ 
    L [0], 
    L [0,64], 
    L [0,0,34], 
    L [0,13,0,0], 
    L [0,0,7,0,0], 
    L [2,0,0,0,4,0]]

tst_tims_pyramid :: Pyramid
tst_tims_pyramid = Py [ 
    L [0], 
    L [0,0], 
    L [0,0,0], 
    L [0,0,0,0], 
    L [0,0,201,0,0], 
    L [0,0,0,0,0,0], 
    L [0,0,58,0,0,0,0], 
    L [0,0,0,0,0,0,0,0], 
    L [0,12,0,0,0,0,3,0,0], 
    L [0,0,0,0,0,0,0,0,0,0]]
   

test_all :: IO()
test_all = do
    test_int
    test_inspiziere
    test_inspiziere'
    test_inspiziere2
    test_inspiziere2'
    test_inspiziere3
    test_inspiziere3'
    test_pyramid
    putStrLn "TESTING ALL COMPLETE"

test_int :: IO()
test_int = do
    putStrLn case_break
    putStrLn "TESTING int:"
    putStrLn case_break

    putStrLn case_break
    putStrLn "TESTING EMPTY PROGRAM:"
    putStrLn case_break
    putStrLn $ showIntResults $ int emptyProgram initialState
    putStrLn test_break

    putStrLn case_break
    putStrLn "TESTING NORMAL PROGRAM:"
    putStrLn case_break
    putStrLn $ showIntResults $ int testProgram initialState
    putStrLn test_break

    -- set to an arbitrary amount of 100 if it's infinite
    -- putStrLn case_break
    -- putStrLn "TESTING INFINITE PROGRAM:"
    -- putStrLn case_break
    -- putStrLn $ showIntResults $ int testInfiniteProgram initialState
    -- putStrLn test_break
    putStrLn "TESTING int COMPLETE"

test_inspiziere :: IO()
test_inspiziere = do
    putStrLn case_break
    putStrLn "TESTING inspiziere:"
    putStrLn case_break

    putStrLn case_break
    putStrLn "TESTING INVALID INDEX:"
    putStrLn case_break
    case inspiziere testProgram initialState (-1) of
        Just a -> putStrLn $ showZustand [] a
        Nothing -> putStrLn "No solution"

    putStrLn test_break
    putStrLn case_break
    putStrLn "TESTING NORMAL PROGRAM:"
    putStrLn case_break
    case inspiziere testProgram initialState 3 of
        Just a -> putStrLn $ showZustand [] a
        Nothing -> putStrLn "No solution"
    putStrLn test_break

    putStrLn case_break
    putStrLn "TESTING EMPTY PROGRAM:"
    putStrLn case_break
    case inspiziere emptyProgram initialState 3 of
        Just a -> putStrLn $ showZustand [] a
        Nothing -> putStrLn "No solution"
    putStrLn test_break

    -- set to an arbitrary amount of 100 if it's infinite
    putStrLn case_break
    putStrLn "TESTING INFINITE PROGRAM:"
    putStrLn case_break
    case inspiziere testInfiniteProgram initialState 3 of
        Just a -> putStrLn $ showZustand [] a
        Nothing -> putStrLn "No solution"
    putStrLn test_break
    putStrLn "TESTING inspiziere COMPLETE"

test_inspiziere' :: IO()
test_inspiziere' = do
    putStrLn case_break
    putStrLn "TESTING inspiziere':"
    putStrLn case_break
    putStrLn test_break

    putStrLn case_break
    putStrLn "TESTING INVALID INDEX:"
    putStrLn case_break
    case inspiziere' testProgram initialState 0 (-1) of
        Just a -> putStrLn $ showIntResults a
        Nothing -> putStrLn "No solution"

    putStrLn test_break
    putStrLn case_break
    putStrLn "TESTING NORMAL PROGRAM:"
    putStrLn case_break
    case inspiziere' testProgram initialState 0 4 of
        Just a -> putStrLn $ showIntResults a
        Nothing -> putStrLn "No solution"
    putStrLn test_break

    putStrLn case_break
    putStrLn "TESTING EMPTY PROGRAM:"
    putStrLn case_break
    case inspiziere' emptyProgram initialState 0 3 of
        Just a -> putStrLn $ showIntResults a
        Nothing -> putStrLn "No solution"
    putStrLn test_break

    -- set to an arbitrary amount of 100 if it's infinite
    putStrLn case_break
    putStrLn "TESTING INFINITE PROGRAM:"
    putStrLn case_break
    case inspiziere' testInfiniteProgram initialState 0 3 of
        Just a -> putStrLn $ showIntResults a
        Nothing -> putStrLn "No solution"
    putStrLn test_break
    putStrLn "TESTING inspiziere' COMPLETE"

test_inspiziere2 :: IO()
test_inspiziere2 = do
    putStrLn case_break
    putStrLn "TESTING inspiziere2:"
    putStrLn case_break
    putStrLn test_break

    putStrLn case_break
    putStrLn "TESTING INVALID INDEX:"
    putStrLn case_break
    putStrLn $ inspiziere2 testProgram initialState (-1)
    putStrLn test_break

    putStrLn case_break
    putStrLn "TESTING NORMAL PROGRAM:"
    putStrLn case_break
    putStrLn $ inspiziere2 testProgram initialState 3
    putStrLn test_break

    putStrLn case_break
    putStrLn "TESTING EMPTY PROGRAM:"
    putStrLn case_break
    putStrLn $ inspiziere2 emptyProgram initialState 3
    putStrLn test_break

    -- set to an arbitrary amount of 100 if it's infinite
    putStrLn case_break
    putStrLn "TESTING INFINITE PROGRAM:"
    putStrLn case_break
    putStrLn $ inspiziere2 testInfiniteProgram initialState 3
    putStrLn test_break
    putStrLn "TESTING inspiziere2 COMPLETE"

test_inspiziere2' :: IO()
test_inspiziere2' = do
    putStrLn case_break
    putStrLn "TESTING inspiziere2':"
    putStrLn case_break
    putStrLn test_break

    putStrLn case_break
    putStrLn "TESTING INVALID INDEX:"
    putStrLn case_break
    putStrLn $ inspiziere2' testProgram initialState 0 (-1)
    putStrLn test_break

    putStrLn case_break
    putStrLn "TESTING NORMAL PROGRAM:"
    putStrLn case_break
    putStrLn $ inspiziere2' testProgram initialState 0 3
    putStrLn test_break

    putStrLn case_break
    putStrLn "TESTING EMPTY PROGRAM:"
    putStrLn case_break
    putStrLn $ inspiziere2' emptyProgram initialState 0 3
    putStrLn test_break

    -- set to an arbitrary amount of 100 if it's infinite
    putStrLn case_break
    putStrLn "TESTING INFINITE PROGRAM:"
    putStrLn case_break
    putStrLn $ inspiziere2' testInfiniteProgram initialState 0 3
    putStrLn test_break
    putStrLn "TESTING inspiziere2' COMPLETE"

test_inspiziere3 :: IO()
test_inspiziere3 = do
    putStrLn case_break
    putStrLn "TESTING inspiziere3:"
    putStrLn case_break
    putStrLn test_break

    putStrLn case_break
    putStrLn "TESTING INVALID INDEX:"
    putStrLn case_break
    putStrLn $ inspiziere3 testProgram initialState (-1)
    putStrLn test_break

    putStrLn case_break
    putStrLn "TESTING NORMAL PROGRAM:"
    putStrLn case_break
    putStrLn $ inspiziere3 testProgram initialState 3
    putStrLn test_break

    putStrLn case_break
    putStrLn "TESTING EMPTY PROGRAM:"
    putStrLn case_break
    putStrLn $ inspiziere3 emptyProgram initialState 3
    putStrLn test_break

    -- set to an arbitrary amount of 100 if it's infinite
    putStrLn case_break
    putStrLn "TESTING INFINITE PROGRAM:"
    putStrLn case_break
    putStrLn $ inspiziere3 testInfiniteProgram initialState 3
    putStrLn test_break
    putStrLn "TESTING inspiziere3 COMPLETE"

test_inspiziere3' :: IO()
test_inspiziere3' = do
    putStrLn case_break
    putStrLn "TESTING inspiziere3':"
    putStrLn case_break
    putStrLn test_break

    putStrLn case_break
    putStrLn "TESTING INVALID INDEX:"
    putStrLn case_break
    putStrLn $ inspiziere3' testProgram initialState 0 (-1)
    putStrLn test_break

    putStrLn case_break
    putStrLn "TESTING NORMAL PROGRAM:"
    putStrLn case_break
    putStrLn $ inspiziere3' testProgram initialState 0 3
    putStrLn test_break

    putStrLn case_break
    putStrLn "TESTING EMPTY PROGRAM:"
    putStrLn case_break
    putStrLn $ inspiziere3' emptyProgram initialState 0 3
    putStrLn test_break

    -- set to an arbitrary amount of 100 if it's infinite
    putStrLn case_break
    putStrLn "TESTING INFINITE PROGRAM:"
    putStrLn case_break
    putStrLn $ inspiziere3' testInfiniteProgram initialState 0 3
    putStrLn test_break
    putStrLn "TESTING inspiziere3' COMPLETE"

test_pyramid :: IO()
test_pyramid = do
    putStrLn case_break
    putStrLn "TESTING A.2 PYRAMID PUZZLE:"
    putStrLn case_break
    putStrLn test_break

    putStrLn "TESTING EMPTY PUZZLE....\n"
    case getAllSolutions tst_pyr_empty of
        Just sol -> print sol
        Nothing -> putStrLn "ERROR: Invalid puzzle\n"
    putStrLn test_break

    putStrLn "TESTING INVALID PUZZLE....\n"
    case getAllSolutions tst_pyr_invalid of
        Just sol -> print sol
        Nothing -> putStrLn "ERROR: Invalid puzzle\n"
    putStrLn test_break

    putStrLn "TESTING NEGATIVE VALUE PUZZLE....\n"
    case getAllSolutions tst_pyr_neg of
        Just sol -> print sol
        Nothing -> putStrLn "ERROR: Invalid puzzle\n"
    putStrLn test_break

    putStrLn "TESTING NEGATIVE VALUE PUZZLE....\n"
    case getAllSolutions tst_pyr_small of
        Just sol -> print sol
        Nothing -> putStrLn "ERROR: Invalid puzzle\n"
    putStrLn test_break

    putStrLn "TESTING ACTUAL PUZZLE....\n"
    case getAllSolutions tst_pyr_actual of
        Just sol -> print sol
        Nothing -> putStrLn "ERROR: Invalid puzzle\n"
    putStrLn test_break

    putStrLn "TESTING pyramid COMPLETE"

test_tims_pyramid :: IO()
test_tims_pyramid = do
    putStrLn "TESTING TIMS PUZZLE....\n"
    case getOneSolution tst_tims_pyramid of
        Just sol -> print sol
        Nothing -> putStrLn "ERROR: Invalid puzzle\n"
    -- case getAllSolutions tst_tims_pyramid of
    --     Just sol -> print sol
    --     Nothing -> putStrLn "ERROR: Invalid puzzle\n"
    putStrLn test_break
