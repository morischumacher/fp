
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use min" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use otherwise" #-}
import qualified Control.Exception as E
import Data.Char (toUpper)
------------------------------------------------------
-- A.1
------------------------------------------------------
-- copied from problem statement
-- takes is_simple_enough solve divide combine problemInstance
div_and_conquer :: (p -> Bool) -> (p -> s) -> (p -> [p]) -> (p -> [s] -> s) -> p -> s
div_and_conquer is_simple_enough solve divide combine problemInstance = dac problemInstance
    where dac pbi
            | is_simple_enough pbi = solve pbi
            -- recurlsivly call dac with the devided pbi. For each (map) resulting pbi combine to get the new pbi
            | True = combine pbi (map dac (divide pbi))

-- a)

{-|
    minimum'
    parameters: a list of type a that is an instance of Ord
    outputs: a
    function: Finds the minimum of a non-empty list
-}
minimum' :: Ord a => [a] -> a
minimum' = div_and_conquer m_is_simple_enough m_solve m_divide m_combine

-- trivial case is just a single element list since the problem statement 
-- specifies a non-empty list
m_is_simple_enough :: Ord a => [a] -> Bool
m_is_simple_enough lst = length lst ==1

-- return the element if it has just one element
m_solve :: Ord a => [a] -> a
m_solve [x] = x

-- split list into halves
-- if list sice is even, the first new list has one element less
m_divide :: [a] -> [[a]]
m_divide lst = [take midIndex lst, drop midIndex lst]
    where
        midIndex = length lst `div` 2

-- get the minimum of x and the minimum of y and compare the two. return the minimum of the two
m_combine :: Ord a => [a] -> [a] -> a
m_combine _ [x,y] = if x < y then x else y


-- b)
{-|
    odds
    parameters: a list of type a that is an instance of Integral
    outputs: a
    function: finds the sum of odd numbers of a list
-}
odds :: Integral a => [a] -> a
odds = div_and_conquer o_is_simple_enough o_solve o_divide o_combine

-- trivial case is an empty list
o_is_simple_enough :: Integral a => [a] -> Bool
o_is_simple_enough lst = length lst <= 1

-- solving the trivial case
o_solve :: Integral a => [a] -> a
o_solve [] = 0
o_solve [x] = if odd x then x else 0

-- split list into halves
o_divide :: [a] -> [[a]
-- split in to, when lenght is odd first half has one element less
o_divide lst = [take midIndex lst, drop midIndex lst]
    where
        midIndex = length lst `div` 2

-- get the oddsum of the concatenated list
o_combine :: Integral a => [a] -> [a] -> a
o_combine _ [x,y] = x+y

-- c)
-- ether nil, two nodes which are trees and a, derivated from Eq (== comparison possbile)
data Tree a = Nil | Node (Tree a) a (Tree a) deriving Eq

{-|
    nodes
    parameters: Tree of type a
    outputs: a
    function: counts the number of nodes in a Tree
-}
nodes :: Tree a -> Integer
nodes = div_and_conquer n_is_simple_enough n_solve n_divide n_combine

-- trivial cases are empty tree and single tree
n_is_simple_enough :: Tree a -> Bool
n_is_simple_enough Nil = True
n_is_simple_enough (Node Nil _ Nil) = True
n_is_simple_enough (Node {}) = False

-- solving the trivial case
n_solve :: Tree a -> Integer
n_solve Nil = 0
n_solve (Node Nil _ Nil) = 1

-- split list into left side and right side
n_divide :: Tree a -> [Tree a]
n_divide (Node left _ right)= [left, right]

-- add one (for current node) to the list of counts from the subtree
n_combine :: Tree a -> [Integer] -> Integer
n_combine _ counts = 1 + sum counts


------------------------------------------------------
-- A.2 
------------------------------------------------------
data NumTree a = Empty | NumNode (NumTree a) Int (NumTree a) deriving Eq

-- arbitrary big number, assuming no one will be passing in something bigger than this.
bigNum :: Int
bigNum = 2^50 

{-|
    minTree
    parameters: a NumTree of type Int
    outputs: Int
    function: finds the minimum in a NumTree
-}
minTree :: NumTree Int -> Int
minTree = div_and_conquer a2_is_simple_enough a2_solve a2_divide a2_combine

a2_is_simple_enough :: NumTree Int -> Bool
-- if tree is empty 
a2_is_simple_enough Empty = True
-- if tree has only one element
a2_is_simple_enough (NumNode Empty _ Empty) = True
a2_is_simple_enough (NumNode {}) = False

a2_solve :: NumTree Int -> Int
-- return bigNum (recognize that its empty)
a2_solve Empty = bigNum
-- return the one node
a2_solve (NumNode _ i _) = i

a2_divide :: NumTree Int -> [NumTree Int]
-- divide in the left tree and the right tree
a2_divide (NumNode left _ right)= [left, right]


a2_combine :: NumTree Int -> [Int] -> Int
-- combintion is the minimum from i and the min of the min list
a2_combine (NumNode _ i _) min_list = minimum' (i : min_list)

------------------------------------------------------
-- A.3
------------------------------------------------------
{-|
    generiere_fib_strom
    parameters: None
    outputs: infinite [Integer]
    function: generates fibonacci sequence beginning with 0
-}
generiere_fib_strom :: [Integer]
-- zipWith (+) function takes two lists and applies the addition function pairwise to their corresponding elements.
-- tail generiere_fib_strom returns all values expect the first 
generiere_fib_strom = 0 : 1 : zipWith (+) generiere_fib_strom (tail generiere_fib_strom)

------------------------------------------------------
-- A.4

-- Note: `squared` and `factorial` need to be double because these 
-- are used in approximiere_exp which outputs a double. Could 
-- also make a function that casts into to Double and map it to 
-- each element of `squared` and `factorial` but it doesn't look 
-- as clean.
------------------------------------------------------
{-|
    approximiere_exp
    parameters: Double Int
    outputs: Double
    function: calculates the approximate exp function as long as the to be addes term is bigger then epsilon 
-}


approximiere_exp :: Double -> Int -> Double
approximiere_exp epsilon x 
    | epsilon <= 0 = error "epsilon must be strictly greater than 0"
    | otherwise = sum $ takeWhile (\term -> term > epsilon) (zipWith (/) squared factorial)



{-|
    squared
    parameters: None
    outputs: [Double]
    function: generates infinite list of squares beginning with 1.

-}
squared :: [Double]
squared = zipWith (*) [1.0 ..] [1.0 ..]

{-|
    factorial
    parameters: None
    outputs: [Double]
    function: generates infinite list of factorials beginning with 1.

-}
factorial :: [Double]
factorial = 1: zipWith (*) factorial [1.0 ..]

------------------------------------------------------
-- A.5
------------------------------------------------------
type Woerterstrom = [String]

-- a) 
{-|
    generiere_woerter
    parameters: None
    outputs: Woerterstrom (infinite list of strings)
    function: generates infinite combinations of "abc" including the empty string

-}
generiere_woerter :: Woerterstrom
generiere_woerter = "": concatMap (getCombos "abc") [1..]

{-|
    getCombos
    parameters: [Char], Int
    outputs: [String], can be infinite or finite
    function: turns a string into a list if characters, then recursively 
    cross concatenates with itself. Is also a helper function for A.7
    example: chars = "abc" and i = 3
    getCombos "abc" 3 -> getCombos "abc" 2 -> getCombos "abc" 1 -> getCombos "abc" 1 returns ["a", "b", "c"].
    Back to getCombos "abc" 2: For each element in ["a", "b", "c"], it concatenates with each element of getCombos "abc" 1. Result: ["aa", "ab", "ac", "ba", "bb", "bc", "ca", "cb", "cc"].
    getCombos "abc" 3 (overall): For each element in ["a", "b", "c"] of getCombos "abc" 2.
    Result: ["aaa", "aab", "aac", "aba", "abb", "abc", "aca", "acb", "acc", "baa", "bab", "bac", "bba", "bbb", "bbc", "bca", "bcb", "bcc", "caa", "cab", "cac", "cba", "cbb", "cbc", "cca", "ccb", "ccc"].

-}

getCombos :: [Char] -> Int -> [String]
getCombos chars i
    -- if current index lenght is 1, return each char from chars as string f.i. [[a][b][c]]
    | i == 1 = map (:[]) chars
    -- combines always the basecase with the case (i-1)
    | otherwise = concatMap (\front -> map (front ++) (getCombos chars 1))
                                (getCombos chars (i - 1))

--b)
{-|
    filtere_prim_a
    parameters: Woerterstrom
    outputs: Woerterstrom (infinite list of strings)
    function: only outputs the strings from `generiere_woerter` that have prime occurrences
    by filtering with a boolean condition

-}
filtere_prim_a :: Woerterstrom -> Woerterstrom
-- filters out the elements of stream which length (primeFactors (count_a str) ) == 1
filtere_prim_a stream = filter primeOccurrence_a stream 
    where
        -- returns true if the number of prime factors of the number of a's only contains itself
        primeOccurrence_a :: String -> Bool
        primeOccurrence_a str = length (primeFactors (count_a str) ) == 1

{-|
    count_a
    parameters: String
    outputs: integer
    function: counts occurences of the character a
-}
count_a :: String -> Integer
count_a [] = 0
count_a (s:ss)
    | s == 'a' = 1 + count_a ss
    | otherwise = count_a ss

-- from ubung2
-- computes all prime factores of an integer
primeFactors :: Integer -> [Integer]
primeFactors k
  | k <= 1 = []
  | k < 4 = [k]
  | otherwise = helper k 2
  where
    helper :: Integer -> Integer -> [Integer]
    helper n i
      | mod n i == 0 = i : primeFactors (n `div` i)
      | mod n i /= 0 = helper n (i + 1)

------------------------------------------------------
-- A.6
------------------------------------------------------
-- a)
{-|
    filtere_palindrome
    parameters: Woerterstrom
    outputs: Woerterstrom
    function: only outputs the strings from `generiere_woerter` that are palindromes
    by filtering with a boolean condition
-}
filtere_palindrome :: Woerterstrom -> Woerterstrom
filtere_palindrome stream = filter isPalindrome stream

{-|
    isPalindrome
    parameters: String
    outputs: Bool
    function: checks if a string is a palindrome
-}
isPalindrome :: String -> Bool
isPalindrome str
    -- if lenght (l) is <= 1
    | l <= 1 = True
    -- when last and first element are the same, recursifly call without last and first
    | head str == str !! (l-1) = isPalindrome $ drop 1 (take (l-1) str)
    | otherwise = False
    where l = length str

-- b)
{-|
    transf
    parameters: Woerterstrom
    outputs: Woerterstrom
    function: Function transforms all the characters in a stream to uppercase
-}
transf :: Woerterstrom -> Woerterstrom
-- for each element of stream call toUpperString
transf stream = map toUpperString stream

{-|
    toUpperString
    parameters: String
    outputs: String
    function: turns all letters in a string to uppercase
-}
toUpperString :: String -> String
-- for each char in string, call toUpper
toUpperString = map toUpper

------------------------------------------------------
-- A.7
------------------------------------------------------
{-|
    wort_gen
    parameters: None
    outputs: Woerterstrom
    function: Function generates words combinations over
        the whole alphabet, then filters palindrome, and 
        transforms the string toUppercase
-}
wort_gen :: Woerterstrom
-- uppercase strom (transf) only with palindroms (filtere_palindrome) of alpha_combos
wort_gen = transf $ filtere_palindrome alpha_combos
    -- concatinate empty string with all combos (getCombos) from a to z and every lenght
    where alpha_combos = "": concatMap (getCombos ['a'..'z']) [1..]

------------------------------------------------------
-- A.8 - Christmas Trees
------------------------------------------------------
{-|
    Design Overview:

    MAIN ALGORITHM (func: solveXmasTreeProblem)
    --------------------
    1. accepts TreeGrid (at least 0 pre-filled trees) and ViewPlan
    2. checks if TreeGrid and ViewPlan are both valid
    3. generates a list of Positions that are open (denoted by 0)
    4. For each position that is open
        4a. generate a list of possible trees that can be placed there
        4b. iterate through the list of possible trees and produce a new grid with that tree
        4c. recurse back to Step 3 with new grid, usw.
    5. After all possible layouts are generated, filter all the possibilities for grids that specifically match the ViewPlan.
    6. Get one solution, and output it to the console.

    newtypes: 
    - XmasTree : Integer height of tree
    - TreeGrid : A grid of XmasTrees [[XmasTree]]
    - Position : (Int, Int) coordinates within the TreeGrid
    - Layouts : Possible Solutions [TreeGrid]
    - View : [Integer] that primarily serves as an alias
    - ViewPlan : [View] the number of trees you can see from that particular point.

    Relevant functions:
    - getOneLayout
    - createLayouts
    - doViewsWork
    - countNumTrees
    - isValidPosition
    - openPositions
    - gridWithPotentialTree
    - rowToString
    - outputSolution
    - isValidGrid
    - isValidView
    - solveXmasTreeProblem

    Test functions:
    - main : tests all

    exampleViewPlan :: ViewPlan
    exampleViewPlan = let
            top = [5,4,3,2,1]
            right = [1,2,2,2,2]
            left = [5,4,3,2,1]
            bottom = [1,2,2,2,2]
        in [top, bottom, right, left]


    exampleProblem :: TreeGrid
    exampleProblem = [
        [10,0,30,0,50],
        [0,30,0,50,0],
        [0,0,0,10,20],
        [40,50,0,20,0],
        [50,0,20,0,40]
        ]

    exampleSolution :: TreeGrid
    exampleSolution = [
        [10,20,30,40,50],
        [20,30,40,50,10],
        [30,40,50,10,20],
        [40,50,10,20,30],
        [50,10,20,30,40]
        ]
    Answers to assignment questions:

    a) see following code implementation
    
    b) Possible edge cases:
    - empty TreeGrid - isValidGrid throws error
    - TreeGrid with invalid values - isValidGrid throws error
    - TreeGrid with invalid dimensions - isValidGrid throws error
    - empty View layout - isValidView throws error
    - View layout  with invalid values - isValidView throws error
    - View layout  with invalid dimensions - isValidView throws error

    Note that in the tests, we use Exception.catch so that we can run through all
    the tests without interruption. However, to see the specific errors being thrown,
    the function calls for the edge cases will need to moved outside of the catch statement.
    Error handling occurs with the use of both `error` and `Maybe | Just | Nothing`.

    c) Our function can scale provided that with increasing size comes a higher 
    number of pre-filled positions. For instance, our solution can solve a 15x15 grid 
    (see largeGrid) in 97.66 secs, but runs longer than 2 minutes for an empty grid of
    the same size. 

-}

-- a) 

type XmasTree = Integer
type TreeGrid = [[XmasTree]]
type Position = (Int, Int) -- stays int
type Layouts = [TreeGrid]
type View = [Integer]
type ViewPlan = [View]

{-|
    getOneLayout
    parameters: Layouts
    outputs: Maybe TreeGrid
    function: returns either Nothing if Layouts is empty, or Just TreeGrid
-}
getOneLayout :: Layouts -> Maybe TreeGrid
getOneLayout [] = Nothing
getOneLayout (l:ls) = Just l

createLayouts :: TreeGrid -> ViewPlan -> Layouts
createLayouts grid vws = helper (openPositions grid) grid
    where
        helper :: [Position] -> TreeGrid -> Layouts
        helper [] grid = [grid]
        -- computes the logic firstly for the first postion
        helper (pos:rest) grid =
            -- compute a list of all Trees which exists in the given grid size
            let allTrees = map (*10) [1..length grid]
                -- for all Trees m from allTrees, is the position valid in the grid? (isValidPosition (toInteger m) pos grid). If yes add Tree to potentialTrees
                -- potentialTrees now includes all trees which can be set at the given position
                potentialTrees = [toInteger m | m <- allTrees, isValidPosition (toInteger m) pos grid]
                -- for all potential trees in potentialTrees, make a new grid with this tree m at the given position and and grid, by updating the tree at the given position in the old grid
                -- make a list out of the updated grids (potentialLayouts)
                potentialLayouts = map (\m -> gridWithPotentialTree m pos grid) potentialTrees
            in
                -- recursifly call the helper with the rest of the open postions, for each grid in potentialLayouts and concatinate at the end
                concatMap (helper rest) potentialLayouts

{-|
    doViewsWork
    parameters: ViewPlan, TreeGrid
    outputs: Bool
    function: checks if the particular view matches the row/column in the TreeGrid. In other words for all view elements in viewPlan left/right/bottom/top, it checks, if looked from there, the right number of XTrees are visible
-}
doViewsWork :: ViewPlan -> TreeGrid -> Bool
-- devide the viewPlan into top-, bottom-, right-, left-viewPlan
doViewsWork (top:bottom:right:left) grid =
    -- all four plans has to be valid
    topViewWorks && bottomViewWorks && rightViewWorks && leftViewWorks
    where
        topViewWorks =
            -- (1)for each row in (r) in the grid (grid) (r <- grid), take element form row r at index i (r !! i), and parse it into new list [r !! i | r <- grid]
            -- (2)for all lists generated from (1) at the index i, count the Trees which are visible from this position (countNumTrees (1))
            -- (3)expression holds, if counted number from (2) is the same provided in topviewPlan for every element,
            all (\i -> countNumTrees [r !! i | r <- grid] == top !! i)
                [0..length top -1]
        bottomViewWorks =
            -- does the same like the one above, just has to reverse the derivated computed row since we look from the bottom (reverse [r !! i | r <- grid])
            all (\i -> countNumTrees (reverse [r !! i | r <- grid]) == (bottom !! i))
                [0..length bottom -1]
        leftViewWorks =
            -- does the same like the first, but we can now eaily take the first element (list of XTrees) from the grid
            all (\i -> countNumTrees (grid !! i) == head left !! i )
                [0..length left -1]
        rightViewWorks =
            -- does the same like the third, just has to reverse the first element (list of XTrees) from the grid since we look from the right
            all (\i -> countNumTrees (reverse (grid !! i)) == right !! i)
                [0..length right -1]
                

{-|
    countNumTrees
    parameters: [Integer]
    outputs: Integer
    function: counts number of trees that you can see from a particular viewpoint
-}
countNumTrees :: [Integer] -> Integer
countNumTrees lst = helper lst 0
    where
        helper :: [Integer] -> Integer -> Integer
        helper [] _ = 0
        helper (l:ls) prev
            -- if next tree is higher then provious, add 1 and recursifly call with the rest
            | l > prev = 1 + helper ls l
            | otherwise = helper ls prev

{-|
    isValidPosition
    parameters: XmasTree, Position, TreeGrid
    outputs: Bool
    function: checks whether or not the XmasTree can be placed in that particular 
    Position in the TreeGrid based on whether or not it already exists in that same row/col
-}
isValidPosition :: XmasTree -> Position -> TreeGrid -> Bool
-- checks if its possible to position a tree at a given position, by checking if the tree (Int) is not an element ('notElem') in the list given from the where statement 
isValidPosition tree (i,j) grid = tree `notElem` sameRowAndCol
    where
        -- returns the whole row and the whole column provied from the position in one list
        sameRowAndCol = (grid !! i) ++[row !! j | row <- grid]

{-|
    openPositions
    parameters: TreeGrid
    outputs: [Position]
    function: returns a list of positions that have 0
-}
openPositions :: TreeGrid -> [Position]
-- checks for all 2D postitions in the grid if there is 0, and returns a list with all these postions
openPositions grid = [(i, j) | i<- indices, j <- indices, grid !! i !! j == 0]
    where indices = [0..(length grid-1)]

{-|
    gridWithPotentialTree
    parameters: XmasTree, Position, TreeGrid 
    outputs: TreeGrid
    function: makes a new TreeGrid with the Position occupied by XmasTree
-}
gridWithPotentialTree :: XmasTree -> Position -> TreeGrid -> TreeGrid
-- returns the concatination of the not effected row before the tobeupdated one, the tobeupdated, and the one after
gridWithPotentialTree tree (i, j) grid = prevRows ++ [updatedRow] ++ rest
    where
        -- the previous rows which are not effected (0...i-1)
        prevRows = take i grid
        -- the row in which we update something
        currRow = grid !! i
        -- uodated the tree
        updatedRow = take j currRow ++ [tree] ++ drop (j + 1) currRow
        .. drop the first i+1 rows
        rest = drop (i + 1) grid

{-|
    rowToString
    parameters: [a]
    outputs: String
    function: returns a string that is essentially all elements in a lists delimited by " "
-}
rowToString :: Show a => [a] -> String
rowToString = concatMap (\v -> show v ++ " ")

{-|
    outputSolution
    parameters: Maybe TreeGrid
    outputs: String
    function: returns no solutions if Nothing is passed in, otherwise it prints out a nice grid
-}
outputSolution :: Maybe TreeGrid -> String
outputSolution Nothing = "No solutions found for this layout."
outputSolution (Just grid) = concatMap (\r -> rowToString r ++ "\n") grid

{-|
    isValidGrid
    parameters: TreeGrid
    outputs: Bool
    function: If the grid is 
                - not empty
                - size n x n
                - and all elements are a subset of 0, 10,... (10*length grid)
             then return true, otherwise, it will throw an error
-}
isValidGrid :: TreeGrid -> Bool
isValidGrid [] = error "empty grid"
isValidGrid [[]] = error "empty grid"
isValidGrid grid
    | not squareSize = error "check that your grid is n x n"
    | not allElementsValid = error "one or more trees are invalid"
    | otherwise = True
    where
        squareSize = all (\row -> length row == length grid) grid
        allElementsValid = all (all (\t -> fromIntegral t `elem` map (*10) [0..length grid])) grid

{-|
    isValidView
    parameters: Int, ViewPlan
    outputs: Bool
    function: If the grid is 
                - not empty
                - not n = 0
                - length 4
                - has n view counts for n x n grid
                - has only elements from [1..n]
              then return true, otherwise, it will throw an error
-}
isValidView :: Int -> ViewPlan -> Bool
isValidView _ [] = error "empty view layout"
isValidView 0 _ = error "view insignficant with an empty grid"
isValidView size vws
    | length vws /= 4 = error "view has to be a 4 lists representing each side of a grid"
    | not sameSize = error "each side of the view has to be size n to match with the n x n grid"
    | not allElementsValid = error "one or more views are invalid"
    | otherwise = True
    where
        sameSize = all (\v -> length v == size) vws
        allElementsValid = all (all (\t -> fromIntegral t `elem` [1..length (head vws)])) vws

{-|
    solveXmasTreeProblem
    parameters: TreeGrid, ViewPlan
    outputs: String
    function: returns one solution if there is a solution, prints a no solution statement to the
         console, or throws an error if the inputs aren't valid
-}
solveXmasTreeProblem :: TreeGrid -> ViewPlan -> String
solveXmasTreeProblem grid views= if isValidGrid grid && isValidView (length grid) views
    then solved
    else error "Something wrong"
    where
        -- compute all possible grids, possible in terms if numbers are valid (createLayouts grid views)
        -- filter for these for which the view works (you only see as much trees ans provied in the view)
        -- get one element of the gridList(Layouts) (getOneLayout)
        -- print the solution
        solved = outputSolution $ getOneLayout $ filter (doViewsWork views) (createLayouts grid views)

------------------------------------------------------
-- A.9
------------------------------------------------------
-- see function headers and comments above

------------------------------------------------------
-- A.10
------------------------------------------------------

main :: IO()
main = do
    test_minimum
    test_odds
    test_nodes
    test_minTree
    test_generiere_fib_strom
    test_approximiere_exp
    test_generiere_woerter
    test_filtere_prim_a
    test_filtere_palindrome
    test_transf
    test_wort_gen
    test_XmasTree
    -- test_largeGrid
    putStrLn "TESTING ALL COMPLETE"

test_minimum :: IO()
test_minimum = do
    putStrLn case_break
    putStrLn "TESTING minimum:"
    test_minimum_helper [
        [10,7,3],
        [10,7,3,5,9,4],
        [9]
        ] 0
    putStrLn test_break
    where
        test_minimum_helper :: [[Integer]] -> Int -> IO()
        test_minimum_helper [] _ = putStrLn "DONE"
        test_minimum_helper (l:ls) i = do
            test_print_helper (minimum l) (minimum' l) i
            test_minimum_helper ls (i+1)

test_odds :: IO()
test_odds = do
    putStrLn case_break
    putStrLn "TESTING odds:"
    test_odds_helper [
        [10,7,3],
        [10,7,3,5,9,5,4],
        [10],
        [5]
        ] 0
    putStrLn test_break
    where
        test_odds_helper :: [[Integer]] -> Int -> IO()
        test_odds_helper [] _ = putStrLn "DONE"
        test_odds_helper (l:ls) i = do
            test_print_helper (sum (filter odd l)) (odds l) i
            test_odds_helper ls (i+1)

test_nodes :: IO()
test_nodes = do
    putStrLn case_break
    putStrLn "TESTING nodes:"
    putStrLn case_break
    test_nodes_helper [0,1,3,7] [
        empty_tree,
        tree1,
        tree3,
        tree7
        ] 0
    putStrLn test_break
    where
        test_nodes_helper :: [Integer] -> [Tree t] -> Int -> IO()
        test_nodes_helper _ [] _ = putStrLn "DONE"
        test_nodes_helper (a:as) (l:ls) i = do
            test_print_helper a (nodes l) i
            test_nodes_helper as ls (i+1)


test_minTree :: IO()
test_minTree = do
    putStrLn case_break
    putStrLn "TESTING minTree:"
    putStrLn case_break
    test_nodes_helper [0,0,0] [
        mintree1,
        mintree3,
        mintree7
        ] 0
    putStrLn test_break
    where
        test_nodes_helper :: [Int] -> [NumTree Int] -> Int -> IO()
        test_nodes_helper _ [] _ = putStrLn "DONE"
        test_nodes_helper (a:as) (l:ls) i = do
            test_print_helper a (minTree l) i
            test_nodes_helper as ls (i+1)


test_generiere_fib_strom :: IO()
test_generiere_fib_strom = do
    putStrLn case_break
    putStrLn "TESTING generiere_fib_strom:"
    putStrLn case_break
    print (take 5 generiere_fib_strom)
    print (take 10 $ filter (> 100) generiere_fib_strom)
    print (take 10 $ filter (< 10000) generiere_fib_strom)
    putStrLn test_break

test_approximiere_exp :: IO()
test_approximiere_exp = do
    putStrLn case_break
    putStrLn "TESTING approximiere_exp:"
    putStrLn case_break
    print (take 10 squared)
    print (take 10 factorial)
    print (approximiere_exp 10)
    putStrLn test_break

test_generiere_woerter :: IO()
test_generiere_woerter = do
    putStrLn case_break
    putStrLn "TESTING generiere_woerter:"
    putStrLn case_break
    print (take 100 generiere_woerter)
    putStrLn test_break

test_filtere_prim_a :: IO()
test_filtere_prim_a = do
    putStrLn case_break
    putStrLn "TESTING filtere_prim_a:"
    putStrLn case_break
    print (take 100 (filtere_prim_a generiere_woerter))
    putStrLn test_break

test_filtere_palindrome :: IO()
test_filtere_palindrome = do
    putStrLn case_break
    putStrLn "TESTING filtere_palindrom:"
    putStrLn case_break
    print (take 100 (filtere_palindrome generiere_woerter))
    putStrLn test_break

test_transf :: IO()
test_transf = do
    putStrLn case_break
    putStrLn "TESTING transf:"
    putStrLn case_break
    print (take 100 (transf generiere_woerter))
    putStrLn test_break

test_wort_gen :: IO()
test_wort_gen = do
    putStrLn case_break
    putStrLn "TESTING wort_gen:"
    putStrLn case_break
    print (take 100 wort_gen)
    putStrLn test_break

--------------------------
-- Xmastree problem testing below
--------------------------


test_XmasTree :: IO()
test_XmasTree = do

    putStrLn case_break
    putStrLn "BEGINNING test_XmasTree"
    putStrLn case_break

    putStrLn "testing empty grid....."
    E.catch (putStrLn $ solveXmasTreeProblem emptyProblem emptyProblemView)
                (\err -> print (err :: E.SomeException))
    putStrLn test_break

    putStrLn "testing wrong size grid....."
    putStrLn $ outputSolution (Just wrongSize)
    E.catch (putStrLn $ solveXmasTreeProblem wrongSize wrongSizeView)
                (\err -> print (err :: E.SomeException))
    putStrLn test_break

    putStrLn "testing grid with invalid tree....."
    putStrLn $ outputSolution (Just invalidTrees)
    E.catch (putStrLn $ solveXmasTreeProblem invalidTrees invalidTreesView)
                (\err -> print (err :: E.SomeException))
    putStrLn test_break

    putStrLn "testing grid with invalid view....."
    putStrLn $ "invalid view: \n" ++ outputSolution (Just invalidView)
    E.catch (putStrLn $ solveXmasTreeProblem exampleProblem invalidView)
                (\err -> print (err :: E.SomeException))
    putStrLn test_break

    putStrLn "testing grid with wrong length views....."
    putStrLn $ "Not square view: " ++ show notSquareView
    E.catch (putStrLn $ solveXmasTreeProblem exampleProblem notSquareView)
                (\err -> print (err :: E.SomeException))
    putStrLn test_break

    putStrLn "testing small grid....."
    E.catch (putStrLn $ solveXmasTreeProblem smallProblem smallViewPlan)
                (\err -> print (err :: E.SomeException))
    putStrLn test_break

    test_exampleGrid
    -- test_largeGrid


test_exampleGrid :: IO ()
test_exampleGrid = do
    putStrLn "testing example grid....."
    E.catch (putStrLn $ solveXmasTreeProblem exampleProblem exampleViewPlan)
                (\err -> print (err :: E.SomeException))
    putStrLn test_break

test_largeGrid :: IO ()
test_largeGrid = do
    putStrLn "testing large grid....."
    E.catch (putStrLn $ solveXmasTreeProblem largeGrid largeGridPlan)
                (\err -> print (err :: E.SomeException))
    putStrLn test_break

--------edge cases---------
emptyProblem :: TreeGrid
emptyProblem = [[]]

emptyProblemView :: ViewPlan
emptyProblemView = [[]]

wrongSize :: TreeGrid
wrongSize = [[0,0]]

wrongSizeView :: ViewPlan
wrongSizeView = [[0,0]]

invalidTrees :: TreeGrid
invalidTrees = [
    [10,0,70,0,50],
    [0,30,0,50,0],
    [0,0,0,10,20],
    [40,50,0,20,0],
    [50,0,20,0,40]
    ]

invalidTreesView :: ViewPlan
invalidTreesView = [[0,0]]

invalidView :: ViewPlan
invalidView = let
        top = [5,4,3,2,0]
        right = [1,2,2,2,2]
        left = [5,4,3,2,1]
        bottom = [1,2,2,2,2]
    in [top, bottom, right, left]

notSquareView :: ViewPlan
notSquareView = let
        top = [5,4,3,2,0]
        right = [1,2,2,2,2]
        left = [5,4,3,2,1]
        bottom = [1,2,2,2]
    in [top, bottom, right, left]

-------------small Grid--------------
smallProblem :: TreeGrid
smallProblem = [[10,0], [0,0]]

smallSolution :: TreeGrid
smallSolution = [[10,20], [20,10]]

smallViewPlan :: ViewPlan
smallViewPlan = let
        top = [2,1]
        right = [1,2]
        left = [2,1]
        bottom = [1,2]
    in [top, bottom, right, left]

-----------Ubung Example----------------
exampleViewPlan :: ViewPlan
exampleViewPlan = let
        top = [5,4,3,2,1]
        right = [1,2,2,2,2]
        left = [5,4,3,2,1]
        bottom = [1,2,2,2,2]
    in [top, bottom, right, left]


exampleProblem :: TreeGrid
exampleProblem = [
    [10,0,30,0,50],
    [0,30,0,50,0],
    [0,0,0,10,20],
    [40,50,0,20,0],
    [50,0,20,0,40]
    ]

exampleSolution :: TreeGrid
exampleSolution = [
    [10,20,30,40,50],
    [20,30,40,50,10],
    [30,40,50,10,20],
    [40,50,10,20,30],
    [50,10,20,30,40]
    ]
------------Large Grid---------------
largeN :: Integer
largeN = 15

n :: Int
n = fromIntegral largeN

largeGrid :: TreeGrid
largeGrid = [
    [10,0,0,0,0,0,0,0,0,0,0,120,0,140,150],
    [0,30,40,50,60,70,80,90,100,110,0,0,140,150,10],
    [0,0,50,60,0,80,90,0,110,0,0,140,150,10,20],
    [0,50,60,0,80,0,0,0,0,0,0,150,0,0,30],
    [0,60,70,0,0,100,110,0,0,140,150,10,20,30,40],
    [60,70,0,0,100,110,120,0,140,150,10,20,30,40,50],
    [70,0,0,100,110,0,0,140,150,10,0,0,0,0,0],
    [0,0,100,110,0,130,140,150,10,0,0,0,0,0,70],
    [90,0,0,0,130,140,150,10,0,30,0,50,60,70,80],
    [100,0,0,0,140,150,0,0,0,0,0,0,70,80,90],
    [0,120,0,140,150,10,0,30,40,50,0,70,0,90,100],
    [0,0,140,0,10,20,0,40,50,60,70,0,0,100,110],
    [130,140,150,10,0,0,0,0,0,70,0,0,0,110,120],
    [140,0,10,20,30,40,0,60,70,0,0,0,110,0,0],
    [150,10,20,30,0,50,60,70,0,0,100,110,0,0,140]
    ]

largeGridPlan :: ViewPlan
largeGridPlan = let
        top = [largeN,(largeN-1)..1]
        right = 1 : replicate (n-1) 2
        left = [largeN,(largeN-1)..1]
        bottom = 1 : replicate (n-1) 2
    in [top, bottom, right, left]

-------------Other definitions-------------

case_break :: String
case_break = "-------------"

test_break :: String
test_break=">\n>\n>\n"

test_print_helper :: (Show a, Eq a, Show b) => a -> a -> b -> IO()
test_print_helper act exp i= do
    putStrLn $ "case "++show i++": "++show exp++" == "++
        show act ++ " "++ show (exp == act)

empty_tree :: Tree a
empty_tree = Nil

tree1 :: Tree Int
tree1 = Node Nil 0 Nil

tree3 :: Tree Int
tree3 = Node left 0 right
    where
        left = Node Nil 1 Nil
        right = Node Nil 2 Nil

tree7 :: Tree Int
tree7 = Node child1 0 child2
    where
        child1 = Node (Node Nil 3 Nil) 1 (Node Nil 4 Nil)
        child2 = Node (Node Nil 5 (Node Nil 6 Nil)) 2 Nil

mintree1 :: NumTree Int
mintree1 = NumNode Empty 0 Empty

mintree3 :: NumTree Int
mintree3 = NumNode left 0 right
    where
        left = NumNode Empty 1 Empty
        right = NumNode Empty 2 Empty

mintree7 :: NumTree Int
mintree7 = NumNode child1 0 child2
    where
        child1 = NumNode (NumNode Empty 3 Empty) 1 (NumNode Empty 4 Empty)
        child2 = NumNode (NumNode Empty 5 (NumNode Empty 6 Empty)) 2 Empty
