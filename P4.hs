--
-- CS 430 P4 (Haskell 2)
--
-- Name: James Schader
-- 20220310
--

module P4 where
import Defs

-- calculate the result of an expression
eval :: Expr -> Int
eval e = -1

-- count the total number of arithmetic operations in an expression
countOps :: Expr -> Int
countOps e = -1

-- calculate the height of the expression tree
-- height of one node = 1
height :: Expr -> Int
height e = -1

-- flatten the expression into a postfix string representation
-- use "(show i)" to convert int i to a string
postfix :: Expr -> String
postfix e = ""

-- extract a sorted list of all unique integers in an expression
uniqInts :: Expr -> [Int]
uniqInts e = []


-- helper function for uniqInts.
-- given a list, remove duplicates
uniq :: Eq t => [t] -> [t]
uniq (x:xs) = xs

-- helper function for uniqInts.
-- given a list, sort it
sort :: Ord t => [t] -> [t]
sort (x:xs) = sort ((partitionLess x xs) : sort (partitionMore x xs))

partitionLess :: Ord a => a -> [a] -> [a]
partitionLess v (t:ts) = filter (\x -> v >= x) ts

partitionMore :: Ord a => a -> [a] -> [a]
partitionMore v (t:ts) = filter (\x -> v < x) ts

