-- Problem 7.3
{- |
    Module: BoolExpr.hs
-}
module BoolExpr (Variable, BoolExpr(..), evaluate, variables, subsets, truthtable) where
-- Imported Data.List for union and sort function
import Data.List
type Variable = Char

-- Defined Data Type BoolExpr
data BoolExpr
    = T
    | F
    | Var Variable
    | Not BoolExpr
    | And BoolExpr BoolExpr
    | Or BoolExpr BoolExpr
    deriving (Show)

-- a) Code variables
-- Function takes a BoolExpr and returns a list of Variables
-- Pattern match the BoolExpr for every case that BoolExpr is defined for
-- T, F and Var BoolExpr are the base cases
-- Not BoolExpr calls itself again with argument BoolExpr
-- And and Or return the sorted union of the recursive calls of variables
variables :: BoolExpr -> [Variable]
variables T = []
variables F = []
variables (Var v)           = [v]
variables (Not v)           = variables v
variables (And e1 e2)       = sort(union (variables e1) (variables e2))
variables (Or e1 e2)        = sort(union (variables e1) (variables e2))

-- b) Code subsets
-- Function takes a list of variables and returns the powerset of the list
-- Base case is the empty list which returns a list with the empty list as the 
-- only element
-- x:xs divides the list into its head and tail
-- It calls itself with the tail and appends the map of the subset with its head
-- function 
subsets :: [Variable] -> [[Variable]]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ (map (x:) (subsets xs))

-- b) Code truthtable
-- Function takes a BoolExpr and returns the entire truth table of the BooExpr
-- using a list of tuples made of each interpretation and its evaluation
-- Function maps each element of the powerset of the variables of the BoolExpr
-- to produce a tuple made of the element and the evaluation of the BoolExpr
-- with the element as its interpretation
-- Mapping all of the interpretations produces a list of tuples that is the 
-- whole truth table of the BoolExpr
truthtable :: BoolExpr -> [([Variable], Bool)]
truthtable expr = map (\x -> (x, evaluate expr x)) (subsets (variables expr))

-- Code evaluate
-- Evaluates a BoolExpr based on pattern matching and recursively calling itself
evaluate :: BoolExpr -> [Variable] -> Bool
evaluate T _                = True
evaluate F _                = False
evaluate (Var v) vs         = v `elem` vs
evaluate (Not e) vs         = not (evaluate e vs)
evaluate (And e1 e2) vs     = evaluate e1 vs && evaluate e2 vs
evaluate (Or e1 e2) vs      = evaluate e1 vs || evaluate e2 vs

main = do
    print(variables T)
    print(variables (And (Var 'a') (Or (Var 'c') (Var 'b'))))
    print(variables (And (Var 'a') (Or (Var 'a') (Var 'a'))))
    print(subsets (variables (And (Var 'a') (Or (Var 'c') (Var 'b')))))
    print(truthtable (And (Var 'a') (Or (Var 'c') (Var 'b'))))