module Main where

{-
Haskell implementation of AInet based symbolic regression algorithm, using
the IT datastructure.

TODO LIST
--trainTestSplit method that returns the dataset divided in (Train, Test)
--create a better textRepresentation, or make Le derive show
--use a filter in unique (if possible)
--use gradient descent if det is zero in adjust method
--create better comments (check if haskell have some standard of comments, like python have the "numPy way")
--implement random LE generator
--implement mutation
--make mutation random
--implement simplify
--make ainet recursive fuction caudal

-}

import Data.Matrix
import System.Random
import Data.List     (sort)


-- DATASET OPERATIONS ----------------------------------------------------------
--defining the auxiliary types for representing the dataset
type X         = Matrix Double
type Y         = Matrix Double
type Dataset   = (X, Y)
type Datapoint = ([Double], Double)

--test dataset, function id
sample = [[1.0,1.0,1.0,1.0,1.0 ],
          [2.0,2.0,2.0,2.0,4.0 ],
          [3.0,3.0,3.0,3.0,9.0 ],
          [4.0,4.0,4.0,4.0,16.0],
          [5.0,5.0,5.0,5.0,25.0],
          [6.0,6.0,6.0,6.0,36.0]]

--returns a dataset
listsToDataset :: [[Double]] -> Dataset
listsToDataset lss = (fromLists xs, fromLists ys)
    where
        xs = [init ls   | ls <- lss] :: [[Double]]
        ys = [[last ls] | ls <- lss] :: [[Double]]

--returns a specific element from the dataset, in the form of (list, Double)
(#) :: Int -> Dataset -> Datapoint
(#) i (xss,ys) = (xs', y')
    where
        xs' = [xss ! (i, j) | j <- [1..ncols xss]]
        y' = ys ! (i, 1)

--trainTestSplit :: Dataset -> (Dataset, Dataset) NEEDS A RANDOM GENERATOR


-- EXPRESSION OPERATIONS -------------------------------------------------------
--defining the auxiliary types for representing the IT
type Score = Double
type Coeff = Double
type Exps  = [Int]
type Op    = Int
type It    = (Coeff, Op, Exps)
type Le    = [It]

--operators used for the ITs. if you want a new operator, first add it here
--then create an aplication of the operator in the "solve" method
ops = ["id", "sin", "cos", "tan", "abs", "sqrt", "exp", "log"] :: [String]

--returns an LE that is a linear combination of n variables
linearExpression :: Int -> Le
linearExpression n = [(1.0, 0, e) | e <- (exps n)]
    where
        exps n  = [one n i | i <- [1..n]]
        one n i = [gen i j | j <- [1..n]]
        gen i j
            | i == j    = 1
            | otherwise = 0

--get a friendly printable expression to better understanding
textRepresentation :: Le -> String
textRepresentation le = concat[show c ++ "*" ++ ops !! o ++ exps e | (c, o, e) <- le]
    where
        exps e = "(" ++ concat[show ei ++ " " | ei <- e] ++ ") "

--returns an LE without duplicated elements (matrix propriety: if two lines
--are exactly the same, then the DET A = 0, so A is not inversible). we need
--to invert the matrix to adjust coeffs! so, it's important to remove duplicated
--ITs from an LE
uniques :: Le -> Le
uniques [x] = [x]
uniques (x:xs)
    | x `elem` xs = uniques xs
    | otherwise   = x : uniques xs

--returns an array with evaluated values for the given datapoint
solve :: Le -> Datapoint -> [Double]
solve le (xs, y) = [(eval o e) * c | (c, o, e) <- le]
    where
        value exps = product[x^e | (e, x) <- zip exps xs]
        eval op exps
            | op == 0   = id   $       value exps
            | op == 1   = sin  $       value exps
            | op == 2   = cos  $       value exps
            | op == 3   = tan  $       value exps
            | op == 4   = abs  $       value exps
            | op == 5   = sqrt $ abs $ value exps
            | op == 6   = exp  $       value exps
            | op == 7   = log  $       value exps
            | otherwise = error ("Unknown operator")

--returns an array containing the optimum coeffs using OLS or an array of 1s if error
--NOTE: adjusting an expression twice results in the expression with all coeffs being 1
adjust :: Le -> Dataset -> Le
adjust le ds = [(c', o, e) | (c', (c, o, e)) <- zip (toList coeffs) le]
    where
        x          = fromLists [solve le (i # ds) | i <- [1..nrows $ fst ds]]
        xt         = transpose x
        xtxi       = inverse $ multStd xt x
        xty        = multStd xt $ snd ds
        coeffs     = either (gradient) (`multStd` xty) (xtxi)
        gradient a = fromList 1 (length le) [c | (c,o,e) <-le]

--returns the SCORE for a LE for a given dataset
evaluate :: Le -> Dataset -> Score
evaluate le ds = 1 / (1 + mae)
    where
        mae     = sum[abs $ diff (i # ds) | i <- [1..nrows $ fst ds]] / length
        diff dp = (product $ solve le dp) - (snd dp)
        length  = fromIntegral $ nrows $ fst ds
        

-- AINET ALGORITHM -------------------------------------------------------------
type PopSize     = Int
type LeSize      = Int
type NumGen      = Int
type NumClones   = Int
type SimplifyT   = Double
type SupressionT = Double

--suposes that all the Le in the pop is adjusted
sortByScore :: [Le] -> Dataset -> [Le]
sortByScore pop ds = [le' | (sort, le') <- sorted] 
    where
        sorted = sort[(evaluate le ds, le) | le <- pop]

--ainet' suposes that the given populaion is always ordened by worst to best score
--ainet' is a recursive call to simulate the interaction
ainet' :: [Le] -> NumGen -> PopSize -> NumClones -> SupressionT -> StdGen -> Dataset -> [Le]
ainet' pop 0 p c sT rndGen ds = []
ainet' pop g p c sT rndGen ds = ainet' newPop (g-1) p c sT rndGen ds
    where
        --the number of clones is the index of each solution (the solutions should be ordered from worst to best, this way there will be more best solutions and few bad solutions)
        clones = concat[ [pop !! leIndex | i<- [1..leIndex]] | leIndex <- [1..length pop] ]

        --mutate the clones and adjust them
        mutatedClones = [adjust solution ds | solution <- [mutate solution | solution <- clones]]

        newPop = pop ++ mutatedClones
        --add new random solutions to the pop (need to create and adjust them)
        --sort the newPop
        --drop the first n solutions with score less than supression s from the newPop and send it to the recursive call

--takes a number of generations and performs an AInet based symbolic regression
--for the given number of generations
ainet :: NumGen -> PopSize -> LeSize -> NumClones -> SupressionT -> SimplifyT -> Dataset -> Le
ainet g p l c sT sS ds = last $ simplify (ainet' (sortByScore pop ds) g p c sT nextGen ds)
    where
        seed = 42 :: Int --seed
        generator = mkStdGen seed :: StdGen --generator

        (pop, nextGen) = rndPopulation generator p l ds

        --creates a new pop and adjust its coeffs
        --pop = [adjust solution ds | solution <- [linearExpression $ ncols $ fst ds | i <- [1.. p]]] --todo: make it create p random solutions, instead of all being the root

        simplify pop = pop


-- EXAMPLES --------------------------------------------------------------------
aux = listsToDataset sample
le = linearExpression $ ncols $ fst aux :: Le
--adjust (uniques le) aux


-- MAIN METHOD -----------------------------------------------------------------
main :: IO ()
main = do
    putStrLn "hello world"