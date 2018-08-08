module Main where

{-
Haskell implementation of AInet based symbolic regression algorithm, using
the IT datastructure.

TODO LIST
--trainTestSplit method that returns the dataset divided in (Train, Test)
--create a better textRepresentation, or make Le derive show
--use gradient descent if det is zero in adjust method
--create better comments (check if haskell have some standard of comments, like python have the "numPy way")
--implement random LE generator
--implement mutation
--make mutation random
--implement simplify
--make ainet recursive fuction caudal
--change for map later simplifyPop if convenient
--remove the unsafe on traintestsplit

-}

import Data.Matrix
import System.Random
import Data.List     (sort)
import System.IO.Unsafe (unsafePerformIO) --this should be eliminated as we learn how to use IO monads


-- DATASET OPERATIONS ----------------------------------------------------------
--defining the auxiliary types for representing the dataset
type X         = Matrix Double
type Y         = Matrix Double
type Dataset   = (X, Y)
type Datapoint = ([Double], Double)

--test dataset, function id with 4 explanatory variables
sample = [[1.0,1.0,1.0,1.0,1.0],
          [2.0,2.0,2.0,2.0,2.0],
          [3.0,3.0,3.0,3.0,3.0],
          [4.0,4.0,4.0,4.0,4.0],
          [5.0,5.0,5.0,5.0,5.0],
          [6.0,6.0,6.0,6.0,6.0]]

--returns a dataset
listsToDataset :: [[Double]] -> Dataset
listsToDataset lss = (fromLists xs, fromLists ys)
    where
        xs = [init ls   | ls <- lss] :: [[Double]]
        ys = [[last ls] | ls <- lss] :: [[Double]]

--returns a specific element from the dataset, in the form of ([Double], Double)
(#) :: Int -> Dataset -> Datapoint
(#) i (xss,ys) = (xs', y')
    where
        xs' = [xss ! (i, j) | j <- [1..ncols xss]]
        y' = ys ! (i, 1)

--returns a shuffled dataset (changes 2 random rows n times)
shuffle :: Dataset -> Int -> Dataset
shuffle ds 0     = ds
shuffle (x, y) n = (x', y')
    where
        --rows to change
        row1 = unsafePerformIO $ randomRIO(1, nrows x)
        row2 = unsafePerformIO $ randomRIO(1, nrows x)

        --new randomly arranged values
        x' = switchRows row1 row2 $ fst $ previouslysorted
        y' = switchRows row1 row2 $ snd $ previouslysorted
        previouslysorted = shuffle (x,y) (n-1)

--trainTestSplit :: Dataset -> Int -> (Dataset, Dataset)


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
ops      = [ id ,  sin ,  cos ,  tan ,  abs ,  sqrt.abs ,  exp ,  log ] :: [Double -> Double]
opsNames = ["id", "sin", "cos", "tan", "abs", "sqrt.abs", "exp", "log"] :: [String]

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
textRepresentation le = concat[show c ++ "*" ++ opsNames !! o ++ exps e | (c, o, e) <- le]
    where
        exps e = "(" ++ concat[show ei ++ " " | ei <- e] ++ ") "

--returns an LE without duplicated elements (matrix propriety: if two lines
--are exactly the same, then the DET A = 0, so A is not inversible). we need
--to invert the matrix to adjust coeffs! so, it's important to remove duplicated
--ITs from an LE
uniques :: Le -> Le
uniques [] = []
uniques (x:xs) = uniques $ filter (x/=) xs

--returns an array with evaluated values for the given datapoint
solve :: Le -> Datapoint -> [Double]
solve le (xs, y) = [(eval o e) * c | (c, o, e) <- le]
    where
        value exps = product[x^e | (e, x) <- zip exps xs]
        eval op exps
            | op >= length ops = error ("Unknown operator")
            | otherwise        = ops !! op $ value exps
--returns an array containing the optimum coeffs using OLS or an array of 1s if error
--NOTE: adjusting an expression twice results in the expression with all coeffs being 1
adjust :: Le -> Dataset -> Le
adjust le ds = [(c', o, e) | (c', (c, o, e)) <- zip (toList coeffs) le]
    where
        x        = fromLists [solve le (i # ds) | i <- [1..nrows $ fst ds]]
        xt       = transpose x
        xty      = xt `multStd` (snd ds)
        coeffs   = case (inverse $ xt `multStd` x) of
            Left msg  -> (gradient)
            Right inv -> (inv `multStd` xty) 
        gradient = fromList 1 (length le) [c | (c,o,e) <-le]

--returns the SCORE for a LE for a given dataset
evaluate :: Le -> Dataset -> Score
evaluate le ds = 1 / (1 + mae)
    where
        mae     = sum[abs $ diff (i # ds) | i <- [1..nrows $ fst ds]] / length
        diff dp = (product $ solve le dp) - (snd dp)
        length  = fromIntegral $ nrows $ fst ds

--returns a simplified expression
simplify :: Le -> SimplifyT -> Maybe Le
simplify le sT = if length simplified == 0 then Nothing --garantee to return a non empty le
                 else Just simplified
    where simplified = [(c, o, e) | (c,o,e) <- le, c>= sT]
   
--returns a mutated LE
mutate :: Le -> Le
mutate le = le


-- IGNORAR DAQUI PARA BAIXO: TUDO FOI FEITO NA FORÇA DA GAMBIARRA --------------
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

--returns a random population and a new random generator
rndPopulation :: StdGen -> PopSize -> LeSize -> Dataset -> ([Le], StdGen)
rndPopulation generator p l ds = ([linearExpression $ ncols $ fst ds | i <- [1.. p]], generator)

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
ainet g p l c sT sS ds = last $ simplifyPop (ainet' (sortByScore pop ds) g p c sT nextGen ds)
    where
        seed = 42 :: Int --seed
        generator = mkStdGen seed :: StdGen --generator

        (pop, nextGen) = rndPopulation generator p l ds

        --creates a new pop and adjust its coeffs
        --pop = [adjust solution ds | solution <- [linearExpression $ ncols $ fst ds | i <- [1.. p]]] --todo: make it create p random solutions, instead of all being the root

        simplifyPop pop = sortByScore (re'pop [] pop) ds
        re'pop ps []         = ps
        re'pop ps (le:les') = case simplify le sT of
            Nothing  -> re'pop ps les'
            Just le' -> ps' `seq` re'pop ps' les'
                where
                    ps' = le':ps


-- EXAMPLES --------------------------------------------------------------------
aux = listsToDataset sample
le = linearExpression $ ncols $ fst aux :: Le
--adjust (uniques le) aux


-- MAIN METHOD -----------------------------------------------------------------
main :: IO ()
main = do
    let seed = 42 :: Int --random seed
    setStdGen $ mkStdGen seed --making the behaviour deterministig for reproducibility

    putStrLn "hello world"
