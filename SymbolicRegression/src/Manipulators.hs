module Manipulators where

{-
Module containing implementation of expression manipulation functions.
-}

import Dataset
import System.Random
import Data.Matrix (fromLists,
                    fromList,
                    toList,
                    nrows,
                    ncols,
                    transpose,
                    multStd,
                    inverse)
import Data.List   (sort)


-- EXPRESSION OPERATIONS -------------------------------------------------------
--defining the auxiliary types for representing the IT
newtype Score = Score Double
        deriving (Eq, Ord, Show)
type Coeff = Double
type Exps  = [Int]
type Op    = Int
type It    = (Coeff, Op, Exps)
type Le    = [It]
type Pop   = [Le]
type Operator = (Double -> Double)
type Op'n'Name = (Operator, String)
type SimplifyT   = Double
type SupressionT = Double
type PopSize     = Int
type LeSize      = Int


-- OPERATORS -------------------------------------------------------------------
--operators used for the ITs. if you want a new operator, first add it here
--then create an aplication of the operator in the "solve" method
ops = [
    (id, "id"),
    (sin, "sin"),
    (cos, "cos"),
    (tan, "tan"),
    (abs, "abs"),
    (exp, "exp"),
    (log, "log"),
    (sqrt.abs, "sqrt.abs")
    ] :: [Op'n'Name]

operator :: Op'n'Name -> Operator
--returns the operator from a tuple
operator (o,_) = o

nomeator :: Op'n'Name -> String
--returns the name associated with the operator
nomeator (_,s) = s

--MANIPULATORS ----------------------------------------------------------------
--EXPRESSION CREATORS -----------------------------------------------------------
linearExpression :: Int -> Le
--returns an LE that is a linear combination of n variables
linearExpression n = [(1.0, 0, e) | e <- (exps n)]
    where
        exps n  = [one n i | i <- [1..n]]
        one n i = [gen i j | j <- [1..n]]
        gen i j
            | i == j    = 1
            | otherwise = 0

randomExps :: Int -> IO ([Int])
--return random expoents
randomExps 0 = do return []
randomExps n = do
    r  <- randomRIO(0,7)
    r' <-  randomExps (n-1)
    return (r:r')

randomIt :: Int -> IO ((Double, Int, [Int]))
--return an it of n variables
randomIt n = do
    randOp <- randomRIO(0, (length ops-1))
    exps <- randomExps n
    return (1.0, randOp, exps)

randomExpression :: Int -> Int -> IO (Le)
--returns a random LE of m its of n variables
randomExpression n 0 = do return []
randomExpression n m = do
    le  <- randomIt n
    le' <- randomExpression n (m-1)
    return (le:le')

textRepresentation :: Le -> String
--get a friendly printable expression to better understanding
textRepresentation le = init $ concat [monomio it | it <- le]
    where
        monomio (c, o, e) = show c ++ "*" ++ nome o ++ exps e
        nome o' = nomeator (ops !! o')
        exps e' = "(" ++ concat[show ei ++ " " | ei <- e'] ++ ") "

-- (matrix propriety: if two lines
--are exactly the same, then the DET A = 0, so A is not inversible). we need
--to invert the matrix to adjust coeffs! so, it's important to remove duplicated
--ITs from an LE
uniques :: Le -> Le
--returns an LE without duplicated elements 
uniques [] = []
uniques (x:xs) = uniques $ filter (x/=) xs

solve :: Le -> DataPoint -> [Double]
--returns an array with evaluated values for the given datapoint
solve le (xs, y) = [(eval o e) * c | (c, o, e) <- le]
    where
        value exps = product[x^e | (e, x) <- zip exps xs]
        eval op exps
            | op >= length ops = error ("Unknown operator")
            | otherwise        = operator (ops !! op) $ value exps

--NOTE: adjusting an expression twice results in the expression with all coeffs being 1
adjust :: Le -> Dataset -> Le
--returns an array containing the optimum coeffs using OLS or an array of 1s if error
adjust le ds = [(c', o, e) | (c', (c, o, e)) <- zip (toList coeffs) le]
    where
        x        = fromLists [solve le (i # ds) | i <- [1..nrows $ fst ds]]
        xt       = transpose x
        xty      = xt `multStd` (snd ds)
        coeffs   = case (inverse $ xt `multStd` x) of
            Left msg  -> (gradient)
            Right inv -> (inv `multStd` xty) 
        gradient = fromList 1 (length le) [c | (c,o,e) <-le]

evaluate :: Le -> Dataset -> Score
--returns the SCORE for a LE for a given dataset
evaluate le ds = Score (1.0 / (1.0 + mae))
    where
        mae     = sum[abs $ diff (i # ds) | i <- [1..(nrows $ fst ds)]] / length
        diff dp = (product $ solve le dp) - (snd dp)
        length  = fromIntegral $ nrows $ fst ds

simplify :: Le -> SimplifyT -> Maybe Le
--returns a simplified expression
simplify le sT
    | leng == 0 = Nothing --garantee to return a non empty le
    | otherwise = Just simplified
    where
        simplified = [(c, o, e) | (c,o,e) <- le, c>= sT]
        leng = length simplified

mutateTrans :: Le -> IO (Le)
--returns a mutated LE
mutateTrans le = do
    chosenIndex <- randomRIO(0, length le -1)
    randOp <- randomRIO(0, (length ops-1))
    let newIt = [(c, randOp, e) | (c, o, e) <- [le !! chosenIndex]]
    let le' = [it | it<-le, it /= (le !! chosenIndex)]

    return (le' ++ newIt)

mutateInter :: Int -> Le -> IO (Le)
--returns a mutated LE. you need to pass the number of variables of the problem
mutateInter n le = do
    chosenIndex <- randomRIO(0, length le -1)
    randExps <- randomExps n
    let newIt = [(c, o, randExps) | (c, o, e) <- [le !! chosenIndex]]
    let le' = [it | it<-le, it /= (le !! chosenIndex)]

    return (le' ++ newIt)


--POPULATION METHODS -----------------------------------------------------------
simplifyPop :: Pop -> SimplifyT -> Pop
-- | calls the simplify method in all expressions, and handles with errors
simplifyPop pop sS = re'pop [] pop
    where
        re'pop p_p []        = p_p
        re'pop p_p (le:les') = case simplify le sS of
            Nothing  -> re'pop p_p les'
            Just le' -> p_p' `seq` re'pop p_p' les'
                where p_p' = le':p_p

sortByScore :: Pop -> Dataset -> Pop
-- | suposes that all the Le in the pop is adjusted. sorts the population by score
sortByScore pop ds = [le' | (sort, le') <- sorted] 
    where
        sorted = sort[(evaluate le ds, le) | le <- pop]

mutatePop :: Pop -> Int -> IO (Pop)
-- | Calls the mutation method in all expressions. need to pass as argument the number of variables
mutatePop [] n = do return []
mutatePop (p:pop) n = do
    mutatedI <- mutateInter n p
    mutatedIT <- mutateTrans mutatedI
    remaining <- mutatePop pop n 
    return (mutatedIT:remaining)

rndPopulation ::  PopSize -> LeSize -> Dataset -> IO (Pop)
--returns a random population
rndPopulation 0 _ _ = do return []
rndPopulation p l ds = do
    le  <- randomExpression (ncols $ fst ds) l
    le' <- rndPopulation (p-1) l ds
    return (le:le')

supressionPop :: Pop -> SupressionT -> Pop
--returns a population without the supressed expressions
supressionPop pop supT = pop