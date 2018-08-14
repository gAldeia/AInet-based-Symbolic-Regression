{-|
Module      : Manipulators
Description : Dataset creation and manipulation 
Copyright   : (c) Guilherme S I Aldeia, 2018
                  Heitor R Savegnago, 2018
License     : GPL-3
Maintainer  : guilherme.aldeia@aluno.ufabc.edu.br
              heitor.rodrigues@aluno.ufabc.edu.br
Stability   : experimental
Portability : POSIX

Module containing implementation of expression manipulation functions.
-}

module Manipulators where

import Dataset
import System.Random
import Data.List   (sort)
import Data.Matrix (fromLists, fromList, toList, --lists manipulation
                    transpose, multStd, inverse, --matrix operations
                    nrows, ncols)                --accessing functions


-- Types declarations ----------------------------------------------------------
newtype Score    = Score Double         deriving (Eq, Ord, Show)
-- ^Double ranging from [0,1] indicating the performance of the solution for 
-- a given dataset

newtype Coeff    = Coeff Double         deriving (Eq, Ord, Show)
-- ^Coefficient associated with the ITs

newtype Exps     = Exps [Int]           deriving (Eq, Ord, Show)
-- ^Vector of expoents of ITs, to be applied to the samples on IT evaluation

newtype Op       = Op Int               deriving (Eq, Ord, Show)
-- ^ 	Index of the operator of the ITs

newtype It       = It (Coeff, Op, Exps) deriving (Eq, Ord, Show)
-- ^IT data structure

type Le          = [It]
-- ^Linear combination of ITs

type Pop         = [Le]
-- ^Vector containing many Les, called population

type Operator    = (Double -> Double)
-- ^One-argument functions, used to compose ITs

type Op'n'Name   = (Operator, String)
-- ^Tuple containing One operator and one string to print it

type SimplifyT   = Double
-- ^Simplification threshold

type SupressionT = Int
-- ^Supression threshold

type PopSize     = Int
-- ^Size of the population

type LeSize      = Int
-- ^Size of the expressions


-- Operators creation and manipulation -----------------------------------------
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
-- ^set of operators used to create ITs. Each operator is a tuple containing a
--  function (Double -> Double) and a string to print the name of the operation.
--  To add new operators to the ITs, create a new tuple of type Op'n'Name here.

operator :: Op'n'Name -> Operator
-- ^Takes one Op'n'Name tuple and returns the operator (first element).
operator (o,_) = o

nomeator :: Op'n'Name -> String
-- ^Takes one Op'n'Name tuple and returns the name (second element).
nomeator (_,s) = s


-- Expressions creation --------------------------------------------------------
linearExpression :: Int -> Le
-- ^Takes the number of variables n and returns an Le with n ITs, where each
--  the linear combination of the ITs results in a linear combination of the n
--  variables.
linearExpression n = [It (Coeff 1.0, Op 0, Exps e) | e <- (exps n)]
    where
        exps n  = [one n i | i <- [1..n]]
        one n i = [gen i j | j <- [1..n]]
        gen i j
            | i == j    = 1
            | otherwise = 0

randomExps :: Int -> IO ([Int])
-- ^Takes the number of variables n and return n random expoents ranging from
--  [0,7].
randomExps 0 = do return []
randomExps n = do
    r  <- randomRIO(0,7)
    r' <- randomExps (n-1)
    return (r:r')

randomIt :: Int -> IO (It)
-- ^Takes the number of variables n and return an random IT.
randomIt n = do
    randOp <- randomRIO(0, (length ops-1))
    exps   <- randomExps n
    return (It (Coeff 1.0, Op randOp, Exps exps))

randomExpression :: Int -> Int -> IO (Le)
-- ^Takes the number of ITs m and the number of variables n and returns a random
--  LE of m its, each IT having n variables.
randomExpression n 0 = do return []
randomExpression n m = do
    le  <- randomIt n
    les <- randomExpression n (m-1)
    return (le:les)


-- Single expression manipulators ----------------------------------------------
textRepresentation :: Le -> String
-- ^Takes one LE and returns a friendly printable expression to get a better
--  representation when printing out the expression.
textRepresentation le = init $ concat [monomio it | it <- le]
    where
        monomio (It (Coeff c, Op o, Exps e)) = show c ++ "*" ++ nome o ++ exps e
        nome o' = nomeator (ops !! o')
        exps e' = "(" ++ concat[show ei ++ " " | ei <- e'] ++ ") "

uniques :: Le -> Le
-- ^Takes one LE and returns this LE without duplicated elements. Matrix 
--  propriety: given a matrix A, if two lines are identical, then the
--  determinant of A is zero and, consequently, A is not inversible. Since the
--  coefficient adjustment is done using OLS, it's important to avoid
--  non-inversible matrices.
uniques [] = []
uniques (x:xs) = uniques $ filter (x/=) xs

solve :: Le -> DataPoint -> [Double]
-- ^Takes one LE and one DataPoint and returns the expression evaluated to the
--  given DataPoint.
solve le (xs, y) = [(eval o e) * c | (It ( Coeff c, Op o, Exps e)) <- le]
    where
        value exps = product[x^e | (e, x) <- zip exps xs]
        eval op exps
            | op >= length ops = error ("Unknown operator")
            | otherwise        = operator (ops !! op) $ value exps

adjust :: Le -> Dataset -> Le
-- ^Takes one LE and one Dataset and ajust the LE coefficients using Ordinary
--  Least Squares, then returns the adjusted LE. NOTE: adjusting an expression
--  twice results in the expression with all coefficients equal to 1.
adjust le ds = [It (Coeff c, o, e) |  (c, It (_, o, e)) <- zip coeffs le]
    where
        x        = fromLists [solve le (i # ds) | i <- [1..nrows $ fst ds]]
        xt       = transpose x
        xty      = xt `multStd` (snd ds)
        coeffs   = case (inverse $ xt `multStd` x) of
            Left msg  -> [1.0,1.0..]
            Right inv -> toList (inv `multStd` xty)


evaluate :: Le -> Dataset -> Score
-- ^Takes one LE and one Dataset and returns the SCORE of the LE for the
--  given dataset.
evaluate le ds = Score (1.0 / (1.0 + mae))
    where
        mae     = m_e [abs $ diff (i # ds) | i <- [1..(nrows $ fst ds)]]
        m_e ds  = sum ds / length
        diff dp = (product $ solve le dp) - (snd dp)
        length  = fromIntegral $ nrows $ fst ds

simplify :: Le -> SimplifyT -> Maybe Le
-- ^Takes one LE and the Simplification Threshold and returns an expression
--  without ITs with coefficient below threshold. If all ITs have low
--  coefficients, then returns the LE without removing any IT.
simplify le sT
    | leng == 0 = Nothing
    | otherwise = Just simplified
    where
        simplified = [It (Coeff c, o, e) | It (Coeff c, o, e) <- le, c >= sT]
        leng       = length simplified

mutateTrans :: Le -> IO (Le)
-- ^Takes one LE and applies the transformation mutation on it. The
--  transformation mutation changes the operator. Returns a mutated LE.
mutateTrans le = do
    chosenIndex <- randomRIO(0, length le -1)
    randOp      <- randomRIO(0, (length ops-1))
    let newIt = [It (c, Op randOp, e) | It (c, _, e) <- [le !! chosenIndex]]
    let le'   = [it | it<-le, it /= (le !! chosenIndex)]

    return (le' ++ newIt)

mutateExps :: Exps -> IO (Exps)
-- ^Takes one Exps vector and iterate through with a small chance of changing some expoents randomly.
mutateExps (Exps [])     = do return (Exps [])
mutateExps (Exps (e:es)) = do
    apply <- randomRIO(0, 100) 
    randomExp <- randomRIO(0,7)

    let e' = if ((apply :: (Int)) < 10) then randomExp else e
    Exps es' <- mutateExps (Exps es)
    return (Exps (e':es'))

mutateInter :: Int -> Le -> IO (Le)
-- ^Takes the number of variables n and one LE and applies the interaction
--  mutation on it. The interaction mutation changes one expoent of one random
--  IT. returns a mutated LE.
mutateInter n le = do
    chosenLeIndex <- randomRIO(0, length le -1)

    let It (c,o,e) = le !! chosenLeIndex
    mutatedExps <- mutateExps e

    let newIt = It (c, o, mutatedExps)
    let le'   = [it | it<-le, it /= (le !! chosenLeIndex)]

    return (newIt:le')


-- Population of expressions manipulators --------------------------------------
simplifyPop :: Pop -> SimplifyT -> Pop
-- ^Takes one Population and the simplification Threshold and calls the
--  simplify for every solution.
simplifyPop pop sS = re'pop [] pop
    where
        re'pop p_p [       ] = p_p
        re'pop p_p (le:les') = case simplify le sS of
            Nothing  -> re'pop p_p les'
            Just le' -> p_p' `seq` re'pop p_p' les'
                where p_p' = le':p_p

sortByScore :: Pop -> Dataset -> Pop
-- ^Takes one population and an dataset and returns the population sorted by
--  their score. It's important that the given population is adjusted.
sortByScore pop ds = [le' | (sort, le') <- sorted] 
    where
        sorted = sort[(evaluate le ds, le) | le <- pop]

mutatePop :: Pop -> Int -> IO (Pop)
-- ^Takes one Population and the number of variables and calls the mutation
--  method in all expressions. returns a mutated population.
mutatePop [     ] n = do return []
mutatePop (p:pop) n = do
    --mutatedI <- mutateInter n p
    mutatedIT <- mutateTrans p
    remaining <- mutatePop pop n 
    return (mutatedIT:remaining)

rndPopulation ::  PopSize -> LeSize -> Dataset -> IO (Pop)
-- ^Takes the size of the population, the size of the expressions and a dataset
--  used to train the population. Then returns a random population.
rndPopulation 0 _ _  = do return []
rndPopulation p l ds = do
    le  <- randomExpression (ncols $ fst ds) l
    le' <- rndPopulation (p-1) l ds
    return (le:le')

dummyDist :: It -> Int
-- ^Takes one It and calculates the distance between the IT and a dummy it
--  (internal use only).
dummyDist (It(Coeff c,_,Exps e))
        | c==0      = 1 + sum[abs ex | ex <-e]
        | otherwise =     sum[abs ex | ex <-e] 

distExpr :: Le -> Le -> Int
-- ^Takes two LEs and return an integer representing the index of similarity.
distExpr [      ] [        ] = 0
distExpr [      ] ( e : le ) = (dummyDist e) + (distExpr [] le)
distExpr ( e:le ) [        ] = (dummyDist e) + (distExpr [] le)
distExpr (it:its) (it':its') = (itDist it it') + (distExpr its its')
    where
        expDist exp exp' = sum[abs(e-e') | (e,e') <- zip exp exp']
        itDist (It (c, o ,Exps e)) (It (c', o' ,Exps e'))
            | c==c'     = expDist e e'
            | otherwise = 1 + (expDist e e')

supress :: Pop -> SupressionT -> Dataset -> Pop
-- ^Takes an population, a supression threshold and a dataset and searchs for
--  nearly identical solutions. Then, keeps the best of them and discarts the
--  remaining.
supress [    ] _ _ = [ ]
supress (p:[]) _ _ = [p]
supress (p:ps) supT ds = (head neighbors):(supress (ps') supT ds)
    where
        neighbors = sortByScore (p:[l | l<-ps, (distExpr p l) < supT]) ds
        ps' = [l | l<-ps, (distExpr p l) > supT]
