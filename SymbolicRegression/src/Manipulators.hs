module Manipulators where

import Matrix

import Data.Matrix (fromLists,
                    fromList,
                    toList,
                    nrows,
                    transpose,
                    multStd,
                    inverse)
import Data.List   (sort)


-- EXPRESSION OPERATIONS -------------------------------------------------------
--defining the auxiliary types for representing the IT
newtype Score = Score Double
        deriving (Eq, Ord)
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
operator (o,_) = o

nomeator :: Op'n'Name -> String
nomeator (_,s) = s

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
textRepresentation le = init $ concat [monomio it | it <- le]
    where
        monomio (c, o, e) = show c ++ "*" ++ nome o ++ exps e
        nome o' = nomeator (ops !! o')
        exps e' = "(" ++ concat[show ei ++ " " | ei <- e'] ++ ") "

--returns an LE without duplicated elements (matrix propriety: if two lines
--are exactly the same, then the DET A = 0, so A is not inversible). we need
--to invert the matrix to adjust coeffs! so, it's important to remove duplicated
--ITs from an LE
uniques :: Le -> Le
uniques [] = []
uniques (x:xs) = uniques $ filter (x/=) xs

--returns an array with evaluated values for the given datapoint
solve :: Le -> DataPoint -> [Double]
solve le (xs, y) = [(eval o e) * c | (c, o, e) <- le]
    where
        value exps = product[x^e | (e, x) <- zip exps xs]
        eval op exps
            | op >= length ops = error ("Unknown operator")
            | otherwise        = operator (ops !! op) $ value exps
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
evaluate le ds = Score (1.0 / (1.0 + mae))
    where
        mae     = sum[abs $ diff (i # ds) | i <- [1..(nrows $ fst ds)]] / length
        diff dp = (product $ solve le dp) - (snd dp)
        length  = fromIntegral $ nrows $ fst ds

--returns a simplified expression
simplify :: Le -> SimplifyT -> Maybe Le
simplify le sT
    | leng == 0 = Nothing --garantee to return a non empty le
    | otherwise = Just simplified
    where
        simplified = [(c, o, e) | (c,o,e) <- le, c>= sT]
        leng = length simplified


simplifyPop :: Pop -> SimplifyT -> Pop
simplifyPop pop sS = re'pop [] pop
    where
        re'pop p_p []        = p_p
        re'pop p_p (le:les') = case simplify le sS of
            Nothing  -> re'pop p_p les'
            Just le' -> p_p' `seq` re'pop p_p' les'
                where p_p' = le':p_p

--suposes that all the Le in the pop is adjusted
sortByScore :: Pop -> Dataset -> Pop
sortByScore pop ds = [le' | (sort, le') <- sorted] 
    where
        sorted = sort[(evaluate le ds, le) | le <- pop]


--returns a mutated LE
mutate :: Le -> Le
mutate le = le