module Main where

{-
Haskell implementation of AInet based symbolic regression algorithm, using
the IT datastructure.
-}


import Data.Matrix


-- DATASET OPERATIONS ----------------------------------------------------------
--defining the auxiliary types for representing the dataset
type X         = Matrix Double
type Y         = Matrix Double
type Dataset   = (X, Y)
type Datapoint = ([Double], Double)

--test dataset, function id for 2 variables
sample = [[1.0,1.0,1.0],
            [2.0,2.0,2.0],
            [3.0,3.0,3.0],
            [4.0,4.0,4.0],
            [5.0,5.0,5.0],
            [6.0,6.0,6.0]]

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

-- TODO: trainTestSplit method that returns the dataset divided in (Train, Test)


-- EXPRESSION OPERATIONS -------------------------------------------------------
--defining the auxiliary types for representing the IT
type Score = Double
type Exps  = [Int]
type Op    = Int
type It    = (Op, Exps) --Int: index of the operator, [Int]: collection of expoents
type Le    = [It]

--operators used for the ITs
ops = ["id", "sin", "cos", "tan", "abs", "sqrt", "exp", "log"] :: [String]

--returns an LE without duplicated elements (matrix propriety: if two lines
--are exactly the same, then the DET A = 0, so A is not inversible). we need
--to invert the matrix to adjust coeffs! so, it's important to remove duplicated
--ITs from an LE
--TODO: use a filter here
uniques :: Le -> Le
uniques [x] = [x]
uniques (x:xs)
    | x `elem` xs = uniques xs
    | otherwise   = x : uniques xs

--returns an array with evaluated values for the given datapoint
solve :: Le -> Datapoint -> [Double]
solve le (xs, y) = [eval it | it <- le]
    where
        value exps = product[x^e | (e, x) <- zip exps xs]
        eval (op, exps)
            | op == 0   = id   $       value exps
            | op == 1   = sin  $       value exps
            | op == 2   = cos  $       value exps
            | op == 3   = tan  $       value exps
            | op == 4   = abs  $       value exps
            | op == 5   = sqrt $ abs $ value exps
            | op == 6   = exp  $       value exps
            | op == 7   = log  $       value exps
            | otherwise = error ("Unknown operator")

--returns an array with evaluated values multiplied by an array of coefficients
calculate :: Le -> Datapoint -> [Double] -> Double
calculate le (xs, y) coeffs = sum[c*v | (c, v) <- zip coeffs $ solve le (xs, y)]

--returns an array containing the optimum coeffs using OLS or an array of 1s if error
-- TODO: use gradient descent if det is zero
adjust :: Le -> Dataset -> [Double]
adjust le ds = toList coeffs
    where
        x      = fromLists [solve le (i # ds) | i <- [1..nrows $ fst ds]]
        xt     = transpose x
        xtxi   = inverse $ multStd xt x
        xty    = multStd xt $ snd ds
        coeffs = either (gradient 5000) (`multStd` xty) (xtxi)
        gradient n a = x

{-

--calculates the MAE of a LE for a given Dataset
--evaluate :: Le -> Dataset -> MAE
--evaluate le ds = sum[ abs (solve le coeffs x - y) | (x, y) <- ds ] / length ds
--    where
--        coeffs = adjust le ds

c = [1, 0] :: [Double]
--solve le c (head ds)
-}
aux = listsToDataset sample
le = [ (0, [1]), (1, [1])  ] :: Le
--adjust (uniques le) aux

main :: IO ()
main = do
    putStrLn "hello world"