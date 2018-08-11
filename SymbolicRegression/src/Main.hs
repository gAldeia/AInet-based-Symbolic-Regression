module Main where

{-
Haskell implementation of AInet based symbolic regression algorithm, using
the IT datastructure.

TODO LIST
--trainTestSplit method that returns the dataset divided in (Train, Test)
--use gradient descent if det is zero in adjust method
--create better comments (check if haskell have some standard of comments, like python have the "numPy way")
--implement random LE generator
--implement mutation
--make mutation random
--make ainet recursive fuction caudal
--rename matrix to dataset
-}

import Matrix
import Manipulators
import AInet
import System.Random


--sample matrix to create a dataset, function with 4 explanatory variables
sample :: [[Double]]
sample = [[1.0,1.0,1.0,1.0,1.0],
          [2.0,2.0,2.0,2.0,2.0],
          [3.0,3.0,3.0,3.0,3.0],
          [4.0,4.0,4.0,4.0,4.0],
          [5.0,5.0,5.0,5.0,5.0],
          [6.0,6.0,6.0,6.0,6.0]]

-- MAIN METHOD -----------------------------------------------------------------
main :: IO ()
main = do
    let seed = 42 :: Int -- seed of the global generator, used to make deterministic
    setStdGen $ mkStdGen seed --defining the global random generator

    let g = 10 :: NumGen --number of generations
    let p = 100 :: PopSize -- size of the initial population
    let l = 2 :: LeSize -- size of the expressions
    let c = 5 :: NumClones -- number of clones
    let supT = 10 :: SupressionT --supression threshold
    let simT = 0.005 :: SimplifyT --simplification threshold
    let ds = listsToDataset sample --sample dataset

    --performing the symbolic regression
    res <- ainet g p l c supT simT ds
    
    --printing the results
    print (textRepresentation res)
    print (evaluate res ds)