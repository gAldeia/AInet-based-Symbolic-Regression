module Main where

{-
Haskell implementation of AInet based symbolic regression algorithm, using
the IT datastructure.
-}

import Dataset
import Manipulators
import AInet
import System.Random


--sample list-of-lists to create a dataset, function with 4 variables
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
    --seed of the global generator, used to make deterministic
    let seed = 42 :: Int

    --defining the global random generator
    setStdGen $ mkStdGen seed

    let g = 10       :: NumGen      --number of generations
    let p = 10       :: PopSize     --size of the initial population
    let l = 2        :: LeSize      --size of the expressions
    let c = 5        :: NumClones   --number of clones
    let supT = 3     :: SupressionT --supression threshold
    let simT = 0.005 :: SimplifyT   --simplification threshold

    let ds = listsToDataset sample  --sample dataset

    --performing the symbolic regression and saving the result 
    res <- ainet g p l c supT simT ds
    
    --printing the result
    print (textRepresentation res)
    print (evaluate res ds)


--TODO LIST
--trainTestSplit method that returns the dataset divided in (Train, Test)
--create better comments (check if haskell have some standard of comments, like python have the "numPy way")
--use sequence and sequenceA in the monads population methods