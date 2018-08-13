{-|
Module      : Main
Description : Main method containing example of usage.
Copyright   : (c) Guilherme S I Aldeia, 2018
                  Heitor R Savegnago, 2018
License     : GPL-3
Maintainer  : guilherme.aldeia@aluno.ufabc.edu.br
Stability   : experimental
Portability : POSIX

Haskell implementation of AInet based symbolic regression algorithm, using
the IT datastructure.
-}

module Main where

import Dataset
import Manipulators
import AInet
import System.Random


sample :: [[Double]]
-- ^Auxiliary list of lists to create a dataset and run the AInet algorithm.
sample = [[1.0,1.0,1.0,1.0,1.0],
          [2.0,2.0,2.0,2.0,2.0],
          [3.0,3.0,3.0,3.0,3.0],
          [4.0,4.0,4.0,4.0,4.0],
          [5.0,5.0,5.0,5.0,5.0],
          [6.0,6.0,6.0,6.0,6.0]]


main :: IO ()
-- ^Main method with example of execution of the AInet algorithm. The given
--  values here were used for testing the algorithm, and are our recomendated
--  values.
main = do
    -- |seed of the global generator, used to make deterministic.
    let seed = 42 :: Int

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