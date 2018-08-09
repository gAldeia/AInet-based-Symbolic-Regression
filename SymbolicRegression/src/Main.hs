module Main where
    
import Matrix
import Manipulators
import AInet
import System.Random
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

-- MAIN METHOD -----------------------------------------------------------------
main :: IO ()
main = do
    let seed = 42 :: Int --random seed
    setStdGen $ mkStdGen seed --making the behaviour deterministig for reproducibility

    putStrLn "hello world"
