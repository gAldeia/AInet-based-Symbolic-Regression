module Dataset where

{-
Module containing implementation of dataset functions. The dataset is the holder
for the data used to train the AInet algorithm.
-}

import Data.Matrix
import Data.List     (sort)


--defining the auxiliary types for representing the dataset
type X         = Matrix Double
type Y         = Matrix Double
type Dataset   = (X, Y)
type DataPoint = ([Double], Double)


listsToDataset :: [[Double]] -> Dataset
--returns a dataset
listsToDataset lss = (fromLists xs, fromLists ys)
    where
        xs = [init ls   | ls <- lss] :: [[Double]]
        ys = [[last ls] | ls <- lss] :: [[Double]]

        
(#) :: Int -> Dataset -> DataPoint
--returns a specific element from the dataset, in the form of ([Double], Double)
(#) i (xss,ys) = (xs', y')
    where
        xs' = [xss ! (i, j) | j <- [1..ncols xss]]
        y' = ys ! (i, 1)