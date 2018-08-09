module Matrix where

import Data.Matrix
import Data.List     (sort)


-- DATASET OPERATIONS ----------------------------------------------------------
--defining the auxiliary types for representing the dataset
type X         = Matrix Double
type Y         = Matrix Double
type Dataset   = (X, Y)
type DataPoint = ([Double], Double)

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
(#) :: Int -> Dataset -> DataPoint
(#) i (xss,ys) = (xs', y')
    where
        xs' = [xss ! (i, j) | j <- [1..ncols xss]]
        y' = ys ! (i, 1)

