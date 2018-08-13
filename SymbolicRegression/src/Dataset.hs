{-|
Module      : Dataset
Description : Dataset creation and manipulation .
Copyright   : (c) Guilherme S I Aldeia, 2018
                  Heitor R Savegnago, 2018
License     : GPL-3
Maintainer  : guilherme.aldeia@aluno.ufabc.edu.br
              heitor.rodrigues@aluno.ufabc.edu.br
Stability   : experimental
Portability : POSIX

Module containing implementation of dataset functions. The dataset is the holder
for the data used to train the AInet algorithm. Due to the necessity of
performing many matrix operations, the dataset is defined as a tuple of
matrices (Data.Matrix).
-}

module Dataset where

import Data.Matrix
import Data.List     (sort)


-- Type declarations -----------------------------------------------------------
type X = Matrix Double
-- ^Matrix of explanatory variables

type Y = Matrix Double
-- ^Column matrix of target variables

type Dataset = (X, Y)
-- ^Tuple with explanatory variables associated with the target variable

type DataPoint = ([Double], Double) 
-- ^A single line from a Dataset in a function friendly type


-- Dataset creation and manipulation methods -----------------------------------
listsToDataset :: [[Double]] -> Dataset
-- ^Takes a list of lists and split all lists in the last element. The first n-1
--  elements are considered explaratory variables, and the last element of each
--  list is considered the target variable. Returns a dataset.
listsToDataset lss = (fromLists xs, fromLists ys)
    where
        xs = [init ls   | ls <- lss] :: [[Double]]
        ys = [[last ls] | ls <- lss] :: [[Double]]

(#) :: Int -> Dataset -> DataPoint
-- ^Takes a index (must be positive and smaller than the number of rows of the 
--  dataset) and a dataset. Returns the equivalent row from the dataset, in the
--  form of ([Double], Double). This makes the processing of those values easier
--  than having to deal with the Matrix functions.
(#) i (xss,ys) = (xs', y')
    where
        xs' = [xss ! (i, j) | j <- [1..ncols xss]]
        y'  = ys ! (i, 1)