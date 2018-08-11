module AInet where 

{-

-}

import Dataset
import Manipulators
import System.Random
import Data.Matrix
import Data.List     (sort, map)


-- AINET ALGORITHM -------------------------------------------------------------
type NumGen      = Int
type NumClones   = Int

ainet' :: NumGen -> Pop -> LeSize -> NumClones -> SupressionT -> Dataset -> IO (Pop)
--performs one interation of the ainet algorithm. suposes that the given populaion is always ordened by worst to best score
ainet' 0 _ _ _ _ _ = do return []
ainet' g pop l c supT ds = do
    --the number of clones is the index of each solution (the solutions should be ordered from worst to best, this way there will be more best solutions and few bad solutions)
    let clones = concat[  [pop !! leIndex | i<- [1..leIndex]] | leIndex <- [1..length pop]  ]
    mutatedClones <- mutatePop clones (ncols $ fst ds)
    let adjustedMutatedCLones = map (`adjust` ds) mutatedClones
    newRandomSolutions <- rndPopulation (length pop) l ds
    let newPop = pop ++ adjustedMutatedCLones ++ newRandomSolutions

    pop' <- ainet' (g-1) (supressionPop (sortByScore newPop ds) supT) l c supT ds

    return pop'

ainet :: NumGen -> PopSize -> LeSize -> NumClones -> SupressionT -> SimplifyT -> Dataset -> IO (Le)
--takes a number of generations and performs an AInet based symbolic regression for the given number of generations
ainet g p l c supT simS ds = do
    pop <- rndPopulation p l ds --needs to be '<-' to use the result without IO
    let adjustedPop = map (`adjust` ds) pop

    return (last $ sortByScore (simplifyPop adjustedPop simS) ds)

{-
ainet' :: Pop -> NumGen -> PopSize -> LeSize -> NumClones -> SupressionT -> Dataset -> Pop
ainet' pop g p c sT rndGen ds = ainet' newPop (g-1) p c sT rndGen ds

ainet g p l c sT sS ds = last $ sortByScore ( simplifyPop (ainet' (sortByScore pop ds) g p c sT nextGen ds) sS ) ds
    where
-}