module AInet where 

import Matrix
import Manipulators

import System.Random
import Data.Matrix
import Data.List     (sort, map)


-- AINET ALGORITHM -------------------------------------------------------------
type NumGen      = Int
type NumClones   = Int

ainet :: NumGen -> PopSize -> LeSize -> NumClones -> SupressionT -> SimplifyT -> Dataset -> IO (Le)
--takes a number of generations and performs an AInet based symbolic regression for the given number of generations
ainet g p l c sT sS ds = do
    pop <- rndPopulation p l ds --needs to be '<-' to use the result without IO
    let adjustedPop = [adjust le ds | le <-pop]

    return (last $ sortByScore (simplifyPop adjustedPop sS) ds)

{-
--ainet' suposes that the given populaion is always ordened by worst to best score
--ainet' is a recursive call to simulate the interaction
ainet' :: Pop -> NumGen -> PopSize -> NumClones -> SupressionT -> StdGen -> Dataset -> Pop
ainet' pop 0 p c sT rndGen ds = []
ainet' pop g p c sT rndGen ds = ainet' newPop (g-1) p c sT rndGen ds
    where
        --the number of clones is the index of each solution (the solutions should be ordered from worst to best, this way there will be more best solutions and few bad solutions)
        clones = concat[ [pop !! leIndex | i<- [1..leIndex]] | leIndex <- [1..length pop] ]

        --mutate the clones and adjust them
        mutatedClones = [adjust solution ds | solution <- [mutate solution | solution <- clones]]

        newPop = pop ++ mutatedClones
        --add new random solutions to the pop (need to create and adjust them)
        --sort the newPop
        --drop the first n solutions with score less than supression s from the newPop and send it to the recursive call

ainet :: NumGen -> PopSize -> LeSize -> NumClones -> SupressionT -> SimplifyT -> Dataset -> Le
ainet g p l c sT sS ds = last $ sortByScore ( simplifyPop (ainet' (sortByScore pop ds) g p c sT nextGen ds) sS ) ds
    where
-}