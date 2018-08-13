{-|
Module      : AInet
Description : Dataset creation and manipulation 
Copyright   : (c) Guilherme S I Aldeia, 2018
                  Heitor R Savegnago, 2018
License     : GPL-3
Maintainer  : guilherme.aldeia@aluno.ufabc.edu.br
              heitor.rodrigues@aluno.ufabc.edu.br
Stability   : experimental
Portability : POSIX

Symbolic regression algorithm. The regression search is done by means of the IT
data structure, and the general structure of the algorithm is based on the AInet
algorithm (artificial imunne network).
-}

module AInet where 

import Dataset
import Manipulators
import System.Random
import Data.Matrix
import Data.List     (sort, map)


-- AINET ALGORITHM -------------------------------------------------------------
type NumGen      = Int
type NumClones   = Int


ainet' :: NumGen -> Pop -> LeSize -> NumClones -> SupressionT -> Dataset -> IO (Pop)
--one interation of the ainet. the pop must be ordered by scores
ainet' 0 pop _ _ _ _ = do return pop
ainet' g pop l c supT ds = do
    --the number of clones is the index of each solution (since the solutions are ordered from worst to best). this way there will be more best solutions and few bad solutions)

    let clones = concat population
    mutatedClones <- mutatePop clones (ncols $ fst ds)

    let adjustedMutatedCLones = map (`adjust` ds) mutatedClones
    newRandomSolutions <- rndPopulation (length pop) l ds

    let newPop = pop ++ adjustedMutatedCLones ++ newRandomSolutions

    pop' <- ainet' (g-1) (sortByScore (supress newPop supT ds) ds) l c supT ds

    return pop'
    where 
        population = [ afinityCloning lIc | lIc <- afinityZipping ]
        afinityCloning (leIndex, c') = [pop !! leIndex | i<- [0..c']]
        afinityZipping = zip [(length pop - 1)..0] [0..c]

ainet :: NumGen -> PopSize -> LeSize -> NumClones -> SupressionT -> SimplifyT -> Dataset -> IO (Le)
--performs an AInet based symbolic regression for the given number of generations
ainet g p l c supT simS ds = do
    pop <- rndPopulation p l ds --needs to be '<-' to use the result without IO
    let adjustedPop = map (`adjust` ds) pop

    iterated <- ainet' g (sortByScore adjustedPop ds) l c supT ds

    return (last $ sortByScore (simplifyPop iterated simS) ds)