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


-- AInet algorithm -------------------------------------------------------------
type NumGen      = Int
-- ^Number of generations to perform the regression

type NumClones   = Int
-- ^Highest number of clones to create on the AInet algorithm

ainet' :: NumGen -> Pop -> LeSize -> NumClones -> SupressionT -> Dataset -> IO (Pop)
-- ^Recursive call of the AInet algorithm, for internal use only.
ainet' 0 pop _ _ _    _  = do return pop
ainet' g pop l c supT ds = do
    let clones = concat population
    mutatedClones <- mutatePop clones (ncols $ fst ds)

    let adjustedMutatedCLones = map (`adjust` ds) [uniques (adjust c ds) | c <-mutatedClones]
    newSolutions <- rndPopulation (length pop) l ds

    let newPop = pop ++ adjustedMutatedCLones ++ [adjust nrs ds | nrs <-newSolutions]

    pop' <- ainet' (g-1) (sortByScore (supress newPop supT ds) ds) l c supT ds

    return pop'
    where 
        population = [ afinityCloning lIc | lIc <- afinityZipping ]
        afinityCloning (leIndex, c') = [pop !! leIndex | i<- [0..c']]
        afinityZipping = zip [(length pop - 1)..0] [0..c]


ainet :: NumGen -> PopSize -> LeSize -> NumClones -> SupressionT -> SimplifyT -> Dataset -> IO (Le)
-- ^Performs an AInet based symbolic regression for the given number of generations.
ainet g p l c supT simS ds = do
    pop <- rndPopulation p l ds
    let adjustedPop = map (`adjust` ds) [uniques p | p <-pop]

    iterated <- ainet' g (sortByScore adjustedPop ds) l c supT ds

    return (last $ sortByScore (simplifyPop iterated simS) ds)
