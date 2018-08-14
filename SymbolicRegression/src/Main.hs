{-|
Module      : Main
Description : Main method containing example of usage.
Copyright   : (c) Guilherme S I Aldeia, 2018
                  Heitor R Savegnago, 2018
License     : GPL-3
Maintainer  : guilherme.aldeia@aluno.ufabc.edu.br
              heitor.rodrigues@aluno.ufabc.edu.br
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
--  Dataset of the function ΔP = ρ·g(m/s)·Δh
sample = [[0.95, 3.75, 34.9374375],
          [0.8,  7.75, 60.8034   ],
          [0.55, 5.25, 28.3177125],
          [0.25, 4.5,  11.032875 ],
          [0.9,  7.5,  66.19725  ],
          [0.4,  3.25, 12.7491   ],
          [0.15, 2.25, 3.3098625 ],
          [0.75, 2.5,  18.388125 ],
          [0.2,  4.0,  7.8456    ],
          [0.45, 2.75, 12.1361625],
          [0.3,  0.75, 2.206575  ],
          [0.65, 1.5,  9.561825  ],
          [0.05, 0.5,  0.245175  ],
          [0.5,  6.5,  31.87275  ],
          [0.0,  0.25, 0.0       ],
          [0.6,  1.25, 7.35525   ],
          [0.85, 4.25, 35.4277875],
          [0.1,  7.25, 7.110075  ],
          [0.7,  5.75, 39.473175 ],
          [0.35, 5.5,  18.878475 ]]


main :: IO ()
-- ^Main method with example of execution of the AInet algorithm. The given
--  values here were used for testing the algorithm, and are our recomendated
--  values.
main = do
    -- seed of the global generator, used to make deterministic.
    let seed = 42 :: Int

    setStdGen $ mkStdGen seed

    let g = 5        :: NumGen      --number of generations
    let p = 10       :: PopSize     --size of the initial population
    let l = 5        :: LeSize      --size of the expressions
    let c = 10       :: NumClones   --number of clones
    let supT = 2     :: SupressionT --supression threshold
    let simT = 0.005 :: SimplifyT   --simplification threshold

    let ds = listsToDataset sample  --sample dataset

    --performing the symbolic regression and saving the result 
    res <- ainet g p l c supT simT ds
    
    --printing the result
    print (textRepresentation res)
    print (evaluate res ds)
