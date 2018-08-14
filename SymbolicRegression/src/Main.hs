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


verticalPressure :: [[Double]]
-- ^Auxiliary list of lists to create a dataset and run the AInet algorithm.
--  Dataset of the function ΔP = ρ·g(m/s)·Δh
verticalPressure = [
    [0.95, 3.75, 34.9374375],
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

work :: [[Double]]
-- ^Auxiliary list of lists to create a dataset and run the AInet algorithm.
--  Dataset of the function ΔP = ρ·g(m/s)·Δh
work = [
    [85.0, 10.0, 0.95,  807.5    ],
    [70.0, 80.0, 0.7,   3920.0   ],
    [12.5, 50.0, 0.2,   125.0    ],
    [50.0, 95.0, 0.85,  4037.5   ],
    [37.5, 32.5, -1.0,  -1218.75 ],
    [82.5, 70.0, 0.9,   5197.5   ],
    [77.5, 92.5, -0.1,  -716.875 ],
    [40.0, 45.0, -0.7,  -1260.0  ],
    [25.0, 87.5, -0.6,  -1312.5  ],
    [95.0, 37.5, -0.95, -3384.375],
    [45.0, 82.5, -0.8,  -2970.0  ],
    [42.5, 85.0, -0.15, -541.875 ],
    [90.0, 12.5, -0.2,  -225.0   ],
    [67.5, 52.5, 0.75,  2657.8125],
    [80.0, 90.0, 0.05,  360.0    ],
    [87.5, 30.0, -0.35, -918.75  ],
    [22.5, 47.5, -0.9,  -961.875 ],
    [27.5, 40.0, -0.25, -275.0   ],
    [35.0, 77.5, 0.25,  678.125  ],
    [20.0, 55.0, -0.5,  -550.0   ]]


main :: IO ()
-- ^Main method with example of execution of the AInet algorithm. The given
--  values here were used for testing the algorithm, and are our recomendated
--  values.
main = do
    -- seed of the global generator, used to make deterministic.
    let seed = 42 :: Int

    setStdGen $ mkStdGen seed

    let g = 8        :: NumGen      --number of generations
    let p = 15       :: PopSize     --size of the initial population
    let l = 3        :: LeSize      --size of the expressions
    let c = 10       :: NumClones   --number of clones
    let supT = 2     :: SupressionT --supression threshold
    let simT = 0.005 :: SimplifyT   --simplification threshold

    --some sample datasets
    let verticalPressureDs = listsToDataset verticalPressure
    let workDs             = listsToDataset work       

    --performing the symbolic regression and saving the result 
    print ("Search for the vertical pressure dataset:")

    res <- ainet g p l c supT simT verticalPressureDs

    print (textRepresentation res)
    print (evaluate res verticalPressureDs)

    print ("Search for the work dataset:")

    res <- ainet g p l c supT simT workDs
    
    print (textRepresentation res)
    print (evaluate res workDs)
