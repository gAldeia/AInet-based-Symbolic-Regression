---
title: AInet Symbolic Regression
layout: default
---

AInet Symbolic Regression
=====


Haskell implementation of a symbolic regression algorithm. The regression search is done by means of the IT data structure, and the general structure of the algorithm is based on the AInet algorithm (artificial imunne network).

The regression is done by creating a random population of solutions, where each solution is a linear combination of IT terms. Then, for a given number of generations, the algorithm performs a symbolic regression inspired by the biological immune system.


## 1.1 The IT (Interaction-Transformation) data structure

The IT data structure is like the building block of expressions in this symbolic regression algorithm. The expressions are composed of a linear sum of many ITs, where each IT is a function composition that can be applied to a sample. 

The samples are the values of the database used to train the algorithm, or unknown values that you can pass to the model to predict the behaviour of the target variable given specific circunstances.


## 1.2 The AInet algorithm

The AInet algorithm is based in a theory that tries to explain the biological immune system.


## 1.3 Further reading

- IT datastructure by Fabrício Olivetti de França
- (PDF containing the pseudoalgorithms)
- Genetic programming
- AInet algorithms


-----

# 2 Source files

The main project is within the SymbolicRegression folder. There are 4 modules that compose the algorithm:

| File | Description |
|:-----|:------------|
| Dataset.hs | Description goes here |
| Manipulators.hs | Description goes here |
| AInet.hs | Description goes here |
| Main.hs | Main file, containing an example of how to use the algorithm and our recomended parameters to run it.|


## 2.1 Our Types, Newtypes and Classes

To increase readability of our code, we created several new datatypes, listed below:

| Name | Datatype | What is this |
|:-----|:---------|:-------------|
|Score| Class (deriving show nananan) | This is how we measure the accuracy of the expressions. The value ranges from 0 to 1, being 0 the worst score and 1 the best possible score. Based on the score the solutions are selected as potential answers to the regression.|


-----

# 3 Instalation

Open the SymbolicRegression folder in a terminal and execute the folowing commands:

```
stack build
```

```
stack exec
```

-----

# 4 Usage

Just set the parameters and get the result from the regression IO (Le)

```haskell
let g = 10       :: NumGen      --number of generations
let p = 10       :: PopSize     --size of the initial population
let l = 2        :: LeSize      --size of the expressions
let c = 5        :: NumClones   --number of clones
let supT = 3     :: SupressionT --supression threshold
let simT = 0.005 :: SimplifyT   --simplification threshold

let ds = listsToDataset sample  --sample dataset

--performing the symbolic regression and saving the result 
res <- ainet g p l c supT simT ds

--printing the result
print (textRepresentation res)
print (evaluate res ds)
```

> Tip: note that all of the parameters are presented in the section 2.1 as *Type*, so they are just a rename of *Int*, *Double* and *[lists]*. 