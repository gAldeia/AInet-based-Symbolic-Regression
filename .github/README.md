AInet Symbolic Regression
=====



Haskell implementation of a symbolic regression algorithm. The regression search is done by means of the IT data structure, and the general algorithm is based on the AInet (artificial immune network) family algorithms.

The regression is done by creating a random population of solutions, where each solution is a linear combination of IT terms. Then, for a given number of generations, the algorithm performs a symbolic regression inspired by the natural immune system of vertebrates.



## 1.1 The IT (Interaction-Transformation) data structure

The IT data structure is like the building block of expressions in this symbolic regression algorithm. The expressions are composed of a linear sum of many ITs, where each IT is a function composition that can be applied to a sample. 

The samples are the values of the database used to train the algorithm, or unknown values that you can pass to the model to predict the behaviour of the target variable given specific circunstances.

The IT is a tuple containing one function and one vector of expoents to be applied to the sample.

> <img src="https://latex.codecogs.com/gif.latex?(\mathrm{op},&space;\mathrm{exp})" title="(\mathrm{op}, \mathrm{exp})"/>

To evaluate a IT, first we perform the *g* function, that takes as argument the sample and the expoent's vector, then aplies each expoent to the respective variable of the sample, and finally multiplicate all results obtained.

> <img src="https://latex.codecogs.com/gif.latex?g(X,&space;E)&space;=&space;\prod_{i=1}^{\left&space;\|&space;X&space;\right&space;\|}&space;x_{i}^{e_{i}}" title="g(X, E) = \prod_{i=1}^{\left \| X \right \|} x_{i}^{e_{i}}"/>

After that, the *f* function is applied to the result obtained from *g*, *f* being the first element of the tuple that represents one IT.

Given this datastructure, the algorithm creates it's solutions by composing linear expressions of IT expressions, and performs the symbolic search manipulating those expressions.



## 1.2 The AInet algorithm

The algorithms of the AInet family are based on the Artificial Immune Systems paradigm.

The natural immune system is responsible for recognition and combat against pathogenic agents. Antigens are any substance that can bind itself to the antibodies (B-cells) - and this generaly leads to a immune response against the pathogen.



### 1.2.1 The Clonal Selection Principle

The response against the pathogen is known as Clonal Selection Principle: when the antigens for a pathogen agent are identified, the immune system starts to clone the B-cells (immune cells), cells capable of binding to the antigens to indicate to the antibodies the structures that should be eliminated.

During the cloning stage, the clones suffers a controlled mutation, creating variations of the B-cells. Together, it's applied a selective pressure that supresses the clones that, after mutation, obtained a worse performance at identifying those antigens.

This way, the iteration of those stages will produce a strongly specialized population of cells with competence to fight against the pathogens.



### 1.2.2 The IT-AInet algorithm

The algorithm used to perform the regression is based on the AInet family, and works just like described above: creating clones, applying mutation and iterating through generations.



-----

# 2 Source files

The main project is within the SymbolicRegression folder. There are 4 modules that compose the algorithm:

| File | Description |
|:-----|:------------|
| Dataset.hs | Module containing implementation of dataset functions. The dataset is the holder for the data used to train the AInet algorithm. |
| Manipulators.hs | Module containing implementation of expression manipulation functions. |
| AInet.hs | Symbolic regression algorithm. The regression search is done by means of the IT data structure, and the general structure of the algorithm is based on the AInet algorithm . |
| Main.hs | Haskell implementation of AInet based symbolic regression algorithm, using the IT datastructure.|



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
stack exec SymbolicRegression-exe
```


-----

# 4 Usage

Just set the parameters and get the result from the regression IO (Le). For a better description, check out the official [documentation page](https://galdeia.github.io/AInet-based-Symbolic-Regression/).

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
