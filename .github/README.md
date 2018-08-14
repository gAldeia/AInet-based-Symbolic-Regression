AInet Symbolic Regression
=====

### English Version:

- [1 AInet Symbolic Regression](#introi)
    - [1.1 The IT (Interaction-Transformation) data structure](#1.1i)
    - [1.2 The AInet algorithm](#1.2i)
        - [1.2.1 The Clonal Selection Principle](#1.2.1i)
        - [1.2.2 The IT-AInet algorithm](#1.2.2i)
- [2 Source files](#2i)
    - [2.1 Our Types, Newtypes and Classes](#2.1i)
        - [2.1.1 Dataset Module](#2.1.1i)
        - [2.1.2 Manipulators Module](#2.1.2i)
        - [2.1.3 Ainet Module](#2.1.3i)
- [3 Instalation](#3i)
- [4 Usage](#4i)
     

### Versão em Português:

- [1 Regressão Simbólica AInet](#introp)
    - [1.1 A estrutura de dados IT (Interação-Transformação)](#1.1p)
    - [1.2 O algoritmo AInet](#1.2p)
        - [1.2.1 Princípio de seleção de clonal](#1.2.1p)
        - [1.2.2 O algoritmo IT-AInet](#1.2.2p)
- [2 Códigos-fonte](#2p)
    - [2.1 Nossos _Types_, _Newtypes_ and _Classes_](#2.1p)
        - [2.1.1 _Dataset Module_](#2.1.1p)
        - [2.1.2 _Manipulators Module_](#2.1.2p)
        - [2.1.3 _Ainet Module_](#2.1.3p)
- [3 Instalação](#3p)
- [4 Uso](#4p)

-----
-----


# <a id="introi">1 AInet Symbolic Regression</a>

Haskell implementation of a symbolic regression algorithm. The regression search is done by means of the IT data structure, and the general algorithm is based on the AInet (artificial immune network) family algorithms.

The regression is done by creating a random population of solutions, where each solution is a linear combination of IT terms. Then, for a given number of generations, the algorithm performs a symbolic regression inspired by the natural immune system of vertebrates.



## <a id="1.1i">1.1 The IT (Interaction-Transformation) data structure</a>

The IT data structure is like the building block of expressions in this symbolic regression algorithm. The expressions are composed of a linear sum of many ITs, where each IT is a function composition that can be applied to a sample. 

The samples are the values of the database used to train the algorithm, or unknown values that you can pass to the model to predict the behaviour of the target variable given specific circunstances.

The IT is a tuple containing one function and one vector of expoents to be applied to the sample.

> <img src="https://latex.codecogs.com/gif.latex?(\mathrm{op},&space;\mathrm{exp})" title="(\mathrm{op}, \mathrm{exp})"/>

To evaluate a IT, first we perform the *g* function, that takes as argument the sample and the expoent's vector, then aplies each expoent to the respective variable of the sample, and finally multiplicate all results obtained.

> <img src="https://latex.codecogs.com/gif.latex?g(X,&space;E)&space;=&space;\prod_{i=1}^{\left&space;\|&space;X&space;\right&space;\|}&space;x_{i}^{e_{i}}" title="g(X, E) = \prod_{i=1}^{\left \| X \right \|} x_{i}^{e_{i}}"/>

After that, the *f* function is applied to the result obtained from *g*, *f* being the first element of the tuple that represents one IT.

Also, there's a linear coefficient multiplied to every IT to adjust and minimize the error.

Given this datastructure, the algorithm creates it's solutions by composing linear expressions of IT expressions, and performs the symbolic search manipulating those expressions.



## <a id="1.2i">1.2 The AInet algorithm</a>

The algorithms of the AInet family are based on the Artificial Immune Systems paradigm.

The natural immune system is responsible for recognition and combat against pathogenic agents. Antigens are any substance that can bind itself to the antibodies (B-cells) - and this generaly leads to a immune response against the pathogen.



### <a id="1.2.1i">1.2.1 The Clonal Selection Principle</a>

The response against the pathogen is known as Clonal Selection Principle: when the antigens for a pathogen agent are identified, the immune system starts to clone the B-cells (immune cells), cells capable of binding to the antigens to indicate to the antibodies the structures that should be eliminated.

During the cloning stage, the clones suffers a controlled mutation, creating variations of the B-cells. Together, it's applied a selective pressure that supresses the clones that, after mutation, obtained a worse performance at identifying those antigens.

This way, the iteration of those stages will produce a strongly specialized population of cells with competence to fight against the pathogens.



### <a id="1.2.2i">1.2.2 The IT-AInet algorithm</a>

The algorithm used to perform the regression is based on the AInet family, and works just like described above: creating clones, applying mutation and iterating through generations.



-----

# <a id="2i">2 Source files</a>

The main project is within the SymbolicRegression folder. There are 4 modules that compose the algorithm:

| File | Description |
|:-----|:------------|
| Dataset.hs|Module containing implementation of dataset functions. The dataset is the holder for the data used to train the AInet algorithm. |
| Manipulators.hs| Module containing implementation of expression manipulation functions. |
| AInet.hs | Symbolic regression algorithm. The regression search is done by means of the IT data structure, and the general structure of the algorithm is based on the AInet algorithm . |
| Main.hs| Haskell implementation of AInet based symbolic regression algorithm, using the IT datastructure.|



## <a id="2.1i">2.1 Our Types, Newtypes and Classes</a>

To increase readability of our code, we created several new datatypes, listed below:



### <a id="2.1.1i">2.1.1 Dataset Module</a>

| Name | Datatype | What is this |
|:-----|:---------|:-------------|
| X| type (Matrix Double)| Matrix of explanatory variables|
| Y| type (Matrix Double)| Column matrix of target variables|
| Dataset| type ((X,Y))| Tuple with explanatory variables associated with the target variable|
| Datapoint| type (([Double], Double))|A single line from a Dataset in a function friendly type|



### <a id="2.1.2i">2.1.2 Manipulators Module</a>

| Name | Datatype | What is this |
|:-----|:---------|:-------------|
| Score| newtype (Double, deriving Eq, Ord, Show)| Double ranging from [0,1] indicating the performance of the solution for a given dataset|
| Coeff| newtype (Double, deriving Eq, Ord, Show)| Coefficient associated with the ITs|
| Exps| newtype ([Int], deriving Eq, Ord, Show)| Vector of expoents of ITs, to be applied to the samples on IT evaluation|
| Op| newtype (Int, deriving Eq, Ord, Show)| Index of the *operator* of the ITs|
| It| newtype ((Coeff, Op, Exps) deriving Eq, Ord, Show) | IT data structure|
| Le| type ([It])  | Linear combination of ITs|
| Pop| type ([Le]) | Vector containing many Les, called population|
| Operator| type ((Double -> Double)) |  One-argument functions, used to compose ITs|
| Op'n'Name| type ((Operator, String)) |  Tuple containing One operator and one string to print it|
| SimplifyT| type (Double) |  Simplification threshold|
| SupressionT| type (Int) |  Supression threshold|
| PopSize| type (Int) |  Size of the population|
| LeSize| type (Int) |  Size of the expressions|



### <a id="2.1.3i">2.1.3 Ainet Module</a>

| Name | Datatype | What is this |
|:-----|:---------|:-------------|
| NumGen| type (Int)| Number of generations to perform the regression|
| NumClones| type (Int)| Highest number of clones to create on the AInet algorithm|



-----

# <a id="3i">3 Instalation<a>

Open the SymbolicRegression folder in a terminal and execute the folowing commands:

```
stack build
```

```
stack exec SymbolicRegression
```


-----

# <a id="4i">4 Usage</a>

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



-----
-----


# <a id="introp"> 1 Regressão simbólica AInet </a>

Implementação em Haskell de um algoritmo de regressão simbólica. A busca da regressão é feita por meio da estrutura de dados IT, e o algoritmo geral é baseado nos algoritmos da família AInet (rede imune artificial).

A regressão é feita criando uma população aleatória de soluções, onde cada solução é uma combinação linear de termos IT. Então, por um determinado número de gerações, o algoritmo realiza uma regressão simbólica inspirada no sistema imunológico natural dos vertebrados.



## <a id="1.1p"> 1.1 A estrutura de dados IT (Interação-Transformação) </a>

A estrutura de dados de IT é como o bloco de construção de expressões neste algoritmo de regressão simbólica. As expressões são compostas de uma soma linear de várias ITs, onde cada IT é uma composição de função que pode ser aplicada a uma dado amostral.

Os dados amostrais são os valores do _dataset_ utilizados para treinar o algoritmo, ou valores desconhecidos que você pode passar para o modelo para prever o comportamento da variável álvo, dadas as circunstâncias específicas.

A IT é uma tupla contendo uma função e um vetor de expoentes a serem aplicados à um dado amostral.

> <img src="https://latex.codecogs.com/gif.latex?(\mathrm{op},&space;\mathrm{exp})" title="(\mathrm{op}, \mathrm{exp})"/>


Para avaliar uma IT, primeiro realizamos a função *g*, que toma como argumento o dado amostral e o vetor da expoente, depois aplica cada expoente à respectiva variável da amostra e, finalmente, multiplica todos os resultados obtidos.

> <img src="https://latex.codecogs.com/gif.latex?g(X,&space;E)&space;=&space;\prod_{i=1}^{\left&space;\|&space;X&space;\right&space;\|}&space;x_{i}^{e_{i}}" title="g(X, E) = \prod_{i=1}^{\left \| X \right \|} x_{i}^{e_{i}}"/>


Depois disso, a função *f* é aplicada ao resultado obtido de *g*, *f* sendo o primeiro elemento da tupla que representa uma IT.

Além disso, há um coeficiente linear multiplicado para cada IT para ajustar e minimizar o erro.

Dada essa estrutura de dados, o algoritmo cria suas soluções ao compor expressões lineares de expressões de IT e realiza a busca simbólica manipulando essas expressões.


## <a id="1.2p"> 1.2 O algoritmo AInet </a>

Os algoritmos da família AInet são baseados no paradigma dos Sistemas Imunológicos Artificiais.

O sistema imunológico natural é responsável pelo reconhecimento e combate de agentes patogênicos. Os antígenos são qualquer substância que possa se ligar aos anticorpos (células B) - e isso geralmente desencadeia uma resposta imune contra o patógeno.



### <a id="1.2.1p"> 1.2.1 O Princípio de Seleção Clonal </a>

A resposta contra o patógeno é conhecida como Princípio de Seleção Clonal: quando os antígenos para um agente patogênico são identificados, o sistema imune começa a clonar as células B (células imunes), células capazes de se ligar aos antígenos para indicar aos anticorpos estruturas que devem ser eliminadas.

Durante a fase de clonagem, os clones sofrem uma mutação controlada, criando variações das células-B. Juntos, é aplicada uma pressão seletiva que suprime os clones que, após a mutação, obtiveram um pior desempenho na identificação desses antígenos.

Dessa forma, a iteração dessas etapas produzirá uma população de células fortemente especializadas, com competência para combater os patógenos.



### <a id="1.2.2p"> 1.2.2 O algoritmo IT-AInet </a>

O algoritmo usado para executar a regressão é baseado na família AInet e funciona como descrito acima: criando clones, aplicando mutação e iterando através de gerações.



-----

# <a id="2p"> 2 Códigos-fonte</a>

O projeto principal está dentro da pasta SymbolicRegression. Existem 4 módulos que compõem o algoritmo:

| Arquivo | Descrição |
|:-----|:------------|
| Dataset.hs | Módulo contendo a implementação das funções do _dataset_. O  _dataset_ é o detentor dos dados usados para treinar o algoritmo AInet. |
| Manipulators.hs | Módulo contendo implementação de funções de manipulação de expressão. |
| AInet.hs | Algoritmo de regressão simbólica. A pesquisa de regressão é feita por meio da estrutura de dados de IT, e a estrutura geral do algoritmo é baseada na família de algoritmos AInet. |
| Main.hs | Implementação de Haskell do algoritmo de regressão simbólica baseado em AInet, usando a estrutura de dados de IT.



## <a id="2.1p"> 2.1 Nossos _Types_, _Newtypes_ and _Classes_</a>

Para aumentar a legibilidade do nosso código, criamos vários novos tipos de dados, listados abaixo:



### <a id="2.1.1p"> 2.1.1 Módulo _Dataset_ </a>

| Nome | Tipo de dados | O que é isso |
|:-----|:---------|:-------------|
| X | type (Matrix Double) | Matriz de variáveis-explicativas |
| Y | type (Matrix Double) | Matriz de colunas das variáveis-alvo |
| Dataset | type ((X, Y)) | Tupla com variáveis explicativas associadas à variável de destino |
| DataPoint | type (([Double], Double)) | Uma única linha de um Dataset em um tipo mais fácil de ser manipulado por funções |



### <a id="2.1.2p"> 2.1.2 Módulo _Manipulators_ </a>

| Nome | Tipo de dados | O que é isso |
|:-----|:---------|:-------------|
| Score | newtype (Double, deriving Eq, Ord, Show) | Double variando de [0,1] indicando o desempenho da solução para um determinado _dataset_ |
| Coeff | newtype (Double, deriving Eq, Ord, Show) | Coeficiente associado às TIs |
| Exps | newtype ([Int], deriving Eq, Ord, Show) | Vetor de expoentes de ITs, a serem aplicados às amostras em avaliação de TI |
| Op | newtype (Int, deriving Eq, Ord, Show) | Índice do * operador * das TI |
| It| newtype ((Coeff, Op, Exps) deriving Eq, Ord, Show) | Estrutura de dados de TI |
| Le| type ([It]) | Combinação linear de ITs |
| Pop| type ([Le]) | Vector contendo muitos Les, chamado população |
| Operator| type ((Double -> Double)) | Funções de um argumento, usadas para compor as TIs |
| Op'n'Name| type ((Operator, String)) | Tupla contendo um operador e uma string para imprimi-lo |
| SimplifyT| type (Double) | Limiar de simplificação |
| SupressionT| type (Int) | Limiar de supressão |
| PopSize| type (Int) | Tamanho da população |
| LeSize| type (Int) | Tamanho das expressões |



### <a id="2.1.3p"> 2.1.3 Módulo Ainet </a>

|Nome | Tipo de dados | O que é isso |
|:-----|:---------|:-------------|
| NumGen | type (Int) | Número de gerações para realizar a regressão |
| NumClones | type (Int) | Maior número de clones para criar no algoritmo AInet |



-----

# <a id="3p"> 3 Instalação <a>

Abra a pasta SymbolicRegression em um terminal e execute os seguintes comandos:

```
stack build
```

```
stack exec SymbolicRegression
```


-----

# <a id="4p"> 4 Uso </a>

Basta definir os parâmetros e obter o resultado da regressão IO (Le). Para uma melhor descrição, confira a [página de documentação (em inglês)](https://galdeia.github.io/AInet-based-Symbolic-Regression/).

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
-----
-----
