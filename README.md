# Copilot Kind

Highly automated proof techniques are a necessary step for the widespread adoption of formal methods in the software industry. Moreover, it could provide a partial answer to one of its main issue which is scalability.

*copilot-kind* is a Copilot library aimed at checking automatically some safety properties on Copilot programs. It includes :

* A general interface for solvers and a *proof scheme* mechanism aimed at splitting the task of proving a complex property into checking a sequence of small lemmas in a given order.

* A prover implementing basic **k-induction model checking**, useful for proving basic k-inductive properties and for pedagogical purposes.

* A prover producing native inputs for the *Kind2* model checker, developed at University of Iowa. The latter uses both a *k-induction* algorithm extended with *path compression* and *structural abstraction* and the **IC3 algorithm** with counterexample generalization based on *approximate quantifier elimination*.


## A Tutorial

### Installation instructions

*copilot-kind* needs the following dependencies to be installed in order to run :

* The *copilot-core* and *copilot-language* Haskell libraries
* The *Yices2* SMT-solver : `yices-smt2` must be in your `$PATH`
* The *Z3* SMT-solver : `z3` must be in your `$PATH`
* The *Kind2* model checker : `kind2` must be in your `$PATH`

To build it, just clone this repository and use `cabal install`. You will find some examples in the `examples` folder, which can be built with `cabal install` too.

### First steps

*copilot-kind* is aimed at checking **safety properties** on Copilot programs. Intuitively, a safety property is a property which express the idea that *nothing bad can happen*. In particular, any invalid safety property can be disproved by a finite execution trace of the program called a **counterexample**. Safety properties are often opposed to **liveness** properties, which express the idea that *something good will eventually happen*. The latters are out of the scope of *copilot-kind*.

Safety properties are simply expressed with standard boolean streams. In addition to triggers and observers declarations, it is possible to bind a boolean stream to a property name with the `prop` construct in the specification.

For instance, here is a straightforward specification declaring one property :

```haskell
spec :: Spec
spec = do
  prop "gt0" (x > 0)
  where
    x = [1] ++ (1 + x)
```

Let's say we want to check that `gt0` holds. For this, we use the `prove :: Prover -> ProofScheme -> Spec -> IO ()` function exported by `Copilot.Kind`. This function takes three arguments :

* The prover we want to use. For now, two provers are available, exported by the `Copilot.Kind.Light` and `Copilot.Kind.Kind2` module.
* A *proof scheme*, which is a sequence of instructions like *check*, *assume*, *assert*...
* The Copilot specification

Here, we can just write 

```haskell
prove (lightProver def) (check "gt0") spec
```

where `lightProver def` stands for the light prover with default configuration.

### Prover

The `Copilot.Kind.Prover` defines a general interface for provers. Therefore, it is really easy to add a new prover by creating a new object of type `Prover`. The latter is defined like this :

```haskell
data Cex = Cex

type Infos = [String]

data Output = Output Status Infos

data Status
  = Valid
  | Invalid (Maybe Cex)
  | Unknown
  | Error
  
data Feature = GiveCex | HandleAssumptions
  
data Prover = forall r . Prover 
  { proverName     :: String
  , hasFeature     :: Feature -> Bool
  , startProver    :: Core.Spec -> IO r
  , askProver      :: r -> [PropId] -> PropId -> IO Output 
  , closeProver    :: r -> IO () 
  }
```

Each prover mostly has to provide a `askProver` function which takes as an argument
* The prover descriptor
* A list of assumptions
* A conclusion
and checks if the assumptions logically entail the conclusion.


Two provers are provided by default : `Light` and `Kind2`.

#### The light prover

The *light prover* is a really simple prover which uses the Yices SMT solver with the *QF_UFLIA* theory and is limited to prove *k-inductive* properties, that is properties such that there exists some k such that :

* The property holds during the first k steps of the algorithm.
* From the hypothesis the property has held during k consecutive steps, we can prove it is still true one step further.

For instance, in this example

```haskell
spec :: Spec
spec = do
  prop "gt0"  (x > 0)
  prop "neq0" (x /= 0)
  where
    x = [1] ++ (1 + x)
```
the property *gt0* is inductive (1-inductive) but the property *neq0* is not.


The *light prover* is defined in `Copilot.Kind.Light`. This module exports the `lightProver :: Options -> Prover` function which builds a prover from a record of type `Options` :

```haskell
data Options = Options 
  { kTimeout  :: Integer
  , onlyBmc   :: Bool
  , debugMode :: Bool } 
```

Here,

* `kTimeout` is the maximum number of steps of the k-induction algorithm the prover runs before giving up.
* If `onlyBmc` is set to `True`, the prover will only search for counterexamples and won't try to prove the properties discharged to it.
* If `debugMode` is set to `True`, the SMTLib queries produced by the prover are displayed in the standard output.

`Options` is an instance of the `Data.Default` typeclass :

```haskell
instance Default Options where
  def = Options 
    { kTimeout  = 100
    , debugMode = False 
    , onlyBmc   = False }
```

Therefore, `def` stands for the default configuration.


#### The Kind2 prover

The *Kind2* prover uses the model checker with the same name, from Iowa university. It translates the Copilot specification into a modular transition system (the Kind2 native format) and then calls the *kind2* executable.

It is provided by the `Copilot.Kind.Kind2` module, which exports a `kind2Prover :: Options -> Prover` where the `Options` type is defined as 

```haskell
data Options = Options { bmcMax :: Int }
```
and where `bmcMax` corresponds to the `--bmc_max` option of *kind2* and is equivalent to the `kTimeout` option of the light prover. Its default value is 0, which stands for infinity.


#### Combining provers

The `combine :: Prover -> Prover -> Prover` function let you merge two provers A and B into a prover C which launches both A and B and returns the most *precise output*. It would be interesting to implement other merging behaviours in the future. For instance, a *lazy* one such that C launches B only if A has returns *unknown* or *error*.

As an example, the following prover is used in `Driver.hs` :

```haskell
prover =
  lightProver def {onlyBmc = True, kTimeout = 5} 
  `combine` kind2Prover def
```

We will discuss the internals and the experimental results of this prover later.


### Proof schemes

Let's consider again this example :

```haskell
spec :: Spec
spec = do
  prop "gt0"  (x > 0)
  prop "neq0" (x /= 0)
  where
    x = [1] ++ (1 + x)
```

and let's say we want to prove *neq0*. Currently, the two available solvers fail at showing this non-inductive property (we will discuss this limitation later). Therefore, we can prove the more general and inductive lemma *gt0* and deduce our main goal from this. Therefore, we use the proof scheme 

```haskell
assert "gt0" >> check "neq0"
```
instead of `check "neq0"` only. A proof scheme is chain of primitives schemes glued by the `>>` operator. For now, the available primitives are :

* `check "prop"` checks whether or not a given property is true in the current context.
* `assume "prop"` adds an assumption in the current context
* `assert "prop"` is a shortcut for `check "prop" >> assume "prop"
* `assuming :: [PropId] -> ProofScheme -> ProofScheme` is such that `assuming props scheme` assumes the list of properties *props*, executes the proof scheme *scheme* in this context, and forget the assumptions.
* `msg "..."` displays a string in the standard output

We will discuss the limitations of this tool and a way to use it in practice later.


### Some examples

Some examples are in the *examples* folder. The `Driver.hs` contains the `main` function to run any example. Each other example file exports a specification `spec` and a proof scheme `scheme`. You can change the example being run just by changing one *import* directive in `Driver.hs`.

These examples include :

* `Incr.hs` : A straightforward example in the style of the previous one.
* `Grey.hs` : An example where two different implementations of a periodical counter are shown to be equivalent.
* `BoyerMoore.hs` : A certified version of the majority vote algorithm introduced in the Copilot tutorial.
* `SerialBoyerMoore.hs` : A *serial* version of the first step of the *Boyer Moore algorithm*, where a new element is added to the list and the majority candidate is updated at each clock tick.



## Technical details

### An introduction to SMT-based model checking

An introduction to the model-checking techniques used by can be found in the `talk` folder of this repository. It consists in a self sufficient set of slides . The talk itself can be found at 

The following readings are highly recommended : 

+ Scaling up the formal verification of Lustre programs with SMT-based techniques, G. Hagen, C. Tinelli
+ SMT-based Unbounded Model Checking with IC3 and Approximate Quantifier Elimination, C. Sticksel, C. Tinelli


### Limitations of copilot-kind

#### Limitations related to Copilot implementation

#### Limitations related to Copilot implementation






## Future work

### Missing features in the Kind2 prover

These features are not currently provided due to the lack of important features in the Kind2 SMT solver.

* Counterexamples are not displayed with the Kind2 prover because Kind2 doesn't support XML output of counterexamples. If the last feature is provided, it should be easy to implement counterexample displaying in *copilot-kind*. For this, we reccomend to keep some informations about *observers* in TransSys and to add one variable per observer in the Kind2 output file. The value of these variables will be traced.

* Non-linear Copilot operators and external functions are poorly handled because of the lack of support of uninterpreted functions in the Kind2 native format. A good way to handle these would be to use uninterpreted functions. With this solution, properties like 
```haskell
2 * sin x + 1 <= 3
```
with `x` any stream can't be proven but at least the following can be proved
```haskell
let y = x in sin x == sin y
```
Currently, the solver fail with the last example, as the results of unknown functions are translated into fresh unconstrained variables.

### Simple extensions

The following extensions would be really simple to implement given the current architecture of Kind2.

+ If inductive proving of a property fails, giving the user a concrete CTI (Counterexample To The Inductiveness, see the introductive talk)


### Refactoring suggestions

+ Implement a cleaner way to deal with arbitrary streams and arbitrary constants by extending the `Copilot.Core.Expr type`. See the `Copilot.Kind.Lib` module to see how inelegant the current solution is.

+ Use `Cnub` as an intermediary step in the translation from `Core.Spec` to `IL.Spec` and `TransSys.Spec`.


## FAQ

### Technical considerations

#### Why does the light prover not deliver counterexamples ?

#### Why does the code related to transition systems look so complex ?


### Practical considerations

#### What kind of properties can I prove with *copilot-kind* ?






