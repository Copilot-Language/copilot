# Copilot Kind

Highly automated proof techniques are a necessary step for the widespread adoption of formal methods in the software industry. Moreover, it could provide a partial answer to one of its main issue which is scalability.

*copilot-kind* is a Copilot library aimed at checking automatically some safety properties on Copilot programs. It includes :

* A general interface for solvers and a *proof scheme* mechanism aimed at splitting the task of proving a complex property into checking a sequence of small lemmas in a given order.

* A prover implementing basic **k-induction model checking** [1], useful for proving basic k-inductive properties and for pedagogical purposes.

* A prover producing native inputs for the *Kind2* model checker, developed at University of Iowa. The latter uses both a *k-induction* algorithm extended with *path compression* and *structural abstraction* [2] and the **IC3 algorithm** with counterexample generalization based on *approximate quantifier elimination* [3].


## A Tutorial

### Installation instructions

*copilot-kind* needs the following dependencies to be installed in order to run :

* The *copilot-core* and *copilot-language* Haskell libraries
* The *Yices2* SMT-solver : `yices-smt2` must be in your `$PATH`
* The *Z3* SMT-solver : `z3` must be in your `$PATH`
* The *Kind2* model checker : `kind2` must be in your `$PATH`

To build it, just clone this repository and use `cabal install`. You will find some examples in the `examples` folder, which can be built with `cabal install`, producing an executable `copilot-kind-example` in your `.cabal/bin` folder.

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
* `assert "prop"` is a shortcut for `check "prop" >> assume "prop"`
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

An introduction to the model-checking techniques used by can be found in the `talk` folder of this repository. It consists in a self sufficient set of slides. You can find in the *References* section some additional readings.

### Architecture of copilot-kind

#### An overview of the proving process

Each solver first translates the Copilot specification into an intermediate representation best suited for model checking. Two representations are available : 

* The **IL** format : a Copilot program is translated into a list of quantifier-free equations over integer sequences, implicitly quantified by a free variable `n`. Each sequence roughly corresponds to a stream. This format is the one used in G. Hagen thesis [4]. The *light solver* works with this format.

* The **TransSys** format : a Copilot program is flattened and translated into a *state transition system* [1]. Moreover, in order to keep some modularity in this representation, the variables of this system are grouped by *nodes*, each node exporting and importing variables. The Kind2 solver uses this format, which can be easily translated into the Kind2 native format.

Note that **Cnub** is a simplified representation of a Copilot program where only the informations useful for SMT-based model checking are kept. For now, it is not used by the two standard solvers but it could be used in the future as an intermediate step in the translation from `Copilot.Spec` to `IL.Spec` or `TransSys.Spec`.

For each of these formats, there is a folder in `src/Copilot/Kind` which contains at least
* `Spec.hs` where the format is defined
* `PrettyPrint.hs` for pretty printing (useful for debugging)
* `Translate.hs` where the translation process from `Core.Spec` is defined.

These three formats share a simplified set of types and operators, defined respectively in `Misc.Type` and `Misc.Operator`. 


##### An example

The following program : 

```haskell
spec = do
  prop "pos" (fib > 0)

  where
    fib :: Stream Word64
    fib = [1, 1] ++ (fib + drop 1 fib)
```

can be translated into this IL specification :

```
SEQUENCES
    s0 : Int

MODEL INIT
    s0[0] = 1
    s0[1] = 1

MODEL REC
    s0[n + 2] = s0[n] + s0[n + 1]

PROPERTIES
    'pos' : s0[n] > 0
```

or this modular transition system : 

```
NODE 's0' DEPENDS ON []
DEFINES
    out : Int =
        1 -> pre out.1
    out.1 : Int =
        1 -> pre out.2
    out.2 : Int =
        (out) + (out.1)

NODE 'prop-pos' DEPENDS ON [s0]
IMPORTS
    (s0 : out) as 's0.out'
    (s0 : out.1) as 's0.out.1'
    (s0 : out.2) as 's0.out.2'
DEFINES
    out : Bool =
        (s0.out) > (0)

NODE 'top' DEPENDS ON [prop-pos, s0]
IMPORTS
    (prop-pos : out) as 'pos'
    (s0 : out) as 's0.out'
    (s0 : out.1) as 's0.out.1'
    (s0 : out.2) as 's0.out.2'

PROPS
'pos' is (top : pos)

```

Note that the names of the streams are lost in the Copilot reification process [7] and so we have no way to keep them.


#### Types

In these three formats, GADTs are used to ensure statically a part of the type-corectness of the specification, in the same spirit it is done in the other Copilot libraries. *copilot-kind* handles only three types which are `Integer`, `Real` and `Bool` and which are handled by the SMTLib standard. *copilot-kind* works with *pure* reals and integers. Thus, it is unsafe in the sense it ignores integer overflow problems and the loss of precision due to floating point arithmetic.

The rules of translation between Copilot types and *copilot-kind* types are defined in `Misc/Cast`.

#### Operators

The operators provided by `Misc.Operator` consists mostly in boolean connectors, linear operators, equality and inequality operators. If other operators are used in the Copilot program, they are handled using non-determinism or uninterpreted functions.

The file `CoreUtils/Operators` contains helper functions to translate Copilot operators into *copilot-kind* operators.


#### The Light prover


#### The Kind2 prover

##### Modular transition systems

##### The translation process




### Limitations of copilot-kind

Now, we will discuss some limitations of the *copilot-kind* tool. These limitations are organized in two categories : the limitations related to the Copilot language itself and its implementation, and the limitations related to the model-checking techniques we are using.

#### Limitations related to Copilot implementation

The reification process used to build the `Core.Spec` object looses many informations about the structure of the original Copilot program. In fact, a stream is kept in the reified program only if it is recursively defined. Otherwise, all its occurences will be inlined. Moreover, let's look at the `intCounter` function defined in the example `Grey.hs` : 

```haskell
intCounter :: Stream Bool -> Stream Word64
intCounter reset = time
  where 
    time = if reset then 0
           else [0] ++ if time == 3 then 0 else time + 1
```

If *n* counters are created with this function, the same code will be inlined *n* times and the structure of the original code will be lost.

There are many problems with this :

* It makes some optimizations of the model-checking based on a static analysis of the program more difficult (for instance structural abstraction - see [2])
* It makes the inputs given to the SMT solvers larger and repetitive

We can't rewrite the Copilot reification process in order to avoid these inconvenients as these informations are lost by GHC itself before it occurs. The only solution I can see would be to use `Template Haskell` to generate automatically some structural annotations, which might not be worth the dirt introduced.

#### Limitations related to the model-checking techniques used

##### Limitations of the IC3 algorithm

The IC3 algorithm was shown to be a very powerful tool for hardware certification. However, the problems encountered when verifying softwares are much more complex. For now, very few non-inductive properties can be proved by Kind2 when basic integer arithmetic is involved.

The critical point of the IC3 algorithm is the counterexample generalization and the lemma tightening part of it. When encountering a counterexample to the inductiveness (CTI) for a property, these techniques are used to find a lemma discarding it which is general enough so all CTIs can be discarded in a finite number of steps. 

In the current version of Kind2, the lemma found are often too weak. Some suggestions to enhance this are presented in [1]. We hope some progress will be made in this area in a near future.

A solution to this problem would be to write kind of an interactive mode where the user is invited to provide some additional lemmas when automatical techniques fail. Another solution would be to use only inductive properties. In this case, copilot-kind is still a useful tool but the verification of a program can be long and require a high level of technicity.


##### Limitations related to the SMT solvers

The use of SMT solvers introduces two kind of limitations : 

1. We are limited by the computing power needed by the SMT solvers
2. SMT solvers can't handle quantifiers efficiently

Let's consider the first point. SMT solving is costly and its performances are sometimes unpredictable. For instance, when running the `SerialBoyerMoore` example with the *light solver*, Yices2 does not terminate. However, the *Z3* SMT solver used by *Kind2* solves the problem instantaneously. Note that this performance gap is not due to the use of the IC3 algorithm because the property to check is inductive. It could be related to the fact the SMT problem produced by the *light solver* use uninterpreted functions for streams instead of simple integer variables (which is the case when the copilot program is translated into a transition system). However, this wouldn't explain why the *light solver* still terminates instantaneously on the `BoyerMoore` example, which seems not simpler by far.




## Future work

### Missing features in the Kind2 prover

These features are not currently provided due to the lack of important features in the Kind2 SMT solver.

#### Counterexamples displaying

Counterexamples are not displayed with the Kind2 prover because Kind2 doesn't support XML output of counterexamples. If the last feature is provided, it should be easy to implement counterexample displaying in *copilot-kind*. For this, we reccomend to keep some informations about *observers* in TransSys and to add one variable per observer in the Kind2 output file. The value of these variables will be traced.

#### Bad handling of non-linear operators and external functions

Non-linear Copilot operators and external functions are poorly handled because of the lack of support of uninterpreted functions in the Kind2 native format. A good way to handle these would be to use uninterpreted functions. With this solution, properties like 
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

+ If inductive proving of a property fails, giving the user a concrete CTI (Counterexample To The Inductiveness, see the introductive talk).

+ Use Template Haskell to declare automatically some observers with the same names used in the original program.

### Refactoring suggestions

+ Implement a cleaner way to deal with arbitrary streams and arbitrary constants by extending the `Copilot.Core.Expr type`. See the `Copilot.Kind.Lib` module to see how inelegant the current solution is. 

+ Use `Cnub` as an intermediary step in the translation from `Core.Spec` to `IL.Spec` and `TransSys.Spec`.

### More advanced enhancements

+ Enhance the proof scheme system such that when proving a property depending on an arbitrary stream, it is possible to assume some specialized versions of this property for given values of the arbitrary stream. In other words, implementing a basic way to deal with universal quantification.

+ It could be useful to extend the Copilot language in a way it is possible to use annotations inside the Copilot code. For instance, we could

  - Declare assumptions and invariants next to the associated code instead of gathering all properties in a single place.
  - Declare a frequent code pattern which should be factorized in the transition problem (see the section about Copilot limitations)


## FAQ

### Why does the light prover not deliver counterexamples ?

The problem is the light prover is using uninterpreted functions to represent streams and Yices2 can't give you values for uninterpreted functions when you ask it for a valid assignment.

### Why does the code related to transition systems look so complex ?

It is true the code of `TransSys` is quite complex. In fact, it would be really straightforward to produce a flattened transition system and then a Kind2 file with just a *top* predicate. In fact, It would be as easy as producing an *IL* specification.

To be honest, I'm not sure producing a modular *Kind2* output is worth the complexity added. It's especially true at the time I write this in the sense that :

* Each predicate introduced is used only one time (which is true because copilot doesn't handle functions or parametrized streams like Lustre does and everything is inlined during the reification process).
* A similar form of structure could be obtained from a flattened Kind2 native input file with some basic static analysis by producing a dependency graph between variables.
* For now, Kind2 ignores these structure informations

However, the current code offers some nice transformation tools (node merging, `Renaming` monad...) which could be useful if you intend to write a tool for simplifying or factorizing transition systems. Moreover, it becomes easier to write local transformations on transition systems as name conflicts can be avoided more easily when introducing more variables, as there is one namespacce per node.



## References

1. *An insight into An insight into SMT-based model checking techniques for formal software verification of synchronous dataflow programs*, talk, Jonathan Laurent  (see the `talk` folder of this repository)
2. *Scaling up the formal verification of Lustre programs with SMT-based techniques*, G. Hagen, C. Tinelli
3. *SMT-based Unbounded Model Checking with IC3 and Approximate Quantifier Elimination*, C. Sticksel, C. Tinelli

4. *Verifying safety properties of Lustre programs : an SMT-based approach*, PhD thesis, G. Hagen 
5. *Understanding IC3*, Aaron R. Bradley
6. *IC3: Where Monolithic and Incremental Meet*, F. Somenzi, A.R. Bradley
7. *Copilot: Monitoring Embedded Systems*, L. Pike, N. Wegmann, S. Niller


