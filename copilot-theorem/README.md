[![Build Status](https://travis-ci.com/Copilot-Language/copilot.svg?branch=master)](https://app.travis-ci.com/github/Copilot-Language/copilot)

# Copilot Theorem

Highly automated proof techniques are a necessary step for the widespread
adoption of formal methods in the software industry. Moreover, it could provide
a partial answer to one of its main issue which is scalability.

*copilot-theorem* is a Copilot library aimed at checking automatically some safety
properties on Copilot programs. It includes:

* A general interface for *provers* and a *proof scheme* mechanism aimed at
  splitting the task of proving a complex property into checking a sequence of
  smaller lemmas.

* A prover implementing basic **k-induction** model checking [1], useful for
  proving basic k-inductive properties and for pedagogical purposes.

* A prover producing native inputs for the *Kind2* model checker, developed at
  University of Iowa. The latter uses both the *k-induction algorithm* extended
  with *path compression* and *structural abstraction* [2] and the **IC3
  algorithm** with counterexample generalization based on *approximate
  quantifier elimination* [3].

## A Tutorial

### Installation instructions

*copilot-theorem* needs the following dependencies to be installed:

* The *copilot-core* and *copilot-language* Haskell libraries
* The *Yices2* SMT-solver: `yices-smt2` must be in your `$PATH`
* The *Z3* SMT-solver: `z3` must be in your `$PATH`
* The *Kind2* model checker: `kind2` must be in your `$PATH`

To build it, just clone this repository and use `cabal install`. You will find
some examples in the `examples` folder, which can be built with `cabal install`
too, producing an executable `copilot-theorem-example` in your `.cabal/bin`
folder.

### First steps

*copilot-theorem* is aimed at checking **safety properties** on Copilot programs.
Intuitively, a safety property is a property which express the idea that
*nothing bad can happen*. In particular, any invalid safety property can be
disproved by a finite execution trace of the program called a
**counterexample**. Safety properties are often opposed to **liveness**
properties, which express the idea that *something good will eventually
happen*. The latters are out of the scope of *copilot-theorem*.

Safety properties are simply expressed with standard boolean streams. In
addition to triggers and observers declarations, it is possible to bind a
boolean stream to a property name with the `prop` construct in the
specification.

For instance, here is a straightforward specification declaring one property:

```haskell
spec :: Spec
spec = do
  void $ prop "gt0" (forAll $ x > 0)
  where
    x = [1] ++ (1 + x)
```

Let's say we want to check that `gt0` holds. For this, we use the `prove ::
Prover -> ProofScheme -> Spec -> IO ()` function exported by `Copilot.Theorem`.
This function takes three arguments:

* The prover we want to use. For now, several provers are available, exported by
  the `Copilot.Theorem.Provers.SMT` and `Copilot.Theorem.Kind2` modules.
* A *proof scheme*, which is a sequence of instructions like *check*, *assume*,
  *assert*...
* The Copilot specification

Here, we can just write

```haskell
prove (kInduction def) (check "gt0") spec
```

where `kInduction def` stands for the *K-Induction prover* with default
configuration.

### The Prover interface

The `Copilot.Theorem.Prover` defines a general interface for provers. Therefore,
it is really easy to add a new prover by creating a new object of type
`Prover`. The latter is defined like this:

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

Each prover mostly has to provide a `askProver` function which takes as an
argument
* The prover descriptor
* A list of assumptions
* A conclusion

and checks if the assumptions logically entail the conclusion.

#### The Kind2 prover

The *Kind2* prover uses the model checker with the same name, from Iowa
university. It translates the Copilot specification into a *modular transition
system* (the Kind2 native format) and then calls the `kind2` executable.

It is provided by the `Copilot.Theorem.Kind2` module, which exports a `kind2Prover
:: Options -> Prover` where the `Options` type is defined as

```haskell
data Options = Options { bmcMax :: Int }
```
and where `bmcMax` corresponds to the `--bmc_max` option of *kind2* and is
equivalent to the `maxK` option of the K-Induction prover. Its default value is
0, which stands for infinity.

#### Combining provers

The `combine :: Prover -> Prover -> Prover` function lets you merge two provers
A and B into a prover C which launches both A and B and returns the *most
precise* output. It would be interesting to implement other merging behaviours
in the future. For instance, a *lazy* one such that C launches B only if A has
returns *unknown* or *error*.

As an example:

```haskell
prover =
  kInduction def {kTimeout = 5}
  `combine` kind2Prover def
```

We will discuss the internals and the experimental results of these provers
later.

### Proof schemes

Let's consider again this example:

```haskell
spec :: Spec
spec = do
  void $ prop "gt0"  (forAll $ x > 0)
  void $ prop "neq0" (forAll $ x /= 0)
  where
    x = [1] ++ (1 + x)
```

and let's say we want to prove `"neq0"`. Currently, the two available solvers
fail at showing this non-inductive property (we will discuss this limitation
later). Therefore, we can prove the more general inductive lemma `"gt0"` and
deduce our main goal from this. For this, we use the proof scheme

```haskell
assert "gt0" >> check "neq0"
```
instead of just `check "neq0"`. A proof scheme is chain of primitives schemes
glued by the `>>` operator. For now, the available primitives are:

* `check "prop"` checks whether or not a given property is true in the current
  context.
* `assume "prop"` adds an assumption in the current context.
* `assert "prop"` is a shortcut for `check "prop" >> assume "prop"`.
* `assuming :: [PropId] -> ProofScheme -> ProofScheme` is such that `assuming
  props scheme` assumes the list of properties *props*, executes the proof
  scheme *scheme* in this context, and forgets the assumptions.
* `msg "..."` displays a string in the standard output

We will discuss the limitations of this tool and a way to use it in practice
later.

### Some examples

Some examples are in the *examples* folder, including:

* `Incr.hs` : a straightforward example in the style of the previous one.
* `Grey.hs` : an example where two different implementations of a periodical
  counter are shown to be equivalent.
* `BoyerMoore.hs` : a certified version of the majority vote algorithm
  introduced in the Copilot tutorial.
* `SerialBoyerMoore.hs` : a *serial* version of the first step of the *Boyer
  Moore algorithm*, where a new element is added to the list and the majority
  candidate is updated at each clock tick. See the section *Limitations related
  to the SMT solvers* for an analysis of this example.

## Technical details

### An introduction to SMT-based model checking

An introduction to the model-checking techniques used by *copilot-theorem* can be
found in the `doc` folder of this repository. It consists in a self sufficient
set of slides. You can find some additional readings in the *References*
section.

### Architecture of copilot-theorem

#### An overview of the proving process

Each prover first translates the Copilot specification into an intermediate
representation best suited for model checking. Two representations are
available:

* The **IL** format: a Copilot program is translated into a list of
  quantifier-free equations over integer sequences, implicitly universally
  quantified by a free variable *n*. Each sequence roughly corresponds to a
  stream. This format is the one used in G. Hagen's thesis [4]. Several provers
  work with this format.

* The **TransSys** format: a Copilot program is *flattened* and translated
  into a *state transition system* [1]. Moreover, in order to keep some
  structure in this representation, the variables of this system are grouped by
  *nodes*, each node exporting and importing variables. The *Kind2 prover* uses
  this format, which can be easily translated into the  native format.

For each of these formats, there is a folder in `src/Copilot/Theorem` which
contains at least
* `Spec.hs` where the format is defined
* `PrettyPrint.hs` for pretty printing (useful for debugging)
* `Translate.hs` where the translation process from `Core.Spec` is defined.

These three formats share a simplified set of types and operators, defined
respectively in `Misc.Type` and `Misc.Operator`.

##### An example

The following program:

```haskell
spec = do
  void $ prop "pos" (forAll $ fib > 0)

  where
    fib :: Stream Word64
    fib = [1, 1] ++ (fib + drop 1 fib)
```

can be translated into this IL specification:

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

or this modular transition system:

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

Note that the names of the streams are lost in the Copilot *reification
process* [7] and so we have no way to keep them.

#### Types

In these three formats, GADTs are used to statically ensure a part of the
type-corectness of the specification, in the same spirit it is done in the
other Copilot libraries. *copilot-theorem* handles only three types which are
`Integer`, `Real` and `Bool` and which are handled by the SMTLib standard.
*copilot-theorem* works with *pure* reals and integers. Thus, it is unsafe in the
sense it ignores integer overflow problems and the loss of precision due to
floating point arithmetic.

The rules of translation between Copilot types and *copilot-theorem* types are
defined in `Misc/Cast`.

#### Operators

The operators provided by `Misc.Operator` mostly consists in boolean
connectors, linear operators, equality and inequality operators. If other
operators are used in the Copilot program, they are handled using
non-determinism or uninterpreted functions.

The file `CoreUtils/Operators` contains helper functions to translate Copilot
operators into *copilot-theorem* operators.


#### The Kind2 prover

The *Kind2 prover* first translates the copilot specification into a *modular
transition system*. Then, a chain of transformations is applied to this system
(for instance, in order to remove dependency cycles among nodes). After this,
the system is translated into the *Kind2 native format* and the `kind2`
executable is launched. The following sections will bring more details about
this process.

##### Modular transition systems

Let's look at the definition of a *modular transition systems*, in the
`TransSys.Spec` module:

```haskell
type NodeId = String
type PropId = String

data Spec = Spec
  { specNodes         :: [Node]
  , specTopNodeId     :: NodeId
  , specProps         :: Map PropId ExtVar }

data Node = Node
  { nodeId            :: NodeId
  , nodeDependencies  :: [NodeId]
  , nodeLocalVars     :: Map Var LVarDescr
  , nodeImportedVars  :: Bimap Var ExtVar
  , nodeConstrs       :: [Expr Bool] }

data Var     =  Var {varName :: String}
                deriving (Eq, Show, Ord)

data ExtVar  =  ExtVar {extVarNode :: NodeId, extVarLocalPart :: Var }
                deriving (Eq, Ord)

data VarDescr = forall t . VarDescr
  { varType :: Type t
  , varDef  :: VarDef t }

data VarDef t =
    Pre t Var
  | Expr (Expr t)
  | Constrs [Expr Bool]

data Expr t where
  Const  :: Type t -> t -> Expr t
  Ite    :: Type t -> Expr Bool -> Expr t -> Expr t -> Expr t
  Op1    :: Type t -> Op1 x t -> Expr x -> Expr t
  Op2    :: Type t -> Op2 x y t -> Expr x -> Expr y -> Expr t
  VarE   :: Type t -> Var -> Expr t
```

A transition system (`Spec` type) is mostly made of a list of nodes. A *node*
is just a set of variables living in a local namespace and corresponding to the
`Var` type. The `ExtVar` type is used to identify a variable in the global
namespace by specifying both a node name and a variable. A node contains two
types of variables:

* Some variables imported from other nodes. The structure `nodeImportedVars`
  binds each imported variable to its local name. The set of nodes from which a
  node imports some variables is stored in the `nodeDependencies` field.

* Some locally defined variables contained in the `nodeLocalVars` field. Such a
  variable can be
  - Defined as the previous value of another variable (`Pre` constructor of
    `VarDef`)
  - Defined by an expression involving other variables (`Expr` constructor)
  - Defined implicitly by a set of constraints (`Constrs` constructor)

##### The translation process

First, a copilot specification is translated into a modular transition system.
This process is defined in the `TransSys.Translate` module. Each stream is
associated to a node. The most significant task of this translation process is
to *flatten* the copilot specification so the value of all streams at time *n*
only depends on the values of all the streams at time *n - 1*, which is not the
case in the `Fib` example shown earlier. This is done by a simple program
transformation which turns this:

```haskell
fib = [1, 1] ++ (fib + drop 1 fib)
```
into this:

```haskell
fib0 = [1] ++ fib1
fib1 = [1] ++ (fib1 + fib0)
```

and then into the node

```
NODE 'fib' DEPENDS ON []
DEFINES
    out : Int =
        1 -> pre out.1
    out.1 : Int =
        1 -> pre out.2
    out.2 : Int =
        (out) + (out.1)
```

Once again, this flattening process is made easier by the fact that the `++`
operator only occurs leftmost in a stream definition after the reification
process.

##### Some transformations over modular transition systems

The transition system obtained by the `TransSys.Translate` module is perfectly
consistent. However, it can't be directly translated into the *Kind2 native
file format*. Indeed, it is natural to bind each node to a predicate but the
Kind2 file format requires that each predicate only uses previously defined
predicates. However, some nodes in our transition system could be mutually
recursive. Therefore, the goal of the `removeCycles :: Spec -> Spec` function
defined in `TransSys.Transform` is to remove such dependency cycles.

This function relies on the `mergeNodes :: [NodeId] -> Spec -> Spec` function
which signature is self-explicit. The latter solves name conflicts by using the
`Misc.Renaming` monad. Some code complexity has been added so the variable
names remains as clear as possible after merging two nodes.

The function `removeCycles` computes the strongly connected components of the
dependency graph and merge each one into a single node. The complexity of this
process is high in the worst case (the square of the total size of the system
times the size of the biggest node) but good in practice as few nodes are to be
merged in most practical cases.

After the cycles have been removed, it is useful to apply another
transformation which makes the translation from `TransSys.Spec` to `Kind2.AST`
easier. This transformation is implemented in the `complete` function. In a
nutshell, it transforms a system such that

* If a node depends on another, it imports *all* its variables.
* The dependency graph is transitive, that is if *A* depends of *B* which
  depends of *C* then *A* depends on *C*.

After this transformation, the translation from `TransSys.Spec` to `Kind2.AST`
is almost only a matter of syntax.

###### Bonus track

Thanks to the `mergeNodes` function, we can get for free the function

```haskell
inline :: Spec -> Spec
inline spec = mergeNodes [nodeId n | n <- specNodes spec] spec
```

which discards all the structure of a *modular transition system* and turns it
into a *non-modular transition system* with only one node. In fact, when
translating a copilot specification to a kind2 file, two styles are available:
the `Kind2.toKind2` function takes a `Style` argument which can take the value
`Inlined` or `Modular`. The only difference is that in the first case, a call
to `removeCycles` is replaced by a call to `inline`.

### Limitations of copilot-theorem

Now, we will discuss some limitations of the *copilot-theorem* tool. These
limitations are organized in two categories: the limitations related to the
Copilot language itself and its implementation, and the limitations related to
the model-checking techniques we are using.

#### Limitations related to Copilot implementation

The reification process used to build the `Core.Spec` object looses many
informations about the structure of the original Copilot program. In fact, a
stream is kept in the reified program only if it is recursively defined.
Otherwise, all its occurences will be inlined. Moreover, let's look at the
`intCounter` function defined in the example `Grey.hs`:

```haskell
intCounter :: Stream Bool -> Stream Word64
intCounter reset = time
  where
    time = if reset then 0
           else [0] ++ if time == 3 then 0 else time + 1
```

If *n* counters are created with this function, the same code will be inlined
*n* times and the structure of the original code will be lost.

There are many problems with this:

* It makes some optimizations of the model-checking based on a static analysis
  of the program more difficult (for instance *structural abstraction* - see
  [2]).
* It makes the inputs given to the SMT solvers larger and repetitive.

We can't rewrite the Copilot reification process in order to avoid these
inconvenients as these informations are lost by GHC itself before it occurs.
The only solution we can see would be to use *Template Haskell* to generate
automatically some structural annotations, which might not be worth the dirt
introduced.

#### Limitations related to the model-checking techniques used

##### Limitations of the IC3 algorithm

The IC3 algorithm was shown to be a very powerful tool for hardware
certification. However, the problems encountered when verifying softwares are
much more complex. For now, very few non-inductive properties can be proved by
*Kind2* when basic integer arithmetic is involved.

The critical point of the IC3 algorithm is the counterexample generalization
and the lemma tightening parts of it. When encountering a *counterexample to
the inductiveness* (CTI) for a property, these techniques are used to find a
lemma discarding it which is general enough so that all CTIs can be discarded
in a finite number of steps.

The lemmas found by the current version fo *Kind2* are often too weak. Some
suggestions to enhance this are presented in [1]. We hope some progress will be
made in this area in a near future.

A workaround to this problem would be to write kind of an interactive mode
where the user is invited to provide some additional lemmas when automatic
techniques fail. Another solution would be to make the properties being checked
quasi-inductive by hand. In this case, *copilot-theorem* is still a useful tool
(especially for finding bugs) but the verification of a program can be long and
requires a high level of technicity.

##### Limitations related to the SMT solvers

The use of SMT solvers introduces two kind of limitations:

1. We are limited by the computing power needed by the SMT solvers
2. SMT solvers can't handle quantifiers efficiently

Let's consider the first point. SMT solving is costly and its performances are
sometimes unpredictable. For instance, when running the `SerialBoyerMoore`
example with the *k-induction prover*, Yices2 does not terminate. However, the *z3*
SMT solver used by *Kind2* solves the problem instantaneously. Note that this
performance gap is not due to the use of the IC3 algorithm because the property
to check is inductive. It could be related to the fact the SMT problem produced
by the *k-induction prover* uses uninterpreted functions for encoding streams instead
of simple integer variables, which is the case when the copilot program is
translated into a transition system. However, this wouldn't explain why the
*k-induction prover* still terminates instantaneously on the `BoyerMoore` example,
which seems not simpler by far.

The second point keeps you from expressing or proving some properties
universally quantified over a stream or a constant. Sometimes, this is still
possible. For instance, in the `Grey` example, as we check a property like
`intCounter reset == greyCounter reset` with `reset` an external stream
(therefore totally unconstrained), we kind of show a universally quantified
property. This fact could be used to enhance the proof scheme system (see the
*Future work* section). However, this trick is not always possible. For
instance, in the `SerialBoyerMoore` example, the property being checked should
be quantified over all integer constants. Here, we can't just introduce an
arbitrary constant stream because it is the quantified property which is
inductive and not the property specialized for a given constant stream. That's
why we have no other solution than replacing universal quantification by
*bounded* universal quantification by assuming all the elements of the input
stream are in the finite list `allowed` and using the function `forAllCst`
defined in `Copilot.Kind.Lib`:

```haskell
conj :: [Stream Bool] -> Stream Bool
conj = foldl (&&) true

forAllCst ::(Typed a) => [a] -> (Stream a -> Stream Bool) -> Stream Bool
forAllCst l f = conj $ map (f . constant) l
```

However, this solution isn't completely satisfying because the size of the
property generated is proportionnal to the cardinal of `allowed`.

#### Some scalability issues

A standard way to prove large programs is to rely on its logical structure by
writing a specification for each of its functions. This very natural approach
is hard to follow in our case because of

* The difficulty to deal with universal quantification.
* The lack of *true* functions in Copilot: the latter offers metaprogramming
  facilities but no concept of functions like *Lustre* does with its *nodes*).
* The inlining policy of the reification process. This point is related to the
  previous one.

Once again, *copilot-theorem* is still a very useful tool, especially for
debugging purposes. However, we don't think it is adapted to write and check a
complete specification for large scale programs.

## Future work

### Missing features in the Kind2 prover

These features are not currently provided due to the lack of important features
in the Kind2 SMT solver.

#### Counterexamples displaying

Counterexamples are not displayed with the Kind2 prover because Kind2 doesn't
support XML output of counterexamples. If the last feature is provided, it
should be easy to implement counterexamples displaying in *copilot-theorem*. For
this, we recommend to keep some informations about *observers* in
`TransSys.Spec` and to add one variable per observer in the Kind2 output file.

#### Bad handling of non-linear operators and external functions

Non-linear Copilot operators and external functions are poorly handled because
of the lack of support of uninterpreted functions in the Kind2 native format. A
good way to handle these would be to use uninterpreted functions. With this
solution, properties like
```haskell
2 * sin x + 1 <= 3
```
with `x` any stream can't be proven but at least the following can be proved
```haskell
let y = x in sin x == sin y
```
Currently, the *Kind2 prover* fail with the last example, as the results of
unknown functions are turned into fresh unconstrained variables.

### Simple extensions

The following extensions would be really simple to implement given the current
architecture of Kind2.

+ If inductive proving of a property fails, giving the user a concrete CTI
  (*Counterexample To The Inductiveness*, see the [1]).

+ Use Template Haskell to declare automatically some observers with the same
  names used in the original program.

### Refactoring suggestions

+ Implement a cleaner way to deal with arbitrary streams and arbitrary
  constants by extending the `Copilot.Core.Expr type`. See the
  `Copilot.Kind.Lib` module to observe how inelegant the current solution is.

+ Use `Cnub` as an intermediary step in the translation from `Core.Spec` to
  `IL.Spec` and `TransSys.Spec`.

### More advanced enhancements

+ Enhance the proof scheme system such that when proving a property depending
  on an arbitrary stream, it is possible to assume some specialized versions of
  this property for given values of the arbitrary stream. In other words,
  implementing a basic way to deal with universal quantification.

+ It could be useful to extend the Copilot language in a way it is possible to
  use annotations inside the Copilot code. For instance, we could

  - Declare assumptions and invariants next to the associated code instead of
    gathering all properties in a single place.
  - Declare a frequent code pattern which should be factorized in the
    transition problem (see the section about Copilot limitations)

## FAQ

### Why does the code related to transition systems look so complex ?

It is true the code of `TransSys` is quite complex. In fact, it would be really
straightforward to produce a flattened transition system and then a Kind2 file
with just a single *top* predicate. In fact, It would be as easy as producing
an *IL* specification.

To be honest, I'm not sure producing a modular *Kind2* output is worth the
complexity added. It's especially true at the time I write this in the sense
that:

* Each predicate introduced is used only one time (which is true because
  copilot doesn't handle functions or parametrized streams like Lustre does and
  everything is inlined during the reification process).
* A similar form of structure could be obtained from a flattened Kind2 native
  input file with some basic static analysis by producing a dependency graph
  between variables.
* For now, the *Kind2* model-checker ignores these structure informations.

However, the current code offers some nice transformation tools (node merging,
`Renaming` monad...) which could be useful if you intend to write a tool for
simplifying or factorizing transition systems. Moreover, it becomes easier to
write local transformations on transition systems as name conflicts can be
avoided more easily when introducing more variables, as there is one namespace
per node.

## References

1. *An insight into SMT-based model checking techniques for
   formal software verification of synchronous dataflow programs*, talk,
   Jonathan Laurent  (see the `doc` folder of this repository)

2. *Scaling up the formal verification of Lustre programs with SMT-based
   techniques*, G. Hagen, C. Tinelli

3. *SMT-based Unbounded Model Checking with IC3 and Approximate Quantifier
   Elimination*, C. Sticksel, C. Tinelli

4. *Verifying safety properties of Lustre programs: an SMT-based approach*,
   PhD thesis, G. Hagen

5. *Understanding IC3*, Aaron R. Bradley

6. *IC3: Where Monolithic and Incremental Meet*, F. Somenzi, A.R. Bradley

7. *Copilot: Monitoring Embedded Systems*, L. Pike, N. Wegmann, S. Niller
