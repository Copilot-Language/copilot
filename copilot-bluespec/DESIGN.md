# The design of `copilot-bluespec`

This document provides an overview of how the `copilot-bluespec` library
compiles a Copilot specification to a Bluespec program. We assume that you
already have some familiarity with core Copilot concepts such as streams,
triggers, externs, etc.

## Bluespec overview

Bluespec is a high-level hardware design language (HDL). Bluespec comes with a
compiler, `bsc`, as well as its own simulator, which allows users to run
Bluespec programs easily. Bluespec also supports compiling to Verilog.

There are two different syntaxes for the Bluespec language: Bluespec Haskell
(BH) and Bluespec SystemVerilog (BSV). Bluespec Haskell (also known as Bluespec
Classic), the older of the two syntaxes, is a more functional, high-level
syntax that is heavily inspired by the Haskell programming language. Bluespec
SystemVerilog, which was created later, more closely SystemVerilog and targets
hardware design engineers who are more familiar with SystemVerilog's syntax.
Both syntaxes are merely front-ends for the same language, and programs written
in one syntax can be converted to the other.

The `copilot-bluespec` library compiles to Bluespec Haskell rather than
Bluespec SystemVerilog. This is primarily because BH's Haskell-like syntax is a
closer fit to how Copilot is structured, which makes the design of the compiler
simpler.

## A small example

To get a sense for how `copilot-bluespec` works, let us compile a simple
Copilot specification down to Bluespec. Our specification will declare a stream
containing the Fibonacci numbers, along with some triggers that will fire if a
Fibonacci number is even or odd:

```hs
module Main (main) where

import qualified Prelude as P ()

import Language.Copilot
import Copilot.Compile.Bluespec

fibs :: Stream Word32
fibs = [1, 1] ++ (fibs + drop 1 fibs)

evenStream :: Stream Word32 -> Stream Bool
evenStream n = (n `mod` constant 2) == constant 0

oddStream :: Stream Word32 -> Stream Bool
oddStream n = not (evenStream n)

spec :: Spec
spec = do
  trigger "even" (evenStream fibs) [arg fibs]
  trigger "odd"  (oddStream fibs) [arg fibs]

main :: IO ()
main = do
  spec' <- reify spec
  compile "Fibs" spec'
```

Note that the only parts of this program that are `copilot-bluespec`–specific
are:

1. The `import Copilot.Compile.Bluespec` statement, and
2. The call to `compile "Fibs" spec'` in `main`. This will compile the Copilot
   specification to a Bluespec program named `Fibs.bs`.

Running this program will generate five files[^1]:

[^1]: The actual code in these files is machine-generated and somewhat
difficult to read. We have cleaned up the code slightly to make it easier to
understand.

* `FibsTypes.bs`: If the Copilot specification contains any structs, then this
  file will contain the corresponding Bluespec struct definitions. The
  specification does not contain any structs, so this file is mostly empty:

  ```bluespec
  package FibsTypes where

  import FloatingPoint
  import Vector
  ```

  Later in this document, we will see a different example that makes use of
  structs. Note that the syntax used here is much like in Haskell, with a
  notable difference being that Bluespec uses the `package` keyword instead of
  `module`.

* `FibsIfc.bs`: This file defines a _module interface_ `FibsIfc`, whose methods
  correspond to the names of the triggers in the Copilot spec:

  ```bluespec
  package FibsIfc where

  import FloatingPoint
  import Vector

  import FibsTypes

  interface FibsIfc =
    even :: UInt 32 -> Action
    odd :: UInt 32 -> Action
  ```

  An interface like `FibsIfc` can be thought of as a special form of data type
  that describes how a module (a hardware object) interacts with the
  surrounding environment. In order for an application to make use of a Copilot
  monitor, it must instantiate `FibsIfc`'s methods. Each method takes a `UInt
  32` (an unsigned 32-bit integer) as an argument and return an `Action`, which
  is the Bluespec type for expressions that act on the state of the circuit (at
  circuit execution time). Possible `Action`s include reading from and writing
  to registers, as well as printing messages.

  We will see an example of how to instantiate `FibsIfc` later.

* `Fibs.bs`: This file defines a `mkFibs` function, which orchestrates
  everything in the generated Bluespec monitor:

  ```bluespec
  package Fibs where

  import FloatingPoint
  import Vector

  import FibsTypes
  import FibsIfc
  import BluespecFP

  mkFibs :: Module FibsIfc -> Module Empty
  mkFibs ifcMod =
    module
      ifc <- ifcMod

      s0_0 :: Reg (UInt 32) <- mkReg 1
      s0_1 :: Reg (UInt 32) <- mkReg 1
      let s0 :: Vector 2 (Reg (UInt 32))
          s0 = update (update newVector 0 s0_0) 1 s0_1
      s0_idx :: Reg (Bit 64) <- mkReg 0

      let s0_get :: Bit 64 -> UInt 32
          s0_get x = (select s0 ((s0_idx + x) % 2))._read

          s0_gen :: UInt 32
          s0_gen = s0_get 0 + s0_get 1

          even_guard :: Bool
          even_guard =
            (s0_get 0 % 2) == 0

          odd_guard :: Bool
          odd_guard =
            not (s0_get 0 % 2 == 0)

      rules
        "even": when even_guard ==>
          ifc.even (s0_get 0)

        "odd:": when odd_guard ==>
          ifc.odd (s0_get 0)

        "step": when True ==>
          action
            select s0 s0_idx := s0_gen
            s0_idx := (s0_idx + 1) % 2
  ```

  `mkFibs` returns a module, which can be thought of as a generator of hardware
  objects. `mkFibs` takes a `FibsIfc` module as an argument and returns another
  module, which is parameterized by `Empty`. `Empty` is a standard interface
  with no methods, and `Empty` is typically used in top-level modules that act
  as program entrypoints.

  Note that the `module` and `action` keywords can be thought of as specialized
  versions of Haskell's `do`-notation, where `module` denotes the `Module`
  monad, and `action` denotes the `ActionValue` monad. (Note that `Action` is
  an alias for `ActionValue ()`.) The `Monad` monad describes how to elaborate
  the structure of a module, whereas the `ActionValue` monad describes the
  behavior of a circuit at execution time.

* `BluespecFP.bsv`: A collection of floating-point operations that leverage BDPI
  (Bluespec's foreign-function interface). We will omit the full contents of
  this file for brevity, but it will look something like this:

  ```bluespec
  import FloatingPoint::*;

  import "BDPI" function Float bs_fp_expf (Float x);
  import "BDPI" function Double bs_fp_exp (Double x);
  import "BDPI" function Float bs_fp_logf (Float x);
  import "BDPI" function Double bs_fp_log (Double x);
  ...
  ```

  For more information on what this file does, see the "Floating-point numbers"
  section below.

* `bs_fp.c`: A collection of floating-point operations implemented in C. These
  functions are imported via BDPI in `BluespecFP.bsv`. We will omit the full
  contents of this file for brevity, but it will look something like this:

  ```bluespec
  #include <math.h>

  union ui_float {
    unsigned int i;
    float f;
  };

  union ull_double {
    unsigned long long i;
    double f;
  };

  unsigned int bs_fp_expf(unsigned int x) {
    ...
  }

  unsigned long long bs_fp_exp(unsigned long long x) {
    ...
  }

  unsigned int bs_fp_logf(unsigned int x) {
    ...
  }

  unsigned long long bs_fp_log(unsigned long long x) {
    ...
  }

  ...
  ```

  For more information on what this file does, see the "Floating-point numbers"
  section below.

In a larger application, a Copilot user would instantiate `mkFibs` with a
`FibsIfc` module that describes what should happen when the `even` and `odd`
triggers fire. `FibsIfc` contains everything that the user must supply;
everything else is handled within the module that `mkFibs` returns.

Here is an example of a larger application might look like:

```bluespec
package Top where

import Fibs
import FibsIfc
import FibsTypes

fibsIfc :: Module FibsIfc
fibsIfc =
  module
    interface
      even x =
        $display "Even Fibonacci number: %0d" x

      odd x =
        $display "Odd  Fibonacci number: %0d" x

mkTop :: Module Empty
mkTop = mkFibs fibsIfc
```

`mkTop` is the top-level module that we will use as a program entrypoint. The
only interesting thing that it does is instantiate `mkFibs` with a custom
`FibsIfc`, where `even` and `odd` are defined to display a custom message
whenever an even or odd Fibonacci number is encountered, respectively.

We can run `mkTop` by using Bluespec's simulator like so:

```
$ bsc -sim -g mkTop -u Top.bs
checking package dependencies
compiling Top.bs
code generation for mkTop starts
Elaborated module file created: mkTop.ba
All packages are up to date.

$ bsc -sim -e mkTop -o mkTop.exe bs_fp.c
Bluesim object created: mkTop.{h,o}
Bluesim object created: model_mkTop.{h,o}
Simulation shared library created: mkTop.exe.so
Simulation executable created: mkTop.exe

$ ./mkTop.exe -m 10
Odd  Fibonacci number: 1
Odd  Fibonacci number: 1
Even Fibonacci number: 2
Odd  Fibonacci number: 3
Odd  Fibonacci number: 5
Even Fibonacci number: 8
Odd  Fibonacci number: 13
Odd  Fibonacci number: 21
Even Fibonacci number: 34
```

We pass `-m 10` to instruct Bluespec's simulator to only run for 10 clock
cycles. Note that the first clock cycle does not fire any rules. This is a
quirk of how hardware works: the reset signal is off in the first cycle, and
the values of registers do not become ready until after the reset signal is
enabled. Because all of the rules depend on registers, none of them will fire
until the second clock cycle or later.

## Streams

Much like in `copilot-c99`, `copilot-bluespec` translates each stream
declaration into a ring buffer. More concretely, it translates a `Stream t`
into a `Vector n (Reg t)`, where:

* A `Vector` is an array whose length is encoded at the type level, must
  like Copilot's arrays. (Note that Bluespec has a separate `Array` type, but
  `Array`s are more low-level, and they are not indexed by their length at the
  type level.)

* `n` is the minimum number of elements needed to compute later values in the
  stream.

* `t` is the stream's element type.

* `Reg` is a register, which stores a value that can be read from and written
  to. As time advances, we will update the `Reg`s in the ring buffer with later
  values in the stream.

(Commentary: a ring buffer is not the only way we could translate a stream to
Bluespec. Bluespec also has a
[`MIMO`](https://github.com/B-Lang-org/bsc/blob/f00d205867eefe09c60e11b4df155bb87041799a/src/Libraries/Base3-Misc/MIMO.bsv#L52-L73)
(many-in, many-out) queue that is _almost_ suitable for our needs, but it comes
with an unusual restriction that it must have a minimum size of 2 elements.
There exist Copilot streams that only require one element of storage, so we
would have to special-case these streams if we wanted to use a `MIMO`.)

The `Fibs.bs` example above contains exactly one stream, which is created at
the top of the `mkFibs` function:

```bluespec
    s0_0 :: Reg (UInt 32) <- mkReg 1
    s0_1 :: Reg (UInt 32) <- mkReg 1
    let s0 :: Vector 2 (Reg (UInt 32))
        s0 = update (update newVector 0 s0_0) 1 s0_1
    s0_idx :: Reg (Bit 64) <- mkReg 0
```

Here, `s0_idx`, tracks the index of the next stream element to be updated.
`mkFibs` then defines several functions in terms of `s0` and `s0_idx`, which
are then used in the rules, which we will describe later. Note that `s0_idx` is
a register of type `Bit 64`, which can be thought of as a raw 64-bit value.

In order to access an element of a stream, we make use of the `s0_get`
function:

```bluespec
      let s0_get :: Bit 64 -> UInt 32
          s0_get x = (select s0 ((s0_idx + x) % 2))._read
```

This will use `s0_idx` and an offset `x` to compute which `Reg` in the `Vector`
to read from.

## Rules

The _rules_ govern what actions are performed during each cycle. There are
three actions in the `Fibs.hs` example:

```bluespec
    rules
      "even": when even_guard ==>
        ifc.even (s0_get 0)

      "odd:": when odd_guard ==>
        ifc.odd (s0_get 0)

      "step": when True ==> do
        select s0 s0_idx := s0_gen
        s0_idx := (s0_idx + 1) % 2
```

Each rule consists of three parts:

1. A label (e.g., `"even"`) that uniquely identifies the rule within the
   module.

2. An explicit condition, which is a boolean expression of the form
   `when <cond> ==> ...`. In order for a rule to fire on a given clock cycle,
   its explicit condition `<cond>` must hold.

3. A rule body (e.g., `ifc.even (s0_get 0)`), which describes what action the
   rule performs if it fires on a given clock cycle.

In the example above, the `"even"` and `"odd"` rules govern the behavior of the
triggers in the Copilot specification. These rules will only fire if
`even_guard` or `odd_guard` hold, and if they fire, they will call the `even`
or `odd` method of `FibsIfcs`, respectively. The definitions of `even_guard`
and `odd_guard` are:

```bluespec
          even_guard :: Bool
          even_guard =
            (s0_get 0 % 2) == 0

          odd_guard :: Bool
          odd_guard =
            not (s0_get 0 % 2 == 0)
```

The `"step"` rule is the heart of the Copilot specification, and it always runs
on each clock cycle. `"step"` does two things:

1. It computes the next element of the `s0` stream using the `s0_gen` function,
   which is defined like so:

   ```bluespec
             s0_gen :: UInt 32
             s0_gen = s0_get 0 + s0_get 1
   ```

2. It increments `s0_idx`, making sure to wrap around to `0` if its value
   exceeds `1`.

### A note on atomicity

Bluespec rules are _atomic_, which means that the Bluespec compiler will ensure
that the rules are executed in some logical order that ensures the absence of
race conditions. Moreover, all register updates that occur within a module's
rules are performed simultaneously at the end of a clock cycle.

This might seem strange if you are accustomed to the execution model of a
language like C, where all statements (and their accompanying side effects) are
invoked sequentially, one after the other. This is not how Bluespec works,
however. Recall the definitions of the three rules in the example program
above:

```bluespec
    rules
      "even": when even_guard ==>
        ifc.even (s0_get 0)

      "odd:": when odd_guard ==>
        ifc.odd (s0_get 0)

      "step": when True ==> do
        select s0 s0_idx := s0_gen
        s0_idx := (s0_idx + 1) % 2
```

If this were a language like C, then the order in which the rules appear would
have an effect on the runtime semantics of the program, as the behavior of
`s0_get` (used in the `"even"` and `"odd"` rules) depends on the current value
of `s0` and `s0_idx`. In Bluespec, however, the order of these rules do _not_
matter. The `"step"` rule performs side effects of updating the value of `s0`
and `s0_idx`, but these effects are performed in a single, atomic transaction
at the end of the clock cycle. This means that the values of `s0` and `s0_idx`
that `s0_get` sees will always match their values at the start of the clock
cycle, regardless of what order Bluespec uses to invoke the rules.

Because the Bluespec compiler can pick whatever rule order it wishes to, this
can impact the order in which `$display` output is presented. This doesn't
impact the example above, as the `"even"` and `"odd"` rules will never fire on
the same clock cycle, meaning that there is always only one `$display` call per
cycle. If program fires multiple rules on a clock cycle and each one calls
`$display`, then the order in which the `$display` calls will run is not
specified.

## External streams

The example above sampled data from a stream that was defined within the
Copilot specification. To see what happens when a specification uses an
external stream, let's make one small change to the example:

```diff
 fibs :: Stream Word32
-fibs = [1, 1] ++ (fibs + drop 1 fibs)
+fibs = extern "fibs" Nothing
```

Now let's re-run the example and inspect the resulting Bluespec program. There
are two notable changes worth commenting on. The first change is in
`FibsIfc.bs`:

```bluespec
package FibsIfc where

import FloatingPoint
import Vector

import FibsTypes

interface FibsIfc =
  even :: UInt 32 -> Action
  odd :: UInt 32 -> Action
  fibs :: Reg (UInt 32)
```

This time, the `FibsIfc` interface has an additional `fibs` method of type `Reg
(UInt 32)`. This register corresponds to the external stream that we just
defined, and it is the responsibility of the application which instantiates
`FibsIfc` to dictate what values should be written to the register.

The second change is in `Fibs.bs`:

```bluespec
package Fibs where

import FloatingPoint
import Vector

import FibsTypes
import FibsIfc

mkFibs :: Module FibsIfc -> Module Empty
mkFibs ifcMod =
  module
    ifc <- ifcMod

    let even_guard :: Bool
        even_guard =
          (ifc.fibs._read % 2) == 0

        odd_guard :: Bool
        odd_guard =
          not (ifc.fibs._read % 2 == 0)

    rules
      "even": when even_guard ==>
        ifc.even ifc.fibs._read

      "odd:": when odd_guard ==>
        ifc.odd ifc.fibs._read
```

This time, the only stream that is referenced is `ifc.fibs`. As a consequence,
the `s0` stream, the `s0_idx` index, and the `"step"` rule are no longer
necessary, making the code considerably simpler.

The flip side to having the generated code be simpler is that the application
which uses `mkFibs` must now do additional work to specify what the value of
`ifc.fibs` is. For example, we can port the behavior of the previous version
of the program like so:

```bluespec
package Top where

import Vector

import Fibs
import FibsIfc
import FibsTypes

fibsIfc :: Module FibsIfc
fibsIfc =
  module
    s0_0 :: Reg (UInt 32) <- mkReg 1
    s0_1 :: Reg (UInt 32) <- mkReg 1
    let s0 :: Vector 2 (Reg (UInt 32))
        s0 = update (update newVector 0 s0_0) 1 s0_1
    s0_idx :: Reg (Bit 64) <- mkReg 0

    let s0_get :: Bit 64 -> UInt 32
        s0_get x = (select s0 ((s0_idx + x) % 2))._read

        s0_gen :: UInt 32
        s0_gen = s0_get 0 + s0_get 1

    fibs_impl :: Reg (UInt 32) <- mkReg 1

    interface
      even x =
        $display "Even Fibonacci number: %0d" x

      odd x =
        $display "Odd  Fibonacci number: %0d" x

      fibs = fibs_impl

    rules
      "fibs": when True ==>
        fibs_impl := s0_get 1

      "step": when True ==>
        action
          select s0 s0_idx := s0_gen
          s0_idx := (s0_idx + 1) % 2

mkTop :: Module Empty
mkTop = mkFibs fibsIfc
```

The code involving `s0`, `s0_idx`, and the `"step"` rule is exactly as before.
The only notable difference is that we create an additional `fibs_impl`
register, declare `fibs = fibs_impl` when instantiating the `FibsIfc`
interface, and declare an additional `"fibs"` rule to update the value of
`fibs_impl` on each clock cycle. Note that we always set the value of
`fibs_impl` to be `s0_get 1`, which corresponds to the most recently computed
Fibonacci number. If we set `fibs_impl` to be `s0_get 0` instead, then
`fibs_impl` would lag behind by one cycle. (Try it.)

# Notes

## Arrays

Copilot's `Array n t` type is translated directly to Bluespec's `Vector n t`
type, which makes life very simple.

## Structs

Like Copilot, Bluespec has a notion of structs, e.g.,

```bluespec
struct Coord =
  x :: UInt 32
  y :: UInt 32

exCoord :: Coord
exCoord = Coord { x = 27, y = 42 }

flipCoord :: Coord -> Coord
flipCoord c = Coord { x = C.y, y = C.x }
```

As such, it proves fairly straightforward to translate Copilot structs to
Bluespec structs. For instance, given these Copilot structs:

```hs
data Volts = Volts
  { numVolts :: Field "numVolts" Word16
  , flag     :: Field "flag"     Bool
  }

instance Struct Volts where
  typeName _ = "Volts"
  toValues volts = [ Value Word16 (numVolts volts)
                   , Value Bool   (flag volts)
                   ]

instance Typed Volts where
  typeOf = Struct (Volts (Field 0) (Field False))

data Battery = Battery
  { temp  :: Field "temp"  Word16
  , volts :: Field "volts" Volts
  }

instance Struct Battery where
  typeName _ = "Battery"
  toValues battery = [ Value typeOf (temp battery)
                     , Value typeOf (volts battery)
                     ]

instance Typed Battery where
  typeOf = Struct (Battery (Field 0) (Field undefined))
```

`copilot-bluespec` will generate the following `StructsTypes.bs` file:

```bluespec
package StructsTypes where

import FloatingPoint
import Vector

struct Volts =
    numVolts :: UInt 16
    flag :: Bool
 deriving (Bits)

struct Vattery =
    temp :: UInt 16
    volts :: BS_volts
 deriving (Bits)
```

The purpose of the `*Types.bs` file is to define structs separately from other
parts of the generated code. Note that each struct definition derives a `Bits`
instance so that it can be stored inside a `Reg`.

## Capitalization of generated names

Copilot allows users to name several things which will influence the names of
generated code, including external streams, triggers, structs, and the file
names. The `copilot-c99` backend is able to turn just about any user-supplied
name into the name of a C identifier, as C is a remarkably case-insensitive
language. For instance, both "`Volts`" and "`volts`" are legal struct names in
C.

Bluespec, on the other hand, imposes restrictions on which identifiers can
begin with an uppercase letter and which can begin with a lowercase letter.
The following identifiers must begin with an uppercase letter:

* Struct names
* Interface names

The following identifiers must begin with a lowercase letter:

* Function and value names
* Interface method names

Package names and rule names may begin with either an uppercase or lowercase
letter.

How do we ensure that `copilot-bluespec` respects Bluespec's capitalization
requirements? As an example, suppose a Copilot specification defines a struct
named "`volts`". This name cannot be used as a Bluespec struct name on its own,
as it does not begin with an uppercase letter. To avoid issues, we prepend the
prefix "`BS_`" to this name to get "`BS_volts`", which is a valid Bluespec
identifier. Similarly, `copilot-bluespec` will prepend the prefix `bs_` to
uppercase Copilot names if it is used in a place that expects lowercase
Bluespec identifiers.

Note that `copilot-bluespec` will only prepend prefixes if they are necessary
to prevent the generated code from being littered with `BS_`/`bs_` prefixes in
the common case.

## Floating-point numbers

`copilot-bluespec` supports all of Copilot's floating-point operations with
varying degrees of performance. The following floating-point operations compile
directly to relatively performant circuits:

* Basic arithmetic (`(+)`, `(-)`, `(*)`, `(/)`)
* Equality checking (`(==)` and `(/=)`)
* Inequality checking (`(<)`, `(<=)`, `(>)`, and `(>=)`)
* `abs`
* `signum`
* `recip`

These operations correspond to the floating-point operations that the Bluespec
standard library provides that are well tested. Unfortunately, the Bluespec
standard library does not offer well-tested versions (or even _any_ versions)
of the remainder of Copilot's floating-point operations. The rest of these
operations are instead implemented by using BDPI (Bluespec's foreign function
interface) to interface with C code:

* `sqrt`
* `acos`
* `asin`
* `atan`
* `atan2`
* `cos`
* `sin`
* `tan`
* `acosh`
* `asinh`
* `atanh`
* `cosh`
* `sinh`
* `tanh`
* `exp`
* `pow`
* `log`
* `logb`
* `ceiling`
* `floor`

Implementing these operations via C provides high confidence that they are
implemented correctly, but at a somewhat steep performance penalty.

Because these operations need to be implemented via BDPI, `copilot-bluespec`
generates two additional files: `BluespecFP.bsv` (which contains the Bluespec
function stubs for each function implemented via BDPI) and `bs_fp.c` (which
contains the corresponding C function definitions). To see how this works,
let us take a look at one of the BDPI'd functions, `sqrt`:

```bluespec
import "BDPI" function Double bs_fp_sqrt (Double x);
import "BDPI" function Float bs_fp_sqrtf (Float x);
```

This declares a Bluespec function `bs_fp_sqrt` that is implemented using a C
function (also of the name `bs_fp_sqrt`) under the hood. This takes a Bluespec
`Double` as an argument and also returns a `Double`. Note that `Double` is not
treated magically by the Bluespec compiler here. This is because any Bluespec
struct can be used in BDPI (provided that the struct type implements the `Bits`
class), and Bluespec's `Double` is implemented as a struct with a `Bits`
instance that exactly matches the bit layout expected by IEEE-754
double-precision floats. (Similarly for Bluespec's `Float` type.)

Note that at present, the `import "BDPI"` feature is only available when using
the BSV syntax, not the BH syntax. As such, this is currently the only place
where we generate BSV code.

The corresponding C code for `bs_fp_sqrt(f)` is:

```c
union ull_double {
  unsigned long long i;
  double f;
};

union ui_float {
  unsigned int i;
  float f;
};

unsigned long long bs_fp_sqrt(unsigned long long x) {
  union ull_double x_u;
  union ull_double r_u;
  x_u.i = x;
  r_u.f = sqrt(x_u.f);
  return r_u.i;
}

unsigned int bs_fp_sqrtf(unsigned int x) {
  union ui_float x_u;
  union ui_float r_u;
  x_u.i = x;
  r_u.f = sqrtf(x_u.f);
  return r_u.i;
}
```

There is a lot to unpack here. Let's go through this step by step:

1. The C version of `bs_fp_sqrt` takes and returns an `unsigned long long`. The
   use of `unsigned long long` is dictated by Bluespec itself: whenever you
   use a Bluespec type in BDPI that fits in exactly 64 bits, then Bluespec
   expects the corresponding C type to be `unsigned long long`. (You can see
   this for yourself by inspecting the generated `imported_BDPI_functions.h`
   header file.)

   There is a similar story for `bs_fp_sqrtf`, which takes an `unsigned int`.
   Bluespec dictates the use of `unsigned int` when the BDPI types fits in
   exactly 32 bits.

2. This poses something of a challenge for us, since we want the implementation
   of `bs_fp_sqrt` to work over `double`s, not `unsigned long long`s. To make
   this possible, we define a `union ull_double` type that allows easily
   converting an `unsigned long long` to a `double` and vice versa.

   There is an analogous story for `ui_float`, which allows conversion to and
   from the `unsigned int` and `float` types.

3. Finally, we perform the `sqrt(f)` function on the argument, using
   `ull_double`/`ui_float` as necessary to make the types work out.

Strictly speaking, it is only necessary to compile the generated `bs_fp.c` file
if the generated Bluespec program makes use of any of the BDPI-related
floating-point operations mentioned above. That being said, it doesn't hurt to
compile it even if the generated Bluespec program _doesn't_ use any of them, so
it's generally good practice to pass `bs_fp.c` to `bsc`.

Eventually, we would like to stop using BDPI in favor of native Bluespec code,
which would be more performant. To do so, would we need to address the
following Bluespec issues:

* The implementation of `sqrt` in Bluespec's standard library is buggy:
  https://github.com/B-Lang-org/bsc/issues/710

* Bluespec's standard library does not implement the remaining floating-point
  operations at all: https://github.com/B-Lang-org/bsc/issues/368

## Warnings

Code generated by `copilot-bluespec` should always compile, but it is not
currently guaranteed that the code will compile without warnings. This is
because unlike `gcc` or `clang`, the `bsc` compiler produces much more warnings
by default, and in rare cases, you may trigger `bsc` warnings. This section
describes some of these warnings.

### Rule bodies with no actions (G0023)

Let's suppose you have a trigger named `nop`, and you instantiate it like so:

```bluespec
exampleIfc :: Module ExampleIfc
exampleIfc =
  module
    interface
      nop :: Action
      nop = return ()
```

As a result, firing the `nop` trigger will do absolutely nothing. `bsc` takes
notice of this fact and warns you about it:

```
Warning: "CopilotTest.bs", line 27, column 8: (G0023)
  The body of rule `nop' has no actions. Removing...
```

Where `G0023` is a unique code associated with this class of warning.

### Warning suppression notification (S0080)

If you find the warnings above to be annoying, `bsc` offers a way to suppress
them with the `-suppress-warnings <warning-code-1>:<warning-code-2>:...` flag.
For instance, you can suppress the G0023 warning code by passing:

```
-suppress-warnings G0023
```

This does indeed suppress them, but now `bsc` produces _another_ warning:

```
Warning: Unknown position: (S0080)
  1 warnings were suppressed.
```

That's right, `bsc` warns you about the use of `-suppress-warnings`. That feels
a bit silly to me, but in any case, the new warning comes with its own warning
code, S0080. As a result, you can _really_ suppress warnings by passing this to
`bsc`:

```
-suppress-warnings G0023:S0080
```

## `language-bluespec`

`copilot-bluespec` uses
[`language-bluespec`](https://github.com/GaloisInc/language-bluespec) as a
library for creating Bluespec Haskell ASTs. Most of the source code from
`language-bluespec` is directly based on the code in the Bluespec compiler,
[`bsc`](https://github.com/B-Lang-org/bsc). This is because there is (to our
knowledge) no other existing Haskell library for representing Bluespec Haskell
ASTs, and adapting the code in `bsc` proves to be the most straightforward way
of achieving what `copilot-bluespec` needs.

It is conceivable that if the Bluespec language evolves in the future, then we
will need to update `language-bluespec` accordingly. This seems somewhat
unlikely, however, as we are generating code in a very simple subset of the
Bluespec language.

Another possibility is to split out the AST parts of `bsc` into their own
library and maintain them going forward. See
https://github.com/B-Lang-org/bsc/issues/546 for more discussion on this point.
