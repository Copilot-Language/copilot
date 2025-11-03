# The design of `copilot-bluespec`

This document provides an overview of how the `copilot-bluespec` library
compiles a Copilot specification to a Bluespec program. We assume that you
already have some familiarity with core Copilot concepts such as streams,
triggers, externs, etc.

# Bluespec overview

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

# A small example

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

Running this program will generate four files[^1]:

[^1]: The actual code in these files is machine-generated and somewhat
difficult to read. We have cleaned up the code slightly to make it easier to
understand.

### `Fibs.bs`

This defines the majority of the Bluespec code needed to implement a monitor:

```bluespec
package Fibs where

import FloatingPoint
import Vector

import FibsTypes
import BluespecFP

interface FibsIfc {-# always_ready, always_enabled #-} =
    even_guard :: Bool
    even_arg0 :: UInt 32
    odd_guard :: Bool
    odd_arg0 :: UInt 32

{-# properties mkFibs = { verilog } #-}
mkFibs :: Module FibsIfc
mkFibs =
  module
    s0_0 :: Reg (UInt 32) <- mkReg 1
    s0_1 :: Reg (UInt 32) <- mkReg 1
    let s0 :: Vector.Vector 2 (Reg (UInt 32))
        s0 = update (update newVector 0 s0_0) 1 s0_1
    s0_idx :: Reg (Bit 64) <- mkReg 0

    let s0_get :: Bit 64 -> UInt 32
        s0_get x = (select s0 ((s0_idx + x) % 2))._read

        s0_gen :: UInt 32
        s0_gen = s0_get 0 + s0_get 1

    rules
      "step": when True ==>
        action
          select s0 s0_idx := s0_gen
          s0_idx := (s0_idx + 1) % 2

    interface FibsIfc
      even_guard :: Bool
      even_guard =
        (s0_get 0 % 2) == 0

      even_arg0 :: UInt 32
      even_arg0 = s0_get 0

      odd_guard :: Bool
      odd_guard =
        not (s0_get 0 % 2 == 0)

      odd_arg0 :: UInt 32
      odd_arg0 =  s0_get 0

interface FibsRulesIfc =
  even_action :: UInt 32 -> Action
  odd_action :: UInt 32 -> Action

mkFibsRules :: FibsIfc -> FibsRulesIfc -> Rules
mkFibsRules ifc ifcRules =
  rules
    "even": when ifc.even_guard ==>
      ifcRules.even_action ifc.even_arg0

    "odd": when ifc.odd_guard ==>
      ifcRules.odd_action ifc.odd_arg0

addFibsRules :: FibsIfc -> FibsRulesIfc -> Module Empty
addFibsRules ifc ifcRules = addRules (mkFibsRules ifc ifcRules)
```

Those familiar with Haskell will likely recognize much of the syntax used here,
although it has some minor differences (e.g., using the `package` keyword
instead of `module`, which means something else in Bluespec). We will go into
the parts of the syntax that are unique to Bluespec as they come up.

The `Fibs.bs` file consists of five definitions:

1. `FibsIfc`, which is a _module interface_. An interface like `FibsIfc` can be
   thought of as a special form of data type that describes how a module (a
   hardware object) interacts with the surrounding environment. In order for an
   application to make use of a Copilot monitor, it must instantiate
   `FibsIfc`'s methods. We will see an example of how to instantiate `FibsIfc`
   momentarily.

   The methods of `FibsIfc` correspond to the names of the triggers in the
   Copilot spec. Each trigger has a corresponding _guard_ method, which always
   has the type `Bool`, and an _argument_ method for each argument that the
   trigger accepts, which has a type corresponding to the type of the argument.
   We will see how these work in more detail shortly. For now, think of these
   methods as being the "outputs" of the monitor program.

2. `mkFibs`, which returns a module of type `Module FibsIfc`. A module can be
   thought of as a generator of hardware objects. The module uses the `FibsIfc`
   interface, which indicates that the implementation of the module
   instantiates all of the methods in `FibsIfc`. If you squint, you'll
   recognize bits and pieces of the original Copilot spec, such as the part
   that checks for even stream elements in `even_guard`. We will go over the
   individual definitions used in the implementation of `mkFibs` later.

   Note `mkFibs`'s use of the `module` and `action` keywords. These can be
   thought of as specialized versions of Haskell's `do`-notation, where
   `module` denotes the `Module` monad, and `action` denotes the `ActionValue`
   monad. (Note that `Action` is an alias for `ActionValue ()`.) The `Module`
   monad describes how to elaborate the structure of a module, whereas the
   `ActionValue` monad describes the behavior of a circuit at execution time.

   Another piece of syntax that is unique to Bluespec modules is the `rules`
   keyword, which denote rules that may or may not be run on each clock cycle
   of the hardware. This particular module only has a single rule, named
   `step`, which updates some internal state used by the module. That internal
   state is then used to define the values that the interface methods return.
   The `when True` part of the rule indicates that it always runs on every
   clock cycle.

3. `FibsRulesIfc` is another module interface, with one method for each of the
   Copilot spec's triggers. Each method takes arguments corresponding to the
   types of the triggers' arguments and returns a value of type `Action`. As
   the name `FibsRulesIfcs` suggests, these methods are intended to be used to
   build up rule definitions, and we will see an example of this shortly.

   As an aside, you may be wondering why there are two different interfaces
   that each have methods corresponding to the Copilot spec's trigger
   functions. Why not just have a single interface instead? The short answer to
   this question is that these interfaces are intended for different use cases:
   modules that instantiate `FibsRulesIfc` are intended for simulation, whereas
   modules that instantiate `FibsIfc` are intended for Verilog code generation.
   For a longer answer to this question, see the "Generating Verilog code"
   section below.

4. `mkFibsRules`, a function which takes two values that instantiate the
   `FibsIfc` and `FibsRulesIfc` interfaces and returns a `Rules` value. A value
   of type `Rules` is just a set of rules, which can be passed around in
   Bluespec like any other value.

   `mkFibsRules` defines two rules, one for each trigger function in the
   original Copilot spec. Unlike the `step` rule seen earlier, these rules may
   not be run on every clock cycle. Specifically, the `even` rule will only run
   if the `ifc.even_guard` expression returns `True` on a given clock cycle,
   and the `odd` rule will only run if `ifc.odd_guard` returns `True` on a
   given clock cycle. If one of these rules are run, then they will invoke the
   corresponding method from the `FibsRulesIfc` interface, passing in the
   corresponding `*_arg` method(s) from the `FibsIfc`.

5. `addFibsRules`, a function which is very similar to `mkFibsRules`, except
   that it has the return type `Module Empty` instead of `Rules`. All that
   `addFibsRules` does is invoke `mkFibsRules` and then pass it to Bluespec's
   `addRules` function, which actually adds the rules to a module. This is a
   convenience that allows users to quickly define a full Bluespec module that
   is suitable for simulation purposes (see the "Simulating Bluespec code"
   section below).

   Note that the module that this returns instantiates the `Empty` interface.
   `Empty` is a standard interface with no methods at all, and `Empty` is
   typically used in top-level Bluespec modules that act as entrypoints to
   simulation.

### `FibsTypes.bs`

If the Copilot specification contained any structs, then the generated
`FibsTypes.bs` file would contain the corresponding Bluespec struct
definitions. The specification in `Fibs.hs` does not contain any structs, so
this file is mostly empty:

```bluespec
package FibsTypes where

import FloatingPoint
import Vector
```

Later in this document, we will see a different example that makes use of
structs.

### `BluespecFP.bsv`

This is a collection of floating-point operations that leverage BDPI
(Bluespec's foreign-function interface). We will omit the full contents of this
file for brevity, but it will look something like this:

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

### `bs_fp.c`

A collection of floating-point operations implemented in C. These functions are
imported via BDPI in `BluespecFP.bsv`. We will omit the full contents of this
file for brevity, but it will look something like this:

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

## Using the generated Bluespec code

What do we do with the Bluespec code after generating it? The answer to this
question ultimately depends on your intended use case. Sometimes, it can be
useful to just simulate the Bluespec code in software, which we can do with a
variety of hardware simulators. In other use cases, it can be more useful to
directly synthesize code that can be embedded in real hardware. We will review
each of these workflows below.

### Simulating Bluespec code

Hardware simulation allows us to model the behavior of hardware without
actually needing access to physical hardware to do so. There are a number of
different simulators out there, such as Bluespec's own Bluesim simulator, as
well as Verilog-specific simulators such as Verilator and Icarus Verilog
(`iverilog`). We will be using Bluesim throughout this section, as it is
already bundled with each installation of Bluespec.

Suppose we have a Bluespec program that we want to monitor with Copilot. This
program will likely have a top-level module (typically of type `Module Empty`),
and it is the role of the program's developer to decide what action the program
should take whenever the monitor fires a trigger. In a full hardware setting,
these actions might constitute something like blinking an LED or displaying a
message on a seven-segment display. For simulation purposes, however, we can
approximate this behavior by doing something simpler (e.g., printing messages
to standard output on the terminal).

Here is an example of a program that uses the Copilot-generated `Fibs.bs`
monitor shown above:

```bluespec
package Top where

import Fibs
import FibsTypes

mkTop :: Module Empty
mkTop =
  module
    fibsMod <- mkFibs
    addFibsRules fibsMod $
      interface FibsRulesIfc
        even_action x =
          $display "Even Fibonacci number: %0d" x

        odd_action x =
          $display "Odd  Fibonacci number: %0d" x
```

`mkTop` is the top-level module that we will use as a program entrypoint. The
only interesting thing that it does is create a `FibsIfc` module internally
(using `mkFibs`) and then define the actions to take when the `even` or `odd`
triggers fire. These actions are defined by the implementations of the
`even_action` and `odd_action` methods, respectively, which print custom
messages to standard output.

We can run `mkTop` by using Bluesim like so:

```
$ bsc -sim -g mkTop -u Top.bs
$ bsc -sim -e mkTop -o mkTop.exe bs_fp.c
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

### Generating Verilog code

Generating code that is suitable for hardware is a somewhat different problem
than generating code that is suitable for simulation. Many types of hardware
(e.g., FPGAs) require specific hardware description languages in order to
synthesize hardware circuits, such as Verilog or VHDL. Bluespec supports
compiling to Verilog, so we will be focusing on that in this section.

A typical Verilog module has a number of _signals_ that represent the paths
into a circuit (called _inputs_), within a circuit (called _wires_), and
outside of the circuit (called _outputs_). The generated Bluespec code seen
above has been carefully designed such that the triggers in the Copilot spec
compile directly to Verilog outputs. (Copilot also has a notion of external
streams that compile to Verilog inputs; see the "External streams" section for
more on this.)

Unlike the simulation use case, generating Verilog code does not make use of
the `FibsRulesIfc` interface at all, as this is a convenience that is only
provided to make simulation easier. Instead, we compile the `mkFibs` module
directly to Verilog:

```
$ bsc -verilog -g mkFibs -u Fibs.bs
```

This generates a Verilog module `mkFibs.v` that looks like the following:

```verilog
module mkFibs(CLK,
	      RST_N,

	      even_guard,

	      even_arg0,

	      odd_guard,

	      odd_arg0);
  input  CLK;
  input  RST_N;

  // value method even_guard
  output even_guard;

  // value method even_arg0
  output [31 : 0] even_arg0;

  // value method odd_guard
  output odd_guard;

  // value method odd_arg0
  output [31 : 0] odd_arg0;

  ...

endmodule  // mkFibs
```

We show only a subset of the generated Verilog code to highlight the inputs and
outputs. There are two inputs corresponding to the `CLK` (clock) and `RST_N`
(reset) signals, which are generated for all Copilot monitors, regardless of
what the underlying spec is. The outputs correspond to the methods of the
`FibsIfc` interface. For instance, the signal in the `even_guard` output
indicates whether the `even` trigger fires on a given clock cycle or not, and
the value in the `even_arg0` output indicates what argument was passed to the
trigger on that clock cycle. These outputs can then be hooked up to another
part of a hardware circuit (e.g., LEDs) that can respond to these values as
needed.

In general, if a Bluespec interface has a method of type `t` (where `t` is a
non-monadic type), then that method will be compiled to a Verilog output. If a
method has type `t -> Action`, however, then that method will be compiled to a
Verilog input instead. This is a key reason why it is necessary to separate
`FibsIfc` from `FibsRulesIfc`. If we compiled a `Module FibsRulesIfc` to
Verilog, then the triggers would become inputs, not outputs!

(As a side note: the generated Bluespec code also employs various other tricks
to get the compiled Verilog code to look the way it does. For instance, note
the `{-# properties mkFibs = { verilog } #-}` pragma above `mkFibs`, which
instructs the Bluespec compiler to synthesize Verilog code for it. Also note
the `{-# always_ready, always_enabled #-}` attributes used in `FibsIfc`, which
ensure that the generated Verilog code does not contain unwanted "ready" or
"enabled" signals as inputs.)

For a more in-depth tutorial on how to use the generated Verilog code on an
actual FPGA, see [this
tutorial](https://github.com/Copilot-Language/copilot/blob/master/copilot/examples/fpga/HelloWorld/README.md).

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
three actions in the `Fibs.bs` example above: one in `mkFibs`:

```bluespec
    rules
      "step": when True ==>
        action
          select s0 s0_idx := s0_gen
          s0_idx := (s0_idx + 1) % 2
```

And two more in `mkFibsRules`:

```bluespec
  rules
    "even": when ifc.even_guard ==>
      ifcRules.even_action ifc.even_arg0

    "odd": when ifc.odd_guard ==>
      ifcRules.odd_action ifc.odd_arg0
```

Each rule consists of three parts:

1. A label (e.g., `"step"`) that uniquely identifies the rule within the
   module.

2. An explicit condition, which is a boolean expression of the form
   `when <cond> ==> ...`. In order for a rule to fire on a given clock cycle,
   its explicit condition `<cond>` must hold.

3. A rule body (of type `Action`), which describes what action the rule
   performs if it fires on a given clock cycle.

In the example above, the `"even"` and `"odd"` rules govern the behavior of the
triggers in the Copilot specification. These rules will only fire if
`even_guard` or `odd_guard` hold, and if they fire, they will call the
`even_action` or `odd_action` method of `FibsRulesIfc`, respectively.

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
"step": when True ==>
  action
    select s0 s0_idx := s0_gen
    s0_idx := (s0_idx + 1) % 2

"even": when ifc.even_guard ==>
  ifcRules.even_action ifc.even_arg0

"odd": when ifc.odd_guard ==>
  ifcRules.odd_action ifc.odd_arg0
```

If this were a language like C, then the order in which the rules appear would
have an effect on the runtime semantics of the program, as the values that
`even_arg0` and `odd_arg0` return (used in the `"even"` and `"odd"` rules)
depends on the current value of `s0` and `s0_idx`. In Bluespec, however, the
order of these rules do _not_ matter. The `"step"` rule performs side effects
of updating the value of `s0` and `s0_idx`, but these effects are performed in
a single, atomic transaction at the end of the clock cycle. This means that the
values of `s0` and `s0_idx` that `s0_get` sees will always match their values
at the start of the clock cycle, regardless of what order Bluespec uses to
invoke the rules.

Because the Bluespec compiler can pick whatever rule order it wishes to, this
can impact the order in which `$display` output is presented. This doesn't
impact the example above, as the `"even"` and `"odd"` rules will never fire on
the same clock cycle, meaning that there is always only one `$display` call per
cycle. If program fires multiple rules on a clock cycle and each one calls
`$display`, then the order in which the `$display` calls will run is not
specified.

## External streams

The example above sampled data from a stream that was defined within the
Copilot specification. Copilot also features a notion of _external_ streams,
which sample data from the surrounding environment. (For instance, one could
populate an external stream with data read from a sensor in a real-world
application.) To see what happens when a specification uses an external stream,
let's make one small change to the example above:

```diff
 fibs :: Stream Word32
-fibs = [1, 1] ++ (fibs + drop 1 fibs)
+fibs = extern "fibs" Nothing
```

Now let's re-run the example and inspect the resulting Bluespec program. There
are several notable changes worth commenting on. The first change is in the
definition of the `FibsIfc` interface:

```bluespec
interface FibsIfc {-# always_ready, always_enabled #-} =
    even_guard :: Bool
    even_arg0 :: UInt 32
    odd_guard :: Bool
    odd_arg0 :: UInt 32
    fibs :: UInt 32 -> Action {-# prefixs = "", arg_names = [fibs] #-}
```

This time, there is an additional `fibs` method of type `UInt 32 -> Action`.
This type is carefully chosen such that when `FibsIfc` is compiled to Verilog,
`fibs` will be represented as an input to the Verilog module, which intuitively
matches how external streams are used in Copilot itself. (Also note the
attributes to the right of the type signature, which ensure that the generated
Verilog input has precisely the name "`fibs`".)

The next change is in `mkFibs`, which demonstrates how the `fibs` method is
instantiated:

```bluespec
{-# properties mkFibs = { verilog } #-}
mkFibs :: Module FibsIfc
mkFibs =
  module
    fibs_wire :: Wire (UInt 32) <- mkBypassWire

    interface FibsIfc
      even_guard :: Bool
      even_guard =
        (fibs_wire._read % 2) == 0

      even_arg0 :: UInt 32
      even_arg0 = fibs_wire._read

      odd_guard :: Bool
      odd_guard =
        not (fibs_wire._read % 2 == 0)

      odd_arg0 :: UInt 32
      odd_arg0 = fibs_wire._read

      fibs val = fibs_wire := val
```

This looks quite a bit different! Let's go over what has changed:

* There are no longer any registers to hold internal state, nor is there a
  `"step"` rule that updates the registers. These registers were ultimately
  used in service of maintaining the ring buffers that comprised the definition
  of the `fibs` stream that was defined within Copilot. Now that `fibs` is an
  external stream, none of the ring buffer machinery is needed anymore.

  (Note that this is not an all-or-nothing proposition, as it is possible to
  have a Copilot spec that combines defined-within-Copilot streams and external
  streams.)

* There is a new piece of internal state: the `fibs_wire` `Wire`. Moreover,
  all places where `s0_get 0` was called before have now been replaced with
  `fibs_wire._read`. A `Wire` is essentially a special type of register that
  compiles to more compact Verilog code.

* Finally, there is now an implementation of the `fibs` method, which writes
  the supplied value to `fibs_wire`. This is what allows other methods to see
  the most recently sampled value in the `fibs` external stream.

The next change is in the definition of `FibsRulesIfc`, which has also gained
another method corresponding to the `fibs` external stream:

```bluespec
interface FibsRulesIfc =
  even_action :: UInt 32 -> Action
  odd_action :: UInt 32 -> Action
  fibs_action :: ActionValue (UInt 32)
```

Unlike `even_action` and `odd_action`, which denote an action for _consuming_ a
value, the type of `fibs_action` (`ActionValue (UInt 32)`) denotes how to
_produce_ a value. This makes sense, since the application being monitored
ultimately has to be the source of the values that populate the external
stream. The implementation of the `fibs_action` method is where this population
happens.

Finally, the definition of `mkFibsRules` now implements a `"fibs"` rule:

```bluespec
mkFibsRules :: FibsIfc -> FibsRulesIfc -> Rules
mkFibsRules ifc ifcRules =
  rules
    "even": when ifc.even_guard ==>
      ifcRules.even_action ifc.even_arg0

    "odd": when ifc.odd_guard ==>
      ifcRules.odd_action ifc.odd_arg0

    "fibs": when Prelude.True ==>
      action
        fibsVal <- ifcRules.fibs_action
        ifc.fibs fibsVal
```

This new rule produces a external value using `ifcRules.fibs_action` and then
feeds it to `ifc.fibs` as an input.

### Simulating Bluespec code involving external streams

Let's update our example from the "Simulating Bluespec code" section to
accommodate `fibs` becoming an external stream. Here is an example of what a
`Top.bs` application might look like:

```bluespec
package Top where

import Vector

import Fibs
import FibsTypes

mkTop :: Module Empty
mkTop =
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

    fibsMod <- mkFibs

    addFibsRules fibsMod $
      interface FibsRulesIfc
        even_action x =
          $display "Even Fibonacci number: %0d" x

        odd_action x =
          $display "Odd  Fibonacci number: %0d" x

        fibs_action = return (s0_get 0)

    rules
      "step": when True ==>
        action
          select s0 s0_idx := s0_gen
          s0_idx := (s0_idx + 1) % 2
```

Note that all of the code involving registers and the `"step"` rule now lives
in `Top.bs`, as it is now the responsibility of the application to produce the
values for the `fibs` stream. There is also now an implementation of the
`fibs_action` method that returns the most recently computed Fibonacci number
on each clock cycle.

### Generating Verilog code involving external streams

Compiling the new version of `mkFibs` to Verilog produces a `mkFibs.v` file
that looks like the following:

```verilog
module mkFibs(CLK,
	      RST_N,

	      even_guard,

	      even_arg0,

	      odd_guard,

	      odd_arg0,

	      fibs);
  input  CLK;
  input  RST_N;

  // value method even_guard
  output even_guard;

  // value method even_arg0
  output [31 : 0] even_arg0;

  // value method odd_guard
  output odd_guard;

  // value method odd_arg0
  output [31 : 0] odd_arg0;

  // action method fibs
  input  [31 : 0] fibs;

  ...

endmodule  // mkFibs
```

As expected, the only notable change is that there is now a `fibs` input.

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
