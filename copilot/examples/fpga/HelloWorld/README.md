# Hello World Example on an FPGA Board

Copilot is a programming language for hard real-time systems.

This tutorial shows how to use Copilot to control LEDs from hardware switches.
The demonstration serves as an introduction of the complete toolchain needed to
integrate Copilot into hardware projects. The specification is verified via the
onboard LEDs on an Artix-7 XC7A35T FPGA, using AMD/Xilinx Vivado Design Suite.

This page lists all the necessary applications and auxiliary files required to
run Copilot onto an FPGA. Specifically, this page shows:

- How to install the necessary dependencies.
- How to create Copilot code that invokes the Bluespec backend.
- How to compile the Bluespec files into Verilog.
- How to run and test the design on an FPGA board.

## Table of Contents

- [Installation](#installation)
- [Example](#example)
  - [Hello World Code](#hello-world-code)
  - [Compiling Copilot into Bluespec](#compiling-copilot-into-bluespec)
  - [Compiling Bluespec to Verilog](#compiling-bluespec-to-verilog)
- [Running the Hello World Example on Vivado](#running-the-hello-world-example-on-vivado)
- [Acknowledgements](#acknowledgments)

# Installation
<sup>[(Back to top)](#table-of-contents)</sup>

## Vivado 2025.1

Vivado can be installed through terminal as suggested by the following process.
The steps provided are for a Vivado version that is ~50 GB less than the one
directly available on the AMD website. If issues arise, see the official
[Vivado
documentation](https://docs.amd.com/r/en-US/ug910-vivado-getting-started/Installing-the-Vivado-Design-Suite)
for an alternative installation method.

1. Create an account on the
[AMD](https://www.amd.com/en/registration/create-account.html) website, which
allows downloading the Vivado Design Suite.

2. Download the [Vivado
Edition 2025.1](https://www.xilinx.com/member/forms/download)
specific for your device and verify the information required by AMD.

3. Open the command terminal, navigate to the directory containing the
   downloaded Vivado installation file, and adjust the permissions of the file
   so that it can be executed:

   ```sh
   $ chmod a+x FPGAs_AdaptiveSoCs_Unified_SDI_2025.1_0530_0145_<OS>.bin
   ```
   Replace `<OS>` with your operating system.

4. Run the installation program:

   ```sh
   $ ./FPGAs_AdaptiveSoCs_Unified_SDI_2025.1_0530_0145_<OS>.bin
   ```

5. An installation window will pop up. Follow the installation prompt. When
   the window *Select Product to Install* is shown, select the product
   *Vivado*. Then, select the *Vivado ML Standard* edition from the two
   options. In the *Vivado ML Standard* window, in the subcategory *Devices*,
   select the FPGA board that you're working with (in our case the *7 Series*).
   Hit *install* when prompted. The download should be ~20 GB to 50 GB.

6. Ensure that the directory where you choose to install Vivado has
   read/write permissions. If the candidate installation
   directory is highlighted red, it means it has the wrong permissions and/or
   owner. If that happens, change the owner into a different user, or try to
   run the binary installer as sudo (root) by running the command:
   ```sh
   $ sudo ./FPGAs_AdaptiveSoCs_Unified_SDI_2025.1_0530_0145_<OS>.bin
   ```

   The rest of the instructions assume you have installed Vivado under
   `/tools`. Adjust paths accordingly as needed.

7. Install the drivers with:

   ```sh
   $ source /tools/Xilinx/2025.1/Vivado/.settings64-Vivado.sh
   $ sudo /tools/Xilinx/2025.1/data/xicom/cable_drivers/<OS>/install_script/install_drivers/install_drivers
   ```

   The terminal should show a message indicating that the drivers were
   installed successfully. A sample output is included below. Note that the
   installation path may be different on your computer.

   <details><summary>Terminal Output (click to expand)</summary>

   ```
   INFO: Installing cable drivers.
   INFO: Script name = /tools/Xilinx/2025.1/data/xicom/cable_drivers/lin64/install_script/install_drivers/install_drivers
   INFO: HostName = iron-B650M-C-V2-Y1
   INFO: RDI_BINROOT= /tools/Xilinx/2025.1/data/xicom/cable_drivers/lin64/install_script/install_drivers
   INFO: Current working dir = /home/iron/Downloads
   INFO: Kernel version = 6.8.0-60-generic.
   INFO: Arch = x86_64.
   Successfully installed Digilent Cable Drivers
   --File /etc/udev/rules.d/52-xilinx-ftdi-usb.rules does not exist.
   --File version of /etc/udev/rules.d/52-xilinx-ftdi-usb.rules = 0000.
   --Updating rules file.
   --File /etc/udev/rules.d/52-xilinx-pcusb.rules does not exist.
   --File version of /etc/udev/rules.d/52-xilinx-pcusb.rules = 0000.
   --Updating rules file.

   INFO: Digilent Return code = 0
   INFO: Xilinx Return code = 0
   INFO: Xilinx FTDI Return code = 0
   INFO: Return code = 0
   INFO: Driver installation successful.
   CRITICAL WARNING: Cable(s) on the system must be unplugged then plugged back in order for the driver scripts to update the cables.
   ```
   </details>

8. Verify that Vivado has been installed on your device by running the
   following command and checking that you see version information
   being printed:

   ```sh
   $ vivado -version
   vivado v2025.1 (64-bit)
   Tool Version Limit: 2025.05
   SW Build 6140274 on Wed May 21 22:58:25 MDT 2025
   IP Build 6138677 on Thu May 22 03:10:11 MDT 2025
   SharedData Build 6139179 on Tue May 20 17:58:58 MDT 2025
   Copyright 1986-2022 Xilinx, Inc. All Rights Reserved.
   Copyright 2022-2025 Advanced Micro Devices, Inc. All Rights Reserved.
   ```

# Example

## Hello World Code
<sup>[(Back to top)](#table-of-contents)</sup>

This example shows how Copilot can be used to control LEDs using switches. We
use four switches and four LEDs available on the FPGA board.

The main Copilot specification is created in a file `HelloWorld.hs`. The final
contents of that file  is available in the [Copilot
repository](https://github.com/Copilot-Language/copilot/tree/master/copilot/examples/fpga/HelloWorld/HelloWorld.hs).
The same directory contains any auxiliary files needed to run the code onto an
FPGA board.

The first definition in a Copilot specification is a module declaration, which
should always refer to the module as the `Main` module:

```haskell
module Main where
```

Next, we import the main definitions of the Copilot language, as well as the
Bluespec backend:

```haskell
import Copilot.Compile.Bluespec
import Language.Copilot
```

Copilot normally represents data as infinite successions of values or
*streams*. Streams whose values are provided by input devices are considered
external inputs for this specification or *externs*. Because switches are
inputs to the Copilot logic, we define the current value of the state of the
switches as an extern carrying values of typed 8-bit unsigned integer, or
`Word8`:

```haskell
sw :: Stream Word8
sw = extern "sw" Nothing
```

The four least-significant bits of the current value of `sw` represent the
current state of each of the four switches on the board, respectively.

Next, we define the current value of the four LEDs in a similar way. To make
the state of the LEDs match that of the switches, we state that their values
are the same:

```haskell
leds :: Stream Word8
leds = sw
```

To connect the current values of the LEDs to the actual LEDs on the board, we
must communicate the current values as outputs of the specification which we do
with a *trigger*:

```haskell
spec = do
  trigger "leds" true [arg leds]
```

In the above trigger definition, `"leds"` is the name of the output, whereas
`leds` (without double quotes) is a Copilot stream that, at each point in time,
will contain the current values that the LEDs should have.

We instruct Copilot to compile the file and generate Bluespec when the Copilot
spec is executed:

```haskell
main = do
  spec' <- reify spec
  compile "HelloWorld" spec'
```

## Compiling Copilot into Bluespec
<sup>[(Back to top)](#table-of-contents)</sup>

Copilot and Bluespec compiler must be installed on the device (see
[Copilot](https://github.com/Copilot-Language/copilot) and
[Bluespec](https://github.com/Copilot-Language/copilot-bluespec) for
installation instructions).

In the terminal, navigate to the directory that has the `HelloWorld.hs`
example. Compile the Copilot specification into Bluespec by running:

```shell
$ runhaskell HelloWorld.hs
```

This command generates three Bluespec files in the same directory:
`HelloWorld.bs`, `HelloWorldIfc.bs` and `HelloWorldTypes.bs`. These files make
up the internal logic and interface in Verilog. For reference, the expected
content of these files is included below. The specific content may change
slightly across versions of Copilot.

<details>
<summary>Contents of generated Bluespec files (click to expand)</summary>

**`HelloWorld.bs`**

```bluespec
package HelloWorld where {
import FloatingPoint;

import Vector;

import HelloWorldTypes;

import HelloWorldIfc;

import BluespecFP;

mkHelloWorld :: Prelude.Module HelloWorldIfc ->
                Prelude.Module Prelude.Empty;
mkHelloWorld ifcMod =
    module {
      ifc <- ifcMod;
      let { leds_0_guard :: Prelude.Bool;
            leds_0_guard =  Prelude.True;
            leds_0_arg0 :: Prelude.UInt 8;
            leds_0_arg0 =  ifc.sw._read; };
      rules {

        "leds_0":  when leds_0_guard   ==> ifc.leds leds_0_arg0
      }
    };
}
```

**`HelloWorldIfc.bs`**

```bluespec
package HelloWorldIfc where {
import FloatingPoint;

import Vector;

import HelloWorldTypes;

interface HelloWorldIfc = {
    leds :: Prelude.UInt 8 -> Prelude.Action;
    sw :: Prelude.Reg (Prelude.UInt 8)
}
}
```

**`HelloWorldTypes.bs`**

```bluespec
package HelloWorldTypes where {
import FloatingPoint;

import Vector
}
```
</details>

In the same directory, create a new file named `Top.bs`. This file serves as
top-level Bluespec module, connects the three generated files together and
contains the interface definitions. This file will later be compiled into
Verilog and deployed onto the FPGA.

We define both the `helloWorldIfc` and `mkTop` modules. When `HelloWorld.bs` is
compiled, it makes `leds` an output, which translates to an *action* method in
Bluespec. Since an *action* method updates state rather than directly driving a
signal, we need a way to hold its value, so we create the register `led_reg`,
which stores the LEDs' value. The *type* of `led_reg` must match the *type* of
`leds` in `HelloWorldIfc.bs`.

```bluespec
package Top where

  import HelloWorld
  import HelloWorldIfc
  import HelloWorldTypes

  helloWorldIfc :: Prelude.Module HelloWorldIfc
  helloWorldIfc =
    module
      -- Register that connects the LEDs to values produced by Copilot
      led_reg :: Prelude.Reg (Prelude.UInt 8) <- mkReg 0
      interface
        leds reg = action
          led_reg := reg

  mkTop :: Prelude.Module Empty
  mkTop = mkHelloWorld helloWorldIfc
```
## Compiling Bluespec to Verilog
<sup>[(Back to top)](#table-of-contents)</sup>

We use the Bluespec compiler to generate Verilog output using the top-level
module. `mkTop` is a function in the `Top.bs` file that instantiates the
top-level module of the design. In the terminal, we run the following command:

```shell
$ bsc -verilog -g mkTop -u Top.bs
```

This step generates the Verilog code that can be compiled to run onto the FPGA
board. We can ignore the warnings shown in terminal output, but we must ensure
that the file `mkTop.v` is successfully created in the current directory.

To program the code onto the FPGA, the ports declarations and assignments need
modifications. In the `Top.bs` file, the module that had the register and
interface for the LEDs and switches is defined in `helloWorldIfc`. Due to this,
when compiling into Verilog, the Top level module does not have `sw` and `leds`
defined in its port declaration. This has to be modified manually.

The following `mkTop.v` lines list the module ports in the `Top.bs` file. These
are the external signals that connect to this module:

```verilog
module mkTop(CLK,
             RST_N);
  input  CLK;
  input  RST_N;
```

We modify `mkTop` and add the input port `sw` and output port `leds` inside the
module port because these signals will externally connect to the FPGA. The FPGA
is limited to 4 LEDs and 4 switches. Alongside the existing definitions, we
need to *assign* the value of `ifc_led_reg$D_IN`, which is an internal wire
created by the Verilog code to the output `leds`. We *assign* the output `leds`
to the register. This register was defined in our `Top.bs` and appears as an
internal register `ifc_led_reg` and its current contents appears as the value
in `leds`. The value is updated on every rising edge of the `CLK`.

For reference, the complete contents of the modified `mkTop.v` is included
below.

<details>
<summary>Contents of modified `mkTop.v` file (click to expand)</summary>

```verilog
//
// Generated by Bluespec Compiler, version 2025.01.1 (build 65e3a87a)
//
// Further modified by Sukhman Kahlon.
//
// On Thu Aug 14 14:52:29 PDT 2025
//
//
// Ports:
// Name                         I/O  size props
// CLK                            I     1 clock
// RST_N                          I     1 reset
//
// No combinational paths from inputs to outputs
//
//

`ifdef BSV_ASSIGNMENT_DELAY
`else
  `define BSV_ASSIGNMENT_DELAY
`endif

`ifdef BSV_POSITIVE_RESET
  `define BSV_RESET_VALUE 1'b1
  `define BSV_RESET_EDGE posedge
`else
  `define BSV_RESET_VALUE 1'b0
  `define BSV_RESET_EDGE negedge
`endif

module mkTop(CLK,
             RST_N,
             sw,
             leds);
  input  CLK;
  input  RST_N;
  input [3:0] sw;
  output [3:0] leds;

  assign leds[3:0] = ifc_led_reg[3:0];

  // register ifc_led_reg
  reg [7 : 0] ifc_led_reg;
  wire [7 : 0] ifc_led_reg$D_IN;
  wire ifc_led_reg$EN;

  // register ifc_led_reg
  assign ifc_led_reg$D_IN = sw[3:0];
  assign ifc_led_reg$EN = 1'd1 ;

  // handling of inlined registers

  always@(posedge CLK)
  begin
    if (RST_N == `BSV_RESET_VALUE)
      begin
        ifc_led_reg <= `BSV_ASSIGNMENT_DELAY 8'd0;
      end
    else
      begin
        if (ifc_led_reg$EN)
          ifc_led_reg <= `BSV_ASSIGNMENT_DELAY ifc_led_reg$D_IN;
      end
  end

  // synopsys translate_off
  `ifdef BSV_NO_INITIAL_BLOCKS
  `else // not BSV_NO_INITIAL_BLOCKS
  initial
  begin
    ifc_led_reg = 8'hAA;
  end
  `endif // BSV_NO_INITIAL_BLOCKS
  // synopsys translate_on
endmodule  // mkTop
```
</details>

This code will be copied to Vivado and replace the default empty module created
by Vivado.

To interface between the FPGA board and the code, a constraints file needs to
be created and is dependent on the type of FPGA board used. Please refer to the
reference manual for your specific FPGA board, to assign pins for LEDs
accordingly. Digilent offers I/O pins for the respective boards on the
[Digilent Github Repository](https://github.com/Digilent/digilent-xdc).

Ensure the name of the external pins after `get_ports` matches the inputs /
outputs created in the port declaration. In this example, `CLK`, `RST_N`,
`leds`, and `sw` *must* be assigned. For reference, here is the constraints
file assigning LED 4-7 for the Arty 35T CSG324-1 to the output `leds`, and
setting the switches and period accordingly.

<details>
<summary>Contents of constraints file `new_constraints.xdc` (click to expand)</summary>

```xdc
set_property -dict { PACKAGE_PIN E3    IOSTANDARD LVCMOS33 } [get_ports CLK]
create_clock -add -name sys_clk_pin -period 10.00 -waveform {0 5} [get_ports CLK]

set_property PACKAGE_PIN C2 [get_ports RST_N]
set_property IOSTANDARD LVCMOS33 [get_ports RST_N]

# Switches
set_property -dict { PACKAGE_PIN A8    IOSTANDARD LVCMOS33 } [get_ports { sw[0] }]; #IO_L12N_T1_MRCC_16 Sch=sw[0]
set_property -dict { PACKAGE_PIN C11   IOSTANDARD LVCMOS33 } [get_ports { sw[1] }]; #IO_L13P_T2_MRCC_16 Sch=sw[1]
set_property -dict { PACKAGE_PIN C10   IOSTANDARD LVCMOS33 } [get_ports { sw[2] }]; #IO_L13N_T2_MRCC_16 Sch=sw[2]
set_property -dict { PACKAGE_PIN A10   IOSTANDARD LVCMOS33 } [get_ports { sw[3] }]; #IO_L14P_T2_SRCC_16 Sch=sw[3]

# LEDs
set_property -dict { PACKAGE_PIN H5    IOSTANDARD LVCMOS33 } [get_ports { leds[0] }]; #IO_L24N_T3_35 Sch=led[4]
set_property -dict { PACKAGE_PIN J5    IOSTANDARD LVCMOS33 } [get_ports { leds[1] }]; #IO_25_35 Sch=led[5]
set_property -dict { PACKAGE_PIN T9    IOSTANDARD LVCMOS33 } [get_ports { leds[2] }]; #IO_L24P_T3_A01_D17_14 Sch=led[6]
set_property -dict { PACKAGE_PIN T10   IOSTANDARD LVCMOS33 } [get_ports { leds[3] }]; #IO_L24N_T3_A00_D16_14 Sch=led[7]
```
</details>

# Running the Hello World Example on Vivado
<sup>[(Back to top)](#table-of-contents)</sup>

Once installed, open the Vivado Design Suite.

1. Click *Create Project* > *Next* > *Project Name:* `HelloWorld` and choose
the desired location.

2. Select *Create Project Subdirectory* > *Next* > *RTL Project* > *Next*.

3. Select *Do not specify sources at this time* > *Next*.

4. Search for the board / chip. In this example, the board used is
`xc7a35tcsg324-1`. The project should be created and initialized.

6. Once the Vivado workspace opens, under the *Sources* tab, select *+* > *Add
or create design sources* > *Next* > *Create File*.

7. Introduce the following values:
   - *File Type*: `Verilog`
   - *File Name*: `mkTop`
   - *File Location*: `<Local to Project>`

8. Under the *Sources* tab, select *+* > *Add or create constraints* > *Next* >
   *Create File*.

10. Introduce the following values:
    - *File Type*: `XDC`
    - *File Name*: `new_constraints`
    - *File Location*: `<Local to Project>`

11. From the *Sources* drop down, select *Design Sources* and open the
created design file `mkTop.v`.

12. Copy the contents of the Verilog code `mkTop.v` as previously modified.

13. From the *Sources* drop down, select *Constraints* and select the created
constraints file `new_constraints.xdc`.

14. Copy and modify the constraints file according to the specific FPGA board
that aligns with the project.

15. To run and compile the code, on the left-hand bar labeled *Flow Navigator*,
under *Project Manager* > *Program and Debug*, Select *Generate Bitstream* to
compile the code.

16. Once bitsream generation is completed. Navigate under *Hardware Manager* >
*Open Target* > *Plug in your FPGA board via USB* > *Auto Connect*  > *Program
Device*.

The LEDs on your FPGA board should now display the LEDs counting up in
correspondence to its binary value.

# Acknowledgements
<sup>[(Back to top)](#table-of-contents)</sup>

This tutorial has been created by Sukhman Kahlon.

Special thanks to the following individuals and teams for their guidance and
support throughout this project:

Pavlo Vlastos – for providing clear and helpful instructions on setting up
Vivado.

Ryan Scott and the Bluespec team – for their assistance with `copilot-bluespec`
and Bluespec.

Ivan Perez – for his support with Copilot and set up for this example.

Kaveh Zare - for sampling the tutorial and providing feedback.
