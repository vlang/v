Note: To more deep study see https://en.wikibooks.org/wiki/X86_Assembly

# SSE and MMX Extensions

This document provides an overview of the SSE and MMX extensions used in the project.

## Table of Contents

- [Introduction](#introduction)
- [SSE Extensions](#sse-extensions)
- [MMX Extensions](#mmx-extensions)
- [Usage](#usage)

## Introduction

SSE (Streaming SIMD Extensions) and MMX (MultiMedia eXtensions) are instruction sets used to
enhance the performance of multimedia and signal processing applications.

## SSE Extensions

SSE extensions provide a set of instructions that can handle multiple data with a single
instruction, improving the performance of applications that require heavy mathematical
computations.

from: [wikibooks](https://en.wikibooks.org/wiki/X86_Assembly/SSE#SSE_Instruction_Set)
There are literally hundreds of SSE instructions, some of which are capable of much more than
simple SIMD arithmetic. For more in-depth references take a look at the resources chapter of this
book.

You may notice that many floating point SSE instructions end with something like PS or SD. These
suffixes differentiate between different versions of the operation. The first letter describes
whether the instruction should be Packed or Scalar. Packed operations are applied to every member
of the register, while scalar operations are applied to only the first value. For example, in
pseudo-code, a packed add would be executed as:

```
v1[0] = v1[0] + v2[0]
v1[1] = v1[1] + v2[1]
v1[2] = v1[2] + v2[2]
v1[3] = v1[3] + v2[3]
```

While a scalar add would only be:

```
v1[0] = v1[0] + v2[0]
```

The second letter refers to the data size: either Single or Double. This simply tells the
processor whether to use the register as four 32-bit floats or two 64-bit doubles, respectively.

## MMX Extensions

MMX extensions are designed to accelerate multimedia and communication applications by providing
instructions that can process multiple data elements in parallel.

## Usage

To use these extensions in your project, ensure that your compiler supports them and that you have
enabled the appropriate flags.
On Linux, you can run the command `lscpu`

Note: the examples here will compile, but not run on CPU architectures != amd64, like ARM or RISCV .
