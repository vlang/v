## Purpose: tests data for the output of V's C code generator

## TLDR: `v run vlib/v/gen/c/coutput_test.v`

coutput_test.v is a *test runner*, that checks whether the generated C
source code matches *all* expectations, specified in *.c.must_have files,
in the folder vlib/v/gen/c/testdata/ .

Each `.c.must_have` file, *has* to have a corresponding `.vv` file.

Each `.c.must_have` file, consists of multiple lines. Each of these
lines, *should* be present *at least once* in the output, when the .vv
file is compiled with `-o -` .

Note: this test runner can also be used to verify and keep from regressing
*partial fixes* to V's checker, i.e. ones that make a program type check fine,
but that still lead to codegen problems. To do that, put the V code that the
checker can typecheck in a `.vv` file in this folder, then create a
corresponding `.c.must_have` file in this folder too. It should have a single
line in it: `// THE END.` . The V codegen will end every source that it
generates with that line, so its presence, effectively guarantees that cgen
succeeded/ended, even if the produced C code can not be compiled yet.
