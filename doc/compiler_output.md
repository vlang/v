# Compiler output files

With the C or FastC backend, `./v -b c -o app source.v` retains generated C as `app.c`.
For a Windows target, the executable is `app.exe`; its suffix does not change the C filename.
An explicit `.exe` output name is respected: `-o app.exe` retains C as `app.exe.c`.
An output name ending in `.c` generates only C and does not invoke the C compiler.

In constant fixed arrays, shorthand enum values resolve against the array element type,
including aliases of enum types. Other enums with the same member name do not affect the value.

The C backend uses pointer-sized option/result status fields and sum-type tags when needed to
fill padding before pointer-bearing payloads. This keeps pointers aligned for conservative GC
scanning when optimized C code copies these values. Pointer-free options and sums keep their
smaller headers; sum types with an ownership flag already fill that padding. Packed payloads
keep their existing layout. This does not remove padding from ordinary user-defined structs.
