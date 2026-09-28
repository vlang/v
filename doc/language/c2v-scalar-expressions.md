# Scalar expressions in translated C

In `@[translated]` files, boolean values can index arrays and pointers. Boolean,
character, and enum values can offset pointers in arithmetic and compound assignments.
Subtracting two pointers produces a pointer-width `isize` difference.
Mixed boolean and numeric conditional branches
retain the numeric branch type. Narrow integer shifts use C's minimum 32-bit
operand width. Shifts of translated `int` values use C's 32-bit `int` width.

Translated scalar return statements also use C conversions, including float to
integer and negative integer sentinels returned as unsigned values. Unary numeric
and bitwise operations, and shifts, accept enum, character, and boolean operands
through C's integral promotion rules, including boolean shifts.
Postfix `++` and `--` accept translated enum, character, and boolean scalars.
Numeric conversions to boolean destinations produce `false` for zero and `true` for nonzero
values, including fractional values. Assignments, arguments, returns, casts, and compound updates
apply this normalization even on targets that store booleans as unsigned bytes.
Static translated `int` globals narrow their initial values to C's 32-bit `int` width.
This conversion also applies to `int` elements in static fixed-array globals, including nested
arrays and aliases, and constant struct fields in `@[cinit]` globals. Numeric boolean fields
in these static structs use the same nonzero normalization. Wider destination types and ordinary
V files keep their usual conversions.
Mixed numeric arithmetic and conditional branches use C's usual arithmetic
conversions when determining the expression type.

Negative integer sentinels can be converted to unsigned destinations and compared
using C's integer conversion rules. Function addresses supplied through `voidptr`
can be assigned to callback variables. Callbacks can be compared with integer
sentinels such as `0` and `-1`. These rules apply only to translated files;
ordinary V source keeps its existing checks and mixed-sign comparison behavior.
Mixed-sign comparisons use C's promoted operand widths, including the backing
widths of explicitly backed enums. Wide backed enums retain their backing type
through arithmetic and bitwise operations.
Mixed-width arithmetic is evaluated in the same C common type before its result
is used by an enclosing expression or returned.

Compound shifts use the same promoted operand width as ordinary shifts, including
boolean and enum operands in scalar, array, and pointer lvalues. Logical right shifts (`>>>`)
infer their unsigned result type after integral promotion. With `-check-overflow`, integer compound
arithmetic checks the promoted common type before converting the result back to its destination.
Checked postfix updates of translated `int` values use the same 32-bit bounds, including aliases
and indexed elements. Explicitly wider integers and ordinary V files retain their usual bounds.
