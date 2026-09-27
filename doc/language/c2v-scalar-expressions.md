# Scalar expressions in translated C

In `@[translated]` files, boolean values can index arrays and pointers or offset
pointers in compound assignments. Mixed boolean and numeric conditional branches
retain the numeric branch type. Narrow integer shifts use C's minimum 32-bit
operand width.

Translated scalar return statements also use C conversions, including float to
integer and negative integer sentinels returned as unsigned values. Unary numeric
and bitwise operations, and shifts, accept enum, character, and boolean operands
through C's integral promotion rules.

Negative integer sentinels can be converted to unsigned destinations and compared
using C's integer conversion rules. Function addresses supplied through `voidptr`
can be assigned to callback variables. Callbacks can be compared with integer
sentinels such as `0` and `-1`. These rules apply only to translated files;
ordinary V source keeps its existing checks and mixed-sign comparison behavior.
