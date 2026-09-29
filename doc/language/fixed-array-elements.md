# Fixed-array element evaluation

Fixed-array elements that require runtime temporary statements retain source order.
Ordinary calls emitted directly in a C initializer follow C's evaluation order.
An `unsafe` block inside an element keeps its own statements local without hiding
temporary values needed by the rest of the array. This applies to declarations
and assignments.
