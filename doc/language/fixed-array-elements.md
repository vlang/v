# Fixed-array element evaluation

Fixed-array literals evaluate their elements in source order. An `unsafe` block
inside an element keeps its own statements local without hiding temporary values
needed by the rest of the array. This applies to declarations and assignments.
