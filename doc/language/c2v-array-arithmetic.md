# Fixed-array arithmetic in translated C

In `@[translated]` files, adding an integer to a fixed array yields a pointer to the
selected element, as in C. Subtracting an integer moves the pointer back, and subtracting
compatible pointers and fixed arrays yields an element count.
Offsets can come from value `if` and `match` branches. Pointer subtraction
requires matching element types after fixed-array decay.
Fixed-array aliases with declared arithmetic operators keep their operator behavior.
Ordinary V files retain their array arithmetic restrictions.
