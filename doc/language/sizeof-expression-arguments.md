# Expression sizes in function calls

`sizeof(expression)` can be passed directly to V and C functions. It measures the
expression's storage, including the full size of a fixed array reached through a
pointer, just as it does outside a function argument. The expression is not evaluated.
