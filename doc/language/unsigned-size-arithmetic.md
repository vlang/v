# Unsigned arithmetic expressions

Subtracting values from `sizeof` or `__offsetof` is an arithmetic expression.
Such unsigned expressions can be assigned to unsigned fields and variables or
passed to unsigned parameters. Negated variables and computed expressions also
use numeric conversion, including unsigned wraparound. Direct negative integer
literals remain invalid for unsigned destinations.
