## Description

`math` provides commonly used mathematical functions for
trigonometry, logarithms, etc.

For floating point values, `math.abs` maps both signs of zero to positive zero.
`math.expm1` preserves its input's zero sign, so `math.expm1(-0.0)` returns negative zero.
`math.min` and `math.max` return NaN when either floating point argument is NaN.
For equal non-NaN values, including opposite signs of zero, both return their second argument.

`modf` preserves the input sign on both its integer and fractional results, including zero.
`log_gamma` returns the logarithm of the absolute value of Gamma; either positive or negative
infinity as input returns positive infinity.

The trigonometric functions `sin`, `cos`, `sincos`, `tan` and `cot` reduce large finite angles
with multiword arithmetic before evaluation, including inputs near a multiple of pi/2 and
values up to `math.max_f64`. Infinite angles produce NaN.
