## Description

`math` provides commonly used mathematical functions for
trigonometry, logarithms, etc.

For floating point values, `math.abs` maps both signs of zero to positive zero.
`math.expm1` preserves its input's zero sign, so `math.expm1(-0.0)` returns negative zero.

`modf` preserves the input sign on both its integer and fractional results, including zero.
`log_gamma` returns the logarithm of the absolute value of Gamma; either positive or negative
infinity as input returns positive infinity.

The trigonometric functions `sin`, `cos`, `sincos`, `tan` and `cot` reduce large finite angles
with multiword arithmetic before evaluation, including inputs near a multiple of pi/2 and
values up to `math.max_f64`. Infinite angles produce NaN.

`lgamma` aliases `log_gamma`. `remainder(x, y)` uses the IEEE nearest-integer,
ties-to-even remainder and preserves the dividend's sign on zero. `fma(x, y, z)` computes
one rounded multiply-add, including products whose intermediate value would overflow.
The C backend uses its math library for both operations. Other backends use portable
remainder and a two-round multiply-add fallback. `floor_mod(x, y)` instead follows the
divisor's sign, including zero. `to_degrees` and `to_radians` convert angle units.
`is_power_of_two` and `next_power_of_two` operate on `u64`; the latter returns zero when the
next power cannot fit. `classify` distinguishes NaN, infinite, normal, subnormal and zero.
