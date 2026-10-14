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

`lgamma` aliases `log_gamma`. `remainder(x, y)` uses the IEEE nearest-integer,
ties-to-even remainder and preserves the dividend's sign on zero. On the C backend,
`fma(x, y, z)` computes one rounded multiply-add, including intermediate product overflow.
The C backend uses its math library for both operations. Other backends use portable
remainder and a two-round multiply-add fallback. `floor_mod(x, y)` instead follows the
divisor's sign, including zero. `to_degrees` and `to_radians` convert angle units.
`is_power_of_two` and `next_power_of_two` operate on `u64`; the latter returns zero when the
next power cannot fit. `classify` distinguishes NaN, infinite, normal, subnormal and zero.

`wrapping(value)` and `saturating(value)` create integer arithmetic wrappers for
`i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, and `int`.
Their `+`, `-`, and `*` operators wrap modulo the integer width or clamp to its bounds,
respectively. `neg()` applies the corresponding negation policy.
For signed minimum divided by `-1`, wrapping returns the minimum and saturation returns
the maximum. Both `%` operators return zero for that pair without overflowing the backend's
division operation. Zero divisors follow ordinary integer division behavior.
