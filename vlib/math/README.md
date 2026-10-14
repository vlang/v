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

`wrapping(value)` and `saturating(value)` create integer arithmetic wrappers for
`i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, and `int`.
Their `+`, `-`, and `*` operators wrap modulo the integer width or clamp to its bounds,
respectively. `neg()` applies the corresponding negation policy.
For signed minimum divided by `-1`, wrapping returns the minimum and saturation returns
the maximum. Both `%` operators return zero for that pair without overflowing the backend's
division operation. Zero divisors follow ordinary integer division behavior.

`gcd(a, b)` and `lcm(a, b)` accept matching integer types from `i8` through `i64`,
`u8` through `u64`, and `int`. They compute with unsigned magnitudes, including signed minimum
values, and clamp an unrepresentable result to the type maximum.
`gcd_many(values)` computes the common divisor before applying that clamp; its empty result is zero.
`lcm_many(values)` returns one for an empty input and zero when any element is zero.
