## Description

`math` provides commonly used mathematical functions for
trigonometry, logarithms, etc.

`factorial(n)` returns `n!` for nonnegative integers and uses `gamma(n + 1)` for other
values. When the result overflows `f64`, it returns positive infinity, including for
`n >= 171` and positive infinity. For nonnegative inputs, use `is_inf(result, 1)`
to detect this overflow; `factorial(170)` remains finite. The gamma extension also
returns positive infinity at the pole `n = -1`, so infinity alone does not indicate
overflow for arbitrary inputs.

For floating point values, `math.abs` maps both signs of zero to positive zero.
`math.expm1` preserves its input's zero sign, so `math.expm1(-0.0)` returns negative zero.
`math.min` and `math.max` return NaN when either floating point argument is NaN.
For equal non-NaN values, including opposite signs of zero, both return their second argument.

`modf` preserves the input sign on both its integer and fractional results, including zero.
`log_gamma` returns the logarithm of the absolute value of Gamma; either positive or negative
infinity as input returns positive infinity.
`log2(x)` returns an exact integer exponent for every representable positive power of two,
including subnormal inputs.
`log1p(x)` returns positive infinity for positive infinity, negative infinity for `-1`,
and NaN for values below `-1`.
`logf(x)` provides the natural logarithm for `f32` inputs, including in pure V builds.
Its special cases are the same as for `log`.
`sqrt(x)` rounds its result to the nearest `f64`, including in pure V builds and for
subnormal inputs. It preserves signed zero and returns NaN for negative inputs.

The trigonometric functions `sin`, `cos`, `sincos`, `tan` and `cot` reduce large finite angles
with multiword arithmetic before evaluation, including inputs near a multiple of pi/2 and
values up to `math.max_f64`. Infinite angles produce NaN.

`lgamma` aliases `log_gamma`. `remainder(x, y)` uses the IEEE nearest-integer,
ties-to-even remainder and preserves the dividend's sign on zero. On the C backend,
`fma(x, y, z)` computes one rounded multiply-add, including intermediate product overflow.
The C backend uses its math library for both operations. Other backends and pure V builds
that exclude `math/*.c.v` use portable remainder and a two-round multiply-add fallback.
`floor_mod(x, y)` instead follows the divisor's sign, including zero.
`to_degrees` and `to_radians` convert angle units.
`is_power_of_two` and `next_power_of_two` operate on `u64`; the latter returns zero when the
next power cannot fit. `classify` distinguishes NaN, infinite, normal, subnormal and zero.

To run the pure V math tests, use `v -d pure_v_math -exclude '@vlib/math/*.c.v' test vlib/math/`.
The define selects the portable fallback's test expectations; normal C builds also test
the math library's single-round `fma` behavior.

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
