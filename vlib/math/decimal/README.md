## Description

`math.decimal` provides arbitrary-precision fixed-point decimal arithmetic.
It stores values as an integer coefficient plus a decimal scale, so addition,
subtraction, and multiplication stay exact.

`round` and `div` round to a caller-chosen number of decimal places with a
`RoundingMode`: `half_up` (ties away from zero), `half_even` (banker's
rounding), `half_down`, `up`, `down`, `ceiling` or `floor`. `div_prec` is
`div` with `half_up`. The `/` operator uses `decimal.default_division_precision`
and panics on a zero divisor; use `div` where the divisor can be zero.

`rem` returns the remainder of a division truncated towards zero, so it has the
sign of the dividend, like `%` on V integers. `floor_div` and `floor_mod` round
the quotient towards negative infinity, so the remainder has the sign of the
divisor, like `math.floor_mod`. `div_euclid` and `mod_euclid` give a remainder
that is never negative. All of them return an error on a zero divisor.

`sign` returns -1, 0 or 1, and `decimal.min` and `decimal.max` return the smaller
and the larger of two values. `from_minor_units` and `to_minor_units` convert
between a decimal and an integer count of `10^-precision` units: `19.99` is
`1999` units at precision 2. `to_minor_units` returns an error rather than drop
digits, so round first where needed.

`from_string` accepts an optional base-10 exponent, for example `1.5e-2` is `0.015`.
An exponent can have one leading `+` or `-` sign and any number of leading zeros.
Exponents must be between -100 and 100 inclusive. Larger magnitudes return an
`exponent out of range` error rather than changing the value or allocating large powers of ten.

## Example

```v
import math.decimal

price := decimal.from_string('19.99')!
fee := decimal.from_string('1.50')!
total := price + fee
assert total.str() == '21.49'

share := total.div_prec(decimal.from_int(3), 2)!
assert share.str() == '7.16'

vat := price * decimal.from_string('0.21')!
assert vat.str() == '4.1979'
assert vat.round(2, .half_up)!.str() == '4.2'

tie := decimal.from_string('0.125')!
assert tie.round(2, .half_even)!.str() == '0.12'
assert tie.round(2, .half_up)!.str() == '0.13'

angle := decimal.from_int(-30)
assert angle.rem(decimal.from_int(360))!.str() == '-30'
assert angle.floor_mod(decimal.from_int(360))!.str() == '330'
assert price.to_minor_units(2)! == 1999
```
