## Description

`math.decimal` provides arbitrary-precision fixed-point decimal arithmetic.
It stores values as an integer coefficient plus a decimal scale, so addition,
subtraction, and multiplication stay exact.

A value keeps its scale, so `from_string('5.00')` prints `5.00`. `+` and `-`
take the larger scale of their operands, `*` adds the scales, and `round` and
`div` give exactly the requested number of places. An exact `/` keeps the
dividend's scale minus the divisor's where it can, so `10.00 / 4` is `2.50`.
Comparisons ignore the scale (`5.0 == 5.00`), and `normalize` removes trailing
zeros.

`round` and `div` round to a caller-chosen number of decimal places with a
`RoundingMode`: `half_up` (ties away from zero), `half_even` (banker's
rounding), `half_down`, `up`, `down`, `ceiling` or `floor`. `div_prec` is
`div` with `half_up`. The `/` operator uses `decimal.default_division_precision`
and panics on a zero divisor; use `div` where the divisor can be zero.

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

amount := decimal.from_string('5.00')!
assert amount.str() == '5.00'
assert amount.normalize().str() == '5'
assert amount == decimal.from_int(5)

vat := price * decimal.from_string('0.21')!
assert vat.str() == '4.1979'
assert vat.round(2, .half_up)!.str() == '4.20'

tie := decimal.from_string('0.125')!
assert tie.round(2, .half_even)!.str() == '0.12'
assert tie.round(2, .half_up)!.str() == '0.13'
```
