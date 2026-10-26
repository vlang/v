## Description

`math.decimal` provides arbitrary-precision fixed-point decimal arithmetic.
It stores values as an integer coefficient plus a decimal scale, so addition,
subtraction, and multiplication stay exact.

`round` and `div` round to a caller-chosen number of decimal places with a
`RoundingMode`: `half_up` (ties away from zero), `half_even` (banker's
rounding), `half_down`, `up`, `down`, `ceiling` or `floor`. `div_prec` is
`div` with `half_up`. The `/` operator uses `decimal.default_division_precision`
and panics on a zero divisor; use `div` where the divisor can be zero.

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
```
