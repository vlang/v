## Description

`math.decimal` provides arbitrary-precision fixed-point decimal arithmetic.
It stores values as an integer coefficient plus a decimal scale, so addition,
subtraction, and multiplication stay exact.

Like Go's [shopspring/decimal](https://pkg.go.dev/github.com/shopspring/decimal),
constructors keep the supplied scale internally, while `str()` omits trailing
fractional zeros: `from_string('5.00')` has coefficient 500, scale 2, and string
`5`. Use `str_fixed(places)` for half-up rounding and exactly that many places,
for example `amount.str_fixed(2)!` returns `5.00`. Negative places return an error.
`+` and `-` keep the larger scale, `*` adds the scales, and `round` and `div`
store exactly the requested scale. Comparisons use numeric value regardless
of scale. `normalize()` removes trailing coefficient zeros and reduces the
stored scale without changing the value or its normal string representation.
V uses nonnegative scales and retains its existing exponent parsing limits.

`round` and `div` round to a caller-chosen number of decimal places with a
`RoundingMode`: `half_up` (ties away from zero), `half_even` (banker's
rounding), `half_down`, `up`, `down`, `ceiling` or `floor`. `div_prec` is
`div` with `half_up`. The `/` operator uses `decimal.default_division_precision`
(16 fractional places), independently of either operand's stored scale, and
panics on a zero divisor; use `div` where the divisor can be zero. Use `div_prec`
with more places when small values would otherwise round to zero.

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

`Decimal` implements the `json2` custom encoder and decoders, like `big.Integer`.
`json2.encode` writes a value as a JSON number in plain notation with its stored scale,
so `10.50` stays `10.50`. Consumers that read JSON numbers as binary floating point,
such as JavaScript's `JSON.parse`, can round such values. `json2.decode` accepts a
JSON number within the exponent range of `from_string`, or a JSON string with the
syntax of a JSON number such as `"10.50"`, and keeps its scale; other strings,
booleans and arrays return an error. Outside strict mode a nested `null` decodes to
zero; use a `?decimal.Decimal` field to tell a missing value apart from zero. As for
`big.Integer`, json2 writes a sum type variant as a tagged object of its fields, and
decodes a JSON object into those fields without validation.

## Example

```v
import math.decimal

price := decimal.from_string('19.99')!
fee := decimal.from_string('1.50')!
total := price + fee
assert total.str() == '21.49'

amount := decimal.from_string('5.00')!
assert amount.scale() == 2
assert amount.str() == '5'
assert amount.str_fixed(2)! == '5.00'
assert amount.normalize().scale() == 0
assert amount == decimal.from_int(5)

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
