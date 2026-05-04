## Description

`math.decimal` provides arbitrary-precision fixed-point decimal arithmetic.
It stores values as an integer coefficient plus a decimal scale, so addition,
subtraction, and multiplication stay exact.

Division is exposed through `div_prec`, which rounds half up to a caller-chosen
number of decimal places. The `/` operator uses
`decimal.default_division_precision`.

## Example

```v
import math.decimal

price := decimal.from_string('19.99')!
fee := decimal.from_string('1.50')!
total := price + fee
assert total.str() == '21.49'

share := total.div_prec(decimal.from_int(3), 2)!
assert share.str() == '7.16'
```
