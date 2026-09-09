# math.fractions

`math.fractions` provides exact rational arithmetic.

## Existing `i64` API

```v
import math.fractions

f := fractions.fraction(4, 8)
assert f.reduce().str() == '1/2'
```

## Generic backends

`fractions.Rational[T]` supports signed builtin integers and `math.big.Integer`.

```v
import math.big
import math.fractions

huge := big.integer_from_string('1000000000000000000000000000000')!
f := fractions.big_fraction(huge, big.integer_from_int(3))
g := fractions.rational(i32(5), i32(10))

assert f.reduce().str() == '1000000000000000000000000000000/3'
assert g.reduce().str() == '1/2'
```
