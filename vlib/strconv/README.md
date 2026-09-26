## Description

`strconv` provides functions for converting strings to numbers and numbers to strings.

## Buffer formatting

`write_dec` and `write_dec_u` write a decimal integer into a caller-provided `[]u8`
buffer. They return the number of bytes written, or `-1` when the buffer is too small.

```v
import strconv

mut buf := []u8{len: 20}
n := strconv.write_dec(-12345, mut buf)
assert n == 6
assert buf[..n].bytestr() == '-12345'
```

## Thousands separators

`format_thousands` formats built-in integer and floating-point values with a
thousands separator. Pass a string for the integer separator, or a `Separator`
to configure both integer and decimal separators.

```v
import strconv

assert strconv.format_thousands(1234567, ' ') == '1 234 567'
assert strconv.format_thousands(1234567.89, strconv.Separator{
	integer: '.'
	decimal: ','
}) == '1.234.567,89'
```

The numeric type is checked at compile time; unsupported types produce a
compile-time error.
