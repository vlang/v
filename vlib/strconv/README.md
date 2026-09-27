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
