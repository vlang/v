# bytes

`bytes` provides byte-slice (`[]u8`) manipulation helpers, similar to Go's
`bytes` package. V's `string` type already has many such helpers; this module
offers the same conveniences for raw byte slices.

```v
import bytes

assert bytes.contains([u8(1), 2, 3], [u8(2), 3])
assert bytes.join([[u8(1)], [u8(2)]], [u8(0)]) == [u8(1), 0, 2]
```
