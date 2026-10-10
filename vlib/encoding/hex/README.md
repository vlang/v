## Description

`encoding.hex` converts between byte arrays and hexadecimal strings.

`hex.encode` emits lowercase digits by default. Set `uppercase: true` for uppercase digits
and `with_prefix: '0x'` to prepend a prefix. The prefix is also emitted for empty input:

```v
import encoding.hex

assert hex.encode([u8(0xab), 0xcd]) == 'abcd'
assert hex.encode([]u8{}, with_prefix: '0x') == '0x'
assert hex.encode([u8(0xab)], uppercase: true, with_prefix: '0X') == '0XAB'
```

`hex.decode` accepts either case and an optional `0x` or `0X` prefix. An odd number of
digits is decoded as if the first digit had a leading zero. Invalid digits return an error.
