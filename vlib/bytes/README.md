# bytes

`bytes` provides manipulation helpers for raw byte arrays (`[]u8`). All searches,
offsets, separators, and cutsets operate on bytes. NUL bytes and invalid UTF-8 are
preserved. Functions that return arrays make independent copies of their results.

`to_lower` and `to_upper` convert ASCII letters only. `trim_space` removes the ASCII
space, tab, newline, carriage return, form feed, and vertical tab bytes. These
functions do not perform Unicode case conversion or Unicode whitespace trimming.

```v
import bytes

assert bytes.contains([u8(1), 2, 3], [u8(2), 3])
assert bytes.join([[u8(1)], [u8(2)]], [u8(0)]) == [u8(1), 0, 2]
assert bytes.split_n([u8(1), 2, 3], []u8{}, 2) == [[u8(1)], [u8(2), 3]]
assert bytes.replace([u8(1), 2], []u8{}, [u8(0)], -1) == [u8(0), 1, 0, 2, 0]
```

## Empty separators and counts

- `count(s, [])` counts every byte boundary, including both ends: `s.len + 1`.
- `replace(s, [], replacement, n)` inserts at those same boundaries. A negative `n`
  replaces every match, zero replaces none, and a positive `n` limits the replacements.
- `split(s, [])` returns individual bytes. Splitting empty input with an empty
  separator returns no parts. With a nonempty separator, it returns one empty part.
- `split_n` returns no parts for `n == 0` and all parts for `n < 0`. For positive `n`,
  it returns at most `n` parts and keeps the unsplit remainder in the last part.
- `repeat` returns an empty array for a nonpositive count or empty input.

Empty-separator behavior is byte-oriented even for UTF-8 text. For example, the
two bytes encoding `é` count as two bytes, not one character.

`count` panics if its result exceeds the maximum `int`. `join`, `repeat`, and `replace`
panic if their result length exceeds that limit, before allocating the result.
