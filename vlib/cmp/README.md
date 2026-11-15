## Description

`cmp` provides generic comparison, range, and default-value helpers. Ordered helpers
accept numbers, strings, and types with suitable comparison operators. `coalesce`
requires equality comparison with the type's default value.

```v
import cmp

assert cmp.compare('apple', 'pear') == -1
assert cmp.less(2, 3)
assert cmp.min(2, 3) == 2
assert cmp.max(2, 3) == 3
assert cmp.clamp(12, 0, 10) == 10
assert cmp.coalesce('', 'fallback', 'later') == 'fallback'
assert cmp.coalesce[int]() == 0
```

## Floating point values

`compare` returns -1, 0, or 1. `compare` and `less` place NaN before every non-NaN
value, consider two NaNs equal, and consider negative and positive zero equal.
This gives floating point values the same ordering as Go's `cmp.Compare` and `cmp.Less`.

`min` and `max` propagate NaN from either argument, matching V's `math.min` and
`math.max`. Equal non-NaN arguments select the second value, including its zero sign.

`clamp` requires `lo <= hi` and non-NaN bounds. It does not validate these preconditions.
Equal bounds are allowed. A NaN input is returned unchanged, matching `math.clamp`.

## Choosing a non-default value

`coalesce` returns the first argument unequal to `T{}`. If none qualifies, including
when no arguments are supplied, it returns `T{}`. Specify the type explicitly for
an empty call, such as `cmp.coalesce[string]()`.

For numbers the default is zero; for strings it is `''`; for booleans it is `false`.
For structs, `T{}` includes declared field defaults. Floating point NaN qualifies as
non-default. Both signs of floating point zero are skipped, and an all-zero call
returns positive zero.
