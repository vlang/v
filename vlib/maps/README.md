## Description

`maps` is a module that provides utility functions to make working with maps easier.

`new_counter[T]()` creates a `Counter[T]` for builtin map key types.
`inc(key)`, `dec(key)`, and `add_many(values)` change integer counts; missing keys read as zero
through `get(key)`. Counts may be zero or negative, and `total()` sums all counts.
`most_common(n)` returns up to `n` key/count entries in descending count order without
changing the counter. A nonpositive limit returns an empty array.

`new_default_map[K, V](factory)` creates a `DefaultMap[K, V]`.
`get(key)` invokes the factory only when the key is absent and stores its result.
`get_opt(key)` checks without creating an entry. `set(key, value)` stores or replaces a value,
and `len()` counts stored keys. Returned values use ordinary V assignment semantics;
references inside them are not deep-cloned. To replace an array or other stored value,
retrieve it, modify the local value, and call `set` with the result.
