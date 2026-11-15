# fnv

`fnv` implements FNV-1 and FNV-1a hash functions, similar to Go's `hash/fnv`.
The functions without an `a` suffix use FNV-1; those with an `a` suffix use FNV-1a.
Both variants return 32-bit or 64-bit unsigned integer digests. They are
non-cryptographic hashes, so they are unsuitable for authentication or password storage.

```v
import hash.fnv

assert fnv.sum32a('a'.bytes()) == u32(0xe40c292c)
```

For incremental hashing, start with the matching `new32`, `new32a`, `new64`, or
`new64a` value, then assign the state returned by each corresponding `write` function.
Empty chunks leave the state unchanged. Chunk boundaries do not affect the digest.

```v
import hash.fnv

mut state := fnv.new64a()
state = fnv.write64a(state, 'foo'.bytes())
state = fnv.write64a(state, 'bar'.bytes())
assert state == fnv.sum64a('foobar'.bytes())
```

The existing `hash.fnv1a` module also provides FNV-1a helpers, including string and
streaming APIs. Its `sum32` and `sum64` functions produce the same digests as this
module's `sum32a` and `sum64a`.
