# maphash

`maphash` provides a seeded 64-bit byte-stream hash for hash-table use. It is not
cryptographically secure and does not guarantee resistance to deliberate collision attacks.
Its API is inspired by Go's `hash/maphash`; hash values are not compatible with Go.

```v
import hash.maphash

seed := maphash.make_seed()
mut h := maphash.new_hasher(seed)
h.write_string('hello')
println(h.sum64())
```

## Seeds and state

Use a fresh `make_seed()` for each independent hash table and reuse that seed for
all keys in the table. Seeds are opaque, immutable values and can be shared between
threads. `make_seed` obtains bytes from the operating system, does not consume or
depend on the global `rand` generator, and may be called concurrently. It panics if
the operating system cannot provide random bytes.

`Seed{}` is a valid deterministic zero seed. A default `Hasher{}` behaves like
`new_hasher(Seed{})`; call `new_hasher(make_seed())` to select a randomized seed.
`reset` discards the input and keeps the original seed, including for a default hasher.
A hasher is mutable and must not be written or reset concurrently. Separate hashers
can safely use the same seed.

## Stream behavior

Only the seed and byte sequence determine the result. Splitting input across several
`write` or `write_string` calls produces the same hash as one concatenated write.
Strings are hashed as raw bytes, including NUL and invalid UTF-8. `write_u64` writes
exactly eight bytes in big-endian order. Empty writes have no effect.

`sum64` observes the current hash without changing it; further writes continue the
same stream. Callers hashing multiple fields should encode their boundaries when
needed: writing `ab` then `c` is equivalent to writing `a` then `bc`.

Hash values are not a persistent identifier or file format. The implementation may
change between versions, even for the same seed and input.
