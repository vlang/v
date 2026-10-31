# maphash

`maphash` provides hash functions for use in hash tables, similar to Go's
`hash/maphash`. It is not cryptographically secure.

```v
import hash.maphash

seed := maphash.make_seed()
mut h := maphash.new_hasher(seed)
h.write_string('hello')
println(h.sum64())
```
