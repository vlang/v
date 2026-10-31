# fnv

`fnv` implements FNV-1 and FNV-1a hash functions, similar to Go's `hash/fnv`.

```v
import hash.fnv

assert fnv.sum32a('a'.bytes()) == u32(0xe40c292c)
```
