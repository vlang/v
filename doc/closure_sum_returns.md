# Closures returning sum types

Closures can return imported sum types and containers of them, such as `[]json2.Any`.
Their variables remain function values and can be called or assigned to other callback variables.

```v ignore
import json2

parts := fn (s string) []json2.Any {
    return s.split(',').map(json2.Any(it))
}
println(json2.encode(parts('a,b'))) // ["a","b"]
```
