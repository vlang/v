# path

`path` implements slash-separated path manipulation, similar to Go's `path`
package (as opposed to OS-specific paths handled by `os.filepath`).

```v
import path

assert path.clean('a//b/../c') == 'a/c'
assert path.join('a', 'b', 'c') == 'a/b/c'
assert path.base('a/b/c.txt') == 'c.txt'
```
