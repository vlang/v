# path

`path` implements lexical manipulation of slash-separated paths. Only `/` is a
separator, on every platform. Backslashes and drive letters have no special meaning
to the path manipulation functions. Use the path helpers in `os` for native filesystem paths.

```v
import path

assert path.clean('a//b/../c') == 'a/c'
assert path.join('a', 'b', 'c') == 'a/b/c'
assert path.base('a/b/c.txt') == 'c.txt'
assert path.base('////') == '/'
assert path.ext('.profile') == '.profile'
assert path.ext('a.txt/') == ''
assert path.match('docs/*.md', 'docs/readme.md')!
assert !path.match('*', 'docs/readme.md')!
```

## Lexical operations

`clean` collapses repeated slashes and `.` elements, resolves `..` against preceding
ordinary elements, and preserves unresolved leading `..` in relative paths. A rooted
path cannot ascend above `/`. The empty path cleans to `.`. These operations do not
inspect the filesystem or resolve symbolic links.

`join` ignores empty arguments and cleans the combined result. It returns an empty
string when every argument is empty. A leading slash in a later argument does not
discard earlier arguments: `join('a', '/b')` returns `a/b`.

`split` preserves the original spelling and splits immediately after the last slash.
`base` ignores trailing slashes, returns `.` for an empty path, and `/` for a path made
only of slashes. `dir` cleans the directory part. `ext` returns the suffix beginning at
the last dot in the final element, including a leading dot; a trailing slash yields no extension.

## Pattern matching

`match` checks the entire name without cleaning either argument first.

| Pattern | Meaning |
| --- | --- |
| `*` | Any sequence of bytes except `/`, including an empty sequence |
| `?` | One UTF-8 character other than `/` |
| `[abc]`, `[a-z]` | One character from a nonempty class or inclusive range |
| `[^abc]`, `[!abc]` | One character outside the class |
| `\c` | Literal `c`, including a pattern metacharacter |

Character classes use Unicode code points. Escape `-`, `]`, or `\` inside a class
with a backslash. A class can match `/` when its ranges or negation include it.
Each invalid UTF-8 byte in a name is treated as U+FFFD by `?` and classes; literal
pattern bytes still match exactly. Invalid UTF-8 in a class is a malformed pattern.
As in Go, `*` can stop at a continuation byte: `match('*??', '世')` succeeds even
though `match('??', '世')` does not.

Malformed patterns, including empty or unterminated classes and trailing escapes,
return an error even when an earlier part of the pattern fails to match the name.
Matching uses bounded iteration without recursive star expansion.

The lexical rules and pattern grammar follow the [Go path package](https://pkg.go.dev/path),
with `!` also accepted as a class-negation prefix.
