# V3 source formatter

The V3 formatter formats source syntax without type checking.

## String literal spelling

Non-interpolated string literals retain their original source spelling for
everything except the quote delimiter: escape sequences and the `r`, `c`, or `js`
prefix are copied byte for byte. For example, `'x=\0'` stays `'x=\0'` and
`'x=\x00'` stays `'x=\x00'`. Surrounding code is still formatted normally.

The formatter uses the original literal's source span rather than decoding and
re-encoding it. This avoids collapsing hex or Unicode escapes into characters and
keeps a NUL followed by digits distinct from an octal escape.

The delimiter is normalized: single quotes are preferred, unless the literal
contains a `'` but no `"`. So `"text"` becomes `'text'`, while `"it's"` keeps its
double quotes. Only the quote escaping is rewritten along with the delimiter, so
`"say \"hi\""` becomes `'say "hi"'` and `'it\'s'` becomes `"it's"`. This is the
rule that interpolated literals and the legacy formatter already follow, so plain
and interpolated literals are spelled the same way. A raw literal cannot escape a
quote, so its delimiter decides what it can hold and is kept as written.

Literals without a usable source spelling, such as synthesized AST nodes, still
use the existing escaping fallback. That fallback emits NUL as `\x00` so that
following octal digits cannot change the value.

The C-backend rewrite from a string literal's `.str` selector to a `c` literal
retains the literal spelling and is stable on subsequent formatting passes.

This policy does not change attribute formatting or compiler/scanner escape
semantics. Interpolated strings continue to use their existing formatting path so
embedded expressions can be formatted.
