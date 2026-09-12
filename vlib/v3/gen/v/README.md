# V3 source formatter

The V3 formatter formats source syntax without type checking.

## String literal spelling

Non-interpolated string literals retain their original source spelling, including
quote delimiters, escape sequences, and the `r`, `c`, or `js` prefix. For example,
`'x=\0'` stays `'x=\0'`, `'x=\x00'` stays `'x=\x00'`, and `"text"` keeps its double
quotes. Surrounding code is still formatted normally.

The formatter uses the original literal's source span rather than decoding and
re-encoding it. This also avoids collapsing hex or Unicode escapes into characters
and keeps a NUL followed by digits distinct from an octal escape.

Literals without a usable source spelling, such as synthesized AST nodes, still
use the existing escaping fallback. That fallback emits NUL as `\x00` so that
following octal digits cannot change the value.

The C-backend rewrite from a string literal's `.str` selector to a `c` literal
retains the literal spelling and is stable on subsequent formatting passes.

This policy does not change interpolation formatting, attribute formatting, or
compiler/scanner escape semantics. Interpolated strings continue to use their
existing formatting path so embedded expressions can be formatted.
