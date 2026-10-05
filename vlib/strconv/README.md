## Description

`strconv` provides functions for converting strings to numbers and numbers to strings.

## Buffer formatting

`write_dec` and `write_dec_u` write a decimal integer into a caller-provided `[]u8`
buffer. They return the number of bytes written, or `-1` when the buffer is too small.

```v
import strconv

mut buf := []u8{len: 20}
n := strconv.write_dec(-12345, mut buf)
assert n == 6
assert buf[..n].bytestr() == '-12345'
```

## Thousands separators

`format_thousands` formats built-in integer and floating-point values with a
thousands separator. Pass a string for the integer separator, or a `Separator`
to configure both integer and decimal separators.

```v
import strconv

assert strconv.format_thousands(1234567, ' ') == '1 234 567'
assert strconv.format_thousands(1234567.89, strconv.Separator{
	integer: '.'
	decimal: ','
}) == '1.234.567,89'
```

The numeric type is checked at compile time; unsupported types produce a
compile-time error.

`add_thousands_sep` groups the integer part of an already formatted numeric string,
preserving its sign, fraction, and exponent. It accepts the same separator options.
Unlike the numeric API, the string API keeps scientific notation as supplied.

```v
import strconv

assert strconv.add_thousands_sep('1234567.89', ',') == '1,234,567.89'
assert strconv.add_thousands_sep('1.5e+21', ',') == '1.5e+21'
assert strconv.add_thousands_sep('-12345.5', strconv.Separator{
	integer: '.'
	decimal: ','
}) == '-12.345,5'
```

## Quoting strings and runes

`quote` returns a double-quoted Go-syntax string literal for a string, and
`quote_rune` a single-quoted Go rune literal for one rune. Control characters
and non-printable runes become Go escape sequences; everything else is kept as
it is, so UTF-8 text stays readable in the output. The result follows Go's
syntax rather than V's: `$` is not escaped, so `quote('\${x}')` returns
`"${x}"`, which V would read as an interpolation.

```v
import strconv

assert strconv.quote('café') == '"café"'
assert strconv.quote('a\tb') == '"a\\tb"'
assert strconv.quote_rune(`A`) == "'A'"
assert strconv.quote_rune(`é`) == "'é'"
```

The `*_to_ascii` variants escape every non-ASCII rune as `\u` or `\U`, and the
`*_to_graphic` variants escape only the runes `is_graphic` rejects, so they keep
spaces such as U+00A0 that `quote` escapes.

```v
import strconv

assert strconv.quote_to_ascii('café') == '"caf\\u00e9"'
assert strconv.quote('a b') == '"a\\u00a0b"'
assert strconv.quote_to_graphic('a b') == '"a b"'
```

Each function has an `append_` form that writes into a `[]u8` the caller
already owns instead of allocating a new string, which is what you want inside
a loop.

```v
import strconv

mut buf := 'log: '.bytes()
for word in ['a', 'b\tc'] {
	strconv.append_quote(mut buf, word)
	buf << ` `
}
assert buf.bytestr() == 'log: "a" "b\\tc" '
```

`is_print` and `is_graphic` answer the two questions those functions ask, and
`can_backquote` reports whether a string can be written unchanged as a Go raw
string literal, between backquotes. That rules out control characters other
than tab, the backquote itself, DEL, invalid UTF-8 and the byte order mark; a
backslash is fine, since a raw string has no escapes.

```v
import strconv

assert strconv.is_print(`a`)
assert !strconv.is_print(0x07)
assert !strconv.is_print(0x00A0)
assert strconv.is_graphic(0x00A0)
assert !strconv.is_graphic(0x00AD)
assert strconv.can_backquote('a\\b')
assert !strconv.can_backquote('a`b')
```

The lookup tables in `printable_tables.v` are generated from Go 1.26.1's
`unicode.IsPrint` and `unicode.IsGraphic` (Unicode 15.0.0) by
`testdata/gen_quote_data.go`, which also produces the expected results the tests
compare against. `is_print`, `is_graphic` and `can_backquote` are each checked
against Go over every code point from 0 to `0x10FFFF`.

## Unquoting literals

`unquote` returns the string value a Go-syntax literal spells. It accepts a
double-quoted, single-quoted or backquoted (raw) form, and fails on an
unterminated literal or an invalid escape. `quoted_prefix` reads only the
literal at the start of its input and returns it verbatim, quotes included,
which is what you want when parsing a stream.

```v
import strconv

assert strconv.unquote('"a\\tb"') or { '?' } == 'a\tb'
assert strconv.unquote('`raw`') or { '?' } == 'raw'
assert strconv.quoted_prefix('"a" then more') or { '?' } == '"a"'
```

A single-quoted literal holds exactly one character, and a raw literal drops
any carriage return inside it.

```v
import strconv

assert strconv.unquote("'a'") or { '?' } == 'a'
assert strconv.unquote("'ab'") or { '?' } == '?'
assert strconv.unquote('`a\rb`') or { '?' } == 'ab'
```

`unquote_char` decodes one character and returns it together with whether it
was written as a multi-byte sequence and the input that followed it. `quote`
must be the quote byte of the literal the input comes from.

```v
import strconv

r := strconv.unquote_char('\\x41BC', `"`) or { return }

assert r.value == `A`
assert r.multibyte == false
assert r.tail == 'BC'
```

Like the quoting functions, all three are checked against Go 1.26.1: over
21760 generated literals (every byte in all three quote styles, and every
two-byte body whose second byte is 0x00-0x28 in the two quoted styles), and
over 54 hand-picked inputs covering the numeric escape forms and their
boundaries, invalid UTF-8, multi-byte runes and the error cases.
