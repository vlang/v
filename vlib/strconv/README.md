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

`quote` returns a double-quoted V string literal for a string, and
`quote_rune` the single-quoted form for one rune. Control characters and
non-printable runes become Go escape sequences; everything else is kept as it
is, so UTF-8 text stays readable in the output.

```v
import strconv

assert strconv.quote('café') == '"café"'
assert strconv.quote('a\tb') == '"a\\tb"'
assert strconv.quote_rune('A'.runes()[0]) == "'A'"
assert strconv.quote_rune(0x00E9) == "'é'"
```

The `*_to_ascii` variants escape every non-ASCII rune as `\u` or `\U`, and the
`*_to_graphic` variants keep a rune only when it is in the graphic set.

```v
import strconv

assert strconv.quote_to_ascii('café') == '"caf\\u00e9"'
assert strconv.quote_to_graphic('a\u00e9') == '"aé"'
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
`can_backquote` reports whether a string can be written as a raw backquoted
literal.

```v
import strconv

assert strconv.is_print('a'.runes()[0])
assert !strconv.is_print(0x07)
assert strconv.is_graphic(' '.runes()[0])
assert !strconv.is_graphic(0x00AD)
assert !strconv.can_backquote('a\\b')
```

The lookup tables live in `printable_tables.v`, generated from Go's
`unicode.IsPrint`, `unicode.IsGraphic` and `strconv.CanBackquote`. The test
suite checks all three against Go's output, and `is_print`, `is_graphic` and
`can_backquote` are each verified over every code point from 0 to `0x10FFFF`.

## Unquoting literals

`unquote` returns the string value a literal spells. It accepts a
double-quoted, single-quoted or backquoted form, and fails on an unterminated
literal or an invalid escape. `quoted_prefix` reads only the literal at the
start of its input and returns it verbatim, quotes included, which is what you
want when parsing a stream.

```v
import strconv

assert strconv.unquote('"a\\tb"') or { '?' } == 'a	b'
assert strconv.unquote('`raw`') or { '?' } == 'raw'
assert strconv.quoted_prefix('"a" then more') or { '?' } == '"a"'
```

A single-quoted literal holds exactly one character, and a raw literal drops
any carriage return inside it.

```v
import strconv

sq := [1]u8[0x27].bytestr() // a single quote, as a string

assert strconv.unquote(sq + 'a' + sq) or { '?' } == 'a'
assert strconv.unquote('"a\rb"') or { '?' } == 'ab'
```

`unquote_char` decodes one character and reports what followed it. It is the
building block the other two are written from, and returns a struct because V
has no multi-value return. `quote` must be the quote byte the literal uses.

```v
import strconv

r := strconv.unquote_char('\\x41BC', 0x22) or { return }

assert r.value == `A`
assert r.multibyte == false
assert r.tail == 'BC'
```

Like the quoting functions, all three are checked against Go 1.26.1 over a
corpus of 29675 inputs: every single byte, every two-byte escape body, the
numeric escape forms including their boundaries, invalid UTF-8 tails, and
multi-byte runes inside literals.

