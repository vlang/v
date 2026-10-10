## Description

`strconv` provides functions for converting strings to numbers and numbers to strings.

## Integer parsing

`parse_int` and `parse_uint` accept an explicit base from 2 to 36, or base 0
for prefix inference: `0b` selects binary, `0o` or a bare leading `0` selects
octal, and `0x` selects hexadecimal. Other inputs use decimal. Use base 10
when leading zeros should remain decimal digits.

```v
import strconv

assert strconv.parse_int('0777', 0, 64)! == 511
assert strconv.parse_uint('010', 0, 64)! == 8
assert strconv.parse_int('0777', 10, 64)! == 777
```

Digits must be valid for the selected base, so `08` and `09` fail with base 0.
An explicit prefix, with its optional underscore separator, must be followed by digits.
V integer literal analysis keeps bare leading zeros decimal; octal literals use `0o`.

Bit size 0 uses the width of `int` in the selected target and backend, as given by
`sizeof(int) * 8`. Explicit bit sizes from 1 to 64 select that many bits regardless of the target.
`parse_int` saturates at the signed limits; `parse_uint` reports overflow as an error.
`atoi` and `string.int()` retain their 32-bit range even on targets with a 64-bit `int`.

String numeric conveniences such as `.int()`, `.i64()`, `.u64()`, and their narrower variants
also keep bare leading zeros decimal. Explicit `0b`, `0o`, and `0x` prefixes still select a base.
Use `.parse_int(0, bits)` or `.parse_uint(0, bits)` for base-zero inference on a string.

Underscores may separate digits, as in `1_000` or `0xFF_FF`, with base 0 only: with an explicit
base, `parse_int` and `parse_uint` return an error for them. The lower-level `common_parse_int`,
`common_parse_uint` and `common_parse_uint2` accept the separators in every base; `.int()` and
the other string conveniences below use them, so `'1_000'.int()` is 1000.

```v
assert '010'.int() == 10
assert '0o10'.int() == 8
assert '010'.parse_int(0, 64)! == 8
```

## Floating-point parsing

On the C backend, `atof64` parses decimal numbers with an optional sign,
decimal point, and exponent. Underscores may separate digits, as in `1_000` or `1.2_5e1_0`.
It also accepts case-insensitive `nan`, `inf`, and `infinity`, with an optional
sign for infinity. Whitespace, bare signs, missing digits, and misplaced
underscores return an error.
Conversion retains up to 18 significant decimal digits and rounds binary halfway cases
to the nearest value with an even significand. Subnormal results use the same rounding rule.

```v
import strconv
import math

assert strconv.atof64('1_000')! == 1000.0
assert math.is_inf(strconv.atof64('-inf')!, -1)
assert math.is_nan(strconv.atof64('NaN')!)
```

On the C backend, `allow_extra_chars: true` permits trailing characters after a decimal number,
for example `atof64('1.5units', allow_extra_chars: true)` returns `1.5`.
A mantissa and any exponent must still contain digits.

On the C backend, a number whose magnitude is too large for an `f64`, such as `1e400`,
returns a `value out of range` error, so that it cannot be mistaken for an `inf` in the input.
Pass `allow_overflow: true` to get `+inf` or `-inf` for such a number instead; `string.f64()`
and `string.f32()`, which have no error to return, do that. The `inf` and `infinity` spellings
never return this error. A number too small for an `f64`, such as `1e-400`, is not an error:
it rounds to a subnormal value or to a signed zero.

```v
import strconv
import math

if value := strconv.atof64('1e400') {
	assert false, 'parsed as ${value}'
} else {
	assert err.msg() == 'strconv.atof64: parsing "1e400": value out of range'
}
assert math.is_inf(strconv.atof64('-1e400', allow_overflow: true)!, -1)
assert strconv.atof64('1.7976931348623157e308')! == math.max_f64
assert strconv.atof64('1e-400')! == 0.0
```

## Integer formatting

`format_int` and `format_uint` represent signed and unsigned integers in any radix
from 2 to 36. Digits above 9 use lowercase letters; negative signed values keep
a leading minus sign, including `min_i64`.

```v
import strconv

assert strconv.format_int(min_i64, 16) == '-8000000000000000'
assert strconv.format_uint(max_u64, 16) == 'ffffffffffffffff'
```

## Scientific floating-point formatting

On the C backend, `f32_to_str_pad` and `f64_to_str_pad` format a value in scientific notation
with the requested number of digits after the decimal point. A zero or negative precision
omits the decimal point, while preserving the exponent. Zero values receive the requested
padding, and a rounding carry adjusts the exponent.

```v
import strconv

assert strconv.f64_to_str_pad(9.5, 0) == '1e+01'
assert strconv.f64_to_str_pad(0.0, 3) == '0.000e+00'
assert strconv.f32_to_str_pad(999984.0, 1) == '1.0e+06'
```

These functions round the shortest decimal representation half up, then append zeros as
needed. They do not round the exact binary value like C's `printf`: for example,
`f64_to_str_pad(0.1, 20)` gives `1.00000000000000000000e-01`.

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
