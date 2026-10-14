# UTF-8 utilities

## Decoding a rune

`get_rune(s, index)` returns the rune that starts at the byte offset `index` of `s`.
`decode_rune_at(s, index)` returns that rune and the number of bytes that encode it,
like Go's `utf8.DecodeRuneInString(s[index:])`:

| At `index`                                       | rune     | size   |
|--------------------------------------------------|----------|--------|
| a valid sequence                                 | the rune | 1 to 4 |
| an invalid or truncated sequence                 | U+FFFD   | 1      |
| nothing: `index` is outside `s`, or `s` is empty | U+FFFD   | 0      |

A U+FFFD that is really in `s` has a size of 3, and a NUL byte decodes to `rune(0)` with a
size of 1, so the size tells an out of range read from every character of the string.

```v
import encoding.utf8

s := 'a★'
mut i := 0
for {
	r, size := utf8.decode_rune_at(s, i)
	if size == 0 {
		break
	}
	println('${r} is ${size} byte(s) long')
	i += size
}
```

## Unicode categories

The letter, number and punctuation predicates use Unicode 15.0.0 general categories:
`is_letter` recognizes category `L`, `is_number` recognizes `N`, and `is_punct` and
`is_rune_punct` recognize all `P` categories.

`is_punct` takes a UTF-8 string and a byte offset; `is_rune_punct` takes a rune.
Both recognize punctuation from every script, including ASCII hyphen-minus (`-`),
Arabic comma and ideographic full stop. Their membership matches `is_global_punct`
and `is_rune_global_punct`.

The category tables match the
[Unicode 15.0.0 character database](https://www.unicode.org/Public/15.0.0/ucd/UnicodeData.txt)
and [Go's Unicode 15.0.0 tables](https://github.com/golang/go/blob/go1.26.1/src/unicode/tables.go).
