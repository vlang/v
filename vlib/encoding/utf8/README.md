# UTF-8 utilities

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
