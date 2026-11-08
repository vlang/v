# unicode

Module `unicode` provides Unicode character classification backed by
generated tables.

## API

- `unicode.category(r rune) u8` and `category_name` (General_Category).
- Predicates: `is_letter`, `is_digit`, `is_number`, `is_space`,
  `is_punct`, `is_symbol`, `is_mark`, `is_control`, `is_alphabetic`,
  `is_upper`, `is_lower`.
- Case mapping: `to_lower`, `to_upper`, `to_title`, `fold`, `fold_full`.
- `unicode_version()` reports the UCD release (currently 16.0.0).

## Regenerating tables

Tables are generated reproducibly from the Unicode Character Database:

```sh
v run vlib/unicode/gen/make_tables.v -- <ucd-dir> <version>
```

`<ucd-dir>` must hold `UnicodeData.txt`, `PropList.txt`,
`DerivedCoreProperties.txt` and `CaseFolding.txt` from one UCD release.
Cross-checked against an independent implementation on 400 random code
points (398 exact; 2 are characters added after the reference data).
