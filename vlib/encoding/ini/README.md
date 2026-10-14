# encoding.ini

`parse_string(text)!` and `parse_file(path)!` read a strict INI dialect.
Headers use `[section]`, including an empty section name; entries use `key = value`
or `key : value`. Blank lines and full-line `;` or `#` comments are ignored.
Duplicate sections or keys, keys before a header, missing separators, and unbalanced
headers produce errors with line numbers. Values retain inline `;` and `#` characters.

`get(section, key)` returns an optional string; `get_or(section, key, fallback)` supplies
a default. `set(section, key, value)` creates a missing section. `sections()` returns
section names in insertion order. `encode()` writes sections in that order and sorts their keys.

Encoding does not escape names or values. For reparsable output, use single-line names and values,
section names without `]`, and keys without `=` or `:` or a leading `[`, `;`, or `#`.
The parser trims surrounding whitespace from names, keys, and values.
