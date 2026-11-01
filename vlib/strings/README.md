## Description

`strings` provides utilities for efficiently processing large strings.

If you got here looking for methods available on the `string` struct, those
methods are found in the `builtin` module.

`pad_left(s, width, pad)`, `pad_right(s, width, pad)`, and `center(s, width, pad)` extend a
string to the requested rune count. They repeat multi-rune padding and truncate it on the right
without splitting UTF-8 sequences. Empty padding, nonpositive widths, and widths no greater
than the input rune count return the input unchanged. `center` puts any extra padding on the
right and restarts the padding pattern on each side: `center('ab', 5, 'xy')` is `'xabxy'`.
Widths count runes, not grapheme clusters or terminal display columns.

`zfill(s, width)` adds zeros on the left, after a leading ASCII `+` or `-` if present.
It includes that sign in the requested rune count and leaves the original input bytes intact.
For example, `zfill('-42', 5)` is `'-0042'`; the input does not have to be numeric.

`cut(s, sep)` returns the text before and after the first exact byte sequence `sep`,
plus a boolean indicating a match. An absent separator returns `s`, `''`, and `false`;
an empty separator returns `''`, `s`, and `true`, even when `s` is empty.
`cut_prefix(s, prefix)` and `cut_suffix(s, suffix)` remove at most one matching affix
and return the remaining string plus the match boolean. An empty affix always matches
without changing `s`. These helpers preserve UTF-8 and arbitrary byte content as supplied.

Builder rune methods write U+FFFD for surrogate halves and values above U+10FFFF,
including repeated writes. Valid Unicode scalar values retain their UTF-8 encoding.

`levenshtein_distance_percentage` returns 100 for identical strings, including two empty strings.
An empty string compared with a nonempty string returns zero.

`get_close_matches(word, candidates, cutoff)` ranks candidates by Levenshtein similarity
from highest to lowest, retaining candidate order for tied scores. The required cutoff
is on a 0..100 scale; NaN and values outside that range panic. For example,
`get_close_matches('appel', ['apple', 'pear', 'ape'], 60)` returns `['apple', 'ape']`.

Builder buffer methods clamp requested byte ranges to the stored buffer. Negative counts to
`go_back` or `cut_last` do nothing; counts beyond its length discard or return the whole buffer.
`go_back_to` and `cut_to` treat negative positions as zero and leave positions past the end alone.
`spart` returns only the intersection of the requested range with the buffer.

`split_after(s, sep)` retains each separator in its preceding piece, including a final empty
piece after a trailing separator. An empty separator returns `[s]`. `equal_fold(a, b)` folds
ASCII letters only. `last_index_any(s, chars)` searches bytes and returns a byte index or `-1`;
use `contains_rune(s, r)` to search for a complete Unicode rune.

`indent(s, prefix, skip_first)` adds `prefix` to each line separated by `\n`.
When `skip_first` is true, the first line is unchanged. Empty lines are also prefixed,
including the empty final line after a trailing newline. An empty input is one empty line:
it returns `prefix` when `skip_first` is false, and an empty string otherwise.

## Tabular text

`TabWriter` collects rows with `write_row(columns)` and aligns their columns using spaces.
`min_width` is the minimum content width of each column; `padding` is the additional spacing
after it. Their defaults are zero and one, respectively, and negative values are treated as zero.
Widths count Unicode runes, so they do not measure terminal display widths or grapheme clusters.
Cells are copied as supplied; embedded tabs, line breaks, and escape sequences are not interpreted.

Rows can have different lengths; missing cells are treated as empty strings. The final column
is not padded. `render()` joins the rows with newlines without adding an extra final newline,
and leaves the collected rows available for later renders. `write_row` copies the input array,
and `reset()` clears the rows while keeping the configuration. An overflowing column width panics.

```v
import strings

fn main() {
	mut table := strings.TabWriter{}
	table.write_row(['name', 'age'])
	table.write_row(['alice', '30'])
	assert table.render() == 'name  age\nalice 30'
}
```
