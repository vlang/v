## Description

`strings` provides utilities for efficiently processing large strings.

If you got here looking for methods available on the `string` struct, those
methods are found in the `builtin` module.

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
