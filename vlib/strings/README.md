## Description

`strings` provides utilities for efficiently processing large strings.

If you got here looking for methods available on the `string` struct, those
methods are found in the `builtin` module.

`levenshtein_distance_percentage` returns 100 for identical strings, including two empty strings.
An empty string compared with a nonempty string returns zero.

`get_close_matches(word, candidates, cutoff)` ranks candidates by Levenshtein similarity
from highest to lowest, retaining candidate order for tied scores. The required cutoff
is on a 0..100 scale; NaN and values outside that range panic. For example,
`get_close_matches('appel', ['apple', 'pear', 'ape'], 60)` returns `['apple', 'ape']`.
