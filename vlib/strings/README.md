## Description

`strings` provides utilities for efficiently processing large strings.

If you got here looking for methods available on the `string` struct, those
methods are found in the `builtin` module.

`levenshtein_distance_percentage` returns 100 for identical strings, including two empty strings.
An empty string compared with a nonempty string returns zero.
