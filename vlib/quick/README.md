# quick

Module `quick` is V's equivalent of Go's `testing/quick`: property-based
testing with deterministic seeded generators for `bool`, `int`, `f64`,
and `string`. Shrinking counterexamples is future work.

`IntGenerator` generates integers in `[lo, hi)`, including intervals wider
than `max_int`. If `hi <= lo`, it returns `lo`.
