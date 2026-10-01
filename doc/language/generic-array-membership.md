# Membership in generic array results

Arrays returned by generic functions support `in`, `!in`, `.contains()`, `.index()` and
`.last_index()` directly, without first assigning the result to a local variable. For example,
`value in arrays.flatten(rows)` compares `value` using the element type of `rows`.

Interface arrays also accept concrete values returned by generic functions. Membership compares
the concrete type and value, preserving evaluation order: `needle in values` evaluates the needle
first, while `values.contains(needle)` and the index methods evaluate the array first.
