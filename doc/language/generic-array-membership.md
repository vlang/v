# Membership in generic array results

Arrays returned by generic functions support `in`, `!in`, `.contains()` and `.index()`
directly, without first assigning the result to a local variable. For example,
`value in arrays.flatten(rows)` compares `value` using the element type of `rows`.
