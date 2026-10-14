# Combinatorics

`permutations(items)` returns every ordering of the input positions using Heap's algorithm.
`combinations(items, k)` returns the k-element selections in input-index order, and
`product(a, b)` returns pairs with each element of `a` followed by every element of `b`.
These functions eagerly allocate all result rows; factorial or combinatorial output can be large.
Repeated input values can produce repeated rows. Inputs and result rows have independent arrays.
Empty input has one empty permutation; choosing zero elements has one empty combination.
An invalid combination size or a product with either input empty returns no rows.
