## Description

`arrays` is a module that provides utility functions to make working with arrays easier.

## Examples

```v
import arrays

fn main() {
	a := [1, 5, 7, 0, 9]
	assert arrays.min(a)! == 0
	assert arrays.max(a)! == 9
	assert arrays.idx_min(a)! == 3
}
```

`lower_bound`, `upper_bound`, and `binary_search` return an error when no matching element
exists. The error names the operation and the value that could not be found.

`chunk(array, size)` returns an empty array when `size` is zero or negative.

`permutations(items)` returns every ordering of the input positions using Heap's algorithm.
`combinations(items, k)` returns the k-element selections in input-index order, and
`product(a, b)` returns pairs with each element of `a` followed by every element of `b`.
These functions eagerly allocate all result rows; factorial or combinatorial output can be large.
Repeated input values can produce repeated rows. Inputs and result rows have independent arrays.
Empty input has one empty permutation; choosing zero elements has one empty combination.
An invalid combination size or a product with either input empty returns no rows.

`bisect_left(array, value)` and `bisect_right(array, value)` return insertion positions in an
ascending sorted array, before or after existing equal values. `insort_left` and `insort_right`
insert at those positions. Empty arrays return position zero; unsorted input is unsupported.
