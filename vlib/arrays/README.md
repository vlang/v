## Description

`arrays` is a module that provides utility functions to make working with arrays easier.

See [combinatorics](combinatorics.md) for permutations, combinations, and Cartesian products.

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

`is_sorted(array)` checks ascending order, allowing equal adjacent elements. Empty and
single-element arrays are sorted. The element type must support ordering comparisons.
`bisect_left(array, value)` and `bisect_right(array, value)` return insertion positions in an
ascending sorted array, before or after existing equal values. `insort_left` and `insort_right`
insert at those positions. Empty arrays return position zero; unsorted input is unsupported.
