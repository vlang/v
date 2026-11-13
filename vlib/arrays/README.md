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

`group` zips its input arrays into rows. A single array is one input: `arrays.group([1, 2])`
returns `[[1], [2]]`. To supply an array of inputs, spread it with `arrays.group(...inputs)`.
Generic element types can be inferred from the input arrays, including inside interpolation.

Nested arrays follow the same rule: `arrays.group([[1, 2], [3, 4]])` returns
`[[[1, 2]], [[3, 4]]]`, while `arrays.group(...[[1, 2], [3, 4]])` returns `[[1, 3], [2, 4]]`.

## Stable sorting

`stable_sort(mut array, less)` sorts an array or slice in place, preserving the input order of
elements that compare as equivalent. `less(a, b)` must define a consistent strict weak ordering:
it returns true when `a` precedes `b`, and false in both directions for equivalent elements.
The comparator must not modify the array. Empty and single-element arrays need no comparisons.
The merge sort uses O(n log n) comparisons and O(n) temporary elements.

```v
import arrays

fn main() {
	mut words := ['apple', 'pear', 'plum', 'fig']
	arrays.stable_sort(mut words, fn (a string, b string) bool {
		return a.len < b.len
	})
	assert words == ['fig', 'pear', 'plum', 'apple']
}
```
