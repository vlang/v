module arrays

// permutations returns all n! orderings of items (Heap's algorithm,
// iterative, O(n) auxiliary space). The empty input yields one empty
// permutation. It is the V-idiomatic equivalent of Python's
// `itertools.permutations`.
pub fn permutations[T](items []T) [][]T {
	mut res := [][]T{}
	mut a := items.clone()
	mut c := []int{len: a.len}
	res << a.clone()
	mut i := 0
	for i < a.len {
		if c[i] < i {
			if i % 2 == 0 {
				a[0], a[i] = a[i], a[0]
			} else {
				a[c[i]], a[i] = a[i], a[c[i]]
			}
			res << a.clone()
			c[i]++
			i = 0
		} else {
			c[i] = 0
			i++
		}
	}
	return res
}

// combinations returns all C(n, k) k-sized subsets of items in input-index
// order. Out-of-range k yields no rows; k == 0 yields one empty row.
// It is the V-idiomatic equivalent of Python's `itertools.combinations`.
pub fn combinations[T](items []T, k int) [][]T {
	mut res := [][]T{}
	if k < 0 || k > items.len {
		return res
	}
	if k == 0 {
		res << []T{}
		return res
	}
	mut idx := []int{len: k}
	for i in 0 .. k {
		idx[i] = i
	}
	for {
		mut row := []T{cap: k}
		for j in idx {
			row << items[j]
		}
		res << row
		mut i := k - 1
		for i >= 0 && idx[i] == items.len - k + i {
			i--
		}
		if i < 0 {
			break
		}
		idx[i]++
		for j in i + 1 .. k {
			idx[j] = idx[j - 1] + 1
		}
	}
	return res
}

// product returns the cartesian product of a and b: one [x, y] row per pair.
// It is the V-idiomatic equivalent of Python's `itertools.product` for the
// single-type case.
pub fn product[T](a []T, b []T) [][]T {
	mut res := [][]T{}
	for x in a {
		for y in b {
			res << [x, y]
		}
	}
	return res
}
