module canvas

// A port of libc++ 21's std::sort (introsort, pattern-defeating partitions,
// guarded/unguarded insertion sorts and a heap sort fallback) for the draw
// sort keys. Skia sorts draws with std::sort, which is not stable: equal keys
// keep the order this exact algorithm produces, and that order decides how
// draws batch into instanced draw calls, which changes MSAA results. Keep the
// comparisons and moves identical to the C++ templates for non-arithmetic
// value types (`__use_branchless_sort` is false).

// SortKey mirrors skgpu::graphite::DrawList::SortKey: two 64-bit fields
// compared lexicographically, plus the index of the draw that produced it.
struct SortKey {
	pipeline_key u64
	uniform_key  u64
	draw         int
}

@[inline]
fn key_less(a SortKey, b SortKey) bool {
	return a.pipeline_key < b.pipeline_key
		|| (a.pipeline_key == b.pipeline_key && a.uniform_key < b.uniform_key)
}

@[inline]
fn key_swap(mut a []SortKey, i int, j int) {
	t := a[i]
	a[i] = a[j]
	a[j] = t
}

fn libcpp_sort(mut a []SortKey) {
	if a.len == 0 {
		return
	}
	// __depth_limit = 2 * __bit_log2(len)
	mut log2 := 0
	for n := u64(a.len); n > 1; n >>= 1 {
		log2++
	}
	introsort(mut a, 0, a.len, 2 * log2, true)
}

fn sort3(mut a []SortKey, x int, y int, z int) bool {
	if !key_less(a[y], a[x]) {
		if !key_less(a[z], a[y]) {
			return false
		}
		key_swap(mut a, y, z)
		if key_less(a[y], a[x]) {
			key_swap(mut a, x, y)
		}
		return true
	}
	if key_less(a[z], a[y]) {
		key_swap(mut a, x, z)
		return true
	}
	key_swap(mut a, x, y)
	if key_less(a[z], a[y]) {
		key_swap(mut a, y, z)
	}
	return true
}

fn sort4(mut a []SortKey, x1 int, x2 int, x3 int, x4 int) {
	sort3(mut a, x1, x2, x3)
	if key_less(a[x4], a[x3]) {
		key_swap(mut a, x3, x4)
		if key_less(a[x3], a[x2]) {
			key_swap(mut a, x2, x3)
			if key_less(a[x2], a[x1]) {
				key_swap(mut a, x1, x2)
			}
		}
	}
}

fn sort5(mut a []SortKey, x1 int, x2 int, x3 int, x4 int, x5 int) {
	sort4(mut a, x1, x2, x3, x4)
	if key_less(a[x5], a[x4]) {
		key_swap(mut a, x4, x5)
		if key_less(a[x4], a[x3]) {
			key_swap(mut a, x3, x4)
			if key_less(a[x3], a[x2]) {
				key_swap(mut a, x2, x3)
				if key_less(a[x2], a[x1]) {
					key_swap(mut a, x1, x2)
				}
			}
		}
	}
}

fn insertion_sort(mut a []SortKey, first int, last int) {
	if first == last {
		return
	}
	for i := first + 1; i != last; i++ {
		mut j := i - 1
		if key_less(a[i], a[j]) {
			t := a[i]
			mut k := j
			j = i
			for {
				a[j] = a[k]
				j = k
				if j == first {
					break
				}
				k--
				if !key_less(t, a[k]) {
					break
				}
			}
			a[j] = t
		}
	}
}

// insertion_sort_unguarded relies on an element left of `first` that is not
// greater than any element of the range.
fn insertion_sort_unguarded(mut a []SortKey, first int, last int) {
	if first == last {
		return
	}
	for i := first + 1; i != last; i++ {
		mut j := i - 1
		if key_less(a[i], a[j]) {
			t := a[i]
			mut k := j
			j = i
			for {
				a[j] = a[k]
				j = k
				k--
				if !key_less(t, a[k]) {
					break
				}
			}
			a[j] = t
		}
	}
}

fn insertion_sort_incomplete(mut a []SortKey, first int, last int) bool {
	match last - first {
		0, 1 {
			return true
		}
		2 {
			if key_less(a[last - 1], a[first]) {
				key_swap(mut a, first, last - 1)
			}
			return true
		}
		3 {
			sort3(mut a, first, first + 1, last - 1)
			return true
		}
		4 {
			sort4(mut a, first, first + 1, first + 2, last - 1)
			return true
		}
		5 {
			sort5(mut a, first, first + 1, first + 2, first + 3, last - 1)
			return true
		}
		else {}
	}
	mut j := first + 2
	sort3(mut a, first, first + 1, j)
	limit := 8
	mut count := 0
	for i := j + 1; i != last; i++ {
		if key_less(a[i], a[j]) {
			t := a[i]
			mut k := j
			j = i
			for {
				a[j] = a[k]
				j = k
				if j == first {
					break
				}
				k--
				if !key_less(t, a[k]) {
					break
				}
			}
			a[j] = t
			count++
			if count == limit {
				return i + 1 == last
			}
		}
		j = i
	}
	return true
}

// partition_with_equals_on_right returns the pivot position and whether the
// range was already partitioned.
fn partition_with_equals_on_right(mut a []SortKey, first_in int, last_in int) (int, bool) {
	begin := first_in
	mut first := first_in
	mut last := last_in
	pivot := a[first]
	for {
		first++
		if !key_less(a[first], pivot) {
			break
		}
	}
	if begin == first - 1 {
		for first < last {
			last--
			if key_less(a[last], pivot) {
				break
			}
		}
	} else {
		for {
			last--
			if key_less(a[last], pivot) {
				break
			}
		}
	}
	already_partitioned := first >= last
	for first < last {
		key_swap(mut a, first, last)
		for {
			first++
			if !key_less(a[first], pivot) {
				break
			}
		}
		for {
			last--
			if key_less(a[last], pivot) {
				break
			}
		}
	}
	pivot_pos := first - 1
	if begin != pivot_pos {
		a[begin] = a[pivot_pos]
	}
	a[pivot_pos] = pivot
	return pivot_pos, already_partitioned
}

fn partition_with_equals_on_left(mut a []SortKey, first_in int, last_in int) int {
	begin := first_in
	mut first := first_in
	mut last := last_in
	pivot := a[first]
	if key_less(pivot, a[last - 1]) {
		for {
			first++
			if key_less(pivot, a[first]) {
				break
			}
		}
	} else {
		for {
			first++
			if !(first < last && !key_less(pivot, a[first])) {
				break
			}
		}
	}
	if first < last {
		for {
			last--
			if !key_less(pivot, a[last]) {
				break
			}
		}
	}
	for first < last {
		key_swap(mut a, first, last)
		for {
			first++
			if key_less(pivot, a[first]) {
				break
			}
		}
		for {
			last--
			if !key_less(pivot, a[last]) {
				break
			}
		}
	}
	pivot_pos := first - 1
	if begin != pivot_pos {
		a[begin] = a[pivot_pos]
	}
	a[pivot_pos] = pivot
	return first
}

fn introsort(mut a []SortKey, first_in int, last_in int, depth_in int, leftmost_in bool) {
	limit := 24
	ninther_threshold := 128
	mut first := first_in
	mut last := last_in
	mut depth := depth_in
	mut leftmost := leftmost_in
	for {
		len := last - first
		match len {
			0, 1 {
				return
			}
			2 {
				if key_less(a[last - 1], a[first]) {
					key_swap(mut a, first, last - 1)
				}
				return
			}
			3 {
				sort3(mut a, first, first + 1, last - 1)
				return
			}
			4 {
				sort4(mut a, first, first + 1, first + 2, last - 1)
				return
			}
			5 {
				sort5(mut a, first, first + 1, first + 2, first + 3, last - 1)
				return
			}
			else {}
		}
		if len < limit {
			if leftmost {
				insertion_sort(mut a, first, last)
			} else {
				insertion_sort_unguarded(mut a, first, last)
			}
			return
		}
		if depth == 0 {
			heap_sort(mut a, first, last)
			return
		}
		depth--
		half_len := len / 2
		if len > ninther_threshold {
			sort3(mut a, first, first + half_len, last - 1)
			sort3(mut a, first + 1, first + (half_len - 1), last - 2)
			sort3(mut a, first + 2, first + (half_len + 1), last - 3)
			sort3(mut a, first + (half_len - 1), first + half_len, first + (half_len + 1))
			key_swap(mut a, first, first + half_len)
		} else {
			sort3(mut a, first + half_len, first, last - 1)
		}
		if !leftmost && !key_less(a[first - 1], a[first]) {
			first = partition_with_equals_on_left(mut a, first, last)
			continue
		}
		mut i, partitioned := partition_with_equals_on_right(mut a, first, last)
		if partitioned {
			fs := insertion_sort_incomplete(mut a, first, i)
			if insertion_sort_incomplete(mut a, i + 1, last) {
				if fs {
					return
				}
				last = i
				continue
			} else {
				if fs {
					i++
					first = i
					continue
				}
			}
		}
		introsort(mut a, first, i, depth, leftmost)
		leftmost = false
		i++
		first = i
	}
}

// heap_sort is libc++'s __partial_sort(first, last, last): make_heap, then
// sort_heap with Floyd's sift-down.
fn heap_sort(mut a []SortKey, first int, last int) {
	n := last - first
	if n > 1 {
		for start := (n - 2) / 2; start >= 0; start-- {
			sift_down(mut a, first, n, start)
		}
	}
	mut end := last
	for len := n; len > 1; len-- {
		pop_heap(mut a, first, end, len)
		end--
	}
}

fn sift_down(mut a []SortKey, first int, len int, start_in int) {
	mut start := start_in
	mut child := start
	if len < 2 || (len - 2) / 2 < child {
		return
	}
	child = 2 * child + 1
	if child + 1 < len && key_less(a[first + child], a[first + child + 1]) {
		child++
	}
	if key_less(a[first + child], a[first + start]) {
		return
	}
	top := a[first + start]
	for {
		a[first + start] = a[first + child]
		start = child
		if (len - 2) / 2 < child {
			break
		}
		child = 2 * child + 1
		if child + 1 < len && key_less(a[first + child], a[first + child + 1]) {
			child++
		}
		if key_less(a[first + child], top) {
			break
		}
	}
	a[first + start] = top
}

fn floyd_sift_down(mut a []SortKey, first int, len int) int {
	mut hole := first
	mut child_i := first
	mut child := 0
	for {
		child_i += child + 1
		child = 2 * child + 1
		if child + 1 < len && key_less(a[child_i], a[child_i + 1]) {
			child_i++
			child++
		}
		a[hole] = a[child_i]
		hole = child_i
		if child > (len - 2) / 2 {
			return hole
		}
	}
	return hole
}

fn pop_heap(mut a []SortKey, first int, last_in int, len int) {
	mut last := last_in
	if len > 1 {
		top := a[first]
		mut hole := floyd_sift_down(mut a, first, len)
		last--
		if hole == last {
			a[hole] = top
		} else {
			a[hole] = a[last]
			hole++
			a[last] = top
			sift_up(mut a, first, hole, hole - first)
		}
	}
}

fn sift_up(mut a []SortKey, first int, last_in int, len_in int) {
	mut last := last_in
	mut len := len_in
	if len > 1 {
		len = (len - 2) / 2
		mut ptr := first + len
		last--
		if key_less(a[ptr], a[last]) {
			t := a[last]
			for {
				a[last] = a[ptr]
				last = ptr
				if len == 0 {
					break
				}
				len = (len - 1) / 2
				ptr = first + len
				if !key_less(a[ptr], t) {
					break
				}
			}
			a[last] = t
		}
	}
}
