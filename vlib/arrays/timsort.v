module arrays

// timsort sorts `arr` in place using a stable timsort: it detects naturally
// occurring runs, extends short runs with insertion sort, and merges runs with
// the standard timsort stack invariants. Equal elements keep their input order.
//
// `less(a, b)` must return true when `a` should come before `b` and define a
// strict weak ordering. It must not modify the array being sorted.
pub fn timsort[T](mut arr []T, less fn (a T, b T) bool) {
	n := arr.len
	if n < 2 {
		return
	}
	mut aux := []T{len: n}

	// Compute minrun.
	mut minrun := n
	mut r := 0
	for minrun >= 32 {
		r |= minrun & 1
		minrun >>= 1
	}
	minrun += r
	if minrun < 1 {
		minrun = 1
	}

	mut run_starts := []int{cap: 64}
	mut run_lens := []int{cap: 64}

	mut i := 0
	for i < n {
		run := count_run[T](arr, i, less)
		mut run_len := run.len
		mut descending := run.descending
		if descending {
			reverse_range[T](mut arr, i, i + run_len)
		}
		if run_len < minrun {
			mut force := minrun
			if i + force > n {
				force = n - i
			}
			insertion_sort[T](mut arr, i, i + force, less)
			run_len = force
		}
		run_starts << i
		run_lens << run_len
		i += run_len
		merge_collapse[T](mut arr, mut aux, mut run_starts, mut run_lens, less)
	}
	// Merge remaining runs.
	for run_starts.len > 1 {
		merge_last_two[T](mut arr, mut aux, mut run_starts, mut run_lens, less)
	}
}

struct TimRun {
	len        int
	descending bool
}

fn count_run[T](arr []T, start int, less fn (a T, b T) bool) TimRun {
	n := arr.len
	if start + 1 >= n {
		return TimRun{1, false}
	}
	mut descending := false
	if less(arr[start + 1], arr[start]) {
		descending = true
	}
	mut run_len := 2
	for start + run_len < n {
		if descending {
			if less(arr[start + run_len], arr[start + run_len - 1]) {
				run_len++
			} else {
				break
			}
		} else {
			if !less(arr[start + run_len], arr[start + run_len - 1]) {
				run_len++
			} else {
				break
			}
		}
	}
	return TimRun{run_len, descending}
}

fn reverse_range[T](mut arr []T, lo int, hi int) {
	mut a := lo
	mut b := hi - 1
	for a < b {
		tmp := arr[a]
		arr[a] = arr[b]
		arr[b] = tmp
		a++
		b--
	}
}

fn insertion_sort[T](mut arr []T, lo int, hi int, less fn (a T, b T) bool) {
	for i in lo + 1 .. hi {
		key := arr[i]
		mut j := i - 1
		for j >= lo && less(key, arr[j]) {
			arr[j + 1] = arr[j]
			j--
		}
		arr[j + 1] = key
	}
}

fn merge_collapse[T](mut arr []T, mut aux []T, mut starts []int, mut lens []int, less fn (a T, b T) bool) {
	for {
		n := starts.len
		if n < 2 {
			break
		}
		if n >= 3 && lens[n - 3] <= lens[n - 2] + lens[n - 1] {
			if n >= 4 && lens[n - 4] <= lens[n - 3] {
				// merge the smaller of the two possible
			}
			if lens[n - 3] < lens[n - 1] {
				// merge runs n-3 and n-2
				merge_at[T](mut arr, mut aux, mut starts, mut lens, n - 3, less)
			} else {
				merge_at[T](mut arr, mut aux, mut starts, mut lens, n - 2, less)
			}
		} else if lens[n - 2] <= lens[n - 1] {
			merge_at[T](mut arr, mut aux, mut starts, mut lens, n - 2, less)
		} else {
			break
		}
	}
}

fn merge_at[T](mut arr []T, mut aux []T, mut starts []int, mut lens []int, idx int, less fn (a T, b T) bool) {
	// merge runs idx and idx+1
	base1 := starts[idx]
	len1 := lens[idx]
	len2 := lens[idx + 1]
	merge_runs[T](mut arr, mut aux, base1, len1, len2, less)
	// update stack
	starts[idx] = base1
	lens[idx] = len1 + len2
	// remove idx+1
	starts.delete(idx + 1)
	lens.delete(idx + 1)
}

fn merge_last_two[T](mut arr []T, mut aux []T, mut starts []int, mut lens []int, less fn (a T, b T) bool) {
	n := starts.len
	if n < 2 {
		return
	}
	merge_at[T](mut arr, mut aux, mut starts, mut lens, n - 2, less)
}

fn merge_runs[T](mut arr []T, mut aux []T, base int, len1 int, len2 int, less fn (a T, b T) bool) {
	if len1 == 0 || len2 == 0 {
		return
	}
	mut i := base
	mut j := base + len1
	mut k := base
	end1 := base + len1
	end2 := base + len1 + len2
	for i < end1 && j < end2 {
		if less(arr[j], arr[i]) {
			aux[k] = arr[j]
			j++
		} else {
			aux[k] = arr[i]
			i++
		}
		k++
	}
	for i < end1 {
		aux[k] = arr[i]
		i++
		k++
	}
	for j < end2 {
		aux[k] = arr[j]
		j++
		k++
	}
	for t in base .. end2 {
		arr[t] = aux[t]
	}
}
