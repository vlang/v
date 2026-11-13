module arrays

// stable_sort sorts `arr` in place using a stable merge sort. The order of
// equivalent elements (neither is less than the other) is preserved.
//
// `less(a, b)` must return true when `a` should come before `b` and define
// a strict weak ordering. It must not modify the array being sorted.
// The sort uses O(n log n) comparisons and O(n) temporary elements.
pub fn stable_sort[T](mut arr []T, less fn (a T, b T) bool) {
	if arr.len < 2 {
		return
	}
	mut aux := []T{len: arr.len}
	merge_sort(mut arr, mut aux, 0, arr.len, less)
}

fn merge_sort[T](mut arr []T, mut aux []T, lo int, hi int, less fn (a T, b T) bool) {
	if hi - lo <= 1 {
		return
	}
	mid := lo + (hi - lo) / 2
	merge_sort(mut arr, mut aux, lo, mid, less)
	merge_sort(mut arr, mut aux, mid, hi, less)
	// merge
	mut i := lo
	mut j := mid
	mut k := lo
	for i < mid && j < hi {
		if less(arr[j], arr[i]) {
			aux[k] = arr[j]
			j++
		} else {
			aux[k] = arr[i]
			i++
		}
		k++
	}
	for i < mid {
		aux[k] = arr[i]
		i++
		k++
	}
	for j < hi {
		aux[k] = arr[j]
		j++
		k++
	}
	for t in lo .. hi {
		arr[t] = aux[t]
	}
}
