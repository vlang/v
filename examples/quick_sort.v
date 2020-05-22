import time
import rand

const (
	gen_len = 1000 // how many random numbers to generate
	gen_max = 10000 // max of the generated numbers
)

fn main() {
	rand.seed(time.now().unix)
	rand.next(gen_max) // skip the first
	mut arr := []int{}
	for _ in 0..gen_len {
		arr << rand.next(gen_max)
	}
	println('length of random array is $arr.len')
	println('before quick sort whether array is sorted: ${is_sorted(arr)}')
	quick_sort(mut arr, 0, arr.len-1)
	println('after quick sort whether array is sorted: ${is_sorted(arr)}')
}

fn quick_sort(arr mut []int, l int, r int) {
	if l>=r { return }
	mut sep := l  // what is sep: [...all_value<arr[sep]...sep...all_value>=arr[sep]...]
	for i in l+1..r+1 {
		if arr[i] < arr[l] {
			sep++
			swap(mut arr, i, sep)
		}
	}
	swap(mut arr, l, sep)
	quick_sort(mut arr, l, sep-1)
	quick_sort(mut arr, sep+1, r)
}

[inline]
fn swap(arr mut []int, i int, j int) {
	temp := arr[i]
	arr[i] = arr[j]
	arr[j] = temp
}

fn is_sorted(arr []int) bool {
	for i in 0..arr.len-1 {
		if arr[i] > arr[i+1] {
			return false
		}
	}
	return true
}
