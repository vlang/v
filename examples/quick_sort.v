import rand

const (
	gen_len = 1000 // how many random numbers to generate
	gen_max = 10000 // max of the generated numbers
)

fn main() {
	mut arr := []int{}
	for _ in 0 .. gen_len {
		arr << rand.intn(gen_max) or { 0 }
	}
	println('length of random array is $arr.len')
	println('before quick sort whether array is sorted: ${is_sorted<int>(arr)}')
	quick_sort<int>(mut arr, 0, arr.len - 1)
	println('after quick sort whether array is sorted: ${is_sorted<int>(arr)}')
}

fn quick_sort<T>(mut arr []T, l int, r int) {
	if l >= r {
		return
	}
	mut sep := l // what is sep: [...all_value<arr[sep]...sep...all_value>=arr[sep]...]
	for i in l + 1 .. r + 1 {
		if arr[i] < arr[l] {
			sep++
			arr[i], arr[sep] = arr[sep], arr[i]
		}
	}
	arr[l], arr[sep] = arr[sep], arr[l]
	quick_sort<T>(mut arr, l, sep - 1)
	quick_sort<T>(mut arr, sep + 1, r)
}

fn is_sorted<T>(arr []T) bool {
	for i in 0 .. arr.len - 1 {
		if arr[i] > arr[i + 1] {
			return false
		}
	}
	return true
}
