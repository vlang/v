module main

fn sum_cache(n int) int {
	cache := []int{len: n}
	mut sum := 0
	for i := 0; i < cache.len; i++ {
		sum += cache[i]
	}
	return sum
}

fn main() {
	println(sum_cache(256))
}
