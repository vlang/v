// some tests on recursion, using fibonacci sequence etc

// use small values here, to not slow-down tests execution
const (
	num         = 10 // number to use
	fib_for_num = 55 // expected result
)
// calculate fibonacci number in a recursive way
// attention, this is very slow and could lead to stack overflow
fn fib_recursive(n int) u64 {
	if n < 1 {
		return 0
	} else if n == 1 {
		return u64(n)
	}
	return fib_recursive(n - 1) + fib_recursive(n - 2)
}

fn test_fib_recursive() {
	println(@FN + ' ' + 'Test fib_recursive...')
	result := fib_recursive(num)
	assert result == fib_for_num
}

// calculate fibonacci number in a recursive way
// but with memoization (caching of previous values), 
// so faster than typical recursive only approach but more momory consuming
// note that cache here is used from outer scope, as a mutable variable ...
fn (mut cache ValuesCache) fib_recursive_memoized_external_cache(n int) u64 {
	if n < 1 {
		return 0
	} else if n == 1 {
		cache.nums[n] = u64(n)
	}
	if cache.nums[n] != 0 {
		return cache.nums[n]
	}
	cache.nums[n] = cache.fib_recursive_memoized_external_cache(n - 1) + cache.fib_recursive_memoized_external_cache(n - 2)
	return cache.nums[n]
}

struct ValuesCache {
mut:
	nums []u64
}

fn test_fib_recursive_memoized_external_cache() {
	println(@FN + ' ' + 'Test fibonacci recursive with an external cache of values...')
	mut cache := ValuesCache{
		nums: [u64(0)].repeat(num + 1)
	}
	result := cache.fib_recursive_memoized_external_cache(num)
	assert result == fib_for_num
}


// calculate fibonacci number in a recursive way
// but with memoization (caching of previous values), 
// so faster than typical recursive only approach but more momory consuming
// note that cache here is passed as a mutable argument ...
fn fib_recursive_memoized(n int, mut cache []u64) u64 {
	if n < 1 {
		return 0
	} else if n == 1 {
		cache.nums[n] = n
	}
	if cache.nums[n] != 0 {
		return cache.nums[n]
	}
	cache.nums[n] = fib_recursive_memoized(n - 1, cache) + fib_recursive_memoized(n - 2, cache)
	return cache.nums[n]
}

fn test_fib_recursive_memoized() {
	println(@FN + ' ' + 'Test fibonacci recursive with a given cache of mutable values...')
	mut cache := [u64(0)].repeat(num + 1)
	result := fib_recursive_memoized(num, cache)
	assert result == fib_for_num
}
