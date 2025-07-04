module lockfree

import sync.stdatomic as _

// Define cache line size to prevent false sharing between CPU cores
const cache_line_size = 64

// next_power_of_two calculates the smallest power of two >= n
@[inline]
fn next_power_of_two(n u32) u32 {
	if n == 0 {
		return 1
	}
	mut x := n - 1

	// Efficient bit manipulation to find next power of two
	x |= x >> 1
	x |= x >> 2
	x |= x >> 4
	x |= x >> 8
	x |= x >> 16
	return x + 1
}
