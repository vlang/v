module sketches

import hash

const max_counter = u64(0xffffffffffffffff)

// CountMin is a probabilistic frequency counter. It never underestimates a
// frequency, but may overestimate it because of bucket collisions. Counters
// saturate instead of wrapping, so that promise holds for long streams too.
pub struct CountMin {
pub:
	width int
	depth int
	seed  u64
mut:
	counters [][]u64
}

@[ignore_overflow]
fn row_seed(seed u64, row int) u64 {
	return seed + u64(row) * 0x9e3779b97f4a7c15
}

fn saturating_add(a u64, b u64) u64 {
	if a > max_counter - b {
		return max_counter
	}
	return a + b
}

pub fn new_countmin(width int, depth int, seed u64) !CountMin {
	if width < 1 || depth < 1 {
		return error('width and depth must be positive')
	}
	mut counters := [][]u64{len: depth}
	for i in 0 .. depth {
		counters[i] = []u64{len: width, init: 0}
	}
	return CountMin{
		width:    width
		depth:    depth
		seed:     seed
		counters: counters
	}
}

pub fn (mut c CountMin) add(value string, count u64) {
	for row in 0 .. c.depth {
		h := hash.sum64_string(value, row_seed(c.seed, row))
		idx := int(h % u64(c.width))
		c.counters[row][idx] = saturating_add(c.counters[row][idx], count)
	}
}

pub fn (c &CountMin) estimate(value string) u64 {
	mut best := max_counter
	for row in 0 .. c.depth {
		h := hash.sum64_string(value, row_seed(c.seed, row))
		v := c.counters[row][int(h % u64(c.width))]
		if v < best {
			best = v
		}
	}
	return best
}
