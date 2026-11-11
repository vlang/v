// Reads `a[i]` in `for i in low .. a.len` over an immutable `a` are emitted
// without a bounds check. These loops must still see the right elements.

struct Point {
	x int
	y int
}

fn param_sum(a []int) int {
	mut s := 0
	for i in 0 .. a.len {
		s += a[i]
	}
	return s
}

fn point_sum(points []Point) int {
	mut s := 0
	for i in 1 .. points.len {
		s += points[i].x * points[i].y
	}
	return s
}

fn count_byte(s string, c u8) int {
	mut n := 0
	for i in 0 .. s.len {
		if s[i] == c {
			n++
		}
	}
	return n
}

fn pair_products(a []int) []int {
	mut out := []int{}
	for i in 0 .. a.len {
		for j in i .. a.len {
			out << a[i] * a[j]
		}
	}
	return out
}

fn deferred_reads(a []int) []fn () int {
	mut fns := []fn () int{}
	for i in 0 .. a.len {
		fns << fn [a, i] () int {
			return a[i]
		}
	}
	return fns
}

fn generic_sum[T](a []T) T {
	mut s := T(0)
	for i in 0 .. a.len {
		s += a[i]
	}
	return s
}

fn test_range_len_reads() {
	assert param_sum([]int{}) == 0
	assert param_sum([4, 5, 6]) == 15
	assert point_sum([Point{1, 2}, Point{3, 4}, Point{5, 6}]) == 42
	assert count_byte('', `x`) == 0
	assert count_byte('xaxbx', `x`) == 3
	assert pair_products([1, 2, 3]) == [1, 2, 3, 4, 6, 9]
	assert generic_sum([1.5, 2.5]) == 4.0
	assert generic_sum([u8(1), 2, 3]) == 6
	local := [7, 8, 9]
	mut sum := 0
	for i in 0 .. local.len {
		sum += local[i]
	}
	assert sum == 24
	reads := deferred_reads([10, 20, 30])
	assert reads.map(it()) == [10, 20, 30]
}

fn test_range_len_reads_of_a_slice() {
	a := [1, 2, 3, 4, 5]
	tail := a[2..]
	mut got := []int{}
	for i in 0 .. tail.len {
		got << tail[i]
	}
	assert got == [3, 4, 5]
}
