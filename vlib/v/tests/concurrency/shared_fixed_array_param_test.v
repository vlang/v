// A `shared [N]T` parameter is passed as its lock wrapper and read through the wrapper's
// `val`, a fixed array value. Indexing, compound assignment, iteration, slicing and whole
// copies of it must use that array, not dereference it once more as if it were a `mut`
// fixed-array parameter (`(*a->val)[i]`). Values are copied out of the locks before the
// asserts, so a failing assert does not leave a lock held for the next test.
struct Point {
mut:
	x int
	y int
}

enum Small as u8 {
	a
	b
	c
}

fn sum3(a [3]int) int {
	return a[0] + a[1] + a[2]
}

fn set_then_get(shared a [2]int) int {
	lock a {
		a[1] = 5
	}
	mut got := 0
	rlock a {
		got = a[1]
	}
	return got
}

fn compound(shared a [3]int) {
	lock a {
		a[1] += 10
		a[2] *= 3
		a[0] <<= 2
	}
}

fn describe(shared a [3]int) string {
	mut out := []string{}
	rlock a {
		for e in a {
			out << e.str()
		}
		for i, e in a {
			out << '${i}:${e}'
		}
		out << a.len.str()
		out << sum3(a).str()
		out << a[1..].str()
		out << '${a}'
		out << (a == [4, 12, 9]!).str()
		out << (12 in a).str()
	}
	return out.join(',')
}

fn forward(shared a [3]int) int {
	compound(shared a)
	mut got := 0
	rlock a {
		got = a[2]
	}
	return got
}

fn nested(shared m [2][2]int) {
	lock m {
		m[1][0] = 7
		m[0][1] += 3
	}
}

fn points(shared ps [2]Point) {
	lock ps {
		ps[1].x = 4
		ps[0].y += 2
	}
}

fn smalls(shared es [3]Small) {
	lock es {
		es[0] = .c
		es[2] = .b
	}
}

fn copy_out(shared a [3]int) [3]int {
	mut c := [0, 0, 0]!
	rlock a {
		c = a
	}
	return c
}

fn assign_whole(shared a [3]int) {
	lock a {
		a = [7, 8, 9]!
	}
}

fn bump(shared a [2]int, n int) {
	for _ in 0 .. n {
		lock a {
			a[0]++
			a[1] += 2
		}
	}
}

fn test_index_assign_and_read() {
	shared x := [1, 2]!
	assert set_then_get(shared x) == 5
	mut got := [0, 0]!
	rlock x {
		got = x
	}
	assert got == [1, 5]!
}

fn test_compound_assign_iteration_and_slices() {
	shared x := [1, 2, 3]!
	compound(shared x)
	mut got := [0, 0, 0]!
	rlock x {
		got = x
	}
	assert got == [4, 12, 9]!
	assert describe(shared x) == '4,12,9,0:4,1:12,2:9,3,25,[12, 9],[4, 12, 9],true,true'
	assert forward(shared x) == 27
}

fn test_nested_struct_and_enum_elements() {
	shared m := [[1, 2]!, [3, 4]!]!
	nested(shared m)
	mut got_m := [[0, 0]!, [0, 0]!]!
	rlock m {
		got_m = m
	}
	assert got_m == [[1, 5]!, [7, 4]!]!
	shared ps := [Point{1, 1}, Point{2, 2}]!
	points(shared ps)
	mut got_ps := [Point{}, Point{}]!
	rlock ps {
		got_ps = ps
	}
	assert got_ps[0] == Point{1, 3}
	assert got_ps[1] == Point{4, 2}
	shared es := [Small.a, .a, .a]!
	smalls(shared es)
	mut got_es := [Small.a, .a, .a]!
	rlock es {
		got_es = es
	}
	assert got_es == [Small.c, .a, .b]!
}

fn test_whole_array_copy_and_assign() {
	shared x := [1, 2, 3]!
	assert copy_out(shared x) == [1, 2, 3]!
	assign_whole(shared x)
	assert copy_out(shared x) == [7, 8, 9]!
}

fn test_threads_share_the_array() {
	shared x := [0, 0]!
	mut threads := []thread{}
	for _ in 0 .. 4 {
		threads << spawn bump(shared x, 100)
	}
	threads.wait()
	mut got := [0, 0]!
	rlock x {
		got = x
	}
	assert got == [400, 800]!
}
