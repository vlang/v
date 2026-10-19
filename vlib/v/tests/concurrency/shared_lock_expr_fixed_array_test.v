// A `lock`/`rlock` expression whose value is a fixed array (`v := rlock s { s }`) yields a
// copy of the array. C arrays can be neither assigned nor returned from a statement
// expression, so the value travels in a one-field struct, as for an `if` expression.
enum Small as u8 {
	a
	b
	c
}

struct Holder {
	vals [3]int
}

fn sum3(a [3]int) int {
	return a[0] + a[1] + a[2]
}

fn snapshot(shared s [3]int) [3]int {
	return rlock s {
		s
	}
}

fn test_lock_expr_fixed_array_decl() {
	shared s := [1, 2, 3]!
	v := rlock s {
		s
	}
	assert v == [1, 2, 3]!
	lock s {
		s[0] = 10
	}
	// `v` is a copy, not a view of the shared storage.
	assert v == [1, 2, 3]!
	w := lock s {
		s[1] = 20
		s
	}
	assert w == [10, 20, 3]!
}

fn test_lock_expr_fixed_array_assign_args_and_fields() {
	shared s := [1, 2, 3]!
	mut v := [0, 0, 0]!
	v = rlock s {
		s
	}
	assert v == [1, 2, 3]!
	assert sum3(rlock s {
		s
	}) == 6
	assert snapshot(shared s) == [1, 2, 3]!
	h := Holder{
		vals: rlock s {
			s
		}
	}
	assert h.vals == [1, 2, 3]!
	text := '${rlock s {
		s
	}}'
	assert text == '[1, 2, 3]'
}

fn test_lock_expr_fixed_array_elements() {
	shared es := [Small.a, .b, .c]!
	e := rlock es {
		es
	}
	assert e == [Small.a, .b, .c]!
	shared m := [[1, 2]!, [3, 4]!]!
	n := rlock m {
		m
	}
	assert n == [[1, 2]!, [3, 4]!]!
	row := rlock m {
		m[1]
	}
	assert row == [3, 4]!
}

fn test_lock_expr_fixed_arrays_multi_return() {
	shared s := [1, 2, 3]!
	shared t := [4, 5, 6]!
	a, b := rlock s, t {
		s, t
	}
	assert a == [1, 2, 3]!
	assert b == [4, 5, 6]!
}
