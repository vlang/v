struct St {
mut:
	i int
}

fn test_lock_expr() {
	shared xx := St{
		i: 173
	}
	shared y := St{
		i: -57
	}
	mut m := 0
	m = lock y {
		y.i
	}
	n := rlock xx {
		xx.i
	}
	assert m == -57
	assert n == 173
}

struct Abc {
mut:
	a f64
}

fn test_multi_objects() {
	shared x := Abc{
		a: 12.5
	}
	shared y := Abc{
		a: -7.5
	}
	shared z := Abc{
		a: 13.125
	}
	a, b, c := rlock z, x, y {
		y.a, z.a, x.a
	}
	assert a == -7.5
	assert b == 13.125
	assert c == 12.5
}

fn (mut st Abc) getvals(mut a Abc, mut b Abc) (f64, f64, f64) {
	return a.a, st.a, b.a
}

fn test_mult_ret_method() {
	shared x := Abc{
		a: 12.5
	}
	shared y := Abc{
		a: -7.5
	}
	shared z := Abc{
		a: 13.125
	}
	a, b, c := lock z, x, y {
		z.getvals(mut x, mut y)
	}
	assert a == 12.5
	assert b == 13.125
	assert c == -7.5
}

fn test_shared_lock_map_index_expr() {
	shared m := {
		'qwe': 'asd'
	}
	for key in ['fdf', 'qwe'] {
		v := rlock m {
			m[key] or { 'key not found' }
		}
		if key == 'qwe' {
			assert v == 'asd'
		} else {
			assert v == 'key not found'
		}
	}
}

fn test_shared_lock_array_index_expr() {
	shared a := [-12.5, 6.25]
	for i in -2 .. 4 {
		v := rlock a {
			a[i] or { 23.75 }
		}
		if i == 0 {
			assert v == -12.5
		} else if i == 1 {
			assert v == 6.25
		} else {
			assert v == 23.75
		}
	}
}

fn test_shared_lock_array_slice_expr() {
	// Test slicing shared arrays in rlock expressions (fixes issue #26663)
	// Note: slicing a shared array automatically clones for safety,
	// since a slice is a view over the same memory buffer.
	shared a := ['a', 'b', 'c', 'd']

	// Basic slice - implicit clone happens (safe independent copy)
	slice1 := lock {
		a[1..3]
	}
	assert slice1.len == 2
	assert slice1[0] == 'b'
	assert slice1[1] == 'c'

	// Full slice
	slice2 := lock {
		a[..]
	}
	assert slice2.len == 4
	assert slice2 == ['a', 'b', 'c', 'd']

	// Slice from start
	slice3 := lock {
		a[..2]
	}
	assert slice3.len == 2
	assert slice3[0] == 'a'
	assert slice3[1] == 'b'

	// Slice to end
	slice4 := lock {
		a[2..]
	}
	assert slice4.len == 2
	assert slice4[0] == 'c'
	assert slice4[1] == 'd'

	// Test with int array
	shared arr := [1, 2, 3, 4, 5]
	slice5 := lock {
		arr[1..4]
	}
	assert slice5.len == 3
	assert slice5[0] == 2
	assert slice5[1] == 3
	assert slice5[2] == 4

	// Explicit clone - same behavior, no warning
	slice6 := lock {
		a[1..3].clone()
	}
	assert slice6 == ['b', 'c']

	// Unsafe block - get a view (user takes responsibility)
	slice7 := lock {
		unsafe { a[1..3] }
	}
	assert slice7 == ['b', 'c']
}

struct DummySt {
	a int
}

fn test_shared_lock_chan_rec_expr() {
	ch := chan int{cap: 10}
	shared st := DummySt{}
	ch <- 7
	ch <- -13
	ch.close()
	for i in 0 .. 3 {
		v := rlock st {
			<-ch or { -17 }
		}
		if i == 0 {
			assert v == 7
		} else if i == 1 {
			assert v == -13
		} else {
			assert v == -17
		}
	}
}
