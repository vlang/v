struct St[T] {
mut:
	a []T
}

fn (s St[T]) peek() ?T {
	if s.a.len > 0 {
		return s.a[0]
	} else {
		return none
	}
}

fn (mut s St[T]) peek_or_default() T {
	return s.peek() or { T{} }
}

fn (mut s St[T]) push(e T) {
	s.peek() or { T{} }
	x := s.peek() or { T{} } // this is deliberate
	$if x is $array {
		dump(x)
		if s.a.len > 0 {
			assert x.len > 0
		} else {
			assert x.len == 0
		}
	}
	$if x is $int {
		dump(x)
		if s.a.len > 0 {
			assert x == 2
		} else {
			assert x == 0
		}
	}
	$if x is $map {
		dump(x)
		if s.a.len > 0 {
			assert x == {
				'abc': u8(4)
				'def': 7
			}
		}
	}
	s.a << e
}

fn test_ints() {
	mut s := St[int]{}
	r := s.peek_or_default()
	s.push(2)
	s.push(3)
	s.push(99)
	assert s.a == [2, 3, 99]
	dump(s)
}

fn test_array_of_ints() {
	mut s := St[[]int]{}
	r := s.peek_or_default()
	s.push([2, 3, 5])
	s.push([]int{})
	s.push([10, 20, 99])
	s.push([55, 88])
	assert s.a == [[2, 3, 5], []int{}, [10, 20, 99], [55, 88]]
	dump(s)
}

fn test_maps_of_u8s() {
	mut s := St[map[string]u8]{}
	r := s.peek_or_default()
	s.push({
		'abc': u8(4)
		'def': 7
	})
	s.push({
		'xyz': u8(13)
	})
	s.push({
		'zzz': u8(99)
	})
	assert s.a == [{
		'abc': u8(4)
		'def': 7
	}, {
		'xyz': u8(13)
	}, {
		'zzz': u8(99)
	}]
	dump(s)
}
