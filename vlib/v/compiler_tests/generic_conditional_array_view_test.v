struct ConditionalViewPoint {
	x f64
	y f64
}

@[unsafe]
fn conditional_array_view[T](data &T, length int, capacity int) []T {
	return unsafe {
		array{
			data:         data
			len:          length
			cap:          capacity
			element_size: sizeof(T)
			flags:        .nogrow | .nofree
		}
	}
}

fn conditional_point_view(inline bool) {
	mut storage := [4]ConditionalViewPoint{}
	mut points := if inline {
		unsafe { conditional_array_view(&storage[0], 0, storage.len) }
	} else {
		[]ConditionalViewPoint{cap: storage.len}
	}
	points << ConditionalViewPoint{1.5, 2.5}
	points << ConditionalViewPoint{3.5, 4.5}
	assert points[0] == ConditionalViewPoint{1.5, 2.5}
	assert points.last() == ConditionalViewPoint{3.5, 4.5}
	for point in points {
		assert point.y == point.x + 1
	}
}

fn test_inferred_generic_array_view_keeps_struct_elements_in_conditional() {
	conditional_point_view(true)
	conditional_point_view(false)
}

fn test_inferred_generic_array_view_keeps_string_elements_in_conditional() {
	mut storage := [4]string{}
	for inline in [true, false] {
		mut values := if inline {
			unsafe { conditional_array_view(&storage[0], 0, storage.len) }
		} else {
			[]string{cap: storage.len}
		}
		values << 'one'
		values << 'two'
		assert values[0] == 'one'
		assert values.last() == 'two'
	}
}
