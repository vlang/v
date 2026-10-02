fn select_fixed_row() [2]int {
	return [31, 47]!
}

struct SelectFixedArrayTrace {
mut:
	calls int
}

fn select_fixed_array_index(mut trace SelectFixedArrayTrace) int {
	trace.calls++
	return 1
}

fn test_select_fixed_arrays_copy_send_and_receive_values() {
	channel := chan [2]int{cap: 1}
	mut values := [11, 13]!
	select {
		channel <- values {
		}
	}
	values[0] = 17
	select {
		row := <-channel {
			assert row == [11, 13]!
		}
	}
	select {
		channel <- select_fixed_row() {
		}
	}
	select {
		row := <-channel {
			assert row == [31, 47]!
		}
	}
	channel <- [19, 23]!
	mut rows := [[0, 0]!, [0, 0]!]
	mut trace := SelectFixedArrayTrace{}
	select {
		rows[select_fixed_array_index(mut trace)] = <-channel {
			assert trace.calls == 1
			assert rows == [[0, 0]!, [19, 23]!]
		}
	}
}

fn test_select_fixed_arrays_convert_to_dynamic_array_destination() {
	channel := chan [2]int{cap: 1}
	channel <- [29, 31]!
	mut values := []int{}
	select {
		values = <-channel {
		}
	}
	assert values == [29, 31]
}

fn select_mutable_fixed_row(channel chan [2]int, mut values [2]int) {
	select {
		values = <-channel {
		}
	}
}

fn select_retained_fixed_row(channel chan [2]int) &[2]int {
	mut values := [0, 0]!
	select {
		values = <-channel {
		}
	}
	// Taking a fixed-array address requires unsafe; its escaping storage is retained.
	return unsafe { &values }
}

fn test_select_fixed_array_assignment_preserves_mutable_and_retained_storage() {
	channel := chan [2]int{cap: 1}
	channel <- [37, 41]!
	mut values := [0, 0]!
	select_mutable_fixed_row(channel, mut values)
	assert values == [37, 41]!
	channel <- [43, 47]!
	retained := select_retained_fixed_row(channel)
	assert *retained == [43, 47]!
}
