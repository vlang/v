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
