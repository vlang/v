struct SelectAssertPoint {
	x int
}

fn test_select_only_else_assert_message_uses_branch_locals() {
	select {
		else {
			values := [1, 2]
			assert values.len == 2, values.str()
			point := SelectAssertPoint{3}
			assert point.x == 3, '${point}'
		}
	}
}

fn test_select_else_assert_message_uses_branch_locals() {
	empty := chan []int{cap: 1}
	select {
		_ := <-empty {
			assert false, 'empty channel cannot receive'
		}
		else {
			values := [4, 5]
			assert values == [4, 5], '${values}'
			point := SelectAssertPoint{6}
			assert point.x == 6, point.str()
		}
	}
}

fn test_select_receive_assert_message_uses_received_and_local_values() {
	channel := chan []int{cap: 1}
	channel <- [7, 8]
	select {
		found := <-channel {
			assert found.len == 2, found.str()
			values := [found[0], found[1], 9]
			assert values.len == 3, '${values}'
		}
		else {
			assert false, 'buffered channel must receive'
		}
	}
}

fn test_select_send_assert_message_uses_branch_local() {
	channel := chan int{cap: 1}
	select {
		channel <- 10 {
			values := [11, 12]
			assert values.len == 2, values.str()
		}
		else {
			assert false, 'buffered channel must send'
		}
	}
	assert <-channel == 10
}
