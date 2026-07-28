module sync

const select_handoff_pairs = 64
const select_handoff_timeout = i64(500_000_000)

fn select_send_after_start(ch &Channel, start chan bool, results chan bool, value int) {
	_ := <-start
	mut channels := [ch]
	directions := [Direction.push]
	mut sent := value
	mut objects := [voidptr(&sent)]
	idx := channel_select(mut channels, directions, mut objects, select_handoff_timeout)
	results <- (idx == 0)
}

fn select_receive_after_start(ch &Channel, start chan bool, results chan bool, expected int) {
	_ := <-start
	mut channels := [ch]
	directions := [Direction.pop]
	mut received := 0
	mut objects := [voidptr(&received)]
	idx := channel_select(mut channels, directions, mut objects, select_handoff_timeout)
	results <- (idx == 0 && received == expected)
}

fn test_opposing_unbuffered_selects_do_not_miss_waiting_transition() {
	start := chan bool{cap: select_handoff_pairs * 2}
	results := chan bool{cap: select_handoff_pairs * 2}
	for i in 0 .. select_handoff_pairs {
		ch := new_channel[int](0)
		spawn select_send_after_start(ch, start, results, i + 1)
		spawn select_receive_after_start(ch, start, results, i + 1)
	}
	for _ in 0 .. select_handoff_pairs * 2 {
		start <- true
	}
	for _ in 0 .. select_handoff_pairs * 2 {
		assert <-results
	}
}
