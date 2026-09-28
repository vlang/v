import time

struct DropSignal {
	done chan int
}

fn (mut value DropSignal) drop() {
	value.done <- 1
}

fn make_drop_signal(done chan int) DropSignal {
	return DropSignal{ done: done }
}

fn make_drop_signal_array(done chan int) []DropSignal {
	return [DropSignal{ done: done }]
}

fn make_drop_signal_map(done chan int) map[string]DropSignal {
	return {
		'value': DropSignal{ done: done }
	}
}

fn make_drop_signal_tuple(done chan int) (DropSignal, []int) {
	return DropSignal{ done: done }, [1, 2, 3]
}

fn test_discarded_spawn_drops_return_value() {
	done := chan int{cap: 4}
	spawn make_drop_signal(done)
	spawn make_drop_signal_array(done)
	spawn make_drop_signal_map(done)
	spawn make_drop_signal_tuple(done)
	for _ in 0 .. 4 {
		select {
			value := <-done {
				assert value == 1
			}
			5 * time.second {
				assert false
			}
		}
	}
}
