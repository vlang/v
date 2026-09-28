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

fn test_discarded_spawn_drops_return_value() {
	done := chan int{cap: 3}
	spawn make_drop_signal(done)
	spawn make_drop_signal_array(done)
	spawn make_drop_signal_map(done)
	for _ in 0 .. 3 {
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
