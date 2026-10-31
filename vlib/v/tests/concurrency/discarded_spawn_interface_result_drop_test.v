import time

interface ReturnedOwned {
	marker() int
}

struct InterfaceDropSignal {
	done chan int
}

fn (value InterfaceDropSignal) marker() int {
	return 1
}

fn (mut value InterfaceDropSignal) drop() {
	value.done <- 1
}

fn make_interface_drop_signal(done chan int) ReturnedOwned {
	return InterfaceDropSignal{ done: done }
}

fn test_discarded_spawn_drops_boxed_interface_result() {
	done := chan int{cap: 1}
	spawn make_interface_drop_signal(done)
	select {
		value := <-done {
			assert value == 1
		}
		5 * time.second {
			assert false
		}
	}
}
