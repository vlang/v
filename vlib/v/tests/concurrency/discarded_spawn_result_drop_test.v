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

struct GenericDropSignalBox[T] {
	done chan int
}

fn (mut value GenericDropSignalBox[T]) drop() {
	value.done <- 1
}

fn make_generic_drop_signal_box(done chan int) GenericDropSignalBox[int] {
	return GenericDropSignalBox[int]{ done: done }
}

struct DiscardedSpawnLogicCounter {
mut:
	count int
}

fn mark_discarded_spawn_logic(mut counter DiscardedSpawnLogicCounter) bool {
	counter.count++
	return true
}

fn test_discarded_spawn_drops_return_value() {
	done := chan int{cap: 5}
	spawn make_drop_signal(done)
	spawn make_drop_signal_array(done)
	spawn make_drop_signal_map(done)
	spawn make_drop_signal_tuple(done)
	spawn make_generic_drop_signal_box(done)
	for _ in 0 .. 5 {
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

fn test_discarded_spawn_drops_results_under_prefix_expression() {
	done := chan int{cap: 2}
	_ := !((spawn make_drop_signal(done)) == (spawn make_drop_signal(done)))
	for _ in 0 .. 2 {
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

fn test_discarded_spawn_drops_results_under_logical_expression() {
	done := chan int{cap: 4}
	mut counter := DiscardedSpawnLogicCounter{}
	_ := ((spawn make_drop_signal(done)) != (spawn make_drop_signal(done)))
		&& mark_discarded_spawn_logic(mut counter)
	_ := ((spawn make_drop_signal(done)) == (spawn make_drop_signal(done)))
		|| mark_discarded_spawn_logic(mut counter)
	assert counter.count == 2
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

fn test_discarded_spawn_drops_results_in_conditional_condition() {
	done := chan int{cap: 5}
	_ := if (spawn make_drop_signal(done)) == (spawn make_drop_signal(done)) {
		1
	} else {
		2
	}
	_ := if (spawn make_drop_signal(done)) == (spawn make_drop_signal(done)) {
		spawn make_drop_signal(done)
	} else {
		spawn make_drop_signal(done)
	}
	for _ in 0 .. 5 {
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
