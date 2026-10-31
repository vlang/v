import time

struct RecursiveDropSignal {
	done chan int
}

fn (mut signal RecursiveDropSignal) drop() {
	signal.done <- 1
}

struct RecursiveDropNode {
	children []RecursiveDropNode
	signal   RecursiveDropSignal
	worker   thread RecursiveDropSignal
}

fn make_recursive_drop_signal(done chan int) RecursiveDropSignal {
	return RecursiveDropSignal{ done: done }
}

fn make_recursive_drop_node(done chan int) RecursiveDropNode {
	return RecursiveDropNode{
		signal:   RecursiveDropSignal{ done: done }
		children: [RecursiveDropNode{
			signal: RecursiveDropSignal{ done: done }
			worker: spawn make_recursive_drop_signal(done)
		}]
	}
}

fn make_recursive_drop_node_tuple(done chan int) (RecursiveDropNode, int) {
	return make_recursive_drop_node(done), 1
}

fn test_discarded_spawn_drops_recursive_result_and_nested_thread() {
	done := chan int{cap: 6}
	for _ in 0 .. 2 {
		spawn make_recursive_drop_node(done)
	}
	for _ in 0 .. 6 {
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

fn test_discarded_spawn_drops_recursive_multi_return_result() {
	done := chan int{cap: 3}
	spawn make_recursive_drop_node_tuple(done)
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
