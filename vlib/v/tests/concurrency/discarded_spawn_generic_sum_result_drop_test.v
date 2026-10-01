import time

struct GenericSumDropSignal {
	done chan int
}

fn (mut signal GenericSumDropSignal) drop() {
	signal.done <- 1
}

type GenericDropOutcome[T] = T | int

fn make_generic_drop_outcome(done chan int) GenericDropOutcome[GenericSumDropSignal] {
	return GenericSumDropSignal{
		done: done
	}
}

fn test_discarded_spawn_drops_generic_sum_variant() {
	done := chan int{cap: 1}
	spawn make_generic_drop_outcome(done)
	select {
		value := <-done {
			assert value == 1
		}
		5 * time.second {
			assert false
		}
	}
}
