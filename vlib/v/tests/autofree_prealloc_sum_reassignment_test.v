// vtest vflags: -autofree -prealloc -gc none
import time

struct Initial {
	done chan int
}

fn (initial &Initial) free() {
	initial.done <- 1
}

struct Replacement {
	done  chan int
	value int
}

fn (replacement &Replacement) free() {
	replacement.done <- replacement.value
}

type Value = Initial | Replacement

fn replace_preallocated_sum(done chan int) {
	mut value := Value(Initial{ done: done })
	value = Value(Replacement{ done: done, value: 17 })
	assert done.len == 1
	assert <-done == 1
	assert value is Replacement
	if value is Replacement {
		assert value.value == 17
	}
	value = Value(Initial{ done: done })
}

fn test_autofree_reassigns_preallocated_sum_box() {
	done := chan int{cap: 2}
	replace_preallocated_sum(done)
	assert done.len > 0
	assert <-done == 17
}

fn detached_preallocated_sum(done chan int) Value {
	return Replacement{ done: done, value: 31 }
}

fn test_autofree_drops_detached_preallocated_sum_box() {
	done := chan int{cap: 1}
	spawn detached_preallocated_sum(done)
	select {
		value := <-done {
			assert value == 31
		}
		5 * time.second {
			assert false
		}
	}
	// The destructor signals before its box and the thread's result are released.
	time.sleep(200 * time.millisecond)
}
