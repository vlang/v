module arrays

import sync
import math
import runtime

// run_parallel lets the user run an array of input with a
// user provided function in parallel. It limits the number of
// worker threads. The function makes use of channels to achieve it
// The worker function is supposed to not return any error or a return value
// Any error handling should have happened within the worker function
// Example: arrays.run_parallel([1, 2, 3, 4, 5], 2, fn (i) { println(i) })
pub fn run_parallel[T](input []T, max_workers int, worker fn (T) !) {
	mut wg := sync.new_waitgroup()
	wg.add(input.len)
	ch := chan T{}

	// create workers to handle the load
	workers := math.min(math.max(1, max_workers), runtime.nr_cpus())
	for _ in 0 .. workers {
		spawn fn [ch, worker, mut wg] [T]() {
			for {
				task := <-ch or { break }
				worker(task) or {
					eprintln('error executing parallel task...')
					return
				}
				wg.done()
			}
		}()
	}

	// put the input into the channel
	for i in input {
		ch <- i
	}

	// wait for all tasks to complete
	wg.wait()
	ch.close()
}

struct Task[T] {
	idx  int
	task T
}

// map_parallel lets the user run an array of input with a
// user provided function in parallel. It limits the number of
// worker threads to max number of cpus. The function makes use of channels to achieve it
// The worker function can return a value. The returning array maintains the input order
// Any error handling should have happened within the worker function
// Example: squares := arrays.map_parallel([1, 2, 3, 4, 5], 2, fn (i) { return i * i })
pub fn map_parallel[T, R](input []T, max_workers int, worker fn (T) R) []R {
	mut wg := sync.new_waitgroup()
	mut output := []R{cap: input.len}
	mut output_ref := &output
	wg.add(input.len)

	ch := chan Task[T]{}

	// create workers to handle the load
	workers := math.min(math.max(1, max_workers), runtime.nr_cpus())
	for _ in 0 .. workers {
		spawn fn [ch, worker, mut wg, mut output_ref] [T, R]() {
			for {
				task := <-ch or { break }
				output_ref << worker(task.task)
				wg.done()
			}
		}()
	}

	// put the input into the channel
	for idx, inp in input {
		task := Task[T]{idx, inp}
		ch <- task
	}

	// wait for all tasks to complete
	wg.wait()
	ch.close()
	return output
}
