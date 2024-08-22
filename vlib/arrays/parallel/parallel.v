module parallel

import sync
import math
import runtime

// run lets the user run an array of input with a
// user provided function in parallel. It limits the number of
// worker threads to min(num_workers, num_cpu)
// The function aborts if an error is encountered.
// Example: parallel.run([1, 2, 3, 4, 5], 2, fn (i) { println(i) })
pub fn run[T](input []T, max_workers int, worker fn (T)) {
	mut wg := sync.new_waitgroup()
	wg.add(input.len)
	ch := chan T{}
	defer { ch.close() }

	// create workers to handle the load
	workers := math.min(math.max(1, max_workers), runtime.nr_cpus())
	for _ in 0 .. workers {
		spawn fn [ch, worker, mut wg] [T]() {
			for {
				task := <-ch or { break }
				worker(task)
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
}

struct Task[T, R] {
	idx    int
	task   T
	result R
}

// amap lets the user run an array of input with a
// user provided function in parallel. It limits the number of
// worker threads to max number of cpus.
// The worker function can return a value. The returning array maintains the input order.
// Any error handling should have happened within the worker function.
// Example: squares := parallel.amap([1, 2, 3, 4, 5], 2, fn (i) { return i * i })
pub fn amap[T, R](input []T, max_workers int, worker fn (T) R) []R {
	mut wg := sync.new_waitgroup()
	mut tasks := []Task[T, R]{cap: input.len}
	// to pass the result array to closure
	mut tasks_ref := &tasks
	wg.add(input.len)

	ch := chan Task[T, R]{}
	defer { ch.close() }

	// create workers to handle the load
	workers := math.min(math.max(1, max_workers), runtime.nr_cpus())
	for _ in 0 .. workers {
		spawn fn [ch, worker, mut wg, mut tasks_ref] [T, R]() {
			for {
				mut task := <-ch or { break }
				tasks_ref << Task[T, R]{
					idx:    task.idx
					task:   task.task
					result: worker(task.task)
				}
				wg.done()
			}
		}()
	}

	// put the input into the channel
	for idx, inp in input {
		task := Task[T, R]{
			idx:  idx
			task: inp
		}
		ch <- task
	}

	// wait for all tasks to complete
	wg.wait()
	tasks.sort(a.idx < b.idx)
	return tasks.map(it.result)
}
