module parallel

import sync
import runtime

// Params contains the optional parameters that can be passed to `run` and `amap`.
@[params]
pub struct Params {
pub mut:
	workers int // 0 by default, so that VJOBS will be used, through runtime.nr_jobs()
}

fn limited_workers(max_workers int, ilen int) int {
	// create a limited amount of workers to handle the load
	workers := if max_workers != 0 { max_workers } else { runtime.nr_jobs() }
	if ilen < workers {
		return ilen
	}
	return workers
}

// run lets the user run an array of input with a user provided function in parallel.
// It limits the number of worker threads to min(num_workers, num_cpu).
// The function aborts if an error is encountered.
// Example: parallel.run([1, 2, 3, 4, 5], 2, fn (i) { println(i) })
pub fn run[T](input []T, worker fn (T), opt Params) {
	if input.len == 0 {
		return
	}
	workers := limited_workers(opt.workers, input.len)
	ch := chan T{cap: workers * 2}
	mut wg := sync.new_waitgroup()
	wg.add(input.len)
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
	ch.close() // this will signal all the workers to exit, and we can return, without having to wait for them to finish
}

struct Task[T, R] {
	idx    int
	input  T
	result R
}

// amap lets the user run an array of input with a user provided function in parallel.
// It limits the number of worker threads to max number of cpus.
// The worker function can return a value. The returning array maintains the input order.
// Any error handling should have happened within the worker function.
// Example: squares := parallel.amap([1, 2, 3, 4, 5], 2, fn (i) { return i * i })
pub fn amap[T, R](input []T, worker fn (T) R, opt Params) []R {
	if input.len == 0 {
		return []
	}
	mut tasks := []Task[T, R]{len: input.len}
	// the tasks array will be passed to the closure of each worker by reference, so that it could
	// then modify the same tasks:
	mut tasks_ref := &tasks

	workers := limited_workers(opt.workers, input.len)
	// use a buffered channel for transfering the tasks, that has enough space to keep all the workers busy,
	// without blocking the main thread needlessly
	ch := chan Task[T, R]{cap: workers * 2}
	mut wg := sync.new_waitgroup()
	wg.add(input.len)
	for _ in 0 .. workers {
		spawn fn [ch, worker, mut wg, mut tasks_ref] [T, R]() {
			for {
				mut task := <-ch or { break }
				unsafe {
					tasks_ref[task.idx] = Task[T, R]{
						idx:    task.idx
						input:  task.input
						result: worker(task.input)
					}
				}
				wg.done()
			}
		}()
	}

	// put the input into the channel
	for idx, inp in input {
		ch <- Task[T, R]{
			idx:   idx
			input: inp
		}
	}

	// wait for all tasks to complete
	wg.wait()
	ch.close()
	tasks.sort(a.idx < b.idx)
	return tasks.map(it.result)
}
