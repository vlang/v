import benchmark
import context
import os
import time
import x.async as xasync

const default_group_rounds = 8
const default_group_jobs = 16
const default_task_rounds = 32
const default_pool_jobs = 64
const default_pool_workers = 4
const default_pool_bounded_rounds = 8
const default_pool_bounded_timeout_ms = 2
const default_timeout_rounds = 32
const default_every_iterations = 5
const default_every_interval_ms = 1

fn main() {
	run_benchmarks() or {
		eprintln('x.async benchmark failed: ${err.msg()}')
		exit(1)
	}
}

fn run_benchmarks() ! {
	group_rounds := env_int('XASYNC_BENCH_GROUP_ROUNDS', default_group_rounds, 1, 200)
	group_jobs := env_int('XASYNC_BENCH_GROUP_JOBS', default_group_jobs, 1, 512)
	task_rounds := env_int('XASYNC_BENCH_TASK_ROUNDS', default_task_rounds, 1, 500)
	pool_jobs := env_int('XASYNC_BENCH_POOL_JOBS', default_pool_jobs, 1, 1000)
	pool_workers := env_int('XASYNC_BENCH_POOL_WORKERS', default_pool_workers, 1, 64)
	pool_bounded_rounds := env_int('XASYNC_BENCH_POOL_BOUNDED_ROUNDS', default_pool_bounded_rounds,
		1, 100)
	pool_bounded_timeout_ms := env_int('XASYNC_BENCH_POOL_BOUNDED_TIMEOUT_MS',
		default_pool_bounded_timeout_ms, 1, 100)
	timeout_rounds := env_int('XASYNC_BENCH_TIMEOUT_ROUNDS', default_timeout_rounds, 1, 500)
	every_iterations := env_int('XASYNC_BENCH_EVERY_ITERATIONS', default_every_iterations, 1, 100)
	every_interval_ms :=
		env_int('XASYNC_BENCH_EVERY_INTERVAL_MS', default_every_interval_ms, 1, 100)

	println('x.async cautious benchmark')
	println('Override sizes with XASYNC_BENCH_* environment variables.')

	mut checksum := 0
	mut b := benchmark.start()
	checksum += bench_group(group_rounds, group_jobs)!
	b.measure('Group: rounds=${group_rounds}, jobs_per_round=${group_jobs}, checksum=${checksum}')

	checksum += bench_task(task_rounds)!
	b.measure('Task: rounds=${task_rounds}, checksum=${checksum}')

	checksum += bench_pool(pool_jobs, pool_workers)!
	b.measure('Pool: jobs=${pool_jobs}, workers=${pool_workers}, checksum=${checksum}')

	checksum += bench_pool_bounded_admission(pool_bounded_rounds, pool_bounded_timeout_ms)!
	b.measure('Pool bounded admission: rounds=${pool_bounded_rounds}, timeout_ms=${pool_bounded_timeout_ms}, checksum=${checksum}')

	checksum += bench_timeout(timeout_rounds)!
	b.measure('with_timeout: rounds=${timeout_rounds}, checksum=${checksum}')

	checksum += bench_every(every_iterations, every_interval_ms)!
	b.measure('every: iterations=${every_iterations}, interval_ms=${every_interval_ms}, checksum=${checksum}')
}

fn env_int(name string, default_value int, min_value int, max_value int) int {
	raw := os.getenv(name)
	if raw == '' {
		return default_value
	}
	mut value := raw.int()
	if value < min_value {
		value = min_value
	}
	if value > max_value {
		value = max_value
	}
	return value
}

fn bench_group(rounds int, jobs_per_round int) !int {
	mut total := 0
	for _ in 0 .. rounds {
		done := chan int{cap: jobs_per_round}
		mut group := xasync.new_group(context.background())
		for _ in 0 .. jobs_per_round {
			group.go(fn [done] (mut ctx context.Context) ! {
				_ = ctx
				done <- 1
			})!
		}
		group.wait()!
		for _ in 0 .. jobs_per_round {
			total += <-done
		}
	}
	return total
}

fn bench_task(rounds int) !int {
	mut total := 0
	for i in 0 .. rounds {
		mut task := xasync.run[int](fn [i] (mut ctx context.Context) !int {
			_ = ctx
			return i + 1
		})!
		total += task.wait()!
	}
	return total
}

fn bench_pool(jobs int, workers int) !int {
	done := chan int{cap: jobs}
	mut pool := xasync.new_pool(workers: workers, queue_size: jobs)!
	for _ in 0 .. jobs {
		pool.try_submit(fn [done] (mut ctx context.Context) ! {
			_ = ctx
			done <- 1
		})!
	}
	pool.close()!
	mut total := 0
	for _ in 0 .. jobs {
		total += <-done
	}
	return total
}

fn bench_pool_bounded_admission(rounds int, timeout_ms int) !int {
	mut total := 0
	admission_timeout := time.Duration(timeout_ms) * time.millisecond
	for _ in 0 .. rounds {
		mut pool := xasync.new_pool(workers: 1, queue_size: 1)!
		started := chan bool{cap: 2}
		release := chan bool{cap: 3}
		finished := chan bool{cap: 2}
		attempting := chan bool{cap: 1}
		accepted := chan bool{cap: 1}
		ran := chan int{cap: 1}
		blocking_job := fn [started, release, finished] (mut ctx context.Context) ! {
			_ = ctx
			started <- true
			_ := <-release
			finished <- true
		}

		pool.try_submit(blocking_job)!
		wait_for_benchmark_bool(started, 'bounded admission pool job did not start')!
		pool.try_submit(blocking_job)!

		mut timed_out := false
		pool.submit_with_timeout(admission_timeout, fn (mut ctx context.Context) ! {
			_ = ctx
		}) or {
			if err.msg() != 'async: timeout' {
				return err
			}
			timed_out = true
		}
		if !timed_out {
			release <- true
			release <- true
			pool.close()!
			return error('submit_with_timeout accepted while the pool was full')
		}

		submit_thread := spawn fn [mut pool, attempting, accepted, ran] () {
			attempting <- true
			pool.submit_with_context(context.background(), fn [ran] (mut ctx context.Context) ! {
				_ = ctx
				ran <- 1
			}) or {
				accepted <- false
				return
			}
			accepted <- true
		}()
		wait_for_benchmark_bool(attempting, 'bounded admission submitter did not start')!

		release <- true
		wait_for_benchmark_bool(finished, 'bounded admission first job did not finish')!
		wait_for_benchmark_bool(accepted,
			'submit_with_context did not accept after capacity opened')!
		release <- true
		total += read_benchmark_int(ran, 'bounded admission accepted job did not run')!
		pool.close()!
		submit_thread.wait()
	}
	return total
}

fn bench_timeout(rounds int) !int {
	mut total := 0
	for _ in 0 .. rounds {
		xasync.with_timeout(1 * time.second, fn (mut ctx context.Context) ! {
			_ = ctx
		})!
		total++
	}
	return total
}

fn bench_every(iterations int, interval_ms int) !int {
	ctx, cancel := xasync.with_cancel()
	ticks := chan int{cap: iterations + 4}
	result := chan string{cap: 1}
	interval := time.Duration(interval_ms) * time.millisecond
	worker := spawn fn [ctx, ticks, result, interval] () {
		xasync.every(ctx, interval, fn [ticks] (mut ctx context.Context) ! {
			_ = ctx
			ticks <- 1
		}) or {
			result <- err.msg()
			return
		}
		result <- 'ok'
	}()

	mut total := 0
	for _ in 0 .. iterations {
		select {
			value := <-ticks {
				total += value
			}
			1 * time.second {
				cancel()
				worker.wait()
				return error('every benchmark did not receive a tick')
			}
		}
	}
	cancel()
	select {
		msg := <-result {
			if msg != 'context canceled' {
				worker.wait()
				return error('unexpected every benchmark result: ${msg}')
			}
		}
		1 * time.second {
			worker.wait()
			return error('every benchmark did not stop after cancellation')
		}
	}
	worker.wait()
	return total
}

fn wait_for_benchmark_bool(signal chan bool, message string) ! {
	select {
		ok := <-signal {
			if !ok {
				return error(message)
			}
		}
		1 * time.second {
			return error(message)
		}
	}
}

fn read_benchmark_int(signal chan int, message string) !int {
	select {
		value := <-signal {
			return value
		}
		1 * time.second {
			return error(message)
		}
	}
	return 0
}
