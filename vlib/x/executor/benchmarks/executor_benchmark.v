import benchmark
import context
import os
import time
import x.executor

const default_drain_rounds = 16
const default_drain_jobs = 32
const default_sync_rounds = 32
const default_timeout_rounds = 8
const default_timeout_ms = 2
const default_producers = 16

fn main() {
	run_benchmarks() or {
		eprintln('x.executor benchmark failed: ${err.msg()}')
		exit(1)
	}
}

fn run_benchmarks() ! {
	drain_rounds := env_int('XEXECUTOR_BENCH_DRAIN_ROUNDS', default_drain_rounds, 1, 200)
	drain_jobs := env_int('XEXECUTOR_BENCH_DRAIN_JOBS', default_drain_jobs, 1, 512)
	sync_rounds := env_int('XEXECUTOR_BENCH_SYNC_ROUNDS', default_sync_rounds, 1, 500)
	timeout_rounds := env_int('XEXECUTOR_BENCH_TIMEOUT_ROUNDS', default_timeout_rounds, 1, 100)
	timeout_ms := env_int('XEXECUTOR_BENCH_TIMEOUT_MS', default_timeout_ms, 1, 100)
	producers := env_int('XEXECUTOR_BENCH_PRODUCERS', default_producers, 1, 256)

	println('x.executor cautious benchmark')
	println('Override sizes with XEXECUTOR_BENCH_* environment variables.')

	mut checksum := 0
	mut b := benchmark.start()
	checksum += bench_try_post_drain(drain_rounds, drain_jobs)!
	b.measure('try_post + drain_pending: rounds=${drain_rounds}, jobs=${drain_jobs}, checksum=${checksum}')

	checksum += bench_run_sync(sync_rounds)!
	b.measure('run_sync: rounds=${sync_rounds}, checksum=${checksum}')

	checksum += bench_bounded_timeout(timeout_rounds, timeout_ms)!
	b.measure('bounded admission timeout: rounds=${timeout_rounds}, timeout_ms=${timeout_ms}, checksum=${checksum}')

	checksum += bench_many_producers(producers)!
	b.measure('multi-producer post_with_context: producers=${producers}, checksum=${checksum}')
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

fn bench_try_post_drain(rounds int, jobs int) !int {
	mut total := 0
	for _ in 0 .. rounds {
		mut ex := executor.new(queue_size: jobs)!
		done := chan int{cap: jobs}
		for _ in 0 .. jobs {
			ex.try_post(fn [done] () ! {
				done <- 1
			})!
		}
		drained := ex.drain_pending(jobs)!
		total += drained
		for _ in 0 .. jobs {
			total += <-done
		}
		ex.stop()
		ran_after_stop := ex.run_one()!
		if ran_after_stop {
			return error('try_post drain benchmark ran an unexpected job after stop')
		}
		ex.wait()!
	}
	return total
}

fn bench_run_sync(rounds int) !int {
	mut ex := executor.new(queue_size: rounds)!
	run_result := chan string{cap: 1}
	runner := spawn fn [mut ex, run_result] () {
		ex.run() or {
			run_result <- err.msg()
			return
		}
		run_result <- 'ok'
	}()

	done := chan int{cap: rounds}
	for _ in 0 .. rounds {
		ex.run_sync(fn [done] () ! {
			done <- 1
		})!
	}

	mut total := 0
	for _ in 0 .. rounds {
		total += <-done
	}
	ex.stop()
	run_msg := <-run_result
	if run_msg != 'ok' {
		runner.wait()
		return error('run_sync benchmark owner loop returned an error')
	}
	ex.wait()!
	runner.wait()
	return total
}

fn bench_bounded_timeout(rounds int, timeout_ms int) !int {
	timeout := time.Duration(timeout_ms) * time.millisecond
	mut total := 0
	for _ in 0 .. rounds {
		mut ex := executor.new(queue_size: 1)!
		ex.try_post(fn () ! {})!
		ex.post_with_timeout(timeout, fn () ! {}) or {
			if err.msg() != 'executor: timeout' {
				return err
			}
			total++
		}
		ran := ex.run_one()!
		if !ran {
			return error('bounded timeout benchmark did not run the accepted job')
		}
		ex.stop()
		ran_after_stop := ex.run_one()!
		if ran_after_stop {
			return error('bounded timeout benchmark ran an unexpected job after stop')
		}
		ex.wait()!
	}
	return total
}

fn bench_many_producers(producers int) !int {
	mut ex := executor.new(queue_size: producers)!
	accepted := chan bool{cap: producers}
	ran := chan int{cap: producers}
	mut threads := []thread{}

	for i in 0 .. producers {
		threads << spawn fn [mut ex, accepted, ran, i] () {
			ex.post_with_context(context.background(), fn [ran, i] () ! {
				value := i + 1
				ran <- value
				return
			}) or {
				accepted <- false
				return
			}
			accepted <- true
		}()
	}
	for _ in 0 .. producers {
		accepted_ok := <-accepted
		if !accepted_ok {
			return error('producer admission failed')
		}
	}
	drained := ex.drain_pending(producers)!
	if drained != producers {
		return error('multi-producer benchmark drained ${drained} jobs instead of ${producers}')
	}

	mut total := 0
	for _ in 0 .. producers {
		total += <-ran
	}
	for producer_thread in threads {
		producer_thread.wait()
	}
	ex.stop()
	ran_after_stop := ex.run_one()!
	if ran_after_stop {
		return error('multi-producer benchmark ran an unexpected job after stop')
	}
	ex.wait()!
	return total
}
