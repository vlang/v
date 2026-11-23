// scheduler_test.v - the scheduler decides correctly, and jobs actually run.
//
// The due-date tests use explicit Unix times, and the live loop test waits for
// two job signals with a timeout.
module scheduler

import time

fn t(year int, month int, day int, hour int, minute int, second int) i64 {
	return time.Time{
		year:   year
		month:  month
		day:    day
		hour:   hour
		minute: minute
		second: second
	}.unix()
}

fn test_an_interval_job_is_due_after_its_period() {
	mut s := &Scheduler{}
	mut job := s.every('tick', 60 * time.second, fn () {})
	// Pin `last_run` so the assertions measure the period, not the epoch: with
	// `last_run` at zero everything is due, which the next test covers.
	job.last_run = t(2026, 10, 15, 9, 0, 0)
	assert job.is_due(t(2026, 10, 15, 9, 1, 1))
	assert !job.is_due(t(2026, 10, 15, 9, 0, 59))
}

fn test_an_interval_job_runs_on_its_first_tick() {
	// `last_run` starts at zero, which is longer ago than any interval.
	mut s := &Scheduler{}
	mut job := s.every('boot', time.hour, fn () {})
	assert job.is_due(t(2026, 10, 15, 9, 0, 0))
}

fn test_fractional_intervals_round_up_to_whole_seconds() {
	intervals := [time.nanosecond, 500 * time.millisecond, time.second, 1500 * time.millisecond,
		2 * time.second, 2 * time.second + time.nanosecond]
	expected_seconds := [i64(1), 1, 1, 2, 2, 3]
	for i, interval in intervals {
		mut s := &Scheduler{}
		s.every('tick', interval, fn () {})
		assert s.run_pending(1000) == 1
		assert s.run_pending(1000) == 0
		for elapsed in i64(1) .. expected_seconds[i] {
			assert s.run_pending(1000 + elapsed) == 0
		}
		assert s.run_pending(1000 + expected_seconds[i]) == 1
		assert s.run_pending(1000 + expected_seconds[i]) == 0
	}
}

fn test_a_daily_job_is_due_inside_its_minute() {
	mut s := &Scheduler{}
	mut job := s.daily_at('report', 9, 30, fn () {})
	// Pretend it ran yesterday so only today's window matters.
	job.last_run = t(2026, 10, 14, 9, 30, 0)
	assert job.is_due(t(2026, 10, 15, 9, 30, 0))
	assert job.is_due(t(2026, 10, 15, 9, 30, 45))
	assert !job.is_due(t(2026, 10, 15, 9, 29, 59))
	assert !job.is_due(t(2026, 10, 15, 9, 31, 0))
}

fn test_a_daily_job_that_ran_today_is_not_due_again() {
	mut s := &Scheduler{}
	mut job := s.daily_at('report', 9, 30, fn () {})
	job.last_run = t(2026, 10, 15, 9, 30, 10)
	assert !job.is_due(t(2026, 10, 15, 9, 30, 50))
	assert !job.is_due(t(2026, 10, 15, 23, 59, 59))
}

fn test_a_fresh_daily_job_does_not_replay_a_missed_run_on_boot() {
	// Simulate a scheduler set up at 09:00 for a 09:30 job: the minute window
	// rule means only a tick inside 09:30:00-09:30:59 can fire it, so 10:00
	// passes quietly and tomorrow's window still works.
	mut s := &Scheduler{}
	mut job := s.daily_at('report', 9, 30, fn () {})
	job.last_run = t(2026, 10, 15, 9, 0, 0)
	assert !job.is_due(t(2026, 10, 15, 10, 0, 0))
	assert !job.is_due(t(2026, 10, 16, 9, 29, 0))
	assert job.is_due(t(2026, 10, 16, 9, 30, 0))
}

fn test_run_pending_starts_due_jobs_and_counts_them() {
	mut s := &Scheduler{}
	s.every('a', time.second, fn () {})
	mut b := s.every('b', time.hour, fn () {})
	// On a fresh scheduler `a` is due (it has never run) while `b` ran ten
	// seconds ago and is not.
	b.last_run = t(2026, 10, 15, 9, 0, 0) - 10
	assert s.run_pending(t(2026, 10, 15, 9, 0, 0)) == 1
}

fn test_due_jobs_run_sequentially_in_registration_order() {
	// Jobs run to completion in the loop thread, so a channel records the
	// order with no synchronization: first registered, first run.
	mut ch := chan string{cap: 4}
	mut s := &Scheduler{}
	s.every('first', time.second, fn [ch] () {
		ch <- 'first'
	})
	s.every('second', time.second, fn [ch] () {
		ch <- 'second'
	})
	assert s.run_pending(t(2026, 10, 15, 9, 0, 0)) == 2
	assert <-ch == 'first'
	assert <-ch == 'second'
}

fn test_a_live_loop_runs_an_interval_job() {
	runs := chan bool{cap: 2}
	mut s := &Scheduler{}
	s.every('tick', time.second, fn [runs] () {
		_ = runs.try_push(true)
	})
	s.start()
	defer { s.stop() }

	sw := time.new_stopwatch()
	for run in 1 .. 3 {
		remaining := 2200 * time.millisecond - sw.elapsed()
		assert remaining > 0, 'timed out waiting for scheduler run ${run}'
		select {
			_ := <-runs {
			}
			remaining {
				assert false, 'timed out waiting for scheduler run ${run}'
			}
		}
	}
}
