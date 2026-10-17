// scheduler_test.v - the scheduler decides correctly, and jobs actually run.
//
// Scheduling has two halves and they are tested separately: the due-date math
// is pure (`is_due` over explicit unix times, no sleeping), and the loop is
// covered by one short live run with a channel-counted job. Time literals are
// built field-by-field so no test depends on the wall clock.
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
	// The only test that sleeps: a 1s job over ~2.2s must run at least twice
	// (once immediately, then on ticks). The channel, not a shared counter,
	// records the runs.
	ch := chan int{cap: 8}
	mut s := &Scheduler{}
	s.every('tick', time.second, fn [ch] () {
		ch <- 1
	})
	s.start()
	time.sleep(2200 * time.millisecond)
	s.stop()
	mut runs := 0
	mut got := 0
	for _ in 0 .. 8 {
		if ch.try_pop(mut got) == .success {
			runs++
		} else {
			break
		}
	}
	assert runs >= 2, 'expected at least 2 runs in ~2.2s, got ${runs}'
}
