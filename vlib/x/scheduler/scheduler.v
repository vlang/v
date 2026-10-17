module scheduler

import time

// Job is one named unit of scheduled work. Create it through
// `Scheduler.every` or `Scheduler.daily_at` and never touch it again: the
// scheduler owns it from `start` on. Jobs run sequentially in the loop thread,
// to completion, in registration order — so a job can neither overlap itself
// nor race another. A job that needs concurrency spawns it internally.
@[heap]
pub struct Job {
	name string
	f    fn () @[required]
mut:
	every_secs i64
	daily_hour int = -1
	daily_min  int
	last_run   i64
}

// is_due reports whether the job should run at `now`. It is a pure function
// of the job's schedule and `last_run`, which is what makes scheduling
// decisions testable without sleeping.
fn (j &Job) is_due(now_unix i64) bool {
	if j.daily_hour >= 0 {
		return is_daily_due(now_unix, j.daily_hour, j.daily_min, j.last_run)
	}
	return now_unix - j.last_run >= j.every_secs
}

// is_daily_due reports whether a daily `hour:minute` job is due: the clock is
// inside the target minute and the job has not run since it started. The
// minute window (rather than "at or past the target") is what keeps a
// scheduler started at 10:00 from replaying the 09:30 run, and what keeps an
// already-run job quiet for the rest of its minute.
fn is_daily_due(now_unix i64, hour int, minute int, last_run i64) bool {
	now := time.unix(now_unix)
	day_start := now_unix - (now.hour * 3600 + now.minute * 60 + now.second)
	target := day_start + hour * 3600 + minute * 60
	return now_unix >= target && now_unix < target + 60 && last_run < target
}

// Scheduler runs named jobs in one process on intervals or daily times. The
// loop ticks once a second and runs what is due, sequentially, to completion.
// The struct is heap-allocated so `start` can hand it to its loop thread.
@[heap]
pub struct Scheduler {
mut:
	jobs    []&Job
	stopped bool
	started bool
	thread  thread
}

// every schedules `f` to run every `interval` under `name`. The job also runs
// on the first tick after `start`, since it has never run.
pub fn (mut s Scheduler) every(name string, interval time.Duration, f fn ()) &Job {
	job := &Job{
		name:       name
		f:          f
		every_secs: i64(interval) / i64(time.second)
	}
	s.jobs << job
	return job
}

// daily_at schedules `f` to run once a day at `hour:minute` under `name`.
// The job fires when a tick lands inside its minute; a scheduler started
// after today's time simply waits for tomorrow.
pub fn (mut s Scheduler) daily_at(name string, hour int, minute int, f fn ()) &Job {
	job := &Job{
		name:       name
		f:          f
		daily_hour: hour
		daily_min:  minute
	}
	s.jobs << job
	return job
}

// run_pending runs every job that is due at `now`, sequentially and to
// completion, and returns how many ran. It is the loop body factored out so
// tests can drive time directly. Keep jobs short: a long job delays the ticks
// behind it.
pub fn (mut s Scheduler) run_pending(now_unix i64) int {
	mut ran := 0
	for mut job in s.jobs {
		if !job.is_due(now_unix) {
			continue
		}
		job.last_run = now_unix
		job.f()
		ran++
	}
	return ran
}

// start launches the scheduling loop in its own thread. Call it once; a
// second call does nothing.
pub fn (mut s Scheduler) start() {
	if s.started {
		return
	}
	s.started = true
	s.stopped = false
	s.thread = spawn s.run_loop()
}

// stop asks the loop to exit after its current tick and waits for it. A job
// that is running finishes first, because jobs run inside the loop thread.
pub fn (mut s Scheduler) stop() {
	s.stopped = true
	if s.started {
		s.thread.wait()
		s.started = false
	}
}

fn (mut s Scheduler) run_loop() {
	for !s.stopped {
		s.run_pending(time.now().unix())
		time.sleep(time.second)
	}
}
