module time

// Timer sends the current time on `c` after its duration elapses.
pub struct Timer {
	stop chan chan bool
	done chan bool
pub:
	c chan Time
}

struct TimerThreadArgs {
mut:
	duration       Duration
	output         chan Time
	stop           chan chan bool
	done           chan bool
	prealloc_scope voidptr
}

fn new_timer_thread_args(duration Duration, output chan Time, stop chan chan bool, done chan bool) &TimerThreadArgs {
	// Keep the channels visible to tracing collectors until the detached worker finishes.
	mut args := unsafe { &TimerThreadArgs(vcalloc(sizeof(TimerThreadArgs))) }
	if args == unsafe { nil } {
		panic('could not allocate timer thread arguments')
	}
	args.duration = duration
	args.output = output
	args.stop = stop
	args.done = done
	$if prealloc {
		args.prealloc_scope = unsafe { prealloc_scope_retain_current() }
	}
	return args
}

fn free_timer_thread_args(args &TimerThreadArgs) {
	$if prealloc {
		scope := args.prealloc_scope
		unsafe {
			prealloc_scope_release(scope)
		}
	}
	unsafe {
		free(args)
	}
}

// new_timer creates a Timer that sends the current time on its unbuffered channel after `duration`.
pub fn new_timer(duration Duration) &Timer {
	timer := &Timer{
		c:    chan Time{}
		stop: chan chan bool{}
		done: chan bool{}
	}
	start_timer(duration, timer.c, timer.stop, timer.done)
	return timer
}

fn run_timer(duration Duration, output chan Time, stop chan chan bool, done chan bool) {
	defer {
		done.close()
	}
	select {
		reply := <-stop {
			reply <- true
			return
		}
		duration {}
	}
	fired_at := now()
	select {
		reply := <-stop {
			reply <- true
		}
		output <- fired_at {}
	}
}

// stop prevents the Timer from firing and reports whether it stopped an active timer.
pub fn (timer &Timer) stop() bool {
	reply := chan bool{cap: 1}
	select {
		timer.stop <- reply {}
		_ := <-timer.done {
			return false
		}
	}
	return <-reply
}
