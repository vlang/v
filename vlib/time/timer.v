module time

import sync

// Timer represents a single event. After duration d, its channel C receives
// the current time. Mirrors Go's time.Timer.
pub struct Timer {
pub:
	c chan Time
mut:
	stop_ch chan chan bool
	done_ch chan bool
	done    bool
	mu      &sync.Mutex
}

struct TimerThreadArgs {
mut:
	duration       Duration
	output         chan Time
	stop           chan chan bool
	done           chan bool
	timer          &Timer
	prealloc_scope voidptr
}

@[unsafe]
fn new_timer_thread_args(duration Duration, output chan Time, stop chan chan bool, done chan bool, timer &Timer) &TimerThreadArgs {
	mut args := unsafe { &TimerThreadArgs(vcalloc(int(sizeof(TimerThreadArgs)))) }
	if args == unsafe { nil } {
		panic('could not allocate timer thread arguments')
	}
	args.duration = duration
	args.output = output
	args.stop = stop
	args.done = done
	args.timer = timer
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

fn timer_thread_entry(args_ptr voidptr) u32 {
	mut args := unsafe { &TimerThreadArgs(args_ptr) }
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		defer {
			unsafe {
				prealloc_scope_end(scope)
				prealloc_thread_cleanup()
			}
		}
	}
	run_timer(args.duration, args.output, args.stop, args.done, mut args.timer)
	free_timer_thread_args(args)
	return 0
}

fn run_timer(duration Duration, output chan Time, stop chan chan bool, done chan bool, mut timer &Timer) {
	defer {
		done.close()
	}
	select {
		reply := <-stop {
			timer.done = true
			reply <- true
			return
		}
		duration {
		}
	}
	select {
		reply := <-stop {
			timer.done = true
			reply <- true
			return
		}
		output <- time.now() {
			timer.done = true
		}
	}
}

fn start_timer(duration Duration, output chan Time, stop chan chan bool, done chan bool, timer &Timer) {
	args := new_timer_thread_args(duration, output, stop, done, timer)
	result := C.v_sync_thread_create_detached(voidptr(timer_thread_entry), voidptr(args))
	if result != 0 {
		free_timer_thread_args(args)
		panic('could not start timer: error ${result}')
	}
}

// new_timer creates a Timer that fires after d.
pub fn new_timer(d Duration) &Timer {
	mut t := &Timer{
		c:       chan Time{cap: 1}
		stop_ch: chan chan bool{}
		done_ch: chan bool{}
		mu:      sync.new_mutex()
	}
	start_timer(d, t.c, t.stop_ch, t.done_ch, t)
	return t
}

// stop prevents the Timer from firing. It returns true if the call stopped
// the timer, false if it already fired or was stopped.
pub fn (mut t Timer) stop() bool {
	t.mu.lock()
	defer { t.mu.unlock() }
	select {
		_ := <-t.done_ch {
			return false
		}
		else {
		}
	}
	reply := chan bool{cap: 1}
	select {
		t.stop_ch <- reply {
		}
		_ := <-t.done_ch {
			return false
		}
	}
	select {
		r := <-reply {
			return r
		}
		_ := <-t.done_ch {
			return false
		}
	}
	return false
}

// reset changes the timer to expire after d. It returns true if the timer had
// been active.
pub fn (mut t Timer) reset(d Duration) bool {
	t.mu.lock()
	defer { t.mu.unlock() }
	mut was_active := true
	select {
		_ := <-t.done_ch {
			was_active = false
		}
		else {
		}
	}
	if was_active {
		reply := chan bool{cap: 1}
		select {
			t.stop_ch <- reply {
			}
			_ := <-t.done_ch {
			}
		}
		select {
			_ := <-reply {
			}
			_ := <-t.done_ch {
			}
		}
	}
	t.stop_ch = chan chan bool{}
	t.done_ch = chan bool{}
	t.done = false
	start_timer(d, t.c, t.stop_ch, t.done_ch, t)
	return was_active
}
