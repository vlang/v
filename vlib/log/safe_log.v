module log

import sync

// ThreadSafeLog embeds Log, and adds a mutex field.
// It uses the mutex to synchronise accesses to the embedded Log.
pub struct ThreadSafeLog {
	Log
pub mut:
	mu &sync.Mutex = sync.new_mutex()
}

// new_thread_safe_log returns a new log structure, whose methods are safe
// to call by multiple threads.
pub fn new_thread_safe_log() &ThreadSafeLog {
	slevel := $d('log_default_level', 'info')
	level := level_from_tag(slevel.to_upper()) or { panic('invalid log_default_level: ${slevel}') }
	mut x := &ThreadSafeLog{
		level: level
	}
	return x
}

// free frees the given ThreadSafeLog instance.
@[unsafe]
pub fn (mut x ThreadSafeLog) free() {
	unsafe {
		// make sure other threads are not in the blocks protected by the mutex:
		if x.mu.try_lock() {
			x.mu.unlock()
		}
		x.mu.destroy()
		free(x.mu)
		x.mu = nil
		x.Log.free()
	}
}

// set_level changes the log level
pub fn (mut x ThreadSafeLog) set_level(level Level) {
	if unsafe { x.mu == 0 } {
		return
	}
	x.mu.lock()
	x.Log.set_level(level)
	x.mu.unlock()
}

// set_always_flush called with true, will make the log flush after every single .fatal(), .error(), .warn(), .info(), .debug() call.
// That can be much slower, if you plan to do lots of frequent calls, but if your program exits early or crashes, your logs will be more complete.
pub fn (mut x ThreadSafeLog) set_always_flush(should_flush bool) {
	if unsafe { x.mu == 0 } {
		return
	}
	x.mu.lock()
	x.Log.set_always_flush(should_flush)
	x.mu.unlock()
}

// debug logs a debug message
pub fn (mut x ThreadSafeLog) debug(s string) {
	if unsafe { x.mu == 0 } {
		return
	}
	x.mu.lock()
	x.Log.debug(s)
	x.mu.unlock()
}

// info logs an info messagep
pub fn (mut x ThreadSafeLog) info(s string) {
	if unsafe { x.mu == 0 } {
		return
	}
	x.mu.lock()
	x.Log.info(s)
	x.mu.unlock()
}

// warn logs a warning message
pub fn (mut x ThreadSafeLog) warn(s string) {
	if unsafe { x.mu == 0 } {
		return
	}
	x.mu.lock()
	x.Log.warn(s)
	x.mu.unlock()
}

// error logs an error message
pub fn (mut x ThreadSafeLog) error(s string) {
	if unsafe { x.mu == 0 } {
		return
	}
	x.mu.lock()
	x.Log.error(s)
	x.mu.unlock()
}

// fatal logs a fatal message, and panics
@[noreturn]
pub fn (mut x ThreadSafeLog) fatal(s string) {
	if unsafe { x.mu == 0 } {
		panic(s)
	}
	x.mu.lock()
	defer {
		// TODO: Log.fatal() is marked as noreturn, but this defer is allowed.
		// Think whether it should be, or if it should be a compiler notice at least,
		// since it would not be reached at all (.fatal() panics).
		x.mu.unlock()
	}
	x.Log.fatal(s)
}
