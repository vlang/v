module log

import sync

// ThreadSafeLog embeds Log, and adds a mutex field.
// It uses the mutex to synchronise accesses to the embedded Log.
pub struct ThreadSafeLog {
	Log
pub mut:
	mu sync.Mutex
}

// new_thread_safe_log returns a new log structure, whose methods are safe
// to call by multiple threads.
pub fn new_thread_safe_log() &ThreadSafeLog {
	mut x := &ThreadSafeLog{
		level: .info
	}
	x.mu.init()
	return x
}

// free frees the given ThreadSafeLog instance.
[unsafe]
pub fn (mut x ThreadSafeLog) free() {
	unsafe {
		x.Log.free()
		x.mu.destroy()
		free(x) // TODO: think more about how freeing and interface value wrappers interact with -autofree
	}
}

// set_level changes the log level
pub fn (mut x ThreadSafeLog) set_level(level Level) {
	x.mu.@lock()
	x.Log.set_level(level)
	x.mu.unlock()
}

// debug logs a debug message
pub fn (mut x ThreadSafeLog) debug(s string) {
	x.mu.@lock()
	x.Log.debug(s)
	x.mu.unlock()
}

// info logs an info messagep
pub fn (mut x ThreadSafeLog) info(s string) {
	x.mu.@lock()
	x.Log.info(s)
	x.mu.unlock()
}

// warn logs a warning message
pub fn (mut x ThreadSafeLog) warn(s string) {
	x.mu.@lock()
	x.Log.warn(s)
	x.mu.unlock()
}

// error logs an error message
pub fn (mut x ThreadSafeLog) error(s string) {
	x.mu.@lock()
	x.Log.error(s)
	x.mu.unlock()
}

// fatal logs a fatal message, and panics
[noreturn]
pub fn (mut x ThreadSafeLog) fatal(s string) {
	x.mu.@lock()
	defer {
		// TODO: Log.fatal() is marked as noreturn, but this defer is allowed.
		// Think whether it should be, or if it should be a compiler notice at least,
		// since it would not be reached at all (.fatal() panics).
		x.mu.unlock()
	}
	x.Log.fatal(s)
}
