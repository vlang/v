module log

import sync

pub struct ThreadSafeLog {
	Log
pub mut:
	mu sync.Mutex
}

pub fn new_thread_safe_log() &ThreadSafeLog {
	mut x := &ThreadSafeLog{
		level: .info
	}
	x.mu.init()
	return x
}

[unsafe]
pub fn (mut x ThreadSafeLog) free() {
	unsafe {
		x.Log.free()
		x.mu.destroy()
	}
}

pub fn (mut x ThreadSafeLog) set_level(level Level) {
	x.mu.@lock()
	x.Log.set_level(level)
	x.mu.unlock()
}

pub fn (mut x ThreadSafeLog) debug(s string) {
	x.mu.@lock()
	x.Log.debug(s)
	x.mu.unlock()
}

pub fn (mut x ThreadSafeLog) info(s string) {
	x.mu.@lock()
	x.Log.info(s)
	x.mu.unlock()
}

pub fn (mut x ThreadSafeLog) warn(s string) {
	x.mu.@lock()
	x.Log.warn(s)
	x.mu.unlock()
}

pub fn (mut x ThreadSafeLog) error(s string) {
	x.mu.@lock()
	x.Log.error(s)
	x.mu.unlock()
}

pub fn (mut x ThreadSafeLog) fatal(s string) {
	x.mu.@lock()
	x.Log.error(s)
	x.mu.unlock()
}
