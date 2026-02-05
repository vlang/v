// BEAM backend log default implementation
// Provides default logger for BEAM backend
@[has_globals]
module log

__global default_logger &Logger

// init will be called before the user's main program starts
fn init() {
	default_logger = new_thread_safe_log()
}

// free_logger frees the memory allocated for the logger
// On BEAM, garbage collection handles this, but we keep the API
@[manualfree]
fn free_logger(logger &Logger) {
	if voidptr(logger) == unsafe { nil } {
		return
	}
	// On BEAM, GC handles memory - this is mostly a no-op
	unsafe {
		logger.free()
	}
}
