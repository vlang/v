module log

// init will be called before the user's main program starts, to initialize the default logger
fn init() {
	default_logger = new_thread_safe_log()
	C.atexit(deinit)
}
