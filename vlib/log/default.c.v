module log

// init will be called before the user's main program starts, to initialize the default logger
fn init() {
	default_logger = new_thread_safe_log()
	C.atexit(deinit)
}

// TODO: remove this hack, when the language has a way to access the raw pointer to an interface value directly:
[typedef]
struct C.log__Logger {
mut:
	_object voidptr
}

[manualfree]
fn free_logger(logger &Logger) {
	if voidptr(logger) == unsafe { nil } {
		return
	}
	unsafe {
		// C.printf(c'free_logger logger: %p\n', logger)
		logger.free()
		//
		pobject := &C.log__Logger(logger)._object
		// C.printf(c'free_logger pobject: %p\n', pobject)
		free(pobject)
		//
		free(logger)
	}
}
