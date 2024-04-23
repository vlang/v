// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
@[has_globals]
module log

__global default_logger &Logger

// TODO: remove this hack, when the language has a way to access the raw pointer to an interface value directly:
@[typedef]
pub struct C.log__Logger {
mut:
	_object voidptr
}

// init will be called before the user's main program starts, to initialize the default logger
fn init() {
	default_logger = new_thread_safe_log()
	at_exit(deinit) or {}
}

// deinit will be called on exit of the program and will free the memory allocated for the default logger
fn deinit() {
	free_logger(default_logger)
}

@[manualfree]
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
