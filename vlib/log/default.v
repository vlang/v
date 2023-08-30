// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
[has_globals]
module log

__global default_logger &Logger

// init will be called before the user's main program starts, to initialize the default logger
fn init() {
	default_logger = new_default()
	C.atexit(deinit)
}

// deinit will be called on exit of the program and will free the memory allocated for the default logger
fn deinit() {
	free_logger(default_logger)
}

fn free_logger(logger &Logger) {
	if logger == unsafe { nil } {
		return
	}
	unsafe {
		logger.free()
		free(logger)
	}
}

// set_logger changes the default logger instance to the one provided by the user.
// The existing logger will be freed, *after* the change is done.
pub fn set_logger(logger &Logger) {
	old_logger := unsafe { default_logger }
	default_logger = unsafe { logger }
	free_logger(old_logger)
}

pub fn new_default() &Logger {
	return &ThreadSafeLog{
		level: .info
	}
}

pub fn get_level() Level {
	return default_logger.get_level()
}

pub fn set_level(level Level) {
	default_logger.set_level(level)
}

pub fn fatal(s string) {
	default_logger.fatal(s)
}

pub fn error(s string) {
	default_logger.error(s)
}

pub fn warn(s string) {
	default_logger.warn(s)
}

pub fn info(s string) {
	default_logger.info(s)
}

pub fn debug(s string) {
	default_logger.debug(s)
}
