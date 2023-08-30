// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
[has_globals]
module log

__global default_logger &Logger

// deinit will be called on exit of the program and will free the memory allocated for the default logger
fn deinit() {
	free_logger(default_logger)
}

[manualfree]
fn free_logger(logger &Logger) {
	if voidptr(logger) == unsafe { nil } {
		return
	}
	unsafe {
		logger.free()
		free(logger)
	}
}

// set_logger changes the default logger instance to the one provided by the user.
// The existing logger will be freed, *after* the change is done.
[manualfree]
pub fn set_logger(logger &Logger) {
	// C.printf(c"set_logger  logger: %p | old logger: %p\n", logger, default_logger)
	old_logger := unsafe { default_logger }
	default_logger = unsafe { logger }
	free_logger(old_logger)
}

// get_level returns the log level of the default Logger instance
pub fn get_level() Level {
	return default_logger.get_level()
}

// set_level changes the log level of the default Logger instance
pub fn set_level(level Level) {
	default_logger.set_level(level)
}

// fatal logs a `fatal` message, using the default Logger instance
pub fn fatal(s string) {
	default_logger.fatal(s)
}

// error logs an `error` message, using the default Logger instance
pub fn error(s string) {
	default_logger.error(s)
}

// error logs a `warning` message, using the default Logger instance
pub fn warn(s string) {
	default_logger.warn(s)
}

// info logs an `info` message, using the default Logger instance
pub fn info(s string) {
	default_logger.info(s)
}

// debug logs a `debug` message, using the default Logger instance
pub fn debug(s string) {
	default_logger.debug(s)
}
