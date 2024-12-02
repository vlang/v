// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module log

// set_logger changes the default logger instance to the one provided by the user.
// The existing logger will be freed, *after* the change is done.
@[manualfree]
pub fn set_logger(logger &Logger) {
	// C.printf(c"set_logger  logger: %p | old logger: %p\n", logger, default_logger)
	old_logger := unsafe { default_logger }
	default_logger = unsafe { logger }
	free_logger(old_logger)
}

// get_logger returns a pointer to the current default logger instance
@[unsafe]
pub fn get_logger() &Logger {
	return default_logger
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
@[noreturn]
pub fn fatal(s string) {
	default_logger.fatal(s)
	// the compiler currently has no way to mark functions in an interface
	// as @[noreturn], so we need to make sure this is never returning ourselves
	exit(1)
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

// set_always_flush called with true, will make the log flush after every single .fatal(), .error(), .warn(), .info(), .debug() call.
pub fn set_always_flush(should_flush_on_every_message bool) {
	default_logger.set_always_flush(should_flush_on_every_message)
}
