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
	set_handler_level(current_record_logger().handler, level)
}

// current_record_logger reads the global structured logger under its lock.
fn current_record_logger() &RecordLogger {
	default_record_mu.lock()
	defer { default_record_mu.unlock() }
	return default_record_logger
}

// fatal logs a `fatal` message, using the default Logger instance
@[noreturn]
pub fn fatal(s string, attrs ...Attr) {
	if attrs.len > 0 {
		l := current_record_logger()
		if l.enabled(.fatal) {
			l.send(.fatal, s, attrs)
		}
		panic(s)
	}
	default_logger.fatal(s)
	// the compiler currently has no way to mark functions in an interface
	// as @[noreturn], so we need to make sure this is never returning ourselves
	exit(1)
}

// error logs an `error` message, using the default Logger instance
pub fn error(s string, attrs ...Attr) {
	if attrs.len > 0 {
		current_record_logger().send_if(.error, s, attrs)
		return
	}
	default_logger.error(s)
}

// warn logs a `warning` message, using the default Logger instance
pub fn warn(s string, attrs ...Attr) {
	if attrs.len > 0 {
		current_record_logger().send_if(.warn, s, attrs)
		return
	}
	default_logger.warn(s)
}

// info logs an `info` message, using the default Logger instance
pub fn info(s string, attrs ...Attr) {
	if attrs.len > 0 {
		current_record_logger().send_if(.info, s, attrs)
		return
	}
	default_logger.info(s)
}

// debug logs a `debug` message, using the default Logger instance
pub fn debug(s string, attrs ...Attr) {
	if attrs.len > 0 {
		current_record_logger().send_if(.debug, s, attrs)
		return
	}
	default_logger.debug(s)
}

// set_always_flush called with true, will make the log flush after every single .fatal(), .error(), .warn(), .info(), .debug() call.
pub fn set_always_flush(should_flush_on_every_message bool) {
	default_logger.set_always_flush(should_flush_on_every_message)
}

// set_record_logger replaces the logger used by package-level calls that pass
// structured attributes.
pub fn set_record_logger(l &RecordLogger) {
	default_record_mu.lock()
	defer { default_record_mu.unlock() }
	default_record_logger = l
}

// get_record_logger returns the logger used by structured package-level calls.
pub fn get_record_logger() &RecordLogger {
	return current_record_logger()
}

// set_record_level changes the level of the global structured logger's handler.
pub fn set_record_level(level Level) {
	set_handler_level(current_record_logger().handler, level)
}
