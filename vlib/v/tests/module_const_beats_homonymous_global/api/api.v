@[has_globals]
module api

pub interface Logger {
	value() int
}

pub struct Impl {
pub:
	n int
}

pub fn (logger &Impl) value() int {
	return logger.n
}

__global default_logger &Logger

fn init() {
	default_logger = &Impl{
		n: 7
	}
}

fn read(logger &Logger) int {
	return logger.value()
}

pub fn current() int {
	return read(default_logger)
}
