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
