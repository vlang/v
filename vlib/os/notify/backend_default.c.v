module notify

// Implement the API
pub struct DefaultNotifier {}

pub fn (d DefaultNotifier) add(fd int, events FdEventType, conf ...FdConfigFlags) ? {
	panic('unsupported')
}

pub fn (d DefaultNotifier) remove(fd int) ? {
	panic('unsupported')
}

pub fn (d DefaultNotifier) wait(timeout time.Duration) []FdEvent {
	panic('unsupported')
}

pub fn (d DefaultNotifier) close() ? {
	panic('unsupported')
}

pub fn new() ?DefaultNotifier {
	panic('unsupported')
}
