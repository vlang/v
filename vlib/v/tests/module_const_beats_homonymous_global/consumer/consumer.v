module consumer

import api

pub const default_logger = &api.Impl{
	n: 99
}

// This const, not api's homonymous `__global`.
pub fn current() int {
	return default_logger.value()
}
