module consumer

import api

pub const default_logger = &api.Impl{
	n: 99
}

// Bare `default_logger` here is consumer's own const, not api's homonymous
// `__global` — the declaring module's const wins (docs.md:3645-3653).
pub fn current() int {
	return default_logger.value()
}
