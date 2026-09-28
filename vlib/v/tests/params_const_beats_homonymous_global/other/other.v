@[has_globals]
module other

import api

__global default_logger &api.Logger

fn init() {
	default_logger = &api.Impl{n: 99}
}

// Bare `default_logger` here is other's own `__global`.
pub fn current() int {
	return default_logger.value()
}
