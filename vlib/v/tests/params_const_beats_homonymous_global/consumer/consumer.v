module consumer

import api

pub const default_logger = &api.Impl{ n: 7 }

// Field default is this module's const, not other's homonymous `__global`.
@[params]
pub struct Opt {
pub:
	logger &api.Logger = default_logger
}

pub fn current(opt Opt) int {
	return opt.logger.value()
}
