module consumer

import api

pub const default_logger = &api.Impl{n: 7}

// The default value of a struct field declared in this module is the module's
// own const, not other's homonymous `__global`.
@[params]
pub struct Opt {
pub:
	logger &api.Logger = default_logger
}

pub fn current(opt Opt) int {
	return opt.logger.value()
}
