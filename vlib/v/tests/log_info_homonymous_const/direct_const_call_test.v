module main

import log

fn mk() &log.Log {
	return &log.Log{}
}

const default_logger = mk()

// A direct bare call on a homonymous user const must keep the const's method
// and the const's C symbol (`main__default_logger`), not mix in `log`'s global
// type (which panics `interface method log__Logger.info not implemented`).
fn test_direct_call_on_homonymous_user_const() {
	default_logger.info('direct const call')
	assert true
}
