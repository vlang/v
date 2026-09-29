module main

import log

fn setup_default_logger() &log.Log {
	return &log.Log{}
}

const default_logger = setup_default_logger()

// Must keep the const's method and C symbol; mixing in log's global type
// panics `interface method log__Logger.info not implemented`.
fn test_direct_call_on_homonymous_user_const() {
	default_logger.info('direct const call')
}
