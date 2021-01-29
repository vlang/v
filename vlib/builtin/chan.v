module builtin

enum ChanState {
	success
	not_ready // push()/pop() would have to wait, but no_block was requested
	closed
}

// The following methods are only stubs. The real implementation
// is in `vlib/sync/channels.v`

pub fn (_ chan) close() {}

pub fn (_ chan) try_pop(_ voidptr) ChanState {
	return .success
}

pub fn (_ chan) try_push(_ voidptr) ChanState {
	return .success
}
