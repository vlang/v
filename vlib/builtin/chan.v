module builtin

enum ChanState {
	success
	not_ready // push()/pop() would have to wait, but no_block was requested
	closed
}

// The following methods are only stubs. The real implementation
// is in `vlib/sync/channels.v`

pub fn (ch chan) close() {}

pub fn (ch chan) try_pop(obj voidptr) ChanState {
	return .success
}

pub fn (ch chan) try_push(obj voidptr) ChanState {
	return .success
}
