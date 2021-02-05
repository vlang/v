module builtin

// ChanState describes the state a channel can be in.
enum ChanState {
	success
	not_ready // push()/pop() would have to wait, but no_block was requested
	closed
}

/*
The following methods are only stubs.
The real implementation is in `vlib/sync/channels.v`
*/

// close closes the channel for further operation.
// closed channels cannot be pushed - nor popped for content.
pub fn (ch chan) close() {}

// try_pop returns `ChanState.success` if the channel can be popped from.
// try_pop thus effectively tests if `item := <-ch` is possible.
pub fn (ch chan) try_pop(obj voidptr) ChanState {
	return .success
}

// try_push returns `ChanState.success` if the channel can be pushed to.
// try_push thus effectively tests if `ch <- a` is possible.
pub fn (ch chan) try_push(obj voidptr) ChanState {
	return .success
}
