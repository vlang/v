module builtin

// ChanState describes the result of an attempted channel transaction.
pub enum ChanState {
	success
	not_ready // push()/pop() would have to wait, but no_block was requested
	closed
}

/*
The following methods are only stubs.
The real implementation is in `vlib/sync/channels.v`
*/

// close closes the channel for further push transactions.
// closed channels cannot be pushed to, however they can be popped
// from as long as there is still objects available in the channel buffer.
pub fn (ch chan) close() {}

// try_pop returns `ChanState.success` if an object is popped from the channel.
// try_pop effectively pops from the channel without waiting for objects to become available.
// Both the test and pop transaction is done atomically.
pub fn (ch chan) try_pop(obj voidptr) ChanState {
	return .success
}

// try_push returns `ChanState.success` if the object is pushed to the channel.
// try_push effectively both push and test if the transaction `ch <- a` succeeded.
// Both the test and push transaction is done atomically.
pub fn (ch chan) try_push(obj voidptr) ChanState {
	return .success
}
