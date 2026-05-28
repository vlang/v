// This module defines the Context type, which carries deadlines, cancellation signals,
// and other request-scoped values across API boundaries and between processes.
// Based on:   https://github.com/golang/go/tree/master/src/context
// Last commit: https://github.com/golang/go/commit/52bf14e0e8bdcd73f1ddfb0c4a1d0200097d3ba2
module context

import time

// An EmptyContext is never canceled, has no values.
// The done_ch field is an open channel that is never closed, returned by done()
// on every call. This mirrors Go's context.Background().Done() == nil behavior:
// selecting on this channel blocks forever, meaning the context is never done.
pub struct EmptyContext {
mut:
	done_ch chan int
}

// deadline returns none, since an EmptyContext has no deadline.
pub fn (ctx &EmptyContext) deadline() ?time.Time {
	return none
}

// done returns an open channel that is never closed, since an EmptyContext can
// never be canceled. Selecting on the returned channel blocks forever.
// The same channel instance is returned on every call for a given EmptyContext.
pub fn (mut ctx EmptyContext) done() chan int {
	return ctx.done_ch
}

// err returns none, since an EmptyContext is never canceled.
pub fn (ctx &EmptyContext) err() IError {
	return none
}

// value returns none, since an EmptyContext carries no values.
pub fn (ctx &EmptyContext) value(key Key) ?Any {
	return none
}

// str returns a string describing the Context.
pub fn (ctx &EmptyContext) str() string {
	return 'unknown empty Context'
}

// A BackgroundContext is never canceled, has no values.
struct BackgroundContext {
	EmptyContext
}

// str returns a string describing the Context.
pub fn (ctx &BackgroundContext) str() string {
	return 'context.Background'
}

// background returns an empty Context. It is never canceled, has no
// values, and has no deadline. It is typically used by the main function,
// initialization, and tests, and as the top-level Context for incoming
// requests.
pub fn background() Context {
	return &BackgroundContext{}
}

// A TodoContext is never canceled, has no values, and has no deadline. It is
// never used for real work.
struct TodoContext {
	EmptyContext
}

// str returns a string describing the Context.
pub fn (ctx &TodoContext) str() string {
	return 'context.TODO'
}

// TODO: returns an empty Context. Code should use todo when
// it's unclear which Context to use or it is not yet available (because the
// surrounding function has not yet been extended to accept a Context
// parameter).
pub fn todo() Context {
	return &TodoContext{}
}
