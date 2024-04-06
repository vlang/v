// This module defines the Context type, which carries deadlines, cancellation signals,
// and other request-scoped values across API boundaries and between processes.
// Based on:   https://github.com/golang/go/tree/master/src/context
// Last commit: https://github.com/golang/go/commit/52bf14e0e8bdcd73f1ddfb0c4a1d0200097d3ba2
module context

import time

// An EmptyContext is never canceled, has no values.
pub struct EmptyContext {}

pub fn (ctx &EmptyContext) deadline() ?time.Time {
	return none
}

pub fn (ctx &EmptyContext) done() chan int {
	ch := chan int{}
	ch.close()
	return ch
}

pub fn (ctx &EmptyContext) err() IError {
	return none
}

pub fn (ctx &EmptyContext) value(key Key) ?Any {
	return none
}

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
