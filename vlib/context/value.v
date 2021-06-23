// This module defines the Context type, which carries deadlines, cancellation signals,
// and other request-scoped values across API boundaries and between processes.
// Based off:   https://github.com/golang/go/tree/master/src/context
// Last commit: https://github.com/golang/go/commit/52bf14e0e8bdcd73f1ddfb0c4a1d0200097d3ba2
module context

import time

// A ValueContext carries a key-value pair. It implements Value for that key and
// delegates all other calls to the embedded Context.
pub struct ValueContext {
	key   string
	value voidptr
mut:
	context Context
}

// with_value returns a copy of parent in which the value associated with key is
// val.
//
// Use context Values only for request-scoped data that transits processes and
// APIs, not for passing optional parameters to functions.
//
// The provided key must be comparable and should not be of type
// string or any other built-in type to avoid collisions between
// packages using context. Users of with_value should define their own
// types for keys
pub fn with_value(parent Context, key string, value voidptr) Context {
	return &ValueContext{
		context: parent
		key: key
		value: value
	}
}

pub fn (ctx ValueContext) deadline() ?time.Time {
	return ctx.context.deadline()
}

pub fn (ctx ValueContext) done() chan int {
	return ctx.context.done()
}

pub fn (ctx ValueContext) err() IError {
	return ctx.context.err()
}

pub fn (ctx ValueContext) value(key string) ?voidptr {
	if ctx.key == key {
		return ctx.value
	}
	return ctx.context.value(key)
}

pub fn (ctx ValueContext) str() string {
	return context_name(ctx.context) + '.with_value'
}
