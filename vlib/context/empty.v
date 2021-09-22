// This module defines the Context type, which carries deadlines, cancellation signals,
// and other request-scoped values across API boundaries and between processes.
// Based off:   https://github.com/golang/go/tree/master/src/context
// Last commit: https://github.com/golang/go/commit/52bf14e0e8bdcd73f1ddfb0c4a1d0200097d3ba2
module context

import time

// An EmptyContext is never canceled, has no values, and has no deadline. It is not
// struct{}, since vars of this type must have distinct addresses.
pub type EmptyContext = int

pub fn (ctx EmptyContext) deadline() ?time.Time {
	return none
}

pub fn (ctx EmptyContext) done() chan int {
	ch := chan int{}
	defer {
		ch.close()
	}
	return ch
}

pub fn (ctx EmptyContext) err() IError {
	// TODO: Change this to `none`
	return none_
}

pub fn (ctx EmptyContext) value(key string) ?voidptr {
	return none
}

pub fn (ctx EmptyContext) str() string {
	if ctx == background {
		return 'context.Background'
	}
	if ctx == todo {
		return 'context.TODO'
	}
	return 'unknown empty Context'
}
