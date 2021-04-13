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

pub fn (ctx EmptyContext) err() string {
	return ''
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
