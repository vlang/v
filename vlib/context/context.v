// This module defines the Context type, which carries deadlines, cancellation signals,
// and other request-scoped values across API boundaries and between processes.
// Based on:   https://github.com/golang/go/tree/master/src/context
// Last commit: https://github.com/golang/go/commit/52bf14e0e8bdcd73f1ddfb0c4a1d0200097d3ba2
module context

import time

const (
	background         = EmptyContext(0)
	todo               = EmptyContext(1)

	cancel_context_key = Key('context.CancelContext')

	// canceled is the error returned by Context.err when the context is canceled.
	canceled           = error('context canceled')

	// deadline_exceeded is the error returned by Context.err when the context's
	// deadline passes.
	deadline_exceeded  = error('context deadline exceeded')
)

// Key represents the type for the ValueContext key
pub type Key = bool | f32 | f64 | i16 | i64 | i8 | int | string | u16 | u32 | u64 | u8 | voidptr

// Any represents a generic type for the ValueContext
pub interface Any {}

// `Context` is an interface that defined the minimum required functionality
// for a Context.
//
// `deadline()` returns the time when work done on behalf of this context
// should be canceled. deadline returns none when no deadline is
// set. Successive calls to deadline return the same results.
//
// `value(key)` returns an Optional that wraps the value associated with this context for key.
// It returns none if no value is associated with key. Successive calls to Value with
// the same key returns the same result.
//
// Use context values only for request-scoped data that transits
// processes and API boundaries, not for passing optional parameters to
// functions.
//
// A key identifies a specific value in a Context. Functions that wish
// to store values in Context typically allocate a key in a global
// variable then use that key as the argument to context.with_value and
// Context.value. A key can be any type that supports equality;
// modules should define keys as an unexported type to avoid
// collisions.
//
// `done()` returns a channel that's closed when work done on behalf of this
// context should be canceled. done may return a closed channel if this context can
// never be canceled. Successive calls to done return the same value.
// The close of the done channel may happen asynchronously,
// after the cancel function returns.
//
// with_cancel arranges for done to be closed when cancel is called;
// with_deadline arranges for done to be closed when the deadline
// expires; with_timeout arranges for done to be closed when the timeout
// elapses.
//
// `err()` returns an IError based on some conditions
// If done is not yet closed, err returns none.
// If done is closed, err returns a non-none error explaining why:
// canceled if the context was canceled
// or deadline_exceeded if the context's deadline passed.
// After err returns a non-none error, successive calls to err return the same error.
pub interface Context {
	deadline() ?time.Time
	value(key Key) ?Any
	str() string
mut:
	done() chan int
	err() IError
}

// background returns an empty Context. It is never canceled, has no
// values, and has no deadline. It is typically used by the main function,
// initialization, and tests, and as the top-level Context for incoming
// requests.
pub fn background() Context {
	return context.background
}

// todo returns an empty Context. Code should use todo when
// it's unclear which Context to use or it is not yet available (because the
// surrounding function has not yet been extended to accept a Context
// parameter).
pub fn todo() Context {
	return context.todo
}

fn context_name(ctx Context) string {
	return typeof(ctx)
}
