// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

// IError holds information about an error instance
pub interface IError {
	msg() string
	code() int
}

pub fn (err IError) str() string {
	return match err {
		None__ { 'none' }
		Error { err.msg() }
		else { '$err.type_name(): $err.msg()' }
	}
}

// BaseError is an empty implementation of the IError interface, it is used to make custom error types simpler
pub struct BaseError {}

[inline]
pub fn (err BaseError) msg() string {
	return ''
}

[inline]
pub fn (err BaseError) code() int {
	return 0
}

// Error is the default implementation of IError with `msg` and `code` fields, that is returned by e.g. `error()`
pub struct Error {
pub:
	msg  string
	code int
}

[inline]
pub fn (err Error) msg() string {
	return err.msg
}

[inline]
pub fn (err Error) code() int {
	return err.code
}

// None__ must contain `msg` and `code` fields because currently some compiler magic depends on it.
struct None__ {
	msg  string
	code int
}

[inline]
fn (n None__) msg() string {
	return n.msg
}

[inline]
fn (n None__) code() int {
	return n.code
}

fn (n None__) str() string {
	return 'none'
}

const none__ = IError(&None__{})

[if trace_error ?]
fn trace_error(x string) {
	eprintln('> ${@FN} | $x')
}

// error returns a default error instance containing the error given in `message`.
// Example: `if ouch { return error('an error occurred') }`
[inline]
pub fn error(message string) IError {
	trace_error(message)
	return &Error{
		msg: message
	}
}

// error_with_code returns a default error instance containing the given `message` and error `code`.
// `if ouch { return error_with_code('an error occurred', 1) }`
[inline]
pub fn error_with_code(message string, code int) IError {
	trace_error('$message | code: $code')
	return &Error{
		msg: message
		code: code
	}
}

// Option is the base of V's internal optional return system.
struct Option {
	state byte
	err   IError = none__
	// Data is trailing after err
	// and is not included in here but in the
	// derived Option_xxx types
}

fn opt_ok(data voidptr, mut option Option, size int) {
	unsafe {
		*option = Option{}
		// use err to get the end of OptionBase and then memcpy into it
		vmemcpy(&byte(&option.err) + sizeof(IError), data, size)
	}
}

[unsafe]
pub fn (e &Error) free() {
	unsafe { e.msg.free() }
}

[unsafe]
pub fn (n &None__) free() {
	unsafe { n.msg.free() }
}
