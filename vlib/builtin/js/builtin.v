// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

fn (a any) toString()

[noreturn]
pub fn panic(s string) {
	eprintln('V panic: $s\n$js_stacktrace()')
	exit(1)
}

// IError holds information about an error instance
pub interface IError {
	msg() string
	code() int
}

pub fn (err IError) str() string {
	return match err {
		None__ { 'none' }
		Error { err.msg() }
		else { 'Error: $err.msg()' }
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

// Error is the default implementation of IError, that is returned by e.g. `error()`
pub struct Error {
pub:
	msg  string
	code int
}

pub fn (err Error) msg() string {
	return err.msg
}

pub fn (err Error) code() int {
	return err.code
}

struct None__ {
	msg  string
	code int
}

fn (n None__) msg() string {
	return n.str()
}

fn (_ None__) code() int {
	return 0
}

fn (_ None__) str() string {
	return 'none'
}

pub const none__ = IError(None__{})

pub struct Option {
	state byte
	err   IError = none__
}

pub fn (o Option) str() string {
	if o.state == 0 {
		return 'Option{ ok }'
	}
	if o.state == 1 {
		return 'Option{ none }'
	}
	return 'Option{ error: "$o.err" }'
}

fn trace_error(x string) {
	eprintln('> ${@FN} | $x')
}

// error returns a default error instance containing the error given in `message`.
// Example: `if ouch { return error('an error occurred') }`
[inline]
pub fn error(message string) IError {
	// trace_error(message)
	return &Error{
		msg: message
	}
}

// error_with_code returns a default error instance containing the given `message` and error `code`.
// `if ouch { return error_with_code('an error occurred', 1) }`
[inline]
pub fn error_with_code(message string, code int) IError {
	// trace_error('$message | code: $code')
	return &Error{
		msg: message
		code: code
	}
}

// free allows for manually freeing memory allocated at the address `ptr`. no-op on JS backend
[unsafe]
pub fn free(ptr voidptr) {
	_ := ptr
}
