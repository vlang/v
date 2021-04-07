// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

// IError holds information about an error instance
pub interface IError {
	msg string
	code int
}

// Error is the default implementation of IError, that is returned by e.g. `error()`
pub struct Error {
pub:
	msg  string
	code int
}

pub fn (err IError) str() string {
	return match err {
		None__ { 'none' }
		Error { err.msg }
		else { '$err.type_name(): $err.msg' }
	}
}

const none__ = IError(&None__{})

struct None__ {
	msg  string
	code int
}

fn (_ None__) str() string {
	return 'none'
}

// error returns a default error instance containing the error given in `message`.
// Example: `if ouch { return error('an error occurred') }`
[inline]
pub fn error(message string) IError {
	return &Error{
		msg: message
	}
}

// error_with_code returns a default error instance containing the given `message` and error `code`.
// `if ouch { return error_with_code('an error occurred', 1) }`
[inline]
pub fn error_with_code(message string, code int) IError {
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
		C.memcpy(&byte(&option.err) + sizeof(IError), data, size)
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

[typedef]
struct C.IError {
	_object voidptr
}

[unsafe]
pub fn (ie &IError) free() {
	unsafe {
		ie.msg.free()
		cie := &C.IError(ie)
		free(cie._object)
	}
}
