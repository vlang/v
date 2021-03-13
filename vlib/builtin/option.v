// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

// IError holds information about an error instance
pub interface IError {
	msg  string
	code int
}

// Error is the default implementation of IError, that is returned by e.g. `error()`
pub struct Error {
pub:
	msg  string
	code int
}

struct Option3 {
	state byte
	err   IError = none__
}

const none__ = IError(&None__{})

struct None__ {
	msg  string
	code int
}

fn (_ None__) str() string { return 'none' }

fn opt_ok3(data voidptr, mut option Option3, size int) {
	unsafe {
		*option = Option3{}
		// use err to get the end of Option3 and then memcpy into it
		C.memcpy(byteptr(&option.err) + sizeof(IError), data, size)
	}
}

[inline]
pub fn error3(message string) IError {
	return &Error{
		msg: message
	}
}

pub fn error_with_code3(message string, code int) IError {
	return &Error {
		msg: message
		code: code
	}
}

////////////////////////////////////////

// these are just here temporarily to avoid breaking the compiler; they will be removed soon
pub fn error(a string) Option2 { return {} }
pub fn error_with_code(a string, b int) Option2 { return {} }

// Option2 is the base of V's new internal optional return system.
struct Option2 {
	state byte
	err   Error
	// Data is trailing after err
	// and is not included in here but in the
	// derived Option2_xxx types
}

// `fn foo() ?Foo { return foo }` => `fn foo() ?Foo { return opt_ok(foo); }`
fn opt_ok(data voidptr, mut option Option2, size int) {
	unsafe {
		*option = Option2{}
		// use err to get the end of OptionBase and then memcpy into it
		C.memcpy(byteptr(&option.err) + sizeof(Error), data, size)
	}
}

// error returns an optional containing the error given in `message`.
// `if ouch { return error('an error occurred') }`
pub fn error2(message string) Option2 {
	return Option2{
		state: 2
		err: {
			msg: message
		}
	}
}

// error_with_code returns an optional containing both error `message` and error `code`.
// `if ouch { return error_with_code('an error occurred',1) }`
pub fn error_with_code2(message string, code int) Option2 {
	return Option2{
		state: 2
		err: {
			msg: message
			code: code
		}
	}
}
