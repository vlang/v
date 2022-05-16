// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
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
	// >> Hack to allow old style custom error implementations
	// TODO: remove once deprecation period for `IError` methods has ended
	msg string
	code int // <<
	msg() string
	code() int
}

// str returns the message of IError
pub fn (err IError) str() string {
	return match err {
		None__ {
			'none'
		}
		Error {
			err.msg()
		}
		MessageError {
			err.msg()
		}
		else {
			// >> Hack to allow old style custom error implementations
			// TODO: remove once deprecation period for `IError` methods has ended
			old_error_style := unsafe { voidptr(&err.msg) != voidptr(&err.code) } // if fields are not defined (new style) they don't have an offset between them
			if old_error_style {
				'$err.type_name(): $err.msg'
			} else {
				// <<
				'$err.type_name(): $err.msg()'
			}
		}
	}
}

// Error is the empty default implementation of `IError`.
pub struct Error {}

// msg returns the message of Error
pub fn (err Error) msg() string {
	return ''
}

// code returns the code of Error
pub fn (err Error) code() int {
	return 0
}

// MessageError is the default implementation of the `IError` interface that is returned by the `error()` function
struct MessageError {
pub:
	msg  string
	code int
}

// msg returns the message of the MessageError
pub fn (err MessageError) msg() string {
	return err.msg
}

// code returns the code of MessageError
pub fn (err MessageError) code() int {
	return err.code
}

pub const none__ = IError(&None__{})

struct None__ {
	Error
}

fn (_ None__) str() string {
	return 'none'
}

pub struct Option {
	state u8
	err   IError = none__
}

// str returns the Option type: ok, none, or error
pub fn (o Option) str() string {
	if o.state == 0 {
		return 'Option{ ok }'
	}
	if o.state == 1 {
		return 'Option{ none }'
	}
	return 'Option{ error: "$o.err" }'
}

pub struct _option {
	state u8
	err   IError = none__
}

// str returns the Option type: ok, none, or error
pub fn (o _option) str() string {
	if o.state == 0 {
		return 'Option{ ok }'
	}
	if o.state == 1 {
		return 'Option{ none }'
	}
	return 'Option{ error: "$o.err" }'
}

// trace_error prints to stderr a string and a backtrace of the error
fn trace_error(x string) {
	eprintln('> ${@FN} | $x')
}

// error returns a default error instance containing the error given in `message`.
// Example: if ouch { return error('an error occurred') }
[inline]
pub fn error(message string) IError {
	// trace_error(message)
	return &MessageError{
		msg: message
	}
}

// error_with_code returns a default error instance containing the given `message` and error `code`.
// Example: if ouch { return error_with_code('an error occurred', 1) }
[inline]
pub fn error_with_code(message string, code int) IError {
	// trace_error('$message | code: $code')
	return &MessageError{
		msg: message
		code: code
	}
}

// free allows for manually freeing memory allocated at the address `ptr`.
// However, this is a no-op on JS backend
[unsafe]
pub fn free(ptr voidptr) {
	_ := ptr
}
