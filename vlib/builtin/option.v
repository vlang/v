// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

struct OptionBase {
	ok      bool
	is_none bool
	error   string
	ecode   int
	// Data is trailing after ecode
	// and is not included in here but in the
	// derived Option_xxx types
}

// `fn foo() ?Foo { return foo }` => `fn foo() ?Foo { return opt_ok(foo); }`
fn opt_ok2(data voidptr, mut option OptionBase, size int) {
	unsafe {
		*option = OptionBase{
			ok: true
		}
		// use ecode to get the end of OptionBase and then memcpy into it
		C.memcpy(byteptr(&option.ecode) + sizeof(int), data, size)
	}
}

// Option is the old option type used for bootstrapping
struct Option {
	ok      bool
	is_none bool
	error   string
	ecode   int
}

// str returns the string representation of the Option.
pub fn (o Option) str() string {
	if o.ok && !o.is_none {
		return 'Option{ ok }'
	}
	if o.is_none {
		return 'Option{ none }'
	}
	return 'Option{ error: "$o.error" }'
}

// opt_none is used internally when returning `none`.
fn opt_none() Option {
	return Option{
		ok: false
		is_none: true
	}
}

// error returns an optional containing the error given in `message`.
// `if ouch { return error('an error occurred') }`
pub fn error(message string) Option {
	return Option{
		ok: false
		is_none: false
		error: message
	}
}

// error_with_code returns an optional containing both error `message` and error `code`.
// `if ouch { return error_with_code('an error occurred',1) }`
pub fn error_with_code(message string, code int) Option {
	return Option{
		ok: false
		is_none: false
		error: message
		ecode: code
	}
}

struct Option2 {
	state byte
	err   Error
}

// OptionBase is the the base of V's internal optional return system.
struct OptionBase2 {
	state byte
	err   Error
	// Data is trailing after err
	// and is not included in here but in the
	// derived Option2_xxx types
}

// Error holds information about an error instance
struct Error {
	msg  string
	code int
}

// /*
pub fn (o Option2) str() string {
	if o.state == 0 {
		return 'Option2{ ok }'
	}
	if o.state == 1 {
		return 'Option2{ none }'
	}
	return 'Option2{ err: "$o.err.msg" }'
}

// opt_none is used internally when returning `none`.
fn opt_none2() Option2 {
	return Option2{
		state: 1
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
// */
