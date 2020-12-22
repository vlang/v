// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

/*
struct Option2<T> {
	ok bool
	is_none bool
	error string
	ecode int
	data T
}
*/
// OptionBase is the the base of V's internal optional return system.
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
