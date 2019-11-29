// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

struct Option {
	data     [300]byte
	error    string
	ecode    int
	ok       bool
	is_none  bool
}

// `fn foo() ?Foo { return foo }` => `fn foo() ?Foo { return opt_ok(foo); }`
fn opt_ok(data voidptr, size int) Option {
	if size >= 300 {
		panic('option size too big: $size (max is 300), this is a temporary limit')
	}
	res := Option {
		ok: true
	}
	C.memcpy(res.data, data, size)
	return res
}

// used internally when returning `none`
fn opt_none() Option {
	return Option{ is_none: true }
}

pub fn error(s string) Option {
	return Option {
		error: s
	}
}

pub fn error_with_code(s string, code int) Option {
	return Option {
		error: s
		ecode: code
	}
}



