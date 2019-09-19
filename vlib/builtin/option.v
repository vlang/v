// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

struct Option {
	data     [255]byte
	error    string
	ok       bool
	is_none  bool
}

// `fn foo() ?Foo { return foo }` => `fn foo() ?Foo { return opt_ok(foo); }`
fn opt_ok(data voidptr, size int) Option {
	if size >= 255 {
		panic('option size too big: $size (max is 255), this is a temporary limit')
	}
	res := Option {
		ok: true
	}
	C.memcpy(res.data, data, size)
	return res
}

fn opt_none() Option {
	return Option{ is_none: true }
}

pub fn error(s string) Option {
	return Option {
		error: s
	}
}


