// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

struct Option {
	data *byte
	error string
	ok    bool
}

// `fn foo() ?Foo { return foo }` => `fn foo() ?Foo { return opt_ok(foo); }`
fn opt_ok(data voidptr, size int) Option {
	res := Option {
		ok: true
		data : C.alloca(size)
	}

	C.memcpy(res.data, data, size) 
	return res 
}

pub fn error(s string) Option {
	return Option {
		error: s
	}
}


