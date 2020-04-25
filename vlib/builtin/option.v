// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin
/*
struct Option2<T> {
	data T
	error string
	ecode int
	ok bool
	is_none bool
}
*/


struct Option {
	data    [400]byte
	error   string
	ecode   int
	ok      bool
	is_none bool
}

pub fn (o Option) str() string {
   if o.ok && !o.is_none {
	  return 'Option{ data: ' + o.data[0..32].hex() + ' }'
   }
   if o.is_none {
	  return 'Option{ none }'	  
   }
   return 'Option{ error: "${o.error}" }'
}

// `fn foo() ?Foo { return foo }` => `fn foo() ?Foo { return opt_ok(foo); }`
fn opt_ok(data voidptr, size int) Option {
	if size >= 400 {
		panic('option size too big: $size (max is 400), this is a temporary limit')
	}
	res := Option{
		ok: true
	}
	C.memcpy(res.data, data, size)
	return res
}

// used internally when returning `none`
fn opt_none() Option {
	return Option{
		is_none: true
	}
}

pub fn error(s string) Option {
	return Option{
		error: s
	}
}

pub fn error_with_code(s string, code int) Option {
	return Option{
		error: s
		ecode: code
	}
}
