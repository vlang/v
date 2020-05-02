// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin
/*
struct Option2<T> {
	ecode int
	ok bool
	is_none bool
        error string
	data T
}
*/


struct Option {
	ecode   int
	ok      bool
	is_none bool
	error   string
	data    [400]byte
}

pub fn (o Option) str() string {
   if o.ok && !o.is_none {
	  return 'Option{ ecode: 0, ok: false, is_none: false, error: "", data: ' + o.data[0..32].hex() + ' }'
   }
   if o.is_none {
	  return 'Option{ none }'	  
   }
   return 'Option{ ecode: 0, ok: false, is_none: false, error: "${o.error}" }'
}

// `fn foo() ?Foo { return foo }` => `fn foo() ?Foo { return opt_ok(foo); }`
fn opt_ok(data voidptr, size int) Option {
	if size >= 400 {
		panic('option size too big: $size (max is 400), this is a temporary limit')
	}
	res := Option{
		ecode: 0
		ok: true
	}
	C.memcpy(res.data, data, size)
	return res
}

// used internally when returning `none`
fn opt_none() Option {
	return Option{
		ecode: 0
		ok: false
		is_none: true
	}
}

pub fn error(s string) Option {
	return Option{
		ecode: 0
                ok: false
                is_none: false
		error: s
	}
}

pub fn error_with_code(s string, code int) Option {
	return Option{
		ecode: code
                ok: false
                is_none: true
		error: s
	}
}
