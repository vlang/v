// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

fn (a any) toString()

// Exits the process in node, and halts execution in the browser
// because `process.exit` is undefined. Workaround for not having
// a 'real' way to exit in the browser.
pub fn exit(c int) {
	JS.process.exit(c)
	js_throw('exit($c)')
}

pub fn unwrap(opt any) any {
	o := &Option(opt)
	if o.state != 0 {
		js_throw(o.err)
	}
	return opt
}

pub fn panic(s string) {
	eprintln('V panic: $s')
	exit(1)
}

struct Option {
	state byte
	err   Error
}

pub struct Error {
pub:
	msg  string
	code int
}

pub fn (o Option) str() string {
	if o.state == 0 {
		return 'Option{ ok }'
	}
	if o.state == 1 {
		return 'Option{ none }'
	}
	return 'Option{ error: "$o.err" }'
}

pub fn error(s string) Option {
	return Option{
		state: 2
		err: Error{
			msg: s
		}
	}
}

pub fn error_with_code(s string, code int) Option {
	return Option{
		state: 2
		err: Error{
			msg: s
			code: code
		}
	}
}
