// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

fn (a any) toString()

pub fn println(s any) {
	// Quickfix to properly print basic types
	// TODO: Add proper detection code for this
	JS.console.log(s.toString())
}

pub fn print(s any) {
	// TODO
	// $if js.node {
	JS.process.stdout.write(s.toString())
	// } $else {
	//	panic('Cannot `print` in a browser, use `println` instead')
	// }
}

pub fn eprintln(s any) {
	JS.console.error(s.toString())
}

pub fn eprint(s any) {
	// TODO
	// $if js.node {
	JS.process.stderr.write(s.toString())
	// } $else {
	//	panic('Cannot `eprint` in a browser, use `eprintln` instead')
	// }
}

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

struct Option<T> {
	state byte
	err   Error
	data  T
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
		err: {
			msg: s
		}
	}
}

pub fn error_with_code(s string, code int) Option {
	return Option{
		state: 2
		err: {
			msg: s
			code: code
		}
	}
}
