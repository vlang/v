// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
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
	if o.not_ok {
		js_throw(o.error)
	}
	return opt
}

pub fn panic(s string) {
	eprintln('V panic: $s')
	exit(1)
}


struct Option {
	not_ok  bool
	is_none bool
	error   string
	ecode   int
	data    any
}

pub fn (o Option) str() string {
   if !o.not_ok {
	  return 'Option{ ok }'
   }
   if o.is_none {
	  return 'Option{ none }'
   }
   return 'Option{ error: "${o.error}" }'
}

pub fn error(s string) Option {
	return Option{
		not_ok: true
		is_none: false
		error: s
	}
}

pub fn error_with_code(s string, code int) Option {
	return Option{
		not_ok: true
		is_none: false
		error: s
		ecode: code
	}
}
