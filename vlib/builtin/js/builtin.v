// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

pub fn println(s any) {
	JS.console.log(s)
}

pub fn print(s any) {
	// TODO
	// $if js.node {
		JS.process.stdout.write(s)
	// } $else {
	//	panic('Cannot `print` in a browser, use `println` instead')
	// }
}

pub fn eprintln(s any) {
	JS.console.error(s)
}

pub fn eprint(s any) {
	// TODO
	// $if js.node {
		JS.process.stderr.write(s)
	// } $else {
	//	panic('Cannot `eprint` in a browser, use `eprintln` instead')
	// }
}

// Exits the process in node, and halts execution in the browser
// because `process.exit` is undefined. Workaround for not having
// a 'real' way to exit in the browser.
pub fn exit(c int) {
	JS.process.exit(c)
}

pub fn panic(s string) {
	eprintln('V panic: $s')
	exit(1)
}
