// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

pub fn exit(code int) {
	println('js.exit()')
}

// isnil returns true if an object is nil (only for C objects).
pub fn isnil(v voidptr) bool {
	return v == 0
}

pub fn panic(s string) {
	println('V panic: ' + s)
	exit(1)
}

pub fn println(s string) {
	#console.log(s)
}

pub fn eprintln(s string) {
	#console.log(s)
}

pub fn print(s string) {
	#console.log(s)
}




