// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

import strings

pub struct map {
	obj voidptr
}

//fn (m mut map) insert(n mut mapnode, key string, val voidptr) {
//}

//////fn (n & mapnode) find(key string, out voidptr, element_size int) bool{
	//return false
//}

// same as `find`, but doesn't return a value. Used by `exists`
//fn (n & mapnode) find2(key string, element_size int) bool{
	//return false
//}

//fn preorder_keys(node &mapnode, keys mut []string, key_i int) int {
	//return 0
//}

pub fn (m mut map) keys() []string {
	return ['']
}

fn (m map) get(key string, out voidptr) bool {
	return false
}

pub fn (m mut map) delete(key string) {
}

fn (m map) exists(key string) bool {
	return false
}

pub fn (m map) print() {
	println('<<<<<<<<')
	println('>>>>>>>>>>')
}

pub fn (m map) free() {
	// C.free(m.table)
	// C.free(m.keys_table)
}

pub fn (m map_string) str() string {
	/*
	if m.size == 0 {
		return '{}'
	}
	*/
	mut sb := strings.new_builder(50)
	sb.writeln('{')
	for key, val  in m {
		//sb.writeln('  "$key" => "$val"')
	}
	sb.writeln('}')
	return sb.str()
}
