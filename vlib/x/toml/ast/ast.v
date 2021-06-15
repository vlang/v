// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import x.toml.input
//import x.toml.token

// Root represents the root structure of any parsed TOML text snippet or file.
[heap]
pub struct Root {
pub:
	input              input.Config // User input configuration
pub mut:
	table              Value
	//errors           []errors.Error    // all the checker errors in the file
}

pub fn (r Root) str() string {
	mut s := typeof(r).name+'{\n'
	s += '  input:  $r.input\n'
	s += '  table:  $r.table\n'
	s += '}'
	return s
}

/*
pub fn (r Root) has_table(key string) bool {
	return key in r.tables
}
*/

/*
pub fn (r Root) find(key string) ?Value {
	if key == '/' {
		return r.table
	} else {
		skey := key.trim_right('/').split('/')
		for k, v in t.pairs {
			//if kv.key.str() == skey[0] {
			if k == skey[0] {
				val := v//.value
				if val is Quoted || val is Date {
					return v //kv.value
				} else if val is map[string]Value {
					//if skey.len > 1 {
					tbl := Table{...val}
					return tbl.find(skey[1..].join('/'))
					//} else {
					//	val
					//}
				}
				else {
					return error(@MOD + '.' + @STRUCT + '.' + @FN + ' TODO BUG')
				}
			}
		}
		return r.table.find(key.trim_left('/'))
	}
}
*/

/*
pub fn (r Root) get_active_table() &ast.Table {
	return r.get_table(r.active_table)
}
*/

/*
pub fn (mut r Root) new_table(parent string, key string) {
	if ! r.has_table(key) {
		r.tables[key] = &ast.Table{}
		util.printdbg(@MOD + '.' + @FN, 'prepared r.tables[\'$key\']')
	} else {
		panic(@MOD + '.' + @FN + ' r.tables[\'$key\'] already exist')
	}
}*/
