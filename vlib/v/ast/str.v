// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast
/*
These methods are used only by vfmt, vdoc, and for debugging.
*/


import (
	v.table
	strings
)

pub fn (node &FnDecl) str(t &table.Table) string {
	mut f := strings.new_builder(30)
	mut receiver := ''
	if node.is_method {
		sym := t.get_type_symbol(node.receiver.typ)
		name := sym.name.after('.')
		m := if node.rec_mut { 'mut ' } else { '' }
		receiver = '($node.receiver.name ${m}$name) '
	}
	name := node.name.after('.')
	f.write(' fn ${receiver}${name}(')
	for i, arg in node.args {
		is_last_arg := i == node.args.len - 1
		should_add_type := is_last_arg || node.args[i + 1].typ != arg.typ
		f.write(arg.name)
		if should_add_type {
			f.write(' ' + t.type_to_str(arg.typ))
		}
		if !is_last_arg {
			f.write(', ')
		}
	}
	f.write(')')
	if node.typ != table.void_type {
		f.write(' ' + t.type_to_str(node.typ))
	}
	return f.str()
}
