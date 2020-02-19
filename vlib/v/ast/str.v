module ast

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
	f.write('fn ${receiver}${node.name}(')
	for i, arg in node.args {
		is_last_arg := i == node.args.len - 1
		should_add_type := is_last_arg || node.args[i + 1].typ != arg.typ
		f.write(arg.name)
		if should_add_type {
			arg_typ_sym := t.get_type_symbol(arg.typ)
			f.write(' ${arg_typ_sym.name}')
		}
		if !is_last_arg {
			f.write(', ')
		}
	}
	f.write(')')
	if node.typ != table.void_type {
		sym := t.get_type_symbol(node.typ)
		f.write(' ' + sym.name)
	}
	return f.str()
}
