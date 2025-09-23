import v.builder
import v.parser
import v.pref
import v.ast

type T001 = [47]int

fn test_update_sym() {
	// we will update the `size` & `size_expr` in sym.info for `T001`
	mut pref_ := pref.new_preferences()
	$if x64 {
		pref_.m64 = true
	}
	mut b := builder.new_builder(pref_)
	mut files := b.get_builtin_files()
	b.set_module_lookup_paths()
	parser.parse_files(files, mut b.table, b.pref)
	b.parse_imports()
	parser.parse_file(@FILE, mut b.table, .parse_comments, b.pref)

	mut t := b.table

	// retrieve old sym first
	typ := t.type_idxs['[47]int']!
	old_str := t.type_to_str(typ)
	dump(old_str)
	assert old_str == '[47]int'
	old_sym := t.sym(typ)
	old_info := old_sym.info as ast.ArrayFixed
	dump(old_info)

	// update the existing sym
	new_info := ast.ArrayFixed{
		...old_info
		size:      100
		size_expr: ast.empty_expr
	}
	dump(new_info)
	new_sym := ast.TypeSymbol{
		...old_sym
		info: new_info
	}
	t.update_sym(new_sym, typ)
	new_str := t.type_to_str(typ)
	dump(new_str)
	assert new_str == '[100]int'
}
