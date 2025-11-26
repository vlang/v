import v.ast
import v.pref
import v.parser

fn test_update_sym_by_idx() {
	old_size := 47
	new_size := 100
	b_old_size := 4 * old_size
	b_new_size := 4 * new_size
	source := 'type T001 = [${old_size}]u32' // u32 is an element type, with a known size, that is not going to change in the future, unlike int
	// we will update the `size` & `size_expr` in sym.info for `T001`
	mut t := ast.new_table()
	parser.parse_text(source, '', mut t, .parse_comments, pref.new_preferences())

	// retrieve old sym first:
	typ := t.type_idxs['[${old_size}]u32']!
	old_str := t.type_to_str(typ)
	assert old_str == '[${old_size}]u32'
	old_sym := t.sym(typ)
	old_info := old_sym.info as ast.ArrayFixed

	alias_typ := t.type_idxs['main.T001']!
	alias_str := t.type_to_str(alias_typ)
	assert alias_str == 'main.T001'
	// make sure that the alias type had the correct size before the change (the size is stored in the alias type symbol)
	alias_size, _ := t.type_size(alias_typ)
	assert alias_size == b_old_size
	alias_sym := t.sym(alias_typ)
	assert (alias_sym.info as ast.Alias).parent_type == typ
	assert alias_sym.size == b_old_size

	// update the existing sym
	new_info := ast.ArrayFixed{
		...old_info
		size:      new_size
		size_expr: ast.empty_expr
	}
	new_sym := ast.TypeSymbol{
		...old_sym
		info: new_info
	}
	t.update_sym_by_idx(typ, new_sym)
	new_str := t.type_to_str(typ)
	assert new_str == '[${new_size}]u32'

	// check again the alias size (it should be indirectly changed as well):
	new_alias_sym := t.sym(alias_typ)
	assert new_alias_sym.size == -1
	new_alias_size, _ := t.type_size(alias_typ)
	assert new_alias_size == b_new_size
	assert new_alias_sym.size == b_new_size // make sure that `new_alias_sym` is now updated too (since it is a pointer to a symbol value stored in the table)
}
