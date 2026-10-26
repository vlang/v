module types

import v.flat

fn test_known_symbol_alias_keeps_present_empty_binding() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	assert tc.type_from_known_symbol('missing') == none
	tc.type_aliases.set_id('Empty', 0)
	typ := tc.type_from_known_symbol('Empty') or { panic('lost present empty alias') }
	assert typ is Alias
	assert typ.name() == 'Empty'
	if typ is Alias {
		assert typ.base_type is Void
	}
}

fn test_known_symbol_alias_precedes_other_type_kinds() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.type_aliases.set('Counter', 'int')
	tc.structs['Counter'] = []StructField{}
	typ := tc.type_from_known_symbol('Counter') or { panic('missing integer alias') }
	assert typ is Alias
	assert typ.name() == 'Counter'
	if typ is Alias {
		assert typ.base_type.name() == 'int'
	}
}
