module types

import v.flat

fn test_generic_receiver_field_keeps_explicit_main_argument() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_module = 'foreign'
	tc.cur_file = 'foreign.v'
	tc.structs['Record'] = []StructField{}
	tc.structs['foreign.Record'] = []StructField{}
	tc.struct_generic_params['foreign.Slot'] = ['T']
	tc.structs['foreign.Slot'] = [StructField{
		name: 'value'
		typ:  unknown_type('generic placeholder `T`')
	}]
	assert tc.qualified_generic_suffix([flat.source_type_id('main.Record')]) == '[main.Record]'
	receiver := tc.parse_scope_param_type(flat.source_type_id('&foreign.Slot[main.Record]'))
	assert receiver.name() == '&foreign.Slot[main.Record]'
	field := tc.struct_field_type(flat.source_type_id('foreign.Slot[main.Record]'), 'value') or {
		panic('missing generic field')
	}
	assert field.name() == 'Record'
	// A real import named main must still resolve through that import.
	tc.register_file_import('main', 'other')
	tc.structs['other.Record'] = []StructField{}
	assert tc.qualified_generic_suffix([flat.source_type_id('main.Record')]) == '[other.Record]'
}

fn test_qualified_generic_argument_ids_keep_nested_commas_and_file_imports() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_module = 'app'
	tc.cur_file = 'one.v'
	tc.structs['left.Item'] = []StructField{}
	tc.structs['right.Item'] = []StructField{}
	tc.register_file_import('dep', 'left')
	args := [flat.source_type_id(' \t[]dep.Item\n'),
		flat.source_type_id('map[string]fn (int, string) bool')]
	assert tc.qualified_generic_suffix(args) == '[[]left.Item, map[string]fn(int, string) bool]'
	tc.cur_file = 'two.v'
	tc.register_file_import('dep', 'right')
	assert tc.qualified_generic_suffix(args) == '[[]right.Item, map[string]fn(int, string) bool]'
	assert tc.qualified_generic_suffix([args[1], args[0]]) == '[map[string]fn(int, string) bool, []right.Item]'
	assert tc.qualified_generic_suffix([]u32{}) == '[]'
	assert flat.source_type_name(args[0]) == ' \t[]dep.Item\n'
}

fn test_generic_alias_argument_ids_keep_substitution_order_and_arity_fallback() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.type_aliases.set_id('scope.Callbacks', flat.source_type_id('map[K]fn (V, K) V'))
	tc.type_alias_generic_params['Callbacks'] = ['K', 'V']
	args := [flat.source_type_string, flat.source_type_int]
	alias := tc.parse_generic_alias_application(flat.source_type_id('scope.Callbacks'), args, '[string, int]')
	assert alias is Alias
	if alias is Alias {
		assert alias.name_id == flat.source_type_id('scope.Callbacks[string, int]')
		assert alias.base_type.name() == 'map[string]fn(int, string) int'
	}
	tc.type_aliases.set_id('Fallback', flat.source_type_u64)
	tc.type_alias_generic_params['Fallback'] = ['T', 'U']
	fallback := tc.parse_generic_alias_application(flat.source_type_id('Fallback'), [flat.source_type_int], '[int]')
	assert fallback is Alias
	if fallback is Alias {
		assert fallback.name_id == flat.source_type_id('Fallback')
		assert fallback.base_type.source_type_id() == flat.source_type_u64
	}
}

fn test_main_generic_base_keeps_canonical_specialization_spelling() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_module = 'foreign'
	tc.struct_generic_params['Box'] = ['T']
	tc.structs['Box'] = []StructField{}
	tc.struct_generic_params['foreign.Box'] = ['T']
	tc.structs['foreign.Box'] = []StructField{}
	assert tc.qualified_generic_suffix([flat.source_type_id('main.Box[string]')]) == '[Box[string]]'
}

fn test_main_callback_type_keeps_unqualified_diagnostic_spelling() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_module = 'main'
	tc.structs['Context'] = []StructField{}
	assert flat.source_type_name(tc.qualify_resolution_type_text(flat.source_type_id('fn (mut main.Context) bool'))) == 'fn(mut Context) bool'
}
