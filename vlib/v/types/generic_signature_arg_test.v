module types

import v.flat

fn test_concrete_generic_arguments_lock_nested_caller_types() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_module = 'main'
	tc.structs[flat.source_type_id('Row')] = []StructField{}
	tc.structs[flat.source_type_id('foreign.Row')] = []StructField{}
	assert tc.explicit_generic_concrete_arg_text(flat.source_type_id('fn (row Row) Row')) == flat.source_type_id('fn (main.Row) main.Row')
	assert tc.explicit_generic_concrete_arg_text(flat.source_type_id('fn (mut row Row) (Row, []Row)')) == flat.source_type_id('fn (mut main.Row) (main.Row, []main.Row)')
	assert tc.explicit_generic_concrete_arg_text(flat.source_type_id('chan Row')) == flat.source_type_id('chan main.Row')
	for prefix in ['thread ', 'shared ', 'atomic ', 'chan mut '] {
		assert tc.explicit_generic_concrete_arg_text(flat.source_type_id(prefix + 'Row')) == flat.source_type_id(prefix + 'main.Row')
	}
	assert tc.explicit_generic_concrete_arg_text(flat.source_type_id('foreign.Cell[Row]')) == flat.source_type_id('foreign.Cell[main.Row]')
	assert tc.explicit_generic_concrete_arg_text(flat.source_type_id('fn (foreign.Row) foreign.Row')) == flat.source_type_id('fn (foreign.Row) foreign.Row')
}

fn test_generic_struct_c_name_keeps_composite_argument_shape() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.structs[flat.source_type_id('Row')] = []StructField{}
	tc.structs[flat.source_type_id('foreign.Row')] = []StructField{}
	for arg in ['fn (foreign.Row) foreign.Row', 'chan foreign.Row', 'foreign.Cell[foreign.Row]'] {
		assert tc.c_generic_struct_arg_name(flat.source_type_id(arg)) == flat.source_type_id(arg)
	}
}
