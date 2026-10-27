module types

import v.flat

fn test_concrete_generic_arguments_lock_nested_caller_types() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_module = 'main'
	tc.structs['Row'] = []StructField{}
	tc.structs['foreign.Row'] = []StructField{}
	assert tc.explicit_generic_concrete_arg_text_bytes('fn (row Row) Row') == 'fn (main.Row) main.Row'
	assert tc.explicit_generic_concrete_arg_text_bytes('fn (mut row Row) (Row, []Row)') == 'fn (mut main.Row) (main.Row, []main.Row)'
	assert tc.explicit_generic_concrete_arg_text_bytes('chan Row') == 'chan main.Row'
	for prefix in ['thread ', 'shared ', 'atomic ', 'chan mut '] {
		assert tc.explicit_generic_concrete_arg_text_bytes(prefix + 'Row') == prefix + 'main.Row'
	}
	assert tc.explicit_generic_concrete_arg_text_bytes('foreign.Cell[Row]') == 'foreign.Cell[main.Row]'
	assert tc.explicit_generic_concrete_arg_text_bytes('fn (foreign.Row) foreign.Row') == 'fn (foreign.Row) foreign.Row'
}

fn test_generic_struct_c_name_keeps_composite_argument_shape() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.structs['Row'] = []StructField{}
	tc.structs['foreign.Row'] = []StructField{}
	for arg in ['fn (foreign.Row) foreign.Row', 'chan foreign.Row', 'foreign.Cell[foreign.Row]'] {
		assert tc.c_generic_struct_arg_name(arg) == arg
	}
}
