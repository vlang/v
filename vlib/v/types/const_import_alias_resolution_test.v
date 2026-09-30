module types

import v.flat

fn test_const_length_does_not_fall_back_to_module_when_alias_differs_across_files() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.const_exprs['fixture.max_name_size'] = a.add_val(.int_literal, '256')
	tc.const_exprs['second.max_name_size'] = a.add_val(.int_literal, '128')
	tc.const_exprs['fx.max_name_size'] = a.add_val(.int_literal, '16')
	tc.file_imports_by_file['main.v'] = &FileImportInfo{
		imports: {
			'fx':      'fixture'
			'otherfx': 'fx'
		}
	}
	tc.file_imports_by_file['second_import.v'] = &FileImportInfo{
		imports: {
			'fx': 'second'
		}
	}
	if value := tc.const_int_value('fx.max_name_size', []string{}) {
		assert false, 'ambiguous alias folded to ${value}'
	}
	assert tc.const_int_value('fixture.max_name_size', []string{}) or { -1 } == 256
	assert tc.const_int_value('second.max_name_size', []string{}) or { -1 } == 128
	assert tc.const_int_value('otherfx.max_name_size', []string{}) or { -1 } == 16
}

fn test_const_length_resolves_unique_alias_and_unaliased_module() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.const_exprs['fixture.max_name_size'] = a.add_val(.int_literal, '256')
	tc.const_exprs['fx.max_name_size'] = a.add_val(.int_literal, '16')
	tc.file_imports_by_file['main.v'] = &FileImportInfo{
		imports: {
			'fx': 'fixture'
		}
	}
	tc.file_imports_by_file['other.v'] = &FileImportInfo{
		imports: {
			'fx': 'fixture'
		}
	}
	assert tc.const_int_value('fx.max_name_size', []string{}) or { -1 } == 256
	tc.file_imports_by_file.clear()
	assert tc.const_int_value('fx.max_name_size', []string{}) or { -1 } == 16
}

fn test_const_length_ignores_alias_of_module_without_the_const() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.const_exprs['fx.max_name_size'] = a.add_val(.int_literal, '16')
	tc.file_imports_by_file['main.v'] = &FileImportInfo{
		imports: {
			'fx': 'fx'
		}
	}
	tc.file_imports_by_file['other.v'] = &FileImportInfo{
		imports: {
			'fx': 'strings'
		}
	}
	assert tc.const_int_value('fx.max_name_size', []string{}) or { -1 } == 16
}
