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

fn test_const_import_alias_snapshot_preserves_ambiguity_and_ignores_unrelated_modules() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.const_exprs['first.limit'] = a.add_val(.int_literal, '32')
	tc.const_exprs['second.limit'] = a.add_val(.int_literal, '64')
	for file, module_name in {
		'first.v':  'first'
		'repeat.v': 'first'
		'other.v':  'unrelated'
	} {
		tc.cur_file = file
		tc.register_file_import('dep', module_name)
	}
	tc.prepare_check_name_filters()
	assert tc.const_import_modules['dep'].len == 2
	assert tc.const_int_value('dep.limit', []string{}) or { -1 } == 32
	fork := tc.fork_for_parallel_codegen()
	assert fork.const_import_files_count == 3
	assert fork.const_int_value('dep.limit', []string{}) or { -1 } == 32
	// Updating an existing file leaves the file count unchanged.
	tc.cur_file = 'repeat.v'
	tc.register_file_import('dep', 'second')
	assert tc.const_import_files_count == -1
	_, ambiguous := tc.const_import_alias_key('dep', 'limit')
	assert ambiguous
	tc.prepare_check_name_filters()
	_, indexed_ambiguous := tc.const_import_alias_key('dep', 'limit')
	assert indexed_ambiguous
	// The const table is consulted live, independently of the alias snapshot.
	tc.const_exprs.delete('second.limit')
	assert tc.const_int_value('dep.limit', []string{}) or { -1 } == 32
}

fn test_const_import_alias_snapshot_falls_back_when_files_are_added_or_cleared() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.const_exprs['first.limit'] = a.add_val(.int_literal, '32')
	tc.const_exprs['second.limit'] = a.add_val(.int_literal, '64')
	tc.cur_file = 'first.v'
	tc.register_file_import('dep', 'first')
	tc.prepare_check_name_filters()
	tc.file_imports_by_file['second.v'] = &FileImportInfo{
		imports: {
			'dep': 'second'
		}
	}
	_, ambiguous := tc.const_import_alias_key('dep', 'limit')
	assert ambiguous
	tc.file_imports_by_file.clear()
	key, cleared_ambiguous := tc.const_import_alias_key('dep', 'limit')
	assert key == ''
	assert !cleared_ambiguous
}
