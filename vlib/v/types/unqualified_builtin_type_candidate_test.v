module types

import v.flat

fn test_unqualified_builtin_type_candidates_keep_their_declared_owners() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_file = 'consumer.v'
	tc.cur_module = 'consumer'
	tc.struct_modules['Error'] = 'builtin'
	tc.struct_modules['User'] = 'other'
	tc.enum_names['BuiltinKind'] = true
	tc.flag_enums['BuiltinFlags'] = true
	assert tc.unqualified_type_symbol_is_builtin('Error')
	assert tc.unqualified_type_symbol_is_builtin('IError')
	assert tc.unqualified_type_symbol_is_builtin('BuiltinKind')
	assert tc.unqualified_type_symbol_is_builtin('BuiltinFlags')
	assert !tc.unqualified_type_symbol_is_builtin('User')
	assert !tc.unqualified_type_symbol_is_builtin('Unknown')
	assert !tc.unqualified_type_symbol_is_builtin('')
	// A non-builtin struct owner wins even if another table has the bare name.
	tc.enum_names['User'] = true
	assert !tc.unqualified_type_symbol_is_builtin('User')
}

fn test_unqualified_builtin_types_are_shadowed_by_every_local_type_kind() {
	for kind in 0 .. 6 {
		a := flat.FlatAst.new()
		mut tc := TypeChecker.new(&a)
		tc.cur_file = 'consumer.v'
		tc.cur_module = 'consumer'
		tc.struct_modules['Error'] = 'builtin'
		assert tc.unqualified_type_symbol_is_builtin('Error')
		match kind {
			0 { tc.type_aliases.set('consumer.Error', 'int') }
			1 { tc.structs['consumer.Error'] = []StructField{} }
			2 { tc.interface_names['consumer.Error'] = true }
			3 { tc.flag_enums['consumer.Error'] = true }
			4 { tc.enum_names['consumer.Error'] = true }
			5 { tc.sum_types.set('consumer.Error', ['int', 'string']) }
			else {}
		}
		assert !tc.unqualified_type_symbol_is_builtin('Error'), 'type kind ${kind}'
	}
}

fn test_unqualified_builtin_type_source_shadow_tracks_the_active_file() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_file = 'consumer.v'
	tc.cur_module = 'consumer'
	tc.struct_modules['Error'] = 'builtin'
	tc.declared_type_scope_keys[scope_type_key('consumer.v', 'consumer', 'Error')] = true
	assert !tc.unqualified_type_symbol_is_builtin('Error')
	tc.cur_file = 'other.v'
	assert tc.unqualified_type_symbol_is_builtin('Error')
}

fn test_unqualified_builtin_type_selective_imports_keep_shadow_priority() {
	for owner in ['other', 'builtin', 'ambiguous'] {
		a := flat.FlatAst.new()
		mut tc := TypeChecker.new(&a)
		tc.cur_file = 'consumer.v'
		tc.cur_module = 'consumer'
		tc.struct_modules['Error'] = 'builtin'
		candidates := if owner == 'ambiguous' { []string{} } else { ['${owner}.Error'] }
		tc.file_selective_imports[file_import_key(tc.cur_file, 'Error')] = candidates
		tc.file_imports_by_file[tc.cur_file] = &FileImportInfo{
			selective_imports: {
				'Error': candidates
			}
		}
		if owner != 'ambiguous' {
			tc.structs['${owner}.Error'] = []StructField{}
			tc.struct_modules['${owner}.Error'] = owner
		}
		assert tc.unqualified_type_symbol_is_builtin('Error') == (owner == 'builtin'), owner
	}
}
