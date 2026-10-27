module types

import v.flat

fn test_unqualified_builtin_type_candidates_keep_their_declared_owners() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_file = 'consumer.v'
	tc.cur_module = 'consumer'
	tc.struct_modules[flat.source_type_id('Error')] = 'builtin'
	tc.struct_modules[flat.source_type_id('User')] = 'other'
	tc.enum_names[flat.source_type_id('BuiltinKind')] = true
	tc.flag_enums[flat.source_type_id('BuiltinFlags')] = true
	assert tc.unqualified_type_symbol_is_builtin('Error')
	assert tc.unqualified_type_symbol_is_builtin('IError')
	assert tc.unqualified_type_symbol_is_builtin('BuiltinKind')
	assert tc.unqualified_type_symbol_is_builtin('BuiltinFlags')
	assert !tc.unqualified_type_symbol_is_builtin('User')
	assert !tc.unqualified_type_symbol_is_builtin('Unknown')
	assert !tc.unqualified_type_symbol_is_builtin('')
	// A non-builtin struct owner wins even if another table has the bare name.
	tc.enum_names[flat.source_type_id('User')] = true
	assert !tc.unqualified_type_symbol_is_builtin('User')
}

fn test_unqualified_builtin_types_are_shadowed_by_every_local_type_kind() {
	for kind in 0 .. 6 {
		a := flat.FlatAst.new()
		mut tc := TypeChecker.new(&a)
		tc.cur_file = 'consumer.v'
		tc.cur_module = 'consumer'
		tc.struct_modules[flat.source_type_id('Error')] = 'builtin'
		assert tc.unqualified_type_symbol_is_builtin('Error')
		match kind {
			0 { tc.type_aliases.set(flat.source_type_id('consumer.Error'), 'int') }
			1 { tc.structs[flat.source_type_id('consumer.Error')] = []StructField{} }
			2 { tc.interface_names[flat.source_type_id('consumer.Error')] = true }
			3 { tc.flag_enums[flat.source_type_id('consumer.Error')] = true }
			4 { tc.enum_names[flat.source_type_id('consumer.Error')] = true }
			5 { tc.sum_types.set(flat.source_type_id('consumer.Error'), ['int', 'string']) }
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
	tc.struct_modules[flat.source_type_id('Error')] = 'builtin'
	tc.declared_type_scope_keys[scope_type_key('consumer.v', 'consumer', flat.source_type_id('Error'))] = true
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
		tc.struct_modules[flat.source_type_id('Error')] = 'builtin'
		candidates := if owner == 'ambiguous' { []string{} } else { ['${owner}.Error'] }
		tc.file_selective_imports[file_import_key(tc.cur_file, 'Error')] = candidates
		tc.file_imports_by_file[tc.cur_file] = &FileImportInfo{
			selective_imports: {
				'Error': candidates
			}
		}
		if owner != 'ambiguous' {
			tc.structs[flat.source_type_id('${owner}.Error')] = []StructField{}
			tc.struct_modules[flat.source_type_id('${owner}.Error')] = owner
		}
		assert tc.unqualified_type_symbol_is_builtin('Error') == (owner == 'builtin'), owner
	}
}

fn test_numeric_type_scope_keys_preserve_context_and_exact_names() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	name_id := flat.source_type_id('ScopedType')
	qualified_id := flat.source_type_id('owner.ScopedType')
	other_id := flat.source_type_id('OtherType')
	assert scope_type_key('scope.v', '', name_id) == scope_type_key('scope.v', 'main', name_id)
	assert scope_type_key('scope.v', 'one', name_id) != scope_type_key('scope.v', 'two', name_id)
	assert scope_type_key('scope.v', 'one', name_id) != scope_type_key('other.v', 'one', name_id)
	assert scope_type_key('scope.v', 'one', name_id) != scope_type_key('scope.v', 'one', other_id)
	assert scope_type_key('scope.v', 'one', 0) != scope_type_key('scope.v', 'one', name_id)

	tc.declared_type_scope_keys[scope_type_key('scope.v', 'main', name_id)] = true
	assert tc.source_declares_type_in_scope(name_id, 'scope.v', '')
	assert tc.source_declares_type_in_scope(name_id, 'scope.v', 'main')
	assert !tc.source_declares_type_in_scope(name_id, 'other.v', 'main')
	assert !tc.source_declares_type_in_scope(name_id, 'scope.v', 'other')
	assert !tc.source_declares_type_in_scope(name_id, '', 'main')
	assert !tc.source_declares_type_in_scope(qualified_id, 'scope.v', 'main')
	tc.declared_type_scope_keys[scope_type_key('scope.v', 'owner', qualified_id)] = true
	assert tc.source_declares_type_in_scope(qualified_id, 'scope.v', 'owner')
	assert !tc.source_declares_type_in_scope(name_id, 'scope.v', 'owner')
	without_ast := TypeChecker{}
	assert !without_ast.source_declares_type_in_scope(name_id, 'scope.v', 'main')

	tc.cur_file = 'scope.v'
	tc.cur_module = 'owner'
	tc.concrete_type_scope_keys[scope_type_key(tc.cur_file, tc.cur_module, name_id)] = true
	assert tc.concrete_type_declared_in_current_file(name_id)
	assert tc.concrete_type_declared_in_current_file(qualified_id)
	assert !tc.concrete_type_declared_in_current_file(other_id)
	tc.cur_file = 'other.v'
	assert !tc.concrete_type_declared_in_current_file(qualified_id)
}
