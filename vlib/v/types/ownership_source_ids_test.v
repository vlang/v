module types

import v.flat

fn test_generated_borrowed_projection_copies_are_private_and_reset() {
	$if ownership ? {
		mut a := flat.FlatAst.new()
		source := a.add_node(flat.Node{ kind: .ident, value: 'source' })
		first := a.add_node(flat.Node{ kind: .ident, value: 'first' })
		second := a.add_node(flat.Node{ kind: .ident, value: 'second' })
		mut tc := TypeChecker.new(&a)
		tc.ownership = new_ownership_state()
		tc.ownership.borrowed_projection_actions[int(source)] = .clone_value
		tc.note_generated_borrowed_projection_copy(source, first)
		tc.note_generated_borrowed_projection_copy(first, second)
		assert tc.ownership_expr_is_borrowed_projection(first)
		assert tc.ownership_expr_is_borrowed_projection(second)
		worker := tc.fork_for_parallel_transform(&a)
		assert worker.ownership_expr_is_borrowed_projection(source)
		assert !worker.ownership_expr_is_borrowed_projection(first)
		assert !worker.ownership_expr_is_borrowed_projection(second)
		worker.note_generated_borrowed_projection_copy(source, first)
		assert worker.ownership_expr_is_borrowed_projection(first)
		assert !worker.ownership_expr_is_borrowed_projection(second)
		// Exercise the recycled-cache path, which must discard generated ids too.
		tc.pre_transform_type_cache = tc.type_cache
		tc.set_fresh_type_cache(false)
		assert tc.ownership_expr_is_borrowed_projection(source)
		assert !tc.ownership_expr_is_borrowed_projection(first)
		assert !tc.ownership_expr_is_borrowed_projection(second)
	}
}

fn test_ownership_source_ids_keep_wrapper_drop_targets() {
	$if ownership ? {
		mut a := flat.FlatAst.new()
		mut tc := TypeChecker.new(&a)
		tc.ownership = new_ownership_state()
		for spelling in ['?pkg.Resource[int]', '!pkg.Resource[int]', '&?pkg.Resource[int]',
			'&&!pkg.Resource[int]'] {
			target := tc.ownership_drop_target_for_type_name(flat.source_type_id(spelling)) or {
				assert false
				continue
			}
			assert target.type_name == flat.source_type_id(spelling.trim_left('&'))
			assert !target.optional_wrapper
		}
		assert tc.ownership_drop_target_for_type_name(0) == none
		string_alias := Type(Alias{
			name_id:   flat.source_type_id('pkg.Text')
			base_type: &builtin_string_type
		})
		target := tc.ownership_drop_target_for_resolved_type(string_alias, true) or {
			assert false
			return
		}
		assert target.type_name == flat.source_type_string
		assert target.optional_wrapper
	}
}

fn test_ownership_source_ids_preserve_marker_generic_and_prefix_rules() {
	$if ownership ? {
		mut a := flat.FlatAst.new()
		mut tc := TypeChecker.new(&a)
		tc.ownership = new_ownership_state()
		base_id := flat.source_type_id('pkg.Resource')
		tc.ownership.drop_structs[base_id] = true
		for spelling in ['pkg.Resource', 'pkg.Resource[map[string][]int]', '?pkg.Resource[int]',
			'&&??pkg.Resource[int]'] {
			assert tc.ownership_type_has_explicit_drop(flat.source_type_id(spelling))
		}
		assert !tc.ownership_type_has_explicit_drop(flat.source_type_id('!pkg.Resource[int]'))
		for spelling in ['', 'pkg.Resource', '[]pkg.Resource', '[4]pkg.Resource', '[4]pkg.Resource[int]',
			'pkg.Resource[map[string][]int]'] {
			id := flat.source_type_id(spelling)
			assert ownership_generic_base_type_name(id) == flat.source_type_id(generic_base_name(spelling))
		}
	}
}

fn test_ownership_source_ids_keep_first_clone_failure_and_diagnostic_spelling() {
	$if ownership ? {
		mut a := flat.FlatAst.new()
		mut tc := TypeChecker.new(&a)
		tc.ownership = new_ownership_state()
		key_id := flat.source_type_id('first.Resource[map[string]int]')
		value_id := flat.source_type_id('second.Resource')
		typ := Type(OptionType{
			base_type: &Type(Array{
				elem_type: &Type(Map{
					key_type:   &Type(Interface{ name_id: key_id })
					value_type: &Type(Interface{ name_id: value_id })
				})
			})
		})
		mut seen := map[u32]bool{}
		assert tc.ownership_default_clone_missing_method_inner(typ, mut seen)? == key_id
		assert tc.ownership_default_clone_missing_method(typ)? == 'first.Resource[map[string]int]'
	}
}

fn test_ownership_source_ids_clone_compatibility_keeps_short_name_fallback() {
	$if ownership ? {
		mut a := flat.FlatAst.new()
		mut tc := TypeChecker.new(&a)
		resource := Type(Struct{ name_id: flat.source_type_id('pkg.Resource') })
		for spelling in ['pkg.Resource', 'other.Resource', 'other.Different', ''] {
			tc.fn_ret_types['pkg.Resource.clone'] = &Type(Struct{
				name_id: flat.source_type_id(spelling)
			})
			assert tc.ownership_ierror_has_compatible_clone_method(resource) == (spelling in [
				'pkg.Resource',
				'other.Resource',
			])
		}
		tc.fn_ret_types['pkg.Resource.clone'] = &Type(Pointer{ base_type: &resource })
		assert tc.ownership_ierror_has_compatible_clone_method(resource)
	}
}

fn test_ownership_source_ids_keep_global_and_descendant_metadata_defaults() {
	$if ownership ? {
		mut a := flat.FlatAst.new()
		mut tc := TypeChecker.new(&a)
		resource_id := flat.source_type_id('pkg.Resource[map[string]int]')
		tc.ownership_mark_owned_global('resource', 'pkg.resource', '.field', resource_id)
		tc.ownership_mark_owned_global('local', 'local', '.empty', 0)
		assert tc.ownership.owned_globals.type_id('pkg.resource.field')? == resource_id
		assert !tc.ownership.owned_globals.contains('resource.field')
		assert tc.ownership.owned_globals.type_id('local.empty')? == 0
		assert tc.ownership_owned_global_type_name('pkg.resource')? == resource_id
		tc.ownership_add_fn_return_descendant('make', 0, '.field', resource_id)
		tc.ownership_add_fn_return_descendant('make', 0, '.field', 0)
		tc.ownership_add_fn_return_descendant('make', 1, '.empty', 0)
		returns := tc.ownership.ownership_fn_return_descs['make']
		assert returns.len == 2
		assert returns[0].type_name == resource_id
		assert returns[1].type_name == flat.source_type_string
		tc.ownership_add_fn_param_descendant('take', 0, '.field', resource_id)
		tc.ownership_add_fn_param_descendant('take', 0, '.field', 0)
		tc.ownership_add_fn_param_descendant('take', 1, '.empty', 0)
		params := tc.ownership.ownership_fn_param_descs['take']
		assert params.len == 2
		assert params[0].type_name == resource_id
		assert params[1].type_name == flat.source_type_string
	}
}
