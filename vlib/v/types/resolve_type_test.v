module types

import v.flat

fn integer_widen_cast(mut a flat.FlatAst, typ string) flat.NodeId {
	value := a.add_node(flat.Node{ kind: .int_literal, value: '1' })
	children_start := a.begin_children()
	a.add_child(value)
	return a.add_node(flat.Node{
		kind:           .cast_expr
		typ:            typ
		children_start: children_start
		children_count: 1
	})
}

fn integer_widen_infix(mut a flat.FlatAst, left flat.NodeId, right flat.NodeId, op flat.Op) flat.NodeId {
	children_start := a.begin_children()
	a.add_child(left)
	a.add_child(right)
	return a.add_node(flat.Node{
		kind:           .infix
		op:             op
		children_start: children_start
		children_count: 2
	})
}

fn test_integer_widening_retries_provisional_operands() {
	mut a := flat.FlatAst.new()
	left := integer_widen_cast(mut a, 'u64')
	right := a.add_node(flat.Node{ kind: .ident, value: 'late' })
	inner := integer_widen_infix(mut a, left, right, .plus)
	outer := integer_widen_infix(mut a, left, inner, .plus)
	mut tc := TypeChecker.new(&a)
	tc.extend_node_caches(a.nodes.len)
	tc.arm_body_resolve_memo(0, a.nodes.len - 1)
	tc.register_synth_type(inner, builtin_u64_type)
	tc.register_synth_type(outer, builtin_u64_type)
	assert tc.resolve_type(outer).name() == 'u64'
	// Inference may supply a binding before that identifier is checked/published.
	tc.file_scope.insert('late', builtin_u128_type)
	assert tc.resolve_type(outer).name() == 'u128'
}

fn test_integer_widening_updates_cached_ancestors_on_child_rewrite() {
	mut a := flat.FlatAst.new()
	left := integer_widen_cast(mut a, 'u64')
	right := integer_widen_cast(mut a, 'u64')
	inner := integer_widen_infix(mut a, left, right, .plus)
	outer := integer_widen_infix(mut a, left, inner, .plus)
	mut tc := TypeChecker.new(&a)
	tc.extend_node_caches(a.nodes.len)
	tc.register_synth_type(inner, builtin_u64_type)
	tc.register_synth_type(outer, builtin_u64_type)
	tc.trust_checked_expr_types = true
	assert tc.resolve_type(outer).name() == 'u64'
	a.nodes[int(right)].typ = 'u128'
	tc.invalidate_checked_expr_type(int(right))
	assert tc.resolve_type(outer).name() == 'u128'
	assert tc.expr_type_values[int(outer)].name() == 'u64'
	a.nodes[int(right)].typ = 'u64'
	tc.invalidate_checked_expr_type(int(right))
	assert tc.resolve_type(outer).name() == 'u64'
}

fn test_integer_widening_primitive_predicates_match_type_names() {
	for bits in 0 .. 32 {
		for size in [u8(0), 1, 7, 8, 16, 24, 32, 63, 64, 65, 127, 128, 255] {
			typ := Type(Primitive{
				// All values here are combinations of the five declared flag bits.
				props: unsafe { Properties(bits) }
				size:  size
			})
			name := short_name_view(typ.name())
			assert narrow_integer_type_for_widening(typ) == (name in narrow_integer_type_names)
			assert wide_integer_type_for_widening(typ) == (name in ['u128', 'i128'])
		}
	}
	for typ in [Type(Alias{ name: 'sample.u64', base_type: builtin_f64_type }),
		Type(Alias{ name: 'sample.u128', base_type: builtin_int_type }),
		Type(Struct{ name: 'sample.i128' }),
		Type(Pointer{ base_type: Type(Struct{ name: 'sample.u128' }) }), Type(rune_), Type(char_),
		Type(isize_), Type(usize_), Type(unknown_type('unresolved'))] {
		name := short_name_view(typ.name())
		assert narrow_integer_type_for_widening(typ) == (name in narrow_integer_type_names)
		assert wide_integer_type_for_widening(typ) == (name in ['u128', 'i128'])
	}
}

fn test_resolve_type_treats_return_statements_as_void() {
	mut a := flat.FlatAst.new()
	value := a.add_node(flat.Node{
		kind:  .int_literal
		value: '1'
	})
	children_start := a.begin_children()
	a.add_child(value)
	return_with_value := a.add_node(flat.Node{
		kind:           .return_stmt
		children_start: children_start
		children_count: 1
	})
	bare_return := a.add_node(flat.Node{
		kind: .return_stmt
	})
	tc := TypeChecker.new(&a)

	assert tc.resolve_type(return_with_value) is Void
	assert tc.resolve_type(bare_return) is Void
}

fn test_infix_primitive_keeps_exact_declared_operator() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	for lhs in [Type(int_), Type(u64_), Type(f64_), Type(u128_), Type(bool_)] {
		assert tc.infix_operator_signature(.plus, lhs) == none
	}
	tc.fn_ret_types['u64.+'] = Type(u64_)
	tc.fn_param_types['u64.+'] = [Type(u64_), Type(u64_)]
	signature := tc.infix_operator_signature(.plus, Type(u64_)) or { panic('missing operator') }
	assert signature.param_count == 2
	assert signature.return_type == Type(u64_)
	assert tc.infix_operator_return_type(.plus, Type(u64_), Type(u64_)) or { Type(void_) } == Type(u64_)
}

fn test_infix_primitive_keeps_alias_operator_and_primitive_fallback() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	alias := Type(Alias{ name: 'Count', base_type: Type(u64_) })
	tc.type_aliases['Count'] = 'u64'
	tc.fn_ret_types['Count.+'] = alias
	tc.fn_param_types['Count.+'] = [alias, alias]
	signature := tc.infix_operator_signature(.plus, alias) or { panic('missing alias operator') }
	assert signature.return_type == alias
	// An alias without its own operator keeps the existing exact primitive method.
	tc.fn_ret_types.delete('Count.+')
	tc.fn_param_types.delete('Count.+')
	tc.fn_ret_types['u64.+'] = Type(u64_)
	tc.fn_param_types['u64.+'] = [Type(u64_), Type(u64_)]
	fallback := tc.infix_operator_signature(.plus, alias) or { panic('missing base operator') }
	assert fallback.return_type == Type(u64_)
}

fn test_infix_primitive_keeps_generic_struct_phantom_and_concrete_operators() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.structs['Box'] = []StructField{}
	tc.struct_generic_params['Box'] = ['T']
	tc.fn_ret_types['Box[T].+'] = Type(Struct{ name: 'Box' })
	tc.fn_ret_type_texts['Box[T].+'] = 'Box[T]'
	tc.fn_param_types['Box[T].+'] = [Type(Struct{ name: 'Box' }), Type(Struct{ name: 'Box' })]
	tc.fn_param_type_texts['Box[T].+'] = ['Box[T]', 'Box[T]']
	for lhs in [Type(Struct{ name: 'Box' }), Type(Struct{ name: 'Box[int]' })] {
		signature := tc.infix_operator_signature(.plus, lhs) or { panic('missing generic operator') }
		assert signature.param_count == 2
		assert signature.return_type is Struct
	}
}

fn test_infix_primitive_keeps_generic_owner_metadata_candidates() {
	// Preserve the resolver's conservative behavior for synthetic/shadowed
	// metadata under primitive spellings, including module/import context.
	for owner in ['u64', 'other.Number', 'Number'] {
		mut a := flat.FlatAst.new()
		mut tc := TypeChecker.new(&a)
		tc.cur_module = 'dep'
		tc.cur_file = '/tmp/primitive_infix.v'
		tc.struct_generic_params[owner] = ['T']
		tc.structs[owner] = []StructField{}
		if owner != 'u64' {
			tc.structs['other.Number'] = []StructField{}
			tc.file_imports_by_file[tc.cur_file] = &FileImportInfo{
				selective_imports: {
					'u64': ['other.Number']
				}
			}
		}
		key := '${owner}[T].+'
		tc.fn_ret_types[key] = Type(u64_)
		tc.fn_param_types[key] = [Type(u64_), Type(u64_)]
		expected := tc.resolve_generic_struct_method('u64', '+') or { panic('missing baseline candidate ${owner}') }
		signature := tc.infix_operator_signature(.plus, Type(u64_)) or { panic('missing generic fallback ${owner}') }
		assert signature.param_count == expected.params.len
		assert signature.return_type == expected.return_type
	}
}

fn test_infix_primitive_keeps_generic_alias_redirect_metadata() {
	for alias_key in ['u64', 'dep.u64'] {
		mut a := flat.FlatAst.new()
		mut tc := TypeChecker.new(&a)
		tc.cur_module = 'dep'
		tc.type_aliases[alias_key] = 'Box[int]'
		tc.struct_generic_params['Box'] = ['T']
		tc.structs['Box'] = []StructField{}
		tc.fn_ret_types['Box[T].+'] = Type(u64_)
		tc.fn_param_types['Box[T].+'] = [Type(u64_), Type(u64_)]
		if alias_key == 'dep.u64' {
			// Builtin names are never module-qualified by this resolver.
			assert tc.resolve_generic_struct_method('u64', '+') == none
			assert tc.infix_operator_signature(.plus, Type(u64_)) == none
		} else {
			expected := tc.resolve_generic_struct_method('u64', '+') or { panic('missing redirected operator') }
			signature := tc.infix_operator_signature(.plus, Type(u64_)) or { panic('missing alias redirect fallback') }
			assert signature.return_type == expected.return_type
			assert signature.param_count == expected.params.len
		}
	}
}
