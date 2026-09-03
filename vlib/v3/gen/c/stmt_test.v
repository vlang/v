module c

import v3.flat
import v3.types

fn stmt_test_node(mut a flat.FlatAst, kind flat.NodeKind, value string, children []flat.NodeId) flat.NodeId {
	start := a.children.len
	a.children << children
	return a.add_node(flat.Node{
		kind:           kind
		value:          value
		children_start: i32(start)
		children_count: flat.child_count(children.len)
	})
}

fn stmt_test_prefix(mut a flat.FlatAst, op flat.Op, child flat.NodeId) flat.NodeId {
	start := a.children.len
	a.children << child
	return a.add_node(flat.Node{
		kind:           .prefix
		op:             op
		children_start: i32(start)
		children_count: 1
	})
}

fn test_inline_asm_quoted_label_references_drop_v_quotes() {
	assert lower_c_inline_asm_template("jz '1f'", 'amd64', map[string]bool{}, false) == 'jz 1f'
	assert lower_c_inline_asm_template("jnz '23b'", 'amd64', map[string]bool{}, false) == 'jnz 23b'
	assert lower_c_inline_asm_template("call 'named_target'", 'amd64', map[string]bool{}, false) == 'call named_target'
	assert lower_c_inline_asm_template("jmp 'next_block'", 'i386', map[string]bool{}, false) == 'jmp next_block'
	assert lower_c_inline_asm_template("jmp 'x'", 'amd64', map[string]bool{}, false) == 'jmp x'
}

fn test_inline_asm_quoted_numbered_operands_drop_v_quotes() {
	assert lower_c_inline_asm_template("lock cmpxchgq '%1', '%2'", 'amd64', map[string]bool{}, true) == 'lock cmpxchgq %2, %1'
}

fn test_inline_asm_character_tokens_use_assembly_quotes() {
	aliases := map[string]bool{}
	assert lower_c_inline_asm_template('mov rax, `A`', 'amd64', aliases, false) == "mov 'A', %rax"
	assert lower_c_inline_asm_template('mov rax, `,`', 'amd64', aliases, false) == "mov ',', %rax"
	assert lower_c_inline_asm_template('mov x0, `A`', 'arm64', aliases, false) == "mov x0, 'A'"
}

fn test_inline_asm_x86_addresses_preserve_unscaled_indexes() {
	aliases := map[string]bool{}
	assert lower_c_inline_asm_template('mov rax, [rbx + rcx + 8]', 'amd64', aliases, false) == 'mov 8(%rbx, %rcx, 1), %rax'
	assert lower_c_inline_asm_template('mov eax, [ebx + ecx + 4]', 'i386', aliases, false) == 'mov 4(%ebx, %ecx, 1), %eax'
	assert lower_c_inline_asm_template('lea rax, [rip + named_target]', 'amd64', aliases, false) == 'lea named_target(%rip), %rax'
}

fn test_inline_asm_i386_uses_x86_att_operand_lowering() {
	aliases := map[string]bool{}
	assert lower_c_inline_asm_template('mov eax, ebx', 'i386', aliases, false) == 'mov %ebx, %eax'
	assert lower_c_inline_asm_template('mov eax, 7', 'i386', aliases, false) == 'mov \$7, %eax'
	assert lower_c_inline_asm_template('mov eax, [ebx + ecx*4 + 8]', 'i386', aliases, false) == 'mov 8(%ebx, %ecx, 4), %eax'
}

fn test_inline_asm_x86_register_branch_targets_are_indirect() {
	aliases := {
		'callback': true
	}
	assert lower_c_inline_asm_template('call rax', 'amd64', aliases, false) == 'call *%rax'
	assert lower_c_inline_asm_template('call rax', 'amd64', aliases, true) == 'call *%%rax'
	assert lower_c_inline_asm_template('jmp callback', 'amd64', aliases, true) == 'jmp *%[callback]'
	assert lower_c_inline_asm_template('jmp eax', 'i386', aliases, false) == 'jmp *%eax'
	assert lower_c_inline_asm_template("call 'named_target'", 'amd64', aliases, false) == 'call named_target'
}

fn test_inline_asm_block_comments_do_not_create_operand_sections() {
	source := 'mov rax, "/* ; quoted */"
/* outer ; /* nested ; */ still a comment ; */
mov rbx, "// ; quoted"
// line comment ;
; +r (value)'
	clean := strip_c_inline_asm_comments(source)
	assert clean.contains('"/* ; quoted */"')
	assert clean.contains('"// ; quoted"')
	assert !clean.contains('outer')
	assert !clean.contains('line comment')
	sections := split_c_inline_asm_sections(clean)
	assert sections.len == 2
	assert sections[0].split_into_lines().filter(it.trim_space().len > 0) == [
		'mov rax, "/* ; quoted */"',
		'mov rbx, "// ; quoted"',
	]
	assert sections[1].trim_space() == '+r (value)'
}

fn test_lowered_storage_dereference_prefers_annotated_pointer_type() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	pointer_type := types.Type(types.Pointer{
		base_type: types.Type(types.int_)
	})
	value_id := stmt_test_node(mut a, .ident, 'value', [])
	tc.register_synth_type(value_id, pointer_type)
	children_start := a.children.len
	a.children << value_id
	deref_id := a.add_node(flat.Node{
		kind:           .prefix
		op:             .mul
		typ:            '&int'
		children_start: i32(children_start)
		children_count: 1
	})

	actual := g.usable_expr_type(deref_id)
	assert actual is types.Pointer
	if actual is types.Pointer {
		assert actual.base_type == types.Type(types.int_)
	}
}

fn test_primitive_fixed_array_zero_initializer_is_compact() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc

	large := types.ArrayFixed{
		elem_type: types.Type(types.u8_)
		len:       65536
	}
	assert g.empty_fixed_array_initializer_string(large) == '{0}'

	nested := types.ArrayFixed{
		elem_type: types.Type(types.ArrayFixed{
			elem_type: types.Type(types.i32_)
			len:       32
		})
		len:       32
	}
	assert g.empty_fixed_array_initializer_string(nested) == '{0}'

	dynamic_arrays := types.ArrayFixed{
		elem_type: types.Type(types.Array{
			elem_type: types.Type(types.int_)
		})
		len:       2
	}
	dynamic_init := g.empty_fixed_array_initializer_string(dynamic_arrays)
	assert dynamic_init.count('array_new(') == 2
}

fn test_fixed_array_optional_abi_conversions_use_memcpy() {
	fixed := types.Type(types.ArrayFixed{
		elem_type: types.Type(types.int_)
		len:       2
	})
	mut forward_gen := FlatGen.new()
	forward := forward_gen.optional_forward_return_abi_wrap_expr('Optional_source',
		'Optional_destination', fixed, 'source()')
	assert forward.contains('if (_t1.ok) { memcpy(_t2.value, _t1.value, sizeof(_t2.value)); }'), forward
	assert !forward.contains('.value = _t1.value'), forward

	mut interface_gen := FlatGen.new()
	interface_gen.gen_interface_dispatch_optional_abi_value_return('Optional_destination',
		'_iface_result', fixed)
	interface_output := interface_gen.sb.str()
	assert interface_output.contains('if (_iface_result.ok) {'), interface_output
	copy_statement := 'memcpy(_iface_abi_result_out_0.value, _iface_result.value, sizeof(_iface_abi_result_out_0.value));'
	assert interface_output.contains(copy_statement), interface_output
	assert !interface_output.contains('.value = _iface_result.value'), interface_output
}

fn test_ownership_recursive_drop_helpers_deduplicate_emitted_c_symbol() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	// Distinct logical ownership keys can refer to the same runtime struct after
	// lifetime erasure. Only one C helper definition may be emitted for it.
	g.recursive_drop_helpers['struct:pkg.Sink[^p,^s,Writer]'] = 'pkg.Sink[Writer]'
	g.recursive_drop_helpers['struct:pkg.Sink[^s,^s,Writer]'] = 'pkg.Sink[Writer]'
	g.gen_ownership_recursive_drop_helpers()
	assert g.sb.str().count('static void __v3_ownership_drop_pkg__Sink_Writer(') == 1
}

fn test_usable_resolved_sum_type_requires_exact_qualified_name() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.sum_types['bar.Result'] = ['bar.Failed', 'bar.Ok']
	tc.sum_types['foo.Choice'] = ['foo.Left', 'foo.Right']
	mut g := FlatGen.new()
	g.tc = &tc
	g.precompute_sum_name_lookup()

	preserved := g.usable_resolved_sum_type(types.Type(types.Struct{
		name: 'foo.Result'
	}))
	assert preserved is types.Struct
	if preserved is types.Struct {
		assert preserved.name == 'foo.Result'
	}

	exact := g.usable_resolved_sum_type(types.Type(types.Struct{
		name: 'foo.Choice'
	}))
	assert exact is types.SumType
	if exact is types.SumType {
		assert exact.name == 'foo.Choice'
	}
}

fn test_fixed_array_address_to_byte_pointer_decl_uses_data_pointer() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	fixed_type := types.Type(types.ArrayFixed{
		elem_type: types.Type(types.u8_)
		len:       2
	})
	byte_pointer := types.Type(types.Pointer{
		base_type: types.Type(types.u8_)
	})
	tc.push_scope()
	tc.cur_scope.insert('buf', fixed_type)
	buf_id := stmt_test_node(mut a, .ident, 'buf', [])
	rhs_id := stmt_test_prefix(mut a, .amp, buf_id)
	g.gen_decl_init_expr(rhs_id, a.nodes[int(rhs_id)], byte_pointer, 'u8*', true)
	assert g.sb.str() == '((u8*)(buf))'
	fixed_pointer := types.Type(types.Pointer{
		base_type: fixed_type
	})
	mut assign_gen := FlatGen.new()
	assign_gen.a = &a
	assign_gen.tc = &tc
	p_id := stmt_test_node(mut a, .ident, 'p', [])
	assert assign_gen.gen_fixed_array_address_to_byte_pointer_assign(p_id, rhs_id, byte_pointer,
		fixed_pointer)
	assert assign_gen.sb.str() == 'p = ((u8*)(buf));\n'
	int_fixed_type := types.Type(types.ArrayFixed{
		elem_type: types.Type(types.i32_)
		len:       2
	})
	tc.cur_scope.insert('int_buf', int_fixed_type)
	int_buf_id := stmt_test_node(mut a, .ident, 'int_buf', [])
	int_rhs_id := stmt_test_prefix(mut a, .amp, int_buf_id)
	int_fixed_pointer := types.Type(types.Pointer{
		base_type: int_fixed_type
	})
	mut rejected_gen := FlatGen.new()
	rejected_gen.a = &a
	rejected_gen.tc = &tc
	assert !rejected_gen.gen_fixed_array_address_to_byte_pointer_assign(p_id, int_rhs_id,
		byte_pointer, int_fixed_pointer)
	assert rejected_gen.sb.str() == ''
	tc.pop_scope()
}

fn test_mut_parameter_power_assign_uses_scalar_result_type() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	int_type := types.Type(types.int_)
	ptr_type := types.Type(types.Pointer{
		base_type: int_type
	})

	tc.push_scope()
	arg_owner := tc.cur_scope.insert_with_owner('arg', ptr_type)
	g.cur_param_types['arg'] = ptr_type
	g.cur_mut_params['arg'] = true
	g.cur_mut_param_owners['arg'] = arg_owner
	arg_id := stmt_test_node(mut a, .ident, 'arg', [])
	exponent_id := stmt_test_node(mut a, .int_literal, '2', [])
	children_start := a.children.len
	a.children << arg_id
	a.children << exponent_id
	g.gen_assign(flat.Node{
		kind:           .assign
		op:             .power_assign
		children_start: i32(children_start)
		children_count: 2
	})
	compact := g.sb.str().replace('\t', '').replace(' ', '').replace('\n', '')
	assert compact.contains('*arg=((i64)__v_pow_i64('), compact
	assert !compact.contains('(i64*)__v_pow_i64('), compact
	tc.pop_scope()
}

fn test_local_pointer_alias_clear_preserves_outer_branch_markers() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.tc = &tc
	ptr_type := types.Type(types.Pointer{
		base_type: types.Type(types.int_)
	})

	tc.push_scope()
	outer_owner := tc.cur_scope.insert_with_owner('p', ptr_type)
	g.declare_local_pointer_alias_source(outer_owner, 'x')
	assert g.local_pointer_alias_assignment_can_clear(outer_owner)

	tc.push_scope()
	g.enter_conditional_branch(true)
	assert !g.local_pointer_alias_assignment_can_clear(outer_owner)

	branch_owner := tc.cur_scope.insert_with_owner('q', ptr_type)
	g.declare_local_pointer_alias_source(branch_owner, 'y')
	assert g.local_pointer_alias_assignment_can_clear(branch_owner)

	tc.push_scope()
	assert g.local_pointer_alias_assignment_can_clear(branch_owner)
	nested_owner := tc.cur_scope.insert_with_owner('r', ptr_type)
	g.declare_local_pointer_alias_source(nested_owner, 'z')
	assert g.local_pointer_alias_assignment_can_clear(nested_owner)
	tc.pop_scope()

	g.leave_conditional_branch()
	tc.pop_scope()

	g.enter_conditional_branch(false)
	assert !g.local_pointer_alias_assignment_can_clear(outer_owner)
	g.leave_conditional_branch()

	assert g.local_pointer_alias_assignment_can_clear(outer_owner)
	tc.pop_scope()
}

fn test_fn_decl_signature_registration_preserves_call_name_aliases() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.cur_module = 'pkg'
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	name := 'Widget.call'
	full_name := qualify_name_in_module(tc.cur_module, name)
	params := [types.Type(types.int_)]
	shared_flags := [true]
	g.register_fn_decl_signature(name, full_name, params, shared_flags, true, true, 'int')
	aliases := [
		fn_decl_module_key(tc.cur_module, name),
		name,
		g.cname(name),
		'${tc.cur_module}.${name}',
		g.cname('${tc.cur_module}.${name}'),
	]
	for alias in aliases {
		assert g.fn_decl_param_types[alias] == params
		assert g.fn_decl_ret_types[alias] or { types.Type(types.void_) } == types.Type(types.int_)
		assert g.fn_decl_variadic[alias]
		if alias != fn_decl_module_key(tc.cur_module, name) {
			assert g.fn_decl_shared_params[alias] == shared_flags
			assert g.fn_decl_mut_receivers[alias]
		}
	}
}

fn test_local_pointer_alias_branch_assignment_merges_outer_markers() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	int_type := types.Type(types.int_)
	ptr_type := types.Type(types.Pointer{
		base_type: int_type
	})

	tc.push_scope()
	tc.cur_scope.insert_with_owner('x', int_type)
	arg_owner := tc.cur_scope.insert_with_owner('arg', int_type)
	g.cur_mut_params['arg'] = true
	g.cur_mut_param_owners['arg'] = arg_owner
	p_owner := tc.cur_scope.insert_with_owner('p', ptr_type)
	g.declare_local_pointer_alias_source(p_owner, 'x')

	tc.push_scope()
	g.enter_conditional_branch(true)
	arg_id := stmt_test_node(mut a, .ident, 'arg', [])
	amp_arg := stmt_test_prefix(mut a, .amp, arg_id)
	g.track_local_pointer_alias_assign(flat.Node{
		kind:  .ident
		value: 'p'
	}, amp_arg)
	assert g.local_pointer_alias_source('p') or { '' } == 'x'
	assert !g.local_pointer_alias_source_is_mut_param('p')
	g.leave_conditional_branch()
	tc.pop_scope()

	maybe_owner := tc.cur_scope.insert_with_owner('maybe', ptr_type)
	tc.push_scope()
	g.enter_conditional_branch(true)
	x_id := stmt_test_node(mut a, .ident, 'x', [])
	amp_x := stmt_test_prefix(mut a, .amp, x_id)
	g.track_local_pointer_alias_assign(flat.Node{
		kind:  .ident
		value: 'maybe'
	}, amp_x)
	assert g.local_pointer_alias_source('maybe') or { '' } == ''
	assert !g.local_pointer_alias_assignment_can_clear(maybe_owner)
	g.leave_conditional_branch()
	tc.pop_scope()

	g.declare_local_pointer_alias_source_kind(p_owner, 'arg', true)
	tc.push_scope()
	g.enter_conditional_branch(true)
	g.track_local_pointer_alias_assign(flat.Node{
		kind:  .ident
		value: 'p'
	}, amp_x)
	assert g.local_pointer_alias_source('p') or { '' } == 'arg'
	assert !g.local_pointer_alias_source_is_mut_param('p')
	g.leave_conditional_branch()
	tc.pop_scope()
	tc.pop_scope()
}

fn test_local_pointer_alias_branch_assignment_without_outer_marker_stays_conditional() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	int_type := types.Type(types.int_)
	ptr_type := types.Type(types.Pointer{
		base_type: int_type
	})

	tc.push_scope()
	tc.cur_scope.insert_with_owner('x', int_type)
	tc.cur_scope.insert_with_owner('p', ptr_type)
	tc.push_scope()
	g.enter_conditional_branch(true)
	x_id := stmt_test_node(mut a, .ident, 'x', [])
	amp_x := stmt_test_prefix(mut a, .amp, x_id)
	g.track_local_pointer_alias_assign(flat.Node{
		kind:  .ident
		value: 'p'
	}, amp_x)
	assert g.local_pointer_alias_source('p') or { '' } == ''
	assert !g.local_pointer_alias_source_is_mut_param('p')
	g.leave_conditional_branch()
	tc.pop_scope()
	tc.pop_scope()
}

fn test_pointer_alias_stack_source_propagates_identifier_aliases() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	int_type := types.Type(types.int_)
	ptr_type := types.Type(types.Pointer{
		base_type: int_type
	})

	tc.push_scope()
	tc.cur_scope.insert_with_owner('x', int_type)
	p_owner := tc.cur_scope.insert_with_owner('p', ptr_type)
	x_id := stmt_test_node(mut a, .ident, 'x', [])
	amp_x := stmt_test_prefix(mut a, .amp, x_id)
	g.track_local_pointer_alias_source(flat.Node{
		kind:  .ident
		value: 'p'
	}, p_owner, amp_x, ptr_type)
	assert g.local_pointer_alias_source('p') or { '' } == 'x'
	assert !g.local_pointer_alias_source_is_mut_param('p')

	q_owner := tc.cur_scope.insert_with_owner('q', ptr_type)
	p_id := stmt_test_node(mut a, .ident, 'p', [])
	g.track_local_pointer_alias_source(flat.Node{
		kind:  .ident
		value: 'q'
	}, q_owner, p_id, ptr_type)
	assert g.local_pointer_alias_source('q') or { '' } == 'x'
	assert !g.local_pointer_alias_source_is_mut_param('q')

	assigned_owner := tc.cur_scope.insert_with_owner('assigned', ptr_type)
	g.declare_local_pointer_alias_source(assigned_owner, '')
	g.track_local_pointer_alias_assign(flat.Node{
		kind:  .ident
		value: 'assigned'
	}, p_id)
	assert g.local_pointer_alias_source('assigned') or { '' } == 'x'

	mut_owner := tc.cur_scope.insert_with_owner('mut_alias', ptr_type)
	g.declare_local_pointer_alias_source_kind(mut_owner, 'x', true)
	mut_id := stmt_test_node(mut a, .ident, 'mut_alias', [])
	mut_copy_owner := tc.cur_scope.insert_with_owner('mut_copy', ptr_type)
	g.track_local_pointer_alias_source(flat.Node{
		kind:  .ident
		value: 'mut_copy'
	}, mut_copy_owner, mut_id, ptr_type)
	assert g.local_pointer_alias_source('mut_copy') or { '' } == 'x'
	assert g.local_pointer_alias_source_is_mut_param('mut_copy')
	tc.pop_scope()
}

fn test_heap_local_memdup_expr_uses_aligned_memdup_for_aligned_structs() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.tc = &tc
	g.register_struct_decl_info('Aligned', 'Aligned', 'main', '', flat.Node{
		value: 'Aligned'
		typ:   'aligned=64'
	})
	aligned_type := types.Type(types.Struct{
		name: 'Aligned'
	})
	pointer_copy := g.heap_local_memdup_expr('p', aligned_type, 'Aligned', true)
	assert pointer_copy == '(Aligned*)v3_aligned_memdup(p, sizeof(Aligned), 64)'
	value_copy := g.heap_local_memdup_expr('x', aligned_type, 'Aligned', false)
	assert value_copy == '(Aligned*)v3_aligned_memdup(&x, sizeof(Aligned), 64)'
	plain_type := types.Type(types.Struct{
		name: 'Plain'
	})
	assert g.heap_local_memdup_expr('p', plain_type, 'Plain', true) == '(Plain*)memdup(p, sizeof(Plain))'
}

fn test_heap_local_address_expr_copies_pointer_local_slot() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	int_ptr := types.Type(types.Pointer{
		base_type: types.Type(types.int_)
	})
	int_ptr_ptr := types.Type(types.Pointer{
		base_type: int_ptr
	})
	tc.push_scope()
	tc.cur_scope.insert_with_owner('p', int_ptr)
	p_id := stmt_test_node(mut a, .ident, 'p', [])
	amp_p_id := stmt_test_prefix(mut a, .amp, p_id)
	heap_expr := g.heap_local_address_expr(amp_p_id, int_ptr_ptr) or {
		assert false, 'expected the address of a pointer local to escape through a heap copy'
		return
	}
	assert heap_expr == '(i64**)memdup(&p, sizeof(i64*))'
	tc.pop_scope()
}

fn test_heap_local_address_expr_copies_selector_from_stack_alias() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	int_type := types.Type(types.int_)
	struct_type := types.Type(types.Struct{
		name: 'S'
	})
	struct_ptr := types.Type(types.Pointer{
		base_type: struct_type
	})
	int_ptr := types.Type(types.Pointer{
		base_type: int_type
	})
	tc.structs['S'] = [types.StructField{
		name: 'x'
		typ:  int_type
	}]
	tc.push_scope()
	tc.cur_scope.insert_with_owner('s', struct_type)
	p_owner := tc.cur_scope.insert_with_owner('p', struct_ptr)
	g.declare_local_pointer_alias_source(p_owner, 's')
	p_id := stmt_test_node(mut a, .ident, 'p', [])
	selector_id := stmt_test_node(mut a, .selector, 'x', [p_id])
	amp_selector_id := stmt_test_prefix(mut a, .amp, selector_id)
	heap_expr := g.heap_local_address_expr(amp_selector_id, int_ptr) or {
		assert false, 'expected a selected field in stack-aliased storage to escape through a heap copy'
		return
	}
	assert heap_expr == '(i64*)memdup(&p->x, sizeof(i64))'
	g.declare_local_pointer_alias_source_kind(p_owner, 'arg', true)
	mut_param_expr := g.heap_local_address_expr(amp_selector_id, int_ptr) or {
		assert false, 'expected a selected field backed by a mut parameter to keep its address'
		return
	}
	assert mut_param_expr == '&p->x'
	g.declare_local_pointer_alias_source(p_owner, '')
	external_expr := g.heap_local_address_expr(amp_selector_id, int_ptr) or {
		assert false, 'expected a selected field in non-stack storage to keep its address'
		return
	}
	assert external_expr == '&p->x'
	tc.pop_scope()
}
