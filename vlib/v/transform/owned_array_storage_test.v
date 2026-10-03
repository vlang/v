module transform

import os
import v.flat
import v.types

fn test_owned_array_storage_void_wrappers_preserve_the_source() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	assert t.optional_base_type('!') == 'void'
	assert t.optional_base_type('?') == 'void'
	assert t.optional_base_type('![]int') == '[]int'
	assert t.optional_base_type('?[]int') == '[]int'
	for wrapper in ['!', '?', '!void', '?void'] {
		t.set_var_type('source', wrapper)
		source := t.make_ident('source')
		nodes_before := a.nodes.len
		for clone_owned_value in [false, true] {
			// Bare markers must unwrap to void so the storage scan terminates.
			assert t.clone_owned_array_storage_value(source, wrapper, clone_owned_value) == source
			assert a.nodes.len == nodes_before
			assert t.pending_stmts.len == 0
		}
	}
}

fn test_owned_array_storage_with_ownership_checker_api() {
	$if ownership ? {
		return
	}
	// Bootstrap the ownership APIs with the regular checker. Compiling the compiler
	// modules as an ownership program would also instrument their internal Type values.
	dir := os.join_path(os.vtmp_dir(), 'owned_array_storage_api_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	binary := os.join_path(dir, 'owned_array_storage_api')
	compiler_source := os.join_path(@VEXEROOT, 'cmd', 'v')
	build := os.exec([@VEXE, '-new-compiler', '-d', 'ownership', '-building-v', '-o', binary,
		'-file-list', os.real_path(@FILE), compiler_source])
	assert build.exit_code == 0, build.output
	run := os.exec([binary])
	assert run.exit_code == 0, run.output
}

fn configure_owned_array_storage_checker(mut tc types.TypeChecker) {
	tc.collect(tc.a)
	tc.structs['Res'] = [types.StructField{
		name: 'text'
		typ:  types.Type(types.string_)
	}]
	tc.interface_names['IError'] = true
	tc.type_aliases['ArrayResult'] = '![]Res'
	tc.type_aliases['ArrayResultAlias'] = 'ArrayResult'
	tc.type_aliases['ArrayOption'] = '?[]Res'
	assert tc.ownership_type_requires_destruction(tc.parse_type('Res'))
}

fn test_owned_array_result_storage_clones_only_the_active_owner() {
	$if !ownership ? {
		return
	}
	for wrapper in ['![]Res', 'ArrayResultAlias'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		configure_owned_array_storage_checker(mut tc)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		t.set_var_type('source', wrapper)
		source := t.make_ident('source')
		owned := t.clone_owned_array_value_for_capture(source, wrapper)
		assert t.is_owned_array_storage_value(owned)
		assert t.pending_stmts.len == 2
		check_owned_array_result_branch(&a, t.pending_stmts.last())
		// Reusing the acquired value must not create a second error or payload owner.
		assert t.clone_owned_array_value_for_capture(owned, wrapper) == owned
		assert t.pending_stmts.len == 2
	}
}

fn test_owned_array_result_sum_storage_keeps_error_acquisition_inside_its_variant() {
	$if !ownership ? {
		return
	}
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	configure_owned_array_storage_checker(mut tc)
	tc.sum_types['Storage'] = ['ArrayResultAlias', 'int']
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.sum_types['Storage'] = ['ArrayResultAlias', 'int']
	t.set_var_type('source', 'Storage')
	owned := t.clone_owned_array_value_for_capture(t.make_ident('source'), 'Storage')
	assert t.is_owned_array_storage_value(owned)
	mut result_branches := []flat.NodeId{}
	mut error_clones := 0
	for statement in t.pending_stmts {
		result_branches << owned_array_storage_ok_branches(&a, statement)
		statement_error_clones := owned_array_storage_call_count(&a, statement, '__v3_clone_owned_ierror')
		error_clones += statement_error_clones
		if statement_error_clones > 0 {
			variant_branch := a.nodes[int(statement)]
			assert variant_branch.kind == .if_expr
			assert owned_array_storage_selector_count(&a, a.child(&variant_branch, 0), sum_type_tag_selector_field) > 0
			assert owned_array_storage_call_count(&a, a.child(&variant_branch, 2), '__v3_clone_owned_ierror') == 0
		}
		// The shallow source still owns its boxes and must survive retaining the copy.
		assert owned_array_storage_call_count(&a, statement, 'drop_owned') == 0
	}
	assert result_branches.len == 1
	assert error_clones == 1
	check_owned_array_result_branch(&a, result_branches[0])
}

fn test_owned_array_optional_and_borrowed_result_storage_do_not_clone_absent_errors() {
	$if !ownership ? {
		return
	}
	for wrapper in ['?[]Res', 'ArrayOption', 'ArrayResultAlias'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		configure_owned_array_storage_checker(mut tc)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		t.set_var_type('source', wrapper)
		if wrapper == 'ArrayResultAlias' {
			t.clone_owned_array_view_for_storage(t.make_ident('source'), wrapper)
		} else {
			t.clone_owned_array_value_for_capture(t.make_ident('source'), wrapper)
		}
		assert t.pending_stmts.len == 2
		branch := a.nodes[int(t.pending_stmts.last())]
		assert branch.kind == .if_expr
		assert a.child_node(&branch, 0).value == 'ok'
		assert a.child_node(&branch, 2).kind == .empty
		for statement in t.pending_stmts {
			assert owned_array_storage_call_count(&a, statement, '__v3_clone_owned_ierror') == 0
			assert owned_array_storage_selector_count(&a, statement, 'err') == 0
		}
	}
}

fn test_owned_array_storage_preserves_explicit_array_references() {
	$if !ownership ? {
		return
	}
	for wrapper in ['&[]Res', '&[]u8'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		configure_owned_array_storage_checker(mut tc)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		t.set_var_type('source', wrapper)
		source := t.make_ident('source')
		assert t.clone_owned_array_view_for_storage(source, wrapper) == source
		assert t.clone_owned_array_value_for_capture(source, wrapper) == source
		assert t.pending_stmts.len == 0
	}
}

fn test_owned_array_reference_wrappers_keep_data_borrowed() {
	$if !ownership ? {
		return
	}
	for wrapper in ['?&[]Res', '!&[]Res'] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		configure_owned_array_storage_checker(mut tc)
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		t.set_var_type('source', wrapper)
		t.clone_owned_array_view_for_storage(t.make_ident('source'), wrapper)
		assert t.pending_stmts.len == 2
		branch := t.pending_stmts.last()
		assert owned_array_storage_call_count(&a, branch, default_clone_helper_name('[]Res')) == 0
		assert owned_array_storage_call_count(&a, branch, 'v3_heap_array') == 0
		assert owned_array_storage_call_count(&a, branch, '__v3_clone_owned_ierror') == 0
	}
}

fn test_owned_array_synthetic_pointer_acquisition_still_copies() {
	$if !ownership ? {
		return
	}
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	configure_owned_array_storage_checker(mut tc)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.set_var_type('source', '&[]Res')
	source := t.make_ident('source')
	assert t.clone_owned_array_storage_value(source, '&[]Res', true) != source
	assert t.pending_stmts.len == 2
	branch := t.pending_stmts.last()
	assert owned_array_storage_call_count(&a, branch, default_clone_helper_name('[]Res')) == 1
	assert owned_array_storage_call_count(&a, branch, 'v3_heap_array') == 1
}

fn check_owned_array_result_branch(a &flat.FlatAst, branch_id flat.NodeId) {
	branch := a.nodes[int(branch_id)]
	assert branch.kind == .if_expr
	condition := a.child_node(&branch, 0)
	assert condition.kind == .selector && condition.value == 'ok'
	success := a.child(&branch, 1)
	failure := a.child(&branch, 2)
	assert owned_array_storage_call_count(a, success, '__v3_clone_owned_ierror') == 0
	assert owned_array_storage_selector_count(a, success, 'err') == 0
	assert owned_array_storage_call_count(a, success, default_clone_helper_name('[]Res')) == 1
	assert owned_array_storage_call_count(a, failure, '__v3_clone_owned_ierror') == 1
	assert owned_array_storage_selector_count(a, failure, 'err') == 1
	assert owned_array_storage_selector_count(a, failure, 'value') == 0
	assert owned_array_storage_call_count(a, failure, default_clone_helper_name('[]Res')) == 0
	assert owned_array_storage_call_count(a, failure, 'drop_owned') == 0
	failure_block := a.nodes[int(failure)]
	assert failure_block.kind == .block && failure_block.children_count == 1
	assert failure_block.value == skip_scope_drops_block_value
	assignment := a.child_node(&failure_block, 0)
	assert assignment.kind == .assign && assignment.skip_ownership_drops()
	failed := a.child_node(assignment, 1)
	assert failed.kind == .struct_init && failed.value == '![]Res'
	assert failed.children_count == 2
	ok_field := a.child_node(failed, 0)
	assert ok_field.value == 'ok' && a.child_node(ok_field, 0).value == 'false'
	err_field := a.child_node(failed, 1)
	assert err_field.value == 'err'
	err_call := a.child_node(err_field, 0)
	assert a.child_node(err_call, 0).value == '__v3_clone_owned_ierror'
	err_read := a.child_node(err_call, 1)
	assert err_read.kind == .selector && err_read.value == 'err'
	// Replacement must clone the same bound error without consuming its borrowed owner.
	assert a.child_node(err_read, 0).value == a.child_node(assignment, 0).value
}

fn owned_array_storage_call_count(a &flat.FlatAst, id flat.NodeId, name string) int {
	node := a.nodes[int(id)]
	mut count := if node.kind == .call && node.children_count > 0
		&& a.child_node(&node, 0).value == name {
		1
	} else {
		0
	}
	for i in 0 .. int(node.children_count) {
		count += owned_array_storage_call_count(a, a.child(&node, i), name)
	}
	return count
}

fn owned_array_storage_selector_count(a &flat.FlatAst, id flat.NodeId, field string) int {
	node := a.nodes[int(id)]
	mut count := if node.kind == .selector && node.value == field { 1 } else { 0 }
	for i in 0 .. int(node.children_count) {
		count += owned_array_storage_selector_count(a, a.child(&node, i), field)
	}
	return count
}

fn owned_array_storage_ok_branches(a &flat.FlatAst, id flat.NodeId) []flat.NodeId {
	node := a.nodes[int(id)]
	mut branches := []flat.NodeId{}
	if node.kind == .if_expr && node.children_count == 3 {
		condition := a.child_node(&node, 0)
		if condition.kind == .selector && condition.value == 'ok' {
			branches << id
		}
	}
	for i in 0 .. int(node.children_count) {
		branches << owned_array_storage_ok_branches(a, a.child(&node, i))
	}
	return branches
}

fn test_owned_sum_reassignment_keeps_declared_storage_under_smartcast() {
	$if !ownership ? {
		return
	}
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.collect(tc.a)
	tc.structs['First'] = [types.StructField{
		name: 'text'
		typ:  types.Type(types.string_)
	}]
	tc.structs['Second'] = tc.structs['First']
	tc.sum_types['Value'] = ['First', 'Second']
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.sum_types['Value'] = ['First', 'Second']
	t.set_var_type('value', 'Value')
	t.set_var_type('replacement', 'Value')
	t.push_smartcast('value', 'First', 'Value')
	assignment := t.make_assign(t.make_ident('value'), t.make_ident('replacement'))
	statements := t.transform_stmt(assignment)
	assert statements.len > 0
	mut saw_replacement := false
	for statement in statements {
		node := a.nodes[int(statement)]
		if node.kind == .decl_assign && node.children_count == 2 {
			binding := a.child_node(&node, 0)
			if binding.value.starts_with('__drop_assign') {
				assert node.typ == 'Value', node.typ
				saw_replacement = true
			}
		}
	}
	assert saw_replacement
}

fn test_blank_assignment_has_no_previous_owner_to_drop() {
	$if !ownership ? {
		return
	}
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	configure_owned_array_storage_checker(mut tc)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	// Earlier discard expressions can leave an inferred type for this spelling.
	t.set_var_type('_', 'Res')
	t.set_var_type('items', '[]int')
	assignment := t.make_assign(t.make_ident('_'), t.make_ident('items'))
	statements := t.transform_stmt(assignment)
	assert statements.len == 1
	assert a.nodes[int(statements[0])].kind == .assign
	assert owned_array_storage_call_count(&a, statements[0], 'drop_owned') == 0
}
