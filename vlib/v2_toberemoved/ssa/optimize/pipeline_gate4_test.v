// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module optimize

import os
import v2.ssa

fn gate4_has_error(errors []VerifyError, op string, needle string) bool {
	for err in errors {
		if err.msg.contains(op) && err.msg.contains(needle) {
			return true
		}
	}
	return false
}

fn gate4_corrupt_uses_list_module() &ssa.Module {
	mut m := ssa.Module.new('gate4_corrupt_uses_list')
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function('main', i64_t, [])
	entry := m.add_block(func_id, 'entry')
	lhs := m.get_or_add_const(i64_t, '1')
	rhs := m.get_or_add_const(i64_t, '2')
	sum := m.add_instr(.add, entry, i64_t, [lhs, rhs])
	m.add_instr(.ret, entry, 0, [sum])
	mut lhs_val := m.values[lhs]
	lhs_val.uses.clear()
	m.values[lhs] = lhs_val
	return m
}

fn test_verify_rejects_call_sret_without_target() {
	mut m := ssa.Module.new('gate4_verify_call_sret')
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function('caller', i64_t, [])
	entry := m.add_block(func_id, 'entry')
	m.add_instr(.call_sret, entry, i64_t, [])
	zero := m.get_or_add_const(i64_t, '0')
	m.add_instr(.ret, entry, 0, [zero])

	errors := verify(m)
	assert gate4_has_error(errors, 'call_sret', 'no operands'), 'expected call_sret without target to fail verification'
}

fn test_verify_rejects_concurrency_calls_without_target() {
	for op in [ssa.OpCode.go_call, .spawn_call] {
		mut m := ssa.Module.new('gate4_verify_concurrency_call')
		func_id := m.new_function('caller', 0, [])
		entry := m.add_block(func_id, 'entry')
		m.add_instr(op, entry, 0, [])
		m.add_instr(.ret, entry, 0, [])

		errors := verify(m)
		assert gate4_has_error(errors, op.str(), 'no operands'), 'expected ${op} without target to fail verification'
	}
}

fn test_dce_preserves_global_initializer_store() {
	mut m := ssa.Module.new('gate4_dce_global_store')
	i64_t := m.type_store.get_int(64)
	global_id := m.add_global('module__state', i64_t, false)
	value := m.get_or_add_const(i64_t, '7')
	func_id := m.new_function('_vinit', 0, [])
	entry := m.add_block(func_id, 'entry')
	store_id := m.add_instr(.store, entry, 0, [value, global_id])
	m.add_instr(.ret, entry, 0, [])

	changed := dead_code_elimination(mut m)

	assert !changed
	assert store_id in m.blocks[entry].instrs
	assert m.values[global_id].uses.len == 1
	assert m.values[global_id].uses[0] == store_id
}

fn test_dce_preserves_imported_const_initializer_store() {
	mut m := ssa.Module.new('gate4_dce_imported_const_init')
	i64_t := m.type_store.get_int(64)
	global_id := m.add_global('dep__runtime_value', i64_t, false)
	value := m.get_or_add_const(i64_t, '11')
	func_id := m.new_function('dep____v_init_consts_dep', 0, [])
	entry := m.add_block(func_id, 'entry')
	store_id := m.add_instr(.store, entry, 0, [value, global_id])
	m.add_instr(.ret, entry, 0, [])

	changed := dead_code_elimination(mut m)

	assert !changed
	assert store_id in m.blocks[entry].instrs
	assert m.values[global_id].uses.len == 1
	assert m.values[global_id].uses[0] == store_id
}

fn test_optimize_can_verify_each_pipeline_checkpoint() {
	mut m := ssa.Module.new('gate4_verify_each_pipeline_checkpoint')
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function('main', i64_t, [])
	entry := m.add_block(func_id, 'entry')
	zero := m.get_or_add_const(i64_t, '0')
	ret_id := m.add_instr(.ret, entry, 0, [zero])

	optimize_with_options(mut m, OptimizeOptions{
		verify_each_pass: true
	})

	assert ret_id in m.blocks[entry].instrs
	assert verify(m).len == 0
}

fn test_optimize_can_verify_strict_each_pipeline_checkpoint() {
	mut m := ssa.Module.new('gate4_strict_verify_each_pipeline_checkpoint')
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function('main', i64_t, [])
	entry := m.add_block(func_id, 'entry')
	zero := m.get_or_add_const(i64_t, '0')
	ret_id := m.add_instr(.ret, entry, 0, [zero])

	optimize_with_options(mut m, OptimizeOptions{
		strict_verify: true
	})

	assert ret_id in m.blocks[entry].instrs
	assert verify(m).len == 0
}

fn test_optimize_runs_dead_code_elimination_pass() {
	mut m := ssa.Module.new('gate4_optimize_runs_dce')
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function('main', i64_t, [])
	entry := m.add_block(func_id, 'entry')
	lhs := m.get_or_add_const(i64_t, '20')
	rhs := m.get_or_add_const(i64_t, '22')
	dead_add := m.add_instr(.add, entry, i64_t, [lhs, rhs])
	zero := m.get_or_add_const(i64_t, '0')
	ret_id := m.add_instr(.ret, entry, 0, [zero])

	optimize_with_options(mut m, OptimizeOptions{
		verify_each_pass: true
	})

	assert dead_add !in m.blocks[entry].instrs
	assert ret_id in m.blocks[entry].instrs
}

fn test_public_optimize_uses_v2_verify_pipeline_checkpoints() {
	old_verify := os.getenv_opt('V2_VERIFY')
	os.setenv('V2_VERIFY', '1', true)
	defer {
		if old := old_verify {
			os.setenv('V2_VERIFY', old, true)
		} else {
			os.unsetenv('V2_VERIFY')
		}
	}

	mut m := ssa.Module.new('gate4_public_optimize_v2_verify')
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function('main', i64_t, [])
	entry := m.add_block(func_id, 'entry')
	lhs := m.get_or_add_const(i64_t, '100')
	rhs := m.get_or_add_const(i64_t, '23')
	dead_add := m.add_instr(.add, entry, i64_t, [lhs, rhs])
	zero := m.get_or_add_const(i64_t, '0')
	ret_id := m.add_instr(.ret, entry, 0, [zero])

	optimize_with_options(mut m, OptimizeOptions{})

	assert dead_add !in m.blocks[entry].instrs
	assert ret_id in m.blocks[entry].instrs
	assert verify(m).len == 0
}

fn test_public_optimize_uses_v2_verify_strict_pipeline_checkpoints() {
	old_verify := os.getenv_opt('V2_VERIFY')
	old_strict := os.getenv_opt('V2_VERIFY_STRICT')
	os.unsetenv('V2_VERIFY')
	os.setenv('V2_VERIFY_STRICT', '1', true)
	defer {
		if old := old_verify {
			os.setenv('V2_VERIFY', old, true)
		} else {
			os.unsetenv('V2_VERIFY')
		}
		if old := old_strict {
			os.setenv('V2_VERIFY_STRICT', old, true)
		} else {
			os.unsetenv('V2_VERIFY_STRICT')
		}
	}

	mut m := ssa.Module.new('gate4_public_optimize_v2_verify_strict')
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function('main', i64_t, [])
	entry := m.add_block(func_id, 'entry')
	zero := m.get_or_add_const(i64_t, '0')
	ret_id := m.add_instr(.ret, entry, 0, [zero])

	optimize(mut m)

	assert ret_id in m.blocks[entry].instrs
	assert verify(m).len == 0
}

fn test_optimize_rebuilds_uses_before_mem2reg_promotability() {
	mut m := ssa.Module.new('gate4_mem2reg_rebuilds_escaped_alloca_uses')
	i8_t := m.type_store.get_int(8)
	ptr_i8_t := m.type_store.get_ptr(i8_t)
	func_id := m.new_function('main', 0, [])
	entry := m.add_block(func_id, 'entry')
	alloca_id := m.add_instr(.alloca, entry, ptr_i8_t, [])
	ten := m.get_or_add_const(i8_t, '10')
	m.add_instr(.store, entry, 0, [ten, alloca_id])
	callee := m.add_value_node(.unknown, 0, 'sink', 0)
	m.add_instr(.call, entry, 0, [callee, alloca_id])
	m.add_instr(.ret, entry, 0, [])

	mut alloca_val := m.values[alloca_id]
	alloca_val.uses = []
	m.values[alloca_id] = alloca_val

	optimize_with_options(mut m, OptimizeOptions{})

	alloca_instr := m.instrs[m.values[alloca_id].index]
	assert alloca_instr.op == .alloca
}

fn test_verify_and_panic_allows_c_extern_prototype_without_blocks() {
	mut m := ssa.Module.new('gate4_c_extern_prototype')
	func_id := m.new_function('C_puts', 0, [])
	m.func_set_c_extern(func_id, true)

	verify_and_panic(m, 'c extern prototype')
}

fn test_verify_and_panic_allows_marked_prototype_without_blocks() {
	mut m := ssa.Module.new('gate4_marked_prototype')
	func_id := m.new_function('registered_only', 0, [])
	m.func_set_prototype(func_id, true)

	verify_and_panic(m, 'marked prototype')
}

fn test_strict_verify_and_panic_allows_valid_declarations_without_blocks() {
	mut m := ssa.Module.new('gate4_strict_valid_declarations')
	c_func_id := m.new_function('C_puts', 0, [])
	m.func_set_c_extern(c_func_id, true)
	prototype_func_id := m.new_function('registered_only', 0, [])
	m.func_set_prototype(prototype_func_id, true)

	verify_and_panic_with_options(m, 'strict valid declarations', VerifyPanicOptions{
		allow_noncritical: false
	})
}

fn test_verify_and_panic_allows_uses_list_warning_by_default() {
	mut m := gate4_corrupt_uses_list_module()

	errors := verify(m)
	assert gate4_has_error(errors, 'uses list', 'not in uses list'), 'expected uses list warning'

	verify_and_panic(m, 'uses list default warning')
}

fn test_strict_verify_and_panic_rejects_uses_list_warning() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_gate4_strict_verify_panic_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	sample_path := os.join_path(tmp_dir, 'strict_verify_panic_sample.v')
	os.write_file(sample_path, 'module main

import v2.ssa
import v2.ssa.optimize

fn main() {
	mut m := ssa.Module.new("gate4_strict_uses_list")
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function("main", i64_t, [])
	entry := m.add_block(func_id, "entry")
	lhs := m.get_or_add_const(i64_t, "1")
	rhs := m.get_or_add_const(i64_t, "2")
	sum := m.add_instr(.add, entry, i64_t, [lhs, rhs])
	m.add_instr(.ret, entry, 0, [sum])
	mut lhs_val := m.values[lhs]
	lhs_val.uses.clear()
	m.values[lhs] = lhs_val
	optimize.verify_and_panic_with_options(m, "strict uses list", optimize.VerifyPanicOptions{
		allow_noncritical: false
	})
}
') or {
		panic(err)
	}

	res := os.execute('${os.quoted_path(@VEXE)} run ${os.quoted_path(sample_path)}')

	assert res.exit_code != 0
	assert res.output.contains('uses list'), res.output
}

fn test_strict_optimize_rejects_uses_list_warning() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_gate4_strict_optimize_panic_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	sample_path := os.join_path(tmp_dir, 'strict_optimize_panic_sample.v')
	os.write_file(sample_path, 'module main

import v2.ssa
import v2.ssa.optimize

fn main() {
	mut m := ssa.Module.new("gate4_strict_optimize_uses_list")
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function("main", i64_t, [])
	entry := m.add_block(func_id, "entry")
	lhs := m.get_or_add_const(i64_t, "1")
	rhs := m.get_or_add_const(i64_t, "2")
	sum := m.add_instr(.add, entry, i64_t, [lhs, rhs])
	m.add_instr(.ret, entry, 0, [sum])
	mut lhs_val := m.values[lhs]
	lhs_val.uses.clear()
	m.values[lhs] = lhs_val
	optimize.optimize_with_options(mut m, optimize.OptimizeOptions{
		strict_verify: true
	})
}
') or {
		panic(err)
	}

	res := os.execute('${os.quoted_path(@VEXE)} run ${os.quoted_path(sample_path)}')

	assert res.exit_code != 0
	assert res.output.contains('uses list'), res.output
	assert res.output.contains('input'), res.output
}

fn test_verify_and_panic_rejects_v_function_without_blocks() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_gate4_verify_panic_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	sample_path := os.join_path(tmp_dir, 'verify_panic_sample.v')
	os.write_file(sample_path, 'module main

import v2.ssa
import v2.ssa.optimize

fn main() {
	mut m := ssa.Module.new("gate4_v_prototype")
	m.new_function("v_decl_without_body", 0, [])
	optimize.verify_and_panic(m, "v prototype negative")
}
') or {
		panic(err)
	}

	res := os.execute('${os.quoted_path(@VEXE)} run ${os.quoted_path(sample_path)}')

	assert res.exit_code != 0
	assert res.output.contains('function has no blocks'), res.output
}

fn test_dce_preserves_call_sret_side_effect() {
	mut m := ssa.Module.new('gate4_dce_call_sret')
	i8_t := m.type_store.get_int(8)
	i64_t := m.type_store.get_int(64)
	ptr_t := m.type_store.get_ptr(i8_t)
	result_t := m.type_store.register(ssa.Type{
		kind:   .struct_t
		fields: [i64_t, i64_t]
	})
	callee := m.add_value_node(.func_ref, ptr_t, 'make_pair', 0)
	arg := m.get_or_add_const(i64_t, '3')
	func_id := m.new_function('caller', 0, [])
	entry := m.add_block(func_id, 'entry')
	call_id := m.add_instr(.call_sret, entry, result_t, [callee, arg])
	m.add_instr(.ret, entry, 0, [])

	changed := dead_code_elimination(mut m)

	assert !changed
	assert call_id in m.blocks[entry].instrs
	assert m.values[callee].uses.len == 1
	assert m.values[callee].uses[0] == call_id
}

fn test_dce_preserves_concurrency_call_side_effects() {
	mut m := ssa.Module.new('gate4_dce_concurrency_calls')
	i8_t := m.type_store.get_int(8)
	ptr_t := m.type_store.get_ptr(i8_t)
	go_callee := m.add_value_node(.func_ref, ptr_t, 'launch_go', 0)
	spawn_callee := m.add_value_node(.func_ref, ptr_t, 'launch_spawn', 0)
	func_id := m.new_function('caller', 0, [])
	entry := m.add_block(func_id, 'entry')
	go_id := m.add_instr(.go_call, entry, 0, [go_callee])
	spawn_id := m.add_instr(.spawn_call, entry, 0, [spawn_callee])
	m.add_instr(.ret, entry, 0, [])

	changed := dead_code_elimination(mut m)

	assert !changed
	assert go_id in m.blocks[entry].instrs
	assert spawn_id in m.blocks[entry].instrs
	assert m.values[go_callee].uses[0] == go_id
	assert m.values[spawn_callee].uses[0] == spawn_id
}

fn test_dce_preserves_cmpxchg_side_effect() {
	mut m := ssa.Module.new('gate4_dce_cmpxchg')
	i64_t := m.type_store.get_int(64)
	global_id := m.add_global('module__atomic_state', i64_t, false)
	expected := m.get_or_add_const(i64_t, '0')
	desired := m.get_or_add_const(i64_t, '1')
	func_id := m.new_function('update', 0, [])
	entry := m.add_block(func_id, 'entry')
	cmpxchg_id := m.add_instr(.cmpxchg, entry, i64_t, [global_id, expected, desired])
	m.add_instr(.ret, entry, 0, [])

	changed := dead_code_elimination(mut m)

	assert !changed
	assert cmpxchg_id in m.blocks[entry].instrs
	assert m.values[global_id].uses[0] == cmpxchg_id
}
