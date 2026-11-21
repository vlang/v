module optimize

import os
import v.ssa

const production_options = OptimizeOptions{
	mem2reg:        true
	eliminate_phis: true
}

fn pipeline_has_error(errors []VerifyError, op string, needle string) bool {
	for err in errors {
		if err.msg.contains(op) && err.msg.contains(needle) {
			return true
		}
	}
	return false
}

// pipeline_corrupt_uses_list_module returns a module whose `sum` value lost
// its only user from its use list.
fn pipeline_corrupt_uses_list_module() &ssa.Module {
	mut m := ssa.Module.new()
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function('main', i64_t)
	entry := m.add_block(func_id, 'entry')
	lhs := m.get_or_add_const(i64_t, '1')
	rhs := m.get_or_add_const(i64_t, '2')
	sum := m.add_instr(.add, entry, i64_t, [lhs, rhs])
	double := m.add_instr(.add, entry, i64_t, [sum, sum])
	m.add_instr(.ret, entry, 0, [double])
	mut sum_val := m.values[sum]
	sum_val.uses.clear()
	m.values[sum] = sum_val
	return m
}

fn new_main_returning_zero() (&ssa.Module, ssa.BlockID, ssa.ValueID) {
	mut m := ssa.Module.new()
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function('main', i64_t)
	entry := m.add_block(func_id, 'entry')
	zero := m.get_or_add_const(i64_t, '0')
	ret_id := m.add_instr(.ret, entry, 0, [zero])
	return m, entry, ret_id
}

fn with_env(name string, value ?string, cb fn ()) {
	old := os.getenv_opt(name)
	if v := value {
		os.setenv(name, v, true)
	} else {
		os.unsetenv(name)
	}
	defer {
		if o := old {
			os.setenv(name, o, true)
		} else {
			os.unsetenv(name)
		}
	}
	cb()
}

fn test_verify_rejects_call_sret_without_target() {
	mut m := ssa.Module.new()
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function('caller', i64_t)
	entry := m.add_block(func_id, 'entry')
	m.add_instr(.call_sret, entry, i64_t, [])
	zero := m.get_or_add_const(i64_t, '0')
	m.add_instr(.ret, entry, 0, [zero])

	assert pipeline_has_error(verify(m), 'call_sret', 'no operands'), 'expected call_sret without target to fail verification'
}

fn test_verify_rejects_concurrency_calls_without_target() {
	for op in [ssa.OpCode.go_call, .spawn_call] {
		mut m := ssa.Module.new()
		func_id := m.new_function('caller', 0)
		entry := m.add_block(func_id, 'entry')
		m.add_instr(op, entry, 0, [])
		m.add_instr(.ret, entry, 0, [])

		assert pipeline_has_error(verify(m), op.str(), 'no operands'), 'expected ${op} without target to fail verification'
	}
}

fn test_dce_preserves_global_initializer_store() {
	mut m := ssa.Module.new()
	i64_t := m.type_store.get_int(64)
	global_id := m.add_global('module__state', i64_t)
	value := m.get_or_add_const(i64_t, '7')
	func_id := m.new_function('_vinit', 0)
	entry := m.add_block(func_id, 'entry')
	store_id := m.add_instr(.store, entry, 0, [value, global_id])
	ret_id := m.add_instr(.ret, entry, 0, [])

	dead_code_elimination(mut m)

	assert m.blocks[entry].instrs == [store_id, ret_id]
}

fn test_dce_preserves_imported_const_initializer_store() {
	mut m := ssa.Module.new()
	i64_t := m.type_store.get_int(64)
	global_id := m.add_global('dep__runtime_value', i64_t)
	value := m.get_or_add_const(i64_t, '11')
	func_id := m.new_function('dep____v_init_consts_dep', 0)
	entry := m.add_block(func_id, 'entry')
	store_id := m.add_instr(.store, entry, 0, [value, global_id])
	ret_id := m.add_instr(.ret, entry, 0, [])

	dead_code_elimination(mut m)

	assert m.blocks[entry].instrs == [store_id, ret_id]
}

fn test_dce_removes_stores_to_unread_local() {
	mut m := ssa.Module.new()
	i64_t := m.type_store.get_int(64)
	ptr_t := m.type_store.get_ptr(i64_t)
	func_id := m.new_function('main', 0)
	entry := m.add_block(func_id, 'entry')
	slot := m.add_instr(.alloca, entry, ptr_t, [])
	value := m.get_or_add_const(i64_t, '5')
	m.add_instr(.store, entry, 0, [value, slot])
	ret_id := m.add_instr(.ret, entry, 0, [])

	dead_code_elimination(mut m)

	// The store is dead because nothing reads the slot; the slot then has no users.
	assert m.blocks[entry].instrs == [ret_id]
}

fn test_optimize_can_verify_each_pipeline_checkpoint() {
	mut m, entry, ret_id := new_main_returning_zero()

	optimize_with_options(mut m, OptimizeOptions{
		...production_options
		verify_each_pass: true
	})

	assert ret_id in m.blocks[entry].instrs
	assert verify(m).len == 0
}

fn test_optimize_can_verify_strict_each_pipeline_checkpoint() {
	mut m, entry, ret_id := new_main_returning_zero()

	optimize_with_options(mut m, OptimizeOptions{
		...production_options
		strict_verify: true
	})

	assert ret_id in m.blocks[entry].instrs
	assert verify(m).len == 0
}

fn test_optimize_runs_dead_code_elimination_pass() {
	mut m := ssa.Module.new()
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function('main', i64_t)
	entry := m.add_block(func_id, 'entry')
	lhs := m.get_or_add_const(i64_t, '20')
	rhs := m.get_or_add_const(i64_t, '22')
	dead_add := m.add_instr(.add, entry, i64_t, [lhs, rhs])
	zero := m.get_or_add_const(i64_t, '0')
	ret_id := m.add_instr(.ret, entry, 0, [zero])

	optimize_with_options(mut m, OptimizeOptions{
		...production_options
		verify_each_pass: true
	})

	assert dead_add !in m.blocks[entry].instrs
	assert ret_id in m.blocks[entry].instrs
}

fn test_public_optimize_uses_v3_verify_pipeline_checkpoints() {
	with_env('V3_VERIFY', '1', fn () {
		mut m := ssa.Module.new()
		i64_t := m.type_store.get_int(64)
		func_id := m.new_function('main', i64_t)
		entry := m.add_block(func_id, 'entry')
		lhs := m.get_or_add_const(i64_t, '100')
		rhs := m.get_or_add_const(i64_t, '23')
		dead_add := m.add_instr(.add, entry, i64_t, [lhs, rhs])
		zero := m.get_or_add_const(i64_t, '0')
		ret_id := m.add_instr(.ret, entry, 0, [zero])

		optimize(mut m)

		assert dead_add !in m.blocks[entry].instrs
		assert ret_id in m.blocks[entry].instrs
		assert verify(m).len == 0
	})
}

fn test_public_optimize_uses_v3_verify_strict_pipeline_checkpoints() {
	with_env('V3_VERIFY', none, fn () {
		with_env('V3_VERIFY_STRICT', '1', fn () {
			mut m, entry, ret_id := new_main_returning_zero()

			optimize(mut m)

			assert ret_id in m.blocks[entry].instrs
			assert verify(m).len == 0
		})
	})
}

fn test_optimize_rebuilds_uses_before_mem2reg_promotability() {
	mut m := ssa.Module.new()
	i8_t := m.type_store.get_int(8)
	ptr_i8_t := m.type_store.get_ptr(i8_t)
	func_id := m.new_function('main', 0)
	entry := m.add_block(func_id, 'entry')
	alloca_id := m.add_instr(.alloca, entry, ptr_i8_t, [])
	ten := m.get_or_add_const(i8_t, '10')
	m.add_instr(.store, entry, 0, [ten, alloca_id])
	callee := m.add_value(.func_ref, ptr_i8_t, 'sink', 0)
	m.add_instr(.call, entry, 0, [callee, alloca_id])
	m.add_instr(.ret, entry, 0, [])

	// Hide the escaping call from the use list: the optimizer must rebuild it
	// instead of promoting a slot whose address leaks.
	mut alloca_val := m.values[alloca_id]
	alloca_val.uses = []
	m.values[alloca_id] = alloca_val

	optimize_with_options(mut m, production_options)

	alloca_instr := m.instrs[m.values[alloca_id].index]
	assert alloca_instr.op == .alloca
	assert alloca_id in m.blocks[entry].instrs
}

fn test_optimize_repairs_corrupt_uses_list_before_input_checkpoint() {
	mut m := pipeline_corrupt_uses_list_module()
	assert pipeline_has_error(verify(m), 'uses list', 'not in uses list')

	optimize_with_options(mut m, OptimizeOptions{
		...production_options
		strict_verify: true
	})

	assert verify(m).len == 0
}

fn test_verify_and_panic_allows_c_extern_prototype_without_blocks() {
	mut m := ssa.Module.new()
	func_id := m.new_function('C_puts', 0)
	m.func_set_c_extern(func_id, true)

	verify_and_panic(m, 'c extern prototype')
}

fn test_verify_and_panic_allows_marked_prototype_without_blocks() {
	mut m := ssa.Module.new()
	func_id := m.new_function('registered_only', 0)
	m.func_set_prototype(func_id, true)

	verify_and_panic(m, 'marked prototype')
}

fn test_strict_verify_and_panic_allows_valid_declarations_without_blocks() {
	mut m := ssa.Module.new()
	c_func_id := m.new_function('C_puts', 0)
	m.func_set_c_extern(c_func_id, true)
	prototype_func_id := m.new_function('registered_only', 0)
	m.func_set_prototype(prototype_func_id, true)

	verify_and_panic_with_options(m, 'strict valid declarations', VerifyPanicOptions{
		allow_noncritical: false
	})
}

fn test_verify_and_panic_allows_uses_list_warning_by_default() {
	mut m := pipeline_corrupt_uses_list_module()

	assert pipeline_has_error(verify(m), 'uses list', 'not in uses list'), 'expected uses list warning'

	verify_and_panic(m, 'uses list default warning')
}

// Panics cannot be caught in-process, so the fatal paths run in one helper
// program, compiled once and started once per scenario.
const panic_sample = 'module main

import os
import v.ssa
import v.ssa.optimize

fn corrupt_uses_list_module() &ssa.Module {
	mut m := ssa.Module.new()
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function("main", i64_t)
	entry := m.add_block(func_id, "entry")
	lhs := m.get_or_add_const(i64_t, "1")
	rhs := m.get_or_add_const(i64_t, "2")
	sum := m.add_instr(.add, entry, i64_t, [lhs, rhs])
	double := m.add_instr(.add, entry, i64_t, [sum, sum])
	m.add_instr(.ret, entry, 0, [double])
	mut sum_val := m.values[sum]
	sum_val.uses.clear()
	m.values[sum] = sum_val
	return m
}

fn load_from_non_pointer_module() &ssa.Module {
	mut m := ssa.Module.new()
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function("main", i64_t)
	entry := m.add_block(func_id, "entry")
	value := m.get_or_add_const(i64_t, "1")
	loaded := m.add_instr(.load, entry, i64_t, [value])
	m.add_instr(.ret, entry, 0, [loaded])
	return m
}

fn main() {
	match os.args[1] {
		"strict_uses_list" {
			m := corrupt_uses_list_module()
			optimize.verify_and_panic_with_options(m, "strict uses list", optimize.VerifyPanicOptions{
				allow_noncritical: false
			})
		}
		"lenient_optimize" {
			mut m := load_from_non_pointer_module()
			optimize.optimize_with_options(mut m, optimize.OptimizeOptions{
				mem2reg: true
				eliminate_phis: true
				verify_each_pass: true
			})
		}
		"strict_optimize" {
			mut m := load_from_non_pointer_module()
			optimize.optimize_with_options(mut m, optimize.OptimizeOptions{
				mem2reg: true
				eliminate_phis: true
				strict_verify: true
			})
		}
		"v_fn_without_blocks" {
			mut m := ssa.Module.new()
			m.new_function("v_decl_without_body", 0)
			optimize.verify_and_panic(m, "v prototype negative")
		}
		else {
			panic("unknown scenario")
		}
	}
	println("ok")
}
'

fn test_fatal_verify_paths() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'ssa_optimize_pipeline_test_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	sample_path := os.join_path(tmp_dir, 'verify_panic_sample.v')
	mut exe_path := os.join_path(tmp_dir, 'verify_panic_sample')
	$if windows {
		exe_path += '.exe'
	}
	os.write_file(sample_path, panic_sample)!
	build := os.exec([@VEXE, '-o', exe_path, sample_path])
	assert build.exit_code == 0, build.output

	run := fn [exe_path] (scenario string) os.Result {
		return os.exec([exe_path, scenario])
	}

	// Strict verification escalates a use-list warning.
	strict_uses := run('strict_uses_list')
	assert strict_uses.exit_code != 0
	assert strict_uses.output.contains('uses list'), strict_uses.output

	// A non-critical finding passes lenient per-pass verification...
	lenient := run('lenient_optimize')
	assert lenient.exit_code == 0, lenient.output
	assert lenient.output.contains('ok'), lenient.output

	// ...but strict mode rejects it at the first (input) checkpoint.
	strict := run('strict_optimize')
	assert strict.exit_code != 0
	assert strict.output.contains('should be pointer type'), strict.output
	assert strict.output.contains('after input'), strict.output

	// A V function without blocks is not a declaration and always fails.
	no_blocks := run('v_fn_without_blocks')
	assert no_blocks.exit_code != 0
	assert no_blocks.output.contains('function has no blocks'), no_blocks.output
}

fn test_dce_preserves_call_sret_side_effect() {
	mut m := ssa.Module.new()
	i8_t := m.type_store.get_int(8)
	i64_t := m.type_store.get_int(64)
	ptr_t := m.type_store.get_ptr(i8_t)
	result_t := m.type_store.register(ssa.Type{
		kind:   .struct_t
		fields: [i64_t, i64_t]
	})
	callee := m.add_value(.func_ref, ptr_t, 'make_pair', 0)
	arg := m.get_or_add_const(i64_t, '3')
	func_id := m.new_function('caller', 0)
	entry := m.add_block(func_id, 'entry')
	call_id := m.add_instr(.call_sret, entry, result_t, [callee, arg])
	ret_id := m.add_instr(.ret, entry, 0, [])

	dead_code_elimination(mut m)

	assert m.blocks[entry].instrs == [call_id, ret_id]
}

fn test_dce_preserves_concurrency_call_side_effects() {
	mut m := ssa.Module.new()
	i8_t := m.type_store.get_int(8)
	ptr_t := m.type_store.get_ptr(i8_t)
	go_callee := m.add_value(.func_ref, ptr_t, 'launch_go', 0)
	spawn_callee := m.add_value(.func_ref, ptr_t, 'launch_spawn', 0)
	func_id := m.new_function('caller', 0)
	entry := m.add_block(func_id, 'entry')
	go_id := m.add_instr(.go_call, entry, 0, [go_callee])
	spawn_id := m.add_instr(.spawn_call, entry, 0, [spawn_callee])
	ret_id := m.add_instr(.ret, entry, 0, [])

	dead_code_elimination(mut m)

	assert m.blocks[entry].instrs == [go_id, spawn_id, ret_id]
}

fn test_dce_preserves_cmpxchg_side_effect() {
	mut m := ssa.Module.new()
	i64_t := m.type_store.get_int(64)
	global_id := m.add_global('module__atomic_state', i64_t)
	expected := m.get_or_add_const(i64_t, '0')
	desired := m.get_or_add_const(i64_t, '1')
	func_id := m.new_function('update', 0)
	entry := m.add_block(func_id, 'entry')
	cmpxchg_id := m.add_instr(.cmpxchg, entry, i64_t, [global_id, expected, desired])
	ret_id := m.add_instr(.ret, entry, 0, [])

	dead_code_elimination(mut m)

	assert m.blocks[entry].instrs == [cmpxchg_id, ret_id]
}
