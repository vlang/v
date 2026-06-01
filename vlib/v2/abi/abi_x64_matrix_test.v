// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module abi

import v2.mir
import v2.ssa

fn register_x64_matrix_struct_bytes(mut ssa_mod ssa.Module, size int) ssa.TypeID {
	i8_t := ssa_mod.type_store.get_int(8)
	mut fields := []ssa.TypeID{}
	for _ in 0 .. size {
		fields << i8_t
	}
	return register_x64_matrix_struct_fields(mut ssa_mod, fields)
}

fn register_x64_matrix_struct_fields(mut ssa_mod ssa.Module, fields []ssa.TypeID) ssa.TypeID {
	return ssa_mod.type_store.register(ssa.Type{
		kind:   .struct_t
		fields: fields
	})
}

fn assert_abi_value_class(class mir.AbiValueClass, mode mir.AbiPassMode, classes []mir.AbiEightbyteClass) {
	assert class.mode == mode
	assert class.classes == classes
}

fn assert_abi_layout_locs(layout mir.AbiValueLayout, kinds []mir.AbiLocationKind, indexes []int) {
	assert layout.locs.len == kinds.len
	assert indexes.len == kinds.len
	for i, kind in kinds {
		assert layout.locs[i].kind == kind
		assert layout.locs[i].index == indexes[i]
	}
}

fn assert_abi_layout(layout mir.AbiValueLayout, kinds []mir.AbiLocationKind, indexes []int, offsets []int, classes []mir.AbiEightbyteClass) {
	assert layout.locs.len == kinds.len
	assert indexes.len == kinds.len
	assert offsets.len == kinds.len
	assert classes.len == kinds.len
	for i, kind in kinds {
		assert layout.locs[i].kind == kind
		assert layout.locs[i].index == indexes[i]
		assert layout.locs[i].offset == offsets[i]
		assert layout.locs[i].class == classes[i]
	}
}

fn build_x64_matrix_direct_call_module(x64_abi X64Abi) (mir.Module, ssa.ValueID) {
	mut ssa_mod := ssa.Module.new('abi_test_x64_direct_call_matrix')
	i64_t := ssa_mod.type_store.get_int(64)
	sizes := [1, 2, 3, 4, 5, 6, 7, 8, 9, 16, 17, 32]
	mut struct_types := []ssa.TypeID{}
	for size in sizes {
		struct_types << register_x64_matrix_struct_bytes(mut ssa_mod, size)
	}

	callee_id := ssa_mod.new_function('callee', i64_t, [])
	for i, struct_t in struct_types {
		param := ssa_mod.add_value_node(.argument, struct_t, 'p${sizes[i]}', 0)
		ssa_mod.funcs[callee_id].params << param
	}
	caller_id := ssa_mod.new_function('caller', i64_t, [])
	mut args := []ssa.ValueID{}
	for i, struct_t in struct_types {
		arg := ssa_mod.add_value_node(.argument, struct_t, 'a${sizes[i]}', 0)
		ssa_mod.funcs[caller_id].params << arg
		args << arg
	}
	caller_entry := ssa_mod.add_block(caller_id, 'entry')
	fn_val := ssa_mod.add_value_node(.unknown, 0, 'callee', 0)
	mut operands := [fn_val]
	operands << args
	call_val := ssa_mod.add_instr(.call, caller_entry, i64_t, operands)
	zero := ssa_mod.get_or_add_const(i64_t, '0')
	ssa_mod.add_instr(.ret, caller_entry, 0, [zero])

	mut mir_mod := mir.lower_from_ssa(ssa_mod)
	lower_with_x64_abi(mut mir_mod, .x64, x64_abi)
	return mir_mod, call_val
}

fn build_x64_matrix_return_call_module(x64_abi X64Abi, size int) (mir.Module, ssa.ValueID) {
	mut ssa_mod := ssa.Module.new('abi_test_x64_return_call_matrix')
	i64_t := ssa_mod.type_store.get_int(64)
	struct_t := register_x64_matrix_struct_bytes(mut ssa_mod, size)

	callee_id := ssa_mod.new_function('callee', struct_t, [])
	callee_entry := ssa_mod.add_block(callee_id, 'entry')
	zero := ssa_mod.get_or_add_const(i64_t, '0')
	ssa_mod.add_instr(.ret, callee_entry, 0, [zero])

	caller_id := ssa_mod.new_function('caller', i64_t, [])
	caller_entry := ssa_mod.add_block(caller_id, 'entry')
	fn_val := ssa_mod.add_value_node(.unknown, 0, 'callee', 0)
	call_val := ssa_mod.add_instr(.call, caller_entry, struct_t, [fn_val])
	ssa_mod.add_instr(.ret, caller_entry, 0, [zero])

	mut mir_mod := mir.lower_from_ssa(ssa_mod)
	lower_with_x64_abi(mut mir_mod, .x64, x64_abi)
	return mir_mod, call_val
}

fn build_sysv_layout_call_module(prefix_count int, prefix_float bool, aggregate_kind string) (mir.Module, int, ssa.ValueID, int) {
	mut ssa_mod := ssa.Module.new('abi_test_x64_sysv_location_metadata')
	i64_t := ssa_mod.type_store.get_int(64)
	f64_t := ssa_mod.type_store.get_float(64)
	prefix_t := if prefix_float { f64_t } else { i64_t }
	aggregate_t := match aggregate_kind {
		'i64_i64' { register_x64_matrix_struct_fields(mut ssa_mod, [i64_t, i64_t]) }
		'f64_f64' { register_x64_matrix_struct_fields(mut ssa_mod, [f64_t, f64_t]) }
		'i64_f64' { register_x64_matrix_struct_fields(mut ssa_mod, [i64_t, f64_t]) }
		else { register_x64_matrix_struct_fields(mut ssa_mod, [i64_t]) }
	}

	callee_id := ssa_mod.new_function('callee', i64_t, [])
	mut param_types := []ssa.TypeID{len: prefix_count, init: prefix_t}
	param_types << aggregate_t
	for i, typ in param_types {
		param := ssa_mod.add_value_node(.argument, typ, 'p${i}', 0)
		ssa_mod.funcs[callee_id].params << param
	}
	caller_id := ssa_mod.new_function('caller', i64_t, [])
	mut args := []ssa.ValueID{}
	for i, typ in param_types {
		arg := ssa_mod.add_value_node(.argument, typ, 'a${i}', 0)
		ssa_mod.funcs[caller_id].params << arg
		args << arg
	}
	caller_entry := ssa_mod.add_block(caller_id, 'entry')
	fn_val := ssa_mod.add_value_node(.unknown, 0, 'callee', 0)
	mut operands := [fn_val]
	operands << args
	call_val := ssa_mod.add_instr(.call, caller_entry, i64_t, operands)
	zero := ssa_mod.get_or_add_const(i64_t, '0')
	ssa_mod.add_instr(.ret, caller_entry, 0, [zero])

	mut mir_mod := mir.lower_from_ssa(ssa_mod)
	lower_with_x64_abi(mut mir_mod, .x64, .sysv)
	return mir_mod, callee_id, call_val, prefix_count
}

fn build_sysv_scalar_boundary_module(mut ssa_mod ssa.Module, typ ssa.TypeID, count int) (mir.Module, int, ssa.ValueID) {
	i64_t := ssa_mod.type_store.get_int(64)
	callee_id := ssa_mod.new_function('callee', i64_t, [])
	for i := 0; i < count; i++ {
		param := ssa_mod.add_value_node(.argument, typ, 'p${i}', 0)
		ssa_mod.funcs[callee_id].params << param
	}
	caller_id := ssa_mod.new_function('caller', i64_t, [])
	mut args := []ssa.ValueID{}
	for i := 0; i < count; i++ {
		arg := ssa_mod.add_value_node(.argument, typ, 'a${i}', 0)
		ssa_mod.funcs[caller_id].params << arg
		args << arg
	}
	caller_entry := ssa_mod.add_block(caller_id, 'entry')
	fn_val := ssa_mod.add_value_node(.unknown, 0, 'callee', 0)
	mut operands := [fn_val]
	operands << args
	call_val := ssa_mod.add_instr(.call, caller_entry, i64_t, operands)
	zero := ssa_mod.get_or_add_const(i64_t, '0')
	ssa_mod.add_instr(.ret, caller_entry, 0, [zero])

	mut mir_mod := mir.lower_from_ssa(ssa_mod)
	lower_with_x64_abi(mut mir_mod, .x64, .sysv)
	return mir_mod, callee_id, call_val
}

fn test_x64_windows_struct_size_matrix_classification() {
	mut ssa_mod := ssa.Module.new('abi_test_x64_windows_size_matrix')
	i64_t := ssa_mod.type_store.get_int(64)
	sizes := [1, 2, 3, 4, 5, 6, 7, 8, 9, 16, 17, 32]
	direct_sizes := [1, 2, 4, 8]
	mut struct_types := []ssa.TypeID{}
	for size in sizes {
		struct_types << register_x64_matrix_struct_bytes(mut ssa_mod, size)
	}

	param_fn_id := ssa_mod.new_function('param_matrix', i64_t, [])
	for i, struct_t in struct_types {
		param := ssa_mod.add_value_node(.argument, struct_t, 's${sizes[i]}', 0)
		ssa_mod.funcs[param_fn_id].params << param
	}
	mut ret_fn_ids := []int{}
	for i, struct_t in struct_types {
		ret_fn_ids << ssa_mod.new_function('ret${sizes[i]}', struct_t, [])
	}

	mut mir_mod := mir.lower_from_ssa(ssa_mod)
	lower_with_x64_abi(mut mir_mod, .x64, .windows)

	for i, size in sizes {
		should_be_indirect := size !in direct_sizes
		assert (mir_mod.funcs[param_fn_id].abi_param_class[i] == .indirect) == should_be_indirect
		assert mir_mod.funcs[ret_fn_ids[i]].abi_ret_indirect == should_be_indirect
	}
}

fn test_x64_windows_callsite_return_size_matrix_classification() {
	sizes := [1, 2, 3, 4, 5, 6, 7, 8, 9, 16, 17, 32]
	direct_sizes := [1, 2, 4, 8]
	for size in sizes {
		mir_mod, call_val := build_x64_matrix_return_call_module(.windows, size)
		call_instr := mir_mod.instrs[mir_mod.values[call_val].index]
		should_be_indirect := size !in direct_sizes
		assert call_instr.abi_ret_indirect == should_be_indirect
		assert (call_instr.op == .call_sret) == should_be_indirect
	}
}

fn test_x64_windows_direct_callsite_struct_size_matrix_classification() {
	sizes := [1, 2, 3, 4, 5, 6, 7, 8, 9, 16, 17, 32]
	direct_sizes := [1, 2, 4, 8]
	mir_mod, call_val := build_x64_matrix_direct_call_module(.windows)
	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]

	assert call_instr.abi_arg_class.len == sizes.len
	for i, size in sizes {
		should_be_indirect := size !in direct_sizes
		assert (call_instr.abi_arg_class[i] == .indirect) == should_be_indirect
	}
}

fn test_non_sysv_legacy_thresholds_stay_unchanged_with_metadata_fields() {
	mut ssa_mod := ssa.Module.new('abi_test_non_sysv_legacy_metadata_guard')
	i64_t := ssa_mod.type_store.get_int(64)
	struct9_t := register_x64_matrix_struct_bytes(mut ssa_mod, 9)
	struct16_t := register_x64_matrix_struct_bytes(mut ssa_mod, 16)
	struct17_t := register_x64_matrix_struct_bytes(mut ssa_mod, 17)

	param_fn_id := ssa_mod.new_function('param_matrix', i64_t, [])
	for typ in [struct9_t, struct16_t, struct17_t] {
		param := ssa_mod.add_value_node(.argument, typ, 'p', 0)
		ssa_mod.funcs[param_fn_id].params << param
	}
	ret16_id := ssa_mod.new_function('ret16', struct16_t, [])
	ret17_id := ssa_mod.new_function('ret17', struct17_t, [])

	mut win_mod := mir.lower_from_ssa(ssa_mod)
	lower_with_x64_abi(mut win_mod, .x64, .windows)
	assert win_mod.funcs[param_fn_id].abi_param_class == [.indirect, .indirect, .indirect]
	assert win_mod.funcs[ret16_id].abi_ret_indirect == true
	assert win_mod.funcs[ret17_id].abi_ret_indirect == true
	for layout in win_mod.funcs[param_fn_id].abi_param_layouts {
		assert layout.locs.len == 0
	}

	mut arm_mod := mir.lower_from_ssa(ssa_mod)
	lower_with_x64_abi(mut arm_mod, .arm64, .sysv)
	assert arm_mod.funcs[param_fn_id].abi_param_class == [.in_reg, .in_reg, .indirect]
	assert arm_mod.funcs[ret16_id].abi_ret_indirect == false
	assert arm_mod.funcs[ret17_id].abi_ret_indirect == true
	for layout in arm_mod.funcs[param_fn_id].abi_param_layouts {
		assert layout.locs.len == 0
	}
}

fn test_x64_sysv_location_metadata_rolls_back_integer_aggregate_when_gprs_are_insufficient() {
	mir_mod, callee_id, call_val, aggregate_idx := build_sysv_layout_call_module(6, false,
		'i64_i64')
	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]

	assert_abi_layout_locs(mir_mod.funcs[callee_id].abi_param_layouts[aggregate_idx], [
		.stack,
		.stack,
	], [0, 1])
	assert_abi_layout_locs(call_instr.abi_arg_layouts[aggregate_idx], [.stack, .stack], [
		0,
		1,
	])
}

fn test_x64_sysv_location_metadata_uses_two_gprs_when_available_for_integer_aggregate() {
	mir_mod, callee_id, call_val, aggregate_idx := build_sysv_layout_call_module(4, false,
		'i64_i64')
	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]

	assert_abi_layout_locs(mir_mod.funcs[callee_id].abi_param_layouts[aggregate_idx], [
		.int_reg,
		.int_reg,
	], [4, 5])
	assert_abi_layout_locs(call_instr.abi_arg_layouts[aggregate_idx], [.int_reg, .int_reg], [
		4,
		5,
	])
}

fn test_x64_sysv_location_metadata_rolls_back_integer_aggregate_after_five_gprs() {
	mir_mod, callee_id, call_val, aggregate_idx := build_sysv_layout_call_module(5, false,
		'i64_i64')
	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]

	assert_abi_layout_locs(mir_mod.funcs[callee_id].abi_param_layouts[aggregate_idx], [
		.stack,
		.stack,
	], [0, 1])
	assert_abi_layout_locs(call_instr.abi_arg_layouts[aggregate_idx], [.stack, .stack], [
		0,
		1,
	])
}

fn test_x64_sysv_location_metadata_rolls_back_sse_aggregate_when_xmms_are_insufficient() {
	mir_mod, callee_id, call_val, aggregate_idx := build_sysv_layout_call_module(7, true, 'f64_f64')
	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]

	assert_abi_layout_locs(mir_mod.funcs[callee_id].abi_param_layouts[aggregate_idx], [
		.stack,
		.stack,
	], [0, 1])
	assert_abi_layout_locs(call_instr.abi_arg_layouts[aggregate_idx], [.stack, .stack], [
		0,
		1,
	])
}

fn test_x64_sysv_location_metadata_assigns_mixed_integer_sse_aggregate() {
	mir_mod, callee_id, call_val, aggregate_idx := build_sysv_layout_call_module(0, false,
		'i64_f64')
	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]

	assert_abi_layout_locs(mir_mod.funcs[callee_id].abi_param_layouts[aggregate_idx], [
		.int_reg,
		.sse_reg,
	], [0, 0])
	assert_abi_layout_locs(call_instr.abi_arg_layouts[aggregate_idx], [.int_reg, .sse_reg], [
		0,
		0,
	])
}

fn test_x64_sysv_location_metadata_reserves_rdi_for_hidden_sret_pointer() {
	mut ssa_mod := ssa.Module.new('abi_test_x64_sysv_sret_location_metadata')
	i64_t := ssa_mod.type_store.get_int(64)
	f64_t := ssa_mod.type_store.get_float(64)
	ret_t := register_x64_matrix_struct_fields(mut ssa_mod, [i64_t, i64_t, i64_t])
	aggregate_t := register_x64_matrix_struct_fields(mut ssa_mod, [i64_t, f64_t])

	callee_id := ssa_mod.new_function('callee', ret_t, [])
	callee_param := ssa_mod.add_value_node(.argument, aggregate_t, 'p', 0)
	ssa_mod.funcs[callee_id].params << callee_param

	caller_id := ssa_mod.new_function('caller', i64_t, [])
	caller_arg := ssa_mod.add_value_node(.argument, aggregate_t, 'a', 0)
	ssa_mod.funcs[caller_id].params << caller_arg
	caller_entry := ssa_mod.add_block(caller_id, 'entry')
	fn_val := ssa_mod.add_value_node(.unknown, 0, 'callee', 0)
	call_val := ssa_mod.add_instr(.call, caller_entry, ret_t, [fn_val, caller_arg])
	zero := ssa_mod.get_or_add_const(i64_t, '0')
	ssa_mod.add_instr(.ret, caller_entry, 0, [zero])

	mut mir_mod := mir.lower_from_ssa(ssa_mod)
	lower_with_x64_abi(mut mir_mod, .x64, .sysv)
	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]

	assert mir_mod.funcs[callee_id].abi_ret_indirect
	assert call_instr.abi_ret_indirect
	assert call_instr.op == .call_sret
	assert_abi_layout(mir_mod.funcs[callee_id].abi_param_layouts[0], [
		.int_reg,
		.sse_reg,
	], [1, 0], [0, 8], [.integer, .sse])
	assert_abi_layout(call_instr.abi_arg_layouts[0], [.int_reg, .sse_reg], [
		1,
		0,
	], [0, 8], [.integer, .sse])
}

fn test_x64_sysv_location_metadata_places_seventh_integer_scalar_on_stack() {
	mut ssa_mod := ssa.Module.new('abi_test_x64_sysv_integer_scalar_boundary')
	i64_t := ssa_mod.type_store.get_int(64)
	mir_mod, callee_id, call_val := build_sysv_scalar_boundary_module(mut ssa_mod, i64_t, 7)
	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]

	assert_abi_layout(mir_mod.funcs[callee_id].abi_param_layouts[6], [.stack], [0], [
		0,
	], [.integer])
	assert_abi_layout(call_instr.abi_arg_layouts[6], [.stack], [0], [0], [.integer])
}

fn test_x64_sysv_location_metadata_places_ninth_sse_scalar_on_stack() {
	mut ssa_mod := ssa.Module.new('abi_test_x64_sysv_sse_scalar_boundary')
	f64_t := ssa_mod.type_store.get_float(64)
	mir_mod, callee_id, call_val := build_sysv_scalar_boundary_module(mut ssa_mod, f64_t, 9)
	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]

	assert_abi_layout(mir_mod.funcs[callee_id].abi_param_layouts[8], [.stack], [0], [
		0,
	], [.sse])
	assert_abi_layout(call_instr.abi_arg_layouts[8], [.stack], [0], [0], [.sse])
}

fn test_x64_sysv_location_metadata_float128_uses_one_sse_register() {
	// x86-64 psABI 3.2.3 classifies __float128 as SSE plus SSEUP; SSEUP is
	// carried in the upper half of the previous SSE register, not a new XMM.
	mut ssa_mod := ssa.Module.new('abi_test_x64_sysv_float128_one_xmm')
	f128_t := ssa_mod.type_store.get_float(128)
	mir_mod, callee_id, call_val := build_sysv_scalar_boundary_module(mut ssa_mod, f128_t, 1)
	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]

	assert_abi_layout(mir_mod.funcs[callee_id].abi_param_layouts[0], [.sse_reg, .sse_reg], [
		0,
		0,
	], [0, 8], [.sse, .sseup])
	assert_abi_layout(call_instr.abi_arg_layouts[0], [.sse_reg, .sse_reg], [
		0,
		0,
	], [0, 8], [.sse, .sseup])
}

fn test_x64_sysv_location_metadata_float128_rolls_back_when_sse_regs_are_exhausted() {
	// x86-64 psABI 3.2.3 says an argument that cannot fit entirely in the
	// available registers is passed on the stack; this guards the SSE/SSEUP
	// rollback path without claiming codegen consumes the metadata yet.
	mut ssa_mod := ssa.Module.new('abi_test_x64_sysv_float128_sse_exhausted')
	f64_t := ssa_mod.type_store.get_float(64)
	f128_t := ssa_mod.type_store.get_float(128)
	i64_t := ssa_mod.type_store.get_int(64)
	mut param_types := []ssa.TypeID{len: 8, init: f64_t}
	param_types << f128_t

	callee_id := ssa_mod.new_function('callee', i64_t, [])
	for i, typ in param_types {
		param := ssa_mod.add_value_node(.argument, typ, 'p${i}', 0)
		ssa_mod.funcs[callee_id].params << param
	}
	caller_id := ssa_mod.new_function('caller', i64_t, [])
	mut args := []ssa.ValueID{}
	for i, typ in param_types {
		arg := ssa_mod.add_value_node(.argument, typ, 'a${i}', 0)
		ssa_mod.funcs[caller_id].params << arg
		args << arg
	}
	caller_entry := ssa_mod.add_block(caller_id, 'entry')
	fn_val := ssa_mod.add_value_node(.unknown, 0, 'callee', 0)
	mut operands := [fn_val]
	operands << args
	call_val := ssa_mod.add_instr(.call, caller_entry, i64_t, operands)
	zero := ssa_mod.get_or_add_const(i64_t, '0')
	ssa_mod.add_instr(.ret, caller_entry, 0, [zero])

	mut mir_mod := mir.lower_from_ssa(ssa_mod)
	lower_with_x64_abi(mut mir_mod, .x64, .sysv)
	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]

	assert_abi_layout(mir_mod.funcs[callee_id].abi_param_layouts[8], [.stack, .stack], [
		0,
		1,
	], [0, 8], [.sse, .sseup])
	assert_abi_layout(call_instr.abi_arg_layouts[8], [.stack, .stack], [0, 1], [
		0,
		8,
	], [.sse, .sseup])
	assert call_instr.abi_arg_class[8] == .in_reg
	assert call_instr.op == .call
}

fn test_x64_sysv_location_metadata_assigns_two_xmms_when_available_for_sse_aggregate() {
	mir_mod, callee_id, call_val, aggregate_idx := build_sysv_layout_call_module(0, true, 'f64_f64')
	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]

	assert_abi_layout(mir_mod.funcs[callee_id].abi_param_layouts[aggregate_idx], [
		.sse_reg,
		.sse_reg,
	], [0, 1], [0, 8], [.sse, .sse])
	assert_abi_layout(call_instr.abi_arg_layouts[aggregate_idx], [.sse_reg, .sse_reg], [
		0,
		1,
	], [0, 8], [.sse, .sse])
}

fn test_x64_sysv_location_metadata_passes_memory_class_aggregate_indirectly() {
	mut ssa_mod := ssa.Module.new('abi_test_x64_sysv_memory_class_layout')
	i64_t := ssa_mod.type_store.get_int(64)
	f64_t := ssa_mod.type_store.get_float(64)
	memory_t := register_x64_matrix_struct_fields(mut ssa_mod, [f64_t, f64_t, f64_t])
	callee_id := ssa_mod.new_function('callee', i64_t, [])
	param := ssa_mod.add_value_node(.argument, memory_t, 'p', 0)
	ssa_mod.funcs[callee_id].params << param
	caller_id := ssa_mod.new_function('caller', i64_t, [])
	arg := ssa_mod.add_value_node(.argument, memory_t, 'a', 0)
	ssa_mod.funcs[caller_id].params << arg
	caller_entry := ssa_mod.add_block(caller_id, 'entry')
	fn_val := ssa_mod.add_value_node(.unknown, 0, 'callee', 0)
	call_val := ssa_mod.add_instr(.call, caller_entry, i64_t, [fn_val, arg])
	zero := ssa_mod.get_or_add_const(i64_t, '0')
	ssa_mod.add_instr(.ret, caller_entry, 0, [zero])

	mut mir_mod := mir.lower_from_ssa(ssa_mod)
	lower_with_x64_abi(mut mir_mod, .x64, .sysv)
	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]

	assert mir_mod.funcs[callee_id].abi_param_class == [.indirect]
	assert call_instr.abi_arg_class == [.indirect]
	assert_abi_value_class(mir_mod.funcs[callee_id].abi_param_classes[0], .indirect, [
		.memory,
	])
	assert_abi_value_class(call_instr.abi_arg_classes[0], .indirect, [.memory])
	assert_abi_layout(mir_mod.funcs[callee_id].abi_param_layouts[0], [.int_reg], [0], [
		0,
	], [.integer])
	assert_abi_layout(call_instr.abi_arg_layouts[0], [.int_reg], [0], [0], [.integer])
}

fn test_x64_sysv_location_metadata_rolls_back_mixed_aggregate_when_gprs_are_exhausted() {
	mir_mod, callee_id, call_val, aggregate_idx := build_sysv_layout_call_module(6, false,
		'i64_f64')
	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]

	assert_abi_layout(mir_mod.funcs[callee_id].abi_param_layouts[aggregate_idx], [
		.stack,
		.stack,
	], [0, 1], [0, 8], [.integer, .sse])
	assert_abi_layout(call_instr.abi_arg_layouts[aggregate_idx], [.stack, .stack], [0, 1], [
		0,
		8,
	], [.integer, .sse])
	assert call_instr.abi_arg_class[aggregate_idx] == .in_reg
	assert call_instr.op == .call
}

fn test_x64_sysv_location_metadata_rolls_back_mixed_aggregate_when_xmms_are_exhausted() {
	mir_mod, callee_id, call_val, aggregate_idx := build_sysv_layout_call_module(8, true, 'i64_f64')
	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]

	assert_abi_layout(mir_mod.funcs[callee_id].abi_param_layouts[aggregate_idx], [
		.stack,
		.stack,
	], [0, 1], [0, 8], [.integer, .sse])
	assert_abi_layout(call_instr.abi_arg_layouts[aggregate_idx], [.stack, .stack], [0, 1], [
		0,
		8,
	], [.integer, .sse])
	assert call_instr.abi_arg_class[aggregate_idx] == .in_reg
	assert call_instr.op == .call
}

fn test_x64_sysv_location_metadata_stack_indexes_continue_after_scalar_spill() {
	mut ssa_mod := ssa.Module.new('abi_test_x64_sysv_stack_continuation_types')
	i64_t := ssa_mod.type_store.get_int(64)
	struct_i64_i64_t := register_x64_matrix_struct_fields(mut ssa_mod, [i64_t, i64_t])
	mut param_types := []ssa.TypeID{len: 6, init: i64_t}
	param_types << i64_t
	param_types << struct_i64_i64_t

	callee_id := ssa_mod.new_function('callee', i64_t, [])
	for i, typ in param_types {
		param := ssa_mod.add_value_node(.argument, typ, 'p${i}', 0)
		ssa_mod.funcs[callee_id].params << param
	}
	caller_id := ssa_mod.new_function('caller', i64_t, [])
	mut args := []ssa.ValueID{}
	for i, typ in param_types {
		arg := ssa_mod.add_value_node(.argument, typ, 'a${i}', 0)
		ssa_mod.funcs[caller_id].params << arg
		args << arg
	}
	caller_entry := ssa_mod.add_block(caller_id, 'entry')
	fn_val := ssa_mod.add_value_node(.unknown, 0, 'callee', 0)
	mut operands := [fn_val]
	operands << args
	call_val := ssa_mod.add_instr(.call, caller_entry, i64_t, operands)
	zero := ssa_mod.get_or_add_const(i64_t, '0')
	ssa_mod.add_instr(.ret, caller_entry, 0, [zero])

	mut mir_mod := mir.lower_from_ssa(ssa_mod)
	lower_with_x64_abi(mut mir_mod, .x64, .sysv)
	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]

	assert_abi_layout_locs(mir_mod.funcs[callee_id].abi_param_layouts[6], [.stack], [
		0,
	])
	assert_abi_layout_locs(mir_mod.funcs[callee_id].abi_param_layouts[7], [.stack, .stack], [
		1,
		2,
	])
	assert_abi_layout_locs(call_instr.abi_arg_layouts[6], [.stack], [0])
	assert_abi_layout_locs(call_instr.abi_arg_layouts[7], [.stack, .stack], [1, 2])
}

fn test_x64_sysv_direct_callsite_struct_size_matrix_current_threshold() {
	sizes := [1, 2, 3, 4, 5, 6, 7, 8, 9, 16, 17, 32]
	mir_mod, call_val := build_x64_matrix_direct_call_module(.sysv)
	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]

	assert call_instr.abi_arg_class.len == sizes.len
	for i, size in sizes {
		should_be_indirect := size > 16
		assert (call_instr.abi_arg_class[i] == .indirect) == should_be_indirect
	}
}

fn test_x64_sysv_callsite_return_size_matrix_current_threshold() {
	sizes := [1, 2, 3, 4, 5, 6, 7, 8, 9, 16, 17, 32]
	for size in sizes {
		mir_mod, call_val := build_x64_matrix_return_call_module(.sysv, size)
		call_instr := mir_mod.instrs[mir_mod.values[call_val].index]
		should_be_indirect := size > 16
		assert call_instr.abi_ret_indirect == should_be_indirect
		assert (call_instr.op == .call_sret) == should_be_indirect
	}
}

fn test_x64_sysv_struct_size_matrix_current_lowering_threshold() {
	mut ssa_mod := ssa.Module.new('abi_test_x64_sysv_size_matrix')
	i64_t := ssa_mod.type_store.get_int(64)
	sizes := [1, 2, 3, 4, 5, 6, 7, 8, 9, 16, 17, 32]
	mut struct_types := []ssa.TypeID{}
	for size in sizes {
		struct_types << register_x64_matrix_struct_bytes(mut ssa_mod, size)
	}

	param_fn_id := ssa_mod.new_function('param_matrix', i64_t, [])
	for i, struct_t in struct_types {
		param := ssa_mod.add_value_node(.argument, struct_t, 's${sizes[i]}', 0)
		ssa_mod.funcs[param_fn_id].params << param
	}
	mut ret_fn_ids := []int{}
	for i, struct_t in struct_types {
		ret_fn_ids << ssa_mod.new_function('ret${sizes[i]}', struct_t, [])
	}

	mut mir_mod := mir.lower_from_ssa(ssa_mod)
	lower_with_x64_abi(mut mir_mod, .x64, .sysv)

	for i, size in sizes {
		should_be_indirect := size > 16
		assert (mir_mod.funcs[param_fn_id].abi_param_class[i] == .indirect) == should_be_indirect
		assert mir_mod.funcs[ret_fn_ids[i]].abi_ret_indirect == should_be_indirect
	}
}

fn test_x64_sysv_aggregate_memory_class_for_representable_mir_types() {
	mut ssa_mod := ssa.Module.new('abi_test_x64_sysv_memory_class')
	i64_t := ssa_mod.type_store.get_int(64)
	f64_t := ssa_mod.type_store.get_float(64)
	f80_t := ssa_mod.type_store.get_float(80)
	f128_t := ssa_mod.type_store.get_float(128)
	two_eightbytes_t := register_x64_matrix_struct_fields(mut ssa_mod, [f64_t, f64_t])
	three_eightbytes_t := register_x64_matrix_struct_fields(mut ssa_mod, [f64_t, f64_t, f64_t])
	x87_aggregate_t := register_x64_matrix_struct_fields(mut ssa_mod, [f80_t])
	float128_aggregate_t := register_x64_matrix_struct_fields(mut ssa_mod, [f128_t])

	param_fn_id := ssa_mod.new_function('sysv_memory_params', i64_t, [])
	for typ in [two_eightbytes_t, three_eightbytes_t, x87_aggregate_t, float128_aggregate_t] {
		param := ssa_mod.add_value_node(.argument, typ, 'p', 0)
		ssa_mod.funcs[param_fn_id].params << param
	}
	ret_two_id := ssa_mod.new_function('ret_two_eightbytes', two_eightbytes_t, [])
	ret_three_id := ssa_mod.new_function('ret_three_eightbytes', three_eightbytes_t, [])
	ret_x87_id := ssa_mod.new_function('ret_x87_aggregate', x87_aggregate_t, [])
	ret_float128_id := ssa_mod.new_function('ret_float128_aggregate', float128_aggregate_t, [])

	mut mir_mod := mir.lower_from_ssa(ssa_mod)
	lower_with_x64_abi(mut mir_mod, .x64, .sysv)

	assert_abi_value_class(mir_mod.funcs[param_fn_id].abi_param_classes[0], .direct, [
		.sse,
		.sse,
	])
	assert_abi_value_class(mir_mod.funcs[param_fn_id].abi_param_classes[1], .indirect, [
		.memory,
	])
	assert_abi_value_class(mir_mod.funcs[param_fn_id].abi_param_classes[2], .indirect, [
		.memory,
	])
	assert_abi_value_class(mir_mod.funcs[param_fn_id].abi_param_classes[3], .direct, [
		.sse,
		.sseup,
	])
	assert_abi_value_class(mir_mod.funcs[ret_two_id].abi_ret_class, .direct, [.sse, .sse])
	assert_abi_value_class(mir_mod.funcs[ret_three_id].abi_ret_class, .indirect, [
		.memory,
	])
	assert_abi_value_class(mir_mod.funcs[ret_x87_id].abi_ret_class, .indirect, [
		.memory,
	])
	assert_abi_value_class(mir_mod.funcs[ret_float128_id].abi_ret_class, .direct, [
		.sse,
		.sseup,
	])
}

fn test_x64_sysv_metadata_eightbyte_classification_for_representable_types() {
	mut ssa_mod := ssa.Module.new('abi_test_x64_sysv_eightbyte_metadata')
	i64_t := ssa_mod.type_store.get_int(64)
	f64_t := ssa_mod.type_store.get_float(64)
	f80_t := ssa_mod.type_store.get_float(80)
	f128_t := ssa_mod.type_store.get_float(128)
	struct_i64_t := register_x64_matrix_struct_fields(mut ssa_mod, [i64_t])
	struct_i64_i64_t := register_x64_matrix_struct_fields(mut ssa_mod, [i64_t, i64_t])
	struct_f64_t := register_x64_matrix_struct_fields(mut ssa_mod, [f64_t])
	struct_f64_f64_t := register_x64_matrix_struct_fields(mut ssa_mod, [f64_t, f64_t])
	struct_i64_f64_t := register_x64_matrix_struct_fields(mut ssa_mod, [i64_t, f64_t])
	struct_f64_i64_t := register_x64_matrix_struct_fields(mut ssa_mod, [f64_t, i64_t])
	too_large_t := register_x64_matrix_struct_fields(mut ssa_mod, [f64_t, f64_t, f64_t])
	x87_aggregate_t := register_x64_matrix_struct_fields(mut ssa_mod, [f80_t])
	float128_aggregate_t := register_x64_matrix_struct_fields(mut ssa_mod, [f128_t])
	types := [
		struct_i64_t,
		struct_i64_i64_t,
		struct_f64_t,
		struct_f64_f64_t,
		struct_i64_f64_t,
		struct_f64_i64_t,
		too_large_t,
		x87_aggregate_t,
		float128_aggregate_t,
	]

	param_fn_id := ssa_mod.new_function('sysv_metadata_params', i64_t, [])
	for typ in types {
		param := ssa_mod.add_value_node(.argument, typ, 'p', 0)
		ssa_mod.funcs[param_fn_id].params << param
	}
	mut ret_fn_ids := []int{}
	for i, typ in types {
		ret_fn_ids << ssa_mod.new_function('sysv_metadata_ret_${i}', typ, [])
	}

	mut mir_mod := mir.lower_from_ssa(ssa_mod)
	lower_with_x64_abi(mut mir_mod, .x64, .sysv)

	expected_classes := [
		[mir.AbiEightbyteClass.integer],
		[mir.AbiEightbyteClass.integer, .integer],
		[mir.AbiEightbyteClass.sse],
		[mir.AbiEightbyteClass.sse, .sse],
		[mir.AbiEightbyteClass.integer, .sse],
		[mir.AbiEightbyteClass.sse, .integer],
		[mir.AbiEightbyteClass.memory],
		[mir.AbiEightbyteClass.memory],
		[mir.AbiEightbyteClass.sse, .sseup],
	]
	expected_modes := [
		mir.AbiPassMode.direct,
		.direct,
		.direct,
		.direct,
		.direct,
		.direct,
		.indirect,
		.indirect,
		.direct,
	]
	for i in 0 .. types.len {
		assert_abi_value_class(mir_mod.funcs[param_fn_id].abi_param_classes[i], expected_modes[i],
			expected_classes[i])
		assert_abi_value_class(mir_mod.funcs[ret_fn_ids[i]].abi_ret_class, expected_modes[i],
			expected_classes[i])
	}
}

fn test_x64_sysv_memory_class_projects_to_legacy_indirect_flags() {
	mut ssa_mod := ssa.Module.new('abi_test_x64_sysv_callsite_memory_class')
	i64_t := ssa_mod.type_store.get_int(64)
	f80_t := ssa_mod.type_store.get_float(80)
	x87_aggregate_t := register_x64_matrix_struct_fields(mut ssa_mod, [f80_t])

	callee_id := ssa_mod.new_function('callee', x87_aggregate_t, [])
	callee_param := ssa_mod.add_value_node(.argument, x87_aggregate_t, 'p', 0)
	ssa_mod.funcs[callee_id].params << callee_param
	caller_id := ssa_mod.new_function('caller', i64_t, [])
	arg := ssa_mod.add_value_node(.argument, x87_aggregate_t, 'a', 0)
	ssa_mod.funcs[caller_id].params << arg
	caller_entry := ssa_mod.add_block(caller_id, 'entry')
	fn_val := ssa_mod.add_value_node(.unknown, 0, 'callee', 0)
	call_val := ssa_mod.add_instr(.call, caller_entry, x87_aggregate_t, [fn_val, arg])
	zero := ssa_mod.get_or_add_const(i64_t, '0')
	ssa_mod.add_instr(.ret, caller_entry, 0, [zero])

	mut mir_mod := mir.lower_from_ssa(ssa_mod)
	lower_with_x64_abi(mut mir_mod, .x64, .sysv)

	assert_abi_value_class(mir_mod.funcs[callee_id].abi_param_classes[0], .indirect, [
		.memory,
	])
	assert_abi_value_class(mir_mod.funcs[callee_id].abi_ret_class, .indirect, [
		.memory,
	])
	assert mir_mod.funcs[callee_id].abi_param_class == [.indirect]
	assert mir_mod.funcs[callee_id].abi_ret_indirect == true
	assert_abi_layout(mir_mod.funcs[callee_id].abi_param_layouts[0], [.int_reg], [1], [
		0,
	], [.integer])

	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]
	assert_abi_value_class(call_instr.abi_arg_classes[0], .indirect, [.memory])
	assert_abi_value_class(call_instr.abi_ret_class, .indirect, [.memory])
	assert call_instr.abi_arg_class == [.indirect]
	assert call_instr.abi_ret_indirect == true
	assert call_instr.op == .call_sret
	assert_abi_layout(call_instr.abi_arg_layouts[0], [.int_reg], [1], [0], [.integer])
}

fn test_x64_sysv_direct_aggregate_size_guard_boundary() {
	assert !sysv_aggregate_must_be_memory_before_classification(16)
	assert sysv_aggregate_must_be_memory_before_classification(17)
}

fn test_x64_sysv_huge_fixed_array_uses_early_memory_guard_and_bounded_metadata() {
	mut ssa_mod := ssa.Module.new('abi_test_x64_sysv_huge_fixed_array_memory')
	i64_t := ssa_mod.type_store.get_int(64)
	u8_t := ssa_mod.type_store.get_uint(8)
	huge_array_t := ssa_mod.type_store.register(ssa.Type{
		kind:      .array_t
		elem_type: u8_t
		len:       1_000_000
	})

	callee_id := ssa_mod.new_function('callee', huge_array_t, [])
	param := ssa_mod.add_value_node(.argument, huge_array_t, 'p', 0)
	ssa_mod.funcs[callee_id].params << param
	caller_id := ssa_mod.new_function('caller', i64_t, [])
	arg := ssa_mod.add_value_node(.argument, huge_array_t, 'a', 0)
	ssa_mod.funcs[caller_id].params << arg
	caller_entry := ssa_mod.add_block(caller_id, 'entry')
	fn_val := ssa_mod.add_value_node(.unknown, 0, 'callee', 0)
	call_val := ssa_mod.add_instr(.call, caller_entry, huge_array_t, [fn_val, arg])
	zero := ssa_mod.get_or_add_const(i64_t, '0')
	ssa_mod.add_instr(.ret, caller_entry, 0, [zero])

	mut mir_mod := mir.lower_from_ssa(ssa_mod)
	lower_with_x64_abi(mut mir_mod, .x64, .sysv)
	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]

	huge_size := mir_mod.type_size(huge_array_t)
	assert huge_size == 1_000_000
	assert sysv_aggregate_must_be_memory_before_classification(huge_size)
	direct_class := sysv_abi_value_class(mir_mod, huge_array_t)
	assert_abi_value_class(direct_class, .indirect, [.memory])
	assert direct_class.classes.len == 1
	assert direct_class.classes.len < (huge_size + 7) / 8

	param_class := mir_mod.funcs[callee_id].abi_param_classes[0]
	ret_class := mir_mod.funcs[callee_id].abi_ret_class
	assert_abi_value_class(param_class, .indirect, [.memory])
	assert_abi_value_class(ret_class, .indirect, [.memory])
	assert param_class.classes.len == 1
	assert ret_class.classes.len == 1
	assert mir_mod.funcs[callee_id].abi_param_class == [.indirect]
	assert mir_mod.funcs[callee_id].abi_ret_indirect
	assert mir_mod.funcs[callee_id].abi_param_layouts[0].locs.len == 1
	assert_abi_layout(mir_mod.funcs[callee_id].abi_param_layouts[0], [.int_reg], [1], [
		0,
	], [.integer])

	assert_abi_value_class(call_instr.abi_arg_classes[0], .indirect, [.memory])
	assert_abi_value_class(call_instr.abi_ret_class, .indirect, [.memory])
	assert call_instr.abi_arg_classes[0].classes.len == 1
	assert call_instr.abi_ret_class.classes.len == 1
	assert call_instr.abi_arg_class == [.indirect]
	assert call_instr.abi_ret_indirect
	assert call_instr.op == .call_sret
	assert call_instr.abi_arg_layouts[0].locs.len == 1
	assert_abi_layout(call_instr.abi_arg_layouts[0], [.int_reg], [1], [0], [.integer])
}

fn test_x64_sysv_indirect_argument_consumes_gpr_before_direct_aggregate_layout() {
	mut ssa_mod := ssa.Module.new('abi_test_x64_sysv_indirect_arg_gpr_consumption')
	i64_t := ssa_mod.type_store.get_int(64)
	f80_t := ssa_mod.type_store.get_float(80)
	x87_aggregate_t := register_x64_matrix_struct_fields(mut ssa_mod, [f80_t])
	integer_pair_t := register_x64_matrix_struct_fields(mut ssa_mod, [i64_t, i64_t])

	callee_id := ssa_mod.new_function('callee', i64_t, [])
	memory_param := ssa_mod.add_value_node(.argument, x87_aggregate_t, 'memory_arg', 0)
	pair_param := ssa_mod.add_value_node(.argument, integer_pair_t, 'pair_arg', 0)
	ssa_mod.funcs[callee_id].params << [memory_param, pair_param]
	caller_id := ssa_mod.new_function('caller', i64_t, [])
	memory_arg := ssa_mod.add_value_node(.argument, x87_aggregate_t, 'memory_arg', 0)
	pair_arg := ssa_mod.add_value_node(.argument, integer_pair_t, 'pair_arg', 0)
	ssa_mod.funcs[caller_id].params << [memory_arg, pair_arg]
	caller_entry := ssa_mod.add_block(caller_id, 'entry')
	fn_val := ssa_mod.add_value_node(.unknown, 0, 'callee', 0)
	call_val := ssa_mod.add_instr(.call, caller_entry, i64_t, [fn_val, memory_arg, pair_arg])
	zero := ssa_mod.get_or_add_const(i64_t, '0')
	ssa_mod.add_instr(.ret, caller_entry, 0, [zero])

	mut mir_mod := mir.lower_from_ssa(ssa_mod)
	lower_with_x64_abi(mut mir_mod, .x64, .sysv)
	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]

	assert mir_mod.funcs[callee_id].abi_param_class == [.indirect, .in_reg]
	assert call_instr.abi_arg_class == [.indirect, .in_reg]
	assert_abi_layout(mir_mod.funcs[callee_id].abi_param_layouts[0], [.int_reg], [0], [
		0,
	], [.integer])
	assert_abi_layout(mir_mod.funcs[callee_id].abi_param_layouts[1], [
		.int_reg,
		.int_reg,
	], [1, 2], [0, 8], [.integer, .integer])
	assert_abi_layout(call_instr.abi_arg_layouts[0], [.int_reg], [0], [0], [.integer])
	assert_abi_layout(call_instr.abi_arg_layouts[1], [.int_reg, .int_reg], [1, 2], [
		0,
		8,
	], [.integer, .integer])
}

fn test_x64_sysv_indirect_argument_spills_after_gprs_are_exhausted() {
	mut ssa_mod := ssa.Module.new('abi_test_x64_sysv_indirect_arg_stack_spill')
	i64_t := ssa_mod.type_store.get_int(64)
	f80_t := ssa_mod.type_store.get_float(80)
	x87_aggregate_t := register_x64_matrix_struct_fields(mut ssa_mod, [f80_t])
	mut param_types := []ssa.TypeID{len: 6, init: i64_t}
	param_types << x87_aggregate_t
	param_types << i64_t

	callee_id := ssa_mod.new_function('callee', i64_t, [])
	for i, typ in param_types {
		param := ssa_mod.add_value_node(.argument, typ, 'p${i}', 0)
		ssa_mod.funcs[callee_id].params << param
	}
	caller_id := ssa_mod.new_function('caller', i64_t, [])
	mut args := []ssa.ValueID{}
	for i, typ in param_types {
		arg := ssa_mod.add_value_node(.argument, typ, 'a${i}', 0)
		ssa_mod.funcs[caller_id].params << arg
		args << arg
	}
	caller_entry := ssa_mod.add_block(caller_id, 'entry')
	fn_val := ssa_mod.add_value_node(.unknown, 0, 'callee', 0)
	mut operands := [fn_val]
	operands << args
	call_val := ssa_mod.add_instr(.call, caller_entry, i64_t, operands)
	zero := ssa_mod.get_or_add_const(i64_t, '0')
	ssa_mod.add_instr(.ret, caller_entry, 0, [zero])

	mut mir_mod := mir.lower_from_ssa(ssa_mod)
	lower_with_x64_abi(mut mir_mod, .x64, .sysv)
	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]

	assert mir_mod.funcs[callee_id].abi_param_class[6] == .indirect
	assert call_instr.abi_arg_class[6] == .indirect
	assert_abi_layout(mir_mod.funcs[callee_id].abi_param_layouts[6], [.stack], [0], [
		0,
	], [.integer])
	assert_abi_layout(call_instr.abi_arg_layouts[6], [.stack], [0], [0], [.integer])
	assert_abi_layout(mir_mod.funcs[callee_id].abi_param_layouts[7], [.stack], [1], [
		0,
	], [.integer])
	assert_abi_layout(call_instr.abi_arg_layouts[7], [.stack], [1], [0], [.integer])
}

fn test_x64_windows_indirect_call_uses_function_pointer_signature_for_aggregate_args() {
	mut ssa_mod := ssa.Module.new('abi_test_x64_windows_indirect_call_matrix')
	i64_t := ssa_mod.type_store.get_int(64)
	struct8_t := register_x64_matrix_struct_bytes(mut ssa_mod, 8)
	struct9_t := register_x64_matrix_struct_bytes(mut ssa_mod, 9)
	fn_sig_t := ssa_mod.type_store.register(ssa.Type{
		kind:     .func_t
		params:   [struct8_t, struct9_t]
		ret_type: i64_t
	})
	fn_ptr_t := ssa_mod.type_store.get_ptr(fn_sig_t)

	caller_id := ssa_mod.new_function('caller', i64_t, [])
	fn_ptr := ssa_mod.add_value_node(.argument, fn_ptr_t, 'fn_ptr', 0)
	arg8 := ssa_mod.add_value_node(.argument, struct8_t, 'arg8', 0)
	arg9 := ssa_mod.add_value_node(.argument, struct9_t, 'arg9', 0)
	ssa_mod.funcs[caller_id].params << [fn_ptr, arg8, arg9]
	caller_entry := ssa_mod.add_block(caller_id, 'entry')
	call_val := ssa_mod.add_instr(.call_indirect, caller_entry, i64_t, [fn_ptr, arg8, arg9])
	zero := ssa_mod.get_or_add_const(i64_t, '0')
	ssa_mod.add_instr(.ret, caller_entry, 0, [zero])

	mut mir_mod := mir.lower_from_ssa(ssa_mod)
	lower_with_x64_abi(mut mir_mod, .x64, .windows)

	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]
	assert call_instr.op == .call_indirect
	assert call_instr.abi_arg_class == [.in_reg, .indirect]
}

fn test_x64_windows_unresolved_callsite_uses_alloca_logical_aggregate_type() {
	mut ssa_mod := ssa.Module.new('abi_test_x64_windows_alloca_logical_arg')
	i64_t := ssa_mod.type_store.get_int(64)
	struct9_t := register_x64_matrix_struct_bytes(mut ssa_mod, 9)
	struct9_ptr_t := ssa_mod.type_store.get_ptr(struct9_t)

	caller_id := ssa_mod.new_function('caller', i64_t, [])
	caller_entry := ssa_mod.add_block(caller_id, 'entry')
	tmp := ssa_mod.add_instr(.alloca, caller_entry, struct9_ptr_t, []ssa.ValueID{})
	external_fn := ssa_mod.add_value_node(.unknown, 0, 'external_accept_struct9', 0)
	call_val := ssa_mod.add_instr(.call, caller_entry, i64_t, [external_fn, tmp])
	zero := ssa_mod.get_or_add_const(i64_t, '0')
	ssa_mod.add_instr(.ret, caller_entry, 0, [zero])

	mut mir_mod := mir.lower_from_ssa(ssa_mod)
	lower_with_x64_abi(mut mir_mod, .x64, .windows)

	call_instr := mir_mod.instrs[mir_mod.values[call_val].index]
	assert call_instr.abi_arg_class == [.indirect]
}
