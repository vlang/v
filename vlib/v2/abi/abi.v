// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module abi

import v2.mir
import v2.pref

pub enum X64Abi {
	sysv
	windows
}

const sysv_int_arg_reg_count = 6
const sysv_sse_arg_reg_count = 8
const sysv_max_direct_aggregate_size = 16

struct SysVLocationState {
mut:
	int_regs    int
	sse_regs    int
	stack_slots int
}

// lower annotates MIR with ABI classification metadata.
// Current scope is intentionally conservative: it classifies which arguments
// and return values must be passed indirectly for each function.
pub fn lower(mut m mir.Module, arch pref.Arch) {
	lower_with_x64_abi(mut m, arch, .sysv)
}

pub fn lower_with_x64_abi(mut m mir.Module, arch pref.Arch, x64_abi X64Abi) {
	is_x64 := arch == .x64
	mut fn_by_name := map[string]int{}
	for i := 0; i < m.funcs.len; i++ {
		mut f := &m.funcs[i]
		fn_by_name[f.name] = i
		if is_x64 {
			f.abi_ret_class = abi_value_class(m, f.typ, arch, x64_abi)
			f.abi_ret_indirect = abi_class_is_indirect(f.abi_ret_class, m, f.typ, arch, x64_abi)
		} else {
			f.abi_ret_indirect = needs_indirect(m, f.typ, arch, x64_abi)
		}
		f.abi_param_class = []mir.AbiArgClass{len: f.params.len, init: .in_reg}
		if is_x64 {
			f.abi_param_classes = []mir.AbiValueClass{len: f.params.len}
			f.abi_param_layouts = []mir.AbiValueLayout{len: f.params.len}
		}
		mut param_loc_state := SysVLocationState{}
		if arch == .x64 && x64_abi == .sysv && f.abi_ret_indirect {
			param_loc_state.int_regs = 1
		}
		for pi, param_id in f.params {
			if param_id < 0 || param_id >= m.values.len {
				continue
			}
			param_typ := m.values[param_id].typ
			if is_x64 {
				param_class := abi_value_class(m, param_typ, arch, x64_abi)
				f.abi_param_classes[pi] = param_class
				if x64_abi == .sysv {
					f.abi_param_layouts[pi] = sysv_assign_value_layout(param_class, mut
						param_loc_state)
				}
				if abi_class_is_indirect(param_class, m, param_typ, arch, x64_abi) {
					f.abi_param_class[pi] = .indirect
				}
			} else if needs_indirect(m, param_typ, arch, x64_abi) {
				f.abi_param_class[pi] = .indirect
			}
		}
	}

	lower_calls(mut m, arch, x64_abi, fn_by_name)
}

fn needs_indirect(m mir.Module, typ_id int, arch pref.Arch, x64_abi X64Abi) bool {
	ssa_mod := m.ssa()
	if ssa_mod == unsafe { nil } || typ_id <= 0 || typ_id >= ssa_mod.type_store.types.len {
		return false
	}
	typ := ssa_mod.type_store.types[typ_id]
	if typ.kind !in [.struct_t, .array_t] {
		return false
	}
	size := m.type_size(typ_id)
	return match arch {
		.arm64 {
			size > 16
		}
		.x64 {
			match x64_abi {
				.sysv { size > sysv_max_direct_aggregate_size }
				.windows { size !in [1, 2, 4, 8] }
			}
		}
		else {
			size > 16
		}
	}
}

fn abi_class_is_indirect(value_class mir.AbiValueClass, m mir.Module, typ_id int, arch pref.Arch, x64_abi X64Abi) bool {
	if value_class.mode == .indirect {
		return true
	}
	return needs_indirect(m, typ_id, arch, x64_abi)
}

fn abi_value_class(m mir.Module, typ_id int, arch pref.Arch, x64_abi X64Abi) mir.AbiValueClass {
	ssa_mod := m.ssa()
	size := m.type_size(typ_id)
	if ssa_mod == unsafe { nil } || typ_id <= 0 || typ_id >= ssa_mod.type_store.types.len {
		return mir.AbiValueClass{
			mode: .direct
			size: size
		}
	}
	typ := ssa_mod.type_store.types[typ_id]
	if arch == .x64 && x64_abi == .sysv {
		if typ.kind !in [.struct_t, .array_t] {
			return sysv_scalar_abi_value_class(m, typ_id)
		}
		return sysv_abi_value_class(m, typ_id)
	}
	indirect := needs_indirect(m, typ_id, arch, x64_abi)
	return mir.AbiValueClass{
		mode: if indirect { .indirect } else { .direct }
		size: size
	}
}

fn sysv_scalar_abi_value_class(m mir.Module, typ_id int) mir.AbiValueClass {
	ssa_mod := m.ssa()
	size := m.type_size(typ_id)
	if ssa_mod == unsafe { nil } || typ_id <= 0 || typ_id >= ssa_mod.type_store.types.len {
		return mir.AbiValueClass{
			mode: .direct
			size: size
		}
	}
	typ := ssa_mod.type_store.types[typ_id]
	classes := match typ.kind {
		.int_t, .ptr_t, .func_t {
			[mir.AbiEightbyteClass.integer]
		}
		.float_t {
			match typ.width {
				32, 64 { [mir.AbiEightbyteClass.sse] }
				128 { [mir.AbiEightbyteClass.sse, .sseup] }
				else { []mir.AbiEightbyteClass{} }
			}
		}
		else {
			[]mir.AbiEightbyteClass{}
		}
	}

	return mir.AbiValueClass{
		mode:    .direct
		size:    size
		classes: classes
	}
}

fn sysv_abi_value_class(m mir.Module, typ_id int) mir.AbiValueClass {
	size := m.type_size(typ_id)
	if size <= 0 {
		return mir.AbiValueClass{
			mode: .direct
			size: size
		}
	}
	if sysv_aggregate_must_be_memory_before_classification(size) {
		return sysv_memory_value_class(size)
	}
	mut classes := []mir.AbiEightbyteClass{len: (size + 7) / 8, init: .no_class}
	mut visiting := map[int]bool{}
	if !sysv_classify_type_into(m, typ_id, 0, mut classes, mut visiting) {
		return sysv_memory_value_class(size)
	}
	classes = sysv_post_merge_classes(size, classes)
	if classes.len == 1 && classes[0] == .memory {
		return sysv_memory_value_class(size)
	}
	return mir.AbiValueClass{
		mode:    .direct
		size:    size
		classes: classes
	}
}

fn sysv_aggregate_must_be_memory_before_classification(size int) bool {
	// Current SSA aggregate types do not model a SysV vector aggregate larger
	// than two eightbytes, so larger struct/array aggregates cannot be passed
	// in registers and should not be classified element by element.
	return size > sysv_max_direct_aggregate_size
}

fn sysv_memory_value_class(size int) mir.AbiValueClass {
	return mir.AbiValueClass{
		mode:    .indirect
		size:    size
		classes: [.memory]
	}
}

fn sysv_classify_type_into(m mir.Module, typ_id int, byte_offset int, mut classes []mir.AbiEightbyteClass, mut visiting map[int]bool) bool {
	ssa_mod := m.ssa()
	if ssa_mod == unsafe { nil } || typ_id <= 0 || typ_id >= ssa_mod.type_store.types.len {
		return true
	}
	if visiting[typ_id] {
		return sysv_merge_class_range(mut classes, byte_offset, 8, .integer)
	}
	visiting[typ_id] = true
	typ := ssa_mod.type_store.types[typ_id]
	ok := match typ.kind {
		.int_t, .ptr_t, .func_t {
			sysv_merge_class_range(mut classes, byte_offset, m.type_size(typ_id), .integer)
		}
		.float_t {
			match typ.width {
				32, 64 {
					sysv_merge_class_range(mut classes, byte_offset, m.type_size(typ_id), .sse)
				}
				80 {
					false
				}
				128 {
					sysv_merge_class_range(mut classes, byte_offset, 8, .sse)
						&& sysv_merge_class_range(mut classes, byte_offset + 8, 8, .sseup)
				}
				else {
					false
				}
			}
		}
		.array_t {
			sysv_classify_array_into(m, typ.elem_type, typ.len, byte_offset, mut classes, mut
				visiting)
		}
		.struct_t {
			sysv_classify_struct_into(m, typ.fields, byte_offset, mut classes, mut visiting)
		}
		.void_t, .label_t, .metadata_t {
			true
		}
	}

	visiting.delete(typ_id)
	return ok
}

fn sysv_classify_array_into(m mir.Module, elem_type int, len int, byte_offset int, mut classes []mir.AbiEightbyteClass, mut visiting map[int]bool) bool {
	elem_size := m.type_size(elem_type)
	for i := 0; i < len; i++ {
		if !sysv_classify_type_into(m, elem_type, byte_offset + i * elem_size, mut classes, mut
			visiting) {
			return false
		}
	}
	return true
}

fn sysv_classify_struct_into(m mir.Module, fields []int, byte_offset int, mut classes []mir.AbiEightbyteClass, mut visiting map[int]bool) bool {
	mut field_offset := 0
	for field_typ in fields {
		align := m.type_align(field_typ)
		if align > 1 && field_offset % align != 0 {
			field_offset = (field_offset + align - 1) & ~(align - 1)
		}
		if !sysv_classify_type_into(m, field_typ, byte_offset + field_offset, mut classes, mut
			visiting) {
			return false
		}
		field_offset += m.type_size(field_typ)
	}
	return true
}

fn sysv_merge_class_range(mut classes []mir.AbiEightbyteClass, byte_offset int, size int, class mir.AbiEightbyteClass) bool {
	if size <= 0 {
		return true
	}
	start := byte_offset / 8
	end := (byte_offset + size + 7) / 8
	if start < 0 || end > classes.len {
		return false
	}
	for i := start; i < end; i++ {
		classes[i] = sysv_merge_eightbyte_class(classes[i], class)
		if classes[i] == .memory {
			return false
		}
	}
	return true
}

fn sysv_merge_eightbyte_class(a mir.AbiEightbyteClass, b mir.AbiEightbyteClass) mir.AbiEightbyteClass {
	if a == b {
		return a
	}
	if a == .no_class {
		return b
	}
	if b == .no_class {
		return a
	}
	if a == .memory || b == .memory {
		return .memory
	}
	if a == .integer || b == .integer {
		return .integer
	}
	return .sse
}

fn sysv_post_merge_classes(size int, input []mir.AbiEightbyteClass) []mir.AbiEightbyteClass {
	mut classes := input.clone()
	for class in classes {
		if class == .memory {
			return [.memory]
		}
	}
	if size > sysv_max_direct_aggregate_size {
		mut vector_like := classes.len > 0 && classes[0] == .sse
		for i := 1; i < classes.len; i++ {
			if classes[i] != .sseup {
				vector_like = false
				break
			}
		}
		if !vector_like {
			return [.memory]
		}
	}
	for i, class in classes {
		if class == .sseup && (i == 0 || classes[i - 1] !in [.sse, .sseup]) {
			classes[i] = .sse
		}
	}
	return classes
}

fn sysv_assign_value_layout(value_class mir.AbiValueClass, mut state SysVLocationState) mir.AbiValueLayout {
	if value_class.classes.len == 0 {
		return mir.AbiValueLayout{
			value_class: value_class
		}
	}
	if value_class.mode == .indirect || value_class.classes == [mir.AbiEightbyteClass.memory] {
		return sysv_indirect_pointer_layout(value_class, mut state)
	}

	mut needed_int := 0
	mut needed_sse := 0
	for class in value_class.classes {
		match class {
			.integer { needed_int++ }
			.sse { needed_sse++ }
			else {}
		}
	}
	if state.int_regs + needed_int > sysv_int_arg_reg_count
		|| state.sse_regs + needed_sse > sysv_sse_arg_reg_count {
		return sysv_stack_value_layout(value_class, mut state)
	}

	mut locs := []mir.AbiLocation{cap: value_class.classes.len}
	mut last_sse := -1
	for i, class in value_class.classes {
		offset := i * 8
		match class {
			.integer {
				locs << mir.AbiLocation{
					kind:   .int_reg
					index:  state.int_regs
					offset: offset
					class:  class
				}
				state.int_regs++
			}
			.sse {
				last_sse = state.sse_regs
				locs << mir.AbiLocation{
					kind:   .sse_reg
					index:  state.sse_regs
					offset: offset
					class:  class
				}
				state.sse_regs++
			}
			.sseup {
				locs << mir.AbiLocation{
					kind:   .sse_reg
					index:  if last_sse >= 0 { last_sse } else { state.sse_regs }
					offset: offset
					class:  class
				}
			}
			else {
				locs << mir.AbiLocation{
					kind:   .none
					index:  -1
					offset: offset
					class:  class
				}
			}
		}
	}
	return mir.AbiValueLayout{
		value_class: value_class
		locs:        locs
	}
}

fn sysv_indirect_pointer_layout(value_class mir.AbiValueClass, mut state SysVLocationState) mir.AbiValueLayout {
	mut locs := []mir.AbiLocation{cap: 1}
	if state.int_regs < sysv_int_arg_reg_count {
		locs << mir.AbiLocation{
			kind:   .int_reg
			index:  state.int_regs
			offset: 0
			class:  .integer
		}
		state.int_regs++
	} else {
		locs << mir.AbiLocation{
			kind:   .stack
			index:  state.stack_slots
			offset: 0
			class:  .integer
		}
		state.stack_slots++
	}
	return mir.AbiValueLayout{
		value_class: value_class
		locs:        locs
	}
}

fn sysv_stack_value_layout(value_class mir.AbiValueClass, mut state SysVLocationState) mir.AbiValueLayout {
	slots := if value_class.size > 0 { (value_class.size + 7) / 8 } else { value_class.classes.len }
	mut locs := []mir.AbiLocation{cap: slots}
	for i := 0; i < slots; i++ {
		class := if i < value_class.classes.len {
			value_class.classes[i]
		} else {
			mir.AbiEightbyteClass.memory
		}
		locs << mir.AbiLocation{
			kind:   .stack
			index:  state.stack_slots
			offset: i * 8
			class:  class
		}
		state.stack_slots++
	}
	return mir.AbiValueLayout{
		value_class: value_class
		locs:        locs
	}
}

fn fallback_arg_type(m mir.Module, arg_id int) int {
	if arg_id < 0 || arg_id >= m.values.len {
		return 0
	}
	if logical_typ := logical_arg_type_from_value(m, arg_id, 0) {
		return logical_typ
	}
	return m.values[arg_id].typ
}

fn logical_arg_type_from_value(m mir.Module, val_id int, depth int) ?int {
	if depth > 8 || val_id < 0 || val_id >= m.values.len {
		return none
	}
	val := m.values[val_id]
	if val.kind != .instruction {
		return none
	}
	instr := m.instrs[val.index]
	match instr.op {
		.alloca {
			if val.typ <= 0 || val.typ >= m.type_store.types.len {
				return none
			}
			typ := m.type_store.types[val.typ]
			if typ.kind != .ptr_t || typ.elem_type <= 0 || typ.elem_type >= m.type_store.types.len {
				return none
			}
			elem_typ := m.type_store.types[typ.elem_type]
			if elem_typ.kind in [.struct_t, .array_t] {
				return typ.elem_type
			}
		}
		.bitcast {
			if instr.operands.len > 0 {
				return logical_arg_type_from_value(m, instr.operands[0], depth + 1)
			}
		}
		.assign {
			if instr.operands.len > 1 {
				return logical_arg_type_from_value(m, instr.operands[1], depth + 1)
			}
		}
		else {}
	}

	return none
}

fn lower_calls(mut m mir.Module, arch pref.Arch, x64_abi X64Abi, fn_by_name map[string]int) {
	if arch !in [.arm64, .x64] {
		return
	}
	is_x64 := arch == .x64

	for i := 0; i < m.instrs.len; i++ {
		mut instr := &m.instrs[i]
		if instr.op !in [.call, .call_indirect, .call_sret] || instr.operands.len == 0 {
			continue
		}
		ret_typ, sig_param_types := call_signature(m, instr, fn_by_name)
		ret_class := if is_x64 {
			abi_value_class(m, ret_typ, arch, x64_abi)
		} else {
			mir.AbiValueClass{}
		}
		ret_indirect := if is_x64 {
			abi_class_is_indirect(ret_class, m, ret_typ, arch, x64_abi)
		} else {
			needs_indirect(m, ret_typ, arch, x64_abi)
		}
		num_args := instr.operands.len - 1
		instr.abi_arg_class = []mir.AbiArgClass{len: num_args, init: .in_reg}
		if is_x64 {
			instr.abi_arg_classes = []mir.AbiValueClass{len: num_args}
			instr.abi_arg_layouts = []mir.AbiValueLayout{len: num_args}
		}
		mut arg_loc_state := SysVLocationState{}
		if arch == .x64 && x64_abi == .sysv && ret_indirect {
			arg_loc_state.int_regs = 1
		}
		for arg_idx := 0; arg_idx < num_args; arg_idx++ {
			mut arg_typ := 0
			if arg_idx < sig_param_types.len && sig_param_types[arg_idx] > 0 {
				arg_typ = sig_param_types[arg_idx]
			} else {
				arg_id := instr.operands[arg_idx + 1]
				arg_typ = fallback_arg_type(m, arg_id)
			}
			if is_x64 {
				arg_class := abi_value_class(m, arg_typ, arch, x64_abi)
				instr.abi_arg_classes[arg_idx] = arg_class
				if x64_abi == .sysv {
					instr.abi_arg_layouts[arg_idx] = sysv_assign_value_layout(arg_class, mut
						arg_loc_state)
				}
				if abi_class_is_indirect(arg_class, m, arg_typ, arch, x64_abi) {
					instr.abi_arg_class[arg_idx] = .indirect
				}
			} else if needs_indirect(m, arg_typ, arch, x64_abi) {
				instr.abi_arg_class[arg_idx] = .indirect
			}
		}

		instr.abi_ret_class = ret_class
		instr.abi_ret_indirect = ret_indirect
		// Lower ABI-indirect returns to call_sret for backend consumption.
		if instr.abi_ret_indirect {
			instr.op = .call_sret
		}
	}
}

fn call_signature(m mir.Module, instr &mir.Instruction, fn_by_name map[string]int) (int, []int) {
	mut ret_typ := instr.typ
	mut param_types := []int{}
	if instr.operands.len == 0 {
		return ret_typ, param_types
	}

	if instr.op in [.call, .call_sret] {
		callee_id := instr.operands[0]
		if callee_id >= 0 && callee_id < m.values.len {
			callee_name := m.values[callee_id].name
			if callee_name != '' && callee_name in fn_by_name {
				fn_idx := fn_by_name[callee_name]
				if fn_idx >= 0 && fn_idx < m.funcs.len {
					callee := m.funcs[fn_idx]
					ret_typ = callee.typ
					param_types = []int{len: callee.params.len}
					for i, pid in callee.params {
						if pid >= 0 && pid < m.values.len {
							param_types[i] = m.values[pid].typ
						}
					}
				}
			}
		}
	} else if instr.op == .call_indirect {
		callee_id := instr.operands[0]
		if callee_id >= 0 && callee_id < m.values.len {
			fn_ptr_typ_id := m.values[callee_id].typ
			if fn_ptr_typ_id > 0 && fn_ptr_typ_id < m.type_store.types.len {
				fn_ptr_typ := m.type_store.types[fn_ptr_typ_id]
				if fn_ptr_typ.kind == .ptr_t && fn_ptr_typ.elem_type > 0
					&& fn_ptr_typ.elem_type < m.type_store.types.len {
					fn_typ := m.type_store.types[fn_ptr_typ.elem_type]
					if fn_typ.kind == .func_t {
						ret_typ = fn_typ.ret_type
						param_types = fn_typ.params.clone()
					}
				} else if fn_ptr_typ.kind == .func_t {
					// Function pointer extracted from struct field via extractvalue
					// has func_t type directly (not wrapped in ptr_t).
					ret_typ = fn_ptr_typ.ret_type
					param_types = fn_ptr_typ.params.clone()
				}
			}
		}
	}

	// Fallback to call operand types when we could not resolve a signature.
	if param_types.len == 0 {
		num_args := if instr.operands.len > 0 { instr.operands.len - 1 } else { 0 }
		param_types = []int{len: num_args}
		for i := 0; i < num_args; i++ {
			arg_id := instr.operands[i + 1]
			param_types[i] = fallback_arg_type(m, arg_id)
		}
	}
	return ret_typ, param_types
}
