// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module abi

import v2.mir
import v2.pref

// lower annotates MIR with ABI classification metadata.
// Current scope is intentionally conservative: it classifies which arguments
// and return values must be passed indirectly for each function.
pub fn lower(mut m mir.Module, arch pref.Arch) {
	mut fn_by_name := map[string]int{}
	for i := 0; i < m.funcs.len; i++ {
		mut f := &m.funcs[i]
		fn_by_name[f.name] = i
		f.abi_param_class = []mir.AbiArgClass{len: f.params.len, init: .in_reg}
		for pi, param_id in f.params {
			if param_id < 0 || param_id >= m.values.len {
				continue
			}
			param_typ := m.values[param_id].typ
			if needs_indirect(m, param_typ, arch) {
				f.abi_param_class[pi] = .indirect
			}
		}
		f.abi_ret_indirect = needs_indirect(m, f.typ, arch)
	}

	lower_calls(mut m, arch, fn_by_name)
}

fn needs_indirect(m mir.Module, typ_id int, arch pref.Arch) bool {
	ssa_mod := m.ssa()
	if ssa_mod == unsafe { nil } || typ_id <= 0 || typ_id >= ssa_mod.type_store.types.len {
		return false
	}
	typ := ssa_mod.type_store.types[typ_id]
	if typ.kind != .struct_t {
		return false
	}
	size := m.type_size(typ_id)
	return match arch {
		.arm64 { size > 16 }
		.x64 { size > 16 }
		else { size > 16 }
	}
}

fn lower_calls(mut m mir.Module, arch pref.Arch, fn_by_name map[string]int) {
	if arch !in [.arm64, .x64] {
		return
	}

	for i := 0; i < m.instrs.len; i++ {
		mut instr := &m.instrs[i]
		if instr.op !in [.call, .call_indirect, .call_sret] || instr.operands.len == 0 {
			continue
		}
		ret_typ, sig_param_types := call_signature(m, instr, fn_by_name)
		num_args := instr.operands.len - 1
		instr.abi_arg_class = []mir.AbiArgClass{len: num_args, init: .in_reg}
		for arg_idx := 0; arg_idx < num_args; arg_idx++ {
			mut arg_typ := 0
			if arg_idx < sig_param_types.len && sig_param_types[arg_idx] > 0 {
				arg_typ = sig_param_types[arg_idx]
			} else {
				arg_id := instr.operands[arg_idx + 1]
				if arg_id >= 0 && arg_id < m.values.len {
					arg_typ = m.values[arg_id].typ
				}
			}
			if needs_indirect(m, arg_typ, arch) {
				instr.abi_arg_class[arg_idx] = .indirect
			}
		}

		instr.abi_ret_indirect = needs_indirect(m, ret_typ, arch)
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
			if arg_id >= 0 && arg_id < m.values.len {
				param_types[i] = m.values[arg_id].typ
			}
		}
	}
	return ret_typ, param_types
}
