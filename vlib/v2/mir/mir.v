// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module mir

import v2.ssa
import v2.types

pub enum ValueKind {
	unknown
	constant
	argument
	global
	instruction
	basic_block
	string_literal
	c_string_literal
	func_ref
}

pub enum AbiArgClass {
	in_reg
	indirect
}

pub struct Value {
pub:
	id    int
	typ   ssa.TypeID
	index int
pub mut:
	kind ValueKind
	name string
	uses []ssa.ValueID
}

pub struct Instruction {
pub mut:
	op               ssa.OpCode
	operands         []ssa.ValueID
	selected_op      string
	abi_ret_indirect bool
	abi_arg_class    []AbiArgClass
pub:
	typ       ssa.TypeID
	block     int
	src_index int
}

pub struct BasicBlock {
pub:
	id     int
	val_id int
	name   string
	parent int
pub mut:
	instrs []ssa.ValueID
	preds  []ssa.BlockID
	succs  []ssa.BlockID
}

pub struct Function {
pub:
	id          int
	name        string
	typ         ssa.TypeID
	linkage     ssa.Linkage
	call_conv   ssa.CallConv
	is_c_extern bool // C-language extern function (provided by libc/system libraries)
pub mut:
	blocks           []ssa.BlockID
	params           []ssa.ValueID
	abi_ret_indirect bool
	abi_param_class  []AbiArgClass
}

@[heap]
pub struct Module {
pub:
	name    string
	target  ssa.TargetData
	ssa_mod &ssa.Module        = unsafe { nil }
	env     &types.Environment = unsafe { nil }
pub mut:
	type_store ssa.TypeStore
	values     []Value
	instrs     []Instruction
	blocks     []BasicBlock
	funcs      []Function
	globals    []ssa.GlobalVar
}

fn clone_value_ids(values []ssa.ValueID) []ssa.ValueID {
	return values.clone()
}

fn clone_block_ids(blocks []ssa.BlockID) []ssa.BlockID {
	return blocks.clone()
}

fn opcode_label(op ssa.OpCode) string {
	return int(op).str()
}

pub fn lower_from_ssa(ssa_mod &ssa.Module) Module {
	mut mod := Module{
		name:       ssa_mod.name
		target:     ssa_mod.target
		ssa_mod:    ssa_mod
		env:        ssa_mod.env
		type_store: ssa_mod.type_store
		values:     []Value{len: ssa_mod.values.len}
		instrs:     []Instruction{len: ssa_mod.instrs.len}
		blocks:     []BasicBlock{len: ssa_mod.blocks.len}
		funcs:      []Function{len: ssa_mod.funcs.len}
		globals:    ssa_mod.globals.clone()
	}

	for i, val in ssa_mod.values {
		mod.values[i] = Value{
			id:    val.id
			typ:   val.typ
			index: val.index
			kind:  value_kind_from_ssa(val.kind)
			name:  val.name
			uses:  clone_value_ids(val.uses)
		}
	}

	for i, instr in ssa_mod.instrs {
		mod.instrs[i] = Instruction{
			op:               instr.op
			operands:         clone_value_ids(instr.operands)
			selected_op:      opcode_label(instr.op)
			abi_ret_indirect: false
			abi_arg_class:    []AbiArgClass{}
			typ:              instr.typ
			block:            instr.block
			src_index:        i
		}
	}

	for i, blk in ssa_mod.blocks {
		mod.blocks[i] = BasicBlock{
			id:     blk.id
			val_id: blk.val_id
			name:   blk.name
			parent: blk.parent
			instrs: clone_value_ids(blk.instrs)
			preds:  clone_block_ids(blk.preds)
			succs:  clone_block_ids(blk.succs)
		}
	}

	for i, f in ssa_mod.funcs {
		mod.funcs[i] = Function{
			id:               f.id
			name:             f.name
			typ:              f.typ
			linkage:          f.linkage
			call_conv:        f.call_conv
			is_c_extern:      f.is_c_extern
			blocks:           clone_block_ids(f.blocks)
			params:           clone_value_ids(f.params)
			abi_ret_indirect: false
			abi_param_class:  []AbiArgClass{len: f.params.len, init: .in_reg}
		}
	}

	return mod
}

pub fn (m &Module) ssa() &ssa.Module {
	return m.ssa_mod
}

pub fn (m &Module) type_size(typ_id ssa.TypeID) int {
	if m.ssa_mod == unsafe { nil } || typ_id < 0 || typ_id >= m.ssa_mod.type_store.types.len {
		return 0
	}
	if typ_id == 0 {
		return 0
	}
	typ := m.ssa_mod.type_store.types[typ_id]
	match typ.kind {
		.void_t {
			return 0
		}
		.int_t {
			return if typ.width > 0 { (typ.width + 7) / 8 } else { 8 }
		}
		.float_t {
			return if typ.width > 0 { (typ.width + 7) / 8 } else { 8 }
		}
		.ptr_t {
			return 8
		}
		.array_t {
			elem_size := m.type_size(typ.elem_type)
			return typ.len * elem_size
		}
		.struct_t {
			mut total := 0
			mut max_align := 1
			for field_typ in typ.fields {
				align := m.type_align(field_typ)
				if align > max_align {
					max_align = align
				}
				if align > 1 && total % align != 0 {
					total = (total + align - 1) & ~(align - 1)
				}
				total += m.type_size(field_typ)
			}
			// Align struct size to its largest field alignment
			if max_align > 1 && total % max_align != 0 {
				total = (total + max_align - 1) & ~(max_align - 1)
			}
			return if total > 0 { total } else { 8 }
		}
		.func_t {
			return 8
		}
		.label_t {
			return 0
		}
		.metadata_t {
			return 0
		}
	}
}

pub fn (m &Module) type_align(typ_id ssa.TypeID) int {
	if m.ssa_mod != unsafe { nil } && typ_id > 0 && typ_id < m.ssa_mod.type_store.types.len {
		typ := m.ssa_mod.type_store.types[typ_id]
		if typ.kind == .array_t {
			return m.type_align(typ.elem_type)
		}
	}
	size := m.type_size(typ_id)
	if size >= 8 {
		return 8
	}
	if size >= 4 {
		return 4
	}
	if size >= 2 {
		return 2
	}
	return 1
}

fn value_kind_from_ssa(kind ssa.ValueKind) ValueKind {
	return match kind {
		.unknown { .unknown }
		.constant { .constant }
		.argument { .argument }
		.global { .global }
		.instruction { .instruction }
		.basic_block { .basic_block }
		.string_literal { .string_literal }
		.c_string_literal { .c_string_literal }
		.func_ref { .func_ref }
	}
}
