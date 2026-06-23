module mir

import v3.ssa

// TargetArch lists target arch values used by mir.
pub enum TargetArch {
	unknown
	arm64
}

// AbiKind lists abi kind values used by mir.
pub enum AbiKind {
	unknown
	aapcs64
}

// Target represents target data used by mir.
pub struct Target {
pub:
	arch         TargetArch
	abi          AbiKind
	pointer_size int
	int_arg_regs int
	ret_regs     int
	stack_align  int
}

// AbiLocationKind lists abi location kind values used by mir.
pub enum AbiLocationKind {
	none
	register
	stack
	indirect
}

// AbiLocation represents abi location data used by mir.
pub struct AbiLocation {
pub:
	kind           AbiLocationKind
	register_index int
	register_count int
	stack_offset   int
	size           int
}

// FunctionAbi represents function abi data used by mir.
pub struct FunctionAbi {
pub:
	params          []AbiLocation
	ret             AbiLocation
	stack_args_size int
	stack_align     int
}

// Value represents value data used by mir.
pub struct Value {
pub mut:
	id    int
	kind  ssa.ValueKind
	typ   ssa.TypeID
	name  string
	index int
	uses  []ssa.ValueID
}

// Instruction represents instruction data used by mir.
pub struct Instruction {
pub mut:
	op       ssa.OpCode
	operands []ssa.ValueID
	typ      ssa.TypeID
	block    int
}

// BasicBlock represents basic block data used by mir.
pub struct BasicBlock {
pub mut:
	id     int
	name   string
	parent int
	instrs []ssa.ValueID
	preds  []ssa.BlockID
	succs  []ssa.BlockID
}

// Function represents function data used by mir.
pub struct Function {
pub mut:
	id          int
	name        string
	typ         ssa.TypeID
	blocks      []ssa.BlockID
	params      []ssa.ValueID
	is_c_extern bool
	abi         FunctionAbi
}

// Module represents module data used by mir.
@[heap]
pub struct Module {
pub mut:
	target     Target
	type_store ssa.TypeStore
	values     []Value
	instrs     []Instruction
	blocks     []BasicBlock
	funcs      []Function
	globals    []ssa.GlobalVar
}

// default_target returns a conservative target descriptor for target-neutral MIR.
pub fn default_target() Target {
	return Target{
		arch:         .unknown
		abi:          .unknown
		pointer_size: 8
		stack_align:  8
	}
}

// arm64_target returns the target descriptor used by the native ARM64 pipeline.
pub fn arm64_target() Target {
	return Target{
		arch:         .arm64
		abi:          .aapcs64
		pointer_size: 8
		int_arg_regs: 8
		ret_regs:     8
		stack_align:  16
	}
}

// lower_from_ssa lowers an SSA module into target-neutral MIR.
pub fn lower_from_ssa(m &ssa.Module) Module {
	return lower_from_ssa_for_target(m, default_target())
}

// lower_from_ssa_for_target lowers an SSA module into MIR with target ABI metadata.
pub fn lower_from_ssa_for_target(m &ssa.Module, target Target) Module {
	mut mod := Module{
		target:     target
		type_store: m.type_store
		values:     []Value{len: m.values.len}
		instrs:     []Instruction{len: m.instrs.len}
		blocks:     []BasicBlock{len: m.blocks.len}
		funcs:      []Function{len: m.funcs.len}
		globals:    m.globals
	}

	for i, val in m.values {
		mod.values[i] = Value{
			id:    val.id
			kind:  val.kind
			typ:   val.typ
			name:  val.name
			index: val.index
			uses:  val.uses
		}
	}

	for i, instr in m.instrs {
		mod.instrs[i] = Instruction{
			op:       instr.op
			operands: instr.operands
			typ:      instr.typ
			block:    instr.block
		}
	}

	for i, blk in m.blocks {
		mod.blocks[i] = BasicBlock{
			id:     blk.id
			name:   blk.name
			parent: blk.parent
			instrs: blk.instrs
			preds:  blk.preds
			succs:  blk.succs
		}
	}

	for i, f in m.funcs {
		mod.funcs[i] = Function{
			id:          f.id
			name:        f.name
			typ:         f.typ
			blocks:      f.blocks
			params:      f.params
			is_c_extern: f.is_c_extern
		}
	}

	for i, f in mod.funcs {
		mut func := f
		func.abi = mod.build_function_abi(func)
		mod.funcs[i] = func
	}

	return mod
}

// type_size returns type size data for Module.
pub fn (m &Module) type_size(typ_id ssa.TypeID) int {
	return m.type_size_inner(typ_id, 0)
}

// type_size_inner returns type size inner data for Module.
fn (m &Module) type_size_inner(typ_id ssa.TypeID, depth int) int {
	if typ_id <= 0 || typ_id >= m.type_store.types.len {
		return 0
	}
	if depth > 32 {
		return 8
	}
	typ := m.type_store.types[typ_id]
	if typ.width > 0 {
		return (typ.width + 7) / 8
	}
	if typ.elem_type > 0 && typ.fields.len == 0 {
		return 8
	}
	if typ.fields.len == 0 {
		if typ.params.len > 0 || typ.ret_type > 0 {
			return 8
		}
		return 0
	}
	if typ.fields.len > 256 {
		return 8
	}
	mut offset := 0
	mut max_align := 1
	for i in 0 .. typ.fields.len {
		field_typ := typ.fields[i]
		align := m.type_align_inner(field_typ, depth + 1)
		if align > max_align {
			max_align = align
		}
		if align > 1 && offset % align != 0 {
			offset = (offset + align - 1) & ~(align - 1)
		}
		offset += m.type_size_inner(field_typ, depth + 1)
	}
	total := if max_align > 1 && offset % max_align != 0 {
		(offset + max_align - 1) & ~(max_align - 1)
	} else {
		offset
	}
	if total > 0 {
		return total
	}
	return 8
}

// type_align returns type align data for Module.
pub fn (m &Module) type_align(typ_id ssa.TypeID) int {
	return m.type_align_inner(typ_id, 0)
}

// type_align_inner returns type align inner data for Module.
fn (m &Module) type_align_inner(typ_id ssa.TypeID, depth int) int {
	if typ_id <= 0 || typ_id >= m.type_store.types.len {
		return 1
	}
	if depth > 32 {
		return 8
	}
	typ := m.type_store.types[typ_id]
	if typ.width > 0 {
		size := (typ.width + 7) / 8
		if size >= 8 {
			return 8
		}
		if size >= 4 {
			return 4
		}
		return 1
	}
	if typ.elem_type > 0 && typ.fields.len == 0 {
		return 8
	}
	if typ.fields.len > 0 {
		if typ.fields.len > 256 {
			return 8
		}
		mut max_align := 1
		for i in 0 .. typ.fields.len {
			field_typ := typ.fields[i]
			a := m.type_align_inner(field_typ, depth + 1)
			if a > max_align {
				max_align = a
			}
		}
		return max_align
	}
	if typ.params.len > 0 || typ.ret_type > 0 {
		return 8
	}
	size := m.type_size_inner(typ_id, depth + 1)
	if size >= 8 {
		return 8
	}
	if size >= 4 {
		return 4
	}
	return 1
}

// build_function_abi builds function abi data for mir.
fn (m &Module) build_function_abi(f Function) FunctionAbi {
	mut params := []AbiLocation{}
	mut next_reg := 0
	mut next_stack := 0
	word_size := m.target_word_size()
	for pid in f.params {
		size := m.value_size(pid)
		n_words := words_for(size, word_size)
		if m.target.int_arg_regs > 0 && next_reg + n_words <= m.target.int_arg_regs {
			params << AbiLocation{
				kind:           .register
				register_index: next_reg
				register_count: n_words
				size:           size
			}
			next_reg += n_words
			continue
		}
		stack_offset := align_to(next_stack, word_size)
		params << AbiLocation{
			kind:           .stack
			register_count: n_words
			stack_offset:   stack_offset
			size:           size
		}
		next_stack = stack_offset + align_to(size, word_size)
	}

	ret_size := m.type_size(f.typ)
	ret_words := words_for(ret_size, word_size)
	ret := if ret_size == 0 {
		AbiLocation{
			kind: .none
		}
	} else if m.target.ret_regs > 0 && ret_words <= m.target.ret_regs {
		AbiLocation{
			kind:           .register
			register_index: 0
			register_count: ret_words
			size:           ret_size
		}
	} else {
		AbiLocation{
			kind:           .indirect
			register_index: 0
			register_count: 1
			size:           ret_size
		}
	}
	return FunctionAbi{
		params:          params
		ret:             ret
		stack_args_size: align_to(next_stack, m.target_stack_align())
		stack_align:     m.target_stack_align()
	}
}

// value_size returns value size data for Module.
fn (m &Module) value_size(val_id ssa.ValueID) int {
	if val_id <= 0 || val_id >= m.values.len {
		return m.target_word_size()
	}
	size := m.type_size(m.values[val_id].typ)
	if size > 0 {
		return size
	}
	return m.target_word_size()
}

// target_word_size supports target word size handling for Module.
fn (m &Module) target_word_size() int {
	if m.target.pointer_size > 0 {
		return m.target.pointer_size
	}
	return 8
}

// target_stack_align supports target stack align handling for Module.
fn (m &Module) target_stack_align() int {
	if m.target.stack_align > 0 {
		return m.target.stack_align
	}
	return m.target_word_size()
}

// words_for supports words for handling for mir.
fn words_for(size int, word_size int) int {
	if size <= 0 {
		return 0
	}
	return (size + word_size - 1) / word_size
}

// align_to supports align to handling for mir.
fn align_to(value int, alignment int) int {
	if alignment <= 1 {
		return value
	}
	return (value + alignment - 1) & ~(alignment - 1)
}
