module x64

import v2.mir
import v2.ssa

fn contains_bytes(haystack []u8, needle []u8) bool {
	if needle.len == 0 {
		return true
	}
	if needle.len > haystack.len {
		return false
	}
	for i in 0 .. haystack.len - needle.len + 1 {
		if haystack[i..i + needle.len] == needle {
			return true
		}
	}
	return false
}

fn test_x64_abi_int_argument_registers() {
	assert X64Abi.sysv.int_arg_regs() == [int(rdi), int(rsi), int(rdx), int(rcx), int(r8), int(r9)]
	assert X64Abi.windows.int_arg_regs() == [int(rcx), int(rdx), int(r8), int(r9)]
	assert X64Abi.sysv.int_arg_reg_at(0) == int(rdi)
	assert X64Abi.sysv.int_arg_reg_at(5) == int(r9)
	assert X64Abi.sysv.int_arg_reg_at(6) == x64_no_arg_reg
}

fn test_x64_abi_float_argument_registers() {
	assert X64Abi.sysv.float_arg_regs() == [0, 1, 2, 3, 4, 5, 6, 7]
	assert X64Abi.windows.float_arg_regs() == [0, 1, 2, 3]
	assert X64Abi.sysv.float_arg_reg_at(0) == 0
	assert X64Abi.sysv.float_arg_reg_at(7) == 7
	assert X64Abi.sysv.float_arg_reg_at(8) == x64_no_arg_reg
}

fn test_x64_windows_argument_registers_are_position_based() {
	assert X64Abi.windows.uses_positional_arg_regs()
	assert X64Abi.windows.int_arg_reg_for_position(0) == int(rcx)
	assert X64Abi.windows.float_arg_reg_for_position(1) == 1
	assert X64Abi.windows.int_arg_reg_for_position(2) == int(r8)
	assert X64Abi.windows.float_arg_reg_for_position(3) == 3
	assert X64Abi.windows.int_arg_reg_for_position(4) == x64_no_arg_reg
	assert X64Abi.windows.float_arg_reg_for_position(4) == x64_no_arg_reg
}

fn test_x64_sysv_argument_helpers_keep_compact_register_order() {
	assert !X64Abi.sysv.uses_positional_arg_regs()
	assert X64Abi.sysv.int_arg_reg_at(0) == int(rdi)
	assert X64Abi.sysv.int_arg_reg_at(1) == int(rsi)
	assert X64Abi.sysv.float_arg_reg_at(0) == 0
	assert X64Abi.sysv.float_arg_reg_at(1) == 1
}

fn test_x64_abi_shadow_space_size() {
	assert X64Abi.sysv.shadow_space_size() == 0
	assert X64Abi.windows.shadow_space_size() == 32
	assert X64Abi.windows.call_stack_arg_offset(4) == 32
	assert X64Abi.windows.call_stack_arg_offset(5) == 40
	assert X64Abi.windows.stack_arg_offset(4) == 48
	assert X64Abi.windows.stack_arg_offset(5) == 56
	assert X64Abi.windows.call_frame_size(0) == 32
	assert X64Abi.windows.call_frame_size(1) == 48
}

fn test_x64_abi_sret_registers_are_explicit() {
	assert X64Abi.sysv.sret_reg() == rdi
	assert X64Abi.sysv.sret_arg_regs() == [int(rsi), int(rdx), int(rcx), int(r8), int(r9)]
	assert X64Abi.windows.sret_reg() == rcx
	assert X64Abi.windows.sret_arg_regs() == [int(rdx), int(r8), int(r9)]
}

fn test_x64_gen_defaults_to_sysv_abi() {
	mut mod := mir.Module{}
	gen := Gen.new(&mod)
	assert gen.abi == .sysv

	coff_gen := Gen.new_with_format(&mod, .coff)
	assert coff_gen.abi == .sysv

	windows_gen := Gen.new_with_format_and_abi(&mod, .coff, .windows)
	assert windows_gen.abi == .windows
}

fn test_x64_windows_stack_arg_mask_is_position_based() {
	mut mod := new_x64_abi_test_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .coff, .windows)
	instr := mir.Instruction{
		operands: [ssa.ValueID(0), 1, 2, 3, 4, 5]
	}

	assert gen.call_stack_arg_mask(instr, gen.abi.int_arg_regs().len, 0) == [false, false, false,
		false, true]
	assert gen.call_stack_arg_mask(instr, gen.abi.sret_arg_regs().len, 1) == [false, false, false,
		true, true]
}

fn test_x64_sysv_stack_arg_mask_keeps_separate_int_and_float_counters() {
	mut mod := new_x64_abi_test_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .elf, .sysv)
	instr := mir.Instruction{
		operands: [ssa.ValueID(0), 1, 2, 3, 4, 5]
	}

	assert gen.call_stack_arg_mask(instr, gen.abi.int_arg_regs().len, 0) == [false, false, false,
		false, false]
}

fn test_x64_windows_call_frame_reserves_shadow_space() {
	mut mod := new_x64_abi_test_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .coff, .windows)

	cleanup := gen.emit_windows_call_frame(1)
	assert cleanup == 48
	gen.cleanup_windows_call_frame(cleanup)
	assert gen.coff.text_data == [u8(0x48), 0x83, 0xec, 0x30, 0x48, 0x83, 0xc4, 0x30]
}

fn test_x64_windows_stack_arg_store_starts_after_shadow_space() {
	mut mod := new_x64_abi_test_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .coff, .windows)
	instr := mir.Instruction{
		operands: [ssa.ValueID(0), 1, 2, 3, 4, 5]
	}

	gen.store_windows_call_stack_arg(5, 4, 4, instr)
	assert gen.coff.text_data == [u8(0xb8), 0x05, 0, 0, 0, 0x48, 0x89, 0x44, 0x24, 0x20]
}

fn test_x64_windows_does_not_emit_sysv_sse_arg_count() {
	mut mod := new_x64_abi_test_module()
	mut win_gen := Gen.new_with_format_and_abi(&mod, .coff, .windows)
	win_gen.emit_sse_arg_count(2)
	assert win_gen.coff.text_data.len == 0

	mut sysv_gen := Gen.new_with_format_and_abi(&mod, .elf, .sysv)
	sysv_gen.emit_sse_arg_count(2)
	assert sysv_gen.elf.text_data == [u8(0xb8), 0x02, 0, 0, 0]
}

fn test_x64_windows_codegen_mixed_call_uses_positional_registers() {
	mut mod := new_x64_abi_call_module(false)
	mut gen := Gen.new_with_format_and_abi(&mod, .coff, .windows)
	gen.gen()
	code := gen.coff.text_data

	assert contains_bytes(code, [u8(0x48), 0x83, 0xec, 0x30])
	assert contains_bytes(code, [u8(0xb9), 0x01, 0, 0, 0]) // arg0 int -> RCX
	assert contains_bytes(code, [u8(0xf2), 0x0f, 0x10, 0x4d]) // arg1 float -> XMM1
	assert contains_bytes(code, [u8(0x41), 0xb8, 0x03, 0, 0, 0]) // arg2 int -> R8
	assert contains_bytes(code, [u8(0xf2), 0x0f, 0x10, 0x5d]) // arg3 float -> XMM3
	assert contains_bytes(code, [u8(0xb8), 0x05, 0, 0, 0, 0x48, 0x89, 0x44, 0x24, 0x20])
	assert !contains_bytes(code, [u8(0xb8), 0x02, 0, 0, 0]) // no SysV AL SSE count
}

fn test_x64_windows_codegen_call_sret_shifts_user_arg_registers() {
	mut mod := new_x64_abi_call_module(true)
	mut gen := Gen.new_with_format_and_abi(&mod, .coff, .windows)
	gen.gen()
	code := gen.coff.text_data

	assert contains_bytes(code, [u8(0x48), 0x89, 0xc1]) // hidden sret pointer -> RCX
	assert contains_bytes(code, [u8(0xba), 0x01, 0, 0, 0]) // first user int -> RDX
	assert contains_bytes(code, [u8(0xf2), 0x0f, 0x10, 0x55]) // second user float -> XMM2
}

fn test_x64_windows_codegen_callee_stack_arg_loads_from_rbp_48() {
	mut mod := new_x64_abi_callee_stack_arg_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .coff, .windows)
	gen.gen()
	code := gen.coff.text_data

	assert contains_bytes(code, [u8(0x48), 0x8b, 0x45, 0x30]) // mov rax, [rbp+48]
}

fn test_x64_windows_codegen_large_aggregate_arg_uses_indirect_pointer() {
	mut mod := new_x64_abi_indirect_aggregate_call_module(true)
	mut gen := Gen.new_with_format_and_abi(&mod, .coff, .windows)
	gen.gen()
	code := gen.coff.text_data

	assert contains_bytes(code, [u8(0x48), 0x83, 0xec, 0x20]) // shadow space
	assert contains_bytes(code, [u8(0x48), 0x8d, 0x45]) // lea rax, [rbp+disp8]
	assert contains_bytes(code, [u8(0x48), 0x89, 0xc1]) // indirect arg pointer -> RCX
}

fn test_x64_windows_codegen_unmarked_large_aggregate_arg_uses_indirect_pointer() {
	mut mod := new_x64_abi_indirect_aggregate_call_module(false)
	mut gen := Gen.new_with_format_and_abi(&mod, .coff, .windows)
	gen.gen()
	code := gen.coff.text_data

	assert contains_bytes(code, [u8(0x48), 0x83, 0xec, 0x20]) // shadow space
	assert contains_bytes(code, [u8(0x48), 0x8d, 0x45]) // lea rax, [rbp+disp8]
	assert contains_bytes(code, [u8(0x48), 0x89, 0xc1]) // indirect arg pointer -> RCX
}

fn test_x64_windows_codegen_struct8_arg_remains_by_value() {
	mut mod := new_x64_abi_struct8_call_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .coff, .windows)
	gen.gen()
	code := gen.coff.text_data

	assert contains_bytes(code, [u8(0x49), 0x8b, 0x0a]) // mov rcx, [r10]
	assert !contains_bytes(code, [u8(0x48), 0x89, 0xc1]) // no pointer move into RCX
}

fn test_x64_windows_codegen_stack_large_aggregate_arg_uses_indirect_pointer() {
	mut mod := new_x64_abi_stack_indirect_aggregate_call_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .coff, .windows)
	gen.gen()
	code := gen.coff.text_data

	assert contains_bytes(code, [u8(0x48), 0x83, 0xec, 0x30]) // shadow space + one stack slot
	assert contains_bytes(code, [u8(0x48), 0x8d, 0x45]) // lea rax, [rbp+disp8]
	assert contains_bytes(code, [u8(0x48), 0x89, 0x44, 0x24, 0x20]) // pointer -> [rsp+32]
}

fn new_x64_abi_test_module() mir.Module {
	mut ts := ssa.TypeStore.new()
	int_t := ts.get_int(64)
	float_t := ts.get_float(64)
	return mir.Module{
		type_store: unsafe { *ts }
		values:     [
			mir.Value{
				id:    0
				typ:   int_t
				kind:  .func_ref
				name:  'callee'
				index: 0
			},
			mir.Value{
				id:    1
				typ:   int_t
				kind:  .constant
				name:  '1'
				index: 1
			},
			mir.Value{
				id:    2
				typ:   float_t
				kind:  .constant
				name:  '2'
				index: 2
			},
			mir.Value{
				id:    3
				typ:   int_t
				kind:  .constant
				name:  '3'
				index: 3
			},
			mir.Value{
				id:    4
				typ:   float_t
				kind:  .constant
				name:  '4'
				index: 4
			},
			mir.Value{
				id:    5
				typ:   int_t
				kind:  .constant
				name:  '5'
				index: 5
			},
		]
	}
}

fn new_x64_abi_call_module(sret bool) mir.Module {
	mut ts := ssa.TypeStore.new()
	int_t := ts.get_int(64)
	float_t := ts.get_float(64)
	struct9_t := ts.register(ssa.Type{
		kind:   .struct_t
		fields: [int_t, ts.get_int(8)]
	})
	mut m := mir.Module{
		type_store: unsafe { *ts }
		values:     []mir.Value{len: 8}
		instrs:     []mir.Instruction{len: 2}
		blocks:     []mir.BasicBlock{len: 1}
		funcs:      []mir.Function{len: 1}
	}
	m.values[0] = mir.Value{
		id:    0
		typ:   int_t
		kind:  .func_ref
		name:  'callee'
		index: 0
	}
	for i in 1 .. 6 {
		m.values[i] = mir.Value{
			id:    i
			typ:   if i in [2, 4] { float_t } else { int_t }
			kind:  .constant
			name:  i.str()
			index: i
		}
	}
	m.values[6] = mir.Value{
		id:    6
		typ:   struct9_t
		kind:  .instruction
		name:  'v6'
		index: 0
	}
	m.values[7] = mir.Value{
		id:    7
		typ:   int_t
		kind:  .instruction
		name:  'v7'
		index: 1
	}
	m.instrs[0] = mir.Instruction{
		op:       if sret { .call_sret } else { .call }
		operands: if sret { [ssa.ValueID(0), 1, 2] } else { [ssa.ValueID(0), 1, 2, 3, 4, 5] }
		typ:      if sret { struct9_t } else { int_t }
		block:    0
	}
	m.instrs[1] = mir.Instruction{
		op:       .ret
		operands: []ssa.ValueID{}
		typ:      0
		block:    0
	}
	m.blocks[0] = mir.BasicBlock{
		id:     0
		val_id: 0
		name:   'entry'
		parent: 0
		instrs: [ssa.ValueID(6), 7]
	}
	m.funcs[0] = mir.Function{
		id:     0
		name:   'caller'
		typ:    0
		blocks: [ssa.BlockID(0)]
	}
	return m
}

fn new_x64_abi_struct8_call_module() mir.Module {
	mut ts := ssa.TypeStore.new()
	int_t := ts.get_int(64)
	struct8_t := ts.register(ssa.Type{
		kind:   .struct_t
		fields: [int_t]
	})
	mut m := mir.Module{
		type_store: unsafe { *ts }
		values:     []mir.Value{len: 5}
		instrs:     []mir.Instruction{len: 2}
		blocks:     []mir.BasicBlock{len: 1}
		funcs:      []mir.Function{len: 1}
	}
	m.values[0] = mir.Value{
		id:    0
		typ:   int_t
		kind:  .func_ref
		name:  'callee'
		index: 0
	}
	m.values[1] = mir.Value{
		id:    1
		typ:   struct8_t
		kind:  .argument
		name:  's'
		index: 1
	}
	m.values[2] = mir.Value{
		id:    2
		typ:   int_t
		kind:  .instruction
		name:  'v2'
		index: 0
	}
	m.values[3] = mir.Value{
		id:    3
		typ:   int_t
		kind:  .instruction
		name:  'v3'
		index: 1
	}
	m.values[4] = mir.Value{
		id:    4
		typ:   int_t
		kind:  .basic_block
		name:  'entry'
		index: 0
	}
	m.instrs[0] = mir.Instruction{
		op:       .call
		operands: [ssa.ValueID(0), 1]
		typ:      int_t
		block:    0
	}
	m.instrs[1] = mir.Instruction{
		op:       .ret
		operands: []ssa.ValueID{}
		typ:      0
		block:    0
	}
	m.blocks[0] = mir.BasicBlock{
		id:     0
		val_id: 4
		name:   'entry'
		parent: 0
		instrs: [ssa.ValueID(2), 3]
	}
	m.funcs[0] = mir.Function{
		id:     0
		name:   'caller'
		typ:    0
		blocks: [ssa.BlockID(0)]
		params: [ssa.ValueID(1)]
	}
	return m
}

fn new_x64_abi_stack_indirect_aggregate_call_module() mir.Module {
	mut ts := ssa.TypeStore.new()
	int_t := ts.get_int(64)
	struct16_t := ts.register(ssa.Type{
		kind:   .struct_t
		fields: [int_t, int_t]
	})
	mut m := mir.Module{
		type_store: unsafe { *ts }
		values:     []mir.Value{len: 10}
		instrs:     []mir.Instruction{len: 2}
		blocks:     []mir.BasicBlock{len: 1}
		funcs:      []mir.Function{len: 1}
	}
	m.values[0] = mir.Value{
		id:    0
		typ:   int_t
		kind:  .func_ref
		name:  'callee'
		index: 0
	}
	for i in 1 .. 5 {
		m.values[i] = mir.Value{
			id:    i
			typ:   int_t
			kind:  .constant
			name:  i.str()
			index: i
		}
	}
	m.values[5] = mir.Value{
		id:    5
		typ:   struct16_t
		kind:  .argument
		name:  's'
		index: 5
	}
	m.values[6] = mir.Value{
		id:    6
		typ:   int_t
		kind:  .instruction
		name:  'v6'
		index: 0
	}
	m.values[7] = mir.Value{
		id:    7
		typ:   int_t
		kind:  .instruction
		name:  'v7'
		index: 1
	}
	m.values[8] = mir.Value{
		id:    8
		typ:   int_t
		kind:  .basic_block
		name:  'entry'
		index: 0
	}
	m.values[9] = mir.Value{
		id:    9
		typ:   int_t
		kind:  .constant
		name:  '0'
		index: 9
	}
	m.instrs[0] = mir.Instruction{
		op:            .call
		operands:      [ssa.ValueID(0), 1, 2, 3, 4, 5]
		typ:           int_t
		block:         0
		abi_arg_class: [.in_reg, .in_reg, .in_reg, .in_reg, .indirect]
	}
	m.instrs[1] = mir.Instruction{
		op:       .ret
		operands: []ssa.ValueID{}
		typ:      0
		block:    0
	}
	m.blocks[0] = mir.BasicBlock{
		id:     0
		val_id: 8
		name:   'entry'
		parent: 0
		instrs: [ssa.ValueID(6), 7]
	}
	m.funcs[0] = mir.Function{
		id:              0
		name:            'caller'
		typ:             0
		blocks:          [ssa.BlockID(0)]
		params:          [ssa.ValueID(5)]
		abi_param_class: [.indirect]
	}
	return m
}

fn new_x64_abi_callee_stack_arg_module() mir.Module {
	mut ts := ssa.TypeStore.new()
	int_t := ts.get_int(64)
	mut m := mir.Module{
		type_store: unsafe { *ts }
		values:     []mir.Value{len: 7}
		instrs:     []mir.Instruction{len: 1}
		blocks:     []mir.BasicBlock{len: 1}
		funcs:      []mir.Function{len: 1}
	}
	for i in 0 .. 5 {
		m.values[i] = mir.Value{
			id:    i
			typ:   int_t
			kind:  .argument
			name:  'a${i}'
			index: i
		}
	}
	m.values[5] = mir.Value{
		id:    5
		typ:   int_t
		kind:  .instruction
		name:  'v5'
		index: 0
	}
	m.values[6] = mir.Value{
		id:    6
		typ:   int_t
		kind:  .basic_block
		name:  'entry'
		index: 0
	}
	m.instrs[0] = mir.Instruction{
		op:       .ret
		operands: []ssa.ValueID{}
		typ:      0
		block:    0
	}
	m.blocks[0] = mir.BasicBlock{
		id:     0
		val_id: 6
		name:   'entry'
		parent: 0
		instrs: [ssa.ValueID(5)]
	}
	m.funcs[0] = mir.Function{
		id:     0
		name:   'callee'
		typ:    0
		blocks: [ssa.BlockID(0)]
		params: [ssa.ValueID(0), 1, 2, 3, 4]
	}
	return m
}

fn new_x64_abi_indirect_aggregate_call_module(mark_indirect bool) mir.Module {
	mut ts := ssa.TypeStore.new()
	int_t := ts.get_int(64)
	struct16_t := ts.register(ssa.Type{
		kind:   .struct_t
		fields: [int_t, int_t]
	})
	mut m := mir.Module{
		type_store: unsafe { *ts }
		values:     []mir.Value{len: 5}
		instrs:     []mir.Instruction{len: 2}
		blocks:     []mir.BasicBlock{len: 1}
		funcs:      []mir.Function{len: 1}
	}
	m.values[0] = mir.Value{
		id:    0
		typ:   int_t
		kind:  .func_ref
		name:  'callee'
		index: 0
	}
	m.values[1] = mir.Value{
		id:    1
		typ:   struct16_t
		kind:  .argument
		name:  's'
		index: 1
	}
	m.values[2] = mir.Value{
		id:    2
		typ:   int_t
		kind:  .instruction
		name:  'v2'
		index: 0
	}
	m.values[3] = mir.Value{
		id:    3
		typ:   int_t
		kind:  .instruction
		name:  'v3'
		index: 1
	}
	m.values[4] = mir.Value{
		id:    4
		typ:   int_t
		kind:  .basic_block
		name:  'entry'
		index: 0
	}
	m.instrs[0] = mir.Instruction{
		op:            .call
		operands:      [ssa.ValueID(0), 1]
		typ:           int_t
		block:         0
		abi_arg_class: if mark_indirect { [.indirect] } else { []mir.AbiArgClass{} }
	}
	m.instrs[1] = mir.Instruction{
		op:       .ret
		operands: []ssa.ValueID{}
		typ:      0
		block:    0
	}
	m.blocks[0] = mir.BasicBlock{
		id:     0
		val_id: 4
		name:   'entry'
		parent: 0
		instrs: [ssa.ValueID(2), 3]
	}
	m.funcs[0] = mir.Function{
		id:              0
		name:            'caller'
		typ:             0
		blocks:          [ssa.BlockID(0)]
		params:          [ssa.ValueID(1)]
		abi_param_class: if mark_indirect { [.indirect] } else { []mir.AbiArgClass{} }
	}
	return m
}
