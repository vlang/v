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

fn index_bytes(haystack []u8, needle []u8) int {
	if needle.len == 0 {
		return 0
	}
	if needle.len > haystack.len {
		return -1
	}
	for i in 0 .. haystack.len - needle.len + 1 {
		if haystack[i..i + needle.len] == needle {
			return i
		}
	}
	return -1
}

fn last_index_bytes(haystack []u8, needle []u8) int {
	if needle.len == 0 {
		return haystack.len
	}
	if needle.len > haystack.len {
		return -1
	}
	for i := haystack.len - needle.len; i >= 0; i-- {
		if haystack[i..i + needle.len] == needle {
			return i
		}
	}
	return -1
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

fn test_x64_union_type_layout_uses_overlapping_fields() {
	mut ts := ssa.TypeStore.new()
	f64_t := ts.get_float(64)
	u64_t := ts.get_uint(64)
	union_t := ts.register(ssa.Type{
		kind:        .struct_t
		fields:      [f64_t, u64_t]
		field_names: ['f', 'u']
		is_union:    true
	})
	mut mod := mir.Module{
		type_store: unsafe { *ts }
	}
	gen := Gen.new(&mod)

	assert gen.struct_field_offset_bytes(union_t, 0) == 0
	assert gen.struct_field_offset_bytes(union_t, 1) == 0
	assert gen.type_align(union_t) == 8
	assert gen.type_size(union_t) == 8
}

fn test_x64_abi_basic_argument_position_matrix() {
	sysv_int := [int(rdi), int(rsi), int(rdx), int(rcx), int(r8), int(r9), x64_no_arg_reg,
		x64_no_arg_reg, x64_no_arg_reg]
	sysv_float := [0, 1, 2, 3, 4, 5, 6, 7, x64_no_arg_reg]
	windows_int := [int(rcx), int(rdx), int(r8), int(r9), x64_no_arg_reg, x64_no_arg_reg,
		x64_no_arg_reg, x64_no_arg_reg, x64_no_arg_reg]
	windows_float := [0, 1, 2, 3, x64_no_arg_reg, x64_no_arg_reg, x64_no_arg_reg, x64_no_arg_reg,
		x64_no_arg_reg]

	for position in 0 .. 9 {
		assert X64Abi.sysv.int_arg_reg_at(position) == sysv_int[position]
		assert X64Abi.sysv.float_arg_reg_at(position) == sysv_float[position]
		assert X64Abi.windows.int_arg_reg_for_position(position) == windows_int[position]
		assert X64Abi.windows.float_arg_reg_for_position(position) == windows_float[position]
	}
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

fn test_x64_windows_stack_argument_offset_matrix() {
	expected_call_offsets := [32, 40, 48, 56, 64]
	expected_callee_offsets := [48, 56, 64, 72, 80]
	positions := [4, 5, 6, 7, 8]
	for i, position in positions {
		assert X64Abi.windows.call_stack_arg_offset(position) == expected_call_offsets[i]
		assert X64Abi.windows.stack_arg_offset(position) == expected_callee_offsets[i]
	}
	assert X64Abi.windows.call_frame_size(0) == 32
	assert X64Abi.windows.call_frame_size(1) == 48
	assert X64Abi.windows.call_frame_size(2) == 48
	assert X64Abi.windows.call_frame_size(3) == 64
	assert X64Abi.windows.call_frame_size(4) == 64
	assert X64Abi.windows.call_frame_size(5) == 80
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

fn test_x64_sysv_stack_arg_mask_counts_two_eightbyte_aggregate_registers() {
	mut mod := new_x64_abi_pair_arg_test_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .elf, .sysv)
	// x86-64 psABI 3.2.3 classifies small integer aggregates by eightbyte:
	// this Pair consumes r8/r9 after four scalar INTEGER arguments, so the
	// following scalar argument must be passed on the stack.
	pair_then_scalar := mir.Instruction{
		operands: [ssa.ValueID(0), 1, 2, 3, 4, 5, 6]
	}
	stack_after_pair := gen.call_stack_arg_mask(pair_then_scalar, gen.abi.int_arg_regs().len, 0)

	assert stack_after_pair == [false, false, false, false, false, true]
	assert gen.call_stack_slots(pair_then_scalar, stack_after_pair) == 1
}

fn test_x64_sysv_stack_arg_mask_spilled_aggregate_does_not_consume_remaining_register() {
	mut mod := new_x64_abi_pair_arg_test_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .elf, .sysv)
	// If an aggregate cannot fit entirely in the remaining INTEGER registers,
	// psABI 3.2.3 requires it to roll back to the stack; the next scalar can
	// still use the remaining register.
	spilled_pair_then_scalar := mir.Instruction{
		operands: [ssa.ValueID(0), 1, 2, 3, 4, 6, 5, 7]
	}
	stack_after_spill := gen.call_stack_arg_mask(spilled_pair_then_scalar,
		gen.abi.int_arg_regs().len, 0)

	assert stack_after_spill == [false, false, false, false, false, true, false]
	assert gen.call_stack_slots(spilled_pair_then_scalar, stack_after_spill) == 2
}

fn test_x64_windows_call_frame_reserves_shadow_space() {
	mut mod := new_x64_abi_test_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .coff, .windows)

	cleanup := gen.emit_windows_call_frame(1)
	assert cleanup == 48
	gen.cleanup_windows_call_frame(cleanup)
	assert gen.coff.text_data == [u8(0x48), 0x83, 0xec, 0x30, 0x48, 0x83, 0xc4, 0x30]
}

fn test_x64_windows_large_stack_allocation_probes_each_page() {
	mut mod := new_x64_abi_test_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .coff, .windows)
	gen.stack_size = x64_windows_stack_probe_page_size * 2 + 8

	gen.emit_stack_allocation()

	page_probe := [u8(0x48), 0x81, 0xec, 0x00, 0x10, 0x00, 0x00, 0xf6, 0x04, 0x24, 0x00]
	mut expected := []u8{}
	expected << page_probe
	expected << page_probe
	expected << [u8(0x48), 0x83, 0xec, 0x08, 0xf6, 0x04, 0x24, 0x00]
	assert gen.coff.text_data == expected
}

fn test_x64_sysv_large_stack_allocation_uses_single_sub() {
	mut mod := new_x64_abi_test_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .elf, .sysv)
	gen.stack_size = x64_windows_stack_probe_page_size * 2 + 8

	gen.emit_stack_allocation()

	assert gen.elf.text_data == [u8(0x48), 0x81, 0xec, 0x08, 0x20, 0x00, 0x00]
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

fn test_x64_windows_integer_stack_arg_store_writes_full_slot() {
	mut mod := new_x64_abi_u32_stack_arg_test_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .coff, .windows)
	instr := mir.Instruction{
		operands: [ssa.ValueID(0), 1, 2, 3, 4, 5]
	}

	gen.store_windows_call_stack_arg(5, 4, 4, instr)
	assert gen.coff.text_data == [u8(0x31), 0xc0, 0x48, 0x89, 0x44, 0x24, 0x20]
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

fn test_x64_windows_codegen_call_sret_with_indirect_receiver_shifts_user_args() {
	mut mod := new_x64_abi_sret_indirect_receiver_call_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .coff, .windows)
	gen.gen()
	code := gen.coff.text_data

	end_arg := index_bytes(code, [u8(0x41), 0xb9, 0x03, 0, 0, 0]) // end -> R9
	start_arg := index_bytes(code, [u8(0x41), 0xb8, 0x01, 0, 0, 0]) // start -> R8
	receiver_arg := index_bytes(code, [u8(0x48), 0x89, 0xc2]) // receiver pointer -> RDX
	hidden_sret := index_bytes(code, [u8(0x48), 0x89, 0xc1]) // hidden sret pointer -> RCX

	assert end_arg >= 0
	assert start_arg > end_arg
	assert receiver_arg > start_arg
	assert hidden_sret > receiver_arg
}

fn test_x64_windows_codegen_sret_callee_prologue_shifts_indirect_receiver_and_scalars() {
	mut mod := new_x64_abi_sret_indirect_receiver_callee_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .coff, .windows)
	gen.gen()
	code := gen.coff.text_data

	receiver_from_rdx := index_bytes(code, [u8(0x49), 0x89, 0xd2]) // mov r10, rdx
	receiver_from_rcx := index_bytes(code, [u8(0x49), 0x89, 0xca]) // mov r10, rcx
	start_from_r8 := index_bytes(code, [u8(0x4c), 0x89, 0x45]) // mov [rbp+disp8], r8
	end_from_r9 := index_bytes(code, [u8(0x4c), 0x89, 0x4d]) // mov [rbp+disp8], r9

	assert receiver_from_rdx >= 0
	assert receiver_from_rcx < 0
	assert start_from_r8 > receiver_from_rdx
	assert end_from_r9 > start_from_r8
}

fn test_x64_sysv_codegen_direct_integer_pair_call_result_stores_rax_and_rdx() {
	mut mod := new_x64_abi_pair_return_call_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .elf, .sysv)
	gen.gen()
	code := gen.elf.text_data

	rax_store := index_bytes(code, [u8(0x48), 0x89, 0x45]) // mov [rbp+disp8], rax
	rdx_store := index_bytes(code, [u8(0x48), 0x89, 0x55]) // mov [rbp+disp8], rdx

	assert rax_store >= 0
	assert rdx_store > rax_store
}

fn test_x64_sysv_codegen_direct_integer_pair_return_loads_rax_and_rdx() {
	mut mod := new_x64_abi_pair_return_callee_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .elf, .sysv)
	gen.gen()
	code := gen.elf.text_data

	rax_load := index_bytes(code, [u8(0x49), 0x8b, 0x02]) // mov rax, [r10]
	rdx_load := index_bytes(code, [u8(0x49), 0x8b, 0x52, 0x08]) // mov rdx, [r10+8]

	assert rax_load >= 0
	assert rdx_load > rax_load
}

fn test_x64_sysv_codegen_mixed_aggregate_param_stores_gpr_and_xmm() {
	mut mod := new_x64_abi_mixed_aggregate_call_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .elf, .sysv)
	gen.gen()
	code := gen.elf.text_data

	assert contains_bytes(code, [u8(0x48), 0x89, 0x7d]) // RDI -> integer eightbyte
	assert contains_bytes(code, [u8(0xf2), 0x0f, 0x11, 0x45]) // XMM0 -> SSE eightbyte
}

fn test_x64_sysv_codegen_mixed_aggregate_call_arg_loads_gpr_and_xmm() {
	mut mod := new_x64_abi_mixed_aggregate_call_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .elf, .sysv)
	gen.gen()
	code := gen.elf.text_data

	rdi_load := index_bytes(code, [u8(0x49), 0x8b, 0x3a]) // mov rdi, [r10]
	xmm0_load := index_bytes(code, [u8(0xf2), 0x41, 0x0f, 0x10, 0x42, 0x08]) // movsd xmm0, [r10+8]

	assert rdi_load >= 0
	assert xmm0_load > rdi_load
}

fn test_x64_sysv_codegen_mixed_aggregate_call_sets_sse_arg_count_to_one() {
	mut mod := new_x64_abi_mixed_aggregate_call_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .elf, .sysv)
	gen.gen()
	code := gen.elf.text_data

	xmm0_load := index_bytes(code, [u8(0xf2), 0x41, 0x0f, 0x10, 0x42, 0x08]) // movsd xmm0, [r10+8]
	al_count := index_bytes(code, [u8(0xb8), 0x01, 0, 0, 0]) // mov eax, 1
	call := last_index_bytes(code, [u8(0xe8)])

	assert xmm0_load >= 0
	assert al_count > xmm0_load
	assert call > al_count
}

fn test_x64_sysv_codegen_indirect_f64_call_loads_callee_before_sse_arg_count() {
	code := emit_x64_abi_indirect_f64_call_with_callee_in_rax(false)

	callee_load := index_bytes(code, [u8(0x49), 0x89, 0xc2]) // mov r10, rax
	al_count := index_bytes(code, [u8(0xb8), 0x01, 0, 0, 0]) // mov eax, 1
	call := index_bytes(code, [u8(0x41), 0xff, 0xd2]) // call r10

	assert callee_load >= 0
	assert al_count > callee_load
	assert call > al_count
}

fn test_x64_sysv_codegen_indirect_f64_call_sret_loads_callee_before_sse_arg_count() {
	code := emit_x64_abi_indirect_f64_call_with_callee_in_rax(true)

	callee_load := index_bytes(code, [u8(0x49), 0x89, 0xc2]) // mov r10, rax
	al_count := index_bytes(code, [u8(0xb8), 0x01, 0, 0, 0]) // mov eax, 1
	call := index_bytes(code, [u8(0x41), 0xff, 0xd2]) // call r10

	assert callee_load >= 0
	assert al_count > callee_load
	assert call > al_count
}

fn test_x64_sysv_codegen_call_sret_shifts_mixed_user_arg_to_rsi_and_xmm0() {
	mut mod := new_x64_abi_sret_mixed_aggregate_call_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .elf, .sysv)
	gen.gen()
	code := gen.elf.text_data

	sret := index_bytes(code, [u8(0x48), 0x89, 0xc7]) // hidden sret pointer -> RDI
	rsi_load := index_bytes(code, [u8(0x49), 0x8b, 0x32]) // mov rsi, [r10]
	xmm0_load := index_bytes(code, [u8(0xf2), 0x41, 0x0f, 0x10, 0x42, 0x08]) // movsd xmm0, [r10+8]
	al_count := index_bytes(code, [u8(0xb8), 0x01, 0, 0, 0]) // mov eax, 1
	call := last_index_bytes(code, [u8(0xe8)])

	assert sret >= 0
	assert rsi_load > sret
	assert xmm0_load > rsi_load
	assert al_count > xmm0_load
	assert call > al_count
}

fn test_x64_sysv_codegen_sse_pair_aggregate_param_stores_xmm0_and_xmm1() {
	mut mod := new_x64_abi_sse_pair_aggregate_call_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .elf, .sysv)
	gen.gen()
	code := gen.elf.text_data

	xmm0_store := index_bytes(code, [u8(0xf2), 0x0f, 0x11, 0x45]) // XMM0 -> first f64
	xmm1_store := index_bytes(code, [u8(0xf2), 0x0f, 0x11, 0x4d]) // XMM1 -> second f64

	assert xmm0_store >= 0
	assert xmm1_store > xmm0_store
}

fn test_x64_sysv_codegen_sse_pair_aggregate_call_arg_loads_xmm0_and_xmm1() {
	mut mod := new_x64_abi_sse_pair_aggregate_call_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .elf, .sysv)
	gen.gen()
	code := gen.elf.text_data

	xmm0_load := index_bytes(code, [u8(0xf2), 0x41, 0x0f, 0x10, 0x02]) // movsd xmm0, [r10]
	xmm1_load := index_bytes(code, [u8(0xf2), 0x41, 0x0f, 0x10, 0x4a, 0x08]) // movsd xmm1, [r10+8]
	al_count := index_bytes(code, [u8(0xb8), 0x02, 0, 0, 0]) // mov eax, 2
	call := last_index_bytes(code, [u8(0xe8)])

	assert xmm0_load >= 0
	assert xmm1_load > xmm0_load
	assert al_count > xmm1_load
	assert call > al_count
}

fn test_x64_sysv_codegen_sse_aggregate_call_result_stores_xmms() {
	mut mod := new_x64_abi_sse_pair_return_call_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .elf, .sysv)
	gen.gen()
	code := gen.elf.text_data

	xmm0_store := index_bytes(code, [u8(0xf2), 0x0f, 0x11, 0x45]) // movsd [rbp+disp], xmm0
	xmm1_store := index_bytes(code, [u8(0xf2), 0x0f, 0x11, 0x4d]) // movsd [rbp+disp], xmm1

	assert xmm0_store >= 0
	assert xmm1_store > xmm0_store
}

fn test_x64_sysv_codegen_mixed_aggregate_direct_return_loads_gpr_and_xmm() {
	mut mod := new_x64_abi_mixed_return_callee_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .elf, .sysv)
	gen.gen()
	code := gen.elf.text_data

	rax_load := index_bytes(code, [u8(0x49), 0x8b, 0x02]) // mov rax, [r10]
	xmm0_load := index_bytes(code, [u8(0xf2), 0x41, 0x0f, 0x10, 0x42, 0x08]) // movsd xmm0, [r10+8]

	assert rax_load >= 0
	assert xmm0_load > rax_load
}

fn test_x64_sysv_codegen_sseup_aggregate_param_and_call_arg_use_one_xmm_register() {
	mut mod := new_x64_abi_sseup_aggregate_call_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .elf, .sysv)
	gen.gen()
	code := gen.elf.text_data

	xmm0_store := index_bytes(code, [u8(0xf3), 0x0f, 0x7f, 0x45]) // movdqu [rbp+disp], xmm0
	xmm0_load := index_bytes(code, [u8(0xf3), 0x41, 0x0f, 0x6f, 0x02]) // movdqu xmm0, [r10]

	assert xmm0_store >= 0
	assert xmm0_load > xmm0_store
}

fn test_x64_sysv_codegen_sseup_aggregate_call_result_stores_one_xmm_register() {
	mut mod := new_x64_abi_sseup_return_call_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .elf, .sysv)
	gen.gen()
	code := gen.elf.text_data

	assert contains_bytes(code, [u8(0xf3), 0x0f, 0x7f, 0x45]) // movdqu [rbp+disp], xmm0
}

fn test_x64_sysv_codegen_sseup_aggregate_direct_return_loads_one_xmm_register() {
	mut mod := new_x64_abi_sseup_return_callee_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .elf, .sysv)
	gen.gen()
	code := gen.elf.text_data

	assert contains_bytes(code, [u8(0xf3), 0x41, 0x0f, 0x6f, 0x02]) // movdqu xmm0, [r10]
}

fn test_x64_windows_does_not_take_sysv_integer_pair_return_path() {
	mut mod := new_x64_abi_pair_return_call_module()
	gen := Gen.new_with_format_and_abi(&mod, .coff, .windows)

	assert !gen.is_sysv_integer_pair_return(x64_abi_integer_pair_return_class())
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

fn test_x64_windows_codegen_fifth_nil_pointer_arg_uses_8_byte_shadow_stack_slot() {
	mut mod := new_x64_abi_fifth_nil_pointer_call_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .coff, .windows)
	gen.gen()
	code := gen.coff.text_data

	assert contains_bytes(code, [u8(0x48), 0x83, 0xec, 0x30]) // 32 shadow bytes + 8 stack arg + align
	assert contains_bytes(code, [u8(0x31), 0xc0, 0x48, 0x89, 0x44, 0x24, 0x20]) // nil -> [rsp+32]
}

fn test_x64_windows_codegen_local_u32_pointer_arg_uses_r9_before_fifth_nil() {
	mut mod := new_x64_abi_writefile_like_pointer_call_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .coff, .windows)
	gen.gen()
	code := gen.coff.text_data

	assert contains_bytes(code, [u8(0x48), 0x83, 0xec, 0x30]) // 32 shadow bytes + 8 stack arg + align
	assert contains_bytes(code, [u8(0x48), 0x8d, 0x45]) // materialize local u32 address
	assert contains_bytes(code, [u8(0x4c), 0x8d, 0x4d]) // fourth arg pointer -> R9
	assert contains_bytes(code, [u8(0x31), 0xc0, 0x48, 0x89, 0x44, 0x24, 0x20]) // fifth nil -> [rsp+32]
}

fn test_x64_windows_codegen_writefile_buffer_pointer_uses_rdx() {
	mut mod := new_x64_abi_writefile_buffer_pointer_call_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .coff, .windows)
	gen.gen()
	code := gen.coff.text_data
	rdx_buffer := index_bytes(code, [u8(0x48), 0x8d, 0x55])
	r8_len := index_bytes(code, [u8(0x41), 0xb8, 0x01, 0, 0, 0])
	r9_written := index_bytes(code, [u8(0x4c), 0x8d, 0x4d])
	rcx_handle := last_index_bytes(code, [u8(0xb9), 0x01, 0, 0, 0])

	assert contains_bytes(code, [u8(0x48), 0x83, 0xec, 0x30]) // 32 shadow bytes + 8 stack arg + align
	assert rdx_buffer >= 0 // second arg &u8 buffer pointer -> RDX
	assert r8_len >= 0 // third arg length -> R8
	assert r9_written >= 0 // fourth arg &u32 written pointer -> R9
	assert rcx_handle > rdx_buffer // first arg is loaded last to protect RCX from scratch use
	assert rcx_handle > r8_len
	assert rcx_handle > r9_written
	assert contains_bytes(code, [u8(0x31), 0xc0, 0x48, 0x89, 0x44, 0x24, 0x20]) // fifth nil -> [rsp+32]
}

fn test_x64_windows_codegen_indirect_string_literal_materializes_before_call() {
	mut mod := new_x64_abi_indirect_string_literal_call_module()
	mut gen := Gen.new_with_format_and_abi(&mod, .coff, .windows)
	gen.gen()
	code := gen.coff.text_data

	assert gen.coff.rodata == [u8(`h`), `e`, `l`, `l`, `o`, 0]
	assert contains_bytes(code, [u8(0x48), 0x89, 0x45]) // store string.str in the stack slot
	assert contains_bytes(code, [u8(0xb8), 0x05, 0, 0, 0]) // materialized len = 5
	assert contains_bytes(code, [u8(0xb8), 0x01, 0, 0, 0]) // materialized is_lit = 1
	assert contains_bytes(code, [u8(0x48), 0x89, 0xc1]) // string slot pointer -> RCX
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

fn new_x64_abi_u32_stack_arg_test_module() mir.Module {
	mut ts := ssa.TypeStore.new()
	ptr_t := ts.get_ptr(ts.get_int(8))
	u32_t := ts.get_uint(32)
	return mir.Module{
		type_store: unsafe { *ts }
		values:     [
			mir.Value{
				id:    0
				typ:   ptr_t
				kind:  .func_ref
				name:  'callee'
				index: 0
			},
			mir.Value{
				id:    1
				typ:   ptr_t
				kind:  .constant
				name:  '0'
				index: 1
			},
			mir.Value{
				id:    2
				typ:   ptr_t
				kind:  .constant
				name:  '0'
				index: 2
			},
			mir.Value{
				id:    3
				typ:   ptr_t
				kind:  .constant
				name:  '0'
				index: 3
			},
			mir.Value{
				id:    4
				typ:   ptr_t
				kind:  .constant
				name:  '0'
				index: 4
			},
			mir.Value{
				id:    5
				typ:   u32_t
				kind:  .constant
				name:  '0'
				index: 5
			},
		]
	}
}

fn new_x64_abi_pair_arg_test_module() mir.Module {
	mut ts := ssa.TypeStore.new()
	int_t := ts.get_int(64)
	pair_t := ts.register(ssa.Type{
		kind:   .struct_t
		fields: [int_t, int_t]
	})
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
				typ:   int_t
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
				typ:   int_t
				kind:  .constant
				name:  '4'
				index: 4
			},
			mir.Value{
				id:    5
				typ:   pair_t
				kind:  .argument
				name:  'pair'
				index: 5
			},
			mir.Value{
				id:    6
				typ:   int_t
				kind:  .constant
				name:  '6'
				index: 6
			},
			mir.Value{
				id:    7
				typ:   int_t
				kind:  .constant
				name:  '7'
				index: 7
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

fn emit_x64_abi_indirect_f64_call_with_callee_in_rax(sret bool) []u8 {
	mut mod := new_x64_abi_indirect_f64_call_module(sret)
	mut gen := Gen.new_with_format_and_abi(&mod, .elf, .sysv)
	gen.stack_map = {
		1: -8
		2: -24
	}
	gen.reg_map = {
		0: int(rax)
	}
	gen.gen_instr(2)
	return gen.elf.text_data
}

fn new_x64_abi_indirect_f64_call_module(sret bool) mir.Module {
	mut ts := ssa.TypeStore.new()
	i64_t := ts.get_int(64)
	f64_t := ts.get_float(64)
	ret_t := ts.register(ssa.Type{
		kind:   .struct_t
		fields: [i64_t, i64_t, i64_t]
	})
	fn_t := ts.register(ssa.Type{
		kind:     .func_t
		params:   [f64_t]
		ret_type: if sret { ret_t } else { i64_t }
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
		typ:   fn_t
		kind:  .argument
		name:  'callee'
		index: 0
	}
	m.values[1] = mir.Value{
		id:    1
		typ:   f64_t
		kind:  .argument
		name:  'arg'
		index: 1
	}
	m.values[2] = mir.Value{
		id:    2
		typ:   if sret { ret_t } else { i64_t }
		kind:  .instruction
		name:  'call'
		index: 0
	}
	m.values[3] = mir.Value{
		id:    3
		typ:   0
		kind:  .instruction
		name:  'ret'
		index: 1
	}
	m.values[4] = mir.Value{
		id:    4
		typ:   0
		kind:  .basic_block
		name:  'entry'
		index: 0
	}
	m.instrs[0] = mir.Instruction{
		op:               if sret { .call_sret } else { .call }
		operands:         [ssa.ValueID(0), 1]
		typ:              if sret { ret_t } else { i64_t }
		block:            0
		abi_ret_indirect: sret
		abi_arg_class:    [.in_reg]
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
		params: [ssa.ValueID(0), 1]
	}
	return m
}

fn new_x64_abi_sret_indirect_receiver_call_module() mir.Module {
	mut ts := ssa.TypeStore.new()
	int_t := ts.get_int(64)
	array_t := ts.register(ssa.Type{
		kind:   .struct_t
		fields: [int_t, int_t, int_t, int_t]
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
		typ:   array_t
		kind:  .func_ref
		name:  'array__slice'
		index: 0
	}
	m.values[1] = mir.Value{
		id:    1
		typ:   array_t
		kind:  .argument
		name:  'receiver'
		index: 1
	}
	m.values[2] = mir.Value{
		id:    2
		typ:   int_t
		kind:  .constant
		name:  '1'
		index: 2
	}
	m.values[3] = mir.Value{
		id:    3
		typ:   int_t
		kind:  .constant
		name:  '3'
		index: 3
	}
	m.values[4] = mir.Value{
		id:    4
		typ:   array_t
		kind:  .instruction
		name:  'slice'
		index: 0
	}
	m.values[5] = mir.Value{
		id:    5
		typ:   int_t
		kind:  .instruction
		name:  'ret'
		index: 1
	}
	m.values[6] = mir.Value{
		id:    6
		typ:   int_t
		kind:  .basic_block
		name:  'entry'
		index: 0
	}
	m.values[7] = mir.Value{
		id:    7
		typ:   int_t
		kind:  .constant
		name:  '0'
		index: 7
	}
	m.instrs[0] = mir.Instruction{
		op:               .call_sret
		operands:         [ssa.ValueID(0), 1, 2, 3]
		typ:              array_t
		block:            0
		abi_ret_indirect: true
		abi_arg_class:    [.indirect, .in_reg, .in_reg]
	}
	m.instrs[1] = mir.Instruction{
		op:       .ret
		operands: [ssa.ValueID(7)]
		typ:      0
		block:    0
	}
	m.blocks[0] = mir.BasicBlock{
		id:     0
		val_id: 6
		name:   'entry'
		parent: 0
		instrs: [ssa.ValueID(4), 5]
	}
	m.funcs[0] = mir.Function{
		id:              0
		name:            'caller'
		typ:             int_t
		blocks:          [ssa.BlockID(0)]
		params:          [ssa.ValueID(1)]
		abi_param_class: [.indirect]
	}
	return m
}

fn new_x64_abi_sret_indirect_receiver_callee_module() mir.Module {
	mut ts := ssa.TypeStore.new()
	int_t := ts.get_int(64)
	array_t := ts.register(ssa.Type{
		kind:   .struct_t
		fields: [int_t, int_t, int_t, int_t]
	})
	mut m := mir.Module{
		type_store: unsafe { *ts }
		values:     []mir.Value{len: 6}
		instrs:     []mir.Instruction{len: 1}
		blocks:     []mir.BasicBlock{len: 1}
		funcs:      []mir.Function{len: 1}
	}
	m.values[0] = mir.Value{
		id:    0
		typ:   array_t
		kind:  .argument
		name:  'receiver'
		index: 0
	}
	m.values[1] = mir.Value{
		id:    1
		typ:   int_t
		kind:  .argument
		name:  'start'
		index: 1
	}
	m.values[2] = mir.Value{
		id:    2
		typ:   int_t
		kind:  .argument
		name:  'end'
		index: 2
	}
	m.values[3] = mir.Value{
		id:    3
		typ:   array_t
		kind:  .instruction
		name:  'ret'
		index: 0
	}
	m.values[4] = mir.Value{
		id:    4
		typ:   int_t
		kind:  .basic_block
		name:  'entry'
		index: 0
	}
	m.values[5] = mir.Value{
		id:    5
		typ:   int_t
		kind:  .constant
		name:  '0'
		index: 5
	}
	m.instrs[0] = mir.Instruction{
		op:       .ret
		operands: [ssa.ValueID(0)]
		typ:      0
		block:    0
	}
	m.blocks[0] = mir.BasicBlock{
		id:     0
		val_id: 4
		name:   'entry'
		parent: 0
		instrs: [ssa.ValueID(3)]
	}
	m.funcs[0] = mir.Function{
		id:               0
		name:             'array__slice'
		typ:              array_t
		blocks:           [ssa.BlockID(0)]
		params:           [ssa.ValueID(0), 1, 2]
		abi_ret_indirect: true
		abi_param_class:  [.indirect, .in_reg, .in_reg]
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

fn new_x64_abi_fifth_nil_pointer_call_module() mir.Module {
	mut ts := ssa.TypeStore.new()
	int_t := ts.get_int(64)
	u8_t := ts.get_int(8)
	ptr_u8_t := ts.get_ptr(u8_t)
	mut m := mir.Module{
		type_store: unsafe { *ts }
		values:     []mir.Value{len: 9}
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
		typ:   ptr_u8_t
		kind:  .constant
		name:  '0'
		index: 5
	}
	m.values[6] = mir.Value{
		id:    6
		typ:   int_t
		kind:  .instruction
		name:  'call'
		index: 0
	}
	m.values[7] = mir.Value{
		id:    7
		typ:   0
		kind:  .instruction
		name:  'ret'
		index: 1
	}
	m.values[8] = mir.Value{
		id:    8
		typ:   0
		kind:  .basic_block
		name:  'entry'
		index: 0
	}
	m.instrs[0] = mir.Instruction{
		op:       .call
		operands: [ssa.ValueID(0), 1, 2, 3, 4, 5]
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
		val_id: 8
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

fn new_x64_abi_writefile_like_pointer_call_module() mir.Module {
	mut ts := ssa.TypeStore.new()
	int_t := ts.get_int(64)
	u32_t := ts.get_int(32)
	u8_t := ts.get_int(8)
	ptr_u32_t := ts.get_ptr(u32_t)
	ptr_u8_t := ts.get_ptr(u8_t)
	mut m := mir.Module{
		type_store: unsafe { *ts }
		values:     []mir.Value{len: 9}
		instrs:     []mir.Instruction{len: 3}
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
	for i in 1 .. 4 {
		m.values[i] = mir.Value{
			id:    i
			typ:   int_t
			kind:  .constant
			name:  i.str()
			index: i
		}
	}
	m.values[4] = mir.Value{
		id:    4
		typ:   ptr_u32_t
		kind:  .instruction
		name:  'written'
		index: 0
	}
	m.values[5] = mir.Value{
		id:    5
		typ:   ptr_u8_t
		kind:  .constant
		name:  '0'
		index: 5
	}
	m.values[6] = mir.Value{
		id:    6
		typ:   int_t
		kind:  .instruction
		name:  'call'
		index: 1
	}
	m.values[7] = mir.Value{
		id:    7
		typ:   0
		kind:  .instruction
		name:  'ret'
		index: 2
	}
	m.values[8] = mir.Value{
		id:    8
		typ:   0
		kind:  .basic_block
		name:  'entry'
		index: 0
	}
	m.instrs[0] = mir.Instruction{
		op:       .alloca
		operands: []ssa.ValueID{}
		typ:      ptr_u32_t
		block:    0
	}
	m.instrs[1] = mir.Instruction{
		op:       .call
		operands: [ssa.ValueID(0), 1, 2, 3, 4, 5]
		typ:      int_t
		block:    0
	}
	m.instrs[2] = mir.Instruction{
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
		instrs: [ssa.ValueID(4), 6, 7]
	}
	m.funcs[0] = mir.Function{
		id:     0
		name:   'caller'
		typ:    0
		blocks: [ssa.BlockID(0)]
	}
	return m
}

fn new_x64_abi_writefile_buffer_pointer_call_module() mir.Module {
	mut ts := ssa.TypeStore.new()
	int_t := ts.get_int(64)
	u32_t := ts.get_int(32)
	u8_t := ts.get_int(8)
	ptr_u32_t := ts.get_ptr(u32_t)
	ptr_u8_t := ts.get_ptr(u8_t)
	mut m := mir.Module{
		type_store: unsafe { *ts }
		values:     []mir.Value{len: 9}
		instrs:     []mir.Instruction{len: 4}
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
		typ:   int_t
		kind:  .constant
		name:  '1'
		index: 1
	}
	m.values[2] = mir.Value{
		id:    2
		typ:   ptr_u8_t
		kind:  .instruction
		name:  'buf'
		index: 0
	}
	m.values[3] = mir.Value{
		id:    3
		typ:   int_t
		kind:  .constant
		name:  '1'
		index: 3
	}
	m.values[4] = mir.Value{
		id:    4
		typ:   ptr_u32_t
		kind:  .instruction
		name:  'written'
		index: 1
	}
	m.values[5] = mir.Value{
		id:    5
		typ:   ptr_u8_t
		kind:  .constant
		name:  '0'
		index: 5
	}
	m.values[6] = mir.Value{
		id:    6
		typ:   int_t
		kind:  .instruction
		name:  'call'
		index: 2
	}
	m.values[7] = mir.Value{
		id:    7
		typ:   0
		kind:  .instruction
		name:  'ret'
		index: 3
	}
	m.values[8] = mir.Value{
		id:    8
		typ:   0
		kind:  .basic_block
		name:  'entry'
		index: 0
	}
	m.instrs[0] = mir.Instruction{
		op:       .alloca
		operands: []ssa.ValueID{}
		typ:      ptr_u8_t
		block:    0
	}
	m.instrs[1] = mir.Instruction{
		op:       .alloca
		operands: []ssa.ValueID{}
		typ:      ptr_u32_t
		block:    0
	}
	m.instrs[2] = mir.Instruction{
		op:       .call
		operands: [ssa.ValueID(0), 1, 2, 3, 4, 5]
		typ:      int_t
		block:    0
	}
	m.instrs[3] = mir.Instruction{
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
		instrs: [ssa.ValueID(2), 4, 6, 7]
	}
	m.funcs[0] = mir.Function{
		id:     0
		name:   'caller'
		typ:    0
		blocks: [ssa.BlockID(0)]
	}
	return m
}

fn new_x64_abi_indirect_string_literal_call_module() mir.Module {
	mut ts := ssa.TypeStore.new()
	i64_t := ts.get_int(64)
	i32_t := ts.get_int(32)
	i8_t := ts.get_int(8)
	ptr_u8_t := ts.get_ptr(i8_t)
	string_t := ts.register(ssa.Type{
		kind:        .struct_t
		fields:      [ptr_u8_t, i32_t, i32_t]
		field_names: ['str', 'len', 'is_lit']
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
		typ:   i64_t
		kind:  .func_ref
		name:  'callee'
		index: 0
	}
	m.values[1] = mir.Value{
		id:    1
		typ:   string_t
		kind:  .string_literal
		name:  'hello'
		index: 5
	}
	m.values[2] = mir.Value{
		id:    2
		typ:   i64_t
		kind:  .instruction
		name:  'call'
		index: 0
	}
	m.values[3] = mir.Value{
		id:    3
		typ:   0
		kind:  .instruction
		name:  'ret'
		index: 1
	}
	m.values[4] = mir.Value{
		id:    4
		typ:   0
		kind:  .basic_block
		name:  'entry'
		index: 0
	}
	m.instrs[0] = mir.Instruction{
		op:            .call
		operands:      [ssa.ValueID(0), 1]
		typ:           i64_t
		block:         0
		abi_arg_class: [.indirect]
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
	}
	return m
}

fn x64_abi_integer_pair_return_class() mir.AbiValueClass {
	return mir.AbiValueClass{
		mode:    .direct
		size:    16
		classes: [.integer, .integer]
	}
}

fn x64_abi_mixed_aggregate_class() mir.AbiValueClass {
	return mir.AbiValueClass{
		mode:    .direct
		size:    16
		classes: [.integer, .sse]
	}
}

fn x64_abi_sse_pair_class() mir.AbiValueClass {
	return mir.AbiValueClass{
		mode:    .direct
		size:    16
		classes: [.sse, .sse]
	}
}

fn x64_abi_sseup_aggregate_class() mir.AbiValueClass {
	return mir.AbiValueClass{
		mode:    .direct
		size:    16
		classes: [.sse, .sseup]
	}
}

fn x64_abi_mixed_aggregate_layout() mir.AbiValueLayout {
	value_class := x64_abi_mixed_aggregate_class()
	return mir.AbiValueLayout{
		value_class: value_class
		locs:        [
			mir.AbiLocation{
				kind:   .int_reg
				index:  0
				offset: 0
				class:  .integer
			},
			mir.AbiLocation{
				kind:   .sse_reg
				index:  0
				offset: 8
				class:  .sse
			},
		]
	}
}

fn x64_abi_sret_mixed_aggregate_arg_layout() mir.AbiValueLayout {
	value_class := x64_abi_mixed_aggregate_class()
	return mir.AbiValueLayout{
		value_class: value_class
		locs:        [
			mir.AbiLocation{
				kind:   .int_reg
				index:  1
				offset: 0
				class:  .integer
			},
			mir.AbiLocation{
				kind:   .sse_reg
				index:  0
				offset: 8
				class:  .sse
			},
		]
	}
}

fn x64_abi_sse_pair_layout() mir.AbiValueLayout {
	value_class := x64_abi_sse_pair_class()
	return mir.AbiValueLayout{
		value_class: value_class
		locs:        [
			mir.AbiLocation{
				kind:   .sse_reg
				index:  0
				offset: 0
				class:  .sse
			},
			mir.AbiLocation{
				kind:   .sse_reg
				index:  1
				offset: 8
				class:  .sse
			},
		]
	}
}

fn x64_abi_sseup_aggregate_layout() mir.AbiValueLayout {
	value_class := x64_abi_sseup_aggregate_class()
	return mir.AbiValueLayout{
		value_class: value_class
		locs:        [
			mir.AbiLocation{
				kind:   .sse_reg
				index:  0
				offset: 0
				class:  .sse
			},
			mir.AbiLocation{
				kind:   .sse_reg
				index:  0
				offset: 8
				class:  .sseup
			},
		]
	}
}

fn new_x64_abi_pair_return_call_module() mir.Module {
	mut ts := ssa.TypeStore.new()
	int_t := ts.get_int(64)
	pair_t := ts.register(ssa.Type{
		kind:   .struct_t
		fields: [int_t, int_t]
	})
	mut m := mir.Module{
		type_store: unsafe { *ts }
		values:     []mir.Value{len: 4}
		instrs:     []mir.Instruction{len: 2}
		blocks:     []mir.BasicBlock{len: 1}
		funcs:      []mir.Function{len: 1}
	}
	m.values[0] = mir.Value{
		id:    0
		typ:   pair_t
		kind:  .func_ref
		name:  'callee'
		index: 0
	}
	m.values[1] = mir.Value{
		id:    1
		typ:   pair_t
		kind:  .instruction
		name:  'pair'
		index: 0
	}
	m.values[2] = mir.Value{
		id:    2
		typ:   0
		kind:  .instruction
		name:  'ret'
		index: 1
	}
	m.values[3] = mir.Value{
		id:    3
		typ:   0
		kind:  .basic_block
		name:  'entry'
		index: 0
	}
	m.instrs[0] = mir.Instruction{
		op:            .call
		operands:      [ssa.ValueID(0)]
		typ:           pair_t
		block:         0
		abi_ret_class: x64_abi_integer_pair_return_class()
	}
	m.instrs[1] = mir.Instruction{
		op:       .ret
		operands: []ssa.ValueID{}
		typ:      0
		block:    0
	}
	m.blocks[0] = mir.BasicBlock{
		id:     0
		val_id: 3
		name:   'entry'
		parent: 0
		instrs: [ssa.ValueID(1), 2]
	}
	m.funcs[0] = mir.Function{
		id:     0
		name:   'caller'
		typ:    0
		blocks: [ssa.BlockID(0)]
	}
	return m
}

fn new_x64_abi_pair_return_callee_module() mir.Module {
	mut ts := ssa.TypeStore.new()
	int_t := ts.get_int(64)
	pair_t := ts.register(ssa.Type{
		kind:   .struct_t
		fields: [int_t, int_t]
	})
	mut m := mir.Module{
		type_store: unsafe { *ts }
		values:     []mir.Value{len: 3}
		instrs:     []mir.Instruction{len: 1}
		blocks:     []mir.BasicBlock{len: 1}
		funcs:      []mir.Function{len: 1}
	}
	m.values[0] = mir.Value{
		id:    0
		typ:   pair_t
		kind:  .argument
		name:  'pair'
		index: 0
	}
	m.values[1] = mir.Value{
		id:    1
		typ:   0
		kind:  .instruction
		name:  'ret'
		index: 0
	}
	m.values[2] = mir.Value{
		id:    2
		typ:   0
		kind:  .basic_block
		name:  'entry'
		index: 0
	}
	m.instrs[0] = mir.Instruction{
		op:       .ret
		operands: [ssa.ValueID(0)]
		typ:      0
		block:    0
	}
	m.blocks[0] = mir.BasicBlock{
		id:     0
		val_id: 2
		name:   'entry'
		parent: 0
		instrs: [ssa.ValueID(1)]
	}
	m.funcs[0] = mir.Function{
		id:            0
		name:          'callee'
		typ:           pair_t
		blocks:        [ssa.BlockID(0)]
		params:        [ssa.ValueID(0)]
		abi_ret_class: x64_abi_integer_pair_return_class()
	}
	return m
}

fn new_x64_abi_mixed_aggregate_call_module() mir.Module {
	mut ts := ssa.TypeStore.new()
	i64_t := ts.get_int(64)
	f64_t := ts.get_float(64)
	mixed_t := ts.register(ssa.Type{
		kind:   .struct_t
		fields: [i64_t, f64_t]
	})
	layout := x64_abi_mixed_aggregate_layout()
	mut m := mir.Module{
		type_store: unsafe { *ts }
		values:     []mir.Value{len: 5}
		instrs:     []mir.Instruction{len: 2}
		blocks:     []mir.BasicBlock{len: 1}
		funcs:      []mir.Function{len: 1}
	}
	m.values[0] = mir.Value{
		id:    0
		typ:   i64_t
		kind:  .func_ref
		name:  'callee'
		index: 0
	}
	m.values[1] = mir.Value{
		id:    1
		typ:   mixed_t
		kind:  .argument
		name:  'mixed'
		index: 0
	}
	m.values[2] = mir.Value{
		id:    2
		typ:   i64_t
		kind:  .instruction
		name:  'call'
		index: 0
	}
	m.values[3] = mir.Value{
		id:    3
		typ:   0
		kind:  .instruction
		name:  'ret'
		index: 1
	}
	m.values[4] = mir.Value{
		id:    4
		typ:   0
		kind:  .basic_block
		name:  'entry'
		index: 0
	}
	m.instrs[0] = mir.Instruction{
		op:              .call
		operands:        [ssa.ValueID(0), 1]
		typ:             i64_t
		block:           0
		abi_arg_class:   [.in_reg]
		abi_arg_classes: [layout.value_class]
		abi_arg_layouts: [layout]
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
		id:                0
		name:              'caller'
		typ:               0
		blocks:            [ssa.BlockID(0)]
		params:            [ssa.ValueID(1)]
		abi_param_classes: [layout.value_class]
		abi_param_layouts: [layout]
	}
	return m
}

fn new_x64_abi_sret_mixed_aggregate_call_module() mir.Module {
	mut ts := ssa.TypeStore.new()
	i64_t := ts.get_int(64)
	f64_t := ts.get_float(64)
	ret_t := ts.register(ssa.Type{
		kind:   .struct_t
		fields: [i64_t, i64_t, i64_t]
	})
	mixed_t := ts.register(ssa.Type{
		kind:   .struct_t
		fields: [i64_t, f64_t]
	})
	layout := x64_abi_sret_mixed_aggregate_arg_layout()
	mut m := mir.Module{
		type_store: unsafe { *ts }
		values:     []mir.Value{len: 5}
		instrs:     []mir.Instruction{len: 2}
		blocks:     []mir.BasicBlock{len: 1}
		funcs:      []mir.Function{len: 1}
	}
	m.values[0] = mir.Value{
		id:    0
		typ:   ret_t
		kind:  .func_ref
		name:  'callee'
		index: 0
	}
	m.values[1] = mir.Value{
		id:    1
		typ:   mixed_t
		kind:  .argument
		name:  'mixed'
		index: 0
	}
	m.values[2] = mir.Value{
		id:    2
		typ:   ret_t
		kind:  .instruction
		name:  'call'
		index: 0
	}
	m.values[3] = mir.Value{
		id:    3
		typ:   0
		kind:  .instruction
		name:  'ret'
		index: 1
	}
	m.values[4] = mir.Value{
		id:    4
		typ:   0
		kind:  .basic_block
		name:  'entry'
		index: 0
	}
	m.instrs[0] = mir.Instruction{
		op:               .call_sret
		operands:         [ssa.ValueID(0), 1]
		typ:              ret_t
		block:            0
		abi_ret_indirect: true
		abi_arg_class:    [.in_reg]
		abi_arg_classes:  [layout.value_class]
		abi_arg_layouts:  [layout]
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
	}
	return m
}

fn new_x64_abi_sse_pair_aggregate_call_module() mir.Module {
	mut ts := ssa.TypeStore.new()
	i64_t := ts.get_int(64)
	f64_t := ts.get_float(64)
	pair_t := ts.register(ssa.Type{
		kind:   .struct_t
		fields: [f64_t, f64_t]
	})
	layout := x64_abi_sse_pair_layout()
	mut m := mir.Module{
		type_store: unsafe { *ts }
		values:     []mir.Value{len: 5}
		instrs:     []mir.Instruction{len: 2}
		blocks:     []mir.BasicBlock{len: 1}
		funcs:      []mir.Function{len: 1}
	}
	m.values[0] = mir.Value{
		id:    0
		typ:   i64_t
		kind:  .func_ref
		name:  'callee'
		index: 0
	}
	m.values[1] = mir.Value{
		id:    1
		typ:   pair_t
		kind:  .argument
		name:  'pair'
		index: 0
	}
	m.values[2] = mir.Value{
		id:    2
		typ:   i64_t
		kind:  .instruction
		name:  'call'
		index: 0
	}
	m.values[3] = mir.Value{
		id:    3
		typ:   0
		kind:  .instruction
		name:  'ret'
		index: 1
	}
	m.values[4] = mir.Value{
		id:    4
		typ:   0
		kind:  .basic_block
		name:  'entry'
		index: 0
	}
	m.instrs[0] = mir.Instruction{
		op:              .call
		operands:        [ssa.ValueID(0), 1]
		typ:             i64_t
		block:           0
		abi_arg_class:   [.in_reg]
		abi_arg_classes: [layout.value_class]
		abi_arg_layouts: [layout]
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
		id:                0
		name:              'caller'
		typ:               0
		blocks:            [ssa.BlockID(0)]
		params:            [ssa.ValueID(1)]
		abi_param_classes: [layout.value_class]
		abi_param_layouts: [layout]
	}
	return m
}

fn new_x64_abi_sseup_aggregate_call_module() mir.Module {
	mut ts := ssa.TypeStore.new()
	i64_t := ts.get_int(64)
	f128_t := ts.get_float(128)
	sseup_t := ts.register(ssa.Type{
		kind:   .struct_t
		fields: [f128_t]
	})
	layout := x64_abi_sseup_aggregate_layout()
	mut m := mir.Module{
		type_store: unsafe { *ts }
		values:     []mir.Value{len: 5}
		instrs:     []mir.Instruction{len: 2}
		blocks:     []mir.BasicBlock{len: 1}
		funcs:      []mir.Function{len: 1}
	}
	m.values[0] = mir.Value{
		id:    0
		typ:   i64_t
		kind:  .func_ref
		name:  'callee'
		index: 0
	}
	m.values[1] = mir.Value{
		id:    1
		typ:   sseup_t
		kind:  .argument
		name:  'vector'
		index: 0
	}
	m.values[2] = mir.Value{
		id:    2
		typ:   i64_t
		kind:  .instruction
		name:  'call'
		index: 0
	}
	m.values[3] = mir.Value{
		id:    3
		typ:   0
		kind:  .instruction
		name:  'ret'
		index: 1
	}
	m.values[4] = mir.Value{
		id:    4
		typ:   0
		kind:  .basic_block
		name:  'entry'
		index: 0
	}
	m.instrs[0] = mir.Instruction{
		op:              .call
		operands:        [ssa.ValueID(0), 1]
		typ:             i64_t
		block:           0
		abi_arg_class:   [.in_reg]
		abi_arg_classes: [layout.value_class]
		abi_arg_layouts: [layout]
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
		id:                0
		name:              'caller'
		typ:               0
		blocks:            [ssa.BlockID(0)]
		params:            [ssa.ValueID(1)]
		abi_param_classes: [layout.value_class]
		abi_param_layouts: [layout]
	}
	return m
}

fn new_x64_abi_sse_pair_return_call_module() mir.Module {
	mut ts := ssa.TypeStore.new()
	f64_t := ts.get_float(64)
	pair_t := ts.register(ssa.Type{
		kind:   .struct_t
		fields: [f64_t, f64_t]
	})
	mut m := mir.Module{
		type_store: unsafe { *ts }
		values:     []mir.Value{len: 4}
		instrs:     []mir.Instruction{len: 2}
		blocks:     []mir.BasicBlock{len: 1}
		funcs:      []mir.Function{len: 1}
	}
	m.values[0] = mir.Value{
		id:    0
		typ:   pair_t
		kind:  .func_ref
		name:  'callee'
		index: 0
	}
	m.values[1] = mir.Value{
		id:    1
		typ:   pair_t
		kind:  .instruction
		name:  'pair'
		index: 0
	}
	m.values[2] = mir.Value{
		id:    2
		typ:   0
		kind:  .instruction
		name:  'ret'
		index: 1
	}
	m.values[3] = mir.Value{
		id:    3
		typ:   0
		kind:  .basic_block
		name:  'entry'
		index: 0
	}
	m.instrs[0] = mir.Instruction{
		op:            .call
		operands:      [ssa.ValueID(0)]
		typ:           pair_t
		block:         0
		abi_ret_class: x64_abi_sse_pair_class()
	}
	m.instrs[1] = mir.Instruction{
		op:       .ret
		operands: []ssa.ValueID{}
		typ:      0
		block:    0
	}
	m.blocks[0] = mir.BasicBlock{
		id:     0
		val_id: 3
		name:   'entry'
		parent: 0
		instrs: [ssa.ValueID(1), 2]
	}
	m.funcs[0] = mir.Function{
		id:     0
		name:   'caller'
		typ:    0
		blocks: [ssa.BlockID(0)]
	}
	return m
}

fn new_x64_abi_sseup_return_call_module() mir.Module {
	mut ts := ssa.TypeStore.new()
	f128_t := ts.get_float(128)
	sseup_t := ts.register(ssa.Type{
		kind:   .struct_t
		fields: [f128_t]
	})
	mut m := mir.Module{
		type_store: unsafe { *ts }
		values:     []mir.Value{len: 4}
		instrs:     []mir.Instruction{len: 2}
		blocks:     []mir.BasicBlock{len: 1}
		funcs:      []mir.Function{len: 1}
	}
	m.values[0] = mir.Value{
		id:    0
		typ:   sseup_t
		kind:  .func_ref
		name:  'callee'
		index: 0
	}
	m.values[1] = mir.Value{
		id:    1
		typ:   sseup_t
		kind:  .instruction
		name:  'vector'
		index: 0
	}
	m.values[2] = mir.Value{
		id:    2
		typ:   0
		kind:  .instruction
		name:  'ret'
		index: 1
	}
	m.values[3] = mir.Value{
		id:    3
		typ:   0
		kind:  .basic_block
		name:  'entry'
		index: 0
	}
	m.instrs[0] = mir.Instruction{
		op:            .call
		operands:      [ssa.ValueID(0)]
		typ:           sseup_t
		block:         0
		abi_ret_class: x64_abi_sseup_aggregate_class()
	}
	m.instrs[1] = mir.Instruction{
		op:       .ret
		operands: []ssa.ValueID{}
		typ:      0
		block:    0
	}
	m.blocks[0] = mir.BasicBlock{
		id:     0
		val_id: 3
		name:   'entry'
		parent: 0
		instrs: [ssa.ValueID(1), 2]
	}
	m.funcs[0] = mir.Function{
		id:     0
		name:   'caller'
		typ:    0
		blocks: [ssa.BlockID(0)]
	}
	return m
}

fn new_x64_abi_mixed_return_callee_module() mir.Module {
	mut ts := ssa.TypeStore.new()
	i64_t := ts.get_int(64)
	f64_t := ts.get_float(64)
	mixed_t := ts.register(ssa.Type{
		kind:   .struct_t
		fields: [i64_t, f64_t]
	})
	layout := x64_abi_mixed_aggregate_layout()
	mut m := mir.Module{
		type_store: unsafe { *ts }
		values:     []mir.Value{len: 4}
		instrs:     []mir.Instruction{len: 1}
		blocks:     []mir.BasicBlock{len: 1}
		funcs:      []mir.Function{len: 1}
	}
	m.values[0] = mir.Value{
		id:    0
		typ:   i64_t
		kind:  .constant
		name:  '0'
		index: 0
	}
	m.values[1] = mir.Value{
		id:    1
		typ:   mixed_t
		kind:  .argument
		name:  'mixed'
		index: 0
	}
	m.values[2] = mir.Value{
		id:    2
		typ:   0
		kind:  .instruction
		name:  'ret'
		index: 0
	}
	m.values[3] = mir.Value{
		id:    3
		typ:   0
		kind:  .basic_block
		name:  'entry'
		index: 0
	}
	m.instrs[0] = mir.Instruction{
		op:       .ret
		operands: [ssa.ValueID(1)]
		typ:      0
		block:    0
	}
	m.blocks[0] = mir.BasicBlock{
		id:     0
		val_id: 3
		name:   'entry'
		parent: 0
		instrs: [ssa.ValueID(2)]
	}
	m.funcs[0] = mir.Function{
		id:                0
		name:              'callee'
		typ:               mixed_t
		blocks:            [ssa.BlockID(0)]
		params:            [ssa.ValueID(1)]
		abi_ret_class:     layout.value_class
		abi_param_classes: [layout.value_class]
		abi_param_layouts: [layout]
	}
	return m
}

fn new_x64_abi_sseup_return_callee_module() mir.Module {
	mut ts := ssa.TypeStore.new()
	i64_t := ts.get_int(64)
	f128_t := ts.get_float(128)
	sseup_t := ts.register(ssa.Type{
		kind:   .struct_t
		fields: [f128_t]
	})
	layout := x64_abi_sseup_aggregate_layout()
	mut m := mir.Module{
		type_store: unsafe { *ts }
		values:     []mir.Value{len: 4}
		instrs:     []mir.Instruction{len: 1}
		blocks:     []mir.BasicBlock{len: 1}
		funcs:      []mir.Function{len: 1}
	}
	m.values[0] = mir.Value{
		id:    0
		typ:   i64_t
		kind:  .constant
		name:  '0'
		index: 0
	}
	m.values[1] = mir.Value{
		id:    1
		typ:   sseup_t
		kind:  .argument
		name:  'vector'
		index: 0
	}
	m.values[2] = mir.Value{
		id:    2
		typ:   0
		kind:  .instruction
		name:  'ret'
		index: 0
	}
	m.values[3] = mir.Value{
		id:    3
		typ:   0
		kind:  .basic_block
		name:  'entry'
		index: 0
	}
	m.instrs[0] = mir.Instruction{
		op:       .ret
		operands: [ssa.ValueID(1)]
		typ:      0
		block:    0
	}
	m.blocks[0] = mir.BasicBlock{
		id:     0
		val_id: 3
		name:   'entry'
		parent: 0
		instrs: [ssa.ValueID(2)]
	}
	m.funcs[0] = mir.Function{
		id:                0
		name:              'callee'
		typ:               sseup_t
		blocks:            [ssa.BlockID(0)]
		params:            [ssa.ValueID(1)]
		abi_ret_class:     layout.value_class
		abi_param_classes: [layout.value_class]
		abi_param_layouts: [layout]
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
