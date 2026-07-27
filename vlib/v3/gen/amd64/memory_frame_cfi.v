module amd64

const memory_frame_cfi_max_prologue_ops = 13
const memory_frame_cfi_max_epilogue_ops = 7
const memory_frame_cfi_max_total_ops = 20
const memory_frame_cfi_dwarf_code_alignment = u8(1)
const memory_frame_cfi_dwarf_data_alignment = i8(-8)
const memory_frame_cfi_dwarf_rsp_register = u8(7)
const memory_frame_cfi_dwarf_rip_register = u8(16)
const memory_frame_cfi_entry_cfa_offset_bytes = u64(8)
const memory_frame_cfi_return_cfa_displacement_bytes = i64(-8)

pub enum MemoryFrameCfiDisposition {
	windows_none
	dwarf_zero_delta
	dwarf_transitions
}

pub enum MemoryFrameCfiRegion {
	prologue
	epilogue_template
}

pub enum MemoryFrameCfiPhase {
	after_push
	after_allocation
	after_add
	after_pop
}

pub enum MemoryFrameCfiOpKind {
	def_cfa_offset
	offset_register
}

pub struct MemoryFrameCfiBaseline {
pub:
	present                               bool
	code_alignment_factor                 u8
	data_alignment_factor                 i8
	cfa_register_number                   u8
	cfa_offset_bytes                      u64
	return_address_register_number        u8
	return_address_cfa_displacement_bytes i64
}

pub struct MemoryFrameCfiOp {
pub:
	region                       MemoryFrameCfiRegion
	phase                        MemoryFrameCfiPhase
	instruction_end_offset_bytes u8
	same_pc_ordinal              u8
	kind                         MemoryFrameCfiOpKind
	cfa_offset_bytes             u64
	register_present             bool
	register                     MemorySavedGpr
	dwarf_register_number        u8
	saved_cfa_displacement_bytes i64
}

pub struct MemoryFrameCfiPlan {
pub:
	frame                 MemorySavedFramePlan
	disposition           MemoryFrameCfiDisposition
	baseline              MemoryFrameCfiBaseline
	prologue_ops          []MemoryFrameCfiOp
	epilogue_template_ops []MemoryFrameCfiOp
}

struct MemoryFrameCfiPreflight {
	disposition       MemoryFrameCfiDisposition
	prologue_ops      int
	epilogue_ops      int
	epilogue_add_end  u8
}

fn memory_frame_cfi_error(message string) IError {
	return error('amd64 memory frame cfi: ${message}')
}

fn memory_frame_cfi_checked_add(left u64, right u64) !u64 {
	if left > max_u64 - right {
		return memory_frame_cfi_error('arithmetic overflow')
	}
	return left + right
}

fn memory_frame_cfi_profile_is_dwarf(profile TargetProfile) bool {
	return profile == .linux_x86_64_sysv_elf || profile == .macos_x86_64_sysv_macho
}

fn memory_frame_cfi_dwarf_register(register MemorySavedGpr) u8 {
	return match register {
		.rbx { 3 }
		.rbp { 6 }
		.rsi { 4 }
		.rdi { 5 }
		.r12 { 12 }
		.r13 { 13 }
		.r14 { 14 }
		.r15 { 15 }
	}
}

fn memory_frame_cfi_active_baseline() MemoryFrameCfiBaseline {
	return MemoryFrameCfiBaseline{
		present:                               true
		code_alignment_factor:                 memory_frame_cfi_dwarf_code_alignment
		data_alignment_factor:                 memory_frame_cfi_dwarf_data_alignment
		cfa_register_number:                   memory_frame_cfi_dwarf_rsp_register
		cfa_offset_bytes:                      memory_frame_cfi_entry_cfa_offset_bytes
		return_address_register_number:        memory_frame_cfi_dwarf_rip_register
		return_address_cfa_displacement_bytes: memory_frame_cfi_return_cfa_displacement_bytes
	}
}

fn memory_frame_cfi_def_op(region MemoryFrameCfiRegion, phase MemoryFrameCfiPhase, instruction_end u8, cfa_offset u64) MemoryFrameCfiOp {
	return MemoryFrameCfiOp{
		region:                       region
		phase:                        phase
		instruction_end_offset_bytes: instruction_end
		same_pc_ordinal:              0
		kind:                         .def_cfa_offset
		cfa_offset_bytes:             cfa_offset
		register:                     .rbx
	}
}

fn memory_frame_cfi_offset_op(instruction_end u8, cfa_offset u64, register MemorySavedGpr) MemoryFrameCfiOp {
	return MemoryFrameCfiOp{
		region:                       .prologue
		phase:                        .after_push
		instruction_end_offset_bytes: instruction_end
		same_pc_ordinal:              1
		kind:                         .offset_register
		cfa_offset_bytes:             cfa_offset
		register_present:             true
		register:                     register
		dwarf_register_number:        memory_frame_cfi_dwarf_register(register)
		saved_cfa_displacement_bytes: -i64(cfa_offset)
	}
}

fn memory_frame_cfi_validate_saved_slot_disjointness(frame &MemorySavedFramePlan) ! {
	if frame.saves.len == 0 {
		return
	}
	if frame.layout_frame.red_zone_policy != .forbidden || frame.layout_frame.uses_red_zone {
		return memory_frame_cfi_error('saved-register frame must forbid the red zone')
	}
	if frame.total_stack_extent_bytes > u64(max_i64) {
		return memory_frame_cfi_error('stack extent exceeds signed coordinate domain')
	}
	save_floor := -i64(frame.saves.len * 8)
	total := i64(frame.total_stack_extent_bytes)
	for slot in frame.slots {
		if slot.placement.basis != .body_rsp || slot.address.basis != .body_rsp {
			return memory_frame_cfi_error('saved-register frame contains a non-body slot')
		}
		if slot.placement.size_bytes > u64(max_i64) {
			return memory_frame_cfi_error('slot size exceeds signed coordinate domain')
		}
		start := i64(slot.address.displacement_bytes) - total
		size := i64(slot.placement.size_bytes)
		if start > max_i64 - size {
			return memory_frame_cfi_error('slot interval overflow')
		}
		end := start + size
		if end > save_floor {
			return memory_frame_cfi_error('slot overlaps private saved-register storage')
		}
	}
}

fn memory_frame_cfi_validate_frame_geometry(frame &MemorySavedFramePlan) !u8 {
	if frame.entry_cfa_offset_bytes != memory_frame_cfi_entry_cfa_offset_bytes {
		return memory_frame_cfi_error('entry CFA invariant failed')
	}
	expected_body_cfa := memory_frame_cfi_checked_add(frame.total_stack_extent_bytes,
		memory_frame_cfi_entry_cfa_offset_bytes)!
	if frame.body_cfa_offset_bytes != expected_body_cfa {
		return memory_frame_cfi_error('body CFA invariant failed')
	}
	if frame.saves.len != int(frame.save_push_count) {
		return memory_frame_cfi_error('save count invariant failed')
	}
	mut push_end := u8(0)
	for save in frame.saves {
		if save.push_offset_bytes != push_end
			|| save.push_end_offset_bytes != save.push_offset_bytes + save.push_width_bytes {
			return memory_frame_cfi_error('push template is not contiguous')
		}
		push_end = save.push_end_offset_bytes
	}
	if frame.allocation_bytes == 0 {
		if frame.body_offset_bytes != push_end {
			return memory_frame_cfi_error('zero-allocation body offset invariant failed')
		}
	} else if frame.body_offset_bytes <= push_end {
		return memory_frame_cfi_error('allocation instruction extent invariant failed')
	}
	if int(frame.body_offset_bytes) != frame.prologue_bytes.len {
		return memory_frame_cfi_error('prologue extent invariant failed')
	}

	mut epilogue_cursor := if frame.allocation_bytes == 0 {
		u8(0)
	} else if frame.saves.len == 0 {
		u8(frame.epilogue_bytes.len)
	} else {
		frame.saves[frame.saves.len - 1].pop_offset_bytes
	}
	allocation_end := epilogue_cursor
	if frame.allocation_bytes == 0 && allocation_end != 0 {
		return memory_frame_cfi_error('zero allocation has an ADD extent')
	}
	if frame.allocation_bytes != 0 && allocation_end == 0 {
		return memory_frame_cfi_error('allocation is missing its ADD extent')
	}
	for reverse_index in 0 .. frame.saves.len {
		index := frame.saves.len - 1 - reverse_index
		save := frame.saves[index]
		if save.pop_offset_bytes != epilogue_cursor
			|| save.pop_end_offset_bytes != save.pop_offset_bytes + save.pop_width_bytes
			|| save.pop_width_bytes != save.push_width_bytes {
			return memory_frame_cfi_error('pop template is not contiguous')
		}
		epilogue_cursor = save.pop_end_offset_bytes
	}
	if int(epilogue_cursor) != frame.epilogue_bytes.len {
		return memory_frame_cfi_error('epilogue extent invariant failed')
	}
	memory_frame_cfi_validate_saved_slot_disjointness(frame)!
	return allocation_end
}

fn memory_frame_cfi_preflight(frame &MemorySavedFramePlan) !MemoryFrameCfiPreflight {
	epilogue_add_end := memory_frame_cfi_validate_frame_geometry(frame)!
	if frame.layout_frame.profile == .windows_x86_64_microsoft_abi_coff {
		return MemoryFrameCfiPreflight{
			disposition:      .windows_none
			epilogue_add_end: epilogue_add_end
		}
	}
	if !memory_frame_cfi_profile_is_dwarf(frame.layout_frame.profile) {
		return memory_frame_cfi_error('unsupported target profile')
	}
	if frame.saves.len > 6 {
		return memory_frame_cfi_error('DWARF save count exceeds six')
	}
	has_allocation := frame.allocation_bytes != 0
	prologue_ops := frame.saves.len * 2 + if has_allocation { 1 } else { 0 }
	epilogue_ops := frame.saves.len + if has_allocation { 1 } else { 0 }
	if prologue_ops > memory_frame_cfi_max_prologue_ops {
		return memory_frame_cfi_error('prologue operation cap exceeded')
	}
	if epilogue_ops > memory_frame_cfi_max_epilogue_ops {
		return memory_frame_cfi_error('epilogue operation cap exceeded')
	}
	if prologue_ops + epilogue_ops > memory_frame_cfi_max_total_ops {
		return memory_frame_cfi_error('total operation cap exceeded')
	}
	return MemoryFrameCfiPreflight{
		disposition: if prologue_ops == 0 && epilogue_ops == 0 {
			MemoryFrameCfiDisposition.dwarf_zero_delta
		} else {
			MemoryFrameCfiDisposition.dwarf_transitions
		}
		prologue_ops:     prologue_ops
		epilogue_ops:     epilogue_ops
		epilogue_add_end: epilogue_add_end
	}
}

fn memory_frame_cfi_validate_output(plan &MemoryFrameCfiPlan, preflight MemoryFrameCfiPreflight) ! {
	if plan.disposition != preflight.disposition
		|| plan.prologue_ops.len != preflight.prologue_ops
		|| plan.epilogue_template_ops.len != preflight.epilogue_ops {
		return memory_frame_cfi_error('output count invariant failed')
	}
	if plan.disposition == .windows_none {
		if plan.baseline != MemoryFrameCfiBaseline{} || plan.prologue_ops.len != 0
			|| plan.epilogue_template_ops.len != 0 {
			return memory_frame_cfi_error('Windows-none output invariant failed')
		}
		return
	}
	if plan.baseline != memory_frame_cfi_active_baseline() {
		return memory_frame_cfi_error('DWARF baseline invariant failed')
	}
	if plan.disposition == .dwarf_zero_delta {
		if plan.frame.saves.len != 0 || plan.frame.allocation_bytes != 0 {
			return memory_frame_cfi_error('zero-delta disposition invariant failed')
		}
		return
	}

	mut prologue_index := 0
	mut cfa_offset := memory_frame_cfi_entry_cfa_offset_bytes
	for save in plan.frame.saves {
		cfa_offset = memory_frame_cfi_checked_add(cfa_offset, 8)!
		expected_def := memory_frame_cfi_def_op(.prologue, .after_push,
			save.push_end_offset_bytes, cfa_offset)
		expected_offset := memory_frame_cfi_offset_op(save.push_end_offset_bytes,
			cfa_offset, save.register)
		if plan.prologue_ops[prologue_index] != expected_def
			|| plan.prologue_ops[prologue_index + 1] != expected_offset {
			return memory_frame_cfi_error('push operation invariant failed')
		}
		prologue_index += 2
	}
	if plan.frame.allocation_bytes != 0 {
		expected := memory_frame_cfi_def_op(.prologue, .after_allocation,
			plan.frame.body_offset_bytes, plan.frame.body_cfa_offset_bytes)
		if plan.prologue_ops[prologue_index] != expected {
			return memory_frame_cfi_error('allocation operation invariant failed')
		}
		prologue_index++
	}

	mut epilogue_index := 0
	mut epilogue_cfa := plan.frame.body_cfa_offset_bytes
	if plan.frame.allocation_bytes != 0 {
		epilogue_cfa = memory_frame_cfi_checked_add(u64(plan.frame.saves.len * 8),
			memory_frame_cfi_entry_cfa_offset_bytes)!
		expected := memory_frame_cfi_def_op(.epilogue_template, .after_add,
			preflight.epilogue_add_end, epilogue_cfa)
		if plan.epilogue_template_ops[epilogue_index] != expected {
			return memory_frame_cfi_error('ADD operation invariant failed')
		}
		epilogue_index++
	}
	for reverse_index in 0 .. plan.frame.saves.len {
		index := plan.frame.saves.len - 1 - reverse_index
		save := plan.frame.saves[index]
		if epilogue_cfa < 8 {
			return memory_frame_cfi_error('epilogue CFA underflow')
		}
		epilogue_cfa -= 8
		expected := memory_frame_cfi_def_op(.epilogue_template, .after_pop,
			save.pop_end_offset_bytes, epilogue_cfa)
		if plan.epilogue_template_ops[epilogue_index] != expected {
			return memory_frame_cfi_error('POP operation invariant failed')
		}
		epilogue_index++
	}
	if prologue_index != plan.prologue_ops.len
		|| epilogue_index != plan.epilogue_template_ops.len
		|| epilogue_cfa != memory_frame_cfi_entry_cfa_offset_bytes {
		return memory_frame_cfi_error('operation replay invariant failed')
	}
}

// plan_memory_saved_frame_cfi produces an inert semantic CFI snapshot.
pub fn plan_memory_saved_frame_cfi(facts &MemoryFunctionFrameFacts, saves &MemoryCalleeSaveFacts) !MemoryFrameCfiPlan {
	frame := plan_memory_saved_frame(facts, saves)!
	preflight := memory_frame_cfi_preflight(&frame)!
	if preflight.disposition == .windows_none {
		result := MemoryFrameCfiPlan{
			frame:                 frame
			disposition:           .windows_none
			prologue_ops:          []MemoryFrameCfiOp{}
			epilogue_template_ops: []MemoryFrameCfiOp{}
		}
		memory_frame_cfi_validate_output(&result, preflight)!
		return result
	}

	mut prologue_ops := []MemoryFrameCfiOp{cap: preflight.prologue_ops}
	mut cfa_offset := memory_frame_cfi_entry_cfa_offset_bytes
	for save in frame.saves {
		cfa_offset = memory_frame_cfi_checked_add(cfa_offset, 8)!
		prologue_ops << memory_frame_cfi_def_op(.prologue, .after_push,
			save.push_end_offset_bytes, cfa_offset)
		prologue_ops << memory_frame_cfi_offset_op(save.push_end_offset_bytes,
			cfa_offset, save.register)
	}
	if frame.allocation_bytes != 0 {
		prologue_ops << memory_frame_cfi_def_op(.prologue, .after_allocation,
			frame.body_offset_bytes, frame.body_cfa_offset_bytes)
	}

	mut epilogue_ops := []MemoryFrameCfiOp{cap: preflight.epilogue_ops}
	mut epilogue_cfa := frame.body_cfa_offset_bytes
	if frame.allocation_bytes != 0 {
		epilogue_cfa = memory_frame_cfi_checked_add(u64(frame.saves.len * 8),
			memory_frame_cfi_entry_cfa_offset_bytes)!
		epilogue_ops << memory_frame_cfi_def_op(.epilogue_template, .after_add,
			preflight.epilogue_add_end, epilogue_cfa)
	}
	for reverse_index in 0 .. frame.saves.len {
		index := frame.saves.len - 1 - reverse_index
		epilogue_cfa -= 8
		epilogue_ops << memory_frame_cfi_def_op(.epilogue_template, .after_pop,
			frame.saves[index].pop_end_offset_bytes, epilogue_cfa)
	}

	result := MemoryFrameCfiPlan{
		frame:                 frame
		disposition:           preflight.disposition
		baseline:              memory_frame_cfi_active_baseline()
		prologue_ops:          prologue_ops
		epilogue_template_ops: epilogue_ops
	}
	memory_frame_cfi_validate_output(&result, preflight)!
	return result
}
