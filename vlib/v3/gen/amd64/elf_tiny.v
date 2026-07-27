// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module amd64

const elf_tiny_base_vaddr = u64(0x400000)
const elf_tiny_page_align = u64(0x1000)
const elf_tiny_header_size = u64(64)
const elf_tiny_program_header_size = u64(56)
const elf_tiny_start_size = u64(18)
const elf_tiny_start_call_field_offset = u64(3)
const elf_tiny_sys_exit_group = u32(231)

const elf_tiny_et_exec = u16(2)
const elf_tiny_em_x86_64 = u16(62)
const elf_tiny_pt_load = u32(1)
const elf_tiny_pt_gnu_stack = u32(0x6474e551)
const elf_tiny_pf_x = u32(0x1)
const elf_tiny_pf_w = u32(0x2)
const elf_tiny_pf_r = u32(0x4)

enum ElfTinyEntryResultPolicy {
	void_
	scalar
}

// ElfTinyEntryDefinition is supplied explicitly by the backend producer. It
// deliberately carries no symbol-name, reachability, argv, init, or runtime
// policy. Stage B must map a no-result entry to .void_ and an accepted scalar
// result to .scalar from immutable lowering metadata; it must reject every
// other result shape before calling this writer.
struct ElfTinyEntryDefinition {
	function_index  u32
	parameter_count u32
	result_policy   ElfTinyEntryResultPolicy
}

struct ElfTinyLayout {
	program_header_count u16
	entry_offset         u64
	entry_vaddr          u64
	object_text_offset   u64
	object_text_vaddr    u64
	rx_file_size         u64
	data_offset          u64
	data_vaddr           u64
	file_size            u64
}

fn elf_tiny_validate_entry_result_policy(policy ElfTinyEntryResultPolicy) ! {
	if policy !in [.void_, .scalar] {
		return error('ELF tiny entry result policy ${int(policy)} is unsupported')
	}
}

fn elf_tiny_checked_rel32(field_vaddr u64, target_vaddr u64) !u32 {
	next_instruction := elf64_checked_add(field_vaddr, 4, 'tiny CALL next instruction')!
	if target_vaddr >= next_instruction {
		displacement := target_vaddr - next_instruction
		if displacement > u64(2_147_483_647) {
			return error('ELF tiny CALL displacement is outside signed 32-bit range')
		}
		return u32(displacement)
	}
	magnitude := next_instruction - target_vaddr
	if magnitude > u64(2_147_483_648) {
		return error('ELF tiny CALL displacement is outside signed 32-bit range')
	}
	displacement := -i64(magnitude)
	return u32(displacement & i64(0xffff_ffff))
}

fn elf_tiny_write_u32_at(mut data []u8, offset u64, value u32) ! {
	if offset > u64(data.len) || u64(4) > u64(data.len) - offset {
		return error('ELF tiny rel32 field ${offset} is outside output text size ${data.len}')
	}
	index := int(offset)
	data[index] = u8(value)
	data[index + 1] = u8(value >> 8)
	data[index + 2] = u8(value >> 16)
	data[index + 3] = u8(value >> 24)
}

fn elf_tiny_patch_rel32(mut text []u8, field_offset u64, text_vaddr u64, target_offset u64) ! {
	field_vaddr := elf64_checked_add(text_vaddr, field_offset, 'tiny CALL field address')!
	target_vaddr := elf64_checked_add(text_vaddr, target_offset, 'tiny CALL target address')!
	displacement := elf_tiny_checked_rel32(field_vaddr, target_vaddr)!
	elf_tiny_write_u32_at(mut text, field_offset, displacement)!
}

fn elf_tiny_validate_layout(layout &ElfTinyLayout, object_text_size u64, data_size u64) ! {
	expected_program_headers := if data_size == 0 { u16(2) } else { u16(3) }
	if layout.program_header_count != expected_program_headers {
		return error('ELF tiny layout has ${layout.program_header_count} program headers, expected ${expected_program_headers}')
	}
	program_header_bytes := elf64_checked_mul(u64(expected_program_headers),
		elf_tiny_program_header_size, 'tiny program header table size')!
	program_header_end := elf64_checked_add(elf_tiny_header_size, program_header_bytes,
		'tiny program header extent')!
	expected_entry_offset := elf64_align(program_header_end, 16, 'tiny entry offset')!
	if layout.entry_offset != expected_entry_offset {
		return error('ELF tiny layout entry offset ${layout.entry_offset} does not match ${expected_entry_offset}')
	}
	expected_entry_vaddr := elf64_checked_add(elf_tiny_base_vaddr, expected_entry_offset,
		'tiny entry address')!
	if layout.entry_vaddr != expected_entry_vaddr {
		return error('ELF tiny layout entry address ${layout.entry_vaddr} does not match ${expected_entry_vaddr}')
	}
	entry_end := elf64_checked_add(expected_entry_offset, elf_tiny_start_size, 'tiny entry extent')!
	expected_object_text_offset := elf64_align(entry_end, 16, 'tiny object text offset')!
	if layout.object_text_offset != expected_object_text_offset {
		return error('ELF tiny layout object text offset ${layout.object_text_offset} does not match ${expected_object_text_offset}')
	}
	expected_object_text_vaddr := elf64_checked_add(elf_tiny_base_vaddr,
		expected_object_text_offset, 'tiny object text address')!
	if layout.object_text_vaddr != expected_object_text_vaddr {
		return error('ELF tiny layout object text address ${layout.object_text_vaddr} does not match ${expected_object_text_vaddr}')
	}
	expected_rx_file_size := elf64_checked_add(expected_object_text_offset, object_text_size,
		'tiny RX extent')!
	if layout.rx_file_size != expected_rx_file_size {
		return error('ELF tiny layout RX size ${layout.rx_file_size} does not match ${expected_rx_file_size}')
	}
	_ = elf64_checked_add(elf_tiny_base_vaddr, expected_rx_file_size, 'tiny RX address extent')!
	if data_size == 0 {
		if layout.data_offset != 0 || layout.data_vaddr != 0 {
			return error('ELF tiny layout without private data has a data segment')
		}
		if layout.file_size != expected_rx_file_size {
			return error('ELF tiny layout file size ${layout.file_size} does not match ${expected_rx_file_size}')
		}
	} else {
		expected_data_offset := elf64_align(expected_rx_file_size, elf_tiny_page_align,
			'tiny data offset')!
		if layout.data_offset != expected_data_offset {
			return error('ELF tiny layout data offset ${layout.data_offset} does not match ${expected_data_offset}')
		}
		expected_data_vaddr := elf64_checked_add(elf_tiny_base_vaddr, expected_data_offset,
			'tiny data address')!
		if layout.data_vaddr != expected_data_vaddr {
			return error('ELF tiny layout data address ${layout.data_vaddr} does not match ${expected_data_vaddr}')
		}
		if layout.data_offset % elf_tiny_page_align != layout.data_vaddr % elf_tiny_page_align {
			return error('ELF tiny layout data offset and address are not page-congruent')
		}
		expected_file_size := elf64_checked_add(expected_data_offset, data_size, 'tiny file size')!
		if layout.file_size != expected_file_size {
			return error('ELF tiny layout file size ${layout.file_size} does not match ${expected_file_size}')
		}
		_ = elf64_checked_add(expected_data_vaddr, data_size, 'tiny data address extent')!
	}
	_ = elf64_checked_host_size(layout.file_size)!
}

fn elf_tiny_build_layout(object_text_size u64, data_size u64) !ElfTinyLayout {
	program_header_count := if data_size == 0 { u16(2) } else { u16(3) }
	program_header_bytes := elf64_checked_mul(u64(program_header_count),
		elf_tiny_program_header_size, 'tiny program header table size')!
	program_header_end := elf64_checked_add(elf_tiny_header_size, program_header_bytes,
		'tiny program header extent')!
	entry_offset := elf64_align(program_header_end, 16, 'tiny entry offset')!
	entry_vaddr := elf64_checked_add(elf_tiny_base_vaddr, entry_offset, 'tiny entry address')!
	entry_end := elf64_checked_add(entry_offset, elf_tiny_start_size, 'tiny entry extent')!
	object_text_offset := elf64_align(entry_end, 16, 'tiny object text offset')!
	object_text_vaddr := elf64_checked_add(elf_tiny_base_vaddr, object_text_offset,
		'tiny object text address')!
	rx_file_size := elf64_checked_add(object_text_offset, object_text_size, 'tiny RX extent')!
	data_offset := if data_size == 0 {
		u64(0)
	} else {
		elf64_align(rx_file_size, elf_tiny_page_align, 'tiny data offset')!
	}
	data_vaddr := if data_size == 0 {
		u64(0)
	} else {
		elf64_checked_add(elf_tiny_base_vaddr, data_offset, 'tiny data address')!
	}
	file_size := if data_size == 0 {
		rx_file_size
	} else {
		elf64_checked_add(data_offset, data_size, 'tiny file size')!
	}
	layout := ElfTinyLayout{
		program_header_count: program_header_count
		entry_offset:         entry_offset
		entry_vaddr:          entry_vaddr
		object_text_offset:   object_text_offset
		object_text_vaddr:    object_text_vaddr
		rx_file_size:         rx_file_size
		data_offset:          data_offset
		data_vaddr:           data_vaddr
		file_size:            file_size
	}
	elf_tiny_validate_layout(&layout, object_text_size, data_size)!
	return layout
}

fn elf_tiny_write_header(mut output []u8, layout &ElfTinyLayout) {
	output << [u8(0x7f), 0x45, 0x4c, 0x46, 0x02, 0x01, 0x01, 0x00]
	for _ in 0 .. 8 {
		output << u8(0)
	}
	elf64_write_u16(mut output, elf_tiny_et_exec)
	elf64_write_u16(mut output, elf_tiny_em_x86_64)
	elf64_write_u32(mut output, u32(1))
	elf64_write_u64(mut output, layout.entry_vaddr)
	elf64_write_u64(mut output, elf_tiny_header_size)
	elf64_write_u64(mut output, u64(0))
	elf64_write_u32(mut output, u32(0))
	elf64_write_u16(mut output, u16(elf_tiny_header_size))
	elf64_write_u16(mut output, u16(elf_tiny_program_header_size))
	elf64_write_u16(mut output, layout.program_header_count)
	elf64_write_u16(mut output, u16(0))
	elf64_write_u16(mut output, u16(0))
	elf64_write_u16(mut output, u16(0))
}

fn elf_tiny_write_program_header(mut output []u8, type_ u32, flags u32, offset u64, vaddr u64, filesz u64, memsz u64, alignment u64) {
	elf64_write_u32(mut output, type_)
	elf64_write_u32(mut output, flags)
	elf64_write_u64(mut output, offset)
	elf64_write_u64(mut output, vaddr)
	elf64_write_u64(mut output, vaddr)
	elf64_write_u64(mut output, filesz)
	elf64_write_u64(mut output, memsz)
	elf64_write_u64(mut output, alignment)
}

fn elf_tiny_startup_bytes(result_policy ElfTinyEntryResultPolicy) []u8 {
	mut text := []u8{cap: int(elf_tiny_start_size)}
	text << [u8(0x31), 0xed, 0xe8, 0x00, 0x00, 0x00, 0x00]
	if result_policy == .scalar {
		text << [u8(0x89), 0xc7]
	} else {
		text << [u8(0x31), 0xff]
	}
	text << u8(0xb8)
	elf64_write_u32(mut text, elf_tiny_sys_exit_group)
	text << [u8(0x0f), 0x05, 0x0f, 0x0b]
	return text
}

// elf_tiny_executable_bytes consumes the canonical validated Object directly.
// It resolves only internal E8 rel32 calls and never infers entry/runtime policy.
fn elf_tiny_executable_bytes(o &Object, entry ElfTinyEntryDefinition) ![]u8 {
	elf_tiny_validate_entry_result_policy(entry.result_policy)!
	o.validate()!
	if entry.parameter_count != 0 {
		return error('ELF tiny entry function must not accept scalar parameters')
	}
	entry_id := SymbolID(entry.function_index)
	entry_index := object_symbol_index(o, entry_id) or {
		return error('ELF tiny entry function index ${entry.function_index} is out of range')
	}
	for symbol in o.symbols {
		if symbol.intentional_external {
			return error('ELF tiny executable does not support external function `${symbol.name}`')
		}
	}
	entry_symbol := o.symbols[entry_index]
	if entry_symbol.intentional_external || !entry_symbol.defined {
		return error('ELF tiny entry function index ${entry.function_index} is not a defined function')
	}

	layout := elf_tiny_build_layout(u64(o.text.len), u64(o.private_data.len))!
	mut linked_text := elf_tiny_startup_bytes(entry.result_policy)
	if u64(linked_text.len) != elf_tiny_start_size {
		return error('ELF tiny startup size ${linked_text.len} does not match ${elf_tiny_start_size}')
	}
	object_text_delta := layout.object_text_offset - layout.entry_offset
	elf64_pad_to(mut linked_text, object_text_delta)!
	linked_text << o.text
	entry_target_offset := elf64_checked_add(object_text_delta, entry_symbol.offset,
		'tiny entry target offset')!
	elf_tiny_patch_rel32(mut linked_text, elf_tiny_start_call_field_offset, layout.entry_vaddr,
		entry_target_offset)!
	for relocation in o.call_relocations {
		target_index := object_symbol_index(o, relocation.symbol_id)!
		target_symbol := o.symbols[target_index]
		if target_symbol.intentional_external || !target_symbol.defined {
			return error('ELF tiny CALL target `${target_symbol.name}` is not a defined internal function')
		}
		field_offset := elf64_checked_add(object_text_delta, relocation.offset,
			'tiny CALL field offset')!
		target_offset := elf64_checked_add(object_text_delta, target_symbol.offset,
			'tiny CALL target offset')!
		elf_tiny_patch_rel32(mut linked_text, field_offset, layout.entry_vaddr, target_offset)!
	}
	expected_linked_text_size := elf64_checked_add(object_text_delta, u64(o.text.len),
		'tiny linked text size')!
	if u64(linked_text.len) != expected_linked_text_size {
		return error('ELF tiny linked text size does not match validated layout')
	}

	mut output := []u8{cap: elf64_checked_host_size(layout.file_size)!}
	elf_tiny_write_header(mut output, &layout)
	elf_tiny_write_program_header(mut output, elf_tiny_pt_load, elf_tiny_pf_r | elf_tiny_pf_x, 0,
		elf_tiny_base_vaddr, layout.rx_file_size, layout.rx_file_size, elf_tiny_page_align)
	if o.private_data.len != 0 {
		elf_tiny_write_program_header(mut output, elf_tiny_pt_load, elf_tiny_pf_r | elf_tiny_pf_w,
			layout.data_offset, layout.data_vaddr, u64(o.private_data.len),
			u64(o.private_data.len), elf_tiny_page_align)
	}
	elf_tiny_write_program_header(mut output, elf_tiny_pt_gnu_stack, elf_tiny_pf_r | elf_tiny_pf_w,
		0, 0, 0, 0, 16)
	program_header_bytes := elf64_checked_mul(u64(layout.program_header_count),
		elf_tiny_program_header_size, 'tiny program header table size')!
	program_header_end := elf64_checked_add(elf_tiny_header_size, program_header_bytes,
		'tiny program header extent')!
	if u64(output.len) != program_header_end {
		return error('ELF tiny headers end at ${output.len}, expected ${program_header_end}')
	}
	elf64_pad_to(mut output, layout.entry_offset)!
	output << linked_text
	if o.private_data.len != 0 {
		elf64_pad_to(mut output, layout.data_offset)!
		output << o.private_data
	}
	if u64(output.len) != layout.file_size {
		return error('ELF tiny output size ${output.len} does not match ${layout.file_size}')
	}
	return output
}

const elf_tiny_runtime_alloc_cap = u64(0x8000_0000)
const elf_tiny_runtime_sys_write = u32(1)
const elf_tiny_runtime_sys_mmap = u32(9)

enum ElfTinyStartupPolicy {
	unknown
	no_args_no_init
}

enum ElfTinyRuntimeRole {
	unknown
	write_all
	flush_noop
	exit_group
	i32_decimal
	i64_decimal
	rune_utf8
	string_concat
}

struct ElfTinyRuntimeBinding {
	external_function_index u32
	role                    ElfTinyRuntimeRole
}

struct ElfTinyRuntimeDefinition {
	entry            ElfTinyEntryDefinition
	startup_policy   ElfTinyStartupPolicy
	runtime_bindings []ElfTinyRuntimeBinding
}

enum ElfTinyRuntimeImplementation {
	unknown
	write_all
	flush_noop
	exit_group
	mmap_alloc
	i64_decimal
	i32_decimal
	rune_utf8
	string_concat
}

struct ElfTinyRuntimeCrossFixup {
	field  u64
	target ElfTinyRuntimeImplementation
}

struct ElfTinyRuntimeManifest {
	bytes        []u8
	cross_fixups []ElfTinyRuntimeCrossFixup
}

struct ElfTinyRuntimeCallPlan {
	owner_index  int
	target_index int
	offset       u64
}

struct ElfTinyRuntimeReachability {
	functions []bool
	externals []bool
}

struct ElfTinyRuntimeDataRelocationPlan {
	owner_index    int
	target_index   int
	original_index int
	relocation     ObjectDataRelocation
}

struct ElfTinyRuntimeDataRun {
	section ObjectDataSectionKind
mut:
	old_start u64
	old_end   u64
	new_start u64
}

struct ElfTinyRuntimeDataPlan {
	sections    []ObjectDataSection
	symbols     []ObjectDataSymbol
	old_to_new  []ObjectDataSymbolID
	symbol_kept []bool
	relocations []ElfTinyRuntimeDataRelocationPlan
}

struct ElfTinyRuntimeLayout {
	program_header_count u16
	entry_offset         u64
	function_offset      u64
	text_end             u64
	rodata_offset        u64
	rx_file_size         u64
	rw_offset            u64
	rw_vaddr             u64
	object_data_base     u64
	bss_base             u64
	rw_file_size         u64
	rw_mem_size          u64
	file_size            u64
}

struct ElfTinyRuntimePlan {
	reachability        ElfTinyRuntimeReachability
	call_plans          []ElfTinyRuntimeCallPlan
	data_plan           ElfTinyRuntimeDataPlan
	selected_functions  []int
	role_by_symbol      []ElfTinyRuntimeRole
	implementation_used []bool
	manifests           []ElfTinyRuntimeManifest
	layout              ElfTinyRuntimeLayout
}

struct ElfTinyRuntimeLinkedText {
	bytes                  []u8
	function_offsets       []u64
	implementation_offsets []u64
}

fn elf_tiny_runtime_checked_add(left u64, right u64, label string) !u64 {
	if left > max_u64 - right {
		return error('ELF tiny runtime ${label} overflows u64')
	}
	return left + right
}

fn elf_tiny_runtime_checked_sub(left u64, right u64, label string) !u64 {
	if left < right {
		return error('ELF tiny runtime ${label} underflows u64')
	}
	return left - right
}

fn elf_tiny_runtime_checked_host_size(value u64, label string) !int {
	if value > u64(max_int) {
		return error('ELF tiny runtime ${label} exceeds the host array limit')
	}
	return int(value)
}

fn elf_tiny_runtime_align(value u64, alignment u64, label string) !u64 {
	if alignment == 0 || alignment > elf_tiny_page_align || alignment & (alignment - 1) != 0 {
		return error('ELF tiny runtime ${label} alignment ${alignment} is unsupported')
	}
	remainder := value % alignment
	if remainder == 0 {
		return value
	}
	return elf_tiny_runtime_checked_add(value, alignment - remainder, label)
}

fn elf_tiny_runtime_check_cap(value u64, label string) ! {
	if value > elf_tiny_runtime_alloc_cap {
		return error('ELF tiny runtime ${label} ${value} exceeds 0x80000000')
	}
}

fn elf_tiny_runtime_patch_local_rel32(mut bytes []u8, field int, target int) ! {
	if field < 0 || target < 0 || field + 4 > bytes.len || target > bytes.len {
		return error('ELF tiny runtime local rel32 fixup is outside its manifest')
	}
	displacement := i64(target) - i64(field + 4)
	if displacement < i64(min_i32) || displacement > i64(max_i32) {
		return error('ELF tiny runtime local rel32 displacement is outside signed i32')
	}
	raw := u32(i32(displacement))
	for index in 0 .. 4 {
		bytes[field + index] = u8(raw >> (index * 8))
	}
}

fn elf_tiny_runtime_emit_call(mut bytes []u8) u64 {
	bytes << u8(0xe8)
	field := u64(bytes.len)
	bytes << [u8(0), 0, 0, 0]
	return field
}

fn elf_tiny_runtime_emit_jcc(mut bytes []u8, opcode u8) int {
	bytes << [u8(0x0f), opcode]
	field := bytes.len
	bytes << [u8(0), 0, 0, 0]
	return field
}

fn elf_tiny_runtime_emit_jmp(mut bytes []u8) int {
	bytes << u8(0xe9)
	field := bytes.len
	bytes << [u8(0), 0, 0, 0]
	return field
}

fn elf_tiny_runtime_write_manifest() !ElfTinyRuntimeManifest {
	mut bytes := [
		u8(0x49),
		0x89,
		0xd0,
		0x49,
		0x89,
		0xf1,
		0x45,
		0x31,
		0xd2,
		0x4d,
		0x85,
		0xc0,
	]
	done_zero := elf_tiny_runtime_emit_jcc(mut bytes, 0x84)
	loop_start := bytes.len
	bytes << [
		u8(0xb8),
		0x01,
		0x00,
		0x00,
		0x00,
		0x4c,
		0x89,
		0xce,
		0x4c,
		0x89,
		0xc2,
		0x0f,
		0x05,
		0x48,
		0x85,
		0xc0,
	]
	progress := elf_tiny_runtime_emit_jcc(mut bytes, 0x8f)
	zero_progress := elf_tiny_runtime_emit_jcc(mut bytes, 0x84)
	bytes << [u8(0x48), 0x83, 0xf8, 0xfc]
	eintr := elf_tiny_runtime_emit_jcc(mut bytes, 0x84)
	terminal_error := elf_tiny_runtime_emit_jmp(mut bytes)
	progress_target := bytes.len
	bytes << [u8(0x4c), 0x39, 0xc0]
	over_report := elf_tiny_runtime_emit_jcc(mut bytes, 0x87)
	bytes << [u8(0x49), 0x01, 0xc1]
	pointer_carry := elf_tiny_runtime_emit_jcc(mut bytes, 0x82)
	bytes << [u8(0x49), 0x29, 0xc0, 0x49, 0x01, 0xc2]
	total_carry := elf_tiny_runtime_emit_jcc(mut bytes, 0x82)
	bytes << [u8(0x4d), 0x85, 0xc0]
	more := elf_tiny_runtime_emit_jcc(mut bytes, 0x85)
	done_target := bytes.len
	bytes << [u8(0x4c), 0x89, 0xd0, 0xc3]
	fail_target := bytes.len
	bytes << [u8(0xbf), 0x01, 0x00, 0x00, 0x00, 0x48, 0x83, 0xec, 0x08]
	exit_field := elf_tiny_runtime_emit_call(mut bytes)
	bytes << [u8(0x0f), 0x0b]
	elf_tiny_runtime_patch_local_rel32(mut bytes, done_zero, done_target)!
	elf_tiny_runtime_patch_local_rel32(mut bytes, progress, progress_target)!
	elf_tiny_runtime_patch_local_rel32(mut bytes, zero_progress, fail_target)!
	elf_tiny_runtime_patch_local_rel32(mut bytes, eintr, loop_start)!
	elf_tiny_runtime_patch_local_rel32(mut bytes, terminal_error, fail_target)!
	elf_tiny_runtime_patch_local_rel32(mut bytes, over_report, fail_target)!
	elf_tiny_runtime_patch_local_rel32(mut bytes, pointer_carry, fail_target)!
	elf_tiny_runtime_patch_local_rel32(mut bytes, total_carry, fail_target)!
	elf_tiny_runtime_patch_local_rel32(mut bytes, more, loop_start)!
	if bytes.len != 120 || exit_field != 114 {
		return error('ELF tiny runtime write manifest mismatch')
	}
	return ElfTinyRuntimeManifest{
		bytes:        bytes
		cross_fixups: [
			ElfTinyRuntimeCrossFixup{
				field:  exit_field
				target: .exit_group
			},
		]
	}
}

fn elf_tiny_runtime_flush_manifest() !ElfTinyRuntimeManifest {
	return ElfTinyRuntimeManifest{
		bytes: [u8(0x31), 0xc0, 0xc3]
	}
}

fn elf_tiny_runtime_exit_manifest() !ElfTinyRuntimeManifest {
	return ElfTinyRuntimeManifest{
		bytes: [u8(0xb8), 0xe7, 0x00, 0x00, 0x00, 0x0f, 0x05, 0x0f, 0x0b]
	}
}

fn elf_tiny_runtime_mmap_manifest() !ElfTinyRuntimeManifest {
	mut bytes := [
		u8(0x48),
		0x89,
		0xfe,
		0x31,
		0xff,
		0xba,
		0x03,
		0x00,
		0x00,
		0x00,
		0x41,
		0xba,
		0x22,
		0x00,
		0x00,
		0x00,
		0x49,
		0xc7,
		0xc0,
		0xff,
		0xff,
		0xff,
		0xff,
		0x45,
		0x31,
		0xc9,
		0xb8,
		0x09,
		0x00,
		0x00,
		0x00,
		0x0f,
		0x05,
		0x48,
		0x3d,
		0x01,
		0xf0,
		0xff,
		0xff,
	]
	raw_errno := elf_tiny_runtime_emit_jcc(mut bytes, 0x83)
	bytes << [u8(0x48), 0x85, 0xc0]
	null_mapping := elf_tiny_runtime_emit_jcc(mut bytes, 0x84)
	bytes << u8(0xc3)
	fail_target := bytes.len
	bytes << [u8(0x31), 0xc0, 0xc3]
	elf_tiny_runtime_patch_local_rel32(mut bytes, raw_errno, fail_target)!
	elf_tiny_runtime_patch_local_rel32(mut bytes, null_mapping, fail_target)!
	if bytes.len != 58 || raw_errno != 41 || null_mapping != 50 || fail_target != 55 {
		return error('ELF tiny runtime mmap manifest mismatch')
	}
	return ElfTinyRuntimeManifest{
		bytes: bytes
	}
}

fn elf_tiny_runtime_i64_manifest() !ElfTinyRuntimeManifest {
	mut bytes := [
		u8(0x57),
		0xbf,
		0x20,
		0x00,
		0x00,
		0x00,
	]
	allocator_field := elf_tiny_runtime_emit_call(mut bytes)
	bytes << [u8(0x48), 0x85, 0xc0]
	malloc_ok := elf_tiny_runtime_emit_jcc(mut bytes, 0x85)
	bytes << [u8(0xbf), 0x01, 0x00, 0x00, 0x00]
	exit_field := elf_tiny_runtime_emit_call(mut bytes)
	bytes << [u8(0x0f), 0x0b]
	ok_start := bytes.len
	elf_tiny_runtime_patch_local_rel32(mut bytes, malloc_ok, ok_start)!

	bytes << [
		u8(0x5f),
		0x4c,
		0x8d,
		0x40,
		0x1f,
		0x41,
		0xc6,
		0x00,
		0x00,
		0x48,
		0x89,
		0xf8,
		0x45,
		0x31,
		0xc9,
		0x45,
		0x31,
		0xd2,
		0x48,
		0x85,
		0xc0,
	]
	non_negative := elf_tiny_runtime_emit_jcc(mut bytes, 0x89)
	bytes << [u8(0x41), 0xb2, 0x01, 0x48, 0xf7, 0xd8]
	non_negative_target := bytes.len
	bytes << [u8(0x48), 0x85, 0xc0]
	non_zero := elf_tiny_runtime_emit_jcc(mut bytes, 0x85)
	bytes << [
		u8(0x49),
		0xff,
		0xc8,
		0x41,
		0xc6,
		0x00,
		0x30,
		0x41,
		0xb9,
		0x01,
		0x00,
		0x00,
		0x00,
	]
	maybe_sign_jump := elf_tiny_runtime_emit_jmp(mut bytes)
	loop_start := bytes.len
	bytes << [
		u8(0x31),
		0xd2,
		0xb9,
		0x0a,
		0x00,
		0x00,
		0x00,
		0x48,
		0xf7,
		0xf1,
		0x80,
		0xc2,
		0x30,
		0x49,
		0xff,
		0xc8,
		0x41,
		0x88,
		0x10,
		0x49,
		0xff,
		0xc1,
		0x48,
		0x85,
		0xc0,
	]
	loop_more := elf_tiny_runtime_emit_jcc(mut bytes, 0x85)
	maybe_sign := bytes.len
	bytes << [u8(0x45), 0x84, 0xd2]
	done_digits := elf_tiny_runtime_emit_jcc(mut bytes, 0x84)
	bytes << [
		u8(0x49),
		0xff,
		0xc8,
		0x41,
		0xc6,
		0x00,
		0x2d,
		0x49,
		0xff,
		0xc1,
	]
	done_digits_target := bytes.len
	bytes << [u8(0x4c), 0x89, 0xc0, 0x4c, 0x89, 0xca, 0xc3]
	elf_tiny_runtime_patch_local_rel32(mut bytes, non_negative, non_negative_target)!
	elf_tiny_runtime_patch_local_rel32(mut bytes, non_zero, loop_start)!
	elf_tiny_runtime_patch_local_rel32(mut bytes, maybe_sign_jump, maybe_sign)!
	elf_tiny_runtime_patch_local_rel32(mut bytes, loop_more, loop_start)!
	elf_tiny_runtime_patch_local_rel32(mut bytes, done_digits, done_digits_target)!
	if bytes.len != 149 || allocator_field != 7 || exit_field != 26 {
		return error('ELF tiny runtime i64-decimal manifest mismatch')
	}
	return ElfTinyRuntimeManifest{
		bytes:        bytes
		cross_fixups: [
			ElfTinyRuntimeCrossFixup{
				field:  allocator_field
				target: .mmap_alloc
			},
			ElfTinyRuntimeCrossFixup{
				field:  exit_field
				target: .exit_group
			},
		]
	}
}

fn elf_tiny_runtime_i32_manifest() !ElfTinyRuntimeManifest {
	return ElfTinyRuntimeManifest{
		bytes:        [u8(0x48), 0x63, 0xff, 0xe9, 0x00, 0x00, 0x00, 0x00]
		cross_fixups: [
			ElfTinyRuntimeCrossFixup{
				field:  4
				target: .i64_decimal
			},
		]
	}
}

fn elf_tiny_runtime_rune_manifest() !ElfTinyRuntimeManifest {
	mut bytes := [u8(0x89), 0xf8, 0x3d, 0xff, 0xff, 0x10, 0x00]
	invalid_high := elf_tiny_runtime_emit_jcc(mut bytes, 0x87)
	bytes << [u8(0x57), 0xbf, 0x08, 0x00, 0x00, 0x00]
	allocator_field := elf_tiny_runtime_emit_call(mut bytes)
	bytes << [u8(0x48), 0x85, 0xc0]
	allocator_ok := elf_tiny_runtime_emit_jcc(mut bytes, 0x85)
	bytes << [u8(0xbf), 0x01, 0x00, 0x00, 0x00]
	exit_field := elf_tiny_runtime_emit_call(mut bytes)
	bytes << [u8(0x0f), 0x0b]
	allocator_ok_target := bytes.len
	bytes << [u8(0x5f), 0x49, 0x89, 0xc0, 0x89, 0xf8, 0x3d, 0x7f, 0x00, 0x00, 0x00]
	two_byte := elf_tiny_runtime_emit_jcc(mut bytes, 0x87)
	bytes << [
		u8(0x41),
		0x88,
		0x00,
		0x41,
		0xc6,
		0x40,
		0x01,
		0x00,
		0xba,
		0x01,
		0x00,
		0x00,
		0x00,
	]
	one_done := elf_tiny_runtime_emit_jmp(mut bytes)
	two_byte_target := bytes.len
	bytes << [u8(0x3d), 0xff, 0x07, 0x00, 0x00]
	three_byte := elf_tiny_runtime_emit_jcc(mut bytes, 0x87)
	bytes << [
		u8(0x89),
		0xc1,
		0xc1,
		0xe9,
		0x06,
		0x80,
		0xc9,
		0xc0,
		0x41,
		0x88,
		0x08,
		0x89,
		0xc1,
		0x80,
		0xe1,
		0x3f,
		0x80,
		0xc9,
		0x80,
		0x41,
		0x88,
		0x48,
		0x01,
		0x41,
		0xc6,
		0x40,
		0x02,
		0x00,
		0xba,
		0x02,
		0x00,
		0x00,
		0x00,
	]
	two_done := elf_tiny_runtime_emit_jmp(mut bytes)
	three_byte_target := bytes.len
	bytes << [u8(0x3d), 0xff, 0xff, 0x00, 0x00]
	four_byte := elf_tiny_runtime_emit_jcc(mut bytes, 0x87)
	bytes << [
		u8(0x89),
		0xc1,
		0xc1,
		0xe9,
		0x0c,
		0x80,
		0xc9,
		0xe0,
		0x41,
		0x88,
		0x08,
		0x89,
		0xc1,
		0xc1,
		0xe9,
		0x06,
		0x80,
		0xe1,
		0x3f,
		0x80,
		0xc9,
		0x80,
		0x41,
		0x88,
		0x48,
		0x01,
		0x89,
		0xc1,
		0x80,
		0xe1,
		0x3f,
		0x80,
		0xc9,
		0x80,
		0x41,
		0x88,
		0x48,
		0x02,
		0x41,
		0xc6,
		0x40,
		0x03,
		0x00,
		0xba,
		0x03,
		0x00,
		0x00,
		0x00,
	]
	three_done := elf_tiny_runtime_emit_jmp(mut bytes)
	four_byte_target := bytes.len
	bytes << [
		u8(0x89),
		0xc1,
		0xc1,
		0xe9,
		0x12,
		0x80,
		0xc9,
		0xf0,
		0x41,
		0x88,
		0x08,
		0x89,
		0xc1,
		0xc1,
		0xe9,
		0x0c,
		0x80,
		0xe1,
		0x3f,
		0x80,
		0xc9,
		0x80,
		0x41,
		0x88,
		0x48,
		0x01,
		0x89,
		0xc1,
		0xc1,
		0xe9,
		0x06,
		0x80,
		0xe1,
		0x3f,
		0x80,
		0xc9,
		0x80,
		0x41,
		0x88,
		0x48,
		0x02,
		0x89,
		0xc1,
		0x80,
		0xe1,
		0x3f,
		0x80,
		0xc9,
		0x80,
		0x41,
		0x88,
		0x48,
		0x03,
		0x41,
		0xc6,
		0x40,
		0x04,
		0x00,
		0xba,
		0x04,
		0x00,
		0x00,
		0x00,
	]
	done_target := bytes.len
	bytes << [u8(0x4c), 0x89, 0xc0, 0xc3]
	invalid_target := bytes.len
	bytes << [u8(0x31), 0xc0, 0x31, 0xd2, 0xc3]
	elf_tiny_runtime_patch_local_rel32(mut bytes, invalid_high, invalid_target)!
	elf_tiny_runtime_patch_local_rel32(mut bytes, allocator_ok, allocator_ok_target)!
	elf_tiny_runtime_patch_local_rel32(mut bytes, two_byte, two_byte_target)!
	elf_tiny_runtime_patch_local_rel32(mut bytes, one_done, done_target)!
	elf_tiny_runtime_patch_local_rel32(mut bytes, three_byte, three_byte_target)!
	elf_tiny_runtime_patch_local_rel32(mut bytes, two_done, done_target)!
	elf_tiny_runtime_patch_local_rel32(mut bytes, four_byte, four_byte_target)!
	elf_tiny_runtime_patch_local_rel32(mut bytes, three_done, done_target)!
	if bytes.len != 265 || allocator_field != 20 || exit_field != 39 {
		return error('ELF tiny runtime rune manifest mismatch')
	}
	return ElfTinyRuntimeManifest{
		bytes:        bytes
		cross_fixups: [
			ElfTinyRuntimeCrossFixup{
				field:  allocator_field
				target: .mmap_alloc
			},
			ElfTinyRuntimeCrossFixup{
				field:  exit_field
				target: .exit_group
			},
		]
	}
}

fn elf_tiny_runtime_concat_manifest() !ElfTinyRuntimeManifest {
	mut bytes := [
		u8(0x57),
		0x56,
		0x52,
		0x51,
		0x48,
		0x83,
		0xec,
		0x08,
		0x44,
		0x8b,
		0x44,
		0x24,
		0x18,
		0x44,
		0x03,
		0x44,
		0x24,
		0x08,
	]
	length_overflow := elf_tiny_runtime_emit_jcc(mut bytes, 0x82)
	bytes << [u8(0x4d), 0x89, 0xc1, 0x49, 0x83, 0xc1, 0x01]
	allocation_overflow := elf_tiny_runtime_emit_jcc(mut bytes, 0x82)
	bytes << [u8(0x4c), 0x89, 0xcf]
	allocator_field := elf_tiny_runtime_emit_call(mut bytes)
	bytes << [u8(0x48), 0x85, 0xc0]
	malloc_ok := elf_tiny_runtime_emit_jcc(mut bytes, 0x85)
	fail_start := bytes.len
	bytes << [u8(0xbf), 0x01, 0x00, 0x00, 0x00]
	exit_field := elf_tiny_runtime_emit_call(mut bytes)
	bytes << [u8(0x0f), 0x0b]
	ok_start := bytes.len
	elf_tiny_runtime_patch_local_rel32(mut bytes, length_overflow, fail_start)!
	elf_tiny_runtime_patch_local_rel32(mut bytes, allocation_overflow, fail_start)!
	elf_tiny_runtime_patch_local_rel32(mut bytes, malloc_ok, ok_start)!

	bytes << [
		u8(0x49),
		0x89,
		0xc2,
		0x49,
		0x89,
		0xc0,
		0x48,
		0x8b,
		0x74,
		0x24,
		0x20,
		0x8b,
		0x4c,
		0x24,
		0x18,
		0x48,
		0x85,
		0xc9,
	]
	copy_a_done := elf_tiny_runtime_emit_jcc(mut bytes, 0x84)
	copy_a_loop := bytes.len
	bytes << [
		u8(0x8a),
		0x16,
		0x41,
		0x88,
		0x10,
		0x48,
		0xff,
		0xc6,
		0x49,
		0xff,
		0xc0,
		0x48,
		0xff,
		0xc9,
	]
	copy_a_continue := elf_tiny_runtime_emit_jcc(mut bytes, 0x85)
	copy_a_done_target := bytes.len
	elf_tiny_runtime_patch_local_rel32(mut bytes, copy_a_done, copy_a_done_target)!
	elf_tiny_runtime_patch_local_rel32(mut bytes, copy_a_continue, copy_a_loop)!

	bytes << [
		u8(0x48),
		0x8b,
		0x74,
		0x24,
		0x10,
		0x8b,
		0x4c,
		0x24,
		0x08,
		0x48,
		0x85,
		0xc9,
	]
	copy_b_done := elf_tiny_runtime_emit_jcc(mut bytes, 0x84)
	copy_b_loop := bytes.len
	bytes << [
		u8(0x8a),
		0x16,
		0x41,
		0x88,
		0x10,
		0x48,
		0xff,
		0xc6,
		0x49,
		0xff,
		0xc0,
		0x48,
		0xff,
		0xc9,
	]
	copy_b_continue := elf_tiny_runtime_emit_jcc(mut bytes, 0x85)
	copy_b_done_target := bytes.len
	elf_tiny_runtime_patch_local_rel32(mut bytes, copy_b_done, copy_b_done_target)!
	elf_tiny_runtime_patch_local_rel32(mut bytes, copy_b_continue, copy_b_loop)!

	bytes << [
		u8(0x41),
		0xc6,
		0x00,
		0x00,
		0x8b,
		0x44,
		0x24,
		0x18,
		0x03,
		0x44,
		0x24,
		0x08,
		0x48,
		0x89,
		0xc2,
		0x4c,
		0x89,
		0xd0,
		0x48,
		0x83,
		0xc4,
		0x28,
		0xc3,
	]
	if bytes.len != 171 || allocator_field != 41 || exit_field != 60 {
		return error('ELF tiny runtime string-concat manifest mismatch')
	}
	return ElfTinyRuntimeManifest{
		bytes:        bytes
		cross_fixups: [
			ElfTinyRuntimeCrossFixup{
				field:  allocator_field
				target: .mmap_alloc
			},
			ElfTinyRuntimeCrossFixup{
				field:  exit_field
				target: .exit_group
			},
		]
	}
}

fn elf_tiny_runtime_manifest(implementation ElfTinyRuntimeImplementation) !ElfTinyRuntimeManifest {
	return match implementation {
		.write_all { elf_tiny_runtime_write_manifest()! }
		.flush_noop { elf_tiny_runtime_flush_manifest()! }
		.exit_group { elf_tiny_runtime_exit_manifest()! }
		.mmap_alloc { elf_tiny_runtime_mmap_manifest()! }
		.i64_decimal { elf_tiny_runtime_i64_manifest()! }
		.i32_decimal { elf_tiny_runtime_i32_manifest()! }
		.rune_utf8 { elf_tiny_runtime_rune_manifest()! }
		.string_concat { elf_tiny_runtime_concat_manifest()! }
		.unknown { error('ELF tiny runtime implementation is missing') }
	}
}

fn elf_tiny_runtime_role_implementation(role ElfTinyRuntimeRole) !ElfTinyRuntimeImplementation {
	return match role {
		.write_all { .write_all }
		.flush_noop { .flush_noop }
		.exit_group { .exit_group }
		.i32_decimal { .i32_decimal }
		.i64_decimal { .i64_decimal }
		.rune_utf8 { .rune_utf8 }
		.string_concat { .string_concat }
		.unknown { error('ELF tiny runtime role is missing') }
	}
}

fn elf_tiny_runtime_call_owner(o &Object, offset u64) !int {
	field_end := elf_tiny_runtime_checked_add(offset, 4, 'CALL field extent')!
	mut owner := -1
	for index, symbol in o.symbols {
		if !symbol.defined || symbol.intentional_external {
			continue
		}
		symbol_end := elf_tiny_runtime_checked_add(symbol.offset, symbol.size,
			'function `${symbol.name}` extent')!
		if symbol.offset <= offset && field_end <= symbol_end {
			if owner >= 0 {
				return error('ELF tiny runtime CALL field has multiple function owners')
			}
			owner = index
		}
	}
	if owner < 0 {
		return error('ELF tiny runtime CALL field has no function owner')
	}
	return owner
}

fn elf_tiny_runtime_build_call_plans(o &Object) ![]ElfTinyRuntimeCallPlan {
	mut plans := []ElfTinyRuntimeCallPlan{cap: o.call_relocations.len}
	for relocation in o.call_relocations {
		plans << ElfTinyRuntimeCallPlan{
			owner_index:  elf_tiny_runtime_call_owner(o, relocation.offset)!
			target_index: object_symbol_index(o, relocation.symbol_id)!
			offset:       relocation.offset
		}
	}
	return plans
}

fn elf_tiny_runtime_collect_reachable(o &Object, entry_index int, plans []ElfTinyRuntimeCallPlan) !ElfTinyRuntimeReachability {
	if entry_index < 0 || entry_index >= o.symbols.len {
		return error('ELF tiny runtime entry function is out of range')
	}
	mut functions := []bool{len: o.symbols.len}
	mut externals := []bool{len: o.symbols.len}
	mut queue := [entry_index]
	for queue.len > 0 {
		index := queue[0]
		queue.delete(0)
		if functions[index] {
			continue
		}
		symbol := o.symbols[index]
		if !symbol.defined || symbol.intentional_external {
			return error('ELF tiny runtime reachability selected a non-definition')
		}
		functions[index] = true
		for plan in plans {
			if plan.owner_index != index {
				continue
			}
			target := o.symbols[plan.target_index]
			if target.intentional_external {
				externals[plan.target_index] = true
			} else if target.defined && !functions[plan.target_index] {
				queue << plan.target_index
			} else if !target.defined {
				return error('ELF tiny runtime CALL target `${target.name}` is not defined')
			}
		}
	}
	return ElfTinyRuntimeReachability{
		functions: functions
		externals: externals
	}
}

fn elf_tiny_runtime_data_relocation_owner(o &Object, relocation &ObjectDataRelocation) !int {
	width := object_data_relocation_width_size(relocation.kind, relocation.width)!
	field_end := elf_tiny_runtime_checked_add(relocation.offset, width,
		'object data relocation field extent')!
	mut owner := -1
	for index, symbol in o.symbols {
		if !symbol.defined || symbol.intentional_external {
			continue
		}
		symbol_end := elf_tiny_runtime_checked_add(symbol.offset, symbol.size,
			'function `${symbol.name}` extent')!
		if symbol.offset <= relocation.offset && field_end <= symbol_end {
			if owner >= 0 {
				return error('ELF tiny runtime object data relocation has multiple function owners')
			}
			owner = index
		}
	}
	if owner < 0 {
		return error('ELF tiny runtime object data relocation has no function owner')
	}
	return owner
}

fn elf_tiny_runtime_classify_data_relocations(o &Object) ![]ElfTinyRuntimeDataRelocationPlan {
	for section in o.object_data.sections {
		elf_tiny_runtime_check_cap(section.size, 'source ${section.kind} size')!
		if section.alignment > elf_tiny_page_align {
			return error('ELF tiny runtime source ${section.kind} alignment ${section.alignment} is unsupported')
		}
	}
	mut plans := []ElfTinyRuntimeDataRelocationPlan{cap: o.object_data.relocations.len}
	for original_index, relocation in o.object_data.relocations {
		if relocation.source_section != .text {
			return error('ELF tiny runtime object data relocations must originate in .text')
		}
		mapped := object_data_map_relocation(&relocation, .elf_x86_64)!
		if mapped != .elf_pc32 {
			return error('ELF tiny runtime object data relocation ${original_index} is unsupported')
		}
		target_index := int(relocation.target_symbol.id)
		if !relocation.target_symbol.is_set || target_index < 0
			|| target_index >= o.object_data.symbols.len {
			return error('ELF tiny runtime object data relocation target is invalid')
		}
		target := o.object_data.symbols[target_index]
		if target.section !in [.rodata, .data, .bss] || target.size == 0 {
			return error('ELF tiny runtime object data relocation target must own a non-empty range')
		}
		if relocation.addend < -4 || relocation.addend > max_i64 - 4 {
			return error('ELF tiny runtime object data relocation addend escapes its target symbol')
		}
		logical_delta := u64(relocation.addend + 4)
		if logical_delta >= target.size {
			return error('ELF tiny runtime object data relocation addend escapes its target symbol')
		}
		plans << ElfTinyRuntimeDataRelocationPlan{
			owner_index:    elf_tiny_runtime_data_relocation_owner(o, &relocation)!
			target_index:   target_index
			original_index: original_index
			relocation:     relocation
		}
	}
	return plans
}

fn elf_tiny_runtime_alias_root(symbols []ObjectDataSymbol, symbol_index int) !int {
	if symbol_index < 0 || symbol_index >= symbols.len {
		return error('ELF tiny runtime object data symbol index is out of range')
	}
	mut cursor := symbol_index
	mut steps := 0
	for symbols[cursor].alias_of.is_set {
		target_index := int(symbols[cursor].alias_of.id)
		if target_index < 0 || target_index >= cursor {
			return error('ELF tiny runtime object data alias ancestry is invalid')
		}
		current := symbols[cursor]
		target := symbols[target_index]
		if current.section != target.section || current.offset != target.offset
			|| current.size != target.size || current.size == 0 {
			return error('ELF tiny runtime object data alias interval is invalid')
		}
		cursor = target_index
		steps++
		if steps > symbols.len {
			return error('ELF tiny runtime object data alias ancestry contains a cycle')
		}
	}
	return cursor
}

fn elf_tiny_runtime_data_run_order(section ObjectDataSectionKind) int {
	return match section {
		.rodata { 0 }
		.data { 1 }
		.bss { 2 }
		else { 3 }
	}
}

fn elf_tiny_runtime_data_run_less(left ElfTinyRuntimeDataRun, right ElfTinyRuntimeDataRun) bool {
	left_order := elf_tiny_runtime_data_run_order(left.section)
	right_order := elf_tiny_runtime_data_run_order(right.section)
	if left_order != right_order {
		return left_order < right_order
	}
	if left.old_start != right.old_start {
		return left.old_start < right.old_start
	}
	return left.old_end < right.old_end
}

fn elf_tiny_runtime_sort_data_runs(mut runs []ElfTinyRuntimeDataRun) {
	for index := 1; index < runs.len; index++ {
		mut cursor := index
		for cursor > 0 && elf_tiny_runtime_data_run_less(runs[cursor], runs[cursor - 1]) {
			runs[cursor - 1], runs[cursor] = runs[cursor], runs[cursor - 1]
			cursor--
		}
	}
}

fn elf_tiny_runtime_source_section(o &Object, kind ObjectDataSectionKind) !ObjectDataSection {
	index := object_data_find_section(o.object_data.sections, kind)
	if index < 0 {
		return error('ELF tiny runtime selected object data section is absent')
	}
	return o.object_data.sections[index]
}

fn elf_tiny_runtime_padding_for_residue(current u64, alignment u64, residue u64) !u64 {
	if alignment == 0 || alignment > elf_tiny_page_align || alignment & (alignment - 1) != 0
		|| residue >= alignment {
		return error('ELF tiny runtime data alignment residue is invalid')
	}
	current_residue := current % alignment
	if current_residue <= residue {
		return residue - current_residue
	}
	return elf_tiny_runtime_checked_add(alignment - current_residue, residue,
		'data alignment padding')!
}

fn elf_tiny_runtime_build_data_plan(o &Object, reachability &ElfTinyRuntimeReachability, relocation_plans []ElfTinyRuntimeDataRelocationPlan) !ElfTinyRuntimeDataPlan {
	mut roots := []int{len: o.object_data.symbols.len, init: -1}
	for index in 0 .. o.object_data.symbols.len {
		roots[index] = elf_tiny_runtime_alias_root(o.object_data.symbols, index)!
	}
	mut symbol_kept := []bool{len: o.object_data.symbols.len}
	for plan in relocation_plans {
		if !reachability.functions[plan.owner_index] {
			continue
		}
		mut cursor := plan.target_index
		for {
			symbol_kept[cursor] = true
			if !o.object_data.symbols[cursor].alias_of.is_set {
				break
			}
			cursor = int(o.object_data.symbols[cursor].alias_of.id)
		}
	}

	mut roots_added := []bool{len: o.object_data.symbols.len}
	mut runs := []ElfTinyRuntimeDataRun{}
	for index, kept in symbol_kept {
		if !kept {
			continue
		}
		root_index := roots[index]
		if roots_added[root_index] {
			continue
		}
		roots_added[root_index] = true
		root := o.object_data.symbols[root_index]
		if root.section !in [.rodata, .data, .bss] || root.size == 0 {
			return error('ELF tiny runtime retained data root is unsupported')
		}
		runs << ElfTinyRuntimeDataRun{
			section:   root.section
			old_start: root.offset
			old_end:   elf_tiny_runtime_checked_add(root.offset, root.size,
				'retained object data interval')!
		}
	}
	elf_tiny_runtime_sort_data_runs(mut runs)
	mut coalesced := []ElfTinyRuntimeDataRun{cap: runs.len}
	for run in runs {
		if coalesced.len == 0 || coalesced.last().section != run.section {
			coalesced << run
			continue
		}
		last_index := coalesced.len - 1
		if run.old_start < coalesced[last_index].old_end {
			return error('ELF tiny runtime retained object data roots overlap')
		}
		if run.old_start == coalesced[last_index].old_end {
			coalesced[last_index].old_end = run.old_end
		} else {
			coalesced << run
		}
	}

	mut output_sections := []ObjectDataSection{}
	for kind in [ObjectDataSectionKind.rodata, .data, .bss] {
		mut has_runs := false
		for run in coalesced {
			if run.section == kind {
				has_runs = true
				break
			}
		}
		if !has_runs {
			continue
		}
		source := elf_tiny_runtime_source_section(o, kind)!
		mut final_size := u64(0)
		for run_index in 0 .. coalesced.len {
			if coalesced[run_index].section != kind {
				continue
			}
			residue := coalesced[run_index].old_start % source.alignment
			padding := elf_tiny_runtime_padding_for_residue(final_size, source.alignment, residue)!
			final_size = elf_tiny_runtime_checked_add(final_size, padding,
				'retained object data padding')!
			coalesced[run_index].new_start = final_size
			run_size := elf_tiny_runtime_checked_sub(coalesced[run_index].old_end,
				coalesced[run_index].old_start, 'retained object data run size')!
			final_size = elf_tiny_runtime_checked_add(final_size, run_size,
				'retained object data section size')!
			elf_tiny_runtime_check_cap(final_size, 'retained ${kind} size')!
		}
		mut bytes := []u8{}
		if kind != .bss {
			bytes = []u8{cap: elf_tiny_runtime_checked_host_size(final_size,
				'retained ${kind} size')!}
			for run in coalesced {
				if run.section != kind {
					continue
				}
				for u64(bytes.len) < run.new_start {
					bytes << u8(0)
				}
				start := elf_tiny_runtime_checked_host_size(run.old_start,
					'retained object data run start')!
				end := elf_tiny_runtime_checked_host_size(run.old_end,
					'retained object data run end')!
				if end > source.bytes.len {
					return error('ELF tiny runtime retained object data run exceeds source payload')
				}
				bytes << source.bytes[start..end]
			}
			if u64(bytes.len) != final_size {
				return error('ELF tiny runtime compact data payload size mismatch')
			}
		}
		output_sections << ObjectDataSection{
			kind:      kind
			bytes:     bytes
			size:      final_size
			alignment: source.alignment
		}
	}

	mut old_to_new := []ObjectDataSymbolID{len: o.object_data.symbols.len}
	mut output_symbols := []ObjectDataSymbol{}
	for old_index, kept in symbol_kept {
		if !kept {
			continue
		}
		symbol := o.object_data.symbols[old_index]
		symbol_end := elf_tiny_runtime_checked_add(symbol.offset, symbol.size,
			'retained object data symbol extent')!
		mut mapped_offset := u64(0)
		mut found := false
		for run in coalesced {
			if run.section == symbol.section && run.old_start <= symbol.offset
				&& symbol_end <= run.old_end {
				mapped_offset = elf_tiny_runtime_checked_add(run.new_start, elf_tiny_runtime_checked_sub(symbol.offset,
					run.old_start, 'retained symbol run offset')!, 'retained symbol offset')!
				found = true
				break
			}
		}
		if !found {
			return error('ELF tiny runtime retained object data symbol has no copied interval')
		}
		new_id := ObjectDataSymbolID(output_symbols.len)
		old_to_new[old_index] = new_id
		mut alias_of := ObjectDataSymbolRef{}
		if symbol.alias_of.is_set {
			target_index := int(symbol.alias_of.id)
			if target_index < 0 || target_index >= old_index || !symbol_kept[target_index] {
				return error('ELF tiny runtime retained alias ancestry is incomplete')
			}
			alias_of = object_data_symbol_ref(old_to_new[target_index])
		}
		output_symbols << ObjectDataSymbol{
			kind:     symbol.kind
			name:     symbol.name
			section:  symbol.section
			offset:   mapped_offset
			size:     symbol.size
			alias_of: alias_of
		}
	}
	return ElfTinyRuntimeDataPlan{
		sections:    output_sections
		symbols:     output_symbols
		old_to_new:  old_to_new
		symbol_kept: symbol_kept
		relocations: relocation_plans
	}
}

fn elf_tiny_runtime_private_alignment(o &Object) u64 {
	mut alignment := u64(1)
	for symbol in o.private_data_symbols {
		if symbol.alignment > alignment {
			alignment = symbol.alignment
		}
	}
	return alignment
}

fn elf_tiny_runtime_build_layout(o &Object, data_plan &ElfTinyRuntimeDataPlan, selected_functions []int, implementation_used []bool, manifests []ElfTinyRuntimeManifest) !ElfTinyRuntimeLayout {
	mut function_bytes := u64(0)
	for index in selected_functions {
		symbol := o.symbols[index]
		function_bytes = elf_tiny_runtime_checked_add(function_bytes, symbol.size,
			'selected function bytes')!
	}
	mut helper_bytes := u64(0)
	for implementation_index in 1 .. implementation_used.len {
		if implementation_used[implementation_index] {
			helper_bytes = elf_tiny_runtime_checked_add(helper_bytes,
				u64(manifests[implementation_index].bytes.len), 'runtime helper bytes')!
		}
	}

	rodata_index := object_data_find_section(data_plan.sections, .rodata)
	data_index := object_data_find_section(data_plan.sections, .data)
	bss_index := object_data_find_section(data_plan.sections, .bss)
	has_rodata := rodata_index >= 0
	has_data := data_index >= 0
	has_bss := bss_index >= 0
	rodata := if has_rodata { data_plan.sections[rodata_index] } else { ObjectDataSection{} }
	data := if has_data { data_plan.sections[data_index] } else { ObjectDataSection{} }
	bss := if has_bss { data_plan.sections[bss_index] } else { ObjectDataSection{} }
	private_size := u64(o.private_data.len)
	elf_tiny_runtime_check_cap(private_size, 'private data size')!
	private_alignment := elf_tiny_runtime_private_alignment(o)
	if private_alignment > elf_tiny_page_align {
		return error('ELF tiny runtime private data alignment ${private_alignment} is unsupported')
	}
	object_data_base := if has_data {
		elf_tiny_runtime_align(private_size, data.alignment, 'object data base')!
	} else {
		u64(0)
	}
	rw_file_size := if has_data {
		elf_tiny_runtime_checked_add(object_data_base, data.size, 'RW file size')!
	} else {
		private_size
	}
	bss_base := if has_bss {
		elf_tiny_runtime_align(rw_file_size, bss.alignment, 'BSS base')!
	} else {
		rw_file_size
	}
	rw_mem_size := if has_bss {
		elf_tiny_runtime_checked_add(bss_base, bss.size, 'RW memory size')!
	} else {
		rw_file_size
	}
	elf_tiny_runtime_check_cap(rw_file_size, 'RW file size')!
	elf_tiny_runtime_check_cap(rw_mem_size, 'RW memory size')!
	has_rw := rw_mem_size != 0
	program_header_count := if has_rw { u16(3) } else { u16(2) }
	program_header_bytes := elf64_checked_mul(u64(program_header_count),
		elf_tiny_program_header_size, 'tiny runtime program header table size')!
	program_header_end := elf_tiny_runtime_checked_add(elf_tiny_header_size, program_header_bytes,
		'program header extent')!
	entry_offset := elf_tiny_runtime_align(program_header_end, 16, 'entry offset')!
	entry_end := elf_tiny_runtime_checked_add(entry_offset, elf_tiny_start_size, 'entry extent')!
	function_offset := elf_tiny_runtime_align(entry_end, 16, 'function offset')!
	text_end := elf_tiny_runtime_checked_add(function_offset, elf_tiny_runtime_checked_add(function_bytes,
		helper_bytes, 'text payload size')!, 'text end')!
	rodata_offset := if has_rodata {
		elf_tiny_runtime_align(text_end, rodata.alignment, 'rodata offset')!
	} else {
		u64(0)
	}
	rx_file_size := if has_rodata {
		elf_tiny_runtime_checked_add(rodata_offset, rodata.size, 'RX file size')!
	} else {
		text_end
	}
	elf_tiny_runtime_check_cap(rx_file_size, 'RX file size')!
	rw_offset := if has_rw {
		elf_tiny_runtime_align(rx_file_size, elf_tiny_page_align, 'RW offset')!
	} else {
		u64(0)
	}
	if has_rw {
		elf_tiny_runtime_check_cap(rw_offset, 'RW offset')!
	}
	rw_vaddr := if has_rw {
		elf_tiny_runtime_checked_add(elf_tiny_base_vaddr, rw_offset, 'RW address')!
	} else {
		u64(0)
	}
	file_size := if rw_file_size != 0 {
		elf_tiny_runtime_checked_add(rw_offset, rw_file_size, 'file size')!
	} else {
		rx_file_size
	}
	elf_tiny_runtime_check_cap(file_size, 'file size')!
	_ = elf_tiny_runtime_checked_add(elf_tiny_base_vaddr, rx_file_size, 'RX address extent')!
	if has_rw {
		_ = elf_tiny_runtime_checked_add(rw_vaddr, rw_mem_size, 'RW address extent')!
	}
	_ = elf_tiny_runtime_checked_host_size(file_size, 'file size')!
	return ElfTinyRuntimeLayout{
		program_header_count: program_header_count
		entry_offset:         entry_offset
		function_offset:      function_offset
		text_end:             text_end
		rodata_offset:        rodata_offset
		rx_file_size:         rx_file_size
		rw_offset:            rw_offset
		rw_vaddr:             rw_vaddr
		object_data_base:     object_data_base
		bss_base:             bss_base
		rw_file_size:         rw_file_size
		rw_mem_size:          rw_mem_size
		file_size:            file_size
	}
}

fn elf_tiny_runtime_mark_implementation(mut used []bool, implementation ElfTinyRuntimeImplementation) {
	used[int(implementation)] = true
}

fn elf_tiny_runtime_build_implementation_closure(role_by_symbol []ElfTinyRuntimeRole) !([]bool, []ElfTinyRuntimeManifest) {
	mut used := []bool{len: int(ElfTinyRuntimeImplementation.string_concat) + 1}
	for role in role_by_symbol {
		if role == .unknown {
			continue
		}
		elf_tiny_runtime_mark_implementation(mut used, elf_tiny_runtime_role_implementation(role)!)
	}
	if used[int(ElfTinyRuntimeImplementation.write_all)] {
		elf_tiny_runtime_mark_implementation(mut used, .exit_group)
	}
	if used[int(ElfTinyRuntimeImplementation.i32_decimal)] {
		elf_tiny_runtime_mark_implementation(mut used, .i64_decimal)
	}
	if used[int(ElfTinyRuntimeImplementation.i64_decimal)]
		|| used[int(ElfTinyRuntimeImplementation.rune_utf8)]
		|| used[int(ElfTinyRuntimeImplementation.string_concat)] {
		elf_tiny_runtime_mark_implementation(mut used, .mmap_alloc)
		elf_tiny_runtime_mark_implementation(mut used, .exit_group)
	}
	mut manifests := []ElfTinyRuntimeManifest{len: used.len}
	for implementation_index in 1 .. used.len {
		if used[implementation_index] {
			manifests[implementation_index] =
				elf_tiny_runtime_manifest(unsafe { ElfTinyRuntimeImplementation(implementation_index) })!
		}
	}
	return used, manifests
}

fn elf_tiny_runtime_build_plan(o &Object, definition ElfTinyRuntimeDefinition) !ElfTinyRuntimePlan {
	if definition.startup_policy != .no_args_no_init {
		return error('ELF tiny runtime startup policy is unsupported')
	}
	elf_tiny_validate_entry_result_policy(definition.entry.result_policy)!
	if definition.entry.parameter_count != 0 {
		return error('ELF tiny runtime entry function must not accept parameters')
	}
	o.validate_with_object_data()!
	elf_tiny_runtime_check_cap(u64(o.text.len), 'source text size')!
	elf_tiny_runtime_check_cap(u64(o.private_data.len), 'private data size')!
	entry_index := object_symbol_index(o, SymbolID(definition.entry.function_index)) or {
		return error('ELF tiny runtime entry function index ${definition.entry.function_index} is out of range')
	}
	entry_symbol := o.symbols[entry_index]
	if entry_symbol.intentional_external || !entry_symbol.defined {
		return error('ELF tiny runtime entry must be a defined function')
	}
	call_plans := elf_tiny_runtime_build_call_plans(o)!
	reachability := elf_tiny_runtime_collect_reachable(o, entry_index, call_plans)!

	mut role_by_symbol := []ElfTinyRuntimeRole{len: o.symbols.len}
	mut symbol_by_role := []int{len: int(ElfTinyRuntimeRole.string_concat) + 1, init: -1}
	for binding in definition.runtime_bindings {
		if int(binding.role) <= int(ElfTinyRuntimeRole.unknown)
			|| int(binding.role) > int(ElfTinyRuntimeRole.string_concat) {
			return error('ELF tiny runtime binding role ${int(binding.role)} is unsupported')
		}
		symbol_index := object_symbol_index(o, SymbolID(binding.external_function_index)) or {
			return error('ELF tiny runtime binding function index ${binding.external_function_index} is out of range')
		}
		symbol := o.symbols[symbol_index]
		if !symbol.intentional_external || symbol.defined || symbol.offset != 0 || symbol.size != 0 {
			return error('ELF tiny runtime binding must identify an intentional external')
		}
		if role_by_symbol[symbol_index] != .unknown {
			return error('ELF tiny runtime external function is bound more than once')
		}
		if symbol_by_role[int(binding.role)] >= 0 {
			return error('ELF tiny runtime role is bound more than once')
		}
		if !reachability.externals[symbol_index] {
			return error('ELF tiny runtime binding is not reachable from the selected entry')
		}
		role_by_symbol[symbol_index] = binding.role
		symbol_by_role[int(binding.role)] = symbol_index
	}
	for index, reachable in reachability.externals {
		if reachable && role_by_symbol[index] == .unknown {
			return error('ELF tiny runtime reachable external `${o.symbols[index].name}` has no binding')
		}
	}

	data_relocations := elf_tiny_runtime_classify_data_relocations(o)!
	data_plan := elf_tiny_runtime_build_data_plan(o, &reachability, data_relocations)!
	mut selected_functions := []int{}
	for index, symbol in o.symbols {
		if symbol.defined && !symbol.intentional_external && reachability.functions[index] {
			selected_functions << index
		}
	}
	implementation_used, manifests := elf_tiny_runtime_build_implementation_closure(role_by_symbol)!
	layout := elf_tiny_runtime_build_layout(o, &data_plan, selected_functions, implementation_used,
		manifests)!
	return ElfTinyRuntimePlan{
		reachability:        reachability
		call_plans:          call_plans
		data_plan:           data_plan
		selected_functions:  selected_functions
		role_by_symbol:      role_by_symbol
		implementation_used: implementation_used
		manifests:           manifests
		layout:              layout
	}
}

fn elf_tiny_runtime_checked_pc32(field_vaddr u64, target_vaddr u64, addend i64) !u32 {
	effective_target := object_data_checked_add_signed(target_vaddr, addend,
		'ELF tiny runtime PC32 effective target')!
	if effective_target >= field_vaddr {
		displacement := effective_target - field_vaddr
		if displacement > u64(max_i32) {
			return error('ELF tiny runtime PC32 displacement is outside signed i32')
		}
		return u32(displacement)
	}
	magnitude := field_vaddr - effective_target
	if magnitude > u64(2_147_483_648) {
		return error('ELF tiny runtime PC32 displacement is outside signed i32')
	}
	return u32((-i64(magnitude)) & i64(0xffff_ffff))
}

fn elf_tiny_runtime_patch_call(mut text []u8, layout &ElfTinyRuntimeLayout, field_offset u64, target_offset u64) ! {
	relative_field := elf_tiny_runtime_checked_sub(field_offset, layout.entry_offset,
		'linked CALL field offset')!
	field_vaddr := elf_tiny_runtime_checked_add(elf_tiny_base_vaddr, field_offset,
		'linked CALL field address')!
	target_vaddr := elf_tiny_runtime_checked_add(elf_tiny_base_vaddr, target_offset,
		'linked CALL target address')!
	displacement := elf_tiny_checked_rel32(field_vaddr, target_vaddr)!
	elf_tiny_write_u32_at(mut text, relative_field, displacement)!
}

fn elf_tiny_runtime_data_symbol_vaddr(plan &ElfTinyRuntimeDataPlan, layout &ElfTinyRuntimeLayout, symbol_id ObjectDataSymbolID) !u64 {
	if u64(symbol_id) >= u64(plan.symbols.len) {
		return error('ELF tiny runtime retained object data symbol is out of range')
	}
	symbol := plan.symbols[int(symbol_id)]
	section_offset := match symbol.section {
		.rodata {
			if layout.rodata_offset == 0 {
				return error('ELF tiny runtime retained rodata has no RX location')
			}
			layout.rodata_offset
		}
		.data {
			if layout.rw_vaddr == 0 {
				return error('ELF tiny runtime retained data has no RW location')
			}
			elf_tiny_runtime_checked_add(layout.rw_offset, layout.object_data_base,
				'retained data file-relative base')!
		}
		.bss {
			if layout.rw_vaddr == 0 {
				return error('ELF tiny runtime retained BSS has no RW location')
			}
			elf_tiny_runtime_checked_add(layout.rw_offset, layout.bss_base,
				'retained BSS file-relative base')!
		}
		else {
			return error('ELF tiny runtime retained object data symbol section is unsupported')
		}
	}

	symbol_offset := elf_tiny_runtime_checked_add(section_offset, symbol.offset,
		'retained object data symbol offset')!
	return elf_tiny_runtime_checked_add(elf_tiny_base_vaddr, symbol_offset,
		'retained object data symbol address')
}

fn elf_tiny_runtime_build_linked_text(o &Object, definition ElfTinyRuntimeDefinition, plan &ElfTinyRuntimePlan) !ElfTinyRuntimeLinkedText {
	mut bytes := elf_tiny_startup_bytes(definition.entry.result_policy)
	if u64(bytes.len) != elf_tiny_start_size {
		return error('ELF tiny runtime startup manifest size mismatch')
	}
	function_delta := elf_tiny_runtime_checked_sub(plan.layout.function_offset,
		plan.layout.entry_offset, 'function payload offset')!
	elf64_pad_to(mut bytes, function_delta)!
	mut function_offsets := []u64{len: o.symbols.len}
	for function_index in plan.selected_functions {
		symbol := o.symbols[function_index]
		function_end := elf_tiny_runtime_checked_add(symbol.offset, symbol.size,
			'selected function extent')!
		start := elf_tiny_runtime_checked_host_size(symbol.offset, 'selected function start')!
		end := elf_tiny_runtime_checked_host_size(function_end, 'selected function end')!
		if end > o.text.len {
			return error('ELF tiny runtime selected function exceeds source text')
		}
		function_offsets[function_index] = elf_tiny_runtime_checked_add(plan.layout.entry_offset,
			u64(bytes.len), 'selected function output offset')!
		bytes << o.text[start..end]
	}
	mut implementation_offsets := []u64{len: plan.implementation_used.len}
	for implementation_index in 1 .. plan.implementation_used.len {
		if !plan.implementation_used[implementation_index] {
			continue
		}
		implementation_offsets[implementation_index] = elf_tiny_runtime_checked_add(plan.layout.entry_offset,
			u64(bytes.len), 'runtime implementation output offset')!
		bytes << plan.manifests[implementation_index].bytes
	}
	expected_size := elf_tiny_runtime_checked_sub(plan.layout.text_end, plan.layout.entry_offset,
		'linked text size')!
	if u64(bytes.len) != expected_size {
		return error('ELF tiny runtime linked text size ${bytes.len} does not match ${expected_size}')
	}

	entry_index := object_symbol_index(o, SymbolID(definition.entry.function_index))!
	elf_tiny_runtime_patch_call(mut bytes, &plan.layout, elf_tiny_runtime_checked_add(plan.layout.entry_offset,
		elf_tiny_start_call_field_offset, 'startup CALL field')!, function_offsets[entry_index])!
	for call_plan in plan.call_plans {
		if !plan.reachability.functions[call_plan.owner_index] {
			continue
		}
		owner := o.symbols[call_plan.owner_index]
		relative_field := elf_tiny_runtime_checked_sub(call_plan.offset, owner.offset,
			'reachable CALL field in owner')!
		field_offset := elf_tiny_runtime_checked_add(function_offsets[call_plan.owner_index],
			relative_field, 'reachable CALL output field')!
		target := o.symbols[call_plan.target_index]
		target_offset := if target.intentional_external {
			role := plan.role_by_symbol[call_plan.target_index]
			implementation := elf_tiny_runtime_role_implementation(role)!
			offset := implementation_offsets[int(implementation)]
			if offset == 0 {
				return error('ELF tiny runtime reachable external implementation is absent')
			}
			offset
		} else {
			offset := function_offsets[call_plan.target_index]
			if offset == 0 {
				return error('ELF tiny runtime reachable function target is absent')
			}
			offset
		}
		elf_tiny_runtime_patch_call(mut bytes, &plan.layout, field_offset, target_offset)!
	}
	for implementation_index in 1 .. plan.implementation_used.len {
		if !plan.implementation_used[implementation_index] {
			continue
		}
		implementation_offset := implementation_offsets[implementation_index]
		for fixup in plan.manifests[implementation_index].cross_fixups {
			target_offset := implementation_offsets[int(fixup.target)]
			if target_offset == 0 {
				return error('ELF tiny runtime helper dependency is absent')
			}
			field_offset := elf_tiny_runtime_checked_add(implementation_offset, fixup.field,
				'runtime helper CALL field')!
			elf_tiny_runtime_patch_call(mut bytes, &plan.layout, field_offset, target_offset)!
		}
	}
	for relocation_plan in plan.data_plan.relocations {
		if !plan.reachability.functions[relocation_plan.owner_index] {
			continue
		}
		if !plan.data_plan.symbol_kept[relocation_plan.target_index] {
			return error('ELF tiny runtime reachable object data target was not retained')
		}
		owner := o.symbols[relocation_plan.owner_index]
		relative_field := elf_tiny_runtime_checked_sub(relocation_plan.relocation.offset,
			owner.offset, 'reachable object data field in owner')!
		field_offset := elf_tiny_runtime_checked_add(function_offsets[relocation_plan.owner_index],
			relative_field, 'reachable object data output field')!
		field_vaddr := elf_tiny_runtime_checked_add(elf_tiny_base_vaddr, field_offset,
			'reachable object data field address')!
		target_id := plan.data_plan.old_to_new[relocation_plan.target_index]
		target_vaddr :=
			elf_tiny_runtime_data_symbol_vaddr(&plan.data_plan, &plan.layout, target_id)!
		displacement := elf_tiny_runtime_checked_pc32(field_vaddr, target_vaddr,
			relocation_plan.relocation.addend)!
		output_field := elf_tiny_runtime_checked_sub(field_offset, plan.layout.entry_offset,
			'reachable object data linked field')!
		elf_tiny_write_u32_at(mut bytes, output_field, displacement)!
	}
	return ElfTinyRuntimeLinkedText{
		bytes:                  bytes
		function_offsets:       function_offsets
		implementation_offsets: implementation_offsets
	}
}

// elf_tiny_runtime_executable_bytes emits a direct Linux AMD64 ET_EXEC from
// explicit runtime roles and the reachable, validated ObjectData closure.
fn elf_tiny_runtime_executable_bytes(o &Object, definition ElfTinyRuntimeDefinition) ![]u8 {
	plan := elf_tiny_runtime_build_plan(o, definition)!
	linked := elf_tiny_runtime_build_linked_text(o, definition, &plan)!
	mut output := []u8{cap: elf_tiny_runtime_checked_host_size(plan.layout.file_size, 'file size')!}
	header_layout := ElfTinyLayout{
		program_header_count: plan.layout.program_header_count
		entry_vaddr:          elf_tiny_runtime_checked_add(elf_tiny_base_vaddr,
			plan.layout.entry_offset, 'entry address')!
	}
	elf_tiny_write_header(mut output, &header_layout)
	elf_tiny_write_program_header(mut output, elf_tiny_pt_load, elf_tiny_pf_r | elf_tiny_pf_x, 0,
		elf_tiny_base_vaddr, plan.layout.rx_file_size, plan.layout.rx_file_size,
		elf_tiny_page_align)
	if plan.layout.rw_mem_size != 0 {
		elf_tiny_write_program_header(mut output, elf_tiny_pt_load, elf_tiny_pf_r | elf_tiny_pf_w,
			plan.layout.rw_offset, plan.layout.rw_vaddr, plan.layout.rw_file_size,
			plan.layout.rw_mem_size, elf_tiny_page_align)
	}
	elf_tiny_write_program_header(mut output, elf_tiny_pt_gnu_stack, elf_tiny_pf_r | elf_tiny_pf_w,
		0, 0, 0, 0, 16)
	program_header_bytes := elf64_checked_mul(u64(plan.layout.program_header_count),
		elf_tiny_program_header_size, 'tiny runtime program header table size')!
	program_header_end := elf_tiny_runtime_checked_add(elf_tiny_header_size, program_header_bytes,
		'program header extent')!
	if u64(output.len) != program_header_end {
		return error('ELF tiny runtime headers end at ${output.len}, expected ${program_header_end}')
	}
	elf64_pad_to(mut output, plan.layout.entry_offset)!
	output << linked.bytes
	rodata_index := object_data_find_section(plan.data_plan.sections, .rodata)
	if rodata_index >= 0 {
		elf64_pad_to(mut output, plan.layout.rodata_offset)!
		output << plan.data_plan.sections[rodata_index].bytes
	}
	if u64(output.len) != plan.layout.rx_file_size {
		return error('ELF tiny runtime RX payload size ${output.len} does not match ${plan.layout.rx_file_size}')
	}
	if plan.layout.rw_file_size != 0 {
		elf64_pad_to(mut output, plan.layout.rw_offset)!
		output << o.private_data
		data_index := object_data_find_section(plan.data_plan.sections, .data)
		if data_index >= 0 {
			data_offset := elf_tiny_runtime_checked_add(plan.layout.rw_offset,
				plan.layout.object_data_base, 'object data output offset')!
			elf64_pad_to(mut output, data_offset)!
			output << plan.data_plan.sections[data_index].bytes
		}
	}
	if u64(output.len) != plan.layout.file_size {
		return error('ELF tiny runtime output size ${output.len} does not match ${plan.layout.file_size}')
	}
	return output
}
