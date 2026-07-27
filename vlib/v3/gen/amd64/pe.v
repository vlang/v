// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module amd64

const pe64_dos_stub_size = u64(0x80)
const pe64_signature = u32(0x0000_4550)
const pe64_optional_header_magic = u16(0x020b)
const pe64_optional_header_size = u64(0xf0)
const pe64_file_alignment = u64(0x200)
const pe64_section_alignment = u64(0x1000)
const pe64_image_base = u64(0x1_4000_0000)
const pe64_max_size_of_image = u64(0x8000_0000)
const pe64_directory_count = 16
const pe64_section_header_size = u64(40)
const pe64_file_header_size = u64(20)
const pe64_max_sections = u64(96)
const pe64_import_descriptor_size = u64(20)
const pe64_import_thunk_size = u64(6)
const pe64_runtime_function_size = u64(12)
const pe64_unwind_info_size = u64(8)
const pe64_runtime_strlen_size = u64(17)
const pe64_runtime_wcslen_size = u64(22)
const pe64_runtime_memset_size = u64(23)
const pe64_runtime_memcmp_size = u64(42)
const pe64_runtime_move_size = u64(74)
const pe64_runtime_process_exit_size = u64(10)
const pe64_runtime_malloc_size = u64(75)
const pe64_runtime_free_size = u64(48)
const pe64_runtime_calloc_size = u64(89)
const pe64_runtime_process_exit_dll = 'Kernel32.dll'
const pe64_runtime_process_exit_export = 'ExitProcess'
const pe64_runtime_heap_dll = 'Kernel32.dll'
const pe64_runtime_get_process_heap_export = 'GetProcessHeap'
const pe64_runtime_heap_alloc_export = 'HeapAlloc'
const pe64_runtime_heap_free_export = 'HeapFree'

const pe64_machine_amd64 = u16(0x8664)
const pe64_image_file_relocs_stripped = u16(0x0001)
const pe64_image_file_executable_image = u16(0x0002)
const pe64_image_file_large_address_aware = u16(0x0020)
const pe64_subsystem_windows_cui = u16(3)
const pe64_dll_characteristics_high_entropy_va = u16(0x0020)
const pe64_dll_characteristics_dynamic_base = u16(0x0040)
const pe64_dll_characteristics_nx_compat = u16(0x0100)

const pe64_section_contains_code = u32(0x0000_0020)
const pe64_section_contains_initialized_data = u32(0x0000_0040)
const pe64_section_mem_execute = u32(0x2000_0000)
const pe64_section_mem_read = u32(0x4000_0000)
const pe64_section_mem_write = u32(0x8000_0000)

const pe64_import_directory_index = 1
const pe64_exception_directory_index = 3
const pe64_base_relocation_directory_index = 5
const pe64_iat_directory_index = 12

const pe64_linker_major_version = u8(0)
const pe64_linker_minor_version = u8(1)
const pe64_major_operating_system_version = u16(6)
const pe64_minor_operating_system_version = u16(0)
const pe64_major_subsystem_version = u16(6)
const pe64_minor_subsystem_version = u16(0)
const pe64_size_of_stack_reserve = u64(0x10_0000)
const pe64_size_of_stack_commit = u64(0x1000)
const pe64_size_of_heap_reserve = u64(0x10_0000)
const pe64_size_of_heap_commit = u64(0x1000)

enum Pe64TargetAbi {
	unknown
	windows_x64_microsoft
}

enum Pe64Subsystem {
	unknown
	windows_cui
}

enum Pe64ImagePolicy {
	unknown
	fixed_base
}

enum Pe64EntryPolicy {
	unknown
	raw_noreturn_process_entry
}

struct Pe64EntryDefinition {
	function_index  u32
	parameter_count u32
	policy          Pe64EntryPolicy
}

struct Pe64ImportBinding {
	symbol_id   SymbolID
	dll         string
	export_name string
}

enum Pe64RuntimeHelperKind {
	unknown
	strlen
	wcslen
	memset
	memcmp
	memmove
	memcpy
	process_exit
	malloc
	free
	calloc
}

struct Pe64RuntimeBinding {
	symbol_id SymbolID
	helper    Pe64RuntimeHelperKind
}

struct Pe64RuntimeImportBinding {
	helper      Pe64RuntimeHelperKind
	dll         string
	export_name string
}

struct Pe64ImageDefinition {
	target_abi      Pe64TargetAbi
	subsystem       Pe64Subsystem
	image_policy    Pe64ImagePolicy
	entry           Pe64EntryDefinition
	imports         []Pe64ImportBinding
	runtime_helpers []Pe64RuntimeBinding
	runtime_imports []Pe64RuntimeImportBinding
}

struct Pe64BindingPlan {
	symbol_id   SymbolID
	dll         string
	export_name string
}

struct Pe64PhysicalImport {
	dll         string
	export_name string
}

struct Pe64ImportPlan {
	physical                        []Pe64PhysicalImport
	symbol_physical_index           []int
	process_exit_physical_index     int
	get_process_heap_physical_index int
	heap_alloc_physical_index       int
	heap_free_physical_index        int
}

struct Pe64RuntimeBindingPlan {
	symbol_id SymbolID
	helper    Pe64RuntimeHelperKind
}

struct Pe64RuntimePlan {
	physical                    []Pe64RuntimeHelperKind
	physical_offsets            []u64
	symbol_physical_index       []int
	process_exit_physical_index int
	malloc_physical_index       int
	free_physical_index         int
	calloc_physical_index       int
	size                        u64
}

struct Pe64ImportGroup {
	dll   string
	start int
mut:
	count int
}

struct Pe64IdataLayout {
	groups                []Pe64ImportGroup
	ilt_offsets           []u64
	iat_offsets           []u64
	hint_name_offsets     []u64
	dll_name_offsets      []u64
	import_directory_size u64
	iat_offset            u64
	iat_size              u64
	size                  u64
}

struct Pe64Idata {
	data        []u8
	iat_rvas    []u32
	import_size u32
	iat_rva     u32
	iat_size    u32
}

struct Pe64Section {
	name            string
	virtual_size    u32
	virtual_address u32
	raw_size        u32
	raw_pointer     u32
	characteristics u32
}

struct Pe64Layout {
	sections      []Pe64Section
	header_size   u64
	size_of_image u64
	file_size     u64
	text_index    int
	pdata_index   int
	xdata_index   int
	data_index    int
	idata_index   int
}

struct Pe64IndexedFunction {
	symbol_id SymbolID
	offset    u64
	size      u64
}

struct Pe64Nonleaf {
	symbol_id SymbolID
	end       u32
	unwind    u32
}

fn pe64_checked_add(left u64, right u64, label string) !u64 {
	if left > max_u64 - right {
		return error('PE64 layout: ${label} overflows u64')
	}
	return left + right
}

fn pe64_checked_mul(left u64, right u64, label string) !u64 {
	if left != 0 && right > max_u64 / left {
		return error('PE64 layout: ${label} overflows u64')
	}
	return left * right
}

fn pe64_align(value u64, alignment u64, label string) !u64 {
	if alignment == 0 || alignment & (alignment - 1) != 0 {
		return error('PE64 layout: ${label} has invalid alignment ${alignment}')
	}
	remainder := value & (alignment - 1)
	if remainder == 0 {
		return value
	}
	return pe64_checked_add(value, alignment - remainder, '${label} alignment')
}

fn pe64_require_u32(value u64, label string) !u32 {
	if value > u64(max_u32) {
		return error('PE64 layout: ${label} exceeds the PE32+ RVA/file limit')
	}
	return u32(value)
}

fn pe64_checked_host_size(value u64, label string) !int {
	if value > u64(max_int) {
		return error('PE64 layout: ${label} exceeds the host array limit')
	}
	return int(value)
}

fn pe64_checked_rel32(field_address u64, target_address u64) !u32 {
	if field_address > max_u64 - 4 {
		return error('PE64 REL32 overflow')
	}
	next_instruction := field_address + 4
	if target_address >= next_instruction {
		displacement := target_address - next_instruction
		if displacement > u64(2_147_483_647) {
			return error('PE64 REL32 overflow')
		}
		return u32(displacement)
	}
	magnitude := next_instruction - target_address
	if magnitude > u64(2_147_483_648) {
		return error('PE64 REL32 overflow')
	}
	displacement := -i64(magnitude)
	return u32(displacement & i64(0xffff_ffff))
}

fn pe64_write_u16(mut output []u8, value u16) {
	output << u8(value)
	output << u8(value >> 8)
}

fn pe64_write_u32(mut output []u8, value u32) {
	output << u8(value)
	output << u8(value >> 8)
	output << u8(value >> 16)
	output << u8(value >> 24)
}

fn pe64_write_u64(mut output []u8, value u64) {
	output << u8(value)
	output << u8(value >> 8)
	output << u8(value >> 16)
	output << u8(value >> 24)
	output << u8(value >> 32)
	output << u8(value >> 40)
	output << u8(value >> 48)
	output << u8(value >> 56)
}

fn pe64_put_u32(mut output []u8, offset u64, value u32, label string) ! {
	if offset > u64(output.len) || u64(4) > u64(output.len) - offset {
		return error('PE64 layout: ${label} is outside its output buffer')
	}
	index := pe64_checked_host_size(offset, label)!
	output[index] = u8(value)
	output[index + 1] = u8(value >> 8)
	output[index + 2] = u8(value >> 16)
	output[index + 3] = u8(value >> 24)
}

fn pe64_put_u64(mut output []u8, offset u64, value u64, label string) ! {
	if offset > u64(output.len) || u64(8) > u64(output.len) - offset {
		return error('PE64 layout: ${label} is outside its output buffer')
	}
	index := pe64_checked_host_size(offset, label)!
	for byte_index in 0 .. 8 {
		output[index + byte_index] = u8(value >> (byte_index * 8))
	}
}

fn pe64_read_u16(data []u8, offset u64, label string) !u16 {
	if offset > u64(data.len) || u64(2) > u64(data.len) - offset {
		return error('PE64 layout: ${label} is outside the final image')
	}
	index := pe64_checked_host_size(offset, label)!
	return u16(data[index]) | (u16(data[index + 1]) << 8)
}

fn pe64_read_u32(data []u8, offset u64, label string) !u32 {
	if offset > u64(data.len) || u64(4) > u64(data.len) - offset {
		return error('PE64 layout: ${label} is outside the final image')
	}
	index := pe64_checked_host_size(offset, label)!
	return u32(data[index]) | (u32(data[index + 1]) << 8) | (u32(data[index + 2]) << 16) | (u32(data[
		index + 3]) << 24)
}

fn pe64_read_u64(data []u8, offset u64, label string) !u64 {
	low := u64(pe64_read_u32(data, offset, label)!)
	high_offset := pe64_checked_add(offset, 4, '${label} high word')!
	high := u64(pe64_read_u32(data, high_offset, label)!)
	return low | (high << 32)
}

fn pe64_pad_to(mut output []u8, target u64) ! {
	_ = pe64_checked_host_size(target, 'output offset')!
	if u64(output.len) > target {
		return error('PE64 layout: output moved backwards')
	}
	for u64(output.len) < target {
		output << u8(0)
	}
}

fn pe64_write_short_name(mut output []u8, name string) ! {
	if name.len == 0 || name.len > 8 {
		return error('PE64 layout: section name `${name}` is invalid')
	}
	output << name.bytes()
	for _ in name.len .. 8 {
		output << u8(0)
	}
}

fn pe64_ascii_lower(byte u8) u8 {
	if byte >= `A` && byte <= `Z` {
		return byte + 32
	}
	return byte
}

fn pe64_ascii_case_equal(left string, right string) bool {
	if left.len != right.len {
		return false
	}
	for index in 0 .. left.len {
		if pe64_ascii_lower(left[index]) != pe64_ascii_lower(right[index]) {
			return false
		}
	}
	return true
}

fn pe64_validate_import_name(value string, kind string) ! {
	if value.len == 0 {
		return error('PE64 import binding: ${kind} must not be empty')
	}
	for index in 0 .. value.len {
		byte := value[index]
		if byte == 0 {
			return error('PE64 import binding: ${kind} must not contain NUL')
		}
		if byte > 0x7f {
			return error('PE64 import binding: ${kind} must be ASCII')
		}
	}
}

fn pe64_binding_less(left Pe64BindingPlan, right Pe64BindingPlan) bool {
	if left.dll != right.dll {
		return left.dll < right.dll
	}
	if left.export_name != right.export_name {
		return left.export_name < right.export_name
	}
	return left.symbol_id < right.symbol_id
}

fn pe64_sort_binding_plans(mut plans []Pe64BindingPlan) {
	for index := 1; index < plans.len; index++ {
		mut cursor := index
		for cursor > 0 && pe64_binding_less(plans[cursor], plans[cursor - 1]) {
			plans[cursor - 1], plans[cursor] = plans[cursor], plans[cursor - 1]
			cursor--
		}
	}
}

fn pe64_runtime_binding_less(left Pe64RuntimeBindingPlan, right Pe64RuntimeBindingPlan) bool {
	if left.helper != right.helper {
		return int(left.helper) < int(right.helper)
	}
	return left.symbol_id < right.symbol_id
}

fn pe64_sort_runtime_binding_plans(mut plans []Pe64RuntimeBindingPlan) {
	for index := 1; index < plans.len; index++ {
		mut cursor := index
		for cursor > 0 && pe64_runtime_binding_less(plans[cursor], plans[cursor - 1]) {
			plans[cursor - 1], plans[cursor] = plans[cursor], plans[cursor - 1]
			cursor--
		}
	}
}

fn pe64_runtime_physical_helper(helper Pe64RuntimeHelperKind) !Pe64RuntimeHelperKind {
	return match helper {
		.strlen, .wcslen, .memset, .memcmp, .memmove, .process_exit, .malloc, .free, .calloc {
			helper
		}
		.memcpy {
			Pe64RuntimeHelperKind.memmove
		}
		else {
			error('PE64 runtime binding: helper ${int(helper)} is unsupported')
		}
	}
}

fn pe64_runtime_helper_size(helper Pe64RuntimeHelperKind) !u64 {
	match helper {
		.strlen {
			return pe64_runtime_strlen_size
		}
		.wcslen {
			return pe64_runtime_wcslen_size
		}
		.memset {
			return pe64_runtime_memset_size
		}
		.memcmp {
			return pe64_runtime_memcmp_size
		}
		.memmove, .memcpy {
			return pe64_runtime_move_size
		}
		.process_exit {
			return pe64_runtime_process_exit_size
		}
		.malloc {
			return pe64_runtime_malloc_size
		}
		.free {
			return pe64_runtime_free_size
		}
		.calloc {
			return pe64_runtime_calloc_size
		}
		else {
			return error('PE64 runtime binding: helper ${int(helper)} is unsupported')
		}
	}
}

fn pe64_runtime_helper_bytes(helper Pe64RuntimeHelperKind) ![]u8 {
	match helper {
		.strlen {
			return [u8(0x48), 0x89, 0xc8, 0x80, 0x38, 0x00, 0x74, 0x05, 0x48, 0xff, 0xc0, 0xeb,
				0xf6, 0x48, 0x29, 0xc8, 0xc3]
		}
		.wcslen {
			return [u8(0x48), 0x89, 0xc8, 0x66, 0x83, 0x38, 0x00, 0x74, 0x06, 0x48, 0x83, 0xc0,
				0x02, 0xeb, 0xf4, 0x48, 0x29, 0xc8, 0x48, 0xd1, 0xe8, 0xc3]
		}
		.memset {
			return [u8(0x48), 0x89, 0xc8, 0x4d, 0x85, 0xc0, 0x74, 0x0e, 0x49, 0x89, 0xca, 0x41,
				0x88, 0x12, 0x49, 0xff, 0xc2, 0x49, 0xff, 0xc8, 0x75, 0xf5, 0xc3]
		}
		.memcmp {
			return [u8(0x4d), 0x85, 0xc0, 0x74, 0x16, 0x44, 0x8a, 0x09, 0x44, 0x8a, 0x12, 0x45,
				0x38, 0xd1, 0x75, 0x0e, 0x48, 0xff, 0xc1, 0x48, 0xff, 0xc2, 0x49, 0xff, 0xc8, 0x75,
				0xea, 0x29, 0xc0, 0xc3, 0x41, 0x0f, 0xb6, 0xc1, 0x45, 0x0f, 0xb6, 0xd2, 0x44, 0x29,
				0xd0, 0xc3]
		}
		.memmove, .memcpy {
			return [u8(0x48), 0x89, 0xc8, 0x4d, 0x85, 0xc0, 0x74, 0x41, 0x48, 0x39, 0xd1, 0x76,
				0x25, 0x4e, 0x8d, 0x0c, 0x02, 0x4c, 0x39, 0xc9, 0x73, 0x1c, 0x4e, 0x8d, 0x54, 0x01,
				0xff, 0x4e, 0x8d, 0x5c, 0x02, 0xff, 0x45, 0x8a, 0x0b, 0x45, 0x88, 0x0a, 0x49, 0xff,
				0xca, 0x49, 0xff, 0xcb, 0x49, 0xff, 0xc8, 0x75, 0xef, 0xc3, 0x49, 0x89, 0xca, 0x49,
				0x89, 0xd3, 0x45, 0x8a, 0x0b, 0x45, 0x88, 0x0a, 0x49, 0xff, 0xc2, 0x49, 0xff, 0xc3,
				0x49, 0xff, 0xc8, 0x75, 0xef, 0xc3]
		}
		.process_exit {
			return [u8(0x48), 0x83, 0xec, 0x28, 0xe8, 0, 0, 0, 0, 0xcc]
		}
		.malloc {
			return [u8(0x48), 0x83, 0xec, 0x28, 0x48, 0x89, 0x4c, 0x24, 0x20, 0xe8, 0, 0, 0, 0,
				0x48, 0x85, 0xc0, 0x74, 0x31, 0x48, 0x89, 0xc1, 0x31, 0xd2, 0x4c, 0x8b, 0x44, 0x24,
				0x20, 0x49, 0x83, 0xc0, 0x18, 0x72, 0x21, 0xe8, 0, 0, 0, 0, 0x48, 0x85, 0xc0, 0x74,
				0x17, 0x49, 0x89, 0xc3, 0x49, 0x83, 0xc3, 0x17, 0x49, 0x83, 0xe3, 0xf0, 0x49, 0x89,
				0x43, 0xf8, 0x4c, 0x89, 0xd8, 0x48, 0x83, 0xc4, 0x28, 0xc3, 0x31, 0xc0, 0x48, 0x83,
				0xc4, 0x28, 0xc3]
		}
		.free {
			return [u8(0x48), 0x83, 0xec, 0x28, 0x48, 0x85, 0xc9, 0x74, 0x22, 0x48, 0x8b, 0x41,
				0xf8, 0x48, 0x89, 0x44, 0x24, 0x20, 0xe8, 0, 0, 0, 0, 0x48, 0x85, 0xc0, 0x74, 0x0f,
				0x48, 0x89, 0xc1, 0x31, 0xd2, 0x4c, 0x8b, 0x44, 0x24, 0x20, 0xe8, 0, 0, 0, 0, 0x48,
				0x83, 0xc4, 0x28, 0xc3]
		}
		.calloc {
			return [u8(0x48), 0x83, 0xec, 0x28, 0x48, 0x89, 0xc8, 0x48, 0xf7, 0xe2, 0x48, 0x85,
				0xd2, 0x75, 0x43, 0x48, 0x89, 0x44, 0x24, 0x20, 0xe8, 0, 0, 0, 0, 0x48, 0x85, 0xc0,
				0x74, 0x34, 0x48, 0x89, 0xc1, 0xba, 0x08, 0, 0, 0, 0x4c, 0x8b, 0x44, 0x24, 0x20,
				0x49, 0x83, 0xc0, 0x18, 0x72, 0x21, 0xe8, 0, 0, 0, 0, 0x48, 0x85, 0xc0, 0x74, 0x17,
				0x49, 0x89, 0xc3, 0x49, 0x83, 0xc3, 0x17, 0x49, 0x83, 0xe3, 0xf0, 0x49, 0x89, 0x43,
				0xf8, 0x4c, 0x89, 0xd8, 0x48, 0x83, 0xc4, 0x28, 0xc3, 0x31, 0xc0, 0x48, 0x83, 0xc4,
				0x28, 0xc3]
		}
		else {
			return error('PE64 runtime binding: helper ${int(helper)} is unsupported')
		}
	}
}

fn pe64_prepare_runtime_helpers(o &Object, bindings []Pe64RuntimeBinding) !Pe64RuntimePlan {
	mut symbol_physical_index := []int{len: o.symbols.len, init: -1}
	if bindings.len == 0 {
		return Pe64RuntimePlan{
			symbol_physical_index:       symbol_physical_index
			process_exit_physical_index: -1
			malloc_physical_index:       -1
			free_physical_index:         -1
			calloc_physical_index:       -1
		}
	}
	mut seen := []bool{len: o.symbols.len}
	mut plans := []Pe64RuntimeBindingPlan{cap: bindings.len}
	for binding in bindings {
		_ = pe64_runtime_helper_size(binding.helper)!
		if u64(binding.symbol_id) >= u64(o.symbols.len) {
			return error('PE64 runtime binding: SymbolID ${u64(binding.symbol_id)} is out of range')
		}
		index := int(binding.symbol_id)
		if seen[index] {
			return error('PE64 runtime binding: duplicate binding for SymbolID ${u64(binding.symbol_id)}')
		}
		if !o.symbols[index].intentional_external {
			return error('PE64 runtime binding: SymbolID ${u64(binding.symbol_id)} is not an external function')
		}
		seen[index] = true
		plans << Pe64RuntimeBindingPlan{
			symbol_id: binding.symbol_id
			helper:    binding.helper
		}
	}
	pe64_sort_runtime_binding_plans(mut plans)
	mut physical := []Pe64RuntimeHelperKind{cap: plans.len}
	mut physical_offsets := []u64{cap: plans.len}
	mut process_exit_physical_index := -1
	mut malloc_physical_index := -1
	mut free_physical_index := -1
	mut calloc_physical_index := -1
	mut size := u64(0)
	for plan in plans {
		physical_helper := pe64_runtime_physical_helper(plan.helper)!
		if physical.len == 0 || physical[physical.len - 1] != physical_helper {
			physical_index := physical.len
			physical << physical_helper
			physical_offsets << pe64_checked_add(u64(o.text.len), size, 'runtime helper offset')!
			size = pe64_checked_add(size, pe64_runtime_helper_size(physical_helper)!,
				'runtime helper table size')!
			if physical_helper == .process_exit {
				process_exit_physical_index = physical_index
			} else if physical_helper == .malloc {
				malloc_physical_index = physical_index
			} else if physical_helper == .free {
				free_physical_index = physical_index
			} else if physical_helper == .calloc {
				calloc_physical_index = physical_index
			}
		}
		symbol_physical_index[int(plan.symbol_id)] = physical.len - 1
	}
	return Pe64RuntimePlan{
		physical:                    physical
		physical_offsets:            physical_offsets
		symbol_physical_index:       symbol_physical_index
		process_exit_physical_index: process_exit_physical_index
		malloc_physical_index:       malloc_physical_index
		free_physical_index:         free_physical_index
		calloc_physical_index:       calloc_physical_index
		size:                        size
	}
}

fn pe64_ensure_physical_import(mut physical []Pe64PhysicalImport, mut symbol_physical_index []int,
	dll string, export_name string) {
	mut insertion_index := physical.len
	for index, item in physical {
		if item.dll == dll && item.export_name == export_name {
			return
		}
		if dll < item.dll || (dll == item.dll && export_name < item.export_name) {
			insertion_index = index
			break
		}
	}
	physical.insert(insertion_index, Pe64PhysicalImport{
		dll:         dll
		export_name: export_name
	})
	for index, physical_index in symbol_physical_index {
		if physical_index >= insertion_index {
			symbol_physical_index[index]++
		}
	}
}

fn pe64_find_physical_import(physical []Pe64PhysicalImport, dll string, export_name string) int {
	for index, item in physical {
		if item.dll == dll && item.export_name == export_name {
			return index
		}
	}
	return -1
}

fn pe64_prepare_ordinary_imports(o &Object, bindings []Pe64ImportBinding) ![]Pe64BindingPlan {
	mut seen := []bool{len: o.symbols.len}
	mut plans := []Pe64BindingPlan{cap: bindings.len}
	for binding in bindings {
		if u64(binding.symbol_id) >= u64(o.symbols.len) {
			return error('PE64 import binding: SymbolID ${u64(binding.symbol_id)} is out of range')
		}
		index := int(binding.symbol_id)
		if seen[index] {
			return error('PE64 import binding: duplicate binding for SymbolID ${u64(binding.symbol_id)}')
		}
		if !o.symbols[index].intentional_external {
			return error('PE64 import binding: SymbolID ${u64(binding.symbol_id)} is not an external function')
		}
		pe64_validate_import_name(binding.dll, 'DLL name')!
		pe64_validate_import_name(binding.export_name, 'export name')!
		for previous in plans {
			if previous.dll != binding.dll && pe64_ascii_case_equal(previous.dll, binding.dll) {
				return error('PE64 import binding: DLL spellings `${previous.dll}` and `${binding.dll}` differ only by ASCII case')
			}
		}
		seen[index] = true
		plans << Pe64BindingPlan{
			symbol_id:   binding.symbol_id
			dll:         binding.dll.clone()
			export_name: binding.export_name.clone()
		}
	}
	pe64_sort_binding_plans(mut plans)
	return plans
}

fn pe64_prepare_imports(o &Object, bindings []Pe64ImportBinding, runtime_imports []Pe64RuntimeImportBinding,
	runtime_helpers &Pe64RuntimePlan) !Pe64ImportPlan {
	ordinary_imports := pe64_prepare_ordinary_imports(o, bindings)!
	return pe64_prepare_imports_from_plans(o, ordinary_imports, runtime_imports, runtime_helpers)
}

fn pe64_prepare_imports_from_plans(o &Object, ordinary_imports []Pe64BindingPlan,
	runtime_imports []Pe64RuntimeImportBinding,
	runtime_helpers &Pe64RuntimePlan) !Pe64ImportPlan {
	if runtime_helpers.symbol_physical_index.len != o.symbols.len
		|| runtime_helpers.physical_offsets.len != runtime_helpers.physical.len {
		return error('PE64 runtime binding: internal plan shape mismatch')
	}
	process_exit_helper_index := runtime_helpers.process_exit_physical_index
	malloc_helper_index := runtime_helpers.malloc_physical_index
	free_helper_index := runtime_helpers.free_physical_index
	calloc_helper_index := runtime_helpers.calloc_physical_index
	if process_exit_helper_index >= runtime_helpers.physical.len
		|| (process_exit_helper_index >= 0
		&& runtime_helpers.physical[process_exit_helper_index] != .process_exit) {
		return error('PE64 runtime binding: process_exit physical index is invalid')
	}
	if malloc_helper_index >= runtime_helpers.physical.len
		|| (malloc_helper_index >= 0 && runtime_helpers.physical[malloc_helper_index] != .malloc) {
		return error('PE64 runtime binding: malloc physical index is invalid')
	}
	if free_helper_index >= runtime_helpers.physical.len
		|| (free_helper_index >= 0 && runtime_helpers.physical[free_helper_index] != .free) {
		return error('PE64 runtime binding: free physical index is invalid')
	}
	if calloc_helper_index >= runtime_helpers.physical.len
		|| (calloc_helper_index >= 0 && runtime_helpers.physical[calloc_helper_index] != .calloc) {
		return error('PE64 runtime binding: calloc physical index is invalid')
	}
	mut saw_process_exit_helper := false
	mut saw_malloc_helper := false
	mut saw_free_helper := false
	mut saw_calloc_helper := false
	for index, helper in runtime_helpers.physical {
		match helper {
			.process_exit {
				if saw_process_exit_helper || index != process_exit_helper_index {
					return error('PE64 runtime binding: process_exit physical ownership is ambiguous')
				}
				saw_process_exit_helper = true
			}
			.malloc {
				if saw_malloc_helper || index != malloc_helper_index {
					return error('PE64 runtime binding: malloc physical ownership is ambiguous')
				}
				saw_malloc_helper = true
			}
			.free {
				if saw_free_helper || index != free_helper_index {
					return error('PE64 runtime binding: free physical ownership is ambiguous')
				}
				saw_free_helper = true
			}
			.calloc {
				if saw_calloc_helper || index != calloc_helper_index {
					return error('PE64 runtime binding: calloc physical ownership is ambiguous')
				}
				saw_calloc_helper = true
			}
			else {}
		}
	}
	if saw_process_exit_helper != (process_exit_helper_index >= 0) {
		return error('PE64 runtime binding: process_exit physical ownership is inconsistent')
	}
	if saw_malloc_helper != (malloc_helper_index >= 0) {
		return error('PE64 runtime binding: malloc physical ownership is inconsistent')
	}
	if saw_free_helper != (free_helper_index >= 0) {
		return error('PE64 runtime binding: free physical ownership is inconsistent')
	}
	if saw_calloc_helper != (calloc_helper_index >= 0) {
		return error('PE64 runtime binding: calloc physical ownership is inconsistent')
	}
	mut has_process_exit_import := false
	mut has_malloc_get_process_heap := false
	mut has_malloc_heap_alloc := false
	mut has_free_get_process_heap := false
	mut has_free_heap_free := false
	mut has_calloc_get_process_heap := false
	mut has_calloc_heap_alloc := false
	for binding in runtime_imports {
		match binding.helper {
			.process_exit {
				if has_process_exit_import {
					return error('PE64 runtime import binding: duplicate binding for process_exit')
				}
				if binding.dll != pe64_runtime_process_exit_dll
					|| binding.export_name != pe64_runtime_process_exit_export {
					return error('PE64 runtime import binding: process_exit requires exact `Kernel32.dll` / `ExitProcess`')
				}
				has_process_exit_import = true
			}
			.malloc {
				if binding.dll != pe64_runtime_heap_dll {
					return error('PE64 runtime import binding: malloc requires exact `Kernel32.dll` / `GetProcessHeap` or `HeapAlloc`')
				}
				if binding.export_name == pe64_runtime_get_process_heap_export {
					if has_malloc_get_process_heap {
						return error('PE64 runtime import binding: duplicate malloc GetProcessHeap binding')
					}
					has_malloc_get_process_heap = true
				} else if binding.export_name == pe64_runtime_heap_alloc_export {
					if has_malloc_heap_alloc {
						return error('PE64 runtime import binding: duplicate malloc HeapAlloc binding')
					}
					has_malloc_heap_alloc = true
				} else {
					return error('PE64 runtime import binding: malloc requires exact `Kernel32.dll` / `GetProcessHeap` or `HeapAlloc`')
				}
			}
			.free {
				if binding.dll != pe64_runtime_heap_dll {
					return error('PE64 runtime import binding: free requires exact `Kernel32.dll` / `GetProcessHeap` or `HeapFree`')
				}
				if binding.export_name == pe64_runtime_get_process_heap_export {
					if has_free_get_process_heap {
						return error('PE64 runtime import binding: duplicate free GetProcessHeap binding')
					}
					has_free_get_process_heap = true
				} else if binding.export_name == pe64_runtime_heap_free_export {
					if has_free_heap_free {
						return error('PE64 runtime import binding: duplicate free HeapFree binding')
					}
					has_free_heap_free = true
				} else {
					return error('PE64 runtime import binding: free requires exact `Kernel32.dll` / `GetProcessHeap` or `HeapFree`')
				}
			}
			.calloc {
				if binding.dll != pe64_runtime_heap_dll {
					return error('PE64 runtime import binding: calloc requires exact `Kernel32.dll` / `GetProcessHeap` or `HeapAlloc`')
				}
				if binding.export_name == pe64_runtime_get_process_heap_export {
					if has_calloc_get_process_heap {
						return error('PE64 runtime import binding: duplicate calloc GetProcessHeap binding')
					}
					has_calloc_get_process_heap = true
				} else if binding.export_name == pe64_runtime_heap_alloc_export {
					if has_calloc_heap_alloc {
						return error('PE64 runtime import binding: duplicate calloc HeapAlloc binding')
					}
					has_calloc_heap_alloc = true
				} else {
					return error('PE64 runtime import binding: calloc requires exact `Kernel32.dll` / `GetProcessHeap` or `HeapAlloc`')
				}
			}
			else {
				return error('PE64 runtime import binding: helper ${int(binding.helper)} is unsupported')
			}
		}
	}
	if process_exit_helper_index >= 0 && !has_process_exit_import {
		return error('PE64 runtime import binding: process_exit binding is missing')
	}
	if process_exit_helper_index < 0 && has_process_exit_import {
		return error('PE64 runtime import binding: process_exit binding is orphaned')
	}
	if malloc_helper_index >= 0 {
		if !has_malloc_get_process_heap {
			return error('PE64 runtime import binding: malloc GetProcessHeap binding is missing')
		}
		if !has_malloc_heap_alloc {
			return error('PE64 runtime import binding: malloc HeapAlloc binding is missing')
		}
	} else if has_malloc_get_process_heap || has_malloc_heap_alloc {
		return error('PE64 runtime import binding: malloc binding is orphaned')
	}
	if free_helper_index >= 0 {
		if !has_free_get_process_heap {
			return error('PE64 runtime import binding: free GetProcessHeap binding is missing')
		}
		if !has_free_heap_free {
			return error('PE64 runtime import binding: free HeapFree binding is missing')
		}
	} else if has_free_get_process_heap || has_free_heap_free {
		return error('PE64 runtime import binding: free binding is orphaned')
	}
	if calloc_helper_index >= 0 {
		if !has_calloc_get_process_heap {
			return error('PE64 runtime import binding: calloc GetProcessHeap binding is missing')
		}
		if !has_calloc_heap_alloc {
			return error('PE64 runtime import binding: calloc HeapAlloc binding is missing')
		}
	} else if has_calloc_get_process_heap || has_calloc_heap_alloc {
		return error('PE64 runtime import binding: calloc binding is orphaned')
	}
	mut resolved := []bool{len: o.symbols.len}
	for index, physical_index in runtime_helpers.symbol_physical_index {
		if physical_index >= 0 {
			if physical_index >= runtime_helpers.physical.len {
				return error('PE64 runtime binding: internal physical index is out of range')
			}
			resolved[index] = true
		}
	}
	for binding in ordinary_imports {
		index := int(binding.symbol_id)
		if resolved[index] {
			return error('PE64 resolution: SymbolID ${u64(binding.symbol_id)} has both import and runtime bindings')
		}
		has_kernel32_runtime_import := has_process_exit_import || has_malloc_get_process_heap
			|| has_malloc_heap_alloc || has_free_get_process_heap || has_free_heap_free
			|| has_calloc_get_process_heap || has_calloc_heap_alloc
		if has_kernel32_runtime_import && binding.dll != pe64_runtime_heap_dll
			&& pe64_ascii_case_equal(binding.dll, pe64_runtime_heap_dll) {
			return error('PE64 import binding: DLL spellings `${pe64_runtime_heap_dll}` and `${binding.dll}` differ only by ASCII case')
		}
		resolved[index] = true
	}
	for index, symbol in o.symbols {
		if symbol.intentional_external && !resolved[index] {
			return error('PE64 import binding missing for SymbolID ${index} (`${symbol.name}`)')
		}
	}
	mut physical := []Pe64PhysicalImport{cap: ordinary_imports.len}
	mut symbol_physical_index := []int{len: o.symbols.len, init: -1}
	for plan in ordinary_imports {
		if physical.len == 0 || physical[physical.len - 1].dll != plan.dll
			|| physical[physical.len - 1].export_name != plan.export_name {
			physical << Pe64PhysicalImport{
				dll:         plan.dll.clone()
				export_name: plan.export_name.clone()
			}
		}
		symbol_physical_index[int(plan.symbol_id)] = physical.len - 1
	}
	if has_process_exit_import {
		pe64_ensure_physical_import(mut physical, mut symbol_physical_index,
			pe64_runtime_process_exit_dll, pe64_runtime_process_exit_export)
	}
	if has_malloc_get_process_heap || has_free_get_process_heap || has_calloc_get_process_heap {
		pe64_ensure_physical_import(mut physical, mut symbol_physical_index, pe64_runtime_heap_dll,
			pe64_runtime_get_process_heap_export)
	}
	if has_malloc_heap_alloc || has_calloc_heap_alloc {
		pe64_ensure_physical_import(mut physical, mut symbol_physical_index, pe64_runtime_heap_dll,
			pe64_runtime_heap_alloc_export)
	}
	if has_free_heap_free {
		pe64_ensure_physical_import(mut physical, mut symbol_physical_index, pe64_runtime_heap_dll,
			pe64_runtime_heap_free_export)
	}
	process_exit_import_index := if has_process_exit_import {
		pe64_find_physical_import(physical, pe64_runtime_process_exit_dll,
			pe64_runtime_process_exit_export)
	} else {
		-1
	}
	get_process_heap_import_index := if has_malloc_get_process_heap || has_free_get_process_heap
		|| has_calloc_get_process_heap {
		pe64_find_physical_import(physical, pe64_runtime_heap_dll,
			pe64_runtime_get_process_heap_export)
	} else {
		-1
	}
	heap_alloc_import_index := if has_malloc_heap_alloc || has_calloc_heap_alloc {
		pe64_find_physical_import(physical, pe64_runtime_heap_dll, pe64_runtime_heap_alloc_export)
	} else {
		-1
	}
	heap_free_import_index := if has_free_heap_free {
		pe64_find_physical_import(physical, pe64_runtime_heap_dll, pe64_runtime_heap_free_export)
	} else {
		-1
	}
	if (has_process_exit_import && process_exit_import_index < 0) || ((has_malloc_get_process_heap
		|| has_free_get_process_heap || has_calloc_get_process_heap)
		&& get_process_heap_import_index < 0)
		|| ((has_malloc_heap_alloc || has_calloc_heap_alloc) && heap_alloc_import_index < 0)
		|| (has_free_heap_free && heap_free_import_index < 0) {
		return error('PE64 runtime import binding: physical import materialization is incomplete')
	}
	return Pe64ImportPlan{
		physical:                        physical
		symbol_physical_index:           symbol_physical_index
		process_exit_physical_index:     process_exit_import_index
		get_process_heap_physical_index: get_process_heap_import_index
		heap_alloc_physical_index:       heap_alloc_import_index
		heap_free_physical_index:        heap_free_import_index
	}
}

fn pe64_build_idata_layout(imports []Pe64PhysicalImport) !Pe64IdataLayout {
	if imports.len == 0 {
		return Pe64IdataLayout{}
	}
	mut groups := []Pe64ImportGroup{}
	for index, item in imports {
		if groups.len == 0 || groups[groups.len - 1].dll != item.dll {
			groups << Pe64ImportGroup{
				dll:   item.dll.clone()
				start: index
				count: 1
			}
		} else {
			groups[groups.len - 1].count++
		}
	}
	descriptor_count := pe64_checked_add(u64(groups.len), 1, 'import descriptor count')!
	import_directory_size := pe64_checked_mul(descriptor_count, pe64_import_descriptor_size,
		'import descriptor table size')!
	mut cursor := import_directory_size
	mut ilt_offsets := []u64{cap: groups.len}
	for group in groups {
		ilt_offsets << cursor
		entry_count := pe64_checked_add(u64(group.count), 1, 'ILT entry count')!
		cursor = pe64_checked_add(cursor, pe64_checked_mul(entry_count, 8, 'ILT size')!,
			'ILT extent')!
	}
	iat_offset := cursor
	mut iat_offsets := []u64{cap: groups.len}
	for group in groups {
		iat_offsets << cursor
		entry_count := pe64_checked_add(u64(group.count), 1, 'IAT entry count')!
		cursor = pe64_checked_add(cursor, pe64_checked_mul(entry_count, 8, 'IAT size')!,
			'IAT extent')!
	}
	iat_size := cursor - iat_offset
	mut hint_name_offsets := []u64{cap: imports.len}
	for item in imports {
		cursor = pe64_align(cursor, 2, 'hint/name record')!
		hint_name_offsets << cursor
		record_size := pe64_checked_add(u64(item.export_name.len), 3, 'hint/name record size')!
		cursor = pe64_checked_add(cursor, record_size, 'hint/name record extent')!
	}
	mut dll_name_offsets := []u64{cap: groups.len}
	for group in groups {
		cursor = pe64_align(cursor, 2, 'DLL name')!
		dll_name_offsets << cursor
		name_size := pe64_checked_add(u64(group.dll.len), 1, 'DLL name size')!
		cursor = pe64_checked_add(cursor, name_size, 'DLL name extent')!
	}
	_ = pe64_checked_host_size(cursor, '.idata size')!
	return Pe64IdataLayout{
		groups:                groups
		ilt_offsets:           ilt_offsets
		iat_offsets:           iat_offsets
		hint_name_offsets:     hint_name_offsets
		dll_name_offsets:      dll_name_offsets
		import_directory_size: import_directory_size
		iat_offset:            iat_offset
		iat_size:              iat_size
		size:                  cursor
	}
}

fn pe64_put_string(mut output []u8, offset u64, value string, label string) ! {
	end := pe64_checked_add(offset, u64(value.len), '${label} extent')!
	if end > u64(output.len) {
		return error('PE64 layout: ${label} is outside its output buffer')
	}
	start_index := pe64_checked_host_size(offset, label)!
	for index in 0 .. value.len {
		output[start_index + index] = value[index]
	}
}

fn pe64_build_idata(imports []Pe64PhysicalImport, idata_layout &Pe64IdataLayout, idata_rva u32) !Pe64Idata {
	if imports.len == 0 {
		if idata_layout.size != 0 {
			return error('PE64 layout: empty imports have a nonempty .idata plan')
		}
		return Pe64Idata{}
	}
	mut data := []u8{len: pe64_checked_host_size(idata_layout.size, '.idata size')!}
	mut iat_rvas := []u32{len: imports.len}
	for group_index, group in idata_layout.groups {
		descriptor_offset := pe64_checked_mul(u64(group_index), pe64_import_descriptor_size,
			'import descriptor offset')!
		ilt_rva := pe64_checked_add(u64(idata_rva), idata_layout.ilt_offsets[group_index],
			'ILT RVA')!
		dll_name_rva := pe64_checked_add(u64(idata_rva),
			idata_layout.dll_name_offsets[group_index], 'DLL name RVA')!
		iat_rva := pe64_checked_add(u64(idata_rva), idata_layout.iat_offsets[group_index],
			'IAT RVA')!
		pe64_put_u32(mut data, descriptor_offset, pe64_require_u32(ilt_rva, 'ILT RVA')!,
			'import descriptor ILT')!
		pe64_put_u32(mut data, pe64_checked_add(descriptor_offset, 12,
			'import descriptor name field')!, pe64_require_u32(dll_name_rva, 'DLL name RVA')!,
			'import descriptor name')!
		pe64_put_u32(mut data, pe64_checked_add(descriptor_offset, 16,
			'import descriptor IAT field')!, pe64_require_u32(iat_rva, 'IAT RVA')!,
			'import descriptor IAT')!
		for local_index in 0 .. group.count {
			import_index := group.start + local_index
			hint_name_rva := pe64_checked_add(u64(idata_rva),
				idata_layout.hint_name_offsets[import_index], 'hint/name RVA')!
			entry_delta := pe64_checked_mul(u64(local_index), 8, 'import table entry offset')!
			ilt_entry := pe64_checked_add(idata_layout.ilt_offsets[group_index], entry_delta,
				'ILT entry')!
			iat_entry := pe64_checked_add(idata_layout.iat_offsets[group_index], entry_delta,
				'IAT entry')!
			pe64_put_u64(mut data, ilt_entry, hint_name_rva, 'ILT entry')!
			pe64_put_u64(mut data, iat_entry, hint_name_rva, 'IAT entry')!
			iat_entry_rva := pe64_checked_add(u64(idata_rva), iat_entry, 'IAT entry RVA')!
			iat_rvas[import_index] = pe64_require_u32(iat_entry_rva, 'IAT entry RVA')!
		}
		pe64_put_string(mut data, idata_layout.dll_name_offsets[group_index], group.dll, 'DLL name')!
	}
	for index, item in imports {
		name_offset := pe64_checked_add(idata_layout.hint_name_offsets[index], 2,
			'hint/name string offset')!
		pe64_put_string(mut data, name_offset, item.export_name, 'import export name')!
	}
	iat_directory_rva := pe64_checked_add(u64(idata_rva), idata_layout.iat_offset,
		'IAT directory RVA')!
	return Pe64Idata{
		data:        data
		iat_rvas:    iat_rvas
		import_size: pe64_require_u32(idata_layout.import_directory_size, 'import directory size')!
		iat_rva:     pe64_require_u32(iat_directory_rva, 'IAT directory RVA')!
		iat_size:    pe64_require_u32(idata_layout.iat_size, 'IAT directory size')!
	}
}

fn pe64_make_section(name string, size u64, virtual_address u64, raw_pointer u64, characteristics u32) !Pe64Section {
	if size == 0 {
		return error('PE64 layout: section `${name}` must not be empty')
	}
	raw_size := pe64_align(size, pe64_file_alignment, '${name} raw size')!
	return Pe64Section{
		name:            name
		virtual_size:    pe64_require_u32(size, '${name} virtual size')!
		virtual_address: pe64_require_u32(virtual_address, '${name} virtual address')!
		raw_size:        pe64_require_u32(raw_size, '${name} raw size')!
		raw_pointer:     pe64_require_u32(raw_pointer, '${name} raw pointer')!
		characteristics: characteristics
	}
}

fn pe64_advance_section(section Pe64Section) !(u64, u64) {
	virtual_end := pe64_checked_add(u64(section.virtual_address), u64(section.virtual_size),
		'${section.name} virtual extent')!
	next_rva := pe64_align(virtual_end, pe64_section_alignment,
		'${section.name} next virtual address')!
	next_raw := pe64_checked_add(u64(section.raw_pointer), u64(section.raw_size),
		'${section.name} raw extent')!
	return next_rva, next_raw
}

fn pe64_build_layout(text_size u64, data_size u64, idata_size u64, nonleaf_count u64) !Pe64Layout {
	if text_size == 0 {
		return error('PE64 layout: .text must not be empty')
	}
	mut section_count := u64(1)
	if nonleaf_count != 0 {
		section_count = pe64_checked_add(section_count, 2, 'section count')!
	}
	if data_size != 0 {
		section_count = pe64_checked_add(section_count, 1, 'section count')!
	}
	if idata_size != 0 {
		section_count = pe64_checked_add(section_count, 1, 'section count')!
	}
	if section_count > pe64_max_sections {
		return error('PE64 layout: section count ${section_count} exceeds ${pe64_max_sections}')
	}
	section_headers := pe64_checked_mul(section_count, pe64_section_header_size,
		'section header table size')!
	header_extent := pe64_checked_add(pe64_dos_stub_size, pe64_checked_add(4, pe64_checked_add(pe64_file_header_size, pe64_checked_add(pe64_optional_header_size,
		section_headers, 'optional and section headers')!, 'PE headers')!,
		'PE signature and headers')!, 'complete header extent')!
	header_size := pe64_align(header_extent, pe64_file_alignment, 'header size')!
	mut next_rva := pe64_section_alignment
	mut next_raw := header_size
	mut sections := []Pe64Section{cap: int(section_count)}

	text_index := sections.len
	text_section := pe64_make_section('.text', text_size, next_rva, next_raw,
		pe64_section_contains_code | pe64_section_mem_execute | pe64_section_mem_read)!
	sections << text_section
	next_rva, next_raw = pe64_advance_section(text_section)!

	mut pdata_index := -1
	mut xdata_index := -1
	if nonleaf_count != 0 {
		pdata_size := pe64_checked_mul(nonleaf_count, pe64_runtime_function_size, '.pdata size')!
		xdata_size := pe64_checked_mul(nonleaf_count, pe64_unwind_info_size, '.xdata size')!
		pdata_index = sections.len
		pdata_section := pe64_make_section('.pdata', pdata_size, next_rva, next_raw,
			pe64_section_contains_initialized_data | pe64_section_mem_read)!
		sections << pdata_section
		next_rva, next_raw = pe64_advance_section(pdata_section)!
		xdata_index = sections.len
		xdata_section := pe64_make_section('.xdata', xdata_size, next_rva, next_raw,
			pe64_section_contains_initialized_data | pe64_section_mem_read)!
		sections << xdata_section
		next_rva, next_raw = pe64_advance_section(xdata_section)!
	}

	mut data_index := -1
	if data_size != 0 {
		data_index = sections.len
		data_section := pe64_make_section('.data', data_size, next_rva, next_raw,
			pe64_section_contains_initialized_data | pe64_section_mem_read | pe64_section_mem_write)!
		sections << data_section
		next_rva, next_raw = pe64_advance_section(data_section)!
	}

	mut idata_index := -1
	if idata_size != 0 {
		idata_index = sections.len
		idata_section := pe64_make_section('.idata', idata_size, next_rva, next_raw,
			pe64_section_contains_initialized_data | pe64_section_mem_read | pe64_section_mem_write)!
		sections << idata_section
		next_rva, next_raw = pe64_advance_section(idata_section)!
	}
	if u64(sections.len) != section_count {
		return error('PE64 layout: internal section count mismatch')
	}
	if next_rva > pe64_max_size_of_image {
		return error('PE64 layout: SizeOfImage ${next_rva} exceeds ${pe64_max_size_of_image}')
	}
	_ = pe64_require_u32(next_rva, 'size of image')!
	_ = pe64_require_u32(next_raw, 'final file size')!
	_ = pe64_checked_host_size(next_raw, 'final image size')!
	return Pe64Layout{
		sections:      sections
		header_size:   header_size
		size_of_image: next_rva
		file_size:     next_raw
		text_index:    text_index
		pdata_index:   pdata_index
		xdata_index:   xdata_index
		data_index:    data_index
		idata_index:   idata_index
	}
}

fn pe64_validate_definition(o &Object, definition &Pe64ImageDefinition) !int {
	if definition.target_abi != .windows_x64_microsoft {
		return error('PE64 requires Microsoft x64 ABI')
	}
	if definition.subsystem != .windows_cui {
		return error('PE64 image definition: subsystem ${int(definition.subsystem)} is unsupported')
	}
	if definition.image_policy != .fixed_base {
		return error('PE64 image definition: image policy ${int(definition.image_policy)} is unsupported')
	}
	if definition.entry.policy != .raw_noreturn_process_entry {
		return error('PE64 entry policy ${int(definition.entry.policy)} is unsupported')
	}
	if definition.entry.parameter_count != 0 {
		return error('PE64 entry policy requires a zero-parameter raw process entry')
	}
	if u64(definition.entry.function_index) >= u64(o.symbols.len) {
		return error('PE64 entry function index ${definition.entry.function_index} is out of range')
	}
	entry_index := int(definition.entry.function_index)
	entry_symbol := o.symbols[entry_index]
	if entry_symbol.intentional_external || !entry_symbol.defined {
		return error('PE64 entry function index ${definition.entry.function_index} is not a defined function')
	}
	return entry_index
}

fn pe64_prepare_nonleafs(o &Object) ![]Pe64Nonleaf {
	mut functions := []Pe64IndexedFunction{cap: o.symbols.len}
	for symbol_index, symbol in o.symbols {
		if symbol.intentional_external {
			continue
		}
		functions << Pe64IndexedFunction{
			symbol_id: SymbolID(symbol_index)
			offset:    symbol.offset
			size:      symbol.size
		}
	}
	functions.sort(a.offset < b.offset)

	mut relocations := o.call_relocations.clone()
	relocations.sort(a.offset < b.offset)
	mut nonleafs := []Pe64Nonleaf{cap: functions.len}
	mut relocation_index := 0
	for function in functions {
		function_end := pe64_checked_add(function.offset, function.size, 'function end')!
		mut owns_call := false
		for relocation_index < relocations.len {
			relocation := relocations[relocation_index]
			if relocation.offset == 0 {
				return error('PE64 unwind contract: CALL relocation has no opcode byte')
			}
			call_start := relocation.offset - 1
			field_end := pe64_checked_add(relocation.offset, 4, 'CALL relocation field end')!
			if call_start >= function_end {
				break
			}
			if call_start < function.offset || field_end > function_end {
				return error('PE64 unwind contract: CALL relocation is not owned by the ordered function')
			}
			owns_call = true
			relocation_index++
		}
		if owns_call {
			unwind_offset := pe64_checked_mul(u64(nonleafs.len), pe64_unwind_info_size,
				'.xdata record offset')!
			nonleafs << Pe64Nonleaf{
				symbol_id: function.symbol_id
				end:       pe64_require_u32(function_end, 'function end')!
				unwind:    pe64_require_u32(unwind_offset, '.xdata record offset')!
			}
		}
	}
	if relocation_index != relocations.len {
		return error('PE64 unwind contract: ownership scan did not consume every CALL relocation')
	}
	return nonleafs
}

fn pe64_validate_nonleaf_prologs(o &Object, nonleafs []Pe64Nonleaf) ! {
	for nonleaf in nonleafs {
		if u64(nonleaf.symbol_id) >= u64(o.symbols.len) {
			return error('PE64 unwind contract: non-leaf SymbolID is out of range')
		}
		symbol := o.symbols[int(nonleaf.symbol_id)]
		if symbol.intentional_external || !symbol.defined || symbol.size < 4 {
			return error('PE64 unwind contract: non-leaf SymbolID ${nonleaf.symbol_id} has no canonical prolog')
		}
		start := pe64_checked_host_size(symbol.offset, 'non-leaf function offset')!
		if start > o.text.len - 4 || o.text[start] != 0x48 || o.text[start + 1] != 0x83
			|| o.text[start + 2] != 0xec || o.text[start + 3] != 0x28 {
			return error('PE64 unwind contract: non-leaf SymbolID ${nonleaf.symbol_id} does not begin with `48 83 ec 28`')
		}
	}
}

fn pe64_require_runtime_import(imports &Pe64ImportPlan, physical_index int, dll string,
	export_name string, label string) ! {
	if physical_index < 0 || physical_index >= imports.physical.len {
		return error('PE64 runtime import binding: ${label} physical index is invalid')
	}
	item := imports.physical[physical_index]
	if item.dll != dll || item.export_name != export_name {
		return error('PE64 runtime import binding: ${label} physical tuple is invalid')
	}
}

fn pe64_patch_runtime_import_call(mut text []u8, helper_offset u64, local_field_offset u64,
	import_physical_index int, import_thunk_base u64, text_rva u32, label string) ! {
	if import_physical_index < 0 {
		return error('PE64 runtime import binding: ${label} physical index is invalid')
	}
	field_offset := pe64_checked_add(helper_offset, local_field_offset, '${label} CALL field')!
	thunk_delta := pe64_checked_mul(u64(import_physical_index), pe64_import_thunk_size,
		'${label} import thunk offset')!
	thunk_offset :=
		pe64_checked_add(import_thunk_base, thunk_delta, '${label} import thunk target')!
	field_rva := pe64_checked_add(u64(text_rva), field_offset, '${label} CALL field RVA')!
	thunk_rva := pe64_checked_add(u64(text_rva), thunk_offset, '${label} import thunk RVA')!
	displacement := pe64_checked_rel32(field_rva, thunk_rva)!
	pe64_put_u32(mut text, field_offset, displacement, '${label} CALL field')!
}

fn pe64_link_text(o &Object, runtime_helpers &Pe64RuntimePlan, imports &Pe64ImportPlan, idata &Pe64Idata, text_rva u32) ![]u8 {
	if runtime_helpers.symbol_physical_index.len != o.symbols.len
		|| runtime_helpers.physical_offsets.len != runtime_helpers.physical.len
		|| imports.symbol_physical_index.len != o.symbols.len {
		return error('PE64 layout: resolution plan shape mismatch')
	}
	mut text := o.text.clone()
	for physical_index, helper in runtime_helpers.physical {
		if u64(text.len) != runtime_helpers.physical_offsets[physical_index] {
			return error('PE64 layout: runtime helper offset does not match preflight')
		}
		body := pe64_runtime_helper_bytes(helper)!
		if u64(body.len) != pe64_runtime_helper_size(helper)! {
			return error('PE64 layout: runtime helper size does not match preflight')
		}
		text << body
	}
	runtime_end := pe64_checked_add(u64(o.text.len), runtime_helpers.size,
		'runtime helper table end')!
	if u64(text.len) != runtime_end {
		return error('PE64 layout: runtime helper table size does not match preflight')
	}
	import_thunk_base := runtime_end
	for _ in imports.physical {
		text << [u8(0xff), 0x25, 0, 0, 0, 0]
	}
	process_exit_helper_index := runtime_helpers.process_exit_physical_index
	process_exit_import_index := imports.process_exit_physical_index
	if (process_exit_helper_index >= 0) != (process_exit_import_index >= 0) {
		return error('PE64 runtime import binding: process_exit ownership is inconsistent')
	}
	if process_exit_helper_index >= 0 {
		if process_exit_helper_index >= runtime_helpers.physical.len
			|| runtime_helpers.physical[process_exit_helper_index] != .process_exit {
			return error('PE64 runtime binding: process_exit physical index is invalid')
		}
		if process_exit_import_index >= imports.physical.len {
			return error('PE64 runtime import binding: process_exit physical index is invalid')
		}
		pe64_require_runtime_import(imports, process_exit_import_index,
			pe64_runtime_process_exit_dll, pe64_runtime_process_exit_export, 'process_exit')!
		helper_offset := runtime_helpers.physical_offsets[process_exit_helper_index]
		helper_end := pe64_checked_add(helper_offset, pe64_runtime_process_exit_size,
			'process_exit helper extent')!
		if helper_end > import_thunk_base {
			return error('PE64 layout: process_exit helper exceeds the runtime helper table')
		}
		pe64_patch_runtime_import_call(mut text, helper_offset, 5, process_exit_import_index,
			import_thunk_base, text_rva, 'process_exit')!
	}
	malloc_helper_index := runtime_helpers.malloc_physical_index
	calloc_helper_index := runtime_helpers.calloc_physical_index
	heap_alloc_import_index := imports.heap_alloc_physical_index
	if (malloc_helper_index >= 0 || calloc_helper_index >= 0) != (heap_alloc_import_index >= 0) {
		return error('PE64 runtime import binding: malloc/calloc ownership is inconsistent')
	}
	if malloc_helper_index >= 0 {
		if malloc_helper_index >= runtime_helpers.physical.len
			|| runtime_helpers.physical[malloc_helper_index] != .malloc {
			return error('PE64 runtime binding: malloc physical index is invalid')
		}
		get_process_heap_import_index := imports.get_process_heap_physical_index
		pe64_require_runtime_import(imports, get_process_heap_import_index, pe64_runtime_heap_dll,
			pe64_runtime_get_process_heap_export, 'malloc GetProcessHeap')!
		pe64_require_runtime_import(imports, heap_alloc_import_index, pe64_runtime_heap_dll,
			pe64_runtime_heap_alloc_export, 'malloc HeapAlloc')!
		helper_offset := runtime_helpers.physical_offsets[malloc_helper_index]
		helper_end := pe64_checked_add(helper_offset, pe64_runtime_malloc_size,
			'malloc helper extent')!
		if helper_end > import_thunk_base {
			return error('PE64 layout: malloc helper exceeds the runtime helper table')
		}
		pe64_patch_runtime_import_call(mut text, helper_offset, 10, get_process_heap_import_index,
			import_thunk_base, text_rva, 'malloc GetProcessHeap')!
		pe64_patch_runtime_import_call(mut text, helper_offset, 36, heap_alloc_import_index,
			import_thunk_base, text_rva, 'malloc HeapAlloc')!
	}
	free_helper_index := runtime_helpers.free_physical_index
	heap_free_import_index := imports.heap_free_physical_index
	if (free_helper_index >= 0) != (heap_free_import_index >= 0) {
		return error('PE64 runtime import binding: free ownership is inconsistent')
	}
	if free_helper_index >= 0 {
		if free_helper_index >= runtime_helpers.physical.len
			|| runtime_helpers.physical[free_helper_index] != .free {
			return error('PE64 runtime binding: free physical index is invalid')
		}
		get_process_heap_import_index := imports.get_process_heap_physical_index
		pe64_require_runtime_import(imports, get_process_heap_import_index, pe64_runtime_heap_dll,
			pe64_runtime_get_process_heap_export, 'free GetProcessHeap')!
		pe64_require_runtime_import(imports, heap_free_import_index, pe64_runtime_heap_dll,
			pe64_runtime_heap_free_export, 'free HeapFree')!
		helper_offset := runtime_helpers.physical_offsets[free_helper_index]
		helper_end := pe64_checked_add(helper_offset, pe64_runtime_free_size, 'free helper extent')!
		if helper_end > import_thunk_base {
			return error('PE64 layout: free helper exceeds the runtime helper table')
		}
		pe64_patch_runtime_import_call(mut text, helper_offset, 19, get_process_heap_import_index,
			import_thunk_base, text_rva, 'free GetProcessHeap')!
		pe64_patch_runtime_import_call(mut text, helper_offset, 39, heap_free_import_index,
			import_thunk_base, text_rva, 'free HeapFree')!
	}
	if calloc_helper_index >= 0 {
		if calloc_helper_index >= runtime_helpers.physical.len
			|| runtime_helpers.physical[calloc_helper_index] != .calloc {
			return error('PE64 runtime binding: calloc physical index is invalid')
		}
		get_process_heap_import_index := imports.get_process_heap_physical_index
		pe64_require_runtime_import(imports, get_process_heap_import_index, pe64_runtime_heap_dll,
			pe64_runtime_get_process_heap_export, 'calloc GetProcessHeap')!
		pe64_require_runtime_import(imports, heap_alloc_import_index, pe64_runtime_heap_dll,
			pe64_runtime_heap_alloc_export, 'calloc HeapAlloc')!
		helper_offset := runtime_helpers.physical_offsets[calloc_helper_index]
		helper_end := pe64_checked_add(helper_offset, pe64_runtime_calloc_size,
			'calloc helper extent')!
		if helper_end > import_thunk_base {
			return error('PE64 layout: calloc helper exceeds the runtime helper table')
		}
		pe64_patch_runtime_import_call(mut text, helper_offset, 21, get_process_heap_import_index,
			import_thunk_base, text_rva, 'calloc GetProcessHeap')!
		pe64_patch_runtime_import_call(mut text, helper_offset, 50, heap_alloc_import_index,
			import_thunk_base, text_rva, 'calloc HeapAlloc')!
	}
	for relocation in o.call_relocations {
		target_index := object_symbol_index(o, relocation.symbol_id) or {
			return error('PE64 object contract: ${err.msg()}')
		}
		target_symbol := o.symbols[target_index]
		mut target_offset := u64(0)
		if target_symbol.intentional_external {
			runtime_index := runtime_helpers.symbol_physical_index[target_index]
			import_index := imports.symbol_physical_index[target_index]
			if runtime_index >= 0 && import_index >= 0 {
				return error('PE64 resolution: SymbolID ${u64(relocation.symbol_id)} has both import and runtime bindings')
			}
			if runtime_index >= 0 {
				if runtime_index >= runtime_helpers.physical.len {
					return error('PE64 runtime binding: physical helper index is out of range')
				}
				target_offset = runtime_helpers.physical_offsets[runtime_index]
			} else if import_index >= 0 {
				if import_index >= imports.physical.len {
					return error('PE64 import binding: physical import index is out of range')
				}
				thunk_delta := pe64_checked_mul(u64(import_index), pe64_import_thunk_size,
					'import thunk offset')!
				target_offset = pe64_checked_add(import_thunk_base, thunk_delta,
					'import thunk target')!
			} else {
				return error('PE64 import binding missing for SymbolID ${u64(relocation.symbol_id)} (`${target_symbol.name}`)')
			}
		} else {
			if !target_symbol.defined {
				return error('PE64 object contract: CALL target SymbolID ${u64(relocation.symbol_id)} is undefined')
			}
			target_offset = target_symbol.offset
		}
		field_rva := pe64_checked_add(u64(text_rva), relocation.offset, 'CALL field RVA')!
		target_rva := pe64_checked_add(u64(text_rva), target_offset, 'CALL target RVA')!
		displacement := pe64_checked_rel32(field_rva, target_rva)!
		pe64_put_u32(mut text, relocation.offset, displacement, 'Object CALL field')!
	}
	if imports.physical.len != idata.iat_rvas.len {
		return error('PE64 layout: import thunk and IAT counts differ')
	}
	for physical_index in 0 .. imports.physical.len {
		thunk_delta := pe64_checked_mul(u64(physical_index), pe64_import_thunk_size,
			'import thunk offset')!
		thunk_offset := pe64_checked_add(import_thunk_base, thunk_delta, 'import thunk offset')!
		field_offset := pe64_checked_add(thunk_offset, 2, 'import thunk field offset')!
		field_rva := pe64_checked_add(u64(text_rva), field_offset, 'import thunk field RVA')!
		displacement := pe64_checked_rel32(field_rva, u64(idata.iat_rvas[physical_index]))!
		pe64_put_u32(mut text, field_offset, displacement, 'import thunk IAT field')!
	}
	expected_size := pe64_checked_add(import_thunk_base, pe64_checked_mul(u64(imports.physical.len),
		pe64_import_thunk_size, 'import thunk table size')!, 'linked text size')!
	if u64(text.len) != expected_size {
		return error('PE64 layout: linked text size does not match preflight')
	}
	return text
}

fn pe64_runtime_nonleaf_count(runtime_helpers &Pe64RuntimePlan) !u64 {
	mut count := u64(0)
	for helper in runtime_helpers.physical {
		if helper in [.process_exit, .malloc, .free, .calloc] {
			count = pe64_checked_add(count, 1, 'runtime function count')!
		}
	}
	return count
}

fn pe64_build_unwind_sections(o &Object, nonleafs []Pe64Nonleaf, runtime_helpers &Pe64RuntimePlan,
	text_rva u32, xdata_rva u32) !([]u8, []u8) {
	if runtime_helpers.physical_offsets.len != runtime_helpers.physical.len {
		return error('PE64 unwind contract: runtime helper plan shape mismatch')
	}
	runtime_nonleaf_count := pe64_runtime_nonleaf_count(runtime_helpers)!
	nonleaf_count := pe64_checked_add(u64(nonleafs.len), runtime_nonleaf_count,
		'runtime function count')!
	if nonleaf_count == 0 {
		return []u8{}, []u8{}
	}
	pdata_size := pe64_checked_mul(nonleaf_count, pe64_runtime_function_size, '.pdata size')!
	xdata_size := pe64_checked_mul(nonleaf_count, pe64_unwind_info_size, '.xdata size')!
	mut pdata := []u8{cap: pe64_checked_host_size(pdata_size, '.pdata size')!}
	mut xdata := []u8{cap: pe64_checked_host_size(xdata_size, '.xdata size')!}
	mut previous_begin := u64(0)
	for index, nonleaf in nonleafs {
		symbol := o.symbols[int(nonleaf.symbol_id)]
		begin := pe64_checked_add(u64(text_rva), symbol.offset, '.pdata begin RVA')!
		end := pe64_checked_add(u64(text_rva), u64(nonleaf.end), '.pdata end RVA')!
		expected_unwind_offset := pe64_checked_mul(u64(index), pe64_unwind_info_size,
			'.xdata record offset')!
		expected_unwind := pe64_require_u32(expected_unwind_offset, '.xdata record offset')!
		if nonleaf.unwind != expected_unwind {
			return error('PE64 unwind contract: unwind offsets are not canonical')
		}
		unwind := pe64_checked_add(u64(xdata_rva), expected_unwind_offset, '.pdata unwind RVA')!
		if index > 0 && begin <= previous_begin {
			return error('PE64 unwind contract: .pdata begin RVAs are not strictly sorted')
		}
		if end <= begin {
			return error('PE64 unwind contract: runtime function extent is empty')
		}
		pe64_write_u32(mut pdata, pe64_require_u32(begin, '.pdata begin RVA')!)
		pe64_write_u32(mut pdata, pe64_require_u32(end, '.pdata end RVA')!)
		pe64_write_u32(mut pdata, pe64_require_u32(unwind, '.pdata unwind RVA')!)
		xdata << [u8(0x01), 0x04, 0x01, 0x00, 0x04, 0x42, 0x00, 0x00]
		previous_begin = begin
	}
	runtime_end := pe64_checked_add(u64(o.text.len), runtime_helpers.size,
		'runtime helper table end')!
	mut runtime_nonleaf_index := u64(0)
	for helper_index, helper in runtime_helpers.physical {
		if helper !in [.process_exit, .malloc, .free, .calloc] {
			continue
		}
		begin_offset := runtime_helpers.physical_offsets[helper_index]
		helper_name := match helper {
			.process_exit { 'process_exit' }
			.malloc { 'malloc' }
			.free { 'free' }
			.calloc { 'calloc' }
			else { return error('PE64 unwind contract: runtime helper is not nonleaf') }
		}

		end_offset := pe64_checked_add(begin_offset, pe64_runtime_helper_size(helper)!,
			'${helper_name} runtime function extent')!
		if end_offset > runtime_end {
			return error('PE64 unwind contract: ${helper_name} extent exceeds the runtime helper table')
		}
		begin := pe64_checked_add(u64(text_rva), begin_offset, '.pdata ${helper_name} begin RVA')!
		end := pe64_checked_add(u64(text_rva), end_offset, '.pdata ${helper_name} end RVA')!
		if (nonleafs.len > 0 || runtime_nonleaf_index > 0) && begin <= previous_begin {
			return error('PE64 unwind contract: .pdata begin RVAs are not strictly sorted')
		}
		unwind_record_index := pe64_checked_add(u64(nonleafs.len), runtime_nonleaf_index,
			'.xdata runtime record index')!
		unwind_offset := pe64_checked_mul(unwind_record_index, pe64_unwind_info_size,
			'.xdata ${helper_name} record offset')!
		unwind := pe64_checked_add(u64(xdata_rva), unwind_offset,
			'.pdata ${helper_name} unwind RVA')!
		pe64_write_u32(mut pdata, pe64_require_u32(begin, '.pdata ${helper_name} begin RVA')!)
		pe64_write_u32(mut pdata, pe64_require_u32(end, '.pdata ${helper_name} end RVA')!)
		pe64_write_u32(mut pdata, pe64_require_u32(unwind, '.pdata ${helper_name} unwind RVA')!)
		xdata << [u8(0x01), 0x04, 0x01, 0x00, 0x04, 0x42, 0x00, 0x00]
		previous_begin = begin
		runtime_nonleaf_index++
	}
	if runtime_nonleaf_index != runtime_nonleaf_count {
		return error('PE64 unwind contract: runtime function count differs from preflight')
	}
	if u64(pdata.len) != pdata_size || u64(xdata.len) != xdata_size {
		return error('PE64 unwind contract: materialized unwind sizes differ from preflight')
	}
	return pdata, xdata
}

fn pe64_write_dos_header(mut output []u8) ! {
	output << [u8(`M`), `Z`]
	pe64_pad_to(mut output, 0x3c)!
	pe64_write_u32(mut output, pe64_require_u32(pe64_dos_stub_size, 'PE header offset')!)
	pe64_pad_to(mut output, pe64_dos_stub_size)!
}

fn pe64_write_section_header(mut output []u8, section Pe64Section) ! {
	pe64_write_short_name(mut output, section.name)!
	pe64_write_u32(mut output, section.virtual_size)
	pe64_write_u32(mut output, section.virtual_address)
	pe64_write_u32(mut output, section.raw_size)
	pe64_write_u32(mut output, section.raw_pointer)
	pe64_write_u32(mut output, 0)
	pe64_write_u32(mut output, 0)
	pe64_write_u16(mut output, 0)
	pe64_write_u16(mut output, 0)
	pe64_write_u32(mut output, section.characteristics)
}

fn pe64_write_optional_header(mut output []u8, layout &Pe64Layout, entry_rva u32, idata &Pe64Idata) ! {
	text_section := layout.sections[layout.text_index]
	mut initialized_size := u64(0)
	for index, section in layout.sections {
		if index != layout.text_index {
			initialized_size = pe64_checked_add(initialized_size, u64(section.raw_size),
				'initialized data size')!
		}
	}
	pe64_write_u16(mut output, pe64_optional_header_magic)
	output << pe64_linker_major_version
	output << pe64_linker_minor_version
	pe64_write_u32(mut output, text_section.raw_size)
	pe64_write_u32(mut output, pe64_require_u32(initialized_size, 'size of initialized data')!)
	pe64_write_u32(mut output, 0)
	pe64_write_u32(mut output, entry_rva)
	pe64_write_u32(mut output, text_section.virtual_address)
	pe64_write_u64(mut output, pe64_image_base)
	pe64_write_u32(mut output, pe64_require_u32(pe64_section_alignment, 'section alignment')!)
	pe64_write_u32(mut output, pe64_require_u32(pe64_file_alignment, 'file alignment')!)
	pe64_write_u16(mut output, pe64_major_operating_system_version)
	pe64_write_u16(mut output, pe64_minor_operating_system_version)
	pe64_write_u16(mut output, 0)
	pe64_write_u16(mut output, 0)
	pe64_write_u16(mut output, pe64_major_subsystem_version)
	pe64_write_u16(mut output, pe64_minor_subsystem_version)
	pe64_write_u32(mut output, 0)
	pe64_write_u32(mut output, pe64_require_u32(layout.size_of_image, 'size of image')!)
	pe64_write_u32(mut output, pe64_require_u32(layout.header_size, 'size of headers')!)
	pe64_write_u32(mut output, 0)
	pe64_write_u16(mut output, pe64_subsystem_windows_cui)
	pe64_write_u16(mut output, pe64_dll_characteristics_nx_compat)
	pe64_write_u64(mut output, pe64_size_of_stack_reserve)
	pe64_write_u64(mut output, pe64_size_of_stack_commit)
	pe64_write_u64(mut output, pe64_size_of_heap_reserve)
	pe64_write_u64(mut output, pe64_size_of_heap_commit)
	pe64_write_u32(mut output, 0)
	pe64_write_u32(mut output, u32(pe64_directory_count))
	for directory_index in 0 .. pe64_directory_count {
		mut rva := u32(0)
		mut size := u32(0)
		if directory_index == pe64_import_directory_index && layout.idata_index >= 0 {
			rva = layout.sections[layout.idata_index].virtual_address
			size = idata.import_size
		} else if directory_index == pe64_exception_directory_index && layout.pdata_index >= 0 {
			rva = layout.sections[layout.pdata_index].virtual_address
			size = layout.sections[layout.pdata_index].virtual_size
		} else if directory_index == pe64_iat_directory_index && layout.idata_index >= 0 {
			rva = idata.iat_rva
			size = idata.iat_size
		}
		pe64_write_u32(mut output, rva)
		pe64_write_u32(mut output, size)
	}
}

fn pe64_directory_is_contained(rva u32, size u32, section Pe64Section) bool {
	if size == 0 {
		return rva == 0
	}
	directory_end := u64(rva) + u64(size)
	section_end := u64(section.virtual_address) + u64(section.virtual_size)
	return rva >= section.virtual_address && directory_end <= section_end
}

fn pe64_validate_final_image(output []u8, layout &Pe64Layout, section_data [][]u8, entry_rva u32, idata &Pe64Idata) ! {
	if u64(output.len) != layout.file_size {
		return error('PE64 layout: final size ${output.len} does not match ${layout.file_size}')
	}
	if output.len < int(pe64_dos_stub_size) || output[0] != `M` || output[1] != `Z` {
		return error('PE64 layout: DOS header is malformed')
	}
	if pe64_read_u32(output, 0x3c, 'PE header pointer')! != u32(pe64_dos_stub_size) {
		return error('PE64 layout: DOS header does not point to the PE signature')
	}
	if pe64_read_u32(output, pe64_dos_stub_size, 'PE signature')! != pe64_signature {
		return error('PE64 layout: PE signature is malformed')
	}
	file_header := pe64_checked_add(pe64_dos_stub_size, 4, 'file header offset')!
	if pe64_read_u16(output, file_header, 'machine')! != pe64_machine_amd64
		|| pe64_read_u16(output, file_header + 2, 'section count')! != u16(layout.sections.len) {
		return error('PE64 layout: COFF file header is inconsistent')
	}
	characteristics := pe64_read_u16(output, file_header + 18, 'file characteristics')!
	expected_characteristics := pe64_image_file_relocs_stripped | pe64_image_file_executable_image | pe64_image_file_large_address_aware
	if characteristics != expected_characteristics {
		return error('PE64 layout: fixed-base file characteristics are inconsistent')
	}
	optional := pe64_checked_add(file_header, pe64_file_header_size, 'optional header offset')!
	if pe64_read_u16(output, optional, 'optional header magic')! != pe64_optional_header_magic
		|| pe64_read_u64(output, optional + 24, 'image base')! != pe64_image_base
		|| pe64_read_u32(output, optional + 56, 'size of image')! != u32(layout.size_of_image)
		|| pe64_read_u32(output, optional + 60, 'size of headers')! != u32(layout.header_size) {
		return error('PE64 layout: PE32+ optional header is inconsistent')
	}
	if pe64_read_u32(output, optional + 16, 'entry RVA')! != entry_rva {
		return error('PE64 layout: entry RVA is inconsistent')
	}
	dll_characteristics := pe64_read_u16(output, optional + 70, 'DLL characteristics')!
	if dll_characteristics != pe64_dll_characteristics_nx_compat
		|| dll_characteristics & (pe64_dll_characteristics_dynamic_base | pe64_dll_characteristics_high_entropy_va) != 0 {
		return error('PE64 layout: fixed-base DLL characteristics are inconsistent')
	}
	if pe64_read_u32(output, optional + 108, 'directory count')! != u32(pe64_directory_count) {
		return error('PE64 layout: data-directory count is inconsistent')
	}
	section_table_end := pe64_checked_add(optional, pe64_checked_add(pe64_optional_header_size, pe64_checked_mul(u64(layout.sections.len),
		pe64_section_header_size, 'section header table size')!,
		'optional and section header extent')!, 'complete section header extent')!
	header_padding_start := pe64_checked_host_size(section_table_end, 'header padding start')!
	header_padding_end := pe64_checked_host_size(layout.header_size, 'header padding end')!
	for offset in header_padding_start .. header_padding_end {
		if output[offset] != 0 {
			return error('PE64 layout: header padding is nonzero')
		}
	}
	for directory_index in 0 .. pe64_directory_count {
		directory_offset := optional + 112 + u64(directory_index * 8)
		rva := pe64_read_u32(output, directory_offset, 'directory RVA')!
		size := pe64_read_u32(output, directory_offset + 4, 'directory size')!
		if directory_index == pe64_import_directory_index && layout.idata_index >= 0 {
			idata_section := layout.sections[layout.idata_index]
			if rva != idata_section.virtual_address || size != idata.import_size
				|| !pe64_directory_is_contained(rva, size, idata_section) {
				return error('PE64 layout: import directory is outside .idata')
			}
		} else if directory_index == pe64_exception_directory_index && layout.pdata_index >= 0 {
			pdata_section := layout.sections[layout.pdata_index]
			if rva != pdata_section.virtual_address || size != pdata_section.virtual_size
				|| !pe64_directory_is_contained(rva, size, pdata_section) {
				return error('PE64 layout: exception directory is outside .pdata')
			}
		} else if directory_index == pe64_iat_directory_index && layout.idata_index >= 0 {
			if rva != idata.iat_rva || size != idata.iat_size
				|| !pe64_directory_is_contained(rva, size, layout.sections[layout.idata_index]) {
				return error('PE64 layout: IAT directory is outside .idata')
			}
		} else if rva != 0 || size != 0 {
			return error('PE64 layout: unused data directory ${directory_index} is nonzero')
		}
	}
	text := layout.sections[layout.text_index]
	if entry_rva < text.virtual_address
		|| u64(entry_rva) >= u64(text.virtual_address) + u64(text.virtual_size) {
		return error('PE64 layout: entry is outside executable .text')
	}
	if section_data.len != layout.sections.len {
		return error('PE64 layout: section data count mismatch')
	}
	mut previous_virtual_end := u64(0)
	mut previous_raw_end := layout.header_size
	for index, section in layout.sections {
		if u64(section.virtual_address) % pe64_section_alignment != 0
			|| u64(section.raw_pointer) % pe64_file_alignment != 0
			|| u64(section.raw_size) % pe64_file_alignment != 0 {
			return error('PE64 layout: section `${section.name}` alignment is invalid')
		}
		virtual_end := u64(section.virtual_address) + u64(section.virtual_size)
		raw_end := u64(section.raw_pointer) + u64(section.raw_size)
		expected_virtual_address := if index == 0 {
			pe64_section_alignment
		} else {
			pe64_align(previous_virtual_end, pe64_section_alignment, 'section adjacency')!
		}
		if u64(section.virtual_address) != expected_virtual_address {
			return error('PE64 layout: section virtual ranges are not adjacent')
		}
		if u64(section.raw_pointer) != previous_raw_end || raw_end > u64(output.len) {
			return error('PE64 layout: section raw ranges are not adjacent and contained')
		}
		if u64(section_data[index].len) != u64(section.virtual_size) {
			return error('PE64 layout: section `${section.name}` data size mismatch')
		}
		if section.characteristics & pe64_section_mem_execute != 0
			&& section.characteristics & pe64_section_mem_write != 0 {
			return error('PE64 layout: section `${section.name}` is writable and executable')
		}
		padding_start := u64(section.raw_pointer) + u64(section.virtual_size)
		padding_start_index := pe64_checked_host_size(padding_start, 'section padding start')!
		raw_end_index := pe64_checked_host_size(raw_end, 'section padding end')!
		for offset in padding_start_index .. raw_end_index {
			if output[offset] != 0 {
				return error('PE64 layout: section `${section.name}` padding is nonzero')
			}
		}
		previous_virtual_end = virtual_end
		previous_raw_end = raw_end
	}
}

// pe64_image_bytes consumes the canonical in-memory Object directly. Entry,
// Microsoft ABI provenance, fixed-base policy, subsystem, imports, runtime
// helpers, and helper-owned imports are explicit caller assertions; no symbol
// spelling selects policy. Startup, init, argv, and compiler routing remain
// upstream metadata and are neither inferred nor synthesized here.
fn pe64_image_bytes(o &Object, definition Pe64ImageDefinition) ![]u8 {
	o.validate() or { return error('PE64 object contract: ${err.msg()}') }
	entry_index := pe64_validate_definition(o, &definition)!
	ordinary_imports := pe64_prepare_ordinary_imports(o, definition.imports)!
	runtime_helpers := pe64_prepare_runtime_helpers(o, definition.runtime_helpers)!
	imports := pe64_prepare_imports_from_plans(o, ordinary_imports, definition.runtime_imports,
		&runtime_helpers)!
	nonleafs := pe64_prepare_nonleafs(o)!
	pe64_validate_nonleaf_prologs(o, nonleafs)!
	idata_layout := pe64_build_idata_layout(imports.physical)!
	linked_text_size := pe64_checked_add(pe64_checked_add(u64(o.text.len), runtime_helpers.size,
		'runtime helper table end')!, pe64_checked_mul(u64(imports.physical.len),
		pe64_import_thunk_size, 'import thunk table size')!, 'linked text size')!
	runtime_nonleaf_count := pe64_runtime_nonleaf_count(&runtime_helpers)!
	nonleaf_count := pe64_checked_add(u64(nonleafs.len), runtime_nonleaf_count,
		'runtime function count')!
	layout := pe64_build_layout(linked_text_size, u64(o.private_data.len), idata_layout.size,
		nonleaf_count)!
	text_section := layout.sections[layout.text_index]
	mut idata := Pe64Idata{}
	if layout.idata_index >= 0 {
		idata = pe64_build_idata(imports.physical, &idata_layout,
			layout.sections[layout.idata_index].virtual_address)!
	}
	linked_text := pe64_link_text(o, &runtime_helpers, &imports, &idata,
		text_section.virtual_address)!
	mut pdata := []u8{}
	mut xdata := []u8{}
	if layout.pdata_index >= 0 {
		pdata, xdata = pe64_build_unwind_sections(o, nonleafs, &runtime_helpers,
			text_section.virtual_address, layout.sections[layout.xdata_index].virtual_address)!
	}
	mut section_data := [][]u8{cap: layout.sections.len}
	section_data << linked_text
	if layout.pdata_index >= 0 {
		section_data << pdata
		section_data << xdata
	}
	if layout.data_index >= 0 {
		section_data << o.private_data.clone()
	}
	if layout.idata_index >= 0 {
		section_data << idata.data
	}
	entry_symbol := o.symbols[entry_index]
	entry_rva_u64 := pe64_checked_add(u64(text_section.virtual_address), entry_symbol.offset,
		'entry RVA')!
	entry_rva := pe64_require_u32(entry_rva_u64, 'entry RVA')!

	mut output := []u8{cap: pe64_checked_host_size(layout.file_size, 'final image size')!}
	pe64_write_dos_header(mut output)!
	pe64_write_u32(mut output, pe64_signature)
	pe64_write_u16(mut output, pe64_machine_amd64)
	pe64_write_u16(mut output, u16(layout.sections.len))
	pe64_write_u32(mut output, 0)
	pe64_write_u32(mut output, 0)
	pe64_write_u32(mut output, 0)
	pe64_write_u16(mut output, u16(pe64_optional_header_size))
	pe64_write_u16(mut output,
		pe64_image_file_relocs_stripped | pe64_image_file_executable_image | pe64_image_file_large_address_aware)
	optional_start := u64(output.len)
	pe64_write_optional_header(mut output, &layout, entry_rva, &idata)!
	if u64(output.len) != optional_start + pe64_optional_header_size {
		return error('PE64 layout: optional header size mismatch')
	}
	for section in layout.sections {
		pe64_write_section_header(mut output, section)!
	}
	if u64(output.len) > layout.header_size {
		return error('PE64 layout: section headers exceed SizeOfHeaders')
	}
	pe64_pad_to(mut output, layout.header_size)!
	for index, section in layout.sections {
		pe64_pad_to(mut output, u64(section.raw_pointer))!
		if u64(section_data[index].len) != u64(section.virtual_size) {
			return error('PE64 layout: materialized `${section.name}` size mismatch')
		}
		output << section_data[index]
		section_end := pe64_checked_add(u64(section.raw_pointer), u64(section.raw_size),
			'${section.name} raw extent')!
		pe64_pad_to(mut output, section_end)!
	}
	pe64_validate_final_image(output, &layout, section_data, entry_rva, &idata)!
	return output
}
