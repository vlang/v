module x64

import os
import rand
import v2.mir

fn pe_test_u16(data []u8, off int) u16 {
	return u16(data[off]) | (u16(data[off + 1]) << 8)
}

fn pe_test_u32(data []u8, off int) u32 {
	return u32(data[off]) | (u32(data[off + 1]) << 8) | (u32(data[off + 2]) << 16) | (u32(data[
		off + 3]) << 24)
}

fn pe_test_u64(data []u8, off int) u64 {
	return u64(pe_test_u32(data, off)) | (u64(pe_test_u32(data, off + 4)) << 32)
}

fn pe_test_i32(data []u8, off int) i32 {
	return i32(pe_test_u32(data, off))
}

fn pe_test_rel8_target(instruction_off int, disp u8) int {
	signed_disp := if int(disp) < 0x80 { int(disp) } else { int(disp) - 0x100 }
	return instruction_off + 2 + signed_disp
}

fn pe_test_string(data []u8, off int) string {
	mut end := off
	for end < data.len && data[end] != 0 {
		end++
	}
	return data[off..end].bytestr()
}

struct PeTestByteRange {
	label string
	start u32
	limit u32
}

fn pe_test_byte_range(label string, start u32, size u32) PeTestByteRange {
	return PeTestByteRange{
		label: label
		start: start
		limit: start + size
	}
}

fn pe_test_assert_range_in_file(image []u8, range PeTestByteRange) {
	assert range.start <= range.limit
	assert range.limit <= u32(image.len)
}

fn pe_test_assert_ranges_do_not_overlap(ranges []PeTestByteRange) {
	for i in 0 .. ranges.len {
		for j in i + 1 .. ranges.len {
			if ranges[i].start == ranges[i].limit || ranges[j].start == ranges[j].limit {
				continue
			}
			assert ranges[i].limit <= ranges[j].start || ranges[j].limit <= ranges[i].start
		}
	}
}

fn pe_test_section_header(image []u8, name string) int {
	pe_off := int(pe_test_u32(image, 0x3c))
	nsections := int(pe_test_u16(image, pe_off + 6))
	section_off := pe_off + 4 + 20 + int(pe_test_u16(image, pe_off + 20))
	for i in 0 .. nsections {
		off := section_off + i * 40
		if image[off..off + 8].bytestr().trim_right('\0') == name {
			return off
		}
	}
	return -1
}

fn pe_test_section_raw_ranges(image []u8) []PeTestByteRange {
	pe_off := int(pe_test_u32(image, 0x3c))
	nsections := int(pe_test_u16(image, pe_off + 6))
	section_off := pe_off + 4 + 20 + int(pe_test_u16(image, pe_off + 20))
	mut ranges := []PeTestByteRange{cap: nsections}
	for i in 0 .. nsections {
		off := section_off + i * 40
		raw_size := pe_test_u32(image, off + 16)
		raw_ptr := pe_test_u32(image, off + 20)
		if raw_size == 0 {
			continue
		}
		name := image[off..off + 8].bytestr().trim_right('\0')
		ranges << pe_test_byte_range(name, raw_ptr, raw_size)
	}
	return ranges
}

fn pe_test_section_virtual_ranges(image []u8) []PeTestByteRange {
	pe_off := int(pe_test_u32(image, 0x3c))
	nsections := int(pe_test_u16(image, pe_off + 6))
	section_off := pe_off + 4 + 20 + int(pe_test_u16(image, pe_off + 20))
	mut ranges := []PeTestByteRange{cap: nsections}
	for i in 0 .. nsections {
		off := section_off + i * 40
		virtual_size := pe_test_u32(image, off + 8)
		virtual_address := pe_test_u32(image, off + 12)
		name := image[off..off + 8].bytestr().trim_right('\0')
		ranges << pe_test_byte_range(name, virtual_address, u32(align_int(int(virtual_size),
			pe_section_alignment)))
	}
	return ranges
}

fn pe_test_assert_ranges_are_adjacent(ranges []PeTestByteRange) {
	if ranges.len < 2 {
		return
	}
	for i in 0 .. ranges.len - 1 {
		assert ranges[i].limit == ranges[i + 1].start
	}
}

fn pe_test_section_virtual_range(image []u8, name string) PeTestByteRange {
	section_off := pe_test_section_header(image, name)
	assert section_off >= 0
	virtual_size := pe_test_u32(image, section_off + 8)
	virtual_address := pe_test_u32(image, section_off + 12)
	return pe_test_byte_range(name, virtual_address, u32(align_int(int(virtual_size),
		pe_section_alignment)))
}

fn pe_test_assert_no_zero_sized_sections(image []u8) {
	pe_off := int(pe_test_u32(image, 0x3c))
	nsections := int(pe_test_u16(image, pe_off + 6))
	section_off := pe_off + 4 + 20 + int(pe_test_u16(image, pe_off + 20))
	for i in 0 .. nsections {
		off := section_off + i * 40
		virtual_size := pe_test_u32(image, off + 8)
		raw_size := pe_test_u32(image, off + 16)
		assert virtual_size != 0 || raw_size != 0
	}
}

fn pe_test_assert_size_of_image_is_exact(image []u8) {
	pe_off := int(pe_test_u32(image, 0x3c))
	opt_off := pe_off + 4 + 20
	ranges := pe_test_section_virtual_ranges(image)
	assert ranges.len > 0
	assert pe_test_u32(image, opt_off + 56) == ranges[ranges.len - 1].limit
}

fn pe_test_assert_entrypoint_in_executable_text(image []u8) {
	pe_off := int(pe_test_u32(image, 0x3c))
	opt_off := pe_off + 4 + 20
	entry_rva := pe_test_u32(image, opt_off + 16)
	text_off := pe_test_section_header(image, '.text')
	text_range := pe_test_section_virtual_range(image, '.text')
	assert entry_rva >= text_range.start
	assert entry_rva < text_range.limit
	assert pe_test_u32(image, text_off + 36) & pe_image_scn_mem_execute != 0
}

fn pe_test_assert_directory_contained_in_section(image []u8, directory_index int, section_name string) {
	pe_off := int(pe_test_u32(image, 0x3c))
	opt_off := pe_off + 4 + 20
	dir_rva := pe_test_u32(image, opt_off + 112 + directory_index * 8)
	dir_size := pe_test_u32(image, opt_off + 116 + directory_index * 8)
	section_range := pe_test_section_virtual_range(image, section_name)
	assert dir_rva >= section_range.start
	assert dir_rva + dir_size <= section_range.limit
}

fn pe_test_rva_to_file_off(image []u8, rva u32) int {
	pe_off := int(pe_test_u32(image, 0x3c))
	nsections := int(pe_test_u16(image, pe_off + 6))
	section_off := pe_off + 4 + 20 + int(pe_test_u16(image, pe_off + 20))
	for i in 0 .. nsections {
		off := section_off + i * 40
		va := pe_test_u32(image, off + 12)
		raw_size := pe_test_u32(image, off + 16)
		raw_ptr := pe_test_u32(image, off + 20)
		virtual_size := pe_test_u32(image, off + 8)
		size := if virtual_size > raw_size { virtual_size } else { raw_size }
		if rva >= va && rva < va + size {
			return int(raw_ptr + (rva - va))
		}
	}
	return -1
}

fn pe_test_import_descriptor_offsets(image []u8) []int {
	pe_off := int(pe_test_u32(image, 0x3c))
	opt_off := pe_off + 4 + 20
	import_rva := pe_test_u32(image, opt_off + 112 + pe_import_directory_index * 8)
	if import_rva == 0 {
		return []
	}
	import_off := pe_test_rva_to_file_off(image, import_rva)
	assert import_off > 0
	mut offsets := []int{}
	for i := 0; i < 32; i++ {
		descriptor_off := import_off + i * 20
		ilt_rva := pe_test_u32(image, descriptor_off)
		dll_name_rva := pe_test_u32(image, descriptor_off + 12)
		first_thunk_rva := pe_test_u32(image, descriptor_off + 16)
		if ilt_rva == 0 && dll_name_rva == 0 && first_thunk_rva == 0 {
			return offsets
		}
		assert ilt_rva != 0
		assert dll_name_rva != 0
		assert first_thunk_rva != 0
		offsets << descriptor_off
	}
	assert false
	return offsets
}

fn pe_test_import_names(image []u8) []string {
	mut names := []string{}
	for descriptor_off in pe_test_import_descriptor_offsets(image) {
		ilt_rva := pe_test_u32(image, descriptor_off)
		ilt_off := pe_test_rva_to_file_off(image, ilt_rva)
		assert ilt_off > 0
		for i := 0; i < 128; i++ {
			hint_name_rva := u32(pe_test_u64(image, ilt_off + i * 8))
			if hint_name_rva == 0 {
				break
			}
			hint_name_off := pe_test_rva_to_file_off(image, hint_name_rva)
			assert hint_name_off > 0
			assert pe_test_u16(image, hint_name_off) == 0
			assert hint_name_off % 2 == 0
			names << pe_test_string(image, hint_name_off + 2)
		}
	}
	return names
}

fn pe_test_import_dll_names(image []u8) []string {
	mut dlls := []string{}
	for descriptor_off in pe_test_import_descriptor_offsets(image) {
		dll_name_rva := pe_test_u32(image, descriptor_off + 12)
		dll_name_off := pe_test_rva_to_file_off(image, dll_name_rva)
		assert dll_name_off > 0
		dlls << pe_test_string(image, dll_name_off)
	}
	return dlls
}

fn pe_test_iat_size(image []u8) u32 {
	pe_off := int(pe_test_u32(image, 0x3c))
	opt_off := pe_off + 4 + 20
	return pe_test_u32(image, opt_off + 116 + pe_iat_directory_index * 8)
}

fn pe_test_text_rel32_target(image []u8, text_rva u32, text_raw int, field_off int) u32 {
	field_rva := text_rva + u32(field_off)
	disp := pe_test_i32(image, text_raw + field_off)
	return u32(i32(field_rva + 4) + disp)
}

fn pe_test_first_import_thunk_rva(image []u8, text_rva u32, text_raw int, text_start int) u32 {
	text_off := pe_test_section_header(image, '.text')
	text_raw_size := int(pe_test_u32(image, text_off + 16))
	for off in text_raw + text_start .. text_raw + text_raw_size - 1 {
		if image[off] == 0xff && image[off + 1] == 0x25 {
			return text_rva + u32(off - text_raw)
		}
	}
	assert false, 'missing PE import thunk in .text'
	return 0
}

fn pe_test_import_thunk_rva(image []u8, text_rva u32, text_raw int, text_start int, name string) u32 {
	names := pe_test_import_names(image)
	idx := names.index(name)
	if idx < 0 {
		assert false, 'missing PE import `${name}` in ${names}'
		return 0
	}
	return pe_test_first_import_thunk_rva(image, text_rva, text_raw, text_start) + u32(idx * 6)
}

fn sample_pe_coff_object() &CoffObject {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xc3), 0xc3]
	obj.rodata << 'Hello World!'.bytes()
	obj.rodata << 0
	obj.data_data << [u8(1), 2, 3, 4]
	obj.add_symbol('_vinit', 0, true, 1)
	obj.add_symbol('main', 1, true, 1)
	return obj
}

fn sample_pe_arguments_coff_object(with_vinit bool) &CoffObject {
	mut obj := CoffObject.new()
	mut main_off := 0
	if with_vinit {
		obj.text_data << u8(0xc3)
		obj.add_symbol('_vinit', 0, true, 1)
		main_off = obj.text_data.len
	}
	obj.text_data << [u8(0x8b), 0x05, 0, 0, 0, 0] // mov eax, [rip + g_main_argc]
	obj.text_data << [u8(0x48), 0x8b, 0x15, 0, 0, 0, 0] // mov rdx, [rip + g_main_argv]
	obj.text_data << u8(0xc3)
	obj.add_symbol('main', u64(main_off), true, 1)
	obj.data_data << [u8(0), 0, 0, 0, 0, 0, 0, 0]
	obj.data_data << [u8(0), 0, 0, 0, 0, 0, 0, 0]
	argc_sym := obj.add_symbol('g_main_argc', 0, false, 3)
	argv_sym := obj.add_symbol('g_main_argv', 8, false, 3)
	obj.add_text_reloc(main_off + 2, argc_sym, coff_image_rel_amd64_rel32)
	obj.add_text_reloc(main_off + 9, argv_sym, coff_image_rel_amd64_rel32)
	return obj
}

fn test_pe_linker_emits_pe32_plus_headers_and_sections() {
	obj := sample_pe_coff_object()
	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }

	assert image[0] == `M`
	assert image[1] == `Z`
	pe_off := int(pe_test_u32(image, 0x3c))
	assert pe_off == pe_dos_stub_size
	assert pe_test_u32(image, pe_off) == pe_signature
	assert pe_test_u16(image, pe_off + 4) == coff_image_file_machine_amd64
	assert pe_test_u16(image, pe_off + 6) == 4
	assert pe_test_u32(image, pe_off + 8) == 0
	assert pe_test_u32(image, pe_off + 12) == 0
	assert pe_test_u32(image, pe_off + 16) == 0
	assert pe_test_u16(image, pe_off + 20) == pe_size_of_optional_header64
	assert pe_test_u16(image, pe_off + 22) & pe_image_file_relocs_stripped != 0
	assert pe_test_u16(image, pe_off + 22) & pe_image_file_executable_image != 0
	assert pe_test_u16(image, pe_off + 22) & pe_image_file_large_address_aware != 0

	opt_off := pe_off + 4 + 20
	assert pe_test_u16(image, opt_off) == pe_optional_header64_magic
	assert image[opt_off + 2] == pe_linker_major_version
	assert image[opt_off + 3] == pe_linker_minor_version
	assert pe_test_u32(image, opt_off + 4) != 0
	assert pe_test_u32(image, opt_off + 8) != 0
	assert pe_test_u32(image, opt_off + 12) == 0
	assert pe_test_u32(image, opt_off + 16) == pe_section_alignment
	assert pe_test_u32(image, opt_off + 20) == pe_section_alignment
	assert pe_test_u64(image, opt_off + 24) == pe_image_base
	assert pe_test_u32(image, opt_off + 32) == pe_section_alignment
	assert pe_test_u32(image, opt_off + 36) == pe_file_alignment
	assert pe_test_u16(image, opt_off + 40) == pe_major_operating_system_version
	assert pe_test_u16(image, opt_off + 42) == pe_minor_operating_system_version
	assert pe_test_u16(image, opt_off + 48) == pe_major_subsystem_version
	assert pe_test_u16(image, opt_off + 50) == pe_minor_subsystem_version
	assert pe_test_u32(image, opt_off + 52) == 0
	assert pe_test_u32(image, opt_off + 60) == pe_headers_size(4)
	assert pe_test_u32(image, opt_off + 64) == 0
	assert pe_test_u16(image, opt_off + 68) == pe_image_subsystem_windows_cui
	assert pe_test_u16(image, opt_off + 70) & pe_dll_characteristics_nx_compat != 0
	assert pe_test_u64(image, opt_off + 72) == pe_size_of_stack_reserve
	assert pe_test_u64(image, opt_off + 80) == pe_size_of_stack_commit
	assert pe_test_u64(image, opt_off + 88) == pe_size_of_heap_reserve
	assert pe_test_u64(image, opt_off + 96) == pe_size_of_heap_commit
	assert pe_test_u32(image, opt_off + 104) == 0
	assert pe_test_u32(image, opt_off + 108) == pe_number_of_rva_and_sizes

	text_off := pe_test_section_header(image, '.text')
	rdata_off := pe_test_section_header(image, '.rdata')
	data_off := pe_test_section_header(image, '.data')
	idata_off := pe_test_section_header(image, '.idata')
	assert text_off > 0
	assert rdata_off > text_off
	assert data_off > rdata_off
	assert idata_off > data_off
	mut file_ranges := [
		pe_test_byte_range('PE headers', 0, pe_test_u32(image, opt_off + 60)),
	]
	file_ranges << pe_test_section_raw_ranges(image)
	for range in file_ranges {
		pe_test_assert_range_in_file(image, range)
	}
	pe_test_assert_ranges_do_not_overlap(file_ranges)
	pe_test_assert_ranges_are_adjacent(pe_test_section_virtual_ranges(image))
	pe_test_assert_no_zero_sized_sections(image)
	pe_test_assert_size_of_image_is_exact(image)
	pe_test_assert_entrypoint_in_executable_text(image)
	pe_test_assert_directory_contained_in_section(image, pe_import_directory_index, '.idata')
	pe_test_assert_directory_contained_in_section(image, pe_iat_directory_index, '.idata')
	assert pe_test_u32(image, text_off + 8) != 0
	assert pe_test_u32(image, text_off + 12) == pe_section_alignment
	assert pe_test_u32(image, text_off + 20) == pe_headers_size(4)
	assert pe_test_u32(image, text_off + 20) % pe_file_alignment == 0
	assert pe_test_u32(image, text_off + 16) % pe_file_alignment == 0
	assert pe_test_u32(image, text_off + 24) == 0
	assert pe_test_u32(image, text_off + 28) == 0
	assert pe_test_u16(image, text_off + 32) == 0
	assert pe_test_u16(image, text_off + 34) == 0
	assert pe_test_u32(image, text_off + 36) & pe_image_scn_cnt_code != 0
	assert pe_test_u32(image, text_off + 36) & pe_image_scn_mem_execute != 0
	assert pe_test_u32(image, text_off + 36) & pe_image_scn_mem_read != 0
	assert pe_test_u32(image, rdata_off + 8) == u32(obj.rodata.len)
	assert pe_test_u32(image, rdata_off + 12) % pe_section_alignment == 0
	assert pe_test_u32(image, rdata_off + 16) % pe_file_alignment == 0
	assert pe_test_u32(image, rdata_off + 20) % pe_file_alignment == 0
	assert pe_test_u32(image, rdata_off + 24) == 0
	assert pe_test_u16(image, rdata_off + 32) == 0
	assert pe_test_u32(image, rdata_off + 36) & pe_image_scn_cnt_initialized_data != 0
	assert pe_test_u32(image, rdata_off + 36) & pe_image_scn_mem_read != 0
	assert pe_test_u32(image, data_off + 8) == u32(obj.data_data.len)
	assert pe_test_u32(image, data_off + 16) % pe_file_alignment == 0
	assert pe_test_u32(image, data_off + 20) % pe_file_alignment == 0
	assert pe_test_u32(image, data_off + 24) == 0
	assert pe_test_u16(image, data_off + 32) == 0
	assert pe_test_u32(image, data_off + 36) & pe_image_scn_cnt_initialized_data != 0
	assert pe_test_u32(image, data_off + 36) & pe_image_scn_mem_read != 0
	assert pe_test_u32(image, data_off + 36) & pe_image_scn_mem_write != 0
	assert pe_test_u32(image, idata_off + 8) != 0
	assert pe_test_u32(image, idata_off + 12) % pe_section_alignment == 0
	assert pe_test_u32(image, idata_off + 16) % pe_file_alignment == 0
	assert pe_test_u32(image, idata_off + 20) % pe_file_alignment == 0
	assert pe_test_u32(image, idata_off + 24) == 0
	assert pe_test_u16(image, idata_off + 32) == 0
	assert pe_test_u32(image, idata_off + 36) & pe_image_scn_cnt_initialized_data != 0
	assert pe_test_u32(image, idata_off + 36) & pe_image_scn_mem_read != 0
	assert pe_test_u32(image, idata_off + 36) & pe_image_scn_mem_write != 0

	idata_end := pe_test_u32(image, idata_off + 12) + pe_section_alignment
	assert pe_test_u32(image, opt_off + 56) == idata_end
}

fn test_pe_linker_emits_demand_driven_kernel32_import_table() {
	obj := sample_pe_coff_object()
	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	expected_imports := ['ExitProcess']

	pe_off := int(pe_test_u32(image, 0x3c))
	opt_off := pe_off + 4 + 20
	import_rva := pe_test_u32(image, opt_off + 112 + pe_import_directory_index * 8)
	import_size := pe_test_u32(image, opt_off + 116 + pe_import_directory_index * 8)
	iat_rva := pe_test_u32(image, opt_off + 112 + pe_iat_directory_index * 8)
	iat_size := pe_test_u32(image, opt_off + 116 + pe_iat_directory_index * 8)
	assert import_rva != 0
	assert import_size == 40
	assert iat_rva != 0
	assert iat_size == u32((expected_imports.len + 1) * 8)

	import_off := pe_test_rva_to_file_off(image, import_rva)
	assert import_off > 0
	ilt_rva := pe_test_u32(image, import_off)
	dll_name_rva := pe_test_u32(image, import_off + 12)
	first_thunk_rva := pe_test_u32(image, import_off + 16)
	assert ilt_rva != 0
	assert first_thunk_rva == iat_rva
	assert pe_test_string(image, pe_test_rva_to_file_off(image, dll_name_rva)) == 'kernel32.dll'
	for i in 0 .. 20 {
		assert image[import_off + 20 + i] == 0
	}

	mut names := []string{}
	ilt_off := pe_test_rva_to_file_off(image, ilt_rva)
	iat_off := pe_test_rva_to_file_off(image, iat_rva)
	for i in 0 .. expected_imports.len {
		hint_name_rva := u32(pe_test_u64(image, ilt_off + i * 8))
		hint_name_off := pe_test_rva_to_file_off(image, hint_name_rva)
		assert pe_test_u16(image, hint_name_off) == 0
		assert hint_name_off % 2 == 0
		names << pe_test_string(image, hint_name_off + 2)
		assert pe_test_u64(image, iat_off + i * 8) == pe_test_u64(image, ilt_off + i * 8)
	}
	assert pe_test_u64(image, ilt_off + expected_imports.len * 8) == 0
	assert pe_test_u64(image, iat_off + expected_imports.len * 8) == 0
	assert names == expected_imports
	assert names == pe_test_import_names(image)
	assert pe_test_import_dll_names(image) == ['kernel32.dll']
	assert pe_test_iat_size(image) == u32((expected_imports.len + 1) * 8)
	assert 'HeapAlloc' !in names
	assert 'WriteFile' !in names
	assert 'GetCommandLineW' !in names
	assert 'CommandLineToArgvW' !in names
}

fn test_pe_linker_adds_direct_kernel32_imports_in_stable_order() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xe8, 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 10, true, 1)
	write_file_sym := obj.add_undefined('WriteFile')
	get_std_handle_sym := obj.add_undefined('GetStdHandle')
	obj.add_text_reloc(1, write_file_sym, coff_image_rel_amd64_rel32)
	obj.add_text_reloc(6, get_std_handle_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	names := pe_test_import_names(image)
	dll_names := pe_test_import_dll_names(image)

	assert names == ['ExitProcess', 'GetStdHandle', 'WriteFile']
	assert dll_names == ['kernel32.dll']
	assert pe_test_iat_size(image) == u32((names.len + 1) * 8)
	assert 'ucrtbase.dll' !in dll_names
}

fn test_pe_linker_imports_kernel32_readconsole_readfile_and_patches_call_to_import_thunks() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xe8, 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 10, true, 1)
	read_console_sym := obj.add_undefined('ReadConsole')
	read_file_sym := obj.add_undefined('ReadFile')
	obj.add_text_reloc(1, read_console_sym, coff_image_rel_amd64_rel32)
	obj.add_text_reloc(6, read_file_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	names := pe_test_import_names(image)
	dll_names := pe_test_import_dll_names(image)

	assert names == ['ExitProcess', 'ReadConsoleW', 'ReadFile']
	assert dll_names == ['kernel32.dll']
	assert pe_test_iat_size(image) == u32((names.len + 1) * 8)

	text_off := pe_test_section_header(image, '.text')
	text_rva := pe_test_u32(image, text_off + 12)
	text_raw := int(pe_test_u32(image, text_off + 20))
	read_console_thunk := pe_test_import_thunk_rva(image, text_rva, text_raw,

		linker.entry_stub_size() + obj.text_data.len, 'ReadConsoleW')
	read_file_thunk := pe_test_import_thunk_rva(image, text_rva, text_raw,

		linker.entry_stub_size() + obj.text_data.len, 'ReadFile')
	read_console_field_off := text_raw + linker.entry_stub_size() + 1
	read_console_field_rva := text_rva + u32(linker.entry_stub_size() + 1)
	read_console_disp := pe_test_i32(image, read_console_field_off)
	read_console_target := u32(i32(read_console_field_rva + 4) + read_console_disp)
	assert read_console_target == read_console_thunk
	read_file_field_off := text_raw + linker.entry_stub_size() + 6
	read_file_field_rva := text_rva + u32(linker.entry_stub_size() + 6)
	read_file_disp := pe_test_i32(image, read_file_field_off)
	read_file_target := u32(i32(read_file_field_rva + 4) + read_file_disp)
	assert read_file_target == read_file_thunk
}

fn test_pe_linker_imports_ucrt_log_and_patches_call_to_import_thunk() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 5, true, 1)
	log_sym := obj.add_undefined('log')
	obj.add_text_reloc(1, log_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	names := pe_test_import_names(image)
	dll_names := pe_test_import_dll_names(image)

	assert names == ['ExitProcess', 'log']
	assert dll_names == ['kernel32.dll', 'ucrtbase.dll']
	assert pe_test_iat_size(image) == u32((names.len + dll_names.len) * 8)

	text_off := pe_test_section_header(image, '.text')
	text_rva := pe_test_u32(image, text_off + 12)
	text_raw := int(pe_test_u32(image, text_off + 20))
	field_off := text_raw + linker.entry_stub_size() + 1
	field_rva := text_rva + u32(linker.entry_stub_size() + 1)
	disp := pe_test_i32(image, field_off)
	target := u32(i32(field_rva + 4) + disp)
	log_thunk := pe_test_import_thunk_rva(image, text_rva, text_raw, linker.entry_stub_size() +
		obj.text_data.len, 'log')
	assert target == log_thunk
}

fn test_pe_linker_imports_ucrt_ldexp_and_patches_call_to_import_thunk() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 5, true, 1)
	ldexp_sym := obj.add_undefined('ldexp')
	obj.add_text_reloc(1, ldexp_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	names := pe_test_import_names(image)
	dll_names := pe_test_import_dll_names(image)

	assert names == ['ExitProcess', 'ldexp']
	assert dll_names == ['kernel32.dll', 'ucrtbase.dll']
	assert pe_test_iat_size(image) == u32((names.len + dll_names.len) * 8)

	text_off := pe_test_section_header(image, '.text')
	text_rva := pe_test_u32(image, text_off + 12)
	text_raw := int(pe_test_u32(image, text_off + 20))
	field_off := text_raw + linker.entry_stub_size() + 1
	field_rva := text_rva + u32(linker.entry_stub_size() + 1)
	disp := pe_test_i32(image, field_off)
	target := u32(i32(field_rva + 4) + disp)
	ldexp_thunk := pe_test_import_thunk_rva(image, text_rva, text_raw, linker.entry_stub_size() +
		obj.text_data.len, 'ldexp')
	assert target == ldexp_thunk
}

fn test_pe_linker_imports_ucrt_sqrt_and_patches_call_to_import_thunk() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 5, true, 1)
	sqrt_sym := obj.add_undefined('sqrt')
	obj.add_text_reloc(1, sqrt_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	names := pe_test_import_names(image)
	dll_names := pe_test_import_dll_names(image)

	assert names == ['ExitProcess', 'sqrt']
	assert dll_names == ['kernel32.dll', 'ucrtbase.dll']
	assert pe_test_iat_size(image) == u32((names.len + dll_names.len) * 8)

	text_off := pe_test_section_header(image, '.text')
	text_rva := pe_test_u32(image, text_off + 12)
	text_raw := int(pe_test_u32(image, text_off + 20))
	field_off := text_raw + linker.entry_stub_size() + 1
	field_rva := text_rva + u32(linker.entry_stub_size() + 1)
	disp := pe_test_i32(image, field_off)
	target := u32(i32(field_rva + 4) + disp)
	sqrt_thunk := pe_test_import_thunk_rva(image, text_rva, text_raw, linker.entry_stub_size() +
		obj.text_data.len, 'sqrt')
	assert target == sqrt_thunk
}

fn test_pe_linker_maps_time_and_localtime_to_ucrt64_imports_and_patches_thunks() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xe8, 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 10, true, 1)
	time_sym := obj.add_undefined('time')
	localtime_sym := obj.add_undefined('localtime')
	obj.add_text_reloc(1, time_sym, coff_image_rel_amd64_rel32)
	obj.add_text_reloc(6, localtime_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	names := pe_test_import_names(image)
	dll_names := pe_test_import_dll_names(image)

	assert names == ['ExitProcess', '_time64', '_localtime64']
	assert dll_names == ['kernel32.dll', 'ucrtbase.dll']
	assert 'time' !in names
	assert 'localtime' !in names
	assert pe_test_iat_size(image) == u32((names.len + dll_names.len) * 8)

	text_off := pe_test_section_header(image, '.text')
	text_rva := pe_test_u32(image, text_off + 12)
	text_raw := int(pe_test_u32(image, text_off + 20))
	time_thunk := pe_test_import_thunk_rva(image, text_rva, text_raw, linker.entry_stub_size() +
		obj.text_data.len, '_time64')
	localtime_thunk := pe_test_import_thunk_rva(image, text_rva, text_raw,

		linker.entry_stub_size() + obj.text_data.len, '_localtime64')
	time_field_off := text_raw + linker.entry_stub_size() + 1
	time_field_rva := text_rva + u32(linker.entry_stub_size() + 1)
	time_disp := pe_test_i32(image, time_field_off)
	time_target := u32(i32(time_field_rva + 4) + time_disp)
	assert time_target == time_thunk
	localtime_field_off := text_raw + linker.entry_stub_size() + 6
	localtime_field_rva := text_rva + u32(linker.entry_stub_size() + 6)
	localtime_disp := pe_test_i32(image, localtime_field_off)
	localtime_target := u32(i32(localtime_field_rva + 4) + localtime_disp)
	assert localtime_target == localtime_thunk
}

fn test_pe_linker_imports_msvcrt_scprintf_snprintf_and_patches_call_to_import_thunks() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xe8, 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 10, true, 1)
	scprintf_sym := obj.add_undefined('_scprintf')
	snprintf_sym := obj.add_undefined('_snprintf')
	obj.add_text_reloc(1, scprintf_sym, coff_image_rel_amd64_rel32)
	obj.add_text_reloc(6, snprintf_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	names := pe_test_import_names(image)
	dll_names := pe_test_import_dll_names(image)

	assert names == ['ExitProcess', '_scprintf', '_snprintf']
	assert dll_names == ['kernel32.dll', 'msvcrt.dll']
	assert pe_test_iat_size(image) == u32((names.len + dll_names.len) * 8)

	text_off := pe_test_section_header(image, '.text')
	text_rva := pe_test_u32(image, text_off + 12)
	text_raw := int(pe_test_u32(image, text_off + 20))
	scprintf_thunk := pe_test_import_thunk_rva(image, text_rva, text_raw,

		linker.entry_stub_size() + obj.text_data.len, '_scprintf')
	snprintf_thunk := pe_test_import_thunk_rva(image, text_rva, text_raw,

		linker.entry_stub_size() + obj.text_data.len, '_snprintf')
	scprintf_field_off := text_raw + linker.entry_stub_size() + 1
	scprintf_field_rva := text_rva + u32(linker.entry_stub_size() + 1)
	scprintf_disp := pe_test_i32(image, scprintf_field_off)
	scprintf_target := u32(i32(scprintf_field_rva + 4) + scprintf_disp)
	assert scprintf_target == scprintf_thunk
	snprintf_field_off := text_raw + linker.entry_stub_size() + 6
	snprintf_field_rva := text_rva + u32(linker.entry_stub_size() + 6)
	snprintf_disp := pe_test_i32(image, snprintf_field_off)
	snprintf_target := u32(i32(snprintf_field_rva + 4) + snprintf_disp)
	assert snprintf_target == snprintf_thunk
}

fn test_pe_linker_resolves_atexit_with_internal_runtime_not_crt_import() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 5, true, 1)
	atexit_sym := obj.add_undefined('atexit')
	obj.add_text_reloc(1, atexit_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	names := pe_test_import_names(image)
	dll_names := pe_test_import_dll_names(image)

	assert names == ['ExitProcess']
	assert dll_names == ['kernel32.dll']
	assert 'atexit' !in names
	assert 'ucrtbase.dll' !in dll_names
	assert 'msvcrt.dll' !in dll_names
	assert pe_test_section_header(image, '.data') >= 0

	text_off := pe_test_section_header(image, '.text')
	text_rva := pe_test_u32(image, text_off + 12)
	text_raw := int(pe_test_u32(image, text_off + 20))
	entry_stub_len := linker.entry_stub_size()
	runtime_start_rva := text_rva + u32(entry_stub_len + obj.text_data.len)
	first_import_thunk_rva := pe_test_first_import_thunk_rva(image, text_rva, text_raw,

		entry_stub_len + obj.text_data.len)
	field_off := entry_stub_len + 1
	target := pe_test_text_rel32_target(image, text_rva, text_raw, field_off)

	assert target >= runtime_start_rva
	assert target < first_import_thunk_rva
}

fn test_pe_linker_adds_runtime_kernel32_import_patches_only_when_needed() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 5, true, 1)
	calloc_sym := obj.add_undefined('calloc')
	obj.add_text_reloc(1, calloc_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	names := pe_test_import_names(image)

	assert names == ['ExitProcess', 'GetProcessHeap', 'HeapAlloc']
	assert pe_test_iat_size(image) == u32((names.len + 1) * 8)
	assert 'HeapFree' !in names
	assert 'WriteFile' !in names
}

fn test_pe_linker_adds_malloc_heap_imports_without_crt() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 5, true, 1)
	malloc_sym := obj.add_undefined('malloc')
	obj.add_text_reloc(1, malloc_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	names := pe_test_import_names(image)
	dll_names := pe_test_import_dll_names(image)

	assert names == ['ExitProcess', 'GetProcessHeap', 'HeapAlloc']
	assert dll_names == ['kernel32.dll']
	assert pe_test_iat_size(image) == u32((names.len + 1) * 8)
	assert 'HeapFree' !in names
	assert 'HeapReAlloc' !in names
	assert 'ucrtbase.dll' !in dll_names
	assert 'msvcrt.dll' !in dll_names
}

fn test_pe_linker_adds_shell32_imports_only_for_windows_arguments_globals() {
	obj := sample_pe_arguments_coff_object(false)
	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	names := pe_test_import_names(image)

	assert names == ['ExitProcess', 'GetCommandLineW', 'CommandLineToArgvW']
	assert pe_test_import_dll_names(image) == ['kernel32.dll', 'shell32.dll']
	assert pe_test_iat_size(image) == u32((names.len + 2) * 8)
}

fn test_pe_linker_uses_qualified_import_keys_for_multi_dll_thunks_and_iat() {
	obj := sample_pe_coff_object()
	mut linker := PeLinker.new(obj)
	linker.imports = [
		PeImport{
			dll:  pe_kernel32_dll
			name: 'SharedExport'
		},
		PeImport{
			dll:  pe_shell32_dll
			name: 'SharedExport'
		},
	]

	kernel_key := pe_import_key(pe_kernel32_dll, 'SharedExport')
	shell_key := pe_import_key(pe_shell32_dll, 'SharedExport')
	import_offsets := linker.import_thunk_offsets(0)
	idata := linker.build_idata(0x3000)

	assert 'SharedExport' !in import_offsets
	assert kernel_key in import_offsets
	assert shell_key in import_offsets
	assert import_offsets[kernel_key] != import_offsets[shell_key]
	assert 'SharedExport' !in idata.iat_rvas
	assert kernel_key in idata.iat_rvas
	assert shell_key in idata.iat_rvas
	assert idata.iat_rvas[kernel_key] != idata.iat_rvas[shell_key]
}

fn test_pe_linker_resolves_windows_string_plus_runtime_with_heap_imports() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 5, true, 1)
	string_plus_sym := obj.add_undefined('builtin__string__+')
	obj.add_text_reloc(1, string_plus_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	names := pe_test_import_names(image)

	assert names == ['ExitProcess', 'GetProcessHeap', 'HeapAlloc']
	assert pe_test_iat_size(image) == u32((names.len + 1) * 8)
	assert 'WriteFile' !in names
}

fn test_pe_linker_resolves_windows_i64_str_runtime_with_heap_imports() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 5, true, 1)
	i64_str_sym := obj.add_undefined('builtin__i64__str')
	obj.add_text_reloc(1, i64_str_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	names := pe_test_import_names(image)

	assert names == ['ExitProcess', 'GetProcessHeap', 'HeapAlloc']
	assert pe_test_iat_size(image) == u32((names.len + 1) * 8)
	assert 'WriteFile' !in names
}

fn test_pe_linker_zeroes_unemitted_data_directories() {
	obj := sample_pe_coff_object()
	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }

	pe_off := int(pe_test_u32(image, 0x3c))
	opt_off := pe_off + 4 + 20
	for i in 0 .. pe_number_of_rva_and_sizes {
		if i in [pe_import_directory_index, pe_iat_directory_index] {
			continue
		}
		assert pe_test_u32(image, opt_off + 112 + i * 8) == 0
		assert pe_test_u32(image, opt_off + 116 + i * 8) == 0
	}
	assert pe_test_u32(image, opt_off + 112 + pe_exception_directory_index * 8) == 0
	assert pe_test_u32(image, opt_off + 116 + pe_exception_directory_index * 8) == 0
	assert pe_test_u32(image, opt_off + 112 + pe_base_reloc_directory_index * 8) == 0
	assert pe_test_u32(image, opt_off + 116 + pe_base_reloc_directory_index * 8) == 0
}

fn test_pe_linker_marks_fixed_base_image_without_unwind_or_reloc_directories() {
	obj := sample_pe_coff_object()
	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }

	pe_off := int(pe_test_u32(image, 0x3c))
	characteristics := pe_test_u16(image, pe_off + 22)
	assert characteristics & pe_image_file_relocs_stripped != 0

	opt_off := pe_off + 4 + 20
	assert pe_test_u32(image, opt_off + 112 + pe_base_reloc_directory_index * 8) == 0
	assert pe_test_u32(image, opt_off + 116 + pe_base_reloc_directory_index * 8) == 0
	assert pe_test_u32(image, opt_off + 112 + pe_exception_directory_index * 8) == 0
	assert pe_test_u32(image, opt_off + 116 + pe_exception_directory_index * 8) == 0
}

fn test_pe_linker_omits_empty_rdata_and_data_sections() {
	mut obj := CoffObject.new()
	obj.text_data << u8(0xc3)
	obj.add_symbol('main', 0, true, 1)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	pe_off := int(pe_test_u32(image, 0x3c))
	opt_off := pe_off + 4 + 20
	text_off := pe_test_section_header(image, '.text')
	idata_off := pe_test_section_header(image, '.idata')

	assert pe_test_u16(image, pe_off + 6) == 2
	assert text_off > 0
	assert pe_test_section_header(image, '.rdata') == -1
	assert pe_test_section_header(image, '.data') == -1
	assert idata_off > text_off

	assert pe_test_u32(image, text_off + 12) == pe_section_alignment
	assert pe_test_u32(image, idata_off + 12) == pe_section_alignment * 2
	assert pe_test_u32(image, idata_off + 20) == pe_headers_size(2) + pe_file_alignment
	assert pe_test_u32(image, opt_off + 56) == pe_section_alignment * 3
	pe_test_assert_ranges_are_adjacent(pe_test_section_virtual_ranges(image))
	pe_test_assert_no_zero_sized_sections(image)
	pe_test_assert_size_of_image_is_exact(image)
	pe_test_assert_entrypoint_in_executable_text(image)
	pe_test_assert_directory_contained_in_section(image, pe_import_directory_index, '.idata')
	pe_test_assert_directory_contained_in_section(image, pe_iat_directory_index, '.idata')
}

fn test_pe_linker_entry_stub_targets_main_and_runtime_exit() {
	obj := sample_pe_coff_object()
	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }

	text_off := pe_test_section_header(image, '.text')
	text_rva := pe_test_u32(image, text_off + 12)
	text_raw := int(pe_test_u32(image, text_off + 20))
	entry_stub_len := linker.entry_stub_size()

	assert image[text_raw..text_raw + 4] == [u8(0x48), 0x83, 0xec, 0x28]
	assert image[text_raw + entry_stub_len - 1] == 0xcc

	runtime_start_rva := text_rva + u32(entry_stub_len + obj.text_data.len)
	first_import_thunk_rva := pe_test_first_import_thunk_rva(image, text_rva, text_raw,

		entry_stub_len + obj.text_data.len)
	assert image[text_raw + 4] == 0xe8
	assert pe_test_text_rel32_target(image, text_rva, text_raw, 5) == text_rva + u32(entry_stub_len)
	assert image[text_raw + 9] == 0xe8
	assert pe_test_text_rel32_target(image, text_rva, text_raw, 10) == text_rva +
		u32(entry_stub_len + 1)
	assert image[text_raw + 14..text_raw + 16] == [u8(0x31), 0xc9]
	assert image[text_raw + 16] == 0xe8
	exit_target := pe_test_text_rel32_target(image, text_rva, text_raw, 17)
	assert exit_target >= runtime_start_rva
	assert exit_target < first_import_thunk_rva
}

fn test_pe_linker_entry_stub_uses_common_runtime_exit_when_atexit_is_present() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 5, true, 1)
	atexit_sym := obj.add_undefined('atexit')
	obj.add_text_reloc(1, atexit_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	text_off := pe_test_section_header(image, '.text')
	text_rva := pe_test_u32(image, text_off + 12)
	text_raw := int(pe_test_u32(image, text_off + 20))
	entry_stub_len := linker.entry_stub_size()
	first_import_thunk_rva := pe_test_first_import_thunk_rva(image, text_rva, text_raw,

		entry_stub_len + obj.text_data.len)
	runtime_start_rva := text_rva + u32(entry_stub_len + obj.text_data.len)

	assert image[text_raw..text_raw + 4] == [u8(0x48), 0x83, 0xec, 0x28]
	assert image[text_raw + 4] == 0xe8
	assert pe_test_text_rel32_target(image, text_rva, text_raw, 5) == text_rva +
		u32(entry_stub_len + 5)
	assert image[text_raw + 9..text_raw + 11] == [u8(0x31), 0xc9]
	assert image[text_raw + 11] == 0xe8
	exit_target := pe_test_text_rel32_target(image, text_rva, text_raw, 12)
	assert exit_target >= runtime_start_rva
	assert exit_target < first_import_thunk_rva
	assert image[text_raw + entry_stub_len - 1] == 0xcc
}

fn test_pe_linker_bootstraps_windows_arguments_before_vinit() {
	obj := sample_pe_arguments_coff_object(true)
	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }

	text_off := pe_test_section_header(image, '.text')
	text_rva := pe_test_u32(image, text_off + 12)
	text_raw := int(pe_test_u32(image, text_off + 20))
	data_off := pe_test_section_header(image, '.data')
	data_rva := pe_test_u32(image, data_off + 12)
	entry_stub_len := linker.entry_stub_size()
	bootstrap_len := linker.windows_argv_bootstrap_size()

	assert bootstrap_len > 0
	assert image[text_raw..text_raw + 4] == [u8(0x48), 0x83, 0xec, 0x28]
	assert image[text_raw + 4..text_raw + 12] == [u8(0xc7), 0x44, 0x24, 0x20, 0, 0, 0, 0]
	assert image[text_raw + 12] == 0xe8
	assert image[text_raw + 17..text_raw + 20] == [u8(0x48), 0x89, 0xc1]
	assert image[text_raw + 20..text_raw + 25] == [u8(0x48), 0x8d, 0x54, 0x24, 0x20]
	assert image[text_raw + 25] == 0xe8
	assert image[text_raw + 30..text_raw + 33] == [u8(0x48), 0x85, 0xc0]
	assert image[text_raw + 33..text_raw + 35] == [u8(0x75), 0x0a]
	assert image[text_raw + 35..text_raw + 40] == [u8(0xb9), 1, 0, 0, 0]
	assert image[text_raw + 40] == 0xe8
	assert image[text_raw + 45..text_raw + 48] == [u8(0x4c), 0x8d, 0x15]
	assert image[text_raw + 52..text_raw + 55] == [u8(0x49), 0x89, 0x02]
	assert image[text_raw + 55..text_raw + 58] == [u8(0x4c), 0x8d, 0x15]
	assert image[text_raw + 62..text_raw + 66] == [u8(0x8b), 0x4c, 0x24, 0x20]
	assert image[text_raw + 66..text_raw + 69] == [u8(0x41), 0x89, 0x0a]

	get_command_line_thunk := pe_test_import_thunk_rva(image, text_rva, text_raw, entry_stub_len +
		obj.text_data.len, 'GetCommandLineW')
	command_line_to_argv_thunk := pe_test_import_thunk_rva(image, text_rva, text_raw,

		entry_stub_len + obj.text_data.len, 'CommandLineToArgvW')
	runtime_start_rva := text_rva + u32(entry_stub_len + obj.text_data.len)
	first_import_thunk_rva := pe_test_first_import_thunk_rva(image, text_rva, text_raw,

		entry_stub_len + obj.text_data.len)
	assert pe_test_text_rel32_target(image, text_rva, text_raw, 13) == get_command_line_thunk
	assert pe_test_text_rel32_target(image, text_rva, text_raw, 26) == command_line_to_argv_thunk
	argv_failure_exit_target := pe_test_text_rel32_target(image, text_rva, text_raw, 41)
	assert argv_failure_exit_target >= runtime_start_rva
	assert argv_failure_exit_target < first_import_thunk_rva
	assert pe_test_text_rel32_target(image, text_rva, text_raw, 48) == data_rva + 8
	assert pe_test_text_rel32_target(image, text_rva, text_raw, 58) == data_rva
	vinit_call_field := 4 + bootstrap_len + 1
	main_call_field := vinit_call_field + 5
	assert pe_test_text_rel32_target(image, text_rva, text_raw, vinit_call_field) == text_rva +
		u32(entry_stub_len)
	assert pe_test_text_rel32_target(image, text_rva, text_raw, main_call_field) == text_rva +
		u32(entry_stub_len + 1)
}

fn test_pe_linker_applies_internal_rel32_relocations() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 0, true, 1)
	helper_sym := obj.add_symbol('helper', 5, true, 1)
	obj.add_text_reloc(1, helper_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	text_off := pe_test_section_header(image, '.text')
	text_raw := int(pe_test_u32(image, text_off + 20))
	field_off := text_raw + linker.entry_stub_size() + 1
	assert pe_test_i32(image, field_off) == 0
}

fn test_pe_linker_applies_import_rel32_relocations_to_thunks() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 0, true, 1)
	exit_sym := obj.add_undefined('ExitProcess')
	obj.add_text_reloc(1, exit_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	text_off := pe_test_section_header(image, '.text')
	text_rva := pe_test_u32(image, text_off + 12)
	text_raw := int(pe_test_u32(image, text_off + 20))
	field_off := text_raw + linker.entry_stub_size() + 1
	field_rva := text_rva + u32(linker.entry_stub_size() + 1)
	disp := pe_test_i32(image, field_off)
	target := u32(i32(field_rva + 4) + disp)
	assert target == pe_test_import_thunk_rva(image, text_rva, text_raw, linker.entry_stub_size() +
		obj.text_data.len, 'ExitProcess')
}

fn test_pe_linker_resolves_get_current_thread_id_as_kernel32_import() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 0, true, 1)
	get_tid_sym := obj.add_undefined('GetCurrentThreadId')
	obj.add_text_reloc(1, get_tid_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	text_off := pe_test_section_header(image, '.text')
	text_rva := pe_test_u32(image, text_off + 12)
	text_raw := int(pe_test_u32(image, text_off + 20))
	field_off := text_raw + linker.entry_stub_size() + 1
	field_rva := text_rva + u32(linker.entry_stub_size() + 1)
	disp := pe_test_i32(image, field_off)
	target := u32(i32(field_rva + 4) + disp)
	get_tid_thunk := pe_test_import_thunk_rva(image, text_rva, text_raw, linker.entry_stub_size() +
		obj.text_data.len, 'GetCurrentThreadId')

	assert target == get_tid_thunk
}

fn test_pe_linker_resolves_windows_system_time_imports() {
	for name in ['GetSystemTimeAsFileTime', 'FileTimeToSystemTime', 'SystemTimeToTzSpecificLocalTime',
		'QueryPerformanceFrequency', 'QueryPerformanceCounter'] {
		mut obj := CoffObject.new()
		obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
		obj.add_symbol('main', 0, true, 1)
		sym := obj.add_undefined(name)
		obj.add_text_reloc(1, sym, coff_image_rel_amd64_rel32)

		mut linker := PeLinker.new(obj)
		image := linker.image() or { panic(err) }
		text_off := pe_test_section_header(image, '.text')
		text_rva := pe_test_u32(image, text_off + 12)
		text_raw := int(pe_test_u32(image, text_off + 20))
		field_off := text_raw + linker.entry_stub_size() + 1
		field_rva := text_rva + u32(linker.entry_stub_size() + 1)
		disp := pe_test_i32(image, field_off)
		target := u32(i32(field_rva + 4) + disp)
		thunk := pe_test_import_thunk_rva(image, text_rva, text_raw, linker.entry_stub_size() +
			obj.text_data.len, name)

		assert target == thunk
	}
}

fn test_pe_linker_resolves_memcmp_and_exit_with_internal_runtime_thunks() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xe8, 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 10, true, 1)
	memcmp_sym := obj.add_undefined('memcmp')
	exit_sym := obj.add_undefined('exit')
	obj.add_text_reloc(1, memcmp_sym, coff_image_rel_amd64_rel32)
	obj.add_text_reloc(6, exit_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	text_off := pe_test_section_header(image, '.text')
	text_rva := pe_test_u32(image, text_off + 12)
	text_raw := int(pe_test_u32(image, text_off + 20))
	text_raw_size := int(pe_test_u32(image, text_off + 16))
	entry_stub_len := linker.entry_stub_size()
	runtime_start_rva := text_rva + u32(entry_stub_len + obj.text_data.len)

	mut first_import_thunk_file_off := -1
	for off in text_raw + entry_stub_len + obj.text_data.len .. text_raw + text_raw_size - 1 {
		if image[off] == 0xff && image[off + 1] == 0x25 {
			first_import_thunk_file_off = off
			break
		}
	}
	assert first_import_thunk_file_off > 0
	first_import_thunk_rva := text_rva + u32(first_import_thunk_file_off - text_raw)

	memcmp_field_off := text_raw + entry_stub_len + 1
	memcmp_field_rva := text_rva + u32(entry_stub_len + 1)
	memcmp_target := u32(i32(memcmp_field_rva + 4) + pe_test_i32(image, memcmp_field_off))
	exit_field_off := text_raw + entry_stub_len + 6
	exit_field_rva := text_rva + u32(entry_stub_len + 6)
	exit_target := u32(i32(exit_field_rva + 4) + pe_test_i32(image, exit_field_off))

	assert memcmp_target >= runtime_start_rva
	assert memcmp_target < first_import_thunk_rva
	assert exit_target >= runtime_start_rva
	assert exit_target < first_import_thunk_rva
	assert exit_target != memcmp_target

	memcmp_off := pe_test_rva_to_file_off(image, memcmp_target)
	assert memcmp_off > 0
	assert image[memcmp_off..memcmp_off + 3] == [u8(0x4d), 0x85, 0xc0] // test r8, r8
	assert image[memcmp_off + 22..memcmp_off + 25] == [u8(0x49), 0xff, 0xc8] // dec r8
}

fn test_pe_linker_resolves_errno_with_internal_runtime_data_slot() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 5, true, 1)
	errno_sym := obj.add_undefined('_errno')
	obj.add_text_reloc(1, errno_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	text_off := pe_test_section_header(image, '.text')
	text_rva := pe_test_u32(image, text_off + 12)
	text_raw := int(pe_test_u32(image, text_off + 20))
	text_raw_size := int(pe_test_u32(image, text_off + 16))
	entry_stub_len := linker.entry_stub_size()
	runtime_start_rva := text_rva + u32(entry_stub_len + obj.text_data.len)

	mut first_import_thunk_file_off := -1
	for off in text_raw + entry_stub_len + obj.text_data.len .. text_raw + text_raw_size - 1 {
		if image[off] == 0xff && image[off + 1] == 0x25 {
			first_import_thunk_file_off = off
			break
		}
	}
	assert first_import_thunk_file_off > 0
	first_import_thunk_rva := text_rva + u32(first_import_thunk_file_off - text_raw)

	errno_field_off := text_raw + entry_stub_len + 1
	errno_field_rva := text_rva + u32(entry_stub_len + 1)
	errno_target := u32(i32(errno_field_rva + 4) + pe_test_i32(image, errno_field_off))
	assert errno_target >= runtime_start_rva
	assert errno_target < first_import_thunk_rva

	errno_off := pe_test_rva_to_file_off(image, errno_target)
	assert errno_off > 0
	assert image[errno_off..errno_off + 3] == [u8(0x48), 0x8d, 0x05] // lea rax, [rip + disp32]
	assert image[errno_off + 7] == u8(0xc3) // ret
	slot_target := u32(i32(errno_target + 7) + pe_test_i32(image, errno_off + 3))
	data_off := pe_test_section_header(image, '.data')
	assert data_off > 0
	data_rva := pe_test_u32(image, data_off + 12)
	data_raw_size := pe_test_u32(image, data_off + 16)
	assert slot_target >= data_rva
	assert slot_target < data_rva + data_raw_size
	slot_off := pe_test_rva_to_file_off(image, slot_target)
	assert pe_test_u32(image, slot_off) == 0
}

fn test_pe_linker_resolves_malloc_with_internal_nonzeroing_heap_thunk() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 5, true, 1)
	malloc_sym := obj.add_undefined('malloc')
	obj.add_text_reloc(1, malloc_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	names := pe_test_import_names(image)
	dll_names := pe_test_import_dll_names(image)
	assert names == ['ExitProcess', 'GetProcessHeap', 'HeapAlloc']
	assert dll_names == ['kernel32.dll']

	text_off := pe_test_section_header(image, '.text')
	text_rva := pe_test_u32(image, text_off + 12)
	text_raw := int(pe_test_u32(image, text_off + 20))
	text_raw_size := int(pe_test_u32(image, text_off + 16))
	entry_stub_len := linker.entry_stub_size()
	runtime_start_rva := text_rva + u32(entry_stub_len + obj.text_data.len)

	mut first_import_thunk_file_off := -1
	for off in text_raw + entry_stub_len + obj.text_data.len .. text_raw + text_raw_size - 1 {
		if image[off] == 0xff && image[off + 1] == 0x25 {
			first_import_thunk_file_off = off
			break
		}
	}
	assert first_import_thunk_file_off > 0
	first_import_thunk_rva := text_rva + u32(first_import_thunk_file_off - text_raw)

	malloc_target := pe_test_text_rel32_target(image, text_rva, text_raw, entry_stub_len + 1)
	assert malloc_target >= runtime_start_rva
	assert malloc_target < first_import_thunk_rva

	malloc_off := pe_test_rva_to_file_off(image, malloc_target)
	assert malloc_off > 0
	mut has_shadow_space_frame := false
	mut has_size_saved_from_rcx := false
	mut has_heap_zero_memory_flag := false
	mut has_plain_heap_flags := false
	mut has_rdx_alignment_dependency := false
	mut has_aligned_allocation_padding := false
	mut has_aligned_cookie_store := false
	mut has_aligned_return_pointer := false
	runtime_scan_end := first_import_thunk_file_off
	for off in malloc_off .. runtime_scan_end - 4 {
		if image[off..off + 4] == [u8(0x48), 0x83, 0xec, 0x28] {
			has_shadow_space_frame = true
		}
		if image[off..off + 5] == [u8(0x48), 0x89, 0x4c, 0x24, 0x20] {
			has_size_saved_from_rcx = true
		}
		if image[off..off + 5] == [u8(0xba), 0x08, 0, 0, 0] {
			has_heap_zero_memory_flag = true
		}
		if image[off..off + 2] == [u8(0x31), 0xd2] {
			has_plain_heap_flags = true
		}
		if image[off..off + 4] == [u8(0x48), 0x83, 0xfa, 0x10] {
			has_rdx_alignment_dependency = true
		}
		if image[off..off + 4] == [u8(0x49), 0x83, 0xc0, 0x18] {
			has_aligned_allocation_padding = true
		}
		if image[off..off + 3] == [u8(0x4c), 0x89, 0xd8] {
			has_aligned_return_pointer = true
		}
	}
	for off in malloc_off .. runtime_scan_end - 14 {
		if image[off..off + 15] == [
			u8(0x49),
			0x89,
			0xc3,
			0x49,
			0x83,
			0xc3,
			0x17,
			0x49,
			0x83,
			0xe3,
			0xf0,
			0x49,
			0x89,
			0x43,
			0xf8,
		] {
			has_aligned_cookie_store = true
		}
	}
	assert has_shadow_space_frame
	assert has_size_saved_from_rcx
	assert !has_heap_zero_memory_flag
	assert has_plain_heap_flags
	assert !has_rdx_alignment_dependency
	assert has_aligned_allocation_padding
	assert has_aligned_cookie_store
	assert has_aligned_return_pointer
}

fn test_pe_linker_resolves_malloc_and_free_with_compatible_heap_cookie() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xe8, 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 10, true, 1)
	malloc_sym := obj.add_undefined('malloc')
	free_sym := obj.add_undefined('free')
	obj.add_text_reloc(1, malloc_sym, coff_image_rel_amd64_rel32)
	obj.add_text_reloc(6, free_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	names := pe_test_import_names(image)
	dll_names := pe_test_import_dll_names(image)
	assert names == ['ExitProcess', 'GetProcessHeap', 'HeapAlloc', 'HeapFree']
	assert dll_names == ['kernel32.dll']
	assert 'HeapReAlloc' !in names
	assert 'ucrtbase.dll' !in dll_names
	assert 'msvcrt.dll' !in dll_names

	text_off := pe_test_section_header(image, '.text')
	text_rva := pe_test_u32(image, text_off + 12)
	text_raw := int(pe_test_u32(image, text_off + 20))
	text_raw_size := int(pe_test_u32(image, text_off + 16))
	entry_stub_len := linker.entry_stub_size()
	runtime_start_rva := text_rva + u32(entry_stub_len + obj.text_data.len)

	mut first_import_thunk_file_off := -1
	for off in text_raw + entry_stub_len + obj.text_data.len .. text_raw + text_raw_size - 1 {
		if image[off] == 0xff && image[off + 1] == 0x25 {
			first_import_thunk_file_off = off
			break
		}
	}
	assert first_import_thunk_file_off > 0
	first_import_thunk_rva := text_rva + u32(first_import_thunk_file_off - text_raw)

	malloc_target := pe_test_text_rel32_target(image, text_rva, text_raw, entry_stub_len + 1)
	free_target := pe_test_text_rel32_target(image, text_rva, text_raw, entry_stub_len + 6)
	assert malloc_target >= runtime_start_rva
	assert malloc_target < first_import_thunk_rva
	assert free_target >= runtime_start_rva
	assert free_target < first_import_thunk_rva

	malloc_off := pe_test_rva_to_file_off(image, malloc_target)
	free_off := pe_test_rva_to_file_off(image, free_target)
	assert malloc_off > 0
	assert free_off > 0
	mut malloc_stores_raw_cookie := false
	for off in malloc_off .. first_import_thunk_file_off - 14 {
		if image[off..off + 15] == [
			u8(0x49),
			0x89,
			0xc3,
			0x49,
			0x83,
			0xc3,
			0x17,
			0x49,
			0x83,
			0xe3,
			0xf0,
			0x49,
			0x89,
			0x43,
			0xf8,
		] {
			malloc_stores_raw_cookie = true
		}
	}
	assert malloc_stores_raw_cookie
	mut free_reads_raw_cookie := false
	for off in free_off .. first_import_thunk_file_off - 3 {
		if image[off..off + 4] == [u8(0x48), 0x8b, 0x41, 0xf8] {
			free_reads_raw_cookie = true
		}
	}
	assert free_reads_raw_cookie
}

fn test_pe_linker_resolves_calloc_with_internal_zeroed_heap_thunk() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 5, true, 1)
	calloc_sym := obj.add_undefined('calloc')
	obj.add_text_reloc(1, calloc_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	text_off := pe_test_section_header(image, '.text')
	text_rva := pe_test_u32(image, text_off + 12)
	text_raw := int(pe_test_u32(image, text_off + 20))
	text_raw_size := int(pe_test_u32(image, text_off + 16))
	entry_stub_len := linker.entry_stub_size()
	runtime_start_rva := text_rva + u32(entry_stub_len + obj.text_data.len)

	mut first_import_thunk_file_off := -1
	for off in text_raw + entry_stub_len + obj.text_data.len .. text_raw + text_raw_size - 1 {
		if image[off] == 0xff && image[off + 1] == 0x25 {
			first_import_thunk_file_off = off
			break
		}
	}
	assert first_import_thunk_file_off > 0
	first_import_thunk_rva := text_rva + u32(first_import_thunk_file_off - text_raw)

	calloc_field_off := text_raw + entry_stub_len + 1
	calloc_field_rva := text_rva + u32(entry_stub_len + 1)
	calloc_target := u32(i32(calloc_field_rva + 4) + pe_test_i32(image, calloc_field_off))
	assert calloc_target >= runtime_start_rva
	assert calloc_target < first_import_thunk_rva

	calloc_off := pe_test_rva_to_file_off(image, calloc_target)
	assert calloc_off > 0
	assert image[calloc_off..calloc_off + 3] == [u8(0x48), 0x89, 0xc8] // mov rax, rcx
	assert image[calloc_off + 3..calloc_off + 6] == [u8(0x48), 0xf7, 0xe2] // mul rdx
	mut has_heap_zero_memory_flag := false
	mut has_aligned_allocation_padding := false
	mut has_aligned_cookie_store := false
	mut has_aligned_return_pointer := false
	runtime_scan_end := first_import_thunk_file_off - 16
	for off in calloc_off .. runtime_scan_end {
		if image[off..off + 5] == [u8(0xba), 0x08, 0, 0, 0] {
			has_heap_zero_memory_flag = true
		}
		if image[off..off + 4] == [u8(0x49), 0x83, 0xc0, 0x18] {
			has_aligned_allocation_padding = true
		}
		if image[off..off + 15] == [
			u8(0x49),
			0x89,
			0xc3,
			0x49,
			0x83,
			0xc3,
			0x17,
			0x49,
			0x83,
			0xe3,
			0xf0,
			0x49,
			0x89,
			0x43,
			0xf8,
		] {
			has_aligned_cookie_store = true
		}
		if image[off..off + 3] == [u8(0x4c), 0x89, 0xd8] {
			has_aligned_return_pointer = true
		}
	}
	assert has_heap_zero_memory_flag
	assert has_aligned_allocation_padding
	assert has_aligned_cookie_store
	assert has_aligned_return_pointer
}

fn test_pe_linker_resolves_aligned_malloc_checks_size_overflow_before_heap_alloc() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 5, true, 1)
	malloc_sym := obj.add_undefined('_aligned_malloc')
	obj.add_text_reloc(1, malloc_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	text_off := pe_test_section_header(image, '.text')
	text_rva := pe_test_u32(image, text_off + 12)
	text_raw := int(pe_test_u32(image, text_off + 20))
	text_raw_size := int(pe_test_u32(image, text_off + 16))
	entry_stub_len := linker.entry_stub_size()
	runtime_start_rva := text_rva + u32(entry_stub_len + obj.text_data.len)

	mut first_import_thunk_file_off := -1
	for off in text_raw + entry_stub_len + obj.text_data.len .. text_raw + text_raw_size - 1 {
		if image[off] == 0xff && image[off + 1] == 0x25 {
			first_import_thunk_file_off = off
			break
		}
	}
	assert first_import_thunk_file_off > 0
	first_import_thunk_rva := text_rva + u32(first_import_thunk_file_off - text_raw)

	malloc_field_off := text_raw + entry_stub_len + 1
	malloc_field_rva := text_rva + u32(entry_stub_len + 1)
	malloc_target := u32(i32(malloc_field_rva + 4) + pe_test_i32(image, malloc_field_off))
	assert malloc_target >= runtime_start_rva
	assert malloc_target < first_import_thunk_rva

	malloc_off := pe_test_rva_to_file_off(image, malloc_target)
	assert malloc_off > 0
	mut size_overflow_check_off := -1
	mut padding_overflow_check_off := -1
	mut heap_alloc_call_off := -1
	mut alloc_failed_check_off := -1
	runtime_scan_end := first_import_thunk_file_off - 16
	for off in malloc_off .. runtime_scan_end {
		if image[off..off + 4] == [u8(0x4d), 0x01, 0xc8, 0x72] {
			size_overflow_check_off = off + 3
		}
		if image[off..off + 5] == [u8(0x49), 0x83, 0xc0, 0x08, 0x72] {
			padding_overflow_check_off = off + 4
			heap_alloc_call_off = off + 6
		}
		if image[off..off + 4] == [u8(0x48), 0x85, 0xc0, 0x74] {
			alloc_failed_check_off = off + 3
		}
	}
	assert size_overflow_check_off >= 0
	assert padding_overflow_check_off >= 0
	assert heap_alloc_call_off >= 0
	assert alloc_failed_check_off >= 0
	assert image[heap_alloc_call_off] == 0xe8
	assert size_overflow_check_off < padding_overflow_check_off
	assert padding_overflow_check_off < heap_alloc_call_off
	assert heap_alloc_call_off < alloc_failed_check_off
	size_overflow_target := pe_test_rel8_target(size_overflow_check_off, image[
		size_overflow_check_off + 1])
	padding_overflow_target := pe_test_rel8_target(padding_overflow_check_off, image[
		padding_overflow_check_off + 1])
	alloc_failed_target := pe_test_rel8_target(alloc_failed_check_off, image[
		alloc_failed_check_off + 1])
	assert size_overflow_target == padding_overflow_target
	assert size_overflow_target == alloc_failed_target
	assert alloc_failed_check_off < alloc_failed_target
	assert alloc_failed_target < first_import_thunk_file_off
}

fn test_pe_linker_resolves_strlen_with_internal_runtime_thunk() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 5, true, 1)
	strlen_sym := obj.add_undefined('strlen')
	obj.add_text_reloc(1, strlen_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	text_off := pe_test_section_header(image, '.text')
	text_rva := pe_test_u32(image, text_off + 12)
	text_raw := int(pe_test_u32(image, text_off + 20))
	text_raw_size := int(pe_test_u32(image, text_off + 16))
	entry_stub_len := linker.entry_stub_size()
	runtime_start_rva := text_rva + u32(entry_stub_len + obj.text_data.len)

	mut first_import_thunk_file_off := -1
	for off in text_raw + entry_stub_len + obj.text_data.len .. text_raw + text_raw_size - 1 {
		if image[off] == 0xff && image[off + 1] == 0x25 {
			first_import_thunk_file_off = off
			break
		}
	}
	assert first_import_thunk_file_off > 0
	first_import_thunk_rva := text_rva + u32(first_import_thunk_file_off - text_raw)

	strlen_field_off := text_raw + entry_stub_len + 1
	strlen_field_rva := text_rva + u32(entry_stub_len + 1)
	strlen_target := u32(i32(strlen_field_rva + 4) + pe_test_i32(image, strlen_field_off))
	assert strlen_target >= runtime_start_rva
	assert strlen_target < first_import_thunk_rva

	strlen_off := pe_test_rva_to_file_off(image, strlen_target)
	assert strlen_off > 0
	assert image[strlen_off..strlen_off + 3] == [u8(0x48), 0x89, 0xc8] // mov rax, rcx
	assert image[strlen_off + 3..strlen_off + 6] == [u8(0x80), 0x38, 0x00] // cmp byte ptr [rax], 0
}

fn test_pe_linker_resolves_wcslen_with_internal_runtime_thunk() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 5, true, 1)
	wcslen_sym := obj.add_undefined('wcslen')
	obj.add_text_reloc(1, wcslen_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	text_off := pe_test_section_header(image, '.text')
	text_rva := pe_test_u32(image, text_off + 12)
	text_raw := int(pe_test_u32(image, text_off + 20))
	text_raw_size := int(pe_test_u32(image, text_off + 16))
	entry_stub_len := linker.entry_stub_size()
	runtime_start_rva := text_rva + u32(entry_stub_len + obj.text_data.len)

	mut first_import_thunk_file_off := -1
	for off in text_raw + entry_stub_len + obj.text_data.len .. text_raw + text_raw_size - 1 {
		if image[off] == 0xff && image[off + 1] == 0x25 {
			first_import_thunk_file_off = off
			break
		}
	}
	assert first_import_thunk_file_off > 0
	first_import_thunk_rva := text_rva + u32(first_import_thunk_file_off - text_raw)

	wcslen_field_off := text_raw + entry_stub_len + 1
	wcslen_field_rva := text_rva + u32(entry_stub_len + 1)
	wcslen_target := u32(i32(wcslen_field_rva + 4) + pe_test_i32(image, wcslen_field_off))
	assert wcslen_target >= runtime_start_rva
	assert wcslen_target < first_import_thunk_rva

	wcslen_off := pe_test_rva_to_file_off(image, wcslen_target)
	assert wcslen_off > 0
	assert image[wcslen_off..wcslen_off + 3] == [u8(0x48), 0x89, 0xc8] // mov rax, rcx
	assert image[wcslen_off + 3..wcslen_off + 7] == [u8(0x66), 0x83, 0x38, 0x00] // cmp word ptr [rax], 0
	assert image[wcslen_off + 15..wcslen_off + 18] == [u8(0x48), 0x29, 0xc8] // sub rax, rcx
	assert image[wcslen_off + 18..wcslen_off + 21] == [u8(0x48), 0xd1, 0xe8] // shr rax, 1
}

fn test_pe_linker_resolves_wgetcwd_with_kernel32_runtime_thunk() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 5, true, 1)
	wgetcwd_sym := obj.add_undefined('_wgetcwd')
	obj.add_text_reloc(1, wgetcwd_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	text_off := pe_test_section_header(image, '.text')
	text_rva := pe_test_u32(image, text_off + 12)
	text_raw := int(pe_test_u32(image, text_off + 20))
	text_raw_size := int(pe_test_u32(image, text_off + 16))
	entry_stub_len := linker.entry_stub_size()
	runtime_start_rva := text_rva + u32(entry_stub_len + obj.text_data.len)

	mut first_import_thunk_file_off := -1
	for off in text_raw + entry_stub_len + obj.text_data.len .. text_raw + text_raw_size - 1 {
		if image[off] == 0xff && image[off + 1] == 0x25 {
			first_import_thunk_file_off = off
			break
		}
	}
	assert first_import_thunk_file_off > 0
	first_import_thunk_rva := text_rva + u32(first_import_thunk_file_off - text_raw)

	wgetcwd_field_off := text_raw + entry_stub_len + 1
	wgetcwd_field_rva := text_rva + u32(entry_stub_len + 1)
	wgetcwd_target := u32(i32(wgetcwd_field_rva + 4) + pe_test_i32(image, wgetcwd_field_off))
	assert wgetcwd_target >= runtime_start_rva
	assert wgetcwd_target < first_import_thunk_rva

	wgetcwd_off := pe_test_rva_to_file_off(image, wgetcwd_target)
	assert wgetcwd_off > 0
	assert image[wgetcwd_off..wgetcwd_off + 4] == [u8(0x48), 0x83, 0xec, 0x38] // sub rsp, 56
	assert image[wgetcwd_off + 13..wgetcwd_off + 15] == [u8(0x89), 0xd1] // mov ecx, edx
	assert image[wgetcwd_off + 15..wgetcwd_off + 20] == [
		u8(0x48),
		0x8b,
		0x54,
		0x24,
		0x20,
	] // mov rdx, [rsp+32]
}

fn test_pe_linker_resolves_wgetenv_with_kernel32_runtime_thunk() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 5, true, 1)
	wgetenv_sym := obj.add_undefined('_wgetenv')
	obj.add_text_reloc(1, wgetenv_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	text_off := pe_test_section_header(image, '.text')
	text_rva := pe_test_u32(image, text_off + 12)
	text_raw := int(pe_test_u32(image, text_off + 20))
	text_raw_size := int(pe_test_u32(image, text_off + 16))
	entry_stub_len := linker.entry_stub_size()
	runtime_start_rva := text_rva + u32(entry_stub_len + obj.text_data.len)

	mut first_import_thunk_file_off := -1
	for off in text_raw + entry_stub_len + obj.text_data.len .. text_raw + text_raw_size - 1 {
		if image[off] == 0xff && image[off + 1] == 0x25 {
			first_import_thunk_file_off = off
			break
		}
	}
	assert first_import_thunk_file_off > 0
	first_import_thunk_rva := text_rva + u32(first_import_thunk_file_off - text_raw)

	wgetenv_field_off := text_raw + entry_stub_len + 1
	wgetenv_field_rva := text_rva + u32(entry_stub_len + 1)
	wgetenv_target := u32(i32(wgetenv_field_rva + 4) + pe_test_i32(image, wgetenv_field_off))
	assert wgetenv_target >= runtime_start_rva
	assert wgetenv_target < first_import_thunk_rva

	wgetenv_off := pe_test_rva_to_file_off(image, wgetenv_target)
	assert wgetenv_off > 0
	assert image[wgetenv_off..wgetenv_off + 4] == [u8(0x48), 0x83, 0xec, 0x28] // sub rsp, 40
	data_off := pe_test_section_header(image, '.data')
	assert data_off > 0
	data_rva := pe_test_u32(image, data_off + 12)
	data_size := pe_test_u32(image, data_off + 8)
	assert data_size >= u32(pe_wgetenv_buffer_bytes)

	runtime_text_size := first_import_thunk_file_off - (text_raw + entry_stub_len +
		obj.text_data.len)
	import_offsets := linker.import_thunk_offsets(runtime_text_size)
	names := pe_test_import_names(image)
	assert names == ['ExitProcess', 'GetEnvironmentVariableW']
	getenv_thunk := import_offsets[pe_kernel32_import_key('GetEnvironmentVariableW')] or {
		panic('missing GetEnvironmentVariableW import thunk')
	}
	getenv_thunk_rva := text_rva + getenv_thunk

	mut has_buffer_arg_lea := false
	mut has_buffer_return_lea := false
	mut has_wgetenv_buffer_size := false
	mut calls_getenv := false
	runtime_scan_end := first_import_thunk_file_off - 8
	for off in wgetenv_off .. runtime_scan_end {
		if image[off..off + 3] == [u8(0x48), 0x8d, 0x15] {
			field_rva := text_rva + u32(off - text_raw + 3)
			target := u32(i32(field_rva + 4) + pe_test_i32(image, off + 3))
			if target >= data_rva && target < data_rva + data_size {
				has_buffer_arg_lea = true
			}
		}
		if image[off..off + 3] == [u8(0x48), 0x8d, 0x05] {
			field_rva := text_rva + u32(off - text_raw + 3)
			target := u32(i32(field_rva + 4) + pe_test_i32(image, off + 3))
			if target >= data_rva && target < data_rva + data_size {
				has_buffer_return_lea = true
			}
		}
		if image[off..off + 6] == [u8(0x41), 0xb8, 0x00, 0x80, 0x00, 0x00] {
			has_wgetenv_buffer_size = true
		}
		if image[off] == u8(0xe8) {
			field_rva := text_rva + u32(off - text_raw + 1)
			target := u32(i32(field_rva + 4) + pe_test_i32(image, off + 1))
			if target == getenv_thunk_rva {
				calls_getenv = true
			}
		}
	}
	assert has_buffer_arg_lea
	assert has_buffer_return_lea
	assert has_wgetenv_buffer_size
	assert calls_getenv
	assert 'GetProcessHeap' !in names
	assert 'HeapAlloc' !in names
	assert 'HeapFree' !in names
}

fn test_pe_linker_resolves_aligned_realloc_with_internal_heap_thunk() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 5, true, 1)
	realloc_sym := obj.add_undefined('_aligned_realloc')
	obj.add_text_reloc(1, realloc_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	text_off := pe_test_section_header(image, '.text')
	text_rva := pe_test_u32(image, text_off + 12)
	text_raw := int(pe_test_u32(image, text_off + 20))
	text_raw_size := int(pe_test_u32(image, text_off + 16))
	entry_stub_len := linker.entry_stub_size()
	runtime_start_rva := text_rva + u32(entry_stub_len + obj.text_data.len)

	mut first_import_thunk_file_off := -1
	for off in text_raw + entry_stub_len + obj.text_data.len .. text_raw + text_raw_size - 1 {
		if image[off] == 0xff && image[off + 1] == 0x25 {
			first_import_thunk_file_off = off
			break
		}
	}
	assert first_import_thunk_file_off > 0
	first_import_thunk_rva := text_rva + u32(first_import_thunk_file_off - text_raw)

	realloc_field_off := text_raw + entry_stub_len + 1
	realloc_field_rva := text_rva + u32(entry_stub_len + 1)
	realloc_target := u32(i32(realloc_field_rva + 4) + pe_test_i32(image, realloc_field_off))
	assert realloc_target >= runtime_start_rva
	assert realloc_target < first_import_thunk_rva

	realloc_off := pe_test_rva_to_file_off(image, realloc_target)
	assert realloc_off > 0
	assert image[realloc_off..realloc_off + 4] == [u8(0x49), 0x83, 0xf8, 0x10] // cmp r8, 16
	assert image[realloc_off + 9..realloc_off + 12] == [u8(0x48), 0x85, 0xc9] // test rcx, rcx
}

fn test_pe_linker_resolves_free_with_internal_heap_thunk() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 5, true, 1)
	free_sym := obj.add_undefined('free')
	obj.add_text_reloc(1, free_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	text_off := pe_test_section_header(image, '.text')
	text_rva := pe_test_u32(image, text_off + 12)
	text_raw := int(pe_test_u32(image, text_off + 20))
	text_raw_size := int(pe_test_u32(image, text_off + 16))
	entry_stub_len := linker.entry_stub_size()
	runtime_start_rva := text_rva + u32(entry_stub_len + obj.text_data.len)

	mut first_import_thunk_file_off := -1
	for off in text_raw + entry_stub_len + obj.text_data.len .. text_raw + text_raw_size - 1 {
		if image[off] == 0xff && image[off + 1] == 0x25 {
			first_import_thunk_file_off = off
			break
		}
	}
	assert first_import_thunk_file_off > 0
	first_import_thunk_rva := text_rva + u32(first_import_thunk_file_off - text_raw)

	free_field_off := text_raw + entry_stub_len + 1
	free_field_rva := text_rva + u32(entry_stub_len + 1)
	free_target := u32(i32(free_field_rva + 4) + pe_test_i32(image, free_field_off))
	assert free_target >= runtime_start_rva
	assert free_target < first_import_thunk_rva

	free_off := pe_test_rva_to_file_off(image, free_target)
	assert free_off > 0
	assert image[free_off..free_off + 4] == [u8(0x48), 0x85, 0xc9, 0x74] // test rcx, rcx; je
	assert image[free_off + 5..free_off + 9] == [u8(0x48), 0x83, 0xec, 0x28] // sub rsp, 40
	assert image[free_off + 9..free_off + 14] == [
		u8(0x48),
		0x8b,
		0x41,
		0xf8,
		0x48,
	] // mov rax, [rcx-8]; ...
}

fn test_pe_linker_resolves_array_rune_string_with_internal_runtime_thunk() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 5, true, 1)
	array_rune_string_sym := obj.add_undefined('builtin__Array_rune__string')
	obj.add_text_reloc(1, array_rune_string_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	text_off := pe_test_section_header(image, '.text')
	text_rva := pe_test_u32(image, text_off + 12)
	text_raw := int(pe_test_u32(image, text_off + 20))
	text_raw_size := int(pe_test_u32(image, text_off + 16))
	entry_stub_len := linker.entry_stub_size()
	runtime_start_rva := text_rva + u32(entry_stub_len + obj.text_data.len)

	mut first_import_thunk_file_off := -1
	for off in text_raw + entry_stub_len + obj.text_data.len .. text_raw + text_raw_size - 1 {
		if image[off] == 0xff && image[off + 1] == 0x25 {
			first_import_thunk_file_off = off
			break
		}
	}
	assert first_import_thunk_file_off > 0
	first_import_thunk_rva := text_rva + u32(first_import_thunk_file_off - text_raw)

	array_rune_string_field_off := text_raw + entry_stub_len + 1
	array_rune_string_field_rva := text_rva + u32(entry_stub_len + 1)
	array_rune_string_target := u32(i32(array_rune_string_field_rva + 4) +
		pe_test_i32(image, array_rune_string_field_off))
	assert array_rune_string_target >= runtime_start_rva
	assert array_rune_string_target < first_import_thunk_rva

	array_rune_string_off := pe_test_rva_to_file_off(image, array_rune_string_target)
	assert array_rune_string_off > 0
	assert image[array_rune_string_off..array_rune_string_off + 4] == [
		u8(0x48),
		0x83,
		0xec,
		0x58,
	] // sub rsp, 88
	assert image[array_rune_string_off + 4..array_rune_string_off + 9] == [
		u8(0x48),
		0x89,
		0x4c,
		0x24,
		0x20,
	] // mov [rsp+32], rcx
	mut has_array_len_load := false
	mut has_array_data_load := false
	mut has_array_offset_load := false
	mut has_array_data_offset_add := false
	mut has_array_cap_load_as_len := false
	mut has_aligned_allocation_padding := false
	mut has_aligned_string_cookie_store := false
	mut has_raw_heap_pointer_saved_as_string := false
	mut has_string_len_store := false
	mut has_string_lit_store := false
	mut has_out_of_bounds_string_store := false
	runtime_scan_end := first_import_thunk_file_off - 24
	for off in array_rune_string_off .. runtime_scan_end {
		if image[off..off + 4] == [u8(0x48), 0x63, 0x42, 0x0c] {
			has_array_len_load = true
		}
		if image[off..off + 4] == [u8(0x48), 0x63, 0x42, 0x10] {
			has_array_cap_load_as_len = true
		}
		if image[off..off + 3] == [u8(0x48), 0x8b, 0x02] {
			has_array_data_load = true
		}
		if image[off..off + 4] == [u8(0x4c), 0x63, 0x52, 0x08] {
			has_array_offset_load = true
		}
		if image[off..off + 3] == [u8(0x4c), 0x01, 0xd0] {
			has_array_data_offset_add = true
		}
		if image[off..off + 4] == [u8(0x49), 0x83, 0xc0, 0x18] {
			has_aligned_allocation_padding = true
		}
		if image[off..off + 20] == [
			u8(0x49),
			0x89,
			0xc3,
			0x49,
			0x83,
			0xc3,
			0x17,
			0x49,
			0x83,
			0xe3,
			0xf0,
			0x49,
			0x89,
			0x43,
			0xf8,
			0x4c,
			0x89,
			0x5c,
			0x24,
			0x40,
		] {
			has_aligned_string_cookie_store = true
		}
		if image[off..off + 8] == [u8(0x48), 0x89, 0x44, 0x24, 0x40, 0x49, 0x89, 0xc3] {
			has_raw_heap_pointer_saved_as_string = true
		}
		if image[off..off + 3] == [u8(0x89), 0x51, 0x08] {
			has_string_len_store = true
		}
		if image[off..off + 7] == [u8(0xc7), 0x41, 0x0c, 0, 0, 0, 0] {
			has_string_lit_store = true
		}
		if image[off..off + 8] == [u8(0x48), 0xc7, 0x41, 0x10, 0, 0, 0, 0] {
			has_out_of_bounds_string_store = true
		}
	}
	assert has_array_len_load
	assert !has_array_cap_load_as_len
	assert has_array_data_load
	assert has_array_offset_load
	assert has_array_data_offset_add
	assert has_aligned_allocation_padding
	assert has_aligned_string_cookie_store
	assert !has_raw_heap_pointer_saved_as_string
	assert has_string_len_store
	assert has_string_lit_store
	assert !has_out_of_bounds_string_store
}

fn test_pe_linker_resolves_new_array_from_c_array_noscan_with_internal_heap_thunk() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 5, true, 1)
	new_array_sym := obj.add_undefined('builtin__new_array_from_c_array_noscan')
	obj.add_text_reloc(1, new_array_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	text_off := pe_test_section_header(image, '.text')
	text_rva := pe_test_u32(image, text_off + 12)
	text_raw := int(pe_test_u32(image, text_off + 20))
	text_raw_size := int(pe_test_u32(image, text_off + 16))
	entry_stub_len := linker.entry_stub_size()
	runtime_start_rva := text_rva + u32(entry_stub_len + obj.text_data.len)

	mut first_import_thunk_file_off := -1
	for off in text_raw + entry_stub_len + obj.text_data.len .. text_raw + text_raw_size - 1 {
		if image[off] == 0xff && image[off + 1] == 0x25 {
			first_import_thunk_file_off = off
			break
		}
	}
	assert first_import_thunk_file_off > 0
	first_import_thunk_rva := text_rva + u32(first_import_thunk_file_off - text_raw)

	new_array_field_off := text_raw + entry_stub_len + 1
	new_array_field_rva := text_rva + u32(entry_stub_len + 1)
	new_array_target := u32(i32(new_array_field_rva + 4) + pe_test_i32(image, new_array_field_off))
	assert new_array_target >= runtime_start_rva
	assert new_array_target < first_import_thunk_rva

	new_array_off := pe_test_rva_to_file_off(image, new_array_target)
	assert new_array_off > 0
	assert image[new_array_off..new_array_off + 4] == [u8(0x48), 0x83, 0xec, 0x58] // sub rsp, 88
	mut has_stack_c_array_arg_load := false
	mut has_heap_zero_memory_flag := false
	mut has_managed_array_allocation_padding_size := false
	mut has_header_align_rounding := false
	mut has_header_align_mask := false
	mut has_cookie_before_array_header := false
	mut has_array_header_false_store := false
	mut has_data_after_array_header_store := false
	mut has_copy_to_data_after_array_header := false
	mut has_cookie_over_array_header := false
	mut has_extra_header_skip_after_cookie := false
	mut has_naive_heap_plus_header_data := false
	mut has_noscan_flags_store := false
	mut has_element_size_store := false
	mut has_byte_copy_loop := false
	runtime_scan_end := first_import_thunk_file_off - 16
	for off in new_array_off .. runtime_scan_end {
		if image[off..off + 8] == [u8(0x48), 0x8b, 0x84, 0x24, 0x80, 0, 0, 0] {
			has_stack_c_array_arg_load = true
		}
		if image[off..off + 5] == [u8(0xba), 0x08, 0, 0, 0] {
			has_heap_zero_memory_flag = true
		}
		if image[off..off + 4] == [u8(0x49), 0x83, 0xc0, 0x20] {
			has_managed_array_allocation_padding_size = true
		}
		if image[off..off + 4] == [u8(0x49), 0x83, 0xc3, 0x17] {
			has_header_align_rounding = true
		}
		if image[off..off + 4] == [u8(0x49), 0x83, 0xe3, 0xf0] {
			has_header_align_mask = true
		}
		if image[off..off + 20] == [
			u8(0x49),
			0x89,
			0x43,
			0xf8,
			0x41,
			0xc6,
			0x03,
			0x00,
			0x48,
			0x8b,
			0x4c,
			0x24,
			0x20,
			0x49,
			0x8d,
			0x43,
			0x08,
			0x48,
			0x89,
			0x01,
		] {
			has_cookie_before_array_header = true
			has_array_header_false_store = true
			has_data_after_array_header_store = true
		}
		if image[off..off + 3] == [u8(0x49), 0x89, 0x03] {
			has_cookie_over_array_header = true
		}
		if image[off..off + 4] == [u8(0x4d), 0x8d, 0x43, 0x08] {
			has_copy_to_data_after_array_header = true
		}
		if image[off..off + 4] == [u8(0x49), 0x83, 0xc3, 0x08] {
			has_extra_header_skip_after_cookie = true
		}
		if image[off..off + 4] == [u8(0x4c), 0x8d, 0x58, 0x08] {
			has_naive_heap_plus_header_data = true
		}
		if image[off..off + 7] == [u8(0xc7), 0x41, 0x14, 0x30, 0, 0, 0] {
			has_noscan_flags_store = true
		}
		if image[off..off + 3] == [u8(0x89), 0x41, 0x18] {
			has_element_size_store = true
		}
		if image[off..off + 6] == [u8(0x41), 0x8a, 0x12, 0x41, 0x88, 0x10] {
			has_byte_copy_loop = true
		}
	}
	assert has_stack_c_array_arg_load
	assert has_heap_zero_memory_flag
	assert has_managed_array_allocation_padding_size
	assert has_header_align_rounding
	assert has_header_align_mask
	assert has_cookie_before_array_header
	assert has_array_header_false_store
	assert has_data_after_array_header_store
	assert has_copy_to_data_after_array_header
	assert !has_cookie_over_array_header
	assert !has_extra_header_skip_after_cookie
	assert !has_naive_heap_plus_header_data
	assert has_noscan_flags_store
	assert has_element_size_store
	assert has_byte_copy_loop
}

fn test_pe_linker_resolves_new_array_noscan_with_internal_heap_thunk() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 5, true, 1)
	new_array_sym := obj.add_undefined('builtin____new_array_noscan')
	obj.add_text_reloc(1, new_array_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	text_off := pe_test_section_header(image, '.text')
	text_rva := pe_test_u32(image, text_off + 12)
	text_raw := int(pe_test_u32(image, text_off + 20))
	text_raw_size := int(pe_test_u32(image, text_off + 16))
	entry_stub_len := linker.entry_stub_size()
	runtime_start_rva := text_rva + u32(entry_stub_len + obj.text_data.len)

	mut first_import_thunk_file_off := -1
	for off in text_raw + entry_stub_len + obj.text_data.len .. text_raw + text_raw_size - 1 {
		if image[off] == 0xff && image[off + 1] == 0x25 {
			first_import_thunk_file_off = off
			break
		}
	}
	assert first_import_thunk_file_off > 0
	first_import_thunk_rva := text_rva + u32(first_import_thunk_file_off - text_raw)

	new_array_field_off := text_raw + entry_stub_len + 1
	new_array_field_rva := text_rva + u32(entry_stub_len + 1)
	new_array_target := u32(i32(new_array_field_rva + 4) + pe_test_i32(image, new_array_field_off))
	assert new_array_target >= runtime_start_rva
	assert new_array_target < first_import_thunk_rva

	new_array_off := pe_test_rva_to_file_off(image, new_array_target)
	assert new_array_off > 0
	assert image[new_array_off..new_array_off + 4] == [u8(0x48), 0x83, 0xec, 0x58] // sub rsp, 88
	names := pe_test_import_names(image)
	assert 'GetProcessHeap' in names
	assert 'HeapAlloc' in names
}

fn test_pe_linker_rejects_unsupported_external_symbols() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 0, true, 1)
	unknown_sym := obj.add_undefined('v_missing_runtime_symbol')
	obj.add_text_reloc(1, unknown_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	if _ := linker.image() {
		assert false
	} else {
		assert err.msg().starts_with('x64: unsupported backend feature: ')
		assert err.msg().contains('cannot resolve external symbol `v_missing_runtime_symbol` yet')
		assert err.msg().contains('.text relocation offset 0x00000001')
		assert err.msg().contains('near `main`+0x00000001')
	}
}

fn test_pe_linker_rejects_crt_stdio_symbols_with_minimal_runtime_message() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 0, true, 1)
	stderr_sym := obj.add_undefined('stderr')
	obj.add_text_reloc(1, stderr_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	if _ := linker.image() {
		assert false
	} else {
		assert err.msg().starts_with('x64: unsupported backend feature: ')
		assert err.msg().contains('cannot resolve C stdio/file-descriptor symbol `stderr`')
		assert err.msg().contains('Windows x64 native backend uses Kernel32 handles')
		assert err.msg().contains('Kernel32 handles')
		assert err.msg().contains('C FILE/stdio calls')
		assert !err.msg().contains('reachable helper')
		assert !err.msg().contains('stdout/stderr path')
		assert !err.msg().contains('C FILE streams')
		assert err.msg().contains('near `main`+0x00000001')
	}
}

fn test_pe_linker_rejects_common_crt_stdio_functions_with_minimal_runtime_message() {
	for symbol_name in ['setvbuf', 'printf', 'snprintf', 'puts', 'fputs', 'fopen'] {
		mut obj := CoffObject.new()
		obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
		obj.add_symbol('main', 0, true, 1)
		stdio_sym := obj.add_undefined(symbol_name)
		obj.add_text_reloc(1, stdio_sym, coff_image_rel_amd64_rel32)

		mut linker := PeLinker.new(obj)
		if _ := linker.image() {
			assert false
		} else {
			assert err.msg().starts_with('x64: unsupported backend feature: ')
			assert err.msg().contains('cannot resolve C stdio/file-descriptor symbol `${symbol_name}`')
			assert err.msg().contains('Windows x64 native backend uses Kernel32 handles')
			assert err.msg().contains('Kernel32 handles')
			assert err.msg().contains('C FILE/stdio calls')
			assert !err.msg().contains('reachable helper')
			assert !err.msg().contains('stdout/stderr path')
			assert !err.msg().contains('C FILE streams')
			assert err.msg().contains('near `main`+0x00000001')
		}
	}
}

fn test_pe_linker_rejects_missing_v_runtime_helper_with_targeted_message() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 0, true, 1)
	helper_sym := obj.add_undefined('builtin__Map_string_int__keys')
	obj.add_text_reloc(1, helper_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	if _ := linker.image() {
		assert false
	} else {
		assert err.msg().starts_with('x64: unsupported backend feature: ')
		assert err.msg().contains('cannot resolve V runtime helper `builtin__Map_string_int__keys`')
		assert err.msg().contains('native x64 backend does not implement this feature for this target yet')
		assert !err.msg().contains('not imported')
		assert !err.msg().contains('must be compiled')
		assert !err.msg().contains('lowered before linking')
		assert err.msg().contains('near `main`+0x00000001')
	}
}

fn test_pe_linker_rejects_relocation_to_omitted_rdata_section() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 0, true, 1)
	rodata_sym := obj.add_symbol('missing_rodata', 0, false, 2)
	obj.add_text_reloc(1, rodata_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	if _ := linker.image() {
		assert false
	} else {
		assert err.msg().contains('relocation references .rdata')
		assert err.msg().contains('no .rdata section was emitted')
	}
}

fn test_pe_linker_rejects_relocation_to_omitted_data_section() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 0, true, 1)
	data_sym := obj.add_symbol('missing_data', 0, false, 3)
	obj.add_text_reloc(1, data_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	if _ := linker.image() {
		assert false
	} else {
		assert err.msg().contains('relocation references .data')
		assert err.msg().contains('no .data section was emitted')
	}
}

fn test_x64_gen_link_executable_writes_pe_image() {
	path := os.join_path(os.temp_dir(), 'v_x64_pe_link_api_test_${rand.ulid()}.exe')
	os.rm(path) or {}
	defer {
		os.rm(path) or {}
	}

	mut mod := mir.Module{}
	mut gen := Gen.new_with_format_and_abi(&mod, .coff, .windows)
	gen.coff.text_data << u8(0xc3)
	gen.coff.add_symbol('main', 0, true, 1)

	gen.link_executable(path) or { panic(err) }
	assert os.is_file(path)
	assert os.file_size(path) > 0
	image := os.read_bytes(path) or { panic(err) }

	assert image[0] == `M`
	assert image[1] == `Z`
	pe_off := int(pe_test_u32(image, 0x3c))
	assert pe_test_u32(image, pe_off) == pe_signature
	assert pe_test_u16(image, pe_off + 4) == coff_image_file_machine_amd64
}

fn test_pe_linker_write_postcondition_rejects_missing_or_empty_output() {
	missing_path := os.join_path(os.temp_dir(),
		'v_x64_pe_link_missing_postcondition_test_${rand.ulid()}.exe')
	empty_path := os.join_path(os.temp_dir(),
		'v_x64_pe_link_empty_postcondition_test_${rand.ulid()}.exe')
	os.rm(missing_path) or {}
	os.rm(empty_path) or {}
	defer {
		os.rm(missing_path) or {}
		os.rm(empty_path) or {}
	}

	if _ := pe_check_written_image(missing_path, 512) {
		assert false
	} else {
		assert err.msg().contains('was not created as a file')
	}

	os.write_file(empty_path, '') or { panic(err) }
	if _ := pe_check_written_image(empty_path, 512) {
		assert false
	} else {
		assert err.msg().contains('has size 0 bytes')
	}
}

fn test_x64_gen_link_executable_requires_windows_abi() {
	path := os.join_path(os.temp_dir(), 'v_x64_pe_link_api_sysv_test.exe')
	defer {
		os.rm(path) or {}
	}

	mut mod := mir.Module{}
	mut gen := Gen.new_with_format(&mod, .coff)
	gen.coff.text_data << u8(0xc3)
	gen.coff.add_symbol('main', 0, true, 1)

	if _ := gen.link_executable(path) {
		assert false
	} else {
		assert err.msg().contains('requires Windows ABI')
	}
}

fn test_x64_gen_link_executable_rejects_non_coff_format() {
	path := os.join_path(os.temp_dir(), 'v_x64_pe_link_api_elf_test.exe')
	defer {
		os.rm(path) or {}
	}

	mut mod := mir.Module{}
	mut gen := Gen.new_with_format_and_abi(&mod, .elf, .sysv)

	if _ := gen.link_executable(path) {
		assert false
	} else {
		assert err.msg().contains('only implemented for COFF')
	}
}
