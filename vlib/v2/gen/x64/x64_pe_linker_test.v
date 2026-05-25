module x64

import os
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

fn pe_test_string(data []u8, off int) string {
	mut end := off
	for end < data.len && data[end] != 0 {
		end++
	}
	return data[off..end].bytestr()
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
	assert pe_test_u16(image, pe_off + 20) == pe_size_of_optional_header64
	assert pe_test_u16(image, pe_off + 22) & pe_image_file_relocs_stripped != 0
	assert pe_test_u16(image, pe_off + 22) & pe_image_file_executable_image != 0

	opt_off := pe_off + 4 + 20
	assert pe_test_u16(image, opt_off) == pe_optional_header64_magic
	assert pe_test_u32(image, opt_off + 16) == pe_section_alignment
	assert pe_test_u32(image, opt_off + 20) == pe_section_alignment
	assert pe_test_u64(image, opt_off + 24) == pe_image_base
	assert pe_test_u32(image, opt_off + 32) == pe_section_alignment
	assert pe_test_u32(image, opt_off + 36) == pe_file_alignment
	assert pe_test_u32(image, opt_off + 60) == pe_headers_size(4)
	assert pe_test_u16(image, opt_off + 68) == pe_image_subsystem_windows_cui
	assert pe_test_u16(image, opt_off + 70) & pe_dll_characteristics_nx_compat != 0
	assert pe_test_u32(image, opt_off + 108) == pe_number_of_rva_and_sizes

	text_off := pe_test_section_header(image, '.text')
	rdata_off := pe_test_section_header(image, '.rdata')
	data_off := pe_test_section_header(image, '.data')
	idata_off := pe_test_section_header(image, '.idata')
	assert text_off > 0
	assert rdata_off > text_off
	assert data_off > rdata_off
	assert idata_off > data_off
	assert pe_test_u32(image, text_off + 12) == pe_section_alignment
	assert pe_test_u32(image, text_off + 20) == pe_headers_size(4)
	assert pe_test_u32(image, text_off + 20) % pe_file_alignment == 0
	assert pe_test_u32(image, text_off + 16) % pe_file_alignment == 0
	assert pe_test_u32(image, text_off + 36) & pe_image_scn_cnt_code != 0
	assert pe_test_u32(image, text_off + 36) & pe_image_scn_mem_execute != 0
	assert pe_test_u32(image, rdata_off + 12) % pe_section_alignment == 0
	assert pe_test_u32(image, rdata_off + 36) & pe_image_scn_mem_read != 0
	assert pe_test_u32(image, data_off + 36) & pe_image_scn_mem_write != 0
	assert pe_test_u32(image, idata_off + 12) % pe_section_alignment == 0
	assert pe_test_u32(image, idata_off + 36) & pe_image_scn_mem_write != 0

	size_of_image := pe_test_u32(image, opt_off + 56)
	idata_end := pe_test_u32(image, idata_off + 12) + pe_section_alignment
	assert size_of_image >= idata_end
}

fn test_pe_linker_emits_kernel32_import_table() {
	obj := sample_pe_coff_object()
	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }

	pe_off := int(pe_test_u32(image, 0x3c))
	opt_off := pe_off + 4 + 20
	import_rva := pe_test_u32(image, opt_off + 112 + pe_import_directory_index * 8)
	import_size := pe_test_u32(image, opt_off + 116 + pe_import_directory_index * 8)
	iat_rva := pe_test_u32(image, opt_off + 112 + pe_iat_directory_index * 8)
	iat_size := pe_test_u32(image, opt_off + 116 + pe_iat_directory_index * 8)
	assert import_rva != 0
	assert import_size == 40
	assert iat_rva != 0
	assert iat_size == u32((pe_kernel32_imports.len + 1) * 8)

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
	for i in 0 .. pe_kernel32_imports.len {
		hint_name_rva := u32(pe_test_u64(image, ilt_off + i * 8))
		hint_name_off := pe_test_rva_to_file_off(image, hint_name_rva)
		assert pe_test_u16(image, hint_name_off) == 0
		assert hint_name_off % 2 == 0
		names << pe_test_string(image, hint_name_off + 2)
		assert pe_test_u64(image, iat_off + i * 8) == pe_test_u64(image, ilt_off + i * 8)
	}
	assert pe_test_u64(image, ilt_off + pe_kernel32_imports.len * 8) == 0
	assert pe_test_u64(image, iat_off + pe_kernel32_imports.len * 8) == 0
	assert names == pe_kernel32_imports
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

fn test_pe_linker_empty_rdata_and_data_have_no_raw_storage_but_stable_rvas() {
	mut obj := CoffObject.new()
	obj.text_data << u8(0xc3)
	obj.add_symbol('main', 0, true, 1)

	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }
	pe_off := int(pe_test_u32(image, 0x3c))
	opt_off := pe_off + 4 + 20
	text_off := pe_test_section_header(image, '.text')
	rdata_off := pe_test_section_header(image, '.rdata')
	data_off := pe_test_section_header(image, '.data')
	idata_off := pe_test_section_header(image, '.idata')

	assert pe_test_u32(image, rdata_off + 8) == 0
	assert pe_test_u32(image, rdata_off + 16) == 0
	assert pe_test_u32(image, rdata_off + 20) == 0
	assert pe_test_u32(image, data_off + 8) == 0
	assert pe_test_u32(image, data_off + 16) == 0
	assert pe_test_u32(image, data_off + 20) == 0

	assert pe_test_u32(image, text_off + 12) == pe_section_alignment
	assert pe_test_u32(image, rdata_off + 12) == pe_section_alignment * 2
	assert pe_test_u32(image, data_off + 12) == pe_section_alignment * 3
	assert pe_test_u32(image, idata_off + 12) == pe_section_alignment * 4
	assert pe_test_u32(image, idata_off + 20) == pe_headers_size(4) + pe_file_alignment
	assert pe_test_u32(image, opt_off + 56) == pe_section_alignment * 5
}

fn test_pe_linker_entry_stub_targets_main_and_exitprocess_thunk() {
	obj := sample_pe_coff_object()
	mut linker := PeLinker.new(obj)
	image := linker.image() or { panic(err) }

	text_off := pe_test_section_header(image, '.text')
	text_rva := pe_test_u32(image, text_off + 12)
	text_raw := int(pe_test_u32(image, text_off + 20))
	entry_stub_len := linker.entry_stub_size()

	assert image[text_raw..text_raw + 4] == [u8(0x48), 0x83, 0xec, 0x28]
	assert image[text_raw + entry_stub_len - 1] == 0xcc

	exit_thunk_off := text_raw + entry_stub_len + obj.text_data.len
	assert image[exit_thunk_off] == 0xff
	assert image[exit_thunk_off + 1] == 0x25

	pe_off := int(pe_test_u32(image, 0x3c))
	opt_off := pe_off + 4 + 20
	iat_rva := pe_test_u32(image, opt_off + 112 + pe_iat_directory_index * 8)
	exit_thunk_rva := text_rva + u32(entry_stub_len + obj.text_data.len)
	exit_disp := pe_test_i32(image, exit_thunk_off + 2)
	assert u32(i32(exit_thunk_rva + 6) + exit_disp) == iat_rva
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
	assert target == text_rva + u32(linker.entry_stub_size() + obj.text_data.len)
}

fn test_pe_linker_rejects_unsupported_external_symbols() {
	mut obj := CoffObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0xc3]
	obj.add_symbol('main', 5, true, 1)
	calloc_sym := obj.add_undefined('calloc')
	obj.add_text_reloc(1, calloc_sym, coff_image_rel_amd64_rel32)

	mut linker := PeLinker.new(obj)
	if _ := linker.image() {
		assert false
	} else {
		assert err.msg().contains('cannot resolve external symbol `calloc` yet')
	}
}

fn test_x64_gen_link_executable_writes_pe_image() {
	path := os.join_path(os.temp_dir(), 'v_x64_pe_link_api_test.exe')
	os.rm(path) or {}
	defer {
		os.rm(path) or {}
	}

	mut mod := mir.Module{}
	mut gen := Gen.new_with_format_and_abi(&mod, .coff, .windows)
	gen.coff.text_data << u8(0xc3)
	gen.coff.add_symbol('main', 0, true, 1)

	gen.link_executable(path) or { panic(err) }
	image := os.read_bytes(path) or { panic(err) }

	assert image[0] == `M`
	assert image[1] == `Z`
	pe_off := int(pe_test_u32(image, 0x3c))
	assert pe_test_u32(image, pe_off) == pe_signature
	assert pe_test_u16(image, pe_off + 4) == coff_image_file_machine_amd64
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
