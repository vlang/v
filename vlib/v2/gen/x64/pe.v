// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module x64

import os

const pe_dos_stub_size = 0x80
const pe_signature = u32(0x00004550)
const pe_optional_header64_magic = u16(0x20b)
const pe_file_alignment = 0x200
const pe_section_alignment = 0x1000
const pe_image_base = u64(0x140000000)
const pe_size_of_optional_header64 = u16(0xf0)
const pe_number_of_rva_and_sizes = 16

const pe_image_file_relocs_stripped = u16(0x0001)
const pe_image_file_executable_image = u16(0x0002)
const pe_image_file_large_address_aware = u16(0x0020)
const pe_image_subsystem_windows_cui = u16(3)
const pe_dll_characteristics_nx_compat = u16(0x0100)

const pe_image_scn_cnt_code = u32(0x00000020)
const pe_image_scn_cnt_initialized_data = u32(0x00000040)
const pe_image_scn_mem_execute = u32(0x20000000)
const pe_image_scn_mem_read = u32(0x40000000)
const pe_image_scn_mem_write = u32(0x80000000)

const pe_import_directory_index = 1
const pe_exception_directory_index = 3
const pe_base_reloc_directory_index = 5
const pe_iat_directory_index = 12

const pe_kernel32_imports = ['ExitProcess', 'GetStdHandle', 'GetConsoleMode', 'MultiByteToWideChar',
	'WriteConsoleW', 'WriteFile', 'GetProcessHeap', 'HeapAlloc', 'HeapFree']

struct PeImport {
	dll  string
	name string
}

struct PeSection {
mut:
	name            string
	data            []u8
	virtual_address u32
	virtual_size    u32
	raw_pointer     u32
	raw_size        u32
	characteristics u32
}

struct PeIdata {
	data        []u8
	import_rva  u32
	import_size u32
	iat_rva     u32
	iat_size    u32
}

pub struct PeLinker {
	coff &CoffObject
mut:
	imports []PeImport
}

pub fn PeLinker.new(coff &CoffObject) &PeLinker {
	mut linker := unsafe {
		&PeLinker{
			coff: coff
		}
	}
	for name in pe_kernel32_imports {
		linker.imports << PeImport{
			dll:  'kernel32.dll'
			name: name
		}
	}
	return linker
}

pub fn (mut l PeLinker) write(path string) ! {
	image := l.image()!
	os.write_file_array(path, image)!
}

pub fn (mut l PeLinker) image() ![]u8 {
	mut text := l.build_text_section()!
	text_virtual_size := text.len
	rdata_virtual_size := l.coff.rodata.len
	data_virtual_size := l.coff.data_data.len

	header_size := pe_headers_size(4)
	text_rva := u32(pe_section_alignment)
	rdata_rva := pe_next_section_rva(text_rva, text_virtual_size)
	data_rva := pe_next_section_rva(rdata_rva, rdata_virtual_size)
	idata_rva := pe_next_section_rva(data_rva, data_virtual_size)

	idata := l.build_idata(idata_rva)
	mut raw_pointer := header_size

	text_section, next_raw := pe_make_section('.text', text, text_rva, raw_pointer,
		pe_image_scn_cnt_code | pe_image_scn_mem_execute | pe_image_scn_mem_read)
	raw_pointer = next_raw
	rdata_section, next_raw2 := pe_make_section('.rdata', l.coff.rodata, rdata_rva, raw_pointer,
		pe_image_scn_cnt_initialized_data | pe_image_scn_mem_read)
	raw_pointer = next_raw2
	data_section, next_raw3 := pe_make_section('.data', l.coff.data_data, data_rva, raw_pointer,
		pe_image_scn_cnt_initialized_data | pe_image_scn_mem_read | pe_image_scn_mem_write)
	raw_pointer = next_raw3
	idata_section, next_raw4 := pe_make_section('.idata', idata.data, idata_rva, raw_pointer,
		pe_image_scn_cnt_initialized_data | pe_image_scn_mem_read | pe_image_scn_mem_write)
	raw_pointer = next_raw4
	sections := [text_section, rdata_section, data_section, idata_section]

	import_thunks := l.import_thunk_rvas(text_section.virtual_address)
	l.apply_text_relocations(mut text, text_section.virtual_address, rdata_section.virtual_address,
		data_section.virtual_address, import_thunks)!

	mut patched_sections := sections.clone()
	patched_sections[0].data = text

	size_of_image := pe_size_of_image(patched_sections)
	size_of_code := text_section.raw_size
	size_of_initialized_data := rdata_section.raw_size + data_section.raw_size +
		idata_section.raw_size

	mut buf := []u8{cap: int(raw_pointer)}
	write_pe_dos_stub(mut buf)
	for buf.len < pe_dos_stub_size {
		buf << 0
	}

	write_u32_le(mut buf, pe_signature)
	write_u16_le(mut buf, coff_image_file_machine_amd64)
	write_u16_le(mut buf, u16(patched_sections.len))
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u16_le(mut buf, pe_size_of_optional_header64)
	// No .reloc section is emitted yet, so mark the image fixed-base.
	write_u16_le(mut buf,
		pe_image_file_relocs_stripped | pe_image_file_executable_image | pe_image_file_large_address_aware)

	l.write_optional_header(mut buf, size_of_code, size_of_initialized_data, size_of_image,
		header_size, idata, text_section.virtual_address)

	for section in patched_sections {
		write_pe_section_header(mut buf, section)
	}

	for buf.len < header_size {
		buf << 0
	}
	for section in patched_sections {
		if section.raw_size == 0 {
			continue
		}
		for buf.len < int(section.raw_pointer) {
			buf << 0
		}
		buf << section.data
		for buf.len < int(section.raw_pointer + section.raw_size) {
			buf << 0
		}
	}

	return buf
}

fn (l PeLinker) build_text_section() ![]u8 {
	main_rva := l.defined_symbol_offset('main') or {
		return error('PE linker requires a defined main symbol')
	}
	mut text := []u8{}
	vinit_offset := l.defined_symbol_offset('_vinit') or { u32(0xffff_ffff) }
	text << [u8(0x48), 0x83, 0xec, 0x28] // sub rsp, 40
	if vinit_offset != 0xffff_ffff {
		pe_emit_call_placeholder(mut text)
	}
	pe_emit_call_placeholder(mut text)
	text << [u8(0x31), 0xc9] // xor ecx, ecx
	pe_emit_call_placeholder(mut text)
	text << 0xcc

	entry_stub_len := text.len
	text << l.coff.text_data
	for _ in l.imports {
		text << [u8(0xff), 0x25, 0, 0, 0, 0] // jmp qword ptr [rip + disp32]
	}

	mut cursor := 4
	if vinit_offset != 0xffff_ffff {
		pe_patch_rel32(mut text, cursor + 1, u32(entry_stub_len) + vinit_offset, 0)
		cursor += 5
	}
	pe_patch_rel32(mut text, cursor + 1, u32(entry_stub_len) + main_rva, 0)
	cursor += 5
	cursor += 2
	exit_thunk_off := u32(entry_stub_len + l.coff.text_data.len)
	pe_patch_rel32(mut text, cursor + 1, exit_thunk_off, 0)

	return text
}

fn (l PeLinker) build_idata(idata_rva u32) PeIdata {
	descriptor_size := 20
	descriptor_count := 2 // kernel32 + null terminator
	ilt_off := descriptor_size * descriptor_count
	ilt_size := (l.imports.len + 1) * 8
	iat_off := ilt_off + ilt_size
	iat_size := (l.imports.len + 1) * 8
	hint_name_off := iat_off + iat_size

	mut data := []u8{len: hint_name_off}
	mut hint_name_rvas := []u32{cap: l.imports.len}
	for imp in l.imports {
		hint_name_rvas << idata_rva + u32(data.len)
		write_u16_le(mut data, 0)
		data << imp.name.bytes()
		data << 0
		if data.len % 2 != 0 {
			data << 0
		}
	}
	dll_name_rva := idata_rva + u32(data.len)
	data << 'kernel32.dll'.bytes()
	data << 0

	pe_put_u32_le(mut data, 0, idata_rva + u32(ilt_off))
	pe_put_u32_le(mut data, 12, dll_name_rva)
	pe_put_u32_le(mut data, 16, idata_rva + u32(iat_off))
	for i, rva in hint_name_rvas {
		pe_put_u64_le(mut data, ilt_off + i * 8, u64(rva))
		pe_put_u64_le(mut data, iat_off + i * 8, u64(rva))
	}

	return PeIdata{
		data:        data
		import_rva:  idata_rva
		import_size: u32(descriptor_size * descriptor_count)
		iat_rva:     idata_rva + u32(iat_off)
		iat_size:    u32(iat_size)
	}
}

fn (l PeLinker) apply_text_relocations(mut text []u8, text_rva u32, rdata_rva u32, data_rva u32, import_thunks map[string]u32) ! {
	entry_stub_len := l.entry_stub_size()
	for reloc in l.coff.text_relocs {
		if reloc.type_ != coff_image_rel_amd64_rel32 {
			return error('PE linker does not support COFF relocation type 0x${reloc.type_:04x}')
		}
		if reloc.sym_idx < 0 || reloc.sym_idx >= l.coff.symbols.len {
			return error('PE linker relocation references invalid symbol index ${reloc.sym_idx}')
		}
		sym := l.coff.symbols[reloc.sym_idx]
		target_rva := match sym.section {
			1 {
				text_rva + u32(entry_stub_len) + sym.value
			}
			2 {
				rdata_rva + sym.value
			}
			3 {
				data_rva + sym.value
			}
			0 {
				import_thunks[sym.name] or {
					return error('PE linker cannot resolve external symbol `${sym.name}` yet')
				}
			}
			else {
				return error('PE linker cannot resolve symbol `${sym.name}` in COFF section ${sym.section}')
			}
		}

		field_off := entry_stub_len + int(reloc.offset)
		old_disp := pe_read_i32_le(text, field_off)
		field_rva := text_rva + u32(field_off)
		new_disp := old_disp + i32(int(target_rva) - (int(field_rva) + 4))
		pe_put_u32_le(mut text, field_off, u32(new_disp))
	}

	thunk_base := entry_stub_len + l.coff.text_data.len
	for i, imp in l.imports {
		thunk_off := thunk_base + i * 6
		iat_entry_rva := l.iat_entry_rva(imp.name)
		thunk_rva := text_rva + u32(thunk_off)
		disp := i32(int(iat_entry_rva) - (int(thunk_rva) + 6))
		pe_put_u32_le(mut text, thunk_off + 2, u32(disp))
	}
}

fn (l PeLinker) import_thunk_rvas(text_rva u32) map[string]u32 {
	entry_stub_len := l.entry_stub_size()
	thunk_base := entry_stub_len + l.coff.text_data.len
	mut out := map[string]u32{}
	for i, imp in l.imports {
		out[imp.name] = text_rva + u32(thunk_base + i * 6)
	}
	return out
}

fn (l PeLinker) iat_entry_rva(name string) u32 {
	for i, imp in l.imports {
		if imp.name == name {
			return l.idata_iat_rva() + u32(i * 8)
		}
	}
	return 0
}

fn (l PeLinker) idata_iat_rva() u32 {
	text_size := l.entry_stub_size() + l.coff.text_data.len + l.imports.len * 6
	rdata_rva := pe_next_section_rva(u32(pe_section_alignment), text_size)
	data_rva := pe_next_section_rva(rdata_rva, l.coff.rodata.len)
	idata_rva := pe_next_section_rva(data_rva, l.coff.data_data.len)
	ilt_off := 20 * 2
	ilt_size := (l.imports.len + 1) * 8
	iat_off := ilt_off + ilt_size
	return idata_rva + u32(iat_off)
}

fn (l PeLinker) entry_stub_size() int {
	vinit := l.defined_symbol_offset('_vinit') or { u32(0xffff_ffff) }
	return if vinit == 0xffff_ffff { 17 } else { 22 }
}

fn (l PeLinker) defined_symbol_offset(name string) ?u32 {
	for sym in l.coff.symbols {
		if sym.name == name && sym.section == 1 {
			return sym.value
		}
	}
	return none
}

fn (mut l PeLinker) write_optional_header(mut buf []u8, size_of_code u32, size_of_initialized_data u32, size_of_image u32, size_of_headers int, idata PeIdata, entry_rva u32) {
	write_u16_le(mut buf, pe_optional_header64_magic)
	buf << 0
	buf << 1
	write_u32_le(mut buf, size_of_code)
	write_u32_le(mut buf, size_of_initialized_data)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, entry_rva)
	write_u32_le(mut buf, entry_rva)
	write_u64_le(mut buf, pe_image_base)
	write_u32_le(mut buf, pe_section_alignment)
	write_u32_le(mut buf, pe_file_alignment)
	write_u16_le(mut buf, 6)
	write_u16_le(mut buf, 0)
	write_u16_le(mut buf, 0)
	write_u16_le(mut buf, 0)
	write_u16_le(mut buf, 6)
	write_u16_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, size_of_image)
	write_u32_le(mut buf, u32(size_of_headers))
	write_u32_le(mut buf, 0)
	write_u16_le(mut buf, pe_image_subsystem_windows_cui)
	write_u16_le(mut buf, pe_dll_characteristics_nx_compat)
	write_u64_le(mut buf, 0x100000)
	write_u64_le(mut buf, 0x1000)
	write_u64_le(mut buf, 0x100000)
	write_u64_le(mut buf, 0x1000)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, pe_number_of_rva_and_sizes)

	for i in 0 .. pe_number_of_rva_and_sizes {
		if i == pe_import_directory_index {
			write_u32_le(mut buf, idata.import_rva)
			write_u32_le(mut buf, idata.import_size)
		} else if i == pe_exception_directory_index || i == pe_base_reloc_directory_index {
			// Unwind and base relocation directories are not emitted yet.
			write_u32_le(mut buf, 0)
			write_u32_le(mut buf, 0)
		} else if i == pe_iat_directory_index {
			write_u32_le(mut buf, idata.iat_rva)
			write_u32_le(mut buf, idata.iat_size)
		} else {
			write_u32_le(mut buf, 0)
			write_u32_le(mut buf, 0)
		}
	}
}

fn write_pe_dos_stub(mut buf []u8) {
	buf << [u8(`M`), `Z`]
	for buf.len < 0x3c {
		buf << 0
	}
	write_u32_le(mut buf, pe_dos_stub_size)
}

fn write_pe_section_header(mut buf []u8, section PeSection) {
	write_fixed_string(mut buf, section.name, 8)
	write_u32_le(mut buf, section.virtual_size)
	write_u32_le(mut buf, section.virtual_address)
	write_u32_le(mut buf, section.raw_size)
	write_u32_le(mut buf, section.raw_pointer)
	write_u32_le(mut buf, 0)
	write_u32_le(mut buf, 0)
	write_u16_le(mut buf, 0)
	write_u16_le(mut buf, 0)
	write_u32_le(mut buf, section.characteristics)
}

fn pe_make_section(name string, data []u8, virtual_address u32, raw_pointer int, characteristics u32) (PeSection, int) {
	raw_size := if data.len == 0 { 0 } else { align_int(data.len, pe_file_alignment) }
	raw_ptr := if raw_size == 0 { 0 } else { raw_pointer }
	mut next_raw_pointer := raw_pointer
	if raw_size > 0 {
		next_raw_pointer += raw_size
	}
	return PeSection{
		name:            name
		data:            data.clone()
		virtual_address: virtual_address
		virtual_size:    u32(data.len)
		raw_pointer:     u32(raw_ptr)
		raw_size:        u32(raw_size)
		characteristics: characteristics
	}, next_raw_pointer
}

fn pe_headers_size(section_count int) int {
	return align_int(pe_dos_stub_size + 4 + 20 + int(pe_size_of_optional_header64) +
		section_count * 40, pe_file_alignment)
}

fn pe_size_of_image(sections []PeSection) u32 {
	mut end := 0
	for section in sections {
		virtual_size := if section.virtual_size == 0 { 1 } else { int(section.virtual_size) }
		section_end := int(section.virtual_address) + align_int(virtual_size, pe_section_alignment)
		if section_end > end {
			end = section_end
		}
	}
	return u32(align_int(end, pe_section_alignment))
}

fn pe_next_section_rva(rva u32, size int) u32 {
	virtual_size := if size == 0 { 1 } else { size }
	return u32(align_int(int(rva) + virtual_size, pe_section_alignment))
}

fn pe_emit_call_placeholder(mut text []u8) {
	text << [u8(0xe8), 0, 0, 0, 0]
}

fn pe_patch_rel32(mut text []u8, field_off int, target_off u32, text_rva u32) {
	field_rva := text_rva + u32(field_off)
	disp := i32(int(target_off) - (int(field_rva) + 4))
	pe_put_u32_le(mut text, field_off, u32(disp))
}

fn pe_read_i32_le(data []u8, off int) i32 {
	return i32(u32(data[off]) | (u32(data[off + 1]) << 8) | (u32(data[off + 2]) << 16) | (u32(data[
		off + 3]) << 24))
}

fn pe_put_u32_le(mut data []u8, off int, v u32) {
	data[off] = u8(v)
	data[off + 1] = u8(v >> 8)
	data[off + 2] = u8(v >> 16)
	data[off + 3] = u8(v >> 24)
}

fn pe_put_u64_le(mut data []u8, off int, v u64) {
	pe_put_u32_le(mut data, off, u32(v))
	pe_put_u32_le(mut data, off + 4, u32(v >> 32))
}
