module x64

import os

fn temp_object_path(name string) string {
	return os.join_path(os.vtmp_dir(), 'v2_x64_${name}_${os.getpid()}.o')
}

fn read_u32_le(data []u8, off int) u32 {
	return u32(data[off]) | (u32(data[off + 1]) << 8) | (u32(data[off + 2]) << 16) | (u32(data[
		off + 3]) << 24)
}

fn read_string(data []u8, off int) string {
	mut end := off
	for end < data.len && data[end] != 0 {
		end++
	}
	return data[off..end].bytestr()
}

fn test_elf_writer_emits_x64_relocatable_object() {
	path := temp_object_path('elf')
	defer {
		os.rm(path) or {}
	}

	mut obj := ElfObject.new()
	obj.text_data << u8(0xc3)
	obj.add_symbol('main', 0, true, 1)
	obj.write(path)

	data := os.read_bytes(path) or { panic(err) }
	assert data.len > 64
	assert data[0] == 0x7f
	assert data[1] == `E`
	assert data[2] == `L`
	assert data[3] == `F`
	assert data[4] == elfclass64
	assert data[16] == u8(et_rel)
	assert data[18] == u8(em_x86_64)
}

fn test_macho_writer_emits_x64_relocatable_object() {
	path := temp_object_path('macho')
	defer {
		os.rm(path) or {}
	}

	mut obj := MachOObject.new()
	obj.text_data << u8(0xc3)
	obj.add_symbol('_main', 0, true, 1)
	obj.write(path)

	data := os.read_bytes(path) or { panic(err) }
	assert data.len > 64
	assert data[0] == 0xcf
	assert data[1] == 0xfa
	assert data[2] == 0xed
	assert data[3] == 0xfe
	assert data[4] == 0x07
	assert data[5] == 0x00
	assert data[6] == 0x00
	assert data[7] == 0x01
	assert data[8] == 0x03
	assert data[12] == u8(macho_mh_object)
	assert data[16] == 2
	assert data.bytestr().contains('__text')
	assert data.bytestr().contains('__const')
	assert data.bytestr().contains('__data')
	assert data.bytestr().contains('_main')
}

fn test_macho_writer_records_x64_text_relocations() {
	mut obj := MachOObject.new()
	call_sym := obj.add_undefined('_calloc')
	data_sym := obj.add_symbol('L_str_0', 0, false, 2)
	obj.add_reloc(1, call_sym, x86_64_reloc_branch, true, 2)
	obj.add_reloc(8, data_sym, x86_64_reloc_signed, true, 2)

	assert obj.relocs.len == 2
	assert obj.relocs[0].addr == 1
	assert obj.relocs[0].sym_idx == call_sym
	assert obj.relocs[0].type_ == x86_64_reloc_branch
	assert obj.relocs[0].pcrel
	assert obj.relocs[0].length == 2
	assert obj.relocs[1].addr == 8
	assert obj.relocs[1].sym_idx == data_sym
	assert obj.relocs[1].type_ == x86_64_reloc_signed
	assert obj.relocs[1].pcrel
	assert obj.relocs[1].length == 2
}

fn test_macho_object_format_prefixes_external_symbols() {
	assert ObjectFormat.macho.symbol_name('main') == '_main'
	assert ObjectFormat.macho.symbol_name('calloc') == '_calloc'
	assert ObjectFormat.macho.symbol_name('_main') == '__main'
	assert ObjectFormat.macho.symbol_name('__stdoutp') == '___stdoutp'
	assert ObjectFormat.macho.symbol_name('__stderrp') == '___stderrp'
	assert ObjectFormat.macho.symbol_name('__stdinp') == '___stdinp'
	assert ObjectFormat.macho.symbol_name('__error') == '___error'
	assert ObjectFormat.macho.symbol_name('L_str_0') == 'L_str_0'
	assert ObjectFormat.elf.symbol_name('main') == 'main'
	assert ObjectFormat.elf.symbol_name('calloc') == 'calloc'
	assert ObjectFormat.elf.symbol_name('__stdoutp') == '__stdoutp'
}

fn test_elf_writer_reuses_undefined_symbol_when_it_is_defined_later() {
	mut obj := ElfObject.new()
	undef_idx := obj.add_undefined('app_global')
	def_idx := obj.add_symbol('app_global', 16, false, 2)

	assert def_idx == undef_idx
	assert obj.symbols[def_idx].name == 'app_global'
	assert obj.symbols[def_idx].shndx == 2
	assert obj.symbols[def_idx].value == 16
}

fn test_elf_writer_reuses_defined_symbol_when_it_is_referenced_later() {
	mut obj := ElfObject.new()
	def_idx := obj.add_symbol('app_global', 16, false, 2)
	undef_idx := obj.add_undefined('app_global')

	assert undef_idx == def_idx
	assert obj.symbols[undef_idx].name == 'app_global'
	assert obj.symbols[undef_idx].shndx == 2
	assert obj.symbols[undef_idx].value == 16
}

fn test_macho_writer_reuses_undefined_symbol_when_it_is_defined_later() {
	mut obj := MachOObject.new()
	undef_idx := obj.add_undefined('_app_global')
	def_idx := obj.add_symbol('_app_global', 24, true, 3)

	assert def_idx == undef_idx
	assert obj.symbols[def_idx].name == '_app_global'
	assert obj.symbols[def_idx].sect == 3
	assert obj.symbols[def_idx].value == 24
}

fn test_macho_writer_serializes_text_relocations_and_symbols() {
	path := temp_object_path('macho_relocs')
	defer {
		os.rm(path) or {}
	}

	mut obj := MachOObject.new()
	obj.text_data << [u8(0xe8), 0, 0, 0, 0, 0x48, 0x8d, 0x05, 0, 0, 0, 0]
	call_name := ObjectFormat.macho.symbol_name('__stdoutp')
	local_name := ObjectFormat.macho.symbol_name('L_str_0')
	call_sym := obj.add_undefined(call_name)
	data_sym := obj.add_symbol(local_name, 0, false, 2)
	obj.rodata << 'hello'.bytes()
	obj.rodata << 0
	obj.add_reloc(1, call_sym, x86_64_reloc_branch, true, 2)
	obj.add_reloc(8, data_sym, x86_64_reloc_signed, true, 2)
	obj.write(path)

	data := os.read_bytes(path) or { panic(err) }
	seg_cmd_off := 32
	text_section_off := seg_cmd_off + 72
	reloc_off := int(read_u32_le(data, text_section_off + 56))
	nreloc := int(read_u32_le(data, text_section_off + 60))
	assert nreloc == 2

	reloc0_addr := read_u32_le(data, reloc_off)
	reloc0_info := read_u32_le(data, reloc_off + 4)
	reloc1_addr := read_u32_le(data, reloc_off + 8)
	reloc1_info := read_u32_le(data, reloc_off + 12)
	assert reloc0_addr == 1
	assert reloc0_info & 0x00ff_ffff == u32(call_sym)
	assert ((reloc0_info >> 24) & 1) == 1
	assert ((reloc0_info >> 25) & 3) == 2
	assert ((reloc0_info >> 27) & 1) == 1
	assert (reloc0_info >> 28) == u32(x86_64_reloc_branch)
	assert reloc1_addr == 8
	assert reloc1_info & 0x00ff_ffff == u32(data_sym)
	assert ((reloc1_info >> 24) & 1) == 1
	assert ((reloc1_info >> 25) & 3) == 2
	assert ((reloc1_info >> 27) & 1) == 1
	assert (reloc1_info >> 28) == u32(x86_64_reloc_signed)

	symtab_cmd_off := seg_cmd_off + 72 + (80 * 3)
	assert read_u32_le(data, symtab_cmd_off) == u32(macho_lc_symtab)
	sym_off := int(read_u32_le(data, symtab_cmd_off + 8))
	nsyms := int(read_u32_le(data, symtab_cmd_off + 12))
	str_off := int(read_u32_le(data, symtab_cmd_off + 16))
	str_size := int(read_u32_le(data, symtab_cmd_off + 20))
	assert nsyms == 2
	assert str_size > 1

	call_name_off := int(read_u32_le(data, sym_off))
	local_name_off := int(read_u32_le(data, sym_off + 16))
	assert read_string(data, str_off + call_name_off) == call_name
	assert read_string(data, str_off + local_name_off) == local_name
	assert data[sym_off + 4] == 0x01
	assert data[sym_off + 5] == 0
	assert data[sym_off + 16 + 4] == 0x0e
	assert data[sym_off + 16 + 5] == 2
}
