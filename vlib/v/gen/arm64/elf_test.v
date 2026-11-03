module arm64

import os
import encoding.binary

fn test_elf_layout_symbol_names_and_relocations() {
	path := os.join_path(os.vtmp_dir(), 'arm64_elf_${os.getpid()}.o')
	defer { os.rm(path) or {} }
	mut object := MachOObject.new()
	object.text_data = [u8(0), 0, 0, 0x94]
	object.add_symbol('_local', 0, false, 1)
	errno_symbol := object.add_undefined('___error')
	object.add_reloc(0, errno_symbol, arm64_reloc_branch26, true)
	object.write_elf(path)!
	data := os.read_bytes(path)!
	assert data[..4] == [u8(0x7f), `E`, `L`, `F`]
	assert binary.little_endian_u16(data[18..20]) == 183
	section_headers := int(binary.little_endian_u64(data[40..48]))
	strtab_header := section_headers + 5 * 64
	strtab_offset := int(binary.little_endian_u64(data[strtab_header + 24..strtab_header + 32]))
	strtab_size := int(binary.little_endian_u64(data[strtab_header + 32..strtab_header + 40]))
	assert data[strtab_offset..strtab_offset + strtab_size].bytestr().contains('__errno_location')
	rela_header := section_headers + 6 * 64
	rela_offset := int(binary.little_endian_u64(data[rela_header + 24..rela_header + 32]))
	assert rela_offset >= strtab_offset + strtab_size
	assert binary.little_endian_u64(data[rela_offset + 8..rela_offset + 16]) == ((u64(2) << 32) | 283)
	// One local symbol follows the required null entry; the external symbol starts at index two.
	symtab_header := section_headers + 4 * 64
	assert binary.little_endian_u32(data[symtab_header + 44..symtab_header + 48]) == 2
}

fn test_elf_rejects_untranslated_relocation() {
	path := os.join_path(os.vtmp_dir(), 'arm64_elf_bad_${os.getpid()}.o')
	defer { os.rm(path) or {} }
	mut object := MachOObject.new()
	symbol := object.add_undefined('_external')
	object.add_reloc(0, symbol, 99, false)
	mut rejected := false
	object.write_elf(path) or { rejected = true }
	assert rejected
	assert !os.exists(path)
}

fn test_elf_section_relative_symbols_preserve_binding_and_relocation_indices() {
	path := os.join_path(os.vtmp_dir(), 'arm64_elf_sections_${os.getpid()}.o')
	defer { os.rm(path) or {} }
	mut object := MachOObject.new()
	object.text_data = []u8{len: 16}
	object.str_data = []u8{len: 12}
	object.data_data = []u8{len: 16}
	// gen_post_pass has added the Mach-O section bases to these stored addresses.
	global_data := object.add_symbol('_global_data', 40, true, 3)
	object.add_symbol('L_string', 20, false, 2)
	object.add_symbol('_local_data', 32, false, 3)
	object.add_symbol('_global_string', 24, true, 2)
	object.add_reloc(0, global_data, arm64_reloc_branch26, true)
	object.write_elf(path)!
	data := os.read_bytes(path)!
	section_headers := int(binary.little_endian_u64(data[40..48]))
	symtab_header := section_headers + 4 * 64
	symtab_offset := int(binary.little_endian_u64(data[symtab_header + 24..symtab_header + 32]))
	assert binary.little_endian_u32(data[symtab_header + 44..symtab_header + 48]) == 3
	for i, expected_value in [u64(4), 0, 8, 8] {
		symbol := symtab_offset + (i + 1) * 24
		assert data[symbol + 4] >> 4 == if i < 2 { stb_local } else { stb_global }
		assert binary.little_endian_u16(data[symbol + 6..symbol + 8]) == [u16(3), 2, 2, 3][i]
		assert binary.little_endian_u64(data[symbol + 8..symbol + 16]) == expected_value
	}
	rela_header := section_headers + 6 * 64
	rela_offset := int(binary.little_endian_u64(data[rela_header + 24..rela_header + 32]))
	assert binary.little_endian_u64(data[rela_offset + 8..rela_offset + 16]) == ((u64(3) << 32) | 283)
}

fn test_elf_propagates_file_write_errors() {
	path := os.join_path(os.vtmp_dir(), 'arm64_elf_missing_${os.getpid()}', 'object.o')
	assert !os.exists(os.dir(path))
	mut object := MachOObject.new()
	mut rejected := false
	object.write_elf(path) or { rejected = true }
	assert rejected
	assert !os.exists(path)
}
