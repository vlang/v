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
