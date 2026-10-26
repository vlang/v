module x64

import os

fn test_elf_object_has_x86_64_relocatable_header() {
	mut object := ElfObject.new()
	object.text_data << [u8(0xc3)]
	object.add_symbol('main', 0, true, 1)
	path := os.join_path(os.vtmp_dir(), 'v_x64_elf_${os.getpid()}.o')
	defer {
		os.rm(path) or {}
	}
	object.write(path)
	data := os.read_file_array[u8](path)
	assert data.len >= 64
	assert data[..6] == [u8(0x7f), 0x45, 0x4c, 0x46, 0x02, 0x01]
	// e_type = ET_REL, e_machine = EM_X86_64
	assert data[16] == 1 && data[17] == 0
	assert data[18] == 62 && data[19] == 0
	assert data[60] == 9 && data[61] == 0
	assert data[62] == 7 && data[63] == 0
}
