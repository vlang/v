module pe

import encoding.binary

fn test_too_small() {
	if _ := new_file([]u8{len: 10}) {
		assert false
	} else {
		assert err.msg().contains('too small')
	}
}

fn test_invalid_signature() {
	mut data := []u8{len: 128}
	data[0] = `M`
	data[1] = `Z`
	// e_lfanew -> 64
	data[0x3C] = 64
	if _ := new_file(data) {
		assert false
	} else {
		assert err.msg().contains('invalid PE signature')
	}
}

fn pe_fixture(machine u16, optional_size int) []u8 {
	mut data := []u8{len: 1024}
	data[0] = `M`
	data[1] = `Z`
	binary.little_endian_put_u32_at(mut data, 64, 0x3c)
	data[64] = `P`
	data[65] = `E`
	binary.little_endian_put_u16_at(mut data, machine, 68)
	binary.little_endian_put_u16_at(mut data, 2, 70)
	binary.little_endian_put_u16_at(mut data, u16(optional_size), 84)
	binary.little_endian_put_u16_at(mut data, if optional_size == 224 {
		u16(0x10b)
	} else {
		u16(0x20b)
	}, 88)
	section_offset := 88 + optional_size
	for i, name in ['.text', 'fullname'] {
		offset := section_offset + i * 40
		for j, b in name.bytes() {
			data[offset + j] = b
		}
		binary.little_endian_put_u32_at(mut data, u32(16 + i), offset + 8)
		binary.little_endian_put_u32_at(mut data, u32(0x1000 + i * 0x1000), offset + 12)
		binary.little_endian_put_u32_at(mut data, 16, offset + 16)
		binary.little_endian_put_u32_at(mut data, u32(512 + i * 16), offset + 20)
		binary.little_endian_put_u32_at(mut data, 0x60000020, offset + 36)
	}
	return data
}

fn test_parse_portable_pe32_and_pe32_plus() {
	for machine in [image_file_machine_i386, image_file_machine_amd64, image_file_machine_arm64] {
		optional_size := if machine == image_file_machine_i386 { 224 } else { 240 }
		f := new_file(pe_fixture(machine, optional_size)) or { panic(err) }
		assert f.machine == machine
		assert f.sections.len == 2
		assert f.sections[1].name == 'fullname'
		s := f.section_by_name('.text') or { panic('missing .text') }
		assert s.virtual_size == 16
		assert s.virtual_address == 0x1000
		assert s.size_of_raw_data == 16
		assert s.pointer_to_raw_data == 512
		assert s.characteristics == 0x60000020
		assert f.section_by_name('.missing') == none
	}
}

fn test_invalid_dos_signature() {
	mut data := pe_fixture(image_file_machine_amd64, 240)
	data[0] = 0
	if _ := new_file(data) {
		assert false, 'a PE image must have a DOS signature'
	}
}

fn test_invalid_large_pe_offsets() {
	for offset in [u32(0x7fffffff), 0x80000000, 0xfffffff0, 0xffffffff] {
		mut data := pe_fixture(image_file_machine_amd64, 240)
		binary.little_endian_put_u32_at(mut data, offset, 0x3c)
		if _ := new_file(data) {
			assert false, 'out-of-range e_lfanew must fail'
		}
	}
}

fn test_truncated_optional_header() {
	data := pe_fixture(image_file_machine_amd64, 240)
	if _ := new_file(data[..100]) {
		assert false, 'truncated optional header must fail'
	}
}

fn test_truncated_section_table() {
	data := pe_fixture(image_file_machine_amd64, 240)
	for size in [328, 367, 368, 407] {
		if _ := new_file(data[..size]) {
			assert false, 'all declared section headers must fit'
		}
	}
	f := new_file(data[..408]) or { panic(err) }
	assert f.sections.len == 2
}

fn test_zero_sections_still_checks_optional_header() {
	mut data := pe_fixture(image_file_machine_amd64, 240)
	binary.little_endian_put_u16_at(mut data, 0, 70)
	if _ := new_file(data[..100]) {
		assert false, 'optional header must fit even without sections'
	}
	f := new_file(data[..328]) or { panic(err) }
	assert f.sections.len == 0
}
