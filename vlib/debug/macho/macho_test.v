module macho

import encoding.binary

fn test_too_small() {
	if _ := new_file([]u8{len: 10}) {
		assert false
	} else {
		assert err.msg().contains('too small')
	}
}

fn test_invalid_magic() {
	mut data := []u8{len: 32}
	data[0] = 0xDE
	data[1] = 0xAD
	if _ := new_file(data) {
		assert false
	} else {
		assert err.msg().contains('not a 64-bit Mach-O')
	}
}

fn put_u32(mut data []u8, value u32, offset int, little bool) {
	if little {
		binary.little_endian_put_u32_at(mut data, value, offset)
	} else {
		binary.big_endian_put_u32_at(mut data, value, offset)
	}
}

fn put_u64(mut data []u8, value u64, offset int, little bool) {
	if little {
		binary.little_endian_put_u64_at(mut data, value, offset)
	} else {
		binary.big_endian_put_u64_at(mut data, value, offset)
	}
}

fn macho_fixture(little bool) []u8 {
	mut data := []u8{len: 200}
	put_u32(mut data, magic_64, 0, little)
	put_u32(mut data, 0x01000007, 4, little)
	put_u32(mut data, 3, 8, little)
	put_u32(mut data, 1, 12, little)
	put_u32(mut data, 2, 16, little)
	put_u32(mut data, 160, 20, little)
	put_u32(mut data, 0x2000, 24, little)
	// An unknown command precedes an unnamed object-file segment.
	put_u32(mut data, 0x7ffffffe, 32, little)
	put_u32(mut data, 8, 36, little)
	put_u32(mut data, 0x19, 40, little)
	put_u32(mut data, 152, 44, little)
	put_u32(mut data, 1, 104, little)
	for i, b in '__text'.bytes() {
		data[112 + i] = b
	}
	for i, b in '__TEXT'.bytes() {
		data[128 + i] = b
	}
	put_u64(mut data, 0x123456789abcdef0, 144, little)
	put_u64(mut data, 8, 152, little)
	put_u32(mut data, 192, 160, little)
	return data
}

fn test_parse_both_byte_orders() {
	for little in [true, false] {
		f := new_file(macho_fixture(little)) or { panic(err) }
		assert f.header.magic == magic_64
		assert f.header.cpu_type == 0x01000007
		assert f.header.cpu_subtype == 3
		assert f.header.file_type == 1
		assert f.header.num_cmds == 2
		assert f.header.size_cmds == 160
		assert f.header.flags == 0x2000
		assert f.sections.len == 1
		assert f.sections[0].name == '__text'
		assert f.sections[0].segname == '__TEXT'
		assert f.sections[0].addr == 0x123456789abcdef0
		assert f.sections[0].size == 8
		assert f.sections[0].offset == 192
	}
}

fn test_section_segment_name_is_not_command_segment_name() {
	f := new_file(macho_fixture(false)) or { panic(err) }
	assert f.sections[0].segname == '__TEXT'
}

fn test_short_segment_command() {
	mut data := []u8{len: 40}
	put_u32(mut data, magic_64, 0, false)
	put_u32(mut data, 1, 16, false)
	put_u32(mut data, 8, 20, false)
	put_u32(mut data, 0x19, 32, false)
	put_u32(mut data, 8, 36, false)
	if _ := new_file(data) {
		assert false, 'LC_SEGMENT_64 needs its full fixed header'
	}
}

fn test_invalid_load_command_sizes_and_counts() {
	for size in [u32(0), 1, 7, 9, 160, 0x80000000, 0xffffffff] {
		mut data := macho_fixture(false)
		put_u32(mut data, size, 44, false)
		if _ := new_file(data) {
			assert false, 'load commands must fit the declared command area'
		}
	}
	for count in [u32(0), 1, 3, 0x80000000, 0xffffffff] {
		mut data := macho_fixture(false)
		put_u32(mut data, count, 16, false)
		if _ := new_file(data) {
			assert false, 'command count and command area must agree'
		}
	}
}

fn test_invalid_command_area_sizes() {
	for size in [u32(0), 8, 159, 168, 0x80000000, 0xffffffff] {
		mut data := macho_fixture(false)
		put_u32(mut data, size, 20, false)
		if _ := new_file(data) {
			assert false, 'command area must fit and contain exactly the commands'
		}
	}
}

fn test_sections_must_fit_own_command() {
	for count in [u32(2), 0x80000000, 0xffffffff] {
		mut data := macho_fixture(false)
		put_u32(mut data, count, 104, false)
		if _ := new_file(data) {
			assert false, 'section count must fit the segment command'
		}
	}
	mut data := macho_fixture(false)
	put_u32(mut data, 72, 44, false)
	if _ := new_file(data) {
		assert false, 'trailing file bytes cannot supply missing section headers'
	}
}

fn test_truncated_command_area() {
	data := macho_fixture(false)
	for size in [32, 39, 40, 111, 191] {
		if _ := new_file(data[..size]) {
			assert false, 'truncated commands must return an error'
		}
	}
}
