module dwarf

fn abbrev_fixture() []u8 {
	return [
		u8(0x01),
		0x11,
		0x01,
		0x25,
		0x0e,
		0x1b,
		0x08,
		0x11,
		0x01,
		0x12,
		0x0f,
		0x10,
		0x17,
		0x00,
		0x00,
		0x02,
		0x2e,
		0x00,
		0x03,
		0x08,
		0x11,
		0x01,
		0x12,
		0x0f,
		0x00,
		0x00,
		0x00,
	]
}

fn info_fixture() []u8 {
	mut b := []u8{}
	// unit_length = 50
	b << [u8(50), 0, 0, 0]
	// version 4
	b << [u8(4), 0]
	// abbrev_offset 0
	b << [u8(0), 0, 0, 0]
	// addr_size 8
	b << u8(8)
	// CU DIE, abbrev code 1
	b << u8(1)
	// producer: strp 0
	b << [u8(0), 0, 0, 0]
	// comp_dir: "testdir"
	b << 'testdir'.bytes()
	b << u8(0)
	// low_pc: addr 0x4000
	b << [u8(0x00), 0x40, 0, 0, 0, 0, 0, 0]
	// high_pc: udata 100
	b << u8(100)
	// stmt_list: sec_offset 0
	b << [u8(0), 0, 0, 0]
	// subprogram DIE, abbrev code 2
	b << u8(2)
	// name: "main"
	b << 'main'.bytes()
	b << u8(0)
	// low_pc: addr 0x4010
	b << [u8(0x10), 0x40, 0, 0, 0, 0, 0, 0]
	// high_pc: udata 0x20
	b << u8(0x20)
	// end of CU children
	b << u8(0)
	// end of top-level DIE list
	b << u8(0)
	return b
}

fn test_parse_abbrev_table() {
	decls := parse_abbrev_table(abbrev_fixture(), 0)!
	assert decls.len == 2
	assert decls[1].tag == tag_compile_unit
	assert decls[1].has_children
	assert decls[1].attrs.len == 5
	assert decls[2].tag == tag_subprogram
	assert !decls[2].has_children
}

fn test_parse_unit() {
	units := parse_info(info_fixture(), abbrev_fixture())!
	assert units.len == 1
	u := units[0]
	assert u.version == 4
	assert u.addr_size == 8
	cu := u.die_by_tag(tag_compile_unit) or { panic('missing CU') }
	assert cu.children.len == 1
	assert cu.string_attr(at_comp_dir) or { '' } == 'testdir'
	low := cu.attr(at_low_pc) or { panic('missing low_pc') }
	assert low.value == AttrValue(u64(0x4000))
	sub := cu.children[0]
	assert sub.tag == tag_subprogram
	assert sub.string_attr(at_name) or { '' } == 'main'
	high := sub.attr(at_high_pc) or { panic('missing high_pc') }
	assert high.value == AttrValue(u64(0x20))
}

fn test_rejects_truncated() {
	if _ := parse_info([u8(1), 2], abbrev_fixture()) {
		assert false, 'expected truncation error'
	} else {
		assert err.msg().contains('truncated')
	}
}

fn test_rejects_64bit() {
	if _ := parse_info([u8(0xff), 0xff, 0xff, 0xff, 0, 0, 0, 0], abbrev_fixture()) {
		assert false, 'expected 64-bit rejection'
	} else {
		assert err.msg().contains('64-bit')
	}
}

fn test_rejects_bad_version() {
	mut b := info_fixture()
	b[4] = 9
	if _ := parse_info(b, abbrev_fixture()) {
		assert false, 'expected version error'
	} else {
		assert err.msg().contains('version')
	}
}

fn test_rejects_unknown_abbrev() {
	mut b := info_fixture()
	// first DIE code 1 -> 9 (undeclared)
	b[11] = 9
	if _ := parse_info(b, abbrev_fixture()) {
		assert false, 'expected abbrev error'
	} else {
		assert err.msg().contains('abbrev code')
	}
}

fn test_rejects_bad_abbrev_offset() {
	if _ := parse_abbrev_table(abbrev_fixture(), 9999) {
		assert false, 'expected offset error'
	} else {
		assert err.msg().contains('out of range')
	}
}

fn test_rejects_truncated_abbrev_leb() {
	if _ := parse_abbrev_table([u8(0x80)], 0) {
		assert false, 'expected truncated LEB error'
	} else {
		assert err.msg().contains('truncated') || err.msg().contains('LEB128')
	}
	mut ov := []u8{}
	for _ in 0 .. 11 {
		ov << u8(0x80)
	}
	ov << u8(0x01)
	if _ := parse_abbrev_table(ov, 0) {
		assert false, 'expected overflowing LEB error'
	} else {
		assert err.msg().contains('overflowing') || err.msg().contains('LEB128')
	}
}

fn abbrev_refaddr_fixture() []u8 {
	return [u8(1), 0x11, 0x01, 0x03, 0x10, 0x00, 0x00, 0x00]
}

fn test_ref_addr_v4_is_4_bytes() {
	mut b := []u8{}
	b << [u8(0), 0, 0, 0]
	b << [u8(4), 0]
	b << [u8(0), 0, 0, 0]
	b << u8(8)
	b << u8(1)
	b << [u8(0x78), 0x56, 0x34, 0x12]
	b << u8(0)
	unit_len := b.len - 4
	b[0] = u8(unit_len)
	units := parse_info(b, abbrev_refaddr_fixture())!
	assert units.len == 1
	assert units[0].dies[0].attrs[0].value == AttrValue(u64(0x12345678))
	// truncated 3-byte ref_addr must fail
	mut t := []u8{}
	t << [u8(0), 0, 0, 0]
	t << [u8(4), 0]
	t << [u8(0), 0, 0, 0]
	t << u8(8)
	t << u8(1)
	t << [u8(0x78), 0x56, 0x34]
	unit_len2 := t.len - 4
	t[0] = u8(unit_len2)
	if _ := parse_info(t, abbrev_refaddr_fixture()) {
		assert false, 'expected truncated ref_addr error'
	} else {
		assert err.msg().contains('truncated')
	}
}

fn test_ref_addr_v2_is_addr_sized() {
	mut b := []u8{}
	b << [u8(0), 0, 0, 0]
	b << [u8(2), 0]
	b << [u8(0), 0, 0, 0]
	b << u8(8)
	b << u8(1)
	b << [u8(0x00), 0x40, 0, 0, 0, 0, 0, 0]
	b << u8(0)
	unit_len := b.len - 4
	b[0] = u8(unit_len)
	units := parse_info(b, abbrev_refaddr_fixture())!
	assert units[0].dies[0].attrs[0].value == AttrValue(u64(0x4000))
}

fn test_rejects_truncated_udata_attr() {
	abbr := [u8(1), 0x11, 0x00, 0x03, 0x0f, 0x00, 0x00, 0x00]
	mut info := []u8{}
	info << [u8(0), 0, 0, 0]
	info << [u8(4), 0]
	info << [u8(0), 0, 0, 0]
	info << u8(8)
	info << u8(1)
	info << u8(0x80)
	unit_len := info.len - 4
	info[0] = u8(unit_len)
	if _ := parse_info(info, abbr) {
		assert false, 'expected truncated LEB error'
	} else {
		assert err.msg().contains('truncated')
	}
}

fn block_abbr_fixture() []u8 {
	return [u8(1), 0x11, 0x00, 0x03, 0x09, 0x00, 0x00, 0x00]
}

fn info_with_block_payload(payload []u8) []u8 {
	mut info := []u8{}
	info << [u8(0), 0, 0, 0]
	info << [u8(4), 0]
	info << [u8(0), 0, 0, 0]
	info << u8(8)
	info << u8(1)
	info << payload
	info << u8(0)
	unit_len := info.len - 4
	info[0] = u8(unit_len)
	info[1] = u8(unit_len >> 8)
	info[2] = u8(unit_len >> 16)
	info[3] = u8(unit_len >> 24)
	return info
}

fn test_block_valid_two_bytes() {
	units := parse_info(info_with_block_payload([u8(2), 0xaa, 0xbb]),
		block_abbr_fixture())!
	assert units.len == 1
	val := units[0].dies[0].attrs[0].value
	if val is []u8 {
		assert val == [u8(0xaa), 0xbb]
	} else {
		assert false, 'expected block bytes'
	}
}

fn test_block_truncated_returns_error() {
	mut info := []u8{}
	info << [u8(0), 0, 0, 0]
	info << [u8(4), 0]
	info << [u8(0), 0, 0, 0]
	info << u8(8)
	info << u8(1)
	info << u8(5)
	info << u8(0xaa)
	info << u8(0xbb)
	unit_len := info.len - 4
	info[0] = u8(unit_len)
	if _ := parse_info(info, block_abbr_fixture()) {
		assert false, 'expected truncated block error'
	} else {
		assert err.msg().contains('truncated')
	}
}

fn test_block_wrapping_length_returns_error() {
	// Ten-byte ULEB128 encoding of u64 maximum; u64(nn) + v wraps.
	mut payload := []u8{}
	for _ in 0 .. 9 {
		payload << u8(0xff)
	}
	payload << u8(0x01)
	mut info := []u8{}
	info << [u8(0), 0, 0, 0]
	info << [u8(4), 0]
	info << [u8(0), 0, 0, 0]
	info << u8(8)
	info << u8(1)
	info << payload
	unit_len := info.len - 4
	info[0] = u8(unit_len)
	info[1] = u8(unit_len >> 8)
	info[2] = u8(unit_len >> 16)
	info[3] = u8(unit_len >> 24)
	if _ := parse_info(info, block_abbr_fixture()) {
		assert false, 'expected truncated block error'
	} else {
		assert err.msg().contains('truncated')
	}
}
