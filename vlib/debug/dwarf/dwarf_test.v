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
