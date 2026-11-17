module dwarf

fn line_fixture() []u8 {
	mut b := []u8{}
	// unit_length = 60 (patched below by construction)
	b << [u8(60), 0, 0, 0]
	// version 4
	b << [u8(4), 0]
	// header_length = 34
	b << [u8(34), 0, 0, 0]
	// min_inst_len, max_ops, default_is_stmt, line_base(-5), range(14), opcode_base(13)
	b << [u8(1), 1, 1, 0xfb, 14, 13]
	// std opcode lengths x12
	for _ in 0 .. 12 {
		b << u8(0)
	}
	// dirs: "src", end
	b << 'src'.bytes()
	b << [u8(0), 0]
	// files: "main.c", dir 1, time 0, len 0, end
	b << 'main.c'.bytes()
	b << [u8(0), 1, 0, 0, 0]
	// program: set_address 0x4000
	b << [u8(0), 9, 2, 0x00, 0x40, 0, 0, 0, 0, 0, 0]
	// advance_line +9
	b << [u8(3), 9]
	// copy
	b << u8(1)
	// advance_pc +4, copy
	b << [u8(2), 4, 1]
	// end_sequence
	b << [u8(0), 1, 1]
	return b
}

fn test_line_table() {
	t := parse_line_table(line_fixture(), 8, [], [])!
	assert t.dirs == ['src']
	assert t.files.len == 1
	assert t.files[0].name == 'main.c'
	assert t.files[0].dir == 'src'
	assert t.rows.len == 3
	assert t.rows[0].address == 0x4000
	assert t.rows[0].file == 'main.c'
	assert t.rows[0].line == 10
	assert t.rows[0].is_stmt
	assert t.rows[1].address == 0x4004
	assert t.rows[1].line == 10
	assert t.rows[2].end_sequence
}

fn test_line_tables_plural() {
	mut both := line_fixture()
	both << line_fixture()
	ts := parse_line_tables(both, 8, [], [])!
	assert ts.len == 2
	assert ts[0].rows.len == 3
	assert ts[1].rows[0].line == 10
}

fn test_line_bad_version() {
	mut b := line_fixture()
	b[4] = 9
	if _ := parse_line_table(b, 8, [], []) {
		assert false, 'expected version error'
	} else {
		assert err.msg().contains('version')
	}
}

fn test_line_truncated() {
	if _ := parse_line_table([u8(1), 2], 8, [], []) {
		assert false, 'expected truncation error'
	} else {
		assert err.msg().contains('truncated')
	}
}

fn test_ranges() {
	mut b := []u8{}
	// (0x100, 0x200)
	b << [u8(0), 1, 0, 0, 0, 0, 0, 0]
	b << [u8(0), 2, 0, 0, 0, 0, 0, 0]
	// base selection (all ones, 0x4000)
	b << [u8(0xff), 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]
	b << [u8(0), 0x40, 0, 0, 0, 0, 0, 0]
	// terminator
	b << [u8(0), 0, 0, 0, 0, 0, 0, 0]
	b << [u8(0), 0, 0, 0, 0, 0, 0, 0]
	rs := parse_ranges(b, 8)!
	assert rs.len == 2
	assert rs[0].begin == 0x100 && rs[0].end == 0x200
	assert rs[1].begin == 0xffffffffffffffff
	if _ := parse_ranges([u8(1), 2, 3], 8) {
		assert false, 'expected truncation error'
	} else {
		assert err.msg().contains('truncated')
	}
}

fn test_loc_expr() {
	// fbreg -8 ; addr 0x4000 ; plus_uconst 5 ; lit0 ; plus
	data := [u8(0x91), 0x78, 0x03, 0x00, 0x40, 0, 0, 0, 0, 0, 0, 0x23, 0x05, 0x30, 0x22]
	ops := parse_loc_expr(data, 8)!
	assert ops.len == 5
	assert ops[0].op == dw_op_fbreg
	assert ops[0].operands == [u64(0xfffffffffffffff8)]
	assert ops[1].op == dw_op_addr
	assert ops[1].operands == [u64(0x4000)]
	assert ops[2].operands == [u64(5)]
	assert ops[3].op == 0x30
	assert ops[4].op == dw_op_plus
	if _ := parse_loc_expr([u8(0xff)], 8) {
		assert false, 'expected unknown op error'
	} else {
		assert err.msg().contains('unsupported location op')
	}
}

fn test_line_truncated_header_params() {
	mut c := []u8{}
	c << [u8(0), 0, 0, 0]
	c << [u8(4), 0]
	c << [u8(2), 0, 0, 0]
	c << [u8(1), 1]
	unit_len := c.len - 4
	c[0] = u8(unit_len)
	if _ := parse_line_table(c, 8, [], []) {
		assert false, 'expected truncated params error'
	} else {
		assert err.msg().contains('truncated')
	}
}

fn test_line_truncated_extended_leb() {
	mut b := []u8{}
	b << [u8(0), 0, 0, 0]
	b << [u8(4), 0]
	header_len_pos := b.len
	b << [u8(0), 0, 0, 0]
	header_start := b.len
	b << [u8(1), 1, 1, 0xfb, 14, 13]
	for _ in 0 .. 12 {
		b << u8(0)
	}
	b << 'src'.bytes()
	b << [u8(0), 0]
	b << 'main.c'.bytes()
	b << [u8(0), 1, 0, 0, 0]
	header_len := b.len - header_start
	b[header_len_pos] = u8(header_len)
	b[header_len_pos + 1] = u8(header_len >> 8)
	b[header_len_pos + 2] = u8(header_len >> 16)
	b[header_len_pos + 3] = u8(header_len >> 24)
	b << [u8(0), 0x80]
	unit_len := b.len - 4
	b[0] = u8(unit_len)
	if _ := parse_line_table(b, 8, [], []) {
		assert false, 'expected truncated extended error'
	} else {
		assert err.msg().contains('truncated')
	}
}

fn test_line_rejects_multi_op() {
	mut b := line_fixture()
	// max_ops is byte 11: min(10)=1, max(11)=1 -> set to 2
	b[11] = 2
	// unit_length stays valid (same total length)
	if _ := parse_line_table(b, 8, [], []) {
		assert false, 'expected multi-op rejection'
	} else {
		assert err.msg().contains('multi-op')
	}
}

fn test_line_define_file() {
	mut b := []u8{}
	b << [u8(0), 0, 0, 0]
	b << [u8(4), 0]
	header_len_pos := b.len
	b << [u8(0), 0, 0, 0]
	header_start := b.len
	b << [u8(1), 1, 1, 0xfb, 14, 13]
	for _ in 0 .. 12 {
		b << u8(0)
	}
	b << 'src'.bytes()
	b << [u8(0), 0]
	b << 'main.c'.bytes()
	b << [u8(0), 1, 0, 0, 0]
	header_len := b.len - header_start
	b[header_len_pos] = u8(header_len)
	b[header_len_pos + 1] = u8(header_len >> 8)
	b[header_len_pos + 2] = u8(header_len >> 16)
	b[header_len_pos + 3] = u8(header_len >> 24)
	b << [u8(0), 9, 2, 0x00, 0x40, 0, 0, 0, 0, 0, 0]
	b << [u8(0), 16, 3]
	b << 'generated.c'.bytes()
	b << u8(0)
	b << u8(1)
	b << u8(0)
	b << u8(0)
	b << [u8(4), 2]
	b << u8(1)
	b << [u8(0), 1, 1]
	unit_len := b.len - 4
	b[0] = u8(unit_len)
	b[1] = u8(unit_len >> 8)
	b[2] = u8(unit_len >> 16)
	b[3] = u8(unit_len >> 24)
	t := parse_line_table(b, 8, [], [])!
	assert t.rows.len == 2
	assert t.rows[0].file == 'generated.c'
}

fn v5_zero_based_fixture() []u8 {
	mut b := []u8{}
	b << [u8(0), 0, 0, 0]
	b << [u8(5), 0]
	b << [u8(8), 0]
	header_len_pos := b.len
	b << [u8(0), 0, 0, 0]
	header_start := b.len
	b << [u8(1), 1, 1, 0xfb, 14, 13]
	for _ in 0 .. 12 {
		b << u8(0)
	}
	b << u8(1)
	b << u8(1)
	b << u8(0x08)
	b << u8(2)
	b << 'd0'.bytes()
	b << u8(0)
	b << 'd1'.bytes()
	b << u8(0)
	b << u8(2)
	b << u8(1)
	b << u8(0x08)
	b << u8(2)
	b << u8(0x0f)
	b << u8(2)
	b << 'a.c'.bytes()
	b << u8(0)
	b << u8(0)
	b << 'b.c'.bytes()
	b << u8(0)
	b << u8(1)
	header_len := b.len - header_start
	b[header_len_pos] = u8(header_len)
	b[header_len_pos + 1] = u8(header_len >> 8)
	b[header_len_pos + 2] = u8(header_len >> 16)
	b[header_len_pos + 3] = u8(header_len >> 24)
	b << [u8(0), 9, 2, 0x00, 0x40, 0, 0, 0, 0, 0, 0]
	b << [u8(4), 0]
	b << u8(1)
	b << [u8(4), 1]
	b << u8(1)
	b << [u8(0), 1, 1]
	unit_len := b.len - 4
	b[0] = u8(unit_len)
	b[1] = u8(unit_len >> 8)
	b[2] = u8(unit_len >> 16)
	b[3] = u8(unit_len >> 24)
	return b
}

fn test_line_v5_zero_based() {
	t := parse_line_table(v5_zero_based_fixture(), 8, [], [])!
	assert t.dirs == ['d0', 'd1']
	assert t.files.len == 2
	assert t.files[0].name == 'a.c'
	assert t.files[0].dir == 'd0'
	assert t.files[1].name == 'b.c'
	assert t.files[1].dir == 'd1'
	assert t.rows.len == 3
	assert t.rows[0].file == 'a.c'
	assert t.rows[1].file == 'b.c'
}

fn test_line_v5_bad_strp_offset() {
	mut d := []u8{}
	d << [u8(0), 0, 0, 0]
	d << [u8(5), 0]
	d << [u8(8), 0]
	hlpos := d.len
	d << [u8(0), 0, 0, 0]
	hstart := d.len
	d << [u8(1), 1, 1, 0xfb, 14, 13]
	for _ in 0 .. 12 {
		d << u8(0)
	}
	d << u8(1)
	d << u8(1)
	d << u8(0x1f)
	d << u8(1)
	d << [u8(0xff), 0xff, 0xff, 0xff]
	hlen := d.len - hstart
	d[hlpos] = u8(hlen)
	d[hlpos + 1] = u8(hlen >> 8)
	d[hlpos + 2] = u8(hlen >> 16)
	d[hlpos + 3] = u8(hlen >> 24)
	d << [u8(0), 1, 1]
	ulen := d.len - 4
	d[0] = u8(ulen)
	d[1] = u8(ulen >> 8)
	d[2] = u8(ulen >> 16)
	d[3] = u8(ulen >> 24)
	if _ := parse_line_table(d, 8, [], []) {
		assert false, 'expected bad strp offset error'
	} else {
		assert err.msg().contains('strp')
	}
}

fn test_loc_truncated_leb() {
	if _ := parse_loc_expr([u8(0x91)], 8) {
		assert false, 'expected truncated fbreg error'
	} else {
		assert err.msg().contains('truncated')
	}
	if _ := parse_loc_expr([u8(0x10)], 8) {
		assert false, 'expected truncated constu error'
	} else {
		assert err.msg().contains('truncated')
	}
	if _ := parse_loc_expr([u8(0x9e), 0x80], 8) {
		assert false, 'expected truncated implicit error'
	} else {
		assert err.msg().contains('truncated')
	}
}

fn test_loc_call_ref_is_4_bytes() {
	ops := parse_loc_expr([u8(0x9a), 1, 2, 3, 4], 8)!
	assert ops.len == 1
	assert ops[0].op == dw_op_call_ref
	assert ops[0].operands == [u64(0x04030201)]
	if _ := parse_loc_expr([u8(0x9a), 1, 2, 3], 8) {
		assert false, 'expected truncated call_ref error'
	} else {
		assert err.msg().contains('truncated')
	}
}
