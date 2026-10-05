module token

fn test_operator_properties_are_owned_by_tokens() {
	assert Token.plus.is_infix()
	assert Token.plus.left_binding_power() == .sum
	assert Token.pipe.left_binding_power() == .sum
	assert Token.xor.left_binding_power() == .sum
	assert Token.mul.left_binding_power() == .product
	assert Token.mul.right_binding_power() == .power
	assert Token.amp.left_binding_power() == .product
	assert Token.power.left_binding_power() == .power
	assert Token.power.right_binding_power() == .power
	assert int(Token.power.left_binding_power()) > int(Token.mul.left_binding_power())
	// `<<` `>>` `>>>` share the `product` level with `* / % &`, so they bind
	// tighter than `+ - | ^` at `sum` (V precedence, docs Appendix II).
	assert Token.left_shift.left_binding_power() == .product
	assert int(Token.left_shift.left_binding_power()) > int(Token.plus.left_binding_power())
	assert Token.logical_or.left_binding_power() == .logical_or
	assert Token.logical_or.right_binding_power() == .logical_and
	assert Token.eq.is_comparison()
	assert Token.plus.is_overloadable()
	assert Token.minus.is_prefix()
	assert Token.inc.is_postfix()
	assert Token.right_shift_unsigned_assign.is_assignment()
	assert Token.power_assign.is_assignment()
	assert Token.power.is_overloadable()
	assert !Token.name.is_infix()
	assert !Token.number.is_assignment()
}

fn test_file_position_resolves_file_local_offsets() {
	src := 'line one\nsecond line\nthird\n'
	mut fs := FileSet.new()
	mut f := fs.add_file('x.v', src.len)
	f.index_lines(src)
	// Pos.offset is file-local: offset 0 is the file start, not fs.base.
	start := f.position(new_pos(1, 0))
	assert start.line == 1
	assert start.column == 1
	assert f.line(new_pos(1, 0)) == 1
	// Offset 9 is the start of the second line (`line one\n` is 9 bytes).
	second := f.position(new_pos(1, 9))
	assert second.line == 2
	assert second.column == 1
	assert f.line(new_pos(1, 9)) == 2
}

fn test_reported_column_outside_compact_range_is_ignored() {
	pos := new_span(1, 10, 13)
	wide := pos.with_reported_column(32768)
	assert wide == pos
	assert wide.reported_column() == 0

	typed := pos.with_type_text_id(7)
	assert typed.with_reported_column(32768) == typed
}

fn test_source_file_ids_are_wider_than_u16() {
	file_id := int(max_u16) + 1
	pos := new_pos(file_id, 7)
	span := new_span(file_id, 7, 11)
	assert pos.id == file_id
	assert span.id == file_id
}

fn test_keyword_property_does_not_depend_on_enum_ordinals() {
	assert Token.key_as.is_keyword()
	assert Token.key_unsafe.is_keyword()
	assert Token.key_volatile.is_keyword()
	assert !Token.name.is_keyword()
	assert !Token.lcbr.is_keyword()
}

fn test_a_quick_sum_indexes_the_lines_as_a_digest_does() {
	src := 'module main\n\nfn main() {\n\tprintln(1)\n}\n'
	mut fs := FileSet.new()
	mut with_digest := fs.add_file('a.v', src.len)
	with_digest.index_lines(src)
	mut with_sum := fs.add_file('a.v', src.len)
	with_sum.index_lines_with_quick_sum(src)
	assert with_sum.line_count() == with_digest.line_count()
	for line in 1 .. with_digest.line_count() + 1 {
		assert with_sum.line_start(line) == with_digest.line_start(line)
	}
	assert with_digest.has_source_sha256() && !with_digest.has_source_quick_sum()
	assert with_sum.has_source_quick_sum() && !with_sum.has_source_sha256()
	assert with_sum.source_quick_sum() == quick_sum(src.str, src.len)
	other := src.replace('1', '2')
	assert quick_sum(other.str, other.len) != with_sum.source_quick_sum()
	// Indexing again with a digest drops the sum: a file records one of them.
	with_sum.index_lines(src)
	assert !with_sum.has_source_quick_sum()
}

fn test_line_starts_index_the_lines_as_a_digest_does_without_one() {
	for src in ['module main\n\nfn main() {\n\tprintln(1)\n}\n', '', 'no newline', '\n', '\n\n',
		'a\r\nb\r\nc'] {
		mut fs := FileSet.new()
		mut with_digest := fs.add_file('a.v', src.len)
		with_digest.index_lines(src)
		mut plain := fs.add_file('a.v', src.len)
		plain.index_line_starts(src)
		assert plain.line_count() == src.count('\n') + 1
		assert plain.line_count() == with_digest.line_count()
		for line in 1 .. with_digest.line_count() + 1 {
			assert plain.line_start(line) == with_digest.line_start(line)
		}
		for offset in 0 .. src.len + 1 {
			assert plain.find_line(offset) == with_digest.find_line(offset)
		}
		assert plain.has_source_lines() && with_digest.has_source_lines()
		assert with_digest.has_source_sha256()
		assert !plain.has_source_sha256() && !plain.has_source_quick_sum()
		// Indexing again with a digest records one: a file is indexed one way at a time.
		plain.index_lines(src)
		assert plain.has_source_sha256()
		assert plain.source_sha256() == with_digest.source_sha256()
		plain.index_line_starts(src)
		assert !plain.has_source_sha256()
	}
	mut fs := FileSet.new()
	assert !fs.add_file('a.v', 0).has_source_lines()
	assert !File.unindexed('a.v', 0).has_source_lines()
}

fn test_clone_index_copies_the_lines_and_what_stands_for_the_source() {
	src := '#line 5 "other.zbr"\nA\nB\n'
	mut fs := FileSet.new()
	mut with_digest := fs.add_file('a.v', src.len)
	with_digest.index_lines(src)
	with_digest.add_line_directive(0, 5, 'other.zbr')
	mut with_sum := fs.add_file('b.v', src.len)
	with_sum.index_lines_with_quick_sum(src)
	mut plain := fs.add_file('c.v', src.len)
	plain.index_line_starts(src)
	for file in [with_digest, with_sum, plain] {
		copy := file.clone_index()
		assert copy.name == file.name && copy.name.str != file.name.str
		assert copy.size == file.size
		assert copy.line_count() == file.line_count()
		for line in 1 .. file.line_count() + 1 {
			assert copy.line_start(line) == file.line_start(line)
		}
		assert copy.has_source_lines()
		assert copy.has_source_sha256() == file.has_source_sha256()
		assert copy.source_sha256() == file.source_sha256()
		assert copy.has_source_quick_sum() == file.has_source_quick_sum()
		assert copy.source_quick_sum() == file.source_quick_sum()
		assert copy.has_line_directives() == file.has_line_directives()
	}
	copy_file, copy_line := with_digest.clone_index().logical_line(2)
	assert copy_file == 'other.zbr' && copy_line == 5
}

fn test_source_digest_survives_parser_worker_file_clone() {
	source := 'module main\nfn main() { println(42) }\n'
	mut files := FileSet.new()
	mut original := files.add_file('source.v', source.len)
	original.index_lines(source)
	mut cloned := files.add_file(original.name, original.size)
	assert !cloned.has_source_sha256()
	cloned.set_source_sha256(original.source_sha256())
	assert cloned.has_source_sha256()
	assert cloned.source_sha256() == original.source_sha256()
	// The bootstrap caller can spell the SHA-256 width as a literal too.
	mut literal_width := [32]u8{}
	digest := original.source_sha256()
	for i in 0 .. literal_width.len {
		literal_width[i] = digest[i]
	}
	cloned.set_source_sha256(literal_width)
	assert cloned.source_sha256() == digest
}

fn test_line_directives_remap_the_lines_after_them() {
	src := 'A\n#line 10 "gen.zbr"\nB\nC\n#line 30\nD\n#line 5 "other.zbr"\nE\n'
	mut fs := FileSet.new()
	mut f := fs.add_file('x.v', src.len)
	f.index_lines(src)
	assert !f.has_line_directives()
	f.add_line_directive(src.index('#line 10') or { -1 }, 10, 'gen.zbr')
	f.add_line_directive(src.index('#line 30') or { -1 }, 30, '')
	f.add_line_directive(src.index('#line 5') or { -1 }, 5, 'other.zbr')
	// Scanning a directive again replaces its entry.
	f.add_line_directive(src.index('#line 30') or { -1 }, 30, '')
	assert f.line_directives().len == 3
	a := f.logical_position_at(0)
	assert a.filename == 'x.v' && a.line == 1 && a.column == 1
	// The directive line itself still follows the previous mapping.
	directive := f.logical_position_at(src.index('#line 10') or { -1 })
	assert directive.filename == 'x.v' && directive.line == 2
	b := f.logical_position_at(src.index('B') or { -1 })
	assert b.filename == 'gen.zbr' && b.line == 10 && b.column == 1
	assert f.logical_position_at(src.index('C') or { -1 }).line == 11
	// `#line N` keeps the logical file.
	d := f.logical_position_at(src.index('D') or { -1 })
	assert d.filename == 'gen.zbr' && d.line == 30
	e := f.logical_position_at(src.index('E') or { -1 })
	assert e.filename == 'other.zbr' && e.line == 5
	// The physical positions are unchanged.
	assert f.position_at(src.index('E') or { -1 }).line == 8
	mut copy := fs.add_file('x.v', src.len)
	copy.index_lines(src)
	copy.copy_line_directives(f)
	copy_file, copy_line := copy.logical_line(8)
	assert copy_file == 'other.zbr' && copy_line == 5
}

fn test_line_numbers_after_the_largest_line_directive_do_not_overflow() {
	src := '#line 2147483647
A
B
'
	mut fs := FileSet.new()
	mut f := fs.add_file('x.v', src.len)
	f.index_lines(src)
	f.add_line_directive(0, max_i32, '')
	assert f.logical_position_at(src.index('A') or { -1 }).line == max_i32
	assert f.logical_position_at(src.index('B') or { -1 }).line == max_i32
}

fn parsed_line_directive(args string) string {
	line, file := parse_line_directive(args) or { return 'error: ${err.msg()}' }
	return '${line} ${file}'
}

fn test_parse_line_directive() {
	assert parsed_line_directive(' 42 "src/app.zbr"') == '42 src/app.zbr'
	assert parsed_line_directive("7 'a b.zbr' // generated") == '7 a b.zbr'
	assert parsed_line_directive('3') == '3 '
	assert parsed_line_directive(r'5 "C:\\src\\a \"q\".zbr"') == r'5 C:\src\a "q".zbr'
	assert parsed_line_directive('') == 'error: expected a line number, like `#line 42 "file.v"`'
	assert parsed_line_directive('abc') == 'error: `abc` is not a valid line number'
	assert parsed_line_directive('0') == 'error: line numbers start at 1, not 0'
	assert parsed_line_directive('99999999999') == 'error: line number `99999999999` is too large'
	assert parsed_line_directive('1 a.zbr').starts_with('error: the file name must be a quoted string')
	assert parsed_line_directive('1 "a.zbr') == 'error: unterminated file name string'
	assert parsed_line_directive('1 ""') == 'error: the file name cannot be empty'
	assert parsed_line_directive('1 "a.zbr" xyz') == 'error: unexpected `xyz` after the file name'
}
