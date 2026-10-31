module c

import strings

fn write_byte_reference(mut g FlatGen, s string) {
	if g.line_start {
		g.write_indent()
	}
	if s.len == 0 {
		if g.indent > 0 {
			g.line_start = false
		}
		return
	}
	unsafe { g.sb.write_ptr(s.str, s.len) }
	g.line_start = s[s.len - 1] == `\n`
}

fn test_single_byte_emission_matches_reference_for_all_bytes_and_indentation() {
	for capacity in [0, 1, 32] {
		for indent in [0, 1, 3] {
			for line_start in [false, true] {
				for value in 0 .. 256 {
					text := [u8(value)].bytestr()
					mut actual := FlatGen{ sb: strings.new_builder(capacity), indent: indent, line_start: line_start }
					mut expected := FlatGen{ sb: strings.new_builder(capacity), indent: indent, line_start: line_start }
					actual.write(text)
					write_byte_reference(mut expected, text)
					assert actual.sb == expected.sb
					assert actual.line_start == expected.line_start
				}
			}
		}
	}
}

fn test_single_byte_emission_preserves_mixed_writes_and_repeated_growth() {
	mut actual := FlatGen{ sb: strings.new_builder(0), indent: 2, line_start: true }
	mut expected := FlatGen{ sb: strings.new_builder(0), indent: 2, line_start: true }
	for _ in 0 .. 128 {
		for text in ['', '(', ')', 'longer generated identifier', '\x00', '\n', '字段', 'a\x00b'] {
			actual.write(text)
			write_byte_reference(mut expected, text)
			assert actual.sb == expected.sb
			assert actual.line_start == expected.line_start
		}
	}
}

fn test_single_byte_emission_keeps_borrowed_bytes_in_existing_capacity() {
	mut g := FlatGen{ sb: strings.new_builder(32) }
	g.sb.write_string('x\n')
	first := unsafe { (&u8(g.sb.data)).vstring_with_len(1) }
	g.write(first)
	assert g.sb == 'x\nx'.bytes()
	assert !g.line_start
	last := unsafe { (&u8(g.sb.data) + 1).vstring_with_len(1) }
	g.write(last)
	assert g.sb == 'x\nx\n'.bytes()
	assert g.line_start
	g.line_start = false
	unsafe { *(&u8(g.sb.data) + g.sb.len) = `z` }
	unused := unsafe { (&u8(g.sb.data) + g.sb.len).vstring_with_len(1) }
	g.write(unused)
	assert g.sb == 'x\nx\nz'.bytes()
	assert !g.line_start
}

fn test_single_byte_emission_unshares_slice_builders() {
	mut backing := []u8{len: 32, init: `q`}
	mut g := FlatGen{ sb: strings.Builder(backing[0..1]) }
	g.write('x')
	assert g.sb == 'qx'.bytes()
	assert backing[1] == `q`
}
