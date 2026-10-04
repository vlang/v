module c

import strings

fn test_emission_preserves_empty_writes_indentation_and_line_endings() {
	mut g := FlatGen{
		sb:         strings.new_builder(1)
		line_start: true
		indent:     2
	}
	g.write('')
	g.write('first')
	g.write('\nsecond\n')
	g.writeln('')
	g.writeln('third')
	assert g.sb.str() == '\t\tfirst\nsecond\n\n\t\tthird\n'
	assert g.line_start
}

fn test_emission_borrows_dynamic_and_binary_strings_during_buffer_growth() {
	mut g := FlatGen{
		sb:         strings.new_builder(1)
		line_start: true
	}
	text := 'abc'.repeat(1024)
	g.write(text)
	g.write('a\x00b')
	g.writeln('${text}end')
	assert g.sb.str() == '${text}a\x00b${text}end\n'
	assert g.line_start
}
