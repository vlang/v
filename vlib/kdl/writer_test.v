module kdl

fn test_writer_basic() {
	doc := parse('(t)node 1 -2 2.5 1e10 "a b" bare #true #null key="v" { child; "x y" (u8)0xff }')!
	assert doc.str() == '(t)node 1 -2 2.5 1E+10 "a b" bare #true #null key=v {\n    child\n    "x y" (u8)255\n}\n'
}

fn test_writer_escapes_and_quoting() {
	mut n := Node{
		name: 'a"b'
	}
	n.arguments << Value{
		data: 'line\nbreak\t\\ "q" \x7f \u202e'
	}
	n.arguments << Value{
		data: ''
	}
	n.arguments << Value{
		data: '123'
	}
	n.arguments << Value{
		data: 'true'
	}
	n.arguments << Value{
		data: '-.5'
	}
	n.arguments << Value{
		ty:   ''
		data: i64(1)
	}
	s := n.str()
	assert s == '"a\\"b" "line\\nbreak\\t\\\\ \\"q\\" \\u{7f} \\u{202e}" "" "123" "true" "-.5" ("")1\n'
	back := parse(s)!
	assert back.nodes[0].equals(n)
}

fn test_writer_floats() {
	assert format_float(1.0) == '1.0'
	assert format_float(-0.5) == '-0.5'
	assert format_float(1e21) == '1E+21'
	assert format_float(1.5e-7) == '1.5E-07'
	assert format_float(f64_inf) == '#inf'
	assert format_float(-f64_inf) == '#-inf'
	assert format_float(f64_nan) == '#nan'
	assert format_float(-0.0) == '-0.0'
	doc := parse('n 1e21 1.5e-7 0.1 100.0')!
	assert parse(doc.str())!.nodes[0].arguments == doc.nodes[0].arguments
}

fn test_writer_escapes_unicode_newlines() {
	doc := parse('n "\\u{85}\\u{2028}\\u{2029}\\u{b}\\u{c}" "\\u{85}"=1')!
	text := doc.str()
	assert text == 'n "\\u{85}\\u{2028}\\u{2029}\\u{b}\\f" "\\u{85}"=1\n'
	assert parse(text)!.equals(doc)
}

fn test_zero_value_is_null_and_round_trips() {
	mut n := Node{
		name: 'n'
	}
	n.arguments << Value{}
	n.properties['k'] = Value{}
	assert n.str() == 'n #null k=#null\n'
	assert parse(n.str())!.nodes[0].equals(n)
}
