module kdl

fn test_basic_node() {
	doc := parse('node 1 "two" three key=#true')!
	assert doc.nodes.len == 1
	n := doc.nodes[0]
	assert n.name == 'node'
	assert n.ty == none
	assert n.arguments.len == 3
	assert n.arguments[0].data as i64 == 1
	assert n.arguments[1].data as string == 'two'
	assert n.arguments[2].data as string == 'three'
	assert n.properties['key'].data as bool == true
}

fn test_accessors() {
	doc := parse('server host="localhost" port=8080 { tls #true; route "/a"; route "/b" }')!
	s := doc.get('server')?
	assert s.prop('host').as_string()? == 'localhost'
	assert s.prop('port').as_int()? == 8080
	assert s.prop('port').as_f64()? == 8080.0
	assert s.prop('missing').is_null()
	assert s.prop('missing').as_int() == none
	assert s.prop('missing').as_int() or { 42 } == 42
	assert s.arg(0).is_null()
	assert s.arg(-1).is_null()
	assert s.child('tls')?.arg(0).as_bool()? == true
	assert s.child('tls')?.arg(1).as_bool() == none
	assert s.children_named('route').len == 2
	assert s.child('nope') == none
	assert doc.get('nope') == none
	assert doc.nodes_named('server').len == 1
}

fn test_numbers() {
	doc := parse('n 42 -7 +3 1_000 0xFF -0o17 0b1010 1.5 -2.5e3 1e-2 0.1_5 #inf #-inf #nan')!
	a := doc.nodes[0].arguments
	assert a[0].data as i64 == 42
	assert a[1].data as i64 == -7
	assert a[2].data as i64 == 3
	assert a[3].data as i64 == 1000
	assert a[4].data as i64 == 255
	assert a[5].data as i64 == -15
	assert a[6].data as i64 == 10
	assert a[7].data as f64 == 1.5
	assert a[8].data as f64 == -2500.0
	assert a[9].data as f64 == 0.01
	assert a[10].data as f64 == 0.15
	assert a[11].data as f64 == f64_inf
	assert a[12].data as f64 == -f64_inf
	nan := a[13].data as f64
	assert nan != nan
}

fn test_integer_bounds() {
	doc := parse('n 9223372036854775807 -9223372036854775808 9223372036854775808 -9223372036854775809 0xABCDEF0123456789ABCDEF')!
	a := doc.nodes[0].arguments
	assert a[0].data as i64 == max_i64
	assert a[1].data as i64 == min_i64
	assert (a[2].data as BigInt).str() == '9223372036854775808'
	assert (a[3].data as BigInt).str() == '-9223372036854775809'
	assert (a[4].data as BigInt).str() == '207698809136909011942886895'
	assert a[4].as_int() == none
	assert a[4].as_f64()? > 2e26
	assert doc.nodes[0].arg(4).as_f64()? > 2e26
}

fn test_subnormal_float() {
	doc := parse('n 1.23e-308 5e-324')!
	assert doc.nodes[0].arguments[0].data as f64 == 1.23e-308
	assert doc.nodes[0].arguments[1].data as f64 == 5e-324
}

fn test_invalid_numbers() {
	for src in ['n 1.', 'n 1e', 'n 1e+', 'n 1._2', 'n 1e_2', 'n 0x_1', 'n 0X10', 'n 0b2', 'n 0xg',
		'n 1foo', 'n .5', 'n +.5', 'n 1.2.3', 'n 1e2e3', 'n 0x', 'n 1__0.', 'n 1.0e'] {
		if _ := parse(src) {
			assert false, 'should reject ${src}'
		}
	}
	for src in ['n 1__0', 'n 1_', 'n 1.0_', 'n 1e1_0', 'n 0x1_', 'n 0xa_b', 'n -', 'n +', 'n .',
		'n .foo', 'n -.foo', 'n +-1', 'n --1'] {
		parse(src) or { assert false, 'should accept ${src}: ${err.msg()}' }
	}
}

fn test_keywords() {
	doc := parse('n #true #false #null')!
	a := doc.nodes[0].arguments
	assert a[0].data as bool == true
	assert a[1].data as bool == false
	assert a[2].is_null()
	for src in ['n true', 'n false', 'n null', 'n inf', 'n -inf', 'n nan', 'n #bogus', 'n #', 'n #truex'] {
		if _ := parse(src) {
			assert false, 'should reject ${src}'
		}
	}
	// keyword prefixes are fine in identifiers
	doc2 := parse('n truex nullable infinity')!
	assert doc2.nodes[0].arguments.len == 3
}

fn test_strings_and_escapes() {
	doc := parse(r'n "a\"b\\c\n\r\t\b\f\s" "\u{1F600}" "x\    y" #"raw \n "# ##"has "# inside"##')!
	a := doc.nodes[0].arguments
	assert a[0].data as string == 'a"b\\c\n\r\t\b\f '
	assert a[1].data as string == '😀'
	assert a[2].data as string == 'xy'
	assert a[3].data as string == 'raw \\n '
	assert a[4].data as string == 'has "# inside'
	for src in ['n "\\q"', 'n "\\u{d800}"', 'n "\\u{110000}"', 'n "\\u{0000041}"', 'n "\\u{}"',
		'n "\\u41"', 'n "unterminated', 'n #"unterminated', 'n ##"x"#', 'n "line\nbreak"',
		'n #"line\nbreak"#'] {
		if _ := parse(src) {
			assert false, 'should reject ${src}'
		}
	}
}

fn test_multiline_strings() {
	doc := parse('n """\n    hello\n      world\n\n    end\n    """')!
	assert doc.nodes[0].arguments[0].data as string == 'hello\n  world\n\nend'
	doc2 := parse('n #"""\n  a\\n"b\n  """#')!
	assert doc2.nodes[0].arguments[0].data as string == 'a\\n"b'
	doc3 := parse('n """\n"""')!
	assert doc3.nodes[0].arguments[0].data as string == ''
	doc4 := parse('n """\r\n  a\r\n  b\r\n  """')!
	assert doc4.nodes[0].arguments[0].data as string == 'a\nb'
	for src in ['n """foo"""', 'n """\n  a\n b\n  """', 'n """\n a"""', 'n #"""x"""#',
		'n """\n  bar\\\n  """', 'n """\n\\u{4\\ 1}\n"""', 'n """\n\\u\\ {41}\n"""',
		'n """\n\\u{4\\\n1}\n"""'] {
		if _ := parse(src) {
			assert false, 'should reject ${src}'
		}
	}
}

fn test_identifiers() {
	doc := parse('ノード お名前=ฅ^•ﻌ•^ฅ\nfoo,bar <a>=b! ?x=1 @ .md --flag +x')!
	assert doc.nodes[0].name == 'ノード'
	assert doc.nodes[0].properties['お名前'].data as string == 'ฅ^•ﻌ•^ฅ'
	n := doc.nodes[1]
	assert n.name == 'foo,bar'
	assert n.properties['<a>'].data as string == 'b!'
	assert n.properties['?x'].data as i64 == 1
	assert n.arguments.map((it.data as string)) == ['@', '.md', '--flag', '+x']
	for src in ['n a\\b', 'n a[b', 'n a]b', 'n a(b', 'n a)b', 'n a#b', 'n a/b', 'n a"b', '=x'] {
		if _ := parse(src) {
			assert false, 'should reject ${src}'
		}
	}
}

fn test_type_annotations() {
	doc := parse('(person)node (u8)1 key=(date)"2024" ("")x ( spaced )y (t/*c*/)z')!
	n := doc.nodes[0]
	assert n.ty? == 'person'
	assert n.arguments[0].ty? == 'u8'
	assert n.properties['key'].ty? == 'date'
	assert n.arguments[1].ty? == ''
	assert n.arguments[2].ty? == 'spaced'
	assert n.arguments[3].ty? == 't'
	assert n.arg(0).as_int()? == 1
	for src in ['(1)n', '(#true)n', '(t\n)n', '(t n', '()n', 'n (t)'] {
		if _ := parse(src) {
			assert false, 'should reject ${src}'
		}
	}
}

fn test_properties_last_wins_and_order() {
	doc := parse('n a=1 b=2 a=3')!
	n := doc.nodes[0]
	assert n.properties.len == 2
	assert n.properties['a'].data as i64 == 3
	assert n.properties.keys() == ['a', 'b']
	doc2 := parse('n "quoted key"=1 #"raw"#=2 k /**/ = /**/ 3')!
	assert doc2.nodes[0].properties['quoted key'].data as i64 == 1
	assert doc2.nodes[0].properties['raw'].data as i64 == 2
	assert doc2.nodes[0].properties['k'].data as i64 == 3
	if _ := parse('n k=') {
		assert false
	}
	if _ := parse('n 1=2') {
		assert false
	}
}

fn test_children_and_terminators() {
	doc := parse('a { b; c { d }; e }\nf {}; g\n"h"{i}')!
	assert doc.nodes.len == 4
	assert doc.nodes[0].children.map(it.name) == ['b', 'c', 'e']
	// `}` closes the children block but does not terminate the node
	if _ := parse('a { b; c { d } e }') {
		assert false
	}
	// zero space before `{` and `;` is fine
	doc5 := parse('n a{b}\nn a;b')!
	assert doc5.nodes.len == 3
	assert doc5.nodes[0].children[0].name == 'b'
	assert doc.nodes[0].children[1].children[0].name == 'd'
	assert doc.nodes[1].children.len == 0
	assert doc.nodes[3].children[0].name == 'i'
	for src in ['a { b', 'a }', '}', 'a {} b', 'a {} {}', '{ a }', 'a 1 { b } 2'] {
		if _ := parse(src) {
			assert false, 'should reject ${src}'
		}
	}
}

fn test_comments() {
	doc := parse('// line\nnode /* inline */ 1 // trailing\n/* multi\nline /* nested */ */ other')!
	assert doc.nodes.len == 2
	assert doc.nodes[0].arguments[0].data as i64 == 1
	assert doc.nodes[1].name == 'other'
	doc2 := parse('node//comment')!
	assert doc2.nodes[0].name == 'node'
	if _ := parse('node /* unterminated') {
		assert false
	}
}

fn test_slashdash() {
	doc := parse('/- gone 1\nn /- 1 2 /-k=3 k=4 /-{ x } { y } /- { z }\n/-\nm')!
	assert doc.nodes.len == 1
	n := doc.nodes[0]
	assert n.arguments.len == 1
	assert n.arguments[0].data as i64 == 2
	assert n.properties['k'].data as i64 == 4
	assert n.children.map(it.name) == ['y']
	// slashdash may be followed by newlines
	doc2 := parse('node foo /-\nnot-a-node bar')!
	assert doc2.nodes[0].arguments.map(it.data as string) == ['foo', 'bar']
	for src in ['/-', 'n /-', 'n /- k=', 'n /- { x', 'n /-;', 'n /- /- x', 'n /-{ } 1', 'n /- 1 /-'] {
		if _ := parse(src) {
			assert false, 'should reject ${src}'
		}
	}
}

fn test_line_continuation() {
	doc := parse('n 1 \\\n  2 \\ // comment\n  3\n\\\nm')!
	assert doc.nodes.len == 2
	assert doc.nodes[0].arguments.len == 3
	doc2 := parse('n \\')!
	assert doc2.nodes[0].name == 'n'
	if _ := parse('n \\ 1') {
		assert false
	}
}

fn test_newlines_and_whitespace() {
	doc := parse('a\r\nb\rc\u0085d\u2028e\u2029f\x0bg\x0ch\n')!
	assert doc.nodes.map(it.name) == ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']
	doc2 := parse('a\u00a01\u30002\u20033\t4')!
	assert doc2.nodes[0].arguments.len == 4
	doc3 := parse('\ufeffbom')!
	assert doc3.nodes[0].name == 'bom'
	for src in ['a \ufeffb', 'a\u200e', 'a "\u202e"', 'a\x7f', 'a \x01',
		[u8(`a`), ` `, `"`, 0, `"`].bytestr()] {
		if _ := parse(src) {
			assert false, 'should reject ${src}'
		}
	}
}

fn test_invalid_utf8() {
	bad := [u8(`n`), ` `, 0xFF].bytestr()
	if _ := parse(bad) {
		assert false
	}
	surrogate := [u8(`n`), ` `, 0xED, 0xA0, 0x80].bytestr()
	if _ := parse(surrogate) {
		assert false
	}
	overlong := [u8(`n`), ` `, 0xC0, 0xAF].bytestr()
	if _ := parse(overlong) {
		assert false
	}
}

fn test_version_marker() {
	doc := parse('/- kdl-version 2\nnode')!
	assert doc.nodes.len == 1
	if _ := parse('/- kdl-version 1\nnode') {
		assert false
	}
	// without a trailing newline it is just a slashdashed node
	doc2 := parse('/- kdl-version 2')!
	assert doc2.nodes.len == 0
}

fn test_empty_documents() {
	for src in ['', '\n', '  ', '// c', '/* c */', '/- n', '\\\n'] {
		doc := parse(src) or { panic('${src}: ${err.msg()}') }
		assert doc.nodes.len == 0
	}
}

fn test_error_positions() {
	parse('node 1\n  other "x\n') or {
		e := err as ParseError
		assert e.line == 2
		assert e.col == 11
		assert e.msg().starts_with('2:11: ')
		return
	}
	assert false
}
