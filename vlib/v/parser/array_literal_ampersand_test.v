module parser

import os
import v.pref

// array_expression_shape parses `statement` inside a function, and returns the array
// literals, the array initializers and the infix operators of the tree, in source order.
fn array_expression_shape(path string, statement string, is_fmt bool) string {
	os.write_file(path, 'struct Foo {}\nfn takes(value bool) bool { return value }\nfn f(a []int, b []int, c bool, n int) bool {\n\t${statement}\n\treturn c\n}\n') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.is_fmt = is_fmt
	mut p := Parser.new(prefs)
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, '${statement}: ${p.diagnostics}'
	mut shape := []string{}
	for node in a.nodes {
		match node.kind {
			.array_literal { shape << 'literal' }
			.array_init { shape << 'init ${node.typ}' }
			.infix { shape << node.op.str() }
			else {}
		}
	}
	return shape.join(', ')
}

// `a == [] && b == []` was parsed as `a == []&&b == []`: an array of `&&b`. The compiler
// rejected the expression, and `v fmt` rewrote it to `a == []&&b{} == []`.
fn test_ampersands_after_array_literals_are_binary_operators() {
	path := os.join_path(os.vtmp_dir(), 'array_literal_ampersand_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	expressions := {
		'a == [] && b == []':          'literal, eq, literal, eq, logical_and'
		'a != [] && b != []':          'literal, ne, literal, ne, logical_and'
		'a == [] && c':                'literal, eq, logical_and'
		'c && a == [] && b == []':     'literal, eq, logical_and, literal, eq, logical_and'
		'a == [] || c':                'literal, eq, logical_or'
		'[] && c':                     'literal, logical_and'
		'[] || c':                     'literal, logical_or'
		'[] & n':                      'literal, amp'
		'a == [] & n':                 'literal, amp, eq'
		'a == []&& c':                 'literal, eq, logical_and'
		'a == [] &&c':                 'literal, eq, logical_and'
		'a == []&&/* separator */c':   'literal, eq, logical_and'
		'a == []&&// separator\n c':   'literal, eq, logical_and'
		'a == []/* separator */&&c':   'literal, eq, logical_and'
		'[] &/* separator */n':        'literal, amp'
		'a == [1]&&/* separator */c':  'literal, eq, logical_and'
		'a == [1]!/* separator */&&c': 'literal, eq, logical_and'
		'n !in [] && c':               'literal, logical_and'
		'a == [1] && b == [2]':        'literal, eq, literal, eq, logical_and'
		'a == [n] && c':               'literal, eq, logical_and'
		'a == [1]&& c':                'literal, eq, logical_and'
		'a == [1] & n':                'literal, amp, eq'
		'a == [n] & n':                'literal, amp, eq'
		'a == [1]! && c':              'literal, eq, logical_and'
		'a == [1]! & n':               'literal, amp, eq'
		'a == [1, 2] && c':            'literal, eq, logical_and'
		'a == [1, 2]! && c':           'literal, eq, logical_and'
	}
	statements := ['_ = EXPR', '_ = (EXPR)', 'assert EXPR', 'return EXPR', 'if EXPR {}', 'for EXPR {}',
		'println(takes(EXPR))']
	for is_fmt in [false, true] {
		for expression, expected in expressions {
			for statement in statements {
				source := statement.replace('EXPR', expression)
				assert array_expression_shape(path, source, is_fmt) == expected, source
			}
		}
	}
}

fn test_reference_element_types_after_array_brackets_are_array_inits() {
	path := os.join_path(os.vtmp_dir(), 'array_init_reference_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	expressions := {
		'[]&Foo{}':                'init []&Foo'
		'[]&&Foo{}':               'init []&&Foo'
		'[]&&&Foo{}':              'init []&&&Foo'
		'[] &Foo{}':               'init []&Foo'
		'[]&/* separator */Foo{}': 'init []&Foo'
		'[]&Foo{len: n}':          'init []&Foo'
		'[][]&Foo{}':              'init [][]&Foo'
		'[]&[]int{}':              'init []&[]int'
		'[]?&Foo{}':               'init []?&Foo'
		'[3]&Foo{}':               'init [3]&Foo'
		'[3]&&Foo{}':              'init [3]&&Foo'
		'[3] &Foo{}':              'init [3]&Foo'
		'[]int{}':                 'init []int'
		'[]&Foo{} == [] && c':     'init []&Foo, literal, eq, logical_and'
		'[3]&Foo{} == [3]&Foo{}':  'init [3]&Foo, init [3]&Foo, eq'
	}
	for is_fmt in [false, true] {
		for expression, expected in expressions {
			assert array_expression_shape(path, '_ = ${expression}', is_fmt) == expected, expression
		}
	}
}
