module parser

import (
	v.ast
	v.gen
	v.table
	v.checker
	term
)

fn test_parse_file() {
	if true {
		return
	}
	s := '
fn foo() int {
	f := 23
	return 10+4
}

12 + 3
x := 10
5+7
8+4
'
	table := &table.Table{}
	prog := parse_file(s, table)
	res := gen.cgen([prog], table)
	println(res)
}

fn test_one() {
	println('\n\ntest_one()')
	input := ['a := 10',
	// 'a = 20',
	'b := -a',
	'c := 20',
	//
	]
	expected := 'int a = 10;int b = -a;int c = 20;'
	table := table.new_table()
	mut e := []ast.Stmt
	for line in input {
		e << parse_stmt(line, table)
	}
	program := ast.File{
		stmts: e
	}
	res := gen.cgen([program], table).replace('\n', '').trim_space()
	ok := expected == res
	println(res)
	assert ok
	if !ok {}
	// exit(0)
}

fn test_parse_expr() {
	input := ['1 == 1',
	'2 * 8 + 3',
	'a := 3',
	'a++',
	'b := 4 + 2',
	'neg := -a',
	'a + a',
	'bo := 2 + 3 == 5',
	'2 + 1',
	'q := 1',
	'q + 777',
	'2 + 3',
	'2+2*4',
	// '(2+2)*4',
	'x := 10',
	'mut aa := 12',
	'ab := 10 + 3 * 9',
	's := "hi"',
	// '1 += 2',
	'x = 11',
	'a += 10',
	'1.2 + 3.4',
	'4 + 4',
	'1 + 2 * 5',
	'-a',
	/*
	/*
		'(2 * 3) / 2',
		'3 + (7 * 6)',
		'2 ^ 8 * (7 * 6)',
		'20 + (10 * 15) / 5', // 50
		'(2) + (17*2-30) * (5)+2 - (8/2)*4', // 8
		//'2 + "hi"',
		*/
		*/

	]
	expecting := ['1 == 1;',
	'2 * 8 + 3;',
	'int a = 3;',
	'a++;',
	'int b = 4 + 2;',
	'int neg = -a;',
	'a + a;',
	'bool bo = 2 + 3 == 5;',
	'2 + 1;',
	'int q = 1;',
	'q + 777;',
	'2 + 3;',
	'2 + 2 * 4;',
	// '(2 + 2) * 4',
	'int x = 10;',
	'int aa = 12;',
	'int ab = 10 + 3 * 9;',
	'string s = tos3("hi");',
	// '1 += 2;',
	'x = 11;',
	'a += 10;',
	'1.2 + 3.4;',
	'4 + 4;',
	'1 + 2 * 5;',
	'-a;',
	]
	mut e := []ast.Stmt
	table := table.new_table()
	mut checker := checker.new_checker(table)
	for s in input {
		// println('\n\nst="$s"')
		e << parse_stmt(s, table)
	}
	program := ast.File{
		stmts: e
	}
	checker.check(program)
	res := gen.cgen([program], table)
	println('========')
	println(res)
	println('========')
	lines := res.trim_space().split_into_lines()
	mut i := 0
	for line in lines {
		if line == '' {
			continue
		}
		if line != expecting[i] {
			println('V:"$line" expecting:"${expecting[i]}"')
		}
		assert line == expecting[i]
		println(term.green('$i OK'))
		println(line)
		println('')
		i++
		if i >= expecting.len {
			break
		}
	}
}

/*
	table := &table.Table{}
	for s in text_expr {
		// print using str method
		x := parse_expr(s, table)
		println('source: $s')
		println('parsed: $x')
		println('===================')
*/
