module parser

import (
	v.ast
	v.cgen
	v.table
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
	res := cgen.gen(prog)
	println(res)
}

fn test_parse_expr() {
	input := [
	//
	'q := 1',
	'q + 777',
	'2 + 3',
	'2+2*4',
	// '(2+2)*4',
	'x := 10',
	'a := 12',
	'ab := 10 + 3 * 9',
	's := "hi"',
	'1 += 2',
	// 'a += 10',
	'1.2 + 3.4',
	'4 + 4',
	'1 + 2 * 5',
	/*
		'(2 * 3) / 2',
		'3 + (7 * 6)',
		'2 ^ 8 * (7 * 6)',
		'20 + (10 * 15) / 5', // 50
		'(2) + (17*2-30) * (5)+2 - (8/2)*4', // 8
		//'2 + "hi"',
		*/

	]
	expecting := [
	//
	'int q = 1;',
	'q + 777;',
	'2 + 3;',
	'2 + 2 * 4;',
	// '(2 + 2) * 4',
	'int x = 10;',
	'int a = 12;',
	'int ab = 10 + 3 * 9;',
	'string s = tos3("hi");',
	'1 += 2;',
	// 'a += 10;',
	'1.2 + 3.4;',
	'4 + 4;',
	'1 + 2 * 5;',
	]
	mut e := []ast.Stmt
	table := &table.Table{}
	for s in input {
		e << parse_stmt(s, table)
	}
	program := ast.Program{
		stmts: e
	}
	res := cgen.gen(program)
	println('========')
	println(res)
	println('========')
	lines := res.trim_space().split_into_lines()
	mut i := 0
	for line in lines {
		if line == '' {
			continue
		}
		println('V:"$line" expecting:"${expecting[i]}"')
		assert line == expecting[i]
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
