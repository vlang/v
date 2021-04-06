module parser

// import v.eval
import v.ast
import v.gen.c
import v.checker
import v.pref
import term

fn test_eval() {
	/*
	inputs := [
	//
	'2+2',
	'struct User { age int }',
	'2+3',
	'4',
	'x := 10',
	'x',
	'x + 1',
	'y := 2',
	'x * y', // 20
	//
	]
	expected := [
	//
	'5',
	'4',
	'>>',
	'10',
	'11',
	'>>',
	'20',
	//
	]
	table := ast.new_table()
	vpref := &pref.Preferences{}
	mut scope := &ast.Scope{
		start_pos: 0
		parent: 0
	}
	mut stmts := []ast.Stmt{}
	for input in inputs {
		stmts << parse_stmt(input, table, scope)
	}
	file := ast.File{
		stmts: stmts
		scope: scope
	}
	mut checker := checker.new_checker(table, vpref)
	checker.check(file)
	mut ev := eval.Eval{}
	s := ev.eval(file, table)
	println('eval done')
	println(s)
	assert s == expected.join('\n')
	exit(0)
	*/
	return
}

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
	table := &ast.Table{}
	vpref := &pref.Preferences{}
	gscope := &ast.Scope{
		parent: 0
	}
	prog := parse_file(s, table, .skip_comments, vpref, gscope)
	mut checker := checker.new_checker(table, vpref)
	checker.check(prog)
	res := c.gen([prog], table, vpref)
	println(res)
}

fn test_one() {
	if true {
		return
	}
	println('\n\ntest_one()')
	input := ['a := 10', 'b := -a', 'c := 20']
	expected := 'int a = 10;int b = -a;int c = 20;'
	table := ast.new_table()
	vpref := &pref.Preferences{}
	scope := &ast.Scope{
		start_pos: 0
		parent: 0
	}
	mut e := []ast.Stmt{}
	for line in input {
		e << parse_stmt(line, table, scope)
	}
	program := ast.File{
		stmts: e
		scope: scope
		global_scope: scope
	}
	mut checker := checker.new_checker(table, vpref)
	checker.check(program)
	res := c.gen([program], table, vpref).replace('\n', '').trim_space().after('#endif')
	println(res)
	ok := expected == res
	println(res)
	assert ok
	if !ok {
	}
	// exit(0)
}

fn test_parse_expr() {
	if true {
		return
	}
	input := ['1 == 1', '234234', '2 * 8 + 3', 'a := 3', 'a++', 'b := 4 + 2', 'neg := -a', 'a + a',
		'bo := 2 + 3 == 5', '2 + 1', 'q := 1', 'q + 777', '2 + 3', '2+2*4', 'x := 10', 'mut aa := 12',
		'ab := 10 + 3 * 9', 's := "hi"', 'x = 11', 'a += 10', '1.2 + 3.4', '4 + 4', '1 + 2 * 5',
		'-a+1', '2+2']
	expecting := ['1 == 1;', '234234;', '2 * 8 + 3;', 'int a = 3;', 'a++;', 'int b = 4 + 2;',
		'int neg = -a;', 'a + a;', 'bool bo = 2 + 3 == 5;', '2 + 1;', 'int q = 1;', 'q + 777;',
		'2 + 3;', '2 + 2 * 4;', 'int x = 10;', 'int aa = 12;', 'int ab = 10 + 3 * 9;',
		'string s = tos3("hi");', 'x = 11;', 'a += 10;', '1.2 + 3.4;', '4 + 4;', '1 + 2 * 5;',
		'-a + 1;', '2 + 2;']
	mut e := []ast.Stmt{}
	table := ast.new_table()
	vpref := &pref.Preferences{}
	mut checker := checker.new_checker(table, vpref)
	scope := &ast.Scope{
		start_pos: 0
		parent: 0
	}
	for s in input {
		println('\n\nst="$s"')
		e << parse_stmt(s, table, scope)
	}
	program := ast.File{
		stmts: e
		scope: scope
		global_scope: scope
	}
	checker.check(program)
	res := c.gen([program], table, vpref).after('#endif')
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

fn test_num_literals() {
	inputs := [
		'a := -1',
		'b := -12.e17',
		'c := -12.',
		'd := -a',
	]
	table := ast.new_table()
	mut scope := &ast.Scope{
		start_pos: 0
		parent: 0
	}
	mut rhs_types := []string{}
	for input in inputs {
		stmt := parse_stmt(input, table, scope)
		r := (stmt as ast.AssignStmt).right
		match r[0] {
			ast.IntegerLiteral { rhs_types << 'int literal' }
			ast.FloatLiteral { rhs_types << 'float literal' }
			ast.PrefixExpr { rhs_types << 'prefix expression' }
			else { rhs_types << 'something else' }
		}
	}
	mut rhs_type := rhs_types[0]
	assert rhs_type == 'int literal'
	rhs_type = rhs_types[1]
	assert rhs_type == 'float literal'
	rhs_type = rhs_types[2]
	assert rhs_type == 'float literal'
	rhs_type = rhs_types[3]
	assert rhs_type == 'prefix expression'
}

/*
table := &ast.Table{}
for s in text_expr {
	// print using str method
	x := parse_expr(s, table)
	println('source: $s')
	println('parsed: $x')
	println('===================')
}
*/

fn test_fn_is_html_open_tag() {
	mut s := '<style>'
	mut b := is_html_open_tag('style', s)
	assert b == true

	s = '<style    media="print"    custom-attr    >'
	b = is_html_open_tag('style', s)
	assert b == true

	s = '<style/>'
	b = is_html_open_tag('style', s)
	assert b == false

	s = 'styl'
	b = is_html_open_tag('style', s)
	assert b == false

	s = 'style'
	b = is_html_open_tag('style', s)
	assert b == false

	s = '<style'
	b = is_html_open_tag('style', s)
	assert b == false

	s = '<<style>'
	b = is_html_open_tag('style', s)
	assert b == false

	s = '<style>>'
	b = is_html_open_tag('style', s)
	assert b == false

	s = '<stylex>'
	b = is_html_open_tag('style', s)
	assert b == false

	s = '<html>'
	b = is_html_open_tag('style', s)
	assert b == false

	s = '<sript>'
	b = is_html_open_tag('style', s)
	assert b == false
}
