module parser

// import v.eval
import v.ast
import v.gen.c
import v.checker
import v.pref
import term
import os
import benchmark

const vroot = os.dir(os.dir(os.dir(os.dir(@FILE))))

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
		parent: unsafe { nil }
	}
	mut stmts := []ast.Stmt{}
	for input in inputs {
		stmts << parse_stmt(input, table, scope)
	}
	file := &ast.File{
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

fn test_parse_text() {
	println(@LOCATION)
	source_text := '
fn foo() int {
	f := 23
	return 10+4+f
}
fn ff(x int) {}

fn main() {
  ff(12 + 3)
  x := 10
  bar(5+7)
  ff(8+x)
}
'
	mut table := ast.new_table()
	vpref := &pref.Preferences{}
	mut prog := parse_text(source_text, '', mut table, .skip_comments, vpref)
	mut checker_ := checker.new_checker(table, vpref)
	checker_.check(mut prog)
}

fn test_one() {
	if true {
		return
	}
	println(@LOCATION)
	input := ['a := 10', 'b := -a', 'c := 20']
	expected := 'int a = 10;int b = -a;int c = 20;'
	mut table := ast.new_table()
	vpref := &pref.Preferences{}
	mut scope := &ast.Scope{
		start_pos: 0
	}
	mut e := []ast.Stmt{}
	for line in input {
		e << parse_stmt(line, mut table, mut scope)
	}
	mut program := &ast.File{
		stmts:        e
		scope:        scope
		global_scope: scope
	}
	mut checker_ := checker.new_checker(table, vpref)
	checker_.check(mut program)
	result := c.gen([program], mut table, vpref)
	res := result.res_builder.bytestr().replace('\n', '').trim_space().after('#endif')
	println(res)
	ok := expected == res
	println(res)
	dump(ok)
	assert ok
	if !ok {
	}
	// exit(0)
}

fn test_parse_expr() {
	println(@LOCATION)
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
	mut table := ast.new_table()
	vpref := &pref.Preferences{}
	mut chk := checker.new_checker(table, vpref)
	mut scope := &ast.Scope{
		start_pos: 0
	}
	for s in input {
		println('\n\nst="${s}"')
		e << parse_stmt(s, mut table, mut scope)
	}
	mut program := &ast.File{
		stmts:        e
		scope:        scope
		global_scope: scope
	}
	chk.check(mut program)
	result := c.gen([program], mut table, vpref)
	res := result.res_builder.bytestr().after('#endif')
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
			println('V:"${line}" expecting:"${expecting[i]}"')
		}
		assert line == expecting[i]
		println(term.green('${i} OK'))
		println(line)
		println('')
		i++
		if i >= expecting.len {
			break
		}
	}
}

fn test_num_literals() {
	println(@LOCATION)
	inputs := [
		'a := -1',
		'b := -12.e17',
		'c := -12.',
		'd := -a',
	]
	mut table := ast.new_table()
	mut scope := &ast.Scope{
		start_pos: 0
	}
	mut rhs_types := []string{}
	for input in inputs {
		stmt := parse_stmt(input, mut table, mut scope)
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
	println(@LOCATION)
	mut s := '<style>'
	mut b := is_html_open_tag('style', s)
	assert b == true

	s = '<style    media="print"    custom-attr    >'
	b = is_html_open_tag('style', s)
	assert b == true

	s = '<style/>'
	b = is_html_open_tag('style', s)
	assert !b

	s = 'styl'
	b = is_html_open_tag('style', s)
	assert !b

	s = 'style'
	b = is_html_open_tag('style', s)
	assert !b

	s = '<style'
	b = is_html_open_tag('style', s)
	assert !b

	s = '<<style>'
	b = is_html_open_tag('style', s)
	assert !b

	s = '<style>>'
	b = is_html_open_tag('style', s)
	assert !b

	s = '<stylex>'
	b = is_html_open_tag('style', s)
	assert !b

	s = '<html>'
	b = is_html_open_tag('style', s)
	assert !b

	s = '<script>'
	b = is_html_open_tag('style', s)
	assert !b
}

fn scan_v(mut files []string, path string) ! {
	for i in os.ls(path)! {
		p := os.join_path(path, i)
		if !os.exists(p) {
			continue
		}
		if os.is_file(p) {
			if i.ends_with('.v') && !i.contains_any_substr(['_test.', 'test_', 'tests_']) {
				files << p
			}
		} else {
			if !i.starts_with('test') && !i.ends_with('_tests') && !i.ends_with('builtin') {
				scan_v(mut files, p)!
			}
		}
	}
}

fn parse(output_mode pref.OutputMode) ! {
	mut b := benchmark.start()
	mut files := []string{}
	//	mode_files := os.walk_ext(os.join_path(vroot, 'vlib/v/parser/testdata/${output_mode}'), '.vv')
	//	files << mode_files
	scan_v(mut files, os.join_path(vroot, 'vlib'))!
	scan_v(mut files, os.join_path(vroot, 'cmd'))!
	mut pref_ := pref.new_preferences()
	pref_.output_mode = output_mode
	for idx, f in files {
		// eprintln('> parsing in mode: ${output_mode}, ${idx+1:5}/${files.len} $f ...')
		mut table := ast.new_table()
		p := parse_file(f, mut table, .parse_comments, pref_)
		assert !isnil(p), 'failed to parse `${f}` in mode: ${output_mode}'
		assert p.errors.len == 0, 'file ${f} should have been parsed with 0 errors'
	}
	b.measure('parsing ${files.len} files in ${output_mode} mode')
}

fn test_parse_with_silent() {
	println(@LOCATION)
	parse(.silent)!
}

fn test_parse_with_stdout() {
	println(@LOCATION)
	parse(.stdout)!
}

fn test_parse_vls_info() {
	println(@LOCATION)
	source_text := '
// not_my_const comment line1
// my_const comment line2
const my_const = 123	// my_const end_comment

// not_MyS comment line1
// MyS comment line2
struct MyS {
	// a comment line1
	// a comment line2
	a int	// a end_comment
	// b comment line1
	// b comment line2
	b int	// b end_comment
}

// not_MyInterface comment line1
// MyInterface comment line2
interface MyInterface {
	// method comment line1
	// method comment line2
	method() string	// method end_comment
	// data comment line1
	// data comment line2
	data int // data end_comment
}

// not_my_global comment line1
// my_global comment line2
__global my_global = 456	// my_global end_comment

// not_MyAlias comment line1
// MyAlias comment line2
type MyAlias = u8	// MyAlias end_comment

// not_MySum comment line1
// MySum comment line2
type MySum = u8 | u16	// MySum end_comment

// not_MyFnType comment line1
// MyFnType comment line2
type MyFnType = fn (msg &char, arg usize)	// MyFnType end_comment


// not_MyEnum comment line1
// MyEnum comment line2
enum MyEnum {
	// x comment line1
	// x comment line2
	x	// x end_comment
	// y comment line1
	// y comment line2
	y	// y end_comment
}

// not_add comment line1
// add comment line2
fn (mut k MyS) add(val int) {
	k.a += val
}


// not_foo comment line1
// foo comment line2
fn foo() int {
	f := 23
	return 10+4+f
}

// not_ff comment line1
// ff comment line2
fn ff(x int) {}	// ff end_comment

// not_main comment line1
// main comment line2
fn main() {
  ff(12 + 3)
  x := 10
  bar(5+7)
  ff(8+x)
}
'
	mut table := ast.new_table()
	vpref := &pref.Preferences{
		enable_globals: true
		is_vls:         true
	}
	mut prog := parse_text(source_text, '', mut table, .parse_comments, vpref)
	mut checker_ := checker.new_checker(table, vpref)
	checker_.check(mut prog)

	assert 'const_main.my_const' in table.vls_info
	assert 'struct_main.MyS' in table.vls_info
	assert 'struct_main.MyS.a' in table.vls_info
	assert 'struct_main.MyS.b' in table.vls_info
	assert 'interface_main.MyInterface' in table.vls_info
	assert 'fn_main[MyInterface]method' in table.vls_info // MyInterface method
	assert 'interface_main.MyInterface.data' in table.vls_info
	assert 'fn_main[MyS]add' in table.vls_info // MyS method
	assert 'fn_main[]foo' in table.vls_info
	assert 'global_main.my_global' in table.vls_info // module specific global
	assert 'global_my_global' in table.vls_info
	assert 'aliastype_main.MyAlias' in table.vls_info
	assert 'sumtype_main.MySum' in table.vls_info
	assert 'fntype_main.MyFnType' in table.vls_info
	assert 'enum_main.MyEnum' in table.vls_info
	assert 'enum_main.MyEnum.x' in table.vls_info
	assert 'enum_main.MyEnum.y' in table.vls_info
	dump(table.vls_info['const_main.my_const'])
	dump(table.vls_info['struct_main.MyS'])
	dump(table.vls_info['struct_main.MyS.a'])
	dump(table.vls_info['struct_main.MyS.b'])
	dump(table.vls_info['interface_main.MyInterface'])
	dump(table.vls_info['fn_main[MyInterface]method'])
	dump(table.vls_info['interface_main.MyInterface.data'])
	dump(table.vls_info['fn_main[MyS]add'])
	dump(table.vls_info['fn_main[]foo'])
	dump(table.vls_info['global_main.my_global'])
	dump(table.vls_info['global_my_global'])
	dump(table.vls_info['aliastype_main.MyAlias'])
	dump(table.vls_info['sumtype_main.MySum'])
	dump(table.vls_info['fntype_main.MyFnType'])
	dump(table.vls_info['enum_main.MyEnum'])
	dump(table.vls_info['enum_main.MyEnum.x'])
	dump(table.vls_info['enum_main.MyEnum.y'])
	assert table.vls_info['const_main.my_const'].doc.trim_space() == 'my_const comment line2
my_const end_comment'
	assert table.vls_info['struct_main.MyS'].doc.trim_space() == 'MyS comment line2'
	assert table.vls_info['struct_main.MyS.a'].doc.trim_space() == 'a comment line1
a comment line2
a end_comment'
	assert table.vls_info['struct_main.MyS.b'].doc.trim_space() == 'b comment line1
b comment line2
b end_comment'
	assert table.vls_info['interface_main.MyInterface'].doc.trim_space() == 'MyInterface comment line2'
	assert table.vls_info['fn_main[MyInterface]method'].doc.trim_space() == 'method comment line1
method comment line2
method end_comment'
	assert table.vls_info['interface_main.MyInterface.data'].doc.trim_space() == 'data comment line1
data comment line2
data end_comment'
	assert table.vls_info['fn_main[MyS]add'].doc.trim_space() == 'add comment line2'
	assert table.vls_info['fn_main[]foo'].doc.trim_space() == 'foo comment line2'
	assert table.vls_info['global_main.my_global'].doc.trim_space() == 'my_global comment line2'
	assert table.vls_info['global_my_global'].doc.trim_space() == 'my_global comment line2'
	assert table.vls_info['aliastype_main.MyAlias'].doc.trim_space() == 'MyAlias comment line2
MyAlias end_comment'
	assert table.vls_info['sumtype_main.MySum'].doc.trim_space() == 'MySum comment line2
MySum end_comment'
	assert table.vls_info['fntype_main.MyFnType'].doc.trim_space() == 'MyFnType comment line2
MyFnType end_comment'
	assert table.vls_info['enum_main.MyEnum'].doc.trim_space() == 'MyEnum comment line2'
	assert table.vls_info['enum_main.MyEnum.x'].doc.trim_space() == 'x comment line1
x comment line2
x end_comment'
	assert table.vls_info['enum_main.MyEnum.y'].doc.trim_space() == 'y comment line1
y comment line2
y end_comment'
}
