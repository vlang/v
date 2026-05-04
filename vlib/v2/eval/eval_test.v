module eval

import os
import v2.ast
import v2.parser
import v2.pref
import v2.token
import time

const eval_backend_timeout_ms = 60_000
const eval_backend_poll_ms = 50
const eval_backend_term_grace_ms = 1_000

fn testsuite_begin() {
	skip_test('v2 eval tests are temporarily disabled')
}

fn eval_backend_tmp_dir() string {
	return os.join_path(os.temp_dir(), 'v2_eval_integration_${os.getpid()}')
}

fn ensure_eval_backend_runner(vexe string) !string {
	tmp_dir := eval_backend_tmp_dir()
	os.mkdir_all(tmp_dir)!
	v2_exe := os.join_path(tmp_dir, 'v2_eval_runner')
	if os.exists(v2_exe) {
		return v2_exe
	}
	vroot := os.dir(vexe)
	build_res := os.execute('${os.quoted_path(vexe)} -o ${os.quoted_path(v2_exe)} ${os.quoted_path(os.join_path(vroot,
		'cmd', 'v2', 'v2.v'))}')
	if build_res.exit_code != 0 {
		return error(build_res.output)
	}
	return v2_exe
}

fn run_eval_backend(code string) !os.Result {
	return run_eval_backend_with_args(code, []string{})
}

fn run_eval_backend_with_args(code string, extra_args []string) !os.Result {
	vexe := os.getenv('VEXE')
	if vexe == '' {
		return error('VEXE is not set')
	}
	tmp_dir := eval_backend_tmp_dir()
	os.mkdir_all(tmp_dir)!
	tmp_file := os.join_path(tmp_dir, 'sample_${time.now().unix_micro()}.v')
	os.write_file(tmp_file, code)!
	defer {
		os.rm(tmp_file) or {}
	}
	v2_exe := ensure_eval_backend_runner(vexe)!

	mut p := os.new_process(v2_exe)
	mut args := ['-backend', 'eval', tmp_file]
	if extra_args.len > 0 {
		args << '--'
		args << extra_args
	}
	p.set_args(args)
	p.set_redirect_stdio()
	p.run()
	defer {
		p.close()
	}

	mut waited_ms := 0
	for p.is_alive() && waited_ms < eval_backend_timeout_ms {
		time.sleep(eval_backend_poll_ms * time.millisecond)
		waited_ms += eval_backend_poll_ms
	}
	if p.is_alive() {
		p.signal_term()
		mut grace_ms := 0
		for p.is_alive() && grace_ms < eval_backend_term_grace_ms {
			time.sleep(eval_backend_poll_ms * time.millisecond)
			grace_ms += eval_backend_poll_ms
		}
		if p.is_alive() {
			p.signal_kill()
			return error('eval backend timed out after ${eval_backend_timeout_ms}ms')
		}
	}
	p.wait()
	stdout := p.stdout_slurp()
	stderr := p.stderr_slurp()
	return os.Result{
		exit_code: p.code
		output:    stdout + stderr
	}
}

fn test_eval_function_call_and_for_range() {
	mut e := create()
	e.run_text('
fn sum(n int) int {
	mut acc := 0
	for i in 0 .. n {
		acc += i
	}
	return acc
}

fn main() {
	println(sum(5))
}
') or {
		panic(err)
	}
	assert e.stdout() == '10\n'
}

fn test_eval_if_expr_value() {
	mut e := create()
	e.run_text('
fn main() {
	x := if 3 > 2 {
		41
	} else {
		0
	}
	println(x + 1)
}
') or {
		panic(err)
	}
	assert e.stdout() == '42\n'
}

fn test_eval_array_append_and_string_interpolation() {
	mut e := create()
	e.run_text('
fn main() {
	mut arr := []int{}
	arr << 7
	arr << 9
	println("\${arr[0]}:\${arr[1]}:\${arr.len}")
}
') or {
		panic(err)
	}
	assert e.stdout() == '7:9:2\n'
}

fn test_eval_transformed_array_append_and_string_interpolation() {
	res := run_eval_backend('
fn main() {
	mut arr := []int{}
	arr << 7
	arr << 9
	println("\${arr[0]}:\${arr[1]}:\${arr.len}")
}
') or {
		panic(err)
	}
	if res.exit_code != 0 {
		panic(res.output)
	}
	assert res.output.contains('7:9:2\n')
}

fn test_eval_runtime_args_follow_separator() {
	res := run_eval_backend_with_args('
import os

fn main() {
	println(os.args.join("|"))
}
', [
		'alpha',
		'beta',
	]) or { panic(err) }
	if res.exit_code != 0 {
		panic(res.output)
	}
	assert res.output.contains('|alpha|beta')
}

fn test_eval_transformed_nested_array_clone() {
	res := run_eval_backend("
fn main() {
	mut a := [[1, 2], [3]]
	mut b := a.clone()
	b[0] << 9
	println('\${a[0].len}:\${b[0].len}')
}
	") or {
		panic(err)
	}
	if res.exit_code != 0 {
		panic(res.output)
	}
	assert res.output.contains('2:3\n')
}

fn test_eval_transformed_array_append_many() {
	res := run_eval_backend("
fn main() {
	mut items := [1]
	items << [2, 3]
	println('\${items[0]}:\${items[1]}:\${items[2]}:\${items.len}')
}
	") or {
		panic(err)
	}
	if res.exit_code != 0 {
		panic(res.output)
	}
	assert res.output.contains('1:2:3:3\n')
}

fn test_eval_transformed_types_init_universe() {
	res := run_eval_backend("
import v2.types

fn main() {
	_ = types.init_universe()
	println('ok')
}
	") or {
		panic(err)
	}
	if res.exit_code != 0 {
		panic(res.output)
	}
	assert res.output.contains('ok\n')
}

fn test_eval_transformed_types_lookup_parent_typ() {
	res := run_eval_backend("
import v2.types

fn main() {
	u := types.init_universe()
	if obj := u.lookup_parent('string', 0) {
		_ = obj.typ()
		println('ok')
	} else {
		println('missing')
	}
}
	") or {
		panic(err)
	}
	if res.exit_code != 0 {
		panic(res.output)
	}
	assert res.output.contains('ok\n')
}

fn test_eval_transformed_map_if_guard_with_nested_sumtype() {
	res := run_eval_backend("
import v2.types

fn lookup(m map[string]types.Object, key string) ?types.Object {
	if obj := m[key] {
		return obj
	}
	return none
}

fn main() {
	mut m := map[string]types.Object{}
	m['x'] = types.Object(types.Type(types.string_))
	if obj := lookup(m, 'x') {
		_ = obj.typ()
		println('ok')
	} else {
		println('missing')
	}
}
	") or {
		panic(err)
	}
	if res.exit_code != 0 {
		panic(res.output)
	}
	assert res.output.contains('ok\n')
}

fn test_eval_transformed_map_builtin_fast_paths() {
	res := run_eval_backend("
fn main() {
	mut m := map[string]int{}
	m['a'] = 1
	m['b'] += 2
	a_val := m['a']
	b_val := m['b']
	has_a := 'a' in m
	println('\${a_val}:\${b_val}:\${m.keys().len}:\${m.values().len}:\${has_a}')
	m.delete('a')
	mut cloned := m.clone()
	mut moved := cloned.move()
	cloned.clear()
	has_a_after_delete := 'a' in m
	has_b_in_moved := 'b' in moved
	moved_b := moved['b'] or { 0 }
	println('\${has_a_after_delete}:\${has_b_in_moved}:\${cloned.len}:\${moved_b}')
}
	") or {
		panic(err)
	}
	if res.exit_code != 0 {
		panic(res.output)
	}
	assert res.output.contains('1:2:2:2:true\n')
	assert res.output.contains('false:true:0:2\n')
}

fn test_eval_transformed_os_execute() {
	res := run_eval_backend("
import os

fn main() {
	cmd := if os.user_os() == 'windows' { 'cmd /c echo ok' } else { 'printf ok' }
	result := os.execute(cmd)
	println('\${result.exit_code}:\${result.output}')
}
	") or {
		panic(err)
	}
	if res.exit_code != 0 {
		panic(res.output)
	}
	assert res.output.contains('0:ok')
}

fn test_eval_transformed_smartcast_assignment() {
	res := run_eval_backend('
struct Foo {
mut:
	n int
}

struct Bar {}

type W = Bar | Foo

fn main() {
	mut w := W(Foo{n: 1})
	if w is Foo {
		w.n = 2
	}
	if w is Foo {
		println(w.n)
	}
}
	') or {
		panic(err)
	}
	if res.exit_code != 0 {
		panic(res.output)
	}
	assert res.output.contains('2\n')
}

fn test_eval_transformed_ast_expr_pos() {
	res := run_eval_backend("
import v2.ast

fn main() {
	expr := ast.Expr(ast.PrefixExpr{})
	_ = expr.pos()
	println('ok')
}
	") or {
		panic(err)
	}
	if res.exit_code != 0 {
		panic(res.output)
	}
	assert res.output.contains('ok\n')
}

fn test_eval_array_has_named_struct_field() {
	e := create()
	attrs := ArrayValue{
		values: [
			Value(StructValue{
				type_name: 'Attr'
				fields:    {
					'name': Value('flag')
				}
			}),
		]
	}
	assert e.array_has(attrs, 'flag')
	assert !e.array_has(attrs, 'other')
}

fn test_eval_sumtype_tag_from_type_value_uses_registered_type_kind() {
	mut prefs := pref.new_preferences()
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(&prefs)
	type_files :=
		os.walk_ext(os.join_path(@VMODROOT, 'vlib', 'v2', 'types'), '.v').filter(!it.ends_with('_test.v'))
	dummy_main := os.join_path(os.temp_dir(), 'v2_eval_dummy_main_${os.getpid()}.v')
	os.write_file(dummy_main, 'module main\nfn main() {}\n') or { panic(err) }
	defer {
		os.rm(dummy_main) or {}
	}
	mut files_to_parse := type_files.clone()
	files_to_parse << dummy_main
	files := par.parse_files(files_to_parse, mut file_set)
	mut e := create()
	e.register_files(files) or { panic(err) }
	info := e.sum_type_info('types.Type') or { panic('missing types.Type sumtype info') }
	struct_tag := e.lookup_sumtype_variant_tag(info, 'Struct') or { panic('missing Struct tag') }
	alias_tag := e.lookup_sumtype_variant_tag(info, 'Alias') or { panic('missing Alias tag') }
	struct_value_tag := e.sumtype_tag_from_value(info, TypeValue{
		name: 'types.ObjectCommon'
	}) or { panic('missing struct kind tag') }
	alias_value_tag := e.sumtype_tag_from_value(info, TypeValue{
		name: 'voidptr'
	}) or { panic('missing alias kind tag') }
	assert struct_value_tag == struct_tag
	assert alias_value_tag == alias_tag
}

fn test_eval_type_sum_data_is_nil() {
	mut prefs := pref.new_preferences()
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(&prefs)
	type_files :=
		os.walk_ext(os.join_path(@VMODROOT, 'vlib', 'v2', 'types'), '.v').filter(!it.ends_with('_test.v'))
	dummy_main := os.join_path(os.temp_dir(), 'v2_eval_dummy_main_${os.getpid()}.v')
	os.write_file(dummy_main, 'module main\nfn main() {}\n') or { panic(err) }
	defer {
		os.rm(dummy_main) or {}
	}
	mut files_to_parse := type_files.clone()
	files_to_parse << dummy_main
	files := par.parse_files(files_to_parse, mut file_set)
	mut e := create()
	e.register_files(files) or { panic(err) }
	assert e.type_sum_data_is_nil(e.zero_struct_value('types.Type'))
	assert !e.type_sum_data_is_nil(TypeValue{
		name: 'types.ObjectCommon'
	})
}

fn test_eval_zero_value_for_sumtype_data_field() {
	mut prefs := pref.new_preferences()
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(&prefs)
	ast_files :=
		os.walk_ext(os.join_path(@VMODROOT, 'vlib', 'v2', 'ast'), '.v').filter(!it.ends_with('_test.v'))
	dummy_main := os.join_path(os.temp_dir(), 'v2_eval_dummy_main_${os.getpid()}.v')
	os.write_file(dummy_main, 'module main\nfn main() {}\n') or { panic(err) }
	defer {
		os.rm(dummy_main) or {}
	}
	mut files_to_parse := ast_files.clone()
	files_to_parse << dummy_main
	files := par.parse_files(files_to_parse, mut file_set)
	mut e := create()
	e.register_files(files) or { panic(err) }
	value := e.zero_value_for_sumtype_data_field('ast.Expr._data', '_ArrayInitExpr') or {
		panic('missing sumtype data field zero value')
	}
	assert value is StructValue
	assert (value as StructValue).type_name == 'ast.ArrayInitExpr'
}

fn test_eval_selector_on_sumtype_data_void_field_returns_zero_value() {
	mut prefs := pref.new_preferences()
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(&prefs)
	ast_files :=
		os.walk_ext(os.join_path(@VMODROOT, 'vlib', 'v2', 'ast'), '.v').filter(!it.ends_with('_test.v'))
	dummy_main := os.join_path(os.temp_dir(), 'v2_eval_dummy_main_${os.getpid()}.v')
	os.write_file(dummy_main, 'module main\nfn main() {}\n') or { panic(err) }
	defer {
		os.rm(dummy_main) or {}
	}
	mut files_to_parse := ast_files.clone()
	files_to_parse << dummy_main
	files := par.parse_files(files_to_parse, mut file_set)
	mut e := create()
	e.register_files(files) or { panic(err) }
	e.open_scope()
	defer {
		e.close_scope() or { panic(err) }
	}
	e.declare_var('data', StructValue{
		type_name: 'ast.Expr._data'
		fields:    {
			'_ArrayInitExpr': Value(void_value())
		}
	})
	data_ident := ast.Ident{
		name: 'data'
	}
	data_lhs := ast.Expr(data_ident)
	data_rhs := ast.Ident{
		name: '_ArrayInitExpr'
	}
	data_selector := ast.SelectorExpr{
		lhs: data_lhs
		rhs: data_rhs
	}
	value := e.eval_selector_expr(data_selector) or { panic(err) }
	assert value is StructValue
	array_init := value as StructValue
	assert array_init.type_name == 'ast.ArrayInitExpr'
	assert 'exprs' in array_init.fields
	assert array_init.fields['exprs'] or { panic('missing exprs field') } is ArrayValue
}

fn test_eval_selector_on_sumtype_wrapper_variant_field_returns_zero_value() {
	mut prefs := pref.new_preferences()
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(&prefs)
	ast_files :=
		os.walk_ext(os.join_path(@VMODROOT, 'vlib', 'v2', 'ast'), '.v').filter(!it.ends_with('_test.v'))
	dummy_main := os.join_path(os.temp_dir(), 'v2_eval_dummy_main_${os.getpid()}.v')
	os.write_file(dummy_main, 'module main\nfn main() {}\n') or { panic(err) }
	defer {
		os.rm(dummy_main) or {}
	}
	mut files_to_parse := ast_files.clone()
	files_to_parse << dummy_main
	files := par.parse_files(files_to_parse, mut file_set)
	mut e := create()
	e.register_files(files) or { panic(err) }
	info := e.sum_type_info('ast.Expr') or { panic('missing ast.Expr sum type info') }
	ident_tag := e.lookup_sumtype_variant_tag(info, 'ast.Ident') or {
		panic('missing ast.Ident sumtype tag')
	}
	expr_value := e.build_sumtype_wrapper('ast.Expr', info, ident_tag, StructValue{
		type_name: 'ast.Ident'
		fields:    {
			'name': Value('')
		}
	})
	e.open_scope()
	defer {
		e.close_scope() or { panic(err) }
	}
	e.declare_var('expr', expr_value)
	expr_ident := ast.Ident{
		name: 'expr'
	}
	expr_lhs := ast.Expr(expr_ident)
	expr_rhs := ast.Ident{
		name: '_ArrayInitExpr'
	}
	expr_selector := ast.SelectorExpr{
		lhs: expr_lhs
		rhs: expr_rhs
	}
	value := e.eval_selector_expr(expr_selector) or { panic(err) }
	assert value is StructValue
	assert (value as StructValue).type_name == 'ast.ArrayInitExpr'
}

fn test_eval_lookup_parent_through_parent_scope() {
	res := run_eval_backend("
import v2.types

fn main() {
	mut parent := types.new_scope(unsafe { nil })
	parent.insert('Error', types.Object(types.Type(types.Struct{name: 'builtin__Error'})))
	mut child := types.new_scope(parent)
	if obj := child.lookup_parent('Error', 0) {
		if obj is types.Type {
			println(obj.name())
		}
	}
}
	") or {
		panic(err)
	}
	if res.exit_code != 0 {
		panic(res.output)
	}
	assert res.output.contains('builtin__Error\n')
}

fn test_eval_flag_enum_defaults() {
	res := run_eval_backend('
@[flag]
enum Properties {
	boolean
	float
	integer
	unsigned
	untyped
}

fn main() {
	println(int(Properties.integer))
}
	') or {
		panic(err)
	}
	if res.exit_code != 0 {
		panic(res.output)
	}
	assert res.output.contains('4\n')
}

fn test_eval_option_typed_struct_field() {
	res := run_eval_backend('
import v2.types

fn main() {
	println(types.Type(types.Channel{elem_type: types.int_}).name())
}
	') or {
		panic(err)
	}
	if res.exit_code != 0 {
		panic(res.output)
	}
	assert res.output.contains('chan int\n')
}

fn test_eval_transformed_type_name_for_primitive_variants() {
	res := run_eval_backend('
import v2.types

fn main() {
	println(types.Type(types.bool_).name())
	println(types.Type(types.int_).name())
	println(types.Type(types.f64_).name())
	println(types.Type(types.voidptr_).name())
}
	') or {
		panic(err)
	}
	if res.exit_code != 0 {
		panic(res.output)
	}
	assert res.output.contains('bool\nint\nf64\nvoidptr\n')
}

fn test_eval_transformed_imported_struct_field_assignment_keeps_declaring_module_type() {
	res := run_eval_backend('
import v2.types

fn main() {
	mut m := types.Map{
		key_type: types.Type(types.int_)
		value_type: types.Type(types.int_)
	}
	m.key_type = types.int_
	println(m.key_type.name())
	println(m.key_type.base_type().name())
}
	') or {
		panic(err)
	}
	if res.exit_code != 0 {
		panic(res.output)
	}
	assert res.output.contains('int\nint\n')
}

fn test_eval_transformed_typed_array_index_assignment_keeps_sumtype_wrapper() {
	res := run_eval_backend('
import v2.types

fn main() {
	mut items := []types.Type{len: 1, init: types.Type(types.int_)}
	items[0] = types.int_
	println(items[0].name())
	println(items[0].base_type().name())
}
	') or {
		panic(err)
	}
	if res.exit_code != 0 {
		panic(res.output)
	}
	assert res.output.contains('int\nint\n')
}

fn test_eval_transformed_typed_map_assignment_keeps_sumtype_wrapper() {
	res := run_eval_backend("
import v2.types

fn main() {
	mut m := map[string]types.Type{}
	m['x'] = types.int_
	println(m['x'].name())
	println(m['x'].base_type().name())
}
	") or {
		panic(err)
	}
	if res.exit_code != 0 {
		panic(res.output)
	}
	assert res.output.contains('int\nint\n')
}

fn test_eval_transformed_object_module_roundtrip_keeps_wrapper() {
	res := run_eval_backend("
import v2.types

fn id(obj types.Object) types.Object {
	return obj
}

fn main() {
	mut mod_scope := types.new_scope(unsafe { nil })
	mod_scope.insert('File', types.Object(types.Struct{
		name: 'File'
	}))
	obj := types.Object(types.Module{
		name:  'ast'
		scope: mod_scope
	})
	println(obj.type_name())
	println(id(obj).type_name())
	mut arr := []types.Object{}
	arr << obj
	println(arr[0].type_name())
	mut m := map[string]types.Object{}
	m['ast'] = obj
	println(m['ast'].type_name())
	if got := m['ast'] {
		println(got.type_name())
		if got is types.Module {
			println(got.name)
			if file_obj := got.scope.lookup_parent('File', 0) {
				println(file_obj.type_name())
				println(file_obj.typ().name())
			}
		}
	}
}
	") or {
		panic(err)
	}
	if res.exit_code != 0 {
		panic(res.output)
	}
	assert res.output.contains('types.Module\ntypes.Module\ntypes.Module\ntypes.Module\ntypes.Module\n')
	assert res.output.contains('ast\ntypes.Type\nFile\n')
}

fn test_eval_transformed_array_parameter_keeps_array_shape() {
	res := run_eval_backend('
import v2.ast

fn stmt_list(stmts []ast.Stmt) {
	println(stmts.len)
}

fn main() {
	items := []ast.Stmt{}
	stmt_list(items)
}
	') or {
		panic(err)
	}
	if res.exit_code != 0 {
		panic(res.output)
	}
	assert res.output.contains('0\n')
}

fn test_eval_transformed_array_first_and_last() {
	res := run_eval_backend('
fn main() {
	items := [1, 2, 3]
	println(items.first())
	println(items.last())
}
	') or {
		panic(err)
	}
	if res.exit_code != 0 {
		panic(res.output)
	}
	assert res.output.contains('1\n3\n')
}

fn test_eval_prefix_bit_not() {
	mut e := create()
	e.run_text('
fn main() {
	println(~1)
}
') or { panic(err) }
	assert e.stdout() == '-2\n'
}

fn test_eval_token_and_byte_helpers() {
	mut e := create()
	e.run_text("
fn main() {
	for i := 0; i < 2; i++ {
		println(i)
	}
	println(r'raw')
	println(u8(`A`).is_alnum())
	println(u8(`A`).is_capital())
	println(u8(` `).is_space())
}
") or {
		panic(err)
	}
	assert e.stdout() == '0\n1\nraw\ntrue\ntrue\ntrue\n'
}

fn test_eval_string_int_conversion() {
	mut e := create()
	e.run_text("
fn main() {
	println('42'.int())
	println('84'.i64())
}
") or { panic(err) }
	assert e.stdout() == '42\n84\n'
}

@[noreturn]
fn skip_test(reason string) {
	println('skipping test, because ${reason} .')
	exit(0)
}
