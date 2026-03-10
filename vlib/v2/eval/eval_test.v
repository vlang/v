module eval

import os
import v2.parser
import v2.pref
import v2.token

fn run_eval_backend(code string) !os.Result {
	vexe := os.getenv('VEXE')
	if vexe == '' {
		return error('VEXE is not set')
	}
	vroot := os.dir(vexe)
	tmp_dir := os.join_path(os.temp_dir(), 'v2_eval_integration_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	tmp_file := os.join_path(tmp_dir, 'sample.v')
	v2_exe := os.join_path(tmp_dir, 'v2_eval_runner')
	os.write_file(tmp_file, code)!
	build_res := os.execute('${os.quoted_path(vexe)} -o ${os.quoted_path(v2_exe)} ${os.quoted_path(os.join_path(vroot,
		'cmd', 'v2', 'v2.v'))}')
	if build_res.exit_code != 0 {
		return error(build_res.output)
	}
	return os.execute('${os.quoted_path(v2_exe)} -backend eval ${os.quoted_path(tmp_file)}')
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
	type_files := os.walk_ext(os.join_path(@VMODROOT, 'vlib', 'v2', 'types'), '.v').filter(!it.ends_with('_test.v'))
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
	type_files := os.walk_ext(os.join_path(@VMODROOT, 'vlib', 'v2', 'types'), '.v').filter(!it.ends_with('_test.v'))
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
