import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_v3_review_cgen() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_review_cgen_regressions_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn review_cgen_write_project_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn review_cgen_run_good(v3_bin string, name string, src string) string {
	good_src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(good_src, src) or { panic(err) }
	good_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	compile := os.execute('${v3_bin} ${good_src} -b c -o ${good_bin}')
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed\n${compile.output}'
	run := os.execute(good_bin)
	assert run.exit_code == 0, '${name}: run failed\n${run.output}'
	return run.output.trim_space()
}

fn review_cgen_run_good_project(v3_bin string, name string, files map[string]string, input string) string {
	root := os.join_path(os.temp_dir(), 'v3_${name}_project')
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	for rel, src in files {
		review_cgen_write_project_file(root, rel, src)
	}
	input_path := if input.len == 0 { root } else { os.join_path(root, input) }
	good_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	compile := os.execute('${v3_bin} ${input_path} -b c -o ${good_bin}')
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed\n${compile.output}'
	run := os.execute(good_bin)
	assert run.exit_code == 0, '${name}: run failed\n${run.output}'
	return run.output.trim_space()
}

fn review_cgen_run_bad_project(v3_bin string, name string, files map[string]string, input string, expected string) {
	root := os.join_path(os.temp_dir(), 'v3_${name}_project')
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	for rel, src in files {
		review_cgen_write_project_file(root, rel, src)
	}
	input_path := if input.len == 0 { root } else { os.join_path(root, input) }
	bad_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	result := os.execute('${v3_bin} ${input_path} -b c -o ${bad_bin}')
	assert result.exit_code != 0, '${name}: expected failure, got success\n${result.output}'
	assert result.output.contains(expected), '${name}: expected `${expected}` in\n${result.output}'
	assert !result.output.contains('C compilation failed'), '${name}: reached C compilation\n${result.output}'
}

fn test_module_qualified_free_call_is_not_rewritten_to_array_intrinsic() {
	v3_bin := build_v3_review_cgen()
	out := review_cgen_run_good_project(v3_bin, 'module_qualified_free_call', {
		'v.mod':  'Module { name: "module_qualified_free_call" }\n'
		'main.v': 'module main

import m

fn main() {
	values := [2, 3]
	println(int_str(m.free(values)))
}
'
		'm/m.v':  'module m

pub fn free(values []int) int {
	return values[0] + values.len
}
'
	}, 'main.v')
	assert out == '4'
}

fn test_selectively_imported_free_call_is_not_rewritten_to_array_intrinsic() {
	v3_bin := build_v3_review_cgen()
	out := review_cgen_run_good_project(v3_bin, 'selective_import_free_call', {
		'v.mod':  'Module { name: "selective_import_free_call" }\n'
		'main.v': 'module main

import m { free }

fn main() {
	values := [3, 4]
	println(int_str(free(values)))
}
'
		'm/m.v':  'module m

pub fn free(values []int) int {
	return values[0] + values.len
}
'
	}, 'main.v')
	assert out == '5'
}

fn test_sum_container_variant_import_alias_collision_uses_exact_tag() {
	v3_bin := build_v3_review_cgen()
	out := review_cgen_run_good_project(v3_bin, 'sum_container_variant_alias_collision', {
		'v.mod':            'Module { name: "sum_container_variant_alias_collision" }\n'
		'other/value.v':    'module other\n\npub struct Value {\npub:\n\tn int\n}\n'
		'sub/tast/value.v': 'module tast\n\npub struct Value {\npub:\n\tn int\n}\n'
		'main.v':           'module main\n\nimport other\nimport sub.tast as tast\n\ntype Mixed = map[string]other.Value | map[string]tast.Value\n\nfn score(value Mixed) int {\n\tif value is map[string]tast.Value {\n\t\titems := value as map[string]tast.Value\n\t\treturn items["x"].n\n\t}\n\treturn -1\n}\n\nfn main() {\n\tvalue := Mixed(map[string]tast.Value{\n\t\t"x": tast.Value{\n\t\t\tn: 71\n\t\t}\n\t})\n\tprintln(int_str(score(value)))\n}\n'
	}, 'main.v')
	assert out == '71'
}

fn test_sum_name_import_alias_collision_uses_exact_sum() {
	v3_bin := build_v3_review_cgen()
	out := review_cgen_run_good_project(v3_bin, 'sum_name_import_alias_collision', {
		'v.mod':         'Module { name: "sum_name_import_alias_collision" }\n'
		'a/tast/tast.v': 'module tast\n\npub struct Target {}\npub struct Second {}\npub type Value = Target | Second\n'
		'b/tast/tast.v': 'module tast\n\npub struct First {}\npub struct Target {\npub:\n\tn int\n}\npub type Value = First | Target\n'
		'main.v':        'module main\n\nimport a.tast as other_tast\nimport b.tast as tast\n\nfn score(value tast.Value) int {\n\tif value is tast.Target {\n\t\ttarget := value as tast.Target\n\t\treturn target.n\n\t}\n\treturn -1\n}\n\nfn main() {\n\t_ := other_tast.Value(other_tast.Target{})\n\tvalue := tast.Value(tast.Target{\n\t\tn: 79\n\t})\n\tprintln(int_str(score(value)))\n}\n'
	}, 'main.v')
	assert out == '79'
}

fn test_user_defined_free_method_is_preserved() {
	v3_bin := build_v3_review_cgen()
	out := review_cgen_run_good(v3_bin, 'user_defined_free_method', 'struct Handle {
mut:
	freed bool
}

fn (mut h Handle) free() {
	h.freed = true
	println("freed")
}

fn main() {
	mut h := Handle{}
	h.free()
}
')
	assert out == 'freed'
}

fn test_pointer_receiver_on_pointer_parameter_passes_parameter_directly() {
	v3_bin := build_v3_review_cgen()
	out := review_cgen_run_good(v3_bin, 'pointer_receiver_on_pointer_param', 'struct S {
	n int
}

fn (s &S) value_plus(delta int) int {
	return s.n + delta
}

fn use_pointer_param(s &S) int {
	return s.value_plus(5)
}

fn main() {
	s := S{
		n: 37
	}
	println(int_str(use_pointer_param(&s)))
}
')
	assert out == '42'
}

fn test_mut_receiver_return_self_pointer_uses_receiver_pointer() {
	v3_bin := build_v3_review_cgen()
	out := review_cgen_run_good(v3_bin, 'mut_receiver_return_self_pointer', 'struct Command {
mut:
	args []string
}

fn (mut cmd Command) arg(arg string) &Command {
	cmd.args << arg
	return &cmd
}

fn (mut cmd Command) count() int {
	return cmd.args.len
}

fn main() {
	mut cmd := Command{}
	println(int_str(cmd.arg("a").arg("b").count()))
}
')
	assert out == '2'
}

fn test_pointer_value_compatibility_keeps_qualified_struct_names() {
	v3_bin := build_v3_review_cgen()
	files := {
		'v.mod': 'Module { name: "pointer_value_qualified_names" }\n'
		'a/a.v': 'module a

pub struct Foo {
pub:
	value int
}

pub fn make() &Foo {
	return &Foo{
		value: 7
	}
}
'
		'b/b.v': 'module b

pub struct Foo {
pub:
	value int
}
'
	}
	mut return_files := files.clone()
	return_files['main.v'] = 'module main

import a
import b

fn f() b.Foo {
	return a.make()
}

fn main() {}
'
	review_cgen_run_bad_project(v3_bin, 'bad_pointer_value_cross_module_return', return_files,
		'main.v', 'cannot return')
	mut field_files := files.clone()
	field_files['main.v'] = 'module main

import a
import b

struct Holder {
	item b.Foo
}

fn make_holder() Holder {
	return Holder{
		item: a.make()
	}
	}

fn main() {}
'
	review_cgen_run_bad_project(v3_bin, 'bad_pointer_value_cross_module_field', field_files,
		'main.v', 'cannot initialize field')
}

fn test_optional_single_letter_struct_keeps_concrete_payload_type() {
	v3_bin := build_v3_review_cgen()
	out := review_cgen_run_good(v3_bin, 'optional_single_letter_struct', 'struct T {
	value int
}

fn maybe_t(ok bool) ?T {
	if !ok {
		return none
	}
	return T{
		value: 7
	}
}

fn result_t() !T {
	return T{
		value: 8
	}
}

fn main() {
	a := maybe_t(true) or { T{} }
	b := result_t() or { T{} }
	println(int_str(a.value + b.value))
}
')
	assert out == '15'
}
