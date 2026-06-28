import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_v3_review_checker() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_review_checker_regressions_test')
	build := os.execute('${vexe} -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn run_bad(v3_bin string, name string, src string, expected string) {
	bad_src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(bad_src, src) or { panic(err) }
	bad_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	result := os.execute('${v3_bin} ${bad_src} -b c -o ${bad_bin}')
	assert result.exit_code != 0, '${name}: expected failure, got success\n${result.output}'
	assert result.output.contains(expected), '${name}: expected `${expected}` in\n${result.output}'
	assert !result.output.contains('C compilation failed'), '${name}: reached C compilation\n${result.output}'
}

fn run_good(v3_bin string, name string, src string) string {
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

fn test_reject_pointer_expressions_for_value_returns() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_return_pointer_to_value',
		'fn f() int {\n\tx := 1\n\treturn &x\n}\nfn main() {}\n', 'cannot return `&int` as `int`')
	run_bad(v3_bin, 'bad_result_return_pointer_to_value',
		'fn f() !int {\n\tx := 1\n\treturn &x\n}\nfn main() {}\n', 'cannot return `&int` as `!int`')
}

fn test_restrict_synthetic_hex_fallback_receivers() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_struct_hex_method', 'struct S {}\nfn main() {\n\t_ := S{}.hex()\n}\n',
		'unknown function')
	run_bad(v3_bin, 'bad_int_array_hex_method', 'fn main() {\n\t_ := [1, 2].hex()\n}\n',
		'unknown function')
	run_bad(v3_bin, 'bad_map_hex_method',
		'fn main() {\n\tm := map[string]int{}\n\t_ := m.hex()\n}\n', 'unknown function')
	run_bad(v3_bin, 'bad_float_hex_method', 'fn main() {\n\t_ := f32(1.5).hex()\n}\n',
		'unknown function')
	run_bad(v3_bin, 'bad_pointer_hex_method',
		'fn main() {\n\tx := 1\n\tp := &x\n\t_ := p.hex()\n}\n', 'unknown function')
	out := run_good(v3_bin, 'supported_hex_methods',
		"fn main() {\n\tprintln(u8(15).hex())\n\tprintln(i64(255).hex())\n\tprintln([u8(1), 15, 255].hex())\n\tprintln(char(65).hex())\n\tprintln(`A`.hex())\n\tprintln('abc'.hex())\n}\n")
	assert out == '0f\nff\n010fff\n41\n41\n616263'
}

fn test_map_keys_and_values_reject_arguments() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_map_keys_arg',
		'fn main() {\n\tm := map[string]int{}\n\t_ := m.keys(123)\n}\n', 'argument count mismatch')
	run_bad(v3_bin, 'bad_map_values_arg',
		"fn main() {\n\tm := map[string]int{}\n\t_ := m.values('x')\n}\n",
		'argument count mismatch')
}

fn test_array_insert_and_prepend_reject_wrong_arity() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_array_prepend_missing_arg',
		'fn main() {\n\tmut a := [1, 2]\n\ta.prepend()\n}\n', 'argument count mismatch')
	run_bad(v3_bin, 'bad_array_prepend_extra_arg',
		'fn side_effect() int {\n\treturn 3\n}\nfn main() {\n\tmut a := [1, 2]\n\ta.prepend(0, side_effect())\n}\n',
		'argument count mismatch')
	run_bad(v3_bin, 'bad_array_insert_missing_arg',
		'fn main() {\n\tmut a := [1, 2]\n\ta.insert(0)\n}\n', 'argument count mismatch')
	run_bad(v3_bin, 'bad_array_insert_extra_arg',
		'fn side_effect() int {\n\treturn 3\n}\nfn main() {\n\tmut a := [1, 2]\n\ta.insert(0, 1, side_effect())\n}\n',
		'argument count mismatch')
}

fn test_comptime_if_selected_bodies_are_checked() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_concrete_comptime_if_selected_call',
		'fn main() {\n\t$if int is int {\n\t\tmissing_selected_symbol()\n\t}\n}\n',
		'unknown function `missing_selected_symbol`')
	run_bad(v3_bin, 'bad_generic_comptime_if_preserved_call',
		'fn f[T]() {\n\t$if T is int {\n\t\tmissing_generic_symbol()\n\t}\n}\nfn main() {\n\tf[int]()\n}\n',
		'unknown function `missing_generic_symbol`')
}

fn test_explicit_generic_calls_use_all_type_arguments() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_multi_explicit_generic_call',
		"struct Pair[A, B] {\n\tleft  A\n\tright B\n}\n\nfn make_pair[A, B]() Pair[A, B] {\n\treturn Pair[A, B]{}\n}\n\nfn expect_pair(p Pair[int, string]) string {\n\treturn 'ok'\n}\n\nfn main() {\n\tp := make_pair[int, string]()\n\tprintln(expect_pair(p))\n}\n")
	assert out == 'ok'
	nested := run_good(v3_bin, 'good_nested_explicit_generic_call',
		"struct Pair[A, B] {\n\tleft  A\n\tright B\n}\n\nstruct Box[T] {\n\tvalue T\n}\n\nfn wrap[T]() Box[T] {\n\treturn Box[T]{}\n}\n\nfn expect_box(b Box[Pair[int, string]]) string {\n\treturn 'ok'\n}\n\nfn main() {\n\tb := wrap[Pair[int, string]]()\n\tprintln(expect_box(b))\n}\n")
	assert nested == 'ok'
}

fn test_reject_escaping_capturing_fn_literals() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_return_capturing_fn_literal',
		'fn make(x int) fn () int {\n\treturn fn [x] () int {\n\t\treturn x\n\t}\n}\nfn main() {}\n',
		'capturing fn literal cannot be stored or returned')
	run_bad(v3_bin, 'bad_store_capturing_fn_literal_alias',
		'fn main() {\n\tx := 1\n\tf := fn [x] () int {\n\t\treturn x\n\t}\n\tmut cbs := []fn () int{}\n\tcbs << f\n}\n',
		'capturing fn literal cannot be stored or returned')
}

fn test_generic_functions_report_missing_return() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_generic_missing_return', 'fn f[T]() int {\n}\nfn main() {}\n',
		'missing return at end of function `f`')
}

fn test_local_identifiers_shadow_module_consts() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_const_shadowed_by_param_and_local',
		"const shadowed_value = 'const'\n\nfn param_shadow(shadowed_value int) int {\n\treturn shadowed_value + 1\n}\n\nfn local_shadow() int {\n\tshadowed_value := 2\n\treturn shadowed_value + 1\n}\n\nfn main() {\n\tprintln(int_str(param_shadow(1)))\n\tprintln(int_str(local_shadow()))\n\tprintln(shadowed_value)\n}\n")
	assert out == '2\n3\nconst'
}
