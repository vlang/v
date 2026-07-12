import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

// build_v3 builds v3 data for v3 tests.
fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_option_arg_codegen_test')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// run_good supports run good handling for v3 tests.
fn run_good(v3_bin string, name string, source string) string {
	src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_${name}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0
	assert !compile.output.contains('C compilation failed')

	run := os.execute(bin)
	assert run.exit_code == 0
	return run.output.trim_space()
}

fn generated_c(v3_bin string, name string, source string) string {
	src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(src, source) or { panic(err) }
	c_path := os.join_path(os.temp_dir(), 'v3_${name}.c')
	compile := os.execute('${v3_bin} ${src} -b c -o ${c_path}')
	assert compile.exit_code == 0, compile.output
	return os.read_file(c_path) or { panic(err) }
}

// test_optional_argument_codegen_wraps_values_and_none validates this v3 regression case.
fn test_optional_argument_codegen_wraps_values_and_none() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'option_arg_codegen_input',
		"struct Info {\n\tvalue int\n}\n\nfn pass_int(x ?int) int {\n\treturn 1\n}\n\nfn pass_info(x ?Info) int {\n\treturn 1\n}\n\nfn main() {\n\tassert pass_int(3) == 1\n\tassert pass_int(none) == 1\n\tassert pass_info(Info{value: 4}) == 1\n\tassert pass_info(none) == 1\n\tprintln('ok')\n}\n")
	assert out == 'ok'
}

fn test_error_call_argument_expected_ierror_not_result_wrapper() {
	v3_bin := build_v3()
	source := "fn wrap(err IError) string {\n\treturn err.msg()\n}\n\nfn f() !string {\n\treturn wrap(error('x'))\n}\n\nfn main() {\n\ts := f() or { '' }\n\tassert s == 'x'\n\tprintln('ok')\n}\n"
	out := run_good(v3_bin, 'error_call_argument_expected_ierror', source)
	assert out == 'ok'
	c_code := generated_c(v3_bin, 'error_call_argument_expected_ierror_c', source)
	assert c_code.contains('wrap((IError){._typ = 0'), c_code
	assert !c_code.contains('wrap((Optional_string)'), c_code
}

fn test_ierror_parameter_method_call_inside_static_method_uses_interface_method() {
	v3_bin := build_v3()
	source := "struct CommandError {\n\tmessage string\n}\n\nfn CommandError.io(ioerr IError) CommandError {\n\treturn CommandError{\n\t\tmessage: ioerr.msg()\n\t}\n}\n\nfn (err CommandError) msg() string {\n\treturn err.message\n}\n\nfn (err CommandError) code() int {\n\treturn 0\n}\n\nfn main() {\n\terr := CommandError.io(error('ok'))\n\tassert err.msg() == 'ok'\n\tprintln('ok')\n}\n"
	out := run_good(v3_bin, 'ierror_static_method_param_msg', source)
	assert out == 'ok'
}

fn test_ierror_as_expr_unboxes_concrete_payload() {
	v3_bin := build_v3()
	source := "struct InvalidPatternError {\n\tvalid_up_to int\n}\n\nfn (err InvalidPatternError) msg() string {\n\treturn 'invalid'\n}\n\nfn (err InvalidPatternError) code() int {\n\treturn 0\n}\n\nfn fail() !string {\n\treturn InvalidPatternError{\n\t\tvalid_up_to: 3\n\t}\n}\n\nfn main() {\n\tfail() or {\n\t\tperr := err as InvalidPatternError\n\t\tassert perr.valid_up_to == 3\n\t\tprintln('ok')\n\t\treturn\n\t}\n\tpanic('expected error')\n}\n"
	out := run_good(v3_bin, 'ierror_as_expr_unbox_payload', source)
	assert out == 'ok'
	c_code := generated_c(v3_bin, 'ierror_as_expr_unbox_payload_c', source)
	assert c_code.contains('._object'), c_code
	assert !c_code.contains('InvalidPatternError perr = err;'), c_code
}

fn test_optional_abi_distinguishes_plain_t_name_from_specialized_generic() {
	v3_bin := build_v3()
	c_code := generated_c(v3_bin, 'optional_plain_t_name_abi',
		'fn plain[T](x T) T {\n\treturn x\n}\n\nfn plain_T_name(x ?int) int {\n\treturn x or { 0 }\n}\n\nfn maybe() ?int {\n\treturn 3\n}\n\nfn use_fn(f fn (?int) int) int {\n\treturn f(maybe())\n}\n\nfn take[T](x ?T, fallback T) T {\n\treturn x or { fallback }\n}\n\nfn main() {\n\tprintln(plain_T_name(maybe()) + use_fn(plain_T_name) + take[int](7, 0) + plain[int](4))\n}\n')
	assert c_code.contains('int plain_T_name(Optional x)'), c_code
	assert !c_code.contains('int plain_T_name(Optional_int x)'), c_code
	assert c_code.contains(')(struct Optional);'), c_code
	assert !c_code.contains(')(Optional_int);'), c_code
	assert c_code.contains('plain_T_v_int(') || c_code.contains('plain_T_int('), c_code
	assert c_code.contains('int take_T_v_int(Optional_int x, int fallback)')
		|| c_code.contains('int take_T_int(Optional_int x, int fallback)'), c_code
	assert c_code.contains('(Optional_int){.ok = true, .value = 7}'), c_code
}

fn test_optional_generic_concrete_abi_converts_optional_args() {
	v3_bin := build_v3()
	source := 'fn maybe() ?int {\n\treturn 5\n}\n\nfn maybe_or_none(flag bool) ?int {\n\tif flag {\n\t\treturn 6\n\t}\n\treturn none\n}\n\nfn take[T](x ?T, fallback T) T {\n\treturn x or { fallback }\n}\n\nfn main() {\n\tx := maybe()\n\tassert take[int](maybe(), 0) == 5\n\tassert take[int](x, 0) == 5\n\tassert take[int](maybe_or_none(false), 9) == 9\n\tprintln("ok")\n}\n'
	out := run_good(v3_bin, 'optional_generic_concrete_abi_run', source)
	assert out == 'ok'
	c_code := generated_c(v3_bin, 'optional_generic_concrete_abi_c', source)
	assert c_code.contains('int take_T_v_int(Optional_int x, int fallback)')
		|| c_code.contains('int take_T_int(Optional_int x, int fallback)'), c_code
	assert c_code.contains('Optional _opt'), c_code
	assert c_code.contains('(Optional_int){.ok = true, .value = _opt'), c_code
	assert !c_code.contains('take_T_v_int(maybe(), 0)'), c_code
	assert !c_code.contains('take_T_int(maybe(), 0)'), c_code
	assert !c_code.contains('take_T_v_int(x, 0)'), c_code
	assert !c_code.contains('take_T_int(x, 0)'), c_code
}

// test_optional_if_expr_codegen_initializes_optional_temp validates this v3 regression case.
fn test_optional_if_expr_codegen_initializes_optional_temp() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'optional_if_expr_codegen_input',
		"fn none_int() ?int {\n\treturn none\n}\n\nfn some_int() ?int {\n\treturn 7\n}\n\nfn choose(flag bool) ?int {\n\tvalue := if flag {\n\t\tnone_int()\n\t} else {\n\t\tsome_int()\n\t}\n\treturn value\n}\n\nfn main() {\n\tgood := choose(false) or { 0 }\n\tnone_value := choose(true) or { -1 }\n\tassert good == 7\n\tassert none_value == -1\n\tprintln('ok')\n}\n")
	assert out == 'ok'
}

// test_nested_local_optional_primitive_typedef validates this v3 regression case.
fn test_nested_local_optional_primitive_typedef() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'nested_local_optional_primitive_typedef_input',
		"fn main() {\n\txs := [?f32(1.5)]\n\tassert xs.len == 1\n\tprintln('ok')\n}\n")
	assert out == 'ok'
}

// test_optional_clone_return_wraps_payload validates this v3 regression case.
fn test_optional_clone_return_wraps_payload() {
	v3_bin := build_v3()
	out := run_good(v3_bin, 'optional_clone_return_codegen_input',
		"fn clone_array(src []int) ?[]int {\n\treturn src.clone()\n}\n\nfn clone_array_defer(src []int) ?[]int {\n\tdefer {}\n\treturn src.clone()\n}\n\nfn clone_map(src map[string]int) ?map[string]int {\n\treturn src.clone()\n}\n\nfn clone_map_defer(src map[string]int) ?map[string]int {\n\tdefer {}\n\treturn src.clone()\n}\n\nfn array_sum(values []int) int {\n\tmut total := 0\n\tfor value in values {\n\t\ttotal += value\n\t}\n\treturn total\n}\n\nfn main() {\n\tmut values := []int{}\n\tvalues << 2\n\tvalues << 5\n\tarr := clone_array(values) or { []int{} }\n\tarr_defer := clone_array_defer(values) or { []int{} }\n\tmut lookup := map[string]int{}\n\tlookup['a'] = 3\n\tlookup['b'] = 4\n\tcloned := clone_map(lookup) or { map[string]int{} }\n\tcloned_defer := clone_map_defer(lookup) or { map[string]int{} }\n\tassert array_sum(arr) == 7\n\tassert array_sum(arr_defer) == 7\n\tassert cloned['a'] + cloned['b'] == 7\n\tassert cloned_defer['a'] + cloned_defer['b'] == 7\n\tprintln('ok')\n}\n")
	assert out == 'ok'
}
