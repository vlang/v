import os
import v.tempname

fn option_arg_workspace(name string) (string, string) {
	token := tempname.unique_token()
	dir := os.join_path(os.vtmp_dir(), 'v3_option_arg_${token}')
	os.mkdir(dir) or { panic(err) }
	// Retained C files use the binary basename in the shared V temp directory.
	bin := os.join_path(dir, 'v3_${name}_${token}') + $if windows { '.exe' } $else { '' }
	return dir, bin
}

fn option_arg_compile(input string, bin string) string {
	compile := os.exec([@VEXE, '-b', 'c', '-o', bin, '-keepc', input])
	tmp := os.vtmp_dir()
	prefix := os.file_name(bin) + '.'
	retained := (os.ls(tmp) or { panic(err) }).filter(it.starts_with(prefix) && it.ends_with('.tmp.c'))
	defer {
		for filename in retained {
			os.rm(os.join_path(tmp, filename)) or {}
		}
	}
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	assert retained.len == 1, 'expected one retained C file for ${bin}, got ${retained}'
	return os.read_file(os.join_path(tmp, retained[0])) or { panic(err) }
}

fn option_arg_compile_and_run(name string, source string) (string, string) {
	dir, bin := option_arg_workspace(name)
	defer { os.rmdir_all(dir) or {} }
	src := os.join_path(dir, 'v3_${name}.v')
	os.write_file(src, source) or { panic(err) }
	c_source := option_arg_compile(src, bin)
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	return run.output.trim_space(), c_source
}

struct OptionArgFixture {
	name     string
	source   string
	expected string
}

fn option_arg_compile_batch(name string, cases []OptionArgFixture) string {
	dir, bin := option_arg_workspace(name)
	defer { os.rmdir_all(dir) or {} }
	mut imports := []string{cap: cases.len}
	mut calls := []string{cap: cases.len}
	for case in cases {
		module_dir := os.join_path(dir, case.name)
		os.mkdir(module_dir) or { panic(err) }
		imports << 'import ${case.name}'
		calls << "\t\t'${case.name}' { ${case.name}.exercise() }"
		source := 'module ${case.name}\n\n' + case.source
		os.write_file(os.join_path(module_dir, 'case.v'), source.replace_once('\nfn main()',
			'\npub fn exercise()')) or { panic(err) }
	}
	os.write_file(os.join_path(dir, 'main.v'), "module main\n\n${imports.join('\n')}\n\nfn main() {\n\tmatch arguments()[1] {\n${calls.join('\n')}\n\t\telse { panic('unknown fixture') }\n\t}\n}\n") or {
		panic(err)
	}
	c_source := option_arg_compile(dir, bin)
	for case in cases {
		run := os.exec([bin, case.name])
		assert run.exit_code == 0, '${case.name}: ${run.output}'
		assert run.output.trim_space() == case.expected, '${case.name}: expected `${case.expected}` in\n${run.output}'
	}
	return c_source
}

fn option_arg_c_case(generated string, name string) string {
	header := '\nvoid ${name}__exercise(void) {\n'
	start := generated.index(header) or { panic('missing C entry for ${name}') }
	body := generated[start + header.len..]
	end := body.index('\n}\n') or { panic('missing end of C entry for ${name}') }
	return body[..end]
}

fn option_arg_generated_c(name string, source string) string {
	dir := os.join_path(os.vtmp_dir(), 'v3_option_arg_${tempname.unique_token()}')
	os.mkdir(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	base := os.join_path(dir, 'v3_${name}')
	src := base + '.v'
	os.write_file(src, source) or { panic(err) }
	c_path := base + '.c'
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', c_path])
	assert compile.exit_code == 0, compile.output
	return os.read_file(c_path) or { panic(err) }
}

fn test_optional_arguments_and_payloads() {
	c_code := option_arg_compile_batch('optional_arguments_and_payloads', [
		OptionArgFixture{'option_arg_codegen_input', "struct Info {\n\tvalue int\n}\n\nfn pass_int(x ?int) int {\n\treturn 1\n}\n\nfn pass_info(x ?Info) int {\n\treturn 1\n}\n\nfn main() {\n\tassert pass_int(3) == 1\n\tassert pass_int(none) == 1\n\tassert pass_info(Info{value: 4}) == 1\n\tassert pass_info(none) == 1\n\tprintln('ok')\n}\n", 'ok'},
		OptionArgFixture{'optional_literal_expected_wrapper', 'fn take(x ?i64) i64 {
	return x or { -1 }
}

fn main() {
	assert take(?int(1)) == 1
	println("ok")
}
', 'ok'},
		OptionArgFixture{'optional_reference_string_interpolation', "fn stringify(value ?int) string {
	return '\${&value}'
}

fn main() {
	println(stringify(?int(42)))
	println(stringify(?int(none)))
}
", '&Option(42)\n&Option(&nil)'},
		OptionArgFixture{'optional_params_field_result_wrapper', 'enum Filter {
	linear
}

@[params]
struct Config {
	filter ?Filter
}

struct Image {}

fn create(cfg Config) !Image {
	filter := cfg.filter or { return error("missing filter") }
	if filter != .linear {
		return error("wrong filter")
	}
	return Image{}
}

fn wrapper(filter Filter) !Image {
	return create(filter: filter)!
}

fn main() {
	_ := wrapper(.linear) or { panic(err) }
	println("ok")
}
', 'ok'},
		OptionArgFixture{'optional_generic_concrete_abi_run', 'fn maybe() ?int {\n\treturn 5\n}\n\nfn maybe_or_none(flag bool) ?int {\n\tif flag {\n\t\treturn 6\n\t}\n\treturn none\n}\n\nfn take[T](x ?T, fallback T) T {\n\treturn x or { fallback }\n}\n\nfn main() {\n\tx := maybe()\n\tassert take[int](maybe(), 0) == 5\n\tassert take[int](x, 0) == 5\n\tassert take[int](maybe_or_none(false), 9) == 9\n\tprintln("ok")\n}\n', 'ok'},
		OptionArgFixture{'optional_if_expr_codegen_input', "fn none_int() ?int {\n\treturn none\n}\n\nfn some_int() ?int {\n\treturn 7\n}\n\nfn choose(flag bool) ?int {\n\tvalue := if flag {\n\t\tnone_int()\n\t} else {\n\t\tsome_int()\n\t}\n\treturn value\n}\n\nfn main() {\n\tgood := choose(false) or { 0 }\n\tnone_value := choose(true) or { -1 }\n\tassert good == 7\n\tassert none_value == -1\n\tprintln('ok')\n}\n", 'ok'},
		OptionArgFixture{'nested_local_optional_primitive_typedef_input', "fn main() {\n\txs := [?f32(1.5)]\n\tassert xs.len == 1\n\tprintln('ok')\n}\n", 'ok'},
		OptionArgFixture{'optional_clone_return_codegen_input', "fn clone_array(src []int) ?[]int {\n\treturn src.clone()\n}\n\nfn clone_array_defer(src []int) ?[]int {\n\tdefer {}\n\treturn src.clone()\n}\n\nfn clone_map(src map[string]int) ?map[string]int {\n\treturn src.clone()\n}\n\nfn clone_map_defer(src map[string]int) ?map[string]int {\n\tdefer {}\n\treturn src.clone()\n}\n\nfn array_sum(values []int) int {\n\tmut total := 0\n\tfor value in values {\n\t\ttotal += value\n\t}\n\treturn total\n}\n\nfn main() {\n\tmut values := []int{}\n\tvalues << 2\n\tvalues << 5\n\tarr := clone_array(values) or { []int{} }\n\tarr_defer := clone_array_defer(values) or { []int{} }\n\tmut lookup := map[string]int{}\n\tlookup['a'] = 3\n\tlookup['b'] = 4\n\tcloned := clone_map(lookup) or { map[string]int{} }\n\tcloned_defer := clone_map_defer(lookup) or { map[string]int{} }\n\tassert array_sum(arr) == 7\n\tassert array_sum(arr_defer) == 7\n\tassert cloned['a'] + cloned['b'] == 7\n\tassert cloned_defer['a'] + cloned_defer['b'] == 7\n\tprintln('ok')\n}\n", 'ok'},
	])
	literal := option_arg_c_case(c_code, 'optional_literal_expected_wrapper')
	assert literal.contains('optional_literal_expected_wrapper__take((__v_option_i64){.ok = true, .value = 1})'), literal

	prefix := 'optional_generic_concrete_abi_run__'
	take := if c_code.contains('${prefix}take_T_v_int(') {
		'${prefix}take_T_v_int'
	} else {
		'${prefix}take_T_int'
	}
	assert c_code.contains('i64 ${take}(__v_option_i64 x, i64 fallback)'), c_code
	// A plain ?int and a specialized ?T with T = int share the same C wrapper.
	assert !c_code.contains('__v_option_int'), c_code
	concrete := option_arg_c_case(c_code, 'optional_generic_concrete_abi_run')
	assert concrete.contains('${take}(${prefix}maybe(), 0)'), concrete
	assert concrete.contains('${take}(x, 0)'), concrete
}

fn test_ierror_arguments_and_payloads() {
	c_code := option_arg_compile_batch('ierror_arguments_and_payloads', [
		OptionArgFixture{'error_call_argument_expected_ierror', "fn wrap(err IError) string {\n\treturn err.msg()\n}\n\nfn f() !string {\n\treturn wrap(error('x'))\n}\n\nfn main() {\n\ts := f() or { '' }\n\tassert s == 'x'\n\tprintln('ok')\n}\n", 'ok'},
		OptionArgFixture{'selector_named_error_expected_ierror', "struct Stmt {}

fn (stmt Stmt) error(code int) IError {
	return error('stmt:' + int_str(code))
}

fn wrap(err IError) string {
	return err.msg()
}

fn direct(stmt Stmt) IError {
	return stmt.error(3)
}

fn result(stmt Stmt) !int {
	return stmt.error(7)
}

fn main() {
	stmt := Stmt{}
	assert wrap(stmt.error(4)) == 'stmt:4'
	err := direct(stmt)
	assert err.msg() == 'stmt:3'
	result(stmt) or {
		assert err.msg() == 'stmt:7'
		println('ok')
		return
	}
	assert false
}
", 'ok'},
		OptionArgFixture{'ierror_static_method_param_msg', "struct CommandError {\n\tmessage string\n}\n\nfn CommandError.io(ioerr IError) CommandError {\n\treturn CommandError{\n\t\tmessage: ioerr.msg()\n\t}\n}\n\nfn (err CommandError) msg() string {\n\treturn err.message\n}\n\nfn (err CommandError) code() int {\n\treturn 0\n}\n\nfn main() {\n\terr := CommandError.io(error('ok'))\n\tassert err.msg() == 'ok'\n\tprintln('ok')\n}\n", 'ok'},
		OptionArgFixture{'ierror_as_expr_unbox_payload', "struct InvalidPatternError {\n\tvalid_up_to int\n}\n\nfn (err InvalidPatternError) msg() string {\n\treturn 'invalid'\n}\n\nfn (err InvalidPatternError) code() int {\n\treturn 0\n}\n\nfn fail() !string {\n\treturn InvalidPatternError{\n\t\tvalid_up_to: 3\n\t}\n}\n\nfn main() {\n\tfail() or {\n\t\tperr := err as InvalidPatternError\n\t\tassert perr.valid_up_to == 3\n\t\tprintln('ok')\n\t\treturn\n\t}\n\tpanic('expected error')\n}\n", 'ok'},
	])
	assert c_code.contains('error_call_argument_expected_ierror__wrap((IError){'), c_code
	assert !c_code.contains('error_call_argument_expected_ierror__wrap((__v_option_string)'), c_code

	payload := option_arg_c_case(c_code, 'ierror_as_expr_unbox_payload')
	assert payload.contains('._object'), payload
	assert !payload.contains('ierror_as_expr_unbox_payload__InvalidPatternError perr = err;'), payload
}

fn test_optional_abi_distinguishes_plain_t_name_from_specialized_generic() {
	c_code := option_arg_generated_c('optional_plain_t_name_abi', 'fn plain[T](x T) T {\n\treturn x\n}\n\nfn plain_t_name(x ?int) int {\n\treturn x or { 0 }\n}\n\nfn maybe() ?int {\n\treturn 3\n}\n\nfn use_fn(f fn (?int) int) int {\n\treturn f(maybe())\n}\n\nfn take[T](x ?T, fallback T) T {\n\treturn x or { fallback }\n}\n\nfn main() {\n\tprintln(plain_t_name(maybe()) + use_fn(plain_t_name) + take[int](7, 0) + plain[int](4))\n}\n')
	// With platform `int` lowered to `i64`, `?int` and `?i64` share the
	// `__v_option_i64` representation, so plain and generic-specialized `?int` uses
	// converge on it (the fn-pointer ABI stays consistent because every `?int`
	// site now spells the same optional type).
	assert c_code.contains('i64 plain_t_name(__v_option_i64 x)'), c_code
	assert !c_code.contains('__v_option_int'), c_code
	assert c_code.contains(')(struct __v_option_i64);'), c_code
	assert c_code.contains('plain_T_v_int(') || c_code.contains('plain_T_int('), c_code
	assert c_code.contains('i64 take_T_v_int(__v_option_i64 x, i64 fallback)')
		|| c_code.contains('i64 take_T_int(__v_option_i64 x, i64 fallback)'), c_code
	assert c_code.contains('(__v_option_i64){.ok = true, .value = 7}'), c_code
}

fn test_result_multi_return_declares_optional_wrapper_before_use() {
	source := 'struct Ticket {
	value int
}

struct App {}

fn (app &App) state() !(int, Ticket) {
	_ = app
	return 1, Ticket{
		value: 41
	}
}

fn main() {
	app := App{}
	stage, ticket := app.state() or { panic(err) }
	assert stage + ticket.value == 42
	println("ok")
}
'
	out, c_code := option_arg_compile_and_run('result_multi_return_optional_wrapper', source)
	assert out == 'ok'
	// A `!` multi-return is a Result, so its wrapper is `__v_result_...`; it must be
	// declared before the first function that returns it.
	mut wrapper := 'multi_return_i64_main__Ticket'
	if !c_code.contains('__v_result_${wrapper}') {
		wrapper = 'multi_return_i32_main__Ticket'
	}
	typedef_pos := c_code.index('typedef struct __v_result_${wrapper} {') or { -1 }
	use_pos := c_code.index('__v_result_${wrapper} App__state(') or { -1 }
	assert typedef_pos >= 0, c_code
	assert use_pos > typedef_pos, c_code
}
