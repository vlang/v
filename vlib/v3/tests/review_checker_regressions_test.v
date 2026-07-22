import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_v3_review_checker() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_review_checker_regressions_test')
	build :=
		os.execute('${vexe} -gc none -prealloc -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
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

fn run_runtime_bad(v3_bin string, name string, src string) string {
	bad_src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(bad_src, src) or { panic(err) }
	bad_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	compile := os.execute('${v3_bin} ${bad_src} -b c -o ${bad_bin}')
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed\n${compile.output}'
	run := os.execute(bad_bin)
	assert run.exit_code != 0, '${name}: expected runtime failure, got success\n${run.output}'
	return run.output.trim_space()
}

fn test_reject_pointer_expressions_for_value_returns() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_return_pointer_to_value',
		'fn f() int {\n\tx := 1\n\treturn &x\n}\nfn main() {}\n', 'cannot return `&int` as `int`')
	run_bad(v3_bin, 'bad_result_return_pointer_to_value',
		'fn f() !int {\n\tx := 1\n\treturn &x\n}\nfn main() {}\n', 'cannot return `&int` as `!int`')
	run_bad(v3_bin, 'bad_field_pointer_to_value',
		'struct S {\n\tx int\n}\n\nfn main() {\n\tx := 1\n\t_ := S{\n\t\tx: &x\n\t}\n}\n',
		'cannot initialize field `x` with `&int`; expected `int`')
}

fn test_reject_cross_wrapper_option_result_returns() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_result_value_in_option_return',
		'fn make_result() !int {\n\treturn 7\n}\n\nfn make_option() ?int {\n\treturn make_result()\n}\n\nfn main() {}\n',
		'cannot return `!int` as `?int`')
	run_bad(v3_bin, 'bad_result_value_in_optional_pointer_return',
		'struct Item {}\n\nfn convert(res !Item) ?&Item {\n\treturn res\n}\n\nfn main() {}\n',
		'cannot return `!Item` as `?&Item`')
	run_bad(v3_bin, 'bad_option_value_in_result_pointer_return',
		'struct Item {}\n\nfn convert(opt ?Item) !&Item {\n\treturn opt\n}\n\nfn main() {}\n',
		'cannot return `?Item` as `&Item`')
}

fn test_option_void_is_not_compatible_with_payload_options() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_unsafe_none_option_default', 'struct Holder {
	value ?int = unsafe { none }
}

fn main() {
	holder := Holder{}
	println((holder.value == none).str())
}
')
	assert out == 'true'
	run_bad(v3_bin, 'bad_option_void_returned_as_option_int',
		'fn empty() ? {\n\treturn\n}\n\nfn value() ?int {\n\treturn empty()\n}\n\nfn main() {}\n',
		'cannot return `?void` as `?int`')
	run_bad(v3_bin, 'bad_option_void_assigned_to_option_int',
		'fn empty() ? {\n\treturn\n}\n\nfn main() {\n\tmut value := ?int(1)\n\tvalue = empty()\n}\n',
		'cannot assign `?void` to `?int`')
}

fn test_optional_address_preserves_wrapper_shape() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'optional_address_wrapper_shape', 'fn take_wrapper(value &?int) {
	_ = value
}

fn take_payload(value ?&int) {
	_ = value
}

fn main() {
	maybe := ?int(1)
	take_wrapper(&maybe)
	take_payload(&maybe?)
	println("ok")
}
')
	assert out == 'ok'
}

fn test_generic_alias_substitutes_channel_element() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'generic_channel_alias_element', 'type Ch[T] = chan T

fn receive(ch Ch[int]) int {
	return <-ch
}

fn main() {
	ch := chan int{cap: 1}
	ch <- 42
	println(int_str(receive(ch)))
}
')
	assert out == '42'
	run_bad(v3_bin, 'bad_generic_channel_alias_element', 'type Ch[T] = chan T

fn receive(ch Ch[int]) int {
	return <-ch
}

fn main() {
	ch := chan string{cap: 1}
	_ := receive(ch)
}
',
		'cannot use `chan string`')
}

fn test_optional_parameters_are_required() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_omitted_optional_parameter',
		'fn consume(value ?int) {}\n\nfn main() {\n\tconsume()\n}\n',
		'argument count mismatch for `consume`: expected 1, got 0')
}

fn test_multi_return_arguments_must_consume_the_parameter_tail() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_non_tail_multi_return_argument',
		'fn pair() (int, int) {\n\treturn 1, 2\n}\n\nfn consume(a int, b int, c int) {}\n\nfn main() {\n\tconsume(pair(), 3)\n}\n',
		'argument count mismatch for `consume`: expected 3, got 2')
}

fn test_if_expr_pointer_and_value_branches_are_incompatible() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_if_expr_pointer_value_branch',
		'struct Foo {}\n\nfn main() {\n\t_ := if true {\n\t\tFoo{}\n\t} else {\n\t\t&Foo{}\n\t}\n}\n',
		'if-expression branch type mismatch')
}

fn test_reject_narrowed_interface_method_parameters() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_narrowed_interface_method_param',
		'interface A {\n\ta() string\n}\n\ninterface B {\n\tA\n\tb() string\n}\n\nstruct Impl {}\n\nfn (Impl) m(x B) {\n\t_ = x\n}\n\ninterface I {\n\tm(x A)\n}\n\nfn main() {\n\t_ := I(Impl{})\n}\n',
		'type `Impl` does not implement interface `I`')
}

fn test_implicit_str_sum_does_not_satisfy_interface() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_implicit_str_sum_interface',
		'interface Printable {\n\tstr() string\n}\ntype Value = int | string\nfn main() {\n\t_ := Printable(Value(1))\n}\n',
		'does not implement interface')
}

fn test_implicit_str_unsupported_alias_does_not_satisfy_interface() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_implicit_str_fn_alias_interface',
		'interface Printable {\n\tstr() string\n}\ntype Callback = fn ()\nfn noop() {}\nfn main() {\n\tcb := Callback(noop)\n\t_ := Printable(cb)\n}\n',
		'does not implement interface')
}

fn test_multi_return_tail_slots_use_return_compatibility() {
	v3_bin := build_v3_review_checker()
	if_out := run_good(v3_bin, 'good_multi_return_if_pointer_value_tail',
		'struct S {\n\tn int\n}\nfn pick(ok bool) (S, int) {\n\ts := S{\n\t\tn: 5\n\t}\n\treturn if ok {\n\t\t&s\n\t\t1\n\t} else {\n\t\t&s\n\t\t2\n\t}\n}\nfn main() {\n\ta, b := pick(false)\n\tprintln(int_str(a.n) + "," + int_str(b))\n}\n')
	assert if_out == '5,2'
}

fn test_none_ierror_values_lower_to_builtin_none() {
	v3_bin := build_v3_review_checker()
	ierror_out := run_good(v3_bin, 'good_none_ierror_contexts',
		'struct Holder {\n\terr IError = none\n}\n\nfn take(e IError) int {\n\tif e is none {\n\t\treturn 1\n\t}\n\treturn 0\n}\n\nfn make() IError {\n\treturn none\n}\n\nfn main() {\n\tdefault := Holder{}\n\texplicit := Holder{\n\t\terr: none\n\t}\n\tprintln(int_str(take(none) + take(default.err) + take(explicit.err) + take(make())))\n}\n')
	assert ierror_out == '4'
	out := run_good(v3_bin, 'good_none_option_context',
		'fn maybe() ?int {\n\treturn none\n}\n\nfn main() {\n\tif maybe() == none {\n\t\tprintln("option")\n\t}\n}\n')
	assert out == 'option'
}

fn test_reject_non_optional_or_and_wrapped_string_concat() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_non_optional_literal_or_block', 'fn main() {\n\t_ := 1 or { 2 }\n}\n',
		'unexpected `or` block')
	run_bad(v3_bin, 'bad_non_optional_call_or_block',
		'fn value() int {\n\treturn 1\n}\n\nfn main() {\n\tvalue() or { panic(err) }\n}\n',
		'unexpected `or` block')
	run_bad(v3_bin, 'bad_non_optional_infix_or_block',
		'fn maybe() ?int {\n\treturn 1\n}\n\nfn main() {\n\t_ := maybe() == none or { false }\n}\n',
		'unexpected `or` block, expression of type `bool` is not an Option or a Result')
	run_bad(v3_bin, 'bad_optional_string_concat',
		"fn maybe_name() ?string {\n\treturn 'Ada'\n}\n\nfn main() {\n\t_ := 'hello ' + maybe_name()\n}\n",
		'operator `+` cannot concatenate `string` and `?string`')
	run_bad(v3_bin, 'bad_result_string_concat',
		"fn result_name() !string {\n\treturn 'Ada'\n}\n\nfn main() {\n\t_ := result_name() + '!'\n}\n",
		'operator `+` cannot concatenate `!string` and `string`')
	out := run_good(v3_bin, 'good_map_or_and_unwrapped_string_concat',
		"fn maybe_name() ?string {\n\treturn 'Ada'\n}\n\nfn main() {\n\tnames := {\n\t\t'first': 'Grace'\n\t}\n\tprintln(names['first'] or { 'unknown' })\n\tprintln('hello ' + (maybe_name() or { 'unknown' }))\n}\n")
	assert out == 'Grace\nhello Ada'
}

fn test_rune_receiver_methods_resolve() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_rune_receiver_methods',
		'fn main() {\n\tr := `★`\n\tprintln(int_str(`A`.length_in_bytes()))\n\tprintln(int_str(r.bytes().len))\n\tprintln(`c`.to_upper().str())\n}\n')
	assert out == '1\n3\nC'
}

fn test_numeric_alias_returns_preserve_integer_float_direction() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_int_alias_float_return',
		'type Id = int\n\nfn f() Id {\n\treturn 1.5\n}\n\nfn main() {}\n',
		'cannot return `f64` as `Id`')
	run_bad(v3_bin, 'bad_int_alias_float_variable_return',
		'type Id = int\n\nfn f(x f64) Id {\n\treturn x\n}\n\nfn main() {}\n',
		'cannot return `f64` as `Id`')
	run_bad(v3_bin, 'bad_int_alias_float_expression_return',
		'type Id = int\n\nfn f(x f64) Id {\n\treturn x + 1.0\n}\n\nfn main() {}\n',
		'cannot return `f64` as `Id`')
	out := run_good(v3_bin, 'good_float_alias_int_return',
		'type Amount = f64\n\nfn f() Amount {\n\treturn 1\n}\n\nfn main() {\n\tprintln(f().str())\n}\n')
	assert out == '1.0'
	explicit_out := run_good(v3_bin, 'good_explicit_float_to_int_alias_return',
		'type Id = int\n\nfn f(x f64) Id {\n\treturn Id(x)\n}\n\nfn main() {\n\tprintln(int_str(f(1.5)))\n}\n')
	assert explicit_out == '1'
}

fn test_fn_value_integer_returns_require_matching_c_abi() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_fn_value_integer_return_abi',
		'fn wide() u64 {\n\treturn 257\n}\n\nfn invoke(callback fn () u8) u8 {\n\treturn callback()\n}\n\nfn main() {\n\t_ := invoke(wide)\n}\n',
		'cannot use `fn() u64`')
	out := run_good(v3_bin, 'good_fn_value_matching_integer_return_abi',
		'fn letter() rune {\n\treturn `A`\n}\n\nfn invoke(callback fn () u32) u32 {\n\treturn callback()\n}\n\nfn main() {\n\tprintln(int_str(int(invoke(letter))))\n}\n')
	assert out == '65'
}

fn test_fn_value_aggregate_returns_require_compatible_payloads() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_fn_value_array_return_payload',
		'fn numbers() []int {\n\treturn [1, 2]\n}\n\nfn invoke(callback fn () []string) []string {\n\treturn callback()\n}\n\nfn main() {\n\t_ := invoke(numbers)\n}\n',
		'cannot use `fn() []int`')
}

fn test_alias_with_nested_type_separator_stays_alias() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_alias_nested_type_separator',
		'type Bits = [1 | 2]int\n\nfn values() Bits {\n\treturn [1, 2, 3]!\n}\n\nfn main() {\n\tbits := values()\n\tprintln(int_str(bits[0] + bits[1] + bits[2]))\n}\n')
	assert out == '6'
}

fn test_voidptr_params_reject_non_pointer_values() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_voidptr_scalar_arg', 'fn f(p voidptr) {}\n\nfn main() {\n\tf(1)\n}\n',
		'cannot use `int` as argument 1 to `f`; expected `&void`')
	out := run_good(v3_bin, 'good_voidptr_pointer_arg',
		'fn f(p voidptr) int {\n\t_ = p\n\treturn 7\n}\n\nfn main() {\n\tx := 1\n\tprintln(int_str(f(&x)))\n}\n')
	assert out == '7'
}

fn test_shared_receiver_and_arg_require_shared_bindings() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_mut_receiver_immutable_value',
		'struct St {\nmut:\n\tvalue int\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\nfn main() {\n\ts := St{}\n\ts.bump()\n}\n',
		'method `bump` requires a mutable receiver')
	run_bad(v3_bin, 'bad_generic_mut_receiver_immutable_value',
		'struct Box[T] {\nmut:\n\tvalue T\n}\n\nfn (mut b Box[T]) set(value T) {\n\tb.value = value\n}\n\nfn main() {\n\tb := Box[int]{\n\t\tvalue: 1\n\t}\n\tb.set(2)\n}\n',
		'method `set` requires a mutable receiver')
	run_bad(v3_bin, 'bad_mut_receiver_address_of_immutable_value',
		'struct St {\nmut:\n\tvalue int\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\nfn main() {\n\ts := St{}\n\t(&s).bump()\n}\n',
		'method `bump` requires a mutable receiver')
	run_bad(v3_bin, 'bad_mut_receiver_immutable_pointer_binding',
		'struct St {\nmut:\n\tvalue int\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\nfn main() {\n\tmut s := St{}\n\tp := &s\n\tp.bump()\n}\n',
		'method `bump` requires a mutable receiver')
	run_bad(v3_bin, 'bad_mut_receiver_or_temporary',
		'struct St {\nmut:\n\tvalue int\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\nfn main() {\n\tmut values := {\n\t\t"item": St{}\n\t}\n\t(values["item"] or { St{} }).bump()\n}\n',
		'method `bump` requires a mutable receiver')
	run_bad(v3_bin, 'bad_shared_receiver_plain_value',
		'struct St {}\n\nfn (shared s St) f() {}\n\nfn main() {\n\ts := St{}\n\ts.f()\n}\n',
		'cannot use non-shared `St` as receiver')
	run_bad(v3_bin, 'bad_shared_arg_shadowed_local',
		'struct St {}\n\nfn take(shared s St) {}\n\nfn main() {\n\tshared s := St{}\n\tif true {\n\t\ts := St{}\n\t\ttake(s)\n\t}\n}\n',
		'cannot use non-shared `St` as argument 1')
	run_bad(v3_bin, 'bad_explicit_shared_arg_plain_local',
		'struct St {}\n\nfn take(shared s St) {}\n\nfn main() {\n\ts := St{}\n\ttake(shared s)\n}\n',
		'cannot use non-shared `St` as argument 1')
	out := run_good(v3_bin, 'good_shared_arg_and_receiver',
		'struct St {}\n\nfn take(shared s St) int {\n\treturn 1\n}\n\nfn (shared s St) f() int {\n\treturn 2\n}\n\nfn main() {\n\tshared s := St{}\n\tprintln(int_str(take(s) + s.f()))\n}\n')
	assert out == '3'
	mut_out := run_good(v3_bin, 'good_mut_receiver_mutable_value',
		'struct St {\nmut:\n\tvalue int\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\nfn main() {\n\tmut s := St{}\n\ts.bump()\n\tprintln(int_str(s.value))\n}\n')
	assert mut_out == '1'
	mut_address_out := run_good(v3_bin, 'good_mut_receiver_address_of_mutable_value',
		'struct St {\nmut:\n\tvalue int\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\nfn main() {\n\tmut s := St{}\n\t(&s).bump()\n\tprintln(int_str(s.value))\n}\n')
	assert mut_address_out == '1'
	mut_pointer_out := run_good(v3_bin, 'good_mut_receiver_mutable_pointer_binding',
		'struct St {\nmut:\n\tvalue int\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\nfn main() {\n\tmut s := St{}\n\tmut p := &s\n\tp.bump()\n\tprintln(int_str(s.value))\n}\n')
	assert mut_pointer_out == '1'
	global_pointer_out := run_good(v3_bin, 'good_mut_receiver_global_pointer',
		'struct St {\nmut:\n\tvalue int\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\n__global global_st = &St{}\n\nfn main() {\n\tglobal_st.bump()\n\tprintln(int_str(global_st.value))\n}\n')
	assert global_pointer_out == '1'
	field_pointer_out := run_good(v3_bin, 'good_mut_receiver_pointer_field',
		'struct St {\nmut:\n\tvalue int\n}\n\nstruct Holder {\n\tst &St\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\nfn main() {\n\tmut s := St{}\n\tholder := Holder{\n\t\tst: &s\n\t}\n\tholder.st.bump()\n\tprintln(int_str(s.value))\n}\n')
	assert field_pointer_out == '1'
	if_guard_out := run_good(v3_bin, 'good_mut_receiver_if_guard_binding',
		'struct St {\nmut:\n\tvalue int\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\nfn get() ?St {\n\treturn St{}\n}\n\nfn main() {\n\tif mut s := get() {\n\t\ts.bump()\n\t\tprintln(int_str(s.value))\n\t}\n}\n')
	assert if_guard_out == '1'
	capture_out := run_good(v3_bin, 'good_mut_receiver_explicit_mut_capture',
		'struct St {\nmut:\n\tvalue int\n}\n\nfn (mut s St) bump() {\n\ts.value++\n}\n\nfn main() {\n\tmut s := St{}\n\tfn [mut s] () {\n\t\ts.bump()\n\t\tprintln(int_str(s.value))\n\t}()\n}\n')
	assert capture_out == '1'
	ptr_out := run_good(v3_bin, 'good_pointer_receiver_immutable_value',
		'struct St {\n\tvalue int\n}\n\nfn (s &St) get() int {\n\treturn s.value\n}\n\nfn main() {\n\ts := St{\n\t\tvalue: 2\n\t}\n\tprintln(int_str(s.get()))\n}\n')
	assert ptr_out == '2'
}

fn test_method_receiver_rejects_extra_pointer_layers() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_value_receiver_extra_pointer_layers', 'struct S {}

fn (s S) value() int {
	return 1
}

fn main() {
	s := S{}
	p := &s
	pp := &p
	_ := pp.value()
}
',
		'cannot use receiver `&&S` as `S`')
	run_bad(v3_bin, 'bad_pointer_receiver_extra_pointer_layers', 'struct S {}

fn (s &S) value() int {
	return 1
}

fn main() {
	s := S{}
	p := &s
	pp := &p
	ppp := &pp
	_ := ppp.value()
}
',
		'cannot use receiver `&&&S` as `&S`')
	out := run_good(v3_bin, 'good_receiver_single_pointer_adjustment', 'struct S {
	value int
}

fn (s S) by_value() int {
	return s.value
}

fn (s &S) by_pointer() int {
	return s.value
}

fn main() {
	s := S{
		value: 7
	}
	p := &s
	println(int_str(p.by_value()))
	println(int_str(s.by_pointer()))
}
')
	assert out == '7\n7'
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
	run_bad(v3_bin, 'bad_numeric_hex_arg',
		'fn side_effect() int {\n\treturn 1\n}\n\nfn main() {\n\t_ := u8(1).hex(side_effect())\n}\n',
		'argument count mismatch')
	out := run_good(v3_bin, 'supported_hex_methods',
		"fn main() {\n\tprintln(u8(15).hex())\n\tprintln(i64(255).hex())\n\tprintln([u8(1), 15, 255].hex())\n\tprintln(char(65).hex())\n\tprintln(`A`.hex())\n\tprintln('abc'.hex())\n}\n")
	assert out == '0f\nff\n010fff\n41\n41\n616263'
}

fn test_auto_str_rejects_arguments() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_auto_str_arg',
		'struct S {\n\tx int\n}\n\nfn side_effect() int {\n\treturn 1\n}\n\nfn main() {\n\t_ := S{\n\t\tx: 1\n\t}.str(side_effect())\n}\n',
		'argument count mismatch')
}

fn test_pointer_hex_receiver_methods_are_allowed() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_pointer_hex_receiver_method',
		"struct S {\n\tvalue int\n}\n\nfn (s &S) hex() string {\n\treturn 'ptr:' + int_str(s.value)\n}\n\nfn main() {\n\ts := S{\n\t\tvalue: 7\n\t}\n\tp := &s\n\tprintln(p.hex())\n}\n")
	assert out == 'ptr:7'
}

fn test_map_keys_and_values_reject_arguments() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_map_keys_arg',
		'fn main() {\n\tm := map[string]int{}\n\t_ := m.keys(123)\n}\n', 'argument count mismatch')
	run_bad(v3_bin, 'bad_map_values_arg',
		"fn main() {\n\tm := map[string]int{}\n\t_ := m.values('x')\n}\n",
		'argument count mismatch')
}

fn test_array_to_void_array_is_not_implicitly_compatible() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_array_to_void_array_param',
		'fn take(xs []void) {\n\t_ = xs\n}\n\nfn main() {\n\ttake([1, 2, 3])\n}\n',
		'cannot use `[]int` as argument 1 to `take`; expected `[]void`')
	run_bad(v3_bin, 'bad_array_to_void_array_user_receiver',
		'fn (xs []void) touch() int {\n\treturn xs.len\n}\n\nfn main() {\n\tnums := [1, 2, 3]\n\tprintln(nums.touch().str())\n}\n',
		'unknown function `nums.touch`')
	out := run_good(v3_bin, 'good_array_clone_ignores_void_array_receiver',
		'fn (xs []void) clone() int {\n\treturn 7\n}\n\nfn main() {\n\tnums := [1, 2, 3]\n\tcloned := nums.clone()\n\tprintln(int_str(cloned.len + cloned[2]))\n}\n')
	assert out == '6'
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
	run_bad(v3_bin, 'bad_array_prepend_arg_type',
		"fn main() {\n\tmut a := [1, 2]\n\ta.prepend('x')\n}\n", 'cannot use')
	run_bad(v3_bin, 'bad_array_insert_index_type',
		"fn main() {\n\tmut a := [1, 2]\n\ta.insert('0', 3)\n}\n", 'cannot use')
	run_bad(v3_bin, 'bad_array_insert_value_type',
		"fn main() {\n\tmut a := [1, 2]\n\ta.insert(0, 'x')\n}\n", 'cannot use')
	run_bad(v3_bin, 'bad_array_prepend_many_arg_type',
		"fn main() {\n\tmut a := [1, 2]\n\ta.prepend(['x'])\n}\n", 'cannot use')
	run_bad(v3_bin, 'bad_array_insert_many_arg_type',
		"fn main() {\n\tmut a := [1, 2]\n\ta.insert(0, ['x'])\n}\n", 'cannot use')
}

fn test_array_insert_and_prepend_accept_many_operands() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_array_insert_prepend_many_operands',
		"type Strings = []string\n\nfn main() {\n\tmut a := [3, 4]\n\ta.insert(0, [1, 2])\n\tb := [5, 6]\n\ta.insert(1, b)\n\ta.prepend([0])\n\tfixed := [7, 8]!\n\ta.insert(a.len, fixed)\n\tassert a == [0, 1, 5, 6, 2, 3, 4, 7, 8]\n\tmut strs := Strings(['hi'])\n\tstrs.insert(0, ['there'])\n\tstrs.prepend(['hello'])\n\tassert strs == ['hello', 'there', 'hi']\n\tprintln('ok')\n}\n")
	assert out == 'ok'
}

fn test_comptime_if_selected_bodies_are_checked() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_concrete_comptime_if_selected_call',
		'fn main() {\n\t$if int is int {\n\t\tmissing_selected_symbol()\n\t}\n}\n',
		'unknown function `missing_selected_symbol`')
	out := run_good(v3_bin, 'good_generic_comptime_if_unselected_branch_is_not_checked',
		"fn ok() {}\n\nfn f[T]() {\n\t$if T is int {\n\t\tok()\n\t} $else {\n\t\tonly_for_other_t()\n\t}\n}\n\nfn main() {\n\tf[int]()\n\tprintln('ok')\n}\n")
	assert out == 'ok'
}

fn test_explicit_generic_calls_use_all_type_arguments() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_multi_explicit_generic_call',
		"struct Pair[A, B] {\n\tleft  A\n\tright B\n}\n\nfn make_pair[A, B]() Pair[A, B] {\n\treturn Pair[A, B]{}\n}\n\nfn expect_pair(p Pair[int, string]) string {\n\treturn 'ok'\n}\n\nfn main() {\n\tp := make_pair[int, string]()\n\tprintln(expect_pair(p))\n}\n")
	assert out == 'ok'
	nested := run_good(v3_bin, 'good_nested_explicit_generic_call',
		"struct Pair[A, B] {\n\tleft  A\n\tright B\n}\n\nstruct Box[T] {\n\tvalue T\n}\n\nfn wrap[T]() Box[T] {\n\treturn Box[T]{}\n}\n\nfn expect_box(b Box[Pair[int, string]]) string {\n\treturn 'ok'\n}\n\nfn main() {\n\tb := wrap[Pair[int, string]]()\n\tprintln(expect_box(b))\n}\n")
	assert nested == 'ok'
	run_bad(v3_bin, 'bad_explicit_generic_too_many_type_args',
		'fn id[T](x T) T {\n\treturn x\n}\n\nfn main() {\n\t_ := id[int, string](1)\n}\n',
		'generic argument count mismatch')
}

fn test_reject_escaping_capturing_fn_literals() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_return_capturing_fn_literal',
		'fn make(x int) fn () int {\n\treturn fn [x] () int {\n\t\treturn x\n\t}\n}\nfn main() {}\n',
		'capturing fn literal cannot be stored or returned')
	run_bad(v3_bin, 'bad_return_capturing_fn_literal_alias',
		'fn make(x int) fn () int {\n\tf := fn [x] () int {\n\t\treturn x\n\t}\n\treturn f\n}\nfn main() {}\n',
		'capturing fn literal cannot be stored or returned')
	run_bad(v3_bin, 'bad_struct_field_capturing_fn_literal',
		'struct Holder {\n\tcb fn () int\n}\nfn make(x int) Holder {\n\treturn Holder{\n\t\tcb: fn [x] () int {\n\t\t\treturn x\n\t\t}\n\t}\n}\nfn main() {}\n',
		'capturing fn literal cannot be stored or returned')
	run_bad(v3_bin, 'bad_struct_field_capturing_fn_literal_alias',
		'struct Holder {\n\tcb fn () int\n}\nfn make(x int) Holder {\n\tf := fn [x] () int {\n\t\treturn x\n\t}\n\treturn Holder{\n\t\tcb: f\n\t}\n}\nfn main() {}\n',
		'capturing fn literal cannot be stored or returned')
}

fn test_capturing_fn_literal_aliases_are_binding_scoped() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_capturing_fn_literal_inner_shadow',
		'fn plain() int {\n\treturn 3\n}\n\nfn make(x int) fn () int {\n\tcb := plain\n\tif x > 0 {\n\t\tcb := fn [x] () int {\n\t\t\treturn x\n\t\t}\n\t\t_ = cb\n\t}\n\treturn cb\n}\n\nfn main() {\n\tprintln(int_str(make(0)()))\n}\n')
	assert out == '3'
	lambda_out := run_good(v3_bin, 'good_lambda_capturing_fn_literal_shadow',
		'fn plain() int {\n\treturn 4\n}\n\nfn apply(cb fn (int) int) int {\n\treturn cb(1)\n}\n\nfn make() fn () int {\n\tcb := plain\n\t_ = apply(|n| if n > 0 {\n\t\tcb := fn [n] () int {\n\t\t\treturn n\n\t\t}\n\t\t_ = cb\n\t\tn\n\t} else {\n\t\tn\n\t})\n\treturn cb\n}\n\nfn main() {\n\tprintln(int_str(make()()))\n}\n')
	assert lambda_out == '4'
	run_bad(v3_bin, 'bad_outer_capturing_alias_survives_inner_shadow',
		'fn make(x int) fn () int {\n\tcb := fn [x] () int {\n\t\treturn x\n\t}\n\tif x > 0 {\n\t\tcb := fn [x] () int {\n\t\t\treturn x + 1\n\t\t}\n\t\t_ = cb\n\t}\n\treturn cb\n}\nfn main() {}\n',
		'capturing fn literal cannot be stored or returned')
}

fn test_reject_unsmartcasted_unique_sum_variant_field() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_unsmartcasted_unique_sum_field',
		'struct A {\n\tonly_on_a int\n}\nstruct B {}\ntype K = A | B\nfn main() {\n\tk := K(B{})\n\t_ := k.only_on_a\n}\n',
		'unknown field `only_on_a`')
	out := run_good(v3_bin, 'good_smartcasted_unique_sum_field',
		'struct A {\n\tonly_on_a int\n}\nstruct B {}\ntype K = A | B\nfn main() {\n\tk := K(A{\n\t\tonly_on_a: 7\n\t})\n\tif k is A {\n\t\tprintln(int_str(k.only_on_a))\n\t}\n}\n')
	assert out == '7'
}

fn test_smartcasted_fn_sum_call_keeps_active_variant_arity() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_smartcasted_fn_sum_variant_arity', 'type Callback = fn () int | fn (int) int

fn no_args() int {
	return 7
}

fn invoke(cb Callback) {
	if cb is fn () int {
		_ := cb(1)
	}
}

fn main() {
	invoke(Callback(no_args))
}
',
		'argument count mismatch')
	out := run_good(v3_bin, 'good_smartcasted_fn_sum_variant_arity', 'type Callback = fn () int | fn (int) int

fn with_arg(value int) int {
	return value
}

fn invoke(cb Callback) int {
	if cb is fn (int) int {
		return cb(7)
	}
	return 0
}

fn main() {
	println(int_str(invoke(Callback(with_arg))))
}
')
	assert out == '7'
}

fn test_generic_functions_report_missing_return() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_generic_missing_return', 'fn f[T]() int {\n}\nfn main() {}\n',
		'missing return at end of function `f`')
	run_bad(v3_bin, 'bad_called_generic_missing_return',
		'fn f[T]() int {\n}\nfn main() {\n\t_ := f[int]()\n}\n',
		'missing return at end of function `f`')
	run_bad(v3_bin, 'bad_generic_comptime_branch_missing_return',
		'fn f[T]() int {\n\t$if T is int {\n\t\treturn 1\n\t}\n}\nfn main() {\n\t_ := f[string]()\n}\n',
		'missing return at end of function `f`')
	run_bad(v3_bin, 'bad_generic_option_propagation_missing_return',
		'fn f[T](value ?T) ?T {\n\tvalue?\n}\nfn main() {}\n',
		'missing return at end of function `f`')
	run_bad(v3_bin, 'bad_generic_result_propagation_missing_return',
		'fn f[T](value !T) !T {\n\tvalue!\n}\nfn main() {}\n',
		'missing return at end of function `f`')
}

fn test_no_return_calls_satisfy_return_analysis() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_panic_satisfies_return_analysis',
		"fn choose(ok bool) string {\n\tif ok {\n\t\treturn 'ok'\n\t}\n\tpanic('unreachable')\n}\n\nfn pick(ok bool) int {\n\tif ok {\n\t\treturn 7\n\t}\n\treturn panic('unreachable')\n}\n\nfn main() {\n\tprintln(choose(true))\n\tprintln(int_str(pick(true)))\n}\n")
	assert out == 'ok\n7'
}

fn test_parenthesized_no_return_return_uses_cgen_fallback() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_parenthesized_panic_satisfies_return_codegen',
		"fn f(ok bool) int {\n\tif ok {\n\t\treturn 9\n\t}\n\treturn (panic('x'))\n}\nfn main() {\n\tprintln(int_str(f(true)))\n}\n")
	assert out == '9'
}

fn test_return_panic_with_defer_evaluates_call_before_cleanup() {
	v3_bin := build_v3_review_checker()
	out := run_runtime_bad(v3_bin, 'bad_return_panic_defer_order',
		"fn f() int {\n\tdefer {\n\t\tprintln('cleanup-ran')\n\t}\n\treturn panic('boom')\n}\nfn main() {\n\t_ := f()\n}\n")
	assert out.contains('boom')
	assert !out.contains('cleanup-ran')
}

fn test_declared_c_exit_satisfies_return_analysis() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_declared_c_exit_satisfies_return_analysis',
		'fn C.exit(code int)\nfn f(ok bool) int {\n\tif ok {\n\t\treturn 7\n\t}\n\treturn C.exit(1)\n}\nfn main() {\n\tprintln(int_str(f(true)))\n}\n')
	assert out == '7'
}

fn test_no_return_analysis_requires_resolved_builtin_target() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_shadowed_os_exit_missing_return',
		'struct OsLike {}\nfn (x OsLike) exit() {}\nfn f(os OsLike) int {\n\tos.exit()\n}\nfn main() {}\n',
		'missing return at end of function `f`')
	run_bad(v3_bin, 'bad_local_os_exit_missing_return',
		'struct OsLike {}\nfn (x OsLike) exit() {}\nfn f() int {\n\tos := OsLike{}\n\tos.exit()\n}\nfn main() {}\n',
		'missing return at end of function `f`')
}

fn test_no_return_analysis_rejects_shadowed_builtin_fn_values() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_shadowed_exit_fn_value_missing_return',
		'fn f() int {\n\texit := fn () int { return 1 }\n\texit()\n}\nfn main() {}\n',
		'missing return at end of function `f`')
	run_bad(v3_bin, 'bad_shadowed_panic_fn_value_missing_return',
		'fn f() int {\n\tpanic := fn () int { return 1 }\n\tpanic()\n}\nfn main() {}\n',
		'missing return at end of function `f`')
	run_bad(v3_bin, 'bad_nested_shadowed_exit_fn_value_missing_return',
		'fn f() int {\n\t{\n\t\texit := fn () int { return 1 }\n\t\texit()\n\t}\n}\nfn main() {}\n',
		'missing return at end of function `f`')
	run_bad(v3_bin, 'bad_shadowed_exit_multi_return_branch',
		'fn main() {\n\texit := fn () int { return 1 }\n\tflag := true\n\ta, b := if flag {\n\t\t1\n\t\t2\n\t} else {\n\t\texit()\n\t}\n\tprintln(int_str(a + b))\n}\n',
		'multi-return assignment mismatch')
	run_bad(v3_bin, 'bad_nested_shadowed_exit_multi_return_branch',
		'fn main() {\n\tflag := true\n\ta, b := if flag {\n\t\t1\n\t\t2\n\t} else {\n\t\texit := fn () int { return 1 }\n\t\texit()\n\t}\n\tprintln(int_str(a + b))\n}\n',
		'multi-return assignment mismatch')
}

fn test_returning_receiver_method_named_exit_keeps_value() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_receiver_exit_return_value',
		'struct Plugin {}\nfn (p Plugin) exit() int {\n\treturn 9\n}\nfn f(plugin Plugin) int {\n\treturn plugin.exit()\n}\nfn main() {\n\tprintln(int_str(f(Plugin{})))\n}\n')
	assert out == '9'
}

fn test_imported_module_name_shadowed_by_receiver_for_no_return_analysis() {
	v3_bin := build_v3_review_checker()
	run_bad(v3_bin, 'bad_shadowed_import_os_exit_missing_return',
		'import os\nstruct OsLike {}\nfn (x OsLike) exit(code int) {}\nfn f(os OsLike) int {\n\tos.exit(0)\n}\nfn main() {}\n',
		'missing return at end of function `f`')
}

fn test_returning_shadowed_os_exit_receiver_keeps_value() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_shadowed_os_exit_return_value',
		'import os\nstruct OsLike {}\nfn (x OsLike) exit(code int) int {\n\treturn code + 1\n}\nfn f(os OsLike) int {\n\treturn os.exit(4)\n}\nfn main() {\n\tprintln(int_str(f(OsLike{})))\n}\n')
	assert out == '5'
}

fn test_no_return_fixed_array_return_uses_abi_wrapper() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_fixed_array_return_panic_abi_wrapper',
		"import os\nfn f() [3]int {\n\treturn panic('x')\n}\nfn main() {\n\tif os.args.len == -1 {\n\t\tarr := f()\n\t\tprintln(int_str(arr[0]))\n\t}\n}\n")
	assert out == ''
}

fn test_no_return_fn_return_uses_abi_typedef() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_fn_return_panic_abi_typedef',
		"import os\nfn f() fn () int {\n\treturn panic('x')\n}\nfn main() {\n\tif os.args.len == -1 {\n\t\tcb := f()\n\t\tprintln(int_str(cb()))\n\t}\n}\n")
	assert out == ''
}

fn test_local_identifiers_shadow_module_consts() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'good_const_shadowed_by_param_and_local',
		"const shadowed_value = 'const'\n\nfn param_shadow(shadowed_value int) int {\n\treturn shadowed_value + 1\n}\n\nfn local_shadow() int {\n\tshadowed_value := 2\n\treturn shadowed_value + 1\n}\n\nfn main() {\n\tprintln(int_str(param_shadow(1)))\n\tprintln(int_str(local_shadow()))\n\tprintln(shadowed_value)\n}\n")
	assert out == '2\n3\nconst'
}

fn test_match_const_int_does_not_narrow_subject_type() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'match_const_int_no_subject_narrow',
		'const size_224 = 28\n\nstruct E {\n\tsize int\n}\n\nfn check(hash_size int) !E {\n\tmatch hash_size {\n\t\tsize_224 {\n\t\t\treturn E{\n\t\t\t\tsize: hash_size\n\t\t\t}\n\t\t}\n\t\telse {}\n\t}\n\treturn E{\n\t\tsize: 0\n\t}\n}\n\nfn main() {\n\tprintln(int_str(check(28)!.size))\n}\n')
	assert out == '28'
}

fn test_builtin_function_callee_wins_over_unrelated_const_suffix() {
	v3_bin := build_v3_review_checker()
	out := run_good(v3_bin, 'builtin_fn_unrelated_const_suffix',
		'import math\nfn main() {\n\t_ = math.pi\n\tprintln(f32(1).eq_epsilon(f32(1)))\n}\n')
	assert out == 'true'
}
