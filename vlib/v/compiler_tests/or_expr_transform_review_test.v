import os
import v.tempname

fn or_review_gen_c(name string, src string) string {
	dir := os.join_path(os.vtmp_dir(), 'v3_or_review_${tempname.unique_token()}')
	os.mkdir(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	base := os.join_path(dir, 'v3_${name}')
	src_path := base + '.v'
	os.write_file(src_path, src) or { panic(err) }
	c_path := base + '.c'
	// Some fixtures count calls through global variables.
	compile := os.exec([@VEXE, '-enable-globals', src_path, '-b', 'c', '-o', c_path])
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	assert os.exists(c_path), '${name}: missing generated C'
	return os.read_file(c_path) or { panic(err) }
}

fn or_review_run(name string, src string) string {
	dir := os.join_path(os.vtmp_dir(), 'v3_or_review_${tempname.unique_token()}')
	os.mkdir(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	bin_path := os.join_path(dir, 'v3_${name}')
	src_path := bin_path + '.v'
	os.write_file(src_path, src) or { panic(err) }
	compile := os.exec([@VEXE, '-enable-globals', src_path, '-b', 'c', '-o', bin_path])
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	run := os.exec([bin_path])
	assert run.exit_code == 0, '${name}: run failed\n${run.output}'
	return run.output.trim_space()
}

fn or_review_compile_bad(name string, src string) string {
	dir := os.join_path(os.vtmp_dir(), 'v3_or_review_${tempname.unique_token()}')
	os.mkdir(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	src_path := os.join_path(dir, 'v3_${name}.v')
	os.write_file(src_path, src) or { panic(err) }
	compile := os.exec([@VEXE, src_path, '-b', 'c'])
	assert compile.exit_code != 0, '${name}: invalid source compiled successfully'
	return compile.output
}

fn test_channel_receive_or_stabilizes_side_effectful_source() {
	c_source := or_review_gen_c('channel_receive_or_source_once',
		'__global calls int\n\nfn next_channel() chan int {\n\tcalls++\n\tch := chan int{}\n\tch.close()\n\treturn ch\n}\n\nfn main() {\n\tx := (<-next_channel()) or {\n\t\tprintln(err.msg())\n\t\t-1\n\t}\n\tprintln(int_str(x))\n}\n')
	assert c_source.count('next_channel()') == 1, 'side-effectful channel source was not stabilized'
	assert c_source.contains('sync__Channel* __chan_src_'), 'missing channel source temp'
	assert c_source.contains('sync__Channel__pop(__chan_src_'), 'pop does not use channel source temp'
	assert c_source.contains('sync__Channel__closed_error(__chan_src_'), 'closed_error does not use channel source temp'
	selector_out := or_review_run('channel_receive_or_selector',
		"struct Holder {\n\tch chan int\n}\n\nfn main() {\n\th := Holder{ch: chan int{}}\n\th.ch.close()\n\tvalue := <-h.ch or {\n\t\tprintln('closed')\n\t\treturn\n\t}\n\tprintln(int_str(value))\n}\n")
	assert selector_out == 'closed'
}

fn test_bare_channel_or_is_rejected() {
	output := or_review_compile_bad('bare_channel_or_rejected',
		'fn main() {\n\tch := chan int{}\n\t_ := ch or { ch }\n}\n')
	assert output.contains('cannot use `or {}` block on non-option variable'), output
}

fn test_array_optional_element_or_uses_fallback_without_err_binding() {
	src := 'fn main() {\n\terr := 7\n\tmut arr := []?int{}\n\tarr << none\n\tarr << 5\n\tmissing := arr[0] or { err }\n\tpresent := arr[1] or { -1 }\n\tprintln(int_str(missing))\n\tprintln(int_str(present))\n}\n'
	c_source := or_review_gen_c('array_optional_element_fallback_source', src)
	assert c_source.contains('__v_option_i64 __arr_opt_'), 'missing loaded optional temp'
	assert c_source.contains('i64 err = 7;'), 'explicit outer err was not preserved'
	assert !c_source.contains('IError err = __arr_opt_'), 'element failure branch binds an Option error'
	out := or_review_run('array_optional_element_fallback_run', src)
	assert out == '7\n5'
}

fn test_map_optional_element_or_uses_fallback_without_err_binding() {
	src := "fn main() {\n\terr := 7\n\tmut m := map[string]?int{}\n\tm['x'] = none\n\tm['y'] = 5\n\tmissing := m['x'] or { err }\n\tpresent := m['y'] or { -1 }\n\tabsent := m['z'] or { -2 }\n\tprintln(int_str(missing))\n\tprintln(int_str(present))\n\tprintln(int_str(absent))\n}\n"
	c_source := or_review_gen_c('map_optional_element_fallback_source', src)
	assert c_source.contains('__v_option_i64 __map_opt_'), 'missing loaded map optional temp'
	assert c_source.contains('i64 err = 7;'), 'explicit outer err was not preserved'
	assert !c_source.contains('IError err = __map_opt_'), 'map element failure branch binds an Option error'
	out := or_review_run('map_optional_element_fallback_run', src)
	assert out == '7\n5\n-2'
}

fn test_optional_element_or_rejects_implicit_err() {
	array_output := or_review_compile_bad('array_optional_element_implicit_err',
		'fn main() {\n\tmut arr := []?int{}\n\tarr << none\n\tvalue := arr[0] or {\n\t\tprintln(err.msg())\n\t\t0\n\t}\n\tprintln(int_str(value))\n}\n')
	assert array_output.contains('undefined variable: `err`'), array_output
	map_output := or_review_compile_bad('map_optional_element_implicit_err',
		"fn main() {\n\tmut m := map[string]?int{}\n\tm['x'] = none\n\tvalue := m['x'] or {\n\t\tprintln(err.msg())\n\t\t0\n\t}\n\tprintln(int_str(value))\n}\n")
	assert map_output.contains('undefined variable: `err`'), map_output
}

fn test_map_index_address_or_nil_remains_valid() {
	out := or_review_run('map_index_address_or_nil',
		"fn main() {\n\tmut values := {\n\t\t'x': 7\n\t}\n\tpresent := unsafe { &values['x'] or { nil } }\n\tmissing := unsafe { &values['missing'] or { nil } }\n\tprintln(int_str(*present))\n\tprintln(missing == unsafe { nil })\n}\n")
	assert out == '7\ntrue'
}

fn test_nested_optional_infix_or_uses_whole_expression_fallback() {
	out := or_review_run('nested_optional_infix_or_fallback',
		'__global left_calls int\n__global right_calls int\n\nfn none_int() ?int {\n\treturn none\n}\n\nfn some_int() ?int {\n\treturn 2\n}\n\nfn left_effect() int {\n\tleft_calls++\n\treturn 40\n}\n\nfn right_effect() int {\n\tright_calls++\n\treturn 40\n}\n\nfn main() {\n\tleft_calls = 0\n\tright_calls = 0\n\ta := (none_int() + right_effect()) or { 0 }\n\tprintln(int_str(a))\n\tprintln(int_str(right_calls))\n\tb := (left_effect() + none_int()) or { 0 }\n\tprintln(int_str(b))\n\tprintln(int_str(left_calls))\n\tc := (some_int() + right_effect()) or { 0 }\n\tprintln(int_str(c))\n\tprintln(int_str(right_calls))\n}\n')
	assert out == '0\n0\n0\n1\n42\n1'
}

fn test_nested_optional_logical_or_preserves_short_circuiting() {
	out := or_review_run('nested_optional_logical_or_short_circuit',
		'__global calls int\n\nfn yes() ?bool {\n\treturn true\n}\n\nfn no() ?bool {\n\treturn false\n}\n\nfn fail_bool() ?bool {\n\tcalls++\n\treturn none\n}\n\nfn effect_true() ?bool {\n\tcalls++\n\treturn true\n}\n\nfn main() {\n\tcalls = 0\n\ta := (yes()? || fail_bool()?) or { false }\n\tprintln(if a { "true" } else { "false" })\n\tprintln(int_str(calls))\n\tcalls = 0\n\tb := (no()? && fail_bool()?) or { true }\n\tprintln(if b { "true" } else { "false" })\n\tprintln(int_str(calls))\n\tcalls = 0\n\tc := (no()? || effect_true()?) or { false }\n\tprintln(if c { "true" } else { "false" })\n\tprintln(int_str(calls))\n\tcalls = 0\n\td := (yes()? && effect_true()?) or { false }\n\tprintln(if d { "true" } else { "false" })\n\tprintln(int_str(calls))\n}\n')
	assert out == 'true\n0\nfalse\n0\ntrue\n1\ntrue\n1'
}

fn test_or_fallback_shadowed_noreturn_names_produce_values() {
	out := or_review_run('or_fallback_shadowed_noreturn_names',
		'fn maybe() ?int {\n\treturn none\n}\n\nfn main() {\n\texit := fn () int { return 7 }\n\tx := maybe() or { exit() }\n\tpanic := fn () int { return 9 }\n\ty := maybe() or { panic() }\n\tprintln(int_str(x))\n\tprintln(int_str(y))\n}\n')
	assert out == '7\n9'
}

fn test_user_defined_free_method_is_preserved() {
	out := or_review_run('user_defined_free_method',
		'struct Hc256 {\nmut:\n\tfreed int\n}\n\nfn (mut h Hc256) free() {\n\th.freed = 7\n}\n\nfn main() {\n\tmut h := Hc256{}\n\th.free()\n\tprintln(int_str(h.freed))\n}\n')
	assert out == '7'
}

fn test_default_free_without_user_method_stays_noop() {
	c_source := or_review_gen_c('default_free_without_user_method',
		'struct H {}\n\nfn main() {\n\tmut h := H{}\n\th.free()\n\tprintln("ok")\n}\n')
	assert c_source.contains('((void)0);'), 'default free did not lower to no-op'
	assert !c_source.contains('H__free('), 'default free emitted missing user method call'
}

fn test_backed_enum_map_key_uses_backing_storage_size() {
	c_source := or_review_gen_c('backed_enum_map_key_size',
		'enum Wide as u64 {\n\ta = 1\n\tb = 2\n}\n\nfn main() {\n\tmut m := map[Wide]int{}\n\tm[.a] = 3\n\tm[.b] = 4\n\tm.delete(.a)\n\tprintln(int_str(m[.b] or { 0 }))\n}\n')
	assert c_source.contains('new_map(sizeof(u64), sizeof(i64), map_hash_int_8, map_eq_int_8'), 'backed enum map key size does not match 8-byte callbacks'
	assert c_source.contains('u64 __map_key_'), 'backed enum map key temp does not use backing storage'
	assert !c_source.contains('Wide __map_key_'), 'backed enum map key temp still uses enum typedef storage'
	assert !c_source.contains('new_map(sizeof(main__Wide)'), 'backed enum map allocation still uses enum typedef size'
	assert !c_source.contains('&(main__Wide[]){'), 'backed enum map compound key literal still uses enum typedef storage'
}

fn test_pointer_channel_try_call_derefs_receiver() {
	c_source := or_review_gen_c('pointer_channel_try_receiver',
		'fn push(ch &chan int) bool {\n\treturn ch.try_push(7) == .success\n}\n\nfn pop(ch &chan int, out &int) bool {\n\treturn ch.try_pop(mut *out) == .success\n}\n\nfn main() {\n\tch := chan int{cap: 1}\n\tmut out := 0\n\tprintln(push(&ch))\n\tprintln(pop(&ch, &out))\n\tprintln(int_str(out))\n}\n')
	assert c_source.contains('sync__Channel__try_push(*(ch),'), 'try_push on pointer channel receiver does not dereference the channel handle'
	assert c_source.contains('sync__Channel__try_pop(*(ch),'), 'try_pop on pointer channel receiver does not dereference the channel handle'
}

fn test_channel_try_push_preserves_explicit_voidptr_cast() {
	out := or_review_run('channel_try_push_explicit_voidptr',
		'fn main() {\n\tch := chan voidptr{cap: 1}\n\tx := 42\n\tassert ch.try_push(voidptr(&x)) == .success\n\treceived := <-ch\n\tprintln(int_str(unsafe { *(&int(received)) }))\n}\n')
	assert out == '42'
}

fn test_pointer_channel_send_or_derefs_receiver() {
	out := or_review_run('pointer_channel_send_or_receiver',
		'fn send(ch &chan int) bool {\n\tch <- 7 or { return false }\n\treturn true\n}\n\nfn main() {\n\tch := chan int{cap: 1}\n\tprintln(send(&ch))\n\tprintln(int_str(<-ch))\n}\n')
	assert out == 'true\n7'
}

fn test_channel_send_or_preserves_optional_and_fixed_array_storage() {
	out := or_review_run('channel_send_or_storage',
		'fn make_option() ?int {\n\treturn 3\n}\n\nfn main() {\n\toption_ch := chan ?int{cap: 1}\n\toption_value := make_option()\n\toption_ch <- option_value or { panic(err) }\n\n\tfixed_ch := chan [2]int{cap: 1}\n\tfixed_value := [11, 13]!\n\tfixed_ch <- fixed_value or { panic(err) }\n\tprintln("sent")\n}\n')
	assert out == 'sent'
}

fn test_channel_send_or_binds_error_during_fallback_transform() {
	out := or_review_run('channel_send_or_error_interpolation',
		'fn main() {\n\terr := 7\n\tch := chan int{cap: 1}\n\tch.close()\n\tch <- 1 or {\n\t\tprintln("\${err}")\n\t}\n\tprintln(int_str(err))\n}\n')
	assert out == 'channel closed\n7'
}

fn test_or_storage_preserves_generic_map_value_type() {
	out := or_review_run('or_storage_generic_map_value',
		'struct Box[T] {\n\tvalue T\n}\n\nfn make_map() ?map[string]Box[int] {\n\treturn {"answer": Box[int]{value: 42}}\n}\n\nfn main() {\n\titems := make_map() or { map[string]Box[int]{} }\n\tprintln(int_str(items["answer"].value))\n}\n')
	assert out == '42'
}

fn test_optional_result_pointers_or_are_rejected() {
	option_output := or_review_compile_bad('optional_pointer_or_rejected',
		'fn invalid(ptr &?int) {\n\t_ := ptr or { 0 }\n}\n\nfn main() {}\n')
	assert option_output.contains('cannot use `or {}` block on non-option variable'), option_output
	result_output := or_review_compile_bad('result_pointer_or_rejected',
		'fn invalid(ptr &!int) {\n\t_ := ptr or { 0 }\n}\n\nfn main() {}\n')
	assert result_output.contains('cannot use `or {}` block on non-option variable'), result_output
}
