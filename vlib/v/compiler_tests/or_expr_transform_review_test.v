import os
import v.tempname

fn or_review_gen_c(name string, cases map[string]string) string {
	mut fixtures := []string{cap: cases.len}
	mut calls := []string{cap: cases.len}
	for fixture, source in cases {
		fixtures << source.replace_once('fn main()', 'fn ${fixture}()')
		calls << '${fixture}()'
	}
	src := '${fixtures.join('\n')}\nfn main() {\n\t${calls.join('\n\t')}\n}\n'
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
	bin_path := or_review_compile_fixture(dir, name, src)
	run := os.exec([bin_path])
	assert run.exit_code == 0, '${name}: run failed\n${run.output}'
	return run.output.trim_space()
}

fn or_review_compile_fixture(dir string, name string, src string) string {
	bin_path := os.join_path(dir, 'v3_${name}')
	src_path := bin_path + '.v'
	os.write_file(src_path, src) or { panic(err) }
	compile := os.exec([@VEXE, '-enable-globals', src_path, '-b', 'c', '-o', bin_path])
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	return bin_path
}

fn or_review_run_batch(name string, cases map[string]OrReviewFixture, read_c bool) string {
	dir := os.join_path(os.vtmp_dir(), 'v3_or_review_${tempname.unique_token()}')
	os.mkdir(dir) or { panic(err) }
	defer { os.rmdir_all(dir) or {} }
	mut fixtures := []string{cap: cases.len}
	mut calls := []string{cap: cases.len}
	for fixture, case in cases {
		fixtures << case.source.replace_once('fn main()', 'fn ${fixture}()')
		calls << "\t\t'${fixture}' { ${fixture}() }"
	}
	src := "import os

${fixtures.join('\n')}
fn main() {
	match os.args[1] {
${calls.join('\n')}
		else { panic('unknown fixture') }
	}
}
"
	bin_path := or_review_compile_fixture(dir, name, src)
	for fixture, case in cases {
		run := os.exec([bin_path, fixture])
		assert run.exit_code == 0, '${fixture}: run failed\n${run.output}'
		assert run.output.trim_space() == case.expected, '${fixture}: unexpected output\n${run.output}'
	}
	return if read_c {
		os.read_file(bin_path + '.c') or { panic(err) }
	} else {
		''
	}
}

fn or_review_c_body(c_source string, signature_prefix string) string {
	marker := '\n${signature_prefix}'
	start := c_source.last_index(marker) or {
		panic('missing generated C function for ${signature_prefix}')
	}
	definition := c_source[start + marker.len..]
	signature, rest := definition.split_once('\n') or {
		panic('missing generated C signature for ${signature_prefix}')
	}
	assert signature.ends_with(' {'), 'missing generated C definition for ${signature_prefix}'
	body, _ := rest.split_once('\n}') or {
		panic('missing generated C body end for ${signature_prefix}')
	}
	return body
}

struct OrReviewFixture {
	source   string
	expected string
}

fn or_review_check_rejections(cases map[string]OrReviewFixture) {
	dir := os.join_path(os.vtmp_dir(), 'v3_or_review_${tempname.unique_token()}')
	os.mkdir(dir) or { panic(err) }
	defer { os.rmdir_all(dir) or {} }
	mut imports := []string{cap: cases.len}
	for name, case in cases {
		module_name := 'fixture_${imports.len}'
		module_dir := os.join_path(dir, module_name)
		os.mkdir(module_dir) or { panic(err) }
		imports << 'import ${module_name} as _'
		os.write_file(os.join_path(module_dir, '${name}.v'), 'module ${module_name}\n\n' + case.source) or {
			panic(err)
		}
	}
	main_path := os.join_path(dir, 'main.v')
	os.write_file(main_path, 'module main\n\n${imports.join('\n')}\n\nfn main() {}\n') or { panic(err) }
	compile := os.exec([@VEXE, '-b', 'c', '-nocolor', '-o', os.join_path(dir, 'program'), main_path])
	assert compile.exit_code != 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	mut errors := map[string][]string{}
	for line in compile.output.split_into_lines() {
		location, message := line.split_once(': error: ') or { continue }
		// Drop the line and column from the right to preserve any colon in the path.
		file := location.all_before_last(':').all_before_last(':')
		errors[os.file_name(file)] << message
	}
	for name, case in cases {
		assert errors['${name}.v'].any(it.contains(case.expected)), '${name}: expected `${case.expected}` in\n${compile.output}'
	}
}

fn test_or_review_codegen_fixtures() {
	c_source := or_review_gen_c('or_review_codegen', {
		'channel_receive_or_source_once':   '__global calls int\n\nfn next_channel() chan int {\n\tcalls++\n\tch := chan int{}\n\tch.close()\n\treturn ch\n}\n\nfn main() {\n\tx := (<-next_channel()) or {\n\t\tprintln(err.msg())\n\t\t-1\n\t}\n\tprintln(int_str(x))\n}\n'
		'default_free_without_user_method': 'struct H {}\n\nfn main() {\n\tmut h := H{}\n\th.free()\n\tprintln("ok")\n}\n'
		'backed_enum_map_key_size':         'enum Wide as u64 {\n\ta = 1\n\tb = 2\n}\n\nfn main() {\n\tmut m := map[Wide]int{}\n\tm[.a] = 3\n\tm[.b] = 4\n\tm.delete(.a)\n\tprintln(int_str(m[.b] or { 0 }))\n}\n'
		'pointer_channel_try_receiver':     'fn push(ch &chan int) bool {\n\treturn ch.try_push(7) == .success\n}\n\nfn pop(ch &chan int, out &int) bool {\n\treturn ch.try_pop(mut *out) == .success\n}\n\nfn main() {\n\tch := chan int{cap: 1}\n\tmut out := 0\n\tprintln(push(&ch))\n\tprintln(pop(&ch, &out))\n\tprintln(int_str(out))\n}\n'
	})
	receive_body := or_review_c_body(c_source, 'void channel_receive_or_source_once(')
	assert receive_body.count('next_channel()') == 1, 'side-effectful channel source was not stabilized'
	assert receive_body.contains('sync__Channel* __chan_src_'), 'missing channel source temp'
	assert receive_body.contains('sync__Channel__pop(__chan_src_'), 'pop does not use channel source temp'
	assert receive_body.contains('sync__Channel__closed_error(__chan_src_'), 'closed_error does not use channel source temp'
	free_body := or_review_c_body(c_source, 'void default_free_without_user_method(')
	assert free_body.contains('((void)0);'), 'default free did not lower to no-op'
	assert !c_source.contains('H__free('), 'default free emitted missing user method call'
	map_body := or_review_c_body(c_source, 'void backed_enum_map_key_size(')
	assert map_body.contains('new_map(sizeof(u64), sizeof(i64), map_hash_int_8, map_eq_int_8'), 'backed enum map key size does not match 8-byte callbacks'
	assert map_body.contains('u64 __map_key_'), 'backed enum map key temp does not use backing storage'
	assert !c_source.contains('Wide __map_key_'), 'backed enum map key temp still uses enum typedef storage'
	assert !c_source.contains('new_map(sizeof(main__Wide)'), 'backed enum map allocation still uses enum typedef size'
	assert !c_source.contains('&(main__Wide[]){'), 'backed enum map compound key literal still uses enum typedef storage'
	push_body := or_review_c_body(c_source, 'bool push(')
	assert push_body.contains('sync__Channel__try_push(*(ch),'), 'try_push on pointer channel receiver does not dereference the channel handle'
	pop_body := or_review_c_body(c_source, 'bool pop(')
	assert pop_body.contains('sync__Channel__try_pop(*(ch),'), 'try_pop on pointer channel receiver does not dereference the channel handle'
}

fn test_optional_element_or_uses_fallback_without_err_binding() {
	c_source := or_review_run_batch('optional_element_fallback', {
		'array_optional_element_fallback_run': OrReviewFixture{
			source:   'fn main() {\n\terr := 7\n\tmut arr := []?int{}\n\tarr << none\n\tarr << 5\n\tmissing := arr[0] or { err }\n\tpresent := arr[1] or { -1 }\n\tprintln(int_str(missing))\n\tprintln(int_str(present))\n}\n'
			expected: '7\n5'
		}
		'map_optional_element_fallback_run':   OrReviewFixture{
			source:   "fn main() {\n\terr := 7\n\tmut m := map[string]?int{}\n\tm['x'] = none\n\tm['y'] = 5\n\tmissing := m['x'] or { err }\n\tpresent := m['y'] or { -1 }\n\tabsent := m['z'] or { -2 }\n\tprintln(int_str(missing))\n\tprintln(int_str(present))\n\tprintln(int_str(absent))\n}\n"
			expected: '7\n5\n-2'
		}
	}, true)
	array_body := or_review_c_body(c_source, 'void array_optional_element_fallback_run(')
	assert array_body.contains('__v_option_i64 __arr_opt_'), 'missing loaded optional temp'
	assert array_body.contains('i64 err = 7;'), 'explicit outer err was not preserved'
	assert !array_body.contains('IError err = __arr_opt_'), 'element failure branch binds an Option error'
	map_body := or_review_c_body(c_source, 'void map_optional_element_fallback_run(')
	assert map_body.contains('__v_option_i64 __map_opt_'), 'missing loaded map optional temp'
	assert map_body.contains('i64 err = 7;'), 'explicit outer err was not preserved'
	assert !map_body.contains('IError err = __map_opt_'), 'map element failure branch binds an Option error'
}

fn test_nested_optional_logical_or_preserves_short_circuiting() {
	out := or_review_run('nested_optional_logical_or_short_circuit',
		'__global calls int\n\nfn yes() ?bool {\n\treturn true\n}\n\nfn no() ?bool {\n\treturn false\n}\n\nfn fail_bool() ?bool {\n\tcalls++\n\treturn none\n}\n\nfn effect_true() ?bool {\n\tcalls++\n\treturn true\n}\n\nfn main() {\n\tcalls = 0\n\ta := (yes()? || fail_bool()?) or { false }\n\tprintln(if a { "true" } else { "false" })\n\tprintln(int_str(calls))\n\tcalls = 0\n\tb := (no()? && fail_bool()?) or { true }\n\tprintln(if b { "true" } else { "false" })\n\tprintln(int_str(calls))\n\tcalls = 0\n\tc := (no()? || effect_true()?) or { false }\n\tprintln(if c { "true" } else { "false" })\n\tprintln(int_str(calls))\n\tcalls = 0\n\td := (yes()? && effect_true()?) or { false }\n\tprintln(if d { "true" } else { "false" })\n\tprintln(int_str(calls))\n}\n')
	assert out == 'true\n0\nfalse\n0\ntrue\n1\ntrue\n1'
}

fn test_or_review_runtime_fixtures() {
	or_review_run_batch('or_review_runtime', {
		'channel_receive_or_selector':         OrReviewFixture{
			source:   "struct Holder {\n\tch chan int\n}\n\nfn main() {\n\th := Holder{ch: chan int{}}\n\th.ch.close()\n\tvalue := <-h.ch or {\n\t\tprintln('closed')\n\t\treturn\n\t}\n\tprintln(int_str(value))\n}\n"
			expected: 'closed'
		}
		'channel_try_push_explicit_voidptr':   OrReviewFixture{
			source:   'fn main() {\n\tch := chan voidptr{cap: 1}\n\tx := 42\n\tassert ch.try_push(voidptr(&x)) == .success\n\treceived := <-ch\n\tprintln(int_str(unsafe { *(&int(received)) }))\n}\n'
			expected: '42'
		}
		'pointer_channel_send_or_receiver':    OrReviewFixture{
			source:   'fn send(ch &chan int) bool {\n\tch <- 7 or { return false }\n\treturn true\n}\n\nfn main() {\n\tch := chan int{cap: 1}\n\tprintln(send(&ch))\n\tprintln(int_str(<-ch))\n}\n'
			expected: 'true\n7'
		}
		'channel_send_or_storage':             OrReviewFixture{
			source:   'fn make_option() ?int {\n\treturn 3\n}\n\nfn main() {\n\toption_ch := chan ?int{cap: 1}\n\toption_value := make_option()\n\toption_ch <- option_value or { panic(err) }\n\n\tfixed_ch := chan [2]int{cap: 1}\n\tfixed_value := [11, 13]!\n\tfixed_ch <- fixed_value or { panic(err) }\n\tprintln("sent")\n}\n'
			expected: 'sent'
		}
		'channel_send_or_error_interpolation': OrReviewFixture{
			source:   'fn main() {\n\terr := 7\n\tch := chan int{cap: 1}\n\tch.close()\n\tch <- 1 or {\n\t\tprintln("\${err}")\n\t}\n\tprintln(int_str(err))\n}\n'
			expected: 'channel closed\n7'
		}
		'map_index_address_or_nil':            OrReviewFixture{
			source:   "fn main() {\n\tmut values := {\n\t\t'x': 7\n\t}\n\tpresent := unsafe { &values['x'] or { nil } }\n\tmissing := unsafe { &values['missing'] or { nil } }\n\tprintln(int_str(*present))\n\tprintln(missing == unsafe { nil })\n}\n"
			expected: '7\ntrue'
		}
		'nested_optional_infix_or_fallback':   OrReviewFixture{
			source:   '__global left_calls int\n__global right_calls int\n\nfn none_int() ?int {\n\treturn none\n}\n\nfn some_int() ?int {\n\treturn 2\n}\n\nfn left_effect() int {\n\tleft_calls++\n\treturn 40\n}\n\nfn right_effect() int {\n\tright_calls++\n\treturn 40\n}\n\nfn main() {\n\tleft_calls = 0\n\tright_calls = 0\n\ta := (none_int() + right_effect()) or { 0 }\n\tprintln(int_str(a))\n\tprintln(int_str(right_calls))\n\tb := (left_effect() + none_int()) or { 0 }\n\tprintln(int_str(b))\n\tprintln(int_str(left_calls))\n\tc := (some_int() + right_effect()) or { 0 }\n\tprintln(int_str(c))\n\tprintln(int_str(right_calls))\n}\n'
			expected: '0\n0\n0\n1\n42\n1'
		}
		'or_fallback_shadowed_noreturn_names': OrReviewFixture{
			source:   'fn maybe() ?int {\n\treturn none\n}\n\nfn main() {\n\texit := fn () int { return 7 }\n\tx := maybe() or { exit() }\n\tpanic := fn () int { return 9 }\n\ty := maybe() or { panic() }\n\tprintln(int_str(x))\n\tprintln(int_str(y))\n}\n'
			expected: '7\n9'
		}
		'user_defined_free_method':            OrReviewFixture{
			source:   'struct Hc256 {\nmut:\n\tfreed int\n}\n\nfn (mut h Hc256) free() {\n\th.freed = 7\n}\n\nfn main() {\n\tmut h := Hc256{}\n\th.free()\n\tprintln(int_str(h.freed))\n}\n'
			expected: '7'
		}
		'or_storage_generic_map_value':        OrReviewFixture{
			source:   'struct Box[T] {\n\tvalue T\n}\n\nfn make_map() ?map[string]Box[int] {\n\treturn {"answer": Box[int]{value: 42}}\n}\n\nfn main() {\n\titems := make_map() or { map[string]Box[int]{} }\n\tprintln(int_str(items["answer"].value))\n}\n'
			expected: '42'
		}
	}, false)
}

fn test_or_rejects_non_options_and_implicit_element_errors() {
	non_option := 'cannot use `or {}` block on non-option variable'
	implicit_err := 'undefined variable: `err`'
	or_review_check_rejections({
		'bare_channel_or_rejected':            OrReviewFixture{
			source:   'fn main() {\n\tch := chan int{}\n\t_ := ch or { ch }\n}\n'
			expected: non_option
		}
		'array_optional_element_implicit_err': OrReviewFixture{
			source:   'fn main() {\n\tmut arr := []?int{}\n\tarr << none\n\tvalue := arr[0] or {\n\t\tprintln(err.msg())\n\t\t0\n\t}\n\tprintln(int_str(value))\n}\n'
			expected: implicit_err
		}
		'map_optional_element_implicit_err':   OrReviewFixture{
			source:   "fn main() {\n\tmut m := map[string]?int{}\n\tm['x'] = none\n\tvalue := m['x'] or {\n\t\tprintln(err.msg())\n\t\t0\n\t}\n\tprintln(int_str(value))\n}\n"
			expected: implicit_err
		}
		'optional_pointer_or_rejected':        OrReviewFixture{
			source:   'fn invalid(ptr &?int) {\n\t_ := ptr or { 0 }\n}\n\nfn main() {}\n'
			expected: non_option
		}
		'result_pointer_or_rejected':          OrReviewFixture{
			source:   'fn invalid(ptr &!int) {\n\t_ := ptr or { 0 }\n}\n\nfn main() {}\n'
			expected: non_option
		}
	})
}
