import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_fn_nil_context_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn run_good(v3_bin string, name string, src string) string {
	good_src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(good_src, src) or { panic(err) }
	good_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	compile := os.execute('${v3_bin} ${good_src} -b c -o ${good_bin}')
	assert compile.exit_code == 0, '${name}: compile failed: ${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed: ${compile.output}'
	run := os.execute(good_bin)
	assert run.exit_code == 0, '${name}: run failed: ${run.output}'
	return run.output.trim_space()
}

fn run_bad(v3_bin string, name string, src string, expected string) {
	bad_src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(bad_src, src) or { panic(err) }
	bad_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	result := os.execute('${v3_bin} ${bad_src} -b c -o ${bad_bin}')
	assert result.exit_code != 0
	assert result.output.contains(expected), '${name}: missing `${expected}` in ${result.output}'
	assert !result.output.contains('C compilation failed')
}

fn test_unsafe_nil_contextually_matches_fn_fields() {
	v3_bin := build_v3()
	default_out := run_good(v3_bin, 'fn_nil_default', 'struct S {
	f fn (int) ! = unsafe { nil }
}

fn main() {
	_ := S{}
	println(int_str(1))
}
')
	assert default_out == '1'
	alias_out := run_good(v3_bin, 'fn_alias_nil_field_init', 'type Callback = fn (data voidptr)

struct S {
	f Callback
}

fn main() {
	_ := S{
		f: unsafe { nil }
	}
	println(int_str(2))
}
')
	assert alias_out == '2'
	run_bad(v3_bin, 'fn_field_rejects_voidptr_variable', 'struct S {
	f fn (int) !
}

fn main() {
	v := unsafe { nil }
	_ := S{
		f: v
	}
}
',
		'cannot initialize field `f` with `&void`; expected `fn(int) !void`')
	run_bad(v3_bin, 'fn_field_rejects_non_nil_unsafe_pointer', 'struct S {
	f fn (int) !
}

fn main() {
	_ := S{
		f: unsafe { &int(0) }
	}
}
',
		'cannot initialize field `f` with `&int`; expected `fn(int) !void`')
	run_bad(v3_bin, 'fn_field_rejects_voidptr_zero', 'struct S {
	f fn (int) !
}

fn main() {
	_ := S{
		f: voidptr(0)
	}
}
',
		'cannot initialize field `f` with `&void`; expected `fn(int) !void`')
	run_bad(v3_bin, 'fn_field_rejects_integer_zero', 'struct S {
	f fn (int) !
}

fn main() {
	_ := S{
		f: 0
	}
}
',
		'cannot initialize field `f` with `int`; expected `fn(int) !void`')
	run_bad(v3_bin, 'fn_field_rejects_wrong_arity', 'fn no_args() ! {
	return
}

struct S {
	f fn (int) !
}

fn main() {
	_ := S{
		f: no_args
	}
}
',
		'cannot initialize field `f` with `fn() !void`; expected `fn(int) !void`')
	run_bad(v3_bin, 'fn_field_rejects_wrong_return', 'fn returns_int(i int) !int {
	return i
}

struct S {
	f fn (int) !
}

fn main() {
	_ := S{
		f: returns_int
	}
}
',
		'cannot initialize field `f` with `fn(int) !int`; expected `fn(int) !void`')
	run_bad(v3_bin, 'fn_alias_field_rejects_wrong_arity', 'type Callback = fn (int) !

fn no_args() ! {
	return
}

struct S {
	f Callback
}

fn main() {
	_ := S{
		f: no_args
	}
}
',
		'cannot initialize field `f` with `fn() !void`; expected `Callback`')
}
