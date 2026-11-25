import os
import v.tempname

fn fn_nil_run_good(root string, name string, src string) string {
	good_src := os.join_path(root, 'v3_${name}.v')
	os.write_file(good_src, src) or { panic(err) }
	good_bin := os.join_path(root, 'v3_${name}')
	compile := os.exec([@VEXE, '${good_src}', '-b', 'c', '-o', good_bin])
	assert compile.exit_code == 0, '${name}: compile failed: ${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed: ${compile.output}'
	run := os.exec([good_bin])
	assert run.exit_code == 0, '${name}: run failed: ${run.output}'
	return run.output.trim_space()
}

fn fn_nil_run_bad(root string, name string, src string, expected string) {
	bad_src := os.join_path(root, 'v3_${name}.v')
	os.write_file(bad_src, src) or { panic(err) }
	bad_bin := os.join_path(root, 'v3_${name}')
	result := os.exec([@VEXE, '${bad_src}', '-b', 'c', '-o', bad_bin])
	assert result.exit_code != 0, '${name}: unexpectedly compiled: ${result.output}'
	assert result.output.contains(expected), '${name}: missing `${expected}` in ${result.output}'
	assert !result.output.contains('C compilation failed')
}

fn test_unsafe_nil_contextually_matches_fn_fields() {
	root := os.join_path(os.vtmp_dir(), 'v3_fn_nil_context_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	default_out := fn_nil_run_good(root, 'fn_nil_default', 'struct S {
mut:
	f fn (int) ! = unsafe { nil }
}

fn main() {
	mut value := S{}
	value.f = unsafe { nil }
	println(int_str(1))
}
')
	assert default_out == '1'
	alias_out := fn_nil_run_good(root, 'fn_alias_nil_field_init', 'type Callback = fn (data voidptr)

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
	fn_nil_run_bad(root, 'fn_field_rejects_voidptr_variable', 'struct S {
	f fn (int) !
}

fn main() {
	v := unsafe { nil }
	_ := S{
		f: v
	}
}
',
		'cannot assign to field `f`: expected `fn(int) !void`, not `voidptr`')
	fn_nil_run_bad(root, 'fn_field_assignment_rejects_voidptr_variable', 'struct S {
	mut:
		f fn (int) !
}

fn main() {
	mut value := S{}
	v := unsafe { nil }
	value.f = v
}
',
		'cannot assign to `value.f`: expected `fn (int) !void`, not `voidptr`')
	fn_nil_run_bad(root, 'fn_field_rejects_non_nil_unsafe_pointer', 'struct S {
	f fn (int) !
}

fn main() {
	_ := S{
		f: unsafe { &int(0) }
	}
}
',
		'cannot assign to field `f`: expected `fn(int) !void`, not `&int`')
	fn_nil_run_bad(root, 'fn_field_rejects_voidptr_zero', 'struct S {
	f fn (int) !
}

fn main() {
	_ := S{
		f: voidptr(0)
	}
}
',
		'cannot assign to field `f`: expected `fn(int) !void`, not `voidptr`')
	fn_nil_run_bad(root, 'fn_field_rejects_integer_zero', 'struct S {
	f fn (int) !
}

fn main() {
	_ := S{
		f: 0
	}
}
',
		'cannot assign to field `f`: expected `fn(int) !void`, not `int literal`')
	fn_nil_run_bad(root, 'fn_field_rejects_wrong_arity', 'fn no_args() ! {
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
		'cannot assign to field `f`: expected `fn (int) !`, not `fn () !`')
	fn_nil_run_bad(root, 'fn_field_rejects_wrong_return', 'fn returns_int(i int) !int {
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
		'cannot assign to field `f`: expected `fn (int) !`, not `fn (int) !int`')
	fn_nil_run_bad(root, 'fn_alias_field_rejects_wrong_arity', 'type Callback = fn (int) !

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
		'cannot assign to field `f`: expected `fn (int) !`, not `fn () !`')
}
