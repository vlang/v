module main

import os
import cvariadic28883

const vexe = @VEXE

fn format_with(f fn (&char) int) (int, string) {
	buf := []u8{len: 64}
	// unsafe: C interop needs a raw `&char` view of `buf`; `buf` outlives every use of `ptr`.
	ptr := unsafe { &char(buf.data) }
	n := f(ptr)
	// unsafe: reads a C string; the helpers write through vsnprintf with size 64, the length
	// of the zeroed `buf`, so the text is always NUL-terminated inside `buf`.
	return n, unsafe { cstring_to_vstring(ptr) }
}

// https://github.com/vlang/v/issues/28883
fn test_unqualified_c_variadic_calls_forward_args() {
	mut n, mut text := format_with(cvariadic28883.same_name_plain)
	assert n == 5
	assert text == 'plain'
	n, text = format_with(cvariadic28883.same_name_args)
	assert n == 3
	assert text == '4=7'
	n, text = format_with(cvariadic28883.format_plain)
	assert n == 5
	assert text == 'plain'
	n, text = format_with(cvariadic28883.format_args)
	assert n == 6
	assert text == '1-42-3'
}

fn c_call_lines(generated string, prefix string) string {
	return generated.split_into_lines().filter(it.contains(prefix)).join('\n')
}

// https://github.com/vlang/v/issues/28883
fn test_unqualified_c_variadic_call_without_args_needs_no_array_runtime() {
	root := os.join_path(os.vtmp_dir(), 'c_variadic_unqualified_call_28883_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'foo'))!
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), '')!
	os.write_file(os.join_path(root, 'main.v'), [
		'module main',
		'',
		'import foo',
		'',
		'fn main() {',
		'\tfoo.bar()',
		'}',
	].join_lines())!
	os.write_file(os.join_path(root, 'foo', 'foo.v'), [
		'module foo',
		'',
		'pub fn foo(value voidptr, ...) {}',
		'',
		'pub fn qux(value voidptr, ...) {}',
		'',
		'pub fn bar() {',
		"\tfoo(c'bar')",
		"\tqux(c'bar')",
		"\tqux(c'bar', 1, 2)",
		'}',
	].join_lines())!
	output := os.join_path(root, 'generated.c')
	res :=
		os.execute('${os.quoted_path(vexe)} -new-compiler -gc none -no-builtin -o ${os.quoted_path(output)} ${os.quoted_path(os.join_path(root, 'main.v'))}')
	assert res.exit_code == 0, res.output
	generated := os.read_file(output)!
	calls := c_call_lines(generated, 'foo__')
	assert generated.contains('\tfoo__foo("bar");'), calls
	assert generated.contains('\tfoo__qux("bar");'), calls
	assert generated.contains('\tfoo__qux("bar", 1, 2);'), calls
	assert !generated.contains('array_new'), calls
}
