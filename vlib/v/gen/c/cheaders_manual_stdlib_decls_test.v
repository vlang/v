import os

const cheaders_manual_stdlib_vexe = @VEXE

const cheaders_manual_stdlib_vroot = os.real_path(@VMODROOT)

const cheaders_manual_stdlib_varargs_source = os.join_path(cheaders_manual_stdlib_vroot,
	'vlib/v/gen/c/testdata/c_varargs.vv')

const cheaders_manual_stdlib_stdio_source = os.join_path(cheaders_manual_stdlib_vroot,
	'vlib/v/tests/c_shadowed_c_fn_call_test.v')

fn test_default_c_prelude_uses_manual_stdio_stdlib_string_and_stdarg_decls() {
	cmd := '${os.quoted_path(cheaders_manual_stdlib_vexe)} -o - ${os.quoted_path(cheaders_manual_stdlib_varargs_source)}'
	res := os.execute(cmd)
	assert res.exit_code == 0, '${cmd}\n${res.output}'
	generated_c := res.output.replace('\r\n', '\n')
	assert !generated_c.contains('#include <stdio.h>'), generated_c
	assert !generated_c.contains('#include <stdlib.h>'), generated_c
	assert !generated_c.contains('#include <string.h>'), generated_c
	assert !generated_c.contains('#include <stdarg.h>'), generated_c
	assert generated_c.contains('typedef struct _iobuf FILE;'), generated_c
	assert generated_c.contains('typedef struct __sFILE FILE;'), generated_c
	assert generated_c.contains('typedef struct _IO_FILE FILE;'), generated_c
	assert generated_c.contains('typedef __builtin_va_list va_list;'), generated_c
	assert generated_c.contains('typedef char* va_list;'), generated_c
	assert generated_c.contains('int vfprintf(FILE *stream, const char *format, va_list ap);'), generated_c

	assert generated_c.contains('int vsnprintf(char *str, size_t size, const char *format, va_list ap);'), generated_c

	assert generated_c.contains('void perror(const char *str);'), generated_c
	assert generated_c.contains('int strcmp(const char *left, const char *right);'), generated_c
	assert generated_c.contains('extern FILE* stdout;'), generated_c
	assert generated_c.contains('#define stdout (__acrt_iob_func(1))'), generated_c
}

fn test_manual_stdio_decls_do_not_conflict_with_later_stdio_includes() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'cheaders_manual_stdlib_${os.getpid()}')
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	output_path := os.join_path(tmp_dir, 'shadowed_c_fn_call_test')
	cmd := '${os.quoted_path(cheaders_manual_stdlib_vexe)} -o ${os.quoted_path(output_path)} ${os.quoted_path(cheaders_manual_stdlib_stdio_source)}'
	res := os.execute(cmd)
	assert res.exit_code == 0, '${cmd}\n${res.output}'
}

fn test_manual_stdio_decls_allow_headerless_perror_declarations() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'cheaders_manual_stdlib_perror_${os.getpid()}')
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, 'c_perror.v')
	os.write_file(source_path,

		['fn C.perror(message &char)', '', 'fn main() {', "\tC.perror(c'')", '}'].join('\n') + '\n')!
	output_path := os.join_path(tmp_dir, 'c_perror')
	cmd := '${os.quoted_path(cheaders_manual_stdlib_vexe)} -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}'
	res := os.execute(cmd)
	assert res.exit_code == 0, '${cmd}\n${res.output}'
}
