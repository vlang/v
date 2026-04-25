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
	assert generated_c.contains('typedef struct _iobuf FILE;'), generated_c
	assert generated_c.contains('typedef struct __sFILE FILE;'), generated_c
	assert generated_c.contains('typedef struct _IO_FILE FILE;'), generated_c
	assert generated_c.contains('typedef __builtin_va_list va_list;'), generated_c
	assert generated_c.contains('V_CRT_LINKAGE int V_CRT_CALL vfprintf(FILE *stream, const char *format, va_list ap);'), generated_c

	assert generated_c.contains('V_CRT_LINKAGE int V_CRT_CALL vsnprintf(char *str, size_t size, const char *format, va_list ap);'), generated_c
	assert generated_c.contains('#if defined(_WIN32) || defined(_WIN64)\nV_CRT_LINKAGE int V_CRT_CALL _fileno(FILE *stream);\nV_CRT_LINKAGE FILE * V_CRT_CALL _wfopen(const unsigned short *filename, const unsigned short *mode);\nV_CRT_LINKAGE int V_CRT_CALL _wremove(const unsigned short *path);\n#endif'), generated_c

	assert generated_c.contains('V_CRT_LINKAGE void V_CRT_CALL perror(const char *str);'), generated_c
	assert generated_c.contains('V_CRT_LINKAGE int V_CRT_CALL mkstemp(char *stemplate);'), generated_c
	assert generated_c.contains('V_CRT_LINKAGE int V_CRT_CALL strcmp(const char *left, const char *right);'), generated_c
	assert generated_c.contains('V_CRT_LINKAGE int V_CRT_CALL rand(void);'), generated_c
	assert generated_c.contains('V_CRT_LINKAGE void V_CRT_CALL srand(unsigned int seed);'), generated_c
	assert generated_c.contains('RAND_MAX = 2147483647'), generated_c
	assert generated_c.contains('V_CRT_LINKAGE double V_CRT_CALL atof(const char *str);'), generated_c
	assert generated_c.contains('extern FILE* stdout;'), generated_c
	assert generated_c.contains('#define stdout (__acrt_iob_func(1))'), generated_c
	assert generated_c.contains('#if defined(_MSC_VER) && !defined(__clang__)\n#include <stdarg.h>\n#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>'), generated_c
	assert generated_c.contains('#if defined(_MSC_VER) && !defined(__clang__)\n\t#define V_CRT_LINKAGE __declspec(dllimport)\n\t#define V_CRT_CALL VCALLCONV(cdecl)\n#else\n\t#define V_CRT_LINKAGE\n\t#define V_CRT_CALL\n#endif'), generated_c
	assert generated_c.contains('V_CRT_LINKAGE int V_CRT_CALL _vscprintf(const char *format, va_list ap);'), generated_c
	assert generated_c.contains('V_CRT_LINKAGE int V_CRT_CALL _vsnprintf_s(char *buffer, size_t size, size_t count, const char *format, va_list ap);'), generated_c
	assert generated_c.contains('V_CRT_LINKAGE unsigned short * V_CRT_CALL _wgetenv(const unsigned short *varname);'), generated_c
	assert generated_c.contains('V_CRT_LINKAGE int V_CRT_CALL _wputenv(const unsigned short *envstring);'), generated_c
	assert generated_c.contains('#elif defined(__MINGW32__) || defined(__MINGW64__) || (defined(__clang__) && (defined(_WIN32) || defined(_WIN64)))\ntypedef struct _iobuf FILE;\nFILE* __cdecl __acrt_iob_func(unsigned index);\n#define stdin  (__acrt_iob_func(0))\n#define stdout (__acrt_iob_func(1))\n#define stderr (__acrt_iob_func(2))'), generated_c
	assert generated_c.contains('#elif defined(__TINYC__) && (defined(_WIN32) || defined(_WIN64))'), generated_c
	assert generated_c.contains('#ifndef _FILE_DEFINED\nstruct _iobuf {\n\tchar *_ptr;\n\tint _cnt;\n\tchar *_base;\n\tint _flag;\n\tint _file;\n\tint _charbuf;\n\tint _bufsiz;\n\tchar *_tmpfname;\n};\ntypedef struct _iobuf FILE;\n#define _FILE_DEFINED'), generated_c
	assert generated_c.contains('FILE* __cdecl __iob_func(void);'), generated_c
	assert generated_c.contains('extern FILE (*_imp___iob)[];'), generated_c
	assert generated_c.contains('#define stdout (&__iob_func()[1])'), generated_c
	assert generated_c.contains('#if defined(__APPLE__) || defined(__FreeBSD__)\ntypedef struct __sFILE FILE;\nextern FILE* __stdinp;\nextern FILE* __stdoutp;\nextern FILE* __stderrp;\n#define stdin __stdinp\n#define stdout __stdoutp\n#define stderr __stderrp'), generated_c
	assert generated_c.contains('#elif defined(__NetBSD__) || defined(__DragonFly__)\ntypedef struct __sFILE FILE;\nextern FILE* __stdinp;\nextern FILE* __stdoutp;\nextern FILE* __stderrp;\n#define stdin __stdinp\n#define stdout __stdoutp\n#define stderr __stderrp'), generated_c
	assert generated_c.contains('#elif defined(__OpenBSD__)\ntypedef struct __sFILE FILE;\n#ifndef _STDFILES_DECLARED\n\t#define _STDFILES_DECLARED\nstruct __sFstub { long _stub; };\nextern struct __sFstub __stdin[];\nextern struct __sFstub __stdout[];\nextern struct __sFstub __stderr[];\n#endif\n#define stdin ((struct __sFILE *)__stdin)\n#define stdout ((struct __sFILE *)__stdout)\n#define stderr ((struct __sFILE *)__stderr)'), generated_c
	assert generated_c.contains('#elif defined(__linux__) && !defined(__GLIBC__) && !defined(__GNU_LIBRARY__) && !defined(__BIONIC__) && !defined(__UCLIBC__)\ntypedef struct _IO_FILE FILE;\n// musl exposes the stdio streams as `FILE *const`, so match that to stay\n// compatible with later <stdio.h> includes from headers like miniz.h.\nextern FILE* const stdin;\nextern FILE* const stdout;\nextern FILE* const stderr'), generated_c
}

fn test_msvc_windows_prelude_uses_msvc_crt_headers() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'cheaders_msvc_windows_${os.getpid()}')
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, 'hello.v')
	output_path := os.join_path(tmp_dir, 'hello.c')
	os.write_file(source_path, 'fn main() {\n\tprintln("hi")\n}\n')!
	cmd := '${os.quoted_path(cheaders_manual_stdlib_vexe)} -cc msvc -os windows -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}'
	res := os.execute(cmd)
	assert res.exit_code == 0, '${cmd}\n${res.output}'
	generated_c := os.read_file(output_path)!.replace('\r\n', '\n')
	assert generated_c.contains('#if defined(_MSC_VER) && !defined(__clang__)\n#include <stdarg.h>\n#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>'), generated_c
	assert !generated_c.contains('V_CRT_IMPORT'), generated_c
	assert !generated_c.contains('#if defined(_MSC_VER) && !defined(__clang__)\ntypedef struct _iobuf FILE;'), generated_c
	assert generated_c.contains('#ifndef va_copy\n\t#define va_copy(dest, src) ((dest) = (src))\n#endif\n#ifndef _TRUNCATE'), generated_c
	assert generated_c.contains('#if defined(_MSC_VER) && !defined(__clang__)\n\t#define V_CRT_LINKAGE __declspec(dllimport)\n\t#define V_CRT_CALL VCALLCONV(cdecl)\n#else\n\t#define V_CRT_LINKAGE\n\t#define V_CRT_CALL\n#endif'), generated_c
	assert generated_c.contains('#if !defined(_MSC_VER) || defined(__clang__)\nV_CRT_LINKAGE int V_CRT_CALL vfprintf(FILE *stream, const char *format, va_list ap);'), generated_c
	assert generated_c.contains('V_CRT_LINKAGE int V_CRT_CALL _vscprintf(const char *format, va_list ap);'), generated_c
	assert generated_c.contains('V_CRT_LINKAGE int V_CRT_CALL _vsnprintf_s(char *buffer, size_t size, size_t count, const char *format, va_list ap);'), generated_c
	assert generated_c.contains('#include <windows.h>'), generated_c
}

fn test_msvc_windows_splits_large_string_const_literals() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'cheaders_msvc_long_string_${os.getpid()}')
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	long_text := 'a'.repeat(25000)
	source_path := os.join_path(tmp_dir, 'long_string.v')
	output_path := os.join_path(tmp_dir, 'long_string.c')
	os.write_file(source_path,
		"const long_literal = '${long_text}'\n\nfn main() {\n\tassert long_literal.len == ${long_text.len}\n}\n")!
	cmd := '${os.quoted_path(cheaders_manual_stdlib_vexe)} -cc msvc -os windows -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}'
	res := os.execute(cmd)
	assert res.exit_code == 0, '${cmd}\n${res.output}'
	generated_c := os.read_file(output_path)!.replace('\r\n', '\n')
	marker := 'long_literal = _S('
	start := generated_c.index(marker) or {
		assert false, generated_c
		return
	}
	line := generated_c[start..].all_before(');')
	assert line.contains('" "'), line
	assert !line.contains('a'.repeat(20000)), line
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

fn test_manual_stdio_decls_allow_headerless_rand_declarations() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'cheaders_manual_stdlib_rand_${os.getpid()}')
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, 'c_rand.v')
	os.write_file(source_path,
		['fn C.rand() int', 'fn C.srand(seed u32)', '', 'fn main() {', '\tC.srand(1)', '\t_ = C.rand()', '}'].join('\n') +
		'\n')!
	output_path := os.join_path(tmp_dir, 'c_rand')
	cmd := '${os.quoted_path(cheaders_manual_stdlib_vexe)} -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}'
	res := os.execute(cmd)
	assert res.exit_code == 0, '${cmd}\n${res.output}'
}

fn test_manual_stdio_decls_allow_rand_max_macro_usage() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'cheaders_manual_stdlib_rand_max_${os.getpid()}')
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, 'c_rand_max.v')
	os.write_file(source_path,
		['const rand_max = C.RAND_MAX', '', 'fn main() {', '\tassert rand_max > 0', '}'].join('\n') +
		'\n')!
	output_path := os.join_path(tmp_dir, 'c_rand_max')
	cmd := '${os.quoted_path(cheaders_manual_stdlib_vexe)} -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}'
	res := os.execute(cmd)
	assert res.exit_code == 0, '${cmd}\n${res.output}'
}

fn test_manual_stdio_decls_allow_direct_atof_calls() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'cheaders_manual_stdlib_atof_${os.getpid()}')
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, 'c_atof.v')
	os.write_file(source_path, ['fn main() {', "\t_ = C.atof(c'1.25')", '}'].join('\n') + '\n')!
	output_path := os.join_path(tmp_dir, 'c_atof')
	cmd := '${os.quoted_path(cheaders_manual_stdlib_vexe)} -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}'
	res := os.execute(cmd)
	assert res.exit_code == 0, '${cmd}\n${res.output}'
}
