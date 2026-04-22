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
	assert generated_c.contains('typedef char* va_list;'), generated_c
	assert generated_c.contains('int vfprintf(FILE *stream, const char *format, va_list ap);'), generated_c

	assert generated_c.contains('int vsnprintf(char *str, size_t size, const char *format, va_list ap);'), generated_c

	assert generated_c.contains('void perror(const char *str);'), generated_c
	assert generated_c.contains('int mkstemp(char *stemplate);'), generated_c
	assert generated_c.contains('int strcmp(const char *left, const char *right);'), generated_c
	assert generated_c.contains('int rand(void);'), generated_c
	assert generated_c.contains('void srand(unsigned int seed);'), generated_c
	assert generated_c.contains('#define RAND_MAX'), generated_c
	assert generated_c.contains('double atof(const char *str);'), generated_c
	assert generated_c.contains('extern FILE* stdout;'), generated_c
	assert generated_c.contains('#define stdout (__acrt_iob_func(1))'), generated_c
	assert generated_c.contains('#if defined(_MSC_VER) && !defined(__clang__)\ntypedef struct _iobuf FILE;\ntypedef char* va_list;'), generated_c
	assert generated_c.contains('#elif defined(__MINGW32__) || defined(__MINGW64__) || (defined(__clang__) && (defined(_WIN32) || defined(_WIN64)))\ntypedef struct _iobuf FILE;\nFILE* __cdecl __acrt_iob_func(unsigned index);\n#define stdin  (__acrt_iob_func(0))\n#define stdout (__acrt_iob_func(1))\n#define stderr (__acrt_iob_func(2))'), generated_c
	assert generated_c.contains('#elif defined(__TINYC__) && (defined(_WIN32) || defined(_WIN64))'), generated_c
	assert generated_c.contains('#ifndef _FILE_DEFINED\nstruct _iobuf {\n\tchar *_ptr;\n\tint _cnt;\n\tchar *_base;\n\tint _flag;\n\tint _file;\n\tint _charbuf;\n\tint _bufsiz;\n\tchar *_tmpfname;\n};\ntypedef struct _iobuf FILE;\n#define _FILE_DEFINED'), generated_c
	assert generated_c.contains('FILE* __cdecl __iob_func(void);'), generated_c
	assert generated_c.contains('extern FILE (*_imp___iob)[];'), generated_c
	assert generated_c.contains('#define stdout (&__iob_func()[1])'), generated_c
	assert generated_c.contains('#if defined(__APPLE__) || defined(__FreeBSD__)\ntypedef struct __sFILE FILE;\nextern FILE* __stdinp;\nextern FILE* __stdoutp;\nextern FILE* __stderrp;\n#define stdin __stdinp\n#define stdout __stdoutp\n#define stderr __stderrp'), generated_c
	assert generated_c.contains('#elif defined(__NetBSD__) || defined(__DragonFly__)\ntypedef struct __sFILE FILE;\nextern FILE* __stdinp;\nextern FILE* __stdoutp;\nextern FILE* __stderrp;\n#define stdin __stdinp\n#define stdout __stdoutp\n#define stderr __stderrp'), generated_c
	assert generated_c.contains('#elif defined(__OpenBSD__)\ntypedef struct __sFILE FILE;\nextern FILE* __stdin;\nextern FILE* __stdout;\nextern FILE* __stderr;\n#define stdin __stdin\n#define stdout __stdout\n#define stderr __stderr'), generated_c
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
