import os
import v3.cmdexec

const fastc_backend_v3_dir = os.dir(os.dir(@FILE))
const fastc_backend_vlib_dir = os.dir(fastc_backend_v3_dir)
const fastc_backend_v3_source = os.join_path(fastc_backend_v3_dir, 'v3.v')

struct UnsupportedFastCInvocation {
	args     []string
	expected string
}

fn write_fastc_test_source(path string, source string) {
	os.write_file(path, source) or { panic(err) }
}

fn test_fastc_backend_parses_directly_to_c_without_ast_fallback() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_backend_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := os.join_path(root, 'v3')
	build := cmdexec.run(@VEXE, ['-gc', 'none', '-path', '${fastc_backend_vlib_dir}|@vlib|@vmodules',
		'-o', v3_bin, fastc_backend_v3_source])
	assert build.exit_code == 0, build.output

	valid_source := os.join_path(root, 'valid.v')
	write_fastc_test_source(valid_source, 'module main

fn twice(value int) int {
	return value * 2
}

fn main() {
	value := twice(21)
	println(value)
}
')
	valid_binary := os.join_path(root, 'valid')
	valid_compile := cmdexec.run(v3_bin, ['-macos-v3-compat-c99', '-b', 'fastc', '-o', valid_binary,
		valid_source])
	assert valid_compile.exit_code == 0, valid_compile.output
	assert valid_compile.output.contains('fastc parse+gen'), valid_compile.output
	assert !valid_compile.output.contains(' check'), valid_compile.output
	assert !valid_compile.output.contains(' transform'), valid_compile.output
	assert !valid_compile.output.contains('markused'), valid_compile.output
	retained_c := os.read_file(valid_binary + '.c') or { panic(err) }
	assert retained_c.contains('__typeof__((twice(21))) value = (twice(21));')
	assert retained_c.contains('setvbuf(stdout, NULL, _IONBF, 0);')
	assert !retained_c.contains('builtin__builtin_init')
	valid_run := cmdexec.run(valid_binary, [])
	assert valid_run.exit_code == 0, valid_run.output
	assert valid_run.output.trim_space() == '42'

	cross_c := os.join_path(root, 'cross_linux.c')
	cross_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-os', 'linux', '-o', cross_c,
		valid_source])
	assert cross_compile.exit_code == 0, cross_compile.output
	cross_source := os.read_file(cross_c) or { panic(err) }
	assert cross_source.contains('V_FASTC_PRINT_SELECT')
	assert !cross_source.contains('builtin__builtin_init')

	import_source := os.join_path(root, 'import.v')
	write_fastc_test_source(import_source, 'module main

import os

fn main() {
	println(os.args.len)
}
')
	import_binary := os.join_path(root, 'import')
	import_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', import_binary,
		import_source])
	assert import_compile.exit_code != 0
	assert import_compile.output.contains('fastc parser does not support top-level `import`'), import_compile.output

	assert !os.exists(import_binary)
	assert !os.exists(import_binary + '.c')

	typed_source := os.join_path(root, 'typed.v')
	write_fastc_test_source(typed_source, 'module main

fn main() {
	x := 2147483649 | 0
	println(x)
}
')
	typed_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', os.join_path(root, 'typed'),
		typed_source])
	assert typed_compile.exit_code != 0
	assert typed_compile.output.contains('fastc parser does not support oversized decimal literal expressions'), typed_compile.output

	immutable_source := os.join_path(root, 'immutable.v')
	write_fastc_test_source(immutable_source, 'module main

fn main() {
	value := 1
	value = 2
}
')
	immutable_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o',
		os.join_path(root, 'immutable'), immutable_source])
	assert immutable_compile.exit_code != 0
	assert immutable_compile.output.contains('mutation of immutable or unknown name `value`'), immutable_compile.output

	invalid_c_source := os.join_path(root, 'invalid_c.v')
	write_fastc_test_source(invalid_c_source, 'module main

fn main() {
	value := missing_name
	println(value)
}
')
	invalid_binary := os.join_path(root, 'invalid_c')
	invalid_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', invalid_binary,
		invalid_c_source])
	assert invalid_compile.exit_code != 0
	assert invalid_compile.output.contains('missing_name'), invalid_compile.output
	assert !os.exists(invalid_binary)
	assert !os.exists(invalid_binary + '.c')

	preamble_name_source := os.join_path(root, 'preamble_name.v')
	write_fastc_test_source(preamble_name_source, "module main

fn main() {
	puts('hello')
}
")
	preamble_name_binary := os.join_path(root, 'preamble_name')
	preamble_name_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o',
		preamble_name_binary, preamble_name_source])
	assert preamble_name_compile.exit_code != 0
	assert preamble_name_compile.output.contains('fastc parser does not support unresolved name `puts`'), preamble_name_compile.output

	assert !os.exists(preamble_name_binary)
	assert !os.exists(preamble_name_binary + '.c')

	for invocation in [
		UnsupportedFastCInvocation{
			args:     ['-silent', '-prod', '-b', 'fastc', '-o', os.join_path(root, 'prod'),
				valid_source]
			expected: 'fastc parser does not support `-prod`'
		},
		UnsupportedFastCInvocation{
			args:     ['-silent', '-selfhost', '-b', 'fastc', '-o', os.join_path(root, 'selfhost'),
				fastc_backend_v3_source]
			expected: 'fastc parser does not support compiler self-hosting'
		},
		UnsupportedFastCInvocation{
			args:     ['-silent', '-b', 'fastc', '-d', 'no_main', '-o', os.join_path(root,
				'no_main.c'),
				valid_source]
			expected: 'fastc parser does not support `-d no_main`'
		},
		UnsupportedFastCInvocation{
			args:     ['-silent', '-autofree', '-b', 'fastc', '-o', os.join_path(root, 'autofree'),
				valid_source]
			expected: 'fastc parser does not support ownership/autofree'
		},
	] {
		result := cmdexec.run(v3_bin, invocation.args)
		assert result.exit_code != 0
		assert result.output.contains(invocation.expected), result.output
	}
}
