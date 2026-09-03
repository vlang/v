import os
import v3.cmdexec

const fastc_backend_v3_dir = os.dir(os.dir(@FILE))
const fastc_backend_vlib_dir = os.dir(fastc_backend_v3_dir)
const fastc_backend_vroot = os.dir(fastc_backend_vlib_dir)
const fastc_backend_v3_source = os.join_path(fastc_backend_v3_dir, 'v3.v')

struct UnsupportedFastCInvocation {
	args     []string
	expected string
}

fn write_fastc_test_source(path string, source string) {
	os.write_file(path, source) or { panic(err) }
}

fn run_with_v_environment(program string, args []string, flags string, vexe string) os.Result {
	old_vflags := os.getenv_opt('VFLAGS')
	old_vosargs := os.getenv_opt('VOSARGS')
	old_vexe := os.getenv_opt('VEXE')
	defer {
		if value := old_vflags {
			os.setenv('VFLAGS', value, true)
		} else {
			os.unsetenv('VFLAGS')
		}
		if value := old_vosargs {
			os.setenv('VOSARGS', value, true)
		} else {
			os.unsetenv('VOSARGS')
		}
		if value := old_vexe {
			os.setenv('VEXE', value, true)
		} else {
			os.unsetenv('VEXE')
		}
	}
	if flags == '' {
		os.unsetenv('VFLAGS')
	} else {
		os.setenv('VFLAGS', flags, true)
	}
	os.unsetenv('VOSARGS')
	os.setenv('VEXE', vexe, true)
	return cmdexec.run(program, args)
}

fn test_direct_fastc_compiler_entry_selects_selfhost_driver() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v_direct_fastc_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	host_binary := os.join_path(root, 'v_host')
	os.cp(@VEXE, host_binary) or { panic(err) }
	linked_entry_dir := os.join_path(root, 'linked_entry')
	os.mkdir_all(linked_entry_dir) or { panic(err) }
	linked_entry := os.join_path(linked_entry_dir, 'v3.v')
	os.symlink(fastc_backend_v3_source, linked_entry) or { panic(err) }
	direct_binary := os.join_path(root, 'v_fastc_direct')
	direct_build := cmdexec.run_in(host_binary, ['-silent', '-b', 'fastc', '-o', direct_binary,
		linked_entry], root)
	assert direct_build.exit_code == 0, direct_build.output
	assert os.is_executable(direct_binary)

	self_output := os.join_path(root, 'v_fastc_child')
	self_build := cmdexec.run(direct_binary, ['self', '-silent', '-o', self_output])
	assert self_build.exit_code == 0, self_build.output
	assert self_build.output.contains('V self compiling (-b fastc)...'), self_build.output
	assert os.is_executable(self_output)

	ordinary_source := os.join_path(root, 'v3.v')
	write_fastc_test_source(ordinary_source,
		'module main\nfn main() {\n\t\$if fastc_selfhost ? {\n\t\texit(7)\n\t}\n}\n')
	ordinary_binary := os.join_path(root, 'ordinary_v3')
	ordinary_build := cmdexec.run(@VEXE, ['-silent', '-b', 'fastc', '-o', ordinary_binary,
		ordinary_source])
	assert ordinary_build.exit_code == 0, ordinary_build.output
	ordinary_run := cmdexec.run(ordinary_binary, [])
	assert ordinary_run.exit_code == 0, ordinary_run.output
}

fn test_v_self_accepts_fastc_backend() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v_self_fastc_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	vflags_value_binary := os.join_path(root, 'v from x2 value')
	vflags_value_build := run_with_v_environment(@VEXE, ['-exclude', 'x2', 'self', '-silent', '-o',
		vflags_value_binary], '-exclude x3', @VEXE)
	assert vflags_value_build.exit_code == 0, vflags_value_build.output
	assert vflags_value_build.output.count('V self compiling') == 1, vflags_value_build.output
	assert vflags_value_build.output.contains('-exclude x2'), vflags_value_build.output
	assert vflags_value_build.output.contains('-exclude x3'), vflags_value_build.output
	assert os.is_executable(vflags_value_binary)

	vflags_binary := os.join_path(root, 'v fastc from vflags')
	vflags_self_build := run_with_v_environment(@VEXE, ['self', '-silent', '-o', vflags_binary],
		'-d self -b fastc', @VEXE)
	assert vflags_self_build.exit_code == 0, vflags_self_build.output
	assert vflags_self_build.output.count('V self compiling') == 1, vflags_self_build.output
	assert vflags_self_build.output.contains('-d self'), vflags_self_build.output
	assert vflags_self_build.output.contains('-b fastc'), vflags_self_build.output
	assert os.is_executable(vflags_binary)

	vflags_backend_value_binary := os.join_path(root, 'v fastc backend value')
	vflags_backend_value_build := run_with_v_environment(@VEXE, ['self', '-b', 'fastc', '-o',
		vflags_backend_value_binary], '-exclude -b', @VEXE)
	assert vflags_backend_value_build.exit_code != 0
	assert vflags_backend_value_build.output.count('V self compiling') == 1, vflags_backend_value_build.output
	assert vflags_backend_value_build.output.contains('-exclude -b'), vflags_backend_value_build.output
	assert !vflags_backend_value_build.output.contains('-exclude fastc'), vflags_backend_value_build.output
	assert vflags_backend_value_build.output.contains('only supported by the V1 compiler'), vflags_backend_value_build.output
	assert !os.exists(vflags_backend_value_binary)

	selfhost_binary := os.join_path(root, 'v fastc')
	self_build := cmdexec.run(@VEXE, ['self', '-silent', '-backend', 'fastc', 'x2', '-o',
		selfhost_binary])
	assert self_build.exit_code == 0, self_build.output
	assert self_build.output.count('V self compiling') == 2, self_build.output
	assert self_build.output.contains('-b fastc'), self_build.output
	assert !self_build.output.contains('-backend fastc'), self_build.output
	assert os.is_executable(selfhost_binary)

	isolated_vroot := os.join_path(root, 'isolated_vroot')
	os.mkdir_all(isolated_vroot) or { panic(err) }
	for directory in ['cmd', 'vlib', 'thirdparty'] {
		os.symlink(os.join_path(fastc_backend_vroot, directory), os.join_path(isolated_vroot,
			directory)) or { panic(err) }
	}
	isolated_vexe := os.join_path(isolated_vroot, 'v')
	os.cp(@VEXE, isolated_vexe) or { panic(err) }
	output_alias_binary := os.join_path(root, 'v fastc output alias')
	output_alias_build := run_with_v_environment(isolated_vexe, ['self', '-silent', '-b', 'fastc',
		'-output', output_alias_binary], '', isolated_vexe)
	assert output_alias_build.exit_code == 0, output_alias_build.output
	assert output_alias_build.output.count('V self compiling') == 1, output_alias_build.output
	assert os.is_executable(output_alias_binary)
	assert !os.exists(os.join_path(isolated_vroot, 'v_old'))
	for ccompiler in ['tcc', 'tinyc'] {
		unsupported_cc_repeat := run_with_v_environment(isolated_vexe, ['self', '-silent', '-b',
			'fastc', '-cc', ccompiler, 'x2'], '', isolated_vexe)
		assert unsupported_cc_repeat.exit_code != 0
		assert unsupported_cc_repeat.output.contains('cannot preserve'), unsupported_cc_repeat.output
		assert unsupported_cc_repeat.output.contains('-cc ${ccompiler}'), unsupported_cc_repeat.output
		assert unsupported_cc_repeat.output.count('V self compiling') == 0, unsupported_cc_repeat.output
		assert !os.exists(os.join_path(isolated_vroot, 'v_old'))
	}
	unsupported_repeat := run_with_v_environment(isolated_vexe, ['self', '-silent', '-b', 'fastc',
		'-g', 'x2'], '', isolated_vexe)
	assert unsupported_repeat.exit_code != 0
	assert unsupported_repeat.output.contains('cannot preserve'), unsupported_repeat.output
	assert unsupported_repeat.output.contains('-g'), unsupported_repeat.output
	assert unsupported_repeat.output.count('V self compiling') == 0, unsupported_repeat.output
	assert !os.exists(os.join_path(isolated_vroot, 'v_old'))

	repeated_build := run_with_v_environment(isolated_vexe, ['self', '-silent', '-b', 'fastc',
		'x2'], '', isolated_vexe)
	assert repeated_build.exit_code == 0, repeated_build.output
	compiling_lines :=
		repeated_build.output.split_into_lines().filter(it.contains('V self compiling'))
	assert compiling_lines.len == 2, repeated_build.output
	assert os.is_executable(isolated_vexe)

	deep_self_build := run_with_v_environment(isolated_vexe, ['self', '-silent', 'x5'], '',
		isolated_vexe)
	assert deep_self_build.exit_code == 0, deep_self_build.output
	assert deep_self_build.output.count('V self compiling') == 5, deep_self_build.output
	assert os.is_executable(isolated_vexe)
	v_old := os.join_path(isolated_vroot, 'v_old')
	assert os.is_executable(v_old)
	v_old_repeated_build := run_with_v_environment(v_old, ['self', '-silent', 'x2'], '', v_old)
	assert v_old_repeated_build.exit_code == 0, v_old_repeated_build.output
	assert v_old_repeated_build.output.count('V self compiling') == 2, v_old_repeated_build.output
	assert os.is_executable(v_old)
	deep_self_output := os.join_path(isolated_vroot, 'v2')
	deep_self_output_build := run_with_v_environment(isolated_vexe, ['self', '-silent', '-o',
		deep_self_output], '', isolated_vexe)
	assert deep_self_output_build.exit_code == 0, deep_self_output_build.output
	assert deep_self_output_build.output.count('V self compiling') == 1, deep_self_output_build.output
	assert os.is_executable(deep_self_output)
	v2_repeated_build := run_with_v_environment(deep_self_output, ['self', '-silent', 'x2'], '',
		deep_self_output)
	assert v2_repeated_build.exit_code == 0, v2_repeated_build.output
	assert v2_repeated_build.output.count('V self compiling') == 2, v2_repeated_build.output
	assert os.is_executable(deep_self_output)

	prod_build := cmdexec.run(@VEXE, ['self', '-silent', '-prod', '-b', 'fastc', '-o',
		os.join_path(root, 'v prod')])
	assert prod_build.exit_code != 0
	assert prod_build.output.contains('does not support `-prod`'), prod_build.output

	source := os.join_path(root, 'main.v')
	write_fastc_test_source(source, 'module main\nfn main() { println(42) }\n')
	program := os.join_path(root, 'program')
	compile := cmdexec.run(selfhost_binary, ['-b', 'fastc', '-o', program, source])
	assert compile.exit_code == 0, compile.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42'
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

	collision_source := os.join_path(root, 'collision.v')
	collision_contents := 'module main\nfn main() { println(42) }\n'
	write_fastc_test_source(collision_source, collision_contents)
	collision_dir := os.join_path(root, 'collision_path')
	os.mkdir_all(collision_dir) or { panic(err) }
	collision_output := os.join_path(collision_dir, '..', 'collision.v')
	collision_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', collision_output,
		collision_source])
	assert collision_compile.exit_code != 0
	assert collision_compile.output.contains('fastc output path'), collision_compile.output
	assert collision_compile.output.contains('aliases input source'), collision_compile.output
	assert os.read_file(collision_source) or { panic(err) } == collision_contents

	valid_source := os.join_path(root, 'valid.v')
	write_fastc_test_source(valid_source, "module main

fn twice(value int) int {
	return value * 2
}

fn main() {
	value := twice(21)
	println(value)
	println(0o17)
	for ch in 'ab' {
		println(ch)
	}
}
")
	valid_binary := os.join_path(root, 'valid')
	valid_compile := cmdexec.run(v3_bin, ['-macos-v3-compat-c99', '-b', 'fastc', '-o', valid_binary,
		valid_source])
	assert valid_compile.exit_code == 0, valid_compile.output
	assert valid_compile.output.contains('fastc parse+gen'), valid_compile.output
	assert !valid_compile.output.contains(' check'), valid_compile.output
	assert !valid_compile.output.contains(' transform'), valid_compile.output
	assert !valid_compile.output.contains('markused'), valid_compile.output
	retained_c := os.read_file(valid_binary + '.c') or { panic(err) }
	// A `:=` local whose inferred type is platform `int` is spelled with the explicit
	// (i64) type rather than `__typeof__`, so `int` locals match the width used for
	// params/fields and never truncate through C's own `int` inference.
	assert retained_c.contains('i64 value = (twice(21));')
	assert retained_c.contains('println(017);')
	assert retained_c.contains('strlen(__v_fastc_collection_')
	assert retained_c.contains('((const unsigned char *)__v_fastc_collection_')
	assert retained_c.contains('setvbuf(stdout, NULL, _IONBF, 0);')
	assert !retained_c.contains('builtin__builtin_init')
	valid_run := cmdexec.run(valid_binary, [])
	assert valid_run.exit_code == 0, valid_run.output
	assert valid_run.output.trim_space() == '42\n15\n97\n98'

	tinyc_source := os.join_path(root, 'tinyc_comptime.v')
	write_fastc_test_source(tinyc_source, "module main

fn main() {
	\$if tinyc {
		println('tinyc')
	} \$else {
		println('other')
	}
}
")
	tinyc_binary := os.join_path(root, 'tinyc_comptime')
	tinyc_compile := cmdexec.run(v3_bin,
		['-silent', '-b', 'fastc', '-o', tinyc_binary, tinyc_source])
	assert tinyc_compile.exit_code == 0, tinyc_compile.output
	tinyc_run := cmdexec.run(tinyc_binary, [])
	assert tinyc_run.exit_code == 0, tinyc_run.output
	assert tinyc_run.output.trim_space() == 'tinyc'

	cross_target_os := $if windows { 'linux' } $else { 'windows' }
	cross_c := os.join_path(root, 'cross_target.c')
	cross_compile := cmdexec.run(v3_bin, ['-silent', '-showcc', '-b', 'fastc', '-os', cross_target_os,
		'-o', cross_c, valid_source])
	assert cross_compile.exit_code == 0, cross_compile.output
	assert !cross_compile.output.contains('tcc.exe'), cross_compile.output
	cross_source := os.read_file(cross_c) or { panic(err) }
	assert cross_source.contains('V_FASTC_PRINT_SELECT')
	assert cross_source.contains('strlen(__v_fastc_collection_')
	assert cross_source.contains('((const unsigned char *)__v_fastc_collection_')
	assert !cross_source.contains('builtin__builtin_init')
	unsupported_float_source := os.join_path(root, 'unsupported_float_print.v')
	write_fastc_test_source(unsupported_float_source, 'module main

fn main() {
	println(1.5)
}
')
	unsupported_cross_c := os.join_path(root, 'unsupported_float_print.c')
	unsupported_cross_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-os',
		cross_target_os, '-o', unsupported_cross_c, unsupported_float_source])
	assert unsupported_cross_compile.exit_code != 0
	assert unsupported_cross_compile.output.contains('printing value of type `float literal`'), unsupported_cross_compile.output

	assert !os.exists(unsupported_cross_c)

	run_c := os.join_path(root, 'run_output.c')
	run_c_result := cmdexec.run(v3_bin,
		['-silent', '-b', 'fastc', '-o', run_c, 'run', valid_source])
	assert run_c_result.exit_code == 0, run_c_result.output
	assert os.is_file(run_c)
	assert !os.exists(run_c.all_before_last('.c'))
	run_c_source := os.read_file(run_c) or { panic(err) }
	assert run_c_source.contains('V_FASTC_PRINT_SELECT')

	run_stdout_result := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', '-', 'run',
		valid_source])
	assert run_stdout_result.exit_code == 0, run_stdout_result.output
	assert run_stdout_result.output.contains('V_FASTC_PRINT_SELECT')
	assert !run_stdout_result.output.ends_with('42\n15\n')

	stdout_c := os.join_path(root, 'stdout.c')
	stdout_diagnostics := os.join_path(root, 'stdout.stderr')
	stdout_result :=
		os.execute('${os.quoted_path(v3_bin)} -b fastc -o - ${os.quoted_path(valid_source)} > ${os.quoted_path(stdout_c)} 2> ${os.quoted_path(stdout_diagnostics)}')
	assert stdout_result.exit_code == 0, stdout_result.output
	stdout_source := os.read_file(stdout_c) or { panic(err) }
	assert stdout_source.contains('V_FASTC_PRINT_SELECT')
	assert !stdout_source.contains('=== v3 benchmark ===')
	assert !stdout_source.contains('fastc parse+gen')
	tcc_dir := os.join_path(os.dir(fastc_backend_vlib_dir), 'thirdparty', 'tcc')
	tcc_lib_dir := os.join_path_single(tcc_dir, 'lib')
	tcc_nested_dir := os.join_path_single(tcc_lib_dir, 'tcc')
	tcc_install_dir := if os.is_dir(tcc_nested_dir) { tcc_nested_dir } else { tcc_lib_dir }
	stdout_binary := os.join_path(root, 'stdout')
	stdout_compile := cmdexec.run(os.join_path(tcc_dir, 'tcc.exe'), ['-std=gnu11',
		'-B${tcc_install_dir}', '-I${os.join_path_single(tcc_install_dir, 'include')}',
		'-L${tcc_install_dir}', '-w', '-o', stdout_binary, stdout_c, '-lm'])
	assert stdout_compile.exit_code == 0, stdout_compile.output

	cross_stdout_c := os.join_path(root, 'cross_stdout.c')
	cross_stdout_diagnostics := os.join_path(root, 'cross_stdout.stderr')
	cross_stdout_result :=
		os.execute('${os.quoted_path(v3_bin)} -b fastc -os ${cross_target_os} -o - ${os.quoted_path(valid_source)} > ${os.quoted_path(cross_stdout_c)} 2> ${os.quoted_path(cross_stdout_diagnostics)}')
	assert cross_stdout_result.exit_code == 0, cross_stdout_result.output
	cross_stdout_source := os.read_file(cross_stdout_c) or { panic(err) }
	assert cross_stdout_source.contains('V_FASTC_PRINT_SELECT')
	assert !cross_stdout_source.contains('=== v3 benchmark ===')
	assert !cross_stdout_source.contains('fastc parse+gen')

	module_dir := os.join_path(root, 'mathutil')
	os.mkdir_all(module_dir) or { panic(err) }
	module_source := os.join_path(module_dir, 'mathutil.v')
	module_contents := 'module mathutil

pub fn twice(value int) int {
	return value * 2
}
'
	write_fastc_test_source(module_source, module_contents)
	import_source := os.join_path(root, 'import.v')
	write_fastc_test_source(import_source, 'module main

import mathutil

fn main() {
	println(mathutil.twice(21))
}
')
	import_alias_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', module_source,
		import_source])
	assert import_alias_compile.exit_code != 0
	assert import_alias_compile.output.contains('aliases imported source'), import_alias_compile.output
	assert os.read_file(module_source) or { panic(err) } == module_contents
	import_binary := os.join_path(root, 'import')
	import_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', import_binary,
		import_source])
	assert import_compile.exit_code == 0, import_compile.output
	import_run := cmdexec.run(import_binary, [])
	assert import_run.exit_code == 0, import_run.output
	assert import_run.output.trim_space() == '42'

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

	global_source := os.join_path(root, 'initialized_global.v')
	write_fastc_test_source(global_source, 'module main

__global answer = 42
__global initialized = initialize()

fn initialize() int {
	println("initializing")
	return answer + 1
}

fn main() {
	println(answer)
	println(initialized)
}
')
	global_binary := os.join_path(root, 'initialized_global')
	global_without_flag := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', global_binary,
		global_source])
	assert global_without_flag.exit_code != 0
	assert global_without_flag.output.contains('use `v -enable-globals ...` to enable globals'), global_without_flag.output
	global_compile := cmdexec.run(v3_bin, ['-silent', '-enable-globals', '-b', 'fastc', '-o',
		global_binary, global_source])
	assert global_compile.exit_code == 0, global_compile.output
	global_run := cmdexec.run(global_binary, [])
	assert global_run.exit_code == 0, global_run.output
	assert global_run.output.trim_space() == 'initializing\n42\n43'

	script_global_source := os.join_path(root, 'initialized_script_global.v')
	write_fastc_test_source(script_global_source, 'module main

__global answer = 42

println(answer)
')
	script_global_binary := os.join_path(root, 'initialized_script_global')
	script_global_compile := cmdexec.run(v3_bin, ['-silent', '-enable-globals', '-b', 'fastc',
		'-o', script_global_binary, script_global_source])
	assert script_global_compile.exit_code == 0, script_global_compile.output
	script_global_run := cmdexec.run(script_global_binary, [])
	assert script_global_run.exit_code == 0, script_global_run.output
	assert script_global_run.output.trim_space() == '42'

	runtime_constant_source := os.join_path(root, 'runtime_constant.v')
	write_fastc_test_source(runtime_constant_source, 'module main

__global calls int

const value = next()
const unused = next()

fn next() int {
	calls++
	return calls
}

fn main() {
	println(value)
	println(value)
	println(calls)
}
')
	runtime_constant_binary := os.join_path(root, 'runtime_constant')
	runtime_constant_compile := cmdexec.run(v3_bin, ['-silent', '-enable-globals', '-b', 'fastc',
		'-o', runtime_constant_binary, runtime_constant_source])
	assert runtime_constant_compile.exit_code == 0, runtime_constant_compile.output
	runtime_constant_run := cmdexec.run(runtime_constant_binary, [])
	assert runtime_constant_run.exit_code == 0, runtime_constant_run.output
	assert runtime_constant_run.output.trim_space() == '1\n1\n2'

	nested_mutation_source := os.join_path(root, 'nested_mutation.v')
	write_fastc_test_source(nested_mutation_source, 'module main

fn main() {
	x := 1
	println(x++)
}
')
	nested_mutation_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o',
		os.join_path(root, 'nested_mutation'), nested_mutation_source])
	assert nested_mutation_compile.exit_code != 0
	assert nested_mutation_compile.output.contains('mutation `++` inside an expression'), nested_mutation_compile.output

	select_source := os.join_path(root, 'select.v')
	write_fastc_test_source(select_source, 'module main

fn main() {
	select {
		value := <-messages { println(value) }
		else { println(0) }
	}
}
')
	select_binary := os.join_path(root, 'select')
	select_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', select_binary,
		select_source])
	assert select_compile.exit_code != 0
	assert select_compile.output.contains('fastc parser does not support select statements'), select_compile.output

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

	fallthrough_source := os.join_path(root, 'fallthrough.v')
	write_fastc_test_source(fallthrough_source, 'module main

fn value() int {}

fn main() {
	println(value())
}
')
	fallthrough_binary := os.join_path(root, 'fallthrough')
	fallthrough_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', fallthrough_binary,
		fallthrough_source])
	assert fallthrough_compile.exit_code != 0
	assert fallthrough_compile.output.contains('non-void function `value` that can fall through'), fallthrough_compile.output

	assert !os.exists(fallthrough_binary)
	assert !os.exists(fallthrough_binary + '.c')

	scoped_source := os.join_path(root, 'scoped_and_reserved.v')
	write_fastc_test_source(scoped_source, 'module main

struct Holder {
	auto int
}

fn calculate(holder Holder, register int) int {
	restrict := register
	return holder.auto + restrict
}

fn auto() int {
	return 42
}

fn deferred_return() int {
	defer { println(4) }
	if true {
		defer { println(5) }
		return 6
	}
	return 0
}

fn deferred_value() int {
	mut value := 1
	defer { value = 2 }
	return value
}

fn change(mut value int) {
	value = 2
}

fn main() {
	if true {
		defer { println(1) }
		println(2)
	}
	if false {
		defer { println(99) }
	}
	for i in 0 .. 2 {
		defer { println(i) }
		continue
	}
	for {
		defer { println(7) }
		break
	}
	value := deferred_return()
	println(value)
	println(deferred_value())
	mut changed := 1
	change(mut changed)
	println(changed)
	restrict := auto()
	auto := restrict
	println(auto)
	println(3)
}
')
	scoped_binary := os.join_path(root, 'scoped_and_reserved')
	scoped_compile := cmdexec.run(v3_bin, ['-silent', '-b', 'fastc', '-o', scoped_binary,
		scoped_source])
	assert scoped_compile.exit_code == 0, scoped_compile.output
	scoped_run := cmdexec.run(scoped_binary, [])
	assert scoped_run.exit_code == 0, scoped_run.output
	assert scoped_run.output.trim_space() == '2\n1\n0\n1\n7\n5\n4\n6\n1\n2\n42\n3'

	selfhost_binary := os.join_path(root, 'selfhost')
	selfhost_compile := cmdexec.run(v3_bin, ['-silent', '-selfhost', '-b', 'fastc', '-o',
		selfhost_binary, fastc_backend_v3_source])
	assert selfhost_compile.exit_code == 0, selfhost_compile.output
	selfhost_collision := cmdexec.run(selfhost_binary, ['-b', 'fastc', '-o', collision_output,
		collision_source])
	assert selfhost_collision.exit_code != 0
	assert selfhost_collision.output.contains('fastc output path'), selfhost_collision.output
	assert selfhost_collision.output.contains('aliases input source'), selfhost_collision.output
	assert os.read_file(collision_source) or { panic(err) } == collision_contents
	selfhost_output := os.join_path(root, 'selfhost_output')
	selfhost_program_compile := cmdexec.run(selfhost_binary, ['-b', 'fastc', '-o', selfhost_output,
		valid_source])
	assert selfhost_program_compile.exit_code == 0, selfhost_program_compile.output
	selfhost_program_run := cmdexec.run(selfhost_output, [])
	assert selfhost_program_run.exit_code == 0, selfhost_program_run.output
	assert selfhost_program_run.output.trim_space() == '42\n15\n97\n98'
	selfhost_import_alias := cmdexec.run(selfhost_binary, ['-b', 'fastc', '-o', module_source,
		import_source])
	assert selfhost_import_alias.exit_code != 0
	assert selfhost_import_alias.output.contains('aliases imported source'), selfhost_import_alias.output
	assert os.read_file(module_source) or { panic(err) } == module_contents

	selfhost_fixed_options_output := os.join_path(root, 'selfhost_fixed_options')
	selfhost_fixed_options := cmdexec.run(selfhost_binary, ['-gc', 'none', '-cc', 'tinyc', '-b',
		'fastc', '-o', selfhost_fixed_options_output, valid_source])
	assert selfhost_fixed_options.exit_code == 0, selfhost_fixed_options.output
	discovered_test_source := os.join_path(root, 'discovered_test.v')
	write_fastc_test_source(discovered_test_source, 'fn test_must_run() {
	assert false
}
')
	discovered_c_test_source := os.join_path(root, 'discovered_test.c.v')
	write_fastc_test_source(discovered_c_test_source, 'fn test_must_run() {
	assert false
}
')
	impure_source := os.join_path(root, 'impure.v')
	write_fastc_test_source(impure_source, 'module main

fn C.exit(int)

fn main() {
	C.exit(0)
}
')
	no_main_output := os.join_path(root, 'no_main.c')
	for invocation in [
		UnsupportedFastCInvocation{
			args:     ['-b', 'fastc', '-o', os.join_path(root, 'multiple_sources'), valid_source,
				scoped_source]
			expected: 'accepts only one V source entry file'
		},
		UnsupportedFastCInvocation{
			args:     ['-gc', 'boehm', '-b', 'fastc', valid_source]
			expected: 'only supports `-gc none`'
		},
		UnsupportedFastCInvocation{
			args:     ['-cc', 'clang', '-b', 'fastc', valid_source]
			expected: 'only supports bundled TinyCC'
		},
		UnsupportedFastCInvocation{
			args:     ['-d', 'no_main', '-b', 'fastc', valid_source]
			expected: 'does not support custom `-d no_main` defines'
		},
		UnsupportedFastCInvocation{
			args:     ['-b', 'fastc', discovered_test_source]
			expected: 'does not support test files'
		},
		UnsupportedFastCInvocation{
			args:     ['-b', 'fastc', discovered_c_test_source]
			expected: 'does not support test files'
		},
	] {
		result := cmdexec.run(selfhost_binary, invocation.args)
		assert result.exit_code != 0
		assert result.output.contains(invocation.expected), result.output
	}

	for invocation in [
		UnsupportedFastCInvocation{
			args:     ['-silent', '-prod', '-b', 'fastc', '-o', os.join_path(root, 'prod'),
				valid_source]
			expected: 'fastc parser does not support `-prod`'
		},
		UnsupportedFastCInvocation{
			args:     ['-silent', '-b', 'fastc', '-d', 'no_main', '-o', no_main_output, valid_source]
			expected: 'fastc parser does not support `-d no_main`'
		},
		UnsupportedFastCInvocation{
			args:     ['-silent', '-autofree', '-b', 'fastc', '-o', os.join_path(root, 'autofree'),
				valid_source]
			expected: 'fastc parser does not support ownership/autofree'
		},
		UnsupportedFastCInvocation{
			args:     ['-silent', '-Wimpure-v', '-b', 'fastc', '-o', os.join_path(root, 'impure'),
				impure_source]
			expected: 'fastc parser does not support `-Wimpure-v`'
		},
		UnsupportedFastCInvocation{
			args:     ['-silent', '-skip-running', '-b', 'fastc', '-o',
				os.join_path(root, 'discovered_test'), discovered_test_source]
			expected: 'fastc parser does not support test/checker mode'
		},
		UnsupportedFastCInvocation{
			args:     ['-silent', '-skip-running', '-b', 'fastc', '-o',
				os.join_path(root, 'discovered_c_test'), discovered_c_test_source]
			expected: 'fastc parser does not support test/checker mode'
		},
	] {
		result := cmdexec.run(v3_bin, invocation.args)
		assert result.exit_code != 0
		assert result.output.contains(invocation.expected), result.output
	}
}
