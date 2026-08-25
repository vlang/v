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
	write_fastc_test_source(valid_source, 'module main

fn twice(value int) int {
	return value * 2
}

fn main() {
	value := twice(21)
	println(value)
	println(0o17)
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
	assert retained_c.contains('println(017);')
	assert retained_c.contains('setvbuf(stdout, NULL, _IONBF, 0);')
	assert !retained_c.contains('builtin__builtin_init')
	valid_run := cmdexec.run(valid_binary, [])
	assert valid_run.exit_code == 0, valid_run.output
	assert valid_run.output.trim_space() == '42\n15'

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
	assert !cross_source.contains('builtin__builtin_init')

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
	stdout_binary := os.join_path(root, 'stdout')
	stdout_compile := cmdexec.run(os.join_path(tcc_dir, 'tcc.exe'), ['-std=gnu11',
		'-I${os.join_path(tcc_dir, 'lib', 'include')}', '-L${os.join_path(tcc_dir, 'lib')}', '-w',
		'-o', stdout_binary, stdout_c, '-lm'])
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
	write_fastc_test_source(os.join_path(module_dir, 'mathutil.v'), 'module mathutil

pub fn twice(value int) int {
	return value * 2
}
')
	import_source := os.join_path(root, 'import.v')
	write_fastc_test_source(import_source, 'module main

import mathutil

fn main() {
	println(mathutil.twice(21))
}
')
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
	assert selfhost_program_run.output.trim_space() == '42\n15'

	selfhost_fixed_options_output := os.join_path(root, 'selfhost_fixed_options')
	selfhost_fixed_options := cmdexec.run(selfhost_binary, ['-gc', 'none', '-cc', 'tinyc', '-b',
		'fastc', '-o', selfhost_fixed_options_output, valid_source])
	assert selfhost_fixed_options.exit_code == 0, selfhost_fixed_options.output
	discovered_test_source := os.join_path(root, 'discovered_test.v')
	write_fastc_test_source(discovered_test_source, 'fn test_must_run() {
	assert false
}
')
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
			args:     ['-silent', '-b', 'fastc', '-d', 'no_main', '-o',
				os.join_path(root, 'no_main.c'), valid_source]
			expected: 'fastc parser does not support `-d no_main`'
		},
		UnsupportedFastCInvocation{
			args:     ['-silent', '-autofree', '-b', 'fastc', '-o', os.join_path(root, 'autofree'),
				valid_source]
			expected: 'fastc parser does not support ownership/autofree'
		},
		UnsupportedFastCInvocation{
			args:     ['-silent', '-skip-running', '-b', 'fastc', '-o',
				os.join_path(root, 'discovered_test'), discovered_test_source]
			expected: 'fastc parser does not support test/checker mode'
		},
	] {
		result := cmdexec.run(v3_bin, invocation.args)
		assert result.exit_code != 0
		assert result.output.contains(invocation.expected), result.output
	}
}
