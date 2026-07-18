import os
import v3.cmdexec
import v3.pref

const driver_cli_vlib_dir = os.dir(os.dir(os.dir(@FILE)))
const driver_cli_v3_dir = os.dir(os.dir(@FILE))
const driver_cli_v3_src = os.join_path(driver_cli_v3_dir, 'v3.v')

fn build_driver_cli_v3(root string) string {
	bin := os.join_path(root, 'v3_driver_cli')
	result := cmdexec.run(@VEXE, ['-gc', 'none', '-path', '${driver_cli_vlib_dir}|@vlib|@vmodules',
		'-o', bin, driver_cli_v3_src])
	assert result.exit_code == 0, result.output
	return bin
}

fn assert_driver_cli_failure(v3_bin string, args []string, message string) {
	result := cmdexec.run(v3_bin, args)
	assert result.exit_code != 0
	assert result.output.contains(message), result.output
}

fn run_driver_with_stdin_file(v3_bin string, args []string, stdin_path string) os.Result {
	mut process := os.new_process(v3_bin)
	process.set_args(args)
	process.set_stdin_path(stdin_path)
	process.set_redirect_stdio()
	process.run()
	process.wait()
	mut output := process.stdout_slurp()
	output += process.stderr_slurp()
	if process.err.len > 0 {
		output += process.err
	}
	exit_code := if process.code >= 0 { process.code } else { 1 }
	process.close()
	return os.Result{
		exit_code: exit_code
		output:    output
	}
}

fn test_driver_run_preserves_stdin() {
	root := os.join_path(os.vtmp_dir(), 'v3_driver_stdin_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := build_driver_cli_v3(root)
	source := os.join_path(root, 'stdin_program.v')
	output := os.join_path(root, 'stdin_program')
	input_file := os.join_path(root, 'input.txt')
	os.write_file(source, "import os\n\nfn main() { println('read:' + os.input('')) }\n")!
	os.write_file(input_file, 'from-stdin\n')!
	result := run_driver_with_stdin_file(v3_bin, ['-o', output, 'run', source], input_file)
	assert result.exit_code == 0, result.output
	assert result.output.contains('read:from-stdin'), result.output
}

fn assert_driver_wasm_output(path string) {
	bytes := os.read_bytes(path) or { panic(err) }
	assert bytes.len > 8
	assert bytes[..4] == [u8(0), 0x61, 0x73, 0x6d]
}

fn test_wasm_backend_defaults_target_unless_explicit() {
	root := os.join_path(os.vtmp_dir(), 'v3_driver_wasm_target_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := build_driver_cli_v3(root)

	default_dir := os.join_path(root, 'default_target')
	os.mkdir_all(default_dir) or { panic(err) }
	os.write_file(os.join_path(default_dir, 'main.v'), 'module main

\$if wasm32 {
fn wasm_arch_selected() {}
}

fn main() {
	wasm_os_selected()
	wasm_arch_selected()
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(default_dir, 'target_wasm32_emscripten.v'),
		'module main\n\nfn wasm_os_selected() {}\n') or { panic(err) }
	default_output := os.join_path(root, 'default_target.wasm')
	default_compile := cmdexec.run(v3_bin, ['-b', 'wasm', '-o', default_output, default_dir])
	assert default_compile.exit_code == 0, default_compile.output
	assert_driver_wasm_output(default_output)

	host := pref.host_target()
	explicit_dir := os.join_path(root, 'explicit_target')
	os.mkdir_all(explicit_dir) or { panic(err) }
	os.write_file(os.join_path(explicit_dir, 'main.v'),
		'module main\n\nfn main() { host_os_selected() }\n') or { panic(err) }
	os.write_file(os.join_path(explicit_dir, 'target_${host.os}.v'),
		'module main\n\nfn host_os_selected() {}\n') or { panic(err) }
	explicit_output := os.join_path(root, 'explicit_target.wasm')
	explicit_compile := cmdexec.run(v3_bin, ['-b', 'wasm', '-os', host.os, '-arch', host.arch,
		'-o', explicit_output, explicit_dir])
	assert explicit_compile.exit_code == 0, explicit_compile.output
	assert_driver_wasm_output(explicit_output)
}

fn test_driver_rejects_invalid_cli_and_parses_vmod_subdirs() {
	root := os.join_path(os.vtmp_dir(), 'v3_driver_cli_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := build_driver_cli_v3(root)
	source := os.join_path(root, 'hello.v')
	os.write_file(source, "fn main() { println('ok') }\n") or { panic(err) }

	help := cmdexec.run(v3_bin, ['--help'])
	assert help.exit_code == 0
	assert help.output.contains('-cc <compiler>')
	assert help.output.contains('-no-memory-limit')
	c_output := os.join_path(root, 'hello.c')
	c_compile := cmdexec.run(v3_bin, ['-no-memory-limit', '-o', c_output, source])
	assert c_compile.exit_code == 0, c_compile.output
	c_source := os.read_file(c_output)!
	assert c_source.len > 100
	assert c_source.contains('typedef signed char i8;')
	wasm_c_output := os.join_path(root, 'hello_emscripten.c')
	wasm_compile := cmdexec.run(v3_bin, ['-os', 'wasm32_emscripten', '-o', wasm_c_output, source])
	assert wasm_compile.exit_code == 0, wasm_compile.output
	assert os.is_file(wasm_c_output)
	assert_driver_cli_failure(v3_bin, ['-os', 'wasm32_emscripten', '-arch', 'arm64', '-o',
		wasm_c_output, source], 'target OS `wasm32_emscripten` requires architecture `wasm32`')
	bits_source := os.join_path(root, 'bits_fallback.v')
	os.write_file(bits_source, 'import math.bits

fn main() {
	hi, lo := bits.mul_64(u64(0xffffffffffffffff), u64(2))
	println(hi.str() + ":" + lo.str())
}
') or {
		panic(err)
	}
	bits_output := os.join_path(root, 'bits_fallback')
	bits_compile := cmdexec.run(v3_bin, ['-prod', '-o', bits_output, bits_source])
	assert bits_compile.exit_code == 0, bits_compile.output
	bits_run := cmdexec.run(bits_output, [])
	assert bits_run.exit_code == 0, bits_run.output
	assert bits_run.output.trim_space() == '1:18446744073709551614'
	assert_driver_cli_failure(v3_bin, ['--bogus'], 'unknown option `--bogus`')
	assert_driver_cli_failure(v3_bin, ['-o'], 'option `-o` requires a value')
	assert_driver_cli_failure(v3_bin, ['-b', 'bogus', source], 'unknown backend `bogus`')
	assert_driver_cli_failure(v3_bin, ['-gc', 'boehm', source],
		'currently supports only `-gc none`')
	assert_driver_cli_failure(v3_bin, [source, source], 'multiple input paths are not supported')
	assert_driver_cli_failure(v3_bin, ['-compile-backend', 'bogus', source],
		'unknown compile backend `bogus`')

	if false_exe := os.find_abs_path_of_executable('false') {
		cc_result := cmdexec.run(v3_bin, ['-prod', '-cc', false_exe, source, '-o',
			os.join_path(root, 'false_cc')])
		assert cc_result.exit_code != 0
		assert cc_result.output.contains(cmdexec.display(false_exe, ['-std=gnu11'])), cc_result.output
	}

	work_dir := os.join_path(root, 'work')
	project := os.join_path(root, 'project.with.dots')
	os.mkdir_all(os.join_path(project, 'one')) or { panic(err) }
	os.mkdir_all(os.join_path(project, 'two')) or { panic(err) }
	os.mkdir_all(work_dir) or { panic(err) }
	os.write_file(os.join_path(project, 'v.mod'), 'Module {\n' + "  name: 'driver_cli'\n" +
		"  description: 'subdirs: [wrong, value]'\n" + "  subdirs: ['one', 'two']\n" + '}\n') or {
		panic(err)
	}
	os.write_file(os.join_path(project, 'main.v'),
		'module main\n\nfn main() { println(one() + two()) }\n') or { panic(err) }
	os.write_file(os.join_path(project, 'one', 'one.v'),
		'module main\n\nfn one() int { return 40 }\n') or { panic(err) }
	os.write_file(os.join_path(project, 'two', 'two.v'),
		'module main\n\nfn two() int { return 2 }\n') or { panic(err) }

	compile := cmdexec.run_in(v3_bin, [project], work_dir)
	assert compile.exit_code == 0, compile.output
	output := os.join_path(work_dir, 'project.with.dots')
	assert os.exists(output)
	assert !os.exists(os.join_path(work_dir, 'project'))
	run := cmdexec.run(output, [])
	assert run.exit_code == 0
	assert run.output.trim_space() == '42'
}
