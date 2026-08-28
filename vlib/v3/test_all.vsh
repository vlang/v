#!/usr/bin/env -S v

import os
import time

const total_steps = 8
const temp_prefix = 'v3_test_all'
const unit_test_batch_size = 16
const unit_wrapper_mode_env = 'V3_TEST_UNIT_WRAPPER_MODE'
const unit_wrapper_real_vexe_env = 'V3_TEST_UNIT_REAL_VEXE'
const unit_wrapper_shared_v3_env = 'V3_TEST_UNIT_SHARED_V3'
const unit_wrapper_v3_src_env = 'V3_TEST_UNIT_V3_SRC'
const requested_vlib_tests = [
	'vlib/builtin/string_test.v',
	'vlib/math/math_test.v',
	'vlib/builtin/array_test.v',
	'vlib/math/complex/complex_test.v',
	'vlib/builtin/map_test.v',
	'vlib/crypto/hmac/hmac_test.v',
	'vlib/crypto/sha3/sha3_test.v',
	'vlib/time/time_test.v',
	'vlib/os/process_test.v',
	'vlib/os/file_test.v',
	'vlib/arrays/arrays_test.v',
	'vlib/v3/tests/testdata/multiple_generic_struct_fields.v',
]

struct Config {
	vexe         string
	script_dir   string
	repo_root    string
	vlib_dir     string
	tests_dir    string
	v3_src       string
	c99          bool
	c99_flag     string
	host_backend string
	host_os      string
	temp_prefix  string
}

struct ExampleCase {
	path            string
	args            []string
	stdin           string
	compile_flags   []string
	mode            ExampleRunMode
	timeout_seconds int
}

struct ProcessRunResult {
	exit_code int
	output    string
	timed_out bool
}

enum ExampleRunMode {
	normal
	gui_smoke
}

fn main() {
	if os.getenv(unit_wrapper_mode_env) == '1' {
		run_unit_vexe_wrapper()
		return
	}
	self_check_gui_smoke_timeout_status()
	self_check_unit_shared_compiler_request()
	isolated_vtmp := setup_isolated_vtmp()
	defer {
		os.rmdir_all(isolated_vtmp) or {}
	}
	cfg := parse_config()
	os.chdir(cfg.repo_root) or { fail('failed to enter ${cfg.repo_root}: ${err}') }

	v3_bin := temp_path(cfg, 'v3')
	v3_ownership_bin := temp_path(cfg, 'v3_ownership')
	hello_c_bin := temp_path(cfg, 'hello_c')
	hello_arm_bin := temp_path(cfg, 'hello_arm64')
	v4_arm_bin := temp_path(cfg, 'v4_arm64')
	v3_lang_bin := temp_path(cfg, 'v3_lang')
	v4_bin := temp_path(cfg, 'v4_chain')
	v5_bin := temp_path(cfg, 'v5_chain')
	v6_bin := temp_path(cfg, 'v6_chain')
	cleanup_files([
		v3_bin,
		v3_ownership_bin,
		hello_c_bin,
		hello_c_bin + '.c',
		hello_arm_bin,
		v4_arm_bin,
		v3_lang_bin,
		v3_lang_bin + '.c',
		v4_bin,
		v4_bin + '.c',
		v5_bin,
		v5_bin + '.c',
		v6_bin,
		v6_bin + '.c',
	])

	section(1, 'V3 unit tests')
	run_v3_unit_tests(cfg)

	section(2, 'Build v3')
	run('${host_v_cmd(cfg)} -o ${q(v3_bin)} ${q(cfg.v3_src)}')
	// The unlocked example oracle includes an `-autofree` case. Keep its optional
	// ownership checker out of the compiler used by ordinary compatibility cases.
	run('${host_v_cmd(cfg)} -d ownership -o ${q(v3_ownership_bin)} ${q(cfg.v3_src)}')

	section(3, 'Requested vlib tests')
	for rel_path in requested_vlib_tests {
		test_path := os.join_path(cfg.repo_root, rel_path)
		test_bin := temp_path(cfg, rel_path.replace('/', '_').replace('.v', ''))
		run('${q(v3_bin)} ${q(test_path)} -o ${q(test_bin)}')
		run(q(test_bin))
		cleanup_files([test_bin, test_bin + '.c'])
	}

	section(4, 'C backend hello world')
	hello_v := os.join_path(cfg.tests_dir, 'testdata', 'hello.v')
	run('${q(v3_bin)} ${cfg.c99_flag} ${q(hello_v)} -b c -o ${q(hello_c_bin)}')
	run(q(hello_c_bin))
	cleanup_files([hello_c_bin, hello_c_bin + '.c'])

	section(5, 'Unlocked examples C oracle')
	run_unlocked_examples(cfg, v3_bin, v3_ownership_bin)

	section(6, 'ARM64 self-host hello world')
	if cfg.c99 {
		println('  Skipping ARM64 self-host in C99 mode (-c99 applies to the C backend)')
	} else if cfg.host_backend == 'arm64' && cfg.host_os == 'macos' {
		run('${q(v3_bin)} -selfhost -b arm64 -o ${q(v4_arm_bin)} ${q(cfg.v3_src)}')
		run('${q(v4_arm_bin)} -b arm64 -o ${q(hello_arm_bin)} ${q(hello_v)}')
		run(q(hello_arm_bin))
		cleanup_files([v4_arm_bin, hello_arm_bin])
	} else {
		println('  Skipping ARM64 self-host on ${cfg.host_os}/${cfg.host_backend} host (Mach-O only)')
	}

	section(7, 'Self-host chain (v3->v4->v5->v6)')
	println('  Building v4 from v3...')
	run('${q(v3_bin)} ${cfg.c99_flag} -selfhost -o ${q(v4_bin)} ${q(cfg.v3_src)}')
	println('  Building v5 from v4...')
	run('${q(v4_bin)} ${cfg.c99_flag} -selfhost -o ${q(v5_bin)} ${q(cfg.v3_src)}')
	println('  Building v6 from v5...')
	run('${q(v5_bin)} ${cfg.c99_flag} -selfhost -o ${q(v6_bin)} ${q(cfg.v3_src)}')
	println('  Comparing generated C output from v4 and v5...')
	run('${q(v4_bin)} ${cfg.c99_flag} -selfhost -b c -o ${q(v5_bin + '.c')} ${q(cfg.v3_src)}')
	run('${q(v5_bin)} ${cfg.c99_flag} -selfhost -b c -o ${q(v6_bin + '.c')} ${q(cfg.v3_src)}')
	converged_size := assert_same_file_bytes('v5/v6 generated C output', v5_bin + '.c', v6_bin +
		'.c')
	println('  v5.c=v6.c (${converged_size} bytes) - chain converged')
	cleanup_files([v4_bin, v4_bin + '.c', v5_bin, v5_bin + '.c', v6_bin, v6_bin + '.c'])

	section(8, 'Language feature parity')
	lang_v := os.join_path(cfg.tests_dir, 'testdata', 'test_all_lang_features.v')
	lang_out := os.join_path(cfg.tests_dir, 'test_all_lang_features.out')
	run('${q(v3_bin)} ${cfg.c99_flag} ${q(lang_v)} -b c -o ${q(v3_lang_bin)}')
	v3_c_out := run_output(cfg, q(v3_lang_bin))
	expected_out := read_text_file(lang_out)
	assert_same_text('language feature output', v3_c_out, expected_out)
	println('  v3 C OK (${v3_c_out.split_into_lines().len} lines)')
	println('  ARM64 coverage is the one-generation macOS self-host smoke test in the ARM64 step')
	cleanup_files([v3_bin, v3_ownership_bin, v3_lang_bin, v3_lang_bin + '.c'])

	println('')
	println('=== ALL TESTS PASSED ===')
}

fn run_v3_unit_tests(cfg Config) {
	old_vflags := os.getenv('VFLAGS')
	old_vjobs := os.getenv('VJOBS')
	old_v3cache := os.getenv_opt('V3CACHE')
	old_vexe := os.getenv_opt('VEXE')
	old_wrapper_mode := os.getenv_opt(unit_wrapper_mode_env)
	old_wrapper_real_vexe := os.getenv_opt(unit_wrapper_real_vexe_env)
	old_wrapper_shared_v3 := os.getenv_opt(unit_wrapper_shared_v3_env)
	old_wrapper_v3_src := os.getenv_opt(unit_wrapper_v3_src_env)
	unit_cache := temp_path(cfg, 'unit_cache')
	shared_v3 := temp_path(cfg, 'unit_shared_v3')
	wrapper_vexe := os.join_path(cfg.repo_root, '.v3_test_unit_wrapper_${os.getpid()}')
	cleanup_files([shared_v3, wrapper_vexe])
	println('  Building shared V3 test compiler...')
	run('${host_v_cmd(cfg)} -o ${q(shared_v3)} ${q(cfg.v3_src)}')
	os.link(os.executable(), wrapper_vexe) or {
		os.cp(os.executable(), wrapper_vexe) or {
			fail('failed to create unit-test V wrapper ${wrapper_vexe}: ${err}')
		}
		os.chmod(wrapper_vexe, 0o755) or {
			fail('failed to make unit-test V wrapper executable: ${err}')
		}
	}
	os.setenv('VFLAGS', '${old_vflags} -gc none'.trim_space(), true)
	// Many V3 tests build compilers that share V3CACHE. Keep the outer test runner
	// serial so two nested compilers cannot publish overlapping cache generations.
	// Tests of parallel phases set VJOBS explicitly on their own subprocesses.
	os.setenv('VJOBS', '1', true)
	// A single cache across every test retains generated C for hundreds of unrelated
	// programs. Run bounded batches and reset only this suite-owned cache between them.
	// Cache regression tests use their own roots and still exercise reuse within a test.
	os.setenv('V3CACHE', unit_cache, true)
	os.setenv(unit_wrapper_mode_env, '1', true)
	os.setenv(unit_wrapper_real_vexe_env, cfg.vexe, true)
	os.setenv(unit_wrapper_shared_v3_env, shared_v3, true)
	os.setenv(unit_wrapper_v3_src_env, cfg.v3_src, true)
	os.setenv('VEXE', wrapper_vexe, true)
	test_files := os.walk_ext(cfg.script_dir, '_test.v').sorted()
	for start := 0; start < test_files.len; start += unit_test_batch_size {
		end := if start + unit_test_batch_size < test_files.len {
			start + unit_test_batch_size
		} else {
			test_files.len
		}
		println('  Unit test batch ${start / unit_test_batch_size + 1}: ${start + 1}-${end}/${test_files.len}')
		mut quoted_files := []string{cap: end - start}
		for path in test_files[start..end] {
			quoted_files << q(path)
		}
		run('${q(wrapper_vexe)} -old-compiler -gc none -path ${q(cfg.vlib_dir)} -enable-globals -silent test ${quoted_files.join(' ')}')
		if os.exists(unit_cache) {
			os.rmdir_all(unit_cache) or {
				fail('failed to reset V3 unit-test cache ${unit_cache}: ${err}')
			}
		}
	}
	if old_vflags == '' {
		os.unsetenv('VFLAGS')
	} else {
		os.setenv('VFLAGS', old_vflags, true)
	}
	if old_vjobs == '' {
		os.unsetenv('VJOBS')
	} else {
		os.setenv('VJOBS', old_vjobs, true)
	}
	if value := old_v3cache {
		os.setenv('V3CACHE', value, true)
	} else {
		os.unsetenv('V3CACHE')
	}
	restore_env('VEXE', old_vexe)
	restore_env(unit_wrapper_mode_env, old_wrapper_mode)
	restore_env(unit_wrapper_real_vexe_env, old_wrapper_real_vexe)
	restore_env(unit_wrapper_shared_v3_env, old_wrapper_shared_v3)
	restore_env(unit_wrapper_v3_src_env, old_wrapper_v3_src)
	cleanup_files([shared_v3, wrapper_vexe])
}

fn run_unit_vexe_wrapper() {
	real_vexe := os.getenv(unit_wrapper_real_vexe_env)
	shared_v3 := os.getenv(unit_wrapper_shared_v3_env)
	v3_src := os.getenv(unit_wrapper_v3_src_env)
	if !os.is_executable(real_vexe) {
		fail('unit-test V wrapper cannot find real compiler: ${real_vexe}')
	}
	args := os.args[1..]
	if output := unit_shared_compiler_request(args, v3_src) {
		if !os.is_executable(shared_v3) {
			fail('unit-test V wrapper cannot find shared V3 compiler: ${shared_v3}')
		}
		if os.exists(output) {
			os.rm(output) or { fail('failed to replace unit-test V3 compiler ${output}: ${err}') }
		}
		os.link(shared_v3, output) or {
			os.cp(shared_v3, output) or {
				fail('failed to reuse unit-test V3 compiler at ${output}: ${err}')
			}
			os.chmod(output, 0o755) or {
				fail('failed to make reused unit-test V3 compiler executable: ${err}')
			}
		}
		exit(0)
	}
	// On macOS `os.executable()` resolves a hard link back to this script's
	// original temporary binary. Preserve the repo-root wrapper path used to
	// invoke us so nested test runners derive the correct V root.
	os.setenv('VEXE', absolute_path(os.args[0]), true)
	os.execvp(real_vexe, args) or { fail('failed to run real V compiler ${real_vexe}: ${err}') }
}

fn unit_shared_compiler_request(args []string, v3_src string) ?string {
	if v3_src.len == 0 {
		return none
	}
	mut output := ''
	mut saw_source := false
	mut i := 0
	for i < args.len {
		arg := args[i]
		if arg == v3_src {
			saw_source = true
			i++
			continue
		}
		if arg in ['-o', '-path', '-gc'] {
			if i + 1 >= args.len {
				return none
			}
			if arg == '-gc' && args[i + 1] != 'none' {
				return none
			}
			if arg == '-o' {
				output = args[i + 1]
			}
			i += 2
			continue
		}
		if arg == '-old-compiler' {
			i++
			continue
		}
		// Compiler-build flags can change compiled-in behavior. Those requests
		// must keep building their own dedicated V3 binary.
		if arg.starts_with('-') {
			return none
		}
		return none
	}
	if saw_source && output.len > 0 {
		return output
	}
	return none
}

fn self_check_unit_shared_compiler_request() {
	source := '/repo/vlib/v3/v3.v'
	assert unit_shared_compiler_request(['-gc', 'none', '-path', '/repo/vlib', '-o', '/tmp/plain-v3',
		source], source) or { '' } == '/tmp/plain-v3'
	assert unit_shared_compiler_request(['-gc', 'boehm', '-o', '/tmp/boehm-v3', source], source) == none
	assert unit_shared_compiler_request(['-gc', 'none', '-prealloc', '-o', '/tmp/prealloc-v3',
		source], source) == none
	assert unit_shared_compiler_request(['-gc', 'none', '-d', 'ownership', '-o', '/tmp/ownership-v3',
		source], source) == none
}

fn restore_env(name string, old_value ?string) {
	if value := old_value {
		os.setenv(name, value, true)
	} else {
		os.unsetenv(name)
	}
}

fn setup_isolated_vtmp() string {
	path := os.join_path(os.temp_dir(), '${temp_prefix}_vtmp_${os.getpid()}')
	if os.exists(path) {
		os.rmdir_all(path) or { fail('failed to reset isolated VTMP ${path}: ${err}') }
	}
	os.mkdir_all(path) or { fail('failed to create isolated VTMP ${path}: ${err}') }
	// Several unit tests use os.temp_dir() for helper binaries with stable names.
	// Isolate those paths too, so concurrent V3 test runs cannot overwrite them.
	os.setenv('TMPDIR', path, true)
	os.setenv('VTMP', path, true)
	return path
}

fn parse_config() Config {
	c99 := parse_args()
	script_dir := os.real_path(@DIR)
	repo_root := os.real_path(os.join_path(script_dir, '..', '..'))
	tests_dir := os.join_path(script_dir, 'tests')
	vexe := absolute_path(@VEXE)
	if !os.is_executable(vexe) {
		fail('FAIL: V compiler not found: ${vexe}')
	}
	return Config{
		vexe:         vexe
		script_dir:   script_dir
		repo_root:    repo_root
		vlib_dir:     os.join_path(repo_root, 'vlib')
		tests_dir:    tests_dir
		v3_src:       os.join_path(script_dir, 'v3.v')
		c99:          c99
		c99_flag:     if c99 { '-c99' } else { '' }
		host_backend: native_backend_arch()
		host_os:      os.user_os()
		temp_prefix:  '${temp_prefix}_${os.getpid()}'
	}
}

fn parse_args() bool {
	mut c99 := false
	for arg in os.args[1..] {
		match arg {
			'-c99', '--c99' {
				c99 = true
			}
			'-h', '--help' {
				println('usage: test_all.vsh [-c99]')
				exit(0)
			}
			else {
				fail('unknown argument: ${arg}')
			}
		}
	}
	return c99
}

fn host_v_cmd(cfg Config) string {
	// Keep the bootstrap and V3 module-test builds on V1 after macOS defaults to
	// V3. The later harness steps use the freshly built V3 binary explicitly.
	return '${q(cfg.vexe)} -old-compiler -gc none -path ${q(cfg.vlib_dir)}'
}

fn native_backend_arch() string {
	machine := os.uname().machine.to_lower()
	match machine {
		'x86_64', 'amd64' {
			return 'x64'
		}
		'aarch64', 'arm64' {
			return 'arm64'
		}
		else {
			return machine
		}
	}
}

fn temp_path(cfg Config, name string) string {
	return os.join_path(os.temp_dir(), '${cfg.temp_prefix}_${name}')
}

fn absolute_path(path string) string {
	if os.is_abs_path(path) {
		return path
	}
	return os.join_path(os.getwd(), path)
}

fn section(step int, title string) {
	if step > 1 {
		println('')
	}
	println('=== ${step}/${total_steps}: ${title} ===')
}

fn run(cmd string) {
	println('> ${cmd}')
	code := os.system(cmd)
	if code != 0 {
		exit(code)
	}
}

fn run_output(cfg Config, cmd string) string {
	stdout_path := temp_path(cfg, 'stdout')
	cleanup_files([stdout_path])
	println('> ${cmd}')
	code := os.system('${cmd} > ${q(stdout_path)}')
	if code != 0 {
		exit(code)
	}
	content := read_text_file(stdout_path)
	cleanup_files([stdout_path])
	return content
}

fn unlocked_examples() []ExampleCase {
	return [
		example('examples/dump_factorial.v'),
		example_args('examples/fibonacci.v', ['10']),
		example('examples/fizz_buzz.v'),
		example('examples/function_types.v'),
		example_stdin('examples/get_raw_line.v', 'alpha\nbeta\n'),
		example('examples/graphs/bellman-ford.v'),
		example('examples/graphs/bfs.v'),
		example('examples/graphs/bfs3.v'),
		example('examples/graphs/dfs.v'),
		example('examples/graphs/dfs2.v'),
		example('examples/graphs/dijkstra.v'),
		example('examples/graphs/minimal_spann_tree_prim.v'),
		example('examples/graphs/topological_sorting_dfs.v'),
		example('examples/graphs/topological_sorting_greedy.v'),
		example('examples/hanoi.v'),
		example('examples/hello_world.v'),
		example('examples/js_hello_world.v'),
		example_stdin('examples/mini_calculator.v', '2*(5-1)\nexit\n'),
		example_stdin('examples/mini_calculator_recursive_descent.v', '2 * (5-1)\nexit\n'),
		example_args('examples/primes.v', ['10']),
		example('examples/quick_sort.v'),
		example('examples/random_ips.v'),
		example_args('examples/rule110.v', ['5']),
		example('examples/rune.v'),
		example_args('examples/spectral.v', ['10']),
		example('examples/submodule/main.v'),
		example('examples/sudoku.v'),
		example('examples/tree_of_nodes.v'),
		example('examples/vascii.v'),
		example('examples/binary_search_tree.v'),
		example('examples/custom_error.v'),
		example('examples/errors.v'),
		example_gui('examples/2048/2048.v', 5),
		example_gui('examples/tetris/tetris.v', 5),
		example_flags('vlib/v/tests/options/option_test.c.v', ['-autofree']),
	]
}

fn example(path string) ExampleCase {
	return ExampleCase{
		path: path
	}
}

fn example_args(path string, args []string) ExampleCase {
	return ExampleCase{
		path: path
		args: args
	}
}

fn example_stdin(path string, stdin string) ExampleCase {
	return ExampleCase{
		path:  path
		stdin: stdin
	}
}

fn example_flags(path string, flags []string) ExampleCase {
	return ExampleCase{
		path:          path
		compile_flags: flags
	}
}

fn example_gui(path string, timeout_seconds int) ExampleCase {
	return ExampleCase{
		path:            path
		mode:            .gui_smoke
		timeout_seconds: timeout_seconds
	}
}

fn run_unlocked_examples(cfg Config, v3_bin string, v3_ownership_bin string) {
	examples := unlocked_examples()
	mut ran := 0
	for i, example_case in examples {
		compiler := if '-autofree' in example_case.compile_flags { v3_ownership_bin } else { v3_bin }
		if run_unlocked_example(cfg, compiler, example_case, i) {
			ran++
		}
	}
	println('  ${ran}/${examples.len} real C oracle cases compiled and ran/smoked through V3 C')
}

fn run_unlocked_example(cfg Config, v3_bin string, example_case ExampleCase, index int) bool {
	src := os.join_path(cfg.repo_root, example_case.path)
	bin := temp_path(cfg, 'example_${index}')
	stdin_path := temp_path(cfg, 'example_${index}_stdin')
	cleanup_files([bin, bin + '.c', stdin_path])
	if example_case.mode == .gui_smoke && !gui_smoke_environment_available() {
		println('  SKIP ${example_case.path} (requires `xvfb-run` or an active display)')
		return false
	}
	mut compile_cmd := q(v3_bin)
	if cfg.c99_flag.len > 0 {
		compile_cmd += ' ' + cfg.c99_flag
	}
	if example_case.compile_flags.len > 0 {
		compile_cmd += ' ' + quote_args(example_case.compile_flags)
	}
	compile_cmd += ' ${q(src)} -b c -o ${q(bin)}'
	compile := os.execute(compile_cmd)
	if compile.exit_code != 0 {
		cleanup_files([bin, bin + '.c', stdin_path])
		print_command_failure('compile ${example_case.path}', compile_cmd, compile.output)
	}
	if example_case.mode == .gui_smoke {
		ran := run_gui_smoke_example(example_case, bin, stdin_path)
		cleanup_files([bin, bin + '.c', stdin_path])
		if ran {
			println('  OK ${example_case.path}')
		}
		return ran
	}
	mut run_cmd := q(bin)
	if example_case.args.len > 0 {
		run_cmd += ' ' + quote_args(example_case.args)
	}
	if example_case.stdin.len > 0 {
		os.write_file(stdin_path, example_case.stdin) or {
			cleanup_files([bin, bin + '.c', stdin_path])
			fail('FAIL: failed to write stdin for ${example_case.path}: ${err}')
		}
		run_cmd += ' < ${q(stdin_path)}'
	}
	run_result := os.execute(run_cmd)
	if run_result.exit_code != 0 {
		cleanup_files([bin, bin + '.c', stdin_path])
		print_command_failure('run ${example_case.path}', run_cmd, run_result.output)
	}
	cleanup_files([bin, bin + '.c', stdin_path])
	println('  OK ${example_case.path}')
	return true
}

fn gui_smoke_environment_available() bool {
	return os.getenv('DISPLAY').len > 0 || os.getenv('WAYLAND_DISPLAY').len > 0
		|| shell_command_exists('xvfb-run')
}

fn run_gui_smoke_example(example_case ExampleCase, bin string, stdin_path string) bool {
	if example_case.args.len > 0 || example_case.stdin.len > 0 {
		cleanup_files([bin, bin + '.c', stdin_path])
		fail('FAIL: GUI smoke case ${example_case.path} cannot use args/stdin')
	}
	if example_case.timeout_seconds <= 0 {
		cleanup_files([bin, bin + '.c', stdin_path])
		fail('FAIL: GUI smoke case ${example_case.path} needs a positive timeout')
	}
	mut command := bin
	mut args := []string{}
	if os.getenv('DISPLAY').len == 0 && os.getenv('WAYLAND_DISPLAY').len == 0 {
		if shell_command_exists('xvfb-run') {
			command = 'xvfb-run'
			args = ['-a', bin]
		} else {
			cleanup_files([bin, bin + '.c', stdin_path])
			println('  SKIP ${example_case.path} (requires `xvfb-run` or an active display)')
			return false
		}
	}
	run_cmd := command_with_args(command, args)
	run_result := run_process_with_timeout(command, args, example_case.timeout_seconds)
	if !gui_smoke_run_succeeded(run_result) {
		cleanup_files([bin, bin + '.c', stdin_path])
		print_command_failure('run ${example_case.path}', run_cmd, run_result.output)
	}
	return true
}

fn gui_smoke_run_succeeded(result ProcessRunResult) bool {
	return result.exit_code == 0 || result.timed_out
}

fn self_check_gui_smoke_timeout_status() {
	assert gui_smoke_run_succeeded(ProcessRunResult{
		exit_code: 0
	})
	assert gui_smoke_run_succeeded(ProcessRunResult{
		exit_code: 124
		timed_out: true
	})
	assert !gui_smoke_run_succeeded(ProcessRunResult{
		exit_code: 124
	})
	assert !gui_smoke_run_succeeded(ProcessRunResult{
		exit_code: 1
	})
}

fn shell_command_exists(name string) bool {
	return os.execute('command -v ${q(name)} >/dev/null 2>&1').exit_code == 0
}

fn run_process_with_timeout(command string, args []string, seconds int) ProcessRunResult {
	mut process := os.new_process(command)
	if args.len > 0 {
		process.set_args(args)
	}
	process.set_redirect_stdio()
	process.run()
	mut elapsed_ms := 0
	limit_ms := seconds * 1000
	for process.is_alive() {
		if elapsed_ms >= limit_ms {
			process.signal_kill()
			process.wait()
			output := process.stdout_slurp() + process.stderr_slurp()
			process.close()
			return ProcessRunResult{
				exit_code: 124
				output:    output
				timed_out: true
			}
		}
		time.sleep(50 * time.millisecond)
		elapsed_ms += 50
	}
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	exit_code := process.code
	process.close()
	return ProcessRunResult{
		exit_code: exit_code
		output:    output
	}
}

fn command_with_args(command string, args []string) string {
	if args.len == 0 {
		return q(command)
	}
	return q(command) + ' ' + quote_args(args)
}

fn quote_args(args []string) string {
	mut quoted := []string{cap: args.len}
	for arg in args {
		quoted << q(arg)
	}
	return quoted.join(' ')
}

fn print_command_failure(label string, cmd string, output string) {
	eprintln('FAIL: ${label}')
	eprintln('> ${cmd}')
	if output.len > 0 {
		eprintln(output)
	}
	exit(1)
}

fn read_text_file(path string) string {
	content := os.read_file(path) or {
		fail('FAIL: failed to read ${path}: ${err}')
		return ''
	}
	return content
}

fn read_binary_file(path string) []u8 {
	content := os.read_bytes(path) or {
		fail('FAIL: failed to read ${path}: ${err}')
		return []u8{}
	}
	return content
}

fn assert_same_file_bytes(label string, left_path string, right_path string) int {
	left := read_binary_file(left_path)
	right := read_binary_file(right_path)
	if left != right {
		fail('FAIL: ${label} differs byte-for-byte (${left.len} bytes vs ${right.len} bytes)')
	}
	return left.len
}

fn assert_same_text(label string, actual string, expected string) {
	if actual == expected {
		return
	}
	actual_lines := actual.split_into_lines()
	expected_lines := expected.split_into_lines()
	min_lines := if actual_lines.len < expected_lines.len {
		actual_lines.len
	} else {
		expected_lines.len
	}
	for i in 0 .. min_lines {
		if actual_lines[i] != expected_lines[i] {
			fail('FAIL: ${label} differs at line ${i + 1}: expected `${expected_lines[i]}`, got `${actual_lines[i]}`')
		}
	}
	fail('FAIL: ${label} line count differs: expected ${expected_lines.len}, got ${actual_lines.len}')
}

fn cleanup_files(paths []string) {
	for path in paths {
		os.rm(path) or {}
	}
}

fn q(path string) string {
	return os.quoted_path(path)
}

fn fail(message string) {
	eprintln(message)
	exit(1)
}
