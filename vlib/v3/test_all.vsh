#!/usr/bin/env -S v

import os
import time

const total_steps = 8
const temp_prefix = 'v3_test_all'
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
	self_check_gui_smoke_timeout_status()
	cfg := parse_config()
	os.chdir(cfg.repo_root) or { fail('failed to enter ${cfg.repo_root}: ${err}') }

	v3_bin := temp_path(cfg, 'v3')
	hello_c_bin := temp_path(cfg, 'hello_c')
	hello_arm_bin := temp_path(cfg, 'hello_arm64')
	v4_arm_bin := temp_path(cfg, 'v4_arm64')
	v3_lang_bin := temp_path(cfg, 'v3_lang')
	v4_bin := temp_path(cfg, 'v4_chain')
	v5_bin := temp_path(cfg, 'v5_chain')
	v6_bin := temp_path(cfg, 'v6_chain')
	cleanup_files([
		v3_bin,
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
	run('${host_v_cmd(cfg)} -silent test ${q(cfg.script_dir)}')

	section(2, 'Build v3')
	run('${host_v_cmd(cfg)} -o ${q(v3_bin)} ${q(cfg.v3_src)}')

	section(3, 'Requested vlib tests')
	for rel_path in requested_vlib_tests {
		test_path := os.join_path(cfg.repo_root, rel_path)
		test_bin := temp_path(cfg, rel_path.replace('/', '_').replace('.v', ''))
		run('${q(v3_bin)} ${q(test_path)} -o ${q(test_bin)}')
		run(q(test_bin))
		cleanup_files([test_bin, test_bin + '.c'])
	}

	section(4, 'C backend hello world')
	hello_v := os.join_path(cfg.tests_dir, 'hello.v')
	run('${q(v3_bin)} ${cfg.c99_flag} ${q(hello_v)} -b c -o ${q(hello_c_bin)}')
	run(q(hello_c_bin))
	cleanup_files([hello_c_bin, hello_c_bin + '.c'])

	section(5, 'Unlocked examples C oracle')
	run_unlocked_examples(cfg, v3_bin)

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
	converged_size := assert_same_file_bytes('v5/v6 generated C output', v5_bin + '.c', v6_bin +
		'.c')
	println('  v5.c=v6.c (${converged_size} bytes) - chain converged')
	cleanup_files([v4_bin, v4_bin + '.c', v5_bin, v5_bin + '.c', v6_bin, v6_bin + '.c'])

	section(8, 'Language feature parity')
	lang_v := os.join_path(cfg.tests_dir, 'test_all_lang_features.v')
	lang_out := os.join_path(cfg.tests_dir, 'test_all_lang_features.out')
	run('${q(v3_bin)} ${cfg.c99_flag} ${q(lang_v)} -b c -o ${q(v3_lang_bin)}')
	v3_c_out := run_output(cfg, q(v3_lang_bin))
	expected_out := read_text_file(lang_out)
	assert_same_text('language feature output', v3_c_out, expected_out)
	println('  v3 C OK (${v3_c_out.split_into_lines().len} lines)')
	println('  ARM64 coverage is the one-generation macOS self-host smoke test in the ARM64 step')
	cleanup_files([v3_bin, v3_lang_bin, v3_lang_bin + '.c'])

	println('')
	println('=== ALL TESTS PASSED ===')
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
	return '${q(cfg.vexe)} -gc none -path ${q(cfg.vlib_dir)}'
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

fn run_unlocked_examples(cfg Config, v3_bin string) {
	examples := unlocked_examples()
	mut ran := 0
	for i, example_case in examples {
		if run_unlocked_example(cfg, v3_bin, example_case, i) {
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
