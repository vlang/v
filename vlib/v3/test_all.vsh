#!/usr/bin/env -S v

import os

const total_steps = 7
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
	tests_dir    string
	v3_src       string
	c99          bool
	c99_flag     string
	host_backend string
	host_os      string
	temp_prefix  string
}

fn main() {
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
	run('${q(cfg.vexe)} -silent test ${q(cfg.script_dir)}')

	section(2, 'Build v3')
	run('${q(cfg.vexe)} -o ${q(v3_bin)} ${q(cfg.v3_src)}')

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

	section(5, 'ARM64 self-host hello world')
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

	section(6, 'Self-host chain (v3->v4->v5->v6)')
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

	section(7, 'Language feature parity')
	lang_v := os.join_path(cfg.tests_dir, 'test_all_lang_features.v')
	lang_out := os.join_path(cfg.tests_dir, 'test_all_lang_features.out')
	run('${q(v3_bin)} ${cfg.c99_flag} ${q(lang_v)} -b c -o ${q(v3_lang_bin)}')
	v3_c_out := run_output(cfg, q(v3_lang_bin))
	expected_out := read_text_file(lang_out)
	assert_same_text('language feature output', v3_c_out, expected_out)
	println('  v3 C OK (${v3_c_out.split_into_lines().len} lines)')
	println('  ARM64 coverage is the one-generation macOS self-host smoke test in step 4')
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
