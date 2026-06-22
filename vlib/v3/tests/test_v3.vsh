#!/usr/local/bin/v

import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const test_v = os.join_path(tests_dir, 'test_all_lang_features.v')
const test_out = os.join_path(tests_dir, 'test_all_lang_features.out')
const v3_src = os.join_path(v3_dir, 'v3.v')

fn run(cmd string) os.Result {
	println('> ${cmd}')
	return os.execute(cmd)
}

fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_test_runner')
	cmd := '${vexe} -o ${v3_bin} ${v3_src}'
	r := run(cmd)
	if r.exit_code != 0 {
		eprintln('FAIL: could not build v3')
		eprintln(r.output)
		exit(1)
	}
	return v3_bin
}

fn run_v3_c(v3_bin string) string {
	v3c_bin := '${os.temp_dir()}/v3c_test'
	r := run('${v3_bin} ${test_v} -b c -o ${v3c_bin}')
	if r.exit_code != 0 {
		eprintln('FAIL: v3 C backend compilation failed')
		eprintln(r.output)
		exit(1)
	}
	return run_stdout(v3c_bin)
}

fn run_stdout(cmd string) string {
	stdout_path := '${os.temp_dir()}/v3c_test_stdout'
	os.rm(stdout_path) or {}
	println('> ${cmd}')
	code := os.system('${cmd} > ${os.quoted_path(stdout_path)}')
	if code != 0 {
		eprintln('FAIL: command failed (exit ${code})')
		exit(code)
	}
	output := read_text_file(stdout_path)
	os.rm(stdout_path) or {}
	return output
}

fn read_text_file(path string) string {
	content := os.read_file(path) or {
		eprintln('FAIL: failed to read ${path}: ${err}')
		exit(1)
	}
	return content
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
			eprintln('FAIL: ${label} differs at line ${i + 1}: expected `${expected_lines[i]}`, got `${actual_lines[i]}`')
			exit(1)
		}
	}
	eprintln('FAIL: ${label} line count differs: expected ${expected_lines.len}, got ${actual_lines.len}')
	exit(1)
}

// Build v3
v3_bin := build_v3()
println('built v3: ${v3_bin}')

// Run V3 backends
v3c_out := run_v3_c(v3_bin)
expected_out := read_text_file(test_out)
assert_same_text('v3 C fixture output', v3c_out, expected_out)
println('v3 C:     OK')

println('=== V3 C OK (${v3c_out.split_into_lines().len} lines) ===')
