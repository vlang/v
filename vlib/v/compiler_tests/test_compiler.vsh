#!/usr/local/bin/v

import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const compiler_dir = os.dir(tests_dir)
const test_v = os.join_path(tests_dir, 'testdata', 'test_all_lang_features.v')
const test_out = os.join_path(tests_dir, 'test_all_lang_features.out')
const compiler_src = os.join_path(compiler_dir, 'v.v')

fn run(cmd string) os.Result {
	println('> ${cmd}')
	return os.execute(cmd)
}

fn build_compiler() string {
	compiler_bin := os.join_path(os.temp_dir(), 'v_compiler_test_runner')
	cmd := '${vexe} -o ${compiler_bin} ${compiler_src}'
	r := run(cmd)
	if r.exit_code != 0 {
		eprintln('FAIL: could not build the compiler')
		eprintln(r.output)
		exit(1)
	}
	return compiler_bin
}

fn run_compiler_c(compiler_bin string) string {
	compiler_c_bin := '${os.temp_dir()}/v_compiler_c_test'
	r := run('${compiler_bin} ${test_v} -b c -o ${compiler_c_bin}')
	if r.exit_code != 0 {
		eprintln('FAIL: C backend compilation failed')
		eprintln(r.output)
		exit(1)
	}
	return run_stdout(compiler_c_bin)
}

fn run_stdout(cmd string) string {
	stdout_path := '${os.temp_dir()}/v_compiler_c_test_stdout'
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

// Build the compiler.
compiler_bin := build_compiler()
println('built compiler: ${compiler_bin}')

// Run the C backend.
compiler_c_out := run_compiler_c(compiler_bin)
expected_out := read_text_file(test_out)
assert_same_text('C fixture output', compiler_c_out, expected_out)
println('C backend: OK')

println('=== C BACKEND OK (${compiler_c_out.split_into_lines().len} lines) ===')
