#!/usr/local/bin/v

import os

const vexe = @VEXE
const v1exe = 'v'
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const test_v = os.join_path(tests_dir, 'test_all_lang_features.v')
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

fn run_v1() string {
	v1_bin := '${os.temp_dir()}/v1_test'
	r := run('${v1exe} -enable-globals -o ${v1_bin} ${test_v}')
	if r.exit_code != 0 {
		eprintln('FAIL: global v compilation failed')
		eprintln(r.output)
		exit(1)
	}
	r2 := run(v1_bin)
	if r2.exit_code != 0 {
		eprintln('FAIL: global v binary crashed (exit ${r2.exit_code})')
		exit(1)
	}
	return r2.output
}

fn run_v3_c(v3_bin string) string {
	v3c_bin := '${os.temp_dir()}/v3c_test'
	r := run('${v3_bin} ${test_v} -b c -o ${v3c_bin}')
	if r.exit_code != 0 {
		eprintln('FAIL: v3 C backend compilation failed')
		eprintln(r.output)
		exit(1)
	}
	r2 := run(v3c_bin)
	if r2.exit_code != 0 {
		eprintln('FAIL: v3 C binary crashed (exit ${r2.exit_code})')
		exit(1)
	}
	return r2.output
}

fn run_v3_arm64(v3_bin string) string {
	v3arm_bin := '${os.temp_dir()}/v3arm_test'
	r := run('${v3_bin} ${test_v} -b arm64 -o ${v3arm_bin}')
	if r.exit_code != 0 {
		eprintln('FAIL: v3 arm64 backend compilation failed')
		eprintln(r.output)
		exit(1)
	}
	r2 := run(v3arm_bin)
	if r2.exit_code != 0 {
		eprintln('FAIL: v3 arm64 binary crashed (exit ${r2.exit_code})')
		exit(1)
	}
	return r2.output
}

// Build v3
v3_bin := build_v3()
println('built v3: ${v3_bin}')

// Run all three
v1_out := run_v1()
println('v1:       OK')

v3c_out := run_v3_c(v3_bin)
println('v3 C:     OK')

v3arm_out := run_v3_arm64(v3_bin)
println('v3 arm64: OK')

// Compare
mut ok := true

if v3c_out != v1_out {
	eprintln('FAIL: v3 C output differs from v1')
	v1_lines := v1_out.split_into_lines()
	v3c_lines := v3c_out.split_into_lines()
	for i in 0 .. v1_lines.len {
		if i < v3c_lines.len && v1_lines[i] != v3c_lines[i] {
			eprintln('  line ${i + 1}: v1="${v1_lines[i]}" v3c="${v3c_lines[i]}"')
		}
	}
	if v1_lines.len != v3c_lines.len {
		eprintln('  v1: ${v1_lines.len} lines, v3c: ${v3c_lines.len} lines')
	}
	ok = false
}

if v3arm_out != v1_out {
	eprintln('FAIL: v3 arm64 output differs from v1')
	v1_lines := v1_out.split_into_lines()
	v3arm_lines := v3arm_out.split_into_lines()
	for i in 0 .. v1_lines.len {
		if i < v3arm_lines.len && v1_lines[i] != v3arm_lines[i] {
			eprintln('  line ${i + 1}: v1="${v1_lines[i]}" arm64="${v3arm_lines[i]}"')
		}
	}
	if v1_lines.len != v3arm_lines.len {
		eprintln('  v1: ${v1_lines.len} lines, arm64: ${v3arm_lines.len} lines')
	}
	ok = false
}

if ok {
	println('=== ALL BACKENDS MATCH (${v1_out.split_into_lines().len} lines) ===')
} else {
	exit(1)
}
