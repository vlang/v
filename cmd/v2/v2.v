// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import time
import v2.pref
import v2.builder

fn main() {
	compile_args, runtime_args := split_eval_runtime_args(os.args[1..])

	// Check for subcommands
	if compile_args.len > 0 && compile_args[0] == 'ast' {
		run_ast_command(compile_args[1..])
		return
	}
	if compile_args.len > 0 && compile_args[0] == 'test-self' {
		run_test_self()
		return
	}

	mut prefs := pref.new_preferences_from_args(compile_args)

	files := get_files(compile_args)
	if files.len == 0 {
		eprintln('At least 1 .v file expected')
		exit(1)
	}
	mut eval_runtime_args := [files[0]]
	eval_runtime_args << runtime_args
	prefs.eval_runtime_args = eval_runtime_args

	mut b := builder.new_builder(prefs)
	b.build(files)

	// Auto-run test binaries after compilation (matching v1 behavior)
	if prefs.output_file == '' && is_test_file(files) {
		output_name := os.file_name(files.last()).all_before_last('.v')
		if os.exists(output_name) {
			ret := os.system('./' + output_name)
			os.rm(output_name) or {}
			c_file := output_name + '.c'
			if !prefs.keep_c && os.exists(c_file) {
				os.rm(c_file) or {}
			}
			exit(ret)
		}
	}
}

fn split_eval_runtime_args(args []string) ([]string, []string) {
	for i, arg in args {
		if arg == '--' {
			return args[..i], args[i + 1..]
		}
	}
	return args, []string{}
}

fn run_ast_command(args []string) {
	if args.len == 0 {
		eprintln('Usage: v2 ast <file.v>')
		eprintln('Dumps AST to <file>_ast.json and <file>_ast_transformed.json')
		exit(1)
	}

	// Find the vast2 tool relative to vexe
	vroot := os.dir(@VEXE)
	vast2_path := os.join_path(vroot, 'cmd', 'tools', 'vast2', 'vast2')

	// Build vast2 if it doesn't exist
	if !os.exists(vast2_path) {
		eprintln('Building vast2 tool...')
		vast2_source := os.join_path(vroot, 'cmd', 'tools', 'vast2', 'vast2.v')
		build_result := os.execute('${@VEXE} ${vast2_source}')
		if build_result.exit_code != 0 {
			eprintln('Failed to build vast2 tool:')
			eprintln(build_result.output)
			exit(1)
		}
	}

	// Run vast2 with the provided arguments
	cmd := '${vast2_path} ${args.join(' ')}'
	result := os.execute(cmd)
	print(result.output)
	if result.exit_code != 0 {
		exit(result.exit_code)
	}
}

fn is_test_file(files []string) bool {
	for file in files {
		if file.ends_with('_test.v') {
			return true
		}
	}
	return false
}

// get_files extracts source files from args, excluding options and their values
fn get_files(args []string) []string {
	options_with_values := ['-backend', '-b', '-o', '-output', '-arch', '-printfn', '-gc', '-d',
		'-hot-fn', '-cc']
	mut files := []string{}
	mut skip_next := false
	for arg in args {
		if arg == '--' {
			break
		}
		if skip_next {
			skip_next = false
			continue
		}
		if arg.starts_with('-') {
			if arg in options_with_values {
				skip_next = true
			}
			continue
		}
		files << arg
	}
	return files
}

fn resolve_own_path() string {
	arg0 := os.args[0]
	if os.is_abs_path(arg0) {
		return arg0
	}
	return os.join_path(os.getwd(), arg0)
}

// detect_vroot walks up from `start` looking for a directory containing vlib/builtin.
fn detect_vroot(start string) string {
	mut dir := start
	if !os.is_abs_path(dir) {
		dir = os.join_path(os.getwd(), dir)
	}
	for _ in 0 .. 10 {
		if os.is_dir(os.join_path(dir, 'vlib', 'builtin')) {
			return dir
		}
		parent := os.dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return dir
}

fn run_test_self() {
	t0 := time.now()
	// Resolve the v2 binary's own path. Cannot use @VEXE because when v1
	// compiles v2, @VEXE bakes in v1's path instead of v2's.
	vexe := resolve_own_path()
	// Walk up from the binary to find the repo root (directory containing vlib/builtin).
	// This allows running from subdirectories like cmd/v2/.
	vroot := detect_vroot(vexe)
	v2_dir := os.join_path(vroot, 'cmd', 'v2')

	mut all_test_files := []string{}

	// Builtin test files
	for name in ['array_test.v', 'string_test.v', 'map_test.v'] {
		all_test_files << os.join_path(vroot, 'vlib', 'builtin', name)
	}

	// Math test
	all_test_files << os.join_path(vroot, 'vlib', 'math', 'math_test.v')

	// Sumtype tests in cmd/v2/
	v2_files := os.ls(v2_dir) or { []string{} }
	for file in v2_files {
		if file.starts_with('test_sumtype') && file.ends_with('.v') {
			all_test_files << os.join_path(v2_dir, file)
		}
	}

	// Cleanc regression tests
	cleanc_tests_dir := os.join_path(vroot, 'vlib', 'v2', 'gen', 'cleanc', 'tests')
	cleanc_files := os.ls(cleanc_tests_dir) or { []string{} }
	for file in cleanc_files {
		if file.ends_with('.v') && file != 'run_tests.v' {
			all_test_files << os.join_path(cleanc_tests_dir, file)
		}
	}

	total := all_test_files.len
	mut passed := 0
	mut failed := 0
	mut failed_files := []string{}

	eprintln('---- v2 test-self: ${total} test files ----')

	for i, test_file in all_test_files {
		short_name := test_file.replace(vroot + '/', '')
		t1 := time.now()

		// Determine output binary path
		base := os.file_name(test_file).all_before_last('.v')
		out_bin := os.join_path(os.temp_dir(), 'v2_test_self_${base}')

		// Compile (use os.system to avoid pipe deadlocks with popen/GC)
		compile_cmd := '${vexe} -o ${out_bin} "${test_file}" > /dev/null 2>&1'
		compile_ret := os.system(compile_cmd)
		compile_ms := f64(time.since(t1)) / f64(time.millisecond)

		if compile_ret != 0 || !os.exists(out_bin) {
			failed++
			failed_files << short_name
			eprintln(' FAIL  [${i + 1:4}/${total}] C: ${compile_ms:7.1} ms  ${short_name}')
			os.rm(out_bin) or {}
			os.rm('${out_bin}.c') or {}
			continue
		}

		// Run the compiled binary
		t2 := time.now()
		run_ret := os.system(out_bin + ' > /dev/null 2>&1')
		run_ms := f64(time.since(t2)) / f64(time.millisecond)

		if run_ret != 0 {
			failed++
			failed_files << short_name
			eprintln(' FAIL  [${i + 1:4}/${total}] C: ${compile_ms:7.1} ms, R: ${run_ms:7.1} ms  ${short_name}')
		} else {
			passed++
			eprintln('OK    [${i + 1:4}/${total}] C: ${compile_ms:7.1} ms, R: ${run_ms:7.1} ms  ${short_name}')
		}

		os.rm(out_bin) or {}
		os.rm('${out_bin}.c') or {}
	}

	elapsed := time.since(t0)
	eprintln('------------------------------------------------------------------------')
	eprintln('${passed} passed, ${failed} failed, ${total} total  (${elapsed})')
	if failed_files.len > 0 {
		eprintln('Failed:')
		for f in failed_files {
			eprintln('  ${f}')
		}
	}
	// Always run the self-compilation chain, even when some tests fail.
	// Self-compilation chain: v2 -> v3 -> v4 -> v5
	eprintln('')
	eprintln('---- v2 self-compilation chain ----')

	v2_source := os.join_path(v2_dir, 'v2.v')
	backend := 'cleanc'
	tmpdir := os.temp_dir()
	v3_bin := os.join_path(tmpdir, 'v2_self_v3')
	v4_bin := os.join_path(tmpdir, 'v2_self_v4')
	v5_bin := os.join_path(tmpdir, 'v2_self_v5')

	steps := [
		[vexe, v3_bin, 'v2 -> v3'],
		[v3_bin, v4_bin, 'v3 -> v4'],
		[v4_bin, v5_bin, 'v4 -> v5'],
	]

	for step in steps {
		compiler := step[0]
		out := step[1]
		label := step[2]
		ts := time.now()
		cmd := '${compiler} -gc none -o ${out} -backend ${backend} "${v2_source}" > /dev/null 2>&1'
		ret := os.system(cmd)
		ms := f64(time.since(ts)) / f64(time.millisecond)
		if ret != 0 || !os.exists(out) {
			eprintln(' FAIL  ${label}  (${ms:.1} ms)')
			// Clean up
			for bin in [v3_bin, v4_bin, v5_bin] {
				os.rm(bin) or {}
				os.rm('${bin}.c') or {}
			}
			exit(1)
		}
		eprintln('OK    ${label}  (${ms:.1} ms)')
	}

	// Verify that v3 runs and produces expected output
	result := os.execute('${v3_bin} 2>&1')
	expected := 'At least 1 .v file expected'
	if !result.output.contains(expected) {
		eprintln("FAIL  v3 output check: expected '${expected}', got:")
		eprintln(result.output)
		for bin in [v3_bin, v4_bin, v5_bin] {
			os.rm(bin) or {}
			os.rm('${bin}.c') or {}
		}
		exit(1)
	}
	eprintln('OK    v3 output verified')

	// Clean up
	for bin in [v3_bin, v4_bin, v5_bin] {
		os.rm(bin) or {}
		os.rm('${bin}.c') or {}
	}

	total_elapsed := time.since(t0)
	eprintln('')
	if failed > 0 {
		eprintln('=== SELF-COMPILATION OK, but ${failed} test(s) failed ===')
		eprintln('Total time: ${total_elapsed}')
		exit(1)
	}
	eprintln('=== SELF-COMPILATION TEST PASSED ===')
	eprintln('Total time: ${total_elapsed}')
}
