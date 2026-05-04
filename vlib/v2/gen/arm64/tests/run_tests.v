// Test runner for ARM64 backend tests
// Run with: ./v run vlib/v2/gen/arm64/tests/run_tests.v
// Or: cd vlib/v2/gen/arm64/tests && v run run_tests.v

module main

import os
import time

fn main() {
	t0 := time.now()

	vroot := os.dir(@VEXE)
	v2_source := os.join_path(vroot, 'cmd', 'v2', 'v2.v')
	v2_binary := os.join_path(vroot, 'cmd', 'v2', 'v2')
	tests_dir := os.join_path(vroot, 'vlib', 'v2', 'gen', 'arm64', 'tests')

	// Build v2 compiler
	println('[*] Building v2...')
	build_res := os.execute('${@VEXE} ${v2_source} -o ${v2_binary}')
	if build_res.exit_code != 0 {
		eprintln('Error: Failed to build v2')
		eprintln(build_res.output)
		return
	}

	// Find all test files (*.v except run_tests.v)
	test_files := os.ls(tests_dir) or {
		eprintln('Error: Cannot list tests directory')
		return
	}

	mut passed := 0
	mut failed := 0
	mut test_count := 0

	for file in test_files {
		if !file.ends_with('.v') || file == 'run_tests.v' {
			continue
		}
		test_count++
		test_path := os.join_path(tests_dir, file)
		test_name := file.replace('.v', '')

		println('\n[*] Testing: ${file}')

		// Run v2 with ARM64 backend
		v2_output := os.join_path(tests_dir, 'test_${test_name}')
		v2_cmd := '${v2_binary} -backend arm64 ${test_path} -o ${v2_output}'
		v2_res := os.execute(v2_cmd)
		if v2_res.exit_code != 0 {
			eprintln('  [FAIL] v2 compilation failed')
			eprintln(v2_res.output)
			failed++
			continue
		}

		// Run the generated binary
		gen_res := os.execute(v2_output)

		// Run reference compiler
		ref_res := os.execute('${@VEXE} -n -w -enable-globals run ${test_path}')
		if ref_res.exit_code != 0 {
			eprintln('  [FAIL] Reference compilation failed')
			eprintln(ref_res.output)
			failed++
			continue
		}

		// Compare outputs
		expected := ref_res.output.trim_space().replace('\r\n', '\n')
		actual := gen_res.output.trim_space().replace('\r\n', '\n')

		if expected == actual {
			println('  [PASS] Output matches reference')
			passed++
		} else {
			println('  [FAIL] Output mismatch')
			println('  Expected:\n${expected}')
			println('  Got:\n${actual}')
			failed++
		}

		// Clean up
		os.rm(v2_output) or {}
	}

	println('\n========================================')
	println('Results: ${passed}/${test_count} tests passed')
	if failed > 0 {
		println('${failed} tests FAILED')
	}
	println('Total time: ${time.since(t0)}')
}
