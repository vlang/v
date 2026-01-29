// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import time

fn main() {
	t0 := time.now()

	// Build v2 compiler
	println('[*] Building v2...')
	vroot := os.dir(@VEXE)
	v2_source := os.join_path(vroot, 'cmd', 'v2', 'v2.v')
	v2_binary := os.join_path(vroot, 'cmd', 'v2', 'v2')
	build_res := os.execute('${@VEXE} ${v2_source} -o ${v2_binary}')
	if build_res.exit_code != 0 {
		eprintln('Error: Failed to build v2')
		eprintln(build_res.output)
		return
	}

	// Determine backend from command line args
	mut backend := 'arm64' // default
	if os.args.contains('x64') {
		backend = 'x64'
	} else if os.args.contains('cleanc') {
		backend = 'cleanc'
	}

	// Parse test file from args or default to test.v
	input_file := 'test.v'
	if !os.exists(input_file) {
		eprintln('Error: ${input_file} not found')
		return
	}

	// Run v2 with selected backend
	println('[*] Running v2 -backend ${backend} ${input_file}...')
	mut v2_cmd := '${v2_binary} -backend ${backend} ${input_file}'
	if os.args.contains('--skip-builtin') {
		v2_cmd = '${v2_binary} -backend ${backend} --skip-builtin ${input_file}'
	}
	v2_res := os.execute(v2_cmd)
	if v2_res.exit_code != 0 {
		eprintln('Error: v2 compilation failed')
		eprintln(v2_res.output)
		return
	}
	println(v2_res.output)
	println('compilation took ${time.since(t0)}')

	// Run Reference (v run test.v)
	println('[*] Running reference: v -enable-globals run ${input_file}...')
	ref_res := os.execute('v -n -w -enable-globals run ${input_file}')
	if ref_res.exit_code != 0 {
		eprintln('Error: Reference run failed')
		eprintln(ref_res.output)
		return
	}
	// Normalize newlines
	expected_out := ref_res.output.trim_space().replace('\r\n', '\n')

	// Run Generated Binary
	println('[*] Running generated binary...')
	// Use script command to create a pty (fixes output buffering issues with printf)
	// The -q flag suppresses "Script started" messages, /dev/null discards the typescript file
	mut cmd := 'script -q /dev/null ./out_bin'
	if os.user_os() == 'windows' {
		cmd = 'out_bin.exe'
	} else if os.user_os() != 'macos' {
		// Linux version of script has different syntax
		cmd = 'script -q -c ./out_bin /dev/null'
	}
	gen_res := os.execute(cmd)
	if gen_res.exit_code != 0 {
		if gen_res.exit_code == 142 || gen_res.exit_code == 14 {
			eprintln('Error: Execution timed out (infinite loop detected)')
			return
		}
		println('Warning: Binary exited with code ${gen_res.exit_code}')
	}

	// Strip terminal control characters that script command may prepend
	mut cleaned := gen_res.output.replace('\r\n', '\n').replace('\x04', '').replace('\x08',
		'')
	// Remove "^D" literal string that macOS script may add
	if cleaned.starts_with('^D') {
		cleaned = cleaned[2..]
	}
	actual_out := cleaned.trim_space()

	// Compare
	if expected_out == actual_out {
		println('\n[SUCCESS] Outputs match!')
	} else {
		println('\n[FAILURE] Outputs differ')
		expected_lines := expected_out.split('\n')
		actual_lines := actual_out.split('\n')
		max_lines := if expected_lines.len > actual_lines.len {
			expected_lines.len
		} else {
			actual_lines.len
		}
		for i in 0 .. max_lines {
			exp := if i < expected_lines.len { expected_lines[i] } else { '<missing>' }
			act := if i < actual_lines.len { actual_lines[i] } else { '<missing>' }
			if exp != act {
				println('line ${i + 1}:')
				println('  expected: ${exp}')
				println('       got: ${act}')
			}
		}
	}
}
