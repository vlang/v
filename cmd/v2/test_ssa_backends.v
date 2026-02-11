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
	} else if os.args.contains('c') {
		backend = 'c'
	} else if os.args.contains('cleanc') {
		backend = 'cleanc'
	}

	// Parse test file from args or default to test.v
	// Support: ./test_ssa_backends arm64 path/to/file.v
	mut input_file := 'test.v'
	for arg in os.args {
		if arg.ends_with('.v') && arg != @FILE {
			input_file = arg
			break
		}
	}
	if !os.exists(input_file) {
		eprintln('Error: ${input_file} not found')
		return
	}

	// Derive output binary name from input file
	base_name := os.file_name(input_file).replace('.v', '')
	ref_output_path := './.${base_name}_ref.out.tmp'
	gen_output_path := './.${base_name}_gen.out.tmp'

	// Run v2 with selected backend
	println('[*] Running v2 -backend ${backend} ${input_file}...')
	mut backend_flags := '-backend ${backend}'
	if backend == 'cleanc' {
		// cleanc needs full per-run codegen for this suite right now.
		backend_flags += ' -nomarkused -nocache'
	}
	if os.args.contains('--skip-builtin') && !backend_flags.contains('--skip-builtin') {
		backend_flags += ' --skip-builtin'
	}
	v2_cmd := '${v2_binary} ${backend_flags} ${input_file} -o ${base_name}'
	v2_res := os.execute(v2_cmd)
	if v2_res.exit_code != 0 {
		eprintln('Error: v2 compilation failed')
		eprintln(v2_res.output)
		return
	}
	println(v2_res.output)
	println('compilation took ${time.since(t0)}')

	// Save the v2-produced binary before running reference (which would overwrite it)
	os.rm('./${base_name}_v2') or {}
	if os.user_os() == 'windows' {
		os.rm('./${base_name}_v2.exe') or {}
	}
	os.cp('./${base_name}', './${base_name}_v2') or {
		eprintln('Error: Failed to save v2 binary')
		return
	}

	// Get expected output: use .out file if --skip-builtin, otherwise run reference compiler
	mut expected_out := ''
	out_file := input_file.replace('.v', '.out')
	if os.args.contains('--skip-builtin') && os.exists(out_file) {
		println('[*] Using expected output from ${out_file}')
		expected_out = os.read_file(out_file) or { '' }.trim_space().replace('\r\n', '\n')
	} else {
		// Run Reference (v run test.v)
		println('[*] Running reference: ${@VEXE} -enable-globals run ${input_file}...')
		os.rm(ref_output_path) or {}
		ref_cmd := '${@VEXE} -n -w -enable-globals run ${input_file} > ${ref_output_path} 2>&1'
		ref_res := os.execute(ref_cmd)
		ref_out := os.read_file(ref_output_path) or { '' }
		os.rm(ref_output_path) or {}
		if ref_res.exit_code != 0 {
			eprintln('Error: Reference run failed')
			eprintln(ref_out)
			return
		}
		// Normalize newlines
		expected_out = ref_out.trim_space().replace('\r\n', '\n')
	}

	// Run Generated Binary (the v2-produced one we saved earlier)
	println('[*] Running generated binary...')
	mut cmd := './${base_name}_v2'
	if os.user_os() == 'windows' {
		cmd = '${base_name}_v2.exe'
	}
	os.rm(gen_output_path) or {}
	gen_cmd := '${cmd} > ${gen_output_path} 2>&1'
	gen_res := os.execute(gen_cmd)
	gen_out := os.read_file(gen_output_path) or { '' }
	os.rm(gen_output_path) or {}
	if gen_res.exit_code != 0 {
		if gen_res.exit_code == 142 || gen_res.exit_code == 14 {
			eprintln('Error: Execution timed out (infinite loop detected)')
			return
		}
		println('Warning: Binary exited with code ${gen_res.exit_code}')
	}

	// Strip terminal control characters that script command may prepend
	mut cleaned := gen_out.replace('\r\n', '\n').replace('\x04', '').replace('\x08', '')
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

		// Find first differing line
		mut first_diff := -1
		max_lines := if expected_lines.len > actual_lines.len {
			expected_lines.len
		} else {
			actual_lines.len
		}
		for i in 0 .. max_lines {
			exp := if i < expected_lines.len { expected_lines[i] } else { '<missing>' }
			act := if i < actual_lines.len { actual_lines[i] } else { '<missing>' }
			if exp != act {
				first_diff = i
				break
			}
		}

		if first_diff >= 0 {
			context := 2
			start := if first_diff > context { first_diff - context } else { 0 }
			end := if first_diff + context + 1 < max_lines {
				first_diff + context + 1
			} else {
				max_lines
			}

			println('\nExpected (reference compiler):')
			for i in start .. end {
				line := if i < expected_lines.len { expected_lines[i] } else { '<missing>' }
				println('${i + 1}: ${line}')
			}

			println('\nGot (v2 ${backend}):')
			for i in start .. end {
				line := if i < actual_lines.len { actual_lines[i] } else { '<missing>' }
				println('${i + 1}: ${line}')
			}
		}
	}
}
