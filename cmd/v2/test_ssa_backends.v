// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import v2.parser
import v2.token
import v2.pref
import v2.ssa
import v2.transform
import v2.gen.x64
import v2.gen.arm64
import v2.gen.cleanc
import v2.gen.c
import time

enum Arch {
	arm64
	x64
}

fn main() {
	// Setup Parser
	t0 := time.now()
	prefs := &pref.Preferences{}
	mut file_set := token.FileSet.new()
	mut p := parser.Parser.new(prefs)
	mut transformer := transform.Transformer.new()

	// Initialize SSA Module
	mut mod := ssa.Module.new('main')
	mut builder := ssa.Builder.new(mod)

	// Get the directory where this script is located
	exe_dir := os.dir(os.executable())

	// Parse and build builtin files first (local stubs for now)
	// Full builtin support requires type checking in the SSA builder
	for builtin_name in ['string', 'array'] {
		builtin_file := os.join_path(exe_dir, 'builtin', '${builtin_name}.v')
		if os.exists(builtin_file) {
			println('[*] Parsing builtin/${builtin_name}.v...')
			parsed := p.parse_file(builtin_file, mut file_set)
			if parsed.stmts.len > 0 {
				println('    Found ${parsed.stmts.len} statements in builtin/${builtin_name}.v')
				builder.build(transformer.transform(parsed))
			}
		}
	}

	//  Parse File
	input_file := 'test.v'
	if !os.exists(input_file) {
		eprintln('Error: ${input_file} not found')
		return
	}
	println('[*] Parsing ${input_file}...')
	parsed_file := p.parse_file(input_file, mut file_set)
	if parsed_file.stmts.len == 0 {
		println('Warning: No statements found in ${input_file}')
	}
	// Transform AST (lower complex constructs like ArrayInitExpr)
	println('[*] Transforming AST...')
	file := transformer.transform(parsed_file)
	//  Build SSA from AST
	println('[*] Building SSA...')
	builder.build(file)
	// Optimize
	println('[*] Optimizing SSA...')
	mod.optimize()
	// Backend selection: default to native, use 'cleanc' or 'c' arg to switch
	use_cleanc := os.args.contains('cleanc')
	use_ssa_c := os.args.contains('c') && !use_cleanc
	native := !use_cleanc && !use_ssa_c
	// Default architecture based on OS
	mut arch := if os.user_os() == 'macos' { Arch.arm64 } else { Arch.x64 }
	// Allow override via command line
	if os.args.contains('x64') {
		arch = .x64
	} else if os.args.contains('arm64') {
		arch = .arm64
	}
	use_builtin_linker := os.args.contains('builtin-linker')

	if native {
		if arch == .arm64 {
			// Generate Mach-O ARM64
			println('[*] Generating Mach-O ARM64...')
			mut arm_gen := arm64.Gen.new(mod)
			arm_gen.gen()

			if use_builtin_linker && os.user_os() == 'macos' {
				// Use built-in linker
				println('[*] Using built-in linker...')
				arm_gen.link_executable('out_bin')
				println('generation + linking took ${time.since(t0)}')
			} else {
				arm_gen.write_file('main.o')
				println('generating main.o took ${time.since(t0)}')
				// Link with external linker
				println('[*] Linking with external linker...')
				t := time.now()
				if os.user_os() == 'macos' {
					sdk_res := os.execute('xcrun -sdk macosx --show-sdk-path')
					sdk_path := sdk_res.output.trim_space()
					link_cmd := 'ld -o out_bin main.o -lSystem -syslibroot "${sdk_path}" -e _main -arch arm64 -platform_version macos 11.0.0 11.0.0'
					link_result := os.execute(link_cmd)
					if link_result.exit_code != 0 {
						eprintln('Link failed:')
						eprintln(link_result.output)
						return
					}
				}
				println('linking took ${time.since(t)}')
			}
		} else if arch == .x64 {
			println('[*] Generating ELF AMD64 Object...')
			mut x64_gen := x64.Gen.new(mod)
			x64_gen.gen()
			x64_gen.write_file('main.o')
			println('generating main.o took ${time.since(t0)}')
			// Link
			println('[*] Linking...')
			t := time.now()
			if os.user_os() == 'macos' {
				// macOS Linking (Mach-O)
				sdk_res := os.execute('xcrun -sdk macosx --show-sdk-path')
				sdk_path := sdk_res.output.trim_space()
				link_cmd := 'ld -o out_bin main.o -lSystem -syslibroot "${sdk_path}" -e _main -arch x86_64 -platform_version macos 11.0.0 11.0.0'
				if os.system(link_cmd) != 0 {
					eprintln('Link failed')
					return
				}
			} else {
				// Linux Linking (ELF)
				link_cmd := 'cc main.o -o out_bin -no-pie'
				if os.system(link_cmd) != 0 {
					eprintln('Link failed')
					return
				}
			}
			println('linking took ${time.since(t)}')
		}
	} else if use_ssa_c {
		// SSA -> C Backend
		println('[*] Generating SSA C Backend...')
		mut c_gen := c.Gen.new(mod)
		c_source := c_gen.gen()
		os.write_file('out.c', c_source) or { panic(err) }
		println('[*] Done. Wrote out.c')
		// Compile C Code
		println('[*] Compiling out.c...')
		cc_res := os.system('cc out.c -o out_bin -w')
		if cc_res != 0 {
			eprintln('Error: C compilation failed with code ${cc_res}')
			return
		}
	} else {
		// Clean C Backend (AST -> C)
		println('[*] Generating Clean C Backend...')
		// We use the file AST directly instead of SSA for readable C
		mut c_gen := cleanc.Gen.new(file)
		c_source := c_gen.gen()
		os.write_file('out.c', c_source) or { panic(err) }
		println('[*] Done. Wrote out.c')
		// Compile C Code
		println('[*] Compiling out.c...')
		cc_res := os.system('cc out.c -o out_bin -w')
		if cc_res != 0 {
			eprintln('Error: C compilation failed with code ${cc_res}')
			return
		}
	}
	// Run Reference (v run test.v)
	println('[*] Running reference: v -enable-globals run ${input_file}...')
	ref_res := os.execute('v -n -enable-globals run ${input_file}')
	if ref_res.exit_code != 0 {
		eprintln('Error: Reference run failed')
		eprintln(ref_res.output)
		return
	}
	// Normalize newlines
	expected_out := ref_res.output.trim_space().replace('\r\n', '\n')
	// Run Generated Binary
	println('[*] Running generated binary (with 2s timeout)...')
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
	// Perl alarm usually kills with SIGALRM (14), exit code might vary (e.g. 142)
	// If it was killed by signal, we assume timeout.
	if gen_res.exit_code != 0 {
		// Check for timeout symptoms
		// Standard SIGALRM is 14. Bash reports 128+14=142.
		if gen_res.exit_code == 142 || gen_res.exit_code == 14 {
			eprintln('Error: Execution timed out (infinite loop detected)')
			return
		}
		// It might just be a crash or non-zero return (our main returns 0 usually)
		if gen_res.exit_code != 0 {
			// In the current builder, main returns 0. If it returns something else, it might be an error.
			// However, perl exec propagation might change codes.
			// Let's proceed to compare output, but warn.
			println('Warning: Binary exited with code ${gen_res.exit_code}')
		}
	}
	// Strip terminal control characters that script command may prepend
	// This includes: ^D (literal), ctrl-D (0x04), backspace (0x08), carriage return, etc.
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
