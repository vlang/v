// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import os
import v2.ast
import v2.abi
import v2.gen.arm64
import v2.gen.cleanc
import v2.gen.v as gen_v
import v2.gen.x64
import v2.insel
import v2.mir
import v2.pref
import v2.ssa
import v2.ssa.optimize
import v2.token
import v2.transformer
import v2.types
import time

struct Builder {
	pref &pref.Preferences
mut:
	files      []ast.File
	user_files []string // original user-provided files (for output name)
	file_set   &token.FileSet     = token.FileSet.new()
	env        &types.Environment = unsafe { nil } // Type checker environment
}

pub fn new_builder(prefs &pref.Preferences) &Builder {
	unsafe {
		return &Builder{
			pref: prefs
		}
	}
}

pub fn (mut b Builder) build(files []string) {
	b.user_files = files
	mut sw := time.new_stopwatch()
	$if parallel ? {
		b.files = if b.pref.no_parallel {
			b.parse_files(files)
		} else {
			b.parse_files_parallel(files)
		}
	} $else {
		b.files = b.parse_files(files)
	}
	parse_time := sw.elapsed()
	print_time('Scan & Parse', parse_time)

	if b.pref.skip_type_check {
		b.env = types.Environment.new()
	} else {
		$if parallel ? {
			b.env = if b.pref.no_parallel {
				b.type_check_files()
			} else {
				b.type_check_files_parallel()
			}
		} $else {
			b.env = b.type_check_files()
		}
	}
	type_check_time := time.Duration(sw.elapsed() - parse_time)
	print_time('Type Check', type_check_time)

	// Transform AST (flag enum desugaring, etc.)
	transform_start := sw.elapsed()
	mut trans := transformer.Transformer.new(b.files, b.env)
	b.files = trans.transform_files(b.files)
	transform_time := time.Duration(sw.elapsed() - transform_start)
	print_time('Transform', transform_time)

	// Generate output based on backend
	match b.pref.backend {
		.v {
			if !b.pref.skip_genv {
				b.gen_v_files()
			}
		}
		.cleanc {
			b.gen_cleanc()
		}
		.x64 {
			b.gen_native(.x64)
		}
		.arm64 {
			b.gen_native(.arm64)
		}
	}

	print_time('Total', sw.elapsed())
}

fn (mut b Builder) gen_v_files() {
	mut gen := gen_v.new_gen(b.pref)
	for file in b.files {
		gen.gen(file)
		if b.pref.debug {
			gen.print_output()
		}
	}
}

fn (mut b Builder) gen_cleanc() {
	// Clean C Backend (AST -> C)
	// Pass all files to cleanc - it will handle filtering system types
	mut sw := time.new_stopwatch()

	mut gen := cleanc.Gen.new_with_env_and_pref(b.files, b.env, b.pref)
	c_source := gen.gen()
	print_time('C Gen', sw.elapsed())

	// Check if cleanc.Gen.gen() returned empty (stubbed version)
	if c_source == '' {
		eprintln('error: cleanc backend is not fully functional (compiled with stubbed functions)')
		eprintln('hint: use v2 compiled with v1 for proper C code generation')
		return
	}

	// Determine output name
	output_name := if b.pref.output_file != '' {
		b.pref.output_file
	} else if b.user_files.len > 0 {
		os.file_name(b.user_files.last()).all_before_last('.v')
	} else {
		'out'
	}

	// If output ends with .c, just write the C file
	if output_name.ends_with('.c') {
		os.write_file(output_name, c_source) or { panic(err) }
		println('[*] Wrote ${output_name}')
	} else {
		// Write to temp .c file, compile to binary, then clean up
		c_file := output_name + '.c'
		os.write_file(c_file, c_source) or { panic(err) }
		println('[*] Wrote ${c_file}')

		// Compile C to binary
		cc_start := sw.elapsed()
		cc := os.getenv_opt('V2CC') or { 'cc' }
		cc_flags := os.getenv_opt('V2CFLAGS') or { '' }
		version_res := os.execute('${cc} --version')
		mut error_limit_flag := ''
		if version_res.exit_code == 0 && version_res.output.contains('clang') {
			error_limit_flag = ' -ferror-limit=0'
		}
		cc_cmd := '${cc} ${cc_flags} -w ${c_file} -o ${output_name}${error_limit_flag}'
		if os.getenv('V2VERBOSE') != '' {
			dump(cc_cmd)
		}
		compile_result := os.execute(cc_cmd)
		if compile_result.exit_code != 0 {
			eprintln('C compilation failed:')
			lines := compile_result.output.split_into_lines()
			limit := if lines.len < 50 { lines.len } else { 50 }
			for line in lines[..limit] {
				eprintln(line)
			}
			// Count errors and warnings
			mut error_count := 0
			mut warning_count := 0
			for line in lines {
				if line.contains(': error:') || line.contains(': fatal error:') {
					error_count += 1
				} else if line.contains(': warning:') {
					warning_count += 1
				}
			}
			eprintln('Total: ${warning_count} warnings and ${error_count} errors')
			exit(1)
		}
		print_time('CC', time.Duration(sw.elapsed() - cc_start))

		println('[*] Compiled ${output_name}')

		// Keep C file (always preserve for debugging)
		// To explicitly remove, use os.rm manually
	}
}

fn (mut b Builder) gen_native(backend_arch pref.Arch) {
	arch := if backend_arch == .auto { b.pref.get_effective_arch() } else { backend_arch }

	// Build all files into a single SSA module
	mut mod := ssa.Module.new('main')
	if mod == unsafe { nil } {
		eprintln('error: native backend not available (compiled with stubbed ssa module)')
		eprintln('hint: use v2 compiled with v1 for native code generation')
		return
	}
	mut ssa_builder := ssa.Builder.new_with_env(mod, b.env)
	mut native_sw := time.new_stopwatch()

	// Build all files together with proper multi-file ordering
	mut stage_start := native_sw.elapsed()
	ssa_builder.build_all(b.files)
	print_time('SSA Build', time.Duration(native_sw.elapsed() - stage_start))

	stage_start = native_sw.elapsed()
	optimize.optimize(mut mod)
	print_time('SSA Optimize', time.Duration(native_sw.elapsed() - stage_start))
	$if debug {
		optimize.verify_and_panic(mod, 'full optimization')
	}

	stage_start = native_sw.elapsed()
	mut mir_mod := mir.lower_from_ssa(mod)
	print_time('MIR Lower', time.Duration(native_sw.elapsed() - stage_start))

	stage_start = native_sw.elapsed()
	abi.lower(mut mir_mod, arch)
	print_time('ABI Lower', time.Duration(native_sw.elapsed() - stage_start))

	stage_start = native_sw.elapsed()
	insel.select(mut mir_mod, arch)
	print_time('InsSel', time.Duration(native_sw.elapsed() - stage_start))

	// Determine output binary name from the last user file
	output_binary := if b.pref.output_file != '' {
		b.pref.output_file
	} else if b.user_files.len > 0 {
		os.file_name(b.user_files.last()).all_before_last('.v')
	} else {
		'out'
	}

	if arch == .arm64 && os.user_os() == 'macos' {
		// Use built-in linker for ARM64 macOS
		mut gen := arm64.Gen.new(&mir_mod)
		gen.gen()
		gen.link_executable(output_binary)

		if b.pref.verbose {
			println('[*] Linked ${output_binary} (built-in linker)')
		}
	} else {
		// Generate object file and use external linker
		obj_file := 'main.o'

		if arch == .arm64 {
			mut gen := arm64.Gen.new(&mir_mod)
			gen.gen()
			gen.write_file(obj_file)
		} else {
			mut gen := x64.Gen.new(&mir_mod)
			gen.gen()
			gen.write_file(obj_file)
		}

		if b.pref.verbose {
			println('[*] Wrote ${obj_file}')
		}

		// Link the object file into an executable
		if os.user_os() == 'macos' {
			sdk_res := os.execute('xcrun -sdk macosx --show-sdk-path')
			sdk_path := sdk_res.output.trim_space()
			arch_flag := if arch == .arm64 { 'arm64' } else { 'x86_64' }
			link_cmd := 'ld -o ${output_binary} ${obj_file} -lSystem -syslibroot "${sdk_path}" -e _main -arch ${arch_flag} -platform_version macos 11.0.0 11.0.0'
			link_result := os.execute(link_cmd)
			if link_result.exit_code != 0 {
				eprintln('Link failed:')
				eprintln(link_result.output)
				exit(1)
			}
		} else {
			// Linux linking
			link_result := os.execute('cc ${obj_file} -o ${output_binary} -no-pie')
			if link_result.exit_code != 0 {
				eprintln('Link failed:')
				eprintln(link_result.output)
				exit(1)
			}
		}

		if b.pref.verbose {
			println('[*] Linked ${output_binary}')
		}

		// Clean up object file
		os.rm(obj_file) or {}
	}
}

fn print_time(title string, time_d time.Duration) {
	println(' * ${title}: ${time_d.milliseconds()}ms')
}
