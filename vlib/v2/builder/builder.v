// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import os
import v2.ast
import v2.gen.arm64
import v2.gen.c
import v2.gen.cleanc
import v2.gen.v as gen_v
import v2.gen.x64
import v2.pref
import v2.ssa
import v2.token
import v2.transform
import time

struct Builder {
	pref &pref.Preferences
mut:
	files    []ast.File
	file_set &token.FileSet = token.FileSet.new()
}

pub fn new_builder(prefs &pref.Preferences) &Builder {
	unsafe {
		return &Builder{
			pref: prefs
		}
	}
}

pub fn (mut b Builder) build(files []string) {
	mut sw := time.new_stopwatch()
	b.files = if b.pref.no_parallel {
		b.parse_files(files)
	} else {
		b.parse_files_parallel(files)
	}
	parse_time := sw.elapsed()

	// b.type_check_files()
	type_check_time := time.Duration(sw.elapsed() - parse_time)

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
		.c {
			b.gen_ssa_c()
		}
		.x64 {
			b.gen_native(.x64)
		}
		.arm64 {
			b.gen_native(.arm64)
		}
	}

	gen_time := time.Duration(sw.elapsed() - parse_time - type_check_time)
	total_time := sw.elapsed()

	if b.pref.verbose {
		print_time('Scan & Parse', parse_time)
		print_time('Type Check', type_check_time)
		print_time('Gen', gen_time)
		print_time('Total', total_time)
	}
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
	for file in b.files {
		mut gen := cleanc.Gen.new(file)
		c_source := gen.gen()

		output_file := if b.pref.output_file != '' {
			b.pref.output_file
		} else {
			'out.c'
		}
		os.write_file(output_file, c_source) or { panic(err) }

		if b.pref.verbose {
			println('[*] Wrote ${output_file}')
		}
	}
}

fn (mut b Builder) gen_ssa_c() {
	// SSA -> C Backend - build all files into a single module
	mut mod := ssa.Module.new('main')
	mut ssa_builder := ssa.Builder.new(mod)
	mut t := transform.Transformer.new()

	for file in b.files {
		transformed_file := t.transform(file)
		ssa_builder.build(transformed_file)
	}
	mod.optimize()

	mut gen := c.Gen.new(mod)
	c_source := gen.gen()

	c_file := 'out.c'
	os.write_file(c_file, c_source) or { panic(err) }

	if b.pref.verbose {
		println('[*] Wrote ${c_file}')
	}

	// Compile C to binary
	output_binary := if b.pref.output_file != '' {
		b.pref.output_file
	} else if b.files.len > 0 {
		os.file_name(b.files.last().name).all_before_last('.v')
	} else {
		'out'
	}

	cc_result := os.execute('cc -o ${output_binary} ${c_file}')
	if cc_result.exit_code != 0 {
		eprintln('C compilation failed:')
		eprintln(cc_result.output)
		exit(1)
	}

	if b.pref.verbose {
		println('[*] Compiled ${output_binary}')
	}
}

fn (mut b Builder) gen_native(backend_arch pref.Arch) {
	arch := if backend_arch == .auto { b.pref.get_effective_arch() } else { backend_arch }

	// Build all files into a single SSA module
	mut mod := ssa.Module.new('main')
	mut ssa_builder := ssa.Builder.new(mod)
	mut t := transform.Transformer.new()

	for file in b.files {
		transformed_file := t.transform(file)
		ssa_builder.build(transformed_file)
	}
	mod.optimize()

	// Determine output binary name from the last user file
	output_binary := if b.pref.output_file != '' {
		b.pref.output_file
	} else if b.files.len > 0 {
		os.file_name(b.files.last().name).all_before_last('.v')
	} else {
		'out'
	}

	if arch == .arm64 && os.user_os() == 'macos' {
		// Use built-in linker for ARM64 macOS
		mut gen := arm64.Gen.new(mod)
		gen.gen()
		gen.link_executable(output_binary)

		if b.pref.verbose {
			println('[*] Linked ${output_binary} (built-in linker)')
		}
	} else {
		// Generate object file and use external linker
		obj_file := 'main.o'

		if arch == .arm64 {
			mut gen := arm64.Gen.new(mod)
			gen.gen()
			gen.write_file(obj_file)
		} else {
			mut gen := x64.Gen.new(mod)
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
	println(' * ${title}: ${time_d.milliseconds()}ms (${time_d.microseconds()}µs)')
}
