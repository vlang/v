// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builder

import v2.ast
import v2.gen.v as gen_v
import v2.pref
import v2.token
// import v2.types
// import v2.util
// import v2.ir.ssa
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
	// TODO: build here, or pass into Parser and add there?
	// mut file_set := &token.FileSet{}
	b.files = if b.pref.no_parallel {
		b.parse_files(files)
	} else {
		b.parse_files_parallel(files)
	}
	parse_time := sw.elapsed()
	// b.type_check_files()
	type_check_time := time.Duration(sw.elapsed() - parse_time)
	if !b.pref.skip_genv {
		b.gen_v_files()
	}
	gen_v_time := time.Duration(sw.elapsed() - parse_time - type_check_time)
	total_time := sw.elapsed()
	print_time('Scan & Parse', parse_time)
	print_time('Type Check', type_check_time)
	print_time('Gen (v)', gen_v_time)
	print_time('Total', total_time)

	// for file in b.files {
	// 	mut ssa_builder := ssa.new_builder(b.pref)
	// 	program := ssa_builder.build_file(file)
	// 	println(program)
	// }
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

fn print_time(title string, time_d time.Duration) {
	println(' * ${title}: ${time_d.milliseconds()}ms (${time_d.microseconds()}µs)')
}
