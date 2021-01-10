// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module main

import v.vet
import v.pref
import v.parser
import v.util
import v.table
import os
import os.cmdline

struct VetOptions {
	is_verbose bool
mut:
	errors     []vet.Error
}

fn (vet_options &VetOptions) vprintln(s string) {
	if !vet_options.is_verbose {
		return
	}
	println(s)
}

fn main() {
	args := util.join_env_vflags_and_os_args()
	paths := cmdline.only_non_options(cmdline.options_after(args, ['vet']))
	mut vet_options := VetOptions{
		is_verbose: '-verbose' in args || '-v' in args
	}
	for path in paths {
		if !os.exists(path) {
			eprintln('File/folder $path does not exist')
			continue
		}
		if path.ends_with('_test.v') ||
			(path.contains('/tests/') && !path.contains('cmd/tools/vvet/tests/')) {
			eprintln('skipping $path')
			continue
		}
		if path.ends_with('.v') || path.ends_with('.vv') {
			vet_options.vet_file(path)
		} else if os.is_dir(path) {
			vet_options.vprintln("vetting folder '$path'...")
			vfiles := os.walk_ext(path, '.v')
			vvfiles := os.walk_ext(path, '.vv')
			mut files := []string{}
			files << vfiles
			files << vvfiles
			for file in files {
				if file.ends_with('_test.v') || file.contains('/tests/') { // TODO copy pasta
					continue
				}
				vet_options.vet_file(file)
			}
		}
	}
	if vet_options.errors.len > 0 {
		for err in vet_options.errors.filter(it.kind == .error) {
			eprintln('$err.file_path:$err.pos.line_nr: $err.message')
		}
		eprintln('NB: You can run `v fmt -w file.v` to fix these automatically')
		/*
		for err in vet_options.errors.filter(it.kind == .warning) {
			eprintln('$err.file_path:$err.pos.line_nr: err.message')
		}
		*/
		exit(1)
	}
}

fn (mut vet_options VetOptions) vet_file(path string) {
	mut prefs := pref.new_preferences()
	prefs.is_vet = true
	table := table.new_table()
	vet_options.vprintln("vetting file '$path'...")
	_, errors := parser.parse_vet_file(path, table, prefs)
	// Transfer errors from scanner and parser
	vet_options.errors << errors
}
