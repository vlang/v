// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module main

import v.vet
import v.ast
import v.pref
import v.parser
import v.util
import v.table
import os
import os.cmdline

struct VetOptions {
	is_verbose bool
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
	vet_options := VetOptions{
		is_verbose: '-verbose' in args || '-v' in args
	}
	for path in paths {
		if path.ends_with('.v') {
			vet_options.vet_file(path)
		} else if os.is_dir(path) {
			vet_options.vprintln("vetting folder '$path'...")
			files := os.walk_ext(path, '.v')
			for file in files {
				vet_options.vet_file(file)
			}
		}
	}
}

fn (vet_options &VetOptions) vet_file(path string) {
	mut prefs := pref.new_preferences()
	prefs.is_vet = true
	table := table.new_table()
	if path.contains('/tests') {
		return
	}
	file_ast := parser.parse_file(path, table, .parse_comments, prefs, &ast.Scope{
		parent: 0
	})
	vet_options.vprintln("vetting file '$path'...")
	vet.vet(file_ast, table, true)
}
