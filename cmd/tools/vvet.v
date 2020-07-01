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

fn main() {
	mut prefs := pref.new_preferences()
	prefs.is_vet = true
	table := table.new_table()
	args := util.join_env_vflags_and_os_args()
	if args.len < 3 {
		return
	}
	path := args[2]
	if path.ends_with('.v') {
		vet_file(path, table, prefs)
	} else if os.is_dir(path) {
		println("vet'ing directory '$path'...")
		files := os.walk_ext(path, '.v')
		for file in files {
			vet_file(file, table, prefs)
		}
	}
}

fn vet_file(path string, table &table.Table, prefs &pref.Preferences) {
	if path.contains('/tests') {
		return
	}
	file_ast := parser.parse_file(path, table, .parse_comments, prefs, &ast.Scope{
		parent: 0
	})
	vet.vet(file_ast, table, true)
}
