// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import v.ast
import os

fn (mut c Checker) do_line_info(line string, all_ast_files []&ast.File) {
	// println("do_line_info '${line}'")
	format_err := 'wrong format, use `-line-info "file.v:24"'
	vals := line.split(':')
	if vals.len != 2 {
		eprintln(format_err)
		return
	}
	file_name := vals[0]
	line_nr := vals[1].int() - 1
	if !file_name.ends_with('.v') || line_nr == -1 {
		eprintln(format_err)
		return
	}

	// println('ok  ${c.files.len}')
	// Find which file contains the line
	mut found := false
	mut found_path := ''
	mut found_file_idx := -1
	for i, file in all_ast_files {
		base := os.base(file.path)
		// println(base)
		if base == file_name {
			if found {
				eprintln('more than one "${file_name}" found: "${file.path}" and "${found_path}"')
				return
			}
			found = true
			found_path = file.path
			found_file_idx = i
		}
	}

	if !found {
		eprintln('file "${file_name}" not found among those parsed')
		return
	}

	// println('found ${found_path}')
	c.doing_line_info = line_nr
	c.check_files([all_ast_files[found_file_idx]])
}
