// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module pref

// import v.ast
// import v.pref
// import os

// fn (mut p Pref) parse_line_info(line string, all_ast_files []&ast.File) {
fn (mut p Preferences) parse_line_info(line string) {
	// println("parse_line_info '${line}'")
	format_err := 'wrong format, use `-line-info "file.v:24:expr_to_look_up"'
	vals := line.split(':')
	if vals.len != 3 {
		eprintln(format_err)
		return
	}
	file_name := vals[0]
	line_nr := vals[1].int() - 1
	expr := vals[2]
	if !file_name.ends_with('.v') || line_nr == -1 {
		eprintln(format_err)
		return
	}

	// println('files.len=${c.files.len}')
	// Find which file contains the line
	mut found := true // false
	//// mut found_path := ''
	// mut found_file_idx := -1
	/*
	for i, file in all_ast_files {
		// base := os.base(file.path)
		base := file.path // os.base(file.path)
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
	*/

	if !found {
		eprintln('file "${file_name}" not found among those parsed')
		return
	}

	p.linfo = LineInfo{
		line_nr: line_nr
		path: file_name
		expr: expr
	}
}

pub fn add_line_info_expr_to_program_text(raw_text string, linfo LineInfo) string {
	lines := raw_text.split('\n')
	lines_before := lines[..linfo.line_nr].join('\n')
	mut expr := linfo.expr
	if !expr.contains('.') {
		// Single variable, `foo` => `foo.xx`
		// expr += '.xxx'
		// expr = '_ = ' + expr
		expr = 'println(' + expr + ')'
	}
	lines_after := lines[linfo.line_nr..].join('\n')
	return lines_before + '\n' + expr + '\n' + lines_after
}
