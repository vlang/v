// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module main

import os
import os.cmdline
import v.vet
import v.pref
import v.parser
import v.util
import v.table
import v.token

struct Vet {
	opt    Options
mut:
	errors []vet.Error
	file   string
}

struct Options {
	is_verbose bool
}

fn (vet &Vet) vprintln(s string) {
	if !vet.opt.is_verbose {
		return
	}
	println(s)
}

fn main() {
	args := util.join_env_vflags_and_os_args()
	paths := cmdline.only_non_options(cmdline.options_after(args, ['vet']))
	opt := Options{
		is_verbose: '-verbose' in args || '-v' in args
	}
	mut vet := Vet{
		opt: opt
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
			vet.vet_file(path)
		} else if os.is_dir(path) {
			vet.vprintln("vetting folder '$path'...")
			vfiles := os.walk_ext(path, '.v')
			vvfiles := os.walk_ext(path, '.vv')
			mut files := []string{}
			files << vfiles
			files << vvfiles
			for file in files {
				if file.ends_with('_test.v') || file.contains('/tests/') { // TODO copy pasta
					continue
				}
				vet.vet_file(file)
			}
		}
	}
	if vet.errors.len > 0 {
		for err in vet.errors.filter(it.kind == .error && it.fix == .vfmt) {
			eprintln('$err.file_path:$err.pos.line_nr: $err.message')
		}
		eprintln('NB: You can run `v fmt -w file.v` to fix these automatically')
		/*
		for err in vet.errors.filter(it.kind == .warning) {
			eprintln('$err.file_path:$err.pos.line_nr: err.message')
		}
		*/
		exit(1)
	}
}

fn (mut v Vet) error(msg string, line int, fix vet.FixKind) {
	pos := token.Position{
		line_nr: line
	}
	v.errors << vet.Error{
		message: msg
		file_path: v.file
		pos: pos
		kind: .error
		fix: fix
	}
}

fn (mut v Vet) warn(msg string, line int, fix vet.FixKind) {
	pos := token.Position{
		line_nr: line
	}
	v.errors << vet.Error{
		message: msg
		file_path: v.file
		pos: pos
		kind: .warning
		fix: fix
	}
}

// vet_file vets the file read from `path`.
fn (mut vt Vet) vet_file(path string) {
	vt.file = path
	mut prefs := pref.new_preferences()
	prefs.is_vet = true
	table := table.new_table()
	vet_options.vprintln("vetting file '$path'...")
	_, errors := parser.parse_vet_file(path, table, prefs)
	// Transfer errors from scanner and parser
	vt.errors << errors
	// Scan each line in file for things to improve
	source_lines := os.read_lines(vt.file) or { []string{} }
	for lnumber, line in source_lines {
		vt.vet_line(source_lines, line, lnumber)
	}
}

// vet_line vets the contents of `line` from `vet.file`.
fn (mut vet Vet) vet_line(lines []string, line string, lnumber int) {
	// Vet public functions
	if line.starts_with('pub fn') ||
		(line.starts_with('fn ') && !(line.starts_with('fn C.') || line.starts_with('fn main'))) {
		// Scan function declarations for missing documentation
		is_pub_fn := line.starts_with('pub fn')
		if lnumber > 0 {
			collect_tags := fn (line string) []string {
				mut cleaned := line.all_before('/')
				cleaned = cleaned.replace_each(['[', '', ']', '', ' ', ''])
				return cleaned.split(',')
			}
			ident_fn_name := fn (line string) string {
				mut fn_idx := line.index(' fn ') or { return '' }
				mut skip := false
				mut p_count := 0
				mut fn_name := ''
				for i := fn_idx + 4; i < line.len; i++ {
					char := line[i]
					if !skip && char == `(` {
						p_count++
						skip = true
						continue
					} else if skip && char == `)` {
						skip = false
						continue
					} else if char == ` ` {
						continue
					} else if char.is_letter() {
						// fn_name += char.str()
						fn_name = line[i..].all_before('(')
						break
					}
					if p_count > 1 {
						break
					}
				}
				return fn_name
			}
			mut line_above := lines[lnumber - 1]
			mut tags := []string{}
			if !line_above.starts_with('//') {
				mut grab := true
				for j := lnumber - 1; j >= 0; j-- {
					prev_line := lines[j]
					if prev_line.contains('}') { // We've looked back to the above scope, stop here
						break
					} else if prev_line.starts_with('[') {
						tags << collect_tags(prev_line)
						continue
					} else if prev_line.starts_with('//') { // Single-line comment
						grab = false
						break
					}
				}
				if grab {
					clean_line := line.all_before_last('{').trim(' ')
					if is_pub_fn {
						vet.warn('Function documentation seems to be missing for "$clean_line".',
							lnumber, .doc)
					}
				}
			} else {
				fn_name := ident_fn_name(line)
				mut grab := true
				for j := lnumber - 1; j >= 0; j-- {
					prev_line := lines[j]
					if prev_line.contains('}') { // We've looked back to the above scope, stop here
						break
					} else if prev_line.starts_with('// $fn_name ') {
						grab = false
						break
					} else if prev_line.starts_with('[') {
						tags << collect_tags(prev_line)
						continue
					} else if prev_line.starts_with('//') { // Single-line comment
						continue
					}
				}
				if grab {
					clean_line := line.all_before_last('{').trim(' ')
					if is_pub_fn {
						vet.warn('A function name is missing from the documentation of "$clean_line".',
							lnumber, .doc)
					}
				}
			}
		}
	}
}
