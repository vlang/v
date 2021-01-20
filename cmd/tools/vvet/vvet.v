// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module main

import os
import os.cmdline
import v.vet
import v.pref
import v.parser
import v.table
import v.token

struct Vet {
	opt Options
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

const vet_options = cmdline.options_after(os.args, ['vet'])

const is_force = '-force' in vet_options

const is_verbose = '-verbose' in vet_options || '-v' in vet_options

const show_warnings = '-hide-warnings' !in vet_options

fn main() {
	opt := Options{
		is_verbose: is_verbose
	}
	mut vet := Vet{
		opt: opt
	}
	mut paths := cmdline.only_non_options(vet_options)
	vtmp := os.getenv('VTMP')
	if vtmp != '' {
		// `v test-cleancode` passes also `-o tmpfolder` as well as all options in VFLAGS
		paths = paths.filter(!it.starts_with(vtmp))
	}
	//
	for path in paths {
		if !os.exists(path) {
			eprintln('File/folder $path does not exist')
			continue
		}
		if path.ends_with('.v') || path.ends_with('.vv') {
			if path.contains('cmd/tools/vvet/tests/') {
				if is_force || paths.len == 1 {
					vet.vet_file(path, true)
					continue
				} else {
					// The .vv files in that folder, are regression tests
					// for `v vet` itself and thus are known to fail in
					// a predictable way. They are run 1 by 1 by vet_test.v.
					// They *should be skipped*, when run by more general
					// invocations like for example `v vet cmd/tools`
					eprintln("skipping vvet regression file: '$path' ...")
					continue
				}
			}
		}
		if os.is_dir(path) {
			vet.vprintln("vetting folder: '$path' ...")
			vfiles := os.walk_ext(path, '.v')
			vvfiles := os.walk_ext(path, '.vv')
			mut files := []string{}
			files << vfiles
			files << vvfiles
			for file in files {
				if !is_force && file.ends_with('.vv') && file.contains('cmd/tools/vvet/tests/') {
					continue
				}
				vet.vet_file(file, false)
			}
		}
	}
	//
	warnings := vet.errors.filter(it.kind == .warning)
	errors := vet.errors.filter(it.kind == .error)
	errors_vfmt := vet.errors.filter(it.kind == .error && it.fix == .vfmt)
	if show_warnings {
		for err in warnings {
			eprintln('$err.file_path:$err.pos.line_nr: warning: $err.message')
		}
	}
	for err in errors {
		eprintln('$err.file_path:$err.pos.line_nr: error: $err.message')
	}
	if errors_vfmt.len > 0 {
		eprintln('NB: You can run `v fmt -w file.v` to fix these automatically')
	}
	if errors.len > 0 {
		exit(1)
	}
}

fn (mut v Vet) error(msg string, line int, fix vet.FixKind) {
	pos := token.Position{
		line_nr: line + 1
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
		line_nr: line + 1
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
fn (mut vt Vet) vet_file(path string, is_regression_test bool) {
	if path.contains('/tests/') && !is_regression_test {
		// skip all /tests/ files, since usually their content is not
		// important enough to be documented/vetted, and they may even
		// contain intentionally invalid code.
		eprintln("skipping test file: '$path' ...")
		return
	}
	vt.file = path
	mut prefs := pref.new_preferences()
	prefs.is_vet = true
	table := table.new_table()
	vt.vprintln("vetting file '$path'...")
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
		(line.starts_with('fn ') && !(line.starts_with('fn C.') || line.starts_with('fn main')))
	{
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
