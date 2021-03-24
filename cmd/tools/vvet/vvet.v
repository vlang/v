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
	warns []vet.Error
	file   string
}

struct Options {
	is_force bool
	is_werror bool
	is_verbose bool
	show_warnings bool
}

fn main() {
	vet_options := cmdline.options_after(os.args, ['vet'])
	mut vt := Vet{
		opt: Options{
			is_force: '-force' in vet_options
			is_werror: '-W' in vet_options
			is_verbose: '-verbose' in vet_options || '-v' in vet_options
			show_warnings: '-hide-warnings' !in vet_options
		}
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
				if vt.opt.is_force || paths.len == 1 {
					vt.vet_file(path, true)
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
		if os.is_file(path) {
			vt.vet_file(path, false)
		}
		if os.is_dir(path) {
			vt.vprintln("vetting folder: '$path' ...")
			vfiles := os.walk_ext(path, '.v')
			vvfiles := os.walk_ext(path, '.vv')
			mut files := []string{}
			files << vfiles
			files << vvfiles
			for file in files {
				if !vt.opt.is_force && file.ends_with('.vv') && file.contains('cmd/tools/vvet/tests/') {
					continue
				}
				vt.vet_file(file, false)
			}
		}
	}
	vfmt_err_count := vt.errors.filter(it.fix == .vfmt).len
	if vt.opt.show_warnings {
		for w in vt.warns {
			eprintln('$w.file_path:$w.pos.line_nr: warning: $w.message')
		}
	}
	for err in vt.errors {
		eprintln('$err.file_path:$err.pos.line_nr: error: $err.message')
	}
	if vfmt_err_count > 0 {
		eprintln('NB: You can run `v fmt -w file.v` to fix these automatically')
	}
	if vt.errors.len > 0 || (vt.opt.is_werror && vt.warns.len > 0) {
		exit(1)
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
fn (mut vt Vet) vet_line(lines []string, line string, lnumber int) {
	// Vet public functions
	if line.starts_with('pub fn') || (line.starts_with('fn ') && !(line.starts_with('fn C.')
		|| line.starts_with('fn main'))) {
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
				if line.len < fn_idx + 5 {
					return ''
				}
				mut tokens := line[fn_idx + 4..].split(' ')
				// Skip struct identifier
				if tokens.first().starts_with('(') {
					fn_idx = line.index(')') or { return '' }
					tokens = line[fn_idx..].split(' ')
					if tokens.len > 1 {
						tokens = [tokens[1]]
					}
				}
				if tokens.len > 0 {
					return tokens[0].all_before('(')
				}
				return ''
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
						vt.warn('Function documentation seems to be missing for "$clean_line".',
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
					} else if prev_line.starts_with('// $fn_name') {
						grab = false
						if is_pub_fn {
							clean_line := line.all_before_last('{').trim(' ')
							vt.warn('The documentation for "$clean_line" seems incomplete.',
								lnumber, .doc)
						}
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
						vt.warn('A function name is missing from the documentation of "$clean_line".',
							lnumber, .doc)
					}
				}
			}
		}
	}
}

fn (vt &Vet) vprintln(s string) {
	if !vt.opt.is_verbose {
		return
	}
	println(s)
}

fn (mut vt Vet) error(msg string, line int, fix vet.FixKind) {
	pos := token.Position{
		line_nr: line + 1
	}
	vt.errors << vet.Error{
		message: msg
		file_path: vt.file
		pos: pos
		kind: .error
		fix: fix
	}
}

fn (mut vt Vet) warn(msg string, line int, fix vet.FixKind) {
	pos := token.Position{
		line_nr: line + 1
	}
	vt.warns << vet.Error{
		message: msg
		file_path: vt.file
		pos: pos
		kind: .warning
		fix: fix
	}
}
