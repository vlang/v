// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module main

import os
import os.cmdline
import v.vet
import v.pref
import v.parser
import v.token
import v.ast
import term

struct Vet {
	opt Options
mut:
	errors []vet.Error
	warns  []vet.Error
	file   string
}

struct Options {
	is_force            bool
	is_werror           bool
	is_verbose          bool
	show_warnings       bool
	use_color           bool
	doc_private_fns_too bool
}

const term_colors = term.can_show_color_on_stderr()

fn main() {
	vet_options := cmdline.options_after(os.args, ['vet'])
	mut vt := Vet{
		opt: Options{
			is_force: '-force' in vet_options
			is_werror: '-W' in vet_options
			is_verbose: '-verbose' in vet_options || '-v' in vet_options
			show_warnings: '-hide-warnings' !in vet_options && '-w' !in vet_options
			use_color: '-color' in vet_options || (term_colors && '-nocolor' !in vet_options)
			doc_private_fns_too: '-p' in vet_options
		}
	}
	mut paths := cmdline.only_non_options(vet_options)
	vtmp := os.getenv('VTMP')
	if vtmp != '' {
		// `v test-cleancode` passes also `-o tmpfolder` as well as all options in VFLAGS
		paths = paths.filter(!it.starts_with(vtmp))
	}
	for path in paths {
		if !os.exists(path) {
			eprintln('File/folder $path does not exist')
			continue
		}
		if os.is_file(path) {
			vt.vet_file(path)
		}
		if os.is_dir(path) {
			vt.vprintln("vetting folder: '$path' ...")
			vfiles := os.walk_ext(path, '.v')
			vvfiles := os.walk_ext(path, '.vv')
			mut files := []string{}
			files << vfiles
			files << vvfiles
			for file in files {
				vt.vet_file(file)
			}
		}
	}
	vfmt_err_count := vt.errors.filter(it.fix == .vfmt).len
	if vt.opt.show_warnings {
		for w in vt.warns {
			eprintln(vt.e2string(w))
		}
	}
	for err in vt.errors {
		eprintln(vt.e2string(err))
	}
	if vfmt_err_count > 0 {
		eprintln('Note: You can run `v fmt -w file.v` to fix these errors automatically')
	}
	if vt.errors.len > 0 {
		exit(1)
	}
}

// vet_file vets the file read from `path`.
fn (mut vt Vet) vet_file(path string) {
	if path.contains('/tests/') && !vt.opt.is_force {
		// skip all /tests/ files, since usually their content is not
		// important enough to be documented/vetted, and they may even
		// contain intentionally invalid code.
		vt.vprintln("skipping test file: '$path' ...")
		return
	}
	vt.file = path
	mut prefs := pref.new_preferences()
	prefs.is_vet = true
	prefs.is_vsh = path.ends_with('.vsh')
	table := ast.new_table()
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
	vt.vet_fn_documentation(lines, line, lnumber)
}

// vet_fn_documentation ensures that functions are documented
fn (mut vt Vet) vet_fn_documentation(lines []string, line string, lnumber int) {
	if line.starts_with('fn C.') {
		return
	}
	is_pub_fn := line.starts_with('pub fn ')
	is_fn := is_pub_fn || line.starts_with('fn ')
	if !is_fn {
		return
	}
	if line.starts_with('fn main') {
		return
	}
	if !(is_pub_fn || vt.opt.doc_private_fns_too) {
		return
	}
	// Scan function declarations for missing documentation
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
				vt.warn('Function documentation seems to be missing for "$clean_line".',
					lnumber, .doc)
			}
		} else {
			fn_name := ident_fn_name(line)
			mut grab := true
			for j := lnumber - 1; j >= 0; j-- {
				mut prev_prev_line := ''
				if j - 1 >= 0 {
					prev_prev_line = lines[j - 1]
				}
				prev_line := lines[j]
				if prev_line.contains('}') { // We've looked back to the above scope, stop here
					break
				} else if prev_line.starts_with('// $fn_name ') {
					grab = false
					break
				} else if prev_line.starts_with('// $fn_name') && !prev_prev_line.starts_with('//') {
					grab = false
					clean_line := line.all_before_last('{').trim(' ')
					vt.warn('The documentation for "$clean_line" seems incomplete.', lnumber,
						.doc)
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
				vt.warn('A function name is missing from the documentation of "$clean_line".',
					lnumber, .doc)
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

fn (vt &Vet) e2string(err vet.Error) string {
	mut kind := '$err.kind:'
	mut location := '$err.file_path:$err.pos.line_nr:'
	if vt.opt.use_color {
		kind = match err.kind {
			.warning { term.magenta(kind) }
			.error { term.red(kind) }
		}
		kind = term.bold(kind)
		location = term.bold(location)
	}
	return '$location $kind $err.message'
}

fn (mut vt Vet) error(msg string, line int, fix vet.FixKind) {
	pos := token.Pos{
		line_nr: line + 1
	}
	vt.errors << vet.Error{
		message: msg
		file_path: vt.file
		pos: pos
		kind: .error
		fix: fix
		typ: .default
	}
}

fn (mut vt Vet) warn(msg string, line int, fix vet.FixKind) {
	pos := token.Pos{
		line_nr: line + 1
	}
	mut w := vet.Error{
		message: msg
		file_path: vt.file
		pos: pos
		kind: .warning
		fix: fix
		typ: .default
	}
	if vt.opt.is_werror {
		w.kind = .error
		vt.errors << w
	} else {
		vt.warns << w
	}
}
