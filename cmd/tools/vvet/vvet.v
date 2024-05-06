// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module main

import os
import os.cmdline
import v.pref
import v.parser
import v.token
import v.ast
import v.help
import term

struct Vet {
mut:
	opt            Options
	errors         []VetError
	warns          []VetError
	notices        []VetError
	file           string
	filtered_lines FilteredLines
}

struct Options {
	is_force            bool
	is_werror           bool
	is_verbose          bool
	show_warnings       bool
	use_color           bool
	doc_private_fns_too bool
mut:
	is_vfmt_off bool
}

const term_colors = term.can_show_color_on_stderr()
const clean_seq = ['[', '', ']', '', ' ', '']
const exclude_dirs = ['test', 'slow_test', 'testdata']

fn main() {
	vet_options := cmdline.options_after(os.args, ['vet'])
	mut vt := Vet{
		opt: Options{
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
	if paths.len == 0 || '-help' in vet_options || '--help' in vet_options {
		help.print_and_exit('vet')
	}
	for path in paths {
		if !os.exists(path) {
			eprintln('File/folder ${path} does not exist')
			continue
		}
		if os.is_file(path) {
			vt.vet_file(path)
		}
		if os.is_dir(path) {
			vt.vprintln("vetting folder: '${path}' ...")
			overwrite_exclude := exclude_dirs.any(path.contains(it))
			os.walk(path, fn [mut vt, overwrite_exclude] (p string) {
				if p.ends_with('.v') || p.ends_with('.vv') {
					if !overwrite_exclude {
						for d in exclude_dirs {
							if p.contains(d) {
								return
							}
						}
					}
					vt.vet_file(p)
				}
			})
		}
	}
	vfmt_err_count := vt.errors.filter(it.fix == .vfmt).len
	for n in vt.notices {
		eprintln(vt.e2string(n))
	}
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
	vt.file = path
	mut prefs := pref.new_preferences()
	prefs.is_vet = true
	prefs.is_vsh = path.ends_with('.vsh')
	mut table := ast.new_table()
	vt.vprintln("vetting file '${path}'...")
	file := parser.parse_vet_file(path, mut table, prefs)
	vt.stmts(file.stmts)
	source_lines := os.read_lines(vt.file) or { []string{} }
	for ln, line in source_lines {
		vt.vet_line(source_lines, line, ln)
	}
}

// vet_line vets the contents of `line` from `vet.file`.
fn (mut vt Vet) vet_line(lines []string, line string, lnumber int) {
	vt.vet_fn_documentation(lines, line, lnumber)
	vt.vet_space_usage(line, lnumber)
}

fn (mut vt Vet) vet_space_usage(line string, lnumber int) {
	if line.starts_with('// vfmt off') {
		vt.opt.is_vfmt_off = true
	} else if line.starts_with('// vfmt on') {
		vt.opt.is_vfmt_off = false
	}
	if vt.opt.is_vfmt_off {
		return
	}
	if lnumber !in vt.filtered_lines[.space_indent] {
		if line.starts_with(' ') {
			vt.error('Looks like you are using spaces for indentation.', lnumber, .vfmt)
		}
	}
	if lnumber !in vt.filtered_lines[.trailing_space] {
		if line.ends_with(' ') {
			vt.error('Looks like you have trailing whitespace.', lnumber, .unknown)
		}
	}
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
			cleaned = cleaned.replace_each(clean_seq)
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
				function_name_with_generic_parameters := tokens[0].all_before('(')
				return function_name_with_generic_parameters.all_before('[')
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
				vt.warn('Function documentation seems to be missing for "${clean_line}".',
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

				if prev_line.starts_with('//') {
					if prev_line.starts_with('// ${fn_name} ') {
						grab = false
						break
					} else if prev_line.starts_with('// ${fn_name}')
						&& !prev_prev_line.starts_with('//') {
						grab = false
						clean_line := line.all_before_last('{').trim(' ')
						vt.warn('The documentation for "${clean_line}" seems incomplete.',
							lnumber, .doc)
						break
					}

					continue
				}

				if prev_line.contains('}') { // We've looked back to the above scope, stop here
					break
				} else if prev_line.starts_with('[') {
					tags << collect_tags(prev_line)
					continue
				}
			}
			if grab {
				clean_line := line.all_before_last('{').trim(' ')
				vt.warn('A function name is missing from the documentation of "${clean_line}".',
					lnumber, .doc)
			}
		}
	}
}

fn (mut vt Vet) stmts(stmts []ast.Stmt) {
	for stmt in stmts {
		vt.stmt(stmt)
	}
}

fn (mut vt Vet) stmt(stmt ast.Stmt) {
	match stmt {
		ast.ConstDecl { vt.const_decl(stmt) }
		ast.ExprStmt { vt.expr(stmt.expr) }
		ast.Return { vt.exprs(stmt.exprs) }
		ast.AssertStmt { vt.expr(stmt.expr) }
		ast.AssignStmt { vt.exprs(stmt.right) }
		ast.FnDecl { vt.stmts(stmt.stmts) }
		else {}
	}
}

fn (mut vt Vet) exprs(exprs []ast.Expr) {
	for expr in exprs {
		vt.expr(expr)
	}
}

fn (mut vt Vet) expr(expr ast.Expr) {
	match expr {
		ast.Comment {
			vt.filtered_lines.comments(expr.is_multi, expr.pos)
		}
		ast.StringLiteral, ast.StringInterLiteral {
			vt.filtered_lines.assigns(expr.pos)
		}
		ast.ArrayInit {
			vt.filtered_lines.assigns(expr.pos)
		}
		ast.InfixExpr {
			vt.vet_in_condition(expr)
			vt.expr(expr.right)
		}
		ast.CallExpr {
			vt.expr(expr.left)
			vt.exprs(expr.args.map(it.expr))
		}
		ast.MatchExpr {
			for b in expr.branches {
				vt.stmts(b.stmts)
			}
		}
		ast.IfExpr {
			for b in expr.branches {
				vt.expr(b.cond)
				vt.stmts(b.stmts)
			}
		}
		else {}
	}
}

fn (mut vt Vet) const_decl(stmt ast.ConstDecl) {
	for field in stmt.fields {
		if field.expr is ast.ArrayInit && !field.expr.is_fixed {
			vt.notice('Use a fixed array instead of a dynamic one', field.expr.pos.line_nr,
				.unknown)
		}
		vt.expr(field.expr)
	}
}

fn (vt &Vet) vprintln(s string) {
	if !vt.opt.is_verbose {
		return
	}
	println(s)
}

fn (vt &Vet) e2string(err VetError) string {
	mut kind := '${err.kind}:'
	mut location := '${err.file_path}:${err.pos.line_nr}:'
	if vt.opt.use_color {
		kind = match err.kind {
			.warning { term.magenta(kind) }
			.error { term.red(kind) }
			.notice { term.yellow(kind) }
		}
		kind = term.bold(kind)
		location = term.bold(location)
	}
	return '${location} ${kind} ${err.message}'
}

fn (mut vt Vet) vet_in_condition(expr ast.InfixExpr) {
	if expr.right is ast.ArrayInit && expr.right.exprs.len == 1 && expr.op in [.key_in, .not_in] {
		left := expr.left.str()
		right := expr.right.exprs[0].str()
		eq := if expr.op == .key_in { '==' } else { '!=' }
		vt.error('Use `${left} ${eq} ${right}` instead of `${left} ${expr.op} [${right}]`',
			expr.pos.line_nr, .vfmt)
	}
}

fn (mut vt Vet) error(msg string, line int, fix FixKind) {
	pos := token.Pos{
		line_nr: line + 1
	}
	vt.errors << VetError{
		message: msg
		file_path: vt.file
		pos: pos
		kind: .error
		fix: fix
		typ: .default
	}
}

fn (mut vt Vet) warn(msg string, line int, fix FixKind) {
	pos := token.Pos{
		line_nr: line + 1
	}
	mut w := VetError{
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

fn (mut vt Vet) notice(msg string, line int, fix FixKind) {
	pos := token.Pos{
		line_nr: line + 1
	}
	vt.notices << VetError{
		message: msg
		file_path: vt.file
		pos: pos
		kind: .notice
		fix: fix
		typ: .default
	}
}
