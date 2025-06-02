// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module main

import os
import os.cmdline
import v.pref
import v.parser
import v.ast
import v.help
import term
import arrays

@[heap]
struct Vet {
mut:
	opt            Options
	errors         shared []VetError
	warns          shared []VetError
	notices        shared []VetError
	file           string
	filtered_lines FilteredLines
	analyze        VetAnalyze
}

struct Options {
	is_force            bool
	is_werror           bool
	is_verbose          bool
	show_warnings       bool
	use_color           bool
	doc_private_fns_too bool
	fn_sizing           bool
	repeated_code       bool
	fn_inlining         bool
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
			is_werror:           '-W' in vet_options
			is_verbose:          '-verbose' in vet_options || '-v' in vet_options
			show_warnings:       '-hide-warnings' !in vet_options && '-w' !in vet_options
			doc_private_fns_too: '-p' in vet_options
			use_color:           '-color' in vet_options
				|| (term_colors && '-nocolor' !in vet_options)
			repeated_code:       '-r' in vet_options
			fn_sizing:           '-F' in vet_options
			fn_inlining:         '-I' in vet_options
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
	vt.vet_code_analyze()
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
		rlock vt.errors {
			filtered_out := arrays.distinct(vt.errors.map(it.file_path))
			eprintln('Note: You can run `v fmt -w ${filtered_out.join(' ')}` to fix these errors automatically')
		}
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
	file := parser.parse_file(path, mut table, .parse_comments, prefs)
	vt.stmts(file.stmts)
	source_lines := os.read_lines(vt.file) or { []string{} }
	for ln, line in source_lines {
		vt.vet_line(source_lines, line, ln)
	}
}

// vet_line vets the contents of `line` from `vet.file`.
fn (mut vt Vet) vet_line(lines []string, line string, lnumber int) {
	if line == '' {
		return
	}
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

fn collect_tags(line string) []string {
	mut cleaned := line.all_before('/')
	cleaned = cleaned.replace_each(clean_seq)
	return cleaned.split(',')
}

fn ident_fn_name(line string) string {
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
	mut line_above := lines[lnumber - 1] or { return }
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

fn (mut vt Vet) stmts(stmts []ast.Stmt) {
	for stmt in stmts {
		vt.stmt(stmt)
	}
}

fn (mut vt Vet) stmt(stmt ast.Stmt) {
	match stmt {
		ast.ConstDecl {
			vt.const_decl(stmt)
		}
		ast.ExprStmt {
			vt.expr(stmt.expr)
		}
		ast.Return {
			vt.exprs(stmt.exprs)
		}
		ast.AssertStmt {
			vt.expr(stmt.expr)
		}
		ast.AssignStmt {
			vt.exprs(stmt.left)
			vt.exprs(stmt.right)
			vt.analyze.stmt(&vt, stmt)
		}
		ast.FnDecl {
			old_fn_decl := vt.analyze.cur_fn
			vt.analyze.cur_fn = stmt
			vt.stmts(stmt.stmts)
			if vt.opt.fn_sizing {
				vt.analyze.long_or_empty_fns(mut vt, stmt)
			}
			if vt.opt.fn_inlining {
				vt.analyze.potential_non_inlined(mut vt, stmt)
			}
			vt.analyze.cur_fn = old_fn_decl
		}
		ast.StructDecl {
			vt.exprs(stmt.fields.map(it.default_expr))
		}
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
		ast.StringLiteral {
			vt.filtered_lines.assigns(expr.pos)
			vt.analyze.expr(&vt, expr)
		}
		ast.StringInterLiteral {
			vt.filtered_lines.assigns(expr.pos)
			vt.analyze.expr(&vt, expr)
		}
		ast.ArrayInit {
			vt.filtered_lines.assigns(expr.pos)
			vt.expr(expr.len_expr)
			vt.expr(expr.cap_expr)
			vt.expr(expr.init_expr)
			vt.exprs(expr.exprs)
		}
		ast.InfixExpr {
			vt.vet_in_condition(expr)
			vt.vet_empty_str(expr)
			vt.expr(expr.left)
			vt.expr(expr.right)
			vt.analyze.expr(&vt, expr)
		}
		ast.ParExpr {
			vt.expr(expr.expr)
		}
		ast.CallExpr {
			vt.expr(expr.left)
			vt.exprs(expr.args.map(it.expr))
			vt.analyze.expr(&vt, expr)
		}
		ast.MatchExpr {
			vt.expr(expr.cond)
			for b in expr.branches {
				vt.exprs(b.exprs)
				vt.stmts(b.stmts)
			}
		}
		ast.IfExpr {
			for b in expr.branches {
				vt.expr(b.cond)
				vt.stmts(b.stmts)
			}
		}
		ast.SelectorExpr {
			vt.analyze.expr(&vt, expr)
		}
		ast.IndexExpr {
			vt.analyze.expr(&vt, expr)
		}
		ast.AsCast {
			vt.analyze.expr(&vt, expr)
			vt.expr(expr.expr)
		}
		ast.UnsafeExpr {
			vt.expr(expr.expr)
		}
		ast.CastExpr {
			vt.expr(expr.expr)
		}
		ast.StructInit {
			vt.expr(expr.update_expr)
			vt.exprs(expr.init_fields.map(it.expr))
		}
		ast.DumpExpr {
			vt.expr(expr.expr)
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

fn (mut vt Vet) vet_empty_str(expr ast.InfixExpr) {
	if expr.left is ast.SelectorExpr && expr.right is ast.IntegerLiteral {
		operand := (expr.left as ast.SelectorExpr) // TODO: remove as-casts when multiple conds can be smart-casted.
		if operand.expr is ast.Ident && operand.field_name == 'len'
			&& operand.expr.info.typ == ast.string_type_idx {
			if expr.op != .lt && expr.right.val == '0' {
				// Case: `var.len > 0`, `var.len == 0`, `var.len != 0`
				op := if expr.op == .gt { '!=' } else { expr.op.str() }
				vt.notice("Use `${operand.expr.name} ${op} ''` instead of `${operand.expr.name}.len ${expr.op} 0`",
					expr.pos.line_nr, .unknown)
			} else if expr.op == .lt && expr.right.val == '1' {
				// Case: `var.len < 1`
				vt.notice("Use `${operand.expr.name} == ''` instead of `${operand.expr.name}.len ${expr.op} 1`",
					expr.pos.line_nr, .unknown)
			}
		}
	} else if expr.left is ast.IntegerLiteral && expr.right is ast.SelectorExpr {
		operand := expr.right
		if operand.expr is ast.Ident && operand.expr.info.typ == ast.string_type_idx
			&& operand.field_name == 'len' {
			if expr.op != .gt && (expr.left as ast.IntegerLiteral).val == '0' {
				// Case: `0 < var.len`, `0 == var.len`, `0 != var.len`
				op := if expr.op == .lt { '!=' } else { expr.op.str() }
				vt.notice("Use `'' ${op} ${operand.expr.name}` instead of `0 ${expr.op} ${operand.expr.name}.len`",
					expr.pos.line_nr, .unknown)
			} else if expr.op == .gt && (expr.left as ast.IntegerLiteral).val == '1' {
				// Case: `1 > var.len`
				vt.notice("Use `'' == ${operand.expr.name}` instead of `1 ${expr.op} ${operand.expr.name}.len`",
					expr.pos.line_nr, .unknown)
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

fn (mut vt Vet) vet_in_condition(expr ast.InfixExpr) {
	if expr.right is ast.ArrayInit && expr.right.exprs.len == 1 && expr.op in [.key_in, .not_in] {
		left := expr.left.str()
		right := expr.right.exprs[0].str()
		eq := if expr.op == .key_in { '==' } else { '!=' }
		vt.error('Use `${left} ${eq} ${right}` instead of `${left} ${expr.op} [${right}]`',
			expr.pos.line_nr, .vfmt)
	}
}
