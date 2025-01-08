// Copyright (c) 2025 Felipe Pena. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module main

import v.ast
import v.token
import arrays

// cutoffs
const indexexpr_cutoff = 5
const infixexpr_cutoff = 5
const selectorexpr_cutoff = 10
const callexpr_cutoff = 10

struct VetAnalysis {
mut:
	repeated_expr_cutoff shared map[string]int // repeated code cutoff	
	repeated_expr        shared map[string]map[string]map[string][]token.Pos // repeated exprs in fn scope
	cur_fn               ast.FnDecl // current fn declaration
}

// add_repeated_code registers a repeated code occurrence
fn (mut vt Vet) add_repeated_code(cutoff int, expr string, pos token.Pos) {
	lock vt.analysis.repeated_expr {
		vt.analysis.repeated_expr[vt.analysis.cur_fn.name][expr][vt.file] << pos
	}
	lock vt.analysis.repeated_expr_cutoff {
		vt.analysis.repeated_expr_cutoff[expr] = cutoff
	}
}

// repeated_code checks for repeated code
fn (mut vt Vet) repeated_code(expr ast.Expr) {
	match expr {
		ast.InfixExpr {
			vt.add_repeated_code(infixexpr_cutoff, '${expr.left} ${expr.op} ${expr.right}',
				expr.pos)
		}
		ast.IndexExpr {
			vt.add_repeated_code(indexexpr_cutoff, '${expr.left}[${expr.index}]', expr.pos)
		}
		ast.SelectorExpr {
			// nested selectors
			if expr.expr is ast.SelectorExpr {
				vt.add_repeated_code(selectorexpr_cutoff, '${ast.Expr(expr.expr).str()}.${expr.field_name}',
					expr.pos)
			}
		}
		ast.CallExpr {
			if expr.is_static_method || expr.is_method {
				vt.add_repeated_code(callexpr_cutoff, '${expr.left}.${expr.name}(${expr.args.map(it.str()).join(', ')})',
					expr.pos)
			} else {
				vt.add_repeated_code(callexpr_cutoff, '${expr.name}(${expr.args.map(it.str()).join(', ')})',
					expr.pos)
			}
		}
		ast.AsCast {
			vt.add_repeated_code(10, ast.Expr(expr).str(), expr.pos)
		}
		else {}
	}
}

// long_or_empty_fns checks for long or empty functions
fn (mut vt Vet) long_or_empty_fns(fn_decl ast.FnDecl) {
	nr_lines := if fn_decl.stmts.len == 0 {
		0
	} else {
		fn_decl.stmts.last().pos.line_nr - fn_decl.pos.line_nr
	}
	if nr_lines > 300 {
		vt.notice('Long function - ${nr_lines} lines long.', fn_decl.pos.line_nr, .long_fns)
	} else if nr_lines == 0 {
		vt.notice('Empty function.', fn_decl.pos.line_nr, .empty_fn)
	}
}

// vet_fn_analysis reports repeated code by scope
fn (mut vt Vet) vet_repeated_code() {
	rlock vt.analysis.repeated_expr {
		for fn_name, ref_expr in vt.analysis.repeated_expr {
			scope_name := if fn_name == '' { 'global scope' } else { 'function scope (${fn_name})' }
			for expr, info in ref_expr {
				occurrences := arrays.sum(info.values().map(it.len)) or { 0 }
				if occurrences < vt.analysis.repeated_expr_cutoff[expr] {
					continue
				}
				for file, info_pos in info {
					for k, pos in info_pos {
						vt.notice_with_file(file, '${expr} occurs ${k + 1}/${occurrences} times in ${scope_name}.',
							pos.line_nr, .repeated_code)
					}
				}
			}
		}
	}
}

fn (mut vt Vet) vet_code_analysis() {
	if vt.opt.repeated_code {
		vt.vet_repeated_code()
	}
}
