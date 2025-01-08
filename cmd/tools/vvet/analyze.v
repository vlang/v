// Copyright (c) 2025 Felipe Pena. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module main

import v.ast
import v.token
import arrays

// cutoffs
const indexexpr_cutoff = 10
const infixexpr_cutoff = 10
const selectorexpr_cutoff = 10
const callexpr_cutoff = 10
const stringinterliteral_cutoff = 10
const stringliteral_cutoff = 10
const ascast_cutoff = 10
const stringconcat_cutoff = 10

// minimum size for string literals
const stringliteral_min_size = 20

// long functions cutoff
const long_fns_cutoff = 300

struct VetAnalyze {
mut:
	repeated_expr_cutoff shared map[string]int // repeated code cutoff	
	repeated_expr        shared map[string]map[string]map[string][]token.Pos // repeated exprs in fn scope
	cur_fn               ast.FnDecl // current fn declaration
}

// stmt checks for repeated code in statements
fn (mut vt VetAnalyze) stmt(vet &Vet, stmt ast.Stmt) {
	match stmt {
		ast.AssignStmt {
			if stmt.op == .plus_assign {
				if stmt.right[0] in [ast.StringLiteral, ast.StringInterLiteral] {
					vt.save_expr(stringconcat_cutoff, '${stmt.left[0].str()} += ${stmt.right[0].str()}',
						vet.file, stmt.pos)
				}
			}
		}
		else {}
	}
}

// save_expr registers a repeated code occurrence
fn (mut vt VetAnalyze) save_expr(cutoff int, expr string, file string, pos token.Pos) {
	lock vt.repeated_expr {
		vt.repeated_expr[vt.cur_fn.name][expr][file] << pos
	}
	lock vt.repeated_expr_cutoff {
		vt.repeated_expr_cutoff[expr] = cutoff
	}
}

// exprs checks for repeated code in expressions
fn (mut vt VetAnalyze) exprs(vet &Vet, exprs []ast.Expr) {
	for expr in exprs {
		vt.expr(vet, expr)
	}
}

// expr checks for repeated code
fn (mut vt VetAnalyze) expr(vet &Vet, expr ast.Expr) {
	match expr {
		ast.InfixExpr {
			vt.save_expr(infixexpr_cutoff, '${expr.left} ${expr.op} ${expr.right}', vet.file,
				expr.pos)
		}
		ast.IndexExpr {
			vt.save_expr(indexexpr_cutoff, '${expr.left}[${expr.index}]', vet.file, expr.pos)
		}
		ast.SelectorExpr {
			// nested selectors
			if expr.expr !is ast.Ident {
				vt.save_expr(selectorexpr_cutoff, '${expr.expr.str()}.${expr.field_name}',
					vet.file, expr.pos)
			}
		}
		ast.CallExpr {
			if expr.is_static_method || expr.is_method {
				vt.save_expr(callexpr_cutoff, '${expr.left}.${expr.name}(${expr.args.map(it.str()).join(', ')})',
					vet.file, expr.pos)
			} else {
				vt.save_expr(callexpr_cutoff, '${expr.name}(${expr.args.map(it.str()).join(', ')})',
					vet.file, expr.pos)
			}
		}
		ast.AsCast {
			vt.save_expr(ascast_cutoff, ast.Expr(expr).str(), vet.file, expr.pos)
		}
		ast.StringLiteral {
			if expr.val.len > stringliteral_min_size {
				vt.save_expr(stringliteral_cutoff, ast.Expr(expr).str(), vet.file, expr.pos)
			}
		}
		ast.StringInterLiteral {
			vt.save_expr(stringinterliteral_cutoff, ast.Expr(expr).str(), vet.file, expr.pos)
		}
		else {}
	}
}

// long_or_empty_fns checks for long or empty functions
fn (mut vt VetAnalyze) long_or_empty_fns(mut vet Vet, fn_decl ast.FnDecl) {
	nr_lines := if fn_decl.stmts.len == 0 {
		0
	} else {
		fn_decl.stmts.last().pos.line_nr - fn_decl.pos.line_nr
	}
	if nr_lines > long_fns_cutoff {
		vet.notice('Long function - ${nr_lines} lines long.', fn_decl.pos.line_nr, .long_fns)
	} else if nr_lines == 0 {
		vet.notice('Empty function.', fn_decl.pos.line_nr, .empty_fn)
	}
}

// vet_fn_analysis reports repeated code by scope
fn (mut vt VetAnalyze) vet_repeated_code(mut vet Vet) {
	rlock vt.repeated_expr {
		for fn_name, ref_expr in vt.repeated_expr {
			scope_name := if fn_name == '' { 'global scope' } else { 'function scope (${fn_name})' }
			for expr, info in ref_expr {
				occurrences := arrays.sum(info.values().map(it.len)) or { 0 }
				if occurrences < vt.repeated_expr_cutoff[expr] {
					continue
				}
				for file, info_pos in info {
					for k, pos in info_pos {
						vet.notice_with_file(file, '${expr} occurs ${k + 1}/${occurrences} times in ${scope_name}.',
							pos.line_nr, .repeated_code)
					}
				}
			}
		}
	}
}

// vet_code_analyze performs code analysis
fn (mut vt Vet) vet_code_analyze() {
	if vt.opt.repeated_code {
		vt.analyze.vet_repeated_code(mut vt)
	}
}
