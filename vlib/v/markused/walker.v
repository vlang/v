// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module markused

// Walk the entire program starting at fn main and marks used (called) functions.
// Unused functions can be safely skipped by the backends to save CPU time and space.
import v.ast
import v.table

pub struct Walker {
pub mut:
	table       &table.Table
	used_fns    map[string]bool // used_fns['println'] == true
	used_consts map[string]bool // used_consts['os.args'] == true
	n_maps      int
	n_asserts   int
mut:
	files      []ast.File
	all_fns    map[string]ast.FnDecl
	all_consts map[string]ast.ConstField
}

pub fn (mut w Walker) mark_fn_as_used(fkey string) {
	$if trace_skip_unused_marked ? {
		eprintln('    fn > |$fkey|')
	}
	w.used_fns[fkey] = true
}

pub fn (mut w Walker) mark_const_as_used(ckey string) {
	$if trace_skip_unused_marked ? {
		eprintln('    const > |$ckey|')
	}
	w.used_consts[ckey] = true
	cfield := w.all_consts[ckey] or { return }
	w.expr(cfield.expr)
}

pub fn (mut w Walker) mark_root_fns(all_fn_root_names []string) {
	for fn_name in all_fn_root_names {
		if fn_name !in w.used_fns {
			$if trace_skip_unused_roots ? {
				println('>>>> $fn_name uses: ')
			}
			w.fn_decl(mut w.all_fns[fn_name])
		}
	}
}

pub fn (mut w Walker) stmt(node ast.Stmt) {
	match mut node {
		ast.AssertStmt {
			w.expr(node.expr)
			w.n_asserts++
		}
		ast.AssignStmt {
			w.exprs(node.left)
			w.exprs(node.right)
		}
		ast.Block {
			w.stmts(node.stmts)
		}
		ast.CompFor {
			w.stmts(node.stmts)
		}
		ast.ConstDecl {
			w.const_fields(node.fields)
		}
		ast.ExprStmt {
			w.expr(node.expr)
		}
		ast.FnDecl {
			w.fn_decl(mut node)
		}
		ast.ForCStmt {
			w.expr(node.cond)
			w.stmt(node.inc)
			w.stmts(node.stmts)
		}
		ast.ForInStmt {
			w.expr(node.cond)
			w.expr(node.high)
			w.stmts(node.stmts)
		}
		ast.ForStmt {
			w.expr(node.cond)
			w.stmts(node.stmts)
		}
		ast.GoStmt {
			w.expr(node.call_expr)
		}
		ast.Return {
			w.exprs(node.exprs)
		}
		ast.SqlStmt {
			w.expr(node.db_expr)
			w.expr(node.where_expr)
			w.exprs(node.update_exprs)
		}
		ast.StructDecl {
			w.struct_fields(node.fields)
		}
		ast.DeferStmt {
			w.stmts(node.stmts)
		}
		ast.GlobalDecl {
			for gf in node.fields {
				if gf.has_expr {
					w.expr(gf.expr)
				}
			}
		}
		ast.BranchStmt {}
		ast.EnumDecl {}
		ast.GotoLabel {}
		ast.GotoStmt {}
		ast.HashStmt {}
		ast.Import {}
		ast.InterfaceDecl {}
		ast.Module {}
		ast.TypeDecl {}
	}
}

fn (mut w Walker) defer_stmts(stmts []ast.DeferStmt) {
	for stmt in stmts {
		w.stmts(stmt.stmts)
	}
}

fn (mut w Walker) stmts(stmts []ast.Stmt) {
	for stmt in stmts {
		w.stmt(stmt)
	}
}

fn (mut w Walker) exprs(exprs []ast.Expr) {
	for expr in exprs {
		w.expr(expr)
	}
}

fn (mut w Walker) expr(node ast.Expr) {
	match mut node {
		ast.AnonFn {
			w.fn_decl(mut node.decl)
		}
		ast.Assoc {
			w.exprs(node.exprs)
		}
		ast.ArrayInit {
			w.expr(node.len_expr)
			w.expr(node.cap_expr)
			w.expr(node.default_expr)
			w.exprs(node.exprs)
		}
		ast.ArrayDecompose {
			w.expr(node.expr)
		}
		ast.CallExpr {
			w.call_expr(mut node)
		}
		ast.CastExpr {
			w.expr(node.expr)
			w.expr(node.arg)
		}
		ast.ChanInit {
			w.expr(node.cap_expr)
		}
		ast.ConcatExpr {
			w.exprs(node.vals)
		}
		ast.ComptimeSelector {
			w.expr(node.left)
			w.expr(node.field_expr)
		}
		ast.ComptimeCall {
			w.expr(node.left)
			if node.is_vweb {
				w.stmts(node.vweb_tmpl.stmts)
			}
		}
		ast.DumpExpr {
			w.expr(node.expr)
		}
		ast.GoExpr {
			w.expr(node.go_stmt.call_expr)
		}
		ast.IndexExpr {
			w.expr(node.left)
			w.expr(node.index)
			w.or_block(node.or_expr)
		}
		ast.InfixExpr {
			w.expr(node.left)
			w.expr(node.right)
			w.or_block(node.or_block)
		}
		ast.IfGuardExpr {
			w.expr(node.expr)
		}
		ast.IfExpr {
			w.expr(node.left)
			for b in node.branches {
				w.expr(b.cond)
				w.stmts(b.stmts)
			}
		}
		ast.Ident {
			match node.kind {
				.constant {
					w.mark_const_as_used(node.name)
				}
				.function {
					w.fn_by_name(node.name)
				}
				else {
					// `.unresolved`, `.blank_ident`, `.variable`, `.global`, `.function`
					// println('>>> else, ast.Ident kind: $node.kind')
				}
			}
		}
		ast.Likely {
			w.expr(node.expr)
		}
		ast.MapInit {
			w.exprs(node.keys)
			w.exprs(node.vals)
			w.n_maps++
		}
		ast.MatchExpr {
			w.expr(node.cond)
			for b in node.branches {
				w.exprs(b.exprs)
				w.stmts(b.stmts)
			}
		}
		ast.None {
			w.mark_fn_as_used('opt_none2')
		}
		ast.ParExpr {
			w.expr(node.expr)
		}
		ast.PrefixExpr {
			w.expr(node.right)
		}
		ast.PostfixExpr {
			w.expr(node.expr)
		}
		ast.RangeExpr {
			if node.has_low {
				w.expr(node.low)
			}
			if node.has_high {
				w.expr(node.high)
			}
		}
		ast.SizeOf {
			w.expr(node.expr)
		}
		ast.StringInterLiteral {
			w.exprs(node.exprs)
		}
		ast.SelectorExpr {
			w.expr(node.expr)
		}
		ast.SqlExpr {
			w.expr(node.db_expr)
			w.expr(node.offset_expr)
			w.expr(node.order_expr)
			w.expr(node.limit_expr)
			w.expr(node.where_expr)
		}
		ast.StructInit {
			sym := w.table.get_type_symbol(node.typ)
			if sym.kind == .struct_ {
				info := sym.info as table.Struct
				for ifield in info.fields {
					if ifield.has_default_expr {
						defex := ast.fe2ex(ifield.default_expr)
						w.expr(defex)
					}
				}
			}
			if node.has_update_expr {
				w.expr(node.update_expr)
			}
			for sif in node.fields {
				w.expr(sif.expr)
			}
			for sie in node.embeds {
				w.expr(sie.expr)
			}
		}
		ast.TypeOf {
			w.expr(node.expr)
		}
		///
		ast.AsCast {
			w.expr(node.expr)
		}
		ast.AtExpr {}
		ast.BoolLiteral {}
		ast.FloatLiteral {}
		ast.CharLiteral {}
		ast.IntegerLiteral {}
		ast.StringLiteral {}
		ast.CTempVar {
			w.expr(node.orig)
		}
		ast.Comment {}
		ast.EnumVal {}
		ast.LockExpr {
			w.stmts(node.stmts)
		}
		ast.OffsetOf {}
		ast.OrExpr {
			w.or_block(node)
		}
		ast.SelectExpr {
			for branch in node.branches {
				w.stmt(branch.stmt)
				w.stmts(branch.stmts)
			}
		}
		ast.Type {}
		ast.UnsafeExpr {
			w.expr(node.expr)
		}
	}
}

pub fn (mut w Walker) fn_decl(mut node ast.FnDecl) {
	if node.language == .c {
		return
	}
	fkey := if node.is_method { '${int(node.receiver.typ)}.$node.name' } else { node.name }
	if w.used_fns[fkey] {
		// This function is already known to be called, meaning it has been processed already.
		// Save CPU time and do nothing.
		return
	}
	w.mark_fn_as_used(fkey)
	w.stmts(node.stmts)
	w.defer_stmts(node.defer_stmts)
}

pub fn (mut w Walker) call_expr(mut node ast.CallExpr) {
	for arg in node.args {
		w.expr(arg.expr)
	}
	if node.language == .c {
		return
	}
	w.expr(node.left)
	w.or_block(node.or_block)
	//
	fn_name := if node.is_method { node.receiver_type.str() + '.' + node.name } else { node.name }
	if w.used_fns[fn_name] {
		return
	}
	w.mark_fn_as_used(fn_name)
	stmt := w.all_fns[fn_name] or { return }
	if stmt.name == node.name {
		if !node.is_method || (node.receiver_type == stmt.receiver.typ) {
			w.stmts(stmt.stmts)
		}
	}
}

pub fn (mut w Walker) fn_by_name(fn_name string) {
	if w.used_fns[fn_name] {
		return
	}
	stmt := w.all_fns[fn_name] or { return }
	w.mark_fn_as_used(fn_name)
	w.stmts(stmt.stmts)
}

pub fn (mut w Walker) struct_fields(sfields []ast.StructField) {
	for sf in sfields {
		if sf.has_default_expr {
			w.expr(sf.default_expr)
		}
	}
}

pub fn (mut w Walker) const_fields(cfields []ast.ConstField) {
	for cf in cfields {
		w.expr(cf.expr)
	}
}

pub fn (mut w Walker) or_block(node ast.OrExpr) {
	if node.kind == .block {
		w.stmts(node.stmts)
	}
}
