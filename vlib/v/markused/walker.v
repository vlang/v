// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module markused

// Walk the entire program starting at fn main and marks used (called) functions.
// Unused functions can be safely skipped by the backends to save CPU time and space.
import v.ast
import v.pref

pub struct Walker {
pub mut:
	table        &ast.Table = unsafe { nil }
	used_fns     map[string]bool // used_fns['println'] == true
	used_consts  map[string]bool // used_consts['os.args'] == true
	used_globals map[string]bool
	used_structs map[string]bool
	n_asserts    int
	pref         &pref.Preferences = unsafe { nil }
mut:
	files       []&ast.File
	all_fns     map[string]ast.FnDecl
	all_consts  map[string]ast.ConstField
	all_globals map[string]ast.GlobalField
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
	if w.used_consts[ckey] {
		return
	}
	w.used_consts[ckey] = true
	cfield := w.all_consts[ckey] or { return }
	w.expr(cfield.expr)
}

pub fn (mut w Walker) mark_global_as_used(ckey string) {
	$if trace_skip_unused_marked ? {
		eprintln('  global > |$ckey|')
	}
	if w.used_globals[ckey] {
		return
	}
	w.used_globals[ckey] = true
	gfield := w.all_globals[ckey] or { return }
	w.expr(gfield.expr)
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

pub fn (mut w Walker) mark_exported_fns() {
	for _, mut func in w.all_fns {
		if func.is_exported {
			w.fn_decl(mut func)
		}
	}
}

pub fn (mut w Walker) mark_markused_fns() {
	for _, mut func in w.all_fns {
		if func.is_markused {
			w.fn_decl(mut func)
		}
	}
}

pub fn (mut w Walker) mark_markused_consts() {
	for ckey, mut constfield in w.all_consts {
		if constfield.is_markused {
			w.mark_const_as_used(ckey)
		}
	}
}

pub fn (mut w Walker) mark_markused_globals() {
	for gkey, mut globalfield in w.all_globals {
		if globalfield.is_markused {
			w.mark_global_as_used(gkey)
		}
	}
}

pub fn (mut w Walker) stmt(node_ ast.Stmt) {
	mut node := unsafe { node_ }
	match mut node {
		ast.EmptyStmt {}
		ast.AsmStmt {
			w.asm_io(node.output)
			w.asm_io(node.input)
		}
		ast.AssertStmt {
			if node.is_used {
				w.n_asserts++
				w.expr(node.expr)
				if node.extra !is ast.EmptyExpr {
					w.expr(node.extra)
				}
			}
		}
		ast.AssignStmt {
			w.exprs(node.left)
			w.exprs(node.right)
		}
		ast.Block {
			w.stmts(node.stmts)
		}
		ast.ComptimeFor {
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
			if node.kind == .map {
				w.table.used_maps++
			}
			if node.kind == .struct_ {
				if node.cond_type == 0 {
					return
				}
				// the .next() method of the struct will be used for iteration:
				cond_type_sym := w.table.sym(node.cond_type)
				if next_fn := cond_type_sym.find_method('next') {
					w.fn_decl(mut &ast.FnDecl(next_fn.source_fn))
				}
			}
		}
		ast.ForStmt {
			w.expr(node.cond)
			w.stmts(node.stmts)
		}
		ast.Return {
			w.exprs(node.exprs)
		}
		ast.SqlStmt {
			w.expr(node.db_expr)
			for line in node.lines {
				w.expr(line.where_expr)
				w.exprs(line.update_exprs)
			}
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
		ast.NodeError {}
	}
}

fn (mut w Walker) asm_io(ios []ast.AsmIO) {
	for io in ios {
		w.expr(io.expr)
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

fn (mut w Walker) expr(node_ ast.Expr) {
	mut node := unsafe { node_ }
	match mut node {
		ast.EmptyExpr {
			// TODO make sure this doesn't happen
			// panic('Walker: EmptyExpr')
		}
		ast.ComptimeType {}
		ast.AnonFn {
			w.fn_decl(mut node.decl)
		}
		ast.ArrayInit {
			w.expr(node.len_expr)
			w.expr(node.cap_expr)
			w.expr(node.default_expr)
			w.exprs(node.exprs)
		}
		ast.Assoc {
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
			w.fn_by_name('eprint')
			w.fn_by_name('eprintln')
		}
		ast.GoExpr {
			w.expr(node.call_expr)
			if w.pref.os == .windows {
				w.fn_by_name('panic_lasterr')
				w.fn_by_name('winapi_lasterr_str')
			} else {
				w.fn_by_name('c_error_number_str')
				w.fn_by_name('panic_error_number')
			}
		}
		ast.IndexExpr {
			w.expr(node.left)
			w.expr(node.index)
			w.or_block(node.or_expr)
			if node.left_type == 0 {
				return
			}
			sym := w.table.final_sym(node.left_type)
			if sym.kind == .map {
				w.table.used_maps++
			}
		}
		ast.InfixExpr {
			w.expr(node.left)
			w.expr(node.right)
			w.or_block(node.or_block)
			if node.left_type == 0 {
				return
			}
			sym := w.table.sym(node.left_type)
			if sym.kind == .struct_ {
				if opmethod := sym.find_method(node.op.str()) {
					w.fn_decl(mut &ast.FnDecl(opmethod.source_fn))
				}
			}
			if node.right_type == 0 {
				return
			}
			right_sym := w.table.sym(node.right_type)
			if node.op in [.not_in, .key_in] && right_sym.kind == .map {
				w.table.used_maps++
			}
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
				.global {
					w.mark_global_as_used(node.name)
				}
				else {
					// `.unresolved`, `.blank_ident`, `.variable`, `.function`
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
			w.table.used_maps++
		}
		ast.MatchExpr {
			w.expr(node.cond)
			for b in node.branches {
				w.exprs(b.exprs)
				w.stmts(b.stmts)
			}
		}
		ast.None {}
		ast.Nil {}
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
		ast.SizeOf, ast.IsRefType {
			w.expr(node.expr)
		}
		ast.StringInterLiteral {
			w.exprs(node.exprs)
		}
		ast.SelectorExpr {
			w.expr(node.expr)
			if node.expr_type != 0 {
				if method := w.table.find_method(w.table.sym(node.expr_type), node.field_name) {
					w.fn_by_name(method.fkey())
				}
			}
		}
		ast.SqlExpr {
			w.expr(node.db_expr)
			w.expr(node.offset_expr)
			w.expr(node.order_expr)
			w.expr(node.limit_expr)
			w.expr(node.where_expr)
		}
		ast.StructInit {
			if node.typ == 0 {
				return
			}
			sym := w.table.sym(node.typ)
			if sym.kind == .struct_ {
				info := sym.info as ast.Struct
				w.a_struct_info(sym.name, info)
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
		ast.TypeNode {}
		ast.UnsafeExpr {
			w.expr(node.expr)
		}
		ast.NodeError {}
	}
}

pub fn (mut w Walker) a_struct_info(sname string, info ast.Struct) {
	if sname in w.used_structs {
		return
	}
	w.used_structs[sname] = true
	for ifield in info.fields {
		if ifield.has_default_expr {
			w.expr(ifield.default_expr)
		}
		if ifield.typ != 0 {
			fsym := w.table.sym(ifield.typ)
			if fsym.kind == .map {
				w.table.used_maps++
			}
			if fsym.kind == .struct_ {
				w.a_struct_info(fsym.name, fsym.struct_info())
			}
		}
	}
}

pub fn (mut w Walker) fn_decl(mut node ast.FnDecl) {
	if node.language == .c {
		return
	}
	fkey := node.fkey()
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
		if node.name in ['C.wyhash', 'C.wyhash64'] {
			w.table.used_maps++
		}
		return
	}
	w.expr(node.left)
	w.or_block(node.or_block)
	//
	fn_name := node.fkey()
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
