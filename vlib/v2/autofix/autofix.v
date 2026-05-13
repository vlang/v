// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// Module autofix walks the parsed AST and rewrites source files to fix
// simple, mechanical mistakes (e.g. a variable that needs `mut` because
// it is later mutated). It is enabled by `v2 -autofix <file.v>`.
module autofix

import os
import v2.ast
import v2.token

// Fix represents a single edit to apply to a source file.
struct Fix {
	file_path string
	offset    int // file-relative byte offset where text is inserted
	insert    string
	note      string // human-readable summary
}

// DeclEntry tracks one `name :=` declaration site.
struct DeclEntry {
mut:
	name      string
	pos       token.Pos
	file_path string
	is_mut    bool // already has `mut` modifier
	mutated   bool // observed to be mutated later
}

// Scope is a simple lexical scope used to disambiguate shadowed variables.
struct Scope {
mut:
	parent &Scope = unsafe { nil }
	// name -> index into Autofixer.decls (latest decl visible in this scope)
	names map[string]int
}

@[heap]
struct Autofixer {
mut:
	file_set    &token.FileSet
	cur_file    string
	decls       []DeclEntry
	scope       &Scope = unsafe { nil }
	in_for_init bool // inside for-init: loop var is implicitly mut, skip recording
	files_fixed map[string]bool
}

// run scans the given files and, when fixes are found, rewrites the
// corresponding source files in place. Returns the number of edits applied.
pub fn run(files []ast.File, file_set &token.FileSet) int {
	mut a := &Autofixer{
		file_set: unsafe { file_set }
	}
	for f in files {
		a.scan_file(f)
	}
	fixes := a.collect_fixes()
	if fixes.len == 0 {
		return 0
	}
	apply_fixes(fixes)
	return fixes.len
}

fn (mut a Autofixer) push_scope() {
	a.scope = &Scope{
		parent: a.scope
	}
}

fn (mut a Autofixer) pop_scope() {
	if a.scope != unsafe { nil } {
		a.scope = a.scope.parent
	}
}

// record_decl registers a `name :=` declaration in the current scope.
fn (mut a Autofixer) record_decl(name string, pos token.Pos, is_mut bool) {
	if a.scope == unsafe { nil } {
		return
	}
	idx := a.decls.len
	a.decls << DeclEntry{
		name:      name
		pos:       pos
		file_path: a.cur_file
		is_mut:    is_mut
	}
	a.scope.names[name] = idx
}

// mark_mutated walks up the scope chain and marks the nearest matching
// declaration as mutated.
fn (mut a Autofixer) mark_mutated(name string) {
	mut s := a.scope
	for s != unsafe { nil } {
		if idx := s.names[name] {
			a.decls[idx].mutated = true
			return
		}
		s = s.parent
	}
}

fn (mut a Autofixer) scan_file(f ast.File) {
	a.cur_file = f.name
	a.push_scope()
	for stmt in f.stmts {
		a.walk_stmt(stmt)
	}
	a.pop_scope()
}

fn (mut a Autofixer) walk_stmts(stmts []ast.Stmt) {
	for stmt in stmts {
		a.walk_stmt(stmt)
	}
}

fn (mut a Autofixer) walk_stmt(stmt ast.Stmt) {
	match stmt {
		ast.AssignStmt {
			a.walk_assign_stmt(stmt)
		}
		ast.ExprStmt {
			a.walk_expr(stmt.expr)
		}
		ast.BlockStmt {
			a.push_scope()
			a.walk_stmts(stmt.stmts)
			a.pop_scope()
		}
		ast.ForStmt {
			a.push_scope()
			// for-init: V treats the loop variable as implicitly mut
			// (`for i := 0; i < n; i++` is valid without `mut`), so do not
			// suggest adding `mut` to a decl that appears in the init slot.
			a.in_for_init = true
			a.walk_stmt(stmt.init)
			a.in_for_init = false
			a.walk_expr(stmt.cond)
			a.walk_stmt(stmt.post)
			a.walk_stmts(stmt.stmts)
			a.pop_scope()
		}
		ast.ForInStmt {
			// ForInStmt is only ever the `init` of a ForStmt; ForStmt walks
			// the body itself. Just inspect the iterated expression here.
			a.walk_expr(stmt.expr)
		}
		ast.FnDecl {
			a.push_scope()
			a.walk_stmts(stmt.stmts)
			a.pop_scope()
		}
		ast.ReturnStmt {
			for ex in stmt.exprs {
				a.walk_expr(ex)
			}
		}
		ast.DeferStmt {
			a.push_scope()
			a.walk_stmts(stmt.stmts)
			a.pop_scope()
		}
		ast.AssertStmt {
			a.walk_expr(stmt.expr)
			a.walk_expr(stmt.extra)
		}
		ast.AsmStmt {}
		ast.ComptimeStmt {}
		ast.ConstDecl {}
		ast.Directive {}
		ast.EmptyStmt {}
		ast.EnumDecl {}
		ast.FlowControlStmt {}
		ast.GlobalDecl {}
		ast.ImportStmt {}
		ast.InterfaceDecl {}
		ast.LabelStmt {}
		ast.ModuleStmt {}
		ast.StructDecl {}
		ast.TypeDecl {}
		[]ast.Attribute {}
	}
}

fn (mut a Autofixer) walk_assign_stmt(stmt ast.AssignStmt) {
	if stmt.op == .decl_assign {
		// Walk the RHS first so its mutations register against the
		// already-visible (outer) declarations rather than the new ones.
		for rx in stmt.rhs {
			a.walk_expr(rx)
		}
		if a.in_for_init {
			// Loop var is implicitly mut; do not suggest adding `mut` here.
			return
		}
		for lx in stmt.lhs {
			a.try_record_decl_lhs(lx)
		}
		return
	}
	// Mutation: each LHS expression is being assigned to.
	for lx in stmt.lhs {
		a.mark_lhs_mutated(lx)
		a.walk_expr(lx)
	}
	for rx in stmt.rhs {
		a.walk_expr(rx)
	}
}

// try_record_decl_lhs records a declaration whose LHS is either a bare
// Ident (no `mut` modifier) or a ModifierExpr wrapping an Ident.
fn (mut a Autofixer) try_record_decl_lhs(lx ast.Expr) {
	match lx {
		ast.Ident {
			if lx.name == '_' {
				return
			}
			a.record_decl(lx.name, lx.pos, false)
		}
		ast.ModifierExpr {
			if lx.kind == .key_mut {
				if lx.expr is ast.Ident {
					inner := lx.expr as ast.Ident
					a.record_decl(inner.name, inner.pos, true)
				}
			}
		}
		else {}
	}
}

// mark_lhs_mutated handles the LHS of a non-decl assignment (`=`, `+=`, etc.).
fn (mut a Autofixer) mark_lhs_mutated(lx ast.Expr) {
	match lx {
		ast.Ident {
			a.mark_mutated(lx.name)
		}
		ast.IndexExpr {
			// `arr[i] = ...`: mutates the receiver array/map identifier.
			a.mark_lhs_mutated(lx.lhs)
		}
		ast.SelectorExpr {
			// `obj.field = ...`: mutates the receiver identifier.
			a.mark_lhs_mutated(lx.lhs)
		}
		ast.PrefixExpr {
			// `*p = ...`: not a mutation of `p` itself; skip.
		}
		else {}
	}
}

fn (mut a Autofixer) walk_expr(expr ast.Expr) {
	match expr {
		ast.PostfixExpr {
			if expr.op == .inc || expr.op == .dec {
				a.mark_lhs_mutated(expr.expr)
			}
			a.walk_expr(expr.expr)
		}
		ast.PrefixExpr {
			a.walk_expr(expr.expr)
		}
		ast.InfixExpr {
			a.walk_expr(expr.lhs)
			a.walk_expr(expr.rhs)
		}
		ast.IfExpr {
			a.walk_expr(expr.cond)
			a.push_scope()
			a.walk_stmts(expr.stmts)
			a.pop_scope()
			a.walk_expr(expr.else_expr)
		}
		ast.IfGuardExpr {
			a.walk_assign_stmt(expr.stmt)
		}
		ast.MatchExpr {
			a.walk_expr(expr.expr)
			for branch in expr.branches {
				for cond in branch.cond {
					a.walk_expr(cond)
				}
				a.push_scope()
				a.walk_stmts(branch.stmts)
				a.pop_scope()
			}
		}
		ast.CallExpr {
			a.walk_expr(expr.lhs)
			for arg in expr.args {
				// `mut arg` at the call site mutates the underlying ident.
				if arg is ast.ModifierExpr && arg.kind == .key_mut {
					a.mark_lhs_mutated(arg.expr)
				}
				a.walk_expr(arg)
			}
		}
		ast.CallOrCastExpr {
			a.walk_expr(expr.lhs)
			a.walk_expr(expr.expr)
		}
		ast.IndexExpr {
			a.walk_expr(expr.lhs)
			a.walk_expr(expr.expr)
		}
		ast.SelectorExpr {
			a.walk_expr(expr.lhs)
		}
		ast.ParenExpr {
			a.walk_expr(expr.expr)
		}
		ast.RangeExpr {
			a.walk_expr(expr.start)
			a.walk_expr(expr.end)
		}
		ast.ArrayInitExpr {
			a.walk_expr(expr.init)
			for ex in expr.exprs {
				a.walk_expr(ex)
			}
		}
		ast.MapInitExpr {
			for k in expr.keys {
				a.walk_expr(k)
			}
			for v in expr.vals {
				a.walk_expr(v)
			}
		}
		ast.InitExpr {
			for f in expr.fields {
				a.walk_expr(f)
			}
		}
		ast.FieldInit {
			a.walk_expr(expr.value)
		}
		ast.UnsafeExpr {
			a.push_scope()
			a.walk_stmts(expr.stmts)
			a.pop_scope()
		}
		ast.OrExpr {
			a.walk_expr(expr.expr)
			a.push_scope()
			a.walk_stmts(expr.stmts)
			a.pop_scope()
		}
		ast.LockExpr {
			for ex in expr.lock_exprs {
				a.walk_expr(ex)
			}
			for ex in expr.rlock_exprs {
				a.walk_expr(ex)
			}
			a.walk_stmts(expr.stmts)
		}
		ast.LambdaExpr {
			a.push_scope()
			a.walk_expr(expr.expr)
			a.pop_scope()
		}
		ast.FnLiteral {
			a.push_scope()
			a.walk_stmts(expr.stmts)
			a.pop_scope()
		}
		ast.AsCastExpr {
			a.walk_expr(expr.expr)
		}
		ast.CastExpr {
			a.walk_expr(expr.expr)
		}
		ast.ModifierExpr {
			a.walk_expr(expr.expr)
		}
		ast.StringInterLiteral {
			for inter in expr.inters {
				a.walk_expr(inter.expr)
			}
		}
		ast.Tuple {
			for ex in expr.exprs {
				a.walk_expr(ex)
			}
		}
		ast.ComptimeExpr {
			a.walk_expr(expr.expr)
		}
		ast.AssocExpr {
			a.walk_expr(expr.typ)
			for f in expr.fields {
				a.walk_expr(f)
			}
		}
		ast.GenericArgs {
			a.walk_expr(expr.lhs)
		}
		ast.GenericArgOrIndexExpr {
			a.walk_expr(expr.lhs)
			a.walk_expr(expr.expr)
		}
		ast.SelectExpr {
			a.walk_stmt(expr.stmt)
			a.walk_stmts(expr.stmts)
			a.walk_expr(expr.next)
		}
		ast.SqlExpr {}
		ast.Ident {}
		ast.BasicLiteral {}
		ast.StringLiteral {}
		ast.Keyword {}
		ast.KeywordOperator {
			for ex in expr.exprs {
				a.walk_expr(ex)
			}
		}
		ast.EmptyExpr {}
		ast.Type {}
	}
}

// collect_fixes converts mutated/non-mut declarations into Fix entries.
fn (a &Autofixer) collect_fixes() []Fix {
	mut out := []Fix{}
	for d in a.decls {
		if !d.mutated || d.is_mut {
			continue
		}
		mut fs := unsafe { a.file_set }
		file := fs.file(d.pos)
		rel_offset := d.pos.offset - file.base
		out << Fix{
			file_path: d.file_path
			offset:    rel_offset
			insert:    'mut '
			note:      '`${d.name}` is mutated; declare it with `mut`'
		}
	}
	return out
}

// apply_fixes groups fixes per file, sorts each group by descending offset
// (so earlier inserts do not shift later ones), and rewrites the file.
fn apply_fixes(fixes []Fix) {
	mut by_file := map[string][]Fix{}
	for fx in fixes {
		by_file[fx.file_path] << fx
	}
	for path, file_fixes in by_file {
		mut content := os.read_file(path) or {
			eprintln('autofix: cannot read ${path}: ${err}')
			continue
		}
		// Sort by offset descending so insertions do not invalidate earlier offsets.
		mut sorted := file_fixes.clone()
		sorted.sort(a.offset > b.offset)
		mut applied := 0
		for fx in sorted {
			if fx.offset < 0 || fx.offset > content.len {
				continue
			}
			content = content[..fx.offset] + fx.insert + content[fx.offset..]
			applied++
		}
		os.write_file(path, content) or {
			eprintln('autofix: cannot write ${path}: ${err}')
			continue
		}
		eprintln('autofix: applied ${applied} fix(es) to ${path}')
	}
}
