// Phase 3 of the V2 lifetime/ownership system: body-level escape analysis.
//
// Phases 1 and 2 (checker_lifetimes.v) validated lifetime ANNOTATIONS at
// the signature level. This phase looks at function BODIES and catches the
// canonical dangling-reference bug:
//
//     fn make[^a]() &^a Bar {
//         x := Bar{...}
//         return &x        // ERROR: `x` dies at function exit
//     }
//
// What's caught:
//   * `return &local`           — address-take of a body-local binding
//   * `return &local.field`     — selector chain whose root is a local
//   * `return &local_arr[i]`    — index into a local container
//   * `return &Foo{...}`        — address-take of a transient struct literal
//
// What's intentionally NOT caught (yet):
//   * Escape via closures capturing local refs
//   * Inter-procedural escape (calls that hide a borrow)
//
// What's caught beyond the return-borrow case:
//   * `outer.field = &local`     — field-store escape (root must be non-local)
//   * `outer_arr << &local`      — array-push escape (root must be non-local)
//
// `return &param` is always SAFE — parameters outlive the call from the
// caller's perspective. Method receivers are treated the same as params.
//
// OPT-IN: escape analysis only fires on functions that declare at least
// one lifetime generic param (`[^a]`). Legacy V code where `&Foo{...}`
// means "heap-allocate via GC" remains unaffected — under V's GC model
// this pattern is safe, but a Rust-style lifetime-checked signature
// makes the dangling-reference guarantee explicit. Same opt-in rule as
// Phase 2 elision, for consistency.
//
// Like the other ownership/lifetime phases, this is gated on `-d ownership`
// and lives entirely in `*_ownership.v` / `*_lifetimes.v` / `*_escape.v`
// files, so plain V code is unaffected.
module types

import v2.ast
import v2.errors
import v2.token

fn (mut c Checker) escape_validate_files(files []ast.File) {
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				c.escape_check_fn_decl(stmt as ast.FnDecl)
			}
		}
	}
}

fn (mut c Checker) escape_check_fn_decl(decl ast.FnDecl) {
	if decl.stmts.len == 0 {
		return
	}
	// Opt-in: only check fns that have declared at least one lifetime
	// generic param (`[^a]`). Legacy V code remains unaffected — `&Foo{}`
	// there is a GC heap-allocation, not a stack-literal address.
	if !escape_decl_opts_in(decl) {
		return
	}
	mut locals := map[string]bool{}
	escape_collect_locals_stmts(decl.stmts, mut locals)
	c.escape_walk_stmts(decl.stmts, locals, decl.name)
}

// escape_decl_opts_in returns true if the fn declares at least one
// lifetime generic param. Mirrors the elision opt-in in
// `lifetime_validate_fn_decl`.
fn escape_decl_opts_in(decl ast.FnDecl) bool {
	for p in decl.typ.generic_params {
		if p is ast.LifetimeExpr {
			return true
		}
	}
	return false
}

// escape_collect_locals_stmts records names of every binding declared in
// `stmts` and any nested in-fn block (if/match/for/lock/...). Nested
// `FnDecl`s are *not* descended — they introduce their own scope.
fn escape_collect_locals_stmts(stmts []ast.Stmt, mut out map[string]bool) {
	for stmt in stmts {
		escape_collect_locals_stmt(stmt, mut out)
	}
}

fn escape_collect_locals_stmt(stmt ast.Stmt, mut out map[string]bool) {
	match stmt {
		ast.AssignStmt {
			if stmt.op == .decl_assign {
				for lhs in stmt.lhs {
					// `mut x := ...` parses the LHS as `ModifierExpr{Ident{x}}`,
					// so unwrap one layer before recording the binding.
					name := escape_lhs_decl_name(lhs)
					if name.len > 0 {
						out[name] = true
					}
				}
			}
		}
		ast.BlockStmt {
			escape_collect_locals_stmts(stmt.stmts, mut out)
		}
		ast.DeferStmt {
			escape_collect_locals_stmts(stmt.stmts, mut out)
		}
		ast.ForStmt {
			escape_collect_locals_stmt(stmt.init, mut out)
			escape_collect_locals_stmts(stmt.stmts, mut out)
		}
		ast.ForInStmt {
			if stmt.key is ast.Ident {
				out[(stmt.key as ast.Ident).name] = true
			}
			if stmt.value is ast.Ident {
				out[(stmt.value as ast.Ident).name] = true
			}
		}
		ast.ExprStmt {
			escape_collect_locals_expr(stmt.expr, mut out)
		}
		ast.ReturnStmt {
			for e in stmt.exprs {
				escape_collect_locals_expr(e, mut out)
			}
		}
		else {}
	}
}

fn escape_collect_locals_expr(expr ast.Expr, mut out map[string]bool) {
	match expr {
		ast.IfExpr {
			escape_collect_locals_stmts(expr.stmts, mut out)
			escape_collect_locals_expr(expr.else_expr, mut out)
		}
		ast.MatchExpr {
			for br in expr.branches {
				escape_collect_locals_stmts(br.stmts, mut out)
			}
		}
		ast.LockExpr {
			escape_collect_locals_stmts(expr.stmts, mut out)
		}
		ast.UnsafeExpr {
			// UnsafeExpr wraps a block; widen the local set so refs inside
			// `unsafe { ... }` are still caught.
			// (No public stmts field on UnsafeExpr in this AST — skip.)
		}
		else {}
	}
}

fn (mut c Checker) escape_walk_stmts(stmts []ast.Stmt, locals map[string]bool, fn_name string) {
	for stmt in stmts {
		c.escape_walk_stmt(stmt, locals, fn_name)
	}
}

fn (mut c Checker) escape_walk_stmt(stmt ast.Stmt, locals map[string]bool, fn_name string) {
	match stmt {
		ast.ReturnStmt {
			for expr in stmt.exprs {
				c.escape_check_returned_expr(expr, locals, fn_name)
			}
		}
		ast.AssignStmt {
			// Only catch plain `=` (re-assignment). `:=` declares a fresh
			// local — the RHS borrow can't outlive the new local that holds
			// it, so it can't escape. Compound `+=`/`-=`/... never store a
			// fresh reference into the LHS.
			if stmt.op == .assign {
				for i, lhs in stmt.lhs {
					if i >= stmt.rhs.len {
						continue
					}
					c.escape_check_field_store(lhs, stmt.rhs[i], locals, fn_name, stmt.pos)
				}
			}
		}
		ast.BlockStmt {
			c.escape_walk_stmts(stmt.stmts, locals, fn_name)
		}
		ast.DeferStmt {
			c.escape_walk_stmts(stmt.stmts, locals, fn_name)
		}
		ast.ForStmt {
			c.escape_walk_stmts(stmt.stmts, locals, fn_name)
		}
		ast.ExprStmt {
			c.escape_walk_expr(stmt.expr, locals, fn_name)
		}
		else {}
	}
}

fn (mut c Checker) escape_walk_expr(expr ast.Expr, locals map[string]bool, fn_name string) {
	match expr {
		ast.IfExpr {
			c.escape_walk_stmts(expr.stmts, locals, fn_name)
			c.escape_walk_expr(expr.else_expr, locals, fn_name)
		}
		ast.MatchExpr {
			for br in expr.branches {
				c.escape_walk_stmts(br.stmts, locals, fn_name)
			}
		}
		ast.LockExpr {
			c.escape_walk_stmts(expr.stmts, locals, fn_name)
		}
		ast.InfixExpr {
			// `arr << x` is an InfixExpr with op == .left_shift. When the
			// target array's root is non-local and `x` is `&local_thing`,
			// we'd be stashing a dangling reference into outer storage.
			if expr.op == .left_shift {
				c.escape_check_array_push(expr, locals, fn_name)
			}
		}
		else {}
	}
}

// escape_check_returned_expr fires when a return value is `& <something>`.
// The operand is classified by `escape_borrow_reason` — if it represents a
// short-lived storage location, we emit a dangling-reference diagnostic.
fn (mut c Checker) escape_check_returned_expr(expr ast.Expr, locals map[string]bool, fn_name string) {
	if expr !is ast.PrefixExpr {
		return
	}
	pe := expr as ast.PrefixExpr
	if pe.op != .amp {
		return
	}
	reason := escape_borrow_reason(pe.expr, locals) or { return }
	c.escape_error_returns_local(reason, fn_name, pe.pos)
}

// escape_borrow_reason returns a short, user-facing reason string if
// `operand` refers to storage that does not survive function return;
// none otherwise.
fn escape_borrow_reason(operand ast.Expr, locals map[string]bool) ?string {
	match operand {
		ast.Ident {
			if operand.name in locals {
				return 'local variable `${operand.name}`'
			}
		}
		ast.SelectorExpr {
			root := operand.leftmost()
			if root is ast.Ident {
				ri := root as ast.Ident
				if ri.name in locals {
					return 'local variable `${ri.name}` via field access'
				}
			}
		}
		ast.IndexExpr {
			base := operand.lhs
			if base is ast.Ident {
				bi := base as ast.Ident
				if bi.name in locals {
					return 'index into local `${bi.name}`'
				}
			}
		}
		ast.InitExpr {
			return 'transient struct literal'
		}
		ast.ArrayInitExpr {
			return 'transient array literal'
		}
		ast.MapInitExpr {
			return 'transient map literal'
		}
		else {}
	}

	return none
}

fn (mut c Checker) escape_error_returns_local(reason string, fn_name string, pos token.Pos) {
	if pos.is_valid() {
		file := c.file_set.file(pos)
		errors.error('cannot return reference to ${reason}: it does not survive `${fn_name}` returning', errors.details(file,
			file.position(pos), 2), .error, file.position(pos))
	} else {
		eprintln('error: cannot return reference to ${reason}: it does not survive `${fn_name}` returning')
	}
	eprintln('help: return the value by-move (drop the `&`), or have the caller pass storage in')
	exit(1)
}

// escape_check_field_store fires for `lhs = rhs` where lhs is a field/index
// path with a non-local root and rhs is `&<short-lived storage>`. Storing the
// reference into outer state lets it outlive the body-local it points at.
fn (mut c Checker) escape_check_field_store(lhs ast.Expr, rhs ast.Expr, locals map[string]bool, fn_name string, pos token.Pos) {
	root := escape_lhs_root(lhs) or { return }
	if root in locals {
		// The LHS itself dies with the body; the borrow doesn't escape.
		return
	}
	if rhs !is ast.PrefixExpr {
		return
	}
	pe := rhs as ast.PrefixExpr
	if pe.op != .amp {
		return
	}
	reason := escape_borrow_reason(pe.expr, locals) or { return }
	path := escape_render_path(lhs)
	c.escape_error_stores_local('stored into `${path}`', reason, fn_name, pos)
}

// escape_check_array_push fires for `arr << &local`. Same shape as the
// field-store case: the destination array's root must outlive the local.
fn (mut c Checker) escape_check_array_push(expr ast.InfixExpr, locals map[string]bool, fn_name string) {
	root := escape_lhs_root(expr.lhs) or { return }
	if root in locals {
		return
	}
	if expr.rhs !is ast.PrefixExpr {
		return
	}
	pe := expr.rhs as ast.PrefixExpr
	if pe.op != .amp {
		return
	}
	reason := escape_borrow_reason(pe.expr, locals) or { return }
	path := escape_render_path(expr.lhs)
	c.escape_error_stores_local('pushed into `${path}`', reason, fn_name, expr.pos)
}

// escape_lhs_root returns the name of the root identifier of a write target
// (`x`, `x.f`, `x[i]`, `x.f[i].g`, ...). Returns none for shapes we don't
// recognise (calls, dereferences, ...).
fn escape_lhs_root(lhs ast.Expr) ?string {
	match lhs {
		ast.Ident {
			return lhs.name
		}
		ast.SelectorExpr {
			root := lhs.leftmost()
			if root is ast.Ident {
				return (root as ast.Ident).name
			}
			return escape_lhs_root(root)
		}
		ast.IndexExpr {
			return escape_lhs_root(lhs.lhs)
		}
		else {
			return none
		}
	}
}

// escape_lhs_decl_name extracts the binding name from a `:=` LHS. Handles
// the `mut x` form (parsed as `ModifierExpr{Ident{x}}`) as well as the
// bare-Ident form. Returns '' for shapes we don't track (`_`, tuples, ...).
fn escape_lhs_decl_name(lhs ast.Expr) string {
	match lhs {
		ast.Ident {
			return lhs.name
		}
		ast.ModifierExpr {
			if lhs.expr is ast.Ident {
				return (lhs.expr as ast.Ident).name
			}
			return ''
		}
		else {
			return ''
		}
	}
}

// escape_render_path produces a short, user-readable rendering of a write
// target for diagnostics: `x.f`, `x[i]`, `x.f[i].g`. Falls back to '<expr>'
// for shapes we don't print.
fn escape_render_path(expr ast.Expr) string {
	match expr {
		ast.Ident {
			return expr.name
		}
		ast.SelectorExpr {
			return '${escape_render_path(expr.lhs)}.${expr.rhs.name}'
		}
		ast.IndexExpr {
			return '${escape_render_path(expr.lhs)}[...]'
		}
		else {
			return '<expr>'
		}
	}
}

fn (mut c Checker) escape_error_stores_local(action string, reason string, fn_name string, pos token.Pos) {
	if pos.is_valid() {
		file := c.file_set.file(pos)
		errors.error('cannot store reference to ${reason} ${action}: it does not survive `${fn_name}` returning', errors.details(file,
			file.position(pos), 2), .error, file.position(pos))
	} else {
		eprintln('error: cannot store reference to ${reason} ${action}: it does not survive `${fn_name}` returning')
	}
	eprintln('help: store the value by-move (drop the `&`), or restructure so the borrow is shorter-lived than the container')
	exit(1)
}
