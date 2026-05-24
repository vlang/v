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
// What's caught beyond the return-borrow case:
//   * `outer.field = &local`     — field-store escape (root must be non-local)
//   * `outer_arr << &local`      — array-push escape (root must be non-local)
//   * `return fn [&local] () {}` — closure-capture escape via return
//   * `outer.field = fn [&local] () {}` / `outer_arr << fn [&local] () {}`
//                                — closure-capture escape via store/push
//   * `return passthrough(&local)` — inter-procedural escape via a fn that
//     returns one of its parameters (`fn passthrough(x &^a T) &^a T { return x }`).
//     Also fires for the field-store and array-push forms.
//
// For closure-capture escape, `[&x]` and `[mut x]` are treated as borrows
// (matching the ownership-checker convention); `[x]` is by-value and never
// triggers the check.
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
	// Pre-pass: populate `escape_passthrough_fns` so the main walk can
	// resolve calls to opt-in fns that return one of their parameters
	// (`fn id[^a](x &^a T) &^a T { return x }`). Must precede the walk so a
	// fn defined later in the same file is still recognised at its call site.
	c.escape_prescan_passthrough_fns(files)
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				c.escape_check_fn_decl(stmt as ast.FnDecl)
			}
		}
	}
}

// escape_prescan_passthrough_fns walks opt-in (`[^a]`) fn decls and records
// every parameter index that is returned directly. Used by the call-site
// checker to flag `return passthrough(&local)` and friends. Non-opt-in fns
// are skipped — the rule only matters when the signature claims a lifetime
// relationship between the param and the return.
fn (mut c Checker) escape_prescan_passthrough_fns(files []ast.File) {
	for file in files {
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				decl := stmt as ast.FnDecl
				if !escape_decl_opts_in(decl) {
					continue
				}
				indices := escape_collect_returned_param_indices(decl)
				if indices.len > 0 {
					c.escape_passthrough_fns[decl.name] = indices
				}
			}
		}
	}
}

// escape_collect_returned_param_indices returns every parameter index that
// appears as the operand of a `return` in the fn body. Handles plain
// `return param` and `return &param` (the `&` is a no-op for our purposes —
// either way the borrow flows from caller-supplied storage back to the
// caller, so a `&local` arg still escapes). Multiple branches that return
// different params (`if cond { return a } else { return b }`) yield both
// indices.
fn escape_collect_returned_param_indices(decl ast.FnDecl) []int {
	mut param_names := map[string]int{}
	for i, param in decl.typ.params {
		if param.name.len > 0 {
			param_names[param.name] = i
		}
	}
	if param_names.len == 0 {
		return []int{}
	}
	mut out := map[int]bool{}
	escape_collect_returned_param_indices_stmts(decl.stmts, param_names, mut out)
	mut result := []int{}
	for idx, _ in out {
		result << idx
	}
	return result
}

fn escape_collect_returned_param_indices_stmts(stmts []ast.Stmt, param_names map[string]int, mut out map[int]bool) {
	for stmt in stmts {
		escape_collect_returned_param_indices_stmt(stmt, param_names, mut out)
	}
}

fn escape_collect_returned_param_indices_stmt(stmt ast.Stmt, param_names map[string]int, mut out map[int]bool) {
	match stmt {
		ast.ReturnStmt {
			for expr in stmt.exprs {
				if name := escape_returned_param_name(expr) {
					if name in param_names {
						out[param_names[name]] = true
					}
				}
			}
		}
		ast.BlockStmt {
			escape_collect_returned_param_indices_stmts(stmt.stmts, param_names, mut out)
		}
		ast.DeferStmt {
			escape_collect_returned_param_indices_stmts(stmt.stmts, param_names, mut out)
		}
		ast.ForStmt {
			escape_collect_returned_param_indices_stmts(stmt.stmts, param_names, mut out)
		}
		ast.ExprStmt {
			escape_collect_returned_param_indices_expr(stmt.expr, param_names, mut out)
		}
		else {}
	}
}

fn escape_collect_returned_param_indices_expr(expr ast.Expr, param_names map[string]int, mut out map[int]bool) {
	match expr {
		ast.IfExpr {
			escape_collect_returned_param_indices_stmts(expr.stmts, param_names, mut out)
			escape_collect_returned_param_indices_expr(expr.else_expr, param_names, mut out)
		}
		ast.MatchExpr {
			for br in expr.branches {
				escape_collect_returned_param_indices_stmts(br.stmts, param_names, mut out)
			}
		}
		ast.LockExpr {
			escape_collect_returned_param_indices_stmts(expr.stmts, param_names, mut out)
		}
		else {}
	}
}

// escape_returned_param_name unwraps `param` and `&param` (one `&` layer) and
// returns the Ident name. Used by the pass-through prescan to recognise both
// `return x` and `return &x` as pass-through shapes.
fn escape_returned_param_name(expr ast.Expr) ?string {
	if expr is ast.Ident {
		return expr.name
	}
	if expr is ast.PrefixExpr {
		if expr.op == .amp && expr.expr is ast.Ident {
			return (expr.expr as ast.Ident).name
		}
	}
	return none
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
	// Closure-borrow taint: any local bound to a closure literal that
	// captures another body-local by `&`/`mut`. Tracked separately so the
	// later walk can recognise `return cb` / `outer.f = cb` / `out << cb`
	// when `cb` carries the borrow indirectly.
	mut closure_borrow_locals := map[string]bool{}
	escape_collect_closure_borrow_locals_stmts(decl.stmts, locals, mut closure_borrow_locals)
	c.escape_walk_stmts(decl.stmts, locals, closure_borrow_locals, decl.name)
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

// escape_collect_closure_borrow_locals_stmts walks decl-assign statements and
// records every local bound to a closure literal that captures another
// body-local by `&`/`mut`. Walked nested blocks the same way as
// `escape_collect_locals_*`. Only `:=` is considered: later re-assignment is
// handled directly by the field-store path on the same RHS shape.
fn escape_collect_closure_borrow_locals_stmts(stmts []ast.Stmt, locals map[string]bool, mut out map[string]bool) {
	for stmt in stmts {
		escape_collect_closure_borrow_locals_stmt(stmt, locals, mut out)
	}
}

fn escape_collect_closure_borrow_locals_stmt(stmt ast.Stmt, locals map[string]bool, mut out map[string]bool) {
	match stmt {
		ast.AssignStmt {
			if stmt.op == .decl_assign {
				for i, lhs in stmt.lhs {
					if i >= stmt.rhs.len {
						continue
					}
					name := escape_lhs_decl_name(lhs)
					if name.len == 0 {
						continue
					}
					rhs := stmt.rhs[i]
					if rhs is ast.FnLiteral {
						if _ := escape_closure_borrow_reason(rhs, locals) {
							out[name] = true
						}
					}
				}
			}
		}
		ast.BlockStmt {
			escape_collect_closure_borrow_locals_stmts(stmt.stmts, locals, mut out)
		}
		ast.DeferStmt {
			escape_collect_closure_borrow_locals_stmts(stmt.stmts, locals, mut out)
		}
		ast.ForStmt {
			escape_collect_closure_borrow_locals_stmts(stmt.stmts, locals, mut out)
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

fn (mut c Checker) escape_walk_stmts(stmts []ast.Stmt, locals map[string]bool, closure_borrow_locals map[string]bool, fn_name string) {
	for stmt in stmts {
		c.escape_walk_stmt(stmt, locals, closure_borrow_locals, fn_name)
	}
}

fn (mut c Checker) escape_walk_stmt(stmt ast.Stmt, locals map[string]bool, closure_borrow_locals map[string]bool, fn_name string) {
	match stmt {
		ast.ReturnStmt {
			for expr in stmt.exprs {
				c.escape_check_returned_expr(expr, locals, closure_borrow_locals, fn_name)
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
					c.escape_check_field_store(lhs, stmt.rhs[i], locals, closure_borrow_locals,
						fn_name, stmt.pos)
				}
			}
		}
		ast.BlockStmt {
			c.escape_walk_stmts(stmt.stmts, locals, closure_borrow_locals, fn_name)
		}
		ast.DeferStmt {
			c.escape_walk_stmts(stmt.stmts, locals, closure_borrow_locals, fn_name)
		}
		ast.ForStmt {
			c.escape_walk_stmts(stmt.stmts, locals, closure_borrow_locals, fn_name)
		}
		ast.ExprStmt {
			c.escape_walk_expr(stmt.expr, locals, closure_borrow_locals, fn_name)
		}
		else {}
	}
}

fn (mut c Checker) escape_walk_expr(expr ast.Expr, locals map[string]bool, closure_borrow_locals map[string]bool, fn_name string) {
	match expr {
		ast.IfExpr {
			c.escape_walk_stmts(expr.stmts, locals, closure_borrow_locals, fn_name)
			c.escape_walk_expr(expr.else_expr, locals, closure_borrow_locals, fn_name)
		}
		ast.MatchExpr {
			for br in expr.branches {
				c.escape_walk_stmts(br.stmts, locals, closure_borrow_locals, fn_name)
			}
		}
		ast.LockExpr {
			c.escape_walk_stmts(expr.stmts, locals, closure_borrow_locals, fn_name)
		}
		ast.InfixExpr {
			// `arr << x` is an InfixExpr with op == .left_shift. When the
			// target array's root is non-local and `x` is `&local_thing`,
			// we'd be stashing a dangling reference into outer storage.
			if expr.op == .left_shift {
				c.escape_check_array_push(expr, locals, closure_borrow_locals, fn_name)
			}
		}
		else {}
	}
}

// escape_check_returned_expr fires when a return value either takes the
// address of short-lived storage (`return &local`), returns an inline closure
// that captured one (`return fn [&local] () {}`), returns a local already
// bound to such a closure (`cb := fn [&local] () {}; return cb`), or returns
// the result of a pass-through fn whose relevant arg is `&local`
// (`return passthrough(&local)`). All four make a body-local outlive the
// function via the return value.
fn (mut c Checker) escape_check_returned_expr(expr ast.Expr, locals map[string]bool, closure_borrow_locals map[string]bool, fn_name string) {
	if expr is ast.PrefixExpr {
		if expr.op == .amp {
			if reason := escape_borrow_reason(expr.expr, locals) {
				c.escape_error_returns_local(reason, fn_name, expr.pos)
			}
		}
		return
	}
	if expr is ast.FnLiteral {
		if reason := escape_closure_borrow_reason(expr, locals) {
			c.escape_error_returns_local(reason, fn_name, expr.pos)
		}
		return
	}
	if expr is ast.Ident {
		if expr.name in closure_borrow_locals {
			c.escape_error_returns_local('closure `${expr.name}` capturing local by reference',
				fn_name, expr.pos)
		}
		return
	}
	if call := escape_as_call(expr) {
		if reason := c.escape_call_passthrough_reason(call, locals, closure_borrow_locals) {
			c.escape_error_returns_local(reason, fn_name, call.pos)
		}
	}
}

// escape_as_call normalises a `CallExpr` or single-arg `CallOrCastExpr` into
// a uniform `CallExpr` view. The parser produces `CallOrCastExpr` for the
// `name(arg)` shape since it can't disambiguate call vs cast without type
// info; we don't care about that distinction here — both forms carry an lhs
// and an args list, and the inter-procedural rule applies identically.
fn escape_as_call(expr ast.Expr) ?ast.CallExpr {
	if expr is ast.CallExpr {
		return expr
	}
	if expr is ast.CallOrCastExpr {
		return ast.CallExpr{
			lhs:  expr.lhs
			args: [expr.expr]
			pos:  expr.pos
		}
	}
	return none
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
// path with a non-local root and rhs holds a borrow of body-local storage —
// `&local`, `fn [&local] () {}`, or a local Ident bound to such a closure.
// Storing such a value into outer state lets the borrow outlive the body-local.
fn (mut c Checker) escape_check_field_store(lhs ast.Expr, rhs ast.Expr, locals map[string]bool, closure_borrow_locals map[string]bool, fn_name string, pos token.Pos) {
	root := escape_lhs_root(lhs) or { return }
	if root in locals {
		// The LHS itself dies with the body; the borrow doesn't escape.
		return
	}
	reason := c.escape_rhs_borrow_reason(rhs, locals, closure_borrow_locals) or { return }
	path := escape_render_path(lhs)
	c.escape_error_stores_local('stored into `${path}`', reason, fn_name, pos)
}

// escape_check_array_push fires for `arr << &local`, the inline-closure form
// `arr << fn [&local] () {}`, and the bound-local form `arr << cb`. Same root
// rule as the field-store case: the array's root must outlive the borrow.
fn (mut c Checker) escape_check_array_push(expr ast.InfixExpr, locals map[string]bool, closure_borrow_locals map[string]bool, fn_name string) {
	root := escape_lhs_root(expr.lhs) or { return }
	if root in locals {
		return
	}
	reason := c.escape_rhs_borrow_reason(expr.rhs, locals, closure_borrow_locals) or { return }
	path := escape_render_path(expr.lhs)
	c.escape_error_stores_local('pushed into `${path}`', reason, fn_name, expr.pos)
}

// escape_rhs_borrow_reason classifies a RHS expression and returns a short
// user-facing reason if it produces a borrow of body-local storage; otherwise
// none. Used by both the field-store and array-push checks to share the
// same set of recognised borrow shapes. Lives on Checker so it can consult
// the pass-through fn registry for the inter-procedural case.
fn (c &Checker) escape_rhs_borrow_reason(rhs ast.Expr, locals map[string]bool, closure_borrow_locals map[string]bool) ?string {
	if rhs is ast.PrefixExpr {
		if rhs.op == .amp {
			return escape_borrow_reason(rhs.expr, locals)
		}
		return none
	}
	if rhs is ast.FnLiteral {
		return escape_closure_borrow_reason(rhs, locals)
	}
	if rhs is ast.Ident {
		if rhs.name in closure_borrow_locals {
			return 'closure `${rhs.name}` capturing local by reference'
		}
	}
	if call := escape_as_call(rhs) {
		return c.escape_call_passthrough_reason(call, locals, closure_borrow_locals)
	}
	return none
}

// escape_call_passthrough_reason returns a reason if `call` resolves to a
// pre-scanned pass-through fn and the argument at one of the returned-param
// indices is a borrow of body-local storage (`&local`, a closure literal
// capturing a local by reference, or a local bound to such a closure).
//
// `<unknown fn>` calls — those we can't statically resolve to a name (method
// calls on dynamic receivers, field-call indirection, ...) — are ignored. The
// rule is intentionally conservative: only flag what we can prove.
fn (c &Checker) escape_call_passthrough_reason(call ast.CallExpr, locals map[string]bool, closure_borrow_locals map[string]bool) ?string {
	fn_name := escape_call_fn_short_name(call) or { return none }
	if fn_name !in c.escape_passthrough_fns {
		return none
	}
	indices := c.escape_passthrough_fns[fn_name]
	for idx in indices {
		if idx >= call.args.len {
			continue
		}
		arg := call.args[idx]
		if inner := escape_arg_borrow_reason(arg, locals, closure_borrow_locals) {
			return '${inner}, passed through `${fn_name}`'
		}
	}
	return none
}

// escape_arg_borrow_reason recognises the borrow shapes legal as a
// pass-through argument: `&local`, `&local.f`, `&local[i]`, an inline closure
// capturing a local by reference, or a local already bound to such a closure.
// Same set the store / push / return checks already understand — kept as a
// dedicated helper so the call-site rule stays in lockstep.
fn escape_arg_borrow_reason(arg ast.Expr, locals map[string]bool, closure_borrow_locals map[string]bool) ?string {
	if arg is ast.PrefixExpr {
		if arg.op == .amp {
			return escape_borrow_reason(arg.expr, locals)
		}
	}
	if arg is ast.FnLiteral {
		return escape_closure_borrow_reason(arg, locals)
	}
	if arg is ast.Ident {
		if arg.name in closure_borrow_locals {
			return 'closure `${arg.name}` capturing local by reference'
		}
	}
	return none
}

// escape_call_fn_short_name extracts the short fn name from a CallExpr's
// callee. Returns none for shapes we can't resolve (field-call on a value,
// dynamically-computed fn, ...). Matches the keying used by the pass-through
// prescan, which stores under `decl.name`.
fn escape_call_fn_short_name(call ast.CallExpr) ?string {
	if call.lhs is ast.Ident {
		return (call.lhs as ast.Ident).name
	}
	if call.lhs is ast.SelectorExpr {
		return (call.lhs as ast.SelectorExpr).rhs.name
	}
	return none
}

// escape_closure_borrow_reason returns a reason if `lit` is a closure whose
// capture list contains at least one borrow (`&x` or `mut x`) of a body-local.
// `[x]` is by-value capture and is never a borrow, so it doesn't escape even
// if the closure does.
fn escape_closure_borrow_reason(lit ast.FnLiteral, locals map[string]bool) ?string {
	for cv in lit.captured_vars {
		match cv {
			ast.PrefixExpr {
				if cv.op == .amp && cv.expr is ast.Ident {
					name := (cv.expr as ast.Ident).name
					if name in locals {
						return 'local variable `${name}` captured by reference into closure'
					}
				}
			}
			ast.ModifierExpr {
				// `[mut x]` — mutable borrow into the closure (matches the
				// ownership-checker convention of treating mut-capture as a
				// borrow rather than a copy).
				if cv.kind == .key_mut && cv.expr is ast.Ident {
					name := (cv.expr as ast.Ident).name
					if name in locals {
						return 'local variable `${name}` captured by mutable reference into closure'
					}
				}
			}
			else {}
		}
	}
	return none
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
