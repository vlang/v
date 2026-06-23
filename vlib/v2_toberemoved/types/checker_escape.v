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
//     Also fires for the field-store and array-push forms. The pre-pass
//     follows one or more levels of `:=` aliasing inside the callee, so
//     `fn f(x &^a T) &^a T { y := x; return y }` is recognised too.
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

fn (mut c Checker) escape_validate_files_from_flat(flat &ast.FlatAst) {
	c.escape_prescan_passthrough_fns_from_flat(flat)
	for ff in flat.files {
		stmts := ast.Cursor{
			flat: unsafe { flat }
			id:   ff.file_id
		}.list_at(2)
		for i in 0 .. stmts.len() {
			stmt := stmts.at(i)
			if stmt.kind() == .stmt_fn_decl {
				c.escape_check_fn_decl_cursor(stmt)
			}
		}
	}
}

fn (mut c Checker) escape_prescan_passthrough_fns_from_flat(flat &ast.FlatAst) {
	for ff in flat.files {
		stmts := ast.Cursor{
			flat: unsafe { flat }
			id:   ff.file_id
		}.list_at(2)
		for i in 0 .. stmts.len() {
			decl := stmts.at(i)
			if decl.kind() != .stmt_fn_decl || !escape_decl_opts_in_cursor(decl) {
				continue
			}
			indices := escape_collect_returned_param_indices_cursor(decl)
			if indices.len > 0 {
				c.escape_passthrough_fns[decl.name()] = indices
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
//
// Also recognises one level of local aliasing: `y := param; return y` and
// `y := &param; return y` (or `mut y := ...`) — the alias forwards the same
// borrow, so the call site must still be flagged. Transitive chains
// (`b := a; c := b; return c`) are followed because the alias table is
// populated in declaration order before the return scan.
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
	// Seed the alias table with the params themselves so `return param` and
	// `return alias_of_param` go through the same lookup path.
	mut name_to_idx := param_names.clone()
	escape_collect_param_aliases_stmts(decl.stmts, mut name_to_idx)
	mut out := map[int]bool{}
	escape_collect_returned_param_indices_stmts(decl.stmts, name_to_idx, mut out)
	mut result := []int{}
	for idx, _ in out {
		result << idx
	}
	return result
}

fn escape_collect_returned_param_indices_cursor(decl ast.Cursor) []int {
	typ := decl.edge(1)
	params := typ.list_at(1)
	mut param_names := map[string]int{}
	for i in 0 .. params.len() {
		param := params.at(i)
		name := param.name()
		if name.len > 0 {
			param_names[name] = i
		}
	}
	if param_names.len == 0 {
		return []int{}
	}
	mut name_to_idx := param_names.clone()
	body := decl.list_at(3)
	escape_collect_param_aliases_stmts_cursor(body, mut name_to_idx)
	mut out := map[int]bool{}
	escape_collect_returned_param_indices_stmts_cursor(body, name_to_idx, mut out)
	mut result := []int{}
	for idx, _ in out {
		result << idx
	}
	return result
}

// escape_collect_param_aliases_stmts walks `:=` statements in declaration
// order and forwards `name -> param_idx` whenever the RHS is a known
// param-aliased name (or a `&` of one). Nested blocks are descended too:
// inside the same fn body, an alias declared in any block can still feed a
// later `return alias` at any depth. Statements that aren't decl-assigns are
// ignored — re-assignment can change a binding's contents, but it can't
// retroactively make a non-aliased binding pass-through.
fn escape_collect_param_aliases_stmts(stmts []ast.Stmt, mut name_to_idx map[string]int) {
	for stmt in stmts {
		escape_collect_param_aliases_stmt(stmt, mut name_to_idx)
	}
}

fn escape_collect_param_aliases_stmt(stmt ast.Stmt, mut name_to_idx map[string]int) {
	match stmt {
		ast.AssignStmt {
			if stmt.op == .decl_assign {
				for i, lhs in stmt.lhs {
					if i >= stmt.rhs.len {
						continue
					}
					lhs_name := escape_lhs_decl_name(lhs)
					if lhs_name.len == 0 {
						continue
					}
					rhs_name := escape_returned_param_name(stmt.rhs[i]) or { continue }
					if idx := name_to_idx[rhs_name] {
						name_to_idx[lhs_name] = idx
					}
				}
			}
		}
		ast.BlockStmt {
			escape_collect_param_aliases_stmts(stmt.stmts, mut name_to_idx)
		}
		ast.DeferStmt {
			escape_collect_param_aliases_stmts(stmt.stmts, mut name_to_idx)
		}
		ast.ForStmt {
			escape_collect_param_aliases_stmts(stmt.stmts, mut name_to_idx)
		}
		else {}
	}
}

fn escape_collect_returned_param_indices_stmts(stmts []ast.Stmt, name_to_idx map[string]int, mut out map[int]bool) {
	for stmt in stmts {
		escape_collect_returned_param_indices_stmt(stmt, name_to_idx, mut out)
	}
}

fn escape_collect_returned_param_indices_stmt(stmt ast.Stmt, name_to_idx map[string]int, mut out map[int]bool) {
	match stmt {
		ast.ReturnStmt {
			for expr in stmt.exprs {
				if name := escape_returned_param_name(expr) {
					if name in name_to_idx {
						out[name_to_idx[name]] = true
					}
				}
			}
		}
		ast.BlockStmt {
			escape_collect_returned_param_indices_stmts(stmt.stmts, name_to_idx, mut out)
		}
		ast.DeferStmt {
			escape_collect_returned_param_indices_stmts(stmt.stmts, name_to_idx, mut out)
		}
		ast.ForStmt {
			escape_collect_returned_param_indices_stmts(stmt.stmts, name_to_idx, mut out)
		}
		ast.ExprStmt {
			escape_collect_returned_param_indices_expr(stmt.expr, name_to_idx, mut out)
		}
		else {}
	}
}

fn escape_collect_returned_param_indices_expr(expr ast.Expr, name_to_idx map[string]int, mut out map[int]bool) {
	match expr {
		ast.IfExpr {
			escape_collect_returned_param_indices_stmts(expr.stmts, name_to_idx, mut out)
			escape_collect_returned_param_indices_expr(expr.else_expr, name_to_idx, mut out)
		}
		ast.MatchExpr {
			for br in expr.branches {
				escape_collect_returned_param_indices_stmts(br.stmts, name_to_idx, mut out)
			}
		}
		ast.LockExpr {
			escape_collect_returned_param_indices_stmts(expr.stmts, name_to_idx, mut out)
		}
		else {}
	}
}

// escape_returned_param_name unwraps `name` and `&name` (one `&` layer) and
// returns the Ident name. Used by the pass-through prescan to recognise both
// `return x` and `return &x` as pass-through shapes, and the alias pass to
// chain `y := &x` through to the same lookup.
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

fn escape_returned_param_name_cursor(expr ast.Cursor) ?string {
	if !expr.is_valid() {
		return none
	}
	match expr.kind() {
		.expr_ident {
			return expr.name()
		}
		.expr_prefix {
			if escape_cursor_token(expr) == .amp {
				inner := expr.edge(0)
				if inner.is_valid() && inner.kind() == .expr_ident {
					return inner.name()
				}
			}
		}
		else {}
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

fn (mut c Checker) escape_check_fn_decl_cursor(decl ast.Cursor) {
	body := decl.list_at(3)
	if body.len() == 0 {
		return
	}
	if !escape_decl_opts_in_cursor(decl) {
		return
	}
	mut locals := map[string]bool{}
	escape_collect_locals_stmts_cursor(body, mut locals)
	mut closure_borrow_locals := map[string]bool{}
	escape_collect_closure_borrow_locals_stmts_cursor(body, locals, mut closure_borrow_locals)
	c.escape_walk_stmts_cursor(body, locals, closure_borrow_locals, decl.name())
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

fn escape_decl_opts_in_cursor(decl ast.Cursor) bool {
	generic_params := decl.edge(1).list_at(0)
	for i in 0 .. generic_params.len() {
		if generic_params.at(i).kind() == .expr_lifetime {
			return true
		}
	}
	return false
}

fn escape_cursor_token(c ast.Cursor) token.Token {
	return unsafe { token.Token(int(c.aux())) }
}

fn escape_collect_param_aliases_stmts_cursor(stmts ast.CursorList, mut name_to_idx map[string]int) {
	for i in 0 .. stmts.len() {
		escape_collect_param_aliases_stmt_cursor(stmts.at(i), mut name_to_idx)
	}
}

fn escape_collect_param_aliases_stmt_range_cursor(parent ast.Cursor, start int, mut name_to_idx map[string]int) {
	for i in start .. parent.edge_count() {
		escape_collect_param_aliases_stmt_cursor(parent.edge(i), mut name_to_idx)
	}
}

fn escape_collect_param_aliases_stmt_cursor(stmt ast.Cursor, mut name_to_idx map[string]int) {
	if !stmt.is_valid() {
		return
	}
	match stmt.kind() {
		.stmt_assign {
			if escape_cursor_token(stmt) == .decl_assign {
				lhs_len := stmt.extra_int()
				for i in 0 .. lhs_len {
					if lhs_len + i >= stmt.edge_count() {
						continue
					}
					lhs_name := escape_lhs_decl_name_cursor(stmt.edge(i))
					if lhs_name.len == 0 {
						continue
					}
					rhs_name := escape_returned_param_name_cursor(stmt.edge(lhs_len + i)) or {
						continue
					}
					if idx := name_to_idx[rhs_name] {
						name_to_idx[lhs_name] = idx
					}
				}
			}
		}
		.stmt_block, .stmt_defer {
			escape_collect_param_aliases_stmt_range_cursor(stmt, 0, mut name_to_idx)
		}
		.stmt_for {
			escape_collect_param_aliases_stmt_range_cursor(stmt, 3, mut name_to_idx)
		}
		else {}
	}
}

fn escape_collect_returned_param_indices_stmts_cursor(stmts ast.CursorList, name_to_idx map[string]int, mut out map[int]bool) {
	for i in 0 .. stmts.len() {
		escape_collect_returned_param_indices_stmt_cursor(stmts.at(i), name_to_idx, mut out)
	}
}

fn escape_collect_returned_param_indices_stmt_range_cursor(parent ast.Cursor, start int, name_to_idx map[string]int, mut out map[int]bool) {
	for i in start .. parent.edge_count() {
		escape_collect_returned_param_indices_stmt_cursor(parent.edge(i), name_to_idx, mut out)
	}
}

fn escape_collect_returned_param_indices_stmt_cursor(stmt ast.Cursor, name_to_idx map[string]int, mut out map[int]bool) {
	if !stmt.is_valid() {
		return
	}
	match stmt.kind() {
		.stmt_return {
			for i in 0 .. stmt.edge_count() {
				if name := escape_returned_param_name_cursor(stmt.edge(i)) {
					if name in name_to_idx {
						out[name_to_idx[name]] = true
					}
				}
			}
		}
		.stmt_block, .stmt_defer {
			escape_collect_returned_param_indices_stmt_range_cursor(stmt, 0, name_to_idx, mut out)
		}
		.stmt_for {
			escape_collect_returned_param_indices_stmt_range_cursor(stmt, 3, name_to_idx, mut out)
		}
		.stmt_expr {
			escape_collect_returned_param_indices_expr_cursor(stmt.edge(0), name_to_idx, mut out)
		}
		else {}
	}
}

fn escape_collect_returned_param_indices_expr_cursor(expr ast.Cursor, name_to_idx map[string]int, mut out map[int]bool) {
	if !expr.is_valid() {
		return
	}
	match expr.kind() {
		.expr_if {
			escape_collect_returned_param_indices_stmt_range_cursor(expr, 2, name_to_idx, mut out)
			escape_collect_returned_param_indices_expr_cursor(expr.edge(1), name_to_idx, mut out)
		}
		.expr_match {
			for i in 1 .. expr.edge_count() {
				branch := expr.edge(i)
				escape_collect_returned_param_indices_stmts_cursor(branch.list_at(1), name_to_idx, mut
					out)
			}
		}
		.expr_lock {
			start := escape_lock_stmt_start(expr)
			escape_collect_returned_param_indices_stmt_range_cursor(expr, start, name_to_idx, mut
				out)
		}
		else {}
	}
}

// escape_collect_locals_stmts records names of every binding declared in
// `stmts` and any nested in-fn block (if/match/for/lock/...). Nested
// `FnDecl`s are *not* descended — they introduce their own scope.
fn escape_collect_locals_stmts(stmts []ast.Stmt, mut out map[string]bool) {
	for stmt in stmts {
		escape_collect_locals_stmt(stmt, mut out)
	}
}

fn escape_collect_locals_stmts_cursor(stmts ast.CursorList, mut out map[string]bool) {
	for i in 0 .. stmts.len() {
		escape_collect_locals_stmt_cursor(stmts.at(i), mut out)
	}
}

fn escape_collect_locals_stmt_range_cursor(parent ast.Cursor, start int, mut out map[string]bool) {
	for i in start .. parent.edge_count() {
		escape_collect_locals_stmt_cursor(parent.edge(i), mut out)
	}
}

fn escape_collect_locals_stmt_cursor(stmt ast.Cursor, mut out map[string]bool) {
	if !stmt.is_valid() {
		return
	}
	match stmt.kind() {
		.stmt_assign {
			if escape_cursor_token(stmt) == .decl_assign {
				lhs_len := stmt.extra_int()
				for i in 0 .. lhs_len {
					name := escape_lhs_decl_name_cursor(stmt.edge(i))
					if name.len > 0 {
						out[name] = true
					}
				}
			}
		}
		.stmt_block, .stmt_defer {
			escape_collect_locals_stmt_range_cursor(stmt, 0, mut out)
		}
		.stmt_for {
			escape_collect_locals_stmt_cursor(stmt.edge(0), mut out)
			escape_collect_locals_stmt_range_cursor(stmt, 3, mut out)
		}
		.stmt_for_in {
			key := stmt.edge(0)
			value := stmt.edge(1)
			if key.is_valid() && key.kind() == .expr_ident {
				out[key.name()] = true
			}
			if value.is_valid() && value.kind() == .expr_ident {
				out[value.name()] = true
			}
		}
		.stmt_expr {
			escape_collect_locals_expr_cursor(stmt.edge(0), mut out)
		}
		.stmt_return {
			for i in 0 .. stmt.edge_count() {
				escape_collect_locals_expr_cursor(stmt.edge(i), mut out)
			}
		}
		else {}
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

fn escape_collect_closure_borrow_locals_stmts_cursor(stmts ast.CursorList, locals map[string]bool, mut out map[string]bool) {
	for i in 0 .. stmts.len() {
		escape_collect_closure_borrow_locals_stmt_cursor(stmts.at(i), locals, mut out)
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

fn escape_collect_closure_borrow_locals_stmt_range_cursor(parent ast.Cursor, start int, locals map[string]bool, mut out map[string]bool) {
	for i in start .. parent.edge_count() {
		escape_collect_closure_borrow_locals_stmt_cursor(parent.edge(i), locals, mut out)
	}
}

fn escape_collect_closure_borrow_locals_stmt_cursor(stmt ast.Cursor, locals map[string]bool, mut out map[string]bool) {
	if !stmt.is_valid() {
		return
	}
	match stmt.kind() {
		.stmt_assign {
			if escape_cursor_token(stmt) == .decl_assign {
				lhs_len := stmt.extra_int()
				for i in 0 .. lhs_len {
					if lhs_len + i >= stmt.edge_count() {
						continue
					}
					name := escape_lhs_decl_name_cursor(stmt.edge(i))
					if name.len == 0 {
						continue
					}
					rhs := stmt.edge(lhs_len + i)
					if rhs.is_valid() && rhs.kind() == .expr_fn_literal {
						if _ := escape_closure_borrow_reason_cursor(rhs, locals) {
							out[name] = true
						}
					}
				}
			}
		}
		.stmt_block, .stmt_defer {
			escape_collect_closure_borrow_locals_stmt_range_cursor(stmt, 0, locals, mut out)
		}
		.stmt_for {
			escape_collect_closure_borrow_locals_stmt_range_cursor(stmt, 3, locals, mut out)
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

fn escape_collect_locals_expr_cursor(expr ast.Cursor, mut out map[string]bool) {
	if !expr.is_valid() {
		return
	}
	match expr.kind() {
		.expr_if {
			escape_collect_locals_stmt_range_cursor(expr, 2, mut out)
			escape_collect_locals_expr_cursor(expr.edge(1), mut out)
		}
		.expr_match {
			for i in 1 .. expr.edge_count() {
				branch := expr.edge(i)
				escape_collect_locals_stmts_cursor(branch.list_at(1), mut out)
			}
		}
		.expr_lock {
			escape_collect_locals_stmt_range_cursor(expr, escape_lock_stmt_start(expr), mut out)
		}
		else {}
	}
}

fn (mut c Checker) escape_walk_stmts(stmts []ast.Stmt, locals map[string]bool, closure_borrow_locals map[string]bool, fn_name string) {
	for stmt in stmts {
		c.escape_walk_stmt(stmt, locals, closure_borrow_locals, fn_name)
	}
}

fn (mut c Checker) escape_walk_stmts_cursor(stmts ast.CursorList, locals map[string]bool, closure_borrow_locals map[string]bool, fn_name string) {
	for i in 0 .. stmts.len() {
		c.escape_walk_stmt_cursor(stmts.at(i), locals, closure_borrow_locals, fn_name)
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

fn (mut c Checker) escape_walk_stmt_range_cursor(parent ast.Cursor, start int, locals map[string]bool, closure_borrow_locals map[string]bool, fn_name string) {
	for i in start .. parent.edge_count() {
		c.escape_walk_stmt_cursor(parent.edge(i), locals, closure_borrow_locals, fn_name)
	}
}

fn (mut c Checker) escape_walk_stmt_cursor(stmt ast.Cursor, locals map[string]bool, closure_borrow_locals map[string]bool, fn_name string) {
	if !stmt.is_valid() {
		return
	}
	match stmt.kind() {
		.stmt_return {
			for i in 0 .. stmt.edge_count() {
				c.escape_check_returned_expr_cursor(stmt.edge(i), locals, closure_borrow_locals,
					fn_name)
			}
		}
		.stmt_assign {
			if escape_cursor_token(stmt) == .assign {
				lhs_len := stmt.extra_int()
				for i in 0 .. lhs_len {
					if lhs_len + i >= stmt.edge_count() {
						continue
					}
					c.escape_check_field_store_cursor(stmt.edge(i), stmt.edge(lhs_len + i), locals,
						closure_borrow_locals, fn_name, stmt.pos())
				}
			}
		}
		.stmt_block, .stmt_defer {
			c.escape_walk_stmt_range_cursor(stmt, 0, locals, closure_borrow_locals, fn_name)
		}
		.stmt_for {
			c.escape_walk_stmt_range_cursor(stmt, 3, locals, closure_borrow_locals, fn_name)
		}
		.stmt_expr {
			c.escape_walk_expr_cursor(stmt.edge(0), locals, closure_borrow_locals, fn_name)
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

fn (mut c Checker) escape_walk_expr_cursor(expr ast.Cursor, locals map[string]bool, closure_borrow_locals map[string]bool, fn_name string) {
	if !expr.is_valid() {
		return
	}
	match expr.kind() {
		.expr_if {
			c.escape_walk_stmt_range_cursor(expr, 2, locals, closure_borrow_locals, fn_name)
			c.escape_walk_expr_cursor(expr.edge(1), locals, closure_borrow_locals, fn_name)
		}
		.expr_match {
			for i in 1 .. expr.edge_count() {
				branch := expr.edge(i)
				c.escape_walk_stmts_cursor(branch.list_at(1), locals, closure_borrow_locals,
					fn_name)
			}
		}
		.expr_lock {
			c.escape_walk_stmt_range_cursor(expr, escape_lock_stmt_start(expr), locals,
				closure_borrow_locals, fn_name)
		}
		.expr_infix {
			if escape_cursor_token(expr) == .left_shift {
				c.escape_check_array_push_cursor(expr, locals, closure_borrow_locals, fn_name)
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

fn (mut c Checker) escape_check_returned_expr_cursor(expr ast.Cursor, locals map[string]bool, closure_borrow_locals map[string]bool, fn_name string) {
	if !expr.is_valid() {
		return
	}
	match expr.kind() {
		.expr_prefix {
			if escape_cursor_token(expr) == .amp {
				if reason := escape_borrow_reason_cursor(expr.edge(0), locals) {
					c.escape_error_returns_local(reason, fn_name, expr.pos())
				}
			}
			return
		}
		.expr_fn_literal {
			if reason := escape_closure_borrow_reason_cursor(expr, locals) {
				c.escape_error_returns_local(reason, fn_name, expr.pos())
			}
			return
		}
		.expr_ident {
			if expr.name() in closure_borrow_locals {
				c.escape_error_returns_local('closure `${expr.name()}` capturing local by reference',
					fn_name, expr.pos())
			}
			return
		}
		.expr_call, .expr_call_or_cast {
			if reason := c.escape_call_passthrough_reason_cursor(expr, locals,
				closure_borrow_locals)
			{
				c.escape_error_returns_local(reason, fn_name, expr.pos())
			}
		}
		else {}
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

fn escape_borrow_reason_cursor(operand ast.Cursor, locals map[string]bool) ?string {
	if !operand.is_valid() {
		return none
	}
	match operand.kind() {
		.expr_ident {
			if operand.name() in locals {
				return 'local variable `${operand.name()}`'
			}
		}
		.expr_selector {
			root := escape_lhs_root_cursor(operand) or { return none }
			if root in locals {
				return 'local variable `${root}` via field access'
			}
		}
		.expr_index {
			base := operand.edge(0)
			if base.is_valid() && base.kind() == .expr_ident && base.name() in locals {
				return 'index into local `${base.name()}`'
			}
		}
		.expr_init {
			return 'transient struct literal'
		}
		.expr_array_init {
			return 'transient array literal'
		}
		.expr_map_init {
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

fn (mut c Checker) escape_check_field_store_cursor(lhs ast.Cursor, rhs ast.Cursor, locals map[string]bool, closure_borrow_locals map[string]bool, fn_name string, pos token.Pos) {
	root := escape_lhs_root_cursor(lhs) or { return }
	if root in locals {
		return
	}
	reason := c.escape_rhs_borrow_reason_cursor(rhs, locals, closure_borrow_locals) or { return }
	path := escape_render_path_cursor(lhs)
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

fn (mut c Checker) escape_check_array_push_cursor(expr ast.Cursor, locals map[string]bool, closure_borrow_locals map[string]bool, fn_name string) {
	lhs := expr.edge(0)
	root := escape_lhs_root_cursor(lhs) or { return }
	if root in locals {
		return
	}
	reason := c.escape_rhs_borrow_reason_cursor(expr.edge(1), locals, closure_borrow_locals) or {
		return
	}
	path := escape_render_path_cursor(lhs)
	c.escape_error_stores_local('pushed into `${path}`', reason, fn_name, expr.pos())
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

fn (c &Checker) escape_rhs_borrow_reason_cursor(rhs ast.Cursor, locals map[string]bool, closure_borrow_locals map[string]bool) ?string {
	if !rhs.is_valid() {
		return none
	}
	match rhs.kind() {
		.expr_prefix {
			if escape_cursor_token(rhs) == .amp {
				return escape_borrow_reason_cursor(rhs.edge(0), locals)
			}
		}
		.expr_fn_literal {
			return escape_closure_borrow_reason_cursor(rhs, locals)
		}
		.expr_ident {
			if rhs.name() in closure_borrow_locals {
				return 'closure `${rhs.name()}` capturing local by reference'
			}
		}
		.expr_call, .expr_call_or_cast {
			return c.escape_call_passthrough_reason_cursor(rhs, locals, closure_borrow_locals)
		}
		else {}
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

fn (c &Checker) escape_call_passthrough_reason_cursor(call ast.Cursor, locals map[string]bool, closure_borrow_locals map[string]bool) ?string {
	fn_name := escape_call_fn_short_name_cursor(call) or { return none }
	if fn_name !in c.escape_passthrough_fns {
		return none
	}
	indices := c.escape_passthrough_fns[fn_name]
	for idx in indices {
		if idx >= escape_call_arg_count_cursor(call) {
			continue
		}
		arg := escape_call_arg_cursor(call, idx)
		if inner := escape_arg_borrow_reason_cursor(arg, locals, closure_borrow_locals) {
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

fn escape_arg_borrow_reason_cursor(arg ast.Cursor, locals map[string]bool, closure_borrow_locals map[string]bool) ?string {
	if !arg.is_valid() {
		return none
	}
	match arg.kind() {
		.expr_prefix {
			if escape_cursor_token(arg) == .amp {
				return escape_borrow_reason_cursor(arg.edge(0), locals)
			}
		}
		.expr_fn_literal {
			return escape_closure_borrow_reason_cursor(arg, locals)
		}
		.expr_ident {
			if arg.name() in closure_borrow_locals {
				return 'closure `${arg.name()}` capturing local by reference'
			}
		}
		else {}
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

fn escape_call_fn_short_name_cursor(call ast.Cursor) ?string {
	if !call.is_valid() {
		return none
	}
	lhs := call.edge(0)
	if !lhs.is_valid() {
		return none
	}
	match lhs.kind() {
		.expr_ident {
			return lhs.name()
		}
		.expr_selector {
			rhs := lhs.edge(1)
			if rhs.is_valid() && rhs.kind() == .expr_ident {
				return rhs.name()
			}
		}
		else {}
	}

	return none
}

fn escape_call_arg_count_cursor(call ast.Cursor) int {
	if !call.is_valid() {
		return 0
	}
	if call.kind() == .expr_call_or_cast {
		return 1
	}
	if call.kind() == .expr_call {
		return call.edge_count() - 1
	}
	return 0
}

fn escape_call_arg_cursor(call ast.Cursor, idx int) ast.Cursor {
	if call.kind() == .expr_call_or_cast {
		if idx == 0 {
			return call.edge(1)
		}
		return ast.Cursor{}
	}
	return call.edge(1 + idx)
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

fn escape_closure_borrow_reason_cursor(lit ast.Cursor, locals map[string]bool) ?string {
	captured_len := lit.extra_int()
	for i in 0 .. captured_len {
		cv := lit.edge(1 + i)
		if !cv.is_valid() {
			continue
		}
		match cv.kind() {
			.expr_prefix {
				if escape_cursor_token(cv) == .amp {
					inner := cv.edge(0)
					if inner.is_valid() && inner.kind() == .expr_ident && inner.name() in locals {
						return 'local variable `${inner.name()}` captured by reference into closure'
					}
				}
			}
			.expr_modifier {
				if escape_cursor_token(cv) == .key_mut {
					inner := cv.edge(0)
					if inner.is_valid() && inner.kind() == .expr_ident && inner.name() in locals {
						return 'local variable `${inner.name()}` captured by mutable reference into closure'
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

fn escape_lhs_root_cursor(lhs ast.Cursor) ?string {
	if !lhs.is_valid() {
		return none
	}
	match lhs.kind() {
		.expr_ident {
			return lhs.name()
		}
		.expr_selector {
			return escape_lhs_root_cursor(lhs.edge(0))
		}
		.expr_index {
			return escape_lhs_root_cursor(lhs.edge(0))
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

fn escape_lhs_decl_name_cursor(lhs ast.Cursor) string {
	if !lhs.is_valid() {
		return ''
	}
	match lhs.kind() {
		.expr_ident {
			return lhs.name()
		}
		.expr_modifier {
			inner := lhs.edge(0)
			if inner.is_valid() && inner.kind() == .expr_ident {
				return inner.name()
			}
		}
		else {}
	}

	return ''
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

fn escape_render_path_cursor(expr ast.Cursor) string {
	if !expr.is_valid() {
		return '<expr>'
	}
	match expr.kind() {
		.expr_ident {
			return expr.name()
		}
		.expr_selector {
			rhs := expr.edge(1)
			rhs_name := if rhs.is_valid() && rhs.kind() == .expr_ident {
				rhs.name()
			} else {
				'<field>'
			}
			return '${escape_render_path_cursor(expr.edge(0))}.${rhs_name}'
		}
		.expr_index {
			return '${escape_render_path_cursor(expr.edge(0))}[...]'
		}
		else {
			return '<expr>'
		}
	}
}

fn escape_lock_stmt_start(expr ast.Cursor) int {
	packed := u32(expr.extra_int())
	lock_len := int(packed & 0xFFFF)
	rlock_len := int((packed >> 16) & 0xFFFF)
	return lock_len + rlock_len
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
