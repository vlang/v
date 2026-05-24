// Ownership checking for V2 compiler.
// Implements Rust-like move semantics for owned strings.
module types

import v2.ast
import v2.errors
import v2.token

// MovedVar tracks information about a variable that has been moved.
pub struct MovedVar {
pub:
	moved_to      string    // name of the variable or function parameter it was moved to
	move_pos      token.Pos // position where the move occurred
	is_fn_call    bool      // true if moved via function call argument
	suggest_clone bool = true // show clone suggestion in diagnostics
	fn_name       string // function name (only when is_fn_call is true)
	type_name     string = 'string' // type that does not implement Copy
}

// BorrowInfo tracks an active borrow of a variable.
pub struct BorrowInfo {
pub:
	borrower string    // who is borrowing (function name or variable name)
	pos      token.Pos // position where borrow was created
	is_mut   bool      // true if this is a mutable borrow
}

// ownership_check_ident checks if a variable has been moved before allowing use.
// Called from ident() when -ownership is enabled.
fn (mut c Checker) ownership_check_ident(name string, pos token.Pos) {
	if name in c.moved_vars {
		info := c.moved_vars[name]
		file := c.file_set.file(pos)
		move_file := c.file_set.file(info.move_pos)
		move_position := move_file.position(info.move_pos)
		errors.error('use of moved value: `${name}`', errors.details(file, file.position(pos), 2),
			.error, file.position(pos))
		type_name := if info.type_name.len > 0 { info.type_name } else { 'string' }
		eprintln('  --> move occurs because `${name}` has type `${type_name}`, which does not implement the `Copy` interface')
		if info.is_fn_call {
			eprintln('  --> value moved into function `${info.fn_name}` at ${move_position}')
			eprintln('help: consider cloning the value if the performance cost is acceptable')
			eprintln('  |     ${info.fn_name}(${name}.clone())')
		} else {
			eprintln('  --> value moved to `${info.moved_to}` at ${move_position}')
			if info.suggest_clone {
				eprintln('help: consider cloning the value if the performance cost is acceptable')
				eprintln('  |     ${info.moved_to} := ${name}.clone()')
			}
		}
		exit(1)
	}
}

// ownership_check_assign handles ownership transfer during assignment.
// When `s2 := s1` and s1 is owned, s1 is moved to s2.
fn (mut c Checker) ownership_check_assign(lhs_name string, rhs ast.Expr, assign_pos token.Pos) {
	// Check if RHS is an ident that is owned
	rhs_name := ownership_expr_ident_name(rhs)
	if rhs_name.len > 0 && rhs_name in c.owned_vars {
		// Cannot move a variable that is currently borrowed
		if rhs_name in c.borrowed_vars {
			borrows := c.borrowed_vars[rhs_name]
			if borrows.len > 0 {
				borrow := borrows[0]
				file := c.file_set.file(assign_pos)
				borrow_file := c.file_set.file(borrow.pos)
				borrow_position := borrow_file.position(borrow.pos)
				errors.error('cannot move `${rhs_name}` because it is borrowed', errors.details(file,
					file.position(assign_pos), 2), .error, file.position(assign_pos))
				eprintln('  --> `${rhs_name}` is borrowed by `${borrow.borrower}` at ${borrow_position}')
				exit(1)
			}
		}
		// Move: mark rhs as moved, mark lhs as owned. The LHS inherits the
		// RHS's tracked type name so downstream diagnostics show the right
		// type (e.g. `Foo` rather than the default `string`).
		rhs_type_name := c.owned_var_types[rhs_name] or { 'string' }
		c.moved_vars[rhs_name] = MovedVar{
			moved_to:  lhs_name
			move_pos:  assign_pos
			type_name: rhs_type_name
		}
		c.owned_vars[lhs_name] = assign_pos
		c.owned_var_types[lhs_name] = rhs_type_name
	}
	// Check if RHS is &ident (creating a reference to an owned variable)
	if rhs is ast.PrefixExpr {
		prefix := rhs as ast.PrefixExpr
		if prefix.op == .amp {
			ref_name := ownership_expr_ident_name(prefix.expr)
			if ref_name.len > 0 && ref_name in c.owned_vars {
				c.ownership_add_borrow(ref_name, lhs_name, assign_pos, false)
			}
		}
	}
}

fn (mut c Checker) ownership_consume_expr(expr ast.Expr, pos token.Pos, target string) {
	name := ownership_expr_ident_name(expr)
	if name.len == 0 || name !in c.owned_vars {
		return
	}
	if name in c.borrowed_vars {
		borrows := c.borrowed_vars[name]
		if borrows.len > 0 {
			borrow := borrows[0]
			file := c.file_set.file(pos)
			borrow_file := c.file_set.file(borrow.pos)
			borrow_position := borrow_file.position(borrow.pos)
			errors.error('cannot move `${name}` because it is borrowed', errors.details(file,
				file.position(pos), 2), .error, file.position(pos))
			eprintln('  --> `${name}` is borrowed by `${borrow.borrower}` at ${borrow_position}')
			exit(1)
		}
	}
	type_name := c.owned_var_types[name] or { 'string' }
	c.moved_vars[name] = MovedVar{
		moved_to:      target
		move_pos:      pos
		suggest_clone: false
		type_name:     type_name
	}
}

// ownership_check_reassign checks that a variable is not reassigned while borrowed.
fn (mut c Checker) ownership_check_reassign(name string, pos token.Pos) {
	if name in c.borrowed_vars {
		borrows := c.borrowed_vars[name]
		if borrows.len > 0 {
			borrow := borrows[0]
			file := c.file_set.file(pos)
			borrow_file := c.file_set.file(borrow.pos)
			borrow_position := borrow_file.position(borrow.pos)
			errors.error('cannot assign to `${name}` because it is borrowed', errors.details(file,
				file.position(pos), 2), .error, file.position(pos))
			eprintln('  --> `${name}` is borrowed by `${borrow.borrower}` at ${borrow_position}')
			exit(1)
		}
	}
}

// ownership_check_call_args checks function call arguments for owned variables
// and marks them as moved into the function.
// Arguments passed as &arg are borrows (not moves).
// Borrows from function call arguments are released after the call returns.
fn (mut c Checker) ownership_check_call_args(expr ast.CallExpr) {
	fn_name := ownership_call_fn_name(expr)
	mut call_borrows := []string{} // track borrows created by this call for cleanup
	for i, arg in expr.args {
		// Check if argument is &ident (immutable borrow)
		if arg is ast.PrefixExpr {
			prefix := arg as ast.PrefixExpr
			if prefix.op == .amp {
				borrow_name := ownership_expr_ident_name(prefix.expr)
				if borrow_name.len > 0 && borrow_name in c.owned_vars {
					c.ownership_add_borrow(borrow_name, fn_name, expr.pos, false)
					call_borrows << borrow_name
					continue
				}
			}
		}
		// Check if argument is `mut ident` (mutable borrow)
		if arg is ast.ModifierExpr {
			modifier := arg as ast.ModifierExpr
			if modifier.kind == .key_mut {
				borrow_name := ownership_expr_ident_name(modifier.expr)
				if borrow_name.len > 0 && borrow_name in c.owned_vars {
					c.ownership_add_borrow(borrow_name, fn_name, expr.pos, true)
					call_borrows << borrow_name
					continue
				}
			}
		}
		// Regular argument — moves ownership
		arg_name := ownership_expr_ident_name(arg)
		if arg_name.len > 0 && arg_name in c.owned_vars {
			// Cannot move a variable that is currently borrowed
			if arg_name in c.borrowed_vars {
				borrows := c.borrowed_vars[arg_name]
				if borrows.len > 0 {
					borrow := borrows[0]
					file := c.file_set.file(expr.pos)
					borrow_file := c.file_set.file(borrow.pos)
					borrow_position := borrow_file.position(borrow.pos)
					errors.error('cannot move `${arg_name}` because it is borrowed', errors.details(file,
						file.position(expr.pos), 2), .error, file.position(expr.pos))
					eprintln('  --> `${arg_name}` is borrowed by `${borrow.borrower}` at ${borrow_position}')
					exit(1)
				}
			}
			type_name := c.owned_var_types[arg_name] or { 'string' }
			c.moved_vars[arg_name] = MovedVar{
				moved_to:   fn_name
				move_pos:   expr.pos
				is_fn_call: true
				fn_name:    fn_name
				type_name:  type_name
			}
			// Track that parameter i of this function receives an owned value
			key := '${fn_name}__param_${i}'
			c.ownership_fn_params[key] = true
		}
	}
	// Release borrows created by this call — they only last for the call duration
	for borrow_name in call_borrows {
		c.ownership_release_borrow(borrow_name, fn_name)
	}
}

// ownership_check_return checks if a return statement returns an owned value,
// and if so marks the current function as returning ownership.
fn (mut c Checker) ownership_check_return(stmt ast.ReturnStmt) {
	if c.ownership_cur_fn.len == 0 {
		return
	}
	for expr in stmt.exprs {
		name := ownership_expr_ident_name(expr)
		if name.len > 0 && name in c.owned_vars {
			// This function returns an owned value — mark the function
			c.ownership_fns[c.ownership_cur_fn] = true
			// The returned variable is moved out of this scope
			type_name := c.owned_var_types[name] or { 'string' }
			c.moved_vars[name] = MovedVar{
				moved_to:   c.ownership_cur_fn
				move_pos:   c.owned_vars[name] // use the owned position as fallback
				is_fn_call: true
				fn_name:    c.ownership_cur_fn
				type_name:  type_name
			}
		} else if ownership_is_to_owned_call(expr) {
			// Direct return of .to_owned() call, e.g. `return 'x'.to_owned()`
			c.ownership_fns[c.ownership_cur_fn] = true
		} else if ownership_is_ownership_call(expr, c.ownership_fns) {
			// Direct return of a function known to give ownership, e.g. `return gives_ownership()`
			c.ownership_fns[c.ownership_cur_fn] = true
		}
	}
}

// ownership_mark_from_call detects calls that produce owned values:
// 1. `expr.to_owned()` calls
// 2. Calls to functions known to return ownership
// 3. Calls to functions that return a parameter, when that parameter is owned
fn (mut c Checker) ownership_mark_from_call(lhs_name string, rhs ast.Expr, pos token.Pos) bool {
	if rhs is ast.CallExpr {
		return c.ownership_mark_from_call_expr(lhs_name, rhs as ast.CallExpr, pos)
	}
	if rhs is ast.CallOrCastExpr {
		coce := rhs as ast.CallOrCastExpr
		// Treat as a call with a single arg
		return c.ownership_mark_from_call_expr(lhs_name, ast.CallExpr{
			lhs:  coce.lhs
			args: [coce.expr]
			pos:  coce.pos
		}, pos)
	}
	return false
}

fn (mut c Checker) ownership_mark_from_call_expr(lhs_name string, call ast.CallExpr, pos token.Pos) bool {
	// Check for .to_owned()
	if call.lhs is ast.SelectorExpr {
		sel := call.lhs as ast.SelectorExpr
		if sel.rhs.name == 'to_owned' {
			c.ownership_mark_owned(lhs_name, Type(string_), pos)
			return true
		}
	}
	// Get function name
	fn_name := ownership_call_fn_name(call)
	// Check if calling a function that returns ownership
	if fn_name in c.ownership_fns {
		c.ownership_mark_owned(lhs_name, Type(string_), pos)
		return true
	}
	// Check if the function returns a specific parameter and that parameter's arg was owned.
	// Note: ownership_check_call_args may have already moved the arg from owned_vars
	// to moved_vars (since c.expr(rhs) runs before this), so also check moved_vars
	// for args that were just moved into this specific function call.
	if fn_name in c.ownership_fn_returns_param {
		param_idx := c.ownership_fn_returns_param[fn_name]
		if param_idx >= 0 && param_idx < call.args.len {
			arg_name := ownership_expr_ident_name(call.args[param_idx])
			if arg_name.len > 0 {
				if arg_name in c.owned_vars {
					type_name := c.owned_var_types[arg_name] or { 'string' }
					c.owned_vars[lhs_name] = pos
					c.owned_var_types[lhs_name] = type_name
					return true
				}
				if arg_name in c.moved_vars {
					info := c.moved_vars[arg_name]
					if info.is_fn_call && info.fn_name == fn_name {
						type_name := if info.type_name.len > 0 {
							info.type_name
						} else {
							'string'
						}
						c.owned_vars[lhs_name] = pos
						c.owned_var_types[lhs_name] = type_name
						return true
					}
				}
			}
		}
	}
	return false
}

// ownership_call_fn_name extracts the function name from a CallExpr.
fn ownership_call_fn_name(expr ast.CallExpr) string {
	match expr.lhs {
		ast.Ident {
			return expr.lhs.name
		}
		ast.SelectorExpr {
			sel := expr.lhs as ast.SelectorExpr
			return sel.rhs.name
		}
		else {
			return '<function>'
		}
	}
}

// ownership_add_borrow records a borrow of an owned variable.
fn (mut c Checker) ownership_add_borrow(var_name string, borrower string, pos token.Pos, is_mut bool) {
	// Check existing borrows for conflicts
	if var_name in c.borrowed_vars {
		existing := c.borrowed_vars[var_name]
		if is_mut && existing.len > 0 {
			// Cannot create mutable borrow when any borrow exists
			borrow := existing[0]
			file := c.file_set.file(pos)
			borrow_file := c.file_set.file(borrow.pos)
			borrow_position := borrow_file.position(borrow.pos)
			if borrow.is_mut {
				errors.error('cannot borrow `${var_name}` as mutable more than once', errors.details(file,
					file.position(pos), 2), .error, file.position(pos))
			} else {
				errors.error('cannot borrow `${var_name}` as mutable because it is also borrowed as immutable', errors.details(file,
					file.position(pos), 2), .error, file.position(pos))
			}
			eprintln('  --> previous borrow by `${borrow.borrower}` at ${borrow_position}')
			exit(1)
		}
		if !is_mut {
			for borrow in existing {
				if borrow.is_mut {
					// Cannot create immutable borrow when mutable borrow exists
					file := c.file_set.file(pos)
					borrow_file := c.file_set.file(borrow.pos)
					borrow_position := borrow_file.position(borrow.pos)
					errors.error('cannot borrow `${var_name}` as immutable because it is borrowed as mutable', errors.details(file,
						file.position(pos), 2), .error, file.position(pos))
					eprintln('  --> mutable borrow by `${borrow.borrower}` at ${borrow_position}')
					exit(1)
				}
			}
		}
	}
	new_borrow := BorrowInfo{
		borrower: borrower
		pos:      pos
		is_mut:   is_mut
	}
	if var_name in c.borrowed_vars {
		mut borrows := c.borrowed_vars[var_name]
		borrows << new_borrow
		c.borrowed_vars[var_name] = borrows
	} else {
		c.borrowed_vars[var_name] = [new_borrow]
	}
}

// ownership_release_borrow removes a borrow by a specific borrower from a variable.
fn (mut c Checker) ownership_release_borrow(var_name string, borrower string) {
	if var_name !in c.borrowed_vars {
		return
	}
	existing := c.borrowed_vars[var_name]
	mut remaining := []BorrowInfo{}
	mut removed := false
	for borrow in existing {
		if !removed && borrow.borrower == borrower {
			removed = true // remove first matching borrow
			continue
		}
		remaining << borrow
	}
	if remaining.len > 0 {
		c.borrowed_vars[var_name] = remaining
	} else {
		c.borrowed_vars.delete(var_name)
	}
}

// ownership_enter_fn sets up ownership tracking for a function body.
// Marks parameters that received owned values from call sites as owned within this scope.
fn (mut c Checker) ownership_enter_fn(fn_name string, decl ast.FnDecl) {
	c.ownership_cur_fn = fn_name
	// Check if any parameters of this function received owned values at call sites
	for i, param in decl.typ.params {
		key := '${fn_name}__param_${i}'
		if key in c.ownership_fn_params {
			c.owned_vars[param.name] = decl.pos
			// Best-effort type display name; resolved from the parameter type
			// expression via the checker's name helper.
			c.owned_var_types[param.name] = c.type_ref_name(param.typ)
		}
	}
}

// ownership_leave_fn cleans up ownership tracking when leaving a function body.
fn (mut c Checker) ownership_leave_fn(prev_fn string, prev_owned map[string]token.Pos, prev_owned_types map[string]string, prev_moved map[string]MovedVar, prev_borrowed map[string][]BorrowInfo) {
	c.ownership_cur_fn = prev_fn
	c.owned_vars = prev_owned.clone()
	c.owned_var_types = prev_owned_types.clone()
	c.moved_vars = prev_moved.clone()
	c.borrowed_vars = prev_borrowed.clone()
}

fn ownership_stmts_terminate(stmts []ast.Stmt) bool {
	for i := stmts.len - 1; i >= 0; i-- {
		stmt := stmts[i]
		if stmt is ast.EmptyStmt {
			continue
		}
		return stmt is ast.ReturnStmt
	}
	return false
}

fn ownership_expr_terminates(expr ast.Expr) bool {
	if expr is ast.IfExpr {
		if_expr := expr as ast.IfExpr
		if is_empty_expr(if_expr.cond) {
			return ownership_stmts_terminate(if_expr.stmts)
		}
		if is_empty_expr(if_expr.else_expr) {
			return false
		}
		return ownership_stmts_terminate(if_expr.stmts)
			&& ownership_expr_terminates(if_expr.else_expr)
	}
	return false
}

fn ownership_merge_owned(a map[string]token.Pos, b map[string]token.Pos) map[string]token.Pos {
	mut out := a.clone()
	for name, pos in b {
		out[name] = pos
	}
	return out
}

fn ownership_merge_moved(a map[string]MovedVar, b map[string]MovedVar) map[string]MovedVar {
	mut out := a.clone()
	for name, info in b {
		out[name] = info
	}
	return out
}

fn ownership_merge_borrowed(a map[string][]BorrowInfo, b map[string][]BorrowInfo) map[string][]BorrowInfo {
	mut out := a.clone()
	for name, infos in b {
		mut merged := out[name] or { []BorrowInfo{} }
		merged << infos
		out[name] = merged
	}
	return out
}

fn (mut c Checker) ownership_restore_state(owned map[string]token.Pos, moved map[string]MovedVar, borrowed map[string][]BorrowInfo) {
	c.owned_vars = owned.clone()
	c.moved_vars = moved.clone()
	c.borrowed_vars = borrowed.clone()
	// Prune owned_var_types to match owned_vars after restore. Any owned var
	// that disappeared from `owned` should have its cached type display dropped
	// too, otherwise diagnostics would leak across branches.
	mut new_types := map[string]string{}
	for name, _ in owned {
		if name in c.owned_var_types {
			new_types[name] = c.owned_var_types[name]
		}
	}
	c.owned_var_types = new_types.clone()
}

// OwnershipArmState is one match-arm's post-state snapshot. Used by
// `ownership_merge_match_state` to fold N arms into a single after-match
// ownership view, mirroring the if/else two-branch merge.
pub struct OwnershipArmState {
pub:
	owned      map[string]token.Pos
	moved      map[string]MovedVar
	borrowed   map[string][]BorrowInfo
	terminates bool
}

// ownership_merge_match_state folds N match-arm post-states into a single
// after-match ownership view. Union semantics, mirroring `if/else`:
//
//   * If every arm terminates (each ends in `return`), the match itself
//     can't fall through — restore the before-state so any code after the
//     match (which is unreachable in this fn) inherits the un-mutated view.
//   * Otherwise, a var moved in ANY non-terminating arm is moved after the
//     match. A var newly-owned in any non-terminating arm is owned after.
//
// V's `match` is (effectively) exhaustive — every value reaches some arm —
// so there's no phantom "fall-through" arm to add; a no-op arm contributes
// nothing to the union anyway.
fn (mut c Checker) ownership_merge_match_state(before_owned map[string]token.Pos, before_moved map[string]MovedVar, before_borrowed map[string][]BorrowInfo, arms []OwnershipArmState) {
	if arms.len == 0 {
		c.ownership_restore_state(before_owned, before_moved, before_borrowed)
		return
	}
	mut any_falls_through := false
	for arm in arms {
		if !arm.terminates {
			any_falls_through = true
			break
		}
	}
	if !any_falls_through {
		c.ownership_restore_state(before_owned, before_moved, before_borrowed)
		return
	}
	mut out_owned := before_owned.clone()
	mut out_moved := before_moved.clone()
	mut out_borrowed := before_borrowed.clone()
	for arm in arms {
		if arm.terminates {
			continue
		}
		out_owned = ownership_merge_owned(out_owned, arm.owned)
		out_moved = ownership_merge_moved(out_moved, arm.moved)
		out_borrowed = ownership_merge_borrowed(out_borrowed, arm.borrowed)
	}
	c.ownership_restore_state(out_owned, out_moved, out_borrowed)
}

fn (mut c Checker) ownership_merge_if_state(before_owned map[string]token.Pos, before_moved map[string]MovedVar, before_borrowed map[string][]BorrowInfo, then_owned map[string]token.Pos, then_moved map[string]MovedVar, then_borrowed map[string][]BorrowInfo, then_returns bool, has_else bool, else_owned map[string]token.Pos, else_moved map[string]MovedVar, else_borrowed map[string][]BorrowInfo, else_returns bool) {
	if !has_else {
		if then_returns {
			c.ownership_restore_state(before_owned, before_moved, before_borrowed)
			return
		}
		c.ownership_restore_state(ownership_merge_owned(before_owned, then_owned), ownership_merge_moved(before_moved,
			then_moved), ownership_merge_borrowed(before_borrowed, then_borrowed))
		return
	}
	if then_returns && else_returns {
		c.ownership_restore_state(before_owned, before_moved, before_borrowed)
		return
	}
	if then_returns {
		c.ownership_restore_state(else_owned, else_moved, else_borrowed)
		return
	}
	if else_returns {
		c.ownership_restore_state(then_owned, then_moved, then_borrowed)
		return
	}
	c.ownership_restore_state(ownership_merge_owned(then_owned, else_owned), ownership_merge_moved(then_moved,
		else_moved), ownership_merge_borrowed(then_borrowed, else_borrowed))
}

// ownership_prescan_fn_bodies does a lightweight pre-scan of all pending function bodies
// to detect which functions return owned values. This runs before the full checker pass
// so that callers can know about ownership-returning functions regardless of declaration order.
fn (mut c Checker) ownership_prescan_fn_bodies() {
	// Pass 1: Find functions that directly create and return owned values (via .to_owned())
	for pending in c.pending_fn_bodies {
		if ownership_prescan_returns_owned(pending.decl.stmts) {
			c.ownership_fns[pending.decl.name] = true
		}
	}
	// Pass 2: Find functions that return a parameter — if callers pass owned values,
	// these functions transfer ownership through.
	for pending in c.pending_fn_bodies {
		returned_param := ownership_prescan_returns_param(pending.decl)
		if returned_param >= 0 {
			c.ownership_fn_returns_param[pending.decl.name] = returned_param
		}
	}
	// Pass 3: Find methods declared with a by-value receiver — calls to these
	// methods on owned vars MOVE the receiver (consuming-self semantics).
	c.ownership_prescan_value_receivers()
}

// ownership_prescan_returns_owned checks if a function body creates a .to_owned() value
// and returns it. Pure AST scan, no type checking needed.
fn ownership_prescan_returns_owned(stmts []ast.Stmt) bool {
	mut owned_locals := map[string]bool{}
	for stmt in stmts {
		match stmt {
			ast.AssignStmt {
				// Look for `x := expr.to_owned()`
				if stmt.lhs.len > 0 && stmt.rhs.len > 0 {
					lhs_name := ownership_expr_ident_name(stmt.lhs[0])
					if lhs_name.len > 0 && ownership_is_to_owned_call(stmt.rhs[0]) {
						owned_locals[lhs_name] = true
					}
				}
			}
			ast.ReturnStmt {
				for expr in stmt.exprs {
					name := ownership_expr_ident_name(expr)
					if name.len > 0 && name in owned_locals {
						return true
					}
					// Direct return of .to_owned(), e.g. `return 'x'.to_owned()`
					if ownership_is_to_owned_call(expr) {
						return true
					}
				}
			}
			else {}
		}
	}
	return false
}

// ownership_prescan_returns_param checks if a function returns one of its parameters.
// Returns the parameter index, or -1 if no parameter is returned.
fn ownership_prescan_returns_param(decl ast.FnDecl) int {
	mut param_names := map[string]int{}
	for i, param in decl.typ.params {
		param_names[param.name] = i
	}
	for stmt in decl.stmts {
		if stmt is ast.ReturnStmt {
			ret := stmt as ast.ReturnStmt
			for expr in ret.exprs {
				name := ownership_expr_ident_name(expr)
				if name in param_names {
					return param_names[name]
				}
			}
		}
	}
	return -1
}

// ownership_is_to_owned_call checks if an expression is a `.to_owned()` call.
fn ownership_is_to_owned_call(expr ast.Expr) bool {
	if expr is ast.CallExpr {
		call := expr as ast.CallExpr
		if call.lhs is ast.SelectorExpr {
			sel := call.lhs as ast.SelectorExpr
			return sel.rhs.name == 'to_owned'
		}
	}
	if expr is ast.CallOrCastExpr {
		coce := expr as ast.CallOrCastExpr
		if coce.lhs is ast.SelectorExpr {
			sel := coce.lhs as ast.SelectorExpr
			return sel.rhs.name == 'to_owned'
		}
	}
	return false
}

// ownership_is_ownership_call checks if an expression is a call to a function
// known to return ownership (present in ownership_fns).
fn ownership_is_ownership_call(expr ast.Expr, ownership_fns map[string]bool) bool {
	if expr is ast.CallExpr {
		fn_name := ownership_call_fn_name(expr as ast.CallExpr)
		return fn_name in ownership_fns
	}
	if expr is ast.CallOrCastExpr {
		coce := expr as ast.CallOrCastExpr
		if coce.lhs is ast.Ident {
			return (coce.lhs as ast.Ident).name in ownership_fns
		}
	}
	return false
}

// is_copy_type reports whether a type implements the `Copy` marker interface,
// either intrinsically (primitives, pointers, enums, function types, channels,
// strings) or explicitly via `struct X implements Copy`.
//
// Copy types are *never* moved on assignment. Non-Copy types are not
// automatically moved either; only types that opt into ownership via
// `implements Owned` (struct), `.to_owned()` (string), or a non-Copy
// owned-tracked container are subject to move semantics. See `is_owned_type`.
//
// Backward compatibility: `string` is treated as Copy here. Ownership is
// opted into per value via `.to_owned()`, preserving V's existing implicit
// auto-clone semantics for plain string literals.
pub fn (c &Checker) is_copy_type(t Type) bool {
	return c.is_copy_type_impl(t, 0)
}

fn (c &Checker) is_copy_type_impl(t Type, depth int) bool {
	if depth > 16 {
		// Cycle guard for self-referential type aliases.
		return true
	}
	match t {
		Primitive, Char, Rune, ISize, USize, Enum, Pointer, FnType, Nil, None, Void, Channel,
		Thread, NamedType, String {
			return true
		}
		Alias {
			return c.is_copy_type_impl(t.base_type, depth + 1)
		}
		OptionType {
			return c.is_copy_type_impl(t.base_type, depth + 1)
		}
		ResultType {
			return c.is_copy_type_impl(t.base_type, depth + 1)
		}
		Struct {
			return c.struct_marker_matches(t, 'Copy')
		}
		Interface, Array, ArrayFixed, Map, SumType, Tuple {
			return false
		}
	}

	return true
}

// ownership_is_rc_or_arc_wrapper reports whether a struct is the
// reference-counted wrapper `Rc[T]` or `Arc[T]` (recognised by name, under
// any module). Treated as `Owned` so that `r2 := r1` moves r1 — matching
// Rust semantics where Rc/Arc are *not* Copy and require an explicit
// `.clone()` to share. Goal: source-translating Rust code that uses Rc/Arc.
//
// Name-based matching is intentional: there is no compiler-managed Rc/Arc
// type in V2, so any struct the user (or a Rust-to-V translator) names `Rc`
// or `Arc` participates in shared-ownership tracking. Only fires when
// `-d ownership` is enabled, so simple V code is untouched.
fn ownership_is_rc_or_arc_wrapper(st Struct) bool {
	short := st.name.all_after_last('__')
	return short == 'Rc' || short == 'Arc'
}

// is_owned_type reports whether values of this type are tracked for ownership.
// A type is owned-tracked when the user has explicitly opted in:
//   * struct: `struct X implements Owned { ... }`
//   * alias / option / result: opt-in propagates through transparently
// Untracked types behave like today (auto-clone / shared reference), so
// existing code keeps compiling. This is what the new "owned types beyond
// string" path keys off in `assign_stmt`.
pub fn (c &Checker) is_owned_type(t Type) bool {
	return c.is_owned_type_impl(t, 0)
}

fn (c &Checker) is_owned_type_impl(t Type, depth int) bool {
	if depth > 16 {
		return false
	}
	match t {
		Alias {
			return c.is_owned_type_impl(t.base_type, depth + 1)
		}
		OptionType {
			return c.is_owned_type_impl(t.base_type, depth + 1)
		}
		ResultType {
			return c.is_owned_type_impl(t.base_type, depth + 1)
		}
		Struct {
			if ownership_is_rc_or_arc_wrapper(t) {
				return true
			}
			return c.struct_marker_matches(t, 'Owned')
		}
		else {
			return false
		}
	}
}

// struct_marker_matches checks whether a struct directly lists `target` in its
// `implements` clause. Used for the `Copy` and `Owned` marker interfaces.
fn (c &Checker) struct_marker_matches(st Struct, target string) bool {
	for impl_name in st.implements {
		if impl_name == target || impl_name.all_after_last('__') == target {
			return true
		}
	}
	return false
}

// ownership_type_display returns a short, user-facing name for a type, used in
// the "move occurs because `x` has type `T`" diagnostic.
fn ownership_type_display(t Type) string {
	name := t.name()
	if name.len > 0 {
		return name
	}
	return '<unknown>'
}

// ownership_mark_owned records that the variable `name` holds an owned value
// of type `typ`. Called when:
//   - a non-Copy value is created (struct/array/map literal, fn returning
//     non-Copy, etc.)
//   - a string is explicitly upgraded via `.to_owned()`
//
// If `typ` implements the `Drop` interface, this also schedules a destructor
// call to be emitted at scope exit (see `ownership_schedule_drop`).
fn (mut c Checker) ownership_mark_owned(name string, typ Type, pos token.Pos) {
	if name.len == 0 || name == '_' {
		return
	}
	c.owned_vars[name] = pos
	c.owned_var_types[name] = ownership_type_display(typ)
	c.ownership_schedule_drop(name, typ, pos)
}

// is_drop_type reports whether `t` (or its alias / option / result base) is a
// struct that declares `implements Drop`. Drop types must provide a
// `drop(mut self)` method; the checker schedules a call to it at scope exit
// for every owned, non-moved binding via `drop_schedule`.
pub fn (c &Checker) is_drop_type(t Type) bool {
	return c.is_drop_type_impl(t, 0)
}

fn (c &Checker) is_drop_type_impl(t Type, depth int) bool {
	if depth > 16 {
		return false
	}
	match t {
		Alias {
			return c.is_drop_type_impl(t.base_type, depth + 1)
		}
		OptionType {
			return c.is_drop_type_impl(t.base_type, depth + 1)
		}
		ResultType {
			return c.is_drop_type_impl(t.base_type, depth + 1)
		}
		Struct {
			return c.struct_marker_matches(t, 'Drop')
		}
		else {
			return false
		}
	}
}

// type_has_drop_method reports whether the type (or its base after unwrapping
// alias / option / result) has a registered `drop(...)` method.
fn (c &Checker) type_has_drop_method(t Type) bool {
	mut cur := t
	for _ in 0 .. 16 {
		match cur {
			Alias {
				cur = (cur as Alias).base_type
				continue
			}
			OptionType {
				cur = (cur as OptionType).base_type
				continue
			}
			ResultType {
				cur = (cur as ResultType).base_type
				continue
			}
			else {
				break
			}
		}
	}
	name := cur.name()
	if name.len == 0 {
		return false
	}
	c.env.lookup_method(name, 'drop') or { return false }
	return true
}

// DropEntry records a scheduled scope-exit destructor call. The codegen
// integration (a follow-up) reads `Checker.drop_schedule` and emits
// `var_name.drop()` immediately before each binding's owning scope ends.
pub struct DropEntry {
pub:
	var_name  string    // local binding to drop
	type_name string    // displayed type, for diagnostics / debug output
	fn_name   string    // enclosing function (key into drop_schedule)
	pos       token.Pos // position where the binding became owned
}

// ownership_schedule_drop records that `name` of type `typ` needs a
// destructor call before its enclosing scope exits. No-op if the type does
// not implement `Drop`. Safe to call multiple times for the same binding —
// only the first registration sticks (later reassignments don't add new
// drop entries, since the old value is overwritten and dropped by the
// assignment-time hook).
fn (mut c Checker) ownership_schedule_drop(name string, typ Type, pos token.Pos) {
	if c.ownership_cur_fn.len == 0 {
		return
	}
	if !c.is_drop_type(typ) {
		return
	}
	existing := c.drop_schedule[c.ownership_cur_fn] or { []DropEntry{} }
	for entry in existing {
		if entry.var_name == name {
			return
		}
	}
	mut list := existing.clone()
	list << DropEntry{
		var_name:  name
		type_name: ownership_type_display(typ)
		fn_name:   c.ownership_cur_fn
		pos:       pos
	}
	c.drop_schedule[c.ownership_cur_fn] = list
}

// ownership_validate_drop_impls verifies that every struct declaring
// `implements Drop` actually provides a `drop(mut self)` method. Runs after
// `preregister_all_fn_signatures` (so methods are visible) and before the
// pending fn bodies are walked, so the diagnostic fires once per build.
fn (mut c Checker) ownership_validate_drop_impls() {
	mut to_check := []Struct{}
	rlock c.env.scopes {
		for _, mod_scope in c.env.scopes {
			for _, obj in mod_scope.objects {
				if obj is Type {
					typ := obj as Type
					if typ is Struct {
						st := typ as Struct
						if c.struct_marker_matches(st, 'Drop') {
							to_check << st
						}
					}
				}
			}
		}
	}
	for st in to_check {
		if !c.type_has_drop_method(Type(st)) {
			pos := c.ownership_drop_decl_positions[st.name] or { token.Pos{} }
			c.ownership_emit_drop_method_missing(st.name, pos)
		}
	}
}

// ownership_emit_drop_method_missing prints the "missing drop method"
// diagnostic. Kept separate so tests / callers can call it directly with a
// synthesized position when needed.
fn (mut c Checker) ownership_emit_drop_method_missing(struct_name string, pos token.Pos) {
	if pos.id > 0 {
		file := c.file_set.file(pos)
		errors.error('struct `${struct_name}` implements `Drop` but does not provide a `drop(mut self)` method', errors.details(file,
			file.position(pos), 2), .error, file.position(pos))
	} else {
		eprintln('error: struct `${struct_name}` implements `Drop` but does not provide a `drop(mut self)` method')
	}
	eprintln('  --> add `fn (mut s ${struct_name}) drop() { ... }` to satisfy the Drop contract')
	exit(1)
}

// ownership_is_borrow_or_ref_rhs returns true if the RHS expression is itself
// a borrow (`&x`) or pointer construction that should not produce an owned
// LHS even when the LHS's static type is non-Copy. This is used to avoid
// marking `r := &foo` as owning a new value.
fn ownership_is_borrow_or_ref_rhs(rhs ast.Expr) bool {
	if rhs is ast.PrefixExpr {
		return (rhs as ast.PrefixExpr).op == .amp
	}
	if rhs is ast.ModifierExpr {
		return (rhs as ast.ModifierExpr).kind == .key_mut
	}
	return false
}

// ownership_expr_ident_name extracts the identifier name from an expression,
// unwrapping ModifierExpr if present. Returns empty string if not an ident.
fn ownership_expr_ident_name(expr ast.Expr) string {
	match expr {
		ast.Ident {
			return expr.name
		}
		ast.ModifierExpr {
			if expr.expr is ast.Ident {
				return (expr.expr as ast.Ident).name
			}
		}
		else {}
	}

	return ''
}

// ownership_receiver_short_name extracts the short type name from a receiver
// type expression. Returns empty string if the receiver isn't a recognisable
// named type. Handles plain idents, generic instantiations (`Foo[T]` via
// IndexExpr / GenericArgs), and module-qualified selectors (`pkg.Foo`).
fn ownership_receiver_short_name(typ ast.Expr) string {
	match typ {
		ast.Ident {
			return typ.name
		}
		ast.IndexExpr {
			return ownership_receiver_short_name(typ.lhs)
		}
		ast.GenericArgs {
			return ownership_receiver_short_name(typ.lhs)
		}
		ast.SelectorExpr {
			return typ.rhs.name
		}
		else {
			return ''
		}
	}
}

// ownership_prescan_value_receivers populates `ownership_value_receiver_methods`
// with every method declared with a by-value receiver. A method's receiver is
// considered by-value when it's neither marked `mut` nor prefixed with `&`.
// Calls to such methods on owned vars MOVE the receiver — consuming-self
// semantics, matching Rust's `fn(self)` vs `fn(&self)`.
//
// Borrow-receiver (`(s &Foo)`) and mut-receiver (`(mut s Foo)`) methods are
// borrows and never move the receiver.
fn (mut c Checker) ownership_prescan_value_receivers() {
	for pending in c.pending_fn_bodies {
		decl := pending.decl
		if !decl.is_method {
			continue
		}
		if decl.receiver.is_mut {
			continue
		}
		if decl.receiver.typ is ast.PrefixExpr {
			pe := decl.receiver.typ as ast.PrefixExpr
			if pe.op == .amp {
				continue
			}
		}
		recv_name := ownership_receiver_short_name(decl.receiver.typ)
		if recv_name.len == 0 {
			continue
		}
		short := recv_name.all_after_last('__')
		key := '${short}__${decl.name}'
		c.ownership_value_receiver_methods[key] = true
	}
}

// ownership_check_method_call fires when a method call is made on an owned
// variable and the callee was declared with a by-value receiver. The receiver
// is consumed exactly like a function-call argument move: marked moved, with a
// diagnostic if it's currently borrowed.
//
// No-op when:
//   * the call isn't a method call (no SelectorExpr)
//   * the receiver isn't a tracked local
//   * the method has a `&` / `mut` receiver (borrow, not move)
//   * the type / method pair isn't in the prescan registry
//   * the receiver's STATIC type doesn't `implements Owned` (this is why a
//     `string` value tracked via `.to_owned()` isn't consumed by built-in
//     value-receiver methods like `clone`/`split` — those stay auto-clone)
fn (mut c Checker) ownership_check_method_call(expr ast.CallExpr) {
	if expr.lhs !is ast.SelectorExpr {
		return
	}
	sel := expr.lhs as ast.SelectorExpr
	recv_name := ownership_expr_ident_name(sel.lhs)
	if recv_name.len == 0 || recv_name !in c.owned_vars {
		return
	}
	type_display := c.owned_var_types[recv_name] or { return }
	short_type := type_display.all_after_last('__')
	method_name := sel.rhs.name
	key := '${short_type}__${method_name}'
	if key !in c.ownership_value_receiver_methods {
		return
	}
	// Only apply consuming-self to types that explicitly opt into `Owned`.
	// This guards stdlib types whose value-receiver methods (`string.clone`,
	// `string.split`, ...) are auto-clone in V's semantics — they must not
	// trip the move tracker even when the value happens to be owned-tracked.
	recv_type := c.lookup_type_in_scope_chain(short_type) or { return }
	if !c.is_owned_type(recv_type) {
		return
	}
	// Cannot move while borrowed.
	if recv_name in c.borrowed_vars {
		borrows := c.borrowed_vars[recv_name]
		if borrows.len > 0 {
			borrow := borrows[0]
			file := c.file_set.file(expr.pos)
			borrow_file := c.file_set.file(borrow.pos)
			borrow_position := borrow_file.position(borrow.pos)
			errors.error('cannot move `${recv_name}` because it is borrowed', errors.details(file,
				file.position(expr.pos), 2), .error, file.position(expr.pos))
			eprintln('  --> `${recv_name}` is borrowed by `${borrow.borrower}` at ${borrow_position}')
			exit(1)
		}
	}
	type_name := c.owned_var_types[recv_name] or { 'string' }
	c.moved_vars[recv_name] = MovedVar{
		moved_to:   method_name
		move_pos:   expr.pos
		is_fn_call: true
		fn_name:    method_name
		type_name:  type_name
	}
}
