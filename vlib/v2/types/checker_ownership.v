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
		errors.error('use of moved value: `${name}`', errors.details(file, file.position(pos),
			2), .error, file.position(pos))
		eprintln('  --> move occurs because `${name}` has type `string`, which does not implement the `Copy` interface')
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
		// Move: mark rhs as moved, mark lhs as owned
		c.moved_vars[rhs_name] = MovedVar{
			moved_to: lhs_name
			move_pos: assign_pos
		}
		c.owned_vars[lhs_name] = assign_pos
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
	c.moved_vars[name] = MovedVar{
		moved_to:      target
		move_pos:      pos
		suggest_clone: false
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
			c.moved_vars[arg_name] = MovedVar{
				moved_to:   fn_name
				move_pos:   expr.pos
				is_fn_call: true
				fn_name:    fn_name
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
			c.moved_vars[name] = MovedVar{
				moved_to:   c.ownership_cur_fn
				move_pos:   c.owned_vars[name] // use the owned position as fallback
				is_fn_call: true
				fn_name:    c.ownership_cur_fn
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
			c.owned_vars[lhs_name] = pos
			return true
		}
	}
	// Get function name
	fn_name := ownership_call_fn_name(call)
	// Check if calling a function that returns ownership
	if fn_name in c.ownership_fns {
		c.owned_vars[lhs_name] = pos
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
					c.owned_vars[lhs_name] = pos
					return true
				}
				if arg_name in c.moved_vars {
					info := c.moved_vars[arg_name]
					if info.is_fn_call && info.fn_name == fn_name {
						c.owned_vars[lhs_name] = pos
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
				errors.error('cannot borrow `${var_name}` as mutable more than once',
					errors.details(file, file.position(pos), 2), .error, file.position(pos))
			} else {
				errors.error('cannot borrow `${var_name}` as mutable because it is also borrowed as immutable',
					errors.details(file, file.position(pos), 2), .error, file.position(pos))
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
					errors.error('cannot borrow `${var_name}` as immutable because it is borrowed as mutable',
						errors.details(file, file.position(pos), 2), .error, file.position(pos))
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
		}
	}
}

// ownership_leave_fn cleans up ownership tracking when leaving a function body.
fn (mut c Checker) ownership_leave_fn(prev_fn string, prev_owned map[string]token.Pos, prev_moved map[string]MovedVar, prev_borrowed map[string][]BorrowInfo) {
	c.ownership_cur_fn = prev_fn
	c.owned_vars = prev_owned.clone()
	c.moved_vars = prev_moved.clone()
	c.borrowed_vars = prev_borrowed.clone()
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
