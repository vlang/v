// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.ast

fn (g &Gen) defer_flag_var(stmt &ast.DeferStmt) string {
	return '${g.last_fn_c_name}_defer_${stmt.idx_in_fn}'
}

fn (mut g Gen) write_defer_stmts(scope &ast.Scope, lookup bool) {
	g.indent++
	for i := g.defer_stmts.len - 1; i >= 0; i-- {
		defer_stmt := g.defer_stmts[i]
		is_scoped := defer_stmt.mode == .scoped && g.pref.scoped_defer
		if is_scoped {
			if !((lookup && defer_stmt.scope.start_pos < scope.start_pos
				&& defer_stmt.scope.end_pos > scope.end_pos)
				|| defer_stmt.scope == scope) {
				// generate only `defer`s from the current scope
				continue
			}
			g.writeln('{ // defer begin')
		} else {
			if scope != g.cur_fn.scope && !lookup {
				continue
			}
			g.writeln('if (${g.defer_flag_var(defer_stmt)}) { // defer begin')
		}
		if defer_stmt.ifdef.len > 0 {
			g.writeln(defer_stmt.ifdef)
			g.stmts(defer_stmt.stmts)
			g.writeln2('', '#endif')
		} else {
			g.stmts(defer_stmt.stmts)
		}
		g.writeln('} // defer end')
	}
	g.indent--
}

fn (mut g Gen) write_defer_stmts_when_needed(scope &ast.Scope, lookup bool) {
	// unlock all mutexes, in case we are in a lock statement. defers are not
	// allowed in lock statements.
	g.unlock_locks()
	if g.defer_stmts.len > 0 {
		g.write_defer_stmts(scope, lookup)
	}
	if g.defer_profile_code.len > 0 {
		g.writeln2('', '\t// defer_profile_code')
		g.writeln2(g.defer_profile_code, '')
	}
}
