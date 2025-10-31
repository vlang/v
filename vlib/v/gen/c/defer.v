// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.ast

@[inline]
fn is_same_scope(a &ast.Scope, b &ast.Scope) bool {
	return a.start_pos == b.start_pos && a.end_pos == b.end_pos
}

fn (mut g Gen) write_defer_stmts(scope &ast.Scope, lookup bool) {
	g.indent++
	for i := g.defer_stmts.len - 1; i >= 0; i-- {
		defer_stmt := g.defer_stmts[i]
		if !((lookup && defer_stmt.scope.start_pos < scope.start_pos
			&& defer_stmt.scope.end_pos > scope.end_pos)
			|| is_same_scope(defer_stmt.scope, scope)) {
			// generate only `defer`s from the current scope
			continue
		}
		g.writeln('{ // defer begin')
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
