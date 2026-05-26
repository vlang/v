// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

// Tests for the Cursor API live alongside the implementation since they
// operate on hand-built FlatAst fixtures — building them via the parser
// would introduce a circular dep (parser depends on ast). The fixtures
// mirror the shape that parse_file emits for a few representative inputs.

// build_minimal_file constructs a FlatAst with one file containing:
//   module foo
//   fn add(a int, b int) int { return a + b }
// using the FlatBuilder API directly. We then exercise Cursor over it.
fn build_minimal_file() FlatAst {
	mut b := new_flat_builder()
	src := File{
		name:  'inline_minimal.v'
		mod:   'foo'
		stmts: [
			Stmt(ModuleStmt{
				name: 'foo'
			}),
			Stmt(FnDecl{
				name:  'add'
				typ:   FnType{
					params:      [
						Parameter{
							name: 'a'
							typ:  Expr(Ident{
								name: 'int'
							})
						},
						Parameter{
							name: 'b'
							typ:  Expr(Ident{
								name: 'int'
							})
						},
					]
					return_type: Expr(Ident{
						name: 'int'
					})
				}
				stmts: [
					Stmt(ReturnStmt{
						exprs: [
							Expr(InfixExpr{
								op:  .plus
								lhs: Expr(Ident{
									name: 'a'
								})
								rhs: Expr(Ident{
									name: 'b'
								})
							}),
						]
					}),
				]
			}),
		]
	}
	b.append_file(src)
	return b.flat
}

fn test_file_cursor_basics() {
	flat := build_minimal_file()
	assert flat.files.len == 1
	fc := flat.file_cursor(0)
	assert fc.name() == 'inline_minimal.v'
	assert fc.mod() == 'foo'
	root := fc.root()
	assert root.is_valid()
	assert root.kind() == .file
}

fn test_file_cursor_stmts_iteration() {
	flat := build_minimal_file()
	fc := flat.file_cursor(0)
	stmts := fc.stmts()
	// module decl + fn decl
	assert stmts.len() == 2
	assert stmts.at(0).kind() == .stmt_module
	assert stmts.at(0).name() == 'foo'
	assert stmts.at(1).kind() == .stmt_fn_decl
	assert stmts.at(1).name() == 'add'
}

fn test_fn_decl_flags_and_body() {
	flat := build_minimal_file()
	fc := flat.file_cursor(0)
	fn_cur := fc.stmts().at(1)
	assert fn_cur.kind() == .stmt_fn_decl
	// is_method/is_public/is_static all false for plain `fn add(...)`
	assert !fn_cur.flag(flag_is_method)
	assert !fn_cur.flag(flag_is_public)
	assert !fn_cur.flag(flag_is_static)
	// edge layout per read_stmt: 0=receiver, 1=typ, 2=attrs, 3=stmts(aux_list)
	stmts := fn_cur.list_at(3)
	assert stmts.len() == 1
	ret := stmts.at(0)
	assert ret.kind() == .stmt_return
}

fn test_return_stmt_exprs_via_edges() {
	flat := build_minimal_file()
	fc := flat.file_cursor(0)
	ret := fc.stmts().at(1).list_at(3).at(0)
	assert ret.kind() == .stmt_return
	// ReturnStmt stores its exprs as direct edges of the parent (not aux_list)
	assert ret.edge_count() == 1
	infix := ret.edge(0)
	assert infix.kind() == .expr_infix
	// InfixExpr edges: 0=lhs, 1=rhs (per read_expr)
	assert infix.edge_count() == 2
	lhs := infix.edge(0)
	rhs := infix.edge(1)
	assert lhs.kind() == .expr_ident
	assert lhs.name() == 'a'
	assert rhs.kind() == .expr_ident
	assert rhs.name() == 'b'
}

fn test_invalid_cursor_sentinels() {
	flat := build_minimal_file()
	fc := flat.file_cursor(0)
	bogus := fc.stmts().at(99) // out of range
	assert !bogus.is_valid()
	bogus_edge := fc.root().edge(99) // out of range
	assert !bogus_edge.is_valid()
	empty_list := fc.root().list_at(99)
	assert empty_list.len() == 0
}

// count_unique_kinds_via_cursor walks the whole graph rooted at fc through
// edges with dedup (the FlatBuilder shares aux_list nodes for empty lists,
// so a naive DFS revisits them). Used to cross-check that Cursor traversal
// reaches every reachable node in the FlatAst.
fn count_unique_kinds_via_cursor(fc FileCursor) (map[string]int, int) {
	flat := fc.flat
	mut seen := []bool{len: flat.nodes.len}
	mut out := map[string]int{}
	mut stack := []Cursor{cap: 64}
	mut unique := 0
	stack << fc.root()
	for stack.len > 0 {
		c := stack.pop()
		if !c.is_valid() || seen[c.id] {
			continue
		}
		seen[c.id] = true
		unique++
		out[c.kind().str()]++
		for i in 0 .. c.edge_count() {
			stack << c.edge(i)
		}
	}
	return out, unique
}

fn test_cursor_walk_matches_reachable_count() {
	flat := build_minimal_file()
	_, unique := count_unique_kinds_via_cursor(flat.file_cursor(0))
	// Walking from the single file root via Cursor must reach the same
	// set of unique nodes as the reference reachability walker.
	assert unique == flat.count_reachable_nodes()
}
