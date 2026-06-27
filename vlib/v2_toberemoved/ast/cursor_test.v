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

fn test_fn_decl_signature_from_cursor() {
	flat := build_minimal_file()
	fn_cur := flat.file_cursor(0).stmts().at(1)
	decl := fn_cur.fn_decl_signature()
	assert decl.name == 'add'
	assert !decl.is_method
	assert decl.stmts.len == 0
	assert decl.typ.params.len == 2
	assert decl.typ.params[0].name == 'a'
	assert decl.typ.params[0].typ is Ident
	assert (decl.typ.params[0].typ as Ident).name == 'int'
	assert decl.typ.params[1].name == 'b'
	assert decl.typ.return_type is Ident
	assert (decl.typ.return_type as Ident).name == 'int'
}

fn test_fn_decl_from_cursor_reads_body() {
	flat := build_minimal_file()
	fn_cur := flat.file_cursor(0).stmts().at(1)
	decl := fn_cur.fn_decl()
	assert decl.name == 'add'
	assert decl.stmts.len == 1
	assert decl.stmts[0] is ReturnStmt
}

fn test_stmt_and_expr_escape_hatches_from_cursor() {
	flat := build_minimal_file()
	ret_cur := flat.file_cursor(0).stmts().at(1).list_at(3).at(0)
	ret_stmt := ret_cur.stmt()
	assert ret_stmt is ReturnStmt
	infix_expr := ret_cur.edge(0).expr()
	assert infix_expr is InfixExpr
	assert (infix_expr as InfixExpr).op == .plus
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

fn test_type_expr_from_cursor_reads_signature_types() {
	mut b := new_flat_builder()
	src := File{
		name:  'inline_type_expr.v'
		mod:   'foo'
		stmts: [
			Stmt(ModuleStmt{
				name: 'foo'
			}),
			Stmt(FnDecl{
				name: 'use_box'
				typ:  FnType{
					params:      [
						Parameter{
							name: 'items'
							typ:  Expr(Type(ArrayType{
								elem_type: Expr(GenericArgs{
									lhs:  Expr(Ident{
										name: 'Box'
									})
									args: [
										Expr(Ident{
											name: 'int'
										}),
									]
								})
							}))
						},
					]
					return_type: Expr(Type(OptionType{
						base_type: Expr(Ident{
							name: 'string'
						})
					}))
				}
			}),
		]
	}
	b.append_file(src)
	fn_cur := b.flat.file_cursor(0).stmts().at(1)
	fn_type := fn_cur.edge(1)
	params := fn_type.list_at(1)
	param_typ := params.at(0).edge(0).type_expr()
	assert param_typ is Type
	array_typ := param_typ as Type
	assert array_typ is ArrayType
	array_expr := array_typ as ArrayType
	assert array_expr.elem_type is GenericArgs
	generic := array_expr.elem_type as GenericArgs
	assert generic.lhs is Ident
	assert (generic.lhs as Ident).name == 'Box'
	assert generic.args.len == 1
	assert generic.args[0] is Ident
	assert (generic.args[0] as Ident).name == 'int'
	return_typ := fn_type.edge(2).type_expr()
	assert return_typ is Type
	option_typ := return_typ as Type
	assert option_typ is OptionType
	option_expr := option_typ as OptionType
	assert option_expr.base_type is Ident
	assert (option_expr.base_type as Ident).name == 'string'
}

fn test_decl_readers_from_cursor() {
	mut b := new_flat_builder()
	src := File{
		name:  'inline_decl_readers.v'
		mod:   'foo'
		stmts: [
			Stmt(ModuleStmt{
				name: 'foo'
			}),
			Stmt(ConstDecl{
				fields: [
					FieldInit{
						name:  'answer'
						value: Expr(BasicLiteral{
							kind:  .number
							value: '42'
						})
					},
				]
			}),
			Stmt(EnumDecl{
				name:       'Mode'
				attributes: [
					Attribute{
						name: 'flag'
					},
				]
				fields:     [
					FieldDecl{
						name:  'read'
						value: Expr(BasicLiteral{
							kind:  .number
							value: '1'
						})
					},
				]
			}),
			Stmt(StructDecl{
				name:       'Person'
				attributes: [
					Attribute{
						name: 'heap'
					},
				]
				fields:     [
					FieldDecl{
						name:       'name'
						typ:        Expr(Ident{
							name: 'string'
						})
						value:      Expr(StringLiteral{
							value: 'unknown'
						})
						attributes: [
							Attribute{
								name:  'json'
								value: Expr(StringLiteral{
									value: 'full_name'
								})
							},
						]
						is_public:  true
					},
				]
			}),
		]
	}
	b.append_file(src)
	stmts := b.flat.file_cursor(0).stmts()
	const_decl := stmts.at(1).const_decl()
	assert const_decl.fields.len == 1
	assert const_decl.fields[0].name == 'answer'
	assert const_decl.fields[0].value is BasicLiteral
	assert (const_decl.fields[0].value as BasicLiteral).value == '42'
	enum_decl := stmts.at(2).enum_decl(true)
	assert enum_decl.name == 'Mode'
	assert enum_decl.attributes.has('flag')
	assert enum_decl.fields.len == 1
	assert enum_decl.fields[0].value is BasicLiteral
	assert (enum_decl.fields[0].value as BasicLiteral).value == '1'
	struct_decl := stmts.at(3).struct_decl()
	assert struct_decl.name == 'Person'
	assert struct_decl.attributes.has('heap')
	assert struct_decl.fields.len == 1
	assert struct_decl.fields[0].name == 'name'
	assert struct_decl.fields[0].is_public
	assert struct_decl.fields[0].typ is Ident
	assert (struct_decl.fields[0].typ as Ident).name == 'string'
	assert struct_decl.fields[0].value is StringLiteral
	assert (struct_decl.fields[0].value as StringLiteral).value == 'unknown'
	assert struct_decl.fields[0].attributes.len == 1
	assert struct_decl.fields[0].attributes[0].name == 'json'
	assert struct_decl.fields[0].attributes[0].value is StringLiteral
	assert (struct_decl.fields[0].attributes[0].value as StringLiteral).value == 'full_name'
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
