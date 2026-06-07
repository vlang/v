// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import v2.token

// Cursor is a lightweight, by-value handle pointing at one FlatNode inside a
// FlatAst. Consumers walk the AST via `match c.kind() {}` and descend via
// `c.edge(i)` / `c.list_at(i)` without rehydrating to ast.Stmt / ast.Expr.
//
// A Cursor is 16 bytes: an &FlatAst pointer plus a FlatNodeId. It is safe to
// copy, store in arrays, and pass by value. An "invalid" Cursor (id < 0) is
// used as a sentinel where the legacy AST would use empty_stmt / empty_expr.
pub struct Cursor {
pub:
	flat &FlatAst = unsafe { nil }
	id   FlatNodeId
}

@[inline]
pub fn (c Cursor) is_valid() bool {
	return c.flat != unsafe { nil } && c.id >= 0 && c.id < c.flat.nodes.len
}

@[inline]
pub fn (c Cursor) kind() FlatNodeKind {
	return c.flat.nodes[c.id].kind
}

@[inline]
pub fn (c Cursor) pos() token.Pos {
	return c.flat.nodes[c.id].pos
}

@[inline]
pub fn (c Cursor) flags() u8 {
	return c.flat.nodes[c.id].flags
}

@[inline]
pub fn (c Cursor) flag(bit u8) bool {
	return (c.flat.nodes[c.id].flags & bit) != 0
}

@[inline]
pub fn (c Cursor) aux() u16 {
	return c.flat.nodes[c.id].aux
}

@[inline]
pub fn (c Cursor) extra_int() int {
	return c.flat.nodes[c.id].extra
}

// extra_str interprets the node's `extra` field as an interned string id.
// Use only on kinds where the schema documents `extra` as a string slot
// (e.g. stmt_directive's value, stmt_import's alias).
@[inline]
pub fn (c Cursor) extra_str() string {
	return c.flat.string_at(c.flat.nodes[c.id].extra)
}

// name returns the interned primary string (FlatNode.name_id) for this node.
// Most kinds use this for identifiers, type names, fn names, etc.
@[inline]
pub fn (c Cursor) name() string {
	return c.flat.string_at(c.flat.nodes[c.id].name_id)
}

@[inline]
pub fn (c Cursor) name_id() int {
	return c.flat.nodes[c.id].name_id
}

// ident reads an expr_ident cursor directly into an Ident.
pub fn (c Cursor) ident() Ident {
	if !c.is_valid() || c.kind() != .expr_ident {
		return Ident{}
	}
	return Ident{
		pos:  c.pos()
		name: c.name()
	}
}

// import_stmt reads a stmt_import cursor directly into an ImportStmt.
pub fn (c Cursor) import_stmt() ImportStmt {
	if !c.is_valid() || c.kind() != .stmt_import {
		return ImportStmt{}
	}
	mut symbols := []Expr{cap: c.edge_count()}
	for i in 0 .. c.edge_count() {
		sym := c.edge(i)
		if sym.kind() == .expr_ident {
			symbols << Expr(sym.ident())
		}
	}
	return ImportStmt{
		name:       c.name()
		alias:      c.extra_str()
		is_aliased: c.flag(flag_is_aliased)
		symbols:    symbols
	}
}

// fn_decl_signature reads a stmt_fn_decl cursor into a body-less FnDecl. This
// mirrors FlatAst.decode_fn_decl_signature without going through FlatReader.
pub fn (c Cursor) fn_decl_signature() FnDecl {
	if !c.is_valid() || c.kind() != .stmt_fn_decl {
		return FnDecl{}
	}
	return FnDecl{
		attributes: attrs_from_cursor(c.list_at(2))
		is_public:  c.flag(flag_is_public)
		is_method:  c.flag(flag_is_method)
		is_static:  c.flag(flag_is_static)
		receiver:   parameter_from_cursor(c.edge(0))
		language:   unsafe { Language(int(c.aux())) }
		name:       c.name()
		typ:        fn_type_from_cursor(c.edge(1))
		pos:        c.pos()
	}
}

// type_expr reads a type-expression cursor into the legacy Expr shape used by
// signature consumers. It is intentionally narrower than FlatReader.read_expr:
// non-type payloads such as field defaults or statement bodies stay omitted.
pub fn (c Cursor) type_expr() Expr {
	if !c.is_valid() {
		return empty_expr
	}
	match c.kind() {
		.expr_empty {
			return empty_expr
		}
		.expr_basic_literal {
			return Expr(BasicLiteral{
				kind:  unsafe { token.Token(int(c.aux())) }
				value: c.name()
				pos:   c.pos()
			})
		}
		.expr_ident {
			return Expr(c.ident())
		}
		.expr_lifetime {
			return Expr(LifetimeExpr{
				name: c.name()
				pos:  c.pos()
			})
		}
		.expr_modifier {
			return Expr(ModifierExpr{
				kind: unsafe { token.Token(int(c.aux())) }
				expr: c.edge(0).type_expr()
				pos:  c.pos()
			})
		}
		.expr_prefix {
			return Expr(PrefixExpr{
				op:   unsafe { token.Token(int(c.aux())) }
				expr: c.edge(0).type_expr()
				pos:  c.pos()
			})
		}
		.expr_selector {
			rhs := c.edge(1)
			return Expr(SelectorExpr{
				lhs: c.edge(0).type_expr()
				rhs: Ident{
					name: rhs.name()
					pos:  rhs.pos()
				}
				pos: c.pos()
			})
		}
		.expr_generic_args {
			return Expr(GenericArgs{
				lhs:  c.edge(0).type_expr()
				args: type_exprs_from_edges(c, 1)
				pos:  c.pos()
			})
		}
		.expr_generic_arg_or_index {
			return Expr(GenericArgOrIndexExpr{
				lhs:  c.edge(0).type_expr()
				expr: c.edge(1).type_expr()
				pos:  c.pos()
			})
		}
		.typ_anon_struct {
			return Expr(Type(AnonStructType{
				generic_params: c.list_at(0).type_exprs()
				embedded:       c.list_at(1).type_exprs()
				fields:         field_decl_type_list(c.list_at(2))
			}))
		}
		.typ_array_fixed {
			return Expr(Type(ArrayFixedType{
				len:       c.edge(0).type_expr()
				elem_type: c.edge(1).type_expr()
			}))
		}
		.typ_array {
			return Expr(Type(ArrayType{
				elem_type: c.edge(0).type_expr()
			}))
		}
		.typ_channel {
			return Expr(Type(ChannelType{
				cap:       c.edge(0).type_expr()
				elem_type: c.edge(1).type_expr()
			}))
		}
		.typ_fn {
			return Expr(Type(FnType{
				generic_params: c.list_at(0).type_exprs()
				params:         parameter_list_from_cursor(c.list_at(1))
				return_type:    c.edge(2).type_expr()
			}))
		}
		.typ_generic {
			return Expr(Type(GenericType{
				name:   c.edge(0).type_expr()
				params: type_exprs_from_edges(c, 1)
			}))
		}
		.typ_map {
			return Expr(Type(MapType{
				key_type:   c.edge(0).type_expr()
				value_type: c.edge(1).type_expr()
			}))
		}
		.typ_nil {
			return Expr(Type(NilType{}))
		}
		.typ_none {
			return Expr(Type(NoneType{}))
		}
		.typ_option {
			return Expr(Type(OptionType{
				base_type: c.edge(0).type_expr()
			}))
		}
		.typ_pointer {
			return Expr(Type(PointerType{
				base_type: c.edge(0).type_expr()
				lifetime:  c.name()
			}))
		}
		.typ_result {
			return Expr(Type(ResultType{
				base_type: c.edge(0).type_expr()
			}))
		}
		.typ_thread {
			return Expr(Type(ThreadType{
				elem_type: c.edge(0).type_expr()
			}))
		}
		.typ_tuple {
			return Expr(Type(TupleType{
				types: type_exprs_from_edges(c, 0)
			}))
		}
		else {
			return empty_expr
		}
	}
}

@[inline]
pub fn (c Cursor) edge_count() int {
	return c.flat.nodes[c.id].edge_count
}

// edge returns a Cursor over the i-th direct child edge of this node. Out-of-
// range indices return an invalid Cursor (id == invalid_flat_node_id), which
// callers can detect via `c.is_valid()`. This matches FlatReader's behaviour
// where `r.edge(n, i)` may return invalid_flat_node_id for missing slots.
@[inline]
pub fn (c Cursor) edge(i int) Cursor {
	return Cursor{
		flat: c.flat
		id:   c.flat.child_at(c.id, i)
	}
}

// list_at treats edge `i` as a reference to an `aux_list` node and returns a
// CursorList over its children. The flat schema represents list-typed fields
// (e.g. StructDecl.fields, FnDecl.stmts, EnumDecl.attributes) as a single
// edge to an aux_list whose children are the actual list items. Use `edge()`
// for fields stored as direct child edges of the parent (e.g. AssignStmt's
// LHS/RHS, AssertStmt's expr/extra).
@[inline]
pub fn (c Cursor) list_at(edge_i int) CursorList {
	return CursorList{
		flat:      c.flat
		parent_id: c.flat.child_at(c.id, edge_i)
	}
}

// CursorList is a view over the children of an `aux_list` node. It is the
// flat equivalent of `[]ast.Stmt` / `[]ast.Expr` / `[]FieldDecl` etc., except
// no slice is materialised — `at(i)` decodes one child at a time.
pub struct CursorList {
pub:
	flat      &FlatAst = unsafe { nil }
	parent_id FlatNodeId
}

@[inline]
pub fn (l CursorList) len() int {
	if l.flat == unsafe { nil } || l.parent_id < 0 || l.parent_id >= l.flat.nodes.len {
		return 0
	}
	return l.flat.nodes[l.parent_id].edge_count
}

@[inline]
pub fn (l CursorList) at(i int) Cursor {
	return Cursor{
		flat: l.flat
		id:   l.flat.child_at(l.parent_id, i)
	}
}

// type_exprs reads every item in a cursor list through Cursor.type_expr.
pub fn (l CursorList) type_exprs() []Expr {
	mut out := []Expr{cap: l.len()}
	for i in 0 .. l.len() {
		out << l.at(i).type_expr()
	}
	return out
}

fn attrs_from_cursor(list CursorList) []Attribute {
	mut out := []Attribute{cap: list.len()}
	for i in 0 .. list.len() {
		attr := list.at(i)
		if !attr.is_valid() {
			continue
		}
		out << Attribute{
			name: attr.name()
		}
	}
	return out
}

fn fn_type_from_cursor(c Cursor) FnType {
	if !c.is_valid() || c.kind() != .typ_fn {
		return FnType{}
	}
	return FnType{
		generic_params: c.list_at(0).type_exprs()
		params:         parameter_list_from_cursor(c.list_at(1))
		return_type:    c.edge(2).type_expr()
	}
}

fn type_exprs_from_edges(c Cursor, start int) []Expr {
	if !c.is_valid() || start >= c.edge_count() {
		return []Expr{}
	}
	mut out := []Expr{cap: c.edge_count() - start}
	for i in start .. c.edge_count() {
		out << c.edge(i).type_expr()
	}
	return out
}

fn parameter_from_cursor(c Cursor) Parameter {
	if !c.is_valid() {
		return Parameter{}
	}
	return Parameter{
		name:   c.name()
		typ:    c.edge(0).type_expr()
		is_mut: c.flag(flag_is_mut)
		pos:    c.pos()
	}
}

fn parameter_list_from_cursor(list CursorList) []Parameter {
	mut out := []Parameter{cap: list.len()}
	for i in 0 .. list.len() {
		out << parameter_from_cursor(list.at(i))
	}
	return out
}

fn field_decl_type_list(list CursorList) []FieldDecl {
	mut out := []FieldDecl{cap: list.len()}
	for i in 0 .. list.len() {
		field := list.at(i)
		if !field.is_valid() {
			continue
		}
		out << FieldDecl{
			name:                field.name()
			typ:                 field.edge(0).type_expr()
			is_public:           field.flag(flag_is_public)
			is_mut:              field.flag(flag_is_mut)
			is_module_mut:       field.flag(flag_field_is_module_mut)
			is_interface_method: field.flag(flag_field_is_interface_method)
		}
	}
	return out
}

// FileCursor is a typed wrapper over a FlatFile entry. It exposes the
// file-level metadata (name, mod, selector_names) and the three top-level
// child lists (attributes, imports, stmts) without rehydrating a full
// ast.File.
pub struct FileCursor {
pub:
	flat &FlatAst = unsafe { nil }
	idx  int // index into flat.files
}

// file_cursor returns a FileCursor over the i-th FlatFile in this FlatAst.
@[inline]
pub fn (flat &FlatAst) file_cursor(idx int) FileCursor {
	return FileCursor{
		flat: unsafe { flat }
		idx:  idx
	}
}

// file_cursors returns one FileCursor per FlatFile. Allocates an int-sized
// array; for hot loops prefer `for i in 0 .. flat.files.len { flat.file_cursor(i) }`.
pub fn (flat &FlatAst) file_cursors() []FileCursor {
	mut out := []FileCursor{cap: flat.files.len}
	for i in 0 .. flat.files.len {
		out << flat.file_cursor(i)
	}
	return out
}

@[inline]
pub fn (fc FileCursor) flat_file() FlatFile {
	return fc.flat.files[fc.idx]
}

// root returns a Cursor positioned at this file's root FlatNode (kind == .file).
@[inline]
pub fn (fc FileCursor) root() Cursor {
	return Cursor{
		flat: fc.flat
		id:   fc.flat.files[fc.idx].file_id
	}
}

@[inline]
pub fn (fc FileCursor) name() string {
	return fc.flat.string_at(fc.flat.files[fc.idx].name_idx)
}

@[inline]
pub fn (fc FileCursor) mod() string {
	return fc.flat.string_at(fc.flat.files[fc.idx].mod_idx)
}

@[inline]
pub fn (fc FileCursor) selector_names() map[int]string {
	return fc.flat.files[fc.idx].selector_names
}

// attrs returns the file's top-level attribute list (edge 0 of the file node).
@[inline]
pub fn (fc FileCursor) attrs() CursorList {
	return fc.root().list_at(0)
}

// imports returns the file's top-level import list (edge 1 of the file node).
@[inline]
pub fn (fc FileCursor) imports() CursorList {
	return fc.root().list_at(1)
}

// stmts returns the file's top-level statement list (edge 2 of the file node).
@[inline]
pub fn (fc FileCursor) stmts() CursorList {
	return fc.root().list_at(2)
}
