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

// fn_decl reads a stmt_fn_decl cursor into a legacy FnDecl. Unlike
// fn_decl_signature, this materializes the body statements for consumers that
// still use legacy statement walkers internally.
pub fn (c Cursor) fn_decl() FnDecl {
	if !c.is_valid() || c.kind() != .stmt_fn_decl {
		return FnDecl{}
	}
	signature := c.fn_decl_signature()
	return FnDecl{
		attributes: signature.attributes
		is_public:  signature.is_public
		is_method:  signature.is_method
		is_static:  signature.is_static
		receiver:   signature.receiver
		language:   signature.language
		name:       signature.name
		typ:        signature.typ
		stmts:      c.list_at(3).stmts()
		pos:        signature.pos
	}
}

// stmt reads a cursor through FlatAst.decode_stmt. This is the escape hatch
// for legacy statement walkers; prefer cursor-specific readers when possible.
pub fn (c Cursor) stmt() Stmt {
	if !c.is_valid() {
		return empty_stmt
	}
	return c.flat.decode_stmt(c.id)
}

// expr reads a cursor through FlatAst.decode_expr. This is the escape hatch
// for legacy expression walkers; prefer cursor-specific readers when possible.
pub fn (c Cursor) expr() Expr {
	if !c.is_valid() {
		return empty_expr
	}
	return c.flat.decode_expr(c.id)
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

// for_body_list views a stmt_for node's body as a CursorList. The stmt_for flat
// layout is edges [init, cond, post, body_stmt_0, body_stmt_1, ...] — the body
// is trailing edges of the for-node itself, NOT a separate aux_list — so the
// body list is this node's children from edge index 3 onward.
@[inline]
pub fn (c Cursor) for_body_list() CursorList {
	return CursorList{
		flat:      c.flat
		parent_id: c.id
		offset:    3
	}
}

// CursorList is a view over the children of an `aux_list` node. It is the
// flat equivalent of `[]ast.Stmt` / `[]ast.Expr` / `[]FieldDecl` etc., except
// no slice is materialised — `at(i)` decodes one child at a time.
pub struct CursorList {
pub:
	flat      &FlatAst = unsafe { nil }
	parent_id FlatNodeId
	// offset skips the first `offset` child edges of `parent_id`. Default 0 (the
	// whole child list). Lets a trailing edge range be viewed as a list when the
	// items are direct child edges of a node rather than an aux_list — e.g. a
	// ForStmt body lives in edges [3..] of the stmt_for node itself (see
	// `Cursor.for_body_list`).
	offset int
}

@[inline]
pub fn (l CursorList) len() int {
	if l.flat == unsafe { nil } || l.parent_id < 0 || l.parent_id >= l.flat.nodes.len {
		return 0
	}
	return l.flat.nodes[l.parent_id].edge_count - l.offset
}

@[inline]
pub fn (l CursorList) at(i int) Cursor {
	return Cursor{
		flat: l.flat
		id:   l.flat.child_at(l.parent_id, l.offset + i)
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

// stmts reads every item in a cursor list through FlatAst.decode_stmt. This is
// the escape hatch for legacy statement walkers; prefer cursor-specific
// readers when the caller only needs a declaration signature or metadata.
pub fn (l CursorList) stmts() []Stmt {
	mut out := []Stmt{cap: l.len()}
	for i in 0 .. l.len() {
		c := l.at(i)
		if !c.is_valid() {
			continue
		}
		out << c.stmt()
	}
	return out
}

// attribute_expr reads the small expression subset used inside attributes.
// Attribute payloads are normally identifiers or strings; type_expr handles
// the type-like fallback cases without opening the full FlatReader.
pub fn (c Cursor) attribute_expr() Expr {
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
		.expr_string {
			return Expr(StringLiteral{
				kind:  unsafe { StringLiteralKind(int(c.aux())) }
				value: c.name()
				pos:   c.pos()
			})
		}
		.expr_selector {
			rhs := c.edge(1)
			return Expr(SelectorExpr{
				lhs: c.edge(0).attribute_expr()
				rhs: Ident{
					name: rhs.name()
					pos:  rhs.pos()
				}
				pos: c.pos()
			})
		}
		else {
			return c.type_expr()
		}
	}
}

// attribute reads an aux_attribute cursor into the legacy Attribute shape.
pub fn (c Cursor) attribute() Attribute {
	if !c.is_valid() || c.kind() != .aux_attribute {
		return Attribute{}
	}
	return Attribute{
		name:  c.name()
		value: c.edge(0).attribute_expr()
		// comptime_cond (`@[if cond ?]`) can be an arbitrary expression, not the
		// ident/string subset attribute_expr handles — decode it fully (mirrors
		// FlatReader.read_attribute), else complex conditions silently become
		// empty_expr and `@[if ...]` functions are never elided.
		comptime_cond: c.edge(1).expr()
		pos:           c.pos()
	}
}

// attributes reads every aux_attribute in a cursor list.
pub fn (l CursorList) attributes() []Attribute {
	mut out := []Attribute{cap: l.len()}
	for i in 0 .. l.len() {
		attr := l.at(i)
		if !attr.is_valid() {
			continue
		}
		out << attr.attribute()
	}
	return out
}

// field_init reads an aux_field_init cursor. Field values are still legacy
// expression consumers today, so this materializes only the value expression,
// not the parent declaration.
pub fn (c Cursor) field_init() FieldInit {
	if !c.is_valid() || c.kind() != .aux_field_init {
		return FieldInit{}
	}
	value_c := c.edge(0)
	return FieldInit{
		name:  c.name()
		value: value_c.expr()
	}
}

// field_inits reads every aux_field_init in a cursor list.
pub fn (l CursorList) field_inits() []FieldInit {
	mut out := []FieldInit{cap: l.len()}
	for i in 0 .. l.len() {
		out << l.at(i).field_init()
	}
	return out
}

// field_decl reads an aux_field_decl cursor. Set decode_value to false when a
// caller only needs declaration metadata and type expressions.
pub fn (c Cursor) field_decl(decode_value bool) FieldDecl {
	if !c.is_valid() || c.kind() != .aux_field_decl {
		return FieldDecl{}
	}
	value_c := c.edge(1)
	return FieldDecl{
		name:                c.name()
		typ:                 c.edge(0).type_expr()
		value:               if decode_value { value_c.expr() } else { empty_expr }
		attributes:          c.list_at(2).attributes()
		is_public:           c.flag(flag_is_public)
		is_mut:              c.flag(flag_is_mut)
		is_module_mut:       c.flag(flag_field_is_module_mut)
		is_interface_method: c.flag(flag_field_is_interface_method)
	}
}

// field_decls reads every aux_field_decl in a cursor list.
pub fn (l CursorList) field_decls(decode_values bool) []FieldDecl {
	mut out := []FieldDecl{cap: l.len()}
	for i in 0 .. l.len() {
		out << l.at(i).field_decl(decode_values)
	}
	return out
}

// const_decl reads a stmt_const_decl cursor.
pub fn (c Cursor) const_decl() ConstDecl {
	if !c.is_valid() || c.kind() != .stmt_const_decl {
		return ConstDecl{}
	}
	return ConstDecl{
		is_public: c.flag(flag_is_public)
		fields:    c.list_at(0).field_inits()
	}
}

// enum_decl reads a stmt_enum_decl cursor. Set decode_values to false when
// only field names/attributes are needed.
pub fn (c Cursor) enum_decl(decode_values bool) EnumDecl {
	if !c.is_valid() || c.kind() != .stmt_enum_decl {
		return EnumDecl{}
	}
	return EnumDecl{
		attributes: c.list_at(1).attributes()
		is_public:  c.flag(flag_is_public)
		name:       c.name()
		as_type:    c.edge(0).type_expr()
		fields:     c.list_at(2).field_decls(decode_values)
	}
}

// global_decl reads a stmt_global_decl cursor. Set decode_values to false when
// only field metadata and declared types are needed.
pub fn (c Cursor) global_decl(decode_values bool) GlobalDecl {
	if !c.is_valid() || c.kind() != .stmt_global_decl {
		return GlobalDecl{}
	}
	return GlobalDecl{
		attributes: c.list_at(0).attributes()
		fields:     c.list_at(1).field_decls(decode_values)
		is_public:  c.flag(flag_is_public)
	}
}

// interface_decl reads a stmt_interface_decl cursor.
pub fn (c Cursor) interface_decl() InterfaceDecl {
	if !c.is_valid() || c.kind() != .stmt_interface_decl {
		return InterfaceDecl{}
	}
	return InterfaceDecl{
		is_public:      c.flag(flag_is_public)
		attributes:     c.list_at(0).attributes()
		name:           c.name()
		generic_params: c.list_at(1).type_exprs()
		embedded:       c.list_at(2).type_exprs()
		fields:         c.list_at(3).field_decls(true)
	}
}

// struct_decl reads a stmt_struct_decl cursor.
pub fn (c Cursor) struct_decl() StructDecl {
	if !c.is_valid() || c.kind() != .stmt_struct_decl {
		return StructDecl{}
	}
	return StructDecl{
		attributes:     c.list_at(0).attributes()
		is_public:      c.flag(flag_is_public)
		is_union:       c.flag(flag_is_union)
		implements:     c.list_at(1).type_exprs()
		embedded:       c.list_at(2).type_exprs()
		language:       unsafe { Language(int(c.aux())) }
		name:           c.name()
		generic_params: c.list_at(3).type_exprs()
		fields:         c.list_at(4).field_decls(true)
		pos:            c.pos()
	}
}

// type_decl reads a stmt_type_decl cursor.
pub fn (c Cursor) type_decl() TypeDecl {
	if !c.is_valid() || c.kind() != .stmt_type_decl {
		return TypeDecl{}
	}
	return TypeDecl{
		is_public:      c.flag(flag_is_public)
		language:       unsafe { Language(int(c.aux())) }
		name:           c.name()
		generic_params: c.list_at(2).type_exprs()
		base_type:      c.edge(0).type_expr()
		variants:       c.list_at(3).type_exprs()
	}
}

fn attrs_from_cursor(list CursorList) []Attribute {
	return list.attributes()
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
