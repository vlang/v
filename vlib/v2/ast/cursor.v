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
