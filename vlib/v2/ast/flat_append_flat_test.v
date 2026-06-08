// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

// Tests for `FlatBuilder.append_flat` — the flat-to-flat merge primitive behind
// the flat parallel transform. It concatenates a whole `src` FlatAst into a
// builder, relocating node ids / edge targets and re-interning strings so the
// merged result decodes identically to `src` standalone. The key correctness
// risks: (1) node-id / edge relocation, (2) name_id string remap across a
// DIFFERENT intern order in the destination, (3) re-interning the three kinds
// whose `extra` slot holds a string id (file mod, stmt_directive value,
// stmt_import alias) while leaving packed-int extras (counts/flags) untouched.

fn make_ident_stmt(name string) Stmt {
	return Stmt(ExprStmt{
		expr: Expr(Ident{
			name: name
		})
	})
}

fn make_for_with_idents(names []string) Stmt {
	mut body := []Stmt{}
	for n in names {
		body << make_ident_stmt(n)
	}
	return Stmt(ForStmt{
		init:  Stmt(EmptyStmt{})
		cond:  Expr(BasicLiteral{
			kind:  .number
			value: '1'
		})
		stmts: body
	})
}

// build a small flat with a file root whose stmt list is `stmts`.
fn build_named_flat(file_name string, mod string, stmts []Stmt) (FlatBuilder, FlatNodeId) {
	mut b := new_flat_builder()
	mut ids := []FlatNodeId{}
	for s in stmts {
		ids << b.emit_stmt(s)
	}
	file_id := b.append_file_with_stmt_ids(File{
		name: file_name
		mod:  mod
	}, ids)
	return b, file_id
}

// Seed a destination builder with DIFFERENT strings so the src->dst string
// remap is non-trivial (src ids must not coincide with dst ids).
fn seeded_dst() FlatBuilder {
	mut dst := new_flat_builder()
	dst.intern('zzz_pre_a')
	dst.intern('zzz_pre_b')
	dst.intern('zzz_pre_c')
	dst.emit_stmt(make_ident_stmt('zzz_pre_d'))
	return dst
}

fn test_append_flat_preserves_subtree_structure_and_name_strings() {
	mut a, a_file := build_named_flat('a.v', 'main', [
		make_for_with_idents(['alpha', 'beta']),
		make_ident_stmt('gamma'),
	])
	// edge 2 of a .file node is the stmts list; rooting the signature there
	// avoids the intern-order-dependent .file `extra` slot.
	a_stmts := a.flat.child_at(a_file, 2)
	a_sig := a.flat.subtree_signature(a_stmts)

	mut dst := seeded_dst()
	pre_nodes := dst.flat.nodes.len
	pre_edges := dst.flat.edges.len

	off := dst.append_flat(a.flat)

	assert off == pre_nodes
	assert dst.flat.nodes.len == pre_nodes + a.flat.nodes.len
	assert dst.flat.edges.len == pre_edges + a.flat.edges.len
	// Structure + every interned ident/value survives the id+string relocation.
	assert dst.flat.subtree_signature(a_stmts + off) == a_sig
}

fn test_append_flat_two_sources_stay_independent() {
	mut a, a_file := build_named_flat('a.v', 'main', [
		make_for_with_idents(['alpha', 'beta']),
	])
	mut b, b_file := build_named_flat('b.v', 'other', [make_ident_stmt('delta'),
		make_ident_stmt('epsilon')])
	a_sig := a.flat.subtree_signature(a.flat.child_at(a_file, 2))
	b_sig := b.flat.subtree_signature(b.flat.child_at(b_file, 2))

	mut dst := seeded_dst()
	off_a := dst.append_flat(a.flat)
	off_b := dst.append_flat(b.flat)

	assert dst.flat.files.len == 2
	assert dst.flat.subtree_signature(a.flat.child_at(a_file, 2) + off_a) == a_sig
	assert dst.flat.subtree_signature(b.flat.child_at(b_file, 2) + off_b) == b_sig
	// b's offset accounts for everything already present (seed + a).
	assert off_b == off_a + a.flat.nodes.len
}

fn test_append_flat_reinterns_string_extra_kinds() {
	mut a := new_flat_builder()
	imp_id := a.emit_stmt(Stmt(ImportStmt{
		name:       'os'
		alias:      'myos'
		is_aliased: true
	}))
	dir_id := a.emit_stmt(Stmt(Directive{
		name:  'flag'
		value: '-lpthread'
	}))
	file_id := a.append_file_with_stmt_ids(File{
		name: 'a.v'
		mod:  'mymod'
	}, [imp_id, dir_id])

	mut dst := new_flat_builder()
	// Shift the intern table so src string ids differ from dst string ids.
	dst.intern('os')
	dst.intern('zzz')
	dst.intern('flag_other')
	dst.intern('unrelated_mod')

	off := dst.append_flat(a.flat)

	// stmt_import.alias lives in `extra` — must resolve to the merged table.
	merged_imp := Cursor{
		flat: &dst.flat
		id:   imp_id + off
	}.stmt() as ImportStmt
	assert merged_imp.name == 'os'
	assert merged_imp.alias == 'myos'
	assert merged_imp.is_aliased

	// stmt_directive.value lives in `extra`.
	merged_dir := Cursor{
		flat: &dst.flat
		id:   dir_id + off
	}.stmt() as Directive
	assert merged_dir.name == 'flag'
	assert merged_dir.value == '-lpthread'

	// file mod is carried both in FlatFile.mod_idx (decoder reads this) and in
	// the .file node's `extra` (must also be re-interned for consistency).
	mf := dst.flat.files[dst.flat.files.len - 1]
	assert dst.flat.string_at(mf.name_idx) == 'a.v'
	assert dst.flat.string_at(mf.mod_idx) == 'mymod'
	assert dst.flat.string_at(dst.flat.nodes[file_id + off].extra) == 'mymod'
}

fn test_append_flat_empty_source_is_noop() {
	mut dst := seeded_dst()
	pre_nodes := dst.flat.nodes.len
	pre_edges := dst.flat.edges.len
	pre_files := dst.flat.files.len
	empty := FlatAst{}
	off := dst.append_flat(&empty)
	assert off == pre_nodes
	assert dst.flat.nodes.len == pre_nodes
	assert dst.flat.edges.len == pre_edges
	assert dst.flat.files.len == pre_files
}
