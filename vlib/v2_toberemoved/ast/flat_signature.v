// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import strings

// signature returns a canonical, content-addressed string representation of
// the flat AST. Two flat ASTs produced from the same legacy AST must produce
// identical signatures — this is the round-trip lossless invariant.
//
// The signature includes per-node fields (kind, flags, aux, extra, pos) and
// resolves all interned strings to their text content, so signatures are
// independent of the order in which the intern table was populated.
pub fn (flat &FlatAst) signature() string {
	mut sb := strings.new_builder(flat.nodes.len * 16)
	for ff in flat.files {
		sb.write_string('FILE name=${flat.string_at(ff.name_idx)} mod=${flat.string_at(ff.mod_idx)} root=')
		flat.write_node_sig(mut sb, ff.file_id, 0)
		sb.write_string('\n')
	}
	return sb.str()
}

// subtree_signature returns the same canonical, content-addressed string as
// `signature()` but rooted at a single node id rather than the file roots.
// Useful for tests that want to pin the shape of a specific subtree (e.g.
// the `FlatBuilder.prepend_to_fn_body` bit-equality test) without managing
// a file root or worrying about intern-order leakage in the `.file` node's
// `extra` slot.
pub fn (flat &FlatAst) subtree_signature(node_id FlatNodeId) string {
	mut sb := strings.new_builder(128)
	flat.write_node_sig(mut sb, node_id, 0)
	return sb.str()
}

fn (flat &FlatAst) write_node_sig(mut sb strings.Builder, id FlatNodeId, depth int) {
	if id < 0 || id >= flat.nodes.len {
		sb.write_string('<nil>')
		return
	}
	n := flat.nodes[id]
	sb.write_string('(')
	sb.write_string(n.kind.str())
	sb.write_string(' flags=${n.flags:x} aux=${n.aux:x} extra=${n.extra} pos=${n.pos.offset}:${n.pos.id}')
	if n.name_id >= 0 {
		sb.write_string(' name="')
		sb.write_string(flat.string_at(n.name_id))
		sb.write_string('"')
	}
	for i in 0 .. n.edge_count {
		sb.write_string(' ')
		flat.write_node_sig(mut sb, flat.edges[n.first_edge + i].child_id, depth + 1)
	}
	sb.write_string(')')
}
