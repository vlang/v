// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import os
import v2.token

// FlatAst is a contiguous, index-based AST representation. Every node lives
// in a single `nodes[]` table; relationships are encoded via `edges[]` ranges
// instead of heap-allocated sumtype payloads. The schema is designed to be
// LOSSLESS: every field of every legacy AST variant is preserved either as a
// kind-specific bit/enum in the cell, or as a child node reachable through
// edges. token.Pos is stored inline (8 bytes) per node to preserve diagnostics
// without an extra interning layer.

pub type FlatNodeId = int

pub const invalid_flat_node_id = FlatNodeId(-1)

// flag bit positions used by FlatNode.flags. Bits are reused per variant;
// see add_* helpers below for the exact mapping.
pub const flag_is_public = u8(1) << 0
pub const flag_is_mut = u8(1) << 1
pub const flag_is_method = u8(1) << 2
pub const flag_is_static = u8(1) << 3
pub const flag_is_union = u8(1) << 4
pub const flag_is_gated = u8(1) << 5
pub const flag_is_aliased = u8(1) << 6 // ImportStmt
pub const flag_defer_func = u8(1) << 7 // DeferStmt.mode == .function
pub const flag_is_count = u8(1) << 6 // SqlExpr (reuse aliased bit)
pub const flag_is_create = u8(1) << 7 // SqlExpr (reuse defer bit)

// FlatNodeKind is a dense tag for every stored AST node variant.
pub enum FlatNodeKind {
	file
	stmt_asm
	stmt_assert
	stmt_assign
	stmt_block
	stmt_comptime
	stmt_const_decl
	stmt_defer
	stmt_directive
	stmt_empty
	stmt_enum_decl
	stmt_expr
	stmt_flow_control
	stmt_fn_decl
	stmt_for_in
	stmt_for
	stmt_global_decl
	stmt_import
	stmt_interface_decl
	stmt_label
	stmt_module
	stmt_return
	stmt_struct_decl
	stmt_type_decl
	stmt_attributes
	expr_array_init
	expr_as_cast
	expr_assoc
	expr_basic_literal
	expr_call
	expr_call_or_cast
	expr_cast
	expr_comptime
	expr_empty
	expr_fn_literal
	expr_generic_arg_or_index
	expr_generic_args
	expr_ident
	expr_if
	expr_if_guard
	expr_index
	expr_infix
	expr_init
	expr_keyword
	expr_keyword_operator
	expr_lambda
	expr_lifetime
	expr_lock
	expr_map_init
	expr_match
	expr_modifier
	expr_or
	expr_paren
	expr_postfix
	expr_prefix
	expr_range
	expr_select
	expr_selector
	expr_sql
	expr_string_inter
	expr_string
	expr_tuple
	expr_unsafe
	typ_anon_struct
	typ_array_fixed
	typ_array
	typ_channel
	typ_fn
	typ_generic
	typ_map
	typ_nil
	typ_none
	typ_option
	typ_pointer
	typ_result
	typ_thread
	typ_tuple
	aux_attribute
	aux_field_init
	aux_field_decl
	aux_parameter
	aux_match_branch
	aux_string_inter
	aux_list   // ordered list grouping: edges are the items
	aux_string // single interned string carrier (used where a node needs an extra string slot)
}

// FlatEdge represents a parent->child relationship.
pub struct FlatEdge {
pub:
	child_id FlatNodeId
}

// FlatNode is the compiler-grade flat cell.
// 28 bytes of payload, 32-byte aligned. Carries inline pos for diagnostics
// and four side-channel slots (flags, aux, name_id, extra) for variant data.
pub struct FlatNode {
pub mut:
	kind       FlatNodeKind // 1 byte: variant tag
	flags      u8           // 1 byte: bit-packed booleans (see flag_* consts)
	aux        u16          // 2 bytes: token.Token or sub-kind enum (variant-specific)
	name_id    int          // 4 bytes: primary interned string id (-1 if none)
	extra      int          // 4 bytes: secondary slot (interned string id, packed ints, or list boundary)
	pos        token.Pos    // 8 bytes: inline source position
	first_edge int          // 4 bytes: start offset into FlatAst.edges
	edge_count int          // 4 bytes: number of child edges
}

// FlatFile maps a source file to its flat root node and original selector map.
pub struct FlatFile {
pub:
	file_id        FlatNodeId
	name_idx       int
	mod_idx        int
	selector_names map[int]string
}

// FlatAst is a contiguous AST graph representation.
pub struct FlatAst {
pub mut:
	files   []FlatFile
	nodes   []FlatNode
	edges   []FlatEdge
	strings []string
}

// FlatAstStats captures high-level memory and shape metrics.
pub struct FlatAstStats {
pub:
	file_roots     int
	nodes          int
	edges          int
	strings        int
	string_bytes   u64
	bytes_estimate u64
}

// LegacyAstStats captures estimated dynamic memory and node shape for the existing AST.
pub struct LegacyAstStats {
pub mut:
	files          int
	expr_nodes     int
	stmt_nodes     int
	type_nodes     int
	aux_nodes      int
	node_bytes     u64
	array_bytes    u64
	string_entries int
	string_bytes   u64
	// allocs counts distinct heap allocations the GC must track: every
	// sumtype variant payload, every dynamic array (spine), and every
	// string buffer. Flat AST collapses these to a handful of arenas.
	allocs         u64
	bytes_estimate u64
}

// flatten_files converts recursive v2 AST files into a flat, index-based graph.
pub fn flatten_files(files []File) FlatAst {
	mut total_bytes := i64(0)
	for file in files {
		if file.name == '' {
			continue
		}
		total_bytes += os.file_size(file.name)
	}
	nodes_cap, edges_cap, strings_cap := arena_caps_for_bytes(total_bytes)
	mut b := new_flat_builder_with_capacity(nodes_cap, edges_cap, strings_cap)
	for file in files {
		b.append_file(file)
	}
	return b.flat
}

// arena_caps_for_bytes returns (nodes_cap, edges_cap, strings_cap) sized from
// total source-byte estimate. Calibrated against the v2 self-host parse:
// ~150 nodes/KB, ~170 edges/KB, ~5 unique strings/KB. Floors match the
// default seed caps so tiny inputs don't waste arena space.
pub fn arena_caps_for_bytes(total_bytes i64) (int, int, int) {
	kb := int(total_bytes / 1024) + 1
	mut nodes_cap := 150 * kb
	mut edges_cap := 170 * kb
	mut strings_cap := 5 * kb
	if nodes_cap < 1024 {
		nodes_cap = 1024
	}
	if edges_cap < 2048 {
		edges_cap = 2048
	}
	if strings_cap < 512 {
		strings_cap = 512
	}
	return nodes_cap, edges_cap, strings_cap
}

// legacy_ast_stats estimates dynamic memory and node counts for the recursive AST.
pub fn legacy_ast_stats(files []File) LegacyAstStats {
	mut w := LegacyAstWalker{}
	w.walk_files(files)
	return w.stats
}

// count_legacy_nodes returns the total legacy node count used by the walker.
pub fn count_legacy_nodes(files []File) int {
	stats := legacy_ast_stats(files)
	return stats.files + stats.expr_nodes + stats.stmt_nodes + stats.type_nodes + stats.aux_nodes
}

// stats returns aggregate shape and estimated memory usage for FlatAst.
pub fn (flat &FlatAst) stats() FlatAstStats {
	mut string_bytes := u64(0)
	for s in flat.strings {
		string_bytes += u64(s.len)
	}
	mut bytes := u64(flat.files.len) * u64(sizeof(FlatFile))
	bytes += u64(flat.nodes.len) * u64(sizeof(FlatNode))
	bytes += u64(flat.edges.len) * u64(sizeof(FlatEdge))
	bytes += u64(flat.strings.len) * u64(sizeof(string))
	bytes += string_bytes
	return FlatAstStats{
		file_roots:     flat.files.len
		nodes:          flat.nodes.len
		edges:          flat.edges.len
		strings:        flat.strings.len
		string_bytes:   string_bytes
		bytes_estimate: bytes
	}
}

// count_nodes_by_kind returns a tally of how many nodes exist of each kind.
// Useful for spotting structural overhead (e.g., aux_list wrappers).
pub fn (flat &FlatAst) count_nodes_by_kind() map[string]int {
	mut hist := map[string]int{}
	for n in flat.nodes {
		hist[n.kind.str()]++
	}
	return hist
}

// count_reachable_nodes traverses from file roots and counts unique reachable nodes.
pub fn (flat &FlatAst) count_reachable_nodes() int {
	if flat.nodes.len == 0 || flat.files.len == 0 {
		return 0
	}
	mut seen := []bool{len: flat.nodes.len}
	mut stack := []int{cap: flat.files.len}
	for file in flat.files {
		stack << file.file_id
	}
	mut count := 0
	for stack.len > 0 {
		node_id := stack.pop()
		if node_id < 0 || node_id >= flat.nodes.len {
			continue
		}
		if seen[node_id] {
			continue
		}
		seen[node_id] = true
		count++
		node := flat.nodes[node_id]
		for i in 0 .. node.edge_count {
			edge := flat.edges[node.first_edge + i]
			stack << edge.child_id
		}
	}
	return count
}

// node_type_name returns the short type-name tag associated with a flat node.
pub fn (flat &FlatAst) node_type_name(node_id FlatNodeId) string {
	if node_id < 0 || node_id >= flat.nodes.len {
		return ''
	}
	return flat.nodes[node_id].kind.str()
}

// child_at returns the i-th child of `parent` (0-indexed), or invalid_flat_node_id.
@[inline]
pub fn (flat &FlatAst) child_at(parent FlatNodeId, i int) FlatNodeId {
	if parent < 0 || parent >= flat.nodes.len {
		return invalid_flat_node_id
	}
	node := flat.nodes[parent]
	if i < 0 || i >= node.edge_count {
		return invalid_flat_node_id
	}
	return flat.edges[node.first_edge + i].child_id
}

// string_at returns the interned string at `idx`, or empty when out of range.
@[inline]
pub fn (flat &FlatAst) string_at(idx int) string {
	if idx < 0 || idx >= flat.strings.len {
		return ''
	}
	return flat.strings[idx]
}

// FlatBuilder is the incremental builder behind FlatAst. It is public so
// front-ends (parser, type checker, ...) can append nodes directly without
// going through the legacy recursive AST. The legacy converters (add_expr,
// add_stmt, ...) remain internal to this module and are exposed via the
// `append_file(File)` shim used during the Phase 2 transition.
pub struct FlatBuilder {
pub mut:
	flat FlatAst
mut:
	string_ids    map[string]int
	empty_list_id FlatNodeId = invalid_flat_node_id
	empty_expr_id FlatNodeId = invalid_flat_node_id
	empty_stmt_id FlatNodeId = invalid_flat_node_id
}

// new_flat_builder returns a fresh FlatBuilder seeded with reasonable arena
// capacities. Callers may resize after measuring (b.flat.nodes.grow_cap, ...).
pub fn new_flat_builder() FlatBuilder {
	return new_flat_builder_with_capacity(1024, 2048, 512)
}

// new_flat_builder_with_capacity lets callers pre-size the three arenas up
// front when an estimate is available (e.g. derived from total source size).
// Empirically the v2 self-host produces ~150 nodes/KB, ~165 edges/KB and ~5
// unique strings/KB; supplying tight caps avoids the geometric realloc churn
// that dominates the streaming parse path.
pub fn new_flat_builder_with_capacity(nodes_cap int, edges_cap int, strings_cap int) FlatBuilder {
	return FlatBuilder{
		flat:       FlatAst{
			files:   []FlatFile{}
			nodes:   []FlatNode{cap: nodes_cap}
			edges:   []FlatEdge{cap: edges_cap}
			strings: []string{cap: strings_cap}
		}
		string_ids: map[string]int{}
	}
}

// append_file converts one legacy File into flat nodes and registers it as a
// root. Designed for streaming use: the caller can drop the legacy `file`
// immediately after this returns, capping peak memory at ~one file's legacy
// AST plus the cumulative flat representation.
pub fn (mut b FlatBuilder) append_file(file File) FlatNodeId {
	file_id := b.add_file(file)
	b.flat.files << FlatFile{
		file_id:        file_id
		name_idx:       b.intern(file.name)
		mod_idx:        b.intern(file.mod)
		selector_names: file.selector_names
	}
	return file_id
}

// append_file_with_stmt_ids registers a file root whose stmt list is built
// from FlatNodeIds that were already emitted into this builder by a previous
// per-stmt pass (e.g. the transformer's flat-write path). Attributes and
// imports are still consumed from the legacy `file` because they are not
// subject to transformer rewrites; emitting them here keeps the file shape
// bit-equal to `add_file`'s output.
pub fn (mut b FlatBuilder) append_file_with_stmt_ids(file File, stmt_ids []FlatNodeId) FlatNodeId {
	mut edges := []FlatEdge{}
	b.push_edge(mut edges, b.make_list_attribute(file.attributes))
	b.push_edge(mut edges, b.make_list_imports(file.imports))
	b.push_edge(mut edges, b.make_list_from_stmt_ids(stmt_ids))
	file_id := b.emit(.file, token.Pos{}, b.intern(file.name), b.intern(file.mod), 0, 0, edges)
	b.flat.files << FlatFile{
		file_id:        file_id
		name_idx:       b.intern(file.name)
		mod_idx:        b.intern(file.mod)
		selector_names: file.selector_names
	}
	return file_id
}

// emit_stmt is the public wrapper around the legacy stmt-to-flat conversion.
// Future sessions of the transformer-writes-flat port reach for this when a
// stmt variant still falls back to legacy emission; the long-term goal is for
// transform_stmt_to_flat (in v2.transformer) to stop calling it as each stmt
// arm gets a direct-emit replacement.
pub fn (mut b FlatBuilder) emit_stmt(stmt Stmt) FlatNodeId {
	return b.add_stmt(stmt)
}

fn (mut b FlatBuilder) make_list_from_stmt_ids(stmt_ids []FlatNodeId) FlatNodeId {
	if stmt_ids.len == 0 {
		return b.get_empty_list()
	}
	mut edges := []FlatEdge{cap: stmt_ids.len}
	for id in stmt_ids {
		b.push_edge(mut edges, id)
	}
	return b.emit_simple(.aux_list, token.Pos{}, edges)
}

fn (mut b FlatBuilder) intern(s string) int {
	if s.len == 0 {
		return -1
	}
	if idx := b.string_ids[s] {
		return idx
	}
	idx := b.flat.strings.len
	b.flat.strings << s
	b.string_ids[s] = idx
	return idx
}

// emit appends a node with the supplied cell payload + edges, returning its id.
fn (mut b FlatBuilder) emit(kind FlatNodeKind, pos token.Pos, name_id int, extra int, aux u16, flags u8, edges []FlatEdge) FlatNodeId {
	first_edge := b.flat.edges.len
	if edges.len > 0 {
		b.flat.edges << edges
	}
	node_id := FlatNodeId(b.flat.nodes.len)
	b.flat.nodes << FlatNode{
		kind:       kind
		flags:      flags
		aux:        aux
		name_id:    name_id
		extra:      extra
		pos:        pos
		first_edge: first_edge
		edge_count: edges.len
	}
	return node_id
}

// emit_simple wraps emit() for nodes with no scalar payload beyond kind/pos/edges.
@[inline]
fn (mut b FlatBuilder) emit_simple(kind FlatNodeKind, pos token.Pos, edges []FlatEdge) FlatNodeId {
	return b.emit(kind, pos, -1, -1, 0, 0, edges)
}

fn (mut b FlatBuilder) push_edge(mut edges []FlatEdge, child FlatNodeId) {
	edges << FlatEdge{
		child_id: child
	}
}

fn (mut b FlatBuilder) push_expr(mut edges []FlatEdge, expr Expr) {
	b.push_edge(mut edges, b.add_expr(expr))
}

fn (mut b FlatBuilder) push_stmt(mut edges []FlatEdge, stmt Stmt) {
	b.push_edge(mut edges, b.add_stmt(stmt))
}

fn (mut b FlatBuilder) push_type(mut edges []FlatEdge, typ Type) {
	b.push_edge(mut edges, b.add_type(typ))
}

// get_empty_list returns a shared aux_list node id used as a canonical
// "empty list". Many AST variants have multiple optional sub-lists; reusing
// one node avoids emitting thousands of zero-edge aux_list cells.
fn (mut b FlatBuilder) get_empty_list() FlatNodeId {
	if b.empty_list_id == invalid_flat_node_id {
		b.empty_list_id = b.emit_simple(.aux_list, token.Pos{}, []FlatEdge{})
	}
	return b.empty_list_id
}

// make_list creates an aux_list node containing the supplied edges.
fn (mut b FlatBuilder) make_list_expr(items []Expr) FlatNodeId {
	if items.len == 0 {
		return b.get_empty_list()
	}
	mut edges := []FlatEdge{cap: items.len}
	for it in items {
		b.push_expr(mut edges, it)
	}
	return b.emit_simple(.aux_list, token.Pos{}, edges)
}

fn (mut b FlatBuilder) make_list_stmt(items []Stmt) FlatNodeId {
	if items.len == 0 {
		return b.get_empty_list()
	}
	mut edges := []FlatEdge{cap: items.len}
	for it in items {
		b.push_stmt(mut edges, it)
	}
	return b.emit_simple(.aux_list, token.Pos{}, edges)
}

fn (mut b FlatBuilder) make_list_attribute(items []Attribute) FlatNodeId {
	if items.len == 0 {
		return b.get_empty_list()
	}
	mut edges := []FlatEdge{cap: items.len}
	for it in items {
		b.push_edge(mut edges, b.add_attribute(it))
	}
	return b.emit_simple(.aux_list, token.Pos{}, edges)
}

fn (mut b FlatBuilder) make_list_field_init(items []FieldInit) FlatNodeId {
	if items.len == 0 {
		return b.get_empty_list()
	}
	mut edges := []FlatEdge{cap: items.len}
	for it in items {
		b.push_edge(mut edges, b.add_field_init(it))
	}
	return b.emit_simple(.aux_list, token.Pos{}, edges)
}

fn (mut b FlatBuilder) make_list_field_decl(items []FieldDecl) FlatNodeId {
	if items.len == 0 {
		return b.get_empty_list()
	}
	mut edges := []FlatEdge{cap: items.len}
	for it in items {
		b.push_edge(mut edges, b.add_field_decl(it))
	}
	return b.emit_simple(.aux_list, token.Pos{}, edges)
}

fn (mut b FlatBuilder) make_list_parameter(items []Parameter) FlatNodeId {
	if items.len == 0 {
		return b.get_empty_list()
	}
	mut edges := []FlatEdge{cap: items.len}
	for it in items {
		b.push_edge(mut edges, b.add_parameter(it))
	}
	return b.emit_simple(.aux_list, token.Pos{}, edges)
}

fn (mut b FlatBuilder) make_list_match_branch(items []MatchBranch) FlatNodeId {
	if items.len == 0 {
		return b.get_empty_list()
	}
	mut edges := []FlatEdge{cap: items.len}
	for it in items {
		b.push_edge(mut edges, b.add_match_branch(it))
	}
	return b.emit_simple(.aux_list, token.Pos{}, edges)
}

fn (mut b FlatBuilder) make_list_string_inter(items []StringInter) FlatNodeId {
	if items.len == 0 {
		return b.get_empty_list()
	}
	mut edges := []FlatEdge{cap: items.len}
	for it in items {
		b.push_edge(mut edges, b.add_string_inter(it))
	}
	return b.emit_simple(.aux_list, token.Pos{}, edges)
}

fn (mut b FlatBuilder) make_list_strings(items []string) FlatNodeId {
	if items.len == 0 {
		return b.get_empty_list()
	}
	mut edges := []FlatEdge{cap: items.len}
	for s in items {
		id := b.emit(.aux_string, token.Pos{}, b.intern(s), -1, 0, 0, []FlatEdge{})
		b.push_edge(mut edges, id)
	}
	return b.emit_simple(.aux_list, token.Pos{}, edges)
}

fn (mut b FlatBuilder) make_list_imports(items []ImportStmt) FlatNodeId {
	if items.len == 0 {
		return b.get_empty_list()
	}
	mut edges := []FlatEdge{cap: items.len}
	for it in items {
		b.push_edge(mut edges, b.add_stmt(Stmt(it)))
	}
	return b.emit_simple(.aux_list, token.Pos{}, edges)
}

// add_file emits the file root and returns its FlatNodeId.
fn (mut b FlatBuilder) add_file(file File) FlatNodeId {
	mut edges := []FlatEdge{}
	b.push_edge(mut edges, b.make_list_attribute(file.attributes))
	b.push_edge(mut edges, b.make_list_imports(file.imports))
	b.push_edge(mut edges, b.make_list_stmt(file.stmts))
	return b.emit(.file, token.Pos{}, b.intern(file.name), b.intern(file.mod), 0, 0, edges)
}

fn (mut b FlatBuilder) add_stmt(stmt Stmt) FlatNodeId {
	if stmt is []Attribute {
		return b.emit_simple(.stmt_attributes, token.Pos{}, [
			FlatEdge{
				child_id: b.make_list_attribute(stmt)
			},
		])
	}
	match stmt {
		AsmStmt {
			return b.emit(.stmt_asm, token.Pos{}, b.intern(stmt.arch), -1, 0, 0, []FlatEdge{})
		}
		AssertStmt {
			mut edges := []FlatEdge{}
			b.push_expr(mut edges, stmt.expr)
			b.push_expr(mut edges, stmt.extra)
			return b.emit_simple(.stmt_assert, token.Pos{}, edges)
		}
		AssignStmt {
			mut edges := []FlatEdge{cap: stmt.lhs.len + stmt.rhs.len}
			for e in stmt.lhs {
				b.push_expr(mut edges, e)
			}
			for e in stmt.rhs {
				b.push_expr(mut edges, e)
			}
			return b.emit(.stmt_assign, stmt.pos, -1, stmt.lhs.len, u16(int(stmt.op)), 0, edges)
		}
		BlockStmt {
			mut edges := []FlatEdge{cap: stmt.stmts.len}
			for s in stmt.stmts {
				b.push_stmt(mut edges, s)
			}
			return b.emit_simple(.stmt_block, token.Pos{}, edges)
		}
		ComptimeStmt {
			return b.emit_simple(.stmt_comptime, token.Pos{}, [
				FlatEdge{
					child_id: b.add_stmt(stmt.stmt)
				},
			])
		}
		ConstDecl {
			mut flags := u8(0)
			if stmt.is_public {
				flags |= flag_is_public
			}
			return b.emit_simple_with_flags(.stmt_const_decl, token.Pos{}, flags, [
				FlatEdge{
					child_id: b.make_list_field_init(stmt.fields)
				},
			])
		}
		DeferStmt {
			mut flags := u8(0)
			if stmt.mode == .function {
				flags |= flag_defer_func
			}
			mut edges := []FlatEdge{cap: stmt.stmts.len}
			for s in stmt.stmts {
				b.push_stmt(mut edges, s)
			}
			return b.emit_simple_with_flags(.stmt_defer, token.Pos{}, flags, edges)
		}
		Directive {
			mut edges := []FlatEdge{}
			// ct_cond carried as a child aux_string node when non-empty
			if stmt.ct_cond.len > 0 {
				id := b.emit(.aux_string, token.Pos{}, b.intern(stmt.ct_cond), -1, 0, 0,
					[]FlatEdge{})
				b.push_edge(mut edges, id)
			}
			return b.emit(.stmt_directive, token.Pos{}, b.intern(stmt.name), b.intern(stmt.value),
				0, 0, edges)
		}
		EmptyStmt {
			if int(stmt) == 0 {
				if b.empty_stmt_id == invalid_flat_node_id {
					b.empty_stmt_id = b.emit(.stmt_empty, token.Pos{}, -1, 0, 0, 0, []FlatEdge{})
				}
				return b.empty_stmt_id
			}
			return b.emit(.stmt_empty, token.Pos{}, -1, int(stmt), 0, 0, []FlatEdge{})
		}
		EnumDecl {
			mut flags := u8(0)
			if stmt.is_public {
				flags |= flag_is_public
			}
			mut edges := []FlatEdge{}
			b.push_expr(mut edges, stmt.as_type)
			b.push_edge(mut edges, b.make_list_attribute(stmt.attributes))
			b.push_edge(mut edges, b.make_list_field_decl(stmt.fields))
			return b.emit_simple_with_flags_name(.stmt_enum_decl, token.Pos{}, flags,
				b.intern(stmt.name), edges)
		}
		ExprStmt {
			return b.emit_simple(.stmt_expr, token.Pos{}, [
				FlatEdge{
					child_id: b.add_expr(stmt.expr)
				},
			])
		}
		FlowControlStmt {
			return b.emit(.stmt_flow_control, token.Pos{}, b.intern(stmt.label), -1,
				u16(int(stmt.op)), 0, []FlatEdge{})
		}
		FnDecl {
			mut flags := u8(0)
			if stmt.is_public {
				flags |= flag_is_public
			}
			if stmt.is_method {
				flags |= flag_is_method
			}
			if stmt.is_static {
				flags |= flag_is_static
			}
			mut edges := []FlatEdge{}
			b.push_edge(mut edges, b.add_parameter(stmt.receiver))
			b.push_edge(mut edges, b.add_type(Type(stmt.typ)))
			b.push_edge(mut edges, b.make_list_attribute(stmt.attributes))
			b.push_edge(mut edges, b.make_list_stmt(stmt.stmts))
			return b.emit(.stmt_fn_decl, stmt.pos, b.intern(stmt.name), -1,
				u16(int(stmt.language)), flags, edges)
		}
		ForInStmt {
			mut edges := []FlatEdge{cap: 3}
			b.push_expr(mut edges, stmt.key)
			b.push_expr(mut edges, stmt.value)
			b.push_expr(mut edges, stmt.expr)
			return b.emit_simple(.stmt_for_in, token.Pos{}, edges)
		}
		ForStmt {
			mut edges := []FlatEdge{cap: 3 + stmt.stmts.len}
			b.push_stmt(mut edges, stmt.init)
			b.push_expr(mut edges, stmt.cond)
			b.push_stmt(mut edges, stmt.post)
			for s in stmt.stmts {
				b.push_stmt(mut edges, s)
			}
			return b.emit_simple(.stmt_for, token.Pos{}, edges)
		}
		GlobalDecl {
			mut edges := []FlatEdge{}
			b.push_edge(mut edges, b.make_list_attribute(stmt.attributes))
			b.push_edge(mut edges, b.make_list_field_decl(stmt.fields))
			return b.emit_simple(.stmt_global_decl, token.Pos{}, edges)
		}
		ImportStmt {
			mut flags := u8(0)
			if stmt.is_aliased {
				flags |= flag_is_aliased
			}
			mut edges := []FlatEdge{cap: stmt.symbols.len}
			for sym in stmt.symbols {
				b.push_expr(mut edges, sym)
			}
			return b.emit(.stmt_import, token.Pos{}, b.intern(stmt.name), b.intern(stmt.alias), 0,
				flags, edges)
		}
		InterfaceDecl {
			mut flags := u8(0)
			if stmt.is_public {
				flags |= flag_is_public
			}
			mut edges := []FlatEdge{cap: 4}
			b.push_edge(mut edges, b.make_list_attribute(stmt.attributes))
			b.push_edge(mut edges, b.make_list_expr(stmt.generic_params))
			b.push_edge(mut edges, b.make_list_expr(stmt.embedded))
			b.push_edge(mut edges, b.make_list_field_decl(stmt.fields))
			return b.emit_simple_with_flags_name(.stmt_interface_decl, token.Pos{}, flags,
				b.intern(stmt.name), edges)
		}
		LabelStmt {
			return b.emit(.stmt_label, token.Pos{}, b.intern(stmt.name), -1, 0, 0, [
				FlatEdge{
					child_id: b.add_stmt(stmt.stmt)
				},
			])
		}
		ModuleStmt {
			return b.emit(.stmt_module, token.Pos{}, b.intern(stmt.name), -1, 0, 0, []FlatEdge{})
		}
		ReturnStmt {
			mut edges := []FlatEdge{cap: stmt.exprs.len}
			for e in stmt.exprs {
				b.push_expr(mut edges, e)
			}
			return b.emit_simple(.stmt_return, token.Pos{}, edges)
		}
		StructDecl {
			mut flags := u8(0)
			if stmt.is_public {
				flags |= flag_is_public
			}
			if stmt.is_union {
				flags |= flag_is_union
			}
			mut edges := []FlatEdge{cap: 5}
			b.push_edge(mut edges, b.make_list_attribute(stmt.attributes))
			b.push_edge(mut edges, b.make_list_expr(stmt.implements))
			b.push_edge(mut edges, b.make_list_expr(stmt.embedded))
			b.push_edge(mut edges, b.make_list_expr(stmt.generic_params))
			b.push_edge(mut edges, b.make_list_field_decl(stmt.fields))
			return b.emit(.stmt_struct_decl, stmt.pos, b.intern(stmt.name), -1,
				u16(int(stmt.language)), flags, edges)
		}
		TypeDecl {
			mut flags := u8(0)
			if stmt.is_public {
				flags |= flag_is_public
			}
			mut edges := []FlatEdge{cap: 4}
			b.push_expr(mut edges, stmt.base_type)
			b.push_edge(mut edges, b.make_list_attribute([]Attribute{}))
			b.push_edge(mut edges, b.make_list_expr(stmt.generic_params))
			b.push_edge(mut edges, b.make_list_expr(stmt.variants))
			return b.emit(.stmt_type_decl, token.Pos{}, b.intern(stmt.name), -1,
				u16(int(stmt.language)), flags, edges)
		}
		[]Attribute {
			// handled at top of function
			return invalid_flat_node_id
		}
	}
}

@[inline]
fn (mut b FlatBuilder) emit_simple_with_flags(kind FlatNodeKind, pos token.Pos, flags u8, edges []FlatEdge) FlatNodeId {
	return b.emit(kind, pos, -1, -1, 0, flags, edges)
}

@[inline]
fn (mut b FlatBuilder) emit_simple_with_flags_name(kind FlatNodeKind, pos token.Pos, flags u8, name_id int, edges []FlatEdge) FlatNodeId {
	return b.emit(kind, pos, name_id, -1, 0, flags, edges)
}

fn (mut b FlatBuilder) add_expr(expr Expr) FlatNodeId {
	match expr {
		Type {
			return b.add_type(expr)
		}
		FieldInit {
			return b.add_field_init(expr)
		}
		else {}
	}

	match expr {
		ArrayInitExpr {
			mut edges := []FlatEdge{cap: 4 + expr.exprs.len}
			b.push_expr(mut edges, expr.typ)
			b.push_expr(mut edges, expr.init)
			b.push_expr(mut edges, expr.cap)
			b.push_expr(mut edges, expr.len)
			for e in expr.exprs {
				b.push_expr(mut edges, e)
			}
			return b.emit_simple(.expr_array_init, expr.pos, edges)
		}
		AsCastExpr {
			mut edges := []FlatEdge{cap: 2}
			b.push_expr(mut edges, expr.expr)
			b.push_expr(mut edges, expr.typ)
			return b.emit_simple(.expr_as_cast, expr.pos, edges)
		}
		AssocExpr {
			mut edges := []FlatEdge{cap: 2 + expr.fields.len}
			b.push_expr(mut edges, expr.typ)
			b.push_expr(mut edges, expr.expr)
			for f in expr.fields {
				b.push_edge(mut edges, b.add_field_init(f))
			}
			return b.emit_simple(.expr_assoc, expr.pos, edges)
		}
		BasicLiteral {
			return b.emit(.expr_basic_literal, expr.pos, b.intern(expr.value), -1,
				u16(int(expr.kind)), 0, []FlatEdge{})
		}
		CallExpr {
			mut edges := []FlatEdge{cap: 1 + expr.args.len}
			b.push_expr(mut edges, expr.lhs)
			for a in expr.args {
				b.push_expr(mut edges, a)
			}
			return b.emit_simple(.expr_call, expr.pos, edges)
		}
		CallOrCastExpr {
			mut edges := []FlatEdge{cap: 2}
			b.push_expr(mut edges, expr.lhs)
			b.push_expr(mut edges, expr.expr)
			return b.emit_simple(.expr_call_or_cast, expr.pos, edges)
		}
		CastExpr {
			mut edges := []FlatEdge{cap: 2}
			b.push_expr(mut edges, expr.typ)
			b.push_expr(mut edges, expr.expr)
			return b.emit_simple(.expr_cast, expr.pos, edges)
		}
		ComptimeExpr {
			return b.emit_simple(.expr_comptime, expr.pos, [
				FlatEdge{
					child_id: b.add_expr(expr.expr)
				},
			])
		}
		EmptyExpr {
			// All EmptyExpr instances in the parser are `EmptyExpr(0)` (see
			// ast.empty_expr). Share one node — 65k+ duplicates per build.
			if int(expr) == 0 {
				if b.empty_expr_id == invalid_flat_node_id {
					b.empty_expr_id = b.emit(.expr_empty, token.Pos{}, -1, 0, 0, 0, []FlatEdge{})
				}
				return b.empty_expr_id
			}
			return b.emit(.expr_empty, token.Pos{}, -1, int(expr), 0, 0, []FlatEdge{})
		}
		FnLiteral {
			mut edges := []FlatEdge{}
			b.push_edge(mut edges, b.add_type(Type(expr.typ)))
			for cv in expr.captured_vars {
				b.push_expr(mut edges, cv)
			}
			for s in expr.stmts {
				b.push_stmt(mut edges, s)
			}
			// extra stores captured_vars.len so the boundary between captured
			// vars and stmts is recoverable.
			return b.emit(.expr_fn_literal, expr.pos, -1, expr.captured_vars.len, 0, 0, edges)
		}
		GenericArgOrIndexExpr {
			mut edges := []FlatEdge{cap: 2}
			b.push_expr(mut edges, expr.lhs)
			b.push_expr(mut edges, expr.expr)
			return b.emit_simple(.expr_generic_arg_or_index, expr.pos, edges)
		}
		GenericArgs {
			mut edges := []FlatEdge{cap: 1 + expr.args.len}
			b.push_expr(mut edges, expr.lhs)
			for a in expr.args {
				b.push_expr(mut edges, a)
			}
			return b.emit_simple(.expr_generic_args, expr.pos, edges)
		}
		Ident {
			return b.emit(.expr_ident, expr.pos, b.intern(expr.name), -1, 0, 0, []FlatEdge{})
		}
		IfExpr {
			mut edges := []FlatEdge{cap: 2 + expr.stmts.len}
			b.push_expr(mut edges, expr.cond)
			b.push_expr(mut edges, expr.else_expr)
			for s in expr.stmts {
				b.push_stmt(mut edges, s)
			}
			return b.emit_simple(.expr_if, expr.pos, edges)
		}
		IfGuardExpr {
			return b.emit_simple(.expr_if_guard, expr.pos, [
				FlatEdge{
					child_id: b.add_stmt(Stmt(expr.stmt))
				},
			])
		}
		IndexExpr {
			mut flags := u8(0)
			if expr.is_gated {
				flags |= flag_is_gated
			}
			mut edges := []FlatEdge{cap: 2}
			b.push_expr(mut edges, expr.lhs)
			b.push_expr(mut edges, expr.expr)
			return b.emit(.expr_index, expr.pos, -1, -1, 0, flags, edges)
		}
		InfixExpr {
			mut edges := []FlatEdge{cap: 2}
			b.push_expr(mut edges, expr.lhs)
			b.push_expr(mut edges, expr.rhs)
			return b.emit(.expr_infix, expr.pos, -1, -1, u16(int(expr.op)), 0, edges)
		}
		InitExpr {
			mut edges := []FlatEdge{cap: 1 + expr.fields.len}
			b.push_expr(mut edges, expr.typ)
			for f in expr.fields {
				b.push_edge(mut edges, b.add_field_init(f))
			}
			return b.emit_simple(.expr_init, expr.pos, edges)
		}
		Keyword {
			return b.emit(.expr_keyword, token.Pos{}, -1, -1, u16(int(expr.tok)), 0, []FlatEdge{})
		}
		KeywordOperator {
			mut edges := []FlatEdge{cap: expr.exprs.len}
			for e in expr.exprs {
				b.push_expr(mut edges, e)
			}
			return b.emit(.expr_keyword_operator, expr.pos, -1, -1, u16(int(expr.op)), 0, edges)
		}
		LambdaExpr {
			mut edges := []FlatEdge{cap: 1 + expr.args.len}
			b.push_expr(mut edges, expr.expr)
			for a in expr.args {
				b.push_expr(mut edges, Expr(a))
			}
			return b.emit_simple(.expr_lambda, expr.pos, edges)
		}
		LifetimeExpr {
			return b.emit(.expr_lifetime, expr.pos, b.intern(expr.name), -1, 0, 0, []FlatEdge{})
		}
		LockExpr {
			mut edges := []FlatEdge{cap: expr.lock_exprs.len + expr.rlock_exprs.len + expr.stmts.len}
			for e in expr.lock_exprs {
				b.push_expr(mut edges, e)
			}
			for e in expr.rlock_exprs {
				b.push_expr(mut edges, e)
			}
			for s in expr.stmts {
				b.push_stmt(mut edges, s)
			}
			// Pack (lock.len, rlock.len) into extra; stmts.len = edge_count - lock - rlock.
			packed := (expr.lock_exprs.len & 0xFFFF) | ((expr.rlock_exprs.len & 0xFFFF) << 16)
			return b.emit(.expr_lock, expr.pos, -1, packed, 0, 0, edges)
		}
		MapInitExpr {
			mut edges := []FlatEdge{cap: 1 + expr.keys.len + expr.vals.len}
			b.push_expr(mut edges, expr.typ)
			for k in expr.keys {
				b.push_expr(mut edges, k)
			}
			for v in expr.vals {
				b.push_expr(mut edges, v)
			}
			return b.emit(.expr_map_init, expr.pos, -1, expr.keys.len, 0, 0, edges)
		}
		MatchExpr {
			mut edges := []FlatEdge{cap: 1 + expr.branches.len}
			b.push_expr(mut edges, expr.expr)
			for br in expr.branches {
				b.push_edge(mut edges, b.add_match_branch(br))
			}
			return b.emit_simple(.expr_match, expr.pos, edges)
		}
		ModifierExpr {
			mut edges := []FlatEdge{cap: 1}
			b.push_expr(mut edges, expr.expr)
			return b.emit(.expr_modifier, expr.pos, -1, -1, u16(int(expr.kind)), 0, edges)
		}
		OrExpr {
			mut edges := []FlatEdge{cap: 1 + expr.stmts.len}
			b.push_expr(mut edges, expr.expr)
			for s in expr.stmts {
				b.push_stmt(mut edges, s)
			}
			return b.emit_simple(.expr_or, expr.pos, edges)
		}
		ParenExpr {
			return b.emit_simple(.expr_paren, expr.pos, [
				FlatEdge{
					child_id: b.add_expr(expr.expr)
				},
			])
		}
		PostfixExpr {
			return b.emit(.expr_postfix, expr.pos, -1, -1, u16(int(expr.op)), 0, [
				FlatEdge{
					child_id: b.add_expr(expr.expr)
				},
			])
		}
		PrefixExpr {
			return b.emit(.expr_prefix, expr.pos, -1, -1, u16(int(expr.op)), 0, [
				FlatEdge{
					child_id: b.add_expr(expr.expr)
				},
			])
		}
		RangeExpr {
			mut edges := []FlatEdge{cap: 2}
			b.push_expr(mut edges, expr.start)
			b.push_expr(mut edges, expr.end)
			return b.emit(.expr_range, expr.pos, -1, -1, u16(int(expr.op)), 0, edges)
		}
		SelectExpr {
			mut edges := []FlatEdge{cap: 2 + expr.stmts.len}
			b.push_stmt(mut edges, expr.stmt)
			b.push_expr(mut edges, expr.next)
			for s in expr.stmts {
				b.push_stmt(mut edges, s)
			}
			return b.emit_simple(.expr_select, expr.pos, edges)
		}
		SelectorExpr {
			mut edges := []FlatEdge{cap: 2}
			b.push_expr(mut edges, expr.lhs)
			b.push_expr(mut edges, Expr(expr.rhs))
			return b.emit_simple(.expr_selector, expr.pos, edges)
		}
		SqlExpr {
			mut flags := u8(0)
			if expr.is_count {
				flags |= flag_is_count
			}
			if expr.is_create {
				flags |= flag_is_create
			}
			return b.emit(.expr_sql, expr.pos, b.intern(expr.table_name), -1, 0, flags, [
				FlatEdge{
					child_id: b.add_expr(expr.expr)
				},
			])
		}
		StringInterLiteral {
			mut edges := []FlatEdge{cap: 2}
			b.push_edge(mut edges, b.make_list_strings(expr.values))
			b.push_edge(mut edges, b.make_list_string_inter(expr.inters))
			return b.emit(.expr_string_inter, expr.pos, -1, -1, u16(int(expr.kind)), 0, edges)
		}
		StringLiteral {
			return b.emit(.expr_string, expr.pos, b.intern(expr.value), -1, u16(int(expr.kind)), 0,
				[]FlatEdge{})
		}
		Tuple {
			mut edges := []FlatEdge{cap: expr.exprs.len}
			for e in expr.exprs {
				b.push_expr(mut edges, e)
			}
			return b.emit_simple(.expr_tuple, expr.pos, edges)
		}
		UnsafeExpr {
			mut edges := []FlatEdge{cap: expr.stmts.len}
			for s in expr.stmts {
				b.push_stmt(mut edges, s)
			}
			return b.emit_simple(.expr_unsafe, expr.pos, edges)
		}
		FieldInit, Type {
			// handled at top of function
			return invalid_flat_node_id
		}
	}
}

fn (mut b FlatBuilder) add_type(typ Type) FlatNodeId {
	match typ {
		AnonStructType {
			mut edges := []FlatEdge{cap: 3}
			b.push_edge(mut edges, b.make_list_expr(typ.generic_params))
			b.push_edge(mut edges, b.make_list_expr(typ.embedded))
			b.push_edge(mut edges, b.make_list_field_decl(typ.fields))
			return b.emit_simple(.typ_anon_struct, token.Pos{}, edges)
		}
		ArrayFixedType {
			mut edges := []FlatEdge{cap: 2}
			b.push_expr(mut edges, typ.len)
			b.push_expr(mut edges, typ.elem_type)
			return b.emit_simple(.typ_array_fixed, token.Pos{}, edges)
		}
		ArrayType {
			mut edges := []FlatEdge{cap: 1}
			b.push_expr(mut edges, typ.elem_type)
			return b.emit_simple(.typ_array, token.Pos{}, edges)
		}
		ChannelType {
			mut edges := []FlatEdge{cap: 2}
			b.push_expr(mut edges, typ.cap)
			b.push_expr(mut edges, typ.elem_type)
			return b.emit_simple(.typ_channel, token.Pos{}, edges)
		}
		FnType {
			mut edges := []FlatEdge{cap: 3}
			b.push_edge(mut edges, b.make_list_expr(typ.generic_params))
			b.push_edge(mut edges, b.make_list_parameter(typ.params))
			b.push_expr(mut edges, typ.return_type)
			return b.emit_simple(.typ_fn, token.Pos{}, edges)
		}
		GenericType {
			mut edges := []FlatEdge{cap: 1 + typ.params.len}
			b.push_expr(mut edges, typ.name)
			for p in typ.params {
				b.push_expr(mut edges, p)
			}
			return b.emit_simple(.typ_generic, token.Pos{}, edges)
		}
		MapType {
			mut edges := []FlatEdge{cap: 2}
			b.push_expr(mut edges, typ.key_type)
			b.push_expr(mut edges, typ.value_type)
			return b.emit_simple(.typ_map, token.Pos{}, edges)
		}
		NilType {
			return b.emit_simple(.typ_nil, token.Pos{}, []FlatEdge{})
		}
		NoneType {
			return b.emit_simple(.typ_none, token.Pos{}, []FlatEdge{})
		}
		OptionType {
			mut edges := []FlatEdge{cap: 1}
			b.push_expr(mut edges, typ.base_type)
			return b.emit_simple(.typ_option, token.Pos{}, edges)
		}
		PointerType {
			mut edges := []FlatEdge{cap: 1}
			b.push_expr(mut edges, typ.base_type)
			return b.emit(.typ_pointer, token.Pos{}, b.intern(typ.lifetime), -1, 0, 0, edges)
		}
		ResultType {
			mut edges := []FlatEdge{cap: 1}
			b.push_expr(mut edges, typ.base_type)
			return b.emit_simple(.typ_result, token.Pos{}, edges)
		}
		ThreadType {
			mut edges := []FlatEdge{cap: 1}
			b.push_expr(mut edges, typ.elem_type)
			return b.emit_simple(.typ_thread, token.Pos{}, edges)
		}
		TupleType {
			mut edges := []FlatEdge{cap: typ.types.len}
			for t in typ.types {
				b.push_expr(mut edges, t)
			}
			return b.emit_simple(.typ_tuple, token.Pos{}, edges)
		}
	}
}

fn (mut b FlatBuilder) add_attribute(attr Attribute) FlatNodeId {
	mut edges := []FlatEdge{cap: 2}
	b.push_expr(mut edges, attr.value)
	b.push_expr(mut edges, attr.comptime_cond)
	return b.emit(.aux_attribute, token.Pos{}, b.intern(attr.name), -1, 0, 0, edges)
}

fn (mut b FlatBuilder) add_field_init(field FieldInit) FlatNodeId {
	mut edges := []FlatEdge{cap: 1}
	b.push_expr(mut edges, field.value)
	return b.emit(.aux_field_init, token.Pos{}, b.intern(field.name), -1, 0, 0, edges)
}

fn (mut b FlatBuilder) add_field_decl(field FieldDecl) FlatNodeId {
	mut edges := []FlatEdge{cap: 3}
	b.push_expr(mut edges, field.typ)
	b.push_expr(mut edges, field.value)
	b.push_edge(mut edges, b.make_list_attribute(field.attributes))
	return b.emit(.aux_field_decl, token.Pos{}, b.intern(field.name), -1, 0, 0, edges)
}

fn (mut b FlatBuilder) add_parameter(param Parameter) FlatNodeId {
	mut flags := u8(0)
	if param.is_mut {
		flags |= flag_is_mut
	}
	mut edges := []FlatEdge{cap: 1}
	b.push_expr(mut edges, param.typ)
	return b.emit(.aux_parameter, param.pos, b.intern(param.name), -1, 0, flags, edges)
}

fn (mut b FlatBuilder) add_match_branch(branch MatchBranch) FlatNodeId {
	mut edges := []FlatEdge{cap: 2}
	b.push_edge(mut edges, b.make_list_expr(branch.cond))
	b.push_edge(mut edges, b.make_list_stmt(branch.stmts))
	return b.emit_simple(.aux_match_branch, branch.pos, edges)
}

fn (mut b FlatBuilder) add_string_inter(inter StringInter) FlatNodeId {
	mut edges := []FlatEdge{cap: 2}
	b.push_expr(mut edges, inter.expr)
	b.push_expr(mut edges, inter.format_expr)
	// Pack width (high u16) and precision (low u16) into extra.
	packed := (inter.width & 0xFFFF) << 16 | (inter.precision & 0xFFFF)
	return b.emit(.aux_string_inter, token.Pos{}, b.intern(inter.resolved_fmt), packed,
		u16(int(inter.format)), 0, edges)
}

// LegacyAstWalker estimates dynamic memory usage of the recursive AST for diagnostics.
struct LegacyAstWalker {
mut:
	stats LegacyAstStats
}

fn (mut w LegacyAstWalker) walk_files(files []File) {
	w.stats.files = files.len
	w.stats.node_bytes += u64(files.len) * u64(sizeof(File))
	w.add_array_storage(sizeof(File), files.len)
	for file in files {
		w.walk_file(file)
	}
	w.stats.bytes_estimate = w.stats.node_bytes + w.stats.array_bytes + w.stats.string_bytes
}

fn (mut w LegacyAstWalker) add_array_storage(elem_size u32, len int) {
	if len <= 0 || elem_size == 0 {
		return
	}
	w.stats.array_bytes += u64(elem_size) * u64(len)
	w.stats.allocs++
}

fn (mut w LegacyAstWalker) add_string(s string) {
	w.stats.string_entries++
	w.stats.string_bytes += u64(s.len)
	if s.len > 0 {
		w.stats.allocs++
	}
}

// add_variant_payload accounts for one heap allocation behind a sumtype slot
// (e.g., AssignStmt, CallExpr, ArrayType) plus its struct bytes.
fn (mut w LegacyAstWalker) add_variant_payload(size u32) {
	w.stats.node_bytes += u64(size)
	w.stats.allocs++
}

fn (mut w LegacyAstWalker) walk_file(file File) {
	w.scan_dynamic(file)
}

fn (mut w LegacyAstWalker) walk_stmt(stmt Stmt) {
	if stmt is []Attribute {
		w.add_array_storage(sizeof(Attribute), stmt.len)
		for attr in stmt {
			w.walk_attribute(attr)
		}
		return
	}
	w.stats.stmt_nodes++
	// Sumtype slot is counted by parent (array storage / field sizeof).
	// Add only the heap-allocated variant payload.
	match stmt {
		AssignStmt {
			w.add_variant_payload(sizeof(AssignStmt))
			w.scan_dynamic(stmt)
		}
		AssertStmt {
			w.add_variant_payload(sizeof(AssertStmt))
			w.scan_dynamic(stmt)
		}
		AsmStmt {
			w.add_variant_payload(sizeof(AsmStmt))
			w.scan_dynamic(stmt)
		}
		BlockStmt {
			w.add_variant_payload(sizeof(BlockStmt))
			w.scan_dynamic(stmt)
		}
		ComptimeStmt {
			w.add_variant_payload(sizeof(ComptimeStmt))
			w.scan_dynamic(stmt)
		}
		ConstDecl {
			w.add_variant_payload(sizeof(ConstDecl))
			w.scan_dynamic(stmt)
		}
		DeferStmt {
			w.add_variant_payload(sizeof(DeferStmt))
			w.scan_dynamic(stmt)
		}
		Directive {
			w.add_variant_payload(sizeof(Directive))
			w.scan_dynamic(stmt)
		}
		EmptyStmt {}
		EnumDecl {
			w.add_variant_payload(sizeof(EnumDecl))
			w.scan_dynamic(stmt)
		}
		ExprStmt {
			w.add_variant_payload(sizeof(ExprStmt))
			w.scan_dynamic(stmt)
		}
		FlowControlStmt {
			w.add_variant_payload(sizeof(FlowControlStmt))
			w.scan_dynamic(stmt)
		}
		FnDecl {
			w.add_variant_payload(sizeof(FnDecl))
			w.scan_dynamic(stmt)
		}
		ForInStmt {
			w.add_variant_payload(sizeof(ForInStmt))
			w.scan_dynamic(stmt)
		}
		ForStmt {
			w.add_variant_payload(sizeof(ForStmt))
			w.scan_dynamic(stmt)
		}
		GlobalDecl {
			w.add_variant_payload(sizeof(GlobalDecl))
			w.scan_dynamic(stmt)
		}
		ImportStmt {
			w.add_variant_payload(sizeof(ImportStmt))
			w.scan_dynamic(stmt)
		}
		InterfaceDecl {
			w.add_variant_payload(sizeof(InterfaceDecl))
			w.scan_dynamic(stmt)
		}
		LabelStmt {
			w.add_variant_payload(sizeof(LabelStmt))
			w.scan_dynamic(stmt)
		}
		ModuleStmt {
			w.add_variant_payload(sizeof(ModuleStmt))
			w.scan_dynamic(stmt)
		}
		ReturnStmt {
			w.add_variant_payload(sizeof(ReturnStmt))
			w.scan_dynamic(stmt)
		}
		StructDecl {
			w.add_variant_payload(sizeof(StructDecl))
			w.scan_dynamic(stmt)
		}
		TypeDecl {
			w.add_variant_payload(sizeof(TypeDecl))
			w.scan_dynamic(stmt)
		}
		[]Attribute {}
	}
}

fn (mut w LegacyAstWalker) walk_expr(expr Expr) {
	match expr {
		Type {
			w.walk_type(expr)
			return
		}
		FieldInit {
			w.walk_field_init(expr)
			return
		}
		else {}
	}

	w.stats.expr_nodes++
	// Sumtype slot is counted by parent; add only heap variant payload.
	match expr {
		ArrayInitExpr {
			w.add_variant_payload(sizeof(ArrayInitExpr))
			w.scan_dynamic(expr)
		}
		AsCastExpr {
			w.add_variant_payload(sizeof(AsCastExpr))
			w.scan_dynamic(expr)
		}
		AssocExpr {
			w.add_variant_payload(sizeof(AssocExpr))
			w.scan_dynamic(expr)
		}
		BasicLiteral {
			w.add_variant_payload(sizeof(BasicLiteral))
			w.scan_dynamic(expr)
		}
		CallExpr {
			w.add_variant_payload(sizeof(CallExpr))
			w.scan_dynamic(expr)
		}
		CallOrCastExpr {
			w.add_variant_payload(sizeof(CallOrCastExpr))
			w.scan_dynamic(expr)
		}
		CastExpr {
			w.add_variant_payload(sizeof(CastExpr))
			w.scan_dynamic(expr)
		}
		ComptimeExpr {
			w.add_variant_payload(sizeof(ComptimeExpr))
			w.scan_dynamic(expr)
		}
		EmptyExpr {}
		FnLiteral {
			w.add_variant_payload(sizeof(FnLiteral))
			w.scan_dynamic(expr)
		}
		GenericArgOrIndexExpr {
			w.add_variant_payload(sizeof(GenericArgOrIndexExpr))
			w.scan_dynamic(expr)
		}
		GenericArgs {
			w.add_variant_payload(sizeof(GenericArgs))
			w.scan_dynamic(expr)
		}
		Ident {
			w.add_variant_payload(sizeof(Ident))
			w.scan_dynamic(expr)
		}
		IfExpr {
			w.add_variant_payload(sizeof(IfExpr))
			w.scan_dynamic(expr)
		}
		IfGuardExpr {
			w.add_variant_payload(sizeof(IfGuardExpr))
			w.scan_dynamic(expr)
		}
		IndexExpr {
			w.add_variant_payload(sizeof(IndexExpr))
			w.scan_dynamic(expr)
		}
		InfixExpr {
			w.add_variant_payload(sizeof(InfixExpr))
			w.scan_dynamic(expr)
		}
		InitExpr {
			w.add_variant_payload(sizeof(InitExpr))
			w.scan_dynamic(expr)
		}
		Keyword {}
		KeywordOperator {
			w.add_variant_payload(sizeof(KeywordOperator))
			w.scan_dynamic(expr)
		}
		LambdaExpr {
			w.add_variant_payload(sizeof(LambdaExpr))
			w.scan_dynamic(expr)
		}
		LifetimeExpr {
			w.add_variant_payload(sizeof(LifetimeExpr))
			w.scan_dynamic(expr)
		}
		LockExpr {
			w.add_variant_payload(sizeof(LockExpr))
			w.scan_dynamic(expr)
		}
		MapInitExpr {
			w.add_variant_payload(sizeof(MapInitExpr))
			w.scan_dynamic(expr)
		}
		MatchExpr {
			w.add_variant_payload(sizeof(MatchExpr))
			w.scan_dynamic(expr)
		}
		ModifierExpr {
			w.add_variant_payload(sizeof(ModifierExpr))
			w.scan_dynamic(expr)
		}
		OrExpr {
			w.add_variant_payload(sizeof(OrExpr))
			w.scan_dynamic(expr)
		}
		ParenExpr {
			w.add_variant_payload(sizeof(ParenExpr))
			w.scan_dynamic(expr)
		}
		PostfixExpr {
			w.add_variant_payload(sizeof(PostfixExpr))
			w.scan_dynamic(expr)
		}
		PrefixExpr {
			w.add_variant_payload(sizeof(PrefixExpr))
			w.scan_dynamic(expr)
		}
		RangeExpr {
			w.add_variant_payload(sizeof(RangeExpr))
			w.scan_dynamic(expr)
		}
		SelectExpr {
			w.add_variant_payload(sizeof(SelectExpr))
			w.scan_dynamic(expr)
		}
		SelectorExpr {
			w.add_variant_payload(sizeof(SelectorExpr))
			w.scan_dynamic(expr)
		}
		SqlExpr {
			w.add_variant_payload(sizeof(SqlExpr))
			w.scan_dynamic(expr)
		}
		StringInterLiteral {
			w.add_variant_payload(sizeof(StringInterLiteral))
			w.scan_dynamic(expr)
		}
		StringLiteral {
			w.add_variant_payload(sizeof(StringLiteral))
			w.scan_dynamic(expr)
		}
		Tuple {
			w.add_variant_payload(sizeof(Tuple))
			w.scan_dynamic(expr)
		}
		UnsafeExpr {
			w.add_variant_payload(sizeof(UnsafeExpr))
			w.scan_dynamic(expr)
		}
		FieldInit, Type {}
	}
}

fn (mut w LegacyAstWalker) walk_type(typ Type) {
	w.stats.type_nodes++
	// Sumtype slot is counted by parent; add only heap variant payload.
	match typ {
		AnonStructType {
			w.add_variant_payload(sizeof(AnonStructType))
			w.scan_dynamic(typ)
		}
		ArrayFixedType {
			w.add_variant_payload(sizeof(ArrayFixedType))
			w.scan_dynamic(typ)
		}
		ArrayType {
			w.add_variant_payload(sizeof(ArrayType))
			w.scan_dynamic(typ)
		}
		ChannelType {
			w.add_variant_payload(sizeof(ChannelType))
			w.scan_dynamic(typ)
		}
		FnType {
			w.add_variant_payload(sizeof(FnType))
			w.scan_dynamic(typ)
		}
		GenericType {
			w.add_variant_payload(sizeof(GenericType))
			w.scan_dynamic(typ)
		}
		MapType {
			w.add_variant_payload(sizeof(MapType))
			w.scan_dynamic(typ)
		}
		NilType {}
		NoneType {}
		OptionType {
			w.add_variant_payload(sizeof(OptionType))
			w.scan_dynamic(typ)
		}
		PointerType {
			w.add_variant_payload(sizeof(PointerType))
			w.scan_dynamic(typ)
		}
		ResultType {
			w.add_variant_payload(sizeof(ResultType))
			w.scan_dynamic(typ)
		}
		ThreadType {
			w.add_variant_payload(sizeof(ThreadType))
			w.scan_dynamic(typ)
		}
		TupleType {
			w.add_variant_payload(sizeof(TupleType))
			w.scan_dynamic(typ)
		}
	}
}

// Plain-struct walkers: storage is counted at parent (inline field) or array
// level. Only scan for dynamic content here.

fn (mut w LegacyAstWalker) walk_attribute(attr Attribute) {
	w.stats.aux_nodes++
	w.scan_dynamic(attr)
}

fn (mut w LegacyAstWalker) walk_field_init(field FieldInit) {
	w.stats.aux_nodes++
	w.scan_dynamic(field)
}

fn (mut w LegacyAstWalker) walk_field_decl(field FieldDecl) {
	w.stats.aux_nodes++
	w.scan_dynamic(field)
}

fn (mut w LegacyAstWalker) walk_parameter(param Parameter) {
	w.stats.aux_nodes++
	w.scan_dynamic(param)
}

fn (mut w LegacyAstWalker) walk_match_branch(branch MatchBranch) {
	w.stats.aux_nodes++
	w.scan_dynamic(branch)
}

fn (mut w LegacyAstWalker) walk_string_inter(inter StringInter) {
	w.stats.aux_nodes++
	w.scan_dynamic(inter)
}

fn (mut w LegacyAstWalker) scan_dynamic[T](node T) {
	$for field in T.fields {
		$if field.typ is string {
			w.add_string(node.$(field.name))
		} $else $if field.typ is []string {
			arr := node.$(field.name)
			w.add_array_storage(sizeof(string), arr.len)
			for v in arr {
				w.add_string(v)
			}
		} $else $if field.typ is Expr {
			w.walk_expr(node.$(field.name))
		} $else $if field.typ is []Expr {
			arr := node.$(field.name)
			w.add_array_storage(sizeof(Expr), arr.len)
			for v in arr {
				w.walk_expr(v)
			}
		} $else $if field.typ is Stmt {
			w.walk_stmt(node.$(field.name))
		} $else $if field.typ is []Stmt {
			arr := node.$(field.name)
			w.add_array_storage(sizeof(Stmt), arr.len)
			for v in arr {
				w.walk_stmt(v)
			}
		} $else $if field.typ is Type {
			w.walk_type(node.$(field.name))
		} $else $if field.typ is []Type {
			arr := node.$(field.name)
			w.add_array_storage(sizeof(Type), arr.len)
			for v in arr {
				w.walk_type(v)
			}
		} $else $if field.typ is Attribute {
			w.walk_attribute(node.$(field.name))
		} $else $if field.typ is []Attribute {
			arr := node.$(field.name)
			w.add_array_storage(sizeof(Attribute), arr.len)
			for v in arr {
				w.walk_attribute(v)
			}
		} $else $if field.typ is FieldInit {
			w.walk_field_init(node.$(field.name))
		} $else $if field.typ is []FieldInit {
			arr := node.$(field.name)
			w.add_array_storage(sizeof(FieldInit), arr.len)
			for v in arr {
				w.walk_field_init(v)
			}
		} $else $if field.typ is FieldDecl {
			w.walk_field_decl(node.$(field.name))
		} $else $if field.typ is []FieldDecl {
			arr := node.$(field.name)
			w.add_array_storage(sizeof(FieldDecl), arr.len)
			for v in arr {
				w.walk_field_decl(v)
			}
		} $else $if field.typ is Parameter {
			w.walk_parameter(node.$(field.name))
		} $else $if field.typ is []Parameter {
			arr := node.$(field.name)
			w.add_array_storage(sizeof(Parameter), arr.len)
			for v in arr {
				w.walk_parameter(v)
			}
		} $else $if field.typ is MatchBranch {
			w.walk_match_branch(node.$(field.name))
		} $else $if field.typ is []MatchBranch {
			arr := node.$(field.name)
			w.add_array_storage(sizeof(MatchBranch), arr.len)
			for v in arr {
				w.walk_match_branch(v)
			}
		} $else $if field.typ is StringInter {
			w.walk_string_inter(node.$(field.name))
		} $else $if field.typ is []StringInter {
			arr := node.$(field.name)
			w.add_array_storage(sizeof(StringInter), arr.len)
			for v in arr {
				w.walk_string_inter(v)
			}
		} $else $if field.typ is Ident {
			w.walk_expr(Expr(node.$(field.name)))
		} $else $if field.typ is []Ident {
			arr := node.$(field.name)
			w.add_array_storage(sizeof(Ident), arr.len)
			for v in arr {
				w.walk_expr(Expr(v))
			}
		} $else $if field.typ is ImportStmt {
			w.walk_stmt(Stmt(node.$(field.name)))
		} $else $if field.typ is []ImportStmt {
			arr := node.$(field.name)
			w.add_array_storage(sizeof(ImportStmt), arr.len)
			for v in arr {
				w.walk_stmt(Stmt(v))
			}
		}
	}
}
