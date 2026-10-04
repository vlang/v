module types

import os
import v.flat

// PreparedCollect is what a checker keeps of the declarations it collected
// before the rest of a program was parsed: a diagnostics server collects
// builtin and the modules builtin imports once, and each of its checks
// continues from there with the user's code (collect_continue).
@[heap]
pub struct PreparedCollect {
mut:
	// How much of the AST the preparation covered.
	nodes_len     int
	top_level_len int
	file_ids_len  int
	const_count   int
	// The last file pair of the prepared top-level index, and its module.
	last_trailing int = -1
	last_file     string
	last_module   string
	// The state of the collection steps that carry what they saw to the next
	// declaration.
	// Whether a check continued from the preparation, so that its nodes are
	// the prepared ones followed by its own.
	continued        bool
	c_structs        CStructRedeclarations
	c_fns            CFnRedeclarations
	public_c_structs map[string]bool
	// The names the prepared declarations use as keys that code of another
	// module can use as well, where the order of the two decides the entry,
	// each with the `.file` markers of the prepared files that use it; the
	// import aliases of the prepared files; the modules they declare.
	keys           map[string][]int
	import_aliases map[string][]AliasUse
	modules        map[string]bool
	// What the scans for unused declarations look for in the prepared nodes,
	// which each check would otherwise walk again: the identifiers and
	// selectors (their names and last segments), the callees (the same), and
	// the functions the prepared nodes name as values.
	named          map[string]bool
	called         map[string]bool
	function_named map[string]bool
}

// AliasUse is an import alias of a file: the module it names, and the file.
struct AliasUse {
	path   string
	marker int
}

// DeclarationKeys are the keys the declarations of some files use, which the
// declarations of another module can use as well.
struct DeclarationKeys {
mut:
	keys    map[string][]int
	aliases map[string][]AliasUse
	modules map[string]bool
	// Imports that name a module with an alias of their own or select from it.
	selective_imports map[string]bool
}

// prepare_collect collects `a`, the code parsed so far, and keeps what
// collect_continue needs to collect the code parsed after it. False when the
// prepared code has diagnostics of its own: a check reports those in order.
pub fn (mut tc TypeChecker) prepare_collect(a &flat.FlatAst) bool {
	mut prepared := &PreparedCollect{}
	tc.prepared_collect = prepared
	tc.collect(a)
	if tc.errors.len > 0 || tc.notices.len > 0 || tc.building_v_fast {
		tc.prepared_collect = unsafe { nil }
		return false
	}
	prepared.nodes_len = a.nodes.len
	prepared.top_level_len = tc.top_level_idx.len
	prepared.file_ids_len = a.file_node_ids.len
	prepared.const_count = tc.const_exprs.len
	// Builtin comes first in both orders: only the modules after it can swap
	// places with the user's code, and a key builtin declares too keeps the
	// entry of builtin in both, except the signature of a C function, which the
	// last declaration writes.
	mut first_after_builtin := 0
	for first_after_builtin + 1 < a.file_node_ids.len
		&& a.file_node_ids[first_after_builtin] < a.user_code_start {
		first_after_builtin += 2
	}
	builtin := declaration_keys_of_files(a, 0, first_after_builtin)
	after := declaration_keys_of_files(a, first_after_builtin, a.file_node_ids.len)
	for key, markers in after.keys {
		if key !in builtin.keys || key.starts_with('C.') {
			prepared.keys[key] = markers
		}
	}
	prepared.import_aliases = after.aliases.clone()
	prepared.modules = after.modules.clone()
	tc.index_prepared_names(a)
	tc.prepare_for_checks(a)
	return true
}

// prepare_for_checks does, once in the server, what each check would do again
// for the prepared code. Every check asks of each struct whether it implements
// IError (see ierror_impl_names): a prepared struct answers the same in all of
// them, and the answer stays in the cache that the checks read below their own.
// And the node-indexed caches get room for the nodes of the user's code, which
// a check would otherwise add by copying them whole (see extend_node_caches).
fn (mut tc TypeChecker) prepare_for_checks(a &flat.FlatAst) {
	for name, _ in tc.structs {
		// These two depend on the file and the module that ask.
		if name !in ['Error', 'MessageError'] {
			tc.named_type_compatible_with_ierror(name)
		}
	}
	room := a.nodes.len + int_max(a.nodes.len, 1 << 20)
	tc.reserve_transform_node_caches(room)
	reserve_bool_cache(mut tc.lexical_smartcast_misses, room)
}

// index_prepared_names records what the scans for unused declarations look for
// in the prepared nodes, leaving out those a check prunes as inactive
// compile-time branches before it scans.
fn (mut tc TypeChecker) index_prepared_names(a &flat.FlatAst) {
	mut prepared := tc.prepared_collect
	mut pruned := map[int]bool{}
	for id in tc.inactive_top_level_node_ids {
		pruned[id] = true
	}
	for i in 0 .. prepared.nodes_len {
		if pruned[i] {
			continue
		}
		node := a.nodes[i]
		if node.kind in [.ident, .selector] && node.value.len > 0 {
			prepared.named[node.value] = true
			prepared.named[short_name_view(node.value)] = true
			if name := tc.resolved_fn_value_name(flat.NodeId(i)) {
				prepared.function_named[name] = true
			}
		}
		if node.kind == .call && node.children_count > 0 {
			callee := a.child_node(&node, 0)
			if callee.value.len > 0 {
				prepared.called[callee.value] = true
				prepared.called[short_name_view(callee.value)] = true
			}
		}
	}
}

// prepared_names_start returns where the nodes that the scans for unused
// declarations still walk start: after the prepared ones, whose names
// index_prepared_names recorded, or at the first node.
fn (tc &TypeChecker) prepared_names_start() int {
	if isnil(tc.prepared_collect) || !tc.prepared_collect.continued {
		return 0
	}
	return tc.prepared_collect.nodes_len
}

// declaration_keys_of_files returns the keys that the declarations of the
// file pairs of a.file_node_ids in [first, end) use, which declarations of
// another module can use as well.
fn declaration_keys_of_files(a &flat.FlatAst, first int, end int) DeclarationKeys {
	mut found := DeclarationKeys{}
	for k := first; k + 1 < end; k += 2 {
		marker := int(a.file_node_ids[k])
		trailing := a.nodes[a.file_node_ids[k + 1]]
		for ci in 0 .. trailing.children_count {
			found.add_declaration_keys(a, int(a.child(&trailing, ci)), marker)
		}
	}
	return found
}

// add_declaration_keys adds the keys of the declaration `i` of the file of
// `marker`, found as collect_index_child finds it.
fn (mut found DeclarationKeys) add_declaration_keys(a &flat.FlatAst, i int, marker int) {
	if i < 0 || i >= a.nodes.len {
		return
	}
	node := a.nodes[i]
	match node.kind {
		.comptime_if, .block {
			for ci in 0 .. node.children_count {
				found.add_declaration_keys(a, int(a.child(&node, ci)), marker)
			}
		}
		.module_decl {
			found.modules[node.value] = true
		}
		.import_decl {
			short_name := node.value.all_after_last('.')
			alias := if node.typ.len > 0 { node.typ } else { short_name }
			found.aliases[alias] << AliasUse{
				path:   node.value
				marker: marker
			}
			if alias != short_name || node.children_count > 0 {
				found.selective_imports[node.value] = true
			}
		}
		.fn_decl, .c_fn_decl, .struct_decl, .type_decl, .interface_decl, .enum_decl {
			found.keys[node.value] << marker
			short_name := node.value.all_after_last('.')
			if short_name != node.value {
				found.keys[short_name] << marker
			}
			if node.kind == .c_fn_decl && !node.value.starts_with('C.') {
				found.keys['C.${node.value}'] << marker
			}
		}
		.global_decl {
			for ci in 0 .. node.children_count {
				found.keys[a.child_node(&node, ci).value] << marker
			}
		}
		else {}
	}
}

// prepared_collect_conflict returns what the code parsed after the
// preparation shares with the prepared code, where collecting it after that
// code gives another entry than collecting it in `logical_file_order` (whose
// first file wins a key the first-come way, and loses it the last-come way),
// or ''. Without an order, any shared key conflicts.
fn prepared_collect_conflict(a &flat.FlatAst, prepared &PreparedCollect, logical_file_order []int) string {
	mut position := map[int]int{}
	for i, marker in logical_file_order {
		position[marker] = i
	}
	added := declaration_keys_of_files(a, prepared.file_ids_len, a.file_node_ids.len)
	for key, markers in added.keys {
		prepared_markers := prepared.keys[key] or { continue }
		if precedes_any(markers, prepared_markers, position) {
			return 'the name `${key}`'
		}
	}
	for name, _ in added.modules {
		if name in prepared.modules {
			return 'the module `${name}`'
		}
	}
	for path, _ in added.selective_imports {
		if path in prepared.modules {
			return 'the import of `${path}`'
		}
	}
	for alias, uses in added.aliases {
		prepared_uses := prepared.import_aliases[alias] or { continue }
		for used in uses {
			for prepared_use in prepared_uses {
				if prepared_use.path != used.path
					&& precedes_any([used.marker], [prepared_use.marker], position) {
					return 'the import alias `${alias}`'
				}
			}
		}
	}
	return ''
}

// precedes_any reports whether a file of `markers` comes before a file of
// `prepared_markers` in the order of `position`, or either lacks a place in it.
fn precedes_any(markers []int, prepared_markers []int, position map[int]int) bool {
	for marker in markers {
		pos := position[marker] or { return true }
		for prepared_marker in prepared_markers {
			prepared_pos := position[prepared_marker] or { return true }
			if pos < prepared_pos {
				return true
			}
		}
	}
	return false
}

// collect_continue collects the code parsed after prepare_collect, as collect
// collects the whole program in logical_file_order. It returns false, and
// changes nothing, when that code shares a key with the prepared code whose
// entry the order of collection decides: the program is then collected anew.
pub fn (mut tc TypeChecker) collect_continue(a &flat.FlatAst) bool {
	if !tc.can_continue_collect(a, tc.logical_file_order) {
		return false
	}
	mut prepared := tc.prepared_collect
	start := prepared.nodes_len
	n := a.nodes.len
	tc.a = a
	tc.static_associated_signature_count = -1
	tc.has_spawn_expr = -1
	tc.direct_dependencies_by_fn = map[int][]SymbolId{}
	tc.cur_scope = tc.file_scope
	tc.scope_pool_index = 0
	tc.extend_collect_node_caches(n)
	tc.extend_direct_parent_index(a, start)
	// The top-level index of the new files.
	mut inactive := []bool{}
	for k := prepared.file_ids_len; k + 1 < a.file_node_ids.len; k += 2 {
		trailing := a.nodes[a.file_node_ids[k + 1]]
		for i in 0 .. trailing.children_count {
			tc.mark_inactive_top_level_comptime(a.child(&trailing, i), mut inactive)
		}
	}
	tc.collect_top_level_idx_fast_from(a, inactive, prepared.file_ids_len, prepared.last_trailing,
		prepared.last_file, prepared.last_module)
	new_entries := tc.top_level_idx[prepared.top_level_len..].clone()
	// The user's code comes right after builtin in the logical order, so a
	// loop that carries the module from file to file enters it from builtin.
	start_module := 'builtin'
	if tc.enclosing_generic_param_masks.len < n {
		tc.enclosing_generic_param_masks << []u32{len: n - tc.enclosing_generic_param_masks.len}
	}
	tc.index_enclosing_generic_params(a, new_entries)
	tc.index_type_declarations(a, new_entries, start_module)
	tc.index_declaration_param_mutability(a, new_entries, start_module)
	tc.index_fn_names(a, new_entries, start_module)
	tc.index_file_declarations(a, new_entries)
	if !tc.valid_diagnostic_fast {
		tc.index_module_import_lines_of_new_files(a)
	}
	tc.collect_pass1(a, new_entries, false)
	tc.check_alias_declaration_cycles_of(new_entries, start_module)
	tc.cache_fn_generic_params_of(a, new_entries)
	tc.type_cache.clear_c_type_entries()
	tc.invalidate_short_type_name_index()
	tc.check_c_struct_redeclarations_of(a, new_entries, mut prepared.c_structs)
	tc.check_c_fn_redeclarations_of(a, new_entries, mut prepared.c_fns)
	$if 'c' == 'arm64' {
		tc.type_cache.parse_enabled = false
	} $else {
		tc.type_cache.parse_enabled = true
	}
	tc.cur_module = ''
	fast_pass2_registration := os.getenv('V3_NO_FAST_PASS2_REGISTRATION') == ''
	_, _ = tc.collect_pass2(a, new_entries, []Pass2FnPrep{}, fast_pass2_registration, mut
		prepared.public_c_structs)
	tc.collect_deprecated_symbols_of(new_entries)
	tc.resolve_inferred_global_types_of(a, new_entries)
	mut new_consts := map[string]flat.NodeId{}
	for i, name in tc.const_exprs.keys() {
		if i >= prepared.const_count {
			new_consts[name] = tc.const_exprs[name]
		}
	}
	tc.resolve_const_types_of(new_consts)
	// Globals may depend on constants whose initializer types were still pending.
	tc.resolve_inferred_global_types_of(a, new_entries)
	tc.static_associated_method_names = map[string]bool{}
	for name, _ in tc.fn_ret_types {
		if _, method := flat.decode_static_type_method_name(name) {
			tc.static_associated_method_names[method] = true
		}
	}
	tc.static_associated_signature_count = tc.fn_ret_types.len
	tc.build_const_suffixes()
	tc.build_struct_embed_index()
	if !isnil(tc.visible_mutation_cache) {
		mut visible_mutation_cache := tc.visible_mutation_cache
		visible_mutation_cache.decl_index_ready = true
	}
	tc.top_level_idx_nodes_len = n
	tc.order_top_level_idx_by_files()
	tc.enter_last_top_level_file()
	prepared.continued = true
	return true
}

// extend_collect_node_caches grows to `n` nodes the node-indexed caches that
// collect sizes for the whole AST.
fn (mut tc TypeChecker) extend_collect_node_caches(n int) {
	tc.extend_node_caches(n)
	extend_bool_cache(mut tc.lexical_smartcast_misses, n)
}

// extend_direct_parent_index adds the nodes from `start` on to the parent index.
fn (mut tc TypeChecker) extend_direct_parent_index(a &flat.FlatAst, start int) {
	tc.invalidate_lexical_parent_memo()
	grow := a.nodes.len - tc.direct_parent_ids.len
	if grow > 0 {
		tc.direct_parent_ids << flat.empty_node_ids(grow)
		tc.value_used_nodes << []bool{len: grow}
	}
	chunk := tc.fill_direct_parent_edges_range(a, start, a.nodes.len)
	tc.merge_direct_parent_chunk(chunk)
	for idx in chunk.metadata_node_ids {
		tc.collect_direct_parent_node_metadata(a, idx, a.nodes[idx])
	}
	tc.preflight_index_nodes_len = a.nodes.len
	tc.direct_parent_index_trusted = true
}

// enter_last_top_level_file leaves the checker in the file and module of the
// last declaration of the top-level index, as the walks of collect over the
// whole index leave it.
fn (mut tc TypeChecker) enter_last_top_level_file() {
	mut last := -1
	for i := tc.top_level_idx.len - 1; i >= 0; i-- {
		if tc.a.nodes[tc.top_level_idx[i]].kind == .file {
			last = i
			break
		}
	}
	if last < 0 {
		return
	}
	for i in last .. tc.top_level_idx.len {
		node := tc.a.nodes[tc.top_level_idx[i]]
		if node.kind == .file {
			tc.enter_file(node.value)
		} else if node.kind == .module_decl {
			tc.enter_module(node.value)
		}
	}
}

// can_continue_collect reports whether collect_continue can collect `a` after
// the prepared declarations, the files taken in `logical_file_order`.
pub fn (tc &TypeChecker) can_continue_collect(a &flat.FlatAst, logical_file_order []int) bool {
	return tc.continue_collect_conflict(a, logical_file_order) == ''
}

// continue_collect_conflict returns why collect_continue cannot collect `a`
// after the prepared declarations, or ''.
pub fn (tc &TypeChecker) continue_collect_conflict(a &flat.FlatAst, logical_file_order []int) string {
	if isnil(tc.prepared_collect) || tc.building_v_fast {
		return 'nothing is prepared'
	}
	prepared := tc.prepared_collect
	if a.nodes.len < prepared.nodes_len || a.file_node_ids.len < prepared.file_ids_len
		|| tc.top_level_idx.len != prepared.top_level_len || !file_index_usable(a) {
		return 'the program is not the one prepared'
	}
	return prepared_collect_conflict(a, prepared, logical_file_order)
}
