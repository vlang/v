module transform

import v.gen.c.naming { sum_field_name }
import strings
import time
import v.flat
import v.types { unalias_type }

const scoped_monomorph_scan_nodes = 32768
const semantic_map_key_any_type_id = flat.source_type_id('any')
const generic_main_module_id = flat.source_type_id('main')
const generic_anonymous_struct_type_id = flat.source_type_id('struct')
const generic_c_module_type_id = flat.source_type_id('C')
const generic_js_module_type_id = flat.source_type_id('JS')

struct GenericStructDecl {
	id     flat.NodeId
	node   flat.Node
	file   string
	module string
	key    string
}

struct GenericSumDecl {
	id     flat.NodeId
	node   flat.Node
	file   string
	module string
	key    string
}

struct GenericSpecContext {
	base   u32
	file   string
	module string
}

struct ComptimeTypeLayout {
	size  int
	align int
}

// collect_generic_specs_range_scoped bounds the temporary type parsing and
// string splitting needed when a monomorphization round scans newly generated
// nodes. Only the small set of discovered specs escapes each scratch arena.
fn (mut t Transformer) collect_generic_specs_range_scoped(struct_decls map[u32]GenericStructDecl, sum_decls map[u32]GenericSumDecl, mut struct_specs map[u32]u32, mut sum_specs map[u32]GenericSpecContext, first int, last int) {
	if first >= last {
		return
	}
	t.tc.freeze_type_cache_for_forks()
	defer {
		t.tc.unfreeze_type_cache_after_forks()
	}
	mut start := first
	for start < last {
		end := if start + scoped_monomorph_scan_nodes < last {
			start + scoped_monomorph_scan_nodes
		} else {
			last
		}
		scope := transform_worker_scope_begin(true)
		mut wtc := t.tc.fork_for_parallel_transform(t.a)
		mut w := t.fork_scoped_batch_worker(t.a, wtc)
		// The master has already extended these append-only context arrays to
		// `last`; workers only read them while collecting type spellings.
		w.node_module_map_cache = t.node_module_map_cache
		w.node_file_map_cache = t.node_file_map_cache
		w.node_context_texts = t.node_context_texts
		w.node_module_map_nodes = t.node_module_map_nodes
		w.node_context_lazy_end = t.node_context_lazy_end
		w.node_context_read_only = true
		mut batch_struct_specs := map[u32]u32{}
		mut batch_sum_specs := map[u32]GenericSpecContext{}
		w.collect_generic_struct_specs_range(struct_decls, mut batch_struct_specs, start, end)
		w.collect_generic_sum_specs_range(sum_decls, mut batch_sum_specs, start, end)
		transform_worker_scope_leave(scope)
		for spec, base_id in batch_struct_specs {
			struct_specs[spec] = base_id
		}
		for spec, context in batch_sum_specs {
			sum_specs[spec] = GenericSpecContext{
				base:   context.base
				file:   context.file.clone()
				module: context.module.clone()
			}
		}
		for key in w.generic_specialization_args.keys() {
			args_id := w.generic_specialization_args.key_ids(key) or { continue }
			if t.generic_specialization_args.contains_key(key) {
				continue
			}
			t.generic_specialization_args.set_key_ids(key, args_id)
		}
		transform_worker_scope_free(scope)
		start = end
	}
}

struct GenericCallSite {
	id     flat.NodeId
	module string
}

// MonomorphPassState is what a monomorphize_pass has scanned. The next pass of
// the wrapper loop continues from it instead of scanning the whole tree again.
struct MonomorphPassState {
mut:
	ready                        bool
	ignored_nodes                []bool
	emitted                      GenericFnSpecBoolMap
	generic_call_sites           []GenericCallSite
	recorded_call_sites          map[int]bool
	generic_refresh_call_parents map[int][]int
	generic_refresh_alias_calls  []int
	generic_refresh_scan_start   int
	scan_start                   int
	generic_fn_value_scan_start  int
	interface_box_scan_start     int
	used_fns_at_scan             int
}

struct PendingGenericFnSpec {
	decl GenericFnDecl
	args flat.TypeTextList
	key  GenericFnSpecKey
}

fn (mut t Transformer) is_generic_fn(name string) bool {
	if t.skip_generics {
		return false
	}
	decls := t.cached_generic_fn_decls()
	return name in decls || transform_qualified_fn_name(t.cur_module, name) in decls
}

fn (mut t Transformer) is_generic_struct(name_id u32) bool {
	if t.skip_generics {
		return false
	}
	syntax := flat.source_type_syntax(name_id)
	if syntax.generic_app_valid() {
		return syntax.generic_app_base() in t.structs
	}
	if info := t.lookup_struct_info(name_id) {
		return flat.source_type_shape(info.name).has(.bracketed)
	}
	return false
}

fn (mut t Transformer) monomorphize_pass() []string {
	debug_started := time.ticks()
	if t.parallel_monomorphize {
		t.tc.reset_resolution_type_view_cache()
	} else {
		// The serial pass mutates the master semantic maps as it materializes
		// generic structs. Do not retain lookup views across those mutations.
		t.tc.disable_resolution_type_view_cache()
	}
	defer {
		t.tc.disable_resolution_type_view_cache()
		t.defer_nested_generic_emissions = false
		t.pending_generic_fn_specs.clear()
		t.pending_generic_fn_spec_keys.clear()
	}
	// Generic cloning reuses one scratch child buffer, so nested specializations
	// must be queued until the current clone has finished using that buffer.
	t.defer_nested_generic_emissions = true
	decls := t.cached_generic_fn_decls()
	t.monomorph_profile('mono driver decls: ${time.ticks() - debug_started} ms')
	if decls.len == 0 {
		return []string{}
	}
	methods_by_receiver := generic_struct_methods_by_receiver(decls)
	mut method_level_params := map[string]bool{}
	for _, methods in methods_by_receiver {
		for decl in methods {
			method_level_params[decl.key] = t.generic_decl_has_method_level_params(decl)
		}
	}
	interfaces_by_method := t.generic_struct_interfaces_by_method()
	mut interface_impl_cache := map[u64]bool{}
	// Generic cloning is recursive. Reuse one stack-shaped child-id buffer instead
	// of allocating a temporary array for every cloned AST node.
	// Its live length is bounded by the active generic subtree, not by the whole
	// program; let exceptional deep trees grow it instead of reserving millions
	// of entries up front.
	t.generic_clone_children.ensure_cap(65536)
	// A later pass continues the previous one: it scans the nodes appended since
	// and the bodies that became reachable, not the whole tree again. Nodes the
	// previous pass scanned keep their specializations and recorded call sites;
	// type refresh revisits their recorded calls and any newly appended parents.
	resume := t.mono_pass_state.ready
	struct_decls := t.collect_generic_struct_decls()
	t.monomorph_profile('mono driver struct decls: ${time.ticks() - debug_started} ms')
	if !resume {
		t.materialize_generic_sum_types(false)
	}
	t.monomorph_profile('mono driver sum materialize: ${time.ticks() - debug_started} ms')
	t.seed_generic_specialization_args(struct_decls)
	sum_decls := t.collect_generic_sum_decls()
	t.materialize_generic_struct_specs(t.generic_struct_specs_cache, struct_decls)
	t.monomorph_profile('mono driver struct materialize: ${time.ticks() - debug_started} ms')
	state := if resume {
		t.mono_pass_state
	} else {
		MonomorphPassState{
			ignored_nodes:            t.monomorphize_ignored_nodes(decls)
			interface_box_scan_start: t.a.nodes.len
			used_fns_at_scan:         t.used_fn_count()
		}
	}
	t.mono_pass_state = MonomorphPassState{}
	mut ignored_nodes := unsafe { state.ignored_nodes }
	mut emitted := state.emitted
	mut generated := []string{}
	mut generic_call_sites := unsafe { state.generic_call_sites }
	mut recorded_call_sites := state.recorded_call_sites
	mut generic_refresh_call_parents := state.generic_refresh_call_parents
	mut generic_refresh_alias_calls := state.generic_refresh_alias_calls.clone()
	mut generic_refresh_scan_start := state.generic_refresh_scan_start
	mut changed := true
	mut scan_start := state.scan_start
	mut generic_fn_value_scan_start := state.generic_fn_value_scan_start
	mut interface_box_scan_start := state.interface_box_scan_start
	mut used_fns_at_scan := state.used_fns_at_scan
	mut generic_struct_specs := if t.generic_materialization_ready {
		t.generic_struct_specs_cache.clone()
	} else {
		t.generic_struct_specs(struct_decls)
	}
	mut generic_sum_specs := if t.generic_materialization_ready {
		t.generic_sum_specs_cache.clone()
	} else {
		map[u32]GenericSpecContext{}
	}
	mut generic_struct_specs_scan_start := if resume {
		t.generic_materialization_scan_from
	} else {
		t.a.nodes.len
	}
	mut lowered_operator_uses := t.lowered_generic_struct_operator_uses_for_specs(generic_struct_specs, decls)
	if t.parallel_monomorphize && !resume {
		t.prepare_parallel_monomorph_scan(0, t.a.nodes.len)
	}
	t.monomorph_profile('mono driver setup: ${time.ticks() - debug_started} ms')
	t.in_monomorphize_scan = true
	defer {
		t.in_monomorphize_scan = false
	}
	mut debug_round := 0
	for changed {
		debug_round++
		debug_round_started := time.ticks()
		changed = false
		t.ensure_node_module_map()
		t.monomorph_profile('mono round ${debug_round} modules: ${time.ticks() - debug_round_started} ms')
		node_count := t.a.nodes.len
		// Unlike the reachability scan below, this covers unused source bodies and
		// field defaults too: Cgen still emits those nodes, so each concrete generic
		// function value must name a valid specialization.
		for i in generic_fn_value_scan_start .. node_count {
			if i < t.ignored_comptime_for_nodes.len && t.ignored_comptime_for_nodes[i] {
				continue
			}
			if t.a.nodes[i].kind != .index {
				continue
			}
			// Specialization can append nodes; copy only candidate headers.
			node := t.a.nodes[i]
			// Synthetic nodes without source context are handled by the pass that
			// created them. Guessing their module here can request a duplicate
			// specialization with unqualified type arguments.
			if t.node_file_or(i, '').len == 0 {
				continue
			}
			decl_key, args_source_ids_3 := t.explicit_generic_fn_value_specialization(flat.NodeId(i), node, t.node_module_or(i, ''), decls) or {
				// A generic method named on a value with its type arguments.
				if asked := t.bind_explicit_generic_method_value(flat.NodeId(i), node,
					t.node_module_or(i, ''), decls)
				{
					changed = changed || asked
				}
				continue
			}

			decl := decls[decl_key] or { continue }
			concrete_args_source_ids := t.canonical_generic_specialization_args(args_source_ids_3)

			if !t.generic_specialization_registered(decl, concrete_args_source_ids) {
				t.library_bodies.request_file = t.node_file_or(i, '')
				t.request_generic_fn_specialization(decl, concrete_args_source_ids)
				t.library_bodies.request_file = ''
				changed = true
			}
			spec_value := specialized_generic_fn_value(decl.node.value_text(), concrete_args_source_ids)
			spec_name := transform_qualified_fn_name(decl.module, spec_value)
			t.record_generic_specialization_args_for_names([spec_name, c_name(spec_name)], concrete_args_source_ids)
			fn_type_source_id := t.specialized_generic_fn_type_text(decl, concrete_args_source_ids)

			t.set_node(i, flat.Node{
				kind:  .ident
				value: spec_name
				typ:   fn_type_source_id
				pos:   node.pos
			})
		}
		generic_fn_value_scan_start = node_count
		// A previous round can mark an until-then unused fn as used (a clone
		// or comptime unroll now calls it); its subtree left the ignore set
		// and must be scanned too, or its generic calls stay unspecialized.
		mut rescan := []int{}
		if scan_start > 0 && t.used_fn_count() != used_fns_at_scan {
			new_ignored := t.monomorphize_ignored_nodes(decls)
			rescan = unignored_nodes(ignored_nodes, new_ignored, scan_start)
			ignored_nodes = unsafe { new_ignored }
		}
		used_fns_at_scan = t.used_fn_count()
		t.monomorph_profile('mono round ${debug_round} ignored: ${time.ticks() - debug_round_started} ms')
		use_parallel_scan := t.parallel_monomorph_scan_start == scan_start
			&& t.parallel_monomorph_scan_end == node_count
		if generic_struct_specs_scan_start < node_count {
			spec_count := generic_struct_specs.len
			if use_parallel_scan && generic_struct_specs_scan_start == scan_start {
				for spec, base_id in t.parallel_monomorph_struct_specs {
					generic_struct_specs[spec] = base_id
				}
				for spec, context in t.parallel_monomorph_sum_specs {
					generic_sum_specs[spec] = context
				}
				t.parallel_monomorph_struct_specs.clear()
				t.parallel_monomorph_sum_specs.clear()
			} else {
				if t.scope_parallel_workers {
					t.collect_generic_specs_range_scoped(struct_decls, sum_decls, mut generic_struct_specs, mut generic_sum_specs, generic_struct_specs_scan_start, node_count)
				} else {
					t.collect_generic_struct_specs_range(struct_decls, mut generic_struct_specs, generic_struct_specs_scan_start, node_count)
					t.collect_generic_sum_specs_range(sum_decls, mut generic_sum_specs, generic_struct_specs_scan_start, node_count)
				}
			}
			generic_struct_specs_scan_start = node_count
			if generic_struct_specs.len != spec_count {
				lowered_operator_uses = t.lowered_generic_struct_operator_uses_for_specs(generic_struct_specs, decls)
			}
		}
		t.monomorph_profile('mono round ${debug_round} specs: ${time.ticks() - debug_round_started} ms')
		new_scan_count := if use_parallel_scan {
			t.parallel_monomorph_scan_nodes.len
		} else {
			node_count - scan_start
		}
		for scan_idx in 0 .. rescan.len + new_scan_count {
			i := if scan_idx < rescan.len {
				rescan[scan_idx]
			} else if use_parallel_scan {
				t.parallel_monomorph_scan_nodes[scan_idx - rescan.len]
			} else {
				scan_start + (scan_idx - rescan.len)
			}
			if i < t.ignored_comptime_for_nodes.len && t.ignored_comptime_for_nodes[i] {
				continue
			}
			if t.a.nodes[i].kind !in [.index, .call, .index_assign, .selector_assign] {
				continue
			}
			// Helpers can rewrite or append nodes, so candidate headers stay owned.
			node := t.a.nodes[i]
			// Cgen still emits source bodies and field defaults that markused can prove
			// unreachable. Their concrete generic function values must nevertheless name
			// valid specializations so the generated translation unit compiles.
			if node.kind == .index {
				if t.node_file_or(i, '').len == 0 {
					continue
				}
				if decl_key, args_source_ids_4 := t.explicit_generic_fn_value_specialization(flat.NodeId(i), node, t.node_module_or(i, ''), decls) {
					decl := decls[decl_key] or { continue }
					concrete_args_source_ids_2 := t.canonical_generic_specialization_args(args_source_ids_4)

					if !t.generic_specialization_registered(decl, concrete_args_source_ids_2) {
						t.library_bodies.request_file = t.node_file_or(i, '')
						t.request_generic_fn_specialization(decl, concrete_args_source_ids_2)
						t.library_bodies.request_file = ''
						changed = true
					}
					spec_value := specialized_generic_fn_value(decl.node.value_text(), concrete_args_source_ids_2)
					spec_name := transform_qualified_fn_name(decl.module, spec_value)
					t.record_generic_specialization_args_for_names([spec_name, c_name(spec_name)], concrete_args_source_ids_2)
					fn_type_source_id_2 := t.specialized_generic_fn_type_text(decl, concrete_args_source_ids_2)

					t.set_node(i, flat.Node{
						kind:  .ident
						value: spec_name
						typ:   fn_type_source_id_2
						pos:   node.pos
					})
					continue
				}
				if asked := t.bind_explicit_generic_method_value(flat.NodeId(i), node,
					t.node_module_or(i, ''), decls)
				{
					changed = changed || asked
					continue
				}
			}
			if i < ignored_nodes.len && ignored_nodes[i] {
				continue
			}
			match node.kind {
				.call {
					// Past this point a call carries a recorded specialization, names its
					// callee's specialization exactly, or, in a source file, can name a
					// generic declaration.
					mut skip := false
					if i !in t.generic_call_spec_cache {
						skip = if t.node_file_or(i, '').len == 0 {
							!t.synthetic_generic_call_has_exact_identity(node)
						} else {
							!t.call_may_target_generic(node)
						}
					}
					if skip {
						continue
					}
					if t.call_is_recorded_struct_operator(node)
						|| t.record_lowered_generic_struct_operator_call(node, lowered_operator_uses) {
						continue
					}
					call_module := t.node_module_or(i, '')
					decl_key, args_source_ids := t.cached_generic_call_specialization(flat.NodeId(i), node, call_module, decls) or {
						t.missing_specialized_plain_generic_call(node, call_module, decls) or {
							continue
						}
					}

					decl := decls[decl_key] or { continue }
					if !t.call_has_source_generic_args(node)
						&& t.generic_args_contain_alias(args_source_ids, decl.module) {
						t.set_node_call_type_args(i, args_source_ids)
					}
					if i !in recorded_call_sites {
						generic_call_sites << GenericCallSite{
							id:     flat.NodeId(i)
							module: call_module
						}
						recorded_call_sites[i] = true
					}
					concrete_args_source_ids_3 := t.canonical_generic_specialization_args(args_source_ids)

					spec_key := generic_fn_spec_key(decl_key, concrete_args_source_ids_3)
					if emitted.contains(spec_key) {
						continue
					}
					if t.generic_specialization_registered(decl, concrete_args_source_ids_3) {
						emitted.set(spec_key, true)
						continue
					}
					t.library_bodies.request_file = t.node_file_or(i, '')
					t.request_generic_fn_specialization(decl, concrete_args_source_ids_3)
					t.library_bodies.request_file = ''
					emitted.set(spec_key, true)
					changed = true
				}
				.index {
					t.record_index_operator_fn_name(node, '[]')
				}
				.index_assign {
					if node.children_count > 0 {
						lhs := t.a.child_node(&node, 0)
						if lhs.kind == .index {
							t.record_index_operator_fn_name(lhs, '[]=')
							if node.op != .assign {
								t.record_index_operator_fn_name(lhs, '[]')
								t.record_compound_assign_operator_fn_name(t.a.child(&node, 0),
									node.op)
							}
						}
					}
				}
				.selector_assign {
					if node.children_count > 0 && node.op != .assign {
						t.record_compound_assign_operator_fn_name(t.a.child(&node, 0), node.op)
					}
				}
				else {}
			}
		}
		t.monomorph_profile('mono round ${debug_round} scan: ${time.ticks() - debug_round_started} ms (${node_count - scan_start} new)')
		scan_start = node_count
		if use_parallel_scan {
			t.parallel_monomorph_scan_nodes = []int{}
			t.parallel_monomorph_scan_start = 0
			t.parallel_monomorph_scan_end = 0
		}
		t.generic_struct_specs_cache = generic_struct_specs.clone()
		t.generic_sum_specs_cache = generic_sum_specs.clone()
		t.generic_materialization_scan_from = node_count
		t.generic_materialization_ready = true
		// Generic specialization can turn an unresolved interface conversion such as
		// `Sink[W] -> Interface` into a concrete boxed type. Include those newly
		// materialized boxes before deciding which interface methods need bodies.
		box_refresh_started := time.ticks()
		if interface_box_scan_start < node_count {
			was_frozen := t.interface_boxed_types_frozen
			t.interface_boxed_types_frozen = false
			t.collect_interface_boxed_types_range(interface_box_scan_start, node_count)
			t.interface_boxed_types_frozen = was_frozen
			t.monomorph_profile('mono round ${debug_round} box scan: ${time.ticks() - box_refresh_started} ms (${t.interface_boxed_types.len} keys)')
			interface_box_scan_start = node_count
		}
		box_index_started := time.ticks()
		t.refresh_interface_impl_indexes_for_boxed_types()
		t.monomorph_profile('mono round ${debug_round} box index: ${time.ticks() - box_index_started} ms')
		t.monomorph_profile('mono round ${debug_round} boxes: ${time.ticks() - box_refresh_started} ms')
		// Specialize methods of instantiated generic structs that are never reached
		// through an explicit call node — notably operator overloads (`a + b`), which
		// are infix expressions lowered to method calls only later in the pipeline.
		method_started := time.ticks()
		if t.specialize_generic_struct_methods(generic_struct_specs, decls, methods_by_receiver, method_level_params, interfaces_by_method, mut interface_impl_cache, mut emitted) {
			changed = true
		}
		mut generic_sum_method_specs := map[u32]u32{}
		for spec, context in generic_sum_specs {
			generic_sum_method_specs[spec] = context.base
		}
		if t.specialize_generic_struct_methods(generic_sum_method_specs, decls, methods_by_receiver, method_level_params, interfaces_by_method, mut interface_impl_cache, mut emitted) {
			changed = true
		}
		t.monomorph_profile('mono round ${debug_round} methods: ${time.ticks() - method_started} ms')
		if t.drain_pending_generic_fn_specs(struct_decls, sum_decls, mut emitted, mut generated) {
			changed = true
		}
		// Every specialization scans its own appended range while it is transformed,
		// including on parallel workers; do not rescan those nodes next round.
		interface_box_scan_start = t.a.nodes.len
		t.monomorph_profile('mono round ${debug_round} drain: ${time.ticks() - debug_round_started} ms')
	}
	t.monomorph_profile('mono driver rounds: ${time.ticks() - debug_started} ms')
	t.collect_generic_refresh_call_parents(generic_refresh_scan_start, t.a.nodes.len, mut generic_refresh_call_parents,
		mut generic_refresh_alias_calls)
	generic_refresh_scan_start = t.a.nodes.len
	mut rewritten_call_sites := t.rewrite_generic_call_sites(decls, generic_call_sites)
	rewritten_call_sites << generic_refresh_alias_calls
	t.monomorph_profile('mono driver rewrite: ${time.ticks() - debug_started} ms')
	refresh_nodes := generic_refresh_nodes_for_calls(rewritten_call_sites,
		generic_refresh_call_parents)
	if t.refresh_decl_assign_types_after_generic_rewrite(refresh_nodes) {
		t.monomorph_profile('mono driver refresh: ${time.ticks() - debug_started} ms')
		mut missed_call_sites := []int{cap: t.generic_call_spec_misses.len}
		for id, missed in t.generic_call_spec_misses {
			if missed {
				missed_call_sites << id
			}
		}
		missed_call_sites.sort()
		t.generic_call_spec_misses = map[int]bool{}
		extra_call_sites := t.collect_generic_call_sites_after_type_refresh(generic_struct_specs, decls, ignored_nodes, missed_call_sites, mut emitted, mut recorded_call_sites)
		if extra_call_sites.len > 0 {
			for t.drain_pending_generic_fn_specs(struct_decls, sum_decls, mut emitted, mut generated) {
			}
			t.collect_generic_refresh_call_parents(generic_refresh_scan_start, t.a.nodes.len,
				mut generic_refresh_call_parents, mut generic_refresh_alias_calls)
			generic_refresh_scan_start = t.a.nodes.len
			mut extra_rewrites := t.rewrite_generic_call_sites(decls, extra_call_sites)
			extra_rewrites << generic_refresh_alias_calls
			extra_refresh_nodes := generic_refresh_nodes_for_calls(extra_rewrites,
				generic_refresh_call_parents)
			t.refresh_decl_assign_types_after_generic_rewrite(extra_refresh_nodes)
		}
	}
	t.monomorph_profile('mono driver extra: ${time.ticks() - debug_started} ms')
	t.generic_struct_specs_cache = generic_struct_specs.move()
	t.generic_sum_specs_cache = generic_sum_specs.move()
	t.generic_materialization_scan_from = t.a.nodes.len
	t.generic_materialization_ready = true
	t.mono_pass_state = MonomorphPassState{
		ready:                        true
		ignored_nodes:                ignored_nodes
		emitted:                      emitted
		generic_call_sites:           generic_call_sites
		recorded_call_sites:          recorded_call_sites
		generic_refresh_call_parents: generic_refresh_call_parents
		generic_refresh_alias_calls:  generic_refresh_alias_calls
		generic_refresh_scan_start:   generic_refresh_scan_start
		scan_start:                   scan_start
		generic_fn_value_scan_start:  generic_fn_value_scan_start
		interface_box_scan_start:     interface_box_scan_start
		used_fns_at_scan:             used_fns_at_scan
	}
	t.monomorph_profile('mono driver finish: ${time.ticks() - debug_started} ms')
	return generated
}

fn (mut t Transformer) explicit_generic_fn_value_specialization(id flat.NodeId, node flat.Node, module_name string, decls map[string]GenericFnDecl) ?(string, []u32) {
	if node.kind != .index || node.children_count < 2 || node.value == 'range' {
		return none
	}
	base_id := t.a.child(&node, 0)
	base := t.a.nodes[int(base_id)]
	// Telling `xs[i]` from `f[int]` below resolves the base's type; rule out a base
	// that cannot name a generic function before paying that for every index.
	if !t.generic_fn_value_base_may_match(id, base_id, base, decls) {
		return none
	}
	return t.explicit_generic_fn_value_specialization_of(id, node, base_id, base, module_name,
		decls)
}

fn (mut t Transformer) explicit_generic_fn_value_specialization_of(id flat.NodeId, node flat.Node, base_id flat.NodeId, base flat.Node, module_name string, decls map[string]GenericFnDecl) ?(string, []u32) {
	// Empty for a value index too.
	type_args := t.generic_call_type_args(node)
	if type_args.len == 0 {
		return none
	}
	// A type argument that names a generic parameter of the declaration enclosing
	// this call is not concrete: the enclosing specialization substitutes it while
	// it is cloned. Resolving it here through the writing file's imports would
	// request a specialization for a same-named imported type (`import pkg { T }`)
	// instead of leaving the call to the clone.
	if t.type_args_have_enclosing_generic_param(id, type_args) {
		return none
	}
	raw_args_source_ids := normalize_generic_args(type_args, module_name)

	for candidate in t.explicit_generic_fn_value_decl_candidates(id, base_id, base, module_name) {
		decl_key := generic_fn_decl_base_value(candidate)
		decl := decls[decl_key] or { continue }
		params := t.generic_fn_param_names(decl.node, decl.module)
		if params.len == 0 || params.len != raw_args_source_ids.len {
			continue
		}
		args_source_ids := t.generic_call_args_for_decl(raw_args_source_ids, module_name, decl.module)

		if t.generic_args_have_placeholders(args_source_ids) {
			return none
		}
		for arg_source_id in args_source_ids {
			if !t.explicit_generic_arg_is_known_type_id(arg_source_id, module_name) {
				return none
			}
		}
		return decl_key, args_source_ids.clone()
	}
	return none
}

// explicit_generic_method_value_specialization returns the declaration of the
// generic method that the index `node` names on a value with its type arguments
// and without a call, `h.first[int]`, which the check typed as a method value
// (see types.TypeChecker.check_generic_method_value), and the types of all its
// type parameters: those its receiver gives, and those written.
fn (mut t Transformer) explicit_generic_method_value_specialization(id flat.NodeId, node flat.Node, module_name string, decls map[string]GenericFnDecl) ?(GenericFnDecl, []u32) {
	if isnil(t.tc) || node.children_count < 2 || node.value_text() == 'range' {
		return none
	}
	selector := t.a.child_node(&node, 0)
	if selector.kind != .selector || selector.children_count == 0 {
		return none
	}
	// The generic method the check resolved it to.
	method_key := t.tc.resolved_call_name(id) or { return none }
	if (t.tc.fn_generic_params[method_key] or { []string{} }).len == 0 {
		return none
	}
	type_args := t.generic_call_type_args(node)
	if type_args.len == 0 || t.type_args_have_enclosing_generic_param(id, type_args) {
		return none
	}
	receiver_type_source_id := t.generic_call_arg_type_for_inference(t.a.child(selector, 0))
	if receiver_type_source_id == 0 || t.generic_arg_is_unresolved(receiver_type_source_id)
		|| t.type_arg_id_has_enclosing_generic_param(id, receiver_type_source_id) {
		return none
	}
	mut decl_key := ''
	for candidate in generic_call_name_spellings(method_key) {
		key := generic_fn_decl_base_value(candidate)
		if key in decls {
			decl_key = key
			break
		}
	}
	if decl_key == '' {
		decl_key = t.generic_resolved_call_decl_key(method_key, *selector, node, module_name,
			decls) or { return none }
	}
	decl := decls[decl_key] or { return none }
	param_names := t.generic_fn_param_names(decl.node, decl.module)
	receiver_params := t.generic_receiver_param_names(decl)
	declaring_receiver := t.generic_method_value_declaring_receiver(receiver_type_source_id, decl,
		module_name) or { GenericMethodValueReceiver{ typ: receiver_type_source_id } }
	mut fixed := t.receiver_fixed_type_args(decl, declaring_receiver.typ)
	if decl.node.children_count > 0 {
		receiver_param := t.a.child_node(&decl.node, 0)
		t.infer_generic_receiver_suffix_args(receiver_param.typ, declaring_receiver.typ, mut fixed)
	}
	explicit := normalize_generic_args(type_args, module_name)
	own := param_names.filter(it !in receiver_params)
	mut args := []u32{cap: param_names.len}
	if explicit.len == param_names.len {
		for i, name in param_names {
			args << fixed[name] or { explicit[i] }
		}
	} else if explicit.len == own.len {
		mut next := 0
		for name in param_names {
			if name in receiver_params {
				args << fixed[name] or { return none }
			} else {
				args << explicit[next]
				next++
			}
		}
	} else {
		return none
	}
	scoped_source_ids := t.generic_call_args_for_decl(args, module_name, decl.module)

	if t.generic_args_have_placeholders(scoped_source_ids) {
		return none
	}
	return decl, scoped_source_ids.clone()
}

struct GenericMethodValueReceiver {
	typ  u32
	path []FieldInfo
}

// generic_method_value_declaring_receiver follows concrete embedded fields
// so a generic wrapper retains the declaring receiver's substituted arguments.
fn (t &Transformer) generic_method_value_declaring_receiver(receiver_type_id u32, decl GenericFnDecl, module_name string) ?GenericMethodValueReceiver {
	mut pending := [receiver_type_id]
	mut paths := [][]FieldInfo{}
	paths << []FieldInfo{}
	mut seen := map[u32]bool{}
	for pending.len > 0 {
		current := t.trim_all_pointer_type(t.trimmed_type_id(pending.pop()))
		path := paths.pop()
		if current in seen {
			continue
		}
		seen[current] = true
		if t.generic_receiver_decl_matches_type(current, decl, module_name) {
			return GenericMethodValueReceiver{ typ: current, path: path }
		}
		info := t.lookup_struct_info(current) or { continue }
		for field in info.fields.reverse() {
			if t.is_embedded_field(field) {
				field_type := (if field.raw_typ != 0 {
					field.raw_typ
				} else {
					field.typ
				})
				syntax := flat.source_type_syntax(field_type)
				pending << if syntax.generic_app_valid() {
					flat.source_type_composition_two(.concat, t.normalize_type_in_module(syntax.generic_app_base(), info.module), syntax.generic_app_legacy_suffix())
				} else {
					t.normalize_type_in_module(field_type, info.module)
				}
				mut field_path := path.clone()
				field_path << field
				paths << field_path
			}
		}
	}
	return none
}

// generic_method_decl_of returns the generic method `method` of the type
// `receiver_type`, or of the type it aliases.
fn (mut t Transformer) generic_method_decl_of(receiver_type_id u32, method string, decls map[string]GenericFnDecl) ?GenericFnDecl {
	clean_id := t.trim_pointer_type(receiver_type_id)
	clean_syntax := flat.source_type_syntax(clean_id)
	unaliased_id := types.unalias_type(t.tc.parse_type(clean_id)).source_type_id()
	unaliased_syntax := flat.source_type_syntax(unaliased_id)
	names := [
		if clean_syntax.generic_app_valid() { clean_syntax.generic_app_base() } else { clean_id },
		if unaliased_syntax.generic_app_valid() {
			unaliased_syntax.generic_app_base()
		} else {
			unaliased_id
		},
	]
	for name_id in names {
		for candidate_id in [name_id, flat.source_type_syntax(name_id).unqualified()] {
			key := '${flat.source_type_name(candidate_id)}.${method}'
			if decl := decls[key] { return decl }
		}
	}
	return none
}

// bind_explicit_generic_method_value makes the index `id`, a generic method
// named on a value with its type arguments (see
// explicit_generic_method_value_specialization), the method value of the
// instance of those types, bound to the value as any method value is: a
// selector of the method, whose call the check records as that instance. It
// returns none for another index, and otherwise whether it asked for the
// instance.
fn (mut t Transformer) bind_explicit_generic_method_value(id flat.NodeId, node flat.Node, module_name string, decls map[string]GenericFnDecl) ?bool {
	decl, args_source_ids_5 := t.explicit_generic_method_value_specialization(id, node, module_name, decls) or {
		return none
	}

	selector := *t.a.child_node(&node, 0)
	mut receiver := t.a.child(&selector, 0)
	base_type_source_id := t.generic_call_arg_type_for_inference(receiver)
	base_type_shape := flat.source_type_shape(base_type_source_id)
	if declaring_receiver := t.generic_method_value_declaring_receiver(base_type_source_id, decl, module_name) {
		mut is_pointer := base_type_shape.has(.pointer)
		for field in declaring_receiver.path {
			field_type := (if field.typ != 0 {
				field.typ
			} else {
				field.raw_typ
			})
			receiver = t.make_selector_op(receiver, field.name, field_type, if is_pointer {
				.arrow
			} else {
				.dot
			})
			is_pointer = flat.source_type_shape(field_type).has(.pointer)
		}
	}
	concrete_args_source_ids := t.canonical_generic_specialization_args(args_source_ids_5)

	mut asked := false
	if !t.generic_specialization_registered(decl, concrete_args_source_ids) {
		t.request_generic_fn_specialization(decl, concrete_args_source_ids)
		asked = true
	}
	spec_value := specialized_generic_fn_value(decl.node.value_text(), concrete_args_source_ids)
	spec_name := transform_qualified_fn_name(decl.module, spec_value)
	t.record_generic_specialization_args_for_names([spec_name, c_name(spec_name)], concrete_args_source_ids)
	// The type of the value: the instance without its receiver, which it binds.
	params := t.tc.fn_param_types[spec_name] or {
		t.tc.fn_param_types[spec_value] or { []types.Type{} }
	}
	method_type := if params.len > 0 {
		ret := t.tc.fn_ret_types[spec_name] or {
			t.tc.fn_ret_types[spec_value] or { types.Type(types.void_) }
		}
		types.Type(t.tc.variadic_fn_type(params[1..], &ret, []bool{}, t.tc.fn_variadic[spec_name] or { t.tc.fn_variadic[spec_value] or { false } }))
	} else {
		t.tc.expr_type(id) or { types.Type(types.void_) }
	}
	start := t.a.children.len
	t.a.children << receiver
	t.set_node(int(id), flat.Node{
		kind:           .selector
		op:             selector.op
		children_start: start
		children_count: 1
		pos:            node.pos
		source_value:   selector.source_value
		value:          selector.value
		typ:            t.semantic_type_name(method_type)
	})
	t.tc.remember_method_value_call(id, spec_name)
	return asked
}

fn (mut t Transformer) explicit_generic_arg_is_known_type(arg string, module_name string) bool {
	return t.explicit_generic_arg_is_known_type_id(flat.source_type_id(arg), module_name)
}

fn (mut t Transformer) explicit_generic_arg_is_known_type_id(arg_id u32, module_name string) bool {
	clean_id := transform_trimmed_type_id(arg_id)
	if clean_id == 0 { return false }
	shape := flat.source_type_shape(clean_id)
	syntax := flat.source_type_syntax(clean_id)
	if shape.has(.pointer) || shape.has(.optional) || shape.has(.result_type) || shape.has(.array_type) {
		return t.explicit_generic_arg_is_known_type_id(syntax.raw_base(), module_name)
	}
	if shape.has(.map_type) {
		return syntax.bracket_closed() && syntax.raw_base() != 0
			&& t.explicit_generic_arg_is_known_type_id(syntax.bracket_raw_inner(), module_name)
			&& t.explicit_generic_arg_is_known_type_id(syntax.raw_base(), module_name)
	}
	if shape.has(.leading_bracket) {
		return syntax.bracket_closed() && syntax.raw_base() != 0
			&& t.explicit_generic_arg_is_known_type_id(syntax.raw_base(), module_name)
	}
	if syntax.function_space_prefix() || shape.has(.function_compact) { return true }
	if syntax.tuple_like() {
		if syntax.tuple_components().len == 0 { return false }
		for part_id in syntax.tuple_components() {
			if !t.explicit_generic_arg_is_known_type_id(part_id, module_name) { return false }
		}
		return true
	}
	if syntax.generic_app_valid() {
		if !t.explicit_generic_arg_is_known_type_id(syntax.generic_app_base(), module_name) {
			return false
		}
		for nested_arg_id in syntax.generic_app_args() {
			if !t.explicit_generic_arg_is_known_type_id(nested_arg_id, module_name) { return false }
		}
		return true
	}
	return syntax.c_qualified() || (shape.has(.dotted) && syntax.first_dot_prefix() == flat.source_type_id('JS'))
		|| t.concrete_type_name_known(flat.source_type_name(clean_id), module_name)
}

fn (t &Transformer) explicit_generic_fn_value_decl_candidates(id flat.NodeId, base_id flat.NodeId, base flat.Node, module_name string) []string {
	if base.kind == .ident {
		mut candidates := []string{}
		local := transform_qualified_fn_name(module_name, base.value_text())
		if local != base.value_text() {
			candidates << local
		}
		if !isnil(t.tc) {
			if resolved := t.tc.resolved_fn_value_name(id) {
				candidates << resolved
			}
			if resolved := t.tc.resolved_fn_value_name(base_id) {
				if resolved !in candidates {
					candidates << resolved
				}
			}
			file_name := t.node_file_or(int(id), t.node_file_or(int(base_id), t.cur_file))
			for imported in t.tc.file_selective_imports[file_import_key(file_name, base.value_text())] or {
				[]string{}
			} {
				if imported !in candidates {
					candidates << imported
				}
			}
			if candidates.len == 0 {
				cached_fn_type := if typ := t.tc.expr_type(id) {
					typ is types.FnType
				} else {
					false
				}
				if !cached_fn_type && t.tc.resolve_type(base_id) !is types.FnType {
					return candidates
				}
			}
		}
		for candidate in t.generic_plain_call_candidates(base.value_text(), module_name) {
			if candidate !in candidates {
				candidates << candidate
			}
		}
		return candidates
	}
	if base.kind != .selector || base.children_count == 0 || isnil(t.tc) {
		return []string{}
	}
	qualifier := t.a.child_node(&base, 0)
	if qualifier.kind != .ident {
		return []string{}
	}
	file_name := t.node_file_or(int(id), t.node_file_or(int(base_id), t.cur_file))
	imported_module := if mod := t.file_import_module(file_name, qualifier.value_text()) {
		mod
	} else if mod := t.tc.imports[qualifier.value_text()] {
		mod
	} else {
		return []string{}
	}
	return ['${imported_module}.${base.value_text()}']
}

fn (mut t Transformer) request_generic_fn_specialization(decl GenericFnDecl, args_ids []u32) {
	// Callers pass the canonical arguments used to form `clone_value`. Resolving
	// them again in the declaration module could rebind a caller-owned type.
	concrete_args := args_ids.clone()
	key := t.generic_specialization_progress_key(decl, concrete_args)
	t.record_monomorph_cache_spec(key, decl.key, decl.module, concrete_args)
	t.note_library_instance_request(decl, key)
	if t.generic_fn_spec_nodes.contains(key) || t.generic_fn_specs_in_progress.contains(key)
		|| t.pending_generic_fn_spec_keys.contains(key) {
		return
	}
	t.pending_generic_fn_spec_keys.set(key, true)
	value := specialized_generic_fn_value(decl.node.value, concrete_args)
	if !t.generic_signatures_pre_registered {
		t.register_specialized_fn_signature_value(decl, value, concrete_args)
	}
	t.pending_generic_fn_specs << PendingGenericFnSpec{
		decl: decl
		args: flat.TypeTextList{ ids: concrete_args }
		key:  key
	}
}

fn (mut t Transformer) record_monomorph_cache_spec(key GenericFnSpecKey, decl_key string, module_name string, args_ids []u32) {
	if key.decl_key == '' || decl_key == '' { return }
	// Declaration keys and numeric arrays must outlive a scoped worker arena.
	// Re-record so the last parent-arena copy wins after a worker merge.
	t.monomorph_cache_specs.set(key, MonomorphCacheSpec{
		decl_key: decl_key.clone()
		module:   module_name.clone()
		args:     flat.TypeTextList{ ids: args_ids.clone() }
	})
}

fn (mut t Transformer) seed_cached_monomorph_specs(specs []MonomorphCacheSpec) {
	if specs.len == 0 {
		return
	}
	decls := t.cached_generic_fn_decls()
	for spec in specs {
		decl := decls[spec.decl_key] or { continue }
		concrete_args_source_ids := t.canonical_generic_specialization_args(spec.args.ids)

		if concrete_args_source_ids.len == 0 || t.generic_args_have_placeholders(concrete_args_source_ids) {
			continue
		}
		key := t.generic_specialization_progress_key(decl, concrete_args_source_ids)
		t.record_monomorph_cache_spec(key, spec.decl_key, decl.module, concrete_args_source_ids)
		if !t.generic_specialization_registered(decl, concrete_args_source_ids) {
			value := specialized_generic_fn_value(decl.node.value, concrete_args_source_ids)
			t.register_specialized_fn_signature_value(decl, value, concrete_args_source_ids)
		}
	}
}

fn (mut t Transformer) materialize_monomorph_signature_types() {
	if isnil(t.tc) || t.monomorph_signature_types.len == 0 {
		return
	}
	struct_decls := t.collect_generic_struct_decls()
	sum_decls := t.collect_generic_sum_decls()
	if struct_decls.len == 0 && sum_decls.len == 0 {
		t.monomorph_signature_types.clear()
		return
	}
	mut struct_specs := map[u32]u32{}
	mut sum_specs := map[u32]GenericSpecContext{}
	for signature in t.monomorph_signature_types {
		t.collect_generic_struct_spec_from_type(signature.typ, signature.module, signature.file, struct_decls, mut struct_specs)
		t.collect_generic_sum_spec_from_type(signature.typ, signature.module, signature.file, sum_decls, mut sum_specs)
	}
	t.monomorph_signature_types.clear()
	t.expand_generic_materialization_specs(struct_decls, sum_decls, mut struct_specs, mut sum_specs)
	for spec, context in sum_specs {
		decl := sum_decls[context.base] or { continue }
		t.materialize_generic_sum_spec(spec, decl, context)
	}
	t.materialize_generic_struct_specs(struct_specs, struct_decls)
}

fn (t &Transformer) sorted_monomorph_cache_specs() []MonomorphCacheSpec {
	mut keys := []GenericFnSpecSortKey{cap: t.monomorph_cache_specs.len}
	for key in t.monomorph_cache_specs.keys() {
		keys << GenericFnSpecSortKey{ key: key, text: key.legacy_text(), write_order: t.monomorph_cache_specs.write_order(key) }
	}
	keys.sort_with_compare(compare_generic_fn_spec_sort_keys)
	mut specs := []MonomorphCacheSpec{cap: keys.len}
	for i, sorted_key in keys {
		// Equal rendered aliases historically shared a string key: the last write wins.
		if i + 1 < keys.len && sorted_key.text == keys[i + 1].text { continue }
		spec := t.monomorph_cache_specs.get(sorted_key.key) or { continue }
		// Source spelling bytes are process-wide; own only the worker-backed ID array.
		args := spec.args.ids.clone()
		specs << MonomorphCacheSpec{
			decl_key: spec.decl_key.clone()
			module:   spec.module.clone()
			args:     flat.TypeTextList{ ids: args.clone() }
		}
	}
	return specs
}

fn (mut t Transformer) drain_pending_generic_fn_specs(struct_decls map[u32]GenericStructDecl, sum_decls map[u32]GenericSumDecl, mut emitted GenericFnSpecBoolMap, mut generated []string) bool {
	mut changed := false
	for t.pending_generic_fn_specs.len > 0 {
		specs := t.pending_generic_fn_specs
		t.pending_generic_fn_specs = []PendingGenericFnSpec{}
		mut pending := []PendingGenericFnSpec{cap: specs.len}
		for spec in specs {
			if t.generic_fn_spec_nodes.contains(spec.key) {
				t.pending_generic_fn_spec_keys.delete(spec.key)
				continue
			}
			pending << spec
		}
		if !t.parallel_monomorphize
			&& t.run_serial_scoped_monomorphize_specs(pending, mut emitted, mut generated) {
			changed = true
			continue
		}
		if t.parallel_monomorphize
			&& t.run_parallel_monomorphize_specs(pending, struct_decls, sum_decls, mut emitted, mut generated) {
			changed = true
			continue
		}
		for spec in pending {
			clone_id := t.emit_generic_fn_specialization(spec.decl, spec.args.ids)
			generated << t.generated_fn_used_names(spec.decl, clone_id, spec.args.ids)
			emitted.set(generic_fn_spec_key(spec.decl.key, spec.args.ids), true)
			t.pending_generic_fn_spec_keys.delete(spec.key)
			changed = true
		}
	}
	return changed
}

fn (mut t Transformer) collect_generic_call_sites_after_type_refresh(generic_struct_specs map[u32]u32, decls map[string]GenericFnDecl, ignored_nodes []bool, candidate_ids []int, mut emitted GenericFnSpecBoolMap, mut recorded_call_sites map[int]bool) []GenericCallSite {
	mut sites := []GenericCallSite{}
	t.ensure_node_module_map()
	old_in_scan := t.in_monomorphize_scan
	t.in_monomorphize_scan = true
	defer {
		t.in_monomorphize_scan = old_in_scan
	}
	lowered_operator_uses := t.lowered_generic_struct_operator_uses_for_specs(generic_struct_specs, decls)
	for i in candidate_ids {
		if i < 0 || i >= t.a.nodes.len {
			continue
		}
		if (i < ignored_nodes.len && ignored_nodes[i])
			|| (i < t.ignored_comptime_for_nodes.len && t.ignored_comptime_for_nodes[i]) {
			continue
		}
		node := t.a.nodes[i]
		if node.kind != .call {
			continue
		}
		if t.node_file_or(i, '').len == 0 && i !in t.generic_call_spec_cache && !t.synthetic_generic_call_has_exact_callee(node) {
			continue
		}
		if t.call_is_recorded_struct_operator(node)
			|| t.record_lowered_generic_struct_operator_call(node, lowered_operator_uses) {
			continue
		}
		call_module := t.node_module_or(i, '')
		decl_key, args_source_ids := t.cached_generic_call_specialization(flat.NodeId(i), node, call_module, decls) or {
			t.missing_specialized_plain_generic_call(node, call_module, decls) or { continue }
		}

		decl := decls[decl_key] or { continue }
		if !t.call_has_source_generic_args(node) && t.generic_args_contain_alias(args_source_ids, decl.module) {
			t.set_node_call_type_args(i, args_source_ids)
		}
		if i !in recorded_call_sites {
			sites << GenericCallSite{
				id:     flat.NodeId(i)
				module: call_module
			}
			recorded_call_sites[i] = true
		}
		concrete_args_source_ids := t.canonical_generic_specialization_args(args_source_ids)

		spec_key := generic_fn_spec_key(decl_key, concrete_args_source_ids)
		if emitted.contains(spec_key) {
			continue
		}
		if t.generic_specialization_registered(decl, concrete_args_source_ids) {
			emitted.set(spec_key, true)
			continue
		}
		t.library_bodies.request_file = t.node_file_or(i, '')
		t.request_generic_fn_specialization(decl, concrete_args_source_ids)
		t.library_bodies.request_file = ''
		emitted.set(spec_key, true)
	}
	return sites
}

fn (mut t Transformer) seed_generic_specialization_args(decls map[u32]GenericStructDecl) {
	if decls.len == 0 {
		return
	}
	specs := if t.generic_materialization_ready {
		t.generic_struct_specs_cache
	} else {
		all_specs, _ := t.collect_generic_materialization_specs(decls, map[u32]GenericSumDecl{})
		all_specs
	}
	for spec_id, base_id in specs {
		base := flat.source_type_name(base_id)
		decl := decls[base_id] or { continue }
		syntax := flat.source_type_syntax(spec_id)
		args := syntax.generic_app_args()
		ok := syntax.generic_app_valid()
		if ok && args.len > 0 {
			t.record_generic_specialization_args_in_module(base, decl.module, args)
		}
	}
}

// erase_generic_templates removes generic templates that no longer need to remain in the transformed AST.
pub fn erase_generic_templates(mut a flat.FlatAst, tc &types.TypeChecker, used_fns map[string]bool) map[string]bool {
	mut sw := time.new_stopwatch()
	mut t := new_transformer(mut a, tc, used_fns)
	t.timing_profile('  [ttime]   erase new_transformer ${f64(sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
	sw.restart()
	decls := t.collect_generic_fn_decls_for_erasure()
	t.timing_profile('  [ttime]   erase collect         ${f64(sw.elapsed().microseconds()) / 1000.0:7.2f} ms (decls: ${decls.len})')
	sw.restart()
	if decls.len != 0 {
		keep := t.building_v_type_erased_generic_keep_set(decls)
		mut erased := map[string]GenericFnDecl{}
		for key, decl in decls {
			if keep[key] {
				continue
			}
			erased[key] = decl
		}
		t.erase_consts_initialized_by_erased_templates(decls)
		t.erase_generic_fn_decls(erased)
		t.timing_profile('  [ttime]   erase apply           ${f64(sw.elapsed().microseconds()) / 1000.0:7.2f} ms (erased: ${decls.len - keep.len})')
	}
	return t.used_fns
}

// erase_consts_initialized_by_erased_templates empties const initializers that
// call a generic template. A building-v compile erases those templates instead
// of monomorphizing them, so a surviving initializer call would reference a
// function that no longer exists in the generated C. The V compiler itself
// never reads such consts; an accidental new use fails loudly as an unknown
// identifier in C rather than silently reading zeroed storage.
fn (mut t Transformer) erase_consts_initialized_by_erased_templates(decls map[string]GenericFnDecl) {
	if decls.len == 0 {
		return
	}
	mut cur_module := ''
	for i in 0 .. t.a.nodes.len {
		if t.a.nodes[i].kind !in [.file, .module_decl, .const_decl] {
			continue
		}
		// Only candidate nodes need copies before AST rewriting.
		node := t.a.nodes[i]
		match node.kind {
			.file {
				cur_module = t.tc.file_modules[node.value_text()] or { '' }
			}
			.module_decl {
				cur_module = node.value_text()
			}
			.const_decl {
				for ci in 0 .. node.children_count {
					field := t.a.child_node(&node, ci)
					if field.kind != .const_field || field.children_count == 0 {
						continue
					}
					value_id := t.a.child(field, 0)
					mut called := map[string]bool{}
					t.collect_type_erased_generic_template_calls(value_id, cur_module, decls, mut called)
					if called.len == 0 {
						continue
					}
					mut subtree := map[int]bool{}
					t.collect_node_subtree_ids(value_id, mut subtree)
					for idx, _ in subtree {
						old := t.a.nodes[idx]
						t.set_node(idx, flat.Node{
							kind: .empty
							pos:  old.pos
						})
						t.clear_typechecker_node_cache(idx)
					}
				}
			}
			else {}
		}
	}
}

@[direct_array_access]
fn (mut t Transformer) collect_generic_fn_decls_for_erasure() map[string]GenericFnDecl {
	mut decls := map[string]GenericFnDecl{}
	mut cur_file := ''
	mut cur_module := ''
	// The walk is cache-miss bound (one 72-byte node header per check) and the
	// per-decl candidate analysis is state-free, so fan both out over the
	// worker pool: workers flag file/module markers and prescreen-positive fn
	// decls, and only those nodes are visited here, in ascending order like the
	// serial walk. Prescreen-negative fn decls can never need erasure.
	mut scan_sw := time.new_stopwatch()
	mut flags := []u8{len: t.a.nodes.len}
	if scan_top_level_kind_flags_parallel(t.a, 0, mut flags, t.prefix_param_scan) {
		t.timing_profile('  [ttime]     collect scan        ${f64(scan_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
		scan_sw.restart()
		// Word-scan the flag array: flagged nodes are rare, so loading eight
		// flags per u64 makes the master walk almost free.
		words := flags.len / 8
		word_data := unsafe { &u64(flags.data) }
		for wi in 0 .. words {
			if unsafe { word_data[wi] } == 0 {
				continue
			}
			base_i := wi * 8
			for bi in 0 .. 8 {
				if flags[base_i + bi] != 0 {
					cur_file, cur_module = t.collect_generic_fn_decl_for_erasure_at(base_i + bi, mut decls, cur_file, cur_module)
				}
			}
		}
		for i in words * 8 .. flags.len {
			if flags[i] != 0 {
				cur_file, cur_module = t.collect_generic_fn_decl_for_erasure_at(i, mut decls, cur_file, cur_module)
			}
		}
		t.timing_profile('  [ttime]     collect flag walk   ${f64(scan_sw.elapsed().microseconds()) / 1000.0:7.2f} ms')
		return decls
	}
	for i in 0 .. t.a.nodes.len {
		kind := unsafe { t.a.nodes[i].kind }
		if kind != .file && kind != .module_decl && kind != .fn_decl {
			continue
		}
		cur_file, cur_module = t.collect_generic_fn_decl_for_erasure_at(i, mut decls, cur_file, cur_module)
	}
	return decls
}

// collect_generic_fn_decl_for_erasure_at visits one node of the erasure scan
// and returns the (file, module) context the scan holds after it.
fn (mut t Transformer) collect_generic_fn_decl_for_erasure_at(i int, mut decls map[string]GenericFnDecl, cur_file string, cur_module string) (string, string) {
	node := unsafe { &t.a.nodes[i] }
	match node.kind {
		.file {
			new_file := node.value_text()
			return new_file, t.tc.file_modules[new_file] or { '' }
		}
		.module_decl {
			return cur_file, node.value_text()
		}
		.fn_decl {
			if !t.generic_fn_decl_needs_erasure_scan(*node, cur_module) {
				return cur_file, cur_module
			}
			key := t.generic_fn_decl_key(*node, cur_module)
			decls[key] = GenericFnDecl{
				id:     flat.NodeId(i)
				node:   *node
				file:   cur_file
				module: cur_module
				key:    key
			}
			return cur_file, cur_module
		}
		else {
			return cur_file, cur_module
		}
	}
}

fn (mut t Transformer) building_v_type_erased_generic_keep_set(decls map[string]GenericFnDecl) map[string]bool {
	mut keep := map[string]bool{}
	mut queue := []string{}
	for key, _ in decls {
		if building_v_type_erased_generic_keep_root(key) {
			keep[key] = true
			queue << key
		}
	}
	if queue.len == 0 {
		return keep
	}
	t.generic_fn_decls_cache = decls.clone()
	t.build_generic_receiver_method_index()
	mut cursor := 0
	for cursor < queue.len {
		key := queue[cursor]
		cursor++
		decl := decls[key] or { continue }
		mut called := map[string]bool{}
		t.collect_type_erased_generic_template_calls(decl.id, decl.module, decls, mut called)
		for called_key, _ in called {
			if called_key !in keep {
				keep[called_key] = true
				queue << called_key
			}
		}
	}
	return keep
}

fn (mut t Transformer) collect_type_erased_generic_template_calls(id flat.NodeId, module_name string, decls map[string]GenericFnDecl, mut called map[string]bool) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(id)]
	if node.kind == .call {
		if key := t.type_erased_generic_template_call_decl_key(id, node, module_name, decls) {
			called[key] = true
		}
	}
	for i in 0 .. node.children_count {
		t.collect_type_erased_generic_template_calls(t.a.child(&node, i), module_name, decls, mut called)
	}
}

fn (mut t Transformer) type_erased_generic_template_call_decl_key(id flat.NodeId, node flat.Node, module_name string, decls map[string]GenericFnDecl) ?string {
	if key := t.generic_call_decl_key(id, node, module_name, decls) {
		return key
	}
	if node.children_count == 0 {
		return none
	}
	mut callee := t.a.child_node(&node, 0)
	if callee.kind == .index && callee.children_count > 0 && callee.value != 'range' {
		callee = t.a.child_node(callee, 0)
	}
	if callee.kind == .ident {
		for candidate in t.generic_plain_call_candidates(callee.value_text(), module_name) {
			if key := t.type_erased_generic_template_candidate_key(candidate, node, decls) {
				return key
			}
		}
	} else if callee.kind == .selector && callee.children_count > 0 {
		base := t.a.child_node(callee, 0)
		if base.kind == .ident {
			for candidate in ['${base.value_text()}.${callee.value_text()}',
				'${t.import_alias_module(base.value_text())}.${callee.value_text()}'] {
				if key := t.type_erased_generic_template_candidate_key(candidate, node, decls) {
					return key
				}
			}
		}
	}
	return none
}

fn (t &Transformer) type_erased_generic_template_candidate_key(candidate string, node flat.Node, decls map[string]GenericFnDecl) ?string {
	key := generic_fn_decl_base_value(candidate)
	decl := decls[key] or { return none }
	if !t.generic_call_arg_count_matches_decl(node, decl) {
		return none
	}
	return key
}

fn (mut t Transformer) generic_fn_decl_needs_erasure_scan(node flat.Node, module_name string) bool {
	if generic_params_have_runtime_type_param(node.generic_params())
		|| t.type_text_has_generic_placeholder(node.typ, module_name)
		|| t.node_payload_has_generic_placeholder(node, module_name) {
		return true
	}
	for i in 0 .. node.children_count {
		child := t.a.child_node(&node, i)
		if child.kind != .param {
			if t.prefix_param_scan {
				break
			}
			continue
		}
		if t.type_text_has_generic_placeholder(child.typ, module_name) {
			return true
		}
	}
	return false
}

fn building_v_type_erased_generic_keep_root(key string) bool {
	return key in ['token.new_keywords_matcher_trie', 'sync.pool.PoolProcessor.work_on_items']
}

// is_operator_method_name reports whether a method-name part is an overloaded
// operator symbol (`+`, `-`, `*`, `/`, `%`, `**`, `==`, `!=`, `<`, `>`, `<=`, `>=`,
// `[]`, `[]=`).
fn is_operator_method_name(name string) bool {
	return name in ['+', '-', '*', '/', '%', '**', '==', '!=', '<', '>', '<=', '>=', '[]', '[]=']
}

// record_called_fn_name records the callee name of a direct call so operator overloads
// (lowered to direct calls before this pass) can be specialized only when actually
// called. Operator method names are also recorded mangled
// (`__v3_internal_symbol_op_Vec_int__plus`), matching the name
// `specialize_generic_struct_methods` would emit for that instance and operator.
fn (t &Transformer) call_is_recorded_struct_operator(node flat.Node) bool {
	if node.children_count < 1 {
		return false
	}
	fn_node := t.a.child_node(&node, 0)
	return fn_node.kind == .ident && fn_node.value_text().len > 0
		&& fn_node.value_text() in t.used_struct_operator_fns
}

fn (mut t Transformer) record_lowered_generic_struct_operator_call(node flat.Node, lowered_operator_uses map[string][]string) bool {
	if node.children_count < 1 {
		return false
	}
	fn_node := t.a.child_node(&node, 0)
	if fn_node.kind != .ident || fn_node.value_text().len == 0 {
		return false
	}
	operator_names := lowered_operator_uses[fn_node.value_text()] or { return false }
	mut recorded := false
	for name in operator_names {
		if name.len == 0 {
			continue
		}
		t.mark_struct_operator_used_name(name)
		recorded = true
	}
	return recorded
}

fn (mut t Transformer) lowered_generic_struct_operator_uses(struct_decls map[u32]GenericStructDecl, decls map[string]GenericFnDecl) map[string][]string {
	return t.lowered_generic_struct_operator_uses_for_specs(t.generic_struct_specs(struct_decls), decls)
}

fn (mut t Transformer) generic_struct_specs(struct_decls map[u32]GenericStructDecl) map[u32]u32 {
	mut specs := map[u32]u32{}
	t.collect_generic_struct_specs(struct_decls, mut specs)
	return specs
}

fn (t &Transformer) lowered_generic_struct_operator_uses_for_specs(specs map[u32]u32, decls map[string]GenericFnDecl) map[string][]string {
	mut uses := map[string][]string{}
	for spec_id, base_id in specs {
		spec := flat.source_type_name(spec_id)
		base := flat.source_type_name(base_id)
		syntax := flat.source_type_syntax(spec_id)
		args := syntax.generic_app_args()
		ok := syntax.generic_app_valid()
		if !ok || args.len == 0 {
			continue
		}
		for decl_key, decl in decls {
			if !decl_key.contains('.') || decl_key.all_before_last('.') != base {
				continue
			}
			method := decl_key.all_after_last('.')
			if !is_operator_method_name(method) {
				continue
			}
			operator_name := '${spec}.${method}'
			add_lowered_generic_struct_operator_use(mut uses, operator_name, operator_name)
			add_lowered_generic_struct_operator_use(mut uses, c_name(operator_name), operator_name)
			for alias in specialized_generic_fn_signature_aliases(decl, args) {
				add_lowered_generic_struct_operator_use(mut uses, alias, operator_name)
			}
		}
	}
	return uses
}

fn add_lowered_generic_struct_operator_use(mut uses map[string][]string, call_name string, operator_name string) {
	clean := trimmed_transform_text(call_name)
	if clean.len == 0 || operator_name == '' {
		return
	}
	mut operator_names := uses[clean] or { []string{} }
	if operator_name !in operator_names {
		operator_names << operator_name
		uses[clean] = operator_names
	}
}

// has_generic_index_operators tells whether the checker registered a generic
// `[]` or `[]=` method, the only signatures generic_struct_operator_fn_name finds
// for an index.
fn (mut t Transformer) has_generic_index_operators() bool {
	if t.generic_index_operators_known == 0 {
		t.generic_index_operators_known = -1
		if !isnil(t.tc) {
			for name, _ in t.tc.fn_ret_types {
				if name.ends_with('].[]') || name.ends_with('].[]=') {
					t.generic_index_operators_known = 1
					break
				}
			}
			if t.generic_index_operators_known < 0 {
				for name, _ in t.tc.fn_param_types {
					if name.ends_with('].[]') || name.ends_with('].[]=') {
						t.generic_index_operators_known = 1
						break
					}
				}
			}
		}
	}
	return t.generic_index_operators_known > 0
}

// unignored_nodes lists in order the nodes below `end` that `old` flags and
// `new` does not, testing eight flags at a time: they are bytes of 0 or 1.
@[direct_array_access]
fn unignored_nodes(old []bool, new []bool, end int) []int {
	mut result := []int{}
	limit := int_min(int_min(end, old.len), new.len)
	words := limit / 8
	old_words := unsafe { &u64(old.data) }
	new_words := unsafe { &u64(new.data) }
	for w in 0 .. words {
		if unsafe { old_words[w] & ~new_words[w] } == 0 {
			continue
		}
		for i in w * 8 .. w * 8 + 8 {
			if old[i] && !new[i] {
				result << i
			}
		}
	}
	for i in words * 8 .. limit {
		if old[i] && !new[i] {
			result << i
		}
	}
	// Past the new flags' end every old flag counts as cleared.
	for i in limit .. int_min(end, old.len) {
		if old[i] {
			result << i
		}
	}
	return result
}

fn (mut t Transformer) record_index_operator_fn_name(node flat.Node, method string) {
	if node.kind != .index || node.value == 'range' || node.children_count < 2 {
		return
	}
	// The operator is found only as a generic method signature such as
	// `Box[T].[]`, which the checker registered before this stage.
	if !t.has_generic_index_operators() {
		return
	}
	base_id := t.a.child(&node, 0)
	base_type_source_id := t.node_type(base_id)

	struct_type_source_id_2 := t.generic_struct_instance_name(base_type_source_id)
	struct_type := flat.source_type_name(struct_type_source_id_2)
	if struct_type_source_id_2 == 0 {
		return
	}
	if name := t.generic_struct_operator_fn_name(struct_type_source_id_2, method) {
		t.used_struct_operator_fns[name] = true
		t.used_struct_operator_fns[c_name('${struct_type}.${method}')] = true
	}
}

// has_generic_arithmetic_operators tells whether the checker registered a
// generic arithmetic operator method such as `Vec2[T].+`.
fn (mut t Transformer) has_generic_arithmetic_operators() bool {
	if t.generic_arithmetic_operators_known == 0 {
		t.generic_arithmetic_operators_known = -1
		if !isnil(t.tc) {
			for name, _ in t.tc.fn_ret_types {
				if is_generic_arithmetic_operator_name(name) {
					t.generic_arithmetic_operators_known = 1
					break
				}
			}
		}
	}
	return t.generic_arithmetic_operators_known > 0
}

fn is_generic_arithmetic_operator_name(name string) bool {
	return name.ends_with('].+') || name.ends_with('].-') || name.ends_with('].*')
		|| name.ends_with(']./') || name.ends_with('].%') || name.ends_with('].**')
}

// record_compound_assign_operator_fn_name records the operator method of a
// generic struct that a compound assignment on an array element or a field
// (`a[i] += Vec2[f64]{}`, `m[k].pos += v`) uses. Cgen spells that call itself,
// so no call node asks for the operator's specialization.
fn (mut t Transformer) record_compound_assign_operator_fn_name(lhs_id flat.NodeId, op flat.Op) {
	op_name := compound_assign_struct_operator_symbol(op) or { return }
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind == .index {
		if lhs.value_text() == 'range' || lhs.children_count < 2 {
			return
		}
	} else if lhs.kind != .selector {
		return
	}
	if !t.has_generic_arithmetic_operators() {
		return
	}
	struct_type_source_id := t.generic_struct_instance_name(t.trim_pointer_type(t.node_type(lhs_id)))
	if struct_type_source_id == 0 {
		return
	}
	if name := t.generic_struct_operator_fn_name(struct_type_source_id, op_name) {
		t.used_struct_operator_fns[name] = true
		t.used_struct_operator_fns[c_name('${flat.source_type_name(struct_type_source_id)}.${op_name}')] = true
	}
}

// specialize_generic_struct_methods specializes every generic method of each
// instantiated generic struct (e.g. for `Vec4[f32]`, generate `vec.Vec4[f32].+`,
// `vec.Vec4[f32].one`, ...). It only handles methods whose generic parameters are
// exactly the struct's parameters (no extra method-level `[U]`); those are left to
// the call-driven path. Returns whether any new specialization was emitted.
fn generic_struct_methods_by_receiver(decls map[string]GenericFnDecl) map[u32][]GenericFnDecl {
	mut methods_by_receiver := map[u32][]GenericFnDecl{}
	for decl_key, decl in decls {
		if !decl_key.contains('.') {
			continue
		}
		receiver := flat.source_type_id(decl_key.all_before_last('.'))
		mut methods := methods_by_receiver[receiver] or { []GenericFnDecl{} }
		methods << decl
		methods_by_receiver[receiver] = methods
	}
	return methods_by_receiver
}

fn (t &Transformer) generic_struct_interfaces_by_method() map[string][]u32 {
	mut interfaces_by_method := map[string][]u32{}
	if isnil(t.tc) {
		return interfaces_by_method
	}
	for iface_name_source_id, _ in t.tc.interface_names {
		iface_id := iface_name_source_id
		for method in t.tc.interface_abstract_method_names(iface_id) {
			mut interfaces := interfaces_by_method[method] or { []u32{} }
			interfaces << iface_id
			interfaces_by_method[method] = interfaces
		}
	}
	return interfaces_by_method
}

fn (t &Transformer) used_generic_method_names() map[string]bool {
	mut methods := map[string]bool{}
	for used_map in t.used_fn_maps() {
		for name, used in *used_map {
			if !used || name.len == 0 {
				continue
			}
			if name.contains('.') {
				methods[name.all_after_last('.')] = true
			}
			if name.contains('__') {
				methods[name.all_after_last('__')] = true
			}
		}
	}
	return methods
}

fn (mut t Transformer) specialize_generic_struct_methods(specs map[u32]u32, decls map[string]GenericFnDecl, methods_by_receiver map[u32][]GenericFnDecl, method_level_params map[string]bool, interfaces_by_method map[string][]u32, mut interface_impl_cache map[u64]bool, mut emitted GenericFnSpecBoolMap) bool {
	if !t.needs_generic_struct_method_specialization(decls) {
		return false
	}
	if specs.len == 0 {
		return false
	}
	used_method_names := t.used_generic_method_names()
	mut any := false
	for spec_id, base_id in specs {
		spec := flat.source_type_name(spec_id)
		base := flat.source_type_name(base_id)
		syntax := flat.source_type_syntax(spec_id)
		args := syntax.generic_app_args()
		ok := syntax.generic_app_valid()
		if !ok || args.len == 0 {
			continue
		}
		methods := methods_by_receiver[base_id] or { continue }
		fixture_expand_regular_methods := t.fixture_should_expand_regular_generic_methods(args)
			&& t.generic_struct_spec_has_emitted_method(base, args, decls, emitted)
		concrete_args_source_ids := t.canonical_generic_specialization_args(args)

		for decl in methods {
			decl_key := decl.key
			if method_level_params[decl_key] {
				// Method has its own generic parameters beyond the struct's; leave it
				// to the call-driven specialization which can infer them.
				continue
			}
			spec_key := generic_fn_spec_key(decl_key, concrete_args_source_ids)
			if emitted.contains(spec_key) {
				continue
			}
			if t.generic_specialization_registered(decl, concrete_args_source_ids) {
				emitted.set(spec_key, true)
				continue
			}
			// Only operator overloads need struct-instantiation-driven specialization:
			// they are reached through infix expressions, not call nodes. Regular
			// methods are specialized on demand by the call-driven path, so emitting
			// every method here would generate unused (and possibly unresolved) bodies.
			// The exception is a method used as a *value* (`b.method`): it has no call
			// node to drive specialization, so specialize the ones the checker recorded.
			method := decl_key.all_after_last('.')
			if is_operator_method_name(method) {
				// Specialize an operator overload only for an instance whose operator is
				// actually applied somewhere. Otherwise a stored-but-never-operated
				// instance whose type argument does not support the operation would emit
				// a body that fails C compilation.
				if !t.generic_struct_operator_call_seen(c_name('${spec}.${method}')) {
					continue
				}
			} else if !fixture_expand_regular_methods && !used_method_names[method] {
				continue
			} else if !fixture_expand_regular_methods
				&& !t.generic_struct_method_needed_for_interface(spec_id, method, interfaces_by_method, mut interface_impl_cache) {
				mvkey := '${spec}.${method}'
				if !t.generic_struct_method_used_for_spec(spec, decl, args, method) {
					if isnil(t.tc) || method !in t.tc.generic_method_value_info[spec_id] {
						continue
					}
					// Only specialize a method value the checker recorded inside a *reachable*
					// function: markused seeds the concrete instance key (`Box[int].report`) into
					// `used_fns` per reachable function, so one used only in dead code is skipped —
					// its body may be invalid for that type argument and would fail C compilation.
					if t.has_any_used_fns() && !t.used_fn_contains_name(mvkey)
						&& !t.used_fn_contains_name(c_name(mvkey)) {
						continue
					}
				}
			}
			t.request_generic_fn_specialization(decl, concrete_args_source_ids)
			emitted.set(spec_key, true)
			any = true
		}
	}
	return any
}

fn (t &Transformer) fixture_should_expand_regular_generic_methods(args_ids []u32) bool {
	if isnil(t.tc) || !t.tc.checker_fixture_mode {
		return false
	}
	for raw_arg_source_id in args_ids {
		arg_id := flat.source_type_syntax(flat.source_type_syntax(raw_arg_source_id).source_wrapper_base()).array_chars_base()
		for candidate_id in [arg_id, flat.source_type_syntax(arg_id).unqualified()] {
			if file := t.tc.struct_files[candidate_id] {
				if file in t.tc.diagnostic_files {
					return true
				}
			}
		}
	}
	return false
}

fn (t &Transformer) generic_struct_operator_call_seen(name string) bool {
	if name == '' {
		return false
	}
	return name in t.used_struct_operator_fns || t.used_fns[name] || t.used_fns[c_name(name)]
}

fn (t &Transformer) generic_struct_spec_has_emitted_method(base string, args_ids []u32, decls map[string]GenericFnDecl, emitted GenericFnSpecBoolMap) bool {
	concrete_args_source_ids := t.canonical_generic_specialization_args(args_ids)

	for decl_key, _ in decls {
		if !decl_key.contains('.') || decl_key.all_before_last('.') != base {
			continue
		}
		if emitted.contains(generic_fn_spec_key(decl_key, concrete_args_source_ids)) {
			return true
		}
	}
	return false
}

fn (t &Transformer) generic_struct_method_used_for_spec(spec string, decl GenericFnDecl, args_ids []u32, method string) bool {
	if method == 'str' && t.generic_str_template_is_used(decl) {
		return true
	}
	direct := '${spec}.${method}'
	if t.used_fn_contains_name(direct) {
		return true
	}
	// An operator's C name is in the internal namespace (`Vec_int.+` is
	// `__v3_internal_symbol_op_Vec_int__plus`), so no alias of an ordinary method
	// (`Vec_int.plus`, `Vec_int__plus`) can name an operator call.
	if t.used_fn_contains_name(c_name(direct)) {
		return true
	}
	mut aliases := specialized_generic_fn_signature_aliases(decl, args_ids)
	receiver := generic_fn_decl_base_value(decl.node.value).all_before_last('.')
	if receiver.len > 0 {
		short_flat := '${receiver}_${generic_type_suffixes(args_ids)}'
		qualified_short_flat := transform_qualified_fn_name(decl.module, short_flat)
		full_flat := '${receiver}_${generic_type_full_suffixes(args_ids)}'
		qualified_full_flat := transform_qualified_fn_name(decl.module, full_flat)
		for flat_receiver in [short_flat, qualified_short_flat, full_flat, qualified_full_flat] {
			aliases << '${flat_receiver}.${method}'
			aliases << c_name('${flat_receiver}.${method}')
		}
	}
	for alias in aliases {
		if t.used_fn_contains_name(alias) {
			return true
		}
	}
	return false
}

fn (t &Transformer) needs_generic_struct_method_specialization(decls map[string]GenericFnDecl) bool {
	if !isnil(t.tc) && t.tc.generic_method_value_info.len > 0 {
		return true
	}
	if !isnil(t.tc) && t.tc.interface_names.len > 0 {
		return true
	}
	for decl_key, _ in decls {
		if decl_key.contains('.') && is_operator_method_name(decl_key.all_after_last('.')) {
			return true
		}
	}
	for _, decl in decls {
		if decl.node.value.ends_with('.str') && t.generic_str_template_is_used(decl) {
			return true
		}
	}
	return false
}

fn (t &Transformer) generic_str_template_is_used(decl GenericFnDecl) bool {
	qualified := transform_qualified_fn_name(decl.module, decl.node.value)
	for name in [decl.node.value, decl.key, qualified, c_name(qualified)] {
		if t.used_fn_contains_name(name) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) generic_struct_method_needed_for_interface(spec_id u32, method string, interfaces_by_method map[string][]u32, mut interface_impl_cache map[u64]bool) bool {
	if isnil(t.tc) || spec_id == 0 || method == '' {
		return false
	}
	for iface_id in interfaces_by_method[method] or { return false } {
		dispatch_used := t.interface_dispatch_method_used(iface_id, method)
		boxed_used := t.interface_boxed_type_used(iface_id, spec_id)
		if t.has_used_fn_filter() && (!dispatch_used || !boxed_used) {
			continue
		}
		mut does_implement := false
		for semantic_iface_source_id in t.interface_boxed_semantic_candidates(iface_id, spec_id) {
			cache_key := (u64(semantic_iface_source_id) << 32) | u64(spec_id)
			value := if cached := interface_impl_cache[cache_key] {
				cached
			} else {
				matches := t.tc.named_type_implements_interface(spec_id, semantic_iface_source_id)
				interface_impl_cache[cache_key] = matches
				matches
			}
			if value {
				does_implement = true
				break
			}
		}
		if !does_implement {
			continue
		}
		return true
	}
	return false
}

fn (t &Transformer) interface_dispatch_method_used(iface_name_id u32, method string) bool {
	iface_name := flat.source_type_name(iface_name_id)
	name := '${iface_name}.${method}'
	if t.used_interface_dispatch_key(name) {
		return true
	}
	semantic_prefix := '${iface_name}['
	c_prefix := '${c_name(iface_name)}_'
	for used_map in t.used_fn_maps() {
		for used_name, used in *used_map {
			if used && ((used_name.starts_with(semantic_prefix) && used_name.ends_with('.${method}'))
				|| (used_name.starts_with(c_prefix) && used_name.ends_with('__${method}'))) {
				return true
			}
		}
	}
	if decl_key := t.tc.interface_method_signature_key(iface_name_id, method) {
		if decl_key != name && t.used_interface_dispatch_key(decl_key) {
			return true
		}
		decl_short_name := '${decl_key.all_before_last('.').all_after_last('.')}.${method}'
		if decl_short_name != decl_key && t.interface_dispatch_short_name_allowed(iface_name_id)
			&& t.used_interface_dispatch_key(decl_short_name) {
			return true
		}
	}
	for alias_id in t.interface_alias_names(iface_name_id) {
		alias := flat.source_type_name(alias_id)
		alias_name := '${alias}.${method}'
		if t.used_interface_dispatch_key(alias_name) {
			return true
		}
		short_alias_name := '${flat.source_type_name(flat.source_type_syntax(alias_id).unqualified())}.${method}'
		if short_alias_name != alias_name && t.interface_dispatch_short_name_allowed(alias_id)
			&& t.used_interface_dispatch_key(short_alias_name) {
			return true
		}
	}
	short_name := '${flat.source_type_name(flat.source_type_syntax(iface_name_id).unqualified())}.${method}'
	return short_name != name && t.interface_dispatch_short_name_allowed(iface_name_id)
		&& t.used_interface_dispatch_key(short_name)
}

fn (t &Transformer) interface_alias_names(iface_name_id u32) []u32 {
	mut aliases := []u32{}
	for alias_source_id, target_id in t.tc.type_aliases.ids {
		if target_id == iface_name_id
			|| t.tc.qualify_type_name(target_id) == iface_name_id {
			aliases << alias_source_id
		}
	}
	return aliases
}

fn (t &Transformer) used_interface_dispatch_key(name string) bool {
	return t.used_fn_contains_name(name) || t.used_fn_contains_name(c_name(name))
}

fn (t &Transformer) interface_dispatch_short_name_allowed(iface_name_id u32) bool {
	iface_name_shape := flat.source_type_shape(iface_name_id)
	return !iface_name_shape.has(.dotted)
}

fn (mut t Transformer) interface_boxed_type_used(iface_name_id u32, concrete_type_id u32) bool {
	t.collect_interface_boxed_types()
	return t.interface_boxed_type_marked(iface_name_id, concrete_type_id)
}

fn (t &Transformer) interface_boxed_type_marked(iface_name_id u32, concrete_type_id u32) bool {
	iface_syntax := flat.source_type_syntax(iface_name_id)
	concrete_type := flat.source_type_name(concrete_type_id)
	mut iface_names := [iface_name_id]
	short_name := iface_syntax.unqualified()
	if short_name != iface_name_id {
		iface_names << short_name
	}
	resolved := t.resolve_interface_type_name(iface_name_id)
	if resolved != 0 && resolved != iface_name_id {
		iface_names << resolved
	}
	concrete_c_name := flat.source_type_id(c_name(concrete_type))
	for iface in iface_names {
		if t.interface_boxed_type_key_marked(interface_boxed_type_key(iface, concrete_type_id))
			|| t.interface_boxed_type_key_marked(interface_boxed_type_key(iface, concrete_c_name)) {
			return true
		}
	}
	iface_is_specialized := iface_syntax.generic_app_valid()
	if !iface_is_specialized {
		concrete_base := if flat.source_type_shape(concrete_type_id).has(.bracketed) {
			transform_trimmed_type_id(flat.source_type_syntax(concrete_type_id).generic_base())
		} else {
			u32(0)
		}
		concrete_base_c_name := if concrete_base != 0 {
			flat.source_type_id(c_name(flat.source_type_name(concrete_base)))
		} else {
			u32(0)
		}
		for key, boxed in t.interface_boxed_types {
			if !boxed {
				continue
			}
			boxed_iface := u32(key >> 32)
			boxed_concrete := u32(key)
			if boxed_iface == 0 || boxed_concrete == 0 {
				continue
			}
			boxed_syntax := flat.source_type_syntax(boxed_iface)
			boxed_is_specialized := boxed_syntax.generic_app_valid()
			if boxed_is_specialized
				&& flat.source_type_syntax(boxed_syntax.generic_app_base()).unqualified() == iface_syntax.unqualified()
				&& (boxed_concrete == concrete_type_id || boxed_concrete == concrete_c_name
					|| (concrete_base != 0 && (boxed_concrete == concrete_base
						|| boxed_concrete == concrete_base_c_name))) {
				return true
			}
		}
	}
	// A concrete instantiation (`Foo[int]`) is boxed if its generic form was
	// boxed: the box scan only sees the generic-form argument and records the
	// base name `Foo` (see mark_interface_boxed_type), because the concrete
	// instantiations are materialised after the scan has frozen.
	if flat.source_type_shape(concrete_type_id).has(.bracketed) {
		base := transform_trimmed_type_id(flat.source_type_syntax(concrete_type_id).generic_base())
		if base != 0 {
			base_c_name := flat.source_type_id(c_name(flat.source_type_name(base)))
			for iface in iface_names {
				if t.interface_boxed_type_key_marked(interface_boxed_type_key(iface, base))
					|| t.interface_boxed_type_key_marked(interface_boxed_type_key(iface, base_c_name)) {
					return true
				}
			}
		}
	}
	for candidate in [concrete_type, t.tc.qualify_name(concrete_type)] {
		candidate_id := flat.source_type_existing_id(candidate) or { continue }
		target := t.tc.type_aliases.type_id(candidate_id) or { continue }
		for iface in iface_names {
			if t.interface_boxed_type_key_marked(interface_boxed_type_key(iface, target))
				|| t.interface_boxed_type_key_marked(interface_boxed_type_key(iface, flat.source_type_id(c_name(flat.source_type_name(target)))))
				|| t.interface_boxed_type_key_marked(interface_boxed_type_key(iface, flat.source_type_syntax(target).unqualified())) {
				return true
			}
		}
	}
	return false
}

fn (t &Transformer) interface_boxed_type_key_marked(key u64) bool {
	return t.interface_boxed_types[key] || t.interface_boxed_types_late[key]
}

fn (t &Transformer) interface_boxed_semantic_candidates(iface_name_id u32, concrete_type_id u32) []u32 {
	iface_syntax := flat.source_type_syntax(iface_name_id)
	concrete_type := flat.source_type_name(concrete_type_id)
	mut candidates := [iface_name_id]
	iface_is_specialized := iface_syntax.generic_app_valid()
	if iface_is_specialized {
		return candidates
	}
	concrete_c_name := flat.source_type_id(c_name(concrete_type))
	concrete_base := if flat.source_type_shape(concrete_type_id).has(.bracketed) {
		transform_trimmed_type_id(flat.source_type_syntax(concrete_type_id).generic_base())
	} else {
		u32(0)
	}
	concrete_base_c_name := if concrete_base != 0 {
		flat.source_type_id(c_name(flat.source_type_name(concrete_base)))
	} else {
		u32(0)
	}
	for key, boxed in t.interface_boxed_types {
		if !boxed {
			continue
		}
		boxed_iface := u32(key >> 32)
		boxed_concrete := u32(key)
		if boxed_iface == 0 || boxed_concrete == 0 {
			continue
		}
		boxed_syntax := flat.source_type_syntax(boxed_iface)
		boxed_is_specialized := boxed_syntax.generic_app_valid()
		if !boxed_is_specialized || flat.source_type_syntax(boxed_syntax.generic_app_base()).unqualified() != iface_syntax.unqualified() {
			continue
		}
		if boxed_concrete == concrete_type_id || boxed_concrete == concrete_c_name
			|| (concrete_base != 0 && (boxed_concrete == concrete_base
				|| boxed_concrete == concrete_base_c_name)) {
			if boxed_iface !in candidates {
				candidates << boxed_iface
			}
		}
	}
	return candidates
}

fn (mut t Transformer) collect_interface_boxed_types() {
	if t.interface_boxed_types_done || isnil(t.tc) {
		return
	}
	t.interface_boxed_types_done = true
	old_file := t.cur_file
	old_module := t.cur_module
	old_tc_file := t.tc.cur_file
	old_tc_module := t.tc.cur_module
	defer {
		t.cur_file = old_file
		t.cur_module = old_module
		t.tc.cur_file = old_tc_file
		t.tc.cur_module = old_tc_module
	}
	handed_over := t.tc.transform_boxed_node_count
	if handed_over >= 0 && handed_over <= t.a.nodes.len && t.interface_boxed_types.len == 0 {
		t.collect_interface_boxed_types_after_transform(handed_over)
	} else {
		t.collect_interface_boxed_types_range(0, t.a.nodes.len)
	}
	t.interface_boxed_types_frozen = true
}

// collect_interface_boxed_types_after_transform starts from the boxes the
// transform stage collected from the first `scanned` nodes before lowering and
// scans only what that scan did not see: the library bodies it skipped and the
// nodes lowering appended. File and module markers are read for their context.
fn (mut t Transformer) collect_interface_boxed_types_after_transform(scanned int) {
	for i, key in t.tc.transform_boxed_types {
		t.interface_boxed_types[key] = t.tc.transform_boxed_values[i]
	}
	skipped := t.tc.transform_boxed_skipped
	mut skip := []bool{len: scanned}
	for idx in 0 .. scanned {
		kind := t.a.nodes[idx].kind
		skip[idx] = kind != .file && kind != .module_decl && !(idx < skipped.len && skipped[idx])
	}
	old_skip := t.interface_boxed_skip_nodes
	t.interface_boxed_skip_nodes = skip
	t.collect_interface_boxed_types_range(0, t.a.nodes.len)
	t.interface_boxed_skip_nodes = old_skip
}

// unchecked_library_box_nodes snapshots only unchecked concrete library body ranges.
// The owning transformer shares this snapshot with its scan workers.
fn (t &Transformer) unchecked_library_box_nodes() []bool {
	$if ownership ? {
		return []bool{}
	}
	if isnil(t.tc) || !t.skip_generics || t.building_v || !t.tc.skips_library_bodies
		|| t.tc.autofree_enabled() || t.tc.top_level_idx.len == 0
		|| t.tc.top_level_idx_nodes_len != t.a.nodes.len {
		return []bool{}
	}
	// Reflected calls can make a concrete library method reachable during lowering,
	// after this scan has already supplied interface equality and stringifier metadata.
	for idx in 0 .. t.a.nodes.len {
		if t.a.nodes[idx].kind == .comptime_for {
			_, kind := comptime_for_parts(t.a.nodes[idx].value_text())
			if kind == 'methods' {
				return []bool{}
			}
		}
	}
	mut previous := -1
	for idx in t.tc.top_level_idx {
		if idx <= previous || idx >= t.a.nodes.len {
			return []bool{}
		}
		previous = idx
	}
	mut skipped := []bool{len: t.a.nodes.len}
	mut prev := -1
	mut file := ''
	mut mod_name := ''
	for idx in t.tc.top_level_idx {
		node := t.a.nodes[idx]
		if node.kind == .file {
			file = node.value_text()
			mod_name = t.tc.file_modules[file] or { '' }
		} else if node.kind == .module_decl {
			mod_name = node.value_text()
		} else if node.kind == .fn_decl && t.tc.skips_library_body_in_file(node, file, mod_name) {
			for child_idx in prev + 1 .. idx + 1 {
				skipped[child_idx] = true
			}
		}
		prev = idx
	}
	return skipped
}

// interface_box_scan_reads_kind reports whether collect_interface_boxed_types_range reads
// a node of `kind`.
@[inline]
fn interface_box_scan_reads_kind(kind flat.NodeKind) bool {
	return match kind {
		.file, .module_decl, .fn_decl, .fn_literal, .lambda_expr, .assign, .decl_assign,
		.selector_assign, .index_assign, .select_branch, .infix, .cast_expr, .call, .struct_init {
			true
		}
		else {
			false
		}
	}
}

@[direct_array_access]
fn (mut t Transformer) collect_interface_boxed_types_range(start int, end int) {
	limit := if end < t.a.nodes.len { end } else { t.a.nodes.len }
	own_cache := isnil(t.box_type_text_cache)
	if own_cache {
		t.box_type_text_cache = &BoxTypeTextCache{
			ids:     []u32{len: 4096}
			results: []bool{len: 4096}
		}
	}
	defer {
		if own_cache {
			t.box_type_text_cache = unsafe { nil }
		}
	}
	for idx in start .. limit {
		if idx < t.interface_boxed_skip_nodes.len && t.interface_boxed_skip_nodes[idx] {
			continue
		}
		// Most nodes are of a kind this scan does nothing with.
		if !interface_box_scan_reads_kind(t.a.nodes[idx].kind) {
			continue
		}
		node := t.a.nodes[idx]
		if node.kind == .file {
			t.cur_file = node.value_text()
			t.tc.cur_file = node.value_text()
			t.cur_module = t.tc.file_modules[node.value_text()] or { '' }
			t.tc.cur_module = t.cur_module
			continue
		}
		if node.kind == .module_decl {
			t.cur_module = node.value_text()
			t.tc.cur_module = node.value_text()
			continue
		}
		if node.kind == .fn_decl && t.interface_box_type_text_maybe(node.typ) {
			return_type := t.tc.parse_type(node.typ)
			if interface_box_expected_type(return_type) {
				for i in 0 .. node.children_count {
					t.collect_interface_return_boxes(t.a.child(&node, i), return_type)
				}
			}
		}
		if node.kind in [.fn_literal, .lambda_expr] {
			literal_id := flat.NodeId(idx)
			return_type := t.interface_box_fn_literal_return_type(literal_id, node) or { continue }
			if interface_box_expected_type(return_type) {
				for i in 0 .. node.children_count {
					t.collect_interface_return_boxes(t.a.child(&node, i), return_type)
				}
				if node.kind == .lambda_expr && node.children_count > 0 {
					t.collect_interface_boxed_value(t.a.child(&node, node.children_count - 1), return_type)
				}
			}
		}
		if node.kind in [.assign, .decl_assign, .selector_assign, .index_assign] {
			t.collect_interface_assign_boxes(node)
		}
		if node.kind == .select_branch && node.value == 'recv_assign' {
			t.collect_interface_select_receive_boxes(node)
		}
		if node.kind == .infix && node.children_count >= 2 {
			if node.op == .left_shift {
				t.collect_interface_append_boxes(node)
			} else if node.op == .arrow {
				t.collect_interface_channel_send_boxes(node)
			}
		}
		if node.kind == .cast_expr && node.children_count == 1
			&& t.interface_box_type_text_maybe(node.source_value_id()) {
			expected := t.tc.parse_type(node.source_value_id())
			if interface_box_expected_type(expected) {
				t.collect_interface_boxed_value(t.a.child(&node, 0), expected)
			}
		}
		if node.kind == .call && node.children_count > 0 {
			t.collect_interface_call_boxes(flat.NodeId(idx), node)
		}
		if node.kind != .struct_init {
			continue
		}
		t.collect_interface_boxed_value(flat.NodeId(idx), t.tc.parse_type(node.source_value_id()))
		iface_name := t.interface_literal_name(node.source_value_id()) or { continue }
		for i in 0 .. node.children_count {
			field := t.a.child_node(&node, i)
			if field.kind != .field_init || field.value_text() != '_object' {
				continue
			}
			concrete_type := if flat.source_type_shape(field.typ).has(.pointer) {
				flat.source_type_syntax(field.typ).raw_base()
			} else {
				field.typ
			}
			t.mark_interface_boxed_type(iface_name, concrete_type)
		}
	}
}

fn (mut t Transformer) collect_lowered_interface_boxed_types_range(start int, end int) {
	limit := if end < t.a.nodes.len { end } else { t.a.nodes.len }
	for idx in start .. limit {
		node := t.a.nodes[idx]
		if node.kind != .struct_init {
			continue
		}
		iface_name := t.interface_literal_name(node.source_value_id()) or { continue }
		for i in 0 .. node.children_count {
			field := t.a.child_node(&node, i)
			if field.kind != .field_init || field.value_text() != '_object' {
				continue
			}
			concrete_type := if flat.source_type_shape(field.typ).has(.pointer) {
				flat.source_type_syntax(field.typ).raw_base()
			} else {
				field.typ
			}
			t.mark_interface_boxed_type(iface_name, concrete_type)
		}
	}
}

fn (t &Transformer) interface_box_fn_literal_return_type(id flat.NodeId, node flat.Node) ?types.Type {
	if typ := t.tc.expr_type(id) {
		if return_type := interface_box_fn_type_return_type(typ) {
			return return_type
		}
	}
	// The interface prescan has no lexical locals. Generated typeof witnesses
	// are resolved when lowering their enclosing function, before specialization.
	if node.kind == .fn_literal && node.typ != 0 && !inferred_type_id(node.typ) {
		return *t.tc.parse_type(node.typ)
	}
	return none
}

fn interface_box_fn_type_return_type(typ types.Type) ?types.Type {
	if typ is types.FnType {
		return *typ.return_type
	}
	if typ is types.Alias {
		return interface_box_fn_type_return_type(typ.base_type)
	}
	return none
}

// interface_box_source_type resolves a source binding before transform workers register locals.
fn (t &Transformer) interface_box_source_type(id flat.NodeId) types.Type {
	if typ := t.tc.expr_type(id) {
		if typ !is types.Unknown {
			return typ
		}
	}
	source_id := t.unwrap_parens(id)
	source := t.a.node(source_id)
	if source.kind == .ident {
		mut cursor := int(source_id)
		for _ in 0 .. t.a.nodes.len {
			parent_id := t.source_parent_id(cursor)
			if parent_id < 0 {
				break
			}
			parent := t.a.nodes[parent_id]
			mut previous := 0
			for i in 0 .. parent.children_count {
				if int(t.a.child(&parent, i)) == cursor {
					previous = i
					break
				}
			}
			for i := previous - 1; i >= 0; i-- {
				child_id := t.a.child(&parent, i)
				child := t.a.node(child_id)
				if child.kind == .param && child.value_text() == source.value_text() {
					return *t.tc.parse_type(child.typ)
				}
				if child.kind == .decl_assign {
					if typ_source_id := t.local_decl_type_at(source.value_text(), int(child_id)) {
						return *t.tc.parse_type(typ_source_id)
					}
				}
			}
			if parent.kind in [.fn_decl, .fn_literal, .lambda_expr] {
				break
			}
			cursor = parent_id
		}
	}
	return t.tc.resolve_type(id)
}

// collect_interface_array_insert_boxes records concrete elements before array mutation lowering.
fn (mut t Transformer) collect_interface_array_insert_boxes(node flat.Node) {
	if node.children_count < 2 {
		return
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .selector || callee.children_count == 0
		|| callee.value_text() !in ['insert', 'prepend'] {
		return
	}
	value_index := if callee.value_text() == 'insert' { 2 } else { 1 }
	if node.children_count != value_index + 1 {
		return
	}
	receiver_id := t.a.child(callee, 0)
	receiver_type := unalias_type(types.unwrap_all_pointers(t.interface_box_source_type(receiver_id)))
	if receiver_type !is types.Array {
		return
	}
	value_id := t.a.child(&node, value_index)
	expected := t.interface_box_append_expected_type(receiver_type, receiver_id, value_id)
	if !interface_box_expected_type(expected) {
		return
	}
	value := t.a.node(value_id)
	source_id := if value.kind == .prefix && value.value_text() == '...' && value.children_count > 0 {
		t.a.child(value, 0)
	} else {
		value_id
	}
	actual := t.interface_box_source_type(source_id)
	t.collect_interface_boxed_type(actual, expected)
	t.collect_interface_boxed_value(source_id, expected)
}

fn (mut t Transformer) collect_interface_call_boxes(call_id flat.NodeId, node flat.Node) {
	// Builtin array mutations have no ordinary function parameter signature to scan.
	t.collect_interface_array_insert_boxes(node)
	mut call_name := t.tc.resolved_call_name(call_id) or { '' }
	callee := t.a.child_node(&node, 0)
	if call_name.len == 0 {
		if callee.kind == .ident {
			call_name = callee.value_text()
		}
	}
	params := t.tc.fn_param_types_for_name(call_name)
	if params.len == 0 {
		return
	}
	explicit_args := int(node.children_count) - 1
	mut field_init_args := 0
	for arg_idx in 0 .. explicit_args {
		if t.a.child_node(&node, arg_idx + 1).kind == .field_init {
			field_init_args++
		}
	}
	params_may_box := params.any(t.interface_box_call_param_maybe(it))
	last_param := params[params.len - 1]
	variadic_elem_may_box := if field_init_args > 0 && last_param is types.Array {
		t.interface_box_call_param_maybe(last_param.elem_type)
	} else {
		false
	}
	if !params_may_box && !variadic_elem_may_box {
		return
	}
	is_variadic := t.call_is_variadic(call_name)
	variadic_idx := if is_variadic && params[params.len - 1] is types.Array {
		params.len - 1
	} else {
		-1
	}
	if !params_may_box && variadic_idx < 0 {
		return
	}
	param_offset := if callee.kind == .selector {
		t.call_param_offset(call_name, node, params)
	} else {
		0
	}
	logical_args := explicit_args - field_init_args + if field_init_args > 0 { 1 } else { 0 }
	hidden_ctx_offset := if t.tc.fn_implicit_veb_ctx[call_name]
		&& params.len - param_offset > logical_args {
		1
	} else {
		0
	}
	logical_params := params.len - param_offset - hidden_ctx_offset
	mut omitted_params_struct := u32(0)
	if variadic_idx < 0 && logical_params != logical_args {
		if logical_params == logical_args + 1 {
			omitted_params_struct = t.params_struct_type_name(params[params.len - 1].source_type_id()) or {
				return
			}
		} else {
			return
		}
	}
	if variadic_idx >= 0 && logical_args < variadic_idx - param_offset - hidden_ctx_offset {
		return
	}
	for arg_idx in 0 .. explicit_args {
		arg_id := t.a.child(&node, arg_idx + 1)
		param_idx := arg_idx + param_offset + hidden_ctx_offset
		arg := t.a.nodes[int(arg_id)]
		if arg.kind == .field_init {
			if param_idx < params.len {
				mut param_type := params[param_idx].source_type_id()
				if param_idx == variadic_idx {
					variadic_type := params[param_idx]
					if variadic_type is types.Array {
						param_type = variadic_type.elem_type.source_type_id()
					}
				}
				struct_type_source_id := t.params_struct_type_name(param_type) or {
					t.struct_arg_type_name(param_type) or { u32(0) }
				}

				if struct_type_source_id != 0 {
					t.collect_interface_struct_call_fields(node, arg_idx + 1, struct_type_source_id)
				}
			}
			break
		}
		mut expected := if variadic_idx >= 0 && param_idx >= variadic_idx {
			variadic_type := params[variadic_idx]
			if variadic_type is types.Array {
				*variadic_type.elem_type
			} else {
				variadic_type
			}
		} else if param_idx < params.len {
			params[param_idx]
		} else {
			continue
		}
		mut value_id := arg_id
		if variadic_idx >= 0 && param_idx >= variadic_idx && arg.kind == .prefix
			&& arg.value_text() == '...' && arg.children_count > 0 {
			expected = params[variadic_idx]
			value_id = t.a.child(&arg, 0)
		}
		if interface_box_expected_type(expected) {
			t.collect_interface_boxed_value(value_id, expected)
		}
	}
	if omitted_params_struct != 0 {
		t.collect_interface_struct_default_boxes(omitted_params_struct, []string{})
	}
}

fn (mut t Transformer) interface_box_call_param_maybe(param types.Type) bool {
	// Only structs and aliases need module-dependent field lookup. Other types
	// can use the recursive predicate without constructing a cache key.
	if param !is types.Struct && param !is types.Alias {
		return interface_box_expected_type(param)
	}
	key := t.tc.type_name(param)
	if !isnil(t.interface_box_param_cache) {
		mut cache := t.interface_box_param_cache
		cached := cache.get(t.cur_module, key)
		if cached != 0 {
			return cached > 0
		}
	}
	result := t.interface_box_call_param_maybe_uncached(param)
	if !isnil(t.interface_box_param_cache) {
		mut cache := t.interface_box_param_cache
		cache.put(t.cur_module, key, if result { i8(1) } else { i8(-1) })
	}
	return result
}

fn (t &Transformer) interface_box_call_param_maybe_uncached(param types.Type) bool {
	if interface_box_expected_type(param) {
		return true
	}
	if param !is types.Struct && param !is types.Alias {
		return false
	}
	struct_type_source_id := t.params_struct_type_name(param.source_type_id()) or {
		t.struct_arg_type_name(param.source_type_id()) or { return false }
	}
	struct_type_source_id_4 := struct_type_source_id

	info := t.lookup_struct_info(struct_type_source_id_4) or { return false }
	for field in info.fields {
		field_type_text_source_id := t.lookup_struct_field_type(struct_type_source_id, field.name) or { field.typ }

		if interface_box_expected_type(t.tc.parse_type(field_type_text_source_id)) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) collect_interface_struct_call_fields(node flat.Node, field_start int, struct_type_id u32) {
	info := t.lookup_struct_info(struct_type_id) or { return }
	mut field_index := 0
	mut provided := []string{}
	for i in field_start .. node.children_count {
		field := t.a.child_node(&node, i)
		if field.kind != .field_init {
			break
		}
		field_name := if field.value_text().len > 0 {
			field.value_text()
		} else if field_index < info.fields.len {
			info.fields[field_index].name
		} else {
			field_index++
			continue
		}
		provided << field_name
		if field.children_count == 0 {
			field_index++
			continue
		}
		field_type_text_source_id := t.lookup_struct_field_type(struct_type_id, field_name) or {
			field_index++
			continue
		}

		field_type := t.tc.parse_type(field_type_text_source_id)
		if interface_box_expected_type(field_type) {
			t.collect_interface_boxed_value(t.a.child(field, 0), field_type)
		}
		field_index++
	}
	t.collect_interface_struct_default_boxes(struct_type_id, provided)
}

fn (mut t Transformer) collect_interface_struct_default_boxes(struct_type_id u32, provided []string) {
	info := t.lookup_struct_info(struct_type_id) or { return }
	for field in info.fields {
		if field.name in provided || int(field.default_expr) < 0 {
			continue
		}
		field_type_text_source_id := t.lookup_struct_field_type(struct_type_id, field.name) or { field.typ }

		field_type := t.tc.parse_type(field_type_text_source_id)
		if interface_box_expected_type(field_type) {
			t.collect_interface_boxed_value(field.default_expr, field_type)
		}
	}
}

fn (mut t Transformer) collect_interface_return_boxes(id flat.NodeId, return_type types.Type) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.fn_decl, .fn_literal, .lambda_expr] {
		return
	}
	if node.kind == .return_stmt {
		if node.children_count == 1 {
			if expected_types := multi_return_types_from_type(return_type, 0) {
				value_id := t.a.child(&node, 0)
				if actual_types := t.multi_return_types_for_expr(value_id, expected_types.len) {
					for i, expected in expected_types {
						if interface_box_expected_type(expected) {
							t.collect_interface_boxed_type(actual_types[i], expected)
						}
					}
					return
				}
			}
		}
		if return_types := multi_return_types_from_type(return_type, int(node.children_count)) {
			for i, expected in return_types {
				t.collect_interface_boxed_value(t.a.child(&node, i), expected)
			}
			return
		}
		for i in 0 .. node.children_count {
			t.collect_interface_boxed_value(t.a.child(&node, i), return_type)
		}
		return
	}
	for i in 0 .. node.children_count {
		t.collect_interface_return_boxes(t.a.child(&node, i), return_type)
	}
}

fn (mut t Transformer) collect_interface_assign_boxes(node flat.Node) {
	lhs_count := t.multi_assign_lhs_count(node)
	rhs_count := t.multi_assign_rhs_count(node)
	if lhs_count > 1 && rhs_count == 1 {
		rhs_id := t.multi_assign_rhs_id(node, 0)
		if rhs_types := t.multi_return_types_for_expr(rhs_id, lhs_count) {
			for i in 0 .. lhs_count {
				lhs_id := t.multi_assign_lhs_id(node, i)
				expected := t.interface_box_lhs_type(lhs_id)
				if interface_box_expected_type(expected) {
					t.collect_interface_boxed_type(rhs_types[i], expected)
				}
			}
			return
		}
	}
	for i in 0 .. rhs_count {
		lhs_id := t.multi_assign_lhs_id(node, i)
		rhs_id := t.multi_assign_rhs_id(node, i)
		lhs := t.a.nodes[int(lhs_id)]
		if lhs.typ != 0 && !t.interface_box_type_text_maybe(lhs.typ) {
			continue
		}
		lhs_type := t.interface_box_lhs_type(lhs_id)
		mut expected := lhs_type
		if node.op == .left_shift_assign {
			expected = t.interface_box_append_expected_type(lhs_type, lhs_id, rhs_id)
		}
		if interface_box_expected_type(expected) {
			t.collect_interface_boxed_value(rhs_id, expected)
		}
	}
}

fn (t &Transformer) interface_box_lhs_type(lhs_id flat.NodeId) types.Type {
	lhs := t.a.nodes[int(lhs_id)]
	return if lhs.typ != 0 {
		*t.tc.parse_type(lhs.typ)
	} else {
		t.tc.expr_type(lhs_id) or { t.tc.resolve_type(lhs_id) }
	}
}

fn (mut t Transformer) collect_interface_boxed_type(actual types.Type, expected types.Type) {
	if actual is types.OptionType {
		t.collect_interface_boxed_type(actual.base_type, expected)
		return
	}
	if actual is types.ResultType {
		t.collect_interface_boxed_type(actual.base_type, expected)
		return
	}
	match expected {
		types.Alias {
			t.collect_interface_boxed_type(actual, expected.base_type)
		}
		types.OptionType {
			actual_base := if actual is types.OptionType { *actual.base_type } else { actual }
			t.collect_interface_boxed_type(actual_base, expected.base_type)
		}
		types.ResultType {
			actual_base := if actual is types.ResultType { *actual.base_type } else { actual }
			t.collect_interface_boxed_type(actual_base, expected.base_type)
		}
		types.Pointer {
			t.collect_interface_boxed_type(actual, expected.base_type)
		}
		types.Interface {
			iface_name_source_id := t.resolve_interface_type_name(expected.source_type_id())
			iface_name_source_id_4 := iface_name_source_id

			actual_source_id := actual.source_type_id()
			if iface_name_source_id != 0 && actual_source_id !in [u32(0), flat.source_type_unknown,
				flat.source_type_void] && t.resolve_interface_type_name(actual_source_id) == 0 {
				t.mark_interface_boxed_type(iface_name_source_id_4, t.trim_pointer_type(actual_source_id))
			}
		}
		types.Array {
			actual_base := unalias_type(actual)
			if actual_base is types.Array {
				t.collect_interface_boxed_type(actual_base.elem_type, expected.elem_type)
			} else if actual_base is types.ArrayFixed {
				t.collect_interface_boxed_type(actual_base.elem_type, expected.elem_type)
			}
		}
		types.ArrayFixed {
			actual_base := unalias_type(actual)
			if actual_base is types.ArrayFixed {
				t.collect_interface_boxed_type(actual_base.elem_type, expected.elem_type)
			}
		}
		types.Map {
			actual_base := unalias_type(actual)
			if actual_base is types.Map {
				t.collect_interface_boxed_type(actual_base.key_type, expected.key_type)
				t.collect_interface_boxed_type(actual_base.value_type, expected.value_type)
			}
		}
		else {}
	}
}

fn (mut t Transformer) collect_interface_append_boxes(node flat.Node) {
	lhs_id := t.a.child(&node, 0)
	rhs_id := t.a.child(&node, 1)
	lhs_type := t.tc.expr_type(lhs_id) or { t.tc.resolve_type(lhs_id) }
	expected := t.interface_box_append_expected_type(lhs_type, lhs_id, rhs_id)
	if interface_box_expected_type(expected) {
		actual := t.tc.expr_type(rhs_id) or { t.tc.resolve_type(rhs_id) }
		t.collect_interface_boxed_type(actual, expected)
		t.collect_interface_boxed_value(rhs_id, expected)
	}
}

fn (mut t Transformer) collect_interface_channel_send_boxes(node flat.Node) {
	channel_id := t.a.child(&node, 0)
	value_id := t.a.child(&node, 1)
	channel_type := unalias_type(t.tc.expr_type(channel_id) or {
		t.tc.resolve_type(channel_id)
	})
	if channel_type is types.Channel && interface_box_expected_type(channel_type.elem_type) {
		t.collect_interface_boxed_value(value_id, channel_type.elem_type)
	}
}

fn (mut t Transformer) collect_interface_select_receive_boxes(node flat.Node) {
	if node.children_count < 2 {
		return
	}
	lhs_id := t.a.child(&node, 0)
	recv := t.a.child_node(&node, 1)
	if recv.kind != .prefix || recv.op != .arrow || recv.children_count == 0 {
		return
	}
	expected := t.interface_box_lhs_type(lhs_id)
	if !interface_box_expected_type(expected) {
		return
	}
	channel_id := t.a.child(recv, 0)
	channel_type := unalias_type(t.tc.expr_type(channel_id) or {
		t.tc.resolve_type(channel_id)
	})
	if channel_type is types.Channel {
		t.collect_interface_boxed_type(channel_type.elem_type, expected)
	}
}

fn (t &Transformer) interface_box_append_expected_type(lhs_type types.Type, lhs_id flat.NodeId, rhs_id flat.NodeId) types.Type {
	clean := unalias_type(lhs_type)
	if clean is types.Array {
		rhs_type := t.tc.expr_type(rhs_id) or { t.tc.resolve_type(rhs_id) }
		if t.array_append_rhs_is_push_many(lhs_id, rhs_id, rhs_type.source_type_id(), clean.elem_type.source_type_id()) {
			return lhs_type
		}
		return *clean.elem_type
	}
	return lhs_type
}

// BoxTypeTextCache remembers interface_box_type_text_maybe by type id while a
// box scan runs, which registers no type or alias the test reads.
@[heap]
struct BoxTypeTextCache {
mut:
	ids     []u32
	results []bool
}

fn (t &Transformer) interface_box_type_text_maybe(raw_type_id u32) bool {
	if raw_type_id == 0 {
		return false
	}
	if isnil(t.box_type_text_cache) {
		return t.interface_box_type_text_maybe_uncached(raw_type_id)
	}
	mut cache := t.box_type_text_cache
	slot := int(raw_type_id & 4095)
	if cache.ids[slot] == raw_type_id {
		return cache.results[slot]
	}
	result := t.interface_box_type_text_maybe_uncached(raw_type_id)
	cache.ids[slot] = raw_type_id
	cache.results[slot] = result
	return result
}

fn (t &Transformer) interface_box_type_text_maybe_uncached(raw_type_id u32) bool {
	clean_id := t.trimmed_type_id(t.normalize_type_alias(raw_type_id))
	shape := flat.source_type_shape(clean_id)
	syntax := flat.source_type_syntax(clean_id)
	if shape.has(.optional) {
		if syntax.raw_base() == 0 { return false }
		return t.interface_box_type_text_maybe(t.optional_base_type(clean_id))
	}
	if syntax.tuple_like() && syntax.tuple_contains_comma() {
		for part_id in syntax.tuple_components() {
			if t.interface_box_type_text_maybe(part_id) { return true }
		}
		return false
	}
	return t.resolve_interface_type_name(clean_id) != 0 || shape.has(.array_type)
		|| shape.has(.leading_bracket) || shape.has(.map_type)
}

fn interface_box_expected_type(expected types.Type) bool {
	return match expected {
		types.Interface {
			true
		}
		types.Alias {
			interface_box_expected_type(expected.base_type)
		}
		types.OptionType {
			interface_box_expected_type(expected.base_type)
		}
		types.ResultType {
			interface_box_expected_type(expected.base_type)
		}
		types.Pointer {
			interface_box_expected_type(expected.base_type)
		}
		types.MultiReturn {
			expected.types.any(interface_box_expected_type(it))
		}
		types.Array {
			interface_box_expected_type(expected.elem_type)
		}
		types.ArrayFixed {
			interface_box_expected_type(expected.elem_type)
		}
		types.Map {
			interface_box_expected_type(expected.key_type)
				|| interface_box_expected_type(expected.value_type)
		}
		else {
			false
		}
	}
}

fn (mut t Transformer) collect_interface_boxed_value(id flat.NodeId, expected types.Type) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.paren, .expr_stmt {
			if node.children_count > 0 {
				t.collect_interface_boxed_value(t.a.child(&node, 0), expected)
			}
			return
		}
		.if_expr, .match_stmt {
			for i in 1 .. node.children_count {
				t.collect_interface_boxed_value(t.a.child(&node, i), expected)
			}
			return
		}
		.or_expr {
			if node.children_count > 0 {
				source_id := t.a.child(&node, 0)
				source_type := t.tc.expr_type(source_id) or { t.tc.resolve_type(source_id) }
				t.collect_interface_boxed_type(source_type, expected)
			}
			if node.children_count >= 2 {
				t.collect_interface_boxed_value(t.a.child(&node, 1), expected)
			}
			return
		}
		.block, .match_branch {
			if node.children_count > 0 {
				t.collect_interface_boxed_value(t.a.child(&node, node.children_count - 1), expected)
			}
			return
		}
		else {}
	}

	match expected {
		types.Alias {
			t.collect_interface_boxed_value(id, expected.base_type)
		}
		types.OptionType {
			actual := unalias_type(t.tc.expr_type(id) or { t.tc.resolve_type(id) })
			if actual is types.OptionType {
				t.collect_interface_boxed_type(actual.base_type, expected.base_type)
			} else {
				t.collect_interface_boxed_value(id, expected.base_type)
			}
		}
		types.ResultType {
			actual := unalias_type(t.tc.expr_type(id) or { t.tc.resolve_type(id) })
			if actual is types.ResultType {
				t.collect_interface_boxed_type(actual.base_type, expected.base_type)
			} else {
				t.collect_interface_boxed_value(id, expected.base_type)
			}
		}
		types.Pointer {
			t.collect_interface_boxed_value(id, expected.base_type)
		}
		types.Interface {
			iface_name_source_id_2 := t.resolve_interface_type_name(expected.source_type_id())
			iface_name_source_id_5 := iface_name_source_id_2

			if iface_name_source_id_2 != 0 {
				t.collect_interface_boxed_interface_value(id, iface_name_source_id_5)
			}
		}
		types.Array {
			t.collect_interface_boxed_array_value(node, expected.elem_type, expected)
		}
		types.ArrayFixed {
			t.collect_interface_boxed_array_value(node, expected.elem_type, expected)
		}
		types.Map {
			if node.kind != .map_init {
				return
			}
			for i := 0; i + 1 < node.children_count; i += 2 {
				t.collect_interface_boxed_value(t.a.child(&node, i), expected.key_type)
				t.collect_interface_boxed_value(t.a.child(&node, i + 1), expected.value_type)
			}
		}
		types.Struct {
			t.collect_interface_boxed_struct_value(node, expected.source_type_id())
		}
		else {}
	}
}

fn (mut t Transformer) collect_interface_boxed_struct_value(node flat.Node, struct_name_id u32) {
	if node.kind != .struct_init {
		return
	}
	literal_name_id := if node.value_text().len > 0 {
		node.source_value_id()
	} else {
		struct_name_id
	}
	info := t.lookup_struct_info(literal_name_id) or { return }
	for i in 0 .. node.children_count {
		field := t.a.child_node(&node, i)
		if field.kind != .field_init || field.children_count == 0 {
			continue
		}
		field_name := if field.value_text().len > 0 {
			field.value_text()
		} else if i < info.fields.len {
			info.fields[i].name
		} else {
			continue
		}
		field_type_text_source_id := t.lookup_struct_field_type(literal_name_id, field_name) or {
			t.lookup_struct_field_type(struct_name_id, field_name) or { continue }
		}

		field_type := t.tc.parse_type(field_type_text_source_id)
		if interface_box_expected_type(field_type) {
			t.collect_interface_boxed_value(t.a.child(field, 0), field_type)
		}
	}
	for field in info.fields {
		if int(field.default_expr) < 0
			|| t.interface_box_struct_field_provided(node, info, field.name) {
			continue
		}
		field_type_text_source_id_2 := t.lookup_struct_field_type(literal_name_id, field.name) or {
			t.lookup_struct_field_type(struct_name_id, field.name) or { continue }
		}

		field_type := t.tc.parse_type(field_type_text_source_id_2)
		if interface_box_expected_type(field_type) {
			t.collect_interface_boxed_value(field.default_expr, field_type)
		}
	}
}

fn (t &Transformer) interface_box_struct_field_provided(node flat.Node, info StructInfo, name string) bool {
	for i in 0 .. node.children_count {
		field := t.a.child_node(&node, i)
		if field.kind != .field_init {
			continue
		}
		field_name := if field.value_text().len > 0 {
			field.value_text()
		} else if i < info.fields.len {
			info.fields[i].name
		} else {
			continue
		}
		if field_name == name {
			return true
		}
	}
	return false
}

fn (mut t Transformer) collect_interface_boxed_array_value(node flat.Node, elem_type types.Type, array_type types.Type) {
	if node.kind == .postfix && node.children_count > 0 {
		t.collect_interface_boxed_value(t.a.child(&node, 0), array_type)
		return
	}
	if node.kind !in [.array_literal, .array_init] {
		return
	}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if child.kind == .field_init {
			if child.value_text() == 'init' && child.children_count > 0 {
				t.collect_interface_boxed_value(t.a.child(&child, 0), elem_type)
			}
			continue
		}
		if child.kind == .prefix && child.value_text() == '...' && child.children_count > 0 {
			t.collect_interface_boxed_value(t.a.child(&child, 0), array_type)
			continue
		}
		t.collect_interface_boxed_value(child_id, elem_type)
	}
}

fn (mut t Transformer) collect_interface_boxed_interface_value(id flat.NodeId, iface_name_id u32) {
	node := t.a.nodes[int(id)]
	actual := t.tc.expr_type(id) or { t.tc.resolve_type(id) }
	actual_type_source_id := actual.source_type_id()
	actual_source_id := if actual_type_source_id in [u32(0), flat.source_type_unknown] && node.kind == .struct_init {
		node.source_value_id()
	} else {
		actual_type_source_id
	}
	if actual_source_id !in [u32(0), flat.source_type_unknown, flat.source_type_void] && t.resolve_interface_type_name(actual_source_id) == 0 {
		t.mark_interface_boxed_type(iface_name_id, t.trim_pointer_type(actual_source_id))
	}
}

fn (t &Transformer) interface_literal_name(name_id u32) ?u32 {
	resolved_source_id_2 := t.resolve_interface_type_name(name_id)

	if resolved_source_id_2 != 0 {
		return resolved_source_id_2
	}
	return none
}

fn (mut t Transformer) mark_interface_boxed_type(iface_name_id u32, concrete_type_id u32) {
	if iface_name_id == 0 || concrete_type_id == 0 {
		return
	}
	concrete_type := flat.source_type_name(concrete_type_id)
	mut iface_names := [iface_name_id]
	resolved := t.resolve_interface_type_name(iface_name_id)
	if resolved != 0 && resolved != iface_name_id {
		iface_names << resolved
	}
	// A box also reaches receiver methods inherited from embedded interfaces.
	// Retain its concrete type there so mutable field access uses the shared object.
	mut iface_index := 0
	for iface_index < iface_names.len {
		iface := iface_names[iface_index]
		iface_index++
		for embed in t.tc.interface_embeds.type_ids(iface) or { []u32{} } {
			if embed !in iface_names {
				iface_names << embed
			}
		}
	}
	concrete_c_name := flat.source_type_id(c_name(concrete_type))
	for iface in iface_names {
		t.mark_interface_boxed_type_key(interface_boxed_type_key(iface, concrete_type_id))
		t.mark_interface_boxed_type_key(interface_boxed_type_key(iface, concrete_c_name))
		// Also record the generic base name (`Foo` for `Foo[T]`). The box scan
		// runs once and then freezes, so it sees only the generic-form box (a
		// `Foo[W]` argument passed to an interface parameter); the concrete
		// instantiations (`Foo[int]`) are created later and cannot be marked.
		// Recording the base lets `interface_boxed_type_marked` recognise any of
		// those instantiations as boxed.
		if flat.source_type_shape(concrete_type_id).has(.bracketed) {
			base := transform_trimmed_type_id(flat.source_type_syntax(concrete_type_id).generic_base())
			if base != 0 {
				t.mark_interface_boxed_type_key(interface_boxed_type_key(iface, base))
				t.mark_interface_boxed_type_key(interface_boxed_type_key(iface, flat.source_type_id(c_name(flat.source_type_name(base)))))
			}
		}
	}
}

fn (mut t Transformer) mark_interface_boxed_type_key(key u64) {
	if t.interface_boxed_types_frozen {
		t.interface_boxed_types_late[key] = true
	} else {
		t.interface_boxed_types[key] = true
	}
}

// interface_boxed_type_key retains both complete source spelling IDs.
fn interface_boxed_type_key(iface_name_id u32, concrete_type_id u32) u64 {
	return (u64(iface_name_id) << 32) | u64(concrete_type_id)
}

fn (mut t Transformer) materialize_generic_structs(erase_templates bool) {
	debug_started := time.ticks()
	if isnil(t.tc) {
		return
	}
	decls := t.collect_generic_struct_decls()
	sum_decls := t.collect_generic_sum_decls()
	t.monomorph_profile('mono materialize decls: ${time.ticks() - debug_started} ms')
	if decls.len == 0 && sum_decls.len == 0 {
		return
	}
	mut specs := map[u32]u32{}
	mut sum_specs := map[u32]GenericSpecContext{}
	if t.generic_materialization_ready {
		specs = t.generic_struct_specs_cache.clone()
		sum_specs = t.generic_sum_specs_cache.clone()
		t.collect_generic_struct_specs_range(decls, mut specs, t.generic_materialization_scan_from, t.a.nodes.len)
		t.collect_generic_sum_specs_range(sum_decls, mut sum_specs, t.generic_materialization_scan_from, t.a.nodes.len)
		t.expand_generic_materialization_specs(decls, sum_decls, mut specs, mut sum_specs)
	} else {
		specs, sum_specs = t.collect_generic_materialization_specs(decls, sum_decls)
	}
	t.monomorph_profile('mono materialize specs: ${time.ticks() - debug_started} ms (${specs.len}/${sum_specs.len})')
	for spec, context in sum_specs {
		decl := sum_decls[context.base] or { continue }
		t.materialize_generic_sum_spec(spec, decl, context)
	}
	t.monomorph_profile('mono materialize sums: ${time.ticks() - debug_started} ms')
	t.materialize_generic_struct_specs(specs, decls)
	t.monomorph_profile('mono materialize structs: ${time.ticks() - debug_started} ms')
	if erase_templates {
		for decl_id, _ in decls {
			t.tc.structs.delete(decl_id)
			t.tc.unions.delete(decl_id)
			t.tc.params_structs.delete(decl_id)
			t.tc.unregister_short_type_name(decl_id)
		}
		if !isnil(t.receiver_method_cache) {
			t.receiver_method_cache.bare_structs_len = -1
		}
		// Refresh once after both registries have finished removing templates.
		t.erase_generic_sum_templates(sum_decls)
	}
	t.monomorph_profile('mono materialize erase: ${time.ticks() - debug_started} ms')
}

fn (mut t Transformer) materialize_generic_struct_specs(specs map[u32]u32, decls map[u32]GenericStructDecl) {
	if isnil(t.tc) {
		return
	}
	spec_names := specs.keys().map(t.comptime_field_type_id_key(it, 'main'))
	types.extend_stable_type_indexes(mut t.tc.runtime_type_indexes, spec_names)
	for spec, base_id in specs {
		decl := decls[base_id] or { continue }
		t.materialize_generic_struct_spec(spec, decl)
	}
	started := time.ticks()
	if specs.len > t.interface_impl_spec_count {
		t.tc.clear_interface_impl_cache()
		t.refresh_interface_impl_indexes_for_generic_specs(specs)
		t.interface_impl_spec_count = specs.len
	}
	t.monomorph_profile('mono materialize interfaces: ${time.ticks() - started} ms')
}

fn (mut t Transformer) materialize_generic_sum_types(erase_templates bool) {
	if isnil(t.tc) {
		return
	}
	decls := t.collect_generic_struct_decls()
	sum_decls := t.collect_generic_sum_decls()
	if sum_decls.len == 0 {
		specs, _ := t.collect_generic_materialization_specs(decls, sum_decls)
		t.generic_struct_specs_cache = specs.clone()
		t.generic_sum_specs_cache = map[u32]GenericSpecContext{}
		t.generic_materialization_scan_from = t.a.nodes.len
		t.generic_materialization_ready = true
		return
	}
	specs, sum_specs := t.collect_generic_materialization_specs(decls, sum_decls)
	t.generic_struct_specs_cache = specs.clone()
	t.generic_sum_specs_cache = sum_specs.clone()
	t.generic_materialization_scan_from = t.a.nodes.len
	t.generic_materialization_ready = true
	for spec, context in sum_specs {
		decl := sum_decls[context.base] or { continue }
		t.materialize_generic_sum_spec(spec, decl, context)
	}
	if erase_templates {
		t.erase_generic_sum_templates(sum_decls)
	}
}

fn (mut t Transformer) erase_generic_sum_templates(sum_decls map[u32]GenericSumDecl) {
	for decl_id, decl in sum_decls {
		t.tc.sum_types.delete(decl_id)
		t.sum_types.delete(decl_id)
		t.tc.sum_generic_params.delete(decl_id)
		t.tc.unregister_short_type_name(decl_id)
		if decl.key != decl.node.value {
			t.tc.sum_generic_params.delete(flat.source_type_id(decl.node.value))
		}
	}
	// Counts can return to a previous value after later declarations are added.
	// Neither prepared indexes nor fallback caches may retain the removed keys.
	t.build_generic_alias_name_index()
}

fn (mut t Transformer) collect_generic_materialization_specs(decls map[u32]GenericStructDecl, sum_decls map[u32]GenericSumDecl) (map[u32]u32, map[u32]GenericSpecContext) {
	mut specs := map[u32]u32{}
	mut sum_specs := map[u32]GenericSpecContext{}
	t.collect_generic_struct_specs(decls, mut specs)
	t.collect_generic_sum_specs(sum_decls, mut sum_specs)
	t.expand_generic_materialization_specs(decls, sum_decls, mut specs, mut sum_specs)
	return specs, sum_specs
}

fn (mut t Transformer) expand_generic_materialization_specs(decls map[u32]GenericStructDecl, sum_decls map[u32]GenericSumDecl, mut specs map[u32]u32, mut sum_specs map[u32]GenericSpecContext) {
	for _ in 0 .. 40 {
		before := specs.len
		before_sums := sum_specs.len
		current := specs.clone()
		for spec_id, base_id in current {
			decl := decls[base_id] or { continue }
			syntax := flat.source_type_syntax(spec_id)
			args := syntax.generic_app_args()
			ok := syntax.generic_app_valid()
			if !ok {
				continue
			}
			for i in 0 .. decl.node.children_count {
				field := t.a.child_node(&decl.node, i)
				if field.kind != .field_decl {
					continue
				}
				field_type := substitute_generic_type_text_with_params(field.typ, args, decl.node.generic_params())
				t.collect_generic_struct_spec_from_type(field_type, decl.module, decl.file, decls, mut specs)
				t.collect_generic_sum_spec_from_type(field_type, decl.module, decl.file, sum_decls, mut sum_specs)
			}
		}
		current_sums := sum_specs.clone()
		for spec, context in current_sums {
			decl := sum_decls[context.base] or { continue }
			syntax := flat.source_type_syntax(spec)
			args := syntax.generic_app_args()
			ok := syntax.generic_app_valid()
			if !ok {
				continue
			}
			for i in 0 .. decl.node.children_count {
				variant := t.a.child_node(&decl.node, i)
				variant_type := substitute_generic_type_text_with_params(variant.source_value_id(), args, decl.node.generic_params())
				t.collect_generic_struct_spec_from_type(variant_type, decl.module, decl.file, decls, mut specs)
				t.collect_generic_sum_spec_from_type(variant_type, decl.module, decl.file, sum_decls, mut sum_specs)
			}
		}
		if specs.len == before && sum_specs.len == before_sums {
			break
		}
	}
}

// declaration_scan_prefix reuses the checker's declaration index for its
// original AST prefix. Declarations appended by lowering come from
// appended_declaration_scan.
fn (t &Transformer) declaration_scan_prefix() (int, int) {
	if !isnil(t.tc) && t.tc.top_level_idx.len > 0 && t.tc.top_level_idx_nodes_len > 0
		&& t.tc.top_level_idx_nodes_len <= t.a.nodes.len {
		return t.tc.top_level_idx.len, t.tc.top_level_idx_nodes_len
	}
	return 0, 0
}

@[direct_array_access]
fn (mut t Transformer) collect_generic_struct_decls() map[u32]GenericStructDecl {
	mut decls := map[u32]GenericStructDecl{}
	t.ensure_node_module_map()
	mut cur_file := ''
	mut cur_module := ''
	indexed_count, indexed_nodes := t.declaration_scan_prefix()
	appended, streamed := t.appended_declaration_scan(indexed_nodes)
	for scan_idx in 0 .. indexed_count + appended.len + streamed {
		i := if scan_idx < indexed_count {
			int(t.tc.top_level_idx[scan_idx])
		} else if scan_idx - indexed_count < appended.len {
			appended[scan_idx - indexed_count]
		} else {
			indexed_nodes + scan_idx - indexed_count - appended.len
		}
		kind := t.a.nodes[i].kind
		if kind != .file && kind != .module_decl && kind != .struct_decl {
			continue
		}
		// This scan does not modify the AST arena.
		node := unsafe { &t.a.nodes[i] }
		match node.kind {
			.file {
				cur_file = node.value_text()
				cur_module = t.tc.file_modules[cur_file] or { '' }
			}
			.module_decl {
				cur_module = node.value_text()
			}
			.struct_decl {
				if node.generic_params().len == 0 && flat.source_type_id('generic') !in flat.source_type_syntax(node.typ).plain_comma_parts() {
					continue
				}
				module_name := t.node_module_or(i, cur_module)
				key := generic_struct_decl_key(node.source_value_id(), module_name)
				decls[key] = GenericStructDecl{
					id:     flat.NodeId(i)
					node:   *node
					file:   cur_file
					module: module_name
					key:    flat.source_type_name(key)
				}
			}
			else {}
		}
	}
	return decls
}

fn generic_struct_decl_key(name_id u32, module_name string) u32 {
	if flat.source_type_shape(name_id).has(.dotted) || module_name == '' || module_name == 'main'
		|| module_name == 'builtin' {
		return name_id
	}
	return flat.source_type_composition_two(.qualified, flat.source_type_id(module_name), name_id)
}

@[direct_array_access]
fn (mut t Transformer) collect_generic_sum_decls() map[u32]GenericSumDecl {
	mut decls := map[u32]GenericSumDecl{}
	t.ensure_node_module_map()
	mut cur_file := ''
	mut cur_module := ''
	indexed_count, indexed_nodes := t.declaration_scan_prefix()
	appended, streamed := t.appended_declaration_scan(indexed_nodes)
	for scan_idx in 0 .. indexed_count + appended.len + streamed {
		i := if scan_idx < indexed_count {
			int(t.tc.top_level_idx[scan_idx])
		} else if scan_idx - indexed_count < appended.len {
			appended[scan_idx - indexed_count]
		} else {
			indexed_nodes + scan_idx - indexed_count - appended.len
		}
		kind := t.a.nodes[i].kind
		if kind != .file && kind != .module_decl && kind != .type_decl {
			continue
		}
		// This scan does not modify the AST arena.
		node := unsafe { &t.a.nodes[i] }
		match node.kind {
			.file {
				cur_file = node.value_text()
				cur_module = t.tc.file_modules[cur_file] or { '' }
			}
			.module_decl {
				cur_module = node.value_text()
			}
			.type_decl {
				if node.generic_params().len == 0 || node.children_count == 0 {
					continue
				}
				module_id := t.node_module_map_cache[i] or { u16(0) }
				module_name := if module_id != 0 {
					t.node_context_text(module_id)
				} else {
					cur_module
				}
				key := generic_type_decl_key(node.source_value_id(), module_name)
				decls[key] = GenericSumDecl{
					id:     flat.NodeId(i)
					node:   *node
					file:   cur_file
					module: module_name
					key:    flat.source_type_name(key)
				}
			}
			else {}
		}
	}
	return decls
}

fn generic_type_decl_key(name_id u32, module_name string) u32 {
	return generic_struct_decl_key(name_id, module_name)
}

fn (mut t Transformer) collect_generic_struct_specs(decls map[u32]GenericStructDecl, mut specs map[u32]u32) {
	t.ensure_node_module_map()
	if decls.len == 0 {
		// A spelling is recorded only as a specialization of one of `decls`.
		return
	}
	for _, target_id in t.tc.type_aliases.ids {
		t.collect_generic_struct_spec_from_type(target_id, '', '', decls, mut specs)
	}
	t.collect_generic_struct_specs_range(decls, mut specs, 0, t.a.nodes.len)
}

// type_text_has_unqualified_generic_arg reports whether `typ` applies generic
// arguments that are plain (unqualified) type names, which cannot be resolved
// without knowing the file the text was written in. Nested spellings are visited
// recursively and container spellings are normalized the same way
// `collect_generic_struct_spec_from_type` normalizes them, so
// `Map[string, []Context]` and `Box[[2]Context]` depend on the writing file while
// `Box[map[string]int]` is fully resolvable and must not be skipped.
fn type_text_has_unqualified_generic_arg(typ_id u32) bool {
	text_id := strip_type_modifier_prefixes_id(typ_id)
	if text_id == 0 { return false }
	shape := flat.source_type_shape(text_id)
	syntax := flat.source_type_syntax(text_id)
	if shape.has(.map_type) {
		if !syntax.bracket_closed() { return false }
		return type_text_has_unqualified_generic_arg(syntax.bracket_raw_inner())
			|| type_text_has_unqualified_generic_arg(syntax.raw_base())
	}
	if shape.has(.leading_bracket) {
		if !syntax.bracket_closed() || syntax.raw_base() == 0 { return false }
		return type_text_has_unqualified_generic_arg(syntax.raw_base())
	}
	if !shape.has(.bracketed) { return false }
	for arg_id in syntax.bracket_generic_parts() {
		clean_id := transform_trimmed_type_id(arg_id)
		if clean_id == 0 { continue }
		if type_text_has_unqualified_generic_arg(clean_id) { return true }
		payload_id := strip_type_modifier_prefixes_id(clean_id)
		payload := flat.source_type_syntax(payload_id)
		if payload.generic_app_valid()
			&& flat.source_type_syntax(payload.generic_app_base()).plain_type_name()
			&& !types.is_builtin_type_name_id(payload.generic_app_base()) {
			return true
		}
		if !payload.plain_type_name() || types.is_builtin_type_name_id(payload_id) { continue }
		return true
	}
	return false
}

// strip_type_modifier_prefixes_id follows the collector's raw wrapper children.
fn strip_type_modifier_prefixes_id(typ_id u32) u32 {
	mut text_id := transform_trimmed_type_id(typ_id)
	for text_id != 0 {
		shape := flat.source_type_shape(text_id)
		if shape.has(.pointer) || shape.has(.optional) || shape.has(.result_type)
			|| shape.has(.variadic) || shape.has(.mutable_type) || shape.has(.channel_type)
			|| shape.has(.array_type) {
			text_id = transform_trimmed_type_id(flat.source_type_syntax(text_id).raw_base())
			continue
		}
		if shape.has(.leading_bracket) {
			syntax := flat.source_type_syntax(text_id)
			if syntax.bracket_closed() && syntax.raw_base() != 0 {
				text_id = transform_trimmed_type_id(syntax.raw_base())
				continue
			}
		}
		break
	}
	return text_id
}

fn (mut t Transformer) collect_generic_struct_specs_range(decls map[u32]GenericStructDecl, mut specs map[u32]u32, start int, end int) {
	t.ensure_node_module_map()
	if decls.len == 0 {
		// A spelling is recorded only as a specialization of one of `decls`.
		return
	}
	safe_start := if start < 0 { 0 } else { start }
	safe_end := if end > t.a.nodes.len { t.a.nodes.len } else { end }
	mut type_context_module := ''
	mut type_context_file := ''
	mut seen_context_types := map[u32]bool{}
	// Shared source type IDs keep the byte-test result without spelling lookups.
	// Node annotations do not change during this loop.
	mut tested_texts := []u32{len: 4096}
	mut tested_results := []bool{len: 4096}
	for node_idx in safe_start .. safe_end {
		// The scan appends no node, and most nodes cannot spell a specialization:
		// only a candidate is copied.
		scanned := unsafe { &t.a.nodes[node_idx] }
		mut may_contain_spec := false
		if scanned.typ != 0 {
			text := scanned.typ
			slot := int(text & 4095)
			if tested_texts[slot] == text {
				may_contain_spec = tested_results[slot]
			} else {
				may_contain_spec = type_text_has_bracket_or_underscore(scanned.typ)
				tested_texts[slot] = text
				tested_results[slot] = may_contain_spec
			}
		}
		if !may_contain_spec && scanned.kind !in [.sql_expr, .struct_init, .array_init, .map_init,
			.cast_expr, .as_expr, .sizeof_expr, .offsetof_expr, .typeof_expr, .is_expr] {
			continue
		}
		node := *scanned
		if !may_contain_spec {
			may_contain_spec = match node.kind {
				.sql_expr {
					true
				}
				.struct_init, .array_init, .map_init, .cast_expr, .as_expr, .sizeof_expr,
				.offsetof_expr, .typeof_expr,
				.is_expr {
					type_text_has_bracket_or_underscore(node.source_value_id())
						|| node.generic_params().len > 0
				}
				else {
					false
				}
			}
		}
		if !may_contain_spec {
			continue
		}
		node_module := t.node_module_or(node_idx, '')
		node_file := t.node_file_or(node_idx, '')
		// A synthesized node without recorded context cannot resolve bare type
		// arguments: collecting its spelling would leave e.g. `Middleware[Context]`
		// with no file scope, and materialization would later rebase the argument
		// onto the wrong module. Fully qualified arguments stay usable, and the
		// source node a spelling was cloned from is scanned with its own context.
		if node_module.len == 0 && node_file.len == 0 {
			mut spelling := node.typ
			params := node.generic_params()
			if params.len > 0 && !flat.source_type_shape(node.source_value_id()).has(.bracketed) {
				// collect_generic_struct_specs_from_node joins these into a
				// `Base[Arg]` spelling of its own, so check them here too.
				spelling = node_generic_app_type_id(node)
			}
			if type_text_has_unqualified_generic_arg(spelling)
				|| type_text_has_unqualified_generic_arg(node.source_value_id()) {
				continue
			}
		}
		if node_module != type_context_module || node_file != type_context_file {
			type_context_module = node_module
			type_context_file = node_file
			seen_context_types.clear()
		}
		scan_node_type := node.typ != 0 && node.typ !in seen_context_types
		if scan_node_type {
			seen_context_types[node.typ] = true
		}
		t.collect_generic_struct_specs_from_node(node, node_module, node_file, scan_node_type, decls, mut specs)
	}
}

// type_text_has_bracket_or_underscore uses cached spelling metadata to detect
// either delimiter a generic specialization needs.
@[direct_array_access]
fn type_text_has_bracket_or_underscore(typ_id u32) bool {
	return flat.source_type_shape(typ_id).has(.bracketed)
		|| flat.source_type_syntax(typ_id).underscored()
}

// comma_list_has_item reports whether `item` is one of the comma-separated
// fields of `list`, as `item in list.split(',')` does, without splitting it.
fn comma_list_has_item(list string, item string) bool {
	mut start := 0
	for start <= list.len {
		mut end := start
		for end < list.len && list[end] != `,` {
			end++
		}
		if end - start == item.len && unsafe { vmemcmp(list.str + start, item.str, item.len) } == 0 {
			return true
		}
		start = end + 1
	}
	return false
}

fn node_generic_app_type_id(node flat.Node) u32 {
	params := node.generic_params()
	mut parts := []u32{cap: params.len + 1}
	parts << node.source_value_id()
	for param in params {
		parts << flat.source_type_id(param)
	}
	return flat.source_type_composition(.generic, parts)
}

fn (mut t Transformer) collect_generic_struct_specs_from_node(node flat.Node, module_name string, file_name string, scan_node_type bool, decls map[u32]GenericStructDecl, mut specs map[u32]u32) {
	if scan_node_type {
		t.collect_generic_struct_spec_from_type(node.typ, module_name, file_name, decls, mut specs)
	}
	match node.kind {
		.sql_expr {
			t.collect_generic_struct_specs_from_sql_expr(node.value_text(), module_name, file_name, decls, mut specs)
		}
		.struct_init, .array_init, .map_init, .cast_expr, .as_expr, .sizeof_expr, .offsetof_expr,
		.typeof_expr, .is_expr {
			t.collect_generic_struct_spec_from_type(node.source_value_id(), module_name, file_name, decls, mut specs)
			if !flat.source_type_shape(node.source_value_id()).has(.bracketed) && node.generic_params().len > 0 {
				t.collect_generic_struct_spec_from_type(node_generic_app_type_id(node), module_name, file_name, decls, mut specs)
			}
		}
		else {}
	}
}

fn (mut t Transformer) collect_generic_struct_specs_from_sql_expr(value string, module_name string, file_name string, decls map[u32]GenericStructDecl, mut specs map[u32]u32) {
	tokens := sql_clean_tokens(value.split(' '))
	mut i := 0
	for i < tokens.len {
		if i + 2 < tokens.len && tokens[i] in ['create', 'drop'] && tokens[i + 1] == 'table' {
			table_token, next_idx := sql_table_type_token_from(tokens, i + 2) or {
				i++
				continue
			}
			t.collect_generic_struct_spec_from_type(flat.source_type_id(table_token), module_name, file_name, decls, mut specs)
			i = next_idx
			continue
		}
		if i + 3 < tokens.len && tokens[i] in ['insert', 'upsert'] && tokens[i + 2] == 'into' {
			table_token, next_idx := sql_table_type_token_from(tokens, i + 3) or {
				i++
				continue
			}
			t.collect_generic_struct_spec_from_type(flat.source_type_id(table_token), module_name, file_name, decls, mut specs)
			i = next_idx
			continue
		}
		if tokens[i] == 'select'
			|| (i + 1 < tokens.len && tokens[i] == 'dynamic' && tokens[i + 1] == 'select') {
			is_dynamic := tokens[i] == 'dynamic'
			start := if is_dynamic { i + 1 } else { i }
			end := sql_stmt_end(tokens, if is_dynamic { i + 2 } else { i + 1 })
			stmt_tokens := tokens[start..end]
			from_idx := sql_token_index(stmt_tokens, 'from')
			if from_idx >= 0 && from_idx + 1 < stmt_tokens.len {
				if table_token, _ := sql_table_type_token_from(stmt_tokens, from_idx + 1) {
					t.collect_generic_struct_spec_from_type(flat.source_type_id(table_token), module_name, file_name, decls, mut specs)
				}
			}
			i = end
			continue
		}
		if i + 2 < tokens.len && tokens[i] == 'delete' && tokens[i + 1] == 'from' {
			end := sql_stmt_end(tokens, i + 1)
			stmt_tokens := tokens[i..end]
			if table_token, _ := sql_table_type_token_from(stmt_tokens, 2) {
				t.collect_generic_struct_spec_from_type(flat.source_type_id(table_token), module_name, file_name, decls, mut specs)
			}
			i = end
			continue
		}
		if tokens[i] == 'update'
			|| (i + 1 < tokens.len && tokens[i] == 'dynamic' && tokens[i + 1] == 'update') {
			is_dynamic := tokens[i] == 'dynamic'
			start := if is_dynamic { i + 1 } else { i }
			end := sql_stmt_end(tokens, if is_dynamic { i + 2 } else { i + 1 })
			stmt_tokens := tokens[start..end]
			if table_token, _ := sql_table_type_token_from(stmt_tokens, 1) {
				t.collect_generic_struct_spec_from_type(flat.source_type_id(table_token), module_name, file_name, decls, mut specs)
			}
			i = end
			continue
		}
		i++
	}
}

fn (mut t Transformer) collect_generic_struct_spec_from_type(typ_id u32, module_name string, file_name string, decls map[u32]GenericStructDecl, mut specs map[u32]u32) {
	if typ_id == 0 { return }
	if !flat.source_type_shape(typ_id).has(.bracketed)
		&& !flat.source_type_syntax(typ_id).underscored() {
		return
	}
	clean_id := transform_trimmed_type_id(typ_id)
	if clean_id == 0 { return }
	shape := flat.source_type_shape(clean_id)
	syntax := flat.source_type_syntax(clean_id)
	if shape.has(.pointer) || shape.has(.mutable_type) || shape.has(.optional)
		|| shape.has(.variadic) || shape.has(.array_type) || shape.has(.channel_type) {
		t.collect_generic_struct_spec_from_type(syntax.raw_base(), module_name, file_name, decls, mut specs)
		return
	}
	if shape.has(.map_type) {
		if syntax.bracket_closed() {
			t.collect_generic_struct_spec_from_type(syntax.bracket_raw_inner(), module_name, file_name, decls, mut specs)
			t.collect_generic_struct_spec_from_type(syntax.raw_base(), module_name, file_name, decls, mut specs)
		}
		return
	}
	if shape.has(.leading_bracket) {
		if syntax.bracket_closed() {
			t.collect_generic_struct_spec_from_type(syntax.raw_base(), module_name, file_name, decls, mut specs)
		}
		return
	}
	if !shape.has(.bracketed) {
		// A flattened concrete generic name always includes the separator before
		// its type suffix. Ordinary scalar and qualified names cannot be specs.
		if !syntax.underscored() { return }
		args_source_ids := t.recorded_type_specialization_args(clean_id) or { return }
		spec_base := t.generic_struct_spec_base_from_flattened_type(clean_id, args_source_ids, module_name, file_name, decls) or { return }
		scoped_args_source_ids := t.generic_struct_args_in_scope(args_source_ids, module_name, file_name)
		spec_base_id := spec_base
		mut parts := []u32{cap: scoped_args_source_ids.len + 1}
		parts << spec_base_id
		parts << scoped_args_source_ids
		specs[flat.source_type_composition(.generic, parts)] = spec_base_id
		spec_module := if decl := decls[spec_base] { decl.module } else { module_name }
		t.record_generic_specialization_args_in_module(flat.source_type_name(spec_base), spec_module, scoped_args_source_ids)
		return
	}
	if !syntax.generic_app_valid() { return }
	base_id := syntax.generic_app_base()
	args_source_ids := t.canonical_generic_specialization_args(syntax.generic_app_args())
	for arg_source_id in args_source_ids {
		t.collect_generic_struct_spec_from_type(arg_source_id, module_name, file_name, decls, mut specs)
	}
	if t.generic_args_have_placeholders(args_source_ids) { return }
	alias_name_id := t.generic_qualified_alias_name(base_id, module_name)
	if target_source_id := t.tc.type_aliases.type_id(alias_name_id) {
		params := t.tc.type_alias_generic_params[alias_name_id] or { []string{} }
		if params.len == args_source_ids.len {
			alias_module := if flat.source_type_shape(alias_name_id).has(.dotted) {
				flat.source_type_name(flat.source_type_syntax(alias_name_id).qualifier())
			} else {
				module_name
			}
			expanded := substitute_generic_type_text_with_params(target_source_id, args_source_ids, params)
			normalized_source_id := t.normalize_type_in_module(expanded, alias_module)
			if normalized_source_id != clean_id {
				t.collect_generic_struct_spec_from_type(normalized_source_id, alias_module, file_name, decls, mut specs)
				return
			}
		}
	}
	spec_base := t.generic_struct_spec_base_name(base_id, module_name, file_name, decls) or { return }
	scoped_args_source_ids := t.generic_struct_args_in_scope(args_source_ids, module_name, file_name)
	spec_base_id := spec_base
	mut parts := []u32{cap: scoped_args_source_ids.len + 1}
	parts << spec_base_id
	parts << scoped_args_source_ids
	specs[flat.source_type_composition(.generic, parts)] = spec_base_id
	spec_module := if decl := decls[spec_base] { decl.module } else { module_name }
	t.record_generic_specialization_args_in_module(flat.source_type_name(spec_base), spec_module, scoped_args_source_ids)
}

fn (mut t Transformer) generic_struct_spec_base_from_flattened_type(typ_id u32, args_ids []u32, module_name string, file_name string, decls map[u32]GenericStructDecl) ?u32 {
	if args_ids.len == 0 {
		return none
	}
	clean_id := transform_trimmed_type_id(typ_id)
	clean_c := c_name(flat.source_type_name(clean_id))
	mut found := u32(0)
	for base_id, decl in decls {
		scoped_args := t.generic_struct_args_in_scope(args_ids, module_name, file_name)
		mut parts := []u32{cap: scoped_args.len + 1}
		parts << base_id
		parts << scoped_args
		bracket_name := c_name(flat.source_type_name(flat.source_type_composition(.generic, parts)))
		receiver_name := c_name('${flat.source_type_name(base_id)}_${generic_type_suffixes(scoped_args)}')
		if clean_c !in [bracket_name, receiver_name] {
			continue
		}
		if found != 0 && found != base_id {
			return none
		}
		found = if resolved := t.generic_struct_spec_base_name(base_id, decl.module, decl.file, decls) {
			resolved
		} else {
			base_id
		}
	}
	if found != 0 {
		return found
	}
	return none
}

fn (mut t Transformer) collect_generic_sum_specs(decls map[u32]GenericSumDecl, mut specs map[u32]GenericSpecContext) {
	t.ensure_node_module_map()
	for _, target_id in t.tc.type_aliases.ids {
		t.collect_generic_sum_spec_from_type(target_id, '', '', decls, mut specs)
	}
	t.collect_generic_sum_specs_range(decls, mut specs, 0, t.a.nodes.len)
}

fn (mut t Transformer) collect_generic_sum_specs_range(decls map[u32]GenericSumDecl, mut specs map[u32]GenericSpecContext, start int, end int) {
	if decls.len == 0 {
		return
	}
	t.ensure_node_module_map()
	safe_start := if start < 0 { 0 } else { start }
	safe_end := if end > t.a.nodes.len { t.a.nodes.len } else { end }
	for node_idx in safe_start .. safe_end {
		node := t.a.nodes[node_idx]
		node_module := t.node_module_or(node_idx, '')
		node_file := t.node_file_or(node_idx, '')
		t.collect_generic_sum_specs_from_node(node, node_module, node_file, decls, mut specs)
	}
}

fn (mut t Transformer) collect_generic_sum_specs_from_node(node flat.Node, module_name string, file_name string, decls map[u32]GenericSumDecl, mut specs map[u32]GenericSpecContext) {
	if decls.len == 0 {
		return
	}
	if node.typ != 0 {
		t.collect_generic_sum_spec_from_type(node.typ, module_name, file_name, decls, mut specs)
	}
	match node.kind {
		.struct_init, .array_init, .map_init, .cast_expr, .as_expr, .sizeof_expr, .typeof_expr,
		.is_expr {
			t.collect_generic_sum_spec_from_type(node.source_value_id(), module_name, file_name, decls, mut specs)
			if !flat.source_type_shape(node.source_value_id()).has(.bracketed) && node.generic_params().len > 0 {
				t.collect_generic_sum_spec_from_type(node_generic_app_type_id(node), module_name, file_name, decls, mut specs)
			}
		}
		else {}
	}
}

fn (mut t Transformer) collect_generic_sum_spec_from_type(typ_id u32, module_name string, file_name string, decls map[u32]GenericSumDecl, mut specs map[u32]GenericSpecContext) {
	if typ_id == 0 || !flat.source_type_shape(typ_id).has(.bracketed) { return }
	clean_id := transform_trimmed_type_id(typ_id)
	if clean_id == 0 { return }
	shape := flat.source_type_shape(clean_id)
	syntax := flat.source_type_syntax(clean_id)
	if shape.has(.pointer) || shape.has(.mutable_type) || shape.has(.optional)
		|| shape.has(.variadic) || shape.has(.array_type) || shape.has(.channel_type) {
		t.collect_generic_sum_spec_from_type(syntax.raw_base(), module_name, file_name, decls, mut specs)
		return
	}
	if shape.has(.map_type) {
		if syntax.bracket_closed() {
			t.collect_generic_sum_spec_from_type(syntax.bracket_raw_inner(), module_name, file_name, decls, mut specs)
			t.collect_generic_sum_spec_from_type(syntax.raw_base(), module_name, file_name, decls, mut specs)
		}
		return
	}
	if shape.has(.leading_bracket) {
		if syntax.bracket_closed() {
			t.collect_generic_sum_spec_from_type(syntax.raw_base(), module_name, file_name, decls, mut specs)
		}
		return
	}
	if !shape.has(.bracketed) || !syntax.generic_app_valid() { return }
	for arg in syntax.generic_app_args() {
		t.collect_generic_sum_spec_from_type(arg, module_name, file_name, decls, mut specs)
	}
	if t.generic_args_have_placeholders(syntax.generic_app_args()) { return }
	// A source node that its lowering detached no longer maps to a file. Its spelling
	// (`maybe.Maybe[Token]` for `import iam { Token }` next to another imported `Token`)
	// then names no type, and keying a specialization with it would emit a bogus one.
	if file_name == '' && t.generic_args_name_unknown_type(syntax.generic_app_args()) { return }
	spec_base := t.generic_sum_spec_base_name(syntax.generic_app_base(), module_name, file_name, decls) or { return }
	spec_name := t.generic_sum_spec_name_in_scope(spec_base, syntax.generic_app_args(), module_name, file_name)
	specs[spec_name] = GenericSpecContext{
		base:   spec_base
		file:   file_name
		module: module_name
	}
}

fn (t &Transformer) generic_struct_spec_base_name(base_id u32, module_name string, file_name string, decls map[u32]GenericStructDecl) ?u32 {
	if base_id in decls {
		return base_id
	}
	syntax := flat.source_type_syntax(base_id)
	if syntax.first_dot_prefix() == flat.source_type_id('main') && flat.source_type_shape(base_id).has(.dotted)
		&& !flat.source_type_shape(syntax.first_dot_suffix()).has(.dotted) {
		main_base := syntax.first_dot_suffix()
		if main_base in decls {
			return main_base
		}
	}
	if flat.source_type_shape(base_id).has(.dotted) {
		return none
	}
	if module_name != '' && module_name != 'main' && module_name != 'builtin' {
		qbase := flat.source_type_composition_two(.qualified, flat.source_type_id(module_name), base_id)
		if qbase in decls {
			return qbase
		}
	}
	if resolved := t.selective_import_generic_struct_base(base_id, file_name, decls) {
		return resolved
	}
	mut found := u32(0)
	for key, _ in decls {
		if flat.source_type_syntax(key).unqualified() == base_id {
			if found != 0 && found != key {
				return none
			}
			found = key
		}
	}
	if found != 0 {
		return found
	}
	return none
}

fn (t &Transformer) generic_sum_spec_base_name(base_id u32, module_name string, file_name string, decls map[u32]GenericSumDecl) ?u32 {
	if base_id in decls {
		return base_id
	}
	syntax := flat.source_type_syntax(base_id)
	if syntax.first_dot_prefix() == flat.source_type_id('main') && flat.source_type_shape(base_id).has(.dotted)
		&& !flat.source_type_shape(syntax.first_dot_suffix()).has(.dotted) {
		main_base := syntax.first_dot_suffix()
		if main_base in decls {
			return main_base
		}
	}
	if flat.source_type_shape(base_id).has(.dotted) {
		return none
	}
	if module_name != '' && module_name != 'main' && module_name != 'builtin' {
		qbase := flat.source_type_composition_two(.qualified, flat.source_type_id(module_name), base_id)
		if qbase in decls {
			return qbase
		}
	}
	if resolved := t.selective_import_generic_sum_base(base_id, file_name, decls) {
		return resolved
	}
	mut found := u32(0)
	for key, _ in decls {
		if flat.source_type_syntax(key).unqualified() == base_id {
			if found != 0 && found != key {
				return none
			}
			found = key
		}
	}
	if found != 0 {
		return found
	}
	return none
}

fn (mut t Transformer) generic_sum_spec_name_in_scope(base_id u32, args_ids []u32, module_name string, file_name string) u32 {
	scoped_args := t.generic_sum_args_in_scope(args_ids, module_name, file_name)
	mut parts := []u32{cap: scoped_args.len + 1}
	parts << base_id
	parts << scoped_args
	return flat.source_type_composition(.generic, parts)
}

fn (mut t Transformer) generic_struct_args_in_scope(args_ids []u32, module_name string, file_name string) []u32 {
	if isnil(t.tc) {
		return args_ids.clone()
	}
	old_module := t.tc.cur_module
	old_file := t.tc.cur_file
	t.tc.cur_module = module_name
	t.tc.cur_file = file_name
	mut scoped := []u32{cap: args_ids.len}
	for arg_source_id in args_ids {
		arg_source_id_4 := arg_source_id

		parsed := t.tc.parse_resolution_type(arg_source_id)
		result := if parsed is types.Unknown {
			if resolved_source_id := t.selective_import_type_name_for_file(file_name, arg_source_id_4) {
				resolved_source_id
			} else {
				t.normalize_sum_variant_type(arg_source_id_4, module_name, [])
			}
		} else if parsed is types.Alias && parsed.base_type is types.ArrayFixed {
			// The base is not itself an alias here, so unalias_type yields the alias payload's
			// base type. Spelled this way because the vc bootstrap compiler takes the address of
			// the embedded `&Type` payload field for a `Type(ptr)` cast receiver.
			types.unalias_type(parsed).source_type_id()
		} else {
			// A bare spelling that resolves inside the declaring module still has to
			// honor the writing file's selective imports: `import model { Context }`
			// makes `Context` mean `model.Context`, not a same-named type from an
			// imported module or the declaring module itself.
			name := parsed.source_type_id()
			if !flat.source_type_shape(name).has(.dotted) {
				if resolved_source_id := t.selective_import_type_name_for_file(file_name, name) {
					scoped << resolved_source_id
					continue
				}
			}
			name
		}
		scoped << result
	}
	t.tc.cur_module = old_module
	t.tc.cur_file = old_file
	return scoped.clone()
}

// generic_args_name_unknown_type reports whether a bare generic argument names no
// declared type when it is read without any file's imports.
fn (t &Transformer) generic_args_name_unknown_type(args_ids []u32) bool {
	if isnil(t.tc) { return false }
	for arg_id in args_ids {
		name_id := transform_trimmed_type_id(arg_id)
		shape := flat.source_type_shape(name_id)
		if name_id == 0 || shape.has(.dotted) || shape.has(.bracketed)
			|| !flat.source_type_syntax(name_id).first_capital() {
			continue
		}
		if name_id !in t.tc.structs && !t.tc.sum_types.contains(name_id) && name_id !in t.tc.enum_names
			&& name_id !in t.tc.flag_enums && name_id !in t.tc.interface_names
			&& !t.tc.type_aliases.contains(name_id) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) generic_sum_args_in_scope(args_ids []u32, module_name string, file_name string) []u32 {
	if isnil(t.tc) {
		return args_ids.clone()
	}
	old_module := t.tc.cur_module
	old_file := t.tc.cur_file
	t.tc.cur_module = module_name
	t.tc.cur_file = file_name
	mut scoped := []u32{cap: args_ids.len}
	for arg_source_id in args_ids {
		arg_source_id_5 := arg_source_id

		parsed := t.tc.parse_resolution_type(arg_source_id)
		result := if parsed is types.Unknown {
			if resolved_source_id := t.selective_import_type_name_for_file(file_name, arg_source_id_5) {
				resolved_source_id
			} else {
				t.normalize_sum_variant_type(arg_source_id_5, module_name, [])
			}
		} else {
			// A bare spelling that resolves inside the declaring module still has to
			// honor the writing file's selective imports: `import iam { Token }`
			// makes `Token` mean `iam.Token`, not a same-named type from another
			// imported module or the declaring module itself.
			name := parsed.source_type_id()
			if !flat.source_type_shape(name).has(.dotted) {
				if resolved_source_id := t.selective_import_type_name_for_file(file_name, name) {
					scoped << resolved_source_id
					continue
				}
			}
			name
		}
		scoped << result
	}
	t.tc.cur_module = old_module
	t.tc.cur_file = old_file
	return scoped.clone()
}

fn (t &Transformer) selective_import_generic_struct_base(base_id u32, file_name string, decls map[u32]GenericStructDecl) ?u32 {
	if isnil(t.tc) || flat.source_type_shape(base_id).has(.dotted) || file_name == '' {
		return none
	}
	candidates := t.tc.file_selective_imports[file_import_key(file_name, flat.source_type_name(base_id))] or { return none }
	for candidate in candidates {
		candidate_id := flat.source_type_existing_id(candidate) or { continue }
		if candidate_id in decls {
			return candidate_id
		}
	}
	return none
}

fn (t &Transformer) selective_import_generic_sum_base(base_id u32, file_name string, decls map[u32]GenericSumDecl) ?u32 {
	if isnil(t.tc) || flat.source_type_shape(base_id).has(.dotted) || file_name == '' {
		return none
	}
	candidates := t.tc.file_selective_imports[file_import_key(file_name, flat.source_type_name(base_id))] or { return none }
	for candidate in candidates {
		candidate_id := flat.source_type_existing_id(candidate) or { continue }
		if candidate_id in decls {
			return candidate_id
		}
	}
	return none
}

fn (mut t Transformer) materialize_generic_sum_spec(spec_id u32, decl GenericSumDecl, context GenericSpecContext) {
	syntax := flat.source_type_syntax(spec_id)
	if !syntax.generic_app_valid() || syntax.generic_app_args().len == 0 {
		return
	}
	if variants_source_ids := t.tc.sum_types.type_ids(spec_id) {
		t.sum_types.set_ids(spec_id, variants_source_ids)
		return
	}
	if variants_source_ids := t.sum_types.type_ids(spec_id) {
		t.tc.sum_types.set_ids(spec_id, variants_source_ids)
		t.tc.register_short_type_name(spec_id)
		return
	}
	old_module := t.cur_module
	old_file := t.cur_file
	old_tc_module := t.tc.cur_module
	old_tc_file := t.tc.cur_file
	scoped_args_source_ids := t.generic_sum_args_in_scope(syntax.generic_app_args(), context.module, context.file)

	t.cur_module = decl.module
	t.cur_file = decl.file
	t.tc.cur_module = decl.module
	t.tc.cur_file = decl.file
	mut variants := []u32{}
	mut seen_variants := map[u32]bool{}
	for i in 0 .. decl.node.children_count {
		variant := t.a.child_node(&decl.node, i)
		variant_type := substitute_generic_type_text_with_params(variant.source_value_id(), scoped_args_source_ids, decl.node.generic_params())
		parsed := t.tc.parse_resolution_type(variant_type)
		resolved_variant := if parsed is types.Unknown {
			variant_type
		} else {
			parsed.source_type_id()
		}
		if seen_variants[resolved_variant] {
			continue
		}
		seen_variants[resolved_variant] = true
		variants << resolved_variant
	}
	t.tc.sum_types.set_ids(spec_id, variants)
	t.sum_types.set_ids(spec_id, variants)
	t.tc.register_short_type_name(spec_id)
	if !isnil(t.sum_cache) {
		t.sum_cache.entries.clear()
		t.sum_cache.clear_recent()
	}
	t.cur_module = old_module
	t.cur_file = old_file
	t.tc.cur_module = old_tc_module
	t.tc.cur_file = old_tc_file
}

fn (mut t Transformer) materialize_generic_struct_spec(spec_id u32, decl GenericStructDecl) {
	syntax := flat.source_type_syntax(spec_id)
	if !syntax.generic_app_valid() || syntax.generic_app_args().len == 0 {
		return
	}
	spec_name := flat.source_type_name(spec_id)
	args_ids := syntax.generic_app_args()
	t.record_generic_specialization_args_in_module(flat.source_type_name(syntax.generic_app_base()), decl.module, args_ids)
	old_module := t.cur_module
	old_file := t.cur_file
	old_tc_module := t.tc.cur_module
	old_tc_file := t.tc.cur_file
	t.cur_module = decl.module
	t.cur_file = decl.file
	t.tc.cur_module = decl.module
	t.tc.cur_file = decl.file
	mut old_specialization_main_types := t.active_specialization_main_types.clone()
	t.active_specialization_main_types = t.specialization_main_type_closure(args_ids)
	mut fields := []types.StructField{}
	for i in 0 .. decl.node.children_count {
		field := t.a.child_node(&decl.node, i)
		if field.kind != .field_decl {
			continue
		}
		field_type := substitute_generic_type_text_with_params(field.typ, args_ids, decl.node.generic_params())
		locked_field_type := t.lock_colliding_main_substitution_type_text(field.typ, field_type, decl.module, decl.node.generic_params())
		// A field declared directly as its type parameter keeps the concrete
		// argument's identity (and therefore alias methods/operators). Composite
		// declarations still need normalization to expand aliases such as `Values[T]`.
		field_type_id := transform_trimmed_type_id(field.typ)
		mut direct_parameter := false
		for param in decl.node.generic_params() {
			if field_type_id == flat.source_type_id(param) {
				direct_parameter = true
				break
			}
		}
		normalized_field_type := (if direct_parameter {
			locked_field_type
		} else {
			t.normalize_type_alias(locked_field_type)
		})
		fields << types.StructField{
			name:        field.value
			typ:         t.tc.parse_resolution_type(normalized_field_type)
			has_default: field.children_count > 0
			is_embed:    field.typ == 0 || field.source_value_id() == 0 || field.source_value_id() == field.typ
			is_mut:      field.is_mut
			is_volatile: types.source_field_decl_is_volatile(field)
		}
	}
	t.tc.structs[spec_id] = fields
	t.tc.register_short_type_name(spec_id)
	cspec_name := c_name(spec_name)
	if cspec_name != spec_name {
		cspec_id := flat.source_type_id(cspec_name)
		t.tc.structs[cspec_id] = fields
		t.tc.register_short_type_name(cspec_id)
		if decl.module.len > 0 {
			t.tc.struct_modules[flat.source_type_id(cspec_name)] = decl.module
		}
	}
	if flat.source_type_id(decl.key) in t.tc.unions {
		t.tc.unions[spec_id] = true
		if cspec_name != spec_name {
			t.tc.unions[flat.source_type_id(cspec_name)] = true
		}
	}
	if flat.source_type_id(decl.key) in t.tc.params_structs {
		t.tc.params_structs[spec_id] = true
		if cspec_name != spec_name {
			t.tc.params_structs[flat.source_type_id(cspec_name)] = true
		}
	}
	t.active_specialization_main_types = old_specialization_main_types.move()
	t.cur_module = old_module
	t.cur_file = old_file
	t.tc.cur_module = old_tc_module
	t.tc.cur_file = old_tc_file
}

fn (mut t Transformer) collect_generic_fn_decls() map[string]GenericFnDecl {
	mut decls := map[string]GenericFnDecl{}
	t.ensure_node_module_map()
	mut cur_file := ''
	mut cur_module := ''
	indexed_count, indexed_nodes := t.top_level_scan_prefix()
	appended, streamed := t.appended_declaration_scan(indexed_nodes)
	for scan_idx in 0 .. indexed_count + appended.len + streamed {
		i := if scan_idx < indexed_count {
			int(t.tc.top_level_idx[scan_idx])
		} else if scan_idx - indexed_count < appended.len {
			appended[scan_idx - indexed_count]
		} else {
			indexed_nodes + scan_idx - indexed_count - appended.len
		}
		kind := t.a.nodes[i].kind
		if kind != .file && kind != .module_decl && kind != .fn_decl {
			continue
		}
		// This scan does not modify the AST arena.
		node := unsafe { &t.a.nodes[i] }
		match node.kind {
			.file {
				cur_file = node.value_text()
				cur_module = t.tc.file_modules[cur_file] or { '' }
			}
			.module_decl {
				cur_module = node.value_text()
			}
			.fn_decl {
				fn_module := t.node_module_or(i, cur_module)
				if !t.fn_decl_has_unresolved_generics(*node, fn_module) {
					continue
				}
				key := t.generic_fn_decl_key(*node, fn_module)
				decls[key] = GenericFnDecl{
					id:     flat.NodeId(i)
					node:   *node
					file:   cur_file
					module: fn_module
					key:    key
				}
			}
			else {}
		}
	}
	return decls
}

// IgnoredFn is a function monomorphize_ignored_nodes flagged as unused, with the
// module it is declared in.
struct IgnoredFn {
	id     int
	module string
}

// IgnoredNodesState is what the last monomorphize_ignored_nodes call flagged.
// The used set only grows, so a function it flagged stays unused until the used
// set gains one of the spellings should_transform_fn_in_module tests for it.
struct IgnoredNodesState {
mut:
	ready       bool
	nodes       []bool
	node_count  int
	log_start   int
	scan_module string
	// by_spelling lists the flagged functions under each of those spellings.
	by_spelling map[string][]IgnoredFn
}

@[direct_array_access]
fn (mut t Transformer) monomorphize_ignored_nodes(decls map[string]GenericFnDecl) []bool {
	if t.ignored_state.ready && t.used_fns_log_active && t.has_used_fn_filter() {
		return t.monomorphize_ignored_nodes_since(decls)
	}
	mut nodes := []bool{len: t.a.nodes.len}
	mut stack := []flat.NodeId{cap: 256}
	for _, decl in decls {
		t.collect_node_subtree_flags(decl.id, mut nodes, mut stack)
	}
	if !t.has_used_fn_filter() {
		return nodes
	}
	// Later calls start from this result while every growth of the used set is
	// logged (see monomorphize_ignored_nodes_since).
	keep := t.used_fns_log_active
	mut state := IgnoredNodesState{
		log_start: t.used_fns_log.len
	}
	old_module := t.cur_module
	t.cur_module = ''
	indexed_count, indexed_nodes := t.top_level_scan_prefix()
	appended, streamed := t.appended_declaration_scan(indexed_nodes)
	for scan_idx in 0 .. indexed_count + appended.len + streamed {
		i := if scan_idx < indexed_count {
			int(t.tc.top_level_idx[scan_idx])
		} else if scan_idx - indexed_count < appended.len {
			appended[scan_idx - indexed_count]
		} else {
			indexed_nodes + scan_idx - indexed_count - appended.len
		}
		kind := t.a.nodes[i].kind
		if kind != .module_decl && kind != .fn_decl {
			continue
		}
		// This scan does not modify the AST arena.
		node := unsafe { &t.a.nodes[i] }
		match node.kind {
			.module_decl {
				t.cur_module = node.value_text()
			}
			.fn_decl {
				if node.value_text().starts_with('test_') {
					continue
				}
				t.cur_module = t.node_module_or(i, t.cur_module)
				if !t.should_transform_fn(*node) {
					t.collect_node_subtree_flags(flat.NodeId(i), mut nodes, mut stack)
					// Generic templates stay ignored even when a concrete instance becomes used.
					if keep && !t.fn_decl_has_unresolved_generics(*node, t.cur_module) {
						state.add(node.value_text(), i, t.cur_module)
					}
				}
			}
			else {}
		}
	}
	if keep {
		state.ready = true
		state.nodes = nodes
		state.node_count = t.a.nodes.len
		state.scan_module = t.cur_module
		t.ignored_state = state
	}
	t.cur_module = old_module
	return nodes
}

// monomorphize_ignored_nodes_since updates the last result: it clears the
// subtrees of the flagged functions that the used set named since, and flags the
// generic and unused declarations appended since. The previous result is left
// as it was; callers compare the two.
fn (mut t Transformer) monomorphize_ignored_nodes_since(decls map[string]GenericFnDecl) []bool {
	mut state := t.ignored_state
	mut nodes := state.nodes.clone()
	if nodes.len < t.a.nodes.len {
		nodes << []bool{len: t.a.nodes.len - nodes.len}
	}
	mut stack := []flat.NodeId{cap: 256}
	old_module := t.cur_module
	for li in state.log_start .. t.used_fns_log.len {
		named := state.by_spelling[t.used_fns_log[li]] or { continue }
		for entry in named {
			if entry.id >= nodes.len || !nodes[entry.id] {
				continue
			}
			node := t.a.nodes[entry.id]
			if t.should_transform_fn_in_module(node, entry.module) {
				t.clear_node_subtree_flags(flat.NodeId(entry.id), mut nodes, mut stack)
			}
		}
	}
	for _, decl in decls {
		if int(decl.id) >= state.node_count {
			t.collect_node_subtree_flags(decl.id, mut nodes, mut stack)
		}
	}
	t.cur_module = state.scan_module
	for i in state.node_count .. t.a.nodes.len {
		kind := t.a.nodes[i].kind
		if kind == .module_decl {
			t.cur_module = t.a.nodes[i].value_text()
			continue
		}
		if kind != .fn_decl {
			continue
		}
		node := unsafe { &t.a.nodes[i] }
		if node.value.starts_with('test_') {
			continue
		}
		t.cur_module = t.node_module_or(i, t.cur_module)
		if !t.should_transform_fn(*node) {
			t.collect_node_subtree_flags(flat.NodeId(i), mut nodes, mut stack)
			if !t.fn_decl_has_unresolved_generics(*node, t.cur_module) {
				state.add(node.value, i, t.cur_module)
			}
		}
	}
	state.scan_module = t.cur_module
	t.cur_module = old_module
	state.nodes = nodes
	state.node_count = t.a.nodes.len
	state.log_start = t.used_fns_log.len
	t.ignored_state = state
	return nodes
}

// add records an unused function under each spelling of `value` declared in
// `module_name` that should_transform_fn_in_module tests.
fn (mut s IgnoredNodesState) add(value string, id int, module_name string) {
	entry := IgnoredFn{
		id:     id
		module: module_name
	}
	s.add_spellings(value, entry)
	if value.contains('[') {
		base := generic_fn_decl_base_value(value)
		if base != value {
			s.add_spellings(base, entry)
		}
	}
}

// add_spellings mirrors the lookups of used_fn_contains_in_module.
fn (mut s IgnoredNodesState) add_spellings(name string, entry IgnoredFn) {
	module_name := entry.module
	plain_module := module_name == '' || module_name == 'main' || module_name == 'builtin'
	if !plain_module && name.len > module_name.len && name[module_name.len] == `.`
		&& name.starts_with(module_name) {
		s.add_spelling(name, entry)
		s.add_spelling(c_name(name), entry)
	}
	dfn := transform_dotted_fn_name(module_name, name)
	s.add_spelling(dfn, entry)
	s.add_spelling(c_name(dfn), entry)
}

fn (mut s IgnoredNodesState) add_spelling(spelling string, entry IgnoredFn) {
	mut entries := s.by_spelling[spelling] or { []IgnoredFn{} }
	entries << entry
	s.by_spelling[spelling] = entries
}

@[direct_array_access]
fn (mut t Transformer) ensure_node_module_map() {
	if t.node_context_read_only {
		return
	}
	if t.node_module_map_nodes == t.a.nodes.len {
		return
	}
	if t.node_module_map_nodes < 0 || t.node_module_map_nodes > t.a.nodes.len {
		t.node_module_map_cache = []u16{len: t.a.nodes.len, cap: t.a.nodes.cap}
		t.node_file_map_cache = []u16{len: t.a.nodes.len, cap: t.a.nodes.cap}
		t.node_module_map_nodes = 0
	} else {
		t.ensure_node_context_map_capacity()
	}
	// The nodes the parent index covers take their context on demand, from their
	// nearest marked ancestor (see node_context_owner): only files and
	// declarations are marked here, not every node below them.
	if t.node_module_map_nodes == 0 {
		t.node_context_lazy_end = int_min(t.source_parent_ids.len, t.a.nodes.len)
	}
	mut cur_module := ''
	mut cur_file := ''
	for i := t.node_module_map_nodes; i < t.a.nodes.len; i++ {
		// Context indexes can change here, but the AST arena cannot.
		node := unsafe { &t.a.nodes[i] }
		match node.kind {
			.file {
				cur_file = node.value_text()
				cur_module = t.tc.file_modules[node.value_text()] or { '' }
				t.mark_node_context_root(i, cur_module, cur_file)
			}
			.module_decl {
				cur_module = node.value_text()
			}
			.fn_decl, .const_decl, .global_decl, .struct_decl, .type_decl, .enum_decl,
			.interface_decl {
				// Generated roots already carry their declaration context. Preserve it
				// when incrementally scanning an append-only region that has no `.file`
				// marker of its own.
				context_module := t.node_module_or(i, cur_module)
				context_file := t.node_file_or(i, cur_file)
				t.mark_node_context_root(i, context_module, context_file)
			}
			else {}
		}
	}
	t.node_module_map_nodes = t.a.nodes.len
}

// mark_node_context_root marks a file or declaration with its context: the node
// alone where its subtree takes contexts on demand, else the whole subtree.
@[direct_array_access]
fn (mut t Transformer) mark_node_context_root(idx int, module_name string, file_name string) {
	if idx >= t.node_context_lazy_end {
		t.mark_node_context(flat.NodeId(idx), module_name, file_name)
		return
	}
	t.node_module_map_cache[idx] = t.node_context_text_id(module_name)
	t.node_file_map_cache[idx] = t.node_context_text_id(file_name)
}

// node_context_owner is the nearest ancestor of node `idx` with a recorded
// context, or -1, for a node whose context is taken on demand. The transformer
// that owns the context arrays records the context it finds on the nodes it
// walked through, so asking again, or for a node below them, is short.
@[direct_array_access]
fn (t &Transformer) node_context_owner(idx int) int {
	if idx >= t.node_context_lazy_end {
		return -1
	}
	mut owner := -1
	mut cursor := idx
	// The bound only guards against a cycle in a rewritten tree.
	for _ in 0 .. 1 << 16 {
		if cursor < 0 || cursor >= t.source_parent_ids.len {
			break
		}
		cursor = int(t.source_parent_ids[cursor])
		if cursor < 0 || cursor >= t.node_module_map_cache.len
			|| cursor >= t.node_file_map_cache.len {
			break
		}
		if t.node_module_map_cache[cursor] != 0 || t.node_file_map_cache[cursor] != 0 {
			owner = cursor
			break
		}
	}
	if owner < 0 || t.node_context_read_only {
		return owner
	}
	module_id := t.node_module_map_cache[owner]
	file_id := t.node_file_map_cache[owner]
	unsafe {
		mut modules := &u16(t.node_module_map_cache.data)
		mut files := &u16(t.node_file_map_cache.data)
		cursor = idx
		for cursor != owner {
			modules[cursor] = module_id
			files[cursor] = file_id
			cursor = int(t.source_parent_ids[cursor])
		}
	}
	return owner
}

fn (mut t Transformer) ensure_node_context_map_capacity() {
	if t.node_context_read_only {
		return
	}
	grow_node_context_cache(mut t.node_module_map_cache, t.a.nodes.len, t.a.nodes.cap)
	grow_node_context_cache(mut t.node_file_map_cache, t.a.nodes.len, t.a.nodes.cap)
}

fn grow_node_context_cache(mut cache []u16, size int, capacity int) {
	old_len := cache.len
	if old_len >= size {
		return
	}
	if cache.cap < capacity {
		// Use the AST's reserved size without rounding each side table up again.
		mut grown := []u16{cap: capacity}
		grown << cache
		// Transfer the new backing; this local has no other surviving reference.
		cache = unsafe { grown }
	}
	// Extend in place instead of retaining a temporary zero-filled array.
	unsafe {
		cache.grow_len(size - old_len)
		vmemset(&cache[old_len], 0, int(size - old_len) * int(sizeof(u16)))
	}
}

// node_context_text_id interns a declaration-context spelling for the per-node
// module/file tables. Id 0 means "unset". Only the owning transformer interns
// (workers run with node_context_read_only set), so the shared table needs no
// synchronization.
fn (mut t Transformer) node_context_text_id(value string) u16 {
	if value == '' {
		return 0
	}
	if id := t.node_context_text_ids[value] {
		return id
	}
	if t.node_context_texts.len >= max_u16 {
		// Two bytes per node keep these whole-AST tables small; one compilation
		// never declares anywhere near 65535 distinct file and module names.
		panic('v3 transform: too many distinct declaration contexts (${t.node_context_texts.len})')
	}
	id := u16(t.node_context_texts.len + 1)
	t.node_context_texts << value
	t.node_context_text_ids[value] = id
	return id
}

// node_context_text resolves an interned declaration-context spelling.
@[direct_array_access; inline]
fn (t &Transformer) node_context_text(id u16) string {
	idx := int(id) - 1
	if idx < 0 || idx >= t.node_context_texts.len {
		return ''
	}
	return t.node_context_texts[idx]
}

@[direct_array_access]
fn (t &Transformer) node_file_or(idx int, fallback string) string {
	if idx >= 0 && idx < t.node_file_map_cache.len {
		file_id := t.node_file_map_cache[idx]
		if file_id != 0 {
			return t.node_context_text(file_id)
		}
		if t.node_module_map_cache[idx] == 0 {
			owner := t.node_context_owner(idx)
			if owner >= 0 {
				return t.node_file_or(owner, fallback)
			}
		}
	}
	return fallback
}

@[direct_array_access]
fn (t &Transformer) node_module_or(idx int, fallback string) string {
	if idx >= 0 && idx < t.node_module_map_cache.len {
		module_id := t.node_module_map_cache[idx]
		if module_id != 0 {
			return t.node_context_text(module_id)
		}
		if idx < t.node_file_map_cache.len && t.node_file_map_cache[idx] == 0 {
			owner := t.node_context_owner(idx)
			if owner >= 0 {
				return t.node_module_or(owner, fallback)
			}
		}
		if !isnil(t.tc) && idx < t.node_file_map_cache.len {
			file_name := t.node_context_text(t.node_file_map_cache[idx])
			if source_module := t.tc.file_modules[file_name] {
				return source_module
			}
			if file_name.len > 0 {
				return 'main'
			}
		}
	}
	return fallback
}

@[direct_array_access]
fn (mut t Transformer) mark_node_context(id flat.NodeId, module_name string, file_name string) {
	if t.node_context_read_only {
		return
	}
	module_id := t.node_context_text_id(module_name)
	file_id := t.node_context_text_id(file_name)
	// The stack's length is its high-water mark and `top` the live depth, so room
	// is made once per node rather than once per child.
	t.node_context_stack.clear()
	t.node_context_stack << id
	mut top := 1
	for top > 0 {
		top--
		cur := t.node_context_stack[top]
		idx := int(cur)
		if idx < 0 || idx >= t.a.nodes.len {
			continue
		}
		if idx < t.node_module_map_cache.len && idx < t.node_file_map_cache.len
			&& t.node_module_map_cache[idx] == module_id
			&& t.node_file_map_cache[idx] == file_id {
			continue
		}
		if idx < t.node_module_map_cache.len {
			t.node_module_map_cache[idx] = module_id
		}
		if idx < t.node_file_map_cache.len {
			t.node_file_map_cache[idx] = file_id
		}
		// Context indexes can change here, but the AST arena cannot.
		node := unsafe { &t.a.nodes[idx] }
		start := node.children_start
		end := start + int(node.children_count)
		if start < 0 || end > t.a.children.len {
			continue
		}
		if top + (end - start) > t.node_context_stack.len {
			unsafe { t.node_context_stack.grow_len(top + (end - start) - t.node_context_stack.len) }
		}
		for i in start .. end {
			// The child range is checked above; avoid per-child bounds checks while mapping modules.
			child_id := unsafe { t.a.children[i] }
			if int(child_id) >= 0 {
				t.node_context_stack[top] = child_id
				top++
			}
		}
	}
	t.node_context_stack.clear()
}

fn (mut t Transformer) collect_node_subtree_ids(id flat.NodeId, mut nodes map[int]bool) {
	if int(id) < 0 || int(id) >= t.a.nodes.len || nodes[int(id)] {
		return
	}
	nodes[int(id)] = true
	node := t.a.node(id)
	for i in 0 .. node.children_count {
		t.collect_node_subtree_ids(t.a.child(node, i), mut nodes)
	}
}

// clear_node_subtree_flags clears the flags collect_node_subtree_flags set for
// the subtree of `id`.
@[direct_array_access]
fn (mut t Transformer) clear_node_subtree_flags(id flat.NodeId, mut nodes []bool, mut stack []flat.NodeId) {
	limit := if nodes.len < t.a.nodes.len { nodes.len } else { t.a.nodes.len }
	stack.clear()
	stack << id
	for stack.len > 0 {
		idx := int(stack.pop())
		if idx < 0 || idx >= limit || !nodes[idx] {
			continue
		}
		nodes[idx] = false
		node := &t.a.nodes[idx]
		start := int(node.children_start)
		end := start + int(node.children_count)
		if start < 0 || end > t.a.children.len {
			continue
		}
		for i in start .. end {
			child_id := t.a.children[i]
			if int(child_id) >= 0 {
				stack << child_id
			}
		}
	}
}

@[direct_array_access]
fn (mut t Transformer) collect_node_subtree_flags(id flat.NodeId, mut nodes []bool, mut stack []flat.NodeId) {
	limit := if nodes.len < t.a.nodes.len { nodes.len } else { t.a.nodes.len }
	// `stack` only lends its storage: the pending ids are stack[..top], kept by
	// hand because this walk visits every node of the unused functions.
	if stack.len == 0 {
		stack << id
	} else {
		stack[0] = id
	}
	mut top := 1
	for top > 0 {
		top--
		idx := int(stack[top])
		if idx < 0 || idx >= limit || nodes[idx] {
			continue
		}
		nodes[idx] = true
		node := &t.a.nodes[idx]
		start := int(node.children_start)
		end := start + int(node.children_count)
		if start < 0 || end > t.a.children.len {
			continue
		}
		if top + (end - start) > stack.len {
			stack << []flat.NodeId{len: top + (end - start) - stack.len + 256}
		}
		for i in start .. end {
			child_id := t.a.children[i]
			if int(child_id) >= 0 {
				stack[top] = child_id
				top++
			}
		}
	}
}

fn (mut t Transformer) emit_generic_fn_specialization(decl GenericFnDecl, args_ids []u32) flat.NodeId {

	// Queueing/inference has already canonicalized these arguments in the caller's
	// module. Preserve that identity while entering the imported declaration.
	concrete_args_source_ids := args_ids.clone()
	spec_key := t.generic_specialization_progress_key(decl, concrete_args_source_ids)
	if clone_id := t.generic_fn_spec_nodes.get(spec_key) {
		return clone_id
	}
	if t.generic_fn_specs_in_progress.contains(spec_key) {
		return flat.empty_node
	}
	t.generic_fn_specs_in_progress.set(spec_key, true)
	old_specialization_node_start := t.specialization_node_start
	specialization_nodes_start := t.a.nodes.len
	t.specialization_node_start = specialization_nodes_start
	defer {
		t.generic_fn_specs_in_progress.delete(spec_key)
		t.specialization_node_start = old_specialization_node_start
	}
	old_module := t.cur_module
	old_file := t.cur_file
	old_tc_module := if isnil(t.tc) { '' } else { t.tc.cur_module }
	old_tc_file := if isnil(t.tc) { '' } else { t.tc.cur_file }
	t.cur_module = decl.module
	t.cur_file = decl.file
	if !isnil(t.tc) {
		t.tc.cur_module = decl.module
		t.tc.cur_file = decl.file
	}
	generated_module := if decl.module.len > 0 { decl.module } else { 'main' }
	t.add_generated_fn_decl_context(generated_module)
	old_params := t.active_generic_params
	old_specialization_args := t.active_specialization_args
	mut old_specialization_main_types := t.active_specialization_main_types.move()
	t.clear_node_type_memo()
	t.active_generic_params = t.generic_fn_param_names(decl.node, decl.module)
	t.clear_node_type_memo()
	t.active_specialization_args = flat.TypeTextList{ ids: concrete_args_source_ids.clone() }
	t.active_specialization_main_types = t.specialization_main_type_closure(concrete_args_source_ids)
	if decl.node.value.contains('.') {
		receiver := generic_fn_decl_base_value(decl.node.value).all_before_last('.')
		if receiver.len > 0 {
			t.record_generic_specialization_args_in_module(receiver, decl.module, concrete_args_source_ids)
		}
	}
	old_clone_var_types := t.var_types.clone()
	old_clone_mut_param_values := t.mut_param_values.clone()
	// Seed the template's params (with substituted types) for the duration of
	// the clone: nested generic calls are retargeted while cloning, and their
	// arg-type inference must see the declared value type of a `mut val T`
	// param, not the checker's internal `&T` annotation on the ident — the
	// latter would bind T to `&Concrete` while the later monomorphize scan
	// binds it to `Concrete`, leaving the call-site type and the emitted
	// specialization in disagreement.
	t.reset_var_types()
	for i in 0 .. decl.node.children_count {
		param_child := t.a.child_node(&decl.node, i)
		if int(param_child.kind) != 75 {
			if t.prefix_param_scan {
				break
			}
			continue
		}
		if param_child.value_text().len == 0 || param_child.typ == 0 {
			continue
		}
		mut param_raw_id := param_child.typ
		if flat.source_type_shape(param_raw_id).has(.mutable_type) {
			param_raw_id = flat.source_type_syntax(param_raw_id).raw_base()
		}
		if flat.source_type_shape(param_raw_id).has(.variadic) {
			param_raw_id = flat.source_type_composition_one(.array, flat.source_type_syntax(param_raw_id).raw_base())
		}
		// `mut val T` params use `&T` internally; retain explicit pointer params.
		if param_child.is_mut && param_child.op != .amp
			&& flat.source_type_shape(param_raw_id).has(.pointer) {
			param_raw_id = flat.source_type_syntax(param_raw_id).raw_base()
		}
		param_substituted_source_id := t.subst_type(param_raw_id, concrete_args_source_ids)

		if param_substituted_source_id != 0 {
			t.set_var_type(param_child.value_text(), param_substituted_source_id)
		}
		if param_child.is_mut || param_child.op == .amp || flat.source_type_shape(param_child.typ).has(.mutable_type) {
			t.mut_param_values[param_child.value_text()] = true
		}
	}
	t.cloning_generic_fn_depth++
	old_clone_ret_type_source_id := t.cur_fn_ret_type

	t.set_cur_fn_ret_type(t.specialized_signature_type_text(decl, t.generic_fn_return_type_text(decl), concrete_args_source_ids, t.active_generic_params))
	// A check clones a library instance without its body when the body cannot
	// ask for an instance of the program (see LibraryBodies).
	library_clone := !isnil(t.tc) && t.tc.check_concrete_generic_bodies
		&& decl.file !in t.tc.diagnostic_files
	// An instance that the program's code asks for is checked (see LibraryBodies).
	program_library_instance := library_clone && t.library_bodies.program_specs.contains(spec_key)
	header_only := library_clone && !t.library_instance_needs_body(concrete_args_source_ids.clone())
		&& !program_library_instance
	if library_clone {
		t.tc.library_instances++
		if header_only {
			t.tc.library_headers++
		}
	}
	clone_id := if header_only {
		t.clone_generic_fn_header(decl.node, concrete_args_source_ids)
	} else {
		t.clone_generic_fn_node(decl.node, concrete_args_source_ids)
	}
	t.set_cur_fn_ret_type(old_clone_ret_type_source_id)
	t.cloning_generic_fn_depth--
	// Declaration attributes are indexed by the parsed declaration node. A
	// specialization is appended later and therefore has no entry in that map.
	// Preserve the transform-relevant `manualfree` bit on the cloned function so
	// ownership lowering does not inject frees into an explicitly manual body.
	if !isnil(t.tc) && t.tc.declaration_has_attribute(decl.id, 'manualfree') {
		cloned_fn := t.a.nodes[int(clone_id)]
		t.set_node(int(clone_id), flat.Node{
			kind:           cloned_fn.kind
			op:             cloned_fn.op
			pos:            cloned_fn.pos
			source_value:   cloned_fn.source_value
			value:          cloned_fn.value
			typ:            cloned_fn.typ
			payload:        cloned_fn.payload
			children_start: cloned_fn.children_start
			children_count: cloned_fn.children_count
			is_mut:         cloned_fn.is_mut
			flags:          flat.node_flags(true, cloned_fn.is_static_type_method())
		})
	}
	t.a.specialized_fn_nodes[int(clone_id)] = true
	t.a.specialized_fn_modules[int(clone_id)] = decl.module
	t.a.specialized_fn_files[int(clone_id)] = decl.file
	t.generic_fn_spec_nodes.set(spec_key, clone_id)
	t.restore_var_types(old_clone_var_types)
	t.specialize_cloned_fn_signature(clone_id, decl, concrete_args_source_ids)
	clone := t.a.nodes[int(clone_id)]
	if !t.generic_signatures_pre_registered {
		t.register_specialized_fn_signature(decl, clone, concrete_args_source_ids)
	}
	t.clear_node_type_memo()
	t.active_generic_params = old_params
	t.clear_node_type_memo()
	t.active_specialization_args = old_specialization_args
	t.active_specialization_main_types = old_specialization_main_types.move()
	generic_params := t.generic_fn_param_names(decl.node, decl.module)
	validate_return := t.generic_fn_return_depends_on_comptime_if(decl.node, generic_params)
	mut concrete_error_count := 0
	// An instance whose types its constraints reject is reported at the call
	// that asks for it: the errors of its body would only repeat that one.
	instance_satisfies := isnil(t.tc)
		|| t.tc.generic_instance_satisfies_constraints(decl.node, generic_params, concrete_args_source_ids)
	check_fixture_semantics := !isnil(t.tc) && t.tc.checker_fixture_mode && instance_satisfies
	if check_fixture_semantics {
		concrete_error_count = t.tc.errors.len
		t.tc.check_concrete_fn_semantics(int(clone_id), decl.file, decl.module)
	} else if !isnil(t.tc) && t.tc.check_concrete_generic_bodies && instance_satisfies
		&& (decl.file in t.tc.diagnostic_files || program_library_instance) {
		t.tc.check_concrete_instance_members(int(clone_id), decl.file, decl.module, concrete_args_source_ids)
	}
	// A check only looks at the program's own instances. A library clone stays as
	// it was cloned, so the calls in its body start no specializations to lower.
	check_skips_body := !isnil(t.tc) && t.tc.check_concrete_generic_bodies
		&& decl.file !in t.tc.diagnostic_files
	if !check_skips_body {
		t.transform_specialized_fn_body(clone_id, specialization_nodes_start, decl.module, decl.file, generic_params, concrete_args_source_ids, decl.node.value, validate_return)
	}
	was_boxed_types_frozen := t.interface_boxed_types_frozen
	t.interface_boxed_types_frozen = false
	t.collect_lowered_interface_boxed_types_range(specialization_nodes_start, t.a.nodes.len)
	t.interface_boxed_types_frozen = was_boxed_types_frozen
	t.mut_param_values = old_clone_mut_param_values.clone()
	if check_fixture_semantics && t.tc.errors.len == concrete_error_count {
		t.tc.check_concrete_fn_semantics(int(clone_id), decl.file, decl.module)
	}
	t.ensure_node_context_map_capacity()
	t.mark_node_context(clone_id, decl.module, decl.file)
	t.cur_module = old_module
	t.cur_file = old_file
	if !isnil(t.tc) {
		t.tc.cur_module = old_tc_module
		t.tc.cur_file = old_tc_file
	}
	return clone_id
}

fn (mut t Transformer) transform_specialized_fn_body(clone_id flat.NodeId, specialization_nodes_start int, module_name string, file_name string, params []string, args_ids []u32, source_name string, validate_return bool) {
	if int(clone_id) < 0 || int(clone_id) >= t.a.nodes.len {
		return
	}
	old_module := t.cur_module
	old_file := t.cur_file
	old_tc_file := if isnil(t.tc) { '' } else { t.tc.cur_file }
	old_fn_name := t.cur_fn_name
	old_ret_type_source_id := t.cur_fn_ret_type

	old_var_types := t.var_types.clone()
	old_is_generic := t.cur_fn_is_generic
	old_generic_params := t.active_generic_params
	old_specialization_args := t.active_specialization_args
	mut old_specialization_main_types := t.active_specialization_main_types.clone()
	t.clear_node_type_memo()
	t.active_generic_params = params
	t.clear_node_type_memo()
	t.active_specialization_args = flat.TypeTextList{ ids: args_ids.clone() }
	t.active_specialization_main_types = t.specialization_main_type_closure(args_ids)
	t.cur_module = module_name
	t.cur_file = file_name
	if !isnil(t.tc) {
		t.tc.cur_file = file_name
	}
	// The body transform establishes a real function scope (params seeded into
	// var_types), so scope lookups are trustworthy again inside it.
	old_in_scan := t.in_monomorphize_scan
	old_validating_specialization := t.validating_generic_spec
	t.in_monomorphize_scan = false
	t.validating_generic_spec = true
	old_node_type_memo := t.node_type_memo
	t.node_type_memo = &NodeTypeMemo{}
	if t.memo_node_types {
		t.begin_node_type_memo(specialization_nodes_start, t.a.nodes.len - 1)
	}
	clone_name := t.a.nodes[int(clone_id)].value_text()
	t.add_call_param_types_decl_key(clone_name, int(clone_id), file_name, module_name)
	t.add_call_param_types_decl_key(transform_qualified_fn_name(module_name, clone_name),
		int(clone_id), file_name, module_name)
	t.transform_fn_body(int(clone_id))
	t.end_node_type_memo()
	t.node_type_memo = old_node_type_memo
	if validate_return {
		t.validate_specialized_fn_return(clone_id, source_name)
	}
	t.in_monomorphize_scan = old_in_scan
	t.validating_generic_spec = old_validating_specialization
	t.cur_module = old_module
	t.cur_file = old_file
	if !isnil(t.tc) {
		t.tc.cur_file = old_tc_file
	}
	t.cur_fn_name = old_fn_name
	t.set_cur_fn_ret_type(old_ret_type_source_id)
	t.restore_var_types(old_var_types)
	t.cur_fn_is_generic = old_is_generic
	t.clear_node_type_memo()
	t.active_generic_params = old_generic_params
	t.clear_node_type_memo()
	t.active_specialization_args = old_specialization_args
	t.active_specialization_main_types = old_specialization_main_types.clone()
}

fn (t &Transformer) generic_fn_return_depends_on_comptime_if(node flat.Node, params []string) bool {
	for i := int(node.children_count) - 1; i >= 0; i-- {
		child := t.a.child_node(&node, i)
		if child.kind == .param {
			continue
		}
		return child.kind == .comptime_if && generic_text_contains_param(child.value_text(), params)
	}
	return false
}

fn (mut t Transformer) validate_specialized_fn_return(clone_id flat.NodeId, source_name string) {
	if isnil(t.tc) || int(clone_id) < 0 || int(clone_id) >= t.a.nodes.len {
		return
	}
	ret := t.tc.parse_type(t.cur_fn_ret_type)
	if ret is types.Void {
		return
	}
	if ret is types.OptionType && ret.base_type is types.Void {
		return
	}
	if ret is types.ResultType && ret.base_type is types.Void {
		return
	}
	if !t.tc.fn_body_definitely_returns(t.a.nodes[int(clone_id)]) {
		t.record_monomorph_error('missing return at end of function `${source_name}`; expected `${ret.name()}`')
	}
}

fn (mut t Transformer) generated_fn_used_names(decl GenericFnDecl, clone_id flat.NodeId, args_ids []u32) []string {
	if int(clone_id) < 0 || int(clone_id) >= t.a.nodes.len {
		return []string{}
	}
	clone := t.a.nodes[int(clone_id)]
	qname := transform_qualified_fn_name(decl.module, clone.value)
	mut names := [clone.value, qname, c_name(clone.value), c_name(qname)]
	names << specialized_generic_fn_signature_aliases(decl, args_ids)
	old_module := t.cur_module
	old_file := t.cur_file
	old_specialization_args := t.active_specialization_args
	old_specialization_main_types := t.active_specialization_main_types.clone()
	t.cur_module = decl.module
	t.cur_file = decl.file
	t.clear_node_type_memo()
	t.active_specialization_args = flat.TypeTextList{ ids: args_ids.clone() }
	t.active_specialization_main_types = t.specialization_main_type_closure(args_ids)
	// Keep a caller-owned main type bare while recording the generated callee.
	// Without this live provenance, a same-named type in the declaring module can
	// rebind the specialization name and queue a duplicate body for the wrong type.
	t.record_generic_specialization_args_for_names(names, args_ids)
	names << t.generated_fn_body_call_names(clone_id)
	for name in names {
		t.mark_fn_used_name(name)
	}
	t.cur_module = old_module
	t.cur_file = old_file
	t.clear_node_type_memo()
	t.active_specialization_args = old_specialization_args
	t.active_specialization_main_types = old_specialization_main_types.clone()
	return names
}

fn (mut t Transformer) generated_fn_body_call_names(root flat.NodeId) []string {
	return t.generated_fn_body_call_names_filtered(root, unsafe { &t.used_struct_operator_fns }, false)
}

fn (mut t Transformer) generated_fn_body_candidate_call_names(root flat.NodeId, candidate_names &map[string]bool) []string {
	return t.generated_fn_body_call_names_filtered(root, candidate_names, true)
}

fn (mut t Transformer) generated_fn_body_call_names_filtered(root flat.NodeId, candidate_names &map[string]bool, filter_candidates bool) []string {
	mut names := []string{}
	mut seen := map[string]bool{}
	saved_fn_name := t.cur_fn_name
	saved_ret_type_source_id := t.cur_fn_ret_type

	mut saved_vars := unsafe { t.var_types }
	mut saved_var_indices := t.var_type_indices.move()
	mut saved_mut_param_values := t.mut_param_values.move()
	mut saved_fn_value_locals := t.fn_value_locals.move()
	mut saved_fixed_array_param_values := t.fixed_array_param_values.move()
	mut saved_interface_var_concrete_types := t.interface_var_concrete_types.move()
	mut saved_addr_lvalue_pointer_locals := t.addr_lvalue_pointer_locals.move()
	mut saved_orm_initialized_fields := t.orm_initialized_fields.move()
	mut saved_sql_query_data_aliases := t.sql_query_data_aliases.move()
	t.clear_node_type_memo()
	t.var_types = []VarTypeBinding{}
	t.var_type_indices = map[string]int{}
	t.mut_param_values = map[string]bool{}
	t.seed_generated_fn_body_context(root)
	// The filtered scan only declares or rebinds locals while it walks a body, so its
	// scopes restore from the undo log. Queued nested specializations can run other
	// lowering, which keeps the copying scopes.
	undo_was_active := t.var_type_undo_active
	undo_start := t.var_type_undo_log.len
	t.var_type_undo_active = filter_candidates && !t.defer_nested_generic_emissions
	t.collect_generated_fn_body_call_names(root, candidate_names, filter_candidates, mut names, mut seen)
	t.var_type_undo_active = undo_was_active
	t.var_type_undo_log.trim(undo_start)
	t.cur_fn_name = saved_fn_name
	t.set_cur_fn_ret_type(saved_ret_type_source_id)
	t.clear_node_type_memo()
	t.var_types = unsafe { saved_vars }
	t.var_type_indices = saved_var_indices.move()
	t.mut_param_values = saved_mut_param_values.move()
	t.fn_value_locals = saved_fn_value_locals.move()
	t.fixed_array_param_values = saved_fixed_array_param_values.move()
	t.interface_var_concrete_types = saved_interface_var_concrete_types.move()
	t.addr_lvalue_pointer_locals = saved_addr_lvalue_pointer_locals.move()
	t.orm_initialized_fields = saved_orm_initialized_fields.move()
	t.sql_query_data_aliases = saved_sql_query_data_aliases.move()
	if !isnil(t.var_type_cache) {
		t.var_type_cache.clear()
	}
	return names
}

fn (mut t Transformer) seed_generated_fn_body_context(root flat.NodeId) {
	if int(root) < 0 || int(root) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(root)]
	if node.kind != .fn_decl {
		return
	}
	t.cur_fn_name = node.value_text()
	t.set_cur_fn_ret_type((if node.typ != 0 {
		node.typ
	} else {
		t.fn_body_return_type(node)
	}))
	t.reset_var_types()
	param_count := t.fn_body_param_count(node)
	param_types := t.fn_body_param_types(node, param_count)
	mut param_idx := 0
	for i in 0 .. node.children_count {
		child_id := t.a.children[node.children_start + i]
		if int(child_id) < 0 {
			continue
		}
		child := t.a.nodes[int(child_id)]
		if int(child.kind) != 75 {
			if t.prefix_param_scan {
				break
			}
			continue
		}
		if child.value_text().len == 0 {
			continue
		}
		child_type_shape := flat.source_type_shape(child.typ)
		raw_source_typ := (if child_type_shape.has(.variadic) {
			flat.source_type_composition_one(.array, flat.source_type_syntax(child.typ).raw_base())
		} else {
			child.typ
		})
		raw_typ := if child.typ != 0 {
			if child_type_shape.has(.variadic) {
				elem_typ := flat.source_type_syntax(child.typ).raw_base()
				normalized_elem_typ := t.normalize_type_alias(elem_typ)
				if normalized_elem_typ == elem_typ {
					raw_source_typ
				} else {
					flat.source_type_composition_one(.array, normalized_elem_typ)
				}
			} else {
				t.normalize_type_alias(child.typ)
			}
		} else {
			u32(0)
		}
		typ := (if raw_typ != 0 {
			raw_typ
		} else if param_idx < param_types.len {
			t.normalize_type_alias(param_types[param_idx].source_type_id())
		} else if param_idx == 0 {
			t.fn_body_receiver_type(node.value_text())
		} else {
			u32(0)
		})
		if typ != 0 {
			t.set_var_type_with_raw(child.value_text(), typ, raw_source_typ)
		}
		if child.is_mut || child.op == .amp || child_type_shape.has(.mutable_type) {
			t.mut_param_values[child.value_text()] = true
		}
		param_idx++
	}
}

fn (mut t Transformer) collect_generated_fn_body_call_names(id flat.NodeId, candidate_names &map[string]bool, filter_candidates bool, mut names []string, mut seen map[string]bool) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	// This traversal queues signatures and updates caches without growing the AST.
	node := t.a.node(id)
	if node.kind in [.block, .for_stmt, .for_in_stmt, .select_branch] {
		if t.var_type_undo_active {
			scope := t.mark_var_type_scope()
			for i in 0 .. node.children_count {
				t.collect_generated_fn_body_call_names(t.a.child(node, i), candidate_names,
					filter_candidates, mut names, mut seen)
			}
			t.restore_var_type_scope(scope)
			return
		}
		saved_vars := t.var_types.clone()
		for i in 0 .. node.children_count {
			t.collect_generated_fn_body_call_names(t.a.child(node, i), candidate_names,
				filter_candidates, mut names, mut seen)
		}
		t.restore_var_types(saved_vars)
		return
	}
	if node.kind == .decl_assign {
		for i in 1 .. node.children_count {
			t.collect_generated_fn_body_call_names(t.a.child(node, i), candidate_names, filter_candidates, mut names, mut seen)
		}
		t.seed_generated_decl_assign_binding(*node)
		return
	}
	if node.kind == .call {
		call_name := t.generated_call_name_for_used(id, *node)
		if call_name.len > 0 {
			t.push_generated_used_name(call_name, candidate_names, filter_candidates, mut names, mut seen)
		}
		if t.defer_nested_generic_emissions {
			decls := t.cached_generic_fn_decls()
			if decl_key, args_source_ids := t.cached_generic_call_specialization(id, *node, t.cur_module, decls) {
				if decl := decls[decl_key] {
					concrete_args_source_ids := t.canonical_generic_specialization_args(args_source_ids)

					if !t.generic_specialization_in_progress(decl, concrete_args_source_ids) {
						t.request_generic_fn_specialization(decl, concrete_args_source_ids)
					}
				}
			}
		}
	} else if node.kind in [.ident, .selector, .cast_expr, .paren, .expr_stmt] {
		if fn_value_name := t.generated_fn_value_name_for_used(id, *node, candidate_names,
			filter_candidates)
		{
			t.push_generated_used_name(fn_value_name, candidate_names, filter_candidates, mut names, mut seen)
		}
	}
	for i in 0 .. node.children_count {
		t.collect_generated_fn_body_call_names(t.a.child(node, i), candidate_names, filter_candidates, mut names, mut seen)
	}
}

fn (mut t Transformer) seed_generated_decl_assign_binding(node flat.Node) {
	if node.children_count < 2 {
		return
	}
	lhs_id := t.a.child(&node, 0)
	rhs_id := t.a.child(&node, 1)
	if int(lhs_id) < 0 || int(lhs_id) >= t.a.nodes.len {
		return
	}
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value_text().len == 0 {
		return
	}
	raw_typ_source_id := t.generated_decl_assign_binding_type(node, lhs, rhs_id)

	if raw_typ_source_id != 0 {
		t.set_var_type_with_raw(lhs.value_text(), t.normalize_type_alias(raw_typ_source_id), raw_typ_source_id)
	}
}

fn (mut t Transformer) generated_decl_assign_binding_type(node flat.Node, lhs flat.Node, rhs_id flat.NodeId) u32 {
	if lhs.typ != 0 {
		return lhs.typ
	}
	if node.typ != 0 {
		return node.typ
	}
	if int(rhs_id) >= 0 && int(rhs_id) < t.a.nodes.len {
		rhs := t.a.nodes[int(rhs_id)]
		if rhs.typ != 0 {
			return rhs.typ
		}
		rhs_type_source_id := t.node_type(rhs_id)

		if rhs_type_source_id != 0 {
			return t.normalize_type_alias(rhs_type_source_id)
		}
	}
	return u32(0)
}

// generated_fn_value_name_for_used names the function that a value node refers to. With
// `filter_candidates`, the caller keeps a name only when it passes the late-name scan's
// filter over `candidate_names`.
fn (mut t Transformer) generated_fn_value_name_for_used(id flat.NodeId, node flat.Node, candidate_names &map[string]bool, filter_candidates bool) ?string {
	if !isnil(t.tc) {
		if name := t.tc.resolved_fn_value_name(id) {
			return name
		}
	}
	// Every spelling probed below ends with this node's text: skip the probes when
	// no name with that ending can pass the filter.
	if filter_candidates && node.value_text().len > 0 && !isnil(t.tc)
		&& t.late_scan_gate_rejects(node.value_text(), candidate_names) {
		return none
	}
	if node.kind == .ident && node.value_text().len > 0 {
		if (t.var_type(node.value_text())) != 0 {
			return none
		}
		if t.generated_used_name_is_known_fn(node.value_text()) {
			return node.value_text()
		}
		if !node.value_text().contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
			&& t.cur_module != 'builtin' {
			qname := '${t.cur_module}.${node.value_text()}'
			if t.generated_used_name_is_known_fn(qname) {
				return qname
			}
		}
		return none
	}
	if node.kind == .selector && node.children_count > 0 && node.value_text().len > 0 {
		base := t.a.child_node(&node, 0)
		if base.kind != .ident || base.value_text().len == 0 {
			return none
		}
		// An enum value names no function. Few selectors spell a known function, so
		// that is settled first and the enum question is only asked for those.
		mut name := ''
		full := '${base.value_text()}.${node.value_text()}'
		if t.generated_used_name_is_known_fn(full) {
			name = full
		}
		if name.len == 0 && !isnil(t.tc) && t.cur_file.len > 0 {
			if mod := t.file_import_module(t.cur_file, base.value_text()) {
				resolved := '${mod}.${node.value_text()}'
				if t.generated_used_name_is_known_fn(resolved) {
					name = resolved
				}
			}
		}
		if name.len == 0 && t.cur_module.len > 0 && t.cur_module != 'main'
			&& t.cur_module != 'builtin' && !full.contains('${t.cur_module}.') {
			qname := '${t.cur_module}.${full}'
			if t.generated_used_name_is_known_fn(qname) {
				name = qname
			}
		}
		if name.len == 0 || t.selector_is_enum_value(id) {
			return none
		}
		return name
	}
	return none
}

fn (t &Transformer) generated_used_name_is_known_fn(name string) bool {
	if name == '' {
		return false
	}
	if name in t.fn_ret_types.ids {
		return true
	}
	return !isnil(t.tc) && (name in t.tc.fn_ret_types || name in t.tc.fn_param_types)
}

fn (mut t Transformer) generated_call_name_for_used(id flat.NodeId, node flat.Node) string {
	if node.children_count > 0 {
		fn_id := t.a.child(&node, 0)
		fn_node := t.a.nodes[int(fn_id)]
		if fn_node.kind == .selector && fn_node.value_text().len > 0 && fn_node.children_count > 0 {
			base_id := t.a.child(&fn_node, 0)
			base_type_id := flat.source_type_syntax(t.node_type(base_id)).pointer_raw_base()
			if flat.source_type_syntax(base_type_id).generic_app_valid() {
				return '${flat.source_type_name(base_type_id)}.${fn_node.value_text()}'
			}
		}
		if fn_node.kind == .ident && fn_node.value_text().len > 0 && (t.var_type(fn_node.value_text())) != 0 {
			return ''
		}
	}
	call_name := t.call_name_for_node(id, node)
	if call_name.len > 0 {
		if !t.generated_call_matches_selector_receiver(call_name, node) {
			if receiver_call := t.generated_selector_receiver_call_name(node) {
				return receiver_call
			}
			return ''
		}
		return t.generated_known_call_name(call_name) or { call_name }
	}
	if node.children_count == 0 {
		return ''
	}
	fn_id := t.a.child(&node, 0)
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind == .ident && fn_node.value_text().len > 0 {
		if (t.var_type(fn_node.value_text())) != 0 {
			return ''
		}
		return fn_node.value_text()
	}
	if fn_node.kind == .selector && fn_node.value_text().len > 0 && fn_node.children_count > 0 {
		base_id := t.a.child(&fn_node, 0)
		method_name := t.resolve_receiver_method_name(base_id, fn_node.value_text())
		if method_name.len > 0 {
			return method_name
		}
		base_type_id := flat.source_type_syntax(t.node_type(base_id)).pointer_raw_base()
		if flat.source_type_syntax(base_type_id).generic_app_valid() {
			return '${flat.source_type_name(base_type_id)}.${fn_node.value_text()}'
		}
	}
	return ''
}

fn (t &Transformer) generated_call_matches_selector_receiver(call_name string, node flat.Node) bool {
	if call_name == '' || node.children_count == 0 {
		return true
	}
	fn_node := t.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return true
	}
	receiver_name_id := flat.source_type_syntax(flat.source_type_id(call_name)).qualifier()
	if receiver_name_id == 0 {
		return true
	}
	base_type_source_id_4 := t.generated_selector_receiver_type(fn_node)
	if base_type_source_id_4 == 0 {
		return true
	}
	if receiver_name_id == base_type_source_id_4 {
		return true
	}
	if !flat.source_type_shape(base_type_source_id_4).has(.dotted) && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' && receiver_name_id == flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), base_type_source_id_4) {
		return true
	}
	if flat.source_type_syntax(receiver_name_id).unqualified() != flat.source_type_syntax(base_type_source_id_4).unqualified() {
		return true
	}
	return false
}

fn (t &Transformer) generated_selector_receiver_call_name(node flat.Node) ?string {
	if node.children_count == 0 {
		return none
	}
	fn_node := t.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.children_count == 0 || fn_node.value_text().len == 0 {
		return none
	}
	base_type_source_id_5 := t.generated_selector_receiver_type(fn_node)
	base_type := flat.source_type_name(base_type_source_id_5)
	if base_type_source_id_5 == 0 {
		return none
	}
	return '${base_type}.${fn_node.value_text()}'
}

fn (t &Transformer) generated_selector_receiver_type(fn_node flat.Node) u32 {
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return u32(0)
	}
	base_id := t.a.child(&fn_node, 0)
	base_node := t.a.nodes[int(base_id)]
	mut base_type := u32(0)
	if base_node.kind == .ident {
		base_type = t.var_type(base_node.value_text())
	}
	if base_type == 0 {
		base_type = t.node_type(base_id)
	}
	if base_type == 0 && !isnil(t.tc) {
		base_type = t.tc.resolve_type(base_id).source_type_id()
	}
	base_type = t.trim_all_pointer_type(base_type)
	return t.normalize_type_alias(base_type)
}

fn (t &Transformer) generated_known_call_name(name string) ?string {
	if name == '' {
		return none
	}
	if t.generated_used_name_is_known_fn(name) {
		return name
	}
	if !name.contains('.') || t.cur_module.len == 0 || t.cur_module == 'main'
		|| t.cur_module == 'builtin' || name.starts_with('${t.cur_module}.') {
		return none
	}
	qname := '${t.cur_module}.${name}'
	if t.generated_used_name_is_known_fn(qname) {
		return qname
	}
	return none
}

fn (mut t Transformer) push_generated_used_name(name string, candidate_names &map[string]bool, filter_candidates bool, mut names []string, mut seen map[string]bool) {
	if filter_candidates && !(*candidate_names)[name] && !t.used_struct_operator_fns[name]
		&& !t.late_name_may_expand_interface(name) {
		return
	}
	if name == '' || seen[name] {
		return
	}
	seen[name] = true
	names << name
	cn := c_name(name)
	if cn != name && !seen[cn] {
		seen[cn] = true
		names << cn
	}
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin'
		&& !name.contains('.') && !name.contains('__') {
		qname := '${t.cur_module}.${name}'
		if !seen[qname] {
			seen[qname] = true
			names << qname
		}
		qcn := c_name(qname)
		if qcn != qname && !seen[qcn] {
			seen[qcn] = true
			names << qcn
		}
	}
}

fn (mut t Transformer) specialize_cloned_fn_signature(clone_id flat.NodeId, decl GenericFnDecl, args_ids []u32) {
	if int(clone_id) < 0 || int(clone_id) >= t.a.nodes.len {
		return
	}
	params := t.generic_fn_param_names(decl.node, decl.module)
	t.set_node_typ(int(clone_id), t.specialized_signature_type_text(decl, t.generic_fn_return_type_text(decl), args_ids, params))
	t.set_node_generic_params(int(clone_id), []string{})
	mut dst_params := []flat.NodeId{}
	clone := t.a.nodes[int(clone_id)]
	for i in 0 .. clone.children_count {
		dst_id := t.a.child(&clone, i)
		if t.a.nodes[int(dst_id)].kind != .param {
			if t.prefix_param_scan {
				break
			}
			continue
		}
		dst_params << dst_id
	}
	mut param_idx := 0
	for i in 0 .. decl.node.children_count {
		src := t.a.child_node(&decl.node, i)
		if src.kind != .param {
			if t.prefix_param_scan {
				break
			}
			continue
		}
		if param_idx >= dst_params.len {
			continue
		}
		dst_id := dst_params[param_idx]
		t.set_node_typ(int(dst_id), t.specialized_signature_type_text(decl, src.typ, args_ids, params))
		param_idx++
	}
}

fn (mut t Transformer) register_specialized_fn_signature(decl GenericFnDecl, clone flat.Node, args_ids []u32) {
	t.register_specialized_fn_signature_value(decl, clone.value_text(), args_ids)
}

fn (mut t Transformer) register_specialized_fn_signature_value(decl GenericFnDecl, clone_value string, args_ids []u32) {
	old_module := t.cur_module
	old_file := t.cur_file
	old_tc_module := if isnil(t.tc) { '' } else { t.tc.cur_module }
	old_tc_file := if isnil(t.tc) { '' } else { t.tc.cur_file }
	old_specialization_args := t.active_specialization_args
	mut old_specialization_main_types := t.active_specialization_main_types.move()
	// The specialization name and queue key were already formed from these canonical
	// arguments in the caller's context. Re-resolving them here under another active
	// module can change a bare nominal type without changing `clone_value`.
	concrete_args_source_ids := args_ids.clone()

	t.clear_node_type_memo()
	t.active_specialization_args = flat.TypeTextList{ ids: concrete_args_source_ids.clone() }
	mut caller_main_types := t.specialization_main_type_closure(concrete_args_source_ids)
	if old_module in ['', 'main'] {
		for arg_source_id in concrete_args_source_ids {
			arg_source_id_6 := arg_source_id

			t.collect_caller_main_type_names(arg_source_id_6, mut caller_main_types)
		}
	}
	t.active_specialization_main_types = caller_main_types.move()
	t.cur_module = decl.module
	t.cur_file = decl.file
	if !isnil(t.tc) {
		t.tc.cur_module = decl.module
		t.tc.cur_file = decl.file
	}
	generic_params := t.generic_fn_param_names(decl.node, decl.module)
	signature_key := t.generic_specialization_progress_key(decl, concrete_args_source_ids)
	record_signature_types := !t.monomorph_signature_spec_keys.contains(signature_key)
	if record_signature_types {
		t.monomorph_signature_spec_keys.set(signature_key, true)
	}
	ret_name_source_id := t.specialized_signature_type_text(decl, t.generic_fn_return_type_text(decl), concrete_args_source_ids, generic_params)

	if record_signature_types {
		t.monomorph_signature_types << MonomorphSignatureType{
			typ:    ret_name_source_id
			module: decl.module
			file:   decl.file
		}
	}
	ret := if !isnil(t.tc) {
		t.tc.parse_resolution_type(ret_name_source_id)
	} else {
		types.Type(types.void_)
	}
	mut params := []types.Type{}
	mut variadic := false
	for i in 0 .. decl.node.children_count {
		child := t.a.child_node(&decl.node, i)
		if child.kind != .param {
			if t.prefix_param_scan {
				break
			}
			continue
		}
		param_type := explicit_mut_pointer_param_type_text(child, t.specialized_signature_type_text(decl, child.typ, concrete_args_source_ids, generic_params))
		if record_signature_types {
			t.monomorph_signature_types << MonomorphSignatureType{
				typ:    param_type
				module: decl.module
				file:   decl.file
			}
		}
		if flat.source_type_shape(param_type).has(.variadic) {
			variadic = true
		}
		if !isnil(t.tc) {
			parsed := t.tc.parse_resolution_type(param_type)
			// A `mut a ?T` is `&?&T`, as in a named fn's signature.
			params << if child.is_mut && child.op != .amp {
				types.mut_param_semantic_type(parsed)
			} else {
				parsed
			}
		}
	}
	qname := transform_qualified_fn_name(decl.module, clone_value)
	t.set_fn_ret_type(clone_value, ret_name_source_id)
	t.set_fn_ret_type(qname, ret_name_source_id)
	t.add_receiver_method_suffix_index(clone_value)
	t.add_receiver_method_suffix_index(qname)
	if !isnil(t.tc) {
		t.tc.ensure_private_transform_signatures()
		mut names := [clone_value, qname, c_name(clone_value), c_name(qname)]
		names << specialized_generic_fn_signature_aliases(decl, concrete_args_source_ids)
		t.record_generic_specialization_args_for_names(names, concrete_args_source_ids)
		for name in names {
			// The names and declaration strings can point into a worker scope
			// that is released before the driver promotes the checker metadata,
			// so own them here.
			owned_name := name.clone()
			t.tc.register_generated_fn_return_type(owned_name, ret)
			t.tc.register_generated_fn_param_types(owned_name, params.clone())
			t.tc.fn_variadic[owned_name] = variadic
			t.tc.fn_type_modules[owned_name] = decl.module.clone()
			t.tc.fn_type_files[owned_name] = decl.file.clone()
			t.add_receiver_method_suffix_index(owned_name)
			t.tc.specialized_generic_fns[owned_name] = true
			t.tc_signature_names_log << owned_name
		}
		t.tc.cur_module = old_tc_module
		t.tc.cur_file = old_tc_file
	}
	t.cur_module = old_module
	t.cur_file = old_file
	t.clear_node_type_memo()
	t.active_specialization_args = old_specialization_args
	t.active_specialization_main_types = old_specialization_main_types.move()
}

fn (mut t Transformer) specialized_fn_return_type_text(decl GenericFnDecl, args_ids []u32) u32 {
	return t.specialized_signature_type_text(decl, t.generic_fn_return_type_text(decl), args_ids, t.generic_fn_param_names(decl.node, decl.module))
}

fn (mut t Transformer) specialized_fn_return_display_type_text(decl GenericFnDecl, args_ids []u32) u32 {
	substituted := substitute_generic_type_text_with_params(t.generic_fn_return_type_text(decl), args_ids, t.generic_fn_param_names(decl.node, decl.module))
	return t.qualify_specialized_signature_type_text(substituted, decl)
}

fn (t &Transformer) generic_fn_return_type_text(decl GenericFnDecl) u32 {
	if isnil(t.tc) {
		return decl.node.typ
	}
	qualified_value := transform_qualified_fn_name(decl.module, decl.node.value)
	for name in [decl.key, decl.node.value, qualified_value, c_name(decl.key), c_name(qualified_value)] {
		if text_source_id := t.tc.fn_ret_type_texts.type_id(name) {
			if text_source_id != 0 {
				return text_source_id
			}
		}
	}
	return decl.node.typ
}

fn (mut t Transformer) specialized_signature_type_text(decl GenericFnDecl, typ_id u32, args_ids []u32, params []string) u32 {
	if direct_source_id := t.specialized_direct_generic_type_text(typ_id, args_ids, params) {
		return t.lock_colliding_main_generic_type_text(direct_source_id, decl.module)
	}
	substituted := substitute_generic_type_text_with_params(typ_id, args_ids, params)
	// The scalar `direct` branch above pins a colliding main type to `main.` before it can be
	// rebased into the decl module. A composite (`fn (T)`, `[]T`, `map[string]T`) hides that
	// type from the direct check, so lock it here too: otherwise the decl-module qualify below
	// rebases a nested main `Context` to the decl module's own `Context`, giving the callback
	// or element the wrong ABI. Only lock when substitution actually replaced a generic
	// parameter (mirroring the param clone in clone_generic_node): a decl-module type that
	// appears verbatim (e.g. the method receiver `Builder`) must not be locked, or the lock
	// over-fires its own homonym to a non-existent `main.Builder`.
	locked := if substituted != typ_id {
		t.lock_colliding_main_substitution_type_text(typ_id, substituted, decl.module, params)
	} else {
		substituted
	}
	qualified_source_id := t.qualify_specialized_signature_type_text(locked, decl)
	qualified_syntax := flat.source_type_syntax(qualified_source_id)
	if !isnil(t.tc) {
		alias_base_id := qualified_syntax.generic_app_base()
		if qualified_syntax.generic_app_valid() && (t.tc.type_aliases.contains(alias_base_id)
			|| flat.source_type_composition_two(.qualified, flat.source_type_id(decl.module), alias_base_id) in t.tc.type_aliases.ids) {
			expanded_source_id := t.normalize_type_in_module(qualified_source_id, decl.module)
			expanded_shape := flat.source_type_shape(expanded_source_id)
			// Array indexing needs the expanded element type. Callback aliases keep
			// their nominal signature for the remaining specialization steps.
			if expanded_shape.has(.array_type) {
				return expanded_source_id
			}
		}
	}
	// A composite that carries a collision-locked `main.` type (e.g. `fn (main.Context)`
	// from `fn wrap[T](cb fn (T))` specialized with a main `Context`) must not go through
	// the parse + name() round-trip below: name() renders `main.Context` back to a bare
	// `Context`, which the decl module then rebinds to its own homonym, giving the callback
	// the wrong ABI. Return the locked/qualified spelling directly, as the scalar `direct`
	// branch above already does for a locked scalar.
	if locked != substituted && qualified_syntax.main_fragment() {
		return qualified_source_id
	}
	qualified_shape := flat.source_type_shape(transform_trimmed_type_id(qualified_source_id))
	is_shared := qualified_shape.has(.shared_type)
	if isnil(t.tc) {
		return qualified_source_id
	}
	old_module := t.cur_module
	old_file := t.cur_file
	old_tc_module := t.tc.cur_module
	old_tc_file := t.tc.cur_file
	t.cur_module = decl.module
	t.cur_file = decl.file
	t.tc.cur_module = decl.module
	t.tc.cur_file = decl.file
	parsed := t.tc.parse_resolution_type(qualified_source_id)
	t.cur_module = old_module
	t.cur_file = old_file
	t.tc.cur_module = old_tc_module
	t.tc.cur_file = old_tc_file
	if parsed is types.Unknown {
		return qualified_source_id
	}
	// Parsing a variadic parameter produces an Array and loses the call convention.
	if qualified_shape.has(.variadic) {
		if parsed is types.Array {
			return flat.source_type_composition_one(.variadic_parameter, specialized_signature_storage_type_name(parsed.elem_type))
		}
		return qualified_source_id
	}
	if is_shared {
		return qualified_source_id
	}
	return specialized_signature_storage_type_name(parsed)
}

fn (t &Transformer) specialized_direct_generic_type_text(typ_id u32, args_ids []u32, params []string) ?u32 {
	clean_id := transform_trimmed_type_id(typ_id)
	for i, param in params {
		if clean_id == flat.source_type_id(param) && i < args_ids.len { return args_ids[i] }
	}
	shape := flat.source_type_shape(clean_id)
	kind := if shape.has(.mutable_type) {
		flat.SourceTypeComposition.mutable_parameter
	} else if shape.has(.shared_type) {
		flat.SourceTypeComposition.shared_type
	} else if shape.has(.atomic_type) {
		flat.SourceTypeComposition.atomic_type
	} else if shape.has(.variadic) {
		flat.SourceTypeComposition.variadic_parameter
	} else if shape.has(.array_type) {
		flat.SourceTypeComposition.array
	} else if shape.has(.option_type) {
		flat.SourceTypeComposition.option
	} else if shape.has(.result_type) {
		flat.SourceTypeComposition.result
	} else if shape.has(.pointer) {
		flat.SourceTypeComposition.pointer
	} else {
		return none
	}
	inner := t.specialized_direct_generic_type_text(flat.source_type_syntax(clean_id).raw_base(), args_ids, params) or { return none }
	inner_shape := flat.source_type_shape(inner)
	if (kind == .option && inner_shape.has(.option_type))
		|| (kind == .result && inner_shape.has(.result_type)) {
		return inner
	}
	return flat.source_type_composition_one(kind, inner)
}

fn specialized_signature_storage_type_name(typ types.Type) u32 {
	if typ is types.Alias { return typ.source_type_id() }
	if typ is types.OptionType {
		return flat.source_type_composition_one(.option, specialized_signature_storage_type_name(typ.base_type))
	}
	if typ is types.ResultType {
		return flat.source_type_composition_one(.result, specialized_signature_storage_type_name(typ.base_type))
	}
	if typ is types.Pointer {
		return flat.source_type_composition_one(.pointer, specialized_signature_storage_type_name(typ.base_type))
	}
	if typ is types.ArrayFixed {
		elem := specialized_signature_storage_type_name(typ.elem_type)
		if typ.len > 0 { return flat.source_type_fixed_array(typ.len, '', elem, false) }
		return flat.source_type_composition_two(.fixed_array, flat.source_type_id(typ.len_expr), elem)
	}
	return typ.source_type_id()
}

fn (t &Transformer) qualify_specialized_signature_type_text(typ_id u32, decl GenericFnDecl) u32 {
	clean_id := transform_trimmed_type_id(typ_id)
	if clean_id == 0 || isnil(t.tc) { return typ_id }
	shape := flat.source_type_shape(clean_id)
	syntax := flat.source_type_syntax(clean_id)
	if shape.has(.pointer) || shape.has(.mutable_type) || shape.has(.optional)
		|| shape.has(.variadic) || shape.has(.shared_type) || shape.has(.atomic_type)
		|| shape.has(.channel_type) || shape.has(.thread_type) || shape.has(.array_type) {
		kind := if shape.has(.pointer) {
			flat.SourceTypeComposition.pointer
		} else if shape.has(.mutable_type) {
			flat.SourceTypeComposition.mutable_parameter
		} else if shape.has(.option_type) {
			flat.SourceTypeComposition.option
		} else if shape.has(.result_type) {
			flat.SourceTypeComposition.result
		} else if shape.has(.variadic) {
			flat.SourceTypeComposition.variadic_parameter
		} else if shape.has(.shared_type) {
			flat.SourceTypeComposition.shared_type
		} else if shape.has(.atomic_type) {
			flat.SourceTypeComposition.atomic_type
		} else if shape.has(.channel_type) {
			flat.SourceTypeComposition.channel
		} else if shape.has(.thread_type) {
			flat.SourceTypeComposition.thread
		} else {
			flat.SourceTypeComposition.array
		}
		return flat.source_type_composition_one(kind, t.qualify_specialized_signature_type_text(syntax.raw_base(), decl))
	}
	if shape.has(.function_compact) || shape.has(.function_spaced) {
		if !syntax.function_closed() { return clean_id }
		mut parts := []u32{cap: syntax.function_params().len + 1}
		for param in syntax.function_params() {
			parts << t.qualify_specialized_signature_type_text(flat.source_type_syntax(param).legacy_parameter_mut_type(), decl)
		}
		qualified_ret := t.qualify_specialized_signature_type_text(syntax.function_result(), decl)
		if qualified_ret != 0 {
			parts << qualified_ret
			return flat.source_type_composition(.function_type_spaced, parts)
		}
		return flat.source_type_composition(.void_function_type_spaced, parts)
	}
	if shape.has(.map_type) && syntax.bracket_closed() {
		key := t.qualify_specialized_signature_type_text(syntax.bracket_raw_inner(), decl)
		value := t.qualify_specialized_signature_type_text(syntax.raw_base(), decl)
		return flat.source_type_composition_two(.map, key, value)
	}
	if shape.has(.leading_bracket) && syntax.bracket_closed() {
		return flat.source_type_composition_two(.concat, syntax.bracket_prefix(), t.qualify_specialized_signature_type_text(syntax.raw_base(), decl))
	}
	if fixed := t.canonical_suffix_fixed_array_arg_id(clean_id) {
		return t.qualify_specialized_signature_type_text(fixed, decl)
	}
	if syntax.tuple_like() && syntax.tuple_contains_comma() {
		mut parts := []u32{cap: syntax.tuple_components().len}
		for part in syntax.tuple_components() {
			parts << t.qualify_specialized_signature_type_text(part, decl)
		}
		return flat.source_type_composition(.multi_return, parts)
	}
	if syntax.generic_base() != clean_id && syntax.generic_base() != 0 && syntax.bracket_closed() {
		mut parts := []u32{cap: syntax.bracket_generic_parts().len + 1}
		parts << t.qualify_specialized_signature_type_text(syntax.generic_base(), decl)
		for part in syntax.bracket_generic_parts() {
			parts << t.qualify_specialized_signature_type_text(part, decl)
		}
		return flat.source_type_composition_two(.concat, flat.source_type_composition(.generic, parts), syntax.bracket_suffix())
	}
	if shape.has(.dotted) || types.is_builtin_type_name_id(clean_id)
		|| clean_id in [flat.source_type_void, flat.source_type_none, flat.source_type_nil,
			flat.source_type_id('C'), flat.source_type_id('JS')] {
		return clean_id
	}
	if selective := t.selective_signature_type_symbol_id(decl.file, clean_id) {
		return flat.source_type_id(selective)
	}
	return clean_id
}

fn (t &Transformer) selective_signature_type_symbol(file string, name string) ?string {
	return t.selective_signature_type_symbol_id(file, flat.source_type_id(name))
}

fn (t &Transformer) selective_signature_type_symbol_id(file string, name_id u32) ?string {
	if isnil(t.tc) || file == '' || flat.source_type_shape(name_id).has(.dotted) {
		return none
	}
	candidates := t.tc.file_selective_imports[file_import_key(file, flat.source_type_name(name_id))] or { return none }
	for candidate in candidates {
		candidate_id := flat.source_type_existing_id(candidate) or { continue }
		if t.tc.type_aliases.contains(candidate_id) || candidate_id in t.tc.structs
			|| candidate_id in t.tc.interface_names || candidate_id in t.tc.flag_enums
			|| candidate_id in t.tc.enum_names || t.tc.sum_types.contains(candidate_id) {
			return candidate
		}
	}
	return none
}

fn specialized_generic_fn_signature_aliases(decl GenericFnDecl, args_ids []u32) []string {
	if !decl.node.value.contains('.') {
		return []string{}
	}
	receiver := generic_fn_decl_base_value(decl.node.value).all_before_last('.')
	method := decl.node.value.all_after_last('.')
	if receiver.len == 0 || method.len == 0 {
		return []string{}
	}
	flat_receiver := '${receiver}_${generic_type_suffixes(args_ids)}'
	qflat_receiver := transform_qualified_fn_name(decl.module, flat_receiver)
	return [
		'${flat_receiver}.${method}',
		'${qflat_receiver}.${method}',
		c_name('${flat_receiver}.${method}'),
		c_name('${qflat_receiver}.${method}'),
		'${c_name(flat_receiver)}__${c_name(method)}',
		c_name('${decl.module}.${flat_receiver}.${method}'),
	]
}

fn (mut t Transformer) rewrite_generic_call_sites(decls map[string]GenericFnDecl, call_sites []GenericCallSite) []int {
	mut rewritten := []int{cap: call_sites.len}
	for site in call_sites {
		i := int(site.id)
		if i < 0 || i >= t.a.nodes.len {
			continue
		}
		node := t.a.nodes[i]
		if node.kind != .call {
			continue
		}
		decl_key, args_source_ids := t.cached_generic_call_specialization(site.id, node, site.module, decls) or {
			continue
		}

		decl := decls[decl_key] or { continue }
		if !t.call_has_source_generic_args(node) && t.generic_args_contain_alias(args_source_ids, decl.module) {
			t.set_node_call_type_args(i, args_source_ids)
		}
		concrete_args_source_ids := t.canonical_generic_specialization_args(args_source_ids)

		if t.generic_decl_is_receiver_method(decl.node)
			&& !t.generic_call_is_static_assoc_selector(node, decl) {
			t.rewrite_generic_method_call(site.id, node, decl, concrete_args_source_ids)
		} else {
			t.rewrite_generic_plain_call(site.id, node, decl, concrete_args_source_ids)
		}
		rewritten << i
	}
	return rewritten
}

// collect_generic_refresh_call_parents indexes only the parent nodes whose
// result types can change when their child call is rewritten. Scanning each
// AST range once keeps later refreshes independent of the full specialized tree.
// Ordinary factories returning generic function aliases also need normalization,
// even though their calls are not rewritten as generic specializations.
fn (mut t Transformer) collect_generic_refresh_call_parents(start int, end int, mut parents map[int][]int, mut alias_calls []int) {
	if start < 0 || start >= end {
		return
	}
	limit := int_min(end, t.a.nodes.len)
	for i in start .. limit {
		node := t.a.nodes[i]
		if node.kind == .call && node.children_count > 0 && !isnil(t.tc) {
			callee := t.a.child_node(&node, 0)
			if callee.kind == .ident && t.fn_ret_type_is_generic_app(callee.value_text()) {
				alias_calls << i
			}
		}
		if node.kind == .or_expr {
			if node.children_count > 0 && t.a.child_node(&node, 0).kind == .call {
				call_id := t.a.child(&node, 0)
				parents[int(call_id)] << i
			}
		} else if node.kind == .decl_assign && node.children_count == 2 {
			rhs_id := t.a.child(&node, 1)
			if int(rhs_id) >= 0 && int(rhs_id) < t.a.nodes.len {
				rhs := t.a.nodes[int(rhs_id)]
				if rhs.kind == .call {
					parents[int(rhs_id)] << i
				} else if rhs.kind == .or_expr && rhs.children_count > 0 {
					call_id := t.a.child(&rhs, 0)
					parents[int(call_id)] << i
				}
			}
		}
	}
}

fn generic_refresh_nodes_for_calls(call_ids []int, parents map[int][]int) []int {
	mut seen := map[int]bool{}
	mut nodes := []int{cap: call_ids.len * 2}
	for call_id in call_ids {
		if !seen[call_id] {
			seen[call_id] = true
			nodes << call_id
		}
		for parent_id in parents[call_id] {
			if !seen[parent_id] {
				seen[parent_id] = true
				nodes << parent_id
			}
		}
	}
	nodes.sort()
	return nodes
}

fn (mut t Transformer) refresh_decl_assign_types_after_generic_rewrite(nodes []int) bool {
	mut changed := false
	// Whether a callee returns a generic application depends on its name alone,
	// and the same names are called over and over: remember, by the address of
	// the name, those that do not.
	mut plain_callee_ptrs := []voidptr{len: 4096}
	mut plain_callee_lens := []int{len: 4096}
	// A later specialization can update a call whose parent was indexed in an
	// earlier pass; generic_refresh_nodes_for_calls adds that parent here.
	for i in nodes {
		if i < 0 || i >= t.a.nodes.len {
			continue
		}
		if t.a.nodes[i].kind !in [.call, .or_expr, .decl_assign] {
			continue
		}
		// Only candidate nodes need copies before AST rewriting.
		node := t.a.nodes[i]
		if node.kind == .call && node.children_count > 0 && !isnil(t.tc) {
			callee := t.a.child_node(&node, 0)
			if callee.kind == .ident && callee.value_text().len > 0 {
				slot := int((uint(callee.value_text().str) >> 3) & 4095)
				if plain_callee_ptrs[slot] == voidptr(callee.value_text().str)
					&& plain_callee_lens[slot] == callee.value_text().len {
					continue
				}
				if !t.fn_ret_type_is_generic_app(callee.value_text()) {
					plain_callee_ptrs[slot] = voidptr(callee.value_text().str)
					plain_callee_lens[slot] = callee.value_text().len
					continue
				}
			}
		}
		if node.kind == .call {
			if concrete_type_source_id := t.concrete_fn_alias_call_return_type(i, node) {
				if node.typ != concrete_type_source_id {
					t.set_node_typ(i, concrete_type_source_id)
					t.clear_typechecker_node_cache(i)
					changed = true
				}
			}
		}
		if node.kind == .or_expr && node.children_count > 0 {
			child := t.a.child_node(&node, 0)
			if child.kind == .call && flat.source_type_shape(child.typ).has(.optional)
				&& flat.source_type_syntax(child.typ).raw_base() != 0 {
				base_type := flat.source_type_syntax(child.typ).raw_base()
				if base_type != 0 && !t.generic_arg_is_unresolved(base_type) {
					if node.typ != base_type {
						t.set_node_typ(i, base_type)
						changed = true
					}
					// The node may have inherited the shared template's semantic cache
					// even when its substituted text is already correct.
					t.clear_typechecker_node_cache(i)
				}
			}
			continue
		}
		if node.kind != .decl_assign || node.children_count != 2 {
			continue
		}
		lhs_id := t.a.child(&node, 0)
		rhs_id := t.a.child(&node, 1)
		if int(lhs_id) < 0 || int(rhs_id) < 0 || int(rhs_id) >= t.a.nodes.len {
			continue
		}
		rhs := t.a.nodes[int(rhs_id)]
		mut specialized_call_or := false
		mut concrete_rhs_type := rhs.typ
		if rhs.kind == .or_expr {
			if rhs.children_count == 0 {
				continue
			}
			call_id := t.a.child(&rhs, 0)
			call := t.a.nodes[int(call_id)]
			if call.kind != .call {
				continue
			}
			if call.children_count > 0 {
				callee := t.a.child_node(&call, 0)
				specialized_call_or = callee.kind == .ident
					&& t.generic_callee_is_specialization(callee.value_text())
			}
			// Unless the callee is a specialization, a usable annotation stays as it
			// is (see below); do not resolve the result type of the call for it.
			if !specialized_call_or && decl_type_is_usable(node.typ)
				&& !t.generic_arg_is_unresolved(node.typ) {
				continue
			}
			_, concrete_rhs_type_source_id := t.or_expr_types(call_id, rhs.typ)
			concrete_rhs_type = concrete_rhs_type_source_id
		} else if rhs.kind != .call {
			continue
		}
		if concrete_rhs_type == 0 || t.generic_arg_is_unresolved(concrete_rhs_type) {
			continue
		}
		// A specialized generic result can replace an otherwise usable stale annotation.
		if !(specialized_call_or && node.typ != concrete_rhs_type)
			&& decl_type_is_usable(node.typ)
			&& !t.generic_arg_is_unresolved(node.typ) {
			continue
		}
		decl_type_source_id_2 := concrete_rhs_type

		t.set_node_typ(i, decl_type_source_id_2)
		changed = true
		lhs := t.a.nodes[int(lhs_id)]
		if lhs.kind == .ident {
			t.set_node_typ(int(lhs_id), decl_type_source_id_2)
			t.set_var_type_with_raw(lhs.value_text(), concrete_rhs_type, decl_type_source_id_2)
		}
	}
	return changed
}

// fn_ret_type_is_generic_app tells whether the checker's return type of the
// function `name` is a generic application, the first test of
// concrete_fn_alias_call_return_type.
fn (t &Transformer) fn_ret_type_is_generic_app(name string) bool {
	ret := t.tc.fn_ret_types[name] or { return false }
	return flat.source_type_syntax(ret.source_type_id()).generic_app_valid()
}

fn (mut t Transformer) concrete_fn_alias_call_return_type(id int, node flat.Node) ?u32 {
	if node.children_count == 0 || isnil(t.tc) {
		return none
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .ident || callee.value_text().len == 0 {
		return none
	}
	ret := t.tc.fn_ret_types[callee.value_text()] or { return none }
	ret_source_id := ret.source_type_id()
	ret_syntax := flat.source_type_syntax(ret_source_id)
	if !ret_syntax.generic_app_valid() {
		return none
	}
	// Most call identifiers are ordinary functions. Avoid the contextual scope
	// lookup until the return type proves this can be a generic function alias.
	if (t.var_type(callee.value_text())) != 0 {
		return none
	}
	module_name := t.node_module_or(id, t.cur_module)
	if !t.generic_arg_is_alias_name_id(ret_syntax.generic_app_base(), module_name) {
		return none
	}
	old_module := t.cur_module
	old_file := t.cur_file
	t.cur_module = module_name
	t.cur_file = t.node_file_or(id, old_file)
	concrete_source_id := t.normalize_type_alias(ret_source_id)
	t.cur_module = old_module
	t.cur_file = old_file
	if concrete_source_id == ret_source_id || (!flat.source_type_shape(concrete_source_id).has(.function_compact) && !flat.source_type_shape(concrete_source_id).has(.function_spaced)) {
		return none
	}
	return concrete_source_id
}

fn (mut t Transformer) concrete_generic_call_return_type(id flat.NodeId, node flat.Node) u32 {
	if t.skip_generics {
		return u32(0)
	}
	if node.kind != .call || node.children_count == 0 {
		return u32(0)
	}
	callee := t.a.child_node(&node, 0)
	mut specialized_node_type := u32(0)
	// The recorded type of the call is read first: most calls carry none, and telling a
	// specialized callee apart takes several table lookups.
	if callee.kind == .ident && generic_inference_arg_type_usable(node.typ)
		&& (callee.value_text().contains('_T_') || t.generic_callee_is_specialization(callee.value_text())) {
		concrete_node_type_source_id := t.normalize_type_alias(node.typ)
		concrete_node_type_source_id_2 := concrete_node_type_source_id

		if !t.generic_arg_is_unresolved(concrete_node_type_source_id) {
			specialized_node_type = concrete_node_type_source_id_2
		}
	}
	decls := t.cached_generic_fn_decls()
	if decls.len == 0 {
		return specialized_node_type
	}
	decl_key := t.generic_call_decl_key(id, node, t.cur_module, decls) or {
		return specialized_node_type
	}
	decl := decls[decl_key] or { return specialized_node_type }
	if callee.kind == .ident {
		if exact_source_ids := t.recorded_generic_specialization_args(callee.value_text()) {
			ret_source_id := t.specialized_fn_return_type_text(decl, exact_source_ids)

			if ret_source_id != 0 && !t.generic_arg_is_unresolved(ret_source_id) {
				return t.normalize_type_alias(ret_source_id)
			}
		}
	}
	if specialized_node_type != 0 {
		return specialized_node_type
	}
	mut args := []u32{}
	if explicit_source_ids := t.explicit_generic_call_args(node, t.cur_module) {
		args = (t.infer_generic_call_args_with_explicit(decl, id, node, t.cur_module, explicit_source_ids) or {
			return u32(0)
		}).clone()
	} else {
		// Preserve the expected return context while normalizing an implicit
		// generic call. Literal arguments alone may still carry their untyped
		// placeholder in a combined test program, while the enclosing array has
		// already fixed the concrete return type.
		args = (t.infer_generic_call_args(decl, id, node, t.cur_module) or { return u32(0) }).clone()
	}
	if args.len == 0 || t.generic_args_have_placeholders(args) {
		return u32(0)
	}
	t.generic_call_spec_cache[int(id)] = GenericCallSpec{
		decl_key: decl_key
		args:     flat.TypeTextList{ ids: args.clone() }
	}
	if !t.call_has_source_generic_args(node) && t.generic_args_contain_alias(args, t.cur_module) {
		t.set_node_call_type_args(int(id), args)
	}
	ret_source_id_2 := t.specialized_fn_return_type_text(decl, args)

	if ret_source_id_2 == 0 || t.generic_arg_is_unresolved(ret_source_id_2) {
		return u32(0)
	}
	return t.normalize_type_alias(ret_source_id_2)
}

// rewrite_contextual_generic_plain_call applies a specialization discovered
// only after the enclosing expression supplied its expected return type.
fn (mut t Transformer) rewrite_contextual_generic_plain_call(id flat.NodeId, node flat.Node) bool {
	if t.skip_generics || node.kind != .call || node.children_count == 0 {
		return false
	}
	spec := t.generic_call_spec_cache[int(id)] or { return false }
	decls := t.cached_generic_fn_decls()
	decl := decls[spec.decl_key] or { return false }
	if t.generic_decl_is_receiver_method(decl.node) {
		return false
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind == .ident && t.generic_callee_is_specialization(callee.value_text()) {
		return false
	}
	concrete_args_source_ids := t.canonical_generic_specialization_args(spec.args.ids)

	if !t.generic_specialization_registered(decl, concrete_args_source_ids)
		&& !t.generic_specialization_in_progress(decl, concrete_args_source_ids) {
		// The ordinary function transform may discover a concrete generic call while
		// a match smartcast or other function-local context is active. Emitting the
		// specialization recursively would transform another function body on this
		// same Transformer and clobber that context before the call arguments are
		// lowered. Queue it; the monomorphization pass materializes the exact callee.
		t.request_generic_fn_specialization(decl, concrete_args_source_ids)
	}
	t.rewrite_generic_plain_call(id, node, decl, concrete_args_source_ids)
	return true
}

fn (mut t Transformer) raw_generic_call_return_type(id flat.NodeId, node flat.Node) u32 {
	if t.skip_generics {
		return u32(0)
	}
	if node.kind != .call || node.children_count == 0 {
		return u32(0)
	}
	decls := t.cached_generic_fn_decls()
	if decls.len == 0 {
		return u32(0)
	}
	decl_key := t.generic_call_decl_key(id, node, t.cur_module, decls) or { return u32(0) }
	decl := decls[decl_key] or { return u32(0) }
	callee := t.a.child_node(&node, 0)
	if callee.kind == .ident {
		if exact_source_ids := t.recorded_generic_specialization_args(callee.value_text()) {
			ret_source_id := t.specialized_fn_return_display_type_text(decl, exact_source_ids)

			if ret_source_id != 0 && !t.generic_arg_is_unresolved(ret_source_id) {
				return ret_source_id
			}
		}
	}
	explicit_source_ids := t.explicit_generic_call_args(node, t.cur_module) or { return u32(0) }

	args_source_ids := t.infer_generic_call_args_with_explicit(decl, id, node, t.cur_module, explicit_source_ids) or {
		return u32(0)
	}

	if args_source_ids.len == 0 || t.generic_args_have_placeholders(args_source_ids) {
		return u32(0)
	}
	ret_source_id_2 := t.specialized_fn_return_display_type_text(decl, args_source_ids)

	if ret_source_id_2 == 0 || t.generic_arg_is_unresolved(ret_source_id_2) {
		return u32(0)
	}
	return ret_source_id_2
}

fn (mut t Transformer) concrete_generic_call_param_types(id flat.NodeId, node flat.Node) ?[]types.Type {
	if t.skip_generics {
		return none
	}
	if node.kind != .call || node.children_count == 0 || isnil(t.tc) {
		return none
	}
	decls := t.cached_generic_fn_decls()
	if decls.len == 0 {
		return none
	}
	decl_key := t.generic_call_decl_key(id, node, t.cur_module, decls) or { return none }
	decl := decls[decl_key] or { return none }
	callee := t.a.node(t.a.child(&node, 0))
	mut args := []u32{}
	if callee.kind == .ident {
		if exact_source_ids := t.recorded_generic_specialization_args(callee.value_text()) {
			args = exact_source_ids.clone()
		}
	}
	if args.len == 0 {
		if explicit_source_ids := t.explicit_generic_call_args(node, t.cur_module) {
			args = (t.infer_generic_call_args_with_explicit(decl, id, node, t.cur_module, explicit_source_ids) or {
				return none
			}).clone()
		} else {
			args = (t.infer_generic_call_args_from_params(decl, id, node, t.cur_module) or {
				return none
			}).clone()
		}
	}
	if args.len == 0 || t.generic_args_have_placeholders(args) {
		return none
	}
	t.generic_call_spec_cache[int(id)] = GenericCallSpec{
		decl_key: decl_key
		args:     flat.TypeTextList{ ids: args.clone() }
	}
	if !t.call_has_source_generic_args(node) && t.generic_args_contain_alias(args, t.cur_module) {
		t.set_node_call_type_args(int(id), args)
	}
	params := t.generic_fn_param_names(decl.node, decl.module)
	mut result := []types.Type{}
	for i in 0 .. decl.node.children_count {
		child := t.a.node(t.a.child(&decl.node, i))
		if child.kind != .param {
			if t.prefix_param_scan {
				break
			}
			continue
		}
		param_type := explicit_mut_pointer_param_type_text(child, t.specialized_signature_type_text(decl, child.typ, args, params))
		parsed := t.tc.parse_canonical_type(param_type)
		result << if child.is_mut && child.op != .amp {
			types.mut_param_semantic_type(parsed)
		} else {
			parsed
		}
	}
	if result.len == 0 {
		return none
	}
	return result
}

fn (mut t Transformer) concrete_generic_call_param_type_names(id flat.NodeId, node flat.Node) ?[]u32 {
	if t.skip_generics {
		return none
	}
	if node.kind != .call || node.children_count == 0 || isnil(t.tc) {
		return none
	}
	decls := t.cached_generic_fn_decls()
	if decls.len == 0 {
		return none
	}
	decl_key, args_source_ids := t.cached_generic_call_specialization(id, node, t.cur_module, decls) or {
		return none
	}

	decl := decls[decl_key] or { return none }
	if args_source_ids.len == 0 || t.generic_args_have_placeholders(args_source_ids) {
		return none
	}
	mut result := t.specialized_generic_call_param_type_texts(decl, args_source_ids).clone()
	mut param_idx := 0
	for i in 0 .. decl.node.children_count {
		child := t.a.child_node(&decl.node, i)
		if child.kind != .param {
			if t.prefix_param_scan {
				break
			}
			continue
		}
		if param_idx < result.len && child.is_mut && child.op != .amp {
			parsed := t.tc.parse_canonical_type(result[param_idx])
			slot := types.mut_param_semantic_type(parsed)
			// Keep original alias/variadic spellings unless the optional ABI changes.
			if !types.semantic_types_equal(slot, parsed) {
				result[param_idx] = t.semantic_type_name(slot)
			}
		}
		param_idx++
	}
	return result
}

fn (mut t Transformer) concrete_generic_call_is_variadic(id flat.NodeId, node flat.Node) bool {
	if t.skip_generics || node.kind != .call || node.children_count == 0 {
		return false
	}
	decls := t.cached_generic_fn_decls()
	if decls.len == 0 {
		return false
	}
	decl_key, _ := t.cached_generic_call_specialization(id, node, t.cur_module, decls) or {
		return false
	}
	decl := decls[decl_key] or { return false }
	mut last_param := u32(0)
	for i in 0 .. decl.node.children_count {
		child := t.a.child_node(&decl.node, i)
		if child.kind == .param {
			last_param = child.typ
		} else if t.prefix_param_scan {
			break
		}
	}
	return generic_param_text_is_variadic(last_param)
}

fn (mut t Transformer) concrete_generic_call_is_method(id flat.NodeId) bool {
	spec := t.generic_call_spec_cache[int(id)] or { return false }
	decl := t.cached_generic_fn_decls()[spec.decl_key] or { return false }
	return t.generic_decl_is_receiver_method(decl.node)
}

fn (mut t Transformer) cached_generic_fn_decls() map[string]GenericFnDecl {
	if t.skip_generics {
		if !t.generic_fn_decls_ready {
			t.generic_fn_decls_cache = map[string]GenericFnDecl{}
			t.generic_receiver_methods_by_name = map[string][]string{}
			t.generic_fn_call_names = map[string]bool{}
			t.generic_fn_decls_ready = true
		}
		return t.generic_fn_decls_cache
	}
	if !t.generic_fn_decls_ready {
		t.generic_fn_decls_cache = t.collect_generic_fn_decls()
		t.build_generic_receiver_method_index()
		t.generic_fn_decls_ready = true
	}
	return t.generic_fn_decls_cache
}

fn (mut t Transformer) build_generic_receiver_method_index() {
	mut by_name := map[string][]string{}
	mut call_names := map[string]bool{}
	for key, decl in t.generic_fn_decls_cache {
		if !t.generic_decl_is_receiver_method(decl.node) {
			for name in [key, decl.node.value, key.all_after_last('.'),
				decl.node.value.all_after_last('.')] {
				for spelling in generic_call_name_spellings(name) {
					call_names[spelling] = true
				}
			}
			continue
		}
		method := key.all_after_last('.')
		for spelling in generic_call_name_spellings(method) {
			by_name[spelling] << key
			call_names[spelling] = true
		}
	}
	t.generic_receiver_methods_by_name = by_name.move()
	t.generic_fn_call_names = call_names.move()
	mut tails := map[string]bool{}
	mut static_methods := map[string]bool{}
	for key, _ in t.generic_fn_decls_cache {
		for spelling in generic_call_name_spellings(short_name_view(key)) {
			tails[spelling] = true
		}
		if _, method := flat.decode_static_type_method_name(key) {
			for spelling in generic_call_name_spellings(method) {
				static_methods[spelling] = true
			}
		}
	}
	t.generic_decl_name_tails = tails.move()
	t.generic_static_method_names = static_methods.move()
	t.generic_decl_name_tails_count = t.generic_fn_decls_cache.len
}

// generic_name_cannot_spell_decl reports whether no spelling derived from `name` is a
// key of the generic declaration table: `name` carries none of the markers that select a
// rewritten spelling (`@` escapes and static method names, `__` flattened receivers), and
// no key ends with its final segment. Qualifying a name, dropping its generic arguments
// and toggling the `@` escape all keep that final segment.
@[direct_array_access]
fn (t &Transformer) generic_name_cannot_spell_decl(name string) bool {
	if name.len == 0 {
		return false
	}
	mut tail_start := 0
	for i in 0 .. name.len {
		c := name[i]
		if c == `@` || (c == `_` && i + 1 < name.len && name[i + 1] == `_`) {
			return false
		}
		if c == `.` {
			tail_start = i + 1
		}
	}
	return !t.generic_decl_name_tails[unsafe { name.substr_unsafe(tail_start, name.len) }]
}

// generic_call_cannot_name_decl is an exact, allocation-free rejection for
// unshadowed_generic_call_decl_key. Every key that function can return is spelled from
// the checker's resolved name or from the callee's own text, so a call whose names
// cannot spell a declaration skips the candidate lists it would build to find that out.
// Most calls in a program name no generic declaration at all.
fn (t &Transformer) generic_call_cannot_name_decl(resolved string, has_resolved bool, callee flat.Node, decls map[string]GenericFnDecl) bool {
	if t.generic_decl_name_tails_count != decls.len || decls.len != t.generic_fn_decls_cache.len {
		return false
	}
	if has_resolved && !t.generic_name_cannot_spell_decl(resolved) {
		return false
	}
	if callee.kind == .ident {
		// `_T_` marks a specialized plain call, decoded without the name tables.
		return t.generic_name_cannot_spell_decl(callee.value_text()) && !callee.value_text().contains('_T_')
	}
	if callee.kind == .selector {
		return t.generic_name_cannot_spell_decl(callee.value_text())
			&& !t.generic_static_method_names[callee.value_text()]
	}
	return true
}

fn generic_call_name_spellings(name string) []string {
	if name == '' {
		return []string{}
	}
	short := name.all_after_last('.')
	if short.starts_with('@') {
		unescaped := name[..name.len - short.len] + short[1..]
		return [name, unescaped]
	}
	escaped := name[..name.len - short.len] + '@' + short
	return [name, escaped]
}

// call_name_can_target_generic is an allocation-free superset filter for the
// monomorph scan. Most calls in a large program cannot name any generic
// declaration; rejecting them before module/type inference avoids doing the
// expensive resolution path for every ordinary call node.
fn (t &Transformer) call_name_can_target_generic(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	mut callee := t.a.nodes[int(t.a.child(&node, 0))]
	mut has_explicit_generic_args := false
	if callee.kind == .index && callee.children_count > 0 && callee.value != 'range' {
		has_explicit_generic_args = true
		callee = t.a.nodes[int(t.a.child(&callee, 0))]
	}
	if callee.kind !in [.ident, .selector] || callee.value_text().len == 0 {
		return false
	}
	return has_explicit_generic_args || t.call_is_normalized_explicit_generic_call(node)
		|| t.generic_fn_call_names[callee.value_text()]
		|| callee.value_text().contains('_T_')
		|| t.generic_callee_is_specialization(callee.value_text())
}

fn (t &Transformer) synthetic_recorded_generic_call(node flat.Node) bool {
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	callee := t.a.child_node(&node, 0)
	return callee.kind == .ident
		&& (callee.value_text().contains('_T_') || t.generic_callee_is_specialization(callee.value_text()))
}

// synthetic_generic_call_has_exact_identity reports generated calls whose callee
// encodes its concrete type arguments. Such calls do not have a source-file marker,
// but unlike unresolved synthetic calls they can be monomorphized without guessing
// their declaration module or generic arguments.
fn (t &Transformer) synthetic_generic_call_has_exact_identity(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .ident || callee.value_text().len == 0 {
		return false
	}
	return (callee.value_text().contains('[') && callee.value_text().contains(']'))
		|| callee.value_text().contains('_T_')
		|| t.generic_callee_is_specialization(callee.value_text())
}

// call_may_target_generic is call_name_can_target_generic(node) ||
// synthetic_generic_call_has_exact_identity(node), with the specialization lookup
// that both end with, their costly part, done once.
fn (t &Transformer) call_may_target_generic(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	direct := t.a.nodes[int(t.a.child(&node, 0))]
	if direct.kind == .ident && direct.value_text().len > 0 && ((direct.value_text().contains('[')
		&& direct.value_text().contains(']')) || direct.value_text().contains('_T_')) {
		return true
	}
	mut callee := direct
	mut has_explicit_generic_args := false
	if callee.kind == .index && callee.children_count > 0 && callee.value != 'range' {
		has_explicit_generic_args = true
		callee = t.a.nodes[int(t.a.child(&callee, 0))]
	}
	if callee.kind !in [.ident, .selector] || callee.value_text().len == 0 {
		return false
	}
	if has_explicit_generic_args || t.call_is_normalized_explicit_generic_call(node)
		|| t.generic_fn_call_names[callee.value_text()] || callee.value_text().contains('_T_') {
		return true
	}
	// Here `callee` is the call's own child: the one both tests look up.
	return t.generic_callee_is_specialization(callee.value_text())
}

// generic_fn_value_base_may_match is a superset test for whether the base of an
// index can name a generic function given type arguments. Every candidate that
// explicit_generic_fn_value_decl_candidates forms is spelled from the base's text,
// unless the checker resolved the value itself.
fn (t &Transformer) generic_fn_value_base_may_match(id flat.NodeId, base_id flat.NodeId, base flat.Node, decls map[string]GenericFnDecl) bool {
	if base.kind != .ident && base.kind != .selector {
		return false
	}
	if t.generic_decl_name_tails_count != decls.len || decls.len != t.generic_fn_decls_cache.len
		|| !t.generic_name_cannot_spell_decl(base.value_text()) {
		return true
	}
	if base.kind != .ident || isnil(t.tc) {
		return false
	}
	if _ := t.tc.resolved_fn_value_name(id) {
		return true
	}
	if _ := t.tc.resolved_fn_value_name(base_id) {
		return true
	}
	return false
}

fn (mut t Transformer) infer_generic_call_args_from_params(decl GenericFnDecl, call_id flat.NodeId, node flat.Node, call_module string) ?[]u32 {
	param_names := t.generic_fn_param_names(decl.node, decl.module)
	if param_names.len == 0 {
		return none
	}
	is_receiver := t.generic_decl_is_receiver_method(decl.node)
		&& !t.generic_call_is_static_assoc_selector(node, decl)
	mut inferred := map[string]u32{}
	mut param_idx := 0
	param_count := t.generic_decl_param_count(decl.node)
	for i in 0 .. decl.node.children_count {
		child := t.a.child_node(&decl.node, i)
		if child.kind != .param {
			if t.prefix_param_scan {
				break
			}
			continue
		}
		is_recv_param := is_receiver && param_idx == 0
		if generic_param_text_is_variadic(child.typ) && param_idx == param_count - 1 {
			elem_param_type := generic_variadic_elem_param_text(child.typ)
			arg_ids := t.generic_call_arg_ids_from_param(node, param_idx, is_receiver)
			for arg_id in arg_ids {
				t.infer_generic_variadic_call_arg(child.typ, elem_param_type, arg_id, mut inferred)
			}
			param_idx++
			continue
		}
		arg_id := t.generic_call_arg_id_for_param(node, param_idx, is_receiver) or {
			param_idx++
			continue
		}
		mut inferred_arg_type := t.generic_call_arg_type_for_inference(arg_id)
		inference_param_type := generic_inference_param_type(child)
		if !flat.source_type_query(inference_param_type).has(.generic_fn_name) {
			inferred_arg_type = t.generic_composite_inference_alias_type(inferred_arg_type,
				call_module)
		}
		if child.is_mut && child.op == .amp && flat.source_type_shape(inferred_arg_type).has(.pointer) {
			// The explicit `mut p &T` marker addresses the caller's pointer slot;
			// it is ABI indirection, not part of T's inferred source type.
			inferred_arg_type = flat.source_type_syntax(inferred_arg_type).raw_base()
		}
		if is_recv_param && t.call_is_selector_form(node)
			&& t.generic_receiver_needs_decl_type_fallback(child.typ, inferred_arg_type) {
			recv := t.a.nodes[int(arg_id)]
			if recv.kind == .ident {
				lookup_before := if node.pos.offset > 0 { node } else { recv }
				if decl_type_source_id := t.local_decl_type_before_pos(recv.value_text(), lookup_before) {
					inferred_arg_type = t.generic_receiver_decl_inference_type(child.typ, decl_type_source_id, call_module)
				} else if decl_type_source_id := t.local_decl_type_before(recv.value_text(), call_id) {
					inferred_arg_type = t.generic_receiver_decl_inference_type(child.typ, decl_type_source_id, call_module)
				}
			}
		}
		if (child.is_mut || flat.source_type_shape(child.typ).has(.mutable_type)) && flat.source_type_shape(inferred_arg_type).has(.pointer) {
			// A mutable parameter's leading pointer is its storage ABI, whether the
			// generic placeholder is direct (`T`) or nested (`Container[T]`).
			inferred_arg_type = flat.source_type_syntax(inferred_arg_type).raw_base()
		}
		arg_type := generic_arg_type_for_param(inference_param_type, inferred_arg_type)
		if arg_type != 0 {
			infer_generic_type_args(inference_param_type, arg_type, mut inferred)
			t.infer_generic_sum_variant_args(child.typ, arg_type, mut inferred)
			if is_recv_param {
				t.infer_generic_receiver_suffix_args(child.typ, arg_type, mut inferred)
				t.infer_generic_embedded_receiver_args(child.typ, arg_type, mut inferred)
			}
		}
		t.infer_generic_sum_literal_args(child.typ, arg_id, mut inferred)
		t.infer_generic_struct_init_args(child.typ, arg_id, mut inferred)
		t.infer_generic_short_struct_init_args(child.typ, arg_id, node, mut inferred)
		param_idx++
	}
	mut args := []u32{cap: param_names.len}
	for name in param_names {
		arg_source_id := inferred[name] or { return none }

		qualified_arg_source_id := t.qualify_specialized_signature_type_text(arg_source_id, GenericFnDecl{
			file: t.node_file_or(int(call_id), t.cur_file)
		})

		args << t.generic_arg_for_call_and_decl_module(qualified_arg_source_id, call_module, decl.module)
	}
	return args.clone()
}

fn (t &Transformer) generic_decl_param_count(node flat.Node) int {
	mut count := 0
	for i in 0 .. node.children_count {
		child := t.a.child_node(&node, i)
		if child.kind != .param {
			if t.prefix_param_scan {
				break
			}
			continue
		}
		count++
	}
	return count
}

fn generic_param_text_is_variadic(param_type_id u32) bool {
	return flat.source_type_shape(transform_trimmed_type_id(param_type_id)).has(.variadic)
}

fn generic_variadic_elem_param_text(param_type_id u32) u32 {
	clean := transform_trimmed_type_id(param_type_id)
	if flat.source_type_shape(clean).has(.variadic) {
		return transform_trimmed_type_id(flat.source_type_syntax(clean).raw_base())
	}
	return clean
}

fn (t &Transformer) generic_call_arg_ids_from_param(node flat.Node, param_idx int, is_receiver bool) []flat.NodeId {
	selector_form := is_receiver && t.call_is_selector_form(node)
	start := if selector_form { param_idx } else { param_idx + 1 }
	if start >= int(node.children_count) {
		return []flat.NodeId{}
	}
	mut ids := []flat.NodeId{cap: int(node.children_count) - start}
	for i in start .. node.children_count {
		ids << t.a.child(&node, i)
	}
	return ids
}

fn (mut t Transformer) infer_generic_variadic_call_arg(param_type_id u32, elem_param_type_id u32, arg_id flat.NodeId, mut inferred map[string]u32) {
	inference_param_type, value_id := t.generic_variadic_arg_for_inference(param_type_id, elem_param_type_id, arg_id)
	inferred_arg_type := t.generic_call_arg_type_for_inference(value_id)
	arg_type := generic_arg_type_for_param(inference_param_type, inferred_arg_type)
	if arg_type != 0 {
		infer_generic_type_args(inference_param_type, arg_type, mut inferred)
	}
	if inference_param_type == param_type_id {
		return
	}
	t.infer_generic_struct_init_args(elem_param_type_id, arg_id, mut inferred)
	// The short struct-init call syntax for a variadic generic struct param
	// (`f(a: x, b: y)` for `fn f[T](items ...Params[T])`) packs the element's
	// fields as bare `field_init` args rather than a wrapped `struct_init`, so
	// `infer_generic_struct_init_args` above cannot see them. Infer T from the
	// field's declared type the same way the non-variadic path does.
	t.infer_generic_field_init_type_arg(elem_param_type_id, arg_id, mut inferred)
}

fn (t &Transformer) generic_variadic_arg_for_inference(param_type_id u32, elem_param_type_id u32, arg_id flat.NodeId) (u32, flat.NodeId) {
	if spread_id := t.generic_call_spread_arg_child(arg_id) {
		return param_type_id, spread_id
	}
	if t.generic_call_arg_is_packed_variadic_tail(arg_id) {
		return param_type_id, arg_id
	}
	return elem_param_type_id, arg_id
}

// An array's dimensions alone do not distinguish one variadic element from
// a tail produced by an earlier call transformation.
fn (t &Transformer) generic_call_arg_is_packed_variadic_tail(arg_id flat.NodeId) bool {
	if int(arg_id) < 0 || int(arg_id) >= t.a.nodes.len {
		return false
	}
	if int(arg_id) in t.packed_variadic_tail_types {
		return true
	}
	arg := t.a.nodes[int(arg_id)]
	return arg.kind == .ident
		&& (arg.value_text() == t.cur_fn_variadic_param
			|| (!arg.pos.is_valid() && arg.value_text().starts_with('__varargs_')))
}

// infer_generic_field_init_type_arg infers generic type args from a single
// named `field_init` argument mapped onto a generic struct's declared field
// type (used by the variadic short struct-init call path).
fn (mut t Transformer) infer_generic_field_init_type_arg(param_type_id u32, field_id flat.NodeId, mut inferred map[string]u32) {
	if int(field_id) < 0 || int(field_id) >= t.a.nodes.len {
		return
	}
	field := t.a.nodes[int(field_id)]
	if field.kind != .field_init || field.value_text().len == 0 || field.children_count == 0 {
		return
	}
	mut struct_param_type := t.normalize_type_alias_chain(t.trimmed_type_id(param_type_id))
	if flat.source_type_shape(struct_param_type).has(.pointer) {
		struct_param_type = t.trimmed_type_id(t.trim_pointer_type(struct_param_type))
	}
	param_syntax := flat.source_type_syntax(struct_param_type)
	param_args := param_syntax.generic_app_args()
	if !param_syntax.generic_app_valid() || param_args.len == 0 {
		return
	}
	info := t.lookup_struct_info(param_syntax.generic_app_base()) or { return }
	mut field_type := u32(0)
	for struct_field in info.fields {
		if struct_field.name == field.value_text() {
			field_type = struct_field.typ
			break
		}
	}
	if field_type == 0 {
		return
	}
	// Rebase the struct's own generic parameter names onto the applied args so
	// e.g. `[]A` (from `struct Params[A]`) becomes `[]T` for `...Params[T]`.
	struct_params := t.struct_generic_params_for_base(param_syntax.generic_app_base())
	if struct_params.len > 0 && flat.TypeTextList.from_names(struct_params).ids != param_args {
		field_type = substitute_generic_type_text_with_params(field_type, param_args, struct_params)
	}
	value_id := t.a.child(&field, 0)
	value_type_source_id := t.generic_call_arg_type_for_inference(value_id)
	if value_type_source_id != 0 {
		infer_generic_type_args(field_type, value_type_source_id, mut inferred)
	}
}

fn (t &Transformer) generic_call_spread_arg_child(arg_id flat.NodeId) ?flat.NodeId {
	if int(arg_id) < 0 || int(arg_id) >= t.a.nodes.len {
		return none
	}
	arg := t.a.nodes[int(arg_id)]
	if arg.kind == .prefix && arg.value_text() == '...' && arg.children_count > 0 {
		return t.a.child(&arg, 0)
	}
	return none
}

fn (mut t Transformer) rewrite_generic_plain_call(id flat.NodeId, node flat.Node, decl GenericFnDecl, args_ids []u32) {
	if node.children_count > 0 {
		callee := t.a.child_node(&node, 0)
		if callee.kind == .ident && !t.generic_callee_is_specialization(callee.value_text())
			&& t.plain_concrete_callee_shadows_decl(callee.value_text(), decl) {
			return
		}
	}
	concrete_args_source_ids := t.canonical_generic_specialization_args(args_ids)
	spec_value := specialized_generic_fn_value(decl.node.value_text(), concrete_args_source_ids)
	spec_name := transform_qualified_fn_name(decl.module, spec_value)
	t.record_generic_specialization_args_for_names([spec_name, c_name(spec_name)], concrete_args_source_ids)
	ret_typ_source_id := t.specialized_fn_return_type_text(decl, concrete_args_source_ids)

	param_types_source_ids := t.specialized_generic_call_param_type_texts(decl, concrete_args_source_ids)

	mut children := []flat.NodeId{cap: int(node.children_count)}
	children << t.make_ident(spec_name)
	for i in 1 .. node.children_count {
		arg_id := t.a.child(&node, i)
		param_idx := i - 1
		param_type := (if param_idx < param_types_source_ids.len {
			param_types_source_ids[param_idx]
		} else {
			u32(0)
		})
		children << t.specialize_generic_fn_value_arg(arg_id, param_type, false) or {
			t.retype_generic_call_literal_arg(arg_id, param_type)
		}
	}
	t.append_missing_specialized_params_struct_args(mut children, param_types_source_ids.clone(), decl.module)
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	mut rewritten := flat.Node{
		kind:           .call
		op:             node.op
		children_start: start
		children_count: flat.child_count(children.len)
		pos:            node.pos
		typ:            ret_typ_source_id
	}
	rewritten.set_call_type_args(concrete_args_source_ids)
	t.set_node(int(id), rewritten)
	t.clear_resolved_call(id)
}

fn (t &Transformer) plain_concrete_callee_shadows_decl(name string, decl GenericFnDecl) bool {
	if name == '' || name.contains('.') {
		return false
	}
	if decl.key == name {
		return false
	}
	if decl.node.value == name || decl.key == transform_qualified_fn_name(decl.module, name) {
		return false
	}
	if !isnil(t.tc) {
		if t.concrete_fn_return_known(name) {
			return true
		}
		return false
	}
	return t.fn_ret_types.contains(name)
}

fn (t &Transformer) generic_callee_is_specialization(name string) bool {
	if !isnil(t.tc) && t.tc.specialized_generic_fns[name] {
		return true
	}
	if _ := t.recorded_generic_specialization_args(name) {
		return true
	}
	return false
}

fn (t &Transformer) synthetic_generic_call_has_exact_callee(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	callee := t.a.child_node(&node, 0)
	return callee.kind == .ident && t.generic_callee_is_specialization(callee.value_text())
}

fn (t &Transformer) concrete_fn_return_known(name string) bool {
	ret := t.tc.fn_ret_types[name] or { return false }
	return ret !is types.Unknown
}

fn (mut t Transformer) specialized_generic_call_param_type_texts(decl GenericFnDecl, args_ids []u32) []u32 {
	params := t.generic_fn_param_names(decl.node, decl.module)
	mut result := []u32{}
	for i in 0 .. decl.node.children_count {
		child := t.a.child_node(&decl.node, i)
		if child.kind != .param {
			if t.prefix_param_scan {
				break
			}
			continue
		}
		result << explicit_mut_pointer_param_type_text(child, t.specialized_call_target_type_text(decl, child.typ, args_ids, params))
	}
	return result.clone()
}

fn explicit_mut_pointer_param_type_text(param flat.Node, typ_id u32) u32 {
	if param.is_mut && param.op == .amp && flat.source_type_shape(typ_id).has(.pointer) {
		return flat.source_type_composition_one(.pointer, typ_id)
	}
	return typ_id
}

fn mutable_fn_value_param_type_text(param flat.Node, slot_type_id u32) u32 {
	if !param.is_mut {
		return slot_type_id
	}
	value_type_id := if flat.source_type_shape(slot_type_id).has(.pointer) {
		flat.source_type_syntax(slot_type_id).raw_base()
	} else {
		slot_type_id
	}
	return flat.source_type_composition_one(.mutable_parameter, value_type_id)
}

fn (mut t Transformer) specialized_call_target_type_text(decl GenericFnDecl, typ_id u32, args_ids []u32, params []string) u32 {
	return t.specialized_signature_type_text(decl, typ_id, args_ids, params)
}

fn (mut t Transformer) retype_generic_call_literal_arg(arg_id flat.NodeId, param_type_id u32) flat.NodeId {
	if int(arg_id) < 0 || param_type_id == u32(0) {
		return arg_id
	}
	node := t.a.nodes[int(arg_id)]
	option_wrapper := flat.source_type_id('__v_option') in flat.source_type_syntax(param_type_id).underscore_prefixes()
	if node.kind == .none_expr {
		if t.is_optional_type_name(param_type_id) {
			return t.make_optional_none(t.resolve_substituted_type_text(t.qualify_optional_type(param_type_id)))
		}
		if param_type_id == flat.source_type_id('__v_option') || option_wrapper {
			return t.make_optional_none(param_type_id)
		}
	}
	if node.kind == .struct_init && (node.value_text() == '__v_option' || t.generic_optional_none_init(node))
		&& (t.is_optional_type_name(param_type_id) || option_wrapper) {
		optional_type := (if t.is_optional_type_name(param_type_id) {
			t.resolve_substituted_type_text(t.qualify_optional_type(param_type_id))
		} else {
			param_type_id
		})
		return t.make_optional_none(optional_type)
	}
	if node.kind == .array_literal
		&& t.generic_call_array_literal_can_retype_inline(node, param_type_id) {
		mut values := []flat.NodeId{cap: int(node.children_count)}
		for i in 0 .. node.children_count {
			values << t.a.child(&node, i)
		}
		return t.make_array_literal_typed(values, param_type_id)
	}
	if node.kind == .ident && node.value_text().contains('arr_lit') && flat.source_type_shape(node.typ).has(.array_type)
		&& flat.source_type_shape(param_type_id).has(.array_type) && node.typ != param_type_id {
		if t.retype_lowered_array_literal_temp(arg_id, node.value_text(), param_type_id) {
			t.set_node_typ(int(arg_id), param_type_id)
			t.set_var_type(node.value_text(), param_type_id)
		}
	}
	return arg_id
}

fn (t &Transformer) generic_optional_none_init(node flat.Node) bool {
	if node.kind != .struct_init
		|| (!t.is_optional_type_name(node.source_value_id()) && !t.is_optional_type_name(node.typ)) {
		return false
	}
	mut has_false_ok := false
	for i in 0 .. node.children_count {
		field := t.a.child_node(&node, i)
		if field.kind != .field_init {
			continue
		}
		if field.value_text() == 'value' {
			return false
		}
		if field.value_text() == 'ok' && field.children_count > 0 {
			value := t.a.child_node(field, 0)
			has_false_ok = value.kind == .bool_literal && value.value_text() != 'true'
		}
	}
	return has_false_ok
}

fn (mut t Transformer) specialize_generic_fn_value_arg(arg_id flat.NodeId, expected_type_id u32, defer_emit bool) ?flat.NodeId {
	if int(arg_id) < 0 || expected_type_id == u32(0) {
		return none
	}
	arg := t.a.nodes[int(arg_id)]
	if arg.kind != .ident {
		return none
	}
	mut expected_source_id := t.trimmed_type_id(expected_type_id)
	if flat.source_type_shape(expected_source_id).has(.variadic) {
		expected_source_id = t.trimmed_type_id(flat.source_type_syntax(expected_source_id).raw_base())
	}
	if flat.source_type_shape(expected_source_id).has(.array_type)
		&& flat.source_type_syntax(flat.source_type_syntax(expected_source_id).raw_base()).fn_prefix() {
		expected_source_id = flat.source_type_syntax(expected_source_id).raw_base()
	}
	if !flat.source_type_syntax(expected_source_id).fn_prefix() {
		return none
	}
	name := if !isnil(t.tc) {
		t.tc.resolved_fn_value_name(arg_id) or { arg.value_text() }
	} else {
		arg.value_text()
	}
	decls := t.cached_generic_fn_decls()
	for candidate in t.generic_plain_call_candidates(name, t.cur_module) {
		decl := decls[candidate] or { continue }
		if decl.node.value.contains('.') {
			continue
		}
		params := t.generic_fn_param_names(decl.node, decl.module)
		if params.len == 0 {
			continue
		}
		mut signature_params := []u32{}
		for i in 0 .. decl.node.children_count {
			param := t.a.child_node(&decl.node, i)
			if param.kind != .param {
				if t.prefix_param_scan {
					break
				}
				continue
			}
			signature_params << param.typ
		}
		signature_source_id := if decl.node.typ != 0 && decl.node.typ != flat.source_type_void {
			signature_params << decl.node.typ
			flat.source_type_composition(.function_type_spaced, signature_params)
		} else {
			flat.source_type_composition(.void_function_type_spaced, signature_params)
		}
		mut inferred := map[string]u32{}
		infer_generic_type_args(signature_source_id, expected_source_id, mut inferred)
		mut concrete_args := []u32{cap: params.len}
		for param in params {
			value_source_id := inferred[param] or { continue }

			concrete_args << t.generic_arg_for_decl_module(value_source_id, decl.module)
		}
		if concrete_args.len != params.len || t.generic_args_have_placeholders(concrete_args) {
			continue
		}
		concrete_args = t.canonical_generic_specialization_args(concrete_args).clone()
		if !t.generic_specialization_registered(decl, concrete_args)
			&& !t.generic_specialization_in_progress(decl, concrete_args) {
			if defer_emit {
				t.request_generic_fn_specialization(decl, concrete_args)
			} else {
				t.generated_fn_used_names(decl, t.emit_generic_fn_specialization(decl, concrete_args), concrete_args)
			}
		}
		spec_value := specialized_generic_fn_value(decl.node.value, concrete_args)
		qualified_spec := transform_qualified_fn_name(decl.module, spec_value)
		result := t.make_ident(qualified_spec)
		t.set_node_typ(int(result), expected_source_id)
		return result
	}
	return none
}

fn (t &Transformer) generic_call_array_literal_can_retype_inline(node flat.Node, param_type_id u32) bool {
	param_type_shape := flat.source_type_shape(param_type_id)
	if !param_type_shape.has(.array_type) {
		return false
	}
	for i in 0 .. node.children_count {
		child := t.a.child_node(&node, i)
		if child.kind !in [.int_literal, .float_literal, .bool_literal, .char_literal, .string_literal,
			.cast_expr, .as_expr] {
			return false
		}
	}
	return true
}

fn (mut t Transformer) retype_lowered_array_literal_temp(arg_id flat.NodeId, name string, array_type_id u32) bool {
	if !flat.source_type_shape(array_type_id).has(.array_type) {
		return false
	}
	elem_type_id := flat.source_type_syntax(array_type_id).raw_base()
	// Compiler temporary names restart in every function. Find the declaration
	// immediately preceding this lowered literal result instead of rewriting every
	// declaration with the same spelling across the compilation.
	mut decl_idx := -1
	mut i := int(arg_id) - 1
	for i >= 0 {
		node := t.a.nodes[i]
		if node.kind == .decl_assign && node.children_count >= 2 {
			lhs_id := t.a.child(&node, 0)
			lhs := t.a.nodes[int(lhs_id)]
			if lhs.kind == .ident && lhs.value_text() == name {
				decl_idx = i
				break
			}
		}
		i--
	}
	if decl_idx < 0 {
		return false
	}
	decl := t.a.nodes[decl_idx]
	lhs_id := t.a.child(&decl, 0)
	rhs_id := t.a.child(&decl, 1)
	t.set_node_typ(decl_idx, array_type_id)
	t.set_node_typ(int(lhs_id), array_type_id)
	t.set_var_type(name, array_type_id)
	t.retype_array_new_call(rhs_id, elem_type_id, array_type_id)
	for node_idx in decl_idx + 1 .. int(arg_id) {
		node := t.a.nodes[node_idx]
		if node.kind == .call && t.call_is_array_push_to_name(node, name) {
			value_name := t.array_push_value_name(node) or { continue }
			t.retype_decl_assign_name_in_range(value_name, elem_type_id, decl_idx + 1, int(arg_id))
		}
	}
	return true
}

fn (mut t Transformer) retype_array_new_call(call_id flat.NodeId, elem_type_id u32, array_type_id u32) {
	if int(call_id) < 0 || int(call_id) >= t.a.nodes.len {
		return
	}
	call := t.a.nodes[int(call_id)]
	if call.kind != .call || call.children_count < 2 {
		return
	}
	callee := t.a.child_node(&call, 0)
	if callee.kind != .ident || callee.value_text() != 'array_new' {
		return
	}
	sizeof_id := t.a.child(&call, 1)
	if int(sizeof_id) >= 0 && int(sizeof_id) < t.a.nodes.len {
		if t.a.nodes[int(sizeof_id)].kind == .sizeof_expr {
			t.set_node_source_value(int(sizeof_id), elem_type_id)
		}
	}
	t.set_node_typ(int(call_id), array_type_id)
}

fn (t &Transformer) call_is_array_push_to_name(node flat.Node, name string) bool {
	if node.children_count < 3 {
		return false
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .ident || callee.value_text() != 'array_push' {
		return false
	}
	arg := t.a.child_node(&node, 1)
	if arg.kind != .prefix || arg.children_count == 0 {
		return false
	}
	base := t.a.child_node(arg, 0)
	return base.kind == .ident && base.value_text() == name
}

fn (t &Transformer) array_push_value_name(node flat.Node) ?string {
	if node.children_count < 3 {
		return none
	}
	arg := t.a.child_node(&node, 2)
	if arg.kind != .prefix || arg.children_count == 0 {
		return none
	}
	base := t.a.child_node(arg, 0)
	if base.kind != .ident || base.value_text().len == 0 {
		return none
	}
	return base.value_text()
}

fn (mut t Transformer) retype_decl_assign_name_in_range(name string, typ_id u32, start int, end int) bool {
	mut changed := false
	for i in start .. end {
		node := t.a.nodes[i]
		if node.kind != .decl_assign || node.children_count < 1 {
			continue
		}
		lhs_id := t.a.child(&node, 0)
		lhs := t.a.nodes[int(lhs_id)]
		if lhs.kind != .ident || lhs.value_text() != name {
			continue
		}
		t.set_node_typ(i, typ_id)
		t.set_node_typ(int(lhs_id), typ_id)
		t.set_var_type(name, typ_id)
		changed = true
	}
	return changed
}

// call_is_selector_form reports whether a call node embeds its receiver inside the
// callee (`recv.method(args)`) rather than passing it as an explicit child
// (`Type.method(recv, args)`, the ident-lowered form).
fn (t &Transformer) call_is_selector_form(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	mut callee := t.a.nodes[int(t.a.child(&node, 0))]
	if callee.kind == .index && callee.children_count > 0 {
		callee = t.a.nodes[int(t.a.child(&callee, 0))]
	}
	return callee.kind == .selector && callee.children_count > 0
		&& int(t.a.child(&callee, 0)) >= 0
}

fn (mut t Transformer) rewrite_generic_method_call(id flat.NodeId, node flat.Node, decl GenericFnDecl, args_ids []u32) {
	if node.children_count == 0 {
		return
	}
	t.rewrite_method_level_generic_call(id, node, decl, args_ids)
}

fn (mut t Transformer) specialized_method_receiver_arg(receiver flat.NodeId, decl GenericFnDecl) flat.NodeId {
	if decl.node.children_count == 0 {
		return receiver
	}
	param := t.a.child_node(&decl.node, 0)
	if !param.is_mut {
		return receiver
	}
	// A method becomes a plain specialized call. Preserve the implicit mutable
	// argument so ordering preludes stabilize its storage instead of copying it.
	mut argument := t.a.nodes[int(receiver)]
	argument.is_mut = true
	return t.a.add_node(argument)
}

// rewrite_method_level_generic_call rewrites a call to a method-level generic into
// an explicit call of the specialized function: `SpecName(receiver, args...)`. It
// handles both the selector form (receiver inside the callee) and the
// ident-lowered form (receiver already an explicit child).
fn (mut t Transformer) rewrite_method_level_generic_call(id flat.NodeId, node flat.Node, decl GenericFnDecl, args_ids []u32) {
	old_params := t.active_generic_params
	t.clear_node_type_memo()
	t.active_generic_params = t.generic_fn_param_names(decl.node, decl.module)
	concrete_args_source_ids := t.canonical_generic_specialization_args(args_ids)
	spec_value := specialized_generic_fn_value(decl.node.value_text(), concrete_args_source_ids)
	spec_name := transform_qualified_fn_name(decl.module, spec_value)
	t.record_generic_specialization_args_for_names([spec_name, c_name(spec_name)], concrete_args_source_ids)
	ret_typ_source_id := t.specialized_signature_type_text(decl, t.generic_fn_return_type_text(decl), concrete_args_source_ids, t.active_generic_params)

	t.clear_node_type_memo()
	t.active_generic_params = old_params
	param_types_source_ids := t.specialized_generic_call_param_type_texts(decl, concrete_args_source_ids)

	mut children := []flat.NodeId{cap: int(node.children_count) + 1}
	children << t.make_ident(spec_name)
	if t.call_is_selector_form(node) {
		// Receiver is embedded in the selector callee; explicit args are children 1..n.
		if recv_id := t.generic_call_receiver_id(node) {
			mut receiver_arg := recv_id
			if param_types_source_ids.len > 0 {
				base_type_source_id := t.generic_call_arg_type_for_inference(recv_id)

				mut expected_type := t.trimmed_type_id(param_types_source_ids[0])
				if flat.source_type_shape(expected_type).has(.mutable_type) {
					expected_type = t.trimmed_type_id(flat.source_type_syntax(expected_type).raw_base())
				}
				expected_type = t.trimmed_type_id(t.trim_all_pointer_type(expected_type))
				if expected_type != 0 {
					if embedded := t.embedded_receiver_base_for_type(recv_id, base_type_source_id, expected_type) {
						receiver_arg = embedded
					}
				}
			}
			children << t.specialized_method_receiver_arg(receiver_arg, decl)
		}
		mut i := 1
		for i < node.children_count {
			arg_id := t.a.child(&node, i)
			param_type := if i < param_types_source_ids.len {
				param_types_source_ids[i]
			} else {
				u32(0)
			}
			arg := t.a.nodes[int(arg_id)]
			if arg.kind == .field_init {
				if packed_arg := t.transform_params_struct_call_arg(node, i, param_type) {
					children << packed_arg
					i = t.next_non_field_init_arg(node, i)
					continue
				}
				if packed_arg := t.transform_struct_call_arg(node, i, param_type) {
					children << packed_arg
					i = t.next_non_field_init_arg(node, i)
					continue
				}
			}
			children << t.specialize_generic_fn_value_arg(arg_id, param_type, false) or {
				t.retype_generic_call_literal_arg(arg_id, param_type)
			}
			i++
		}
	} else {
		// Ident-lowered form: receiver and args are already explicit children 1..n.
		mut i := 1
		for i < node.children_count {
			arg_id := t.a.child(&node, i)
			param_idx := i - 1
			param_type := if param_idx < param_types_source_ids.len {
				param_types_source_ids[param_idx]
			} else {
				u32(0)
			}
			arg := t.a.nodes[int(arg_id)]
			if arg.kind == .field_init {
				if packed_arg := t.transform_params_struct_call_arg(node, i, param_type) {
					children << packed_arg
					i = t.next_non_field_init_arg(node, i)
					continue
				}
				if packed_arg := t.transform_struct_call_arg(node, i, param_type) {
					children << packed_arg
					i = t.next_non_field_init_arg(node, i)
					continue
				}
			}
			children << t.specialize_generic_fn_value_arg(arg_id, param_type, false) or {
				t.retype_generic_call_literal_arg(arg_id, param_type)
			}
			i++
		}
	}
	t.append_missing_specialized_params_struct_args(mut children, param_types_source_ids.clone(), decl.module)
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	mut rewritten := flat.Node{
		kind:           .call
		op:             node.op
		children_start: start
		children_count: flat.child_count(children.len)
		pos:            node.pos
		typ:            ret_typ_source_id
	}
	rewritten.set_call_type_args(concrete_args_source_ids)
	t.set_node(int(id), rewritten)
	t.clear_resolved_call(id)
}

fn (mut t Transformer) append_missing_specialized_params_struct_args(mut children []flat.NodeId, param_types_ids []u32, decl_module string) {
	mut param_idx := children.len - 1
	for param_idx < param_types_ids.len {
		param_type_source_id := param_types_ids[param_idx]
		mut struct_type := t.params_struct_type_name(param_type_source_id) or { u32(0) }
		if struct_type == 0 && !flat.source_type_shape(param_type_source_id).has(.dotted) && decl_module != ''
			&& decl_module !in ['main', 'builtin'] {
			struct_type = t.params_struct_type_name(flat.source_type_composition_two(.qualified, flat.source_type_id(decl_module), param_type_source_id)) or { u32(0) }
		}
		if struct_type == 0 {
			break
		}
		children << t.zero_value_for_type(struct_type)
		param_idx++
	}
}

fn (mut t Transformer) clear_resolved_call(id flat.NodeId) {
	if isnil(t.tc) {
		return
	}
	idx := int(id)
	if t.tc.parallel_check_sparse && idx >= t.tc.resolved_call_names.len {
		t.tc.sparse_resolved_call_names.delete(idx)
		return
	}
	if idx >= 0 && idx < t.tc.resolved_call_names.len {
		t.tc.resolved_call_names[idx] = unsafe { nil }
		t.tc.resolved_call_set[idx] = false
	}
}

fn (mut t Transformer) erase_generic_fn_decls(decls map[string]GenericFnDecl) {
	for _, decl in decls {
		t.unregister_generic_fn_signature(decl)
		if int(decl.id) < 0 || int(decl.id) >= t.a.nodes.len {
			continue
		}
		mut subtree := map[int]bool{}
		t.collect_node_subtree_ids(decl.id, mut subtree)
		for idx, _ in subtree {
			old := t.a.nodes[idx]
			t.set_node(idx, flat.Node{
				kind: .empty
				pos:  old.pos
			})
			t.clear_typechecker_node_cache(idx)
		}
	}
	if decls.len > 0 {
		// Erasure changes indexed declarations without changing the AST length.
		t.comptime_method_metas_cache.clear()
		if !isnil(t.tc) {
			t.tc.rebuild_fn_param_suffix_index()
		}
	}
}

fn (mut t Transformer) unregister_generic_fn_signature(decl GenericFnDecl) {
	if isnil(t.tc) {
		return
	}
	mut names := [decl.key, c_name(decl.key)]
	if decl.module.len == 0 || decl.module == 'main' || decl.module == 'builtin' {
		names << decl.node.value
		names << c_name(decl.node.value)
	}
	if decl.module.len > 0 && decl.module != 'main' && decl.module != 'builtin' {
		qname := transform_qualified_fn_name(decl.module, decl.node.value)
		names << qname
		names << c_name(qname)
	}
	for name in names {
		t.tc.unregister_generated_fn_return_type(name)
		t.tc.fn_param_types.delete(name)
		t.tc.fn_variadic.delete(name)
	}
	// A removal can hide a later registration from a count of the signatures.
	t.tc.forget_transform_static_method_names()
	t.alias_receiver_fns_signatures = -1
}

fn (mut t Transformer) cached_generic_call_specialization(id flat.NodeId, node flat.Node, module_name string, decls map[string]GenericFnDecl) ?(string, []u32) {
	idx := int(id)
	// Generated calls without source context cannot safely be re-inferred in the
	// declaration module: a bare caller type in an encoded specialization can bind
	// to a same-named imported type. The encoded callee is authoritative; it may
	// request its exact body once, but later scans must not retarget it.
	if node.children_count > 0 && t.node_file_or(idx, '').len == 0 && !isnil(t.tc) {
		callee := t.a.child_node(&node, 0)
		if callee.kind == .ident && t.generic_callee_is_specialization(callee.value_text()) {
			decl_key := t.generic_call_decl_key(id, node, module_name, decls) or { return none }
			decl := decls[decl_key] or { return none }
			if exact_source_ids := t.exact_generic_specialization_args_from_callee(callee.value_text()) {
				if exact_source_ids.len > 0 && !t.generic_args_have_placeholders(exact_source_ids) {
					key := t.generic_specialization_progress_key(decl, exact_source_ids)
					if !t.generic_fn_spec_nodes.contains(key) && !t.generic_fn_specs_in_progress.contains(key)
						&& !t.pending_generic_fn_spec_keys.contains(key) {
						t.generic_call_spec_cache[idx] = GenericCallSpec{
							decl_key: decl_key
							args:     flat.TypeTextList{ ids: exact_source_ids.clone() }
						}
						return decl_key, exact_source_ids.clone()
					}
				}
			}
			return none
		}
	}
	// A rewritten call records the exact semantic arguments both in its callee
	// identity and in `node.call_type_args()`. Preserve that agreement before consulting a
	// stale cache or re-inferring without the generated function's local scope.
	if node.children_count > 0 && node.call_type_args().len > 0 && !isnil(t.tc) {
		callee := t.a.child_node(&node, 0)
		if callee.kind == .ident && t.generic_callee_is_specialization(callee.value_text()) {
			if exact_source_ids := t.exact_generic_specialization_args_from_callee(callee.value_text()) {

				// The rewritten payload already uses the callee's semantic type names.
				// Resolving it in the current scan module can rebind a caller homonym.
				preserved := node.call_type_args()
				if generic_type_args_equal(preserved, exact_source_ids) {
					decl_key := if spec := t.generic_call_spec_cache[idx] {
						spec.decl_key
					} else {
						t.generic_call_decl_key(id, node, module_name, decls) or { return none }
					}
					decl := decls[decl_key] or { return none }
					if !t.generic_fn_spec_nodes.contains(t.generic_specialization_progress_key(decl, exact_source_ids)) {
						t.generic_call_spec_cache[idx] = GenericCallSpec{
							decl_key: decl_key
							args:     flat.TypeTextList{ ids: exact_source_ids.clone() }
						}
						return decl_key, exact_source_ids.clone()
					}
					return none
				}
			}
		}
	}
	// A cached entry was inferred for this exact node while its generated
	// function scope and local bindings were live. An exact callee can still be
	// newer and more precise (notably for module-qualified homonyms), so only
	// replace it when the current argument nodes confirm the cached arguments.
	if spec := t.generic_call_spec_cache[idx] {
		if node.children_count > 0 && !isnil(t.tc) {
			callee := t.a.child_node(&node, 0)
			if callee.kind == .ident && t.generic_callee_is_specialization(callee.value_text()) {
				decl := decls[spec.decl_key] or { return none }
				if exact_source_ids := t.exact_generic_specialization_args_from_callee(callee.value_text()) {
					if generic_type_args_equal(spec.args.ids, exact_source_ids) {
						ret_type_source_id := t.specialized_fn_return_type_text(decl, exact_source_ids)
						if ret_type_source_id != 0 && !t.generic_arg_is_unresolved(ret_type_source_id)
							&& node.typ != ret_type_source_id {
							t.set_node_typ(idx, ret_type_source_id)
							t.clear_typechecker_node_cache(idx)
						}
						if !t.generic_fn_spec_nodes.contains(t.generic_specialization_progress_key(decl, exact_source_ids)) {
							return spec.decl_key, exact_source_ids.clone()
						}
						return none
					}
				}
			}
		}
		// Explicit source arguments and the alias marker retained in `node.call_type_args()`
		// are more precise than a later inference pass over an alias-erased ABI
		// argument. Preserve that identity for reflected `T.typ_text()` specializations.
		if node.call_type_args().len > 0 {
			if decl := decls[spec.decl_key] {
				if explicit_source_ids := t.explicit_generic_call_args(node, module_name) {
					if scoped_source_ids := t.infer_generic_call_args_with_explicit(decl, id, node, module_name, explicit_source_ids) {
						if scoped_source_ids.len > 0 && !t.generic_args_have_placeholders(scoped_source_ids) {
							return spec.decl_key, scoped_source_ids.clone()
						}
					}
				}
			}
		}
		if node.children_count > 0 && !isnil(t.tc) {
			callee := t.a.child_node(&node, 0)
			if callee.kind == .ident && t.generic_callee_is_specialization(callee.value_text()) {
				decl := decls[spec.decl_key] or { return none }
				if exact_source_ids := t.exact_generic_specialization_args_from_callee(callee.value_text()) {
					if generic_type_args_equal(spec.args.ids, exact_source_ids) {
						ret_type_source_id_2 := t.specialized_fn_return_type_text(decl, exact_source_ids)
						if ret_type_source_id_2 != 0 && !t.generic_arg_is_unresolved(ret_type_source_id_2)
							&& node.typ != ret_type_source_id_2 {
							t.set_node_typ(idx, ret_type_source_id_2)
							t.clear_typechecker_node_cache(idx)
						}
						if !t.generic_fn_spec_nodes.contains(t.generic_specialization_progress_key(decl, exact_source_ids)) {
							return spec.decl_key, exact_source_ids.clone()
						}
						return none
					}
				}
				if raw_source_ids := t.infer_generic_call_args_from_raw_node_types(decl, node) {
					if exact_source_ids := t.exact_generic_specialization_args_from_callee(callee.value_text()) {
						if generic_type_args_equal(raw_source_ids, exact_source_ids) {
							return none
						}
						resolved_raw_source_ids := t.resolved_live_generic_call_args(raw_source_ids, module_name, decl.module)

						if generic_type_args_equal(resolved_raw_source_ids, exact_source_ids) {
							return none
						}
						if t.generic_args_equal_ignoring_mut_storage(decl, exact_source_ids, resolved_raw_source_ids) {
							return none
						}
						if t.generic_alias_erased_args_equal(exact_source_ids, resolved_raw_source_ids, module_name) {
							if !t.generic_fn_spec_nodes.contains(t.generic_specialization_progress_key(decl, exact_source_ids)) {
								return spec.decl_key, exact_source_ids.clone()
							}
							return none
						}
						if generic_type_args_equal(resolved_raw_source_ids, spec.args.ids) {
							return spec.decl_key, spec.args.ids.clone()
						}
						// Generated generic bodies are cloned from the same template nodes.
						// Their cached/exact callees can therefore describe an earlier
						// specialization; the live argument nodes are authoritative after
						// resolving them in the current specialization's module context.
						if !t.generic_args_have_placeholders(resolved_raw_source_ids)
							&& !t.generic_args_name_specialized_functions(resolved_raw_source_ids) {
							return spec.decl_key, resolved_raw_source_ids.clone()
						}
						return none
					}
					if generic_type_args_equal(raw_source_ids, spec.args.ids) {
						return spec.decl_key, spec.args.ids.clone()
					}
					resolved_raw_source_ids_2 := t.resolved_live_generic_call_args(raw_source_ids, module_name, decl.module)

					if generic_type_args_equal(resolved_raw_source_ids_2, spec.args.ids) {
						return spec.decl_key, spec.args.ids.clone()
					}
					if t.generic_alias_erased_args_equal(spec.args.ids, resolved_raw_source_ids_2, module_name) {
						return spec.decl_key, spec.args.ids.clone()
					}
					if !t.generic_args_have_placeholders(resolved_raw_source_ids_2)
						&& !t.generic_args_name_specialized_functions(resolved_raw_source_ids_2) {
						return spec.decl_key, resolved_raw_source_ids_2.clone()
					}
				}
				return none
			}
		}
		return spec.decl_key, spec.args.ids.clone()
	}
	// A prior transform (notably a reflected `$for field`) can already replace
	// the callee with an exact registered specialization. Re-inferring it from
	// runtime arguments is both redundant and lossy: containers erase their
	// element type at the C ABI, so same-named types from different modules can
	// be confused and the exact callee can be rewritten to the wrong instance.
	if node.children_count > 0 && !isnil(t.tc) {
		callee := t.a.child_node(&node, 0)
		if callee.kind == .ident && t.generic_callee_is_specialization(callee.value_text()) {
			decl_key := t.generic_call_decl_key(id, node, module_name, decls) or { return none }
			decl := decls[decl_key] or { return none }
			// The checker and the earlier transform register a concrete signature
			// before the monomorphization Transformer exists. A specialized callee
			// therefore does not prove that its body was emitted. Recover its exact
			// arguments (including return-type-only generics with no value argument)
			// and queue the missing body before applying the usual stale-callee checks.
			mut exact_args := []u32{}
			if t.node_file_or(idx, '').len == 0 && node.call_type_args().len > 0 {
				exact_args = t.canonical_generic_specialization_args(node.call_type_args()).clone()
			}
			if exact_args.len == 0 {
				exact_args = (t.exact_generic_specialization_args_from_callee(callee.value_text()) or {
					t.specialized_plain_generic_call_args(node, decl, module_name) or { []u32{} }
				}).clone()
			}
			if exact_args.any(flat.source_type_id('fn') in flat.source_type_syntax(it).underscore_prefixes()) {
				if inferred_source_ids := t.infer_generic_call_args_from_params(decl, id, node, module_name) {
					if inferred_source_ids.len == exact_args.len && !t.generic_args_have_placeholders(inferred_source_ids) {
						exact_args = inferred_source_ids.clone()
					}
				}
			}
			if exact_args.len > 0 && !t.generic_args_have_placeholders(exact_args)
				&& !t.generic_specialization_registered(decl, exact_args) {
				t.generic_call_spec_cache[idx] = GenericCallSpec{
					decl_key: decl_key
					args:     flat.TypeTextList{ ids: exact_args.clone() }
				}
				return decl_key, exact_args.clone()
			}
			// Worker-local inference caches are intentionally not merged. Once an exact
			// generated callee has a body, its encoded semantic arguments remain
			// authoritative during the following global scan; re-inferring from bare ABI
			// argument types can bind a main-module type to an imported homonym.
			if exact_args.len > 0 && !t.generic_args_have_placeholders(exact_args) {
				return none
			}
			if node.call_type_args().len > 0 {
				if explicit_source_ids := t.explicit_generic_call_args(node, module_name) {
					if scoped_source_ids := t.infer_generic_call_args_with_explicit(decl, id, node, module_name, explicit_source_ids) {
						if scoped_source_ids.len > 0 && !t.generic_args_have_placeholders(scoped_source_ids) {
							t.generic_call_spec_cache[idx] = GenericCallSpec{
								decl_key: decl_key
								args:     flat.TypeTextList{ ids: scoped_source_ids.clone() }
							}
							return decl_key, scoped_source_ids.clone()
						}
					}
				}
			}
			if raw_source_ids := t.infer_generic_call_args_from_raw_node_types(decl, node) {
				resolved_raw_source_ids_3 := t.resolved_live_generic_call_args(raw_source_ids, module_name, decl.module)

				if exact_source_ids := t.exact_generic_specialization_args_from_callee(callee.value_text()) {
					exact_matches := generic_type_args_equal(raw_source_ids, exact_source_ids)
						|| generic_type_args_equal(resolved_raw_source_ids_3, exact_source_ids)
						|| t.generic_args_equal_ignoring_mut_storage(decl, exact_source_ids, resolved_raw_source_ids_3)
						|| t.generic_alias_erased_args_equal(exact_source_ids, resolved_raw_source_ids_3, module_name)
					if exact_matches {
						if !t.generic_fn_spec_nodes.contains(t.generic_specialization_progress_key(decl, exact_source_ids)) {
							t.generic_call_spec_cache[idx] = GenericCallSpec{
								decl_key: decl_key
								args:     flat.TypeTextList{ ids: exact_source_ids.clone() }
							}
							return decl_key, exact_source_ids.clone()
						}
						return none
					}
					if !generic_type_args_equal(resolved_raw_source_ids_3, exact_source_ids)
						&& !t.generic_args_have_placeholders(resolved_raw_source_ids_3)
						&& !t.generic_args_name_specialized_functions(resolved_raw_source_ids_3) {
						t.generic_call_spec_cache[idx] = GenericCallSpec{
							decl_key: decl_key
							args:     flat.TypeTextList{ ids: resolved_raw_source_ids_3.clone() }
						}
						return decl_key, resolved_raw_source_ids_3.clone()
					}
					return none
				}
				if !t.generic_args_have_placeholders(resolved_raw_source_ids_3)
					&& !t.generic_args_name_specialized_functions(resolved_raw_source_ids_3) {
					t.generic_call_spec_cache[idx] = GenericCallSpec{
						decl_key: decl_key
						args:     flat.TypeTextList{ ids: resolved_raw_source_ids_3.clone() }
					}
					return decl_key, resolved_raw_source_ids_3.clone()
				}
			} else if exact_source_ids := t.exact_generic_specialization_args_from_callee(callee.value_text()) {

				// The checker can lower an explicit zero-argument associated call such as
				// `Type.make[T]()` to the exact `Type[T].make` callee before this pass.
				// With no value arguments there is nothing to re-infer, but the exact
				// callee still needs a body materialized.
				if !t.generic_fn_spec_nodes.contains(t.generic_specialization_progress_key(decl, exact_source_ids)) {
					t.generic_call_spec_cache[idx] = GenericCallSpec{
						decl_key: decl_key
						args:     flat.TypeTextList{ ids: exact_source_ids.clone() }
					}
					return decl_key, exact_source_ids.clone()
				}
			}
			return none
		}
	}
	// Rewritten explicit and inferred-alias calls retain their exact generic
	// arguments in `node.call_type_args()`. The recovery helper prefers those semantic
	// arguments before falling back to decoding the flattened callee.
	if spec := t.specialized_plain_generic_call_specialization(node, module_name, decls) {
		t.generic_call_spec_cache[idx] = GenericCallSpec{
			decl_key: spec.decl_key
			args:     spec.args.clone()
		}
		return spec.decl_key, spec.args.ids.clone()
	}
	if t.generic_call_spec_misses[idx] && !t.in_monomorphize_scan {
		return none
	}
	decl_key, args_source_ids := t.generic_call_specialization(id, node, module_name, decls) or { return none }

	t.generic_call_spec_cache[idx] = GenericCallSpec{
		decl_key: decl_key
		args:     flat.TypeTextList{ ids: args_source_ids.clone() }
	}
	return decl_key, args_source_ids.clone()
}

fn (mut t Transformer) missing_specialized_plain_generic_call(node flat.Node, module_name string, decls map[string]GenericFnDecl) ?(string, []u32) {
	if node.children_count == 0 || isnil(t.tc) {
		return none
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .ident || !t.generic_callee_is_specialization(callee.value_text()) {
		return none
	}
	if exact_source_ids := t.exact_generic_specialization_args_from_callee(callee.value_text()) {
		decl_key := t.generic_call_decl_key(flat.empty_node, node, module_name, decls) or {
			return none
		}
		decl := decls[decl_key] or { return none }
		if exact_source_ids.len > 0 && !t.generic_args_have_placeholders(exact_source_ids) {
			if t.generic_specialization_registered(decl, exact_source_ids) {
				return none
			}
			return decl_key, exact_source_ids.clone()
		}
	}
	spec := t.specialized_plain_generic_call_specialization(node, module_name, decls) or {
		return none
	}
	decl := decls[spec.decl_key] or { return none }
	if t.generic_specialization_registered(decl, spec.args.ids) {
		return none
	}
	return spec.decl_key, spec.args.ids.clone()
}

fn (t &Transformer) generic_args_name_specialized_functions(args_ids []u32) bool {
	for arg_source_id in args_ids {
		mut clean := t.trimmed_type_id(arg_source_id)
		for flat.source_type_shape(clean).has(.pointer)
			|| flat.source_type_shape(clean).has(.option_type)
			|| flat.source_type_shape(clean).has(.result_type) {
			clean = t.trimmed_type_id(flat.source_type_syntax(clean).raw_base())
		}
		for flat.source_type_shape(clean).has(.array_type) {
			clean = t.trimmed_type_id(flat.source_type_syntax(clean).raw_base())
		}
		if t.generic_callee_is_specialization(flat.source_type_name(clean)) {
			return true
		}
		if flat.source_type_syntax(clean).generic_encoded_fragment() && !t.type_name_is_declared_id(clean) {
			return true
		}
		syntax := flat.source_type_syntax(clean)
		if syntax.generic_app_valid() && t.generic_args_name_specialized_functions(syntax.generic_app_args()) {
			return true
		}
	}
	return false
}

fn (t &Transformer) resolved_live_generic_call_args(raw_ids []u32, call_module string, decl_module string) []u32 {
	mut resolved := []u32{cap: raw_ids.len}
	for arg_source_id in raw_ids {
		arg_source_id_7 := arg_source_id
		arg_source_id_11 := arg_source_id_7

		resolved << (if t.substituted_type_belongs_to_main_generic(arg_source_id_7) {
			arg_source_id_11
		} else {
			t.generic_arg_for_call_and_decl_module(arg_source_id, call_module, decl_module)
		})
	}
	return t.canonical_generic_specialization_args(resolved).clone()
}

fn generic_type_args_equal(left_ids []u32, right_ids []u32) bool {
	if left_ids.len != right_ids.len { return false }
	for i, id in left_ids {
		if id == right_ids[i] { continue }
		if transform_trimmed_type_id(id) != transform_trimmed_type_id(right_ids[i]) {
			return false
		}
	}
	return true
}

fn (t &Transformer) exact_generic_specialization_args_from_callee(name string) ?[]u32 {
	if args_source_ids := t.recorded_generic_specialization_args(name) {
		return args_source_ids.clone()
	}
	if !name.contains('.') {
		return none
	}
	receiver := name.all_before_last('.')
	syntax := flat.source_type_syntax(flat.source_type_id(receiver))
	args := syntax.generic_app_args()
	if !syntax.generic_app_valid() || args.len == 0 || t.generic_args_have_placeholders(args) {
		return none
	}
	return t.canonical_generic_specialization_args(args).clone()
}

fn (mut t Transformer) generic_args_equal_ignoring_mut_storage(decl GenericFnDecl, exact_ids []u32, inferred_ids []u32) bool {
	if exact_ids.len != inferred_ids.len {
		return false
	}
	params := t.generic_fn_param_names(decl.node, decl.module)
	if params.len != exact_ids.len {
		return false
	}
	mut direct_mut_params := map[u32]bool{}
	for i in 0 .. decl.node.children_count {
		param := t.a.child_node(&decl.node, i)
		if param.kind != .param {
			if t.prefix_param_scan {
				break
			}
			continue
		}
		if param.is_mut || flat.source_type_shape(param.typ).has(.mutable_type) {
			generic_name := generic_inference_param_type(param)
			if flat.source_type_query(generic_name).has(.generic_fn_name) {
				direct_mut_params[generic_name] = true
			}
		}
	}
	for i, exact_arg_source_id in exact_ids {
		inferred_arg := t.trimmed_type_id(inferred_ids[i])
		clean_exact := t.trimmed_type_id(exact_arg_source_id)
		if inferred_arg == clean_exact {
			continue
		}
		if !direct_mut_params[flat.source_type_id(params[i])]
			|| inferred_arg != flat.source_type_composition_one(.pointer, clean_exact) {
			return false
		}
	}
	return true
}

fn (t &Transformer) generic_alias_erased_args_equal(exact_ids []u32, inferred_ids []u32, module_name string) bool {
	if exact_ids.len != inferred_ids.len || !t.generic_args_contain_alias(exact_ids, module_name) {
		return false
	}
	for i, arg_source_id in exact_ids {
		if t.generic_inference_alias_target(arg_source_id, module_name) != t.generic_inference_alias_target(inferred_ids[i], module_name) {
			return false
		}
	}
	return true
}

fn (mut t Transformer) infer_generic_call_args_from_raw_node_types(decl GenericFnDecl, node flat.Node) ?[]u32 {
	params := t.generic_fn_param_names(decl.node, decl.module)
	if params.len == 0 {
		return none
	}
	is_receiver := t.generic_decl_is_receiver_method(decl.node)
		&& !t.generic_call_is_static_assoc_selector(node, decl)
	mut inferred := map[string]u32{}
	mut param_idx := 0
	param_count := t.generic_decl_param_count(decl.node)
	for i in 0 .. decl.node.children_count {
		param := t.a.child_node(&decl.node, i)
		if param.kind != .param {
			if t.prefix_param_scan {
				break
			}
			continue
		}
		if generic_param_text_is_variadic(param.typ) && param_idx == param_count - 1 {
			elem_param_type := generic_variadic_elem_param_text(param.typ)
			for arg_id in t.generic_call_arg_ids_from_param(node, param_idx, is_receiver) {
				t.infer_generic_variadic_call_arg(param.typ, elem_param_type, arg_id, mut inferred)
			}
			param_idx++
			continue
		}
		arg_id := t.generic_call_arg_id_for_param(node, param_idx, is_receiver) or {
			param_idx++
			continue
		}
		arg := t.a.nodes[int(arg_id)]
		inference_param_type := generic_inference_param_type(param)
		mut raw_arg_type := t.fn_value_type_name(arg_id) or { arg.typ }
		if !flat.source_type_query(inference_param_type).has(.generic_fn_name) {
			raw_arg_type = t.generic_composite_inference_alias_type(raw_arg_type, decl.module)
		}
		if (param.is_mut || param.op == .amp || flat.source_type_shape(param.typ).has(.mutable_type))
			&& flat.source_type_shape(raw_arg_type).has(.pointer) {
			// A rewritten `mut value` call stores the C ABI address on either the
			// argument prefix or its cloned value identifier. Infer from the
			// language-level value by removing exactly that storage pointer; a
			// pointer-valued `T` still retains its own leading `&`.
			raw_arg_type = flat.source_type_syntax(raw_arg_type).raw_base()
		}
		raw_type := generic_arg_type_for_param(inference_param_type, raw_arg_type)
		if raw_type != 0 {
			infer_generic_type_args(inference_param_type, raw_type, mut inferred)
		}
		param_idx++
	}
	mut result := []u32{cap: params.len}
	for param in params {
		arg_source_id := inferred[param] or { return none }

		result << arg_source_id
	}
	return result.clone()
}

fn (mut t Transformer) specialized_plain_generic_call_specialization(node flat.Node, module_name string, decls map[string]GenericFnDecl) ?GenericCallSpec {
	if node.children_count == 0 {
		return none
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .ident || !callee.value_text().contains('_T_') {
		return none
	}
	decl_key := t.specialized_plain_generic_call_decl_key(callee.value_text(), module_name, decls) or {
		return none
	}
	decl := decls[decl_key] or { return none }
	if node.call_type_args().len > 0 {
		explicit := t.explicit_generic_call_args(node, module_name) or { []u32{} }
		params := t.generic_fn_param_names(decl.node, decl.module)
		if explicit.len == params.len {
			mut scoped := []u32{cap: explicit.len}
			for arg in explicit {
				scoped << t.generic_arg_for_call_and_decl_module(arg, module_name, decl.module)
			}
			if !t.generic_args_have_placeholders(scoped) {
				return GenericCallSpec{
					decl_key: decl_key
					args:     flat.TypeTextList{ ids: t.canonical_generic_specialization_args(scoped).clone() }
				}
			}
		}
	}
	args_source_ids := t.specialized_plain_generic_call_args(node, decl, module_name) or { return none }
	if args_source_ids.len == 0 || t.generic_args_have_placeholders(args_source_ids) {
		return none
	}
	return GenericCallSpec{
		decl_key: decl_key
		args:     flat.TypeTextList{ ids: args_source_ids.clone() }
	}
}

fn (mut t Transformer) generic_call_specialization(id flat.NodeId, node flat.Node, module_name string, decls map[string]GenericFnDecl) ?(string, []u32) {
	if node.children_count == 0 {
		return none
	}
	old_module := t.cur_module
	old_file := t.cur_file
	old_tc_module := t.tc.cur_module
	old_tc_file := t.tc.cur_file
	t.cur_module = module_name
	t.cur_file = t.node_file_or(int(id), old_file)
	t.tc.cur_module = module_name
	t.tc.cur_file = t.cur_file
	defer {
		t.cur_module = old_module
		t.cur_file = old_file
		t.tc.cur_module = old_tc_module
		t.tc.cur_file = old_tc_file
	}
	decl_key := t.generic_call_decl_key(id, node, module_name, decls) or { return none }
	decl := decls[decl_key] or { return none }
	if t.call_has_source_generic_args(node) || t.call_is_normalized_explicit_generic_call(node) {
		if args_source_ids := t.explicit_generic_call_args(node, module_name) {
			scoped_source_ids := t.infer_generic_call_args_with_explicit(decl, id, node, module_name, args_source_ids) or {
				[]u32{}
			}

			if scoped_source_ids.len > 0 && !t.generic_args_have_placeholders(scoped_source_ids) {
				return decl_key, scoped_source_ids.clone()
			}
		}
	} else if args_source_ids := t.explicit_generic_call_args(node, module_name) {
		scoped_source_ids_2 := t.infer_generic_call_args_with_explicit(decl, id, node, module_name, args_source_ids) or {
			[]u32{}
		}

		if scoped_source_ids_2.len > 0 && !t.generic_args_have_placeholders(scoped_source_ids_2)
			&& t.generic_args_contain_alias(scoped_source_ids_2, module_name) {
			return decl_key, scoped_source_ids_2.clone()
		}
	}
	if args_source_ids := t.specialized_plain_generic_call_args(node, decl, module_name) {
		if args_source_ids.len > 0 && !t.generic_args_have_placeholders(args_source_ids) {
			return decl_key, args_source_ids.clone()
		}
	}
	if args_source_ids := t.infer_generic_call_args(decl, id, node, module_name) {
		if args_source_ids.len > 0 && !t.generic_args_have_placeholders(args_source_ids) {
			return decl_key, args_source_ids.clone()
		}
	}
	if args_source_ids := t.infer_generic_call_args_from_receiver_record(decl, node, module_name) {
		if args_source_ids.len > 0 && !t.generic_args_have_placeholders(args_source_ids) {
			return decl_key, args_source_ids.clone()
		}
	}
	if args_source_ids := t.specialized_plain_generic_call_args(node, decl, module_name) {
		if args_source_ids.len > 0 && !t.generic_args_have_placeholders(args_source_ids) {
			return decl_key, args_source_ids.clone()
		}
	}
	if args_source_ids := t.explicit_generic_call_args(node, module_name) {
		scoped_source_ids_3 := t.infer_generic_call_args_with_explicit(decl, id, node, module_name, args_source_ids) or {
			[]u32{}
		}

		if scoped_source_ids_3.len > 0 && !t.generic_args_have_placeholders(scoped_source_ids_3) {
			return decl_key, scoped_source_ids_3.clone()
		}
	}
	// Only generic calls whose arguments are not inferable yet need another pass
	// after declaration-assignment types have been refreshed. Recording every
	// ordinary call here makes that late pass scan most of a large program.
	t.generic_call_spec_misses[int(id)] = true
	return none
}

fn (t &Transformer) generic_call_args_for_decl(args_ids []u32, call_module string, decl_module string) []u32 {
	mut scoped := []u32{cap: args_ids.len}
	for arg_source_id in args_ids {
		scoped << t.generic_arg_for_call_and_decl_module(arg_source_id, call_module, decl_module)
	}
	return scoped.clone()
}

fn (mut t Transformer) infer_generic_call_args_from_receiver_record(decl GenericFnDecl, node flat.Node, call_module string) ?[]u32 {
	if !t.generic_decl_is_receiver_method(decl.node) || node.children_count == 0
		|| t.generic_call_is_static_assoc_selector(node, decl) {
		return none
	}
	recv_id := t.generic_call_receiver_id(node) or { return none }
	recv_type_source_id := t.generic_call_arg_type_for_inference(recv_id)

	recorded := t.recorded_type_specialization_args(recv_type_source_id) or { return none }
	if recorded.len == 0 {
		return none
	}
	params := t.generic_fn_param_names(decl.node, decl.module)
	if params.len == 0 {
		return none
	}
	mut args := []u32{}
	mut recorded_idx := 0
	for param in params {
		if !is_generic_fn_placeholder_name(param) {
			continue
		}
		if recorded_idx >= recorded.len {
			return none
		}
		args << t.generic_arg_for_call_and_decl_module(recorded[recorded_idx], call_module, decl.module)
		recorded_idx++
	}
	if args.len == 0 {
		return none
	}
	return args.clone()
}

fn (t &Transformer) call_has_source_generic_args(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	fn_node := t.a.child_node(&node, 0)
	return fn_node.kind == .index && fn_node.children_count >= 2 && fn_node.value != 'range'
		&& !t.index_callee_is_value_index(fn_node)
}

fn (t &Transformer) call_is_normalized_explicit_generic_call(node flat.Node) bool {
	if node.call_type_args().len == 0 || node.children_count == 0 {
		return false
	}
	// normalize_generic_call_expr removes the source index node from both
	// `fn[T](...)` and `module.fn[T](...)`, but preserves `T` in the call payload.
	return t.a.child_node(&node, 0).kind in [.ident, .selector]
}

fn (mut t Transformer) generic_call_decl_key(id flat.NodeId, node flat.Node, module_name string, decls map[string]GenericFnDecl) ?string {
	if node.children_count == 0 {
		return none
	}
	mut callee_id := t.a.child(&node, 0)
	mut callee := t.a.nodes[int(callee_id)]
	if callee.kind == .index && callee.children_count > 0 && callee.value != 'range' {
		if t.index_callee_is_value_index(callee) {
			return none
		}
		callee_id = t.a.child(&callee, 0)
		callee = t.a.nodes[int(callee_id)]
	}
	if callee.kind == .ident && !callee.value_text().contains('.') {
		if (t.raw_var_type(callee.value_text())) != 0 {
			return none
		}
	}
	// Non-generic helpers cannot be specialized. Resolve a candidate before looking
	// up lexical bindings: synthesized callees have no source-parent index entry,
	// so an unnecessary lookup can scan the entire AST for every cloned helper call.
	key := t.unshadowed_generic_call_decl_key(id, node, callee, module_name, decls) or {
		return none
	}
	if callee.kind == .ident && !callee.value_text().contains('.') {
		// Collection, cloning and body pre-passes run before locals enter var_types.
		// During declaration-ordered specialization lowering, the live scope resolves
		// shadows without repeatedly scanning the whole AST for synthesized ancestors.
		// Recorded specialization names already identify generated function callees.
		if (!t.validating_generic_spec || !t.fn_body_locals_in_scope || t.in_monomorphize_scan
			|| t.cloning_generic_fn_depth > 0)
			&& !t.generic_callee_is_specialization(callee.value_text()) {
			if _ := t.local_binding_before(callee.value_text(), callee_id) {
				return none
			}
		}
	}
	return key
}

fn (mut t Transformer) unshadowed_generic_call_decl_key(id flat.NodeId, node flat.Node, callee flat.Node, module_name string, decls map[string]GenericFnDecl) ?string {
	mut resolved := ''
	mut has_resolved := false
	if !isnil(t.tc) {
		if name := t.tc.resolved_call_name(id) {
			resolved = name
			has_resolved = true
		}
	}
	if t.generic_call_cannot_name_decl(resolved, has_resolved, callee, decls) {
		return none
	}
	if !isnil(t.tc) {
		if has_resolved {
			if key := t.generic_resolved_call_decl_key(resolved, callee, node, module_name, decls) {
				if decl := decls[key] {
					if !t.generic_call_arg_count_matches_decl(node, decl) {
						return none
					}
					return key
				}
			}
			// A concrete receiver declaration is not part of the generic declaration
			// index. Preserve the checker's exact choice instead of falling through to
			// a broader structured receiver pattern with the same method name.
			if callee.kind == .selector
				&& t.resolved_call_is_concrete_fn(resolved, generic_fn_decl_base_value(resolved)) {
				return none
			}
		}
	}
	if callee.kind == .ident {
		direct_key := generic_fn_decl_base_value(callee.value_text())
		if decl := decls[direct_key] {
			if t.generic_call_arg_count_matches_decl(node, decl) {
				return direct_key
			}
		}
		// Receiver calls lowered from a checker-selected concrete declaration use
		// an identifier callee. Keep that exact declaration instead of rematching
		// the call against an indexed generic receiver pattern.
		if t.plain_concrete_fn_known(callee.value_text(), module_name, decls) {
			return none
		}
		// Synthesized method calls use an identifier callee and can encode only the
		// concrete generic arguments, not a structured receiver pattern such as
		// `Box[[]T]`. Match those declarations against the call's receiver value.
		if callee.value_text().contains('.') {
			method := callee.value_text().all_after_last('.')
			for key in t.generic_receiver_methods_by_name[method] {
				decl := decls[key] or { continue }
				if t.resolved_generic_decl_matches_callee_receiver(callee, node, decl,
					module_name) && t.generic_call_arg_count_matches_decl(node, decl) {
					return key
				}
			}
		}
		if key := t.generic_flat_receiver_call_decl_key(callee.value_text(), module_name, decls) {
			if decl := decls[key] {
				if t.generic_call_arg_count_matches_decl(node, decl) {
					return key
				}
			}
		}
		if key := t.specialized_plain_generic_call_decl_key(callee.value_text(), module_name, decls) {
			if decl := decls[key] {
				if t.generic_call_arg_count_matches_decl(node, decl) {
					return key
				}
			}
		}
		if t.local_concrete_fn_shadows_generic(callee.value_text(), module_name, decls)
			|| t.plain_concrete_fn_known(callee.value_text(), module_name, decls) {
			return none
		}
		for candidate in t.generic_plain_call_candidates(callee.value_text(), module_name) {
			key := generic_fn_decl_base_value(candidate)
			if decl := decls[key] {
				if !t.generic_call_arg_count_matches_decl(node, decl) {
					continue
				}
				return key
			}
		}
	} else if callee.kind == .selector && callee.children_count > 0 {
		base_id := t.a.child(&callee, 0)
		if static_key := t.generic_static_assoc_call_decl_key(base_id, callee.value_text(), decls) {
			if decl := decls[static_key] {
				if t.generic_call_arg_count_matches_decl(node, decl) {
					return static_key
				}
			}
		}
		base := t.a.nodes[int(base_id)]
		if base.kind == .ident && t.ident_is_import_alias(base.value_text()) {
			mod_name := t.import_alias_module(base.value_text())
			for candidate in ['${mod_name}.${callee.value_text()}',
				'${base.value_text()}.${callee.value_text()}'] {
				key := generic_fn_decl_base_value(candidate)
				if decl := decls[key] {
					if !t.generic_call_arg_count_matches_decl(node, decl) {
						continue
					}
					return key
				}
			}
		}
		method_keys := t.generic_receiver_methods_by_name[callee.value_text()] or { return none }
		mut base_type := t.syntactic_static_call_return_type(base_id)
		if base_type == 0 {
			base_type = t.node_type(base_id)
		}
		// Calls in a generic match branch must bind to the concrete variant, even
		// when the sum type declares a same-named generic method.
		if sc := t.find_smartcast_for_expr(base_id) {
			target_type_source_id := t.smartcast_target_type(sc)
			if target_type_source_id != 0 {
				base_type = target_type_source_id
			}
		}
		base_type = t.trim_pointer_type(transform_unshared_receiver_type(base_type))
		mut receiver_types := [base_type]
		if base.kind == .ident {
			if declared_type_source_id := t.local_decl_type_before_pos(base.value_text(), node) {
				if declared_type_source_id !in receiver_types {
					receiver_types << declared_type_source_id
				}
			}
			if declared_type_source_id := t.local_decl_type_before(base.value_text(), base_id) {
				if declared_type_source_id !in receiver_types {
					receiver_types << declared_type_source_id
				}
			}
		}
		base_syntax := flat.source_type_syntax(base_type)
		if base_syntax.generic_app_valid() {
			key := t.generic_receiver_decl_key(flat.source_type_name(base_syntax.generic_app_base()), callee.value_text(), decls)
			if decl := decls[key] {
				if !t.generic_call_arg_count_matches_decl(node, decl) {
					return none
				}
				return key
			}
		}
		for key in method_keys {
			if decl := decls[key] {
				mut receiver_matches := false
				for receiver_type in receiver_types {
					if t.generic_receiver_decl_matches_type(receiver_type, decl, module_name) {
						receiver_matches = true
						break
					}
					mut seen := map[u32]bool{}
					if t.generic_decl_matches_embedded_receiver(receiver_type, decl, module_name, mut seen) {
						receiver_matches = true
						break
					}
				}
				if !receiver_matches {
					continue
				}
				if !t.generic_call_arg_count_matches_decl(node, decl) {
					continue
				}
				return key
			}
		}
	}
	return none
}

fn generic_receiver_match_base(type_id u32) u32 {
	if !flat.source_type_shape(type_id).has(.bracketed) {
		return type_id
	}
	syntax := flat.source_type_syntax(type_id)
	return if syntax.generic_app_valid() { syntax.generic_app_base() } else { type_id }
}

fn (t &Transformer) generic_receiver_decl_matches_type(base_type_id u32, decl GenericFnDecl, module_name string) bool {
	decl_receiver := flat.source_type_id(generic_fn_decl_base_value(decl.node.value).all_before_last('.'))
	if decl_receiver == 0 {
		return false
	}
	clean_base := generic_receiver_match_base(t.trim_pointer_type(transform_unshared_receiver_type(base_type_id)))
	if clean_base == 0 {
		return false
	}
	call_qualified := t.normalize_type_in_module(clean_base, module_name)
	decl_qualified := t.normalize_type_in_module(decl_receiver, decl.module)
	qualified_call_base := generic_receiver_match_base(call_qualified)
	qualified_decl_base := generic_receiver_match_base(decl_qualified)
	if flat.source_type_shape(qualified_call_base).has(.dotted)
		&& flat.source_type_shape(qualified_decl_base).has(.dotted)
		&& qualified_call_base != qualified_decl_base {
		// Receiver short names are not unique across modules. Once both sides can
		// be resolved, do not let `json2.Decoder` match `decoder2.Decoder` merely
		// because both declarations are named `Decoder`.
		return false
	}
	mut call_receivers := []u32{}
	for candidate in [clean_base, t.normalize_type_alias(clean_base),
		t.normalize_type_in_module(clean_base, module_name)] {
		mut roots := [candidate]
		generic_base := generic_receiver_match_base(candidate)
		if generic_base != candidate && generic_base != 0 {
			roots << generic_base
		}
		for root in roots {
			if root != 0 && root !in call_receivers {
				call_receivers << root
			}
			if flat.source_type_shape(root).has(.dotted) {
				short := flat.source_type_syntax(root).unqualified()
				if short != 0 && short !in call_receivers {
					call_receivers << short
				}
			}
		}
	}
	mut decl_receivers := []u32{}
	for candidate in [decl_receiver, t.normalize_type_alias(decl_receiver),
		t.normalize_type_in_module(decl_receiver, decl.module)] {
		mut roots := [candidate]
		generic_base := generic_receiver_match_base(candidate)
		if generic_base != candidate && generic_base != 0 {
			roots << generic_base
		}
		for root in roots {
			if root != 0 && root !in decl_receivers {
				decl_receivers << root
			}
			if flat.source_type_shape(root).has(.dotted) {
				short := flat.source_type_syntax(root).unqualified()
				if short != 0 && short !in decl_receivers {
					decl_receivers << short
				}
			} else if decl.module.len > 0 && decl.module !in ['main', 'builtin'] {
				qualified := flat.source_type_composition_two(.qualified, flat.source_type_id(decl.module), root)
				if qualified !in decl_receivers {
					decl_receivers << qualified
				}
			}
		}
	}
	for call_receiver in call_receivers {
		if call_receiver in decl_receivers {
			return true
		}
	}
	return false
}

fn (t &Transformer) generic_static_assoc_call_decl_key(base_id flat.NodeId, method string, decls map[string]GenericFnDecl) ?string {
	if method == '' || int(base_id) < 0 || int(base_id) >= t.a.nodes.len {
		return none
	}
	for type_name in t.generic_static_assoc_type_candidates(base_id) {
		for method_spelling in generic_call_name_spellings(method) {
			key := generic_fn_decl_base_value(flat.encode_static_type_method_name(flat.source_type_name(type_name), method_spelling))
			if key in decls {
				return key
			}
		}
	}
	return none
}

fn (t &Transformer) generic_call_is_static_assoc_selector(node flat.Node, decl GenericFnDecl) bool {
	if node.children_count == 0 {
		return false
	}
	decl_value := generic_fn_decl_base_value(decl.node.value_text())
	_, method := flat.decode_static_type_method_name(decl_value) or { return false }
	callee_id := t.a.child(&node, 0)
	callee := t.a.nodes[int(callee_id)]
	if callee.kind != .selector || callee.children_count == 0 {
		return false
	}
	if callee.value_text() !in generic_call_name_spellings(method) {
		return false
	}
	base_id := t.a.child(callee, 0)
	for type_name in t.generic_static_assoc_type_candidates(base_id) {
		for method_spelling in generic_call_name_spellings(method) {
			key := generic_fn_decl_base_value(flat.encode_static_type_method_name(flat.source_type_name(type_name), method_spelling))
			if key == decl.key || key == decl_value
				|| transform_qualified_fn_name(decl.module, key) == decl.key {
				return true
			}
		}
	}
	return false
}

fn (t &Transformer) generic_static_assoc_type_candidates(base_id flat.NodeId) []u32 {
	base := t.a.nodes[int(base_id)]
	mut candidates := []u32{}
	if base.kind == .ident {
		if base.value_text() == 'C' || t.ident_is_import_alias(base.value_text()) {
			return candidates
		}
		candidates << base.source_value_id()
	} else if base.kind == .selector && base.children_count > 0 {
		inner := t.a.child_node(&base, 0)
		if inner.kind == .ident {
			if t.ident_is_import_alias(inner.value_text()) {
				mod_name := t.import_alias_module(inner.value_text())
				candidates << flat.source_type_id('${mod_name}.${base.value_text()}')
			}
			candidates << flat.source_type_id('${inner.value_text()}.${base.value_text()}')
		}
	}
	mut result := []u32{}
	for candidate in candidates {
		for type_name in t.static_assoc_type_candidates(candidate) {
			if type_name !in result {
				result << type_name
			}
		}
	}
	return result
}

fn (t &Transformer) plain_concrete_fn_known(name string, module_name string, decls map[string]GenericFnDecl) bool {
	if name == '' {
		return false
	}
	qname := transform_qualified_fn_name(module_name, name)
	for candidate in [qname, name] {
		if decl := decls[candidate] {
			if generic_decl_module_matches_call_module(decl.module, module_name) {
				return false
			}
		}
		if candidate in t.fn_ret_types.ids {
			return true
		}
		if !isnil(t.tc) && t.concrete_fn_return_known(candidate) {
			return true
		}
	}
	return false
}

fn (t &Transformer) generic_flat_receiver_call_decl_key(name string, module_name string, decls map[string]GenericFnDecl) ?string {
	if !name.contains('_') {
		return none
	}
	receiver, method := generic_flat_receiver_call_parts(name) or { return none }
	if receiver.len == 0 || method.len == 0 {
		return none
	}
	method_keys := t.generic_receiver_methods_by_name[method] or { return none }
	for key in method_keys {
		decl := decls[key] or { continue }
		decl_receiver := generic_fn_decl_base_value(decl.node.value).all_before_last('.')
		if decl_receiver.len == 0 {
			continue
		}
		if generic_flat_receiver_matches(receiver, decl_receiver, module_name, decl.module) {
			return key
		}
	}
	return none
}

fn (t &Transformer) specialized_plain_generic_call_decl_key(name string, module_name string, decls map[string]GenericFnDecl) ?string {
	if !name.contains('_T_') {
		return none
	}
	if !isnil(t.tc) && name in t.tc.fn_ret_types && !t.tc.specialized_generic_fns[name] {
		// `_T_` is valid in an ordinary function name. Prefer that concrete source
		// declaration over decoding the suffix as a generated specialization.
		return none
	}
	base := name.all_before('_T_')
	if base.len == 0 {
		return none
	}
	mut candidates := [base]
	if base.contains('__') {
		candidates << base.replace('__', '.')
	}
	if module_name != '' && module_name != 'main' && module_name != 'builtin'
		&& !base.contains('.') && !base.contains('__') {
		candidates << '${module_name}.${base}'
	}
	for candidate in candidates {
		key := generic_fn_decl_base_value(candidate)
		if key in decls {
			return key
		}
	}
	return none
}

fn (mut t Transformer) specialized_plain_generic_call_args(node flat.Node, decl GenericFnDecl, module_name string) ?[]u32 {
	if node.children_count == 0 {
		return none
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .ident {
		return none
	}
	params := t.generic_fn_param_names(decl.node, decl.module)
	if recorded_source_ids := t.recorded_generic_specialization_args(callee.value_text()) {
		if recorded_source_ids.len == params.len {
			// The specialized callee name is already bound to these canonical concrete
			// arguments. Re-resolving a bare main type in the caller's module can turn
			// `fill_T_Item` into `fill_T_other__Item` during the refresh scan.
			return recorded_source_ids.clone()
		}
	}
	if !callee.value_text().contains('_T_') {
		return none
	}
	suffix := callee.value_text().all_after('_T_')
	if suffix.len == 0 {
		return none
	}
	if params.len != 1 {
		return none
	}
	arg := t.generic_type_arg_from_suffix_with_containers_id(flat.source_type_id(suffix))
	if arg == 0 {
		return none
	}
	return [t.generic_arg_for_call_and_decl_module(arg, module_name, decl.module)]
}

fn generic_flat_receiver_call_parts(name string) ?(string, string) {
	if name.contains('.') {
		receiver := name.all_before_last('.')
		method := name.all_after_last('.')
		if receiver.len > 0 && method.len > 0 {
			return receiver, method
		}
	}
	if name.contains('__') {
		receiver := name.all_before_last('__')
		method := name.all_after_last('__')
		if receiver.len > 0 && method.len > 0 {
			return receiver, method
		}
	}
	return none
}

fn generic_flat_receiver_matches(receiver string, decl_receiver string, module_name string, decl_module string) bool {
	mut bases := []u32{}
	decl_receiver_id := flat.source_type_id(decl_receiver)
	mut receiver_roots := [decl_receiver_id]
	decl_syntax := flat.source_type_syntax(decl_receiver_id)
	if decl_syntax.generic_app_valid() && decl_syntax.generic_app_base() != 0 {
		receiver_roots << decl_syntax.generic_app_base()
	}
	for root in receiver_roots {
		if root == 0 || root in bases {
			continue
		}
		bases << root
		if decl_module != '' && decl_module != 'main' && decl_module != 'builtin'
			&& !flat.source_type_shape(root).has(.dotted) {
			bases << flat.source_type_composition_two(.qualified, flat.source_type_id(decl_module), root)
		}
		if module_name != '' && module_name != 'main' && module_name != 'builtin'
			&& !flat.source_type_shape(root).has(.dotted) {
			bases << flat.source_type_composition_two(.qualified, flat.source_type_id(module_name), root)
		}
		if flat.source_type_shape(root).has(.dotted) {
			syntax := flat.source_type_syntax(root)
			short_receiver := syntax.unqualified()
			bases << short_receiver
			bases << flat.source_type_composition_two(.qualified, syntax.qualifier(), short_receiver)
		}
	}
	for base_id in bases {
		if base_id == 0 {
			continue
		}
		base := flat.source_type_name(base_id)
		if receiver.starts_with('${base}_') {
			return true
		}
		cbase := c_name(base)
		if cbase != base && receiver.starts_with('${cbase}_') {
			return true
		}
	}
	return false
}

fn (t &Transformer) generic_resolved_call_decl_key(resolved string, callee flat.Node, node flat.Node, module_name string, decls map[string]GenericFnDecl) ?string {
	mut resolved_candidates := []string{}
	push_generic_call_name_spellings(mut resolved_candidates, resolved)
	if module_name != '' && module_name !in ['main', 'builtin'] && !resolved.starts_with('${module_name}.') {
		push_generic_call_name_spellings(mut resolved_candidates, '${module_name}.${resolved}')
	}
	for candidate in resolved_candidates {
		if decl := decls[candidate] {
			if t.resolved_generic_decl_matches_callee_receiver(callee, node, decl, module_name) {
				return candidate
			}
		}
	}
	key := generic_fn_decl_base_value(resolved)
	for spelling in generic_call_name_spellings(key) {
		if decl := decls[spelling] {
			if !t.resolved_generic_decl_matches_callee_receiver(callee, node, decl, module_name) {
				continue
			}
			return spelling
		}
	}
	if flat_key := t.generic_flat_receiver_call_decl_key(key, module_name, decls) {
		return flat_key
	}
	if flat_key := t.generic_flat_receiver_call_decl_key(resolved, module_name, decls) {
		return flat_key
	}
	if callee.kind == .ident
		&& (t.local_concrete_fn_shadows_generic(callee.value_text(), module_name, decls)
			|| t.resolved_call_is_concrete_fn(resolved, key)) {
		return none
	}
	resolved_mod := if key.contains('.') { key.all_before_last('.') } else { module_name }
	short := key.all_after_last('.')
	if resolved_mod.len > 0 {
		for short_spelling in generic_call_name_spellings(short) {
			qshort := '${resolved_mod}.${short_spelling}'
			if qshort in decls {
				return qshort
			}
		}
	}
	for short_spelling in generic_call_name_spellings(short) {
		if decl := decls[short_spelling] {
			if decl.module == resolved_mod || decl.module == module_name {
				return short_spelling
			}
		}
	}
	if callee.kind == .ident
		&& !t.local_concrete_fn_shadows_generic(callee.value_text(), module_name, decls) {
		for candidate in t.generic_plain_call_candidates(callee.value_text(), module_name) {
			candidate_key := generic_fn_decl_base_value(candidate)
			if candidate_key in decls {
				return candidate_key
			}
		}
	}
	return none
}

fn (t &Transformer) resolved_generic_decl_matches_callee_receiver(callee flat.Node, node flat.Node, decl GenericFnDecl, module_name string) bool {
	if !t.generic_decl_is_receiver_method(decl.node) {
		return true
	}
	mut base_id := flat.empty_node
	if callee.kind == .selector && callee.children_count > 0 {
		base_id = t.a.child(&callee, 0)
	} else if callee.kind == .ident && node.children_count > 1 {
		base_id = t.a.child(&node, 1)
	} else {
		return true
	}
	mut base_type := t.syntactic_static_call_return_type(base_id)
	if base_type == 0 {
		base_type = t.node_type(base_id)
	}
	if base_type == 0 {
		return true
	}
	if t.generic_receiver_decl_matches_type(base_type, decl, module_name) {
		return true
	}
	// The semantic type of a generic function alias is its expanded `fn (...)`
	// signature. Recover the source declaration type before rejecting the
	// checker-selected alias method, so `ParseFunction[string].parse` can still
	// match the open `ParseFunction[T]` receiver.
	base := t.a.nodes[int(base_id)]
	if base.kind == .ident {
		if declared_type_source_id := t.local_decl_type_before_pos(base.value_text(), node) {
			if t.generic_receiver_decl_matches_type(declared_type_source_id, decl, module_name) {
				return true
			}
		}
		if declared_type_source_id := t.local_decl_type_before(base.value_text(), base_id) {
			if t.generic_receiver_decl_matches_type(declared_type_source_id, decl, module_name) {
				return true
			}
		}
	}
	mut seen := map[u32]bool{}
	return t.generic_decl_matches_embedded_receiver(base_type, decl, module_name, mut seen)
}

fn (t &Transformer) generic_decl_matches_embedded_receiver(base_type_id u32, decl GenericFnDecl, module_name string, mut seen map[u32]bool) bool {
	mut lookup_type := t.trimmed_type_id(t.trim_all_pointer_type(t.trimmed_type_id(base_type_id)))
	if flat.source_type_shape(lookup_type).has(.mutable_type) {
		lookup_type = t.trimmed_type_id(flat.source_type_syntax(lookup_type).raw_base())
	}
	if lookup_type in seen {
		return false
	}
	seen[lookup_type] = true
	if lookup_type !in t.embedded_fields && flat.source_type_shape(lookup_type).has(.dotted) {
		short_type := flat.source_type_syntax(lookup_type).unqualified()
		if short_type in t.embedded_fields {
			lookup_type = short_type
		}
	}
	for field in t.embedded_fields[lookup_type] or { []FieldInfo{} } {
		field_type := if field.raw_typ != 0 { field.raw_typ } else { field.typ }
		clean_field := t.trimmed_type_id(t.trim_all_pointer_type(t.trimmed_type_id(field_type)))
		if t.generic_receiver_decl_matches_type(clean_field, decl, module_name) {
			return true
		}
		if t.generic_decl_matches_embedded_receiver(clean_field, decl, module_name, mut seen) {
			return true
		}
	}
	return false
}

fn (t &Transformer) syntactic_static_call_return_type(id flat.NodeId) u32 {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return u32(0)
	}
	node := t.a.nodes[int(id)]
	if node.kind != .call || node.children_count == 0 {
		return u32(0)
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .selector || callee.children_count == 0 {
		return u32(0)
	}
	base := t.a.child_node(callee, 0)
	mut owner := ''
	if base.kind == .ident {
		owner = base.value_text()
	} else if base.kind == .selector && base.children_count > 0 {
		inner := t.a.child_node(base, 0)
		if inner.kind == .ident {
			owner = '${inner.value_text()}.${base.value_text()}'
		}
	}
	if owner == '' {
		return u32(0)
	}
	name := '${owner}.${callee.value_text()}'
	for candidate in [name, transform_qualified_fn_name(t.cur_module, name)] {
		if ret_source_id := t.fn_ret_types.type_id(candidate) {
			return ret_source_id
		}
		if !isnil(t.tc) {
			if typ := t.tc.fn_ret_types[candidate] {
				ret_source_id := typ.source_type_id()
				if ret_source_id !in [u32(0), flat.source_type_unknown, flat.source_type_void] {
					return ret_source_id
				}
			}
		}
	}
	if callee.value_text() == 'new' {
		for type_name in t.static_assoc_type_candidates(flat.source_type_id(owner)) {
			return type_name
		}
		if t.syntactic_new_owner_is_type_like(base, owner) {
			return flat.source_type_id(owner)
		}
	}
	return u32(0)
}

fn (t &Transformer) syntactic_new_owner_is_type_like(base flat.Node, owner string) bool {
	short_owner := owner.all_after_last('.')
	if short_owner.len == 0 || !v3_type_name_starts_upper(short_owner) {
		return false
	}
	if base.kind == .ident {
		return (t.var_type(base.value_text())) == 0 && !t.ident_is_import_alias(base.value_text())
	}
	if base.kind == .selector && base.children_count > 0 {
		inner := t.a.child_node(&base, 0)
		if inner.kind != .ident || (t.var_type(inner.value_text())) != 0 {
			return false
		}
		return t.ident_is_import_alias(inner.value_text()) || v3_type_name_starts_upper(inner.value_text())
	}
	return false
}

fn v3_type_name_starts_upper(name string) bool {
	if name == '' {
		return false
	}
	return name[0] >= `A` && name[0] <= `Z`
}

fn (t &Transformer) resolved_call_is_concrete_fn(resolved string, key string) bool {
	for candidate in [resolved, key] {
		if candidate.len == 0 {
			continue
		}
		if candidate in t.fn_ret_types.ids {
			return true
		}
		if !isnil(t.tc) && t.concrete_fn_return_known(candidate) {
			return true
		}
	}
	return false
}

fn (t &Transformer) generic_call_arg_count_matches_decl(node flat.Node, decl GenericFnDecl) bool {
	actual_args := t.generic_call_effective_arg_count(node)
	is_receiver := t.generic_decl_is_receiver_method(decl.node)
		&& !t.generic_call_is_static_assoc_selector(node, decl)
	actual := if is_receiver && t.call_is_selector_form(node) {
		actual_args + 1
	} else {
		actual_args
	}
	return t.generic_call_actual_arg_count_matches_decl(actual, decl)
}

fn (t &Transformer) generic_call_arg_count_matches_decl_with_implicit_receiver(node flat.Node, decl GenericFnDecl) bool {
	return t.generic_call_actual_arg_count_matches_decl(t.generic_call_effective_arg_count(node) + 1, decl)
}

fn (t &Transformer) generic_call_actual_arg_count_matches_decl(actual int, decl GenericFnDecl) bool {
	mut param_count := 0
	mut is_variadic := false
	mut has_trailing_params_struct := false
	for i in 0 .. decl.node.children_count {
		child := t.a.child_node(&decl.node, i)
		if child.kind != .param {
			if t.prefix_param_scan {
				break
			}
			continue
		}
		param_count++
		if flat.source_type_shape(child.typ).has(.variadic) {
			is_variadic = true
		}
		has_trailing_params_struct = t.generic_param_is_params_struct(child.typ, decl.module)
	}
	if is_variadic {
		return actual >= param_count - 1
	}
	if has_trailing_params_struct {
		return actual >= param_count - 1
	}
	return actual == param_count
}

fn (t &Transformer) generic_param_is_params_struct(param_type_id u32, decl_module string) bool {
	if param_type_id == u32(0) {
		return false
	}
	if _ := t.params_struct_type_name(param_type_id) {
		return true
	}
	if !flat.source_type_shape(param_type_id).has(.dotted) && decl_module != '' && decl_module != 'main'
		&& decl_module != 'builtin' {
		if _ := t.params_struct_type_name(flat.source_type_composition_two(.qualified, flat.source_type_id(decl_module), param_type_id)) {
			return true
		}
	}
	return false
}

fn (t &Transformer) generic_call_effective_arg_count(node flat.Node) int {
	if node.children_count <= 1 {
		return 0
	}
	mut count := 0
	mut i := 1
	for i < node.children_count {
		arg := t.a.child_node(&node, i)
		if arg.kind == .field_init {
			count++
			for i < node.children_count {
				field := t.a.child_node(&node, i)
				if field.kind != .field_init {
					break
				}
				i++
			}
			continue
		}
		count++
		i++
	}
	return count
}

fn (t &Transformer) local_concrete_fn_shadows_generic(name string, module_name string, decls map[string]GenericFnDecl) bool {
	qname := transform_qualified_fn_name(module_name, name)
	if decl := decls[name] {
		if generic_decl_module_matches_call_module(decl.module, module_name) {
			return false
		}
	}
	if decl := decls[qname] {
		if generic_decl_module_matches_call_module(decl.module, module_name) {
			return false
		}
	}
	if qname in t.fn_ret_types.ids {
		return true
	}
	if isnil(t.tc) {
		return t.fn_ret_types.contains(name)
	}
	if qname in t.tc.fn_param_types || qname in t.tc.fn_ret_types {
		return true
	}
	if !t.fn_ret_types.contains(name) && name !in t.tc.fn_param_types && name !in t.tc.fn_ret_types {
		return false
	}
	decl_module := t.tc.fn_type_modules[name] or { '' }
	source_module := if t.cur_fn_source_module.len > 0 {
		t.cur_fn_source_module
	} else {
		module_name
	}
	return generic_decl_module_matches_call_module(decl_module, source_module)
}

fn generic_decl_module_matches_call_module(decl_module string, call_module string) bool {
	return decl_module == call_module
		|| (decl_module in ['', 'main'] && call_module in ['', 'main'])
}

fn (t &Transformer) generic_plain_call_candidates(name string, module_name string) []string {
	mut candidates := []string{}
	if module_name != '' && module_name != 'main' && module_name != 'builtin' {
		push_generic_call_name_spellings(mut candidates, '${module_name}.${name}')
	}
	push_generic_call_name_spellings(mut candidates, name)
	if !isnil(t.tc) {
		qname := t.tc.qualify_fn_name(name)
		push_generic_call_name_spellings(mut candidates, qname)
		if selected := t.tc.resolve_any_selective_import_fn(name) {
			push_generic_call_name_spellings(mut candidates, selected)
		}
	}
	return candidates
}

fn push_generic_call_name_spellings(mut names []string, name string) {
	for spelling in generic_call_name_spellings(name) {
		if spelling !in names {
			names << spelling
		}
	}
}

fn (t &Transformer) generic_receiver_decl_key(base_name string, method string, decls map[string]GenericFnDecl) string {
	for method_spelling in generic_call_name_spellings(method) {
		direct := '${base_name}.${method_spelling}'
		if direct in decls {
			return direct
		}
	}
	short := base_name.all_after_last('.')
	for method_spelling in generic_call_name_spellings(method) {
		for key, _ in decls {
			if key == '${short}.${method_spelling}' || key.ends_with('.${short}.${method_spelling}') {
				return key
			}
		}
	}
	return '${base_name}.${method}'
}

fn (t &Transformer) explicit_generic_call_args(node flat.Node, module_name string) ?[]u32 {
	if node.call_type_args().len > 0 {
		return normalize_generic_args(node.call_type_args(), module_name).clone()
	}
	if node.children_count == 0 {
		return none
	}
	fn_node := t.a.node(t.a.child(&node, 0))
	if fn_node.kind != .index || fn_node.children_count < 2 || fn_node.value == 'range' {
		return none
	}
	type_arg := t.generic_call_type_args(fn_node)
	if type_arg.len == 0 {
		return none
	}
	return normalize_generic_args(type_arg, module_name).clone()
}

fn (t &Transformer) generic_call_arg_id_for_param(node flat.Node, param_idx int, is_receiver bool) ?flat.NodeId {
	is_recv_param := is_receiver && param_idx == 0
	selector_form := is_receiver && t.call_is_selector_form(node)
	if is_recv_param {
		if selector_form {
			return t.generic_call_receiver_id(node)
		}
		if int(node.children_count) <= 1 {
			return none
		}
		return t.a.child(&node, 1)
	}
	arg_idx := if selector_form { param_idx } else { param_idx + 1 }
	if arg_idx >= int(node.children_count) {
		return none
	}
	return t.a.child(&node, arg_idx)
}

fn (mut t Transformer) infer_generic_call_args(decl GenericFnDecl, _id flat.NodeId, node flat.Node, call_module string) ?[]u32 {
	mut inferred := map[string]u32{}
	return (t.infer_generic_call_args_seeded(decl, _id, node, call_module, mut inferred)?).clone()
}

fn (mut t Transformer) infer_generic_call_args_with_explicit(decl GenericFnDecl, id flat.NodeId, node flat.Node, call_module string, explicit_ids []u32) ?[]u32 {
	explicit := explicit_ids
	param_names := t.generic_fn_param_names(decl.node, decl.module)
	mut method_explicit := explicit.clone()
	mut method_param_count := 0
	mut receiver_params := []string{}
	if t.generic_decl_is_receiver_method(decl.node) {
		receiver_params = t.generic_receiver_param_names(decl)
		for raw_param in decl.node.generic_params() {
			param := generic_param_name_from_decl_param(raw_param)
			if param.len > 0 && param !in receiver_params {
				method_param_count++
			}
		}
		// V accepts the full legacy spelling `value.method[T, U]()` as well as
		// `value.method[U]()`. The receiver's `T` is already inferred from value;
		// only the trailing method-level arguments seed this specialization.
		if method_param_count > 0 && explicit_ids.len == receiver_params.len + method_param_count {
			method_explicit = explicit[receiver_params.len..].clone()
		}
	}
	if param_names.len == 0 || method_explicit.len > param_names.len {
		return none
	}
	mut inferred := map[string]u32{}
	mut explicit_idx := 0
	mut decl_params := []string{}
	for raw_param in decl.node.generic_params() {
		param := generic_param_name_from_decl_param(raw_param)
		if param.len > 0 {
			decl_params << param
		}
	}
	// A method on a generic receiver (`fn (b Box[T]) get()`) declares no generic
	// params of its own: `T` is introduced by the receiver type. Its recorded
	// specialization still reaches this seeding path as the explicit list (a
	// receiver instantiated with an alias records its args on the call node), so
	// bind the receiver's params in order instead of finding nothing to seed and
	// losing the specialization.
	if decl_params.len == 0 && method_param_count == 0 {
		decl_params = receiver_params.clone()
	}
	for param in decl_params {
		if param in inferred || (method_param_count > 0 && param in receiver_params) {
			continue
		}
		if explicit_idx >= method_explicit.len {
			break
		}
		inferred[param] = method_explicit[explicit_idx]
		explicit_idx++
	}
	if explicit_idx != method_explicit.len {
		return none
	}
	return (t.infer_generic_call_args_seeded(decl, id, node, call_module, mut inferred)?).clone()
}

fn (mut t Transformer) infer_generic_call_args_seeded(decl GenericFnDecl, _id flat.NodeId, node flat.Node, call_module string, mut inferred map[string]u32) ?[]u32 {
	param_names := t.generic_fn_param_names(decl.node, decl.module)
	if param_names.len == 0 {
		return none
	}
	// Explicit generic arguments seed this map before value-argument inference.
	// Runtime argument types may fill missing parameters, but must not replace an
	// explicit pointer type with the mutable parameter's storage-stripped value type.
	pinned := inferred.clone()
	is_receiver := t.generic_decl_is_receiver_method(decl.node)
		&& !t.generic_call_is_static_assoc_selector(node, decl)
	receiver_params := if is_receiver {
		t.generic_receiver_param_names(decl)
	} else {
		[]string{}
	}
	expected_return := if t.expected_expr_node == int(_id) {
		t.generic_inference_expected_type(t.expected_expr_type)
	} else {
		u32(0)
	}
	expected_value := flat.source_type_syntax(expected_return).source_wrapper_base()
	// An interface target converts the result after the call. Its concrete
	// arguments decide T before that target may fill any missing parameters.
	interface_context := t.is_interface_type(expected_value)
	// An optional assignment may wrap the call's result. Infer its payload from
	// the arguments before allowing the destination to fill missing parameters.
	defer_return_inference := interface_context
		|| flat.source_type_shape(expected_return).has(.option_type)
	if expected_return != 0 && !defer_return_inference {
		t.infer_generic_return_type_args(decl, expected_return, mut inferred, receiver_params)
	}
	// A declared optional return still provides context to untyped literals.
	// Keep it separate so typed arguments retain priority over assignment wrapping.
	mut optional_literal_context := map[string]u32{}
	if flat.source_type_shape(expected_return).has(.option_type)
		&& flat.source_type_shape(decl.node.typ).has(.option_type) && !interface_context {
		t.infer_generic_return_type_args(decl, expected_return, mut optional_literal_context,
			receiver_params)
	}
	mut param_idx := 0
	param_count := t.generic_decl_param_count(decl.node)
	for i in 0 .. decl.node.children_count {
		child := t.a.child_node(&decl.node, i)
		if child.kind != .param {
			if t.prefix_param_scan {
				break
			}
			continue
		}
		is_recv_param := is_receiver && param_idx == 0
		if generic_param_text_is_variadic(child.typ) && param_idx == param_count - 1 {
			elem_param_type := generic_variadic_elem_param_text(child.typ)
			arg_ids := t.generic_call_arg_ids_from_param(node, param_idx, is_receiver)
			for arg_id in arg_ids {
				t.infer_generic_variadic_call_arg(child.typ, elem_param_type, arg_id, mut inferred)
			}
			param_idx++
			continue
		}
		arg_id := t.generic_call_arg_id_for_param(node, param_idx, is_receiver) or {
			param_idx++
			continue
		}
		mut inferred_arg_type := t.generic_call_arg_type_for_inference(arg_id)
		inference_param_type := generic_inference_param_type(child)
		if !flat.source_type_query(inference_param_type).has(.generic_fn_name) {
			inferred_arg_type = t.generic_composite_inference_alias_type(inferred_arg_type,
				call_module)
		}
		if child.is_mut && child.op == .amp && flat.source_type_shape(inferred_arg_type).has(.pointer) {
			// The explicit `mut p &T` marker addresses the caller's pointer slot;
			// it is ABI indirection, not part of T's inferred source type.
			inferred_arg_type = flat.source_type_syntax(inferred_arg_type).raw_base()
		}
		if is_recv_param && t.call_is_selector_form(node)
			&& t.generic_receiver_needs_decl_type_fallback(child.typ, inferred_arg_type) {
			recv := t.a.nodes[int(arg_id)]
			if recv.kind == .ident {
				lookup_before := if node.pos.offset > 0 { node } else { recv }
				if decl_type_source_id := t.local_decl_type_before_pos(recv.value_text(), lookup_before) {
					inferred_arg_type = t.generic_receiver_decl_inference_type(child.typ, decl_type_source_id, call_module)
				} else if decl_type_source_id := t.local_decl_type_before(recv.value_text(), _id) {
					inferred_arg_type = t.generic_receiver_decl_inference_type(child.typ, decl_type_source_id, call_module)
				}
			}
		}
		mut defer_numeric_literal := false
		arg_kind := t.a.nodes[int(arg_id)].kind
		if arg_kind in [.int_literal, .float_literal]
			&& flat.source_type_query(inference_param_type).has(.generic_fn_name) {
			mut later_param_idx := param_idx + 1
			for later_child_idx in i + 1 .. decl.node.children_count {
				later_child := t.a.child_node(&decl.node, later_child_idx)
				if later_child.kind != .param {
					continue
				}
				later_arg_id := t.generic_call_arg_id_for_param(node, later_param_idx, is_receiver) or {
					later_param_idx++
					continue
				}
				if generic_inference_param_type(later_child) == inference_param_type
					&& int(later_arg_id) >= 0 {
					later_kind := t.a.nodes[int(later_arg_id)].kind
					// A later argument that carries a type of its own decides the
					// placeholder. Between untyped numeric literals only a float may
					// override an int, never the other way round: otherwise
					// `f(0.0, 1, 1)` would take `int` from the trailing literal.
					if later_kind !in [.int_literal, .float_literal]
						|| (arg_kind == .int_literal && later_kind == .float_literal) {
						defer_numeric_literal = true
						break
					}
				}
				later_param_idx++
			}
		}
		if !defer_numeric_literal && arg_kind in [.int_literal, .float_literal] {
			param_name := flat.source_type_name(inference_param_type)
			if contextual_type := optional_literal_context[param_name] {
				if is_numeric_type_name(contextual_type) {
					inferred_arg_type = contextual_type
				}
			}
		}
		if (child.is_mut || child.op == .amp || flat.source_type_shape(child.typ).has(.mutable_type))
			&& flat.source_type_shape(inferred_arg_type).has(.pointer) {
			// A mutable parameter's leading pointer is its storage ABI, whether the
			// generic placeholder is direct (`T`) or nested (`Container[T]`). A `mut`
			// argument that is a field/selector (`mut g.cells`) arrives here still
			// carrying that `&`, unlike a plain `mut ident` whose value node already
			// dropped it — strip it so `[]T` matches `[]Cell`, not `&[]Cell`. Mirrors
			// infer_generic_call_args_from_params.
			inferred_arg_type = flat.source_type_syntax(inferred_arg_type).raw_base()
		}
		arg_type := generic_arg_type_for_param(inference_param_type, inferred_arg_type)
		if arg_type != 0 && !defer_numeric_literal {
			infer_generic_type_args(inference_param_type, arg_type, mut inferred)
			arg_type_id := arg_type
			// A type passed for a generic interface, a `UserShelf` for `Shelf[T]`:
			// its methods bind what the members of the interface name.
			t.tc.infer_type_params_from_interface(inference_param_type, arg_type_id, param_names, mut
				inferred)
			t.infer_generic_sum_variant_args(child.typ, arg_type_id, mut inferred)
			if is_recv_param {
				t.infer_generic_receiver_suffix_args(child.typ, arg_type_id, mut inferred)
				t.infer_generic_embedded_receiver_args(child.typ, arg_type_id, mut inferred)
			}
		}
		t.infer_generic_sum_literal_args(child.typ, arg_id, mut inferred)
		t.infer_generic_struct_init_args(child.typ, arg_id, mut inferred)
		t.infer_generic_short_struct_init_args(child.typ, arg_id, node, mut inferred)
		param_idx++
	}
	if defer_return_inference {
		t.infer_generic_return_type_args(decl, expected_return, mut inferred, receiver_params)
	}
	ret_source_id_2 := t.node_type(_id)
	if ret_source_id_2 != 0 {
		t.infer_generic_return_type_args(decl, ret_source_id_2, mut inferred, receiver_params)
		t.infer_missing_generic_receiver_args_from_return(decl, ret_source_id_2, receiver_params, mut inferred)
	}
	for name, typ_source_id in pinned {
		inferred[name] = typ_source_id
	}
	// A type parameter that only the constraint of another one names, `T` of
	// `[C Container[T], T Named]`, from the type bound to that one.
	if param_names.any(it !in inferred) {
		t.tc.infer_type_params_from_constraints(decl.node, mut inferred)
	}
	mut args := []u32{cap: param_names.len}
	for name in param_names {
		arg_source_id := inferred[name] or { return none }

		// Preserve selective imports while the original call file is still known.
		qualified_arg_source_id := t.qualify_specialized_signature_type_text(arg_source_id, GenericFnDecl{
			file: t.node_file_or(int(_id), t.cur_file)
		})

		args << t.generic_arg_for_call_and_decl_module(qualified_arg_source_id, call_module, decl.module)
	}
	return args.clone()
}

fn (mut t Transformer) infer_missing_generic_receiver_args_from_return(decl GenericFnDecl, ret_id u32, receiver_params []string, mut inferred map[string]u32) {
	if decl.node.typ == 0 || ret_id == u32(0) || receiver_params.len == 0 {
		return
	}
	mut ret_inferred := map[string]u32{}
	infer_generic_type_args(decl.node.typ, ret_id, mut ret_inferred)
	for name in receiver_params {
		if name in inferred {
			continue
		}
		arg_source_id := ret_inferred[name] or { continue }
		if arg_source_id != 0 && arg_source_id != flat.source_type_id(name)
			&& !t.generic_arg_is_unresolved(arg_source_id) {
			inferred[name] = arg_source_id
		}
	}
}

fn (mut t Transformer) infer_generic_return_type_args(decl GenericFnDecl, ret_id u32, mut inferred map[string]u32, skip []string) {
	if decl.node.typ == 0 || ret_id == u32(0) {
		return
	}
	mut ret_inferred := map[string]u32{}
	infer_generic_type_args(decl.node.typ, ret_id, mut ret_inferred)
	for name, arg_source_id in ret_inferred {
		if name in inferred || name in skip {
			continue
		}
		inferred[name] = arg_source_id
	}
}

fn (t &Transformer) declared_generic_receiver_param_names(node flat.Node, module_name string) []string {
	mut receiver_params := []string{}
	if t.fn_decl_receiver_is_open_generic(node, module_name) {
		receiver := owner_name_view(node.value_text())
		syntax := flat.source_type_syntax(flat.source_type_id(receiver))
		base := syntax.generic_app_base()
		args := syntax.generic_app_args()
		mut candidates := [base]
		if !flat.source_type_shape(base).has(.dotted) && module_name !in ['', 'main', 'builtin'] {
			candidates = [
				flat.source_type_composition_two(.qualified, flat.source_type_id(module_name), base),
				base,
			]
		}
		for candidate in candidates {
			params := t.tc.struct_generic_params[candidate] or { continue }
			param_ids := flat.TypeTextList.from_names(params).ids
			for arg in args {
				if arg in param_ids && flat.source_type_name(arg) !in receiver_params {
					receiver_params << flat.source_type_name(arg)
				}
			}
			return receiver_params
		}
	}
	return receiver_params
}

fn (mut t Transformer) generic_receiver_param_names(decl GenericFnDecl) []string {
	mut receiver_params := t.declared_generic_receiver_param_names(decl.node, decl.module)
	if decl.node.value.contains('.') {
		receiver := decl.node.value.all_before_last('.')
		t.collect_generic_param_names_from_type_id(flat.source_type_id(receiver), decl.module, mut receiver_params)
	}
	for i in 0 .. decl.node.children_count {
		child := t.a.child_node(&decl.node, i)
		if child.kind == .param {
			t.collect_generic_param_names_from_type_id(child.typ, decl.module, mut receiver_params)
			break
		}
	}
	return receiver_params
}

fn (mut t Transformer) infer_generic_struct_init_args(param_type_id u32, arg_id flat.NodeId, mut inferred map[string]u32) {
	if int(arg_id) < 0 || int(arg_id) >= t.a.nodes.len {
		return
	}
	arg := t.a.nodes[int(arg_id)]
	if arg.kind != .struct_init {
		return
	}
	param_syntax := flat.source_type_syntax(t.trimmed_type_id(param_type_id))
	if !param_syntax.generic_app_valid() || param_syntax.generic_app_args().len == 0 {
		return
	}
	info := t.lookup_struct_info(param_syntax.generic_app_base()) or { return }
	for i in 0 .. arg.children_count {
		field := t.a.child_node(&arg, i)
		if field.kind != .field_init || field.children_count == 0 {
			continue
		}
		field_name := if field.value_text().len > 0 {
			field.value_text()
		} else if i < info.fields.len {
			info.fields[i].name
		} else {
			continue
		}
		mut field_type := u32(0)
		for struct_field in info.fields {
			if struct_field.name == field_name {
				field_type = struct_field.typ
				break
			}
		}
		if field_type == 0 {
			continue
		}
		value_id := t.a.child(field, 0)
		value_type_source_id_2 := t.generic_call_arg_type_for_inference(value_id)
		if value_type_source_id_2 != 0 {
			infer_generic_type_args(field_type, value_type_source_id_2, mut inferred)
		}
	}
}

// infer_generic_short_struct_init_args handles the short struct-init call
// syntax for a generic struct param, e.g. `take_input(a: xs, b: 3)` for
// `fn take_input[T](p Params[T])`. In that form the named arguments are parsed
// as `field_init` children directly on the call node (not wrapped in a
// `struct_init`), so `infer_generic_struct_init_args` cannot see them; map each
// field to the generic struct's declared field type and infer `T` from it.
fn (mut t Transformer) infer_generic_short_struct_init_args(param_type_id u32, arg_id flat.NodeId, node flat.Node, mut inferred map[string]u32) {
	if int(arg_id) < 0 || int(arg_id) >= t.a.nodes.len {
		return
	}
	if t.a.nodes[int(arg_id)].kind != .field_init {
		return
	}
	mut struct_param_type := t.normalize_type_alias_chain(t.trimmed_type_id(param_type_id))
	if flat.source_type_shape(struct_param_type).has(.pointer) {
		if flat.source_type_shape(struct_param_type).has(.pointer_chain) {
			return
		}
		struct_param_type = flat.source_type_syntax(struct_param_type).raw_base()
	}
	param_syntax := flat.source_type_syntax(struct_param_type)
	param_args := param_syntax.generic_app_args()
	if !param_syntax.generic_app_valid() || param_args.len == 0 {
		return
	}
	info := t.lookup_struct_info(param_syntax.generic_app_base()) or { return }
	// The struct declares its fields in its own generic parameter names
	// (`struct Params[A] { a []A }`) while the call must infer the function's
	// parameters (`fn f[T](p Params[T])`). Rebase the field types onto the
	// applied args so `[]A` becomes `[]T` before inference.
	struct_params := t.struct_generic_params_for_base(param_syntax.generic_app_base())
	mut field_idx := 0
	for i in 0 .. int(node.children_count) {
		field := t.a.child_node(&node, i)
		if field.kind != .field_init {
			continue
		}
		field_name := if field.value_text().len > 0 {
			field.value_text()
		} else if field_idx < info.fields.len {
			info.fields[field_idx].name
		} else {
			field_idx++
			continue
		}
		field_idx++
		if field.children_count == 0 {
			continue
		}
		mut field_type := u32(0)
		for struct_field in info.fields {
			if struct_field.name == field_name {
				field_type = struct_field.typ
				break
			}
		}
		if field_type == 0 {
			continue
		}
		if struct_params.len > 0 && flat.TypeTextList.from_names(struct_params).ids != param_args {
			field_type = substitute_generic_type_text_with_params(field_type, param_args, struct_params)
		}
		value_id := t.a.child(field, 0)
		value_type_source_id_3 := t.generic_call_arg_type_for_inference(value_id)
		if value_type_source_id_3 != 0 {
			infer_generic_type_args(field_type, value_type_source_id_3, mut inferred)
		}
	}
}

fn (t &Transformer) struct_generic_params_for_base(base u32) []string {
	if isnil(t.tc) {
		return []string{}
	}
	if params := t.tc.struct_generic_params[base] {
		return params
	}
	short := flat.source_type_syntax(base).unqualified()
	if params := t.tc.struct_generic_params[short] {
		return params
	}
	return []string{}
}

fn (mut t Transformer) infer_generic_sum_variant_args(param_type_id u32, arg_type_id u32, mut inferred map[string]u32) {
	param := t.trimmed_type_id(param_type_id)
	arg := t.trimmed_type_id(arg_type_id)
	if param == 0 || arg == 0 {
		return
	}
	param_syntax := flat.source_type_syntax(param)
	param_base := param_syntax.generic_app_base()
	param_args := param_syntax.generic_app_args()
	if !param_syntax.generic_app_valid() || param_args.len == 0 {
		return
	}
	variants := t.concrete_sum_variants_for_candidate(param)
	if variants.len == 0 {
		return
	}
	mut matched_concrete_variant := false
	for variant in variants {
		if flat.source_type_query(t.trimmed_type_id(variant)).has(.generic_fn_name) {
			continue
		}
		before_len := inferred.len
		infer_generic_type_args(variant, arg, mut inferred)
		if inferred.len == before_len {
			t.infer_generic_receiver_suffix_args(variant, arg, mut inferred)
		}
		if inferred.len > before_len {
			return
		}
		if t.variant_names_match(variant, arg) {
			matched_concrete_variant = true
		}
	}
	arg_is_sum_base := arg == param_base
		|| flat.source_type_syntax(arg).unqualified() == flat.source_type_syntax(param_base).unqualified()
		|| t.resolve_sum_name(arg) == t.resolve_sum_name(param_base)
	for variant in variants {
		if !flat.source_type_query(t.trimmed_type_id(variant)).has(.generic_fn_name) || arg_is_sum_base {
			continue
		}
		before_len := inferred.len
		infer_generic_type_args(variant, arg, mut inferred)
		if inferred.len == before_len {
			t.infer_generic_receiver_suffix_args(variant, arg, mut inferred)
		}
		if inferred.len > before_len {
			return
		}
	}
	if !matched_concrete_variant {
		return
	}
	for param_arg in param_args {
		if flat.source_type_query(param_arg).has(.generic_fn_name) {
			param_name := flat.source_type_name(param_arg)
			if param_name !in inferred { inferred[param_name] = arg }
		}
	}
}

fn (mut t Transformer) infer_generic_sum_literal_args(param_type_id u32, arg_id flat.NodeId, mut inferred map[string]u32) {
	param := t.trimmed_type_id(param_type_id)
	if int(arg_id) < 0 || int(arg_id) >= t.a.nodes.len {
		return
	}
	arg := t.a.nodes[int(arg_id)]
	if arg.kind != .struct_init {
		return
	}
	param_syntax := flat.source_type_syntax(param)
	if !param_syntax.generic_app_valid() || param_syntax.generic_app_args().len == 0 {
		return
	}
	variants := t.concrete_sum_variants_for_candidate(param)
	if variants.len == 0 {
		return
	}
	for i in 0 .. arg.children_count {
		field := t.a.child_node(&arg, i)
		if field.kind != .field_init || field.children_count == 0 {
			continue
		}
		value_id := t.a.child(field, 0)
		value_type_source_id_4 := t.generic_call_arg_type_for_inference(value_id)

		if value_type_source_id_4 == 0 {
			continue
		}
		for variant in variants {
			if t.sum_field_name(variant) == field.value_text() {
				t.infer_generic_sum_variant_args(param, value_type_source_id_4, mut inferred)
				return
			}
		}
	}
}

fn (t &Transformer) generic_arg_for_call_and_decl_module(arg_id u32, call_module string, decl_module string) u32 {
	arg_shape := flat.source_type_shape(arg_id)
	if call_module in ['', 'main'] && !arg_shape.has(.dotted) {
		if info := t.structs[t.trimmed_type_id(arg_id)] {
			if info.module in ['', 'main'] {
				return t.lock_colliding_main_generic_type_text(arg_id, decl_module)
			}
		}
	}
	if t.substituted_type_belongs_to_main_generic(arg_id)
		&& (call_module in ['', 'main'] || t.current_specialization_has_generic_arg(arg_id)) {
		return arg_id
	}
	if t.current_specialization_has_generic_arg(arg_id) {
		base, args, is_generic := generic_provenance_app_parts(arg_id)
		if is_generic && !arg_shape.has(.map_type) {
			mut parts := []u32{len: args.len + 1}
			for i, nested_arg in args {
				parts[i + 1] = t.generic_arg_for_call_and_decl_module(nested_arg, call_module, decl_module)
			}
			parts[0] = t.generic_arg_for_call_and_decl_module(base, call_module, decl_module)
			return flat.source_type_composition(.generic, parts)
		}
	}
	// A bare program (main) type alias that collides by short name with a type in the
	// callee module (e.g. a user `RawHtml` alias against `veb.RawHtml`) must be kept as the
	// caller's own type: rebasing it into the callee module would merge the two into one
	// specialization, so `veb.filter_html` could no longer tell user data from trusted
	// `veb.RawHtml` and would skip escaping it. Only same-named collisions are preserved.
	if call_module in ['', 'main'] && !arg_shape.has(.dotted) && !isnil(t.tc) {
		if t.tc.type_aliases.contains(arg_id) || arg_id in t.tc.structs || t.tc.sum_types.contains(arg_id)
			|| arg_id in t.tc.enum_names || arg_id in t.tc.interface_names {
			qname := flat.source_type_composition_two(.qualified, flat.source_type_id(decl_module), arg_id)
			if t.tc.type_aliases.contains(qname) || qname in t.tc.structs || t.tc.sum_types.contains(qname)
				|| qname in t.tc.enum_names || qname in t.tc.interface_names {
				return arg_id
			}
		}
	}
	if t.generic_type_text_contains_alias(arg_id, call_module) {
		if t.generic_type_text_contains_fixed_array_alias(arg_id, call_module) {
			call_normalized_source_id := t.normalize_type_in_module(arg_id, call_module)
			if call_normalized_source_id != arg_id {
				return t.generic_arg_for_decl_module(call_normalized_source_id, decl_module)
			}
		}
		call_qualified_source_id := t.qualify_generic_arg_for_decl_module(arg_id, call_module)

		return t.generic_arg_for_decl_module(call_qualified_source_id, decl_module)
	}
	call_normalized_source_id_2 := t.normalize_type_in_module(arg_id, call_module)

	return t.generic_arg_for_decl_module(call_normalized_source_id_2, decl_module)
}

fn (t &Transformer) current_specialization_has_generic_arg(arg_id u32) bool {
	clean_id := t.trimmed_type_id(arg_id)
	if t.active_specialization_main_types[clean_id] {
		return true
	}
	for current_arg_id in t.active_specialization_args.ids {
		if t.trimmed_type_id(current_arg_id) == clean_id {
			return true
		}
	}
	// Nested inference can retain containers around a concrete caller type.
	for component in generic_type_container_components(clean_id) {
		if t.current_specialization_has_generic_arg(component) {
			return true
		}
	}
	base, nested_args, is_generic_app := generic_provenance_app_parts(clean_id)
	if is_generic_app {
		if t.current_specialization_has_generic_arg(base) {
			return true
		}
		for nested_arg in nested_args {
			if t.current_specialization_has_generic_arg(nested_arg) {
				return true
			}
		}
	}
	receiver_source_id := t.current_fn_receiver_type()
	if receiver_source_id == 0 {
		return false
	}
	receiver_syntax := flat.source_type_syntax(receiver_source_id)
	if receiver_syntax.generic_app_valid() {
		for current_arg in receiver_syntax.generic_app_args() {
			if t.trimmed_type_id(current_arg) == clean_id {
				return true
			}
		}
	} else {
		args := t.recorded_type_specialization_args(receiver_source_id) or { return false }
		for current_arg in args {
			if t.trimmed_type_id(current_arg) == clean_id {
				return true
			}
		}
	}
	return false
}

fn generic_type_container_components(type_id u32) []u32 {
	clean := transform_trimmed_type_id(type_id)
	shape := flat.source_type_shape(clean)
	syntax := flat.source_type_syntax(clean)
	if _ := generic_type_wrapper_composition(shape) {
		return [transform_trimmed_type_id(syntax.raw_base())]
	}
	if shape.has(.map_type) {
		if syntax.bracket_closed() && syntax.bracket_suffix() != 0 {
			return [syntax.bracket_inner(), syntax.bracket_base()]
		}
	} else if shape.has(.leading_bracket) {
		if syntax.bracket_closed() && syntax.bracket_suffix() != 0 {
			return [syntax.bracket_base()]
		}
	} else if syntax.tuple_like() && syntax.tuple_contains_comma() {
		return syntax.tuple_components()
	} else if shape.has(.function_compact) || shape.has(.function_spaced) {
		if syntax.function_closed() {
			mut components := syntax.function_params().clone()
			components << syntax.function_result()
			return components
		}
	}
	return []u32{}
}

fn generic_provenance_app_parts(type_id u32) (u32, []u32, bool) {
	// A bracket inside an option, map or function belongs to that container;
	// it does not make the entire spelling a named generic application.
	if generic_type_container_components(type_id).len > 0 {
		return 0, []u32{}, false
	}
	syntax := flat.source_type_syntax(type_id)
	return syntax.generic_app_base(), syntax.generic_app_args(), syntax.generic_app_valid()
}

// SpecializationMainTypeClosure retains caller provenance for an ordered source-ID sequence.
@[heap]
struct SpecializationMainTypeClosure {
	args           []u32
	types_in_scope map[u32]bool
}

fn (mut t Transformer) specialization_main_type_closure(args_ids []u32) map[u32]bool {
	// Hash the ordered identity sequence; compare it in full on hash collisions.
	mut key := u64(14695981039346656037)
	for arg_id in args_ids { key = (key ^ u64(arg_id)) * u64(1099511628211) }
	if cached := t.specialization_main_type_closures[key] {
		for entry in cached {
			if entry.args == args_ids { return entry.types_in_scope }
		}
	}
	mut types_in_scope := map[u32]bool{}
	mut seen := map[u32]bool{}
	for arg_id in args_ids {
		t.collect_specialization_main_types(arg_id, mut types_in_scope, mut seen)
	}
	mut entries := t.specialization_main_type_closures[key] or { []&SpecializationMainTypeClosure{} }
	entries << &SpecializationMainTypeClosure{ args: args_ids.clone(), types_in_scope: types_in_scope.clone() }
	t.specialization_main_type_closures[key] = entries
	return types_in_scope
}

// collect_caller_main_type_names preserves the provenance of bare concrete types
// while a specialization is still in the program module. Once signature
// registration enters the imported declaration module, the same spelling may
// resolve to an unrelated homonym.
fn (t &Transformer) collect_caller_main_type_names(typ_id u32, mut types_in_scope map[u32]bool) {
	mut clean := t.trimmed_type_id(typ_id)
	if clean == 0 {
		return
	}
	if flat.source_type_query(clean).has(.main_prefix) && !t.ident_is_import_alias('main') {
		clean = flat.source_type_syntax(clean).main_base()
	}
	shape := flat.source_type_shape(clean)
	syntax := flat.source_type_syntax(clean)
	if _ := generic_type_wrapper_composition(shape) {
		t.collect_caller_main_type_names(syntax.raw_base(), mut types_in_scope)
		return
	}
	if shape.has(.function_compact) || shape.has(.function_spaced) {
		if syntax.function_closed() {
			for param in syntax.function_params() {
				t.collect_caller_main_type_names(param, mut types_in_scope)
			}
			t.collect_caller_main_type_names(syntax.function_result(), mut types_in_scope)
		}
		return
	}
	if shape.has(.map_type) {
		if syntax.bracket_closed() && syntax.bracket_suffix() != 0 {
			t.collect_caller_main_type_names(syntax.bracket_raw_inner(), mut types_in_scope)
			t.collect_caller_main_type_names(syntax.bracket_suffix(), mut types_in_scope)
		}
		return
	}
	if shape.has(.leading_bracket) {
		if syntax.bracket_closed() && syntax.bracket_suffix() != 0 {
			t.collect_caller_main_type_names(syntax.bracket_suffix(), mut types_in_scope)
		}
		return
	}
	if syntax.tuple_like() && syntax.tuple_contains_comma() {
		for part in syntax.tuple_components() {
			t.collect_caller_main_type_names(part, mut types_in_scope)
		}
		return
	}
	if syntax.generic_app_valid() {
		for generic_arg in syntax.generic_app_args() {
			t.collect_caller_main_type_names(generic_arg, mut types_in_scope)
		}
		clean = syntax.generic_app_base()
	}
	if !flat.source_type_shape(clean).has(.dotted) && t.type_name_is_declared_id(clean) {
		types_in_scope[clean] = true
	}
}

fn (t &Transformer) collect_specialization_main_types(typ_id u32, mut types_in_scope map[u32]bool, mut seen map[u32]bool) {
	mut clean := t.trimmed_type_id(typ_id)
	if clean == 0 {
		return
	}
	if seen[clean] {
		return
	}
	seen[clean] = true
	// `main.` is a temporary disambiguation lock on a caller-owned type. Strip it
	// while building the provenance closure so nested substitutions can recognize
	// the same type by its normal bare spelling in an imported generic body.
	if flat.source_type_query(clean).has(.main_prefix) && !t.ident_is_import_alias('main') {
		clean = flat.source_type_syntax(clean).main_base()
	}
	components := generic_type_container_components(clean)
	if components.len > 0 {
		for component in components {
			t.collect_specialization_main_types(component, mut types_in_scope, mut seen)
		}
		return
	}
	syntax := flat.source_type_syntax(clean)
	is_generic := syntax.generic_app_valid()
	generic_args := syntax.generic_app_args()
	if is_generic {
		for generic_arg in generic_args {
			t.collect_specialization_main_types(generic_arg, mut types_in_scope, mut seen)
		}
	}
	struct_name_id := if is_generic { syntax.generic_app_base() } else { clean }
	if flat.source_type_shape(struct_name_id).has(.dotted) {
		return
	}
	struct_name := flat.source_type_name(struct_name_id)
	if !isnil(t.tc) {
		if alias_target_source_id := t.tc.type_aliases.type_id(struct_name_id) {
			types_in_scope[struct_name_id] = true
			t.collect_specialization_main_types(alias_target_source_id, mut types_in_scope, mut seen)
			return
		}
	}
	if variants_source_ids := t.sum_types.type_ids(struct_name_id) {
		types_in_scope[struct_name_id] = true
		for variant_source_id in variants_source_ids {
			t.collect_specialization_main_types(variant_source_id, mut types_in_scope, mut seen)
		}
		return
	}
	if struct_name_id in t.enum_types {
		types_in_scope[struct_name_id] = true
		return
	}
	info := t.structs[struct_name_id] or { return }
	if info.module !in ['', 'main'] {
		return
	}
	types_in_scope[struct_name_id] = true
	params := if is_generic {
		t.generic_struct_param_names_for_base(struct_name)
	} else {
		[]string{}
	}
	for field in info.fields {
		field_type := (if params.len == generic_args.len && params.len > 0 {
			substitute_generic_type_text_with_params(field.typ, generic_args, params)
		} else {
			field.typ
		})
		t.collect_specialization_main_types(field_type, mut types_in_scope, mut seen)
	}
}

fn (t &Transformer) generic_arg_for_decl_module(arg_id u32, module_name string) u32 {
	if t.substituted_type_belongs_to_main_generic(arg_id)
		&& t.current_specialization_has_generic_arg(arg_id) {
		return arg_id
	}
	if t.current_specialization_has_generic_arg(arg_id) {
		base, args, is_generic := generic_provenance_app_parts(arg_id)
		if is_generic && !flat.source_type_shape(arg_id).has(.map_type) {
			mut parts := []u32{len: args.len + 1}
			for i, nested_arg in args {
				parts[i + 1] = t.generic_arg_for_decl_module(nested_arg, module_name)
			}
			parts[0] = t.generic_arg_for_decl_module(base, module_name)
			return flat.source_type_composition(.generic, parts)
		}
	}
	if t.generic_type_text_contains_alias(arg_id, module_name) {
		return t.qualify_generic_arg_for_decl_module(arg_id, module_name)
	}
	normalized_source_id := t.normalize_type_in_module(arg_id, module_name)
	normalized_comparison_type_id_2 := normalized_source_id

	qualified_source_id := t.qualify_generic_arg_for_decl_module(normalized_source_id, module_name)
	qualified_comparison_type_id_2 := qualified_source_id

	if qualified_comparison_type_id_2 != normalized_comparison_type_id_2 {
		return qualified_source_id
	}
	return normalized_source_id
}

fn (t &Transformer) inherited_generic_arg_for_decl_module(arg_id u32, module_name string, outer_args_ids []u32) u32 {
	clean := t.trimmed_type_id(arg_id)
	normalized := t.normalize_type_in_module(clean, module_name)
	for outer_arg in outer_args_ids {
		outer_clean := t.trimmed_type_id(outer_arg)
		if outer_clean == clean || t.normalize_type_in_module(outer_clean, module_name) == normalized {
			return outer_arg
		}
	}
	return t.generic_arg_for_decl_module(arg_id, module_name)
}

fn (mut t Transformer) invalidate_generic_type_name_indexes() {
	t.generic_type_indexes_ready = false
	t.generic_type_name_index_cache = &GenericTypeNameIndexCache{}
}

fn (mut t Transformer) build_generic_alias_name_index() {
	t.invalidate_generic_type_name_indexes()
	mut names := map[u32]bool{}
	mut exact := map[u32]bool{}
	mut concrete := map[u32]bool{}
	mut authority := map[u32]bool{}
	mut transform_concrete := map[u32]bool{}
	mut main_structs := map[u32]bool{}
	mut enums := map[u32]bool{}
	mut checker_structs := map[u32]bool{}
	mut checker_struct_short := map[u32]bool{}
	mut checker_sums := map[u32]bool{}
	mut checker_interfaces := map[u32]bool{}
	mut generic_names := map[u32]bool{}
	if !isnil(t.tc) {
		for alias_source_id, _ in t.tc.type_aliases.ids {
			alias_id := alias_source_id
			names[alias_id] = true
			names[flat.source_type_syntax(alias_id).unqualified()] = true
			exact[alias_id] = true
			authority[alias_id] = true
		}
		for name_source_id, _ in t.tc.structs {
			name_id := name_source_id
			concrete[name_id] = true
			checker_structs[name_id] = true
			checker_struct_short[flat.source_type_syntax(name_id).unqualified()] = true
			base_id := comptime_generic_type_base(name_id)
			if base_id != 0 {
				checker_struct_short[flat.source_type_syntax(base_id).unqualified()] = true
			}
		}
		for name_source_id, _ in t.tc.sum_types.ids {
			name_id := name_source_id
			concrete[name_id] = true
			checker_sums[name_id] = true
		}
		for name_source_id, _ in t.tc.enum_names {
			name_id := name_source_id
			concrete[name_id] = true
			enums[name_id] = true
		}
		for name_source_id, _ in t.tc.interface_names {
			name_id := name_source_id
			concrete[name_id] = true
			checker_interfaces[name_id] = true
		}
		for name_id, _ in concrete { authority[name_id] = true }
		for name_source_id, _ in t.tc.struct_generic_params {
			generic_names[name_source_id] = true
		}
		for name_source_id, _ in t.tc.sum_generic_params { generic_names[name_source_id] = true }
		for name_source_id, _ in t.tc.type_alias_generic_params {
			generic_names[name_source_id] = true
		}
	}
	for name_id, info in t.structs {
		authority[name_id] = true
		transform_concrete[name_id] = true
		if info.module in ['', 'main'] { main_structs[name_id] = true }
	}
	for name_id, _ in t.sum_types.ids {
		authority[name_id] = true
		transform_concrete[name_id] = true
	}
	for name_id, _ in t.enum_types {
		authority[name_id] = true
		transform_concrete[name_id] = true
		enums[name_id] = true
	}
	t.generic_alias_names = names.move()
	t.generic_exact_alias_names = exact.move()
	t.generic_concrete_type_names = concrete.move()
	t.generic_type_authority_names = authority.move()
	t.generic_transform_concrete_type_names = transform_concrete.move()
	t.generic_transform_concrete_index_count = t.structs.len + t.sum_types.ids.len + t.enum_types.len
	t.generic_main_struct_names = main_structs.move()
	t.generic_enum_type_names = enums.move()
	t.generic_checker_struct_names = checker_structs.move()
	t.generic_checker_struct_short_names = checker_struct_short.move()
	t.generic_checker_struct_index_count = if isnil(t.tc) { 0 } else { t.tc.structs.len }
	t.generic_checker_sum_names = checker_sums.move()
	t.generic_checker_sum_index_count = if isnil(t.tc) { 0 } else { t.tc.sum_types.ids.len }
	t.generic_checker_interface_names = checker_interfaces.move()
	t.generic_parameter_type_names = generic_names.move()
	t.generic_parameter_type_index_count = t.generic_parameter_type_name_count()
	t.generic_checker_interface_index_count = if isnil(t.tc) { 0 } else { t.tc.interface_names.len }
	t.generic_enum_index_count = t.enum_types.len + if isnil(t.tc) {
		0
	} else {
		t.tc.enum_names.len
	}
	t.generic_alias_index_concrete_count = t.generic_checker_concrete_name_count()
	t.generic_alias_index_authority_count = t.generic_type_authority_name_count()
	t.generic_type_indexes_ready = true
}

fn (t &Transformer) generic_checker_concrete_name_count() int {
	if isnil(t.tc) { return 0 }
	return t.tc.structs.len + t.tc.sum_types.ids.len + t.tc.enum_names.len + t.tc.interface_names.len
}

fn (t &Transformer) generic_type_authority_name_count() int {
	return t.structs.len + t.sum_types.ids.len + t.enum_types.len +
		t.generic_checker_concrete_name_count() +
		if isnil(t.tc) { 0 } else { t.tc.type_aliases.ids.len }
}

fn (t &Transformer) generic_enum_type_name_known(name_id u32) bool {
	count := t.enum_types.len + if isnil(t.tc) { 0 } else { t.tc.enum_names.len }
	if t.generic_type_indexes_ready && t.generic_enum_index_count == count {
		return t.generic_enum_type_names[name_id]
	}
	// Late registrations make the prepared index stale; the registries are
	// keyed by interned ids, so look the id up directly.
	return name_id in t.enum_types || (!isnil(t.tc) && name_id in t.tc.enum_names)
}

fn (t &Transformer) refresh_checker_struct_type_name_index() &GenericTypeNameIndexCache {
	mut cache := t.generic_type_name_index_cache
	count := if isnil(t.tc) { 0 } else { t.tc.structs.len }
	if cache.struct_count != count {
		mut names := map[u32]bool{}
		mut short_names := map[u32]bool{}
		if !isnil(t.tc) {
			for name_source_id, _ in t.tc.structs {
				name_id := name_source_id
				names[name_id] = true
				short_names[flat.source_type_syntax(name_id).unqualified()] = true
				base_id := comptime_generic_type_base(name_id)
				if base_id != 0 {
					short_names[flat.source_type_syntax(base_id).unqualified()] = true
				}
			}
		}
		cache.struct_names = names.move()
		cache.struct_short_names = short_names.move()
		cache.struct_count = count
	}
	return cache
}

fn (t &Transformer) generic_checker_sum_name_known(name_id u32) bool {
	if isnil(t.tc) { return false }
	count := t.tc.sum_types.ids.len
	if t.generic_type_indexes_ready && t.generic_checker_sum_index_count == count {
		return t.generic_checker_sum_names[name_id]
	}
	return name_id in t.tc.sum_types.ids
}

fn (t &Transformer) generic_checker_interface_name_known(name_id u32) bool {
	if isnil(t.tc) { return false }
	count := t.tc.interface_names.len
	if t.generic_type_indexes_ready && t.generic_checker_interface_index_count == count {
		return t.generic_checker_interface_names[name_id]
	}
	return name_id in t.tc.interface_names
}

fn (t &Transformer) generic_exact_alias_name_known(name_id u32) bool {
	if isnil(t.tc) { return false }
	count := t.tc.type_aliases.ids.len
	if t.generic_type_indexes_ready && t.generic_exact_alias_names.len == count {
		return t.generic_exact_alias_names[name_id]
	}
	return name_id in t.tc.type_aliases.ids
}

// GenericTypeNameIndexCache belongs to one transformer or worker. Late struct
// declarations invalidate the prepared indexes; rebuild the short-name view
// once per registry size change. Exact-name checks look the interned spelling
// up in the registry directly instead.
struct GenericTypeNameIndexCache {
mut:
	struct_count       int = -1
	struct_names       map[u32]bool
	struct_short_names map[u32]bool
}

fn (t &Transformer) generic_transform_concrete_name_known(name_id u32) bool {
	if t.generic_type_indexes_ready
		&& t.generic_transform_concrete_index_count == t.structs.len + t.sum_types.ids.len + t.enum_types.len {
		return t.generic_transform_concrete_type_names[name_id]
	}
	return name_id in t.structs || name_id in t.sum_types.ids || name_id in t.enum_types
}

fn (t &Transformer) generic_resolved_concrete_name_known(name_id u32) bool {
	return t.generic_concrete_name_known(name_id) || t.generic_transform_concrete_name_known(name_id)
}

fn (t &Transformer) generic_concrete_name_known(name_id u32) bool {
	if isnil(t.tc) { return false }
	if t.generic_type_indexes_ready
		&& t.generic_alias_index_concrete_count == t.generic_checker_concrete_name_count() {
		return t.generic_concrete_type_names[name_id]
	}
	return name_id in t.tc.structs || name_id in t.tc.sum_types.ids || name_id in t.tc.enum_names
		|| name_id in t.tc.interface_names
}

fn (t &Transformer) generic_arg_is_alias_name(arg string, module_name string) bool {
	return t.generic_arg_is_alias_name_id(flat.source_type_id(arg), module_name)
}

fn (t &Transformer) generic_arg_is_alias_name_id(arg_id u32, module_name string) bool {
	clean_id := t.trimmed_type_id(arg_id)
	if clean_id == 0 || isnil(t.tc) { return false }
	if t.generic_exact_alias_name_known(clean_id) { return true }
	if flat.source_type_query(clean_id).has(.main_prefix)
		&& t.generic_exact_alias_name_known(flat.source_type_syntax(clean_id).main_base()) {
		return true
	}
	if !flat.source_type_shape(clean_id).has(.dotted) {
		scoped_id := if module_name !in ['', 'main', 'builtin'] {
			flat.source_type_composition_two(.qualified, flat.source_type_id(module_name), clean_id)
		} else {
			clean_id
		}
		if t.generic_exact_alias_name_known(scoped_id) { return true }
		// A concrete type declared in the call's module is authoritative over an
		// imported alias with the same short name.
		if t.generic_concrete_name_known(scoped_id) { return false }
	}
	return t.generic_alias_names[clean_id]
}

fn (t &Transformer) generic_args_contain_alias(args_ids []u32, module_name string) bool {
	for arg_source_id in args_ids {
		if t.generic_type_text_contains_alias(arg_source_id, module_name) {
			return true
		}
	}
	return false
}

fn (t &Transformer) generic_type_text_contains_alias(typ_id u32, module_name string) bool {
	clean_id := t.trimmed_type_id(typ_id)
	if clean_id == 0 { return false }
	if t.generic_arg_is_alias_name_id(clean_id, module_name) { return true }
	syntax := flat.source_type_syntax(clean_id)
	shape := flat.source_type_shape(clean_id)
	if shape.has(.pointer) || shape.has(.mutable_type) || shape.has(.optional)
		|| shape.has(.variadic) || shape.has(.array_type) {
		return t.generic_type_text_contains_alias(syntax.raw_base(), module_name)
	}
	if shape.has(.map_type) && syntax.bracket_closed() {
		return t.generic_type_text_contains_alias(syntax.bracket_inner(), module_name)
			|| t.generic_type_text_contains_alias(syntax.bracket_base(), module_name)
	}
	if shape.has(.leading_bracket) && syntax.bracket_closed() {
		return t.generic_type_text_contains_alias(syntax.bracket_base(), module_name)
	}
	if syntax.generic_app_valid() {
		if t.generic_arg_is_alias_name_id(syntax.generic_app_base(), module_name) { return true }
		for arg_id in syntax.generic_app_args() {
			if t.generic_type_text_contains_alias(arg_id, module_name) { return true }
		}
	}
	return false
}

fn (t &Transformer) generic_type_text_contains_fixed_array_alias(typ_id u32, module_name string) bool {
	clean_id := t.trimmed_type_id(typ_id)
	if clean_id == 0 { return false }
	if t.generic_arg_is_alias_name_id(clean_id, module_name) {
		return t.is_fixed_array_type(t.normalize_type_in_module(clean_id, module_name))
	}
	syntax := flat.source_type_syntax(clean_id)
	shape := flat.source_type_shape(clean_id)
	if shape.has(.pointer) || shape.has(.mutable_type) || shape.has(.optional)
		|| shape.has(.variadic) || shape.has(.array_type) {
		return t.generic_type_text_contains_fixed_array_alias(syntax.raw_base(), module_name)
	}
	if shape.has(.map_type) && syntax.bracket_closed() {
		return t.generic_type_text_contains_fixed_array_alias(syntax.bracket_inner(), module_name)
			|| t.generic_type_text_contains_fixed_array_alias(syntax.bracket_base(), module_name)
	}
	if shape.has(.leading_bracket) && syntax.bracket_closed() {
		return t.generic_type_text_contains_fixed_array_alias(syntax.bracket_base(), module_name)
	}
	if syntax.generic_app_valid() {
		for arg_id in syntax.generic_app_args() {
			if t.generic_type_text_contains_fixed_array_alias(arg_id, module_name) { return true }
		}
	}
	return false
}

fn (t &Transformer) qualify_generic_arg_for_decl_module(arg_id u32, module_name string) u32 {
	clean_id := t.trimmed_type_id(arg_id)
	if clean_id == 0 { return 0 }
	for current_arg in t.active_specialization_args.ids {
		current_clean_id := t.trimmed_type_id(current_arg)
		// c_name renders an emitted specialization name, rather than type syntax.
		if current_clean_id != 0 && current_clean_id != clean_id
			&& flat.source_type_id(c_name(flat.source_type_name(current_clean_id))) == clean_id {
			return current_clean_id
		}
	}
	shape := flat.source_type_shape(clean_id)
	if shape.has(.pointer) || shape.has(.mutable_type) || shape.has(.optional)
		|| shape.has(.variadic) || shape.has(.array_type) {
		kind := if shape.has(.pointer) {
			flat.SourceTypeComposition.pointer
		} else if shape.has(.mutable_type) {
			flat.SourceTypeComposition.mutable_parameter
		} else if shape.has(.option_type) {
			flat.SourceTypeComposition.option
		} else if shape.has(.result_type) {
			flat.SourceTypeComposition.result
		} else if shape.has(.variadic) {
			flat.SourceTypeComposition.variadic_parameter
		} else {
			flat.SourceTypeComposition.array
		}
		return flat.source_type_composition_one(kind,
			t.qualify_generic_arg_for_decl_module(flat.source_type_syntax(clean_id).raw_base(), module_name))
	}
	if shape.has(.map_type) {
		syntax := flat.source_type_syntax(clean_id)
		if syntax.bracket_closed() {
			return flat.source_type_composition_two(.map,
				t.qualify_generic_arg_for_decl_module(syntax.bracket_raw_inner(), module_name),
				t.qualify_generic_arg_for_decl_module(syntax.raw_base(), module_name))
		}
	}
	if shape.has(.leading_bracket) {
		syntax := flat.source_type_syntax(clean_id)
		if syntax.bracket_closed() {
			return flat.source_type_composition_two(.fixed_array, syntax.bracket_raw_inner(),
				t.qualify_generic_arg_for_decl_module(syntax.raw_base(), module_name))
		}
	}
	if shape.has(.dotted) || types.is_builtin_type_name_id(clean_id) { return clean_id }
	if module_name !in ['', 'main', 'builtin'] {
		qualified_id := flat.source_type_composition_two(.qualified, flat.source_type_id(module_name), clean_id)
		if t.type_name_is_declared_id(qualified_id) { return qualified_id }
	}
	// Main declarations keep their bare spelling. A suffix-indexed imported
	// homonym must not replace a concrete argument of a main generic field.
	if module_name == t.cur_module && (t.bare_struct_name_is_local_to_current_module(clean_id)
		|| t.bare_sum_type_name_is_local_to_current_module(clean_id)
		|| (!isnil(t.tc) && module_name in ['', 'main', 'builtin']
			&& t.generic_main_arg_checker_declaration_known(clean_id))) {
		return clean_id
	}
	// Resolve a bare declaration spelling through the numeric qualified-name table.
	if qualified_source_id := t.qualified_types.type_id(clean_id) {
		return qualified_source_id
	}
	return clean_id
}

fn (t &Transformer) generic_main_arg_checker_declaration_known(name_id u32) bool {
	if t.generic_exact_alias_name_known(name_id) { return true }
	if isnil(t.tc) { return false }
	return name_id in t.tc.enum_names || t.generic_checker_interface_name_known(name_id)
}

fn (t &Transformer) generic_arg_module_owns_type(name string, module_name string) bool {
	if module_name == '' || module_name == 'main' || module_name == 'builtin' {
		return false
	}
	qname := flat.source_type_existing_id('${module_name}.${name}') or { return false }
	if qname in t.structs || t.sum_types.contains(qname) || qname in t.enum_types {
		return true
	}
	if !isnil(t.tc) {
		return t.tc.type_aliases.contains(qname) || qname in t.tc.structs || t.tc.sum_types.contains(qname)
			|| qname in t.tc.enum_names || qname in t.tc.interface_names
	}
	return false
}

fn (t &Transformer) generic_arg_expr_type(id flat.NodeId) u32 {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return u32(0)
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			if typ := t.pointer_value_expr_type(id) {
				return typ
			}
			typ_source_id := t.var_type(node.value_text())
			typ_source_id_3 := typ_source_id

			if typ_source_id != 0 {
				raw_typ_source_id := t.raw_var_type(node.value_text())
				raw_typ_source_id_2 := raw_typ_source_id

				return if raw_typ_source_id != 0 { raw_typ_source_id_2 } else { typ_source_id_3 }
			}
			if decl_type_source_id := t.local_decl_type_before(node.value_text(), id) {
				return decl_type_source_id
			}
		}
		.array_literal {
			module_name := t.node_module_or(int(id), t.cur_module)
			if alias_type_source_id := t.generic_array_literal_alias_type_for_inference(node, module_name) {
				return alias_type_source_id
			}
			if checker_alias_type_source_id := t.array_literal_checker_alias_type(id) {
				return checker_alias_type_source_id
			}
			if alias_type_source_id := t.array_literal_alias_type(node) {
				return alias_type_source_id
			}
			if node.children_count > 0 {
				child_id := t.a.child(&node, 0)
				mut elem_type := t.generic_arg_expr_type(child_id)
				if elem_type == 0 {
					elem_type = t.node_type(child_id)
				}
				if elem_type != 0 {
					return flat.source_type_composition_one(.array, elem_type)
				}
			}
			if node.typ != 0 {
				return t.normalize_type_alias(node.typ)
			}
		}
		.fn_literal, .lambda_expr {
			if fn_type_source_id := t.fn_value_type_name(id) {
				return fn_type_source_id
			}
		}
		.cast_expr, .as_expr {
			if node.value_text().len > 0 {
				return node.source_value_id()
			}
		}
		.infix {
			if node.children_count >= 2 {
				left_type_source_id := t.node_type(t.a.child(&node, 0))

				if left_type_source_id != 0 {
					return left_type_source_id
				}
				right_type_source_id := t.node_type(t.a.child(&node, 1))

				if right_type_source_id != 0 {
					return right_type_source_id
				}
			}
		}
		.prefix, .paren {
			if node.children_count > 0 {
				return t.node_type(t.a.child(&node, 0))
			}
		}
		.index {
			typ_source_id_2 := t.node_type(id)

			if typ_source_id_2 != 0 && !t.generic_arg_is_unresolved(typ_source_id_2) {
				return typ_source_id_2
			}
		}
		else {}
	}

	return t.node_type(id)
}

fn (t &Transformer) generic_array_literal_alias_type_for_inference(node flat.Node, module_name string) ?u32 {
	if node.kind != .array_literal || node.children_count == 0 {
		return none
	}
	alias_name := t.generic_array_literal_alias_expr_name(t.a.child(&node, 0)) or { return none }
	if !t.generic_arg_is_alias_name_id(alias_name, module_name) {
		return none
	}
	return flat.source_type_composition_one(.array, t.generic_qualified_alias_name(alias_name, module_name))
}

fn (t &Transformer) generic_array_literal_alias_expr_name(id flat.NodeId) ?u32 {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return t.generic_array_literal_alias_expr_name(t.a.child(&node, 0))
	}
	raw_alias_source_id := t.raw_alias_type_for_expr(id)

	if raw_alias_source_id != 0 {
		return raw_alias_source_id
	}
	if node.kind in [.cast_expr, .as_expr] && node.value_text().len > 0 {
		return node.source_value_id()
	}
	if node.kind == .call && node.children_count > 0 {
		name_id := t.generic_call_type_arg_name(t.a.child(&node, 0))
		if name_id != 0 {
			return name_id
		}
	}
	for candidate in alias_name_candidates(node) {
		if candidate != 0 {
			return candidate
		}
	}
	return none
}

fn (t &Transformer) generic_qualified_alias_name(name_id u32, module_name string) u32 {
	clean_id := t.trimmed_type_id(name_id)
	if clean_id == 0 || isnil(t.tc) { return clean_id }
	if t.generic_exact_alias_name_known(clean_id) { return clean_id }
	if !flat.source_type_shape(clean_id).has(.dotted) && module_name !in ['', 'main', 'builtin'] {
		qname := flat.source_type_composition_two(.qualified, flat.source_type_id(module_name), clean_id)
		if t.generic_exact_alias_name_known(qname) { return qname }
	}
	if flat.source_type_shape(clean_id).has(.dotted) { return clean_id }
	mut found := u32(0)
	for alias_source_id, _ in t.tc.type_aliases.ids {
		alias_id := alias_source_id
		if flat.source_type_syntax(alias_id).unqualified() != clean_id { continue }
		if found != 0 && found != alias_id { return clean_id }
		found = alias_id
	}
	return if found != 0 { found } else { clean_id }
}

fn (mut t Transformer) generic_call_arg_type_for_inference(id flat.NodeId) u32 {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return u32(0)
	}
	node := t.a.nodes[int(id)]
	if view_type := mut_optional_arg_view_type(node) {
		// The `&?&T` built for a `mut a ?T` parameter: `T` is the payload's type.
		return view_type
	}
	if node.kind == .string_literal
		&& !(node.children_count == 1 && node.value_text() in ['__v3_comptime_zero', '__v3_comptime_new']) {
		// Raw and JavaScript string literals keep their quote mode in `typ` as
		// parser metadata. Generic inference still sees their semantic V type.
		return flat.source_type_string
	}
	if node.kind == .field_init && flat.source_type_query(flat.source_type_syntax(node.typ).unqualified()).has(.anon_struct_prefix) {
		return t.normalize_type_in_module(node.typ, t.node_module_or(int(id), t.cur_module))
	}
	if node.kind == .prefix && node.op == .amp && node.children_count > 0 {
		child_id := t.a.child(&node, 0)
		child := t.a.nodes[int(child_id)]
		inner_source_id := t.generic_call_arg_type_for_inference(child_id)
		if inner_source_id != 0 {
			// `mut value` is lowered to an address for the C calling convention, but
			// generic inference still sees the value's language-level type. A source
			// `&value` is a real pointer expression and retains the extra indirection.
			if child.is_mut {
				return inner_source_id
			}
			if child.kind == .ident && t.mut_value_ident_nodes[int(child_id)]
				&& flat.source_type_shape(inner_source_id).has(.pointer) && !flat.source_type_shape(t.raw_var_type(child.value_text())).has(.pointer) {
				// A cloned `mut value T` parameter carries `&Concrete` on its identifier
				// as storage metadata. Source `&value` denotes `&Concrete`, not
				// `&&Concrete`; consume the storage pointer before applying the source one.
				return inner_source_id
			}
			return flat.source_type_composition_one(.pointer, inner_source_id)
		}
	}
	if node.kind == .selector && node.children_count > 0 {
		base_id := t.a.child(&node, 0)
		base_type_source_id := t.generic_call_arg_type_for_inference(base_id)

		if base_type_source_id != 0 {
			lookup_type_source_id := t.trim_pointer_type(t.normalize_type_alias(base_type_source_id))

			if field_type_source_id := t.lookup_struct_field_type(lookup_type_source_id, node.value_text()) {
				if generic_inference_arg_type_usable(field_type_source_id)
					&& !t.generic_arg_is_unresolved(field_type_source_id) {
					return t.generic_inference_argument_type(field_type_source_id, t.node_module_or(int(id),
						t.cur_module))
				}
			}
		}
	}
	if node.kind == .call {
		map_type_source_id := t.new_map_call_type(node)

		if map_type_source_id != 0 {
			return map_type_source_id
		}
		if array_type_source_id := t.array_call_type_name(id, node) {
			return array_type_source_id
		}
		// A checked call can carry its base type after lowering. Preserve the
		// declaration's alias identity when inferring a generic argument.
		if !isnil(t.tc) {
			if name := t.tc.resolved_call_name(id) {
				if ret := t.tc.fn_ret_types[name] {
					ret_name_source_id := t.semantic_type_name(ret)
					decl_module := t.tc.fn_type_modules[name] or { t.cur_module }
					if !t.generic_arg_is_unresolved(ret_name_source_id)
						&& t.generic_type_text_contains_alias(ret_name_source_id, decl_module) {
						return t.generic_inference_argument_type(ret_name_source_id, decl_module)
					}
				}
			}
		}
		// A call already rewritten to a concrete generic specialization carries its
		// authoritative return type on the node. Re-inferring that rewritten callee as
		// though it were the open generic can mistake the specialization name for T.
		if generic_inference_arg_type_usable(node.typ) {
			concrete_node_type_source_id := t.generic_inference_argument_type(node.typ,
				t.node_module_or(int(id), t.cur_module))

			if !t.generic_arg_is_unresolved(concrete_node_type_source_id) {
				return concrete_node_type_source_id
			}
		}
		concrete_ret_source_id := t.concrete_generic_call_return_type(id, node)

		if concrete_ret_source_id != 0 && !t.generic_arg_is_unresolved(concrete_ret_source_id) {
			return concrete_ret_source_id
		}
		call_ret_source_id := t.get_call_return_type(id, node)

		if call_ret_source_id != 0 && !t.generic_arg_is_unresolved(call_ret_source_id) {
			return call_ret_source_id
		}
	}
	if node.kind == .array_literal {
		module_name := t.node_module_or(int(id), t.cur_module)
		if alias_type_source_id := t.generic_array_literal_alias_type_for_inference(node, module_name) {
			return alias_type_source_id
		}
	}
	if node.kind == .map_init {
		map_type_source_id_2 := t.infer_map_init_entry_type(node)

		if map_type_source_id_2 != 0 {
			return map_type_source_id_2
		}
	}
	if node.kind == .index && node.value == 'range' && node.children_count > 0 {
		base_type_source_id_2 := t.generic_call_arg_type_for_inference(t.a.child(&node, 0))
		if flat.source_type_shape(base_type_source_id_2).has(.array_type) || base_type_source_id_2 == flat.source_type_string {
			return base_type_source_id_2
		}
		if t.is_fixed_array_type(base_type_source_id_2) {
			return flat.source_type_composition_one(.array, fixed_array_elem_type(base_type_source_id_2))
		}
	}
	if node.kind in [.array_literal, .fn_literal, .lambda_expr] {
		typ_source_id_2 := t.generic_arg_expr_type(id)

		if typ_source_id_2 != 0 {
			return typ_source_id_2
		}
	}
	if node.kind == .ident {
		if node.value_text().starts_with('__') && generic_inference_arg_type_usable(node.typ)
			&& !t.generic_arg_is_unresolved(t.normalize_type_alias(node.typ)) {
			// Lowered expression temporaries carry their concrete type on the generated
			// identifier. The checker's expression cache belongs to the original tree and
			// can only report the unspecialized container (`map`, `array`) here.
			return t.generic_inference_argument_type(node.typ, t.node_module_or(int(id), t.cur_module))
		}
		if sc := t.find_smartcast(node.value_text()) {
			target_source_id := t.smartcast_target_type(sc)
			if generic_inference_arg_type_usable(target_source_id)
				&& !t.generic_arg_is_unresolved(t.normalize_type_alias(target_source_id)) {
				return t.canonical_generic_specialization_arg(target_source_id)
			}
		}
		if t.mut_value_ident_nodes[int(id)] && generic_inference_arg_type_usable(node.typ) {
			return node.typ
		}
		if refined_id := t.refined_node_types[int(id)] {
			if refined_id != 0 && !t.generic_arg_is_unresolved(refined_id) {
				return refined_id
			}
		}
		// During the ordinary monomorphize node scan there is no live function
		// scope: var_types holds bindings from whatever function was transformed
		// last, so an unrelated local with the same name (`result`, `val`, ...)
		// would hijack inference. While a specialization is actively cloning,
		// emit_generic_fn_specialization has seeded a live parameter scope instead.
		if t.in_monomorphize_scan && t.cloning_generic_fn_depth == 0
			&& generic_inference_arg_type_usable(node.typ)
			&& !t.generic_arg_is_unresolved(t.normalize_type_alias(node.typ)) {
			return t.generic_inference_argument_type(node.typ, t.node_module_or(int(id), t.cur_module))
		}
		var_typ_source_id := t.var_type(node.value_text())

		raw_var_typ_source_id := t.raw_var_type(node.value_text())
		raw_var_typ_source_id_2 := raw_var_typ_source_id

		scoped_var_typ := (if raw_var_typ_source_id != 0 {
			raw_var_typ_source_id_2
		} else {
			var_typ_source_id
		})
		// Cloned generic bodies have a live specialization scope. Prefer that
		// binding over the source node's checker cache, which belongs to the shared
		// template and can reflect a previously emitted specialization.
		if t.cloning_generic_fn_depth > 0 && scoped_var_typ != 0 {
			return t.generic_inference_argument_type(t.canonical_generic_specialization_arg(scoped_var_typ),
				t.node_module_or(int(id), t.cur_module))
		}
		if !isnil(t.tc) {
			if checked := t.tc.expr_type(id) {
				if fn_name_source_id := fn_value_type_name_from_type(checked) {
					fn_name_source_id_2 := fn_name_source_id

					return t.normalize_type_alias(fn_name_source_id_2)
				}
				checked_source_id := checked.source_type_id()
				if generic_inference_arg_type_usable(checked_source_id)
					&& !t.generic_arg_is_unresolved(t.normalize_type_alias(checked_source_id)) {
					return t.generic_inference_argument_type(checked_source_id, t.node_module_or(int(id),
						t.cur_module))
				}
			}
		}
		if scoped_var_typ != 0 {
			return t.generic_inference_argument_type(scoped_var_typ, t.node_module_or(int(id), t.cur_module))
		}
		if decl_type_source_id := t.local_decl_type_before(node.value_text(), id) {
			return t.generic_inference_argument_type(decl_type_source_id, t.node_module_or(int(id), t.cur_module))
		}
		if fn_type_source_id := t.fn_value_type_name(id) {

			// Function declarations carry an encoded node type such as
			// `fn_int_string_bool`. Generic inference needs the semantic function
			// type so a specialization emits the matching function-pointer typedef.
			return fn_type_source_id
		}
	}
	if generic_inference_arg_type_usable(node.typ) {
		if t.generic_type_text_contains_alias(node.typ, t.cur_module) {
			return node.typ
		}
		typ_source_id := t.normalize_type_alias(node.typ)

		if typ_source_id != 0 && !t.generic_arg_is_unresolved(typ_source_id) {
			return typ_source_id
		}
	}
	mut typ := t.node_type(id)
	if typ == 0 {
		typ = t.generic_arg_expr_type(id)
	}
	return typ
}

fn generic_inference_arg_type_usable(typ_id u32) bool {
	clean_id := transform_trimmed_type_id(typ_id)
	return decl_type_is_usable(clean_id) && clean_id != flat.source_type_void
}

fn (t &Transformer) generic_inference_argument_type(typ_id u32, module_name string) u32 {
	clean_id := t.trimmed_type_id(typ_id)
	if t.current_specialization_has_generic_arg(clean_id) {
		locked := t.lock_colliding_main_generic_type_text(clean_id, module_name)
		if locked != clean_id { return locked }
	}
	if t.generic_type_text_contains_alias(clean_id, module_name) {
		return t.generic_arg_for_call_and_decl_module(clean_id, module_name, module_name)
	}
	return t.generic_inference_alias_target(clean_id, module_name)
}

fn (t &Transformer) generic_receiver_decl_inference_type(param_type_id u32, decl_type_id u32, module_name string) u32 {
	mut param := t.trimmed_type_id(param_type_id)
	for flat.source_type_shape(param).has(.pointer) || flat.source_type_shape(param).has(.mutable_type) {
		param = t.trimmed_type_id(flat.source_type_syntax(param).raw_base())
	}
	mut declared := t.trimmed_type_id(decl_type_id)
	for flat.source_type_shape(declared).has(.pointer) || flat.source_type_shape(declared).has(.mutable_type) {
		declared = t.trimmed_type_id(flat.source_type_syntax(declared).raw_base())
	}
	p := flat.source_type_syntax(param)
	d := flat.source_type_syntax(declared)
	if p.generic_app_valid() && d.generic_app_valid()
		&& flat.source_type_syntax(p.generic_app_base()).unqualified() == flat.source_type_syntax(d.generic_app_base()).unqualified()
		&& t.generic_arg_is_alias_name_id(d.generic_app_base(), module_name) {
		return t.generic_inference_argument_type(decl_type_id, module_name)
	}
	return t.generic_inference_alias_target(decl_type_id, module_name)
}

fn (mut t Transformer) generic_receiver_needs_decl_type_fallback(param_type_id u32, arg_type_id u32) bool {
	if !generic_inference_arg_type_usable(arg_type_id) { return true }
	if !flat.source_type_shape(param_type_id).has(.bracketed) { return false }
	mut inferred := map[string]u32{}
	probe_arg_type := generic_arg_type_for_param(param_type_id, arg_type_id)
	if probe_arg_type != 0 {
		infer_generic_type_args(param_type_id, probe_arg_type, mut inferred)
		t.infer_generic_receiver_suffix_args(param_type_id, probe_arg_type, mut inferred)
		t.infer_generic_embedded_receiver_args(param_type_id, probe_arg_type, mut inferred)
	}
	if inferred.len == 0 { return true }
	for name, arg_source_id in inferred {
		if arg_source_id == flat.source_type_id(name) || t.generic_arg_is_unresolved(arg_source_id) {
			return true
		}
	}
	return false
}

fn (t &Transformer) generic_inference_alias_target(typ_id u32, module_name string) u32 {
	clean_id := t.trimmed_type_id(typ_id)
	if clean_id == 0 { return clean_id }
	// Preserve caller provenance for homonymous types in nested generic calls.
	if t.substituted_type_belongs_to_main_generic(clean_id) && t.current_specialization_has_generic_arg(clean_id) {
		return clean_id
	}
	if t.current_specialization_has_generic_arg(clean_id) {
		base, args, is_generic := generic_provenance_app_parts(clean_id)
		if is_generic && !flat.source_type_shape(clean_id).has(.map_type) {
			mut parts := []u32{cap: args.len + 1}
			parts << t.generic_inference_alias_target(base, module_name)
			for nested_arg in args {
				parts << t.generic_inference_alias_target(nested_arg, module_name)
			}
			return flat.source_type_composition(.generic, parts)
		}
	}
	if target_source_id := t.direct_generic_inference_alias_target(clean_id, module_name) {
		return target_source_id
	}
	normalized_source_id := t.normalize_type_in_module(clean_id, module_name)
	if normalized_source_id != 0 && normalized_source_id != clean_id && !t.generic_arg_is_unresolved(normalized_source_id) {
		return normalized_source_id
	}
	return clean_id
}

fn (t &Transformer) direct_generic_inference_alias_target(typ_id u32, module_name string) ?u32 {
	if isnil(t.tc) { return none }
	if target_source_id := t.tc.type_aliases.type_id(typ_id) {
		return t.normalize_type_in_module(target_source_id, module_name)
	}
	if module_name != '' && !flat.source_type_shape(typ_id).has(.dotted) {
		qtyp := flat.source_type_composition_two(.qualified, flat.source_type_id(module_name), typ_id)
		if target_source_id := t.tc.type_aliases.type_id(qtyp) {
			return t.normalize_type_in_module(target_source_id, module_name)
		}
	}
	if !flat.source_type_shape(typ_id).has(.dotted) {
		mut unique_target := u32(0)
		for name_source_id, target_id in t.tc.type_aliases.ids {
			name_id := name_source_id
			if name_id != typ_id && flat.source_type_syntax(name_id).unqualified() != typ_id {
				continue
			}
			if unique_target != 0 && unique_target != target_id { return none }
			unique_target = target_id
		}
		if unique_target != 0 { return t.normalize_type_in_module(unique_target, module_name) }
	}
	return none
}

fn (t &Transformer) local_decl_type_before(name string, before flat.NodeId) ?u32 {
	if name == '' || int(before) <= 0 {
		return none
	}
	limit := int(before)
	if t.specialization_node_start >= 0 {
		candidates := t.specialization_decl_nodes_by_name[name]
		for ci := candidates.len - 1; ci >= 0; ci-- {
			idx := candidates[ci]
			if idx < t.specialization_node_start {
				break
			}
			if idx >= limit {
				continue
			}
			if typ_source_id := t.local_decl_type_at(name, idx) {
				return typ_source_id
			}
		}
		if better_source_id := t.local_decl_type_before_by_pos(name, before, t.specialization_node_start, u32(0)) {
			return better_source_id
		}
		return none
	}
	mut start := 0
	for i := limit - 1; i >= 0; i-- {
		node := t.a.node(flat.NodeId(i))
		if node.kind in [.fn_decl, .fn_literal, .lambda_expr] {
			start = i
			break
		}
		if node.kind != .decl_assign || node.children_count < 2 {
			continue
		}
		lhs_id := t.a.child(node, 0)
		if int(lhs_id) < 0 || int(lhs_id) >= t.a.nodes.len {
			continue
		}
		lhs := t.a.node(lhs_id)
		if lhs.kind != .ident || lhs.value_text() != name {
			continue
		}
		if typ_source_id := t.local_decl_type_at(name, i) {
			return typ_source_id
		}
	}
	if better_source_id := t.local_decl_type_before_by_pos(name, before, start, u32(0)) {
		return better_source_id
	}
	return none
}

fn (t &Transformer) local_decl_type_at(name string, idx int) ?u32 {
	if idx < 0 || idx >= t.a.nodes.len {
		return none
	}
	node := t.a.node(flat.NodeId(idx))
	if node.kind != .decl_assign || node.children_count < 2 {
		return none
	}
	lhs_id := t.a.child(node, 0)
	if int(lhs_id) < 0 || int(lhs_id) >= t.a.nodes.len {
		return none
	}
	lhs := t.a.node(lhs_id)
	if lhs.kind != .ident || lhs.value_text() != name {
		return none
	}
	rhs_id := t.a.child(node, 1)
	rhs := t.a.node(rhs_id)
	if rhs.kind == .call {
		mut raw_type := t.raw_call_decl_return_type(rhs_id, *rhs) or { u32(0) }
		if raw_type == 0 && rhs.children_count > 0 {
			callee := t.a.child_node(rhs, 0)
			if callee.kind == .ident {
				raw_type = t.raw_return_type_for_fn_name(callee.value_text(), *rhs) or { u32(0) }
			}
		}
		if raw_type != 0 {
			syntax := flat.source_type_syntax(t.trimmed_type_id(raw_type))
			if syntax.generic_app_valid() && t.generic_arg_is_alias_name_id(syntax.generic_app_base(), t.cur_module) {
				return raw_type
			}
		}
	}
	mut best := u32(0)
	for candidate in [
		t.refined_node_types[int(lhs_id)] or { u32(0) },
		t.refined_node_types[idx] or { u32(0) },
		lhs.typ,
		node.typ,
	] {
		typ := (if t.generic_type_text_contains_alias(candidate, t.cur_module) {
			candidate
		} else {
			t.normalize_type_alias(candidate)
		})
		if typ != 0 && !t.generic_arg_is_unresolved(typ) && decl_type_is_usable(typ) {
			best = typ
		}
	}
	if best == 0 {
		rhs_typ_source_id := t.node_type(rhs_id)
		rhs_typ_source_id_2 := rhs_typ_source_id
		if rhs_typ_source_id != 0 && !t.generic_arg_is_unresolved(rhs_typ_source_id) && decl_type_is_usable(rhs_typ_source_id_2) {
			best = rhs_typ_source_id_2
		}
	}
	if best == 0 {
		return none
	}
	return best
}

fn (t &Transformer) local_decl_type_before_by_pos(name string, before flat.NodeId, start int, current_id u32) ?u32 {
	if int(before) < 0 || int(before) >= t.a.nodes.len {
		return none
	}
	before_node := t.a.node(before)
	if !before_node.pos.is_valid() {
		return none
	}
	mut best := current_id
	start_pos := if start >= 0 && start < t.a.nodes.len
		&& t.a.nodes[start].pos.id == before_node.pos.id {
		t.a.nodes[start].pos.offset
	} else {
		0
	}
	for i in t.local_decl_nodes_by_name[name] {
		node := t.a.node(flat.NodeId(i))
		if i < start || !node.pos.is_valid() || node.pos.id != before_node.pos.id
			|| node.pos.offset < start_pos || node.pos.offset >= before_node.pos.offset {
			continue
		}
		lhs_id := t.a.child(node, 0)
		if int(lhs_id) < 0 || int(lhs_id) >= t.a.nodes.len {
			continue
		}
		lhs := t.a.node(lhs_id)
		if lhs.kind != .ident || lhs.value_text() != name {
			continue
		}
		if declared_type_source_id := t.local_decl_type_at(name, i) {
			if t.generic_type_text_contains_alias(declared_type_source_id, t.cur_module) {
				best = declared_type_source_id
				continue
			}
		}
		for candidate in [
			t.refined_node_types[int(lhs_id)] or { u32(0) },
			t.refined_node_types[i] or { u32(0) },
			lhs.typ,
			node.typ,
		] {
			typ := (if t.generic_type_text_contains_alias(candidate, t.cur_module) {
				candidate
			} else {
				t.normalize_type_alias(candidate)
			})
			if typ == 0 || t.generic_arg_is_unresolved(typ) || !decl_type_is_usable(typ) {
				continue
			}
			best = typ
		}
	}
	if best == current_id || best == 0 {
		return none
	}
	return best
}

fn (t &Transformer) local_decl_type_before_pos(name string, before flat.Node) ?u32 {
	if name == '' || before.pos.offset <= 0 {
		return none
	}
	mut fn_start := 0
	if offsets := t.fn_decl_offsets_by_file[before.pos.id] {
		mut low := 0
		mut high := offsets.len
		for low < high {
			mid := (low + high) / 2
			if offsets[mid] < before.pos.offset {
				low = mid + 1
			} else {
				high = mid
			}
		}
		if low > 0 {
			fn_start = offsets[low - 1]
		}
	} else {
		for node_id in 0 .. t.a.nodes.len {
			// This scan does not modify the AST arena.
			node := unsafe { &t.a.nodes[node_id] }
			if node.kind == .fn_decl && node.pos.id == before.pos.id
				&& node.pos.offset > fn_start && node.pos.offset < before.pos.offset {
				fn_start = node.pos.offset
			}
		}
	}
	mut best := u32(0)
	mut best_offset := -1
	for idx in t.local_decl_nodes_by_name[name] {
		// Context indexes can change here, but the AST arena cannot.
		node := unsafe { &t.a.nodes[idx] }
		if node.pos.id != before.pos.id || node.pos.offset <= fn_start
			|| node.pos.offset >= before.pos.offset
			|| node.pos.offset <= best_offset {
			continue
		}
		lhs_id := t.a.child(node, 0)
		if int(lhs_id) < 0 || int(lhs_id) >= t.a.nodes.len {
			continue
		}
		lhs := t.a.nodes[int(lhs_id)]
		if lhs.kind != .ident || lhs.value_text() != name {
			continue
		}
		if declared_type_source_id := t.local_decl_type_at(name, idx) {
			if t.generic_type_text_contains_alias(declared_type_source_id, t.cur_module) {
				best = declared_type_source_id
				best_offset = node.pos.offset
				continue
			}
		}
		for candidate in [lhs.typ, node.typ] {
			typ := (if t.generic_type_text_contains_alias(candidate, t.cur_module) {
				candidate
			} else {
				t.normalize_type_alias(candidate)
			})
			if typ != 0 && !t.generic_arg_is_unresolved(typ) && decl_type_is_usable(typ) {
				best = typ
				best_offset = node.pos.offset
			}
		}
		if best_offset == node.pos.offset {
			continue
		}
		rhs_id := t.a.child(node, 1)
		rhs_typ_source_id := t.node_type(rhs_id)
		rhs_typ_source_id_3 := rhs_typ_source_id
		if rhs_typ_source_id != 0 && !t.generic_arg_is_unresolved(rhs_typ_source_id) && decl_type_is_usable(rhs_typ_source_id_3) {
			best = rhs_typ_source_id_3
			best_offset = node.pos.offset
		}
	}
	if best == 0 {
		return none
	}
	return best
}

fn (mut t Transformer) infer_generic_receiver_suffix_args(param_type_id u32, arg_type_id u32, mut inferred map[string]u32) {
	param := flat.source_type_syntax(t.trimmed_type_id(param_type_id)).pointer_raw_base()
	arg := flat.source_type_syntax(t.trimmed_type_id(arg_type_id)).pointer_raw_base()
	syntax := flat.source_type_syntax(param)
	if !syntax.generic_app_valid() || syntax.generic_app_args().len == 0 || arg == 0 { return }
	param_args := syntax.generic_app_args()
	base := syntax.generic_app_base()
	if args_source_ids := t.recorded_type_specialization_args(arg) {
		t.infer_generic_receiver_recorded_args(param_args, args_source_ids, mut inferred)
		return
	}
	mut suffix := u32(0)
	for prefix in [base, t.source_c_name_id(base)] {
		suffix = generic_receiver_suffix_after(arg, prefix)
		if suffix != 0 { break }
	}
	if suffix == 0 {
		short := flat.source_type_syntax(base).unqualified()
		arg_short := flat.source_type_syntax(arg).unqualified()
		for candidate in [arg, arg_short, t.source_c_name_id(arg), t.source_c_name_id(arg_short)] {
			for prefix in [short, t.source_c_name_id(short)] {
				suffix = generic_receiver_suffix_after(candidate, prefix)
				if suffix != 0 { break }
			}
			if suffix != 0 { break }
		}
	}
	if suffix == 0 { return }
	// This fallback decodes an emitted C identifier's specialization suffix.
	decoded := t.generic_type_arg_from_receiver_suffix_id(suffix)
	if decoded == 0 { return }
	decoded_id := decoded
	mut assigned := false
	for i, param_arg in param_args {
		if !flat.source_type_query(param_arg).has(.generic_fn_name) { continue }
		key := flat.source_type_name(param_arg)
		if key in inferred { continue }
		if i == 0 || !assigned {
			inferred[key] = decoded_id
			assigned = true
		}
	}
}

fn generic_receiver_suffix_after(candidate u32, prefix u32) u32 {
	syntax := flat.source_type_syntax(candidate)
	for i, head in syntax.underscore_prefixes() {
		if head == prefix { return syntax.underscore_suffixes()[i] }
	}
	return 0
}

// source_c_name_id memoizes pure C-name rendering at its spelling boundary.
fn (mut t Transformer) source_c_name_id(id u32) u32 {
	if cached := t.source_c_name_ids[id] { return cached }
	result := flat.source_type_id(c_name(flat.source_type_name(id)))
	t.source_c_name_ids[id] = result
	return result
}

// source_sum_field_name_id memoizes emitted sum member rendering at its spelling boundary.
fn (mut t Transformer) source_sum_field_name_id(id u32) u32 {
	if cached := t.source_sum_field_name_ids[id] { return cached }
	result := flat.source_type_id(sum_field_name(flat.source_type_name(id)))
	t.source_sum_field_name_ids[id] = result
	return result
}

fn (t &Transformer) infer_generic_receiver_recorded_args(param_args []u32, args_ids []u32, mut inferred map[string]u32) {
	mut arg_idx := 0
	for param_arg in param_args {
		if !flat.source_type_query(param_arg).has(.generic_fn_name) { continue }
		key := flat.source_type_name(param_arg)
		if key in inferred { continue }
		if arg_idx >= args_ids.len { return }
		inferred[key] = args_ids[arg_idx]
		arg_idx++
	}
}

fn (t &Transformer) infer_generic_embedded_receiver_args(param_type_id u32, arg_type_id u32, mut inferred map[string]u32) {
	param := flat.source_type_syntax(t.trimmed_type_id(param_type_id)).pointer_raw_base()
	syntax := flat.source_type_syntax(param)
	if !syntax.generic_app_valid() || syntax.generic_app_base() == 0 { return }
	base := syntax.generic_app_base()
	mut receiver_type := flat.source_type_syntax(t.trimmed_type_id(arg_type_id)).pointer_raw_base()
	receiver_syntax := flat.source_type_syntax(receiver_type)
	if receiver_syntax.generic_app_valid() { receiver_type = receiver_syntax.generic_app_base() }
	// The declaration maps share the interned source spelling IDs.
	info := t.structs[receiver_type] or {
		t.structs[flat.source_type_syntax(receiver_type).unqualified()] or { return }
	}
	for field in info.fields {
		field_type := flat.source_type_syntax(t.trimmed_type_id(field.typ)).pointer_raw_base()
		field_syntax := flat.source_type_syntax(field_type)
		if !field_syntax.generic_app_valid() { continue }
		field_base := field_syntax.generic_app_base()
		if field_base == base || flat.source_type_syntax(field_base).unqualified() == flat.source_type_syntax(base).unqualified() {
			infer_generic_type_args(param, field_type, mut inferred)
			return
		}
	}
}

fn (t &Transformer) generic_call_receiver_id(node flat.Node) ?flat.NodeId {
	if node.children_count == 0 {
		return none
	}
	mut callee_id := t.a.child(&node, 0)
	mut callee := t.a.nodes[int(callee_id)]
	if callee.kind == .index && callee.children_count > 0 {
		callee_id = t.a.child(&callee, 0)
		callee = t.a.nodes[int(callee_id)]
	}
	if callee.kind != .selector || callee.children_count == 0 {
		return none
	}
	return t.a.child(&callee, 0)
}

fn infer_generic_type_args(param_type_id u32, arg_type_id u32, mut inferred map[string]u32) {
	param := flat.source_type_syntax(param_type_id).trimmed()
	arg := flat.source_type_syntax(arg_type_id).trimmed()
	if param == 0 || arg == 0 || arg in [flat.source_type_unknown, flat.source_type_generic] {
		return
	}
	p_shape := flat.source_type_shape(param)
	a_shape := flat.source_type_shape(arg)
	p := flat.source_type_syntax(param)
	a := flat.source_type_syntax(arg)
	if p_shape.has(.variadic) {
		// Variadic parameters denote packed arrays at this point.
		if a_shape.has(.array_type) {
			infer_generic_type_args(p.raw_base(), a.raw_base(), mut inferred)
		}
		return
	}
	if flat.source_type_query(param).has(.generic_fn_name) {
		// Keep an open generic argument open; T -> T is not inference.
		key := flat.source_type_name(param)
		if key !in inferred && arg != param { inferred[key] = arg }
		return
	}
	if p_shape.has(.pointer) {
		inner := if a_shape.has(.pointer) { a.raw_base() } else { arg }
		infer_generic_type_args(p.raw_base(), inner, mut inferred)
		return
	}
	if p_shape.has(.mutable_type) {
		infer_generic_type_args(p.raw_base(), a.pointer_raw_base(), mut inferred)
		return
	}
	if p_shape.has(.thread_type) {
		if a_shape.has(.thread_type) {
			infer_generic_type_args(p.raw_base(), a.raw_base(), mut inferred)
		}
		return
	}
	if p_shape.has(.array_type) && a_shape.has(.array_type) {
		infer_generic_type_args(p.raw_base(), a.raw_base(), mut inferred)
		return
	}
	if p_shape.has(.channel_type) {
		if a_shape.has(.channel_type) {
			infer_generic_type_args(p.raw_base(), a.raw_base(), mut inferred)
		}
		return
	}
	if p_shape.has(.leading_bracket) && a_shape.has(.leading_bracket) {
		if p.bracket_closed() && a.bracket_closed() {
			infer_generic_type_args(p.bracket_base(), a.bracket_base(), mut inferred)
		}
		return
	}
	if p_shape.has(.leading_bracket) && a.closing_bracket() && a_shape.has(.bracketed) {
		if fixed_array_length_id(param) == fixed_array_length_id(arg) {
			infer_generic_type_args(fixed_array_elem_type(param), fixed_array_elem_type(arg), mut inferred)
		}
		return
	}
	if p_shape.has(.option_type) {
		inner := if a_shape.has(.option_type) { a.raw_base() } else { arg }
		infer_generic_type_args(p.raw_base(), inner, mut inferred)
		return
	}
	if p_shape.has(.result_type) {
		inner := if a_shape.has(.result_type) { a.raw_base() } else { arg }
		infer_generic_type_args(p.raw_base(), inner, mut inferred)
		return
	}
	if p.tuple_like() && a.tuple_like() && p.contains_comma() && a.contains_comma() {
		if p.tuple_components().len == a.tuple_components().len {
			for i, part in p.tuple_components() {
				infer_generic_type_args(part, a.tuple_components()[i], mut inferred)
			}
		}
		return
	}
	if p.function_prefix() && a.function_prefix() {
		infer_generic_fn_type_args(param, arg, mut inferred)
		return
	}
	if p_shape.has(.map_type) && a_shape.has(.map_type) {
		if p.bracket_closed() && a.bracket_closed() {
			infer_generic_type_args(p.bracket_raw_inner(), a.bracket_raw_inner(), mut inferred)
			infer_generic_type_args(p.bracket_base(), a.bracket_base(), mut inferred)
		}
		return
	}
	if p.generic_app_valid() && a.generic_app_valid()
		&& flat.source_type_syntax(p.generic_app_base()).unqualified() == flat.source_type_syntax(a.generic_app_base()).unqualified() {
		for i, p_arg in p.generic_app_args() {
			if i < a.generic_app_args().len {
				infer_generic_type_args(p_arg, a.generic_app_args()[i], mut inferred)
			}
		}
	}
}

fn infer_generic_type_args_bytes(param_type string, arg_type string, mut inferred map[string]u32) {
	infer_generic_type_args(flat.source_type_id(param_type), flat.source_type_id(arg_type), mut inferred)
}

fn infer_generic_fn_type_args(param_id u32, arg_id u32, mut inferred map[string]u32) {
	p := flat.source_type_syntax(param_id)
	a := flat.source_type_syntax(arg_id)
	if !p.function_closed() || !a.function_closed() { return }
	for i, p_param in p.function_params() {
		if i >= a.function_params().len { break }
		infer_generic_type_args(flat.source_type_syntax(p_param).generic_parameter_payload(),
			flat.source_type_syntax(a.function_params()[i]).generic_parameter_payload(), mut inferred)
	}
	if p.function_result() != 0 && a.function_result() != 0 {
		infer_generic_type_args(p.function_result(), a.function_result(), mut inferred)
	}
}

fn fn_type_text_parts(typ string) ?([]string, string) {
	clean := trimmed_transform_text(typ)
	open := clean.index_u8(`(`)
	if open < 0 {
		return none
	}
	mut depth := 1
	mut close := open + 1
	for close < clean.len {
		if clean[close] == `(` {
			depth++
		} else if clean[close] == `)` {
			depth--
			if depth == 0 {
				break
			}
		}
		close++
	}
	if close >= clean.len {
		return none
	}
	params_text := clean[open + 1..close]
	params := split_fn_type_params(params_text)
	ret := trimmed_transform_text(clean[close + 1..])
	return params, ret
}

fn split_fn_type_params(s string) []string {
	mut parts := []string{}
	mut depth := 0
	mut start := 0
	for i := 0; i < s.len; i++ {
		c := s[i]
		if c == `(` || c == `[` {
			depth++
		} else if c == `)` || c == `]` {
			depth--
		} else if c == `,` && depth == 0 {
			part := trimmed_transform_text(s[start..i])
			if part.len > 0 {
				parts << part
			}
			start = i + 1
		}
	}
	tail := trimmed_transform_text(s[start..])
	if tail.len > 0 {
		parts << tail
	}
	return parts
}

fn generic_fn_type_param_payload(param string) string {
	mut text := trimmed_transform_text(param)
	if text.starts_with('mut ') {
		text = trimmed_transform_text(text[4..])
	}
	space := generic_top_level_space_index(text)
	if space > 0 {
		head := trimmed_transform_text(text[..space])
		tail := trimmed_transform_text(text[space + 1..])
		if generic_fn_type_param_head_is_name(head, tail) {
			return tail
		}
	}
	return text
}

fn generic_fn_type_param_mode_payload(param string) string {
	trimmed := trimmed_transform_text(param)
	payload := generic_fn_type_param_payload(trimmed)
	if trimmed.starts_with('mut ') {
		return 'mut ${payload}'
	}
	return payload
}

// generic_const_string_cond evaluates an if condition made purely of
// comptime-known string facts (`T.name in ['x.json2.Any', ...]`,
// `T.name == 'X'`, combined with &&/||/!) once the generic args are bound.
fn (mut t Transformer) generic_const_string_cond(id flat.NodeId, args_ids []u32) ?bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return t.generic_const_string_cond(t.a.child(&node, 0), args_ids)
	}
	if node.kind == .prefix && node.op == .not && node.children_count > 0 {
		v := t.generic_const_string_cond(t.a.child(&node, 0), args_ids) or { return none }
		return !v
	}
	if node.kind == .infix && node.children_count == 2 {
		if node.op == .logical_and {
			l := t.generic_const_string_cond(t.a.child(&node, 0), args_ids) or { return none }
			if !l {
				return false
			}
			return t.generic_const_string_cond(t.a.child(&node, 1), args_ids)
		}
		if node.op == .logical_or {
			l := t.generic_const_string_cond(t.a.child(&node, 0), args_ids) or { return none }
			if l {
				return true
			}
			return t.generic_const_string_cond(t.a.child(&node, 1), args_ids)
		}
		if node.op in [.eq, .ne] {
			l := t.generic_const_string_value(t.a.child(&node, 0), args_ids) or { return none }
			r := t.generic_const_string_value(t.a.child(&node, 1), args_ids) or { return none }
			return if node.op == .eq { l == r } else { l != r }
		}
		return none
	}
	if node.kind == .in_expr && node.children_count == 2 {
		l := t.generic_const_string_value(t.a.child(&node, 0), args_ids) or { return none }
		rhs := t.a.nodes[int(t.a.child(&node, 1))]
		if rhs.kind != .array_literal {
			return none
		}
		for i in 0 .. rhs.children_count {
			item := t.generic_const_string_value(t.a.child(&rhs, i), args_ids) or { return none }
			if item == l {
				return true
			}
		}
		return false
	}
	return none
}

fn (mut t Transformer) generic_const_string_value(id flat.NodeId, args_ids []u32) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .string_literal {
		return node.value_text()
	}
	if node.kind == .selector && node.value_text() == 'name' && node.children_count > 0 {
		base_id := t.a.child(&node, 0)
		base := t.a.nodes[int(base_id)]
		if base.kind == .typeof_expr {
			if reflected_source_id := t.generic_comptime_typeof_target(base, args_ids) {
				reflected := flat.source_type_name(reflected_source_id)

				return generic_type_name_display(reflected)
			}
		}
		if t.selector_base_is_comptime_type_value(base_id) {
			if reflected_source_id := t.generic_comptime_base_type(base_id, args_ids) {
				reflected := flat.source_type_name(reflected_source_id)

				return generic_type_name_display(reflected)
			}
		}
	}
	return none
}

fn (mut t Transformer) clone_generic_fn_node(node flat.Node, args_ids []u32) flat.NodeId {
	return t.clone_generic_root(node, args_ids)
}

// clone_generic_root clones a generic function root with no narrowing from the
// body that requested the instance.
fn (mut t Transformer) clone_generic_root(node flat.Node, args_ids []u32) flat.NodeId {
	outer_smartcasts := t.generic_typeof_smartcasts
	t.generic_typeof_smartcasts = []SmartcastContext{}
	cloned := t.clone_generic_node_from(node, args_ids, true, u32(0))
	t.generic_typeof_smartcasts = outer_smartcasts
	return cloned
}

// clone_generic_fn_header clones the generic function `node` without its body:
// only the parameters, which the signature of the instance needs.
fn (mut t Transformer) clone_generic_fn_header(node flat.Node, args_ids []u32) flat.NodeId {
	start := t.a.children.len
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if t.a.nodes[int(child_id)].kind == .param {
			t.a.children << child_id
		}
	}
	header := flat.Node{
		...node
		children_start: start
		children_count: flat.child_count(t.a.children.len - start)
	}
	return t.clone_generic_root(header, args_ids)
}

@[direct_array_access]
fn (mut t Transformer) clone_generic_node(id flat.NodeId, args_ids []u32) flat.NodeId {
	return t.clone_generic_node_with_return_context(id, args_ids, u32(0))
}

// subtree_references_comptime_for_vars reports whether the subtree `id` of a
// generic body names a loop variable of the `$for` bodies being cloned, as
// `field.name`, `val.$(field.name)` and `$if field.typ is string` do. It looks
// for identifier dependencies in cached type metadata and conservatively scans
// source expression text, which can make it say yes too often.
fn (t &Transformer) subtree_references_comptime_for_vars(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.node(id)
	for loop_var in t.cloning_comptime_for_vars {
		loop_var_id := flat.source_type_id(loop_var)
		if node.value.contains(loop_var) || loop_var_id in flat.source_type_syntax(node.source_value).words()
			|| loop_var_id in flat.source_type_syntax(node.typ).words() {
			return true
		}
		for arg in node.call_type_args() {
			if loop_var_id in flat.source_type_syntax(arg).words() {
				return true
			}
		}
	}
	for i in 0 .. node.children_count {
		if t.subtree_references_comptime_for_vars(t.a.child(node, i)) {
			return true
		}
	}
	return false
}

@[direct_array_access]
fn (mut t Transformer) clone_generic_node_with_return_context(id flat.NodeId, args_ids []u32, return_context_id u32) flat.NodeId {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return id
	}
	node := t.a.nodes[int(id)]
	clone_id := t.clone_generic_node_from(node, args_ids, false, return_context_id)
	t.copy_cloned_resolution(id, clone_id)
	if node.kind == .decl_assign {
		cloned_decl := t.a.nodes[int(clone_id)]
		if cloned_decl.children_count > 2 && t.multi_assign_rhs_count(cloned_decl) == 1 {
			lhs_ids := t.multi_assign_lhs_ids(cloned_decl)
			rhs_id := t.a.child(&cloned_decl, 1)
			if value_types := t.multi_return_types_for_expr(rhs_id, lhs_ids.len) {
				for i, lhs_id in lhs_ids {
					lhs := t.a.nodes[int(lhs_id)]
					value_type := value_types[i].source_type_id()
					if lhs.kind == .ident && lhs.value_text() != '_' && !t.generic_arg_is_unresolved(value_type) {
						t.set_node_typ(int(lhs_id), value_type)
						t.set_var_type_with_raw(lhs.value_text(), t.normalize_type_alias(value_type), value_type)
					}
				}
			}
		}
	}
	return clone_id
}

fn (mut t Transformer) clone_specialized_comptime_new_marker(node flat.Node, target_id u32) flat.NodeId {
	type_expr := t.a.add_node(flat.Node{
		kind:         .ident
		source_value: target_id
	})
	start := t.a.children.len
	t.a.children << type_expr
	marker := t.a.add_node(flat.Node{
		kind:           .string_literal
		pos:            node.pos
		source_value:   node.source_value
		value:          node.value
		typ:            node.typ
		children_start: start
		children_count: 1
	})
	// Keep the deferred marker pointer-typed when call arguments inspect it before transforming it.
	pointer := flat.source_type_composition_one(.pointer, target_id)
	return t.make_cast(pointer, marker, pointer)
}

fn (mut t Transformer) generic_clone_child_return_context(node flat.Node, child_index int, return_context_id u32, argument_types_ids []u32) u32 {
	if node.kind == .return_stmt && node.children_count == 1 && child_index == 0 {
		return t.cur_fn_ret_type
	}
	if node.kind == .call && child_index > 0 && argument_types_ids.len > 0 {
		index := child_index - 1
		if index < argument_types_ids.len {
			return t.variadic_parameter_base_type(argument_types_ids[index])
		}
		last := argument_types_ids.last()
		if flat.source_type_shape(last).has(.variadic) {
			return t.variadic_parameter_base_type(last)
		}
	}
	return if t.generic_clone_child_is_return_value(node, child_index, return_context_id) {
		return_context_id
	} else {
		u32(0)
	}
}

fn (t &Transformer) variadic_parameter_base_type(type_id u32) u32 {
	if !flat.source_type_shape(type_id).has(.variadic) { return type_id }
	return flat.source_type_syntax(type_id).raw_base()
}

fn (mut t Transformer) generic_clone_argument_type(id flat.NodeId, args_ids []u32) u32 {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return u32(0)
	}
	node := t.a.node(id)
	if node.kind in [.paren, .dump_expr] && node.children_count == 1 {
		return t.generic_clone_argument_type(t.a.child(node, 0), args_ids)
	}
	if node.kind == .call {
		// A nested call whose parameters do not determine its result still needs
		// context. Do not turn its template placeholder into an enclosing type arg.
		decls := t.cached_generic_fn_decls()
		if key := t.generic_call_decl_key(id, *node, t.cur_module, decls) {
			if decl := decls[key] {
				call_args_source_ids := t.generic_clone_call_type_args(*node, decl, args_ids, u32(0))

				if call_args_source_ids.len == 0 { return u32(0) }
				return t.specialized_fn_return_type_text(decl, call_args_source_ids)
			}
		}
	}
	return t.resolve_substituted_type_text(t.subst_type(t.generic_call_arg_type_for_inference(id), args_ids))
}

fn (mut t Transformer) generic_clone_call_type_args(node flat.Node, decl GenericFnDecl, args_ids []u32, return_context_id u32) []u32 {
	param_names := t.generic_fn_param_names(decl.node, decl.module)
	is_receiver := t.generic_decl_is_receiver_method(decl.node)
		&& !t.generic_call_is_static_assoc_selector(node, decl)
	receiver_params := if is_receiver { t.generic_receiver_param_names(decl) } else { []string{} }
	mut inferred := map[string]u32{}
	mut param_idx := 0
	param_count := t.generic_decl_param_count(decl.node)
	for i in 0 .. decl.node.children_count {
		param := t.a.child_node(&decl.node, i)
		if param.kind != .param {
			if t.prefix_param_scan { break }
			continue
		}
		if generic_param_text_is_variadic(param.typ) && param_idx == param_count - 1 {
			elem_param_type := generic_variadic_elem_param_text(param.typ)
			for arg_id in t.generic_call_arg_ids_from_param(node, param_idx, is_receiver) {
				param_type, value_id := t.generic_variadic_arg_for_inference(param.typ, elem_param_type, arg_id)
				arg_type := t.generic_clone_argument_type(value_id, args_ids)
				if decl_type_is_usable(arg_type) && !t.generic_arg_is_unresolved(arg_type) {
					infer_generic_type_args(param_type, generic_arg_type_for_param(param_type, arg_type), mut inferred)
				}
			}
			param_idx++
			continue
		}
		arg_id := t.generic_call_arg_id_for_param(node, param_idx, is_receiver) or {
			param_idx++
			continue
		}
		mut arg_type := t.generic_clone_argument_type(arg_id, args_ids)
		if decl_type_is_usable(arg_type) && !t.generic_arg_is_unresolved(arg_type) {
			param_type := generic_inference_param_type(param)
			if (param.is_mut || (is_receiver && param_idx == 0 && !flat.source_type_shape(param_type).has(.pointer)))
				&& flat.source_type_shape(arg_type).has(.pointer) {
				arg_type = t.trim_pointer_type(arg_type)
			}
			infer_generic_type_args(param_type, generic_arg_type_for_param(param_type, arg_type), mut inferred)
		}
		param_idx++
	}
	if explicit_source_ids := t.explicit_generic_call_args(node, t.cur_module) {
		names := if explicit_source_ids.len == param_names.len {
			param_names
		} else {
			param_names.filter(it !in receiver_params)
		}
		for i, name in names {
			if i < explicit_source_ids.len {
				inferred[name] = t.subst_type(explicit_source_ids[i], args_ids)
			}
		}
	} else if node.children_count > 0 {
		callee := t.a.child_node(&node, 0)
		if preserved_source_ids := t.generic_call_args_preserving_explicit(*callee, decl, inferred) {
			return preserved_source_ids.clone()
		}
	}
	if return_context_id != 0 {
		t.infer_generic_return_type_args(decl, t.generic_inference_expected_type(return_context_id), mut inferred, receiver_params)
	}
	mut call_args := []u32{cap: param_names.len}
	for name in param_names {
		arg_source_id := inferred[name] or { return []u32{} }

		if t.generic_arg_is_unresolved(arg_source_id) {
			return []u32{}
		}
		call_args << t.inherited_generic_arg_for_decl_module(arg_source_id, decl.module, args_ids)
	}
	return call_args.clone()
}

fn (mut t Transformer) generic_clone_call_param_types(node flat.Node, args_ids []u32, return_context_id u32) []u32 {
	if node.kind != .call || node.children_count < 2 || t.skip_generics
		|| return_context_id == 0 || t.cloning_comptime_for_depth > 0 {
		return []u32{}
	}
	decls := t.cached_generic_fn_decls()
	key := t.generic_call_decl_key(flat.empty_node, node, t.cur_module, decls) or { return []u32{} }
	decl := decls[key] or { return []u32{} }
	call_args_source_ids := t.generic_clone_call_type_args(node, decl, args_ids, return_context_id)

	if call_args_source_ids.len == 0 { return []u32{} }
	param_types_source_ids := t.specialized_generic_call_param_type_texts(decl, call_args_source_ids)
	if t.generic_decl_is_receiver_method(decl.node)
		&& t.call_is_selector_form(node)
		&& !t.generic_call_is_static_assoc_selector(node, decl) && param_types_source_ids.len > 0 {
		return param_types_source_ids[1..].clone()
	}
	return param_types_source_ids.clone()
}

fn (mut t Transformer) generic_clone_child_is_return_value(node flat.Node, child_index int, return_context_id u32) bool {
	if return_context_id != 0 && node.kind in [.prefix, .infix] {
		mut expected := t.generic_inference_expected_type(return_context_id)
		for flat.source_type_shape(expected).has(.option_type) || flat.source_type_shape(expected).has(.result_type) {
			expected = t.trimmed_type_id(flat.source_type_syntax(expected).raw_base())
		}
		result_type := unalias_type(t.tc.parse_resolution_type(expected))
		if node.kind == .prefix {
			if node.children_count != 1 || child_index != 0 {
				return false
			}
			return match node.op {
				.not { *result_type == types.Type(types.bool_) }
				.plus, .minus { result_type.is_integer() || result_type.is_float() }
				.bit_not { result_type.is_integer() || result_type is types.Enum }
				else { false }
			}
		}
		if node.children_count != 2 {
			return false
		}
		if !types.is_builtin_type_name_id(expected) {
			if _ := t.struct_operator_call_info_any(expected, node.op) {
				return false
			}
		}
		for i in 0 .. node.children_count {
			operand_name_source_id := t.node_type(t.a.child(&node, i))
			operand_type := unalias_type(t.tc.parse_resolution_type(operand_name_source_id))
			// Pointer arithmetic and overloaded operators need their operand types,
			// which can differ from the enclosing return type.
			if operand_type is types.Pointer || operand_type is types.Struct {
				return false
			}
			if !types.is_builtin_type_name_id(operand_name_source_id) {
				if _ := t.struct_operator_call_info_any(operand_name_source_id, node.op) {
					return false
				}
			}
		}
		if result_type.is_integer() || result_type.is_float() {
			return node.op in [.plus, .minus, .mul, .div, .mod, .amp, .pipe, .xor, .power]
				|| (child_index == 0
					&& node.op in [.left_shift, .right_shift, .right_shift_unsigned])
		}
		if result_type is types.Enum {
			return node.op in [.amp, .pipe, .xor]
		}
		if *result_type == types.Type(types.bool_) {
			return node.op in [.logical_and, .logical_or]
		}
		return result_type.is_string() && node.op == .plus
	}
	return (node.kind == .return_stmt && node.children_count == 1 && child_index == 0)
		|| (return_context_id != 0 && node.kind in [.paren, .postfix, .expr_stmt, .dump_expr]
			&& node.children_count == 1 && child_index == 0)
		|| (return_context_id != 0 && node.kind == .or_expr)
		|| (return_context_id != 0 && node.kind in [.if_expr, .match_stmt] && child_index > 0)
		|| (return_context_id != 0 && node.kind == .comptime_if)
		|| (return_context_id != 0 && node.kind in [.block, .match_branch, .lock_expr]
			&& child_index == node.children_count - 1)
}

@[direct_array_access]
fn (mut t Transformer) clone_generic_node_from(node flat.Node, args_ids []u32, is_root bool, return_context_id u32) flat.NodeId {
	if node.kind == .string_literal && node.children_count == 1
		&& node.value_text() in ['__v3_comptime_zero', '__v3_comptime_new'] {
		if target_source_id := t.generic_comptime_type_expr(t.a.child(&node, 0), args_ids) {
			return if node.value_text() == '__v3_comptime_new' {
				t.clone_specialized_comptime_new_marker(node, target_source_id)
			} else {
				t.comptime_zero_value(target_source_id)
			}
		}
	}
	if node.kind == .selector && node.children_count > 0 {
		base_id := t.a.child(&node, 0)
		if node.value_text() == 'name' {
			base := t.a.nodes[int(base_id)]
			if base.kind == .typeof_expr {
				if reflected_source_id := t.generic_comptime_typeof_target(base, args_ids) {
					reflected := flat.source_type_name(reflected_source_id)

					return t.make_string_literal(t.generic_comptime_typeof_display_name(base, args_ids, reflected))
				}
			}
			if t.selector_base_is_comptime_type_value(base_id) {
				if reflected_source_id := t.generic_comptime_base_type(base_id, args_ids) {
					reflected := flat.source_type_name(reflected_source_id)

					return t.make_string_literal(generic_type_name_display(reflected))
				}
			}
		}
		if node.value_text() in ['idx', 'typ', 'key_type', 'value_type', 'element_type', 'payload_type',
			'pointee_type', 'indirections']
			&& t.selector_base_is_comptime_type_value(base_id) {
			if concrete_source_id := t.generic_comptime_type_expr_for_member(base_id, node.value_text(), args_ids) {
				if node.value_text() == 'indirections' {
					return t.make_int_literal(generic_type_indirections(concrete_source_id))
				}
				target := if node.value_text() in ['idx', 'typ'] {
					concrete_source_id
				} else {
					t.generic_comptime_type_member(concrete_source_id, node.value_text()) or { u32(0) }
				}
				if target != 0 {
					return t.make_int_literal(t.type_index(target, t.cur_module))
				}
			}
		}
		if node.value_text() == 'variant_types' && t.selector_base_is_comptime_type_value(base_id) {
			if concrete_source_id := t.generic_comptime_type_expr(base_id, args_ids) {
				resolved := t.resolve_sum_name(concrete_source_id)
				if variants_source_ids := t.sum_types.type_ids(resolved) {
					mut ids := []flat.NodeId{cap: variants_source_ids.len}
					for variant_source_id in variants_source_ids {
						variant_source_id_3 := variant_source_id

						ids << t.make_int_literal(t.type_index(variant_source_id_3, t.cur_module))
					}
					return t.make_array_literal_typed(ids, flat.source_type_id('[]int'))
				}
			}
		}
	}
	// Evaluate `$if` guards that become decidable once the generic args are
	// substituted, cloning only the taken branch. Cloning dead branches would
	// leave their nodes in the flat arena where the monomorphize call-site scan
	// still visits them, emitting bogus specializations for calls that only
	// typecheck in the pruned branch (e.g. `decode_string[SomeStruct]`).
	// A runtime `if` whose condition is entirely comptime-known strings
	// (`if T.name in [...] { } else { <only valid for arrays> }`) keeps only
	// its taken branch, like the reference compiler; the dead branch may not
	// even typecheck for this specialization.
	if node.kind == .if_expr && t.cloning_comptime_for_depth == 0 && node.children_count >= 2 {
		if take_then := t.generic_const_string_cond(t.a.child(&node, 0), args_ids) {
			branch_index := if take_then { 1 } else { 2 }
			if branch_index >= int(node.children_count) {
				return t.make_empty()
			}
			return t.clone_generic_node_with_return_context(t.a.child(&node, branch_index), args_ids, return_context_id)
		}
	}
	if node.kind == .comptime_if && t.cloning_comptime_for_depth == 0 {
		cond := t.subst_comptime_type_condition(node.value_text(), args_ids)
		if binding, pointee := comptime_pointer_type_binding(cond) {
			branch_index := 0
			if branch_index >= int(node.children_count) {
				return t.make_empty()
			}
			old_params := t.active_generic_params.clone()
			t.active_generic_params << binding
			mut bound_args := args_ids.clone()
			bound_args << flat.source_type_id(pointee)
			result := t.clone_generic_node_with_return_context(t.a.child(&node, branch_index), bound_args, return_context_id)
			t.clear_node_type_memo()
			t.active_generic_params = old_params
			return result
		}
		if take_then := t.comptime_type_condition_value(cond) {
			branch_index := if take_then { 0 } else { 1 }
			if branch_index >= int(node.children_count) {
				return t.make_empty()
			}
			return t.clone_generic_node_with_return_context(t.a.child(&node, branch_index), args_ids, return_context_id)
		}
	}
	if node.kind == .typeof_expr {
		if reflected_source_id := t.generic_comptime_typeof_target(node, args_ids) {
			reflected := flat.source_type_name(reflected_source_id)

			return t.make_string_literal(t.generic_comptime_typeof_display_name(node, args_ids, reflected))
		}
	}
	if node.children_count == 0
		&& node.kind in [.empty, .int_literal, .float_literal, .bool_literal, .char_literal,
			.string_literal, .nil_literal, .none_expr, .break_stmt, .continue_stmt]
		&& !generic_id_contains_param(node.typ, t.active_generic_params) {
		return t.a.add_node(node)
	}
	// Calls nested inside a `$for` body must not be specialized during the clone: the loop
	// variable's members are not resolved yet, so mono would infer a garbage type argument and
	// emit an uncalled, uncompilable specialization. The comptime unroll (or its skip) handles
	// them later against the concrete field types.
	is_comptime_for := node.kind == .comptime_for
	if is_comptime_for {
		t.cloning_comptime_for_depth++
		loop_var, _ := comptime_for_parts(node.value_text())
		t.cloning_comptime_for_vars << loop_var
	}
	argument_types_source_ids := t.generic_clone_call_param_types(node, args_ids, return_context_id)

	scratch_start := t.generic_clone_children.len
	for _ in 0 .. node.children_count {
		t.generic_clone_children << flat.empty_node
	}
	for i in 0 .. node.children_count {
		mut match_smartcasts := 0
		typeof_smartcasts_len := t.generic_typeof_smartcasts.len
		// Receiver calls are retargeted while the generic tree is cloned. Preserve
		// the branch narrowing here so they are not bound to the enclosing sum type.
		if node.kind == .match_stmt && i > 0 {
			match_expr_id := t.a.child(&node, 0)
			branch := t.a.child_node(&node, i)
			for sc in t.match_branch_type_contexts(match_expr_id, branch) {
				variant := t.resolve_substituted_type_text(t.subst_type(sc.variant_name, args_ids))
				sum_type := t.resolve_substituted_type_text(t.subst_type(sc.sum_type_name, args_ids))
				t.push_smartcast(sc.expr_name, variant, sum_type)
				t.generic_typeof_smartcasts << SmartcastContext{
					type_texts:    t.var_type_texts
					expr_name:     sc.expr_name
					variant_name:  variant
					sum_type_name: sum_type
				}
				match_smartcasts++
			}
		} else if node.kind == .if_expr && (i == 1 || i == 2) {
			t.collect_generic_typeof_smartcasts(t.a.child(&node, 0), args_ids, i == 2)
		}
		child_context_source_id := t.generic_clone_child_return_context(node, i, return_context_id, argument_types_source_ids.clone())

		child := t.clone_generic_node_with_return_context(t.a.child(&node, i), args_ids,
			child_context_source_id)
		for _ in 0 .. match_smartcasts {
			t.pop_smartcast()
		}
		t.generic_typeof_smartcasts.trim(typeof_smartcasts_len)
		t.generic_clone_children[scratch_start + i] = child
		if node.kind == .if_expr && i == 0 {
			t.unwrap_cloned_if_guard_binding(child)
		}
		if node.kind == .for_in_stmt && i == 2 {
			t.seed_cloned_generic_for_in_bindings(node, t.generic_clone_children[scratch_start], t.generic_clone_children[scratch_start + 1], child)
		}
	}
	mut children := t.generic_clone_children[scratch_start..scratch_start + int(node.children_count)]
	if is_comptime_for {
		t.cloning_comptime_for_vars = t.cloning_comptime_for_vars[..t.cloning_comptime_for_vars.len - 1]
		t.cloning_comptime_for_depth--
	}
	// A string interpolation format is stored in `directive.typ_text()`, but it is not a
	// type name. Qualifying it in an imported generic specialization turns `04X`
	// into e.g. `json2.04X`, which silently drops the requested base and width.
	substituted_node_type_id := if (node.kind == .directive
		&& node.value_text() == 'string_interp_format') || node.kind == .offsetof_expr {
		// `offsetof_expr.typ_text()` stores the field name, not a type annotation.
		node.typ
	} else {
		t.subst_type(node.typ, args_ids)
	}
	mut cloned_typ := (
		if substituted_node_type_id == node.typ {
			node.typ
		} else if t.generic_type_text_contains_alias(substituted_node_type_id, t.cur_module) {
			substituted_node_type_id
		} else if node.kind == .comptime_for {
			// Reflection needs the source-level generic application (`Box[int]`) so it
			// can substitute the receiver method's `T`. The storage spelling
			// (`Box_int`) intentionally loses those arguments.
			substituted_node_type_id
		} else if node.kind == .directive && node.value_text() == 'string_interp_format' {
			substituted_node_type_id
		} else {
			t.resolve_substituted_type_text(substituted_node_type_id)
		}
	)
	// The parser stores `?T{}` on struct_init.value while leaving typ empty. Give
	// the clone its concrete optional type so the empty-literal handling below can
	// mark it as a present zero value, matching generic `?T{}` semantics.
	if node.kind == .struct_init && cloned_typ == 0 && t.is_optional_type_name(node.source_value_id()) {
		cloned_typ = t.resolve_substituted_type_text(t.subst_type(node.source_value_id(), args_ids))
	}
	// A parameter type produced by substituting a generic parameter can carry an
	// external program (main) type that collides by short name with a same-named
	// type in the specialization's module (a user `Context` embedding `veb.Context`).
	// Lock it to an explicit `main.` spelling so codegen (and the closure's field
	// accesses like `ctx.Context`) bind the program type, not the callee module's
	// homonym — mirroring the struct-init literal lock below.
	if substituted_node_type_id != node.typ && node.kind != .directive {
		cloned_typ = t.lock_colliding_main_substitution_type_text(node.typ, cloned_typ, t.cur_module, t.active_generic_params)
	}
	if node.kind == .ident && t.cloning_generic_fn_depth > 0 {
		scoped_type_source_id := t.raw_var_type(node.value_text())
		scoped_type_source_id_4 := scoped_type_source_id

		if scoped_type_source_id != 0 && !t.generic_arg_is_unresolved(scoped_type_source_id) {
			cloned_typ = scoped_type_source_id_4
		}
	}
	if node.kind == .ident && t.mut_param_values[node.value_text()] && flat.source_type_shape(cloned_typ).has(.pointer)
		&& !flat.source_type_shape(t.raw_var_type(node.value_text())).has(.pointer) {
		// A use of a `mut T` parameter is a T value even though the parameter's
		// storage annotation is `&T`. Keeping the pointer annotation on cloned
		// idents makes the later generic-call scan emit duplicate `&T`
		// specializations instead of reusing the value-type specialization. An
		// explicit `mut p &T` has a scoped `&T` rvalue type and must retain it.
		cloned_typ = flat.source_type_syntax(cloned_typ).raw_base()
	}
	if node.kind == .struct_init && node.value_text() == '__v_option'
		&& t.is_optional_type_name(t.cur_fn_ret_type) {
		cloned_typ = t.cur_fn_ret_type
	}
	if node.kind == .array_init && node.source_value_id() != 0 {
		array_source := node.source_value_id()
		array_substituted := t.subst_type(array_source, args_ids)
		array_locked := if array_substituted != array_source {
			t.lock_colliding_main_substitution_type_text(array_source, array_substituted, t.cur_module, t.active_generic_params)
		} else {
			array_substituted
		}
		array_value := if flat.source_type_syntax(array_locked).main_fragment() {
			array_locked
		} else {
			t.resolve_substituted_type_text(array_locked)
		}
		// A fixed-array init keeps its whole type in `value`, a dynamic one only the
		// element type. Once a type parameter is substituted, the shape has to come
		// from the unsubstituted spelling: `[]T{}` with `T = [7]u8` is `[][7]u8`.
		array_value_shape := flat.source_type_shape(array_value)
		is_fixed_init := if array_substituted != array_source {
			flat.source_type_shape(array_source).has(.leading_bracket)
				&& !flat.source_type_shape(array_source).has(.array_type)
		} else {
			t.is_fixed_array_type(array_value) || (array_value_shape.has(.leading_bracket) && !array_value_shape.has(.array_type))
		}
		cloned_typ = if is_fixed_init {
			array_value
		} else {
			flat.source_type_composition_one(.array, array_value)
		}
	} else if node.kind == .struct_init && node.source_value_id() != 0 && node.value_text() != '__v_option'
		&& !t.is_optional_type_name(node.source_value_id()) {
		// The checker can annotate `T{}` with its surrounding optional/result
		// context. For a concrete clone the literal itself is authoritative.
		struct_source := node.source_value_id()
		struct_subst := t.subst_type(struct_source, args_ids)
		struct_subst_locked := if struct_subst != struct_source {
			t.lock_colliding_main_substitution_type_text(struct_source, struct_subst, t.cur_module, t.active_generic_params)
		} else {
			struct_subst
		}
		struct_value_pre := t.resolve_substituted_type_text(struct_subst_locked)
		mut struct_value := struct_value_pre
		if struct_subst != struct_source {
			// Only a `T{}` literal whose type came from substituting a generic
			// parameter can carry an external program (main) type. Lock a colliding
			// one so codegen builds that struct rather than the callee module's
			// homonym. A literal module type (`json2.Decoder{}` written as `Decoder{}`
			// inside json2) is left untouched — it is bare-keyed but not a lock target.
			struct_value = t.lock_colliding_main_substitution_type_text(struct_source, struct_value_pre, t.cur_module, t.active_generic_params)
		}
		if flat.source_type_syntax(struct_value).main_fragment() {
			// Keep the explicit `main.` lock as the literal's type: parsing it back to
			// its bare program name here would let codegen rebase it into the callee
			// module (the very collision the lock prevents). The lock may be nested inside
			// a composite (`chan main.Context`, `[]main.Context`, `map[string]main.Context`),
			// not just a scalar `main.Context`, so match `main.` anywhere — a bare
			// `chan Context{}` literal would otherwise round-trip to the callee's homonym.
			cloned_typ = struct_value
		} else {
			struct_value_normalized := t.normalize_type_alias(struct_value)
			parsed := t.tc.parse_resolution_type(struct_value_normalized)
			cloned_typ = if t.is_fixed_array_type(struct_value) {
				t.resolved_fixed_array_canonical_type(struct_value)
			} else if t.generic_arg_is_alias_name_id(struct_value, t.cur_module) {
				struct_value
			} else if parsed is types.Unknown {
				struct_value_normalized
			} else {
				specialized_signature_storage_type_name(parsed)
			}
		}
	}
	if node.kind == .or_expr && children.len > 0 {
		source_type_source_id := t.node_type(children[0])

		if t.is_optional_type_name(source_type_source_id) {
			cloned_typ = t.optional_base_type(t.qualify_optional_type(source_type_source_id))
		}
	}
	mut cloned_op := node.op
	if node.kind == .selector && children.len > 0 {
		base_type_source_id := t.node_type(children[0])
		base_type_shape_2 := flat.source_type_shape(base_type_source_id)
		if field_type_source_id := t.lookup_struct_field_type(base_type_source_id, node.value_text()) {
			field_type_source_id_2 := field_type_source_id

			cloned_typ = field_type_source_id_2
		}
		if base_type_shape_2.has(.pointer) {
			cloned_op = .arrow
		}
	}
	if node.kind == .infix && children.len >= 2 {
		lhs_type_source_id := t.trim_pointer_type(t.node_type(children[0]))

		struct_type_source_id_3 := t.generic_struct_instance_name(lhs_type_source_id)

		if struct_type_source_id_3 != 0 {
			if call_info := t.struct_operator_call_info(struct_type_source_id_3, node.op) {
				t.mark_struct_operator_used_name(call_info.name)
			}
		}
	}
	if node.kind == .prefix && children.len > 0 {
		raw_child_type_id := t.generic_clone_decl_alias_type(children[0])
		child_type := if t.generic_type_text_contains_alias(raw_child_type_id, t.cur_module) {
			raw_child_type_id
		} else {
			t.node_type(children[0])
		}
		if node.op == .mul && flat.source_type_shape(child_type).has(.pointer) {
			cloned_typ = flat.source_type_syntax(child_type).raw_base()
		} else if node.op == .amp && child_type != 0 {
			cloned_typ = flat.source_type_composition_one(.pointer, child_type)
		}
	}
	// Record the local's substituted type as the body is cloned: later sibling
	// clones (nested generic calls) infer their type args from these locals, and
	// without a binding the lookup falls back to an arena scan that can land in
	// an unrelated, previously cloned function with a same-named local. A local of
	// a `$for` body is recorded too when its value does not depend on the loop
	// variable, which gives it one type for every member: the types that the clone
	// derives from it, as `&routes` does, came from another declaration of its
	// name, such as a constant.
	if node.kind == .decl_assign && node.children_count == 2 && (t.cloning_comptime_for_depth == 0
		|| !t.subtree_references_comptime_for_vars(t.a.child(&node, 1))) {
		lhs_clone := t.a.nodes[int(children[0])]
		if lhs_clone.kind == .ident && lhs_clone.value_text().len > 0 {
			// Synthetic declarations created while transforming a generic template
			// carry their intended type on the declaration itself. Once substituted,
			// that concrete type is more precise than a helper call's broad checker type.
			has_concrete_substituted_type := substituted_node_type_id != node.typ
				&& decl_type_is_usable(cloned_typ) && !t.generic_arg_is_unresolved(cloned_typ)
			rhs_node := t.a.nodes[int(children[1])]
			call_alias_type_source_id := t.generic_clone_decl_alias_type(children[1])
			rhs_raw_typ := if call_alias_type_source_id != 0 {
				call_alias_type_source_id
			} else if t.generic_type_text_contains_alias(rhs_node.typ, t.cur_module) {
				rhs_node.typ
			} else {
				u32(0)
			}
			mut rhs_typ := (if rhs_raw_typ != 0 {
				rhs_raw_typ
			} else if decl_type_is_usable(rhs_node.typ)
				&& !t.generic_arg_is_unresolved(rhs_node.typ) {
				rhs_node.typ
			} else {
				t.concrete_node_type_name(rhs_node)
			})
			if rhs_typ == 0 {
				rhs_typ = t.node_type(children[1])
			}
			if !has_concrete_substituted_type && rhs_typ != 0
				&& !t.generic_arg_is_unresolved(rhs_typ) {
				cloned_typ = rhs_typ
				t.set_node_typ(int(children[0]), rhs_typ)
			}
			lhs_typ := (if has_concrete_substituted_type {
				cloned_typ
			} else if rhs_typ != 0 {
				rhs_typ
			} else {
				cloned_typ
			})
			if has_concrete_substituted_type {
				t.set_node_typ(int(children[0]), cloned_typ)
			}
			if lhs_typ != 0 && !t.generic_arg_is_unresolved(lhs_typ) {
				normalized_lhs_typ := (if flat.source_type_syntax(lhs_typ).main_fragment() {
					lhs_typ
				} else {
					t.normalize_type_alias(lhs_typ)
				})
				t.set_var_type_with_raw(lhs_clone.value_text(), normalized_lhs_typ, lhs_typ)
			}
		}
	}
	t.substitute_cloned_generic_call_type_args(node, mut children, args_ids)
	comptime_payload := if node.kind == .comptime_for {
		if substituted_node_type_id != node.typ {
			flat.canonical_comptime_type_payload
		} else {
			node.payload
		}
	} else {
		u32(0)
	}
	mut cloned_call_args := []u32{cap: node.call_type_args().len}
	for arg in node.call_type_args() {
		substituted_arg := t.subst_type(arg, args_ids)
		cloned_call_args << t.lock_colliding_main_substitution_type_text(arg, substituted_arg, t.cur_module, t.active_generic_params)
	}
	has_source_value := node.source_value != 0 || node.kind in [.struct_init, .assoc, .cast_expr,
		.as_expr, .is_expr, .map_init, .array_init, .array_literal, .sizeof_expr, .typeof_expr,
		.offsetof_expr]
	if t.cloning_comptime_for_depth > 0 {
		// Inside a `$for` body: clone verbatim, no generic-call retargeting.
		mut comptime_source_value := if has_source_value {
			t.subst_node_source_value(node, args_ids)
		} else {
			u32(0)
		}
		comptime_value := if has_source_value { '' } else { t.subst_node_value(node, args_ids) }
		// Lock a colliding caller type like the regular clone below: `T(v)` in a
		// `$for v in T.variants` body of json2 must keep a main `Any` distinct from
		// `json2.Any`.
		if node.kind in [.array_init, .map_init, .cast_expr, .as_expr]
			&& t.subst_type(node.source_value_id(), args_ids) != node.source_value_id() {
			comptime_source_value = t.lock_colliding_main_substitution_type_text(node.source_value_id(),
				comptime_source_value, t.cur_module, t.active_generic_params)
		}
		start2 := t.a.children.len
		for child in children {
			t.a.children << child
		}
		mut cloned_node := flat.Node{
			kind:           node.kind
			op:             cloned_op
			pos:            node.pos
			children_start: start2
			children_count: flat.child_count(children.len)
			typ:            cloned_typ
			value:          comptime_value
			source_value:   comptime_source_value
			payload:        comptime_payload
			is_mut:         node.is_mut
			flags:          flat.clone_node_flags(node, false)
		}
		if cloned_call_args.len > 0 {
			cloned_node.set_call_type_args(cloned_call_args)
		}
		clone_id := t.a.add_node(cloned_node)
		if node.kind == .ident && t.mut_param_values[node.value_text()] {
			t.mut_value_ident_nodes[int(clone_id)] = true
		}
		t.generic_clone_children = t.generic_clone_children[..scratch_start]
		return clone_id
	}
	if node.kind == .struct_init && children.len == 0 && t.is_optional_type_name(cloned_typ)
		&& (node.value_text() == '__v_option' || t.is_optional_type_name(node.source_value_id())) {
		children << t.make_sum_literal_field('ok', t.make_bool_literal(true), flat.source_type_bool)
	}
	cloned_typ = t.retarget_cloned_map_key_storage_type(node, mut children, cloned_typ)
	t.retarget_cloned_new_map_call(node, mut children, cloned_typ)
	t.retarget_cloned_fn_value_generic_call(node, mut children)
	static_assoc_typ_source_id := t.retarget_cloned_static_assoc_call(node, mut children, args_ids)
	static_assoc_typ_source_id_2 := static_assoc_typ_source_id

	retargeted_typ_source_id := t.retarget_cloned_generic_call(node, mut children, args_ids, return_context_id)
	retargeted_typ_source_id_2 := retargeted_typ_source_id

	final_typ := (if retargeted_typ_source_id != 0 {
		retargeted_typ_source_id_2
	} else if static_assoc_typ_source_id != 0 {
		static_assoc_typ_source_id_2
	} else {
		cloned_typ
	})
	if fn_value_id := t.retarget_cloned_generic_fn_value(node, children, final_typ) {
		t.generic_clone_children = t.generic_clone_children[..scratch_start]
		return fn_value_id
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	if node.kind == .call && children.len > 0 {
		call_args := node.call_type_args()
		type_source_id := if flat.source_type_shape(node.typ).has(.bracketed) {
			node.typ
		} else if node.value.len > 0 || node.source_value != 0 {
			node.source_value_id()
		} else if call_args.len == 1 {
			call_args[0]
		} else {
			u32(0)
		}
		if flat.source_type_shape(type_source_id).has(.bracketed) {
			syntax := flat.source_type_syntax(type_source_id)
			if syntax.generic_app_valid() {
				callee_id := children[0]
				mut callee := t.a.nodes[int(callee_id)]
				if callee.kind in [.ident, .selector] && flat.source_type_id(callee.value_text()) == flat.source_type_syntax(syntax.generic_app_base()).unqualified() {
					t.set_node_source_value(int(callee_id), t.subst_type(type_source_id, args_ids))
				}
			}
		}
	}
	mut cloned_source_value := if has_source_value {
		if node.kind == .struct_init && cloned_typ != 0 {
			cloned_typ
		} else {
			t.subst_node_source_value(node, args_ids)
		}
	} else {
		u32(0)
	}
	cloned_value := if has_source_value {
		''
	} else if is_root {
		specialized_generic_fn_value(node.value_text(), args_ids)
	} else {
		t.subst_node_value(node, args_ids)
	}
	if node.kind in [.array_init, .map_init, .cast_expr, .as_expr] {
		substituted_value := t.subst_type(node.source_value_id(), args_ids)
		if substituted_value != node.source_value_id() {
			cloned_source_value = t.lock_colliding_main_substitution_type_text(node.source_value_id(), cloned_source_value, t.cur_module, t.active_generic_params)
		}
	}
	if is_root && t.cur_module.len > 0 {
		t.a.add_node(flat.Node{
			kind:  .module_decl
			value: t.cur_module
		})
	}
	mut cloned_node := flat.Node{
		kind:           node.kind
		op:             cloned_op
		pos:            node.pos
		children_start: start
		children_count: flat.child_count(children.len)
		typ:            final_typ
		value:          cloned_value
		source_value:   cloned_source_value
		payload:        comptime_payload
		is_mut:         node.is_mut
		flags:          flat.clone_node_flags(node, false)
	}
	if cloned_call_args.len > 0 {
		cloned_node.set_call_type_args(cloned_call_args)
	}
	clone_id := t.a.add_node(cloned_node)
	if node.kind == .map_init && !isnil(t.tc) {
		map_type := (if flat.source_type_shape(cloned_source_value).has(.map_type) {
			cloned_source_value
		} else if flat.source_type_shape(final_typ).has(.map_type) {
			final_typ
		} else {
			t.infer_map_init_entry_type(t.a.nodes[int(clone_id)])
		})
		t.validate_concrete_map_key(map_type)
	}
	if node.kind in [.block, .index] {
		nominal_type_source_id := t.generic_clone_decl_alias_type(clone_id)
		if t.generic_type_text_contains_alias(nominal_type_source_id, t.cur_module) {
			t.set_node_typ(int(clone_id), nominal_type_source_id)
		}
	}
	if t.specialization_node_start >= 0 && node.kind == .decl_assign && children.len >= 2 {
		lhs := t.a.nodes[int(children[0])]
		if lhs.kind == .ident && lhs.value_text().len > 0 {
			t.specialization_decl_nodes_by_name[lhs.value_text()] << int(clone_id)
		}
	}
	if node.kind == .param && cloned_value.len > 0 && cloned_typ != 0 {
		mut scoped_param_type := cloned_typ
		if node.is_mut && node.op != .amp && flat.source_type_shape(scoped_param_type).has(.pointer) {
			// The specialized signature keeps `mut value T` as pointer storage, but
			// expressions in its body see the language-level `T`. Do not overwrite
			// the value scope seeded by emit_generic_fn_specialization with `&T`.
			scoped_param_type = t.trim_pointer_type(scoped_param_type)
		}
		t.set_var_type(cloned_value, scoped_param_type)
	}
	if node.kind == .ident && t.mut_param_values[node.value_text()] {
		t.mut_value_ident_nodes[int(clone_id)] = true
	}
	// A literal of a generic struct without its type arguments, `Box{ item: a }`,
	// has the ones that the types of its fields give it in this instance, as
	// `Box[A]{}` has the ones that the substitution gives it: the check of the
	// body cannot tell them from `a A`, and every later reader of the type of the
	// literal took the bare `Box` (see infer_bare_generic_struct_init_type).
	if node.kind == .struct_init && !flat.source_type_shape(cloned_source_value).has(.bracketed) {
		if inferred_source_id := t.infer_bare_generic_struct_init_type(t.a.nodes[int(clone_id)]) {
			inferred_source_id_2 := inferred_source_id
			t.set_node_source_value(int(clone_id), inferred_source_id_2)
			t.set_node_typ(int(clone_id), inferred_source_id_2)
		}
	}
	if node.kind == .call {
		// Retarget nested generic calls while the surrounding concrete arguments and
		// module context are still live. Deferring implicit calls to the global scan
		// lets cloned template types from an earlier specialization win, and requires
		// repeated whole-AST scans to discover the resulting late specializations.
		if retargeted_typ_source_id == 0 {
			t.retarget_cloned_implicit_generic_call(clone_id, node, args_ids)
		}
	}
	t.generic_clone_children = t.generic_clone_children[..scratch_start]
	return clone_id
}

fn (mut t Transformer) validate_concrete_map_key(map_type_id u32) {
	if isnil(t.tc) { return }
	key_type_source_id, _ := t.map_type_parts(map_type_id)

	if t.generic_arg_is_unresolved(key_type_source_id) { return }
	resolved_key := types.unalias_type(t.tc.parse_type(key_type_source_id))
	if resolved_key is types.Struct && resolved_key.source_type_id() != semantic_map_key_any_type_id
		&& (resolved_key.source_type_id() in t.tc.structs || resolved_key.source_type_id() in t.structs) {
		t.record_monomorph_error('map key type `${resolved_key.name()}` not supported')
	}
}

// Keep a call's nominal alias when the template annotation names its storage
// type. Concrete instance checking runs before normal declaration lowering can
// restore that alias, including after an Option/Result call is unwrapped.

// Keep an expression's nominal aliases when the template annotation names its
// storage type. Concrete instance checking precedes normal declaration lowering,
// so unsafe block tails, indexed elements and dereferenced containers need them too.
fn (t &Transformer) generic_clone_decl_alias_type(id flat.NodeId) u32 {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return u32(0)
	}
	node := t.a.nodes[int(id)]
	if node.kind == .call {
		return t.raw_call_decl_return_type(id, node) or { u32(0) }
	}
	if node.kind in [.paren, .expr_stmt] && node.children_count == 1 {
		return t.generic_clone_decl_alias_type(t.a.child(&node, 0))
	}
	if node.kind == .block && node.children_count > 0 {
		return t.generic_clone_decl_alias_type(t.a.child(&node, node.children_count - 1))
	}
	if node.kind == .prefix && node.op == .mul && node.children_count == 1 {
		pointer := t.generic_clone_decl_alias_type(t.a.child(&node, 0))
		if flat.source_type_shape(pointer).has(.pointer) {
			return flat.source_type_syntax(pointer).raw_base()
		}
	}
	if node.kind == .index && node.value != 'range' && node.children_count > 0
		&& !isnil(t.tc) {
		container_source_id := t.generic_clone_decl_alias_type(t.a.child(&node, 0))

		clean := types.unalias_type(types.unwrap_pointer(t.tc.parse_resolution_type(container_source_id)))
		match clean {
			types.Array, types.ArrayFixed {
				return t.semantic_type_name(clean.elem_type)
			}
			types.Map { return t.semantic_type_name(clean.value_type) }
			else {}
		}
	}
	if node.kind == .or_expr && node.children_count > 0 {
		wrapped := t.generic_clone_decl_alias_type(t.a.child(&node, 0))
		if flat.source_type_shape(wrapped).has(.optional) {
			return flat.source_type_syntax(wrapped).raw_base()
		}
	}
	if raw_source_id := t.raw_var_type_for_expr(id) {
		if t.generic_type_text_contains_alias(raw_source_id, t.cur_module) {
			return raw_source_id
		}
	}
	return u32(0)
}

fn comptime_pointer_type_binding(cond string) ?(string, string) {
	clean := comptime_condition_strip_outer_parens(trimmed_transform_text(cond))
	idx := comptime_condition_top_level_index(clean, ' is ')
	if idx < 0 {
		return none
	}
	actual := trimmed_transform_text(clean[..idx])
	expected := clean[idx + ' is '.len..].trim_space()
	if !actual.starts_with('&') || !expected.starts_with('&') {
		return none
	}
	binding := trimmed_transform_text(expected[1..])
	if !is_generic_fn_placeholder_name(binding) {
		return none
	}
	return binding, trimmed_transform_text(actual[1..])
}

fn (mut t Transformer) seed_cloned_generic_for_in_bindings(node flat.Node, key_id flat.NodeId, val_id flat.NodeId, container_id flat.NodeId) {
	if int(container_id) < 0 || int(container_id) >= t.a.nodes.len {
		return
	}
	raw_iter_type_id := t.generic_clone_decl_alias_type(container_id)
	iter_type := (if t.generic_type_text_contains_alias(raw_iter_type_id, t.cur_module) {
		t.trim_all_pointer_type(raw_iter_type_id)
	} else {
		t.normalized_node_type(container_id)
	})
	if iter_type == 0 || t.generic_arg_is_unresolved(iter_type) {
		return
	}
	map_type := (if flat.source_type_shape(iter_type).has(.map_type) {
		iter_type
	} else {
		t.clean_map_type(iter_type)
	})
	has_index := int(val_id) >= 0
	if has_index {
		if int(key_id) >= 0 {
			key := t.a.nodes[int(key_id)]
			mut key_type := flat.source_type_int
			if flat.source_type_shape(map_type).has(.map_type) {
				key_type = t.map_key_type(map_type)
			}
			t.set_node_typ(int(key_id), key_type)
			t.set_var_type(key.value, key_type)
		}
		if int(val_id) >= 0 {
			value := t.a.nodes[int(val_id)]
			mut value_type := if flat.source_type_shape(map_type).has(.map_type) {
				t.map_value_type(map_type)
			} else {
				t.infer_for_in_elem_type(iter_type, node)
			}
			if node.op == .amp && value_type != 0 {
				value_type = flat.source_type_composition_one(.pointer, value_type)
			}
			if value_type != 0 {
				t.set_node_typ(int(val_id), value_type)
				t.set_var_type(value.value, value_type)
			}
		}
		return
	}
	if int(key_id) >= 0 {
		value := t.a.nodes[int(key_id)]
		value_type := if flat.source_type_shape(map_type).has(.map_type) {
			t.map_value_type(map_type)
		} else {
			t.infer_for_in_elem_type(iter_type, node)
		}
		if value_type != 0 {
			t.set_node_typ(int(key_id), value_type)
			t.set_var_type(value.value, value_type)
		}
	}
}

fn (t &Transformer) generic_comptime_base_type(id flat.NodeId, args_ids []u32) ?u32 {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		if is_generic_fn_placeholder_name(node.value_text()) {
			idx := t.active_generic_param_index(node.value_text())
			if idx < args_ids.len {
				return args_ids[idx]
			}
		}
		node_value_id := node.source_value_id()
		substituted_id := t.subst_type(node_value_id, args_ids)
		if substituted_id != node_value_id && !t.generic_args_have_placeholders([substituted_id]) {
			return t.resolve_substituted_type_text(substituted_id)
		}
		typ_source_id := t.raw_var_type(node.value_text())

		if typ_source_id != 0 && !t.generic_arg_is_unresolved(typ_source_id) {
			return typ_source_id
		}
	}
	if node.kind == .typeof_expr {
		if typ := generic_type_name_from_marker(node.value_text()) {
			idx := t.active_generic_param_index(typ)
			if idx < args_ids.len {
				return args_ids[idx]
			}
		}
	}
	return none
}

fn (mut t Transformer) generic_comptime_type_expr_for_member(id flat.NodeId, member string, args_ids []u32) ?u32 {
	if member in ['idx', 'typ'] && int(id) >= 0 && int(id) < t.a.nodes.len {
		node := t.a.nodes[int(id)]
		if node.kind == .typeof_expr && node.children_count > 0 {
			child_id := t.a.child(&node, 0)
			child := t.a.nodes[int(child_id)]
			if child.kind == .ident && t.mut_param_values[child.value_text()] {
				return t.generic_comptime_base_type(child_id, args_ids)
			}
		}
	}
	return t.generic_comptime_type_expr(id, args_ids)
}

fn (mut t Transformer) generic_comptime_type_expr(id flat.NodeId, args_ids []u32) ?u32 {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .array_init && node.value_text() == '__v3_comptime_type_array'
		&& node.children_count == 1 {
		elem := t.generic_comptime_type_expr(t.a.child(&node, 0), args_ids) or { return none }
		return flat.source_type_composition_one(.array, elem)
	}
	if node.kind == .typeof_expr {
		return t.generic_comptime_typeof_target(node, args_ids)
	}
	if node.kind == .ident {
		return t.generic_comptime_base_type(id, args_ids)
	}
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	base_id := t.generic_comptime_type_expr(t.a.child(&node, 0), args_ids) or { return none }
	base_source_id := base_id
	return match node.value_text() {
		'typ' {
			base_id
		}
		'unaliased_typ' {
			t.comptime_typeof_unaliased_type(base_source_id)
		}
		'payload_type', 'pointee_type', 'element_type', 'key_type', 'value_type' {
			t.generic_comptime_type_member(base_source_id, node.value_text())?
		}
		else {
			none
		}
	}
}

// collect_generic_typeof_smartcasts records, for the clone-time `typeof` fold, the
// narrowings an if condition gives its then branch (or, with `else_branch`, its else
// branch): `x is T`, `x != none` and their negations. The types are the instance's.
fn (mut t Transformer) collect_generic_typeof_smartcasts(cond_id flat.NodeId, args_ids []u32, else_branch bool) {
	if int(cond_id) < 0 || int(cond_id) >= t.a.nodes.len {
		return
	}
	cond := t.a.nodes[int(cond_id)]
	if cond.kind == .paren && cond.children_count > 0 {
		t.collect_generic_typeof_smartcasts(t.a.child(&cond, 0), args_ids, else_branch)
		return
	}
	if else_branch {
		if cond.kind == .prefix && cond.op == .not && cond.children_count > 0 {
			inner_id := t.a.child(&cond, 0)
			if t.a.nodes[int(inner_id)].kind == .is_expr {
				t.record_generic_typeof_is_smartcast(inner_id, args_ids, false)
			}
		} else if cond.kind == .infix && cond.op == .eq && cond.children_count >= 2 {
			t.record_generic_typeof_none_smartcast(cond, args_ids)
		}
		return
	}
	if cond.kind == .is_expr {
		t.record_generic_typeof_is_smartcast(cond_id, args_ids, true)
	} else if cond.kind == .infix && cond.op == .ne && cond.children_count >= 2 {
		t.record_generic_typeof_none_smartcast(cond, args_ids)
	} else if cond.kind == .infix && cond.op == .logical_and && cond.children_count >= 2 {
		t.collect_generic_typeof_smartcasts(t.a.child(&cond, 0), args_ids, false)
		t.collect_generic_typeof_smartcasts(t.a.child(&cond, 1), args_ids, false)
	}
}

// generic_typeof_operand_type returns the instance type of a narrowed operand.
fn (t &Transformer) generic_typeof_operand_type(id flat.NodeId, args_ids []u32) u32 {
	if typ := t.generic_comptime_base_type(id, args_ids) {
		return typ
	}
	original := t.original_expr_type(id)
	if original == 0 {
		return 0
	}
	return t.resolve_substituted_type_text(t.subst_type(original, args_ids))
}

// record_generic_typeof_is_smartcast records the narrowing of `x is T`; an interface
// subject narrows only where `allow_interface` is set, as for non-generic code.
fn (mut t Transformer) record_generic_typeof_is_smartcast(is_id flat.NodeId, args_ids []u32, allow_interface bool) {
	is_node := t.a.nodes[int(is_id)]
	variant_source_id := is_node.source_value_id()
	if is_node.children_count == 0 || variant_source_id == 0 {
		return
	}
	expr_id := t.a.child(&is_node, 0)
	ek := t.expr_key(expr_id)
	if ek.len == 0 {
		return
	}
	expr_type := t.generic_typeof_operand_type(expr_id, args_ids)
	if expr_type == 0 {
		return
	}
	variant := t.resolve_substituted_type_text(t.subst_type(variant_source_id, args_ids))
	mut sum_type := t.sum_type_for_is_expr(expr_type, variant)
	if sum_type == 0 && allow_interface && t.is_interface_type_name(expr_type) {
		sum_type = expr_type
	}
	if sum_type == 0 {
		return
	}
	t.generic_typeof_smartcasts << SmartcastContext{
		type_texts:    t.var_type_texts
		expr_name:     ek
		variant_name:  variant
		sum_type_name: sum_type
	}
}

// record_generic_typeof_none_smartcast records the option unwrap of `x != none`
// (or `x == none` for an else branch).
fn (mut t Transformer) record_generic_typeof_none_smartcast(cond flat.Node, args_ids []u32) {
	lhs_id := t.a.child(&cond, 0)
	rhs_id := t.a.child(&cond, 1)
	mut opt_id := flat.empty_node
	if t.a.nodes[int(rhs_id)].kind in [.none_expr, .nil_literal] {
		opt_id = lhs_id
	} else if t.a.nodes[int(lhs_id)].kind in [.none_expr, .nil_literal] {
		opt_id = rhs_id
	}
	if int(opt_id) < 0 {
		return
	}
	ek := t.expr_key(opt_id)
	if ek.len == 0 {
		return
	}
	mut expr_type := t.generic_typeof_operand_type(opt_id, args_ids)
	if !t.is_optional_type_name(expr_type) && flat.source_type_shape(expr_type).has(.pointer) {
		// a `mut x ?T` parameter is stored indirectly
		expr_type = flat.source_type_syntax(expr_type).raw_base()
	}
	if !t.is_optional_type_name(expr_type) {
		return
	}
	base := t.optional_base_type(expr_type)
	if base == 0 || base == flat.source_type_void {
		return
	}
	t.generic_typeof_smartcasts << SmartcastContext{
		type_texts:    t.var_type_texts
		expr_name:     ek
		variant_name:  base
		sum_type_name: option_unwrap_marker_id
	}
}

// generic_typeof_smartcast returns the narrowed type `typeof(x)` observes inside a
// generic body cloned for an instance, mapped like the non-generic `typeof` fold.
fn (t &Transformer) generic_typeof_smartcast(node flat.Node) ?u32 {
	if t.generic_typeof_smartcasts.len == 0 || node.source_value_id() != 0
		|| node.children_count == 0 {
		return none
	}
	child_id := t.a.child(&node, 0)
	key := t.expr_key(child_id)
	if key.len == 0 {
		return none
	}
	for i := t.generic_typeof_smartcasts.len - 1; i >= 0; i-- {
		sc := t.generic_typeof_smartcasts[i]
		if sc.expr_name != key {
			continue
		}
		if sc.sum_type_name == option_unwrap_marker_id {
			child := t.a.nodes[int(child_id)]
			if child.kind == .ident && t.mut_param_values[child.value]
				&& !flat.source_type_shape(sc.variant_name).has(.pointer) {
				return flat.source_type_composition_one(.pointer, sc.variant_name)
			}
			return sc.variant_name
		}
		if t.is_interface_type_name(sc.sum_type_name) {
			return flat.source_type_composition_one(.pointer, t.interface_variant_type(sc.variant_name))
		}
		return t.resolve_variant(sc.sum_type_name, sc.variant_name)
	}
	return none
}

fn (mut t Transformer) generic_comptime_typeof_target(node flat.Node, args_ids []u32) ?u32 {
	if narrowed := t.generic_typeof_smartcast(node) {
		return narrowed
	}
	if typ := generic_type_name_from_marker(node.value_text()) {
		idx := t.active_generic_param_index(typ)
		if idx < args_ids.len {
			return args_ids[idx]
		}
	}
	if node.value_text().len > 0 && !node.value_text().starts_with(generic_type_name_marker_prefix)
		&& generic_text_contains_param(node.value_text(), t.active_generic_params) {
		return t.resolve_substituted_type_text(t.subst_type(node.source_value_id(), args_ids))
	}
	if node.children_count == 0 {
		return none
	}
	child_id := t.a.child(&node, 0)
	child := t.a.nodes[int(child_id)]
	if child.kind == .selector && child.children_count > 0
		&& child.value_text() in ['idx', 'key_type', 'value_type', 'element_type'] {
		base_id := t.a.child(&child, 0)
		if concrete_source_id := t.generic_comptime_type_expr(base_id, args_ids) {
			if child.value_text() == 'idx' {
				return flat.source_type_int
			}
			return t.generic_comptime_type_member(concrete_source_id, child.value_text())
		}
	}
	target_source_id := t.generic_comptime_base_type(child_id, args_ids) or { return none }
	target_shape := flat.source_type_shape(target_source_id)
	if child.kind == .ident && t.mut_param_values[child.value_text()] && !target_shape.has(.pointer) {
		// A `mut value T` parameter is stored indirectly. `typeof(value)` observes
		// that pointer shape even though ordinary value uses infer T itself
		// (`?&T` for a `mut value ?T`, see mut_param_typeof_type).
		return mut_param_typeof_type(target_source_id, target_shape.has(.option_type))
	}
	return target_source_id
}

// generic_comptime_typeof_display_name retains the writing file's spelling for
// a composite type expression, while a direct parameter keeps its caller's name.
fn (t &Transformer) generic_comptime_typeof_display_name(node flat.Node, args_ids []u32, reflected string) string {
	if _ := t.generic_typeof_smartcast(node) {
		return generic_type_name_display(reflected)
	}
	if node.value_text().len > 0 && !node.value_text().starts_with(generic_type_name_marker_prefix)
		&& node.value_text() !in t.active_generic_params {
		return generic_type_name_display(flat.source_type_name(t.subst_type(node.source_value_id(), args_ids)))
	}
	if node.children_count > 0 {
		child := t.a.child_node(&node, 0)
		if child.kind == .ident && child.value_text() !in t.active_generic_params
			&& generic_text_contains_param(child.value_text(), t.active_generic_params)
			&& (t.raw_var_type(child.value_text())) == 0 {
			return generic_type_name_display(flat.source_type_name(t.subst_type(child.source_value_id(), args_ids)))
		}
	}
	return generic_type_name_display(reflected)
}

fn (mut t Transformer) generic_comptime_type_member(raw_id u32, member string) ?u32 {
	clean := t.comptime_normalize_type_alias_chain(raw_id)
	shape := flat.source_type_shape(clean)
	match member {
		'payload_type' {
			if shape.has(.optional) {
				return t.trimmed_type_id(flat.source_type_syntax(clean).raw_base())
			}
		}
		'pointee_type' {
			pointee := if shape.has(.optional) {
				t.trimmed_type_id(flat.source_type_syntax(clean).raw_base())
			} else {
				clean
			}
			if flat.source_type_shape(pointee).has(.pointer) {
				return t.trimmed_type_id(t.trim_pointer_type(pointee))
			}
		}
		'key_type', 'value_type' {
			if !shape.has(.map_type) {
				return none
			}
			key_type, value_type := t.map_type_parts(clean)
			return if member == 'key_type' { key_type } else { value_type }
		}
		'element_type' {
			if shape.has(.array_type) {
				return t.array_elem_type(clean)
			}
			if t.is_fixed_array_type(clean) {
				return fixed_array_elem_type(clean)
			}
		}
		else {}
	}
	return none
}

fn (mut t Transformer) substitute_cloned_generic_call_type_args(node flat.Node, mut children []flat.NodeId, args_ids []u32) {
	if node.kind != .index || node.children_count < 2 || node.value == 'range'
		|| children.len < int(node.children_count) || t.index_callee_is_value_index(node) {
		return
	}
	for i in 1 .. int(node.children_count) {
		arg_source_id_10 := t.generic_call_type_arg_name(t.a.child(&node, i))
		if arg_source_id_10 == 0 {
			continue
		}
		// Only rewrite args that actually contain a generic placeholder. A plain
		// value index like `attr[start]` matches the `callee[arg]` shape too, and
		// resolve_substituted_type_text would "qualify" the local `start` into a
		// phantom type/fn name (`json2.start`), corrupting the index expression.
		// A generic parameter is a placeholder even when the writing file also
		// imports a type with the same spelling: `import pkg { T }` in a generic
		// `fn outer[T]` makes the text `T` look like a concrete `pkg.T` to the
		// declared-type tables, but the lexical parameter has precedence.
		if !t.generic_args_have_placeholders([arg_source_id_10])
			&& !generic_id_contains_param(arg_source_id_10, t.active_generic_params) {
			continue
		}
		concrete := t.subst_type(arg_source_id_10, args_ids)
		locked := t.lock_colliding_main_substitution_type_text(arg_source_id_10, concrete, t.cur_module, t.active_generic_params)
		substituted := t.resolve_substituted_type_text(locked)
		if substituted == 0 || substituted == arg_source_id_10
			|| t.generic_args_have_placeholders([substituted]) {
			continue
		}
		children[i] = t.make_ident(flat.source_type_name(substituted))
	}
}

const generic_type_name_marker_prefix = '__v3_generic_type_name:'

fn generic_type_name_marker(typ string) string {
	return generic_type_name_marker_prefix + typ
}

fn generic_type_name_from_marker(value string) ?string {
	if value.starts_with(generic_type_name_marker_prefix) {
		return value[generic_type_name_marker_prefix.len..]
	}
	return none
}

fn generic_type_name_display(typ string) string {
	clean := trimmed_transform_text(typ)
	unlocked := type_text_without_main_locks_bytes(clean)
	if unlocked != clean {
		return generic_type_name_display(unlocked)
	}
	if !typ.starts_with('fn(') {
		return typ
	}
	close := generic_fn_type_params_close(typ) or { return 'fn ' + typ[2..] }
	params := split_type_display_params(typ[3..close])
	mut displayed_params := []string{cap: params.len}
	for param in params {
		displayed_params << generic_fn_param_type_display(param)
	}
	return 'fn (' + displayed_params.join(', ') + ')' + typ[close + 1..]
}

fn type_text_without_main_locks(type_id u32) u32 {
	return flat.source_type_syntax(type_id).main_unlocked()
}

fn type_text_without_main_locks_bytes(typ string) string {
	return flat.source_type_name(type_text_without_main_locks(flat.source_type_id(typ)))
}

fn generic_fn_param_type_display(typ string) string {
	clean := trimmed_transform_text(typ)
	if clean.starts_with('&') {
		return 'mut ' + generic_type_name_display(clean[1..])
	}
	return generic_type_name_display(clean)
}

fn generic_fn_type_params_close(typ string) ?int {
	mut depth := 0
	for i in 2 .. typ.len {
		if typ[i] == `(` {
			depth++
		} else if typ[i] == `)` {
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return none
}

fn split_type_display_params(s string) []string {
	mut parts := []string{}
	mut paren_depth := 0
	mut bracket_depth := 0
	mut start := 0
	for i in 0 .. s.len {
		match s[i] {
			`(` {
				paren_depth++
			}
			`)` {
				paren_depth--
			}
			`[` {
				bracket_depth++
			}
			`]` {
				bracket_depth--
			}
			`,` {
				if paren_depth == 0 && bracket_depth == 0 {
					parts << trimmed_transform_text(s[start..i])
					start = i + 1
				}
			}
			else {}
		}
	}
	tail := trimmed_transform_text(s[start..])
	if tail.len > 0 {
		parts << tail
	}
	return parts
}

fn (mut t Transformer) retarget_cloned_new_map_call(node flat.Node, mut children []flat.NodeId, map_type_id u32) {
	map_type_shape := flat.source_type_shape(map_type_id)
	if node.kind != .call || children.len < 7 || !map_type_shape.has(.map_type) {
		return
	}
	callee := t.a.nodes[int(children[0])]
	if callee.kind != .ident || callee.value_text() != 'new_map' {
		return
	}
	key_type_source_id_3, _ := t.map_type_parts(map_type_id)

	if key_type_source_id_3 == 0 {
		return
	}
	key_storage_type_source_id := t.map_key_storage_type(key_type_source_id_3)

	children[1] = t.make_sizeof_type(key_storage_type_source_id)
	hash_fn, eq_fn, clone_fn, free_fn := t.map_callback_names_for_type(key_storage_type_source_id)
	children[3] = t.make_ident(hash_fn)
	children[4] = t.make_ident(eq_fn)
	children[5] = t.make_ident(clone_fn)
	children[6] = t.make_ident(free_fn)
}

// retarget_cloned_fn_value_generic_call rewrites a cloned `cb[T](args)` or
// `h.cb[T](args)` to `cb(args)`/`h.cb(args)` when the callee is a fn value (for
// example a `GenericFn[T]` parameter or struct field). The specialization already
// gives the value its concrete fn type, so the type arguments select nothing.
// Cloned nodes have no checker types, and `!`/`?`/`or` lowering types the call
// before `normalize_generic_call_expr` would drop the index, so an index callee
// there hides the fn value's return type.
fn (mut t Transformer) retarget_cloned_fn_value_generic_call(node flat.Node, mut children []flat.NodeId) {
	if node.kind != .call || children.len == 0 {
		return
	}
	callee := t.a.nodes[int(children[0])]
	if callee.kind != .index || callee.children_count < 2 || callee.value == 'range' {
		return
	}
	base_id := t.a.child(&callee, 0)
	base := t.a.nodes[int(base_id)]
	is_fn_field := base.kind == .selector && base.children_count > 0
		&& t.receiver_selector_is_fn_field(t.trim_pointer_type(t.node_type(t.a.child(&base, 0))), base.value_text())
	if is_fn_field || t.generic_call_base_is_fn_value(base_id, base) {
		children[0] = base_id
	}
}

// unwrap_cloned_if_guard_binding gives the binding of a cloned `if v := opt` guard the
// payload type of the option. Cloning the declaration recorded the type of its value, so
// `typeof(v)` in the guarded branch, which the clone resolves, named the option (`?T`).
fn (mut t Transformer) unwrap_cloned_if_guard_binding(cond_id flat.NodeId) {
	if int(cond_id) < 0 || int(cond_id) >= t.a.nodes.len {
		return
	}
	cond := t.a.nodes[int(cond_id)]
	if cond.kind != .decl_assign || cond.children_count != 2 {
		return
	}
	lhs_id := t.a.child(&cond, 0)
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value_text().len == 0 || lhs.value_text() == '_' {
		return
	}
	typ := t.raw_var_type(lhs.value_text())
	if typ == 0 || !t.is_optional_type_name(typ) {
		return
	}
	payload := t.optional_base_type(typ)
	if payload == 0 || payload == flat.source_type_void || t.generic_arg_is_unresolved(payload) {
		return
	}
	normalized := if flat.source_type_syntax(payload).main_fragment() {
		payload
	} else {
		t.normalize_type_alias(payload)
	}
	t.set_var_type_with_raw(lhs.value_text(), normalized, payload)
	t.set_node_typ(int(lhs_id), payload)
}

fn (mut t Transformer) retarget_cloned_map_key_storage_type(node flat.Node, mut children []flat.NodeId, cloned_typ_id u32) u32 {
	cloned_typ_comparison_type_id := cloned_typ_id

	if cloned_typ_id == u32(0) {
		return cloned_typ_id
	}
	mut name := ''
	if node.kind == .ident {
		name = node.value_text()
	} else if node.kind == .decl_assign && children.len > 0 {
		lhs := t.a.nodes[int(children[0])]
		if lhs.kind == .ident {
			name = lhs.value_text()
		}
	}
	if !is_lowered_map_key_temp_name(name) {
		return cloned_typ_id
	}
	key_storage_type_source_id_2 := t.map_key_storage_type(cloned_typ_id)
	key_storage_type_comparison_type_id := key_storage_type_source_id_2

	if key_storage_type_source_id_2 == 0 || key_storage_type_comparison_type_id == cloned_typ_comparison_type_id {
		return cloned_typ_id
	}
	if node.kind == .decl_assign && children.len > 0 {
		t.set_node_typ(int(children[0]), key_storage_type_source_id_2)
	}
	t.set_var_type(name, key_storage_type_source_id_2)
	return key_storage_type_source_id_2
}

fn is_lowered_map_key_temp_name(name string) bool {
	return name.starts_with('__map_key_') || name.starts_with('__map_eq_key_')
}

// hold_receiver_type_args has a check of the instances report the explicit type
// arguments `call_args` of the cloned call `node` of the method `decl` (for
// `param_names`, in their order) that give a type parameter the method repeats
// from its receiver another type than the cloned receiver fixes, and sets them
// to that type: `b.own[int]()` in the instance of a generic function where `b`
// is a `Box[string]` (see types.TypeChecker.explicit_receiver_type_args).
// `callee_id` is the cloned callee. A build leaves them as they are.
fn (mut t Transformer) hold_receiver_type_args(decl GenericFnDecl, callee_id flat.NodeId, node flat.Node, param_names []string, mut call_args []u32) {
	if isnil(t.tc) || !t.tc.check_concrete_generic_bodies || call_args.len != param_names.len
		|| int(callee_id) < 0 || int(callee_id) >= t.a.nodes.len {
		return
	}
	receiver_params := t.generic_receiver_param_names(decl)
	if !param_names.any(it in receiver_params) { return }
	mut callee := t.a.nodes[int(callee_id)]
	if callee.kind == .index && callee.children_count > 0 && callee.value != 'range' {
		callee = t.a.nodes[int(t.a.child(&callee, 0))]
	}
	if callee.kind != .selector || callee.children_count == 0 { return }
	receiver_type_id := t.generic_call_arg_type_for_inference(t.a.child(&callee, 0))
	if receiver_type_id == 0 || t.generic_arg_is_unresolved(receiver_type_id) { return }
	fixed := t.receiver_fixed_type_args(decl, receiver_type_id)
	for i, param in param_names {
		if param !in receiver_params { continue }
		receiver_arg_id := fixed[param] or { continue }
		if receiver_arg_id == call_args[i] { continue }
		expected_source_id := t.tc.parse_type(receiver_arg_id).source_type_id()
		given_source_id := t.tc.parse_type(call_args[i]).source_type_id()
		if expected_source_id == given_source_id { continue }
		expected := flat.source_type_name(expected_source_id)
		given := flat.source_type_name(given_source_id)
		t.tc.report_receiver_type_arg_mismatch(t.cur_file, node.pos, decl.node.value_text().all_after_last('.'),
			param, t.tc.parse_type(receiver_type_id).name().trim_left('&'), expected, given)
		call_args[i] = receiver_arg_id
	}
}

// receiver_fixed_type_args returns the types that a receiver of the type
// `receiver_type` gives the type parameters of the receiver of the method
// `decl`: `T` is `string` for a `Box[string]` and `fn (b Box[T]) own[T]() T`,
// also through an embedded `Box[string]`.
fn (mut t Transformer) receiver_fixed_type_args(decl GenericFnDecl, receiver_type_id u32) map[string]u32 {
	mut fixed := map[string]u32{}
	for i in 0 .. decl.node.children_count {
		receiver_param := t.a.child_node(&decl.node, i)
		if receiver_param.kind != .param {
			continue
		}
		param_type := generic_inference_param_type(receiver_param)
		arg_type := generic_arg_type_for_param(param_type, receiver_type_id)
		infer_generic_type_args(param_type, arg_type, mut fixed)
		t.infer_generic_embedded_receiver_args(receiver_param.typ, arg_type, mut fixed)
		break
	}
	return fixed
}

fn (mut t Transformer) retarget_cloned_generic_call(node flat.Node, mut children []flat.NodeId, args_ids []u32, return_context_id u32) u32 {
	if node.kind != .call || children.len == 0 || t.skip_generics {
		return u32(0)
	}
	decls := t.cached_generic_fn_decls()
	if decls.len == 0 {
		return u32(0)
	}
	decl_key := t.generic_call_decl_key(flat.empty_node, node, t.cur_module, decls) or { return u32(0) }
	decl := decls[decl_key] or { return u32(0) }
	is_receiver := t.generic_decl_is_receiver_method(decl.node)
		&& !t.generic_call_is_static_assoc_selector(node, decl)
	param_names := t.generic_fn_param_names(decl.node, decl.module)
	if param_names.len == 0 {
		return u32(0)
	}
	mut call_args := []u32{}
	if explicit_source_ids := t.explicit_generic_call_args(node, t.cur_module) {
		if is_receiver && explicit_source_ids.len < param_names.len {
			// A partial method list pins only method-level parameters; the receiver's
			// generic arguments must be inferred from the fully cloned receiver below.
			return u32(0)
		}
		for arg_source_id in explicit_source_ids {
			call_args << t.resolve_substituted_type_text(t.subst_type(arg_source_id, args_ids))
		}
		// A full list spells the receiver's parameters too: the cloned receiver
		// fixes them.
		if is_receiver {
			t.hold_receiver_type_args(decl, children[0], node, param_names, mut call_args)
		}
	} else {
		callee_id := children[0]
		if int(callee_id) < 0 || int(callee_id) >= t.a.nodes.len {
			return u32(0)
		}
		callee := t.a.nodes[int(callee_id)]
		mut receiver_id := flat.empty_node
		if is_receiver {
			if callee.kind != .selector || callee.children_count == 0 {
				return u32(0)
			}
			receiver_id = t.a.child(&callee, 0)
		} else {
			if callee.kind != .ident || callee.value_text().contains('[') {
				return u32(0)
			}
			if t.plain_concrete_callee_shadows_decl(callee.value_text(), decl) {
				return u32(0)
			}
		}
		mut inferred := map[string]u32{}
		mut param_idx := 0
		param_count := t.generic_decl_param_count(decl.node)
		for i in 0 .. decl.node.children_count {
			child := t.a.child_node(&decl.node, i)
			if child.kind != .param {
				if t.prefix_param_scan {
					break
				}
				continue
			}
			arg_pos := if is_receiver { param_idx } else { param_idx + 1 }
			mut arg_id := flat.empty_node
			if is_receiver && param_idx == 0 {
				arg_id = receiver_id
			} else if arg_pos < children.len {
				arg_id = children[arg_pos]
			} else {
				param_idx++
				continue
			}
			if generic_param_text_is_variadic(child.typ) && param_idx == param_count - 1 {
				elem_param_type := generic_variadic_elem_param_text(child.typ)
				for tail_id in children[arg_pos..] {
					t.infer_generic_variadic_call_arg(child.typ, elem_param_type, tail_id, mut inferred)
				}
				param_idx++
				continue
			}
			inference_param_type := generic_inference_param_type(child)
			arg_node := t.a.nodes[int(arg_id)]
			mut raw_arg_type := t.generic_call_arg_type_for_inference(arg_id)

			value_arg_id := if arg_node.kind == .prefix && arg_node.op == .amp
				&& arg_node.children_count > 0 {
				t.a.child(&arg_node, 0)
			} else {
				arg_id
			}
			value_arg := t.a.nodes[int(value_arg_id)]
			is_forwarded_mut_value := value_arg.kind == .ident
				&& t.mut_value_ident_nodes[int(value_arg_id)]
			if is_receiver && param_idx == 0 && flat.source_type_shape(raw_arg_type).has(.pointer)
				&& !flat.source_type_shape(child.typ).has(.pointer) && !flat.source_type_shape(child.typ).has(.mutable_type) {
				// A by-value receiver auto-dereferences the captured pointer. Infer
				// its inner arguments from `Box[int]`, rather than `&Box[int]`.
				raw_arg_type = flat.source_type_syntax(raw_arg_type).raw_base()
			}
			if (child.is_mut || flat.source_type_shape(child.typ).has(.mutable_type)) && flat.source_type_shape(raw_arg_type).has(.pointer)
				&& !is_forwarded_mut_value {
				// An ordinary pointer local passed to `mut T` has V's source-level
				// auto-dereferenced value semantics. A forwarded `mut T` parameter is
				// already represented by its semantic type and must keep a genuine `&T`.
				raw_arg_type = flat.source_type_syntax(raw_arg_type).raw_base()
			}
			if !flat.source_type_query(inference_param_type).has(.generic_fn_name) {
				// Preserve aliases nested in a concrete generic receiver. Resolving
				// `LinkedList[Distance]` as a whole erases `Distance` to `int`, so a
				// nested method call is retargeted to the incompatible
				// `LinkedList[int]` specialization. A direct alias still has to be
				// expanded so composite parameters such as `[]T` can infer from it.
				raw_arg_type = t.generic_composite_inference_alias_type(raw_arg_type, t.cur_module)
			}
			arg_type := generic_arg_type_for_param(inference_param_type, raw_arg_type)
			if arg_type != 0 {
				infer_generic_type_args(inference_param_type, arg_type, mut inferred)
			}
			param_idx++
		}
		if preserved_source_ids := t.generic_call_args_preserving_explicit(callee, decl, inferred) {

			// The callee already names the specialization picked by an explicit
			// `fn_name[Type]` list. Every position that list fixed must survive; neither the
			// enclosing return type nor the enclosing generic arguments may replace it.
			if t.callee_matches_generic_args(callee, decl, preserved_source_ids) {
				return u32(0)
			}
			call_args = preserved_source_ids.clone()
		} else {
			if inferred.len < param_names.len && return_context_id != 0 {
				mut return_inferred := map[string]u32{}
				mut expected_return := t.generic_inference_expected_type(return_context_id)
				for flat.source_type_shape(expected_return).has(.option_type) || flat.source_type_shape(expected_return).has(.result_type) {
					expected_return = t.trimmed_type_id(flat.source_type_syntax(expected_return).raw_base())
				}
				declared_raw := t.trimmed_type_id(decl.node.typ)
				mut declared_return := declared_raw
				for flat.source_type_shape(declared_return).has(.optional) {
					declared_return = t.trimmed_type_id(flat.source_type_syntax(declared_return).raw_base())
				}
				// An enclosing tuple result cannot infer a scalar method's receiver.
				// A scalar contextual result may infer method-level parameters, but a
				// receiver parameter must come from the actual receiver type.
				if !flat.source_type_syntax(expected_return).paren_prefix() || flat.source_type_syntax(declared_return).paren_prefix() {
					inference_return := if flat.source_type_shape(declared_raw).has(.result_type) {
						flat.source_type_composition_one(.result, expected_return)
					} else if flat.source_type_shape(declared_raw).has(.option_type) {
						flat.source_type_composition_one(.option, expected_return)
					} else {
						expected_return
					}
					receiver_params := if is_receiver {
						t.generic_receiver_param_names(decl)
					} else {
						[]string{}
					}
					t.infer_generic_return_type_args(decl, inference_return, mut return_inferred,
						receiver_params)
				}
				for name, inferred_type_source_id in return_inferred {
					if name !in inferred {
						inferred[name] = inferred_type_source_id
					}
				}
			}
			for name in param_names {
				arg_source_id_2 := inferred[name] or {
					idx := t.active_generic_param_index(name)
					if idx < 0 || idx >= args_ids.len {
						return u32(0)
					}
					args_ids[idx]
				}

				call_args << t.inherited_generic_arg_for_decl_module(arg_source_id_2, decl.module, args_ids)
			}
		}
	}
	if call_args.len == 0 || t.generic_args_have_placeholders(call_args) {
		return u32(0)
	}
	concrete_call_args_source_ids := t.canonical_generic_specialization_args(call_args)

	if !t.generic_specialization_registered(decl, concrete_call_args_source_ids)
		&& !t.generic_specialization_in_progress(decl, concrete_call_args_source_ids) {
		if t.defer_nested_generic_emissions {
			t.request_generic_fn_specialization(decl, concrete_call_args_source_ids)
		} else {
			t.generated_fn_used_names(decl, t.emit_generic_fn_specialization(decl, concrete_call_args_source_ids), concrete_call_args_source_ids)
		}
	}
	spec_value := specialized_generic_fn_value(decl.node.value_text(), concrete_call_args_source_ids)
	qualified_spec := transform_qualified_fn_name(decl.module, spec_value)
	t.record_generic_specialization_args_for_names([
		spec_value,
		qualified_spec,
		c_name(spec_value),
		c_name(qualified_spec),
	], concrete_call_args_source_ids)
	param_types_source_ids := t.specialized_generic_call_param_type_texts(decl, concrete_call_args_source_ids)

	if is_receiver {
		mut callee := t.a.nodes[int(children[0])]
		if callee.kind == .index && callee.children_count > 0 && callee.value != 'range' {
			callee = t.a.nodes[int(t.a.child(&callee, 0))]
		}
		if callee.kind != .selector || callee.children_count == 0 {
			return u32(0)
		}
		mut receiver := t.a.child(&callee, 0)
		if param_types_source_ids.len > 0 {
			base_type_source_id := t.generic_call_arg_type_for_inference(receiver)

			mut expected_type := t.trimmed_type_id(param_types_source_ids[0])
			if flat.source_type_shape(expected_type).has(.mutable_type) {
				expected_type = t.trimmed_type_id(flat.source_type_syntax(expected_type).raw_base())
			}
			expected_type = t.trimmed_type_id(flat.source_type_syntax(expected_type).pointer_raw_base())
			if expected_type != 0 {
				if embedded := t.embedded_receiver_base_for_type(receiver, base_type_source_id,
					expected_type)
				{
					receiver = embedded
				}
			}
		}
		children[0] = t.make_ident(qualified_spec)
		children.insert(1, t.specialized_method_receiver_arg(receiver, decl))
	} else {
		children[0] = t.make_ident(qualified_spec)
	}
	t.append_missing_specialized_params_struct_args(mut children, param_types_source_ids.clone(), decl.module)
	return t.specialized_fn_return_type_text(decl, concrete_call_args_source_ids)
}

fn (t &Transformer) generic_composite_inference_alias_type(typ_id u32, module_name string) u32 {
	if !t.generic_arg_is_alias_name_id(typ_id, module_name)
		&& t.generic_type_text_contains_alias(typ_id, module_name) {
		return t.generic_arg_for_call_and_decl_module(typ_id, module_name, module_name)
	}
	return t.generic_inference_alias_target(typ_id, module_name)
}

fn (mut t Transformer) retarget_cloned_generic_fn_value(node flat.Node, children []flat.NodeId, final_typ_id u32) ?flat.NodeId {
	if node.kind != .index || node.value == 'range' || children.len < 2 || t.skip_generics {
		return none
	}
	base := t.a.nodes[int(children[0])]
	if base.kind != .ident || (t.var_type(base.value_text())) != 0 {
		return none
	}
	decls := t.cached_generic_fn_decls()
	if decls.len == 0 {
		return none
	}
	mut call_args := []u32{}
	for i in 1 .. children.len {
		arg := t.generic_call_type_arg_name(children[i])
		if arg == 0 {
			return none
		}
		resolved := t.resolve_substituted_type_text(arg)
		call_args << resolved
	}
	if call_args.len == 0 || t.generic_args_have_placeholders(call_args) {
		return none
	}
	for candidate in t.generic_plain_call_candidates(base.value_text(), t.cur_module) {
		decl := decls[candidate] or { continue }
		param_names := t.generic_fn_param_names(decl.node, decl.module)
		if param_names.len != call_args.len
			|| t.plain_concrete_callee_shadows_decl(base.value_text(), decl) {
			continue
		}
		mut concrete_args := []u32{cap: call_args.len}
		for arg_source_id in call_args {
			concrete_args << t.generic_arg_for_decl_module(arg_source_id, decl.module)
		}
		concrete_args = t.canonical_generic_specialization_args(concrete_args).clone()
		if t.generic_args_have_placeholders(concrete_args) {
			return none
		}
		if !t.generic_specialization_registered(decl, concrete_args)
			&& !t.generic_specialization_in_progress(decl, concrete_args) {
			if t.defer_nested_generic_emissions {
				t.request_generic_fn_specialization(decl, concrete_args)
			} else {
				t.generated_fn_used_names(decl, t.emit_generic_fn_specialization(decl, concrete_args), concrete_args)
			}
		}
		spec_value := specialized_generic_fn_value(decl.node.value_text(), concrete_args)
		qualified_spec := transform_qualified_fn_name(decl.module, spec_value)
		id := t.make_ident(qualified_spec)
		if final_typ_id != u32(0) {
			t.set_node_typ(int(id), final_typ_id)
		}
		return id
	}
	return none
}

fn (mut t Transformer) retarget_cloned_static_assoc_call(node flat.Node, mut children []flat.NodeId, args_ids []u32) u32 {
	if node.kind != .call || node.children_count == 0 || children.len == 0 {
		return u32(0)
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .selector || callee.children_count == 0 {
		return u32(0)
	}
	base_id := t.a.child(callee, 0)
	base := t.a.nodes[int(base_id)]
	if base.kind != .ident || !is_generic_fn_placeholder_name(base.value_text()) {
		return u32(0)
	}
	idx := t.active_generic_param_index(base.value_text())
	if idx < 0 || idx >= args_ids.len {
		return u32(0)
	}
	owner := t.resolve_substituted_type_text(t.subst_type(args_ids[idx], args_ids))
	for type_name in t.static_assoc_type_candidates(owner) {
		static_fn := flat.encode_static_type_method_name(flat.source_type_name(type_name), callee.value_text())
		if t.is_known_fn_name(static_fn) {
			children[0] = t.make_ident(static_fn)
			return t.receiver_method_return_type(static_fn, t.subst_type(node.typ, args_ids))
		}
	}
	return u32(0)
}

fn (mut t Transformer) retarget_cloned_implicit_generic_call(clone_id flat.NodeId, source flat.Node, active_args_ids []u32) {
	if source.kind != .call || t.skip_generics || int(clone_id) < 0
		|| int(clone_id) >= t.a.nodes.len {
		return
	}
	clone := t.a.nodes[int(clone_id)]
	if clone.children_count == 0 {
		return
	}
	decls := t.cached_generic_fn_decls()
	if decls.len == 0 {
		return
	}
	decl_key := t.generic_call_decl_key(flat.empty_node, source, t.cur_module, decls) or { return }
	decl := decls[decl_key] or { return }
	is_receiver := t.generic_decl_is_receiver_method(decl.node)
		&& !t.generic_call_is_static_assoc_selector(source, decl)
	source_has_explicit_args := t.call_has_source_generic_args(source)
	if source_has_explicit_args && !is_receiver {
		return
	}
	// Re-infer receiver-only parameters too. Calls inside a cloned generic closure
	// may be the first concrete use of that method, so no receiver specialization
	// necessarily exists yet to retarget the call.
	mut callee_id := t.a.child(&clone, 0)
	if int(callee_id) < 0 || int(callee_id) >= t.a.nodes.len {
		return
	}
	mut callee := t.a.nodes[int(callee_id)]
	if callee.kind == .index && callee.children_count > 0 && callee.value != 'range' {
		callee_id = t.a.child(&callee, 0)
		callee = t.a.nodes[int(callee_id)]
	}
	// The shared template node may already carry the exact callee selected while
	// transforming another specialization. Keep going: the cloned argument types
	// below are authoritative and can retarget that stale specialized callee.
	if callee.kind !in [.ident, .selector] {
		return
	}
	param_names := t.generic_fn_param_names(decl.node, decl.module)
	if param_names.len == 0 {
		return
	}
	mut call_args := []u32{cap: param_names.len}
	if source_has_explicit_args {
		explicit := t.explicit_generic_call_args(source, t.cur_module) or { return }
		mut concrete_explicit := []u32{cap: explicit.len}
		for arg in explicit {
			substituted := t.resolve_substituted_type_text(t.subst_type(arg, active_args_ids))
			concrete_explicit << t.inherited_generic_arg_for_decl_module(substituted, decl.module, active_args_ids)
		}
		call_args = (t.infer_generic_call_args_with_explicit(decl, clone_id, clone, t.cur_module, concrete_explicit) or { return }).clone()
	} else {
		mut inferred := map[string]u32{}
		receiver_params := if is_receiver {
			t.generic_receiver_param_names(decl)
		} else {
			[]string{}
		}
		mut param_idx := 0
		param_count := t.generic_decl_param_count(decl.node)
		for i in 0 .. decl.node.children_count {
			child := t.a.child_node(&decl.node, i)
			if child.kind != .param {
				if t.prefix_param_scan {
					break
				}
				continue
			}
			if generic_param_text_is_variadic(child.typ) && param_idx == param_count - 1 {
				elem_param_type := generic_variadic_elem_param_text(child.typ)
				for arg_id in t.generic_call_arg_ids_from_param(clone, param_idx, is_receiver) {
					t.infer_generic_variadic_call_arg(child.typ, elem_param_type, arg_id, mut inferred)
				}
				param_idx++
				continue
			}
			clone_arg_id := t.generic_call_arg_id_for_param(clone, param_idx, is_receiver) or {
				param_idx++
				continue
			}
			source_arg_id := t.generic_call_arg_id_for_param(source, param_idx, is_receiver) or {
				param_idx++
				continue
			}
			mut arg_type := t.generic_call_arg_type_for_inference(clone_arg_id)
			clone_arg := t.a.nodes[int(clone_arg_id)]
			if clone_arg.kind == .ident {
				scoped_type_source_id := t.raw_var_type(clone_arg.value_text())
				scoped_type_source_id_2 := scoped_type_source_id
				scoped_type_source_id_3 := scoped_type_source_id_2

				if decl_type_is_usable(scoped_type_source_id_2) && !t.generic_arg_is_unresolved(scoped_type_source_id) {
					arg_type = scoped_type_source_id_3
				}
			}
			source_raw_type_source_id := t.generic_call_arg_type_for_inference(source_arg_id)
			source_substituted_source_id := t.subst_type(source_raw_type_source_id, active_args_ids)
			source_has_alias := t.generic_type_text_contains_alias(source_substituted_source_id, t.cur_module)
			source_depends_on_active := generic_id_contains_param(source_raw_type_source_id, t.active_generic_params)
			source_arg_type := (if source_has_alias {
				source_substituted_source_id
			} else {
				t.resolve_substituted_type_text(source_substituted_source_id)
			})
			if active_args_ids.len > 0 && ((source_has_alias && source_depends_on_active)
				|| !decl_type_is_usable(arg_type)
				|| t.generic_arg_is_unresolved(arg_type)) && decl_type_is_usable(source_arg_type)
				&& !t.generic_arg_is_unresolved(source_arg_type) {
				arg_type = source_arg_type
			}
			is_recv_param := is_receiver && param_idx == 0
			inference_param_type := generic_inference_param_type(child)
			is_forwarded_mut_value := clone_arg.kind == .ident
				&& t.mut_value_ident_nodes[int(clone_arg_id)]
			if is_recv_param && flat.source_type_shape(arg_type).has(.pointer) && !flat.source_type_shape(child.typ).has(.pointer)
				&& !flat.source_type_shape(child.typ).has(.mutable_type) {
				arg_type = flat.source_type_syntax(arg_type).raw_base()
			}
			if (child.is_mut || flat.source_type_shape(child.typ).has(.mutable_type)) && flat.source_type_shape(arg_type).has(.pointer)
				&& !is_forwarded_mut_value {
				arg_type = flat.source_type_syntax(arg_type).raw_base()
			}
			if !flat.source_type_query(inference_param_type).has(.generic_fn_name) {
				arg_type = t.generic_inference_alias_target(arg_type, t.cur_module)
			}
			arg_type = generic_arg_type_for_param(inference_param_type, arg_type)
			if arg_type != 0 {
				infer_generic_type_args(inference_param_type, arg_type, mut inferred)
			}
			param_idx++
		}
		if preserved_source_ids := t.generic_call_args_preserving_explicit(callee, decl, inferred) {

			// See `retarget_cloned_generic_call`: an explicit `fn_name[Type]` list pins these
			// positions for every clone of this body.
			if t.callee_matches_generic_args(callee, decl, preserved_source_ids) {
				return
			}
			call_args = preserved_source_ids.clone()
		} else {
			if inferred.len < param_names.len && t.cur_fn_ret_type != 0 {
				mut return_inferred := map[string]u32{}
				t.infer_generic_return_type_args(decl, t.generic_inference_expected_type(t.cur_fn_ret_type), mut return_inferred, receiver_params)
				for name, inferred_type_source_id in return_inferred {
					if name !in inferred {
						inferred[name] = inferred_type_source_id
					}
				}
			}
			for name in param_names {
				arg_source_id := inferred[name] or { return }

				call_args << t.inherited_generic_arg_for_decl_module(arg_source_id, decl.module, active_args_ids)
			}
		}
	}
	if call_args.len == 0 || t.generic_args_have_placeholders(call_args) {
		return
	}
	if !t.generic_callee_is_specialization(callee.value_text())
		&& t.plain_concrete_callee_shadows_decl(callee.value_text(), decl) {
		return
	}
	concrete_call_args_source_ids := t.canonical_generic_specialization_args(call_args)

	if !t.generic_specialization_registered(decl, concrete_call_args_source_ids)
		&& !t.generic_specialization_in_progress(decl, concrete_call_args_source_ids) {
		if t.defer_nested_generic_emissions {
			t.request_generic_fn_specialization(decl, concrete_call_args_source_ids)
		} else {
			t.generated_fn_used_names(decl, t.emit_generic_fn_specialization(decl, concrete_call_args_source_ids), concrete_call_args_source_ids)
		}
	}
	if is_receiver {
		t.rewrite_generic_method_call(clone_id, clone, decl, concrete_call_args_source_ids)
	} else {
		t.rewrite_generic_plain_call(clone_id, clone, decl, concrete_call_args_source_ids)
	}
}

// callee_matches_generic_args reports whether `callee` already names the specialization of `decl`
// for exactly `args`.
fn (t &Transformer) callee_matches_generic_args(callee flat.Node, decl GenericFnDecl, args_ids []u32) bool {
	if callee.kind != .ident || callee.value_text().len == 0 || args_ids.len == 0 {
		return false
	}
	spec_value := specialized_generic_fn_value(decl.node.value, args_ids)
	qualified_spec := transform_qualified_fn_name(decl.module, spec_value)
	return callee.value_text() in [spec_value, qualified_spec, c_name(spec_value),
		c_name(qualified_spec)]
}

// specialized_generic_fn_type_text renders the `fn (params) ret` type that
// `explicit_generic_fn_value_specialization` writes onto a callee ident when it erases an explicit
// `fn_name[Type]` list into a specialization ident.
fn (mut t Transformer) specialized_generic_fn_type_text(decl GenericFnDecl, args_ids []u32) u32 {
	mut params := t.specialized_generic_call_param_type_texts(decl, args_ids)
	ret_type := t.specialized_fn_return_type_text(decl, args_ids)
	if ret_type != 0 && ret_type != flat.source_type_void {
		params << ret_type
		return flat.source_type_composition(.function_type_spaced, params)
	}
	return flat.source_type_composition(.void_function_type_spaced, params)
}

// callee_is_erased_explicit_generic reports whether `callee` is an ident that
// `explicit_generic_fn_value_specialization` produced from a source-level `fn_name[Type]` list.
//
// That erasure is the only thing that writes the specialized signature into a callee ident's type:
// `make_ident`, which retargets an implicit call, types a name only when it resolves to a variable
// and so leaves a specialization ident untyped. Checking the signature therefore separates an erased
// explicit callee from a stale implicit one without per-worker bookkeeping, which matters because
// the erasure and the retarget can happen in different monomorphization workers.
fn (mut t Transformer) callee_is_erased_explicit_generic(callee flat.Node, decl GenericFnDecl, args_ids []u32) bool {
	if !flat.source_type_shape(callee.typ).has(.function_spaced) {
		return false
	}
	return callee.typ == t.specialized_generic_fn_type_text(decl, args_ids)
}

// generic_call_args_preserving_explicit returns the type arguments a cloned call must keep because
// an explicit `fn_name[Type]` list already chose them, or none when the callee is not an erased
// explicit callee bound to a specialization of `decl`.
//
// Such callees are rewritten to a plain specialization ident before the enclosing generic body is
// cloned, so the explicit list is no longer visible on the call node and the retarget paths would
// otherwise treat the call as implicit. Every position the list fixed resolves identically in each
// clone of that body; only positions it left open may be filled from this clone's inference.
//
// A genuinely implicit callee must not be preserved: the template node is shared between instances,
// so it can still name the specialization chosen for an earlier one and has to be retargeted. The
// signature check separates the two, and it is the only thing that may: once a slot is known to come
// from source, inference about it is not evidence against it. A caller instantiated with a type the
// list does not accept still infers something for that slot, and declining on the mismatch would
// hand the call back to implicit inference and lose the explicit arguments entirely -- the very
// thing this function exists to prevent. Mismatches between an explicit argument and a value passed
// for it are the checker's to report.
//
// `explicit_generic_fn_value_specialization` collapses a list only when it covers every generic
// parameter, so the recorded arguments are normally complete, but the tail is filled positionally so
// a partial list can never be silently replaced.
fn (mut t Transformer) generic_call_args_preserving_explicit(callee flat.Node, decl GenericFnDecl, inferred map[string]u32) ?[]u32 {
	if callee.kind != .ident || callee.value_text().len == 0 {
		return none
	}
	recorded_source_ids := t.recorded_generic_specialization_args(callee.value_text()) or { return none }
	if recorded_source_ids.len == 0 || t.generic_args_have_placeholders(recorded_source_ids) {
		return none
	}
	if !t.callee_matches_generic_args(callee, decl, recorded_source_ids) {
		return none
	}
	if !t.callee_is_erased_explicit_generic(callee, decl, recorded_source_ids) {
		return none
	}
	param_names := t.generic_fn_param_names(decl.node, decl.module)
	if param_names.len < recorded_source_ids.len {
		return none
	}
	mut preserved := recorded_source_ids.clone()
	for name in param_names[recorded_source_ids.len..] {
		preserved << inferred[name] or { return none }
	}
	return preserved.clone()
}

fn (t &Transformer) generic_specialization_registered(decl GenericFnDecl, args_ids []u32) bool {

	// Signatures are registered as soon as a specialization is queued, before its
	// body is cloned. Only the emitted-node index proves that the body exists.
	return t.generic_fn_spec_nodes.contains(t.generic_specialization_progress_key(decl, args_ids))
}

fn (t &Transformer) generic_specialization_in_progress(decl GenericFnDecl, args_ids []u32) bool {
	return t.generic_fn_specs_in_progress.contains(t.generic_specialization_progress_key(decl, args_ids))
}

fn (t &Transformer) generic_specialization_progress_key(decl GenericFnDecl, args_ids []u32) GenericFnSpecKey {
	base := generic_fn_decl_base_value(decl.node.value)
	qbase := transform_qualified_fn_name(decl.module, base)
	return generic_fn_spec_key(qbase, args_ids)
}

fn (mut t Transformer) copy_cloned_resolution(src_id flat.NodeId, dst_id flat.NodeId) {
	if isnil(t.tc) {
		return
	}
	src_idx := int(src_id)
	dst_idx := int(dst_id)
	if src_idx < 0 || dst_idx < 0 {
		return
	}
	if !isnil(t.tc.fork_overlay) {
		// Parallel-transform worker: never write the shared node-indexed arrays
		// (other threads read them, and appending would realloc them). Record the
		// resolution in the fork's private overlay; reads check it first and
		// merge_worker replays it into the master under the shifted ids.
		t.copy_cloned_resolution_forked(src_idx, dst_idx)
		return
	}
	if call_name := t.tc.resolved_call_name(src_id) {
		if !t.cloned_call_has_exact_generic_callee(dst_idx)
			&& (!t.resolved_call_is_generic_fn(call_name)
				|| t.tc.c_backed_alias_method_name(call_name)) {
			t.set_resolved_call_entry(dst_idx, call_name)
		}
	}
	if fn_value := t.tc.resolved_fn_value_name(src_id) {
		t.set_resolved_fn_value_entry(dst_idx, fn_value)
	}
}

fn (mut t Transformer) copy_cloned_resolution_forked(src_idx int, dst_idx int) {
	mut overlay := t.tc.fork_overlay
	mut call_name := if src_idx >= overlay.base_node_count {
		overlay.resolved_call_names[src_idx] or { '' }
	} else {
		''
	}
	if call_name.len == 0 && src_idx < t.tc.resolved_call_set.len && t.tc.resolved_call_set[src_idx] {
		call_name = t.tc.resolved_call_names[src_idx].value
	}
	if call_name.len > 0 && !t.cloned_call_has_exact_generic_callee(dst_idx)
		&& (!t.resolved_call_is_generic_fn(call_name)
			|| t.tc.c_backed_alias_method_name(call_name)) {
		overlay.resolved_call_names[dst_idx] = call_name
	}
	mut fn_value := if src_idx >= overlay.base_node_count {
		overlay.resolved_fn_values[src_idx] or { '' }
	} else {
		''
	}
	if fn_value.len == 0 {
		fn_value = t.tc.resolved_fn_value_name(flat.NodeId(src_idx)) or { '' }
	}
	if fn_value.len > 0 {
		overlay.resolved_fn_values[dst_idx] = fn_value
	}
}

fn (t &Transformer) cloned_call_has_exact_generic_callee(idx int) bool {
	if idx < 0 || idx >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[idx]
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	callee := t.a.child_node(&node, 0)
	return callee.kind == .ident && ((callee.value_text().contains('[') && callee.value_text().contains(']'))
		|| t.generic_callee_is_specialization(callee.value_text()))
}

fn (t &Transformer) resolved_call_is_generic_fn(name string) bool {
	if t.skip_generics || name == '' || isnil(t.tc) {
		return false
	}
	if name.contains('[') && name.contains(']') {
		return true
	}
	if name in t.tc.fn_generic_params {
		return true
	}
	base := generic_fn_decl_base_value(name)
	if base in t.tc.fn_generic_params {
		return true
	}
	if name.contains('.') {
		short := name.all_after_last('.')
		if short in t.tc.fn_generic_params {
			return true
		}
	}
	return false
}

fn substitute_generic_node_value(node flat.Node, args_ids []u32) string {
	match node.kind {
		.call {
			return node.value
		}
		.array_init, .map_init, .struct_init, .assoc, .cast_expr, .as_expr, .sizeof_expr,
		.offsetof_expr, .typeof_expr, .is_expr, .type_decl, .field_decl, .param {
			return substitute_generic_type_text_bytes(node.value_text(), args_ids)
		}
		else {
			return node.value_text()
		}
	}
}

fn (mut t Transformer) fn_decl_has_unresolved_generics(node flat.Node, module_name string) bool {
	if generic_params_have_runtime_type_param(node.generic_params()) {
		return true
	}
	if t.fn_decl_receiver_is_open_generic(node, module_name) {
		return true
	}
	if t.skip_generics {
		return false
	}
	if t.type_text_has_generic_placeholder(node.typ, module_name)
		|| t.node_payload_has_generic_placeholder(node, module_name) {
		return true
	}
	for i in 0 .. node.children_count {
		child := t.a.child_node(&node, i)
		if child.kind != .param {
			if t.prefix_param_scan {
				break
			}
			continue
		}
		if t.type_text_has_generic_placeholder(child.typ, module_name) {
			return true
		}
	}
	return false
}

fn (t &Transformer) fn_decl_receiver_is_open_generic(node flat.Node, module_name string) bool {
	value_id := node.source_value_id()
	if !flat.source_type_shape(value_id).has(.dotted) || isnil(t.tc) {
		return false
	}
	receiver := flat.source_type_syntax(value_id).qualifier()
	syntax := flat.source_type_syntax(receiver)
	if !syntax.generic_app_valid() || syntax.generic_app_args().len == 0 {
		return false
	}
	base := syntax.generic_app_base()
	mut candidates := [base]
	if !flat.source_type_shape(base).has(.dotted) && module_name != '' && module_name !in [
		'main',
		'builtin',
	] {
		candidates << flat.source_type_composition_two(.qualified, flat.source_type_id(module_name), base)
	}
	for candidate in candidates {
		// Generic parameter identifiers enter through declaration metadata.
		params := t.tc.struct_generic_params[candidate] or { continue }
		for arg in syntax.generic_app_args() {
			for param in params {
				if t.trimmed_type_id(arg) == flat.source_type_id(param) { return true }
			}
		}
	}
	return false
}

fn generic_params_have_runtime_type_param(params []string) bool {
	for param in params {
		if !generic_param_is_lifetime(param) {
			return true
		}
	}
	return false
}

fn generic_param_is_lifetime(param string) bool {
	return trimmed_transform_text(param).starts_with('^')
}

fn (mut t Transformer) node_payload_has_generic_placeholder(node flat.Node, module_name string) bool {
	if node.value.len > 0 {
		if t.type_text_has_generic_placeholder_bytes(node.value, module_name) {
			return true
		}
	} else if t.type_text_has_generic_placeholder(node.source_value, module_name) {
		return true
	}
	for arg in node.call_type_args() {
		if t.type_text_has_generic_placeholder(arg, module_name) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) node_subtree_has_generic_placeholder(id flat.NodeId, module_name string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if t.type_text_has_generic_placeholder(node.typ, module_name)
		|| t.node_payload_has_generic_placeholder(node, module_name) {
		return true
	}
	for i in 0 .. node.children_count {
		if t.node_subtree_has_generic_placeholder(t.a.child(&node, i), module_name) {
			return true
		}
	}
	return false
}

// generic_placeholder_prescreen reports whether `text` can possibly hold a
// single-capital-letter type component (`T`, `os.T`, `[]T`, `map[K]V`, ...) or
// the word `generic`. Every text type_text_has_generic_placeholder flags
// contains an isolated capital letter (each detection path ends in
// is_generic_fn_placeholder_name on a component whose neighbours in the
// original text are non-identifier characters), so texts failing this byte
// scan skip the full recursive analysis.
@[direct_array_access]
fn generic_placeholder_prescreen(text string) bool {
	for i in 0 .. text.len {
		c := text[i]
		if c >= `A` && c <= `Z` {
			prev_ident := i > 0 && is_placeholder_ident_char(text[i - 1])
			next_ident := i + 1 < text.len && is_placeholder_ident_char(text[i + 1])
			if !prev_ident && !next_ident {
				return true
			}
		} else if c == `g` && i + 7 <= text.len && text[i + 1] == `e` && text[i + 2] == `n`
			&& text[i + 3] == `e` && text[i + 4] == `r` && text[i + 5] == `i` && text[i + 6] == `c` {
			return true
		}
	}
	return false
}

@[inline]
fn is_placeholder_ident_char(c u8) bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`) || c == `_`
}

@[inline]
fn type_id_may_have_generic_placeholder(typ_id u32) bool {
	query := flat.source_type_query(typ_id)
	return query.has(.generic_token) || query.has(.generic_word)
}

fn (mut t Transformer) type_text_has_generic_placeholder(typ_id u32, module_name string) bool {
	if t.skip_generics { return false }
	input_query := flat.source_type_query(typ_id)
	if !input_query.has(.generic_token) && !input_query.has(.generic_word) { return false }
	input_shape := flat.source_type_shape(typ_id)
	clean_id := if input_shape.has(.padded) {
		flat.source_type_syntax(typ_id).trimmed()
	} else {
		typ_id
	}
	if clean_id == 0 { return false }
	if clean_id == flat.source_type_generic { return true }
	clean_query := if clean_id == typ_id { input_query } else { flat.source_type_query(clean_id) }
	if clean_query.has(.generic_fn_name) && !t.concrete_type_name_known_id(clean_id, module_name) {
		return true
	}
	shape := if clean_id == typ_id { input_shape } else { flat.source_type_shape(clean_id) }
	syntax := flat.source_type_syntax(clean_id)
	if shape.has(.pointer) || shape.has(.mutable_type) || shape.has(.optional)
		|| shape.has(.variadic) || shape.has(.array_type) {
		return t.type_text_has_generic_placeholder(syntax.raw_base(), module_name)
	}
	if shape.has(.map_type) {
		if syntax.bracket_closed() {
			return t.type_text_has_generic_placeholder(syntax.bracket_raw_inner(), module_name)
				|| t.type_text_has_generic_placeholder(syntax.bracket_suffix(), module_name)
		}
	}
	if shape.has(.leading_bracket) {
		if syntax.bracket_closed() {
			return t.type_text_has_generic_placeholder(syntax.bracket_suffix(), module_name)
		}
	}
	if syntax.generic_app_valid() {
		for arg in syntax.generic_app_args() {
			if t.type_text_has_generic_placeholder(arg, module_name) { return true }
		}
	}
	return false
}

// Declaration payloads and source expressions enter the numeric type query here.
fn (mut t Transformer) type_text_has_generic_placeholder_bytes(typ string, module_name string) bool {
	return t.type_text_has_generic_placeholder(flat.source_type_id(typ), module_name)
}

fn (mut t Transformer) concrete_type_name_known(name string, module_name string) bool {
	return t.concrete_type_name_known_id(flat.source_type_id(name), module_name)
}

fn (t &Transformer) concrete_type_name_known_id(name_id u32, module_name string) bool {
	if types.is_builtin_type_name_id(name_id) || name_id in [generic_c_module_type_id,
		generic_js_module_type_id] {
		return true
	}
	if t.is_known_main_struct_type_name_id(name_id) { return true }
	if t.type_name_is_declared_id(name_id) { return true }
	if flat.source_type_shape(name_id).has(.dotted) || module_name in ['', 'main', 'builtin'] {
		return false
	}
	qname := flat.source_type_composition_two(.qualified, flat.source_type_id(module_name), name_id)
	return t.type_name_is_declared_id(qname)
}

fn (mut t Transformer) generic_fn_decl_key(node flat.Node, module_name string) string {
	base_value := if generic_fn_receiver_application_is_structured(node.value_text()) {
		node.value_text()
	} else {
		generic_fn_decl_base_value(node.value_text())
	}
	if module_name != '' && module_name != 'main' && module_name != 'builtin' {
		return '${module_name}.${base_value}'
	}
	return base_value
}

fn generic_fn_receiver_application_is_structured(value string) bool {
	value_id := flat.source_type_id(value)
	if !flat.source_type_shape(value_id).has(.dotted) {
		return false
	}
	syntax := flat.source_type_syntax(flat.source_type_syntax(value_id).qualifier())
	if !syntax.generic_app_valid() || syntax.generic_app_args().len == 0 {
		return false
	}
	for arg in syntax.generic_app_args() {
		if !flat.source_type_query(transform_trimmed_type_id(arg)).has(.generic_fn_name) {
			return true
		}
	}
	return false
}

fn generic_fn_decl_base_value(value string) string {
	if receiver, method := flat.decode_static_type_method_name(value) {
		syntax := flat.source_type_syntax(flat.source_type_id(receiver))
		if syntax.generic_app_valid() {
			return flat.encode_static_type_method_name(flat.source_type_name(syntax.generic_app_base()), method)
		}
		return value
	}
	value_id := flat.source_type_id(value)
	if !flat.source_type_shape(value_id).has(.dotted) {
		return value
	}
	value_syntax := flat.source_type_syntax(value_id)
	syntax := flat.source_type_syntax(value_syntax.qualifier())
	if syntax.generic_app_valid() {
		return flat.source_type_name(flat.source_type_composition_two(.qualified, syntax.generic_app_base(), value_syntax.unqualified()))
	}
	return value
}

fn (t &Transformer) generic_decl_is_receiver_method(node flat.Node) bool {
	if !node.value_text().contains('.') || node.children_count == 0 { return false }
	first := t.a.child_node(&node, 0)
	if first.kind != .param || first.typ == 0 { return false }
	mut first_type := t.trimmed_type_id(first.typ)
	for {
		shape := flat.source_type_shape(first_type)
		if shape.has(.mutable_type) || shape.has(.shared_type) || shape.has(.atomic_type) || shape.has(.pointer) {
			first_type = t.trimmed_type_id(flat.source_type_syntax(first_type).raw_base())
		} else {
			break
		}
	}
	method_name := generic_fn_decl_base_value(node.value_text())
	return t.receiver_param_matches_method_name(t.normalize_type_alias(first_type), method_name)
		|| t.receiver_param_matches_method_name(first_type, method_name)
}

fn (mut t Transformer) generic_decl_has_method_level_params(decl GenericFnDecl) bool {
	all_params := t.generic_fn_param_names(decl.node, decl.module)
	if all_params.len == 0 {
		return false
	}
	mut receiver_params := []string{}
	if decl.node.value.contains('.') {
		receiver := decl.node.value.all_before_last('.')
		t.collect_generic_param_names_from_type(receiver, decl.module, mut receiver_params)
	}
	for i in 0 .. decl.node.children_count {
		child := t.a.child_node(&decl.node, i)
		if child.kind == .param {
			t.collect_generic_param_names_from_type_id(child.typ, decl.module, mut receiver_params)
			break
		}
	}
	for param in all_params {
		// Lifetimes do not change the emitted runtime signature. A method-level
		// lifetime can therefore share the receiver's concrete type specialization.
		if param.starts_with('^') {
			continue
		}
		if param !in receiver_params {
			return true
		}
	}
	return false
}

fn (mut t Transformer) generic_fn_param_names(node flat.Node, module_name string) []string {
	mut names := []string{}
	for param in node.generic_params() {
		clean := generic_param_name_from_decl_param(param)
		if clean.len > 0 && clean !in names {
			names << clean
		}
	}
	if node.value_text().contains('.') {
		for name in t.generic_receiver_param_names(GenericFnDecl{
			node:   node
			module: module_name
		}) {
			if name !in names {
				names << name
			}
		}
	}
	t.collect_generic_param_names_from_type(node.value_text(), module_name, mut names)
	t.collect_generic_param_names_from_type_id(node.typ, module_name, mut names)
	for i in 0 .. node.children_count {
		child := t.a.child_node(&node, i)
		if child.kind != .param {
			if t.prefix_param_scan {
				break
			}
			continue
		}
		t.collect_generic_param_names_from_type_id(child.typ, module_name, mut names)
	}
	return names
}

fn generic_param_name_from_decl_param(param string) string {
	mut clean := trimmed_transform_text(param)
	for {
		if clean.starts_with('...') {
			clean = trimmed_transform_text(clean[3..])
			continue
		}
		if clean.starts_with('[]') {
			clean = trimmed_transform_text(clean[2..])
			continue
		}
		if clean.starts_with('&') || clean.starts_with('?') || clean.starts_with('!') {
			clean = trimmed_transform_text(clean[1..])
			continue
		}
		if clean.starts_with('mut ') {
			clean = trimmed_transform_text(clean[4..])
			continue
		}
		break
	}
	if is_generic_fn_placeholder_name(clean) {
		return clean
	}
	return ''
}

fn (mut t Transformer) collect_generic_param_names_from_node(id flat.NodeId, module_name string, mut names []string) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(id)]
	t.collect_generic_param_names_from_type_id(node.typ, module_name, mut names)
	t.collect_generic_param_names_from_type(if node.kind == .call {
		node.value
	} else {
		node.value_text()
	}, module_name, mut names)
	for arg in node.call_type_args() {
		t.collect_generic_param_names_from_type_id(arg, module_name, mut names)
	}
	for i in 0 .. node.children_count {
		t.collect_generic_param_names_from_node(t.a.child(&node, i), module_name, mut names)
	}
}

fn (mut t Transformer) collect_generic_param_names_from_type(typ string, module_name string, mut names []string) {
	t.collect_generic_param_names_from_type_id(flat.source_type_id(typ), module_name, mut names)
}

fn (mut t Transformer) collect_generic_param_names_from_type_id(typ_id u32, module_name string, mut names []string) {
	clean_id := t.trimmed_type_id(typ_id)
	if clean_id == 0 { return }
	shape := flat.source_type_shape(clean_id)
	if shape.has(.pointer) || shape.has(.mutable_type) || shape.has(.optional)
		|| shape.has(.variadic) || shape.has(.array_type) {
		t.collect_generic_param_names_from_type_id(flat.source_type_syntax(clean_id).raw_base(), module_name, mut names)
		return
	}
	if flat.source_type_query(clean_id).has(.generic_fn_name) && !t.concrete_type_name_known_id(clean_id, module_name) {
		for name in names { if flat.source_type_id(name) == clean_id { return } }
		// Generic declaration metadata still stores parameter identifiers as names.
		names << flat.source_type_name(clean_id)
		return
	}
	syntax := flat.source_type_syntax(clean_id)
	if shape.has(.map_type) {
		if syntax.bracket_closed() {
			t.collect_generic_param_names_from_type_id(syntax.bracket_raw_inner(), module_name, mut names)
			t.collect_generic_param_names_from_type_id(syntax.bracket_suffix(), module_name, mut names)
		}
		return
	}
	if syntax.generic_app_valid() {
		for arg in syntax.generic_app_args() {
			t.collect_generic_param_names_from_type_id(arg, module_name, mut names)
		}
		return
	}
	if syntax.bracket_closed() && syntax.bracket_prefix() != 0 {
		for arg in syntax.bracket_generic_parts() {
			t.collect_generic_param_names_from_type_id(arg, module_name, mut names)
		}
	}
}

fn generic_app_parts(typ string) (string, []string, bool) {
	bracket := typ.index_u8(`[`)
	if bracket <= 0 {
		return '', []string{}, false
	}
	if typ.starts_with('fn(') || typ.starts_with('fn (') {
		return '', []string{}, false
	}
	mut paren_depth := 0
	for i in 0 .. bracket {
		match typ[i] {
			`(` { paren_depth++ }
			`)` { paren_depth-- }
			else {}
		}
	}
	if paren_depth != 0 {
		return '', []string{}, false
	}
	bracket_end := generic_matching_bracket(typ, bracket)
	if bracket_end <= bracket || bracket_end >= typ.len {
		return '', []string{}, false
	}
	inner := trimmed_transform_text(typ[bracket + 1..bracket_end])
	if typeof_display_is_fixed_array_len_text(inner) && !inner.starts_with('C.')
		&& !inner.starts_with('JS.') {
		return '', []string{}, false
	}
	return typ[..bracket], split_generic_args(inner), true
}

fn generic_matching_bracket(s string, start int) int {
	mut depth := 0
	for i in start .. s.len {
		if s[i] == `[` {
			depth++
		} else if s[i] == `]` {
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return s.len
}

fn split_generic_args(s string) []string {
	mut parts := []string{}
	mut bracket_depth := 0
	mut paren_depth := 0
	mut start := 0
	for i in 0 .. s.len {
		match s[i] {
			`[` {
				bracket_depth++
			}
			`]` {
				bracket_depth--
			}
			`(` {
				paren_depth++
			}
			`)` {
				paren_depth--
			}
			`,` {
				if bracket_depth == 0 && paren_depth == 0 {
					parts << trimmed_transform_text(s[start..i])
					start = i + 1
				}
			}
			else {}
		}
	}
	parts << trimmed_transform_text(s[start..])
	return parts
}

fn normalize_generic_args(args_ids []u32, module_name string) []u32 {
	mut result := []u32{cap: args_ids.len}
	for arg_id in args_ids { result << normalize_generic_arg_id(arg_id, module_name) }
	return result.clone()
}

fn normalize_generic_arg_id(arg_id u32, module_name string) u32 {
	clean_id := transform_trimmed_type_id(arg_id)
	if clean_id == 0 { return clean_id }
	shape := flat.source_type_shape(clean_id)
	if shape.has(.pointer) || shape.has(.array_type) || shape.has(.optional) {
		syntax := flat.source_type_syntax(clean_id)
		base := normalize_generic_arg_id(syntax.raw_base(), module_name)
		kind := if shape.has(.pointer) {
			flat.SourceTypeComposition.pointer
		} else if shape.has(.array_type) {
			flat.SourceTypeComposition.array
		} else if shape.has(.option_type) {
			flat.SourceTypeComposition.option
		} else {
			flat.SourceTypeComposition.result
		}
		return flat.source_type_composition_one(kind, base)
	}
	if shape.has(.map_type) {
		syntax := flat.source_type_syntax(clean_id)
		if syntax.bracket_closed() {
			return flat.source_type_composition(.map, [
				normalize_generic_arg_id(syntax.bracket_raw_inner(), module_name),
				normalize_generic_arg_id(syntax.raw_base(), module_name),
			])
		}
	}
	return clean_id
}

fn substitute_generic_fixed_array_type_text(clean string, args_ids []u32, params []string) ?string {
	id := substitute_generic_fixed_array_type_id(flat.source_type_id(clean), args_ids, params) or { return none }
	return flat.source_type_name(id)
}

fn substitute_generic_fixed_array_type_id(clean u32, args_ids []u32, params []string) ?u32 {
	shape := flat.source_type_shape(clean)
	syntax := flat.source_type_syntax(clean)
	if shape.has(.array_type) || shape.has(.map_type) || shape.has(.leading_bracket) || !syntax.suffix_bracket_valid() {
		return none
	}
	length := syntax.suffix_bracket_length()
	if !flat.source_type_syntax(length).transform_fixed_length() { return none }
	elem := transform_trimmed_type_id(syntax.last_bracket_prefix())
	if elem == 0 { return none }
	elem_sub := substitute_generic_type_text_with_params(elem, args_ids, params)
	// Lengths are source expressions; substitute their identifier tokens at that boundary.
	len_sub := flat.source_type_id(substitute_generic_expr_text_with_params(flat.source_type_name(length), args_ids, params))
	return flat.source_type_composition_two(.fixed_array_suffix, len_sub, elem_sub)
}

fn substitute_generic_expr_text(text string, args_ids []u32) string {
	return substitute_generic_expr_text_with_params(text, args_ids, []string{})
}

fn substitute_generic_expr_text_with_params(text string, args_ids []u32, params []string) string {
	if text == '' || args_ids.len == 0 {
		return text
	}
	if params.len > 0 && !generic_text_contains_param(text, params) {
		return text
	}
	mut out := strings.new_builder(text.len)
	mut i := 0
	for i < text.len {
		ch := text[i]
		if generic_expr_ident_start(ch) {
			start := i
			i++
			for i < text.len && generic_expr_ident_char(text[i]) {
				i++
			}
			ident := text[start..i]
			out.write_string(substitute_generic_expr_ident(ident, args_ids, params))
			continue
		}
		out.write_u8(ch)
		i++
	}
	result := out.str()
	unsafe { out.free() }
	return result
}

fn substitute_generic_expr_ident(ident string, args_ids []u32, params []string) string {
	for i, param in params {
		if ident == param {
			if i < args_ids.len {
				return flat.source_type_name(args_ids[i])
			}
			return ident
		}
	}
	if params.len == 0 && is_generic_fn_placeholder_name(ident) {
		idx := generic_param_index(ident)
		if idx < args_ids.len {
			return flat.source_type_name(args_ids[idx])
		}
	}
	return ident
}

@[inline]
fn generic_expr_ident_start(ch u8) bool {
	return (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`) || ch == `_`
}

@[inline]
fn generic_expr_ident_char(ch u8) bool {
	return generic_expr_ident_start(ch) || (ch >= `0` && ch <= `9`) || ch == `.`
}

@[direct_array_access; inline]
fn generic_text_contains_param(text string, params []string) bool {
	if text == '' || params.len == 0 {
		return false
	}
	mut i := 0
	for i < text.len {
		if !generic_expr_ident_start(text[i]) {
			i++
			continue
		}
		start := i
		i++
		for i < text.len
			&& (generic_expr_ident_start(text[i]) || (text[i] >= `0` && text[i] <= `9`)) {
			i++
		}
		token_len := i - start
		for param in params {
			if param.len != token_len {
				continue
			}
			mut matches := true
			for j in 0 .. token_len {
				if text[start + j] != param[j] {
					matches = false
					break
				}
			}
			if matches {
				return true
			}
		}
	}
	return false
}

fn substitute_generic_type_text(typ_id u32, args_ids []u32) u32 {
	return substitute_generic_type_id_inner(typ_id, args_ids, []string{}, false)
}

fn substitute_generic_type_text_bytes(typ string, args_ids []u32) string {
	return flat.source_type_name(substitute_generic_type_text(flat.source_type_id(typ), args_ids))
}

fn substitute_generic_type_text_with_params(typ_id u32, args_ids []u32, params []string) u32 {
	return substitute_generic_type_id_inner(typ_id, args_ids, params, true)
}

@[direct_array_access]
fn substitute_generic_type_text_with_params_bytes(typ string, args_ids []u32, params []string) string {
	return flat.source_type_name(substitute_generic_type_text_with_params(flat.source_type_id(typ), args_ids, params))
}

fn generic_param_index_id(name_id u32) int {
	if name_id in [flat.source_type_id('U'), flat.source_type_id('B'), flat.source_type_id('V'),
		flat.source_type_id('Y')] {
		return 1
	}
	if name_id in [flat.source_type_id('C'), flat.source_type_id('W'), flat.source_type_id('Z')] {
		return 2
	}
	return 0
}

fn substitute_generic_type_id_inner(typ_id u32, args_ids []u32, params []string, declared bool) u32 {
	clean := transform_trimmed_type_id(typ_id)
	if clean == 0 || args_ids.len == 0 { return typ_id }
	if declared && params.len > 0 && !generic_id_contains_param(clean, params) { return clean }
	if fixed := substitute_generic_fixed_array_type_id(clean, args_ids, if declared {
		params
	} else {
		[]string{}
	}) {
		return fixed
	}
	if declared {
		for i, param in params {
			if clean == flat.source_type_id(param) {
				return if i < args_ids.len { args_ids[i] } else { clean }
			}
		}
	}
	if (!declared || params.len == 0) && flat.source_type_query(clean).has(.generic_fn_name) {
		idx := generic_param_index_id(clean)
		return if idx < args_ids.len { args_ids[idx] } else { clean }
	}
	shape := flat.source_type_shape(clean)
	syntax := flat.source_type_syntax(clean)
	if kind := generic_type_wrapper_composition(shape) {
		// The positional source-value adapter historically leaves channel, thread and
		// atomic payloads intact; declared-parameter substitution descends into them.
		if declared || (!shape.has(.atomic_type) && !shape.has(.channel_type) && !shape.has(.thread_type)) {
			inner := substitute_generic_type_id_inner(syntax.raw_base(), args_ids, params, declared)
			if declared && ((shape.has(.option_type) && flat.source_type_shape(inner).has(.option_type)) || (shape.has(.result_type) && flat.source_type_shape(inner).has(.result_type))) {
				return inner
			}
			return flat.source_type_composition_one(kind, inner)
		}
	}
	if shape.has(.map_type) && syntax.bracket_closed() {
		return flat.source_type_composition_two(.map, substitute_generic_type_id_inner(syntax.bracket_raw_inner(), args_ids, params, declared), substitute_generic_type_id_inner(syntax.bracket_suffix(), args_ids, params, declared))
	}
	if shape.has(.leading_bracket) && syntax.bracket_closed() {
		// The interior is an expression, rather than a type spelling.
		length := flat.source_type_id(substitute_generic_expr_text_with_params(flat.source_type_name(syntax.bracket_raw_inner()), args_ids, if declared {
			params
		} else {
			[]string{}
		}))
		elem := substitute_generic_type_id_inner(syntax.bracket_suffix(), args_ids, params, declared)
		return flat.source_type_composition_two(.fixed_array, length, elem)
	}
	if syntax.tuple_like() && syntax.contains_comma() {
		mut parts := []u32{cap: syntax.tuple_components().len}
		for part in syntax.tuple_components() {
			parts << substitute_generic_type_id_inner(part, args_ids, params, declared)
		}
		return flat.source_type_composition(.multi_return, parts)
	}
	// Function type substitution is declared-parameter behavior; positional source
	// substitution historically treats its spelling as an opaque leaf.
	if declared && (shape.has(.function_compact) || shape.has(.function_spaced)) {
		if substituted := subst_generic_fn_type_id(clean, args_ids, params) { return substituted }
	}
	if syntax.generic_app_valid() {
		mut parts := []u32{cap: syntax.generic_app_args().len + 1}
		parts << syntax.generic_app_base()
		for arg in syntax.generic_app_args() {
			parts << substitute_generic_type_id_inner(arg, args_ids, params, declared)
		}
		return flat.source_type_composition(.generic, parts)
	}
	return clean
}

// subst_generic_fn_type_text substitutes generic params inside a `fn (...) ...` type text,
// recursing into each parameter type and the return type. Returns none when the signature
// is malformed (unbalanced parens).
fn subst_generic_fn_type_text(clean string, args_ids []u32, params []string) ?string {
	id := subst_generic_fn_type_id(flat.source_type_id(clean), args_ids, params) or { return none }
	return flat.source_type_name(id)
}

fn subst_generic_fn_type_id(clean u32, args_ids []u32, params []string) ?u32 {
	syntax := flat.source_type_syntax(clean)
	if !syntax.function_closed() { return none }
	mut parts := []u32{cap: syntax.substitution_function_params().len + 1}
	for param in syntax.substitution_function_params() {
		parts << subst_generic_fn_type_param_id(param, args_ids, params)
	}
	if syntax.function_result() != 0 {
		parts << substitute_generic_type_text_with_params(syntax.function_result(), args_ids, params)
		return flat.source_type_composition(.function_type, parts)
	}
	return flat.source_type_composition(.void_function_type, parts)
}

fn subst_generic_fn_type_param_text(param string, args_ids []u32, params []string) string {
	return flat.source_type_name(subst_generic_fn_type_param_id(flat.source_type_id(param), args_ids, params))
}

fn subst_generic_fn_type_param_id(param_id u32, args_ids []u32, params []string) u32 {
	syntax := flat.source_type_syntax(param_id)
	sub := substitute_generic_type_text_with_params(syntax.generic_parameter_payload(), args_ids, params)
	mut result := if syntax.parameter_named() {
		flat.source_type_composition_two(.named_parameter, syntax.parameter_name(), sub)
	} else {
		sub
	}
	if syntax.parameter_is_mut() && sub != 0 {
		result = flat.source_type_composition_one(.mutable_parameter, result)
	}
	return result
}

fn generic_top_level_space_index(s string) int {
	mut depth := 0
	for i := 0; i < s.len; i++ {
		match s[i] {
			`(`, `[` {
				depth++
			}
			`)`, `]` {
				depth--
			}
			` ` {
				if depth == 0 {
					return i
				}
			}
			else {}
		}
	}
	return -1
}

fn generic_fn_type_param_head_is_name(head string, tail string) bool {
	if head == '' || tail == '' {
		return false
	}
	if head.starts_with('fn') || head.starts_with('&') || head.starts_with('[') {
		return false
	}
	if head in ['shared', 'atomic', 'chan', 'thread', 'map'] || head.contains('.') {
		return false
	}
	if types.is_builtin_type_name(head) {
		return false
	}
	return (head[0] >= `a` && head[0] <= `z`) || head[0] == `_`
}

// subst_type substitutes generic placeholders in a type-text using the currently
// active generic parameter names (so non-canonical params resolve by name). Falls
// back to positional substitution when no params are active.
fn (t &Transformer) subst_type(typ_id u32, args_ids []u32) u32 {
	return substitute_generic_type_text_with_params(typ_id, args_ids, t.active_generic_params)
}

// subst_node_source_value substitutes type syntax while retaining its interned spelling.
fn (mut t Transformer) subst_node_source_value(node flat.Node, args_ids []u32) u32 {
	source := node.source_value_id()
	match node.kind {
		.is_expr {
			// A comptime loop variable must remain a bare name until the loop is unrolled.
			if node.value_text() in t.cloning_comptime_for_vars {
				return source
			}
			return t.resolve_substituted_type_text(t.subst_type(source, args_ids))
		}
		.array_init, .map_init, .struct_init, .assoc, .cast_expr, .as_expr, .offsetof_expr {
			substituted := t.subst_type(source, args_ids)
			if node.kind == .cast_expr && substituted != source
				&& t.generic_type_text_contains_alias(substituted, t.cur_module) {
				// Qualify before substitution so aliases keep the caller's scope.
				qualified := t.qualify_generic_alias_cast_type_text(source)
				return t.lock_colliding_main_substitution_type_text(qualified, t.subst_type(qualified, args_ids), t.cur_module, t.active_generic_params)
			}
			return t.resolve_substituted_type_text(substituted)
		}
		.sizeof_expr, .typeof_expr {
			substituted := t.subst_type(source, args_ids)
			// An unchanged sizeof/typeof operand can be a local expression name.
			return if substituted == source {
				source
			} else {
				t.resolve_substituted_type_text(substituted)
			}
		}
		.array_literal {
			// Literal payloads were copied unchanged by subst_node_value.
			return source
		}
		else {
			// Explicit type idents retain the ordinary ident substitution rules.
			substituted := t.subst_node_value(node, args_ids)
			return if substituted == node.value_text() {
				source
			} else {
				flat.source_type_id(substituted)
			}
		}
	}
}

// subst_node_value is the param-aware counterpart of substitute_generic_node_value.
fn (mut t Transformer) subst_node_value(node flat.Node, args_ids []u32) string {
	match node.kind {
		.ident {
			if node.value_text().contains('[') {
				return substitute_generic_type_text_with_params_bytes(node.value_text(), args_ids, t.active_generic_params)
			}
			if flat.source_type_shape(node.typ).has(.bracketed) {
				syntax := flat.source_type_syntax(node.typ)
				if syntax.generic_app_valid() && flat.source_type_id(node.value_text()) == flat.source_type_syntax(syntax.generic_app_base()).unqualified() {
					return flat.source_type_name(t.subst_type(node.typ, args_ids))
				}
			}
			return node.value_text()
		}
		.selector {
			if node.value_text().contains('[') {
				return substitute_generic_type_text_with_params_bytes(node.value_text(), args_ids, t.active_generic_params)
			}
			if flat.source_type_shape(node.typ).has(.bracketed) {
				old_field_type_comparison_type_id := t.trim_pointer_type(node.typ)

				new_field_type_comparison_type_id := t.trim_pointer_type(t.subst_type(node.typ, args_ids))

				if old_field_type_comparison_type_id != new_field_type_comparison_type_id
					&& t.source_sum_field_name_id(old_field_type_comparison_type_id) == flat.source_type_id(node.value_text()) {
					return flat.source_type_name(t.source_sum_field_name_id(new_field_type_comparison_type_id))
				}
			}
			return node.value_text()
		}
		.is_expr, .array_init, .map_init, .struct_init, .assoc, .cast_expr, .as_expr,
		.offsetof_expr, .sizeof_expr, .typeof_expr {
			return flat.source_type_name(t.subst_node_source_value(node, args_ids))
		}
		.call {
			return node.value
		}
		.type_decl, .field_decl, .param {
			return substitute_generic_type_text_with_params_bytes(node.value_text(), args_ids, t.active_generic_params)
		}
		.comptime_if {
			return t.subst_comptime_type_condition(node.value_text(), args_ids)
		}
		.sql_expr {
			return t.subst_sql_expr_value(node.value_text(), args_ids)
		}
		else {
			return node.value_text()
		}
	}
}

fn (t &Transformer) subst_sql_expr_value(value string, args_ids []u32) string {
	if value == '' || args_ids.len == 0 || t.active_generic_params.len == 0 {
		return value
	}
	tokens := sql_clean_tokens(value.split(' '))
	mut out := []string{cap: tokens.len}
	for token in tokens {
		out << t.subst_sql_expr_token(token, args_ids)
	}
	return out.join(' ')
}

fn (t &Transformer) subst_sql_expr_token(token string, args_ids []u32) string {
	clean := trimmed_transform_text(token)
	if clean.len == 0 {
		return token
	}
	for param in t.active_generic_params {
		if clean == param || clean.contains('[${param}]') || clean.contains(', ${param}')
			|| clean.contains('${param},') {
			return substitute_generic_type_text_with_params_bytes(clean, args_ids, t.active_generic_params)
		}
	}
	return token
}

fn generic_param_id_is_named(type_id u32, params []string) bool {
	for param in params { if flat.source_type_id(param) == type_id { return true } }
	return false
}

fn generic_type_wrapper_composition(shape flat.SourceTypeShape) ?flat.SourceTypeComposition {
	if shape.has(.mutable_type) { return .mutable_parameter }
	if shape.has(.shared_type) { return .shared_type }
	if shape.has(.atomic_type) { return .atomic_type }
	if shape.has(.variadic) { return .variadic_parameter }
	if shape.has(.array_type) { return .array }
	if shape.has(.option_type) { return .option }
	if shape.has(.result_type) { return .result }
	if shape.has(.pointer) { return .pointer }
	if shape.has(.channel_type) { return .channel }
	if shape.has(.thread_type) { return .thread }
	return none
}

fn (t &Transformer) qualify_generic_alias_cast_type_text(typ_id u32) u32 {
	clean := transform_trimmed_type_id(typ_id)
	if clean == 0 || generic_param_id_is_named(clean, t.active_generic_params) || isnil(t.tc) {
		return clean
	}
	shape := flat.source_type_shape(clean)
	syntax := flat.source_type_syntax(clean)
	if kind := generic_type_wrapper_composition(shape) {
		return flat.source_type_composition_one(kind, t.qualify_generic_alias_cast_type_text(syntax.raw_base()))
	}
	if shape.has(.map_type) && syntax.bracket_closed() && syntax.bracket_suffix() != 0 {
		return flat.source_type_composition_two(.map, t.qualify_generic_alias_cast_type_text(syntax.bracket_raw_inner()), t.qualify_generic_alias_cast_type_text(syntax.bracket_suffix()))
	}
	if shape.has(.leading_bracket) && syntax.bracket_closed() && syntax.bracket_raw_inner() != 0 && syntax.bracket_suffix() != 0 {
		return flat.source_type_composition_two(.concat, syntax.bracket_prefix(), t.qualify_generic_alias_cast_type_text(syntax.bracket_suffix()))
	}
	if (shape.has(.function_compact) || shape.has(.function_spaced)) && syntax.function_closed() {
		mut parts := []u32{cap: syntax.function_params().len + 1}
		for param in syntax.function_params() {
			parts << t.qualify_generic_alias_cast_type_text(flat.source_type_syntax(param).legacy_parameter_mut_type())
		}
		ret := t.qualify_generic_alias_cast_type_text(syntax.function_result())
		if ret != 0 {
			parts << ret
			return flat.source_type_composition(.function_type_spaced, parts)
		}
		return flat.source_type_composition(.void_function_type_spaced, parts)
	}
	if syntax.tuple_like() && syntax.contains_comma() {
		mut parts := []u32{cap: syntax.tuple_components().len}
		for part in syntax.tuple_components() {
			parts << t.qualify_generic_alias_cast_type_text(part)
		}
		return flat.source_type_composition(.multi_return, parts)
	}
	if syntax.generic_app_valid() {
		mut parts := []u32{cap: syntax.generic_app_args().len + 1}
		parts << t.qualify_generic_alias_cast_type_text(syntax.generic_app_base())
		for arg in syntax.generic_app_args() {
			parts << t.qualify_generic_alias_cast_type_text(arg)
		}
		return flat.source_type_composition(.generic, parts)
	}
	if imported := t.resolve_imported_type_name(clean) { return imported }
	if selective := t.selective_signature_type_symbol_id(t.cur_file, clean) {
		return flat.source_type_id(selective)
	}
	if !shape.has(.dotted) && t.generic_arg_module_owns_type(flat.source_type_name(clean), t.cur_module) {
		return flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), clean)
	}
	return clean
}

fn (t &Transformer) qualify_generic_alias_cast_type_text_bytes(typ string) string {
	return flat.source_type_name(t.qualify_generic_alias_cast_type_text(flat.source_type_id(typ)))
}

fn (t &Transformer) resolve_substituted_type_text(typ_id u32) u32 {
	clean := transform_trimmed_type_id(typ_id)
	if clean == 0 || isnil(t.tc) { return typ_id }
	shape := flat.source_type_shape(clean)
	syntax := flat.source_type_syntax(clean)
	if shape.has(.shared_type) || shape.has(.atomic_type) {
		kind := if shape.has(.shared_type) {
			flat.SourceTypeComposition.shared_type
		} else {
			flat.SourceTypeComposition.atomic_type
		}
		return flat.source_type_composition_one(kind, t.resolve_substituted_type_text(syntax.raw_base()))
	}
	if flat.source_type_query(clean).has(.main_prefix) && !t.ident_is_import_alias('main') {
		bare := syntax.main_base()
		if !flat.source_type_shape(bare).has(.dotted) && t.type_name_is_declared_id(bare) {
			return clean
		}
	}
	if t.substituted_type_belongs_to_main_generic(clean) { return clean }
	if kind := generic_type_wrapper_composition(shape) {
		return flat.source_type_composition_one(kind, t.resolve_substituted_type_text(syntax.raw_base()))
	}
	// Resolve component types independently to preserve caller-module provenance.
	if shape.has(.map_type) && syntax.bracket_closed() && syntax.bracket_suffix() != 0 {
		return flat.source_type_composition_two(.map, t.resolve_substituted_type_text(syntax.bracket_raw_inner()), t.resolve_substituted_type_text(syntax.bracket_suffix()))
	}
	if shape.has(.leading_bracket) && syntax.bracket_closed() && syntax.bracket_raw_inner() != 0 && syntax.bracket_suffix() != 0 {
		return flat.source_type_composition_two(.concat, syntax.bracket_prefix(), t.resolve_substituted_type_text(syntax.bracket_suffix()))
	}
	if (shape.has(.function_compact) || shape.has(.function_spaced)) && syntax.function_closed() {
		mut parts := []u32{cap: syntax.function_params().len + 1}
		for param in syntax.function_params() { parts << t.resolve_substituted_type_text(param) }
		ret := t.resolve_substituted_type_text(syntax.function_result())
		if ret != 0 {
			parts << ret
			return flat.source_type_composition(.function_type_spaced, parts)
		}
		return flat.source_type_composition(.void_function_type_spaced, parts)
	}
	base := syntax.generic_app_base()
	args := syntax.generic_app_args()
	if syntax.generic_app_valid() && args.len > 0 && !flat.source_type_shape(base).has(.dotted) && t.cur_module.len > 0 && t.cur_module !in [
		'main',
		'builtin',
	] {
		qualified_base := flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), base)
		if qualified_base in t.tc.struct_generic_params || qualified_base in t.tc.sum_generic_params {
			mut parts := []u32{cap: args.len + 1}
			parts << qualified_base
			for arg in args { parts << t.resolve_substituted_type_text(arg) }
			return flat.source_type_composition(.generic, parts)
		}
	}
	parsed := t.tc.parse_resolution_type(clean)
	if parsed is types.Unknown { return typ_id }
	resolved := parsed.source_type_id()
	if syntax.generic_app_valid() && args.len > 0 {
		resolved_base := flat.source_type_syntax(resolved).generic_base()
		if resolved_base != 0 && (resolved_base != base || flat.source_type_shape(base).has(.dotted)) {
			mut parts := []u32{cap: args.len + 1}
			parts << resolved_base
			for arg in args { parts << t.resolve_substituted_type_text(arg) }
			return flat.source_type_composition(.generic, parts)
		}
	}
	return resolved
}

fn (t &Transformer) resolve_substituted_type_text_bytes(typ string) string {
	return flat.source_type_name(t.resolve_substituted_type_text(flat.source_type_id(typ)))
}

fn (t &Transformer) lock_colliding_main_substitution_type_text(original_id u32, substituted_id u32, module_name string, generic_params []string) u32 {
	source := transform_trimmed_type_id(original_id)
	concrete := transform_trimmed_type_id(substituted_id)
	if source == concrete && !generic_id_contains_param(source, generic_params) { return concrete }
	if generic_param_id_is_named(source, generic_params) {
		return t.lock_colliding_main_generic_type_text(concrete, module_name)
	}
	source_shape := flat.source_type_shape(source)
	concrete_shape := flat.source_type_shape(concrete)
	source_syntax := flat.source_type_syntax(source)
	concrete_syntax := flat.source_type_syntax(concrete)
	if source_shape.has(.mutable_type) && concrete_shape.has(.pointer) {
		return flat.source_type_composition_one(.pointer, t.lock_colliding_main_substitution_type_text(source_syntax.raw_base(), concrete_syntax.raw_base(), module_name, generic_params))
	}
	if source_kind := generic_type_wrapper_composition(source_shape) {
		if concrete_kind := generic_type_wrapper_composition(concrete_shape) {
			if source_kind == concrete_kind {
				return flat.source_type_composition_one(source_kind, t.lock_colliding_main_substitution_type_text(source_syntax.raw_base(), concrete_syntax.raw_base(), module_name, generic_params))
			}
		}
	}
	if (source_shape.has(.function_compact) || source_shape.has(.function_spaced)) && (concrete_shape.has(.function_compact) || concrete_shape.has(.function_spaced)) && source_syntax.function_closed() && concrete_syntax.function_closed() && source_syntax.function_params().len == concrete_syntax.function_params().len {
		mut parts := []u32{cap: concrete_syntax.function_params().len + 1}
		for i, concrete_param in concrete_syntax.function_params() {
			source_param := transform_trimmed_type_id(source_syntax.function_params()[i])
			// Both payloads are declared spellings. A mut prefix is restored separately;
			// an instantiated leading pointer belongs to the generic argument itself.
			locked := t.lock_colliding_main_substitution_type_text(flat.source_type_syntax(source_param).generic_parameter_payload(), flat.source_type_syntax(concrete_param).generic_parameter_payload(), module_name, generic_params)
			parts << if flat.source_type_shape(source_param).has(.mutable_type) {
				flat.source_type_composition_one(.mutable_parameter, locked)
			} else {
				locked
			}
		}
		ret := t.lock_colliding_main_substitution_type_text(source_syntax.function_result(), concrete_syntax.function_result(), module_name, generic_params)
		if ret != 0 {
			parts << ret
			return flat.source_type_composition(.function_type_spaced, parts)
		}
		return flat.source_type_composition(.void_function_type_spaced, parts)
	}
	if source_syntax.tuple_like() && source_syntax.contains_comma() && concrete_syntax.tuple_like() && concrete_syntax.contains_comma() && source_syntax.tuple_components().len == concrete_syntax.tuple_components().len {
		mut parts := []u32{cap: concrete_syntax.tuple_components().len}
		for i, part in concrete_syntax.tuple_components() {
			parts << t.lock_colliding_main_substitution_type_text(source_syntax.tuple_components()[i], part, module_name, generic_params)
		}
		return flat.source_type_composition(.multi_return, parts)
	}
	if source_shape.has(.map_type) && concrete_shape.has(.map_type) && source_syntax.bracket_closed() && concrete_syntax.bracket_closed() && source_syntax.bracket_suffix() != 0 && concrete_syntax.bracket_suffix() != 0 {
		return flat.source_type_composition_two(.map, t.lock_colliding_main_substitution_type_text(source_syntax.bracket_raw_inner(), concrete_syntax.bracket_raw_inner(), module_name, generic_params), t.lock_colliding_main_substitution_type_text(source_syntax.bracket_suffix(), concrete_syntax.bracket_suffix(), module_name, generic_params))
	}
	if source_shape.has(.leading_bracket) && concrete_shape.has(.leading_bracket) && source_syntax.bracket_closed() && concrete_syntax.bracket_closed() && source_syntax.bracket_raw_inner() != 0 && concrete_syntax.bracket_raw_inner() != 0 && source_syntax.bracket_suffix() != 0 && concrete_syntax.bracket_suffix() != 0 {
		elem := t.lock_colliding_main_substitution_type_text(source_syntax.bracket_suffix(), concrete_syntax.bracket_suffix(), module_name, generic_params)
		return flat.source_type_composition_two(.concat, concrete_syntax.bracket_prefix(), elem)
	}
	if source_syntax.generic_app_valid() && concrete_syntax.generic_app_valid() && source_syntax.generic_app_base() == concrete_syntax.generic_app_base() && source_syntax.generic_app_args().len == concrete_syntax.generic_app_args().len {
		mut parts := []u32{cap: concrete_syntax.generic_app_args().len + 1}
		mut args_locked := false
		for i, arg in concrete_syntax.generic_app_args() {
			locked := t.lock_colliding_main_substitution_type_text(source_syntax.generic_app_args()[i], arg, module_name, generic_params)
			if locked != transform_trimmed_type_id(arg) { args_locked = true }
			parts << locked
		}
		// Locked arguments force semantic parsing without the declaration context;
		// qualify a declaration-owned generic base along with those arguments.
		base := if args_locked {
			t.qualify_decl_module_generic_base_id(concrete_syntax.generic_app_base(), module_name)
		} else {
			concrete_syntax.generic_app_base()
		}
		parts.prepend(base)
		return flat.source_type_composition(.generic, parts)
	}
	return t.lock_colliding_main_generic_type_text(concrete, module_name)
}

fn (t &Transformer) lock_colliding_main_substitution_type_text_bytes(original string, substituted string, module_name string, generic_params []string) string {
	return flat.source_type_name(t.lock_colliding_main_substitution_type_text(flat.source_type_id(original), flat.source_type_id(substituted), module_name, generic_params))
}

// qualify_decl_module_generic_base qualifies a bare generic base declared in
// `module_name` (e.g. `List` -> `lib.List`); other bases are returned unchanged.
fn (t &Transformer) qualify_decl_module_generic_base(base string, module_name string) string {
	return flat.source_type_name(t.qualify_decl_module_generic_base_id(flat.source_type_id(base), module_name))
}

fn (t &Transformer) qualify_decl_module_generic_base_id(base_id u32, module_name string) u32 {
	clean := transform_trimmed_type_id(base_id)
	if flat.source_type_shape(clean).has(.dotted) || module_name in ['', 'main', 'builtin'] {
		return clean
	}
	qualified := flat.source_type_composition_two(.qualified, flat.source_type_id(module_name), clean)
	if qualified in t.structs || t.sum_types.contains(qualified) || (!isnil(t.tc) && (qualified in t.tc.struct_generic_params || qualified in t.tc.sum_generic_params || qualified in t.tc.interface_names || qualified in t.tc.interface_generic_params || t.tc.type_aliases.contains(qualified) || qualified in t.tc.type_alias_generic_params)) {
		return qualified
	}
	return clean
}

// lock_colliding_main_generic_type_text rewrites a bare program-module (main)
// type substituted into a generic specialization to an explicit `main.` spelling
// when the specialization's own module declares a same-named type. Without the
// lock, codegen re-resolves the bare name in that module context and captures the
// wrong type (e.g. a user `Context` embedding `veb.Context` becoming `veb.Context`
// inside a specialized veb function). resolve_imported_type_text strips `main.`
// back to the program type.
fn (t &Transformer) lock_colliding_main_generic_type_text(typ_id u32, module_name string) u32 {
	clean := transform_trimmed_type_id(typ_id)
	if module_name in ['', 'main'] { return clean }
	shape := flat.source_type_shape(clean)
	syntax := flat.source_type_syntax(clean)
	if kind := generic_type_wrapper_composition(shape) {
		return flat.source_type_composition_one(kind, t.lock_colliding_main_generic_type_text(syntax.raw_base(), module_name))
	}
	// A function hides lockable parameter and return types. Keep its exact spelling
	// when none of its components changes.
	if (shape.has(.function_compact) || shape.has(.function_spaced)) && syntax.function_closed() {
		mut changed := false
		mut parts := []u32{cap: syntax.function_params().len + 1}
		for param in syntax.function_params() {
			locked := t.lock_colliding_main_generic_type_text(param, module_name)
			if locked != transform_trimmed_type_id(param) { changed = true }
			parts << locked
		}
		ret := t.lock_colliding_main_generic_type_text(syntax.function_result(), module_name)
		if ret != syntax.function_result() { changed = true }
		if !changed { return clean }
		if ret != 0 {
			parts << ret
			return flat.source_type_composition(.function_type_spaced, parts)
		}
		return flat.source_type_composition(.void_function_type_spaced, parts)
	}
	if syntax.tuple_like() && syntax.contains_comma() {
		mut parts := []u32{cap: syntax.tuple_components().len}
		for part in syntax.tuple_components() {
			parts << t.lock_colliding_main_generic_type_text(part, module_name)
		}
		return flat.source_type_composition(.multi_return, parts)
	}
	// Descend through composites before handling simple qualified names, preserving
	// provenance for a bare main type nested beside qualified components.
	if shape.has(.map_type) && syntax.bracket_closed() && syntax.bracket_suffix() != 0 {
		return flat.source_type_composition_two(.map, t.lock_colliding_main_generic_type_text(syntax.bracket_raw_inner(), module_name), t.lock_colliding_main_generic_type_text(syntax.bracket_suffix(), module_name))
	}
	if shape.has(.leading_bracket) && syntax.bracket_closed() && syntax.bracket_raw_inner() != 0 && syntax.bracket_suffix() != 0 {
		return flat.source_type_composition_two(.concat, syntax.bracket_prefix(), t.lock_colliding_main_generic_type_text(syntax.bracket_suffix(), module_name))
	}
	if syntax.generic_app_valid() {
		mut changed := false
		mut parts := []u32{cap: syntax.generic_app_args().len + 1}
		locked_base := t.lock_colliding_main_generic_type_text(syntax.generic_app_base(), module_name)
		if locked_base != transform_trimmed_type_id(syntax.generic_app_base()) { changed = true }
		parts << locked_base
		for arg in syntax.generic_app_args() {
			locked := t.lock_colliding_main_generic_type_text(arg, module_name)
			if locked != transform_trimmed_type_id(arg) { changed = true }
			parts << locked
		}
		if changed { return flat.source_type_composition(.generic, parts) }
		return clean
	}
	if shape.has(.dotted) { return clean }
	// Resolve declaration IDs after numeric syntax and recursive type handling finish.
	main_name := flat.source_type_composition_two(.qualified, generic_main_module_id, clean)
	if flat.source_type_query(clean).has(.generic_fn_name) {
		if info := t.structs[clean] {
			if info.module in ['', 'main'] { return main_name }
		}
	}
	qualified := flat.source_type_composition_two(.qualified, flat.source_type_id(module_name), clean)
	// Active arguments carry caller provenance, including aliases whose targets
	// contain a colliding main type.
	if t.active_specialization_main_types[clean] {
		mut alias_target_needs_lock := false
		if !isnil(t.tc) {
			if target := t.tc.type_aliases.type_id(clean) {
				alias_target_needs_lock = target != clean && t.lock_colliding_main_generic_type_text(target, module_name) != target
			}
		}
		if qualified in t.structs || t.sum_types.contains(qualified) || qualified in t.enum_types || (!isnil(t.tc) && (qualified in t.tc.struct_generic_params || qualified in t.tc.sum_generic_params)) || t.type_short_name_has_non_main_owner(clean) || alias_target_needs_lock {
			return main_name
		}
	}
	if t.main_alias_needs_lock_id(clean, module_name) { return main_name }
	if !(clean in t.structs || t.sum_types.contains(clean) || clean in t.enum_types) {
		return clean
	}
	if info := t.structs[clean] {
		if info.module in ['', 'main'] {
			if !isnil(t.tc) && clean in t.tc.struct_generic_params && qualified in t.tc.struct_generic_params {
				return clean
			}
			if qualified in t.structs || t.sum_types.contains(qualified) || qualified in t.enum_types || t.type_short_name_has_non_main_owner(clean) {
				return main_name
			}
			return clean
		}
	}
	// A type substituted from main may need a lock despite no callee homonym:
	// another imported module can make its short name ambiguous at C generation.
	if !isnil(t.tc) && clean in t.structs {
		decl_module := t.tc.struct_modules[clean] or { '' }
		if decl_module !in ['', 'main'] { return clean }
		if decl_module.len == 0 && clean in t.tc.struct_generic_params && qualified in t.tc.struct_generic_params {
			return clean
		}
	}
	if qualified in t.structs || t.sum_types.contains(qualified) || qualified in t.enum_types {
		return main_name
	}
	return clean
}

fn (t &Transformer) lock_colliding_main_generic_type_text_bytes(typ string, module_name string) string {
	return flat.source_type_name(t.lock_colliding_main_generic_type_text(flat.source_type_id(typ), module_name))
}

// main_alias_needs_lock reports whether a bare program alias must be spelled `main.X`
// in `module_name`: its target is resolved where the alias is parsed, so a `Cells`
// alias of `[]Cell` would otherwise become `[]module_name.Cell` there.
fn (t &Transformer) main_alias_needs_lock(name string, module_name string) bool {
	return t.main_alias_needs_lock_id(flat.source_type_id(name), module_name)
}

fn (t &Transformer) main_alias_needs_lock_id(name_id u32, module_name string) bool {
	if isnil(t.tc) { return false }
	alias_module := t.tc.type_alias_modules[name_id] or { return false }
	if alias_module !in ['', 'main'] { return false }
	target := t.tc.type_aliases.type_id(name_id) or { return false }
	qualified := flat.source_type_composition_two(.qualified, flat.source_type_id(module_name), name_id)
	return t.tc.type_aliases.contains(qualified) || qualified in t.structs || t.sum_types.contains(qualified) || qualified in t.enum_types || (target != name_id && t.lock_colliding_main_generic_type_text(target, module_name) != target)
}

fn (t &Transformer) type_short_name_has_non_main_owner(name_id u32) bool {
	if t.non_main_type_index_ready { return t.non_main_type_short_names[name_id] }
	// Small fixture transformers may run before the numeric short-name index exists.
	for candidate_id, info in t.structs {
		syntax := flat.source_type_syntax(candidate_id)
		owner_id := if info.module.len > 0 {
			flat.source_type_id(info.module)
		} else {
			syntax.qualifier()
		}
		if flat.source_type_shape(candidate_id).has(.dotted) && syntax.unqualified() == name_id && owner_id !in [
			u32(0),
			generic_main_module_id,
			flat.source_type_id('builtin'),
		] {
			return true
		}
	}
	for candidate_id, _ in t.sum_types.ids {
		syntax := flat.source_type_syntax(candidate_id)
		if flat.source_type_shape(candidate_id).has(.dotted) && syntax.unqualified() == name_id && syntax.qualifier() !in [
			u32(0),
			generic_main_module_id,
			flat.source_type_id('builtin'),
		] {
			return true
		}
	}
	for candidate_id, _ in t.enum_types {
		syntax := flat.source_type_syntax(candidate_id)
		if flat.source_type_shape(candidate_id).has(.dotted) && syntax.unqualified() == name_id && syntax.qualifier() !in [
			u32(0),
			generic_main_module_id,
			flat.source_type_id('builtin'),
		] {
			return true
		}
	}
	return false
}

fn (t &Transformer) substituted_type_belongs_to_main_generic(typ_id u32) bool {
	clean := transform_trimmed_type_id(typ_id)
	components := generic_type_container_components(clean)
	if components.len > 0 {
		for component in components {
			if t.substituted_type_belongs_to_main_generic(component) { return true }
		}
		return false
	}
	syntax := flat.source_type_syntax(clean)
	base := syntax.generic_app_base()
	args := syntax.generic_app_args()
	local_base := if syntax.generic_app_valid() { base } else { clean }
	if !isnil(t.tc) && !flat.source_type_shape(local_base).has(.dotted) && t.cur_module.len > 0 && t.cur_module !in [
		'main',
		'builtin',
	] && !t.active_specialization_main_types[local_base] {
		qualified := flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), local_base)
		if qualified in t.structs || t.sum_types.contains(qualified) || qualified in t.enum_types || qualified in t.tc.struct_generic_params || qualified in t.tc.sum_generic_params {
			// Bare types in imported declarations belong to that declaration unless an
			// active specialization argument carries explicit main-module provenance.
			return false
		}
	}
	if syntax.generic_app_valid() && flat.source_type_shape(base).has(.dotted) {
		for arg in args {
			if t.substituted_type_belongs_to_main_generic(arg) && t.current_specialization_has_generic_arg(arg) {
				return true
			}
		}
	}
	if !flat.source_type_shape(clean).has(.dotted) {
		if clean in t.structs || t.sum_types.contains(clean) || clean in t.enum_types {
			return true
		}
	}
	if !syntax.generic_app_valid() || flat.source_type_shape(base).has(.dotted) || isnil(t.tc) {
		return false
	}
	if base !in t.tc.struct_generic_params { return false }
	decl_module := t.tc.struct_modules[base] or { '' }
	return decl_module.len == 0 || decl_module == 'main'
}

fn (mut t Transformer) subst_comptime_type_condition(cond string, args_ids []u32) string {
	mut clean := trimmed_transform_text(cond).replace('sizeof (', 'sizeof(').replace('typeof (', 'typeof(').replace('int (', 'int(')
	clean = t.subst_comptime_runtime_type_metadata(clean, args_ids)
	for i, param in t.active_generic_params {
		if i >= args_ids.len {
			break
		}
		clean = clean.replace('${param}.indirections', generic_type_indirections(args_ids[i]).str())
	}
	if clean.starts_with('(') {
		end := comptime_condition_matching_paren(clean, 0)
		if end == clean.len - 1 {
			inner := t.subst_comptime_type_condition(clean[1..clean.len - 1], args_ids)
			if inner in ['true', 'false'] {
				return inner
			}
			return '(${inner})'
		}
	}
	or_idx := comptime_condition_top_level_index(clean, '||')
	if or_idx >= 0 {
		left := t.subst_comptime_type_condition(clean[..or_idx], args_ids)
		if left == 'true' {
			return 'true'
		}
		right := t.subst_comptime_type_condition(clean[or_idx + 2..], args_ids)
		if right == 'true' {
			return 'true'
		}
		if left == 'false' {
			return right
		}
		if right == 'false' {
			return left
		}
		return '${left} || ${right}'
	}
	and_idx := comptime_condition_top_level_index(clean, '&&')
	if and_idx >= 0 {
		left := t.subst_comptime_type_condition(clean[..and_idx], args_ids)
		if left == 'false' {
			return 'false'
		}
		right := t.subst_comptime_type_condition(clean[and_idx + 2..], args_ids)
		if right == 'false' {
			return 'false'
		}
		if left == 'true' {
			return right
		}
		if right == 'true' {
			return left
		}
		return '${left} && ${right}'
	}
	for op in [' !is ', ' is '] {
		op_idx := comptime_condition_top_level_index(clean, op)
		if op_idx >= 0 {
			left := trimmed_transform_text(clean[..op_idx])
			right := trimmed_transform_text(clean[op_idx + op.len..])
			result := '${t.subst_comptime_type_test_operand(left, args_ids)}${op}${t.subst_comptime_type_test_operand(right, args_ids)}'
			// `T is &U` introduces `U` as the pointee type. Keep the specialized
			// condition for the clone pass to bind instead of deciding that the
			// otherwise-unknown `U` makes the comparison false.
			if op == ' is ' {
				if _, _ := comptime_pointer_type_binding(result) {
					return result
				}
			}
			if t.cloning_comptime_for_depth == 0 {
				if value := t.comptime_type_condition_value(result) {
					return value.str()
				}
			}
			return result
		}
	}
	for op in [' !in', ' in'] {
		op_idx := comptime_condition_top_level_index(clean, op)
		if op_idx >= 0 {
			after := op_idx + op.len
			if after >= clean.len || (clean[after] != `[` && clean[after] != ` `) {
				continue
			}
			left := t.subst_comptime_type_test_operand(clean[..op_idx], args_ids)
			list := trimmed_transform_text(clean[after..])
			if list.starts_with('[') && list.ends_with(']') {
				mut items := []string{}
				for item in split_generic_args(list[1..list.len - 1]) {
					items << t.subst_comptime_type_operand(item, args_ids)
				}
				result := '${left}${op}[${items.join(',')}]'
				if t.cloning_comptime_for_depth == 0 {
					if value := t.comptime_type_condition_value(result) {
						return value.str()
					}
				}
				return result
			}
		}
	}
	for op in [' != ', ' == ', ' <= ', ' >= ', ' < ', ' > '] {
		op_idx := comptime_condition_top_level_index(clean, op)
		if op_idx >= 0 {
			left := t.subst_comptime_type_operand(clean[..op_idx], args_ids)
			right := t.subst_comptime_type_operand(clean[op_idx + op.len..], args_ids)
			result := '${left}${op}${right}'
			if t.cloning_comptime_for_depth == 0 {
				if value := t.comptime_type_condition_value(result) {
					return value.str()
				}
			}
			return result
		}
	}
	if clean.starts_with('!') {
		inner_raw := trimmed_transform_text(clean[1..])
		inner := t.subst_comptime_type_condition(inner_raw, args_ids)
		if inner == 'true' {
			return 'false'
		}
		if inner == 'false' {
			return 'true'
		}
		if inner_raw.starts_with('(') {
			return '!(${inner})'
		}
		return '!${inner}'
	}
	return clean
}

fn (t &Transformer) subst_comptime_runtime_type_metadata(cond string, args_ids []u32) string {
	mut result := cond
	mut offset := 0
	for offset < result.len {
		relative := result[offset..].index('typeof(') or { break }
		start := offset + relative
		open := start + 'typeof'.len
		close := comptime_condition_matching_paren(result, open)
		if close >= result.len {
			break
		}
		inner := trimmed_transform_text(result[open + 1..close])
		raw_type_source_id := t.raw_var_type(inner)

		if raw_type_source_id == 0 {
			offset = close + 1
			continue
		}
		concrete_id := t.resolve_substituted_type_text(t.subst_type(raw_type_source_id, args_ids))
		mut suffix := ''
		mut replacement := ''
		if result[close + 1..].starts_with('.indirections') {
			suffix = '.indirections'
			replacement = generic_type_indirections(concrete_id).str()
		} else if result[close + 1..].starts_with('.idx') {
			suffix = '.idx'
			replacement = t.type_index(concrete_id, t.cur_module).str()
		} else {
			offset = close + 1
			continue
		}
		result = result[..start] + replacement + result[close + 1 + suffix.len..]
		offset = start + replacement.len
	}
	return result
}

fn generic_arg_type_for_param(param_type_id u32, arg_type_id u32) u32 {
	mut actual := flat.source_type_syntax(arg_type_id).trimmed()
	param := flat.source_type_syntax(param_type_id).trimmed()
	if flat.source_type_shape(actual).has(.shared_type) && !flat.source_type_shape(param).has(.shared_type) {
		actual = flat.source_type_syntax(flat.source_type_syntax(actual).raw_base()).trimmed()
	}
	if flat.source_type_shape(actual).has(.atomic_type) && !flat.source_type_shape(param).has(.atomic_type) {
		actual = flat.source_type_syntax(flat.source_type_syntax(actual).raw_base()).trimmed()
	}
	return actual
}

fn generic_inference_param_type(param &flat.Node) u32 {
	mut clean := flat.source_type_syntax(param.typ).trimmed()
	for flat.source_type_shape(clean).has(.shared_type) || flat.source_type_shape(clean).has(.atomic_type) {
		clean = flat.source_type_syntax(flat.source_type_syntax(clean).raw_base()).trimmed()
	}
	// `mut value T` is represented as `&T` for storage, so that synthetic
	// pointer must not become part of T. An explicitly declared `value &T`
	// keeps its pointer: inferring it from an `&int` argument must bind T to
	// `int`, not `&int`.
	if param.is_mut && param.op != .amp && flat.source_type_shape(clean).has(.pointer) {
		return flat.source_type_syntax(clean).raw_base()
	}
	if flat.source_type_shape(clean).has(.mutable_type) {
		return flat.source_type_syntax(flat.source_type_syntax(clean).raw_base()).trimmed()
	}
	return clean
}

fn (t &Transformer) generic_inference_expected_type(typ_id u32) u32 {
	if source_source_id := t.generic_specialized_source_type_name(typ_id) {
		return source_source_id
	}
	return typ_id
}

fn generic_type_indirections(typ_id u32) int {
	mut clean := flat.source_type_syntax(typ_id).trimmed()
	// An option or result wrapper is not itself a level of indirection, but it does
	// not hide the pointer inside it either: `?&Tag` reports one, the same as the
	// `$for field` metadata does. Without this a `$if T.indirections != 0` branch is
	// never taken for an option-of-pointer generic argument.
	for flat.source_type_shape(clean).has(.optional) {
		clean = flat.source_type_syntax(flat.source_type_syntax(clean).raw_base()).trimmed()
	}
	mut count := 0
	if flat.source_type_shape(clean).has(.shared_type) {
		count++
		clean = flat.source_type_syntax(flat.source_type_syntax(clean).raw_base()).trimmed()
	}
	return count + flat.source_type_syntax(clean).pointer_depth()
}

fn (t &Transformer) subst_comptime_type_operand(raw string, args_ids []u32) string {
	clean := trimmed_transform_text(raw)
	// Metadata such as `T.indirections` is replaced with a literal before the
	// comparison operands are normalized. Do not feed that literal back through
	// type resolution, where an imported module can turn `0` into `json2.0`.
	if comptime_is_int(clean) || clean in ['true', 'false'] {
		return clean
	}
	if clean.len >= 2 && clean[0] in [`'`, `"`, `\``] && clean[clean.len - 1] == clean[0] {
		return clean
	}
	if clean.starts_with('\$') {
		// `$int`, `$struct`, ... are metatype keywords, not type names; they must
		// not be substituted or module-qualified (`mymod.$int` breaks matching).
		return clean
	}
	if clean.starts_with('sizeof(') && clean.ends_with(')') {
		inner := clean['sizeof('.len..clean.len - 1].trim_space()
		concrete_id := t.resolve_substituted_type_text(t.subst_type(flat.source_type_id(inner), args_ids))
		if layout := t.comptime_type_layout(concrete_id, []u32{}) {
			return layout.size.str()
		}
	}
	if reflected_type, reflected_member := generic_comptime_typeof_operand(clean) {
		substituted_id := t.resolve_substituted_type_text(t.subst_type(flat.source_type_id(reflected_type), args_ids))
		return match reflected_member {
			'idx' { t.type_index(substituted_id, t.cur_module).str() }
			'unaliased_typ' {
				flat.source_type_name(t.comptime_typeof_unaliased_type(substituted_id))
			}
			'typ' { flat.source_type_name(substituted_id) + '.typ' }
			else { flat.source_type_name(substituted_id) }
		}
	}
	if t.cloning_comptime_for_depth > 0 {
		for loop_var in t.cloning_comptime_for_vars {
			if clean == loop_var || clean.starts_with(loop_var + '.') {
				// Preserve the loop variable and its metadata selectors until the
				// comptime loop is unrolled for its concrete field or variant.
				return clean
			}
		}
	}
	if t.cloning_comptime_for_depth > 0 && !clean.contains('.') && (t.raw_var_type(clean)) != 0 {
		// A runtime value can be smartcast by the current comptime loop variant.
		// Keep its name until the loop body is cloned so nested `$if value is
		// $type_group` conditions see the variant type, not the unspecialized
		// outer sum type.
		return clean
	}
	if !clean.contains('.') {
		var_type_source_id := t.raw_var_type(clean)
		if var_type_source_id != 0 {
			value_type := if t.mut_param_values[clean] {
				transform_without_pointer_prefix(var_type_source_id)
			} else {
				var_type_source_id
			}
			return flat.source_type_name(t.resolve_substituted_type_text(t.subst_type(value_type, args_ids)))
		}
		if comptime_condition_is_unresolved_value_ident(clean) {
			return clean
		}
	}
	payload_suffix := '.unaliased_typ.payload_type'
	if clean.ends_with(payload_suffix) {
		base := clean[..clean.len - payload_suffix.len]
		substituted := t.comptime_normalize_type_alias_chain(t.resolve_substituted_type_text(t.subst_type(flat.source_type_id(base), args_ids)))
		if flat.source_type_shape(substituted).has(.optional) {
			payload := flat.source_type_syntax(flat.source_type_syntax(substituted).raw_base()).trimmed()
			return t.type_index(payload, t.cur_module).str()
		}
		return '0'
	}
	if clean.ends_with('.unaliased_typ') {
		base := clean[..clean.len - '.unaliased_typ'.len]
		substituted_id := t.resolve_substituted_type_text(t.subst_type(flat.source_type_id(base), args_ids))
		return flat.source_type_name(t.comptime_typeof_unaliased_type(substituted_id))
	}
	if clean.ends_with('.typ') {
		base := clean[..clean.len - '.typ'.len]
		if base.len > 0 {
			// Substitute the base but keep the `.typ_text()` selector: subst_type does
			// not recognize the selector form, and evaluation uses the suffix
			// to preserve alias identity (`MyAlias.typ is string` is false).
			return flat.source_type_name(t.subst_type(flat.source_type_id(base), args_ids)) + '.typ'
		}
	}
	return flat.source_type_name(t.resolve_substituted_type_text(t.subst_type(flat.source_type_id(clean), args_ids)))
}

fn (t &Transformer) subst_comptime_type_test_operand(raw string, args_ids []u32) string {
	if reflected_type, reflected_member := generic_comptime_typeof_operand(raw) {
		if reflected_member == 'idx' {
			substituted := flat.source_type_name(t.resolve_substituted_type_text(t.subst_type(flat.source_type_id(reflected_type), args_ids)))
			// In a type test, `.idx` denotes the reflected type rather than the
			// integer value of its runtime type index. Preserve aliases just as
			// `.typ_text()` does; ordinary comparisons still receive the numeric index.
			return substituted + '.typ'
		}
	}
	substituted := t.subst_comptime_type_operand(raw, args_ids)
	if t.current_specialization_has_generic_arg(flat.source_type_id(substituted)) {
		return t.lock_colliding_main_generic_type_text_bytes(substituted, t.cur_module)
	}
	return substituted
}

fn (t &Transformer) comptime_type_layout(raw_id u32, seen_ids []u32) ?ComptimeTypeLayout {
	if isnil(t.tc) {
		return none
	}
	typ := t.tc.parse_type(raw_id)
	return match typ {
		types.Primitive {
			size := int(typ.size) / 8
			if size <= 0 {
				none
			} else {
				ComptimeTypeLayout{size, size}
			}
		}
		types.Char {
			ComptimeTypeLayout{1, 1}
		}
		types.Rune {
			ComptimeTypeLayout{4, 4}
		}
		types.ISize, types.USize, types.Pointer, types.FnType, types.Channel {
			ComptimeTypeLayout{8, 8}
		}
		types.ArrayFixed {
			elem := t.comptime_type_layout(typ.elem_type.source_type_id(), seen_ids) or { return none }
			ComptimeTypeLayout{elem.size * typ.len, elem.align}
		}
		types.Alias {
			t.comptime_type_layout(typ.base_type.source_type_id(), seen_ids)
		}
		types.Struct {
			if typ.source_type_id() in seen_ids {
				return none
			}
			fields := t.tc.structs[typ.source_type_id()] or { return none }
			mut next_seen := seen_ids.clone()
			next_seen << typ.source_type_id()
			mut size := 0
			mut align := 1
			for field in fields {
				field_layout := t.comptime_type_layout(field.typ.source_type_id(), next_seen) or {
					return none
				}
				align = int_max(align, field_layout.align)
				size = comptime_align_size(size, field_layout.align) + field_layout.size
			}
			ComptimeTypeLayout{comptime_align_size(size, align), align}
		}
		else {
			none
		}
	}
}

fn comptime_align_size(size int, alignment int) int {
	if alignment <= 1 {
		return size
	}
	return (size + alignment - 1) / alignment * alignment
}

fn generic_comptime_typeof_operand(raw string) ?(string, string) {
	clean := trimmed_transform_text(raw)
	if !clean.starts_with('typeof[') {
		return none
	}
	open := 'typeof'.len
	close := generic_matching_bracket(clean, open)
	if close >= clean.len {
		return none
	}
	type_name := trimmed_transform_text(clean[open + 1..close])
	if type_name.len == 0 {
		return none
	}
	mut suffix := clean[close + 1..].replace(' ', '')
	if suffix.starts_with('()') {
		suffix = suffix[2..]
	}
	member := if suffix.starts_with('.') { suffix[1..] } else { suffix }
	if member !in ['', 'idx', 'typ', 'unaliased_typ'] {
		return none
	}
	return type_name, member
}

// active_generic_param_index returns the position of a placeholder name within the
// active generic params, or the positional fallback when none are active.
fn (t &Transformer) active_generic_param_index(name string) int {
	for i, p in t.active_generic_params {
		if p == name {
			return i
		}
	}
	return generic_param_index(name)
}

fn specialized_generic_fn_value(value string, args_ids []u32) string {
	value_id := flat.source_type_id(value)
	if flat.source_type_shape(value_id).has(.dotted) {
		value_syntax := flat.source_type_syntax(value_id)
		syntax := flat.source_type_syntax(value_syntax.qualifier())
		receiver := if syntax.generic_app_valid() {
			syntax.generic_app_base()
		} else {
			value_syntax.qualifier()
		}
		mut parts := [receiver]
		for arg in args_ids { parts << generic_type_arg_short_id(arg) }
		return flat.source_type_name(flat.source_type_composition_two(.qualified,
			flat.source_type_composition(.generic, parts), value_syntax.unqualified()))
	}
	// Free-function specializations need the canonical module-qualified type
	// arguments in their identity. Using only the last type component made
	// `keep[a.tast.Value]` and `keep[b.tast.Value]` both become `keep_T_Value`,
	// so one body/prototype was reused with the other module's ABI type.
	return '${value}_T_${generic_type_full_suffixes(args_ids)}'
}

fn generic_type_args_short(args_ids []u32) string {
	mut parts := []string{cap: args_ids.len}
	for arg in args_ids { parts << flat.source_type_name(generic_type_arg_short_id(arg)) }
	return parts.join(', ')
}

fn generic_type_arg_short(type_arg string) string {
	return flat.source_type_name(generic_type_arg_short_id(flat.source_type_id(type_arg)))
}

fn generic_type_arg_short_id(type_arg u32) u32 {
	clean := transform_trimmed_type_id(type_arg)
	shape := flat.source_type_shape(clean)
	syntax := flat.source_type_syntax(clean)
	if shape.has(.function_compact) || shape.has(.function_spaced) {
		if syntax.function_closed() {
			mut normalized_params := []u32{cap: syntax.function_params().len + 1}
			for param in syntax.function_params() {
				mut payload := flat.source_type_syntax(param).generic_parameter_payload()
				if flat.source_type_shape(param).has(.mutable_type) && !flat.source_type_shape(payload).has(.pointer) {
					payload = flat.source_type_composition_one(.pointer, payload)
				}
				normalized_params << payload
			}
			mut normalized := u32(0)
			if syntax.function_result() != 0 {
				normalized_params << syntax.function_result()
				normalized = flat.source_type_composition(.function_type_spaced, normalized_params)
			} else {
				normalized = flat.source_type_composition(.void_function_type_spaced, normalized_params)
			}
			// The final C identifier renderer consumes spelling bytes at this boundary.
			return flat.source_type_id(sanitize_full_type_name_fragment(flat.source_type_name(normalized)))
		}
	}
	if shape.has(.array_type) {
		return flat.source_type_composition(.concat, [flat.source_type_id('Array_'),
			generic_type_arg_short_id(syntax.raw_base())])
	}
	if shape.has(.pointer) {
		return flat.source_type_composition(.concat, [flat.source_type_id('ptr_'),
			generic_type_arg_short_id(syntax.raw_base())])
	}
	if shape.has(.map_type) && syntax.bracket_closed() && syntax.bracket_suffix() != 0 {
		return flat.source_type_composition(.concat, [flat.source_type_id('Map_'),
			generic_type_arg_short_id(syntax.bracket_raw_inner()), flat.source_type_id('_'),
			generic_type_arg_short_id(syntax.bracket_suffix())])
	}
	if shape.has(.optional) {
		prefix := if shape.has(.option_type) {
			flat.source_type_id('Option_')
		} else {
			flat.source_type_id('Result_')
		}
		return flat.source_type_composition(.concat, [prefix,
			generic_type_arg_short_id(syntax.raw_base())])
	}
	if fixed := generic_fixed_array_type_arg_short_id(clean) { return fixed }
	// Compound types must retain qualified members when rendered as C fragments.
	if syntax.opening_paren() || syntax.spaced() {
		return flat.source_type_id(sanitize_full_type_name_fragment(flat.source_type_name(clean)))
	}
	if syntax.generic_app_valid() {
		mut parts := []u32{cap: syntax.generic_app_args().len * 2 + 1}
		parts << generic_type_arg_short_id(syntax.generic_app_base())
		for arg in syntax.generic_app_args() {
			parts << flat.source_type_id('_')
			parts << generic_type_arg_short_id(arg)
		}
		return flat.source_type_composition(.concat, parts)
	}
	return clean
}

fn generic_fixed_array_type_arg_short(type_arg string) ?string {
	return flat.source_type_name(generic_fixed_array_type_arg_short_id(flat.source_type_id(type_arg))?)
}

fn generic_fixed_array_type_arg_short_id(type_arg u32) ?u32 {
	clean := transform_trimmed_type_id(type_arg)
	if !flat.source_type_shape(clean).has(.leading_bracket) { return none }
	syntax := flat.source_type_syntax(clean)
	if !syntax.first_bracket_closed() || syntax.first_bracket_raw_inner() == 0 || syntax.first_bracket_base() == 0 {
		return none
	}
	length := syntax.first_bracket_length()
	element := transform_trimmed_type_id(syntax.first_bracket_base())
	if length == 0 || element == 0 { return none }
	return flat.source_type_composition(.concat, [generic_type_arg_short_id(element),
		flat.source_type_id('_'), length])
}

// sanitize_type_name_fragment reduces an arbitrary type text (notably a function
// type) to a deterministic fragment that is a valid C identifier piece: module
// qualifiers are dropped, `[]`/`&` become readable prefixes, and every other run
// of non-identifier characters collapses to a single underscore.
fn sanitize_full_type_name_fragment(typ string) string {
	return sanitize_type_name_fragment_impl(typ, true)
}

fn sanitize_type_name_fragment_impl(typ string, preserve_modules bool) string {
	mut out := []u8{}
	mut prev_us := false
	mut i := 0
	for i < typ.len {
		c := typ[i]
		if (c >= `A` && c <= `Z`) || (c >= `a` && c <= `z`) || (c >= `0` && c <= `9`) {
			out << c
			prev_us = false
			i++
		} else if c == `[` && i + 1 < typ.len && typ[i + 1] == `]` {
			for ch in 'Array_'.bytes() {
				out << ch
			}
			prev_us = false
			i += 2
		} else if c == `&` {
			for ch in 'ptr_'.bytes() {
				out << ch
			}
			prev_us = false
			i++
		} else if c == `.` {
			if preserve_modules {
				out << `_`
				out << `_`
				prev_us = false
			} else {
				// Drop the preceding module qualifier (everything emitted since the
				// last separator), keeping only the unqualified name.
				for out.len > 0 {
					last := out[out.len - 1]
					if (last >= `A` && last <= `Z`) || (last >= `a` && last <= `z`)
						|| (last >= `0` && last <= `9`) {
						out.delete_last()
					} else {
						break
					}
				}
			}
			i++
		} else {
			if !prev_us {
				out << `_`
				prev_us = true
			}
			i++
		}
	}
	mut s := out.bytestr()
	for s.starts_with('_') {
		s = s[1..]
	}
	for s.ends_with('_') {
		s = s[..s.len - 1]
	}
	return s
}

fn generic_type_suffixes(args_ids []u32) string {
	mut parts := []string{cap: args_ids.len}
	for arg_source_id in args_ids {
		parts << c_name(flat.source_type_name(generic_type_arg_short_id(arg_source_id)).replace('[]', 'Array_').replace('&', 'ptr_'))
	}
	return parts.join('_')
}

fn generic_type_full_suffixes(args_ids []u32) string {
	mut parts := []string{cap: args_ids.len}
	for arg_source_id in args_ids {
		// Keep module qualification while giving composite types an explicit,
		// round-trippable shape. Raw `c_name(map[string]mod.Type)` concatenates
		// the key and value and cannot be decoded by a later transform stage.
		parts << c_name(flat.source_type_name(generic_type_arg_short_id(arg_source_id)))
	}
	return parts.join('_')
}

fn (mut t Transformer) record_generic_specialization_args_in_module(base string, module_name string, args_ids []u32) {
	clean_base := trimmed_transform_text(base)
	if clean_base.len == 0 || args_ids.len == 0 {
		return
	}
	recorded_args_source_ids := t.canonical_generic_specialization_args(args_ids)
	recorded_args := flat.TypeTextList{ ids: recorded_args_source_ids }.names()
	if t.generic_args_have_placeholders(recorded_args_source_ids) {
		return
	}
	suffix := generic_type_suffixes(recorded_args_source_ids)
	qualified_base := if clean_base.contains('.') || module_name == '' || module_name == 'main'
		|| module_name == 'builtin' {
		clean_base
	} else {
		'${module_name}.${clean_base}'
	}
	mut keys := []SpecializationArgsKey{}
	if clean_base.contains('.') || module_name == '' || module_name == 'main'
		|| module_name == 'builtin' {
		source_name := '${clean_base}[${recorded_args.join(', ')}]'
		keys << SpecializationArgsKey{ type_id: flat.source_type_id(source_name) }
		keys << SpecializationArgsKey{ name: c_name(source_name) }
		keys << SpecializationArgsKey{ name: '${clean_base}_${suffix}' }
		keys << SpecializationArgsKey{ name: c_name('${clean_base}_${suffix}') }
	}
	qualified_source_name := '${qualified_base}[${recorded_args.join(', ')}]'
	keys << SpecializationArgsKey{ type_id: flat.source_type_id(qualified_source_name) }
	keys << SpecializationArgsKey{ name: c_name(qualified_source_name) }
	keys << SpecializationArgsKey{ name: '${qualified_base}_${suffix}' }
	keys << SpecializationArgsKey{ name: c_name('${qualified_base}_${suffix}') }
	if qualified_base.contains('.') {
		base_module := qualified_base.all_before_last('.')
		short_base := qualified_base.all_after_last('.')
		keys << SpecializationArgsKey{ name: '${base_module}.${short_base}_${suffix}' }
		keys << SpecializationArgsKey{ name: c_name('${base_module}.${short_base}_${suffix}') }
	}
	for key in keys {
		if (key.type_id != 0 || key.name.len > 0) && !t.has_recorded_generic_specialization_args_key(key) {
			t.generic_specialization_args.set_key_ids(key, recorded_args_source_ids)
			t.generic_specialization_args_log << key
		}
	}
}

fn (mut t Transformer) record_generic_specialization_args_for_names(names []string, args_ids []u32) {
	if args_ids.len == 0 || t.generic_args_have_placeholders(args_ids) { return }
	// Callers supply the canonical arguments used to form `names`. Resolving
	// them again after switching modules could rebind a caller-owned main type.
	for name in names {
		if name.len > 0 && !t.has_recorded_generic_specialization_args(name) {
			t.generic_specialization_args.set_ids(name, args_ids)
			t.generic_specialization_args_log << SpecializationArgsKey{ name: name }
		}
	}
}

fn (t &Transformer) has_recorded_generic_specialization_args_key(key SpecializationArgsKey) bool {
	if t.generic_specialization_args.contains_key(key) {
		return true
	}
	if isnil(t.generic_specialization_args_parent) {
		return false
	}
	parent := unsafe { *t.generic_specialization_args_parent }
	return parent.contains_key(key)
}

fn (t &Transformer) has_recorded_generic_specialization_args(name string) bool {
	if t.generic_specialization_args.contains(name) {
		return true
	}
	if isnil(t.generic_specialization_args_parent) {
		return false
	}
	parent := unsafe { *t.generic_specialization_args_parent }
	return parent.contains(name)
}

fn (t &Transformer) canonical_generic_specialization_args(args_ids []u32) []u32 {
	mut out := []u32{cap: args_ids.len}
	for arg_id in args_ids {
		out << t.canonical_generic_specialization_arg(arg_id)
	}
	return out
}

fn (t &Transformer) canonical_generic_specialization_arg(arg_id u32) u32 {
	if inferred_type_id(arg_id) {
		resolved := t.resolve_inferred_type_id(arg_id)
		if resolved != arg_id { return t.canonical_generic_specialization_arg(resolved) }
		return arg_id
	}
	clean_id := t.trimmed_type_id(arg_id)
	if clean_id == 0 { return clean_id }
	shape := flat.source_type_shape(clean_id)
	syntax := flat.source_type_syntax(clean_id)
	// main.Type is a temporary disambiguation lock in imported generic bodies.
	// Specialization keys use the program module's normal bare spelling.
	if !t.ident_is_import_alias('main') && syntax.main_unlocked() != clean_id {
		return t.canonical_generic_specialization_arg(syntax.main_unlocked())
	}
	// Recover materialized aggregate spellings before forming specialization keys.
	if source := t.generic_specialized_source_type_name(clean_id) {
		if source != clean_id { return t.canonical_generic_specialization_arg(source) }
	}
	if syntax.map_name_prefix() {
		// This input is an emitted C symbol and is decoded at that boundary.
		if source := t.source_composite_type_from_c_name(flat.source_type_name(clean_id)) {
			return t.canonical_generic_specialization_arg(source)
		}
	}
	// Leading bracket forms still need length normalization or generic recovery.
	if !shape.has(.leading_bracket) && t.substituted_type_belongs_to_main_generic(clean_id)
		&& t.current_specialization_has_generic_arg(clean_id) {
		return clean_id
	}
	mut wrapper := flat.SourceTypeComposition.pointer
	mut wrapped := true
	if shape.has(.pointer) {
		wrapper = .pointer
	} else if shape.has(.mutable_type) {
		wrapper = .mutable_parameter
	} else if shape.has(.shared_type) {
		wrapper = .shared_type
	} else if shape.has(.atomic_type) {
		wrapper = .atomic_type
	} else if shape.has(.option_type) {
		wrapper = .option
	} else if shape.has(.result_type) {
		wrapper = .result
	} else if shape.has(.variadic) {
		wrapper = .variadic_parameter
	} else if shape.has(.array_type) {
		wrapper = .array
	} else {
		wrapped = false
	}
	if wrapped {
		return flat.source_type_composition_one(wrapper,
			t.canonical_generic_specialization_arg(syntax.raw_base()))
	}
	// A lowered fixed-array symbol such as _3int can reach another inference pass.
	if syntax.encoded_fixed_base() != 0 && !t.type_name_is_declared_id(clean_id) {
		encoded_elem := syntax.encoded_fixed_base()
		canonical_elem := t.canonical_generic_specialization_arg(encoded_elem)
		if canonical_elem != encoded_elem || types.is_builtin_type_name_id(encoded_elem)
			|| t.type_name_is_declared_id(encoded_elem) {
			candidate := flat.source_type_composition_two(.fixed_array, syntax.encoded_fixed_length(), canonical_elem)
			if flat.source_type_id(c_name(flat.source_type_name(candidate))) == clean_id {
				return candidate
			}
		}
	}
	// Restore shortened Elem_len symbols while preserving real same-named types.
	if fixed := t.canonical_short_fixed_array_arg_id(clean_id) { return fixed }
	if shape.has(.leading_bracket) && syntax.bracket_closed()
		&& syntax.bracket_raw_inner() != 0 && syntax.bracket_suffix() != 0 {
		len_id := t.trimmed_type_id(syntax.bracket_raw_inner())
		elem_id := t.trimmed_type_id(syntax.bracket_suffix())
		if !flat.source_type_syntax(len_id).decimal() && t.generic_type_base_requires_args(elem_id) {
			return flat.source_type_composition(.generic, [
				t.canonical_generic_specialization_arg(elem_id),
				t.canonical_generic_specialization_arg(len_id),
			])
		}
		mut clean_len := len_id
		if !isnil(t.tc) {
			// Array lengths are source constant expressions, parsed only here.
			if value := t.tc.const_int_value(flat.source_type_name(len_id), []string{}) {
				clean_len = flat.source_type_id(value.str())
			}
		}
		return flat.source_type_composition_two(.fixed_array, clean_len,
			t.canonical_generic_specialization_arg(syntax.bracket_suffix()))
	}
	if fixed := t.canonical_suffix_fixed_array_arg_id(clean_id) { return fixed }
	if syntax.underscored() && flat.source_type_syntax(syntax.last_underscore_suffix()).decimal()
		&& !t.type_name_is_declared_id(clean_id) {
		// Short symbol spellings are decoded at their emitted-name boundary.
		if fixed := t.generic_fixed_array_type_arg_from_short_suffix_id(clean_id) {
			return fixed
		}
	}
	if shape.has(.map_type) && syntax.bracket_closed() && syntax.bracket_suffix() != 0 {
		return flat.source_type_composition_two(.map,
			t.canonical_generic_specialization_arg(syntax.bracket_raw_inner()),
			t.canonical_generic_specialization_arg(syntax.bracket_suffix()))
	}
	if syntax.generic_app_valid() {
		mut parts := []u32{cap: syntax.generic_app_args().len + 1}
		parts << syntax.generic_app_base()
		for nested in syntax.generic_app_args() {
			parts << t.canonical_generic_specialization_arg(nested)
		}
		return flat.source_type_composition(.generic, parts)
	}
	if t.generic_arg_is_alias_name_id(clean_id, '') {
		normalized := t.normalize_type_alias(clean_id)
		if normalized != clean_id && t.is_fixed_array_type(normalized) {
			return t.canonical_generic_specialization_arg(normalized)
		}
		return clean_id
	}
	// Checker-resolved qualified types already have canonical semantic names.
	if shape.has(.dotted) && t.generic_resolved_concrete_name_known(clean_id) { return clean_id }
	if shape.has(.dotted) {
		// Import declarations are keyed by module names, resolved at this boundary.
		alias := flat.source_type_name(syntax.first_dot_prefix())
		resolved_mod := t.import_alias_module(alias)
		if resolved_mod != alias && resolved_mod.len > 0 {
			resolved := flat.source_type_composition_two(.qualified, flat.source_type_id(resolved_mod), syntax.first_dot_suffix())
			normalized := t.normalize_type_alias(resolved)
			if normalized != resolved { return t.canonical_generic_specialization_arg(normalized) }
			return resolved
		}
		if !t.type_name_is_declared_id(clean_id) {
			if qualified := t.qualified_types.type_id(syntax.unqualified()) {
				return qualified
			}
		}
	}
	normalized := t.normalize_type_alias(clean_id)
	if normalized != clean_id { return t.canonical_generic_specialization_arg(normalized) }
	if flat.source_type_query(clean_id).has(.c_array_prefix) {
		if decoded_array := t.generic_array_type_arg_from_suffix_id(clean_id) {
			return decoded_array
		}
	}
	return clean_id
}

fn (t &Transformer) generic_fixed_array_type_arg_from_short_suffix_id(suffix_id u32) ?u32 {
	clean := t.trimmed_type_id(suffix_id)
	syntax := flat.source_type_syntax(clean)
	if !syntax.underscored() || syntax.last_underscore_prefix() == 0
		|| !flat.source_type_syntax(syntax.last_underscore_suffix()).decimal() {
		return none
	}
	elem := t.generic_type_arg_from_suffix_with_containers_id(syntax.last_underscore_prefix())
	if elem == 0 { return none }
	candidate := flat.source_type_composition_two(.fixed_array, syntax.last_underscore_suffix(), elem)
	if generic_type_arg_short_id(candidate) == clean { return candidate }
	return none
}

fn (t &Transformer) generic_parameter_type_name_count() int {
	if isnil(t.tc) { return 0 }
	return t.tc.struct_generic_params.len + t.tc.sum_generic_params.len + t.tc.type_alias_generic_params.len
}

fn (t &Transformer) generic_type_base_requires_args(typ_id u32) bool {
	clean_id := t.trimmed_type_id(typ_id)
	if clean_id == 0 || flat.source_type_shape(clean_id).has(.bracketed) || isnil(t.tc) {
		return false
	}
	short_id := flat.source_type_syntax(clean_id).unqualified()
	count := t.generic_parameter_type_name_count()
	if t.generic_type_indexes_ready && t.generic_parameter_type_index_count == count {
		return t.generic_parameter_type_names[clean_id] || t.generic_parameter_type_names[short_id]
	}
	return t.generic_parameter_type_name_known(clean_id)
		|| t.generic_parameter_type_name_known(short_id)
}

fn (t &Transformer) generic_parameter_type_name_known(name_id u32) bool {
	return name_id in t.tc.struct_generic_params || name_id in t.tc.sum_generic_params
		|| name_id in t.tc.type_alias_generic_params
}

fn strip_main_type_locks(typ string) string {
	if !typ.contains('main.') {
		return typ
	}
	mut out := strings.new_builder(typ.len)
	mut start := 0
	mut i := 0
	for i + 'main.'.len <= typ.len {
		if typ[i..].starts_with('main.') && (i == 0 || !is_type_identifier_byte(typ[i - 1])) {
			out.write_string(typ[start..i])
			i += 'main.'.len
			start = i
			continue
		}
		i++
	}
	out.write_string(typ[start..])
	return out.str()
}

fn is_type_identifier_byte(ch u8) bool {
	return (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`)
		|| (ch >= `0` && ch <= `9`) || ch in [`_`, `.`]
}

fn (t &Transformer) source_composite_type_from_c_name(encoded string) ?u32 {
	for node_id in 0 .. t.a.nodes.len {
		// This scan does not modify the AST arena.
		node := unsafe { &t.a.nodes[node_id] }
		if source := source_composite_type_candidate(encoded, node.typ) {
			return source
		}
	}
	for _, info in t.structs {
		for field in info.fields {
			if source := source_composite_type_candidate(encoded, field.raw_typ) {
				return source
			}
			if source := source_composite_type_candidate(encoded, field.typ) {
				return source
			}
		}
	}
	return none
}

fn source_composite_type_candidate(encoded string, raw_id u32) ?u32 {
	clean_id := transform_trimmed_type_id(raw_id)
	if clean_id == 0 { return none }
	shape := flat.source_type_shape(clean_id)
	// This comparison consumes an emitted C symbol, so rendering is a naming
	// boundary; traversal itself stays in source spelling identities.
	if shape.has(.map_type) && c_name(flat.source_type_name(clean_id)) == encoded {
		return clean_id
	}
	if shape.has(.mutable_type) || shape.has(.shared_type) || shape.has(.atomic_type)
		|| shape.has(.variadic) || shape.has(.array_type) || shape.has(.optional)
		|| shape.has(.pointer) || shape.has(.channel_type) {
		return source_composite_type_candidate(encoded, flat.source_type_syntax(clean_id).raw_base())
	}
	if shape.has(.map_type) {
		syntax := flat.source_type_syntax(clean_id)
		if syntax.bracket_closed() && syntax.bracket_suffix() != 0 {
			if source := source_composite_type_candidate(encoded, syntax.bracket_raw_inner()) {
				return source
			}
			return source_composite_type_candidate(encoded, syntax.bracket_suffix())
		}
	}
	return none
}

fn (t &Transformer) canonical_suffix_fixed_array_arg(arg string) ?string {
	return flat.source_type_name(t.canonical_suffix_fixed_array_arg_id(flat.source_type_id(arg))?)
}

fn (t &Transformer) canonical_suffix_fixed_array_arg_id(arg_id u32) ?u32 {
	clean_id := t.trimmed_type_id(arg_id)
	syntax := flat.source_type_syntax(clean_id)
	if !syntax.suffix_bracket_valid() { return none }
	elem_id := t.trimmed_type_id(syntax.last_bracket_prefix())
	len_id := syntax.suffix_bracket_length()
	if elem_id == 0 || len_id == 0 { return none }
	len_syntax := flat.source_type_syntax(len_id)
	if types.is_builtin_type_name_id(len_id) || len_syntax.contains_comma() { return none }
	mut clean_len := len_id
	mut len_resolved := len_syntax.decimal()
	if !len_resolved && !isnil(t.tc) {
		// Constant expressions are parsed at their source expression boundary.
		if value := t.tc.const_int_value(flat.source_type_name(len_id), []string{}) {
			clean_len = flat.source_type_id(value.str())
			len_resolved = true
		}
	}
	if !len_resolved && syntax.generic_app_valid() && !types.is_builtin_type_name_id(elem_id) {
		return none
	}
	return flat.source_type_composition_two(.fixed_array, clean_len,
		t.canonical_generic_specialization_arg(elem_id))
}

fn (t &Transformer) canonical_short_fixed_array_arg(arg string) ?string {
	return flat.source_type_name(t.canonical_short_fixed_array_arg_id(flat.source_type_id(arg))?)
}

fn (t &Transformer) canonical_short_fixed_array_arg_id(arg_id u32) ?u32 {
	clean_id := t.trimmed_type_id(arg_id)
	if clean_id == 0 || t.type_name_is_declared_id(clean_id) { return none }
	syntax := flat.source_type_syntax(clean_id)
	if !syntax.underscored() || syntax.last_underscore_prefix() == 0 || syntax.last_underscore_suffix() == 0 {
		return none
	}
	len_id := syntax.last_underscore_suffix()
	if !flat.source_type_syntax(len_id).decimal() { return none }
	encoded_elem := syntax.last_underscore_prefix()
	canonical_elem := t.canonical_generic_specialization_arg(encoded_elem)
	if canonical_elem == encoded_elem && !types.is_builtin_type_name_id(encoded_elem)
		&& !t.type_name_is_declared_id(encoded_elem) {
		return none
	}
	candidate := flat.source_type_composition_two(.fixed_array, len_id, canonical_elem)
	// This validates an emitted C symbol spelling, not an internal type query.
	encoded := generic_fixed_array_type_arg_short_id(candidate) or { return none }
	if encoded != clean_id { return none }
	return candidate
}

// SpecArgsMissCache keeps misses by source ID for type queries and by declaration
// name for callee queries. Specialization arguments are only ever recorded under new
// keys, so a miss holds until the recorded maps grow or the parent map changes.
@[heap]
struct SpecArgsMissCache {
mut:
	misses      map[u32]bool
	name_misses map[string]bool
	local_len   int = -1
	parent_len  int = -1
	parent      voidptr
}

@[inline]
fn (t &Transformer) specialization_args_miss_cache() &SpecArgsMissCache {
	mut cache := t.spec_args_misses
	if isnil(cache) { return cache }
	parent := voidptr(t.generic_specialization_args_parent)
	parent_len := if isnil(t.generic_specialization_args_parent) {
		-1
	} else {
		t.generic_specialization_args_parent.len
	}
	if cache.local_len != t.generic_specialization_args.len || cache.parent_len != parent_len || cache.parent != parent {
		cache.misses.clear()
		cache.name_misses.clear()
		cache.local_len = t.generic_specialization_args.len
		cache.parent_len = parent_len
		cache.parent = parent
	}
	return cache
}

fn (t &Transformer) recorded_generic_specialization_args(name string) ?[]u32 {
	mut cache := t.specialization_args_miss_cache()
	if !isnil(cache) && cache.name_misses[name] { return none }
	if args := t.recorded_generic_specialization_args_uncached(name) { return args }
	if !isnil(cache) { cache.name_misses[name] = true }
	return none
}

fn (t &Transformer) recorded_type_specialization_args(type_id u32) ?[]u32 {
	mut cache := t.specialization_args_miss_cache()
	if !isnil(cache) && cache.misses[type_id] { return none }
	clean_id := t.trim_all_pointer_type(t.trimmed_type_id(type_id))
	if clean_id != 0 {
		if args := t.recorded_type_specialization_args_exact(clean_id) { return args }
		// The alternatives are actual generated declaration/C spellings.
		if args := t.recorded_generic_specialization_args_fallback(flat.source_type_name(clean_id)) {
			return args
		}
	}
	if !isnil(cache) { cache.misses[type_id] = true }
	return none
}

fn (t &Transformer) recorded_generic_specialization_args_uncached(typ string) ?[]u32 {
	trimmed := trimmed_transform_text(typ)
	clean := if trimmed.starts_with('&') { trimmed.trim_left('&') } else { trimmed }
	if clean.len == 0 {
		return none
	}
	if args_source_ids := t.recorded_generic_specialization_args_exact(clean) {
		return args_source_ids
	}
	return t.recorded_generic_specialization_args_fallback(clean)
}

fn (t &Transformer) recorded_generic_specialization_args_fallback(clean string) ?[]u32 {
	// Only a lookup key: the checker's memo of c_name answers the repeats.
	clean_cname := t.lookup_c_name(clean)
	if clean_cname != clean {
		if args_source_ids := t.recorded_generic_specialization_args_exact(clean_cname) {
			return args_source_ids
		}
	}
	if clean.contains('__') {
		dotted := clean.replace('__', '.')
		if dotted != clean && dotted != clean_cname {
			if args_source_ids := t.recorded_generic_specialization_args_exact(dotted) {
				return args_source_ids
			}
		}
		dotted_cname := t.lookup_c_name(dotted)
		if dotted_cname != clean && dotted_cname != clean_cname && dotted_cname != dotted {
			if args_source_ids := t.recorded_generic_specialization_args_exact(dotted_cname) {
				return args_source_ids
			}
		}
	}
	return none
}

fn (t &Transformer) recorded_type_specialization_args_exact(type_id u32) ?[]u32 {
	if args := t.generic_specialization_args.source_ids(type_id) { return args.clone() }
	if !isnil(t.generic_specialization_args_parent) {
		parent := unsafe { *t.generic_specialization_args_parent }
		if args := parent.source_ids(type_id) { return args.clone() }
	}
	return none
}

fn (t &Transformer) recorded_generic_specialization_args_exact(name string) ?[]u32 {
	if args := t.generic_specialization_args.type_ids(name) {
		return args.clone()
	}
	if !isnil(t.generic_specialization_args_parent) {
		parent := unsafe { *t.generic_specialization_args_parent }
		if args := parent.type_ids(name) {
			return args.clone()
		}
	}
	return none
}

fn generic_param_index(name string) int {
	return match name {
		'T', 'A', 'K', 'X' { 0 }
		'U', 'B', 'V', 'Y' { 1 }
		'C', 'W', 'Z' { 2 }
		else { 0 }
	}
}

fn is_generic_fn_placeholder_name(typ string) bool {
	if typ == '' {
		return false
	}
	if typ.len == 1 {
		return typ[0] >= `A` && typ[0] <= `Z`
	}
	// Fast path: nothing to trim, so a placeholder is exactly one capital
	// letter after the final '.' — no trimmed/suffix copies needed.
	if typ[0] > ` ` && typ[typ.len - 1] > ` ` {
		if typ[typ.len - 1] < `A` || typ[typ.len - 1] > `Z` {
			return false
		}
		return typ[typ.len - 2] == `.`
	}
	clean := trimmed_transform_text(typ)
	if clean.contains('.') {
		return is_generic_fn_placeholder_name(clean.all_after_last('.'))
	}
	return clean.len == 1 && clean[0] >= `A` && clean[0] <= `Z`
}

fn (t &Transformer) generic_args_have_placeholders(args_ids []u32) bool {
	if t.skip_generics { return false }
	for arg_id in args_ids {
		if inferred_type_id(arg_id) || t.generic_arg_is_unresolved(arg_id) { return true }
	}
	return false
}

fn (t &Transformer) generic_arg_is_unresolved(arg_id u32) bool {
	if t.skip_generics {
		// Anonymous literals still need materialization in nongeneric programs.
		// Skipping generic specialization must not make their provisional storage
		// type authoritative over the concrete literal type.
		if !isnil(t.generic_unresolved_cache) {
			mut cache := t.generic_unresolved_cache
			slot := int(arg_id & 1023)
			if cache.anonymous_states[slot] != 0 && cache.anonymous_ids[slot] == arg_id {
				return cache.anonymous_states[slot] > 0
			}
			result := inferred_anonymous_struct_placeholder_id(arg_id)
			cache.anonymous_ids[slot] = arg_id
			cache.anonymous_states[slot] = if result { i8(1) } else { i8(-1) }
			return result
		}
		return inferred_anonymous_struct_placeholder_id(arg_id)
	}
	// Hot: called for every call/assign type text during body transforms, with
	// heavy repetition (a few thousand distinct texts per build). The result is
	// a pure function of the text plus the declared-type tables and cur_module
	// (both fixed during body transforms), memoized per (module, text).
	if !isnil(t.generic_unresolved_cache) {
		mut cache := t.generic_unresolved_cache
		if !same_transform_text(cache.module, t.cur_module) {
			// cur_module can be a view into a scoped worker result. The cache
			// survives that result, so retain its own module spelling.
			cache.module = t.cur_module.clone()
			cache.entries.clear()
			cache.last_name = 0
			cache.last_value = 0
		}
		cached := cache.get(arg_id)
		if cached != 0 {
			return cached > 0
		}
		result := t.generic_arg_is_unresolved_uncached(arg_id)
		cache.put(arg_id, if result { i8(1) } else { i8(-1) })
		return result
	}
	return t.generic_arg_is_unresolved_uncached(arg_id)
}

fn inferred_anonymous_struct_placeholder(typ string) bool {
	return inferred_anonymous_struct_placeholder_id(flat.source_type_id(typ))
}

fn inferred_anonymous_struct_placeholder_id(typ_id u32) bool {
	clean_id := transform_trimmed_type_id(typ_id)
	shape := flat.source_type_shape(clean_id)
	if shape.has(.array_type) || shape.has(.pointer) || shape.has(.optional) {
		return inferred_anonymous_struct_placeholder_id(flat.source_type_syntax(clean_id).raw_base())
	}
	if shape.has(.map_type) {
		syntax := flat.source_type_syntax(clean_id)
		if syntax.bracket_closed() {
			return inferred_anonymous_struct_placeholder_id(syntax.bracket_raw_inner())
				|| inferred_anonymous_struct_placeholder_id(syntax.raw_base())
		}
	}
	if shape.has(.leading_bracket) {
		syntax := flat.source_type_syntax(clean_id)
		if syntax.bracket_closed() {
			return inferred_anonymous_struct_placeholder_id(syntax.raw_base())
		}
	}
	if !shape.has(.dotted) { return clean_id == generic_anonymous_struct_type_id }
	return flat.source_type_syntax(clean_id).unqualified() == generic_anonymous_struct_type_id
}

fn (t &Transformer) generic_arg_is_unresolved_uncached(arg_id u32) bool {
	clean_id := t.trimmed_type_id(arg_id)
	if clean_id == 0 { return true }
	syntax := flat.source_type_syntax(clean_id)
	if syntax.unqualified() in [flat.source_type_unknown, flat.source_type_void,
		flat.source_type_generic, generic_anonymous_struct_type_id] {
		return true
	}
	if syntax.tuple_like() {
		// Tuple checks use the function-parameter splitter, which skips empty parts.
		if syntax.tuple_function_components().len == 0 { return true }
		for item in syntax.tuple_function_components() {
			if t.generic_arg_is_unresolved(item) { return true }
		}
		return false
	}
	shape := flat.source_type_shape(clean_id)
	if shape.has(.pointer) || shape.has(.optional) || shape.has(.variadic)
		|| shape.has(.array_type) {
		return t.generic_arg_is_unresolved(syntax.raw_base())
	}
	if shape.has(.leading_bracket) && syntax.bracket_closed() {
		return t.generic_arg_is_unresolved(syntax.raw_base())
	}
	if shape.has(.map_type) && syntax.bracket_closed() {
		return t.generic_arg_is_unresolved(syntax.bracket_raw_inner())
			|| t.generic_arg_is_unresolved(syntax.raw_base())
	}
	if flat.source_type_query(clean_id).has(.generic_fn_name) {
		// Nominal names retain the declaration identity after stripping wrappers.
		return !t.is_known_concrete_type_name_id(clean_id)
	}
	if shape.has(.function_compact) || shape.has(.function_spaced) {
		if !syntax.function_closed() { return true }
		for param_id in syntax.function_params() {
			if t.generic_arg_is_unresolved(flat.source_type_syntax(param_id).generic_parameter_payload()) {
				return true
			}
		}
		return syntax.function_result() != 0 && t.generic_arg_is_unresolved(syntax.function_result())
	}
	if syntax.generic_app_valid() {
		for nested_id in syntax.generic_app_args() {
			if t.generic_arg_is_unresolved(nested_id) { return true }
		}
	}
	return false
}

// is_known_concrete_type_name reports whether `name` refers to an already
// declared concrete type (struct, sum type, enum, interface or alias), so that
// one-letter type names like `A` are not mistaken for generic placeholders.
fn (t &Transformer) is_known_concrete_type_name(name string) bool {
	return t.is_known_concrete_type_name_id(flat.source_type_id(name))
}

// is_known_concrete_type_name_id queries declarations with a source spelling ID.
fn (t &Transformer) is_known_concrete_type_name_id(name_id u32) bool {
	if t.type_name_is_declared_id(name_id) || t.is_known_main_struct_type_name_id(name_id) {
		return true
	}
	if !flat.source_type_shape(name_id).has(.dotted) {
		if t.cur_module !in ['', 'main', 'builtin']
			&& t.type_name_is_declared_id(flat.source_type_composition_two(.qualified,
				flat.source_type_id(t.cur_module), name_id)) {
			return true
		}
		if t.type_name_is_declared_id(flat.source_type_composition_two(.qualified,
			generic_main_module_id, name_id)) {
			return true
		}
	}
	return false
}

fn (t &Transformer) is_known_main_struct_type_name(name string) bool {
	return t.is_known_main_struct_type_name_id(flat.source_type_id(name))
}

fn (t &Transformer) is_known_main_struct_type_name_id(name_id u32) bool {
	if !flat.source_type_query(name_id).has(.main_prefix) || t.ident_is_import_alias('main') {
		return false
	}
	bare_id := flat.source_type_syntax(name_id).main_base()
	if t.generic_type_indexes_ready
		&& t.generic_alias_index_authority_count == t.generic_type_authority_name_count() {
		return t.generic_main_struct_names[bare_id]
	}
	info := t.structs[bare_id] or { return false }
	return info.module in ['', 'main']
}

fn (t &Transformer) type_name_is_declared(name string) bool {
	return t.type_name_is_declared_id(flat.source_type_id(name))
}

fn (t &Transformer) type_name_is_declared_id(name_id u32) bool {
	if t.generic_type_indexes_ready
		&& t.generic_alias_index_authority_count == t.generic_type_authority_name_count() {
		return t.generic_type_authority_names[name_id]
	}
	return t.generic_transform_concrete_name_known(name_id)
		|| t.generic_concrete_name_known(name_id) || t.generic_exact_alias_name_known(name_id)
}

// EncodedTypeSuffixCache belongs to one transformer or worker. Its values are
// globally interned IDs, so decoding never retains scratch-owned name bytes.
struct EncodedTypeSuffixCache {
mut:
	entries map[u64]u32
}

// decode_encoded_type_suffix is the boundary from a serialized C identifier to
// structural type IDs. Each mode preserves the original codec's distinct grammar.
fn (t &Transformer) decode_encoded_type_suffix(encoded_id u32, mode u64) u32 {
	key := (u64(encoded_id) << 3) | mode
	if !isnil(t.encoded_type_suffix_cache) {
		if cached := t.encoded_type_suffix_cache.entries[key] { return cached }
	}
	encoded := flat.source_type_name(encoded_id)
	decoded := match mode {
		1 { generic_type_arg_from_suffix_parse(encoded) }
		2 { generic_array_type_arg_from_suffix_parse(encoded) or { u32(0) } }
		3 { generic_map_type_arg_from_suffix_parse(encoded) or { u32(0) } }
		4 { generic_type_arg_from_suffix_with_containers_parse(encoded) }
		5 {
			clean := trimmed_transform_text(encoded)
			if clean.len == 0 || (clean.starts_with('Array_') && !clean.starts_with('Array_fixed_')) {
				u32(0)
			} else {
				generic_type_arg_from_suffix_parse(clean)
			}
		}
		else { u32(0) }
	}
	if !isnil(t.encoded_type_suffix_cache) {
		mut cache := t.encoded_type_suffix_cache
		cache.entries[key] = decoded
	}
	return decoded
}

fn (t &Transformer) generic_type_arg_from_suffix_id(encoded_id u32) u32 {
	return t.decode_encoded_type_suffix(encoded_id, 1)
}

fn (t &Transformer) generic_array_type_arg_from_suffix_id(encoded_id u32) ?u32 {
	decoded := t.decode_encoded_type_suffix(encoded_id, 2)
	if decoded == 0 { return none }
	return decoded
}

fn (t &Transformer) generic_map_type_arg_from_suffix_id(encoded_id u32) ?u32 {
	decoded := t.decode_encoded_type_suffix(encoded_id, 3)
	if decoded == 0 { return none }
	return decoded
}

fn (t &Transformer) generic_type_arg_from_suffix_with_containers_id(encoded_id u32) u32 {
	return t.decode_encoded_type_suffix(encoded_id, 4)
}

fn (t &Transformer) generic_type_arg_from_receiver_suffix_id(encoded_id u32) u32 {
	return t.decode_encoded_type_suffix(encoded_id, 5)
}

fn generic_array_type_arg_from_suffix_parse(suffix string) ?u32 {
	clean := trimmed_transform_text(suffix)
	if !clean.starts_with('Array_') || clean.starts_with('Array_fixed_') { return none }
	elem_suffix := clean['Array_'.len..]
	if elem_suffix.len == 0 { return none }
	split := elem_suffix.last_index_u8(`_`)
	if split > 0 {
		len_text := elem_suffix[split + 1..]
		if len_text.len > 0 && is_decimal_text(len_text) {
			elem := generic_type_arg_from_suffix_parse(elem_suffix[..split])
			if elem != 0 {
				return flat.source_type_composition_two(.fixed_array, flat.source_type_id(len_text), elem)
			}
		}
	}
	elem := generic_type_arg_from_suffix_with_containers_parse(elem_suffix)
	if elem == 0 { return none }
	return flat.source_type_composition_one(.array, elem)
}

struct GenericTypeSuffixPart {
	typ u32
	len int
}

fn generic_type_arg_prefix_from_suffix(suffix string) ?GenericTypeSuffixPart {
	clean := trimmed_transform_text(suffix)
	if clean.len == 0 { return none }
	for encoded in ['string', 'bool', 'int', 'u8', 'u16', 'u32', 'u64', 'i8', 'i16', 'i32', 'i64',
		'f32', 'f64', 'v_bool', 'v_int', 'v_u8', 'v_u16', 'v_u32', 'v_u64', 'v_i8', 'v_i16', 'v_i32',
		'v_i64', 'v_f32', 'v_f64'] {
		if clean == encoded || clean.starts_with('${encoded}_') {
			return GenericTypeSuffixPart{ typ: generic_type_arg_from_suffix_parse(encoded), len: encoded.len }
		}
	}
	for prefix, kind in {
		'ptr_':        flat.SourceTypeComposition.pointer
		'Option_':     flat.SourceTypeComposition.option
		'__v_option_': flat.SourceTypeComposition.option
		'Result_':     flat.SourceTypeComposition.result
	} {
		if clean.starts_with(prefix) {
			inner := generic_type_arg_prefix_from_suffix(clean[prefix.len..]) or { return none }
			return GenericTypeSuffixPart{ typ: flat.source_type_composition_one(kind, inner.typ), len: prefix.len + inner.len }
		}
	}
	if clean.starts_with('Array_') && !clean.starts_with('Array_fixed_') {
		inner := generic_type_arg_prefix_from_suffix(clean['Array_'.len..]) or { return none }
		mut typ := flat.source_type_composition_one(.array, inner.typ)
		mut consumed := 'Array_'.len + inner.len
		if consumed + 1 < clean.len && clean[consumed] == `_` {
			mut end := consumed + 1
			for end < clean.len && clean[end] >= `0` && clean[end] <= `9` { end++ }
			if end > consumed + 1 && (end == clean.len || clean[end] == `_`) {
				typ = flat.source_type_composition_two(.fixed_array, flat.source_type_id(clean[consumed + 1..end]), inner.typ)
				consumed = end
			}
		}
		return GenericTypeSuffixPart{ typ: typ, len: consumed }
	}
	if clean.starts_with('Map_') {
		entry_suffix := clean['Map_'.len..]
		key := generic_type_arg_prefix_from_suffix(entry_suffix) or { return none }
		separator := 'Map_'.len + key.len
		if separator >= clean.len || clean[separator] != `_` { return none }
		value := generic_type_arg_prefix_from_suffix(clean[separator + 1..]) or { return none }
		return GenericTypeSuffixPart{ typ: flat.source_type_composition_two(.map, key.typ, value.typ), len: separator + 1 + value.len }
	}
	return none
}

fn generic_map_type_arg_from_suffix_parse(suffix string) ?u32 {
	clean := trimmed_transform_text(suffix)
	if !clean.starts_with('Map_') { return none }
	entry_suffix := clean['Map_'.len..]
	if key := generic_type_arg_prefix_from_suffix(entry_suffix) {
		if key.len < entry_suffix.len && entry_suffix[key.len] == `_` {
			value := generic_type_arg_from_suffix_with_containers_parse(entry_suffix[key.len + 1..])
			if value != 0 { return flat.source_type_composition_two(.map, key.typ, value) }
		}
	}
	split := entry_suffix.last_index_u8(`_`)
	if split <= 0 || split + 1 >= entry_suffix.len { return none }
	key := generic_type_arg_from_suffix_with_containers_parse(entry_suffix[..split])
	value := generic_type_arg_from_suffix_with_containers_parse(entry_suffix[split + 1..])
	if key == 0 || value == 0 { return none }
	return flat.source_type_composition_two(.map, key, value)
}

fn generic_type_arg_from_suffix_parse(suffix string) u32 {
	clean := trimmed_transform_text(suffix)
	if clean.len == 0 { return 0 }
	for prefix, kind in {
		'ptr_':        flat.SourceTypeComposition.pointer
		'Option_':     flat.SourceTypeComposition.option
		'__v_option_': flat.SourceTypeComposition.option
		'Result_':     flat.SourceTypeComposition.result
	} {
		if clean.starts_with(prefix) {
			inner := generic_type_arg_from_suffix_with_containers_parse(clean[prefix.len..])
			if inner != 0 { return flat.source_type_composition_one(kind, inner) }
		}
	}
	return match clean {
		'v_bool' { flat.source_type_id('bool') }
		'v_int' { flat.source_type_id('int') }
		'v_u8' { flat.source_type_id('u8') }
		'v_u16' { flat.source_type_id('u16') }
		'v_u32' { flat.source_type_id('u32') }
		'v_u64' { flat.source_type_id('u64') }
		'v_i8' { flat.source_type_id('i8') }
		'v_i16' { flat.source_type_id('i16') }
		'v_i32' { flat.source_type_id('i32') }
		'v_i64' { flat.source_type_id('i64') }
		'v_f32' { flat.source_type_id('f32') }
		'v_f64' { flat.source_type_id('f64') }
		'Array_u8' { flat.source_type_id('[]u8') }
		else { flat.source_type_id(clean.replace('__', '.')) }
	}
}

fn generic_type_arg_from_suffix_with_containers_parse(suffix string) u32 {
	if decoded := generic_array_type_arg_from_suffix_parse(suffix) { return decoded }
	if decoded := generic_map_type_arg_from_suffix_parse(suffix) { return decoded }
	return generic_type_arg_from_suffix_parse(suffix)
}

// These byte adapters are used only when source/generated identifier text enters
// the decoder, or when callers explicitly request its rendered spelling.
fn generic_type_arg_from_receiver_suffix(suffix string) string {
	clean := trimmed_transform_text(suffix)
	if clean.len == 0 || (clean.starts_with('Array_') && !clean.starts_with('Array_fixed_')) {
		return ''
	}
	return flat.source_type_name(generic_type_arg_from_suffix_parse(clean))
}

fn generic_array_type_arg_from_suffix(suffix string) ?string {
	decoded := generic_array_type_arg_from_suffix_parse(suffix) or { return none }
	return flat.source_type_name(decoded)
}

fn generic_map_type_arg_from_suffix(suffix string) ?string {
	decoded := generic_map_type_arg_from_suffix_parse(suffix) or { return none }
	return flat.source_type_name(decoded)
}

fn (t &Transformer) generic_type_arg_from_suffix(suffix string) string {
	return flat.source_type_name(t.generic_type_arg_from_suffix_id(flat.source_type_id(suffix)))
}

fn generic_type_arg_from_suffix(suffix string) string {
	return flat.source_type_name(generic_type_arg_from_suffix_parse(suffix))
}

fn generic_type_arg_from_suffix_with_containers(suffix string) string {
	return flat.source_type_name(generic_type_arg_from_suffix_with_containers_parse(suffix))
}

fn generic_fn_spec_key(decl_key string, args_ids []u32) GenericFnSpecKey {
	mut hash := u64(14695981039346656037)
	for c in decl_key {
		hash = (hash ^ u64(c)) * u64(1099511628211)
	}
	hash = (hash ^ u64(args_ids.len)) * u64(1099511628211)
	for id in args_ids {
		hash = (hash ^ u64(id)) * u64(1099511628211)
	}
	return GenericFnSpecKey{ decl_key: decl_key, args: args_ids, hash: hash }
}

fn (t &Transformer) ident_is_import_alias(name string) bool {
	if isnil(t.tc) {
		return false
	}
	return name in t.tc.imports || t.has_file_import(t.tc.cur_file, name)
}

fn (t &Transformer) import_alias_module(name string) string {
	if isnil(t.tc) {
		return name
	}
	if mod := t.file_import_module(t.tc.cur_file, name) {
		return mod
	}
	return t.tc.imports[name] or { name }
}

fn file_import_key(file string, alias string) string {
	return '${file}\n${alias}'
}

// has_file_import reports whether `alias` is an import alias in `file`.
fn (t &Transformer) has_file_import(file string, alias string) bool {
	if _ := t.file_import_module(file, alias) {
		return true
	}
	return false
}

// qualified_name_equals reports whether `name` is `owner.member`, without
// building that string.
@[inline]
fn qualified_name_equals(name string, owner string, member string) bool {
	return name.len == owner.len + 1 + member.len && name[owner.len] == `.`
		&& name.starts_with(owner) && name.ends_with(member)
}

// FileImportNameFilter conservatively records aliases in the frozen import table.
// Workers share it without retaining any source or scratch-owned string storage.
struct FileImportNameFilter {
mut:
	count int
	bits  [64]u64
}

@[direct_array_access; inline]
fn file_import_alias_bit(name string) u32 {
	if name.len == 0 {
		return 0
	}
	mut h := u32(name.len) * 0x9e3779b1 ^ u32(name[0]) * 0x85ebca6b
	h ^= u32(name[name.len - 1]) * 0xc2b2ae35 ^ u32(name[name.len / 2]) * 0x27d4eb2f
	return (h ^ (h >> 15)) & 4095
}

fn (mut t Transformer) prepare_file_import_name_filter() {
	if isnil(t.tc) {
		return
	}
	mut filter := &FileImportNameFilter{
		count: t.tc.file_imports.len
	}
	for key, _ in t.tc.file_imports {
		// Every possible separator is included, preserving keys with newlines
		// in the file or alias as well as the ordinary source identifier case.
		for i, ch in key {
			if ch == `\n` {
				bit := file_import_alias_bit(key[i + 1..])
				filter.bits[bit >> 6] |= u64(1) << (bit & 63)
			}
		}
	}
	t.file_import_name_filter = filter
}

@[direct_array_access; inline]
fn (filter &FileImportNameFilter) excludes(alias string, count int) bool {
	if filter.count != count {
		return false
	}
	bit := file_import_alias_bit(alias)
	return filter.bits[bit >> 6] & (u64(1) << (bit & 63)) == 0
}

// file_import_module returns the module that `alias` names through the imports
// of `file`. The checker's import table is complete before transform starts, so
// answers, including misses, are memoized by the parts of the table's key.
fn (t &Transformer) file_import_module(file string, alias string) ?string {
	if isnil(t.tc) {
		return none
	}
	if !isnil(t.file_import_name_filter)
		&& t.file_import_name_filter.excludes(alias, t.tc.file_imports.len) {
		return none
	}
	if isnil(t.file_import_cache) {
		return t.tc.file_imports[file_import_key(file, alias)] or { return none }
	}
	mut cache := t.file_import_cache
	state, cached := cache.get(file, alias, '')
	if state > 0 {
		return cached
	}
	if state < 0 {
		return none
	}
	module_name := t.tc.file_imports[file_import_key(file, alias)] or {
		cache.put(file, alias, '', -1, '')
		return none
	}
	cache.put(file, alias, '', 1, module_name)
	return module_name
}

// typ_text resolves the indexed source type spelling.
@[inline]
fn (value &GenericMethodValueReceiver) typ_text() string {
	return flat.source_type_name(value.typ)
}

// typ_text resolves the indexed source type spelling.
@[inline]
fn (value &GenericTypeSuffixPart) typ_text() string {
	return flat.source_type_name(value.typ)
}

// args_text resolves concrete type arguments at a text integration boundary.
fn (value &PendingGenericFnSpec) args_text() []string {
	return value.args.names()
}
