module transform

import strings
import time
import v.flat
import v.types

const scoped_monomorph_scan_nodes = 32768

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
fn (mut t Transformer) collect_generic_specs_range_scoped(struct_decls map[string]GenericStructDecl, sum_decls map[string]GenericSumDecl, mut struct_specs map[u32]u32, mut sum_specs map[u32]GenericSpecContext, first int, last int) {
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
		for name, args_id in w.generic_specialization_args.ids {
			if t.generic_specialization_args.contains(name) {
				continue
			}
			t.generic_specialization_args.set_ids(name.clone(), args_id)
		}
		transform_worker_scope_free(scope)
		start = end
	}
}

struct GenericCallSite {
	id     flat.NodeId
	module string
}

struct PendingGenericFnSpec {
	decl GenericFnDecl
	args flat.TypeTextList
	key  string
}

fn (mut t Transformer) is_generic_fn(name string) bool {
	if t.skip_generics {
		return false
	}
	decls := t.cached_generic_fn_decls()
	return name in decls || transform_qualified_fn_name(t.cur_module, name) in decls
}

fn (mut t Transformer) is_generic_struct(name string) bool {
	if t.skip_generics {
		return false
	}
	base, _, ok := generic_app_parts(name)
	if ok {
		return base in t.structs
	}
	if info := t.lookup_struct_info(flat.source_type_id(name)) {
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
	struct_decls := t.collect_generic_struct_decls()
	t.monomorph_profile('mono driver struct decls: ${time.ticks() - debug_started} ms')
	t.materialize_generic_sum_types(false)
	t.monomorph_profile('mono driver sum materialize: ${time.ticks() - debug_started} ms')
	t.seed_generic_specialization_args(struct_decls)
	sum_decls := t.collect_generic_sum_decls()
	t.materialize_generic_struct_specs(t.generic_struct_specs_cache, struct_decls)
	t.monomorph_profile('mono driver struct materialize: ${time.ticks() - debug_started} ms')
	mut ignored_nodes := t.monomorphize_ignored_nodes(decls)
	mut emitted := map[string]bool{}
	mut generated := []string{}
	mut generic_call_sites := []GenericCallSite{}
	mut recorded_call_sites := map[int]bool{}
	mut changed := true
	mut scan_start := 0
	mut generic_fn_value_scan_start := 0
	mut interface_box_scan_start := t.a.nodes.len
	mut used_fns_at_scan := t.used_fn_count()
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
	mut generic_struct_specs_scan_start := t.a.nodes.len
	mut lowered_operator_uses := t.lowered_generic_struct_operator_uses_for_specs(generic_struct_specs, decls)
	if t.parallel_monomorphize {
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
				t.request_generic_fn_specialization(decl, concrete_args_source_ids)
				changed = true
			}
			spec_value := specialized_generic_fn_value(decl.node.value, concrete_args_source_ids)
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
			for i in 0 .. scan_start {
				if i < ignored_nodes.len && ignored_nodes[i] && !(i < new_ignored.len
					&& new_ignored[i]) {
					rescan << i
				}
			}
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
			if t.a.nodes[i].kind !in [.index, .call, .index_assign] {
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
						t.request_generic_fn_specialization(decl, concrete_args_source_ids_2)
						changed = true
					}
					spec_value := specialized_generic_fn_value(decl.node.value, concrete_args_source_ids_2)
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
					args := flat.TypeTextList{ ids: args_source_ids }.names()

					decl := decls[decl_key] or { continue }
					if !t.call_has_source_generic_args(node)
						&& t.generic_args_contain_alias(args_source_ids, decl.module) {
						t.set_node_value(i, args.join(', '))
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
					if emitted[spec_key] {
						continue
					}
					if t.generic_specialization_registered(decl, concrete_args_source_ids_3) {
						emitted[spec_key] = true
						continue
					}
					t.request_generic_fn_specialization(decl, concrete_args_source_ids_3)
					emitted[spec_key] = true
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
							}
						}
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
	t.rewrite_generic_call_sites(decls, generic_call_sites)
	t.monomorph_profile('mono driver rewrite: ${time.ticks() - debug_started} ms')
	if t.refresh_decl_assign_types_after_generic_rewrite() {
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
			t.rewrite_generic_call_sites(decls, extra_call_sites)
			t.refresh_decl_assign_types_after_generic_rewrite()
		}
	}
	t.monomorph_profile('mono driver extra: ${time.ticks() - debug_started} ms')
	t.generic_struct_specs_cache = generic_struct_specs.move()
	t.generic_sum_specs_cache = generic_sum_specs.move()
	t.generic_materialization_scan_from = t.a.nodes.len
	t.generic_materialization_ready = true
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
	type_arg_text := t.generic_call_type_args_name(node)
	if type_arg_text.len == 0 {
		return none
	}
	// A type argument that names a generic parameter of the declaration enclosing
	// this call is not concrete: the enclosing specialization substitutes it while
	// it is cloned. Resolving it here through the writing file's imports would
	// request a specialization for a same-named imported type (`import pkg { T }`)
	// instead of leaving the call to the clone.
	if t.type_arg_text_has_enclosing_generic_param(id, type_arg_text) {
		return none
	}
	raw_args_source_ids := normalize_generic_args(flat.TypeTextList.from_names(split_generic_args(type_arg_text)).ids, module_name)

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
			arg := flat.source_type_name(arg_source_id)

			if !t.explicit_generic_arg_is_known_type(arg, module_name) {
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
	if isnil(t.tc) || node.children_count < 2 || node.value == 'range' {
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
	type_arg_text := t.generic_call_type_args_name(node)
	if type_arg_text.len == 0 || t.type_arg_text_has_enclosing_generic_param(id, type_arg_text) {
		return none
	}
	receiver_type_source_id := t.generic_call_arg_type_for_inference(t.a.child(selector, 0))
	receiver_type_source_id_2 := receiver_type_source_id
	receiver_type := flat.source_type_name(receiver_type_source_id_2)
	if receiver_type_source_id == 0 || t.generic_arg_is_unresolved(receiver_type_source_id)
		|| t.type_arg_text_has_enclosing_generic_param(id, receiver_type) {
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
	declaring_receiver := t.generic_method_value_declaring_receiver(receiver_type_source_id_2, decl,
		module_name) or { GenericMethodValueReceiver{ typ: receiver_type_source_id } }
	mut fixed := t.receiver_fixed_type_args(decl, declaring_receiver.typ)
	if decl.node.children_count > 0 {
		receiver_param := t.a.child_node(&decl.node, 0)
		t.infer_generic_receiver_suffix_args(receiver_param.typ, declaring_receiver.typ, mut fixed)
	}
	explicit := normalize_generic_args(flat.TypeTextList.from_names(split_generic_args(type_arg_text)).ids, module_name)
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
				base, _, is_generic := generic_app_parts(flat.source_type_name(field_type))
				pending << if is_generic {
					flat.source_type_id(flat.source_type_name(t.normalize_type_in_module(flat.source_type_id(base), info.module)) + flat.source_type_name(field_type)[base.len..])
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
	clean_source_id := t.trim_pointer_type(receiver_type_id)
	clean := flat.source_type_name(clean_source_id)
	mut names := []string{}
	base, _, is_app := generic_app_parts(clean)
	names << if is_app { base } else { clean }
	unaliased := types.unalias_type(t.tc.parse_type(clean_source_id)).name()
	unaliased_base, _, unaliased_app := generic_app_parts(unaliased)
	names << if unaliased_app { unaliased_base } else { unaliased }
	for name in names {
		for key in ['${name}.${method}', '${name.all_after_last('.')}.${method}'] {
			if decl := decls[key] {
				return decl
			}
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
	spec_value := specialized_generic_fn_value(decl.node.value, concrete_args_source_ids)
	spec_name := transform_qualified_fn_name(decl.module, spec_value)
	t.record_generic_specialization_args_for_names([spec_name, c_name(spec_name)], concrete_args_source_ids)
	// The type of the value: the instance without its receiver, which it binds.
	params := t.tc.fn_param_types[spec_name] or {
		t.tc.fn_param_types[spec_value] or { []types.Type{} }
	}
	method_type := if params.len > 0 {
		types.Type(types.FnType{
			params:      params[1..].clone()
			is_variadic: t.tc.fn_variadic[spec_name] or { t.tc.fn_variadic[spec_value] or { false } }
			return_type: t.tc.fn_ret_types[spec_name] or {
				t.tc.fn_ret_types[spec_value] or { types.Type(types.void_) }
			}
		})
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
		value:          selector.value
		typ:            t.semantic_type_name(method_type)
	})
	t.tc.remember_method_value_call(id, spec_name)
	return asked
}

fn (mut t Transformer) explicit_generic_arg_is_known_type(arg string, module_name string) bool {
	clean := trimmed_transform_text(arg)
	if clean.len == 0 {
		return false
	}
	if clean.starts_with('&') || clean.starts_with('?') || clean.starts_with('!') {
		return t.explicit_generic_arg_is_known_type(clean[1..], module_name)
	}
	if clean.starts_with('[]') {
		return t.explicit_generic_arg_is_known_type(clean[2..], module_name)
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		return bracket_end < clean.len - 1
			&& t.explicit_generic_arg_is_known_type(clean[4..bracket_end], module_name)
			&& t.explicit_generic_arg_is_known_type(clean[bracket_end + 1..], module_name)
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		return bracket_end < clean.len - 1
			&& t.explicit_generic_arg_is_known_type(clean[bracket_end + 1..], module_name)
	}
	if clean.starts_with('fn ') || clean.starts_with('fn(') || clean.starts_with('fn (') {
		return true
	}
	if clean.starts_with('(') && clean.ends_with(')') {
		parts := split_generic_args(clean[1..clean.len - 1])
		if parts.len == 0 {
			return false
		}
		for part in parts {
			if !t.explicit_generic_arg_is_known_type(part, module_name) {
				return false
			}
		}
		return true
	}
	base, nested_args, is_generic := generic_app_parts(clean)
	if is_generic {
		if !t.explicit_generic_arg_is_known_type(base, module_name) {
			return false
		}
		for nested_arg in nested_args {
			if !t.explicit_generic_arg_is_known_type(nested_arg, module_name) {
				return false
			}
		}
		return true
	}
	return clean.starts_with('C.') || clean.starts_with('JS.')
		|| t.concrete_type_name_known(clean, module_name)
}

fn (t &Transformer) explicit_generic_fn_value_decl_candidates(id flat.NodeId, base_id flat.NodeId, base flat.Node, module_name string) []string {
	if base.kind == .ident {
		mut candidates := []string{}
		local := transform_qualified_fn_name(module_name, base.value)
		if local != base.value {
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
			for imported in t.tc.file_selective_imports[file_import_key(file_name, base.value)] or {
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
		for candidate in t.generic_plain_call_candidates(base.value, module_name) {
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
	imported_module := if mod := t.file_import_module(file_name, qualifier.value) {
		mod
	} else if mod := t.tc.imports[qualifier.value] {
		mod
	} else {
		return []string{}
	}
	return ['${imported_module}.${base.value}']
}

fn (mut t Transformer) request_generic_fn_specialization(decl GenericFnDecl, args_ids []u32) {
	// Callers pass the canonical arguments used to form `clone_value`. Resolving
	// them again in the declaration module could rebind a caller-owned type.
	concrete_args := args_ids.clone()
	key := t.generic_specialization_progress_key(decl, concrete_args)
	t.record_monomorph_cache_spec(key, decl.key, decl.module, concrete_args)
	if key in t.generic_fn_spec_nodes || t.generic_fn_specs_in_progress[key]
		|| t.pending_generic_fn_spec_keys[key] {
		return
	}
	t.pending_generic_fn_spec_keys[key] = true
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

fn (mut t Transformer) record_monomorph_cache_spec(key string, decl_key string, module_name string, args_ids []u32) {
	if key == '' || decl_key == '' { return }
	// Declaration keys and numeric arrays must outlive a scoped worker arena.
	// Re-record so the last parent-arena copy wins after a worker merge.
	t.monomorph_cache_specs[key.clone()] = MonomorphCacheSpec{
		decl_key: decl_key.clone()
		module:   module_name.clone()
		args:     flat.TypeTextList{ ids: args_ids.clone() }
	}
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
		decl := sum_decls[flat.source_type_name(context.base)] or { continue }
		t.materialize_generic_sum_spec(spec, decl, context)
	}
	t.materialize_generic_struct_specs(struct_specs, struct_decls)
}

fn (t &Transformer) sorted_monomorph_cache_specs() []MonomorphCacheSpec {
	mut keys := t.monomorph_cache_specs.keys()
	keys.sort()
	mut specs := []MonomorphCacheSpec{cap: keys.len}
	for key in keys {
		spec := t.monomorph_cache_specs[key]
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

fn (mut t Transformer) drain_pending_generic_fn_specs(struct_decls map[string]GenericStructDecl, sum_decls map[string]GenericSumDecl, mut emitted map[string]bool, mut generated []string) bool {
	mut changed := false
	for t.pending_generic_fn_specs.len > 0 {
		specs := t.pending_generic_fn_specs
		t.pending_generic_fn_specs = []PendingGenericFnSpec{}
		mut pending := []PendingGenericFnSpec{cap: specs.len}
		for spec in specs {
			if spec.key in t.generic_fn_spec_nodes {
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
			emitted[generic_fn_spec_key(spec.decl.key, spec.args.ids)] = true
			t.pending_generic_fn_spec_keys.delete(spec.key)
			changed = true
		}
	}
	return changed
}

fn (mut t Transformer) collect_generic_call_sites_after_type_refresh(generic_struct_specs map[u32]u32, decls map[string]GenericFnDecl, ignored_nodes []bool, candidate_ids []int, mut emitted map[string]bool, mut recorded_call_sites map[int]bool) []GenericCallSite {
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
		args := flat.TypeTextList{ ids: args_source_ids }.names()

		decl := decls[decl_key] or { continue }
		if !t.call_has_source_generic_args(node) && t.generic_args_contain_alias(args_source_ids, decl.module) {
			t.set_node_value(i, args.join(', '))
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
		if emitted[spec_key] {
			continue
		}
		if t.generic_specialization_registered(decl, concrete_args_source_ids) {
			emitted[spec_key] = true
			continue
		}
		t.request_generic_fn_specialization(decl, concrete_args_source_ids)
		emitted[spec_key] = true
	}
	return sites
}

fn (mut t Transformer) seed_generic_specialization_args(decls map[string]GenericStructDecl) {
	if decls.len == 0 {
		return
	}
	specs := if t.generic_materialization_ready {
		t.generic_struct_specs_cache
	} else {
		all_specs, _ := t.collect_generic_materialization_specs(decls, map[string]GenericSumDecl{})
		all_specs
	}
	for spec_id, base_id in specs {
		spec := flat.source_type_name(spec_id)
		base := flat.source_type_name(base_id)
		decl := decls[base] or { continue }
		_, args, ok := generic_app_parts(spec)
		if ok && args.len > 0 {
			t.record_generic_specialization_args_in_module(base, decl.module, flat.TypeTextList.from_names(args).ids)
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
				cur_module = t.tc.file_modules[node.value] or { '' }
			}
			.module_decl {
				cur_module = node.value
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
			new_file := node.value
			return new_file, t.tc.file_modules[new_file] or { '' }
		}
		.module_decl {
			return cur_file, node.value
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
		for candidate in t.generic_plain_call_candidates(callee.value, module_name) {
			if key := t.type_erased_generic_template_candidate_key(candidate, node, decls) {
				return key
			}
		}
	} else if callee.kind == .selector && callee.children_count > 0 {
		base := t.a.child_node(callee, 0)
		if base.kind == .ident {
			for candidate in ['${base.value}.${callee.value}',
				'${t.import_alias_module(base.value)}.${callee.value}'] {
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
		|| t.type_text_has_generic_placeholder_bytes(node.value, module_name) {
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
// operator symbol (`+`, `-`, `*`, `/`, `%`, `==`, `!=`, `<`, `>`, `<=`, `>=`,
// `[]`, `[]=`).
fn is_operator_method_name(name string) bool {
	return name in ['+', '-', '*', '/', '%', '==', '!=', '<', '>', '<=', '>=', '[]', '[]=']
}

// record_called_fn_name records the callee name of a direct call so operator overloads
// (lowered to direct calls before this pass) can be specialized only when actually
// called. Operator method names are mangled (`Vec_int__plus`), matching the name
// `specialize_generic_struct_methods` would emit for that instance and operator.
fn (t &Transformer) call_is_recorded_struct_operator(node flat.Node) bool {
	if node.children_count < 1 {
		return false
	}
	fn_node := t.a.child_node(&node, 0)
	return fn_node.kind == .ident && fn_node.value.len > 0
		&& fn_node.value in t.used_struct_operator_fns
}

fn (mut t Transformer) record_lowered_generic_struct_operator_call(node flat.Node, lowered_operator_uses map[string][]string) bool {
	if node.children_count < 1 {
		return false
	}
	fn_node := t.a.child_node(&node, 0)
	if fn_node.kind != .ident || fn_node.value.len == 0 {
		return false
	}
	operator_names := lowered_operator_uses[fn_node.value] or { return false }
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

fn (mut t Transformer) lowered_generic_struct_operator_uses(struct_decls map[string]GenericStructDecl, decls map[string]GenericFnDecl) map[string][]string {
	return t.lowered_generic_struct_operator_uses_for_specs(t.generic_struct_specs(struct_decls), decls)
}

fn (mut t Transformer) generic_struct_specs(struct_decls map[string]GenericStructDecl) map[u32]u32 {
	mut specs := map[u32]u32{}
	t.collect_generic_struct_specs(struct_decls, mut specs)
	return specs
}

fn (t &Transformer) lowered_generic_struct_operator_uses_for_specs(specs map[u32]u32, decls map[string]GenericFnDecl) map[string][]string {
	mut uses := map[string][]string{}
	for spec_id, base_id in specs {
		spec := flat.source_type_name(spec_id)
		base := flat.source_type_name(base_id)
		_, args, ok := generic_app_parts(spec)
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
			for alias in specialized_generic_fn_signature_aliases(decl, flat.TypeTextList.from_names(args).ids) {
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

fn (mut t Transformer) record_index_operator_fn_name(node flat.Node, method string) {
	if node.kind != .index || node.value == 'range' || node.children_count < 2 {
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

// specialize_generic_struct_methods specializes every generic method of each
// instantiated generic struct (e.g. for `Vec4[f32]`, generate `vec__Vec4_f32__plus`,
// `vec__Vec4_f32__one`, ...). It only handles methods whose generic parameters are
// exactly the struct's parameters (no extra method-level `[U]`); those are left to
// the call-driven path. Returns whether any new specialization was emitted.
fn generic_struct_methods_by_receiver(decls map[string]GenericFnDecl) map[string][]GenericFnDecl {
	mut methods_by_receiver := map[string][]GenericFnDecl{}
	for decl_key, decl in decls {
		if !decl_key.contains('.') {
			continue
		}
		receiver := decl_key.all_before_last('.')
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
	for iface_name, _ in t.tc.interface_names {
		iface_id := flat.source_type_id(iface_name)
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

fn (mut t Transformer) specialize_generic_struct_methods(specs map[u32]u32, decls map[string]GenericFnDecl, methods_by_receiver map[string][]GenericFnDecl, method_level_params map[string]bool, interfaces_by_method map[string][]u32, mut interface_impl_cache map[u64]bool, mut emitted map[string]bool) bool {
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
		_, args, ok := generic_app_parts(spec)
		if !ok || args.len == 0 {
			continue
		}
		methods := methods_by_receiver[base] or { continue }
		fixture_expand_regular_methods := t.fixture_should_expand_regular_generic_methods(flat.TypeTextList.from_names(args).ids)
			&& t.generic_struct_spec_has_emitted_method(base, flat.TypeTextList.from_names(args).ids, decls, emitted)
		concrete_args_source_ids := t.canonical_generic_specialization_args(flat.TypeTextList.from_names(args).ids)

		for decl in methods {
			decl_key := decl.key
			if method_level_params[decl_key] {
				// Method has its own generic parameters beyond the struct's; leave it
				// to the call-driven specialization which can infer them.
				continue
			}
			spec_key := generic_fn_spec_key(decl_key, concrete_args_source_ids)
			if emitted[spec_key] {
				continue
			}
			if t.generic_specialization_registered(decl, concrete_args_source_ids) {
				emitted[spec_key] = true
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
				if !t.generic_struct_method_used_for_spec(spec, decl, flat.TypeTextList.from_names(args).ids, method) {
					if isnil(t.tc) || mvkey !in t.tc.generic_method_value_info {
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
			emitted[spec_key] = true
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
		raw_arg := flat.source_type_name(raw_arg_source_id)

		arg := raw_arg.trim_left('&?!').trim_left('[]')
		for candidate in [arg, arg.all_after_last('.')] {
			if file := t.tc.struct_files[candidate] {
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

fn (t &Transformer) generic_struct_spec_has_emitted_method(base string, args_ids []u32, decls map[string]GenericFnDecl, emitted map[string]bool) bool {
	concrete_args_source_ids := t.canonical_generic_specialization_args(args_ids)

	for decl_key, _ in decls {
		if !decl_key.contains('.') || decl_key.all_before_last('.') != base {
			continue
		}
		if emitted[generic_fn_spec_key(decl_key, concrete_args_source_ids)] {
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
	direct_c := c_name(direct)
	if !generic_method_c_name_can_collide_with_operator(method) && t.used_fn_contains_name(direct_c) {
		return true
	}
	mut aliases := []string{}
	for alias in specialized_generic_fn_signature_aliases(decl, args_ids) {
		if !generic_method_c_name_can_collide_with_operator(method) || alias.ends_with('.${method}') {
			aliases << alias
		}
	}
	receiver := generic_fn_decl_base_value(decl.node.value).all_before_last('.')
	if receiver.len > 0 {
		short_flat := '${receiver}_${generic_type_suffixes(args_ids)}'
		qualified_short_flat := transform_qualified_fn_name(decl.module, short_flat)
		full_flat := '${receiver}_${generic_type_full_suffixes(args_ids)}'
		qualified_full_flat := transform_qualified_fn_name(decl.module, full_flat)
		for flat_receiver in [short_flat, qualified_short_flat, full_flat, qualified_full_flat] {
			aliases << '${flat_receiver}.${method}'
			if !generic_method_c_name_can_collide_with_operator(method) {
				aliases << c_name('${flat_receiver}.${method}')
			}
		}
	}
	for alias in aliases {
		if t.used_fn_contains_name(alias) {
			if t.generic_struct_method_alias_is_operator_collision(spec, method, alias) {
				continue
			}
			return true
		}
	}
	return false
}

fn (t &Transformer) generic_struct_method_alias_is_operator_collision(spec string, method string, alias string) bool {
	op_name := operator_symbol_for_mangled_method_name(method) or { return false }
	operator_name := '${spec}.${op_name}'
	if operator_name !in t.used_struct_operator_fns && c_name(operator_name) !in t.used_struct_operator_fns {
		return false
	}
	if t.used_fn_contains_name('${spec}.${method}') {
		return false
	}
	return alias == c_name(operator_name)
}

fn operator_symbol_for_mangled_method_name(method string) ?string {
	match method {
		'plus' {
			return '+'
		}
		'minus' {
			return '-'
		}
		'mul' {
			return '*'
		}
		'div' {
			return '/'
		}
		'mod' {
			return '%'
		}
		'eq' {
			return '=='
		}
		'ne' {
			return '!='
		}
		'lt' {
			return '<'
		}
		'gt' {
			return '>'
		}
		'le' {
			return '<='
		}
		'ge' {
			return '>='
		}
		'index' {
			return '[]'
		}
		'index_set' {
			return '[]='
		}
		else {}
	}

	return none
}

fn generic_method_c_name_can_collide_with_operator(method string) bool {
	return method in ['plus', 'minus', 'mul', 'div', 'mod', 'eq', 'ne', 'lt', 'gt', 'le', 'ge',
		'and', 'or', 'xor', 'left_shift', 'right_shift']
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
		short_alias_name := '${alias.all_after_last('.')}.${method}'
		if short_alias_name != alias_name && t.interface_dispatch_short_name_allowed(alias_id)
			&& t.used_interface_dispatch_key(short_alias_name) {
			return true
		}
	}
	short_name := '${iface_name.all_after_last('.')}.${method}'
	return short_name != name && t.interface_dispatch_short_name_allowed(iface_name_id)
		&& t.used_interface_dispatch_key(short_name)
}

fn (t &Transformer) interface_alias_names(iface_name_id u32) []u32 {
	mut aliases := []u32{}
	for alias, target_id in t.tc.type_aliases.ids {
		if target_id == iface_name_id
			|| flat.source_type_id(t.tc.qualify_name(flat.source_type_name(target_id))) == iface_name_id {
			aliases << flat.source_type_id(alias)
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
	iface_name := flat.source_type_name(iface_name_id)
	concrete_type := flat.source_type_name(concrete_type_id)
	mut iface_names := [iface_name_id]
	short_name := flat.source_type_id(iface_name.all_after_last('.'))
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
	_, _, iface_is_specialized := generic_app_parts(iface_name)
	if !iface_is_specialized {
		concrete_base := if bracket := concrete_type.index('[') {
			flat.source_type_id(trimmed_transform_text(concrete_type[..bracket]))
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
			boxed_base, _, boxed_is_specialized := generic_app_parts(flat.source_type_name(boxed_iface))
			if boxed_is_specialized
				&& boxed_base.all_after_last('.') == iface_name.all_after_last('.')
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
	if bracket := concrete_type.index('[') {
		base := flat.source_type_id(trimmed_transform_text(concrete_type[..bracket]))
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
		target := t.tc.type_aliases.type_id(candidate) or { continue }
		for iface in iface_names {
			if t.interface_boxed_type_key_marked(interface_boxed_type_key(iface, target))
				|| t.interface_boxed_type_key_marked(interface_boxed_type_key(iface, flat.source_type_id(c_name(flat.source_type_name(target)))))
				|| t.interface_boxed_type_key_marked(interface_boxed_type_key(iface, flat.source_type_id(flat.source_type_name(target).all_after_last('.')))) {
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
	iface_name := flat.source_type_name(iface_name_id)
	concrete_type := flat.source_type_name(concrete_type_id)
	mut candidates := [iface_name_id]
	_, _, iface_is_specialized := generic_app_parts(iface_name)
	if iface_is_specialized {
		return candidates
	}
	concrete_c_name := flat.source_type_id(c_name(concrete_type))
	concrete_base := if bracket := concrete_type.index('[') {
		flat.source_type_id(trimmed_transform_text(concrete_type[..bracket]))
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
		boxed_base, _, boxed_is_specialized := generic_app_parts(flat.source_type_name(boxed_iface))
		if !boxed_is_specialized || boxed_base.all_after_last('.') != iface_name.all_after_last('.') {
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
	t.collect_interface_boxed_types_range(0, t.a.nodes.len)
	t.interface_boxed_types_frozen = true
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
			_, kind := comptime_for_parts(t.a.nodes[idx].value)
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
			file = node.value
			mod_name = t.tc.file_modules[file] or { '' }
		} else if node.kind == .module_decl {
			mod_name = node.value
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
			t.cur_file = node.value
			t.tc.cur_file = node.value
			t.cur_module = t.tc.file_modules[node.value] or { '' }
			t.tc.cur_module = t.cur_module
			continue
		}
		if node.kind == .module_decl {
			t.cur_module = node.value
			t.tc.cur_module = node.value
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
			&& t.interface_box_type_text_maybe(flat.source_type_id(node.value)) {
			expected := t.tc.parse_type(flat.source_type_id(node.value))
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
		t.collect_interface_boxed_value(flat.NodeId(idx), t.tc.parse_type(flat.source_type_id(node.value)))
		iface_name := t.interface_literal_name(flat.source_type_id(node.value)) or { continue }
		for i in 0 .. node.children_count {
			field := t.a.child_node(&node, i)
			if field.kind != .field_init || field.value != '_object' {
				continue
			}
			concrete_type := if flat.source_type_shape(field.typ).has(.pointer) {
				flat.source_type_id(field.typ_text()[1..])
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
		iface_name := t.interface_literal_name(flat.source_type_id(node.value)) or { continue }
		for i in 0 .. node.children_count {
			field := t.a.child_node(&node, i)
			if field.kind != .field_init || field.value != '_object' {
				continue
			}
			concrete_type := if flat.source_type_shape(field.typ).has(.pointer) {
				flat.source_type_id(field.typ_text()[1..])
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
	if node.kind == .fn_literal && node.typ != 0 {
		return t.tc.parse_type(node.typ)
	}
	return none
}

fn interface_box_fn_type_return_type(typ types.Type) ?types.Type {
	if typ is types.FnType {
		return typ.return_type
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
				if child.kind == .param && child.value == source.value {
					return t.tc.parse_type(child.typ)
				}
				if child.kind == .decl_assign {
					if typ_source_id := t.local_decl_type_at(source.value, int(child_id)) {
						return t.tc.parse_type(typ_source_id)
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
		|| callee.value !in ['insert', 'prepend'] {
		return
	}
	value_index := if callee.value == 'insert' { 2 } else { 1 }
	if node.children_count != value_index + 1 {
		return
	}
	receiver_id := t.a.child(callee, 0)
	receiver_type := interface_box_unalias_type(types.unwrap_all_pointers(t.interface_box_source_type(receiver_id)))
	if receiver_type !is types.Array {
		return
	}
	value_id := t.a.child(&node, value_index)
	expected := t.interface_box_append_expected_type(receiver_type, receiver_id, value_id)
	if !interface_box_expected_type(expected) {
		return
	}
	value := t.a.node(value_id)
	source_id := if value.kind == .prefix && value.value == '...' && value.children_count > 0 {
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
			call_name = callee.value
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
	mut omitted_params_struct := ''
	if variadic_idx < 0 && logical_params != logical_args {
		if logical_params == logical_args + 1 {
			omitted_params_struct = flat.source_type_name(t.params_struct_type_name(params[params.len - 1].source_type_id()) or {
				return
			})
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
				mut param_type := params[param_idx].name()
				if param_idx == variadic_idx {
					variadic_type := params[param_idx]
					if variadic_type is types.Array {
						param_type = variadic_type.elem_type.name()
					}
				}
				struct_type_source_id := t.params_struct_type_name(flat.source_type_id(param_type)) or {
					t.struct_arg_type_name(flat.source_type_id(param_type)) or { u32(0) }
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
				variadic_type.elem_type
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
			&& arg.value == '...' && arg.children_count > 0 {
			expected = params[variadic_idx]
			value_id = t.a.child(&arg, 0)
		}
		if interface_box_expected_type(expected) {
			t.collect_interface_boxed_value(value_id, expected)
		}
	}
	if omitted_params_struct.len > 0 {
		t.collect_interface_struct_default_boxes(flat.source_type_id(omitted_params_struct), []string{})
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
		field_name := if field.value.len > 0 {
			field.value
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
		t.tc.parse_type(lhs.typ)
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
			actual_base := if actual is types.OptionType { actual.base_type } else { actual }
			t.collect_interface_boxed_type(actual_base, expected.base_type)
		}
		types.ResultType {
			actual_base := if actual is types.ResultType { actual.base_type } else { actual }
			t.collect_interface_boxed_type(actual_base, expected.base_type)
		}
		types.Pointer {
			t.collect_interface_boxed_type(actual, expected.base_type)
		}
		types.Interface {
			iface_name_source_id := t.resolve_interface_type_name(expected.source_type_id())
			iface_name_source_id_4 := iface_name_source_id

			actual_name := actual.name()
			if iface_name_source_id != 0 && actual_name !in ['', 'unknown', 'void'] && (t.resolve_interface_type_name(flat.source_type_id(actual_name))) == 0 {
				t.mark_interface_boxed_type(iface_name_source_id_4, t.trim_pointer_type(flat.source_type_id(actual_name)))
			}
		}
		types.Array {
			actual_base := interface_box_unalias_type(actual)
			if actual_base is types.Array {
				t.collect_interface_boxed_type(actual_base.elem_type, expected.elem_type)
			} else if actual_base is types.ArrayFixed {
				t.collect_interface_boxed_type(actual_base.elem_type, expected.elem_type)
			}
		}
		types.ArrayFixed {
			actual_base := interface_box_unalias_type(actual)
			if actual_base is types.ArrayFixed {
				t.collect_interface_boxed_type(actual_base.elem_type, expected.elem_type)
			}
		}
		types.Map {
			actual_base := interface_box_unalias_type(actual)
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
	channel_type := interface_box_unalias_type(t.tc.expr_type(channel_id) or {
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
	channel_type := interface_box_unalias_type(t.tc.expr_type(channel_id) or {
		t.tc.resolve_type(channel_id)
	})
	if channel_type is types.Channel {
		t.collect_interface_boxed_type(channel_type.elem_type, expected)
	}
}

fn (t &Transformer) interface_box_append_expected_type(lhs_type types.Type, lhs_id flat.NodeId, rhs_id flat.NodeId) types.Type {
	clean := interface_box_unalias_type(lhs_type)
	if clean is types.Array {
		rhs_type := t.tc.expr_type(rhs_id) or { t.tc.resolve_type(rhs_id) }
		if t.array_append_rhs_is_push_many(lhs_id, rhs_id, rhs_type.source_type_id(), clean.elem_type.source_type_id()) {
			return lhs_type
		}
		return clean.elem_type
	}
	return lhs_type
}

fn interface_box_unalias_type(typ types.Type) types.Type {
	if typ is types.Alias {
		return interface_box_unalias_type(typ.base_type)
	}
	return typ
}

fn (t &Transformer) interface_box_type_text_maybe(raw_type_id u32) bool {
	if raw_type_id == 0 {
		return false
	}
	clean_id := t.trimmed_type_id(t.normalize_type_alias(raw_type_id))
	clean := flat.source_type_name(clean_id)
	if flat.source_type_shape(clean_id).has(.optional) {
		if clean.len == 1 { return false }
		return t.interface_box_type_text_maybe(t.optional_base_type(clean_id))
	}
	if clean.starts_with('(') && clean.ends_with(')') && clean.contains(',') {
		for part in split_generic_args(clean[1..clean.len - 1]) {
			if t.interface_box_type_text_maybe(flat.source_type_id(part)) {
				return true
			}
		}
		return false
	}
	return (t.resolve_interface_type_name(clean_id)) != 0 || clean.starts_with('[]')
		|| clean.starts_with('[') || clean.starts_with('map[')
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
			actual := interface_box_unalias_type(t.tc.expr_type(id) or { t.tc.resolve_type(id) })
			if actual is types.OptionType {
				t.collect_interface_boxed_type(actual.base_type, expected.base_type)
			} else {
				t.collect_interface_boxed_value(id, expected.base_type)
			}
		}
		types.ResultType {
			actual := interface_box_unalias_type(t.tc.expr_type(id) or { t.tc.resolve_type(id) })
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
	literal_name_id := if node.value.len > 0 {
		flat.source_type_id(node.value)
	} else {
		struct_name_id
	}
	info := t.lookup_struct_info(literal_name_id) or { return }
	for i in 0 .. node.children_count {
		field := t.a.child_node(&node, i)
		if field.kind != .field_init || field.children_count == 0 {
			continue
		}
		field_name := if field.value.len > 0 {
			field.value
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
		field_name := if field.value.len > 0 {
			field.value
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
			if child.value == 'init' && child.children_count > 0 {
				t.collect_interface_boxed_value(t.a.child(&child, 0), elem_type)
			}
			continue
		}
		if child.kind == .prefix && child.value == '...' && child.children_count > 0 {
			t.collect_interface_boxed_value(t.a.child(&child, 0), array_type)
			continue
		}
		t.collect_interface_boxed_value(child_id, elem_type)
	}
}

fn (mut t Transformer) collect_interface_boxed_interface_value(id flat.NodeId, iface_name_id u32) {
	node := t.a.nodes[int(id)]
	actual := t.tc.expr_type(id) or { t.tc.resolve_type(id) }
	actual_name := if actual.name() in ['', 'unknown'] && node.kind == .struct_init {
		node.value
	} else {
		actual.name()
	}
	if actual_name !in ['', 'unknown', 'void'] && (t.resolve_interface_type_name(flat.source_type_id(actual_name))) == 0 {
		t.mark_interface_boxed_type(iface_name_id, t.trim_pointer_type(flat.source_type_id(actual_name)))
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
		for embed in t.tc.interface_embeds.type_ids(flat.source_type_name(iface)) or { []u32{} } {
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
		if bracket := concrete_type.index('[') {
			base := flat.source_type_id(trimmed_transform_text(concrete_type[..bracket]))
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
		decl := sum_decls[flat.source_type_name(context.base)] or { continue }
		t.materialize_generic_sum_spec(spec, decl, context)
	}
	t.monomorph_profile('mono materialize sums: ${time.ticks() - debug_started} ms')
	t.materialize_generic_struct_specs(specs, decls)
	t.monomorph_profile('mono materialize structs: ${time.ticks() - debug_started} ms')
	if erase_templates {
		t.erase_generic_sum_templates(sum_decls)
		for _, decl in decls {
			t.tc.structs.delete(decl.key)
			t.tc.unions.delete(decl.key)
			t.tc.params_structs.delete(decl.key)
			t.tc.unregister_short_type_name(decl.key)
		}
	}
	t.monomorph_profile('mono materialize erase: ${time.ticks() - debug_started} ms')
}

fn (mut t Transformer) materialize_generic_struct_specs(specs map[u32]u32, decls map[string]GenericStructDecl) {
	if isnil(t.tc) {
		return
	}
	spec_names := specs.keys()
	types.extend_stable_type_indexes_ref(mut t.runtime_type_indexes, spec_names)
	for spec, base_id in specs {
		base := flat.source_type_name(base_id)
		decl := decls[base] or { continue }
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
		decl := sum_decls[flat.source_type_name(context.base)] or { continue }
		t.materialize_generic_sum_spec(spec, decl, context)
	}
	if erase_templates {
		t.erase_generic_sum_templates(sum_decls)
	}
}

fn (mut t Transformer) erase_generic_sum_templates(sum_decls map[string]GenericSumDecl) {
	for _, decl in sum_decls {
		t.tc.sum_types.delete(decl.key)
		t.sum_types.delete(decl.key)
		t.tc.sum_generic_params.delete(decl.key)
		t.tc.unregister_short_type_name(decl.key)
		if decl.key != decl.node.value {
			t.tc.sum_generic_params.delete(decl.node.value)
		}
	}
}

fn (mut t Transformer) collect_generic_materialization_specs(decls map[string]GenericStructDecl, sum_decls map[string]GenericSumDecl) (map[u32]u32, map[u32]GenericSpecContext) {
	mut specs := map[u32]u32{}
	mut sum_specs := map[u32]GenericSpecContext{}
	t.collect_generic_struct_specs(decls, mut specs)
	t.collect_generic_sum_specs(sum_decls, mut sum_specs)
	t.expand_generic_materialization_specs(decls, sum_decls, mut specs, mut sum_specs)
	return specs, sum_specs
}

fn (mut t Transformer) expand_generic_materialization_specs(decls map[string]GenericStructDecl, sum_decls map[string]GenericSumDecl, mut specs map[u32]u32, mut sum_specs map[u32]GenericSpecContext) {
	for _ in 0 .. 40 {
		before := specs.len
		before_sums := sum_specs.len
		current := specs.clone()
		for spec_id, base_id in current {
			spec := flat.source_type_name(spec_id)
			base := flat.source_type_name(base_id)
			decl := decls[base] or { continue }
			_, args, ok := generic_app_parts(spec)
			if !ok {
				continue
			}
			for i in 0 .. decl.node.children_count {
				field := t.a.child_node(&decl.node, i)
				if field.kind != .field_decl {
					continue
				}
				field_type := substitute_generic_type_text_with_params(field.typ, flat.TypeTextList.from_names(args).ids, decl.node.generic_params())
				t.collect_generic_struct_spec_from_type(field_type, decl.module, decl.file, decls, mut specs)
				t.collect_generic_sum_spec_from_type(field_type, decl.module, decl.file, sum_decls, mut sum_specs)
			}
		}
		current_sums := sum_specs.clone()
		for spec, context in current_sums {
			decl := sum_decls[flat.source_type_name(context.base)] or { continue }
			_, args, ok := generic_app_parts(flat.source_type_name(spec))
			if !ok {
				continue
			}
			for i in 0 .. decl.node.children_count {
				variant := t.a.child_node(&decl.node, i)
				variant_type := substitute_generic_type_text_with_params(flat.source_type_id(variant.value), flat.TypeTextList.from_names(args).ids, decl.node.generic_params())
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
fn (mut t Transformer) collect_generic_struct_decls() map[string]GenericStructDecl {
	mut decls := map[string]GenericStructDecl{}
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
				cur_file = node.value
				cur_module = t.tc.file_modules[cur_file] or { '' }
			}
			.module_decl {
				cur_module = node.value
			}
			.struct_decl {
				if node.generic_params().len == 0 && !comma_list_has_item(node.typ_text(), 'generic') {
					continue
				}
				module_name := t.node_module_or(i, cur_module)
				key := generic_struct_decl_key(node.value, module_name)
				decls[key] = GenericStructDecl{
					id:     flat.NodeId(i)
					node:   *node
					file:   cur_file
					module: module_name
					key:    key
				}
			}
			else {}
		}
	}
	return decls
}

fn generic_struct_decl_key(name string, module_name string) string {
	if name.contains('.') || module_name == '' || module_name == 'main'
		|| module_name == 'builtin' {
		return name
	}
	return '${module_name}.${name}'
}

@[direct_array_access]
fn (mut t Transformer) collect_generic_sum_decls() map[string]GenericSumDecl {
	mut decls := map[string]GenericSumDecl{}
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
				cur_file = node.value
				cur_module = t.tc.file_modules[cur_file] or { '' }
			}
			.module_decl {
				cur_module = node.value
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
				key := generic_type_decl_key(node.value, module_name)
				decls[key] = GenericSumDecl{
					id:     flat.NodeId(i)
					node:   *node
					file:   cur_file
					module: module_name
					key:    key
				}
			}
			else {}
		}
	}
	return decls
}

fn generic_type_decl_key(name string, module_name string) string {
	return generic_struct_decl_key(name, module_name)
}

fn (mut t Transformer) collect_generic_struct_specs(decls map[string]GenericStructDecl, mut specs map[u32]u32) {
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
	return type_text_has_unqualified_generic_arg_bytes(flat.source_type_name(typ_id))
}

fn type_text_has_unqualified_generic_arg_bytes(typ string) bool {
	text := strip_type_modifier_prefixes(typ)
	if text.len == 0 {
		return false
	}
	if text.starts_with('map[') {
		end := generic_matching_bracket(text, 3)
		if end <= 3 || end >= text.len {
			return false
		}
		return type_text_has_unqualified_generic_arg_bytes(text[4..end])
			|| type_text_has_unqualified_generic_arg_bytes(text[end + 1..])
	}
	if text.starts_with('[') {
		// `[N]T` / `[N][M]T` fixed arrays: the element type decides (the collector
		// strips the prefix the same way).
		end := generic_matching_bracket(text, 0)
		if end <= 0 || end + 1 >= text.len {
			return false
		}
		return type_text_has_unqualified_generic_arg_bytes(text[end + 1..])
	}
	open := text.index_u8(`[`)
	if open < 0 {
		return false
	}
	close := generic_matching_bracket(text, open)
	if close <= open + 1 {
		return false
	}
	for arg in split_generic_args(text[open + 1..close]) {
		clean := trimmed_transform_text(arg)
		if clean.len == 0 {
			continue
		}
		if type_text_has_unqualified_generic_arg_bytes(clean) {
			return true
		}
		// A nested application can still have a bare base (`Pair[int]`) even
		// when all its arguments are builtin or qualified types.
		payload := strip_type_modifier_prefixes(clean)
		base, _, is_generic := generic_app_parts(payload)
		if is_generic && is_plain_type_name_text(base) && !types.is_builtin_type_name(base) {
			return true
		}
		if !is_plain_type_name_text(payload) || types.is_builtin_type_name(payload) {
			continue
		}
		return true
	}
	return false
}

// strip_type_modifier_prefixes removes the same pointer/container prefixes the
// generic spec collector strips before it inspects a type spelling.
fn strip_type_modifier_prefixes(typ string) string {
	mut text := trimmed_transform_text(typ)
	for text.len > 0 {
		if text[0] in [`&`, `?`, `!`] {
			text = trimmed_transform_text(text[1..])
			continue
		}
		if text.starts_with('...') {
			text = trimmed_transform_text(text[3..])
			continue
		}
		if text.starts_with('mut ') {
			text = trimmed_transform_text(text[4..])
			continue
		}
		if text.starts_with('chan ') {
			text = trimmed_transform_text(text[5..])
			continue
		}
		if text.starts_with('[]') {
			text = trimmed_transform_text(text[2..])
			continue
		}
		if text.starts_with('[') {
			// Fixed-array prefix (`[2]Context`); the collector strips it too.
			end := generic_matching_bracket(text, 0)
			if end > 0 && end + 1 < text.len {
				text = trimmed_transform_text(text[end + 1..])
				continue
			}
			break
		}
		break
	}
	return text
}

// is_plain_type_name_text reports whether `text` is a single unqualified type
// name (letters, digits and underscores only).
fn is_plain_type_name_text(text string) bool {
	if text == '' {
		return false
	}
	first := text[0]
	if !((first >= `a` && first <= `z`) || (first >= `A` && first <= `Z`) || first == `_`) {
		return false
	}
	for i in 0 .. text.len {
		c := text[i]
		if !((c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`)
			|| c == `_`) {
			return false
		}
	}
	return true
}

fn (mut t Transformer) collect_generic_struct_specs_range(decls map[string]GenericStructDecl, mut specs map[u32]u32, start int, end int) {
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
					node.value.contains('[') || node.value.contains('_')
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
			if params.len > 0 && !node.value.contains('[') {
				// collect_generic_struct_specs_from_node joins these into a
				// `Base[Arg]` spelling of its own, so check them here too.
				spelling = flat.source_type_id('${node.value}[${params.join(', ')}]')
			}
			if type_text_has_unqualified_generic_arg(spelling)
				|| type_text_has_unqualified_generic_arg_bytes(node.value) {
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

// type_text_has_bracket_or_underscore reports whether `typ` contains a `[` or
// a `_`, the two bytes a spelling of a generic specialization needs, in one pass.
@[direct_array_access]
fn type_text_has_bracket_or_underscore(typ_id u32) bool {
	if flat.source_type_shape(typ_id).has(.bracketed) {
		return true
	}
	typ := flat.source_type_name(typ_id)
	for i in 0 .. typ.len {
		if typ[i] == `_` {
			return true
		}
	}
	return false
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

fn (mut t Transformer) collect_generic_struct_specs_from_node(node flat.Node, module_name string, file_name string, scan_node_type bool, decls map[string]GenericStructDecl, mut specs map[u32]u32) {
	if scan_node_type {
		t.collect_generic_struct_spec_from_type(node.typ, module_name, file_name, decls, mut specs)
	}
	match node.kind {
		.sql_expr {
			t.collect_generic_struct_specs_from_sql_expr(node.value, module_name, file_name, decls, mut specs)
		}
		.struct_init, .array_init, .map_init, .cast_expr, .as_expr, .sizeof_expr, .offsetof_expr,
		.typeof_expr, .is_expr {
			t.collect_generic_struct_spec_from_type(flat.source_type_id(node.value), module_name, file_name, decls, mut specs)
			if !node.value.contains('[') && node.generic_params().len > 0 {
				t.collect_generic_struct_spec_from_type(flat.source_type_id('${node.value}[${node.generic_params().join(', ')}]'), module_name, file_name, decls, mut specs)
			}
		}
		else {}
	}
}

fn (mut t Transformer) collect_generic_struct_specs_from_sql_expr(value string, module_name string, file_name string, decls map[string]GenericStructDecl, mut specs map[u32]u32) {
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

fn (mut t Transformer) collect_generic_struct_spec_from_type(typ_id u32, module_name string, file_name string, decls map[string]GenericStructDecl, mut specs map[u32]u32) {
	typ := flat.source_type_name(typ_id)
	if typ_id == u32(0) || (!typ.contains('[') && !typ.contains('_')) {
		return
	}
	clean := trimmed_transform_text(typ)
	if clean.len == 0 {
		return
	}
	if clean.starts_with('&') {
		t.collect_generic_struct_spec_from_type(flat.source_type_id(clean[1..]), module_name, file_name, decls, mut specs)
		return
	}
	if clean.starts_with('mut ') {
		t.collect_generic_struct_spec_from_type(flat.source_type_id(clean[4..]), module_name, file_name, decls, mut specs)
		return
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		t.collect_generic_struct_spec_from_type(flat.source_type_id(clean[1..]), module_name, file_name, decls, mut specs)
		return
	}
	if clean.starts_with('...') {
		t.collect_generic_struct_spec_from_type(flat.source_type_id(clean[3..]), module_name, file_name, decls, mut specs)
		return
	}
	if clean.starts_with('[]') {
		t.collect_generic_struct_spec_from_type(flat.source_type_id(clean[2..]), module_name, file_name, decls, mut specs)
		return
	}
	if clean.starts_with('chan ') {
		t.collect_generic_struct_spec_from_type(flat.source_type_id(clean[5..]), module_name, file_name, decls, mut specs)
		return
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			t.collect_generic_struct_spec_from_type(flat.source_type_id(clean[4..bracket_end]), module_name, file_name, decls, mut specs)
			t.collect_generic_struct_spec_from_type(flat.source_type_id(clean[bracket_end + 1..]), module_name, file_name, decls, mut specs)
		}
		return
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			t.collect_generic_struct_spec_from_type(flat.source_type_id(clean[bracket_end + 1..]), module_name, file_name, decls, mut specs)
		}
		return
	}
	if !clean.contains('[') {
		// A flattened concrete generic name always includes the separator before
		// its type suffix. Ordinary scalar and qualified names cannot be specs.
		if !clean.contains('_') {
			return
		}
		args_source_ids := t.recorded_type_specialization_args(flat.source_type_id(clean)) or { return }

		spec_base := t.generic_struct_spec_base_from_flattened_type(flat.source_type_id(clean), args_source_ids, module_name, file_name, decls) or { return }
		scoped_args_source_ids := t.generic_struct_args_in_scope(args_source_ids, module_name, file_name)
		scoped_args := flat.TypeTextList{ ids: scoped_args_source_ids }.names()
		spec_name := '${spec_base}[${scoped_args.join(', ')}]'
		specs[flat.source_type_id(spec_name)] = flat.source_type_id(spec_base)
		spec_module := if decl := decls[spec_base] { decl.module } else { module_name }
		t.record_generic_specialization_args_in_module(spec_base, spec_module, scoped_args_source_ids)
		return
	}
	base, raw_args, ok := generic_app_parts(clean)
	if !ok {
		return
	}
	args_source_ids_2 := t.canonical_generic_specialization_args(flat.TypeTextList.from_names(raw_args).ids)

	for arg_source_id in args_source_ids_2 {
		arg_source_id_3 := arg_source_id

		t.collect_generic_struct_spec_from_type(arg_source_id_3, module_name, file_name, decls, mut specs)
	}
	if t.generic_args_have_placeholders(args_source_ids_2) {
		return
	}
	alias_name_id := t.generic_qualified_alias_name(flat.source_type_id(base), module_name)
	alias_name := flat.source_type_name(alias_name_id)
	if target_source_id := t.tc.type_aliases.type_id(alias_name) {
		params := t.tc.type_alias_generic_params[alias_name] or { []string{} }
		if params.len == args_source_ids_2.len {
			alias_module := if alias_name.contains('.') {
				alias_name.all_before_last('.')
			} else {
				module_name
			}
			expanded := substitute_generic_type_text_with_params(target_source_id, args_source_ids_2, params)
			normalized_source_id := t.normalize_type_in_module(expanded, alias_module)
			normalized := flat.source_type_name(normalized_source_id)
			if normalized != clean {
				t.collect_generic_struct_spec_from_type(normalized_source_id, alias_module, file_name, decls, mut specs)
				return
			}
		}
	}
	spec_base := t.generic_struct_spec_base_name(base, module_name, file_name, decls) or { return }
	scoped_args_source_ids_2 := t.generic_struct_args_in_scope(args_source_ids_2, module_name, file_name)
	scoped_args := flat.TypeTextList{ ids: scoped_args_source_ids_2 }.names()
	spec_name := '${spec_base}[${scoped_args.join(', ')}]'
	specs[flat.source_type_id(spec_name)] = flat.source_type_id(spec_base)
	spec_module := if decl := decls[spec_base] { decl.module } else { module_name }
	t.record_generic_specialization_args_in_module(spec_base, spec_module, scoped_args_source_ids_2)
}

fn (mut t Transformer) generic_struct_spec_base_from_flattened_type(typ_id u32, args_ids []u32, module_name string, file_name string, decls map[string]GenericStructDecl) ?string {
	typ := flat.source_type_name(typ_id)
	if args_ids.len == 0 {
		return none
	}
	clean := trimmed_transform_text(typ)
	clean_c := c_name(clean)
	mut found := ''
	for base, decl in decls {
		scoped_args := t.generic_struct_args_in_scope(args_ids, module_name, file_name)
		bracket_name := c_name('${base}[${flat.TypeTextList{ ids: scoped_args }.names().join(', ')}]')
		receiver_name := c_name('${base}_${generic_type_suffixes(scoped_args)}')
		if clean_c !in [bracket_name, receiver_name] {
			continue
		}
		if found != '' && found != base {
			return none
		}
		found = if resolved := t.generic_struct_spec_base_name(base, decl.module, decl.file, decls) {
			resolved
		} else {
			base
		}
	}
	if found.len > 0 {
		return found
	}
	return none
}

fn (mut t Transformer) collect_generic_sum_specs(decls map[string]GenericSumDecl, mut specs map[u32]GenericSpecContext) {
	t.ensure_node_module_map()
	for _, target_id in t.tc.type_aliases.ids {
		t.collect_generic_sum_spec_from_type(target_id, '', '', decls, mut specs)
	}
	t.collect_generic_sum_specs_range(decls, mut specs, 0, t.a.nodes.len)
}

fn (mut t Transformer) collect_generic_sum_specs_range(decls map[string]GenericSumDecl, mut specs map[u32]GenericSpecContext, start int, end int) {
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

fn (mut t Transformer) collect_generic_sum_specs_from_node(node flat.Node, module_name string, file_name string, decls map[string]GenericSumDecl, mut specs map[u32]GenericSpecContext) {
	if decls.len == 0 {
		return
	}
	if node.typ != 0 {
		t.collect_generic_sum_spec_from_type(node.typ, module_name, file_name, decls, mut specs)
	}
	match node.kind {
		.struct_init, .array_init, .map_init, .cast_expr, .as_expr, .sizeof_expr, .typeof_expr,
		.is_expr {
			t.collect_generic_sum_spec_from_type(flat.source_type_id(node.value), module_name, file_name, decls, mut specs)
			if !node.value.contains('[') && node.generic_params().len > 0 {
				t.collect_generic_sum_spec_from_type(flat.source_type_id('${node.value}[${node.generic_params().join(', ')}]'), module_name, file_name, decls, mut specs)
			}
		}
		else {}
	}
}

fn (mut t Transformer) collect_generic_sum_spec_from_type(typ_id u32, module_name string, file_name string, decls map[string]GenericSumDecl, mut specs map[u32]GenericSpecContext) {
	typ := flat.source_type_name(typ_id)
	if typ_id == u32(0) || !typ.contains('[') {
		return
	}
	clean := trimmed_transform_text(typ)
	if clean.len == 0 {
		return
	}
	if clean.starts_with('&') {
		t.collect_generic_sum_spec_from_type(flat.source_type_id(clean[1..]), module_name, file_name, decls, mut specs)
		return
	}
	if clean.starts_with('mut ') {
		t.collect_generic_sum_spec_from_type(flat.source_type_id(clean[4..]), module_name, file_name, decls, mut specs)
		return
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		t.collect_generic_sum_spec_from_type(flat.source_type_id(clean[1..]), module_name, file_name, decls, mut specs)
		return
	}
	if clean.starts_with('...') {
		t.collect_generic_sum_spec_from_type(flat.source_type_id(clean[3..]), module_name, file_name, decls, mut specs)
		return
	}
	if clean.starts_with('[]') {
		t.collect_generic_sum_spec_from_type(flat.source_type_id(clean[2..]), module_name, file_name, decls, mut specs)
		return
	}
	if clean.starts_with('chan ') {
		t.collect_generic_sum_spec_from_type(flat.source_type_id(clean[5..]), module_name, file_name, decls, mut specs)
		return
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			t.collect_generic_sum_spec_from_type(flat.source_type_id(clean[4..bracket_end]), module_name, file_name, decls, mut specs)
			t.collect_generic_sum_spec_from_type(flat.source_type_id(clean[bracket_end + 1..]), module_name, file_name, decls, mut specs)
		}
		return
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			t.collect_generic_sum_spec_from_type(flat.source_type_id(clean[bracket_end + 1..]), module_name, file_name, decls, mut specs)
		}
		return
	}
	if !clean.contains('[') {
		return
	}
	base, args, ok := generic_app_parts(clean)
	if !ok {
		return
	}
	for arg in args {
		t.collect_generic_sum_spec_from_type(flat.source_type_id(arg), module_name, file_name, decls, mut specs)
	}
	if t.generic_args_have_placeholders(flat.TypeTextList.from_names(args).ids) {
		return
	}
	// A source node that its lowering detached no longer maps to a file. Its spelling
	// (`maybe.Maybe[Token]` for `import iam { Token }` next to another imported `Token`)
	// then names no type, and keying a specialization with it would emit a bogus one.
	if file_name == '' && t.generic_args_name_unknown_type(flat.TypeTextList.from_names(args).ids) {
		return
	}
	spec_base := t.generic_sum_spec_base_name(base, module_name, file_name, decls) or { return }
	spec_name := t.generic_sum_spec_name_in_scope(spec_base, flat.TypeTextList.from_names(args).ids, module_name, file_name)
	specs[spec_name] = GenericSpecContext{
		base:   flat.source_type_id(spec_base)
		file:   file_name
		module: module_name
	}
}

fn (t &Transformer) generic_struct_spec_base_name(base string, module_name string, file_name string, decls map[string]GenericStructDecl) ?string {
	if base in decls {
		return base
	}
	if base.starts_with('main.') && !base['main.'.len..].contains('.') {
		main_base := base['main.'.len..]
		if main_base in decls {
			return main_base
		}
	}
	if base.contains('.') {
		return none
	}
	if module_name != '' && module_name != 'main' && module_name != 'builtin' {
		qbase := '${module_name}.${base}'
		if qbase in decls {
			return qbase
		}
	}
	if resolved := t.selective_import_generic_struct_base(base, file_name, decls) {
		return resolved
	}
	mut found := ''
	for key, _ in decls {
		if key.all_after_last('.') == base {
			if found != '' && found != key {
				return none
			}
			found = key
		}
	}
	if found.len > 0 {
		return found
	}
	return none
}

fn (t &Transformer) generic_sum_spec_base_name(base string, module_name string, file_name string, decls map[string]GenericSumDecl) ?string {
	if base in decls {
		return base
	}
	if base.starts_with('main.') && !base['main.'.len..].contains('.') {
		main_base := base['main.'.len..]
		if main_base in decls {
			return main_base
		}
	}
	if base.contains('.') {
		return none
	}
	if module_name != '' && module_name != 'main' && module_name != 'builtin' {
		qbase := '${module_name}.${base}'
		if qbase in decls {
			return qbase
		}
	}
	if resolved := t.selective_import_generic_sum_base(base, file_name, decls) {
		return resolved
	}
	mut found := ''
	for key, _ in decls {
		if key.all_after_last('.') == base {
			if found != '' && found != key {
				return none
			}
			found = key
		}
	}
	if found.len > 0 {
		return found
	}
	return none
}

fn (mut t Transformer) generic_sum_spec_name_in_scope(base string, args_ids []u32, module_name string, file_name string) u32 {
	scoped_args := flat.TypeTextList{ ids: t.generic_sum_args_in_scope(args_ids, module_name, file_name) }.names()
	return flat.source_type_id('${base}[${scoped_args.join(', ')}]')
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
			types.Type(parsed.base_type).source_type_id()
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
	if isnil(t.tc) {
		return false
	}
	for arg_source_id in args_ids {
		arg := flat.source_type_name(arg_source_id)

		name := trimmed_transform_text(arg)
		if name.len == 0 || !name[0].is_capital() || name.contains('.') || name.contains('[') {
			continue
		}
		if name !in t.tc.structs && !t.tc.sum_types.contains(name) && name !in t.tc.enum_names
			&& name !in t.tc.flag_enums && name !in t.tc.interface_names
			&& !t.tc.type_aliases.contains(name) {
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

fn (t &Transformer) selective_import_generic_struct_base(base string, file_name string, decls map[string]GenericStructDecl) ?string {
	if isnil(t.tc) || base.contains('.') || file_name == '' {
		return none
	}
	candidates := t.tc.file_selective_imports[file_import_key(file_name, base)] or { return none }
	for candidate in candidates {
		if candidate in decls {
			return candidate
		}
	}
	return none
}

fn (t &Transformer) selective_import_generic_sum_base(base string, file_name string, decls map[string]GenericSumDecl) ?string {
	if isnil(t.tc) || base.contains('.') || file_name == '' {
		return none
	}
	candidates := t.tc.file_selective_imports[file_import_key(file_name, base)] or { return none }
	for candidate in candidates {
		if candidate in decls {
			return candidate
		}
	}
	return none
}

fn (mut t Transformer) materialize_generic_sum_spec(spec_id u32, decl GenericSumDecl, context GenericSpecContext) {
	spec_name := flat.source_type_name(spec_id)
	_, args, ok := generic_app_parts(spec_name)
	if !ok || args.len == 0 {
		return
	}
	if variants_source_ids := t.tc.sum_types.type_ids(spec_name) {
		t.sum_types.set_ids(spec_name, variants_source_ids)
		return
	}
	if variants_source_ids := t.sum_types.type_ids(spec_name) {
		t.tc.sum_types.set_ids(spec_name, variants_source_ids)
		t.tc.register_short_type_name(spec_name)
		return
	}
	old_module := t.cur_module
	old_file := t.cur_file
	old_tc_module := t.tc.cur_module
	old_tc_file := t.tc.cur_file
	scoped_args_source_ids := t.generic_sum_args_in_scope(flat.TypeTextList.from_names(args).ids, context.module, context.file)

	t.cur_module = decl.module
	t.cur_file = decl.file
	t.tc.cur_module = decl.module
	t.tc.cur_file = decl.file
	mut variants := []u32{}
	mut seen_fields := map[string]bool{}
	for i in 0 .. decl.node.children_count {
		variant := t.a.child_node(&decl.node, i)
		variant_type := substitute_generic_type_text_with_params(flat.source_type_id(variant.value), scoped_args_source_ids, decl.node.generic_params())
		parsed := t.tc.parse_resolution_type(variant_type)
		resolved_variant := if parsed is types.Unknown {
			variant_type
		} else {
			parsed.source_type_id()
		}
		field_name := t.sum_field_name(resolved_variant)
		if field_name in seen_fields {
			continue
		}
		seen_fields[field_name] = true
		variants << resolved_variant
	}
	t.tc.sum_types.set_ids(spec_name, variants)
	t.sum_types.set_ids(spec_name, variants)
	t.tc.register_short_type_name(spec_name)
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
	spec_name := flat.source_type_name(spec_id)
	base, args, ok := generic_app_parts(spec_name)
	if !ok || args.len == 0 {
		return
	}
	t.record_generic_specialization_args_in_module(base, decl.module, flat.TypeTextList.from_names(args).ids)
	old_module := t.cur_module
	old_file := t.cur_file
	old_tc_module := t.tc.cur_module
	old_tc_file := t.tc.cur_file
	t.cur_module = decl.module
	t.cur_file = decl.file
	t.tc.cur_module = decl.module
	t.tc.cur_file = decl.file
	mut old_specialization_main_types := t.active_specialization_main_types.clone()
	t.active_specialization_main_types = t.specialization_main_type_closure(flat.TypeTextList.from_names(args).ids)
	mut fields := []types.StructField{}
	for i in 0 .. decl.node.children_count {
		field := t.a.child_node(&decl.node, i)
		if field.kind != .field_decl {
			continue
		}
		field_type := substitute_generic_type_text_with_params(field.typ, flat.TypeTextList.from_names(args).ids, decl.node.generic_params())
		locked_field_type := t.lock_colliding_main_substitution_type_text(field.typ, field_type, decl.module, decl.node.generic_params())
		// A field declared directly as its type parameter keeps the concrete
		// argument's identity (and therefore alias methods/operators). Composite
		// declarations still need normalization to expand aliases such as `Values[T]`.
		normalized_field_type := (if trimmed_transform_text(field.typ_text()) in decl.node.generic_params() {
			locked_field_type
		} else {
			t.normalize_type_alias(locked_field_type)
		})
		fields << types.StructField{
			name:        field.value
			typ:         t.tc.parse_resolution_type(normalized_field_type)
			has_default: field.children_count > 0
			is_embed:    field.typ == 0 || field.value.len == 0 || field.value == field.typ_text()
			is_mut:      field.is_mut
			is_volatile: types.source_field_decl_is_volatile(field)
		}
	}
	t.tc.structs[spec_name] = fields
	t.tc.register_short_type_name(spec_name)
	cspec_name := c_name(spec_name)
	if cspec_name != spec_name {
		t.tc.structs[cspec_name] = fields
		t.tc.register_short_type_name(cspec_name)
		if decl.module.len > 0 {
			t.tc.struct_modules[cspec_name] = decl.module
		}
	}
	if decl.key in t.tc.unions {
		t.tc.unions[spec_name] = true
		if cspec_name != spec_name {
			t.tc.unions[cspec_name] = true
		}
	}
	if decl.key in t.tc.params_structs {
		t.tc.params_structs[spec_name] = true
		if cspec_name != spec_name {
			t.tc.params_structs[cspec_name] = true
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
				cur_file = node.value
				cur_module = t.tc.file_modules[cur_file] or { '' }
			}
			.module_decl {
				cur_module = node.value
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

@[direct_array_access]
fn (mut t Transformer) monomorphize_ignored_nodes(decls map[string]GenericFnDecl) []bool {
	mut nodes := []bool{len: t.a.nodes.len}
	mut stack := []flat.NodeId{cap: 256}
	for _, decl in decls {
		t.collect_node_subtree_flags(decl.id, mut nodes, mut stack)
	}
	if !t.has_used_fn_filter() {
		return nodes
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
				t.cur_module = node.value
			}
			.fn_decl {
				if node.value.starts_with('test_') {
					continue
				}
				t.cur_module = t.node_module_or(i, t.cur_module)
				if !t.should_transform_fn(*node) {
					t.collect_node_subtree_flags(flat.NodeId(i), mut nodes, mut stack)
				}
			}
			else {}
		}
	}
	t.cur_module = old_module
	return nodes
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
	mut cur_module := ''
	mut cur_file := ''
	for i := t.node_module_map_nodes; i < t.a.nodes.len; i++ {
		// Context indexes can change here, but the AST arena cannot.
		node := unsafe { &t.a.nodes[i] }
		match node.kind {
			.file {
				cur_file = node.value
				cur_module = t.tc.file_modules[node.value] or { '' }
				t.mark_node_context(flat.NodeId(i), cur_module, cur_file)
			}
			.module_decl {
				cur_module = node.value
			}
			.fn_decl, .const_decl, .global_decl, .struct_decl, .type_decl, .enum_decl,
			.interface_decl {
				// Generated roots already carry their declaration context. Preserve it
				// when incrementally scanning an append-only region that has no `.file`
				// marker of its own.
				context_module := t.node_module_or(i, cur_module)
				context_file := t.node_file_or(i, cur_file)
				t.mark_node_context(flat.NodeId(i), context_module, context_file)
			}
			else {}
		}
	}
	t.node_module_map_nodes = t.a.nodes.len
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
		vmemset(&cache[old_len], 0, isize(size - old_len) * isize(sizeof(u16)))
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
	if clone_id := t.generic_fn_spec_nodes[spec_key] {
		return clone_id
	}
	if t.generic_fn_specs_in_progress[spec_key] {
		return flat.empty_node
	}
	t.generic_fn_specs_in_progress[spec_key] = true
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
		if param_child.value.len == 0 || param_child.typ == 0 {
			continue
		}
		mut param_raw := param_child.typ_text()
		if param_raw.starts_with('mut ') {
			param_raw = param_raw[4..]
		}
		if param_raw.starts_with('...') {
			param_raw = '[]' + param_raw[3..]
		}
		// `mut val T` params are recorded as `&T`; the declared language-level
		// type is `T`, and that is what a by-value use of the param must infer.
		// An explicit `mut val &T` keeps its `&T` rvalue type; only its ABI slot is `&&T`.
		if param_child.is_mut && param_child.op != .amp && param_raw.starts_with('&') {
			param_raw = param_raw[1..]
		}
		param_substituted_source_id := t.subst_type(flat.source_type_id(param_raw), concrete_args_source_ids)

		if param_substituted_source_id != 0 {
			t.set_var_type(param_child.value, param_substituted_source_id)
		}
		if param_child.is_mut || param_child.op == .amp || flat.source_type_shape(param_child.typ).has(.mutable_type) {
			t.mut_param_values[param_child.value] = true
		}
	}
	t.cloning_generic_fn_depth++
	old_clone_ret_type_source_id := t.cur_fn_ret_type

	t.set_cur_fn_ret_type(t.specialized_signature_type_text(decl, t.generic_fn_return_type_text(decl), concrete_args_source_ids, t.active_generic_params))
	// A check clones a library instance without its body when the body cannot
	// ask for an instance of the program (see LibraryBodies).
	library_clone := !isnil(t.tc) && t.tc.check_concrete_generic_bodies
		&& decl.file !in t.tc.diagnostic_files
	header_only := library_clone && !t.library_instance_needs_body(concrete_args_source_ids.clone())
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
	t.generic_fn_spec_nodes[spec_key] = clone_id
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
		&& decl.file in t.tc.diagnostic_files {
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
	clone_name := t.a.nodes[int(clone_id)].value
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
		return child.kind == .comptime_if && generic_text_contains_param(child.value, params)
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
	t.cur_fn_name = node.value
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
		if child.value.len == 0 {
			continue
		}
		raw_source_typ := (if flat.source_type_shape(child.typ).has(.variadic) {
			flat.source_type_id('[]' + child.typ_text()[3..])
		} else {
			child.typ
		})
		raw_typ := if child.typ != 0 {
			if flat.source_type_shape(child.typ).has(.variadic) {
				'[]' + flat.source_type_name(t.normalize_type_alias(flat.source_type_id(child.typ_text()[3..])))
			} else {
				flat.source_type_name(t.normalize_type_alias(child.typ))
			}
		} else {
			''
		}
		typ := (if raw_typ.len > 0 {
			flat.source_type_id(raw_typ)
		} else if param_idx < param_types.len {
			t.normalize_type_alias(param_types[param_idx].source_type_id())
		} else if param_idx == 0 {
			t.fn_body_receiver_type(node.value)
		} else {
			u32(0)
		})
		if typ != 0 {
			t.set_var_type_with_raw(child.value, typ, raw_source_typ)
		}
		if child.is_mut || child.op == .amp || flat.source_type_shape(child.typ).has(.mutable_type) {
			t.mut_param_values[child.value] = true
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
	if lhs.kind != .ident || lhs.value.len == 0 {
		return
	}
	raw_typ_source_id := t.generated_decl_assign_binding_type(node, lhs, rhs_id)

	if raw_typ_source_id != 0 {
		t.set_var_type_with_raw(lhs.value, t.normalize_type_alias(raw_typ_source_id), raw_typ_source_id)
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
	if filter_candidates && node.value.len > 0 && !isnil(t.tc)
		&& t.late_scan_gate_rejects(node.value, candidate_names) {
		return none
	}
	if node.kind == .ident && node.value.len > 0 {
		if (t.var_type(node.value)) != 0 {
			return none
		}
		if t.generated_used_name_is_known_fn(node.value) {
			return node.value
		}
		if !node.value.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
			&& t.cur_module != 'builtin' {
			qname := '${t.cur_module}.${node.value}'
			if t.generated_used_name_is_known_fn(qname) {
				return qname
			}
		}
		return none
	}
	if node.kind == .selector && node.children_count > 0 && node.value.len > 0 {
		base := t.a.child_node(&node, 0)
		if base.kind != .ident || base.value.len == 0 {
			return none
		}
		// An enum value names no function. Few selectors spell a known function, so
		// that is settled first and the enum question is only asked for those.
		mut name := ''
		full := '${base.value}.${node.value}'
		if t.generated_used_name_is_known_fn(full) {
			name = full
		}
		if name.len == 0 && !isnil(t.tc) && t.cur_file.len > 0 {
			if mod := t.file_import_module(t.cur_file, base.value) {
				resolved := '${mod}.${node.value}'
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
		if fn_node.kind == .selector && fn_node.value.len > 0 && fn_node.children_count > 0 {
			base_id := t.a.child(&fn_node, 0)
			base_type := flat.source_type_name(t.node_type(base_id)).trim_left('&')
			_, _, base_is_generic := generic_app_parts(base_type)
			if base_is_generic {
				return '${base_type}.${fn_node.value}'
			}
		}
		if fn_node.kind == .ident && fn_node.value.len > 0 && (t.var_type(fn_node.value)) != 0 {
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
	if fn_node.kind == .ident && fn_node.value.len > 0 {
		if (t.var_type(fn_node.value)) != 0 {
			return ''
		}
		return fn_node.value
	}
	if fn_node.kind == .selector && fn_node.value.len > 0 && fn_node.children_count > 0 {
		base_id := t.a.child(&fn_node, 0)
		method_name := t.resolve_receiver_method_name(base_id, fn_node.value)
		if method_name.len > 0 {
			return method_name
		}
		base_type := flat.source_type_name(t.node_type(base_id)).trim_left('&')
		_, _, base_is_generic := generic_app_parts(base_type)
		if base_is_generic {
			return '${base_type}.${fn_node.value}'
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
	receiver_name := call_name.all_before_last('.')
	if receiver_name.len == 0 {
		return true
	}
	base_type_source_id_4 := t.generated_selector_receiver_type(fn_node)
	base_type := flat.source_type_name(base_type_source_id_4)
	if base_type_source_id_4 == 0 {
		return true
	}
	if receiver_name == base_type {
		return true
	}
	if !base_type.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' && receiver_name == '${t.cur_module}.${base_type}' {
		return true
	}
	if receiver_name.all_after_last('.') != base_type.all_after_last('.') {
		return true
	}
	return false
}

fn (t &Transformer) generated_selector_receiver_call_name(node flat.Node) ?string {
	if node.children_count == 0 {
		return none
	}
	fn_node := t.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.children_count == 0 || fn_node.value.len == 0 {
		return none
	}
	base_type_source_id_5 := t.generated_selector_receiver_type(fn_node)
	base_type := flat.source_type_name(base_type_source_id_5)
	if base_type_source_id_5 == 0 {
		return none
	}
	return '${base_type}.${fn_node.value}'
}

fn (t &Transformer) generated_selector_receiver_type(fn_node flat.Node) u32 {
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return u32(0)
	}
	base_id := t.a.child(&fn_node, 0)
	base_node := t.a.nodes[int(base_id)]
	mut base_type := u32(0)
	if base_node.kind == .ident {
		base_type = t.var_type(base_node.value)
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
	t.register_specialized_fn_signature_value(decl, clone.value, args_ids)
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
	record_signature_types := signature_key !in t.monomorph_signature_spec_keys
	if record_signature_types {
		t.monomorph_signature_spec_keys[signature_key] = true
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
			params << t.tc.parse_resolution_type(param_type)
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
			t.tc.fn_ret_types[owned_name] = ret
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
	qualified := flat.source_type_name(qualified_source_id)
	if !isnil(t.tc) {
		alias_base, _, applied_alias := generic_app_parts(qualified)
		if applied_alias && (t.tc.type_aliases.contains(alias_base)
			|| '${decl.module}.${alias_base}' in t.tc.type_aliases.ids) {
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
	if locked != substituted && qualified.contains('main.') {
		return qualified_source_id
	}
	is_shared := trimmed_transform_text(qualified).starts_with('shared ')
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
	if trimmed_transform_text(qualified).starts_with('...') {
		if parsed is types.Array {
			return flat.source_type_id('...' + flat.source_type_name(specialized_signature_storage_type_name(parsed.elem_type)))
		}
		return qualified_source_id
	}
	if is_shared {
		return qualified_source_id
	}
	return specialized_signature_storage_type_name(parsed)
}

fn (t &Transformer) specialized_direct_generic_type_text(typ_id u32, args_ids []u32, params []string) ?u32 {
	typ := flat.source_type_name(typ_id)
	clean := trimmed_transform_text(typ)
	for i, param in params {
		if clean == param && i < args_ids.len {
			return args_ids[i]
		}
	}
	for prefix in ['mut ', 'shared ', 'atomic ', '...', '[]', '?', '!', '&'] {
		if clean.starts_with(prefix) {
			inner_source_id := t.specialized_direct_generic_type_text(flat.source_type_id(clean[prefix.len..]), args_ids, params) or {
				return none
			}
			inner := flat.source_type_name(inner_source_id)
			if (prefix == '?' && inner.starts_with('?'))
				|| (prefix == '!' && inner.starts_with('!')) {
				return inner_source_id
			}
			return flat.source_type_id(prefix + inner)
		}
	}
	return none
}

fn specialized_signature_storage_type_name(typ types.Type) u32 {
	return flat.source_type_id(specialized_signature_storage_type_name_bytes(typ))
}

fn (t &Transformer) qualify_specialized_signature_type_text(typ_id u32, decl GenericFnDecl) u32 {
	typ := flat.source_type_name(typ_id)
	clean := trimmed_transform_text(typ)
	if clean.len == 0 || isnil(t.tc) {
		return typ_id
	}
	if clean.starts_with('&') {
		return flat.source_type_id('&' + flat.source_type_name(t.qualify_specialized_signature_type_text(flat.source_type_id(clean[1..]), decl)))
	}
	if clean.starts_with('mut ') {
		inner := flat.source_type_name(t.qualify_specialized_signature_type_text(flat.source_type_id(clean[4..]), decl))
		return flat.source_type_id('mut ' + inner)
	}
	if clean.starts_with('?') {
		return flat.source_type_id('?' + flat.source_type_name(t.qualify_specialized_signature_type_text(flat.source_type_id(clean[1..]), decl)))
	}
	if clean.starts_with('!') {
		return flat.source_type_id('!' + flat.source_type_name(t.qualify_specialized_signature_type_text(flat.source_type_id(clean[1..]), decl)))
	}
	if clean.starts_with('...') {
		return flat.source_type_id('...' + flat.source_type_name(t.qualify_specialized_signature_type_text(flat.source_type_id(clean[3..]), decl)))
	}
	if clean.starts_with('shared ') {
		return flat.source_type_id('shared ' + flat.source_type_name(t.qualify_specialized_signature_type_text(flat.source_type_id(clean[7..]), decl)))
	}
	for prefix in ['atomic ', 'chan ', 'thread '] {
		if clean.starts_with(prefix) {
			return flat.source_type_id(prefix + flat.source_type_name(t.qualify_specialized_signature_type_text(flat.source_type_id(clean[prefix.len..]), decl)))
		}
	}
	if clean.starts_with('[]') {
		return flat.source_type_id('[]' + flat.source_type_name(t.qualify_specialized_signature_type_text(flat.source_type_id(clean[2..]), decl)))
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		params, ret := fn_type_text_parts(clean) or { return flat.source_type_id(clean) }
		mut qualified_params := []string{cap: params.len}
		for param in params {
			qualified_params << flat.source_type_name(t.qualify_specialized_signature_type_text(flat.source_type_id(generic_fn_type_param_mode_payload(param)), decl))
		}
		qualified_ret_source_id := t.qualify_specialized_signature_type_text(flat.source_type_id(ret), decl)
		qualified_ret := flat.source_type_name(qualified_ret_source_id)
		return if qualified_ret_source_id != 0 {
			flat.source_type_id('fn (${qualified_params.join(', ')}) ${qualified_ret}')
		} else {
			flat.source_type_id('fn (${qualified_params.join(', ')})')
		}
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := flat.source_type_name(t.qualify_specialized_signature_type_text(flat.source_type_id(clean[4..bracket_end]), decl))
			val := flat.source_type_name(t.qualify_specialized_signature_type_text(flat.source_type_id(clean[bracket_end + 1..]), decl))
			return flat.source_type_id('map[${key}]${val}')
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return flat.source_type_id(clean[..bracket_end + 1] + flat.source_type_name(t.qualify_specialized_signature_type_text(flat.source_type_id(clean[bracket_end + 1..]), decl)))
		}
	}
	if fixed := t.canonical_suffix_fixed_array_arg(clean) {
		return t.qualify_specialized_signature_type_text(flat.source_type_id(fixed), decl)
	}
	if clean.starts_with('(') && clean.ends_with(')') && clean.contains(',') {
		mut parts := []string{}
		for part in split_generic_args(clean[1..clean.len - 1]) {
			parts << flat.source_type_name(t.qualify_specialized_signature_type_text(flat.source_type_id(part), decl))
		}
		return flat.source_type_id('(' + parts.join(', ') + ')')
	}
	bracket := clean.index_u8(`[`)
	if bracket > 0 {
		bracket_end := generic_matching_bracket(clean, bracket)
		if bracket_end < clean.len {
			mut parts := []string{}
			for part in split_generic_args(clean[bracket + 1..bracket_end]) {
				parts << flat.source_type_name(t.qualify_specialized_signature_type_text(flat.source_type_id(part), decl))
			}
			base := flat.source_type_name(t.qualify_specialized_signature_type_text(flat.source_type_id(clean[..bracket]), decl))
			return flat.source_type_id(base + '[' + parts.join(', ') + ']' + clean[bracket_end + 1..])
		}
	}
	if clean.contains('.') || types.is_builtin_type_name(clean)
		|| clean in ['void', 'none', 'nil', 'C', 'JS'] {
		return flat.source_type_id(clean)
	}
	if selective := t.selective_signature_type_symbol(decl.file, clean) {
		return flat.source_type_id(selective)
	}
	return flat.source_type_id(clean)
}

fn (t &Transformer) selective_signature_type_symbol(file string, name string) ?string {
	if isnil(t.tc) || file == '' || name.contains('.') {
		return none
	}
	candidates := t.tc.file_selective_imports[file_import_key(file, name)] or { return none }
	for candidate in candidates {
		if t.tc.type_aliases.contains(candidate) || candidate in t.tc.structs
			|| candidate in t.tc.interface_names || candidate in t.tc.flag_enums
			|| candidate in t.tc.enum_names || t.tc.sum_types.contains(candidate) {
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

fn (mut t Transformer) rewrite_generic_call_sites(decls map[string]GenericFnDecl, call_sites []GenericCallSite) {
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
		args := flat.TypeTextList{ ids: args_source_ids }.names()

		decl := decls[decl_key] or { continue }
		if !t.call_has_source_generic_args(node) && t.generic_args_contain_alias(args_source_ids, decl.module) {
			t.set_node_value(i, args.join(', '))
		}
		concrete_args_source_ids := t.canonical_generic_specialization_args(args_source_ids)

		if t.generic_decl_is_receiver_method(decl.node)
			&& !t.generic_call_is_static_assoc_selector(node, decl) {
			t.rewrite_generic_method_call(site.id, node, decl, concrete_args_source_ids)
		} else {
			t.rewrite_generic_plain_call(site.id, node, decl, concrete_args_source_ids)
		}
	}
}

fn (mut t Transformer) refresh_decl_assign_types_after_generic_rewrite() bool {
	mut changed := false
	for i in 0 .. t.a.nodes.len {
		if t.a.nodes[i].kind !in [.call, .or_expr, .decl_assign] {
			continue
		}
		// Only candidate nodes need copies before AST rewriting.
		node := t.a.nodes[i]
		if node.kind == .call {
			if concrete_type_source_id := t.concrete_fn_alias_call_return_type(i, node) {
				concrete_type := flat.source_type_name(concrete_type_source_id)

				if node.typ_text() != concrete_type {
					t.set_node_typ(i, concrete_type_source_id)
					t.clear_typechecker_node_cache(i)
					changed = true
				}
			}
		}
		if node.kind == .or_expr && node.children_count > 0 {
			child := t.a.child_node(&node, 0)
			if child.kind == .call && child.typ_text().len > 1
				&& (flat.source_type_shape(child.typ).has(.option_type) || flat.source_type_shape(child.typ).has(.result_type)) {
				base_type := flat.source_type_id(child.typ_text()[1..])
				if base_type != 0 && !t.generic_arg_is_unresolved(base_type) {
					if node.typ_text() != flat.source_type_name(base_type) {
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
					&& t.generic_callee_is_specialization(callee.value)
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
		if !(specialized_call_or && node.typ_text() != flat.source_type_name(concrete_rhs_type))
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
			t.set_var_type_with_raw(lhs.value, concrete_rhs_type, decl_type_source_id_2)
		}
	}
	return changed
}

fn (mut t Transformer) concrete_fn_alias_call_return_type(id int, node flat.Node) ?u32 {
	if node.children_count == 0 || isnil(t.tc) {
		return none
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .ident || callee.value.len == 0 {
		return none
	}
	ret := t.tc.fn_ret_types[callee.value] or { return none }
	ret_name := ret.name()
	base, _, is_generic_alias := generic_app_parts(ret_name)
	if !is_generic_alias {
		return none
	}
	// Most call identifiers are ordinary functions. Avoid the contextual scope
	// lookup until the return type proves this can be a generic function alias.
	if (t.var_type(callee.value)) != 0 {
		return none
	}
	module_name := t.node_module_or(id, t.cur_module)
	if !t.generic_arg_is_alias_name(base, module_name) {
		return none
	}
	old_module := t.cur_module
	old_file := t.cur_file
	t.cur_module = module_name
	t.cur_file = t.node_file_or(id, old_file)
	concrete_source_id := t.normalize_type_alias(flat.source_type_id(ret_name))
	concrete := flat.source_type_name(concrete_source_id)
	t.cur_module = old_module
	t.cur_file = old_file
	if concrete == ret_name || (!concrete.starts_with('fn(') && !concrete.starts_with('fn (')) {
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
		&& (callee.value.contains('_T_') || t.generic_callee_is_specialization(callee.value)) {
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
		if exact_source_ids := t.recorded_generic_specialization_args(callee.value) {
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
		t.set_node_value(int(id), flat.TypeTextList{ ids: args }.names().join(', '))
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
	if callee.kind == .ident && t.generic_callee_is_specialization(callee.value) {
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
		if exact_source_ids := t.recorded_generic_specialization_args(callee.value) {
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
	callee := t.a.child_node(&node, 0)
	mut args := []u32{}
	if callee.kind == .ident {
		if exact_source_ids := t.recorded_generic_specialization_args(callee.value) {
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
		t.set_node_value(int(id), flat.TypeTextList{ ids: args }.names().join(', '))
	}
	params := t.generic_fn_param_names(decl.node, decl.module)
	mut result := []types.Type{}
	for i in 0 .. decl.node.children_count {
		child := t.a.child_node(&decl.node, i)
		if child.kind != .param {
			if t.prefix_param_scan {
				break
			}
			continue
		}
		param_type := explicit_mut_pointer_param_type_text(child, t.specialized_signature_type_text(decl, child.typ, args, params))
		result << t.tc.parse_canonical_type(param_type)
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
	return t.specialized_generic_call_param_type_texts(decl, args_source_ids).clone()
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
		return t.generic_name_cannot_spell_decl(callee.value) && !callee.value.contains('_T_')
	}
	if callee.kind == .selector {
		return t.generic_name_cannot_spell_decl(callee.value)
			&& !t.generic_static_method_names[callee.value]
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
	if callee.kind !in [.ident, .selector] || callee.value.len == 0 {
		return false
	}
	return has_explicit_generic_args || t.call_is_normalized_explicit_generic_call(node)
		|| t.generic_fn_call_names[callee.value]
		|| callee.value.contains('_T_')
		|| t.generic_callee_is_specialization(callee.value)
}

fn (t &Transformer) synthetic_recorded_generic_call(node flat.Node) bool {
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	callee := t.a.child_node(&node, 0)
	return callee.kind == .ident
		&& (callee.value.contains('_T_') || t.generic_callee_is_specialization(callee.value))
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
	if callee.kind != .ident || callee.value.len == 0 {
		return false
	}
	return (callee.value.contains('[') && callee.value.contains(']'))
		|| callee.value.contains('_T_')
		|| t.generic_callee_is_specialization(callee.value)
}

// call_may_target_generic is call_name_can_target_generic(node) ||
// synthetic_generic_call_has_exact_identity(node), with the specialization lookup
// that both end with, their costly part, done once.
fn (t &Transformer) call_may_target_generic(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	direct := t.a.nodes[int(t.a.child(&node, 0))]
	if direct.kind == .ident && direct.value.len > 0 && ((direct.value.contains('[')
		&& direct.value.contains(']')) || direct.value.contains('_T_')) {
		return true
	}
	mut callee := direct
	mut has_explicit_generic_args := false
	if callee.kind == .index && callee.children_count > 0 && callee.value != 'range' {
		has_explicit_generic_args = true
		callee = t.a.nodes[int(t.a.child(&callee, 0))]
	}
	if callee.kind !in [.ident, .selector] || callee.value.len == 0 {
		return false
	}
	if has_explicit_generic_args || t.call_is_normalized_explicit_generic_call(node)
		|| t.generic_fn_call_names[callee.value] || callee.value.contains('_T_') {
		return true
	}
	// Here `callee` is the call's own child: the one both tests look up.
	return t.generic_callee_is_specialization(callee.value)
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
		|| !t.generic_name_cannot_spell_decl(base.value) {
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
				t.infer_generic_variadic_call_arg(child.typ, elem_param_type, arg_id, arg_ids.len == 1, mut inferred)
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
		if !is_generic_fn_placeholder_name(flat.source_type_name(inference_param_type)) {
			inferred_arg_type = t.generic_composite_inference_alias_type(inferred_arg_type,
				call_module)
		}
		if child.is_mut && child.op == .amp && flat.source_type_shape(inferred_arg_type).has(.pointer) {
			// The explicit `mut p &T` marker addresses the caller's pointer slot;
			// it is ABI indirection, not part of T's inferred source type.
			inferred_arg_type = flat.source_type_id(flat.source_type_name(inferred_arg_type)[1..])
		}
		if is_recv_param && t.call_is_selector_form(node)
			&& t.generic_receiver_needs_decl_type_fallback(child.typ, inferred_arg_type) {
			recv := t.a.nodes[int(arg_id)]
			if recv.kind == .ident {
				lookup_before := if node.pos.offset > 0 { node } else { recv }
				if decl_type_source_id := t.local_decl_type_before_pos(recv.value, lookup_before) {
					inferred_arg_type = t.generic_receiver_decl_inference_type(child.typ, decl_type_source_id, call_module)
				} else if decl_type_source_id := t.local_decl_type_before(recv.value, call_id) {
					inferred_arg_type = t.generic_receiver_decl_inference_type(child.typ, decl_type_source_id, call_module)
				}
			}
		}
		if (child.is_mut || flat.source_type_shape(child.typ).has(.mutable_type)) && flat.source_type_shape(inferred_arg_type).has(.pointer) {
			// A mutable parameter's leading pointer is its storage ABI, whether the
			// generic placeholder is direct (`T`) or nested (`Container[T]`).
			inferred_arg_type = flat.source_type_id(flat.source_type_name(inferred_arg_type)[1..])
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
	return trimmed_transform_text(flat.source_type_name(param_type_id)).starts_with('...')
}

fn generic_variadic_elem_param_text(param_type_id u32) u32 {
	param_type := flat.source_type_name(param_type_id)
	clean := trimmed_transform_text(param_type)
	if clean.starts_with('...') {
		return flat.source_type_id(trimmed_transform_text(clean[3..]))
	}
	return flat.source_type_id(clean)
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

fn (mut t Transformer) infer_generic_variadic_call_arg(param_type_id u32, elem_param_type_id u32, arg_id flat.NodeId, prefer_array_spread bool, mut inferred map[string]u32) {
	if spread_id := t.generic_call_spread_arg_child(arg_id) {
		inferred_arg_type := t.generic_call_arg_type_for_inference(spread_id)
		arg_type := generic_arg_type_for_param(param_type_id, inferred_arg_type)
		if arg_type != 0 {
			infer_generic_type_args(param_type_id, arg_type, mut inferred)
		}
		return
	}
	inferred_arg_type := t.generic_call_arg_type_for_inference(arg_id)
	if prefer_array_spread && flat.source_type_shape(inferred_arg_type).has(.array_type) {
		arg_type := generic_arg_type_for_param(param_type_id, inferred_arg_type)
		if arg_type != 0 {
			infer_generic_type_args(param_type_id, arg_type, mut inferred)
		}
		return
	}
	arg_type := generic_arg_type_for_param(elem_param_type_id, inferred_arg_type)
	if arg_type != 0 {
		infer_generic_type_args(elem_param_type_id, arg_type, mut inferred)
	}
	t.infer_generic_struct_init_args(elem_param_type_id, arg_id, mut inferred)
	// The short struct-init call syntax for a variadic generic struct param
	// (`f(a: x, b: y)` for `fn f[T](items ...Params[T])`) packs the element's
	// fields as bare `field_init` args rather than a wrapped `struct_init`, so
	// `infer_generic_struct_init_args` above cannot see them. Infer T from the
	// field's declared type the same way the non-variadic path does.
	t.infer_generic_field_init_type_arg(elem_param_type_id, arg_id, mut inferred)
}

// infer_generic_field_init_type_arg infers generic type args from a single
// named `field_init` argument mapped onto a generic struct's declared field
// type (used by the variadic short struct-init call path).
fn (mut t Transformer) infer_generic_field_init_type_arg(param_type_id u32, field_id flat.NodeId, mut inferred map[string]u32) {
	param_type := flat.source_type_name(param_type_id)
	if int(field_id) < 0 || int(field_id) >= t.a.nodes.len {
		return
	}
	field := t.a.nodes[int(field_id)]
	if field.kind != .field_init || field.value.len == 0 || field.children_count == 0 {
		return
	}
	mut struct_param_type := t.normalize_type_alias_chain(flat.source_type_id(trimmed_transform_text(param_type)))
	if flat.source_type_shape(struct_param_type).has(.pointer) {
		struct_param_type = flat.source_type_id(trimmed_transform_text(flat.source_type_name(struct_param_type)[1..]))
	}
	param_base, param_args, is_generic_struct := generic_app_parts(flat.source_type_name(struct_param_type))
	if !is_generic_struct || param_args.len == 0 {
		return
	}
	info := t.lookup_struct_info(flat.source_type_id(param_base)) or { return }
	mut field_type := u32(0)
	for struct_field in info.fields {
		if struct_field.name == field.value {
			field_type = struct_field.typ
			break
		}
	}
	if field_type == 0 {
		return
	}
	// Rebase the struct's own generic parameter names onto the applied args so
	// e.g. `[]A` (from `struct Params[A]`) becomes `[]T` for `...Params[T]`.
	struct_params := t.struct_generic_params_for_base(param_base)
	if struct_params.len > 0 && struct_params != param_args {
		field_type = substitute_generic_type_text_with_params(field_type, flat.TypeTextList.from_names(param_args).ids, struct_params)
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
	if arg.kind == .prefix && arg.value == '...' && arg.children_count > 0 {
		return t.a.child(&arg, 0)
	}
	return none
}

fn (mut t Transformer) rewrite_generic_plain_call(id flat.NodeId, node flat.Node, decl GenericFnDecl, args_ids []u32) {
	if node.children_count > 0 {
		callee := t.a.child_node(&node, 0)
		if callee.kind == .ident && !t.generic_callee_is_specialization(callee.value)
			&& t.plain_concrete_callee_shadows_decl(callee.value, decl) {
			return
		}
	}
	concrete_args_source_ids := t.canonical_generic_specialization_args(args_ids)
	concrete_args := flat.TypeTextList{ ids: concrete_args_source_ids }.names()
	spec_value := specialized_generic_fn_value(decl.node.value, concrete_args_source_ids)
	spec_name := transform_qualified_fn_name(decl.module, spec_value)
	preserved_args := concrete_args.join(', ')
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
	t.set_node(int(id), flat.Node{
		kind:           .call
		op:             node.op
		children_start: start
		children_count: flat.child_count(children.len)
		pos:            node.pos
		value:          preserved_args
		typ:            ret_typ_source_id
	})
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
	return callee.kind == .ident && t.generic_callee_is_specialization(callee.value)
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
		return flat.source_type_id('&${flat.source_type_name(typ_id)}')
	}
	return typ_id
}

fn mutable_fn_value_param_type_text(param flat.Node, slot_type_id u32) u32 {
	slot_type := flat.source_type_name(slot_type_id)
	if !param.is_mut {
		return slot_type_id
	}
	value_type := if slot_type.starts_with('&') { slot_type[1..] } else { slot_type }
	return flat.source_type_id('mut ${value_type}')
}

fn (mut t Transformer) specialized_call_target_type_text(decl GenericFnDecl, typ_id u32, args_ids []u32, params []string) u32 {
	return t.specialized_signature_type_text(decl, typ_id, args_ids, params)
}

fn (mut t Transformer) retype_generic_call_literal_arg(arg_id flat.NodeId, param_type_id u32) flat.NodeId {
	param_type := flat.source_type_name(param_type_id)
	if int(arg_id) < 0 || param_type_id == u32(0) {
		return arg_id
	}
	node := t.a.nodes[int(arg_id)]
	if node.kind == .none_expr {
		if t.is_optional_type_name(param_type_id) {
			return t.make_optional_none(t.resolve_substituted_type_text(t.qualify_optional_type(param_type_id)))
		}
		if param_type == '__v_option' || param_type.starts_with('__v_option_') {
			return t.make_optional_none(param_type_id)
		}
	}
	if node.kind == .struct_init && (node.value == '__v_option' || t.generic_optional_none_init(node))
		&& (t.is_optional_type_name(param_type_id) || param_type.starts_with('__v_option_')) {
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
	if node.kind == .ident && node.value.contains('arr_lit') && flat.source_type_shape(node.typ).has(.array_type)
		&& param_type.starts_with('[]') && node.typ_text() != param_type {
		if t.retype_lowered_array_literal_temp(arg_id, node.value, param_type_id) {
			t.set_node_typ(int(arg_id), param_type_id)
			t.set_var_type(node.value, param_type_id)
		}
	}
	return arg_id
}

fn (t &Transformer) generic_optional_none_init(node flat.Node) bool {
	if node.kind != .struct_init
		|| (!t.is_optional_type_name(flat.source_type_id(node.value)) && !t.is_optional_type_name(node.typ)) {
		return false
	}
	mut has_false_ok := false
	for i in 0 .. node.children_count {
		field := t.a.child_node(&node, i)
		if field.kind != .field_init {
			continue
		}
		if field.value == 'value' {
			return false
		}
		if field.value == 'ok' && field.children_count > 0 {
			value := t.a.child_node(field, 0)
			has_false_ok = value.kind == .bool_literal && value.value != 'true'
		}
	}
	return has_false_ok
}

fn (mut t Transformer) specialize_generic_fn_value_arg(arg_id flat.NodeId, expected_type_id u32, defer_emit bool) ?flat.NodeId {
	expected_type := flat.source_type_name(expected_type_id)
	if int(arg_id) < 0 || expected_type_id == u32(0) {
		return none
	}
	arg := t.a.nodes[int(arg_id)]
	if arg.kind != .ident {
		return none
	}
	mut expected := trimmed_transform_text(expected_type)
	if expected.starts_with('...') {
		expected = trimmed_transform_text(expected[3..])
	}
	if expected.starts_with('[]fn') {
		expected = expected[2..]
	}
	if !expected.starts_with('fn') {
		return none
	}
	name := if !isnil(t.tc) {
		t.tc.resolved_fn_value_name(arg_id) or { arg.value }
	} else {
		arg.value
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
		mut signature_params := []string{}
		for i in 0 .. decl.node.children_count {
			param := t.a.child_node(&decl.node, i)
			if param.kind != .param {
				if t.prefix_param_scan {
					break
				}
				continue
			}
			signature_params << param.typ_text()
		}
		signature := 'fn (${signature_params.join(', ')})${if decl.node.typ_text().len > 0
			&& decl.node.typ_text() != 'void' {
			' ${decl.node.typ_text()}'
		} else {
			''
		}}'
		mut inferred := map[string]u32{}
		infer_generic_type_args(flat.source_type_id(signature), flat.source_type_id(expected), mut inferred)
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
		t.set_node_typ(int(result), flat.source_type_id(expected))
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
	array_type := flat.source_type_name(array_type_id)
	if !array_type.starts_with('[]') {
		return false
	}
	elem_type := array_type[2..]
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
			if lhs.kind == .ident && lhs.value == name {
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
	t.retype_array_new_call(rhs_id, flat.source_type_id(elem_type), array_type_id)
	for node_idx in decl_idx + 1 .. int(arg_id) {
		node := t.a.nodes[node_idx]
		if node.kind == .call && t.call_is_array_push_to_name(node, name) {
			value_name := t.array_push_value_name(node) or { continue }
			t.retype_decl_assign_name_in_range(value_name, flat.source_type_id(elem_type), decl_idx + 1, int(arg_id))
		}
	}
	return true
}

fn (mut t Transformer) retype_array_new_call(call_id flat.NodeId, elem_type_id u32, array_type_id u32) {
	elem_type := flat.source_type_name(elem_type_id)

	if int(call_id) < 0 || int(call_id) >= t.a.nodes.len {
		return
	}
	call := t.a.nodes[int(call_id)]
	if call.kind != .call || call.children_count < 2 {
		return
	}
	callee := t.a.child_node(&call, 0)
	if callee.kind != .ident || callee.value != 'array_new' {
		return
	}
	sizeof_id := t.a.child(&call, 1)
	if int(sizeof_id) >= 0 && int(sizeof_id) < t.a.nodes.len {
		if t.a.nodes[int(sizeof_id)].kind == .sizeof_expr {
			t.set_node_value(int(sizeof_id), elem_type)
		}
	}
	t.set_node_typ(int(call_id), array_type_id)
}

fn (t &Transformer) call_is_array_push_to_name(node flat.Node, name string) bool {
	if node.children_count < 3 {
		return false
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .ident || callee.value != 'array_push' {
		return false
	}
	arg := t.a.child_node(&node, 1)
	if arg.kind != .prefix || arg.children_count == 0 {
		return false
	}
	base := t.a.child_node(arg, 0)
	return base.kind == .ident && base.value == name
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
	if base.kind != .ident || base.value.len == 0 {
		return none
	}
	return base.value
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
		if lhs.kind != .ident || lhs.value != name {
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

// rewrite_method_level_generic_call rewrites a call to a method-level generic into
// an explicit call of the specialized function: `SpecName(receiver, args...)`. It
// handles both the selector form (receiver inside the callee) and the
// ident-lowered form (receiver already an explicit child).
fn (mut t Transformer) rewrite_method_level_generic_call(id flat.NodeId, node flat.Node, decl GenericFnDecl, args_ids []u32) {
	old_params := t.active_generic_params
	t.clear_node_type_memo()
	t.active_generic_params = t.generic_fn_param_names(decl.node, decl.module)
	concrete_args_source_ids := t.canonical_generic_specialization_args(args_ids)
	concrete_args := flat.TypeTextList{ ids: concrete_args_source_ids }.names()
	spec_value := specialized_generic_fn_value(decl.node.value, concrete_args_source_ids)
	spec_name := transform_qualified_fn_name(decl.module, spec_value)
	preserved_args := concrete_args.join(', ')
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

				mut expected_type := trimmed_transform_text(flat.source_type_name(param_types_source_ids[0]))
				if expected_type.starts_with('mut ') {
					expected_type = trimmed_transform_text(expected_type[4..])
				}
				expected_type = expected_type.trim_left('&').trim_space()
				if expected_type.len > 0 {
					if embedded := t.embedded_receiver_base_for_type(recv_id, base_type_source_id, flat.source_type_id(expected_type)) {
						receiver_arg = embedded
					}
				}
			}
			children << receiver_arg
		}
		mut i := 1
		for i < node.children_count {
			arg_id := t.a.child(&node, i)
			param_type := if i < param_types_source_ids.len {
				flat.source_type_name(param_types_source_ids[i])
			} else {
				''
			}
			arg := t.a.nodes[int(arg_id)]
			if arg.kind == .field_init {
				if packed_arg := t.transform_params_struct_call_arg(node, i, flat.source_type_id(param_type)) {
					children << packed_arg
					i = t.next_non_field_init_arg(node, i)
					continue
				}
				if packed_arg := t.transform_struct_call_arg(node, i, flat.source_type_id(param_type)) {
					children << packed_arg
					i = t.next_non_field_init_arg(node, i)
					continue
				}
			}
			children << t.specialize_generic_fn_value_arg(arg_id, flat.source_type_id(param_type), false) or {
				t.retype_generic_call_literal_arg(arg_id, flat.source_type_id(param_type))
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
				flat.source_type_name(param_types_source_ids[param_idx])
			} else {
				''
			}
			arg := t.a.nodes[int(arg_id)]
			if arg.kind == .field_init {
				if packed_arg := t.transform_params_struct_call_arg(node, i, flat.source_type_id(param_type)) {
					children << packed_arg
					i = t.next_non_field_init_arg(node, i)
					continue
				}
				if packed_arg := t.transform_struct_call_arg(node, i, flat.source_type_id(param_type)) {
					children << packed_arg
					i = t.next_non_field_init_arg(node, i)
					continue
				}
			}
			children << t.specialize_generic_fn_value_arg(arg_id, flat.source_type_id(param_type), false) or {
				t.retype_generic_call_literal_arg(arg_id, flat.source_type_id(param_type))
			}
			i++
		}
	}
	t.append_missing_specialized_params_struct_args(mut children, param_types_source_ids.clone(), decl.module)
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	t.set_node(int(id), flat.Node{
		kind:           .call
		op:             node.op
		children_start: start
		children_count: flat.child_count(children.len)
		pos:            node.pos
		value:          preserved_args
		typ:            ret_typ_source_id
	})
	t.clear_resolved_call(id)
}

fn (mut t Transformer) append_missing_specialized_params_struct_args(mut children []flat.NodeId, param_types_ids []u32, decl_module string) {
	mut param_idx := children.len - 1
	for param_idx < param_types_ids.len {
		param_type_source_id := param_types_ids[param_idx]
		param_type := flat.source_type_name(param_type_source_id)
		mut struct_type := t.params_struct_type_name(param_type_source_id) or { u32(0) }
		if struct_type == 0 && !param_type.contains('.') && decl_module != ''
			&& decl_module !in ['main', 'builtin'] {
			struct_type = t.params_struct_type_name(flat.source_type_id('${decl_module}.${param_type}')) or { u32(0) }
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
		t.tc.fn_ret_types.delete(name)
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
		if callee.kind == .ident && t.generic_callee_is_specialization(callee.value) {
			decl_key := t.generic_call_decl_key(id, node, module_name, decls) or { return none }
			decl := decls[decl_key] or { return none }
			if exact_source_ids := t.exact_generic_specialization_args_from_callee(callee.value) {
				if exact_source_ids.len > 0 && !t.generic_args_have_placeholders(exact_source_ids) {
					key := t.generic_specialization_progress_key(decl, exact_source_ids)
					if key !in t.generic_fn_spec_nodes && !t.generic_fn_specs_in_progress[key]
						&& !t.pending_generic_fn_spec_keys[key] {
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
	// identity and in `node.value`. Preserve that agreement before consulting a
	// stale cache or re-inferring without the generated function's local scope.
	if node.children_count > 0 && node.value.len > 0 && !isnil(t.tc) {
		callee := t.a.child_node(&node, 0)
		if callee.kind == .ident && t.generic_callee_is_specialization(callee.value) {
			if exact_source_ids := t.exact_generic_specialization_args_from_callee(callee.value) {

				// The rewritten value already uses the callee's semantic type names.
				// Resolving it in the current scan module can rebind a caller homonym.
				preserved := split_generic_args(node.value)
				if generic_type_args_equal(flat.TypeTextList.from_names(preserved).ids, exact_source_ids) {
					decl_key := if spec := t.generic_call_spec_cache[idx] {
						spec.decl_key
					} else {
						t.generic_call_decl_key(id, node, module_name, decls) or { return none }
					}
					decl := decls[decl_key] or { return none }
					if t.generic_specialization_progress_key(decl, exact_source_ids) !in t.generic_fn_spec_nodes {
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
			if callee.kind == .ident && t.generic_callee_is_specialization(callee.value) {
				decl := decls[spec.decl_key] or { return none }
				if exact_source_ids := t.exact_generic_specialization_args_from_callee(callee.value) {
					if generic_type_args_equal(spec.args.ids, exact_source_ids) {
						ret_type_source_id := t.specialized_fn_return_type_text(decl, exact_source_ids)
						ret_type := flat.source_type_name(ret_type_source_id)
						if ret_type_source_id != 0 && !t.generic_arg_is_unresolved(ret_type_source_id)
							&& node.typ_text() != ret_type {
							t.set_node_typ(idx, ret_type_source_id)
							t.clear_typechecker_node_cache(idx)
						}
						if t.generic_specialization_progress_key(decl, exact_source_ids) !in t.generic_fn_spec_nodes {
							return spec.decl_key, exact_source_ids.clone()
						}
						return none
					}
				}
			}
		}
		// Explicit source arguments and the alias marker retained in `node.value`
		// are more precise than a later inference pass over an alias-erased ABI
		// argument. Preserve that identity for reflected `T.typ_text()` specializations.
		if node.value.len > 0 {
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
			if callee.kind == .ident && t.generic_callee_is_specialization(callee.value) {
				decl := decls[spec.decl_key] or { return none }
				if exact_source_ids := t.exact_generic_specialization_args_from_callee(callee.value) {
					if generic_type_args_equal(spec.args.ids, exact_source_ids) {
						ret_type_source_id_2 := t.specialized_fn_return_type_text(decl, exact_source_ids)
						ret_type := flat.source_type_name(ret_type_source_id_2)
						if ret_type_source_id_2 != 0 && !t.generic_arg_is_unresolved(ret_type_source_id_2)
							&& node.typ_text() != ret_type {
							t.set_node_typ(idx, ret_type_source_id_2)
							t.clear_typechecker_node_cache(idx)
						}
						if t.generic_specialization_progress_key(decl, exact_source_ids) !in t.generic_fn_spec_nodes {
							return spec.decl_key, exact_source_ids.clone()
						}
						return none
					}
				}
				if raw_source_ids := t.infer_generic_call_args_from_raw_node_types(decl, node) {
					if exact_source_ids := t.exact_generic_specialization_args_from_callee(callee.value) {
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
							if t.generic_specialization_progress_key(decl, exact_source_ids) !in t.generic_fn_spec_nodes {
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
		if callee.kind == .ident && t.generic_callee_is_specialization(callee.value) {
			decl_key := t.generic_call_decl_key(id, node, module_name, decls) or { return none }
			decl := decls[decl_key] or { return none }
			// The checker and the earlier transform register a concrete signature
			// before the monomorphization Transformer exists. A specialized callee
			// therefore does not prove that its body was emitted. Recover its exact
			// arguments (including return-type-only generics with no value argument)
			// and queue the missing body before applying the usual stale-callee checks.
			mut exact_args := []u32{}
			if t.node_file_or(idx, '').len == 0 && node.value.len > 0 {
				exact_args = t.canonical_generic_specialization_args(flat.TypeTextList.from_names(split_generic_args(node.value)).ids).clone()
			}
			if exact_args.len == 0 {
				exact_args = (t.exact_generic_specialization_args_from_callee(callee.value) or {
					t.specialized_plain_generic_call_args(node, decl, module_name) or { []u32{} }
				}).clone()
			}
			if exact_args.any(flat.source_type_name(it).starts_with('fn_')) {
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
			if node.value.len > 0 {
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

				if exact_source_ids := t.exact_generic_specialization_args_from_callee(callee.value) {
					exact_matches := generic_type_args_equal(raw_source_ids, exact_source_ids)
						|| generic_type_args_equal(resolved_raw_source_ids_3, exact_source_ids)
						|| t.generic_args_equal_ignoring_mut_storage(decl, exact_source_ids, resolved_raw_source_ids_3)
						|| t.generic_alias_erased_args_equal(exact_source_ids, resolved_raw_source_ids_3, module_name)
					if exact_matches {
						if t.generic_specialization_progress_key(decl, exact_source_ids) !in t.generic_fn_spec_nodes {
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
			} else if exact_source_ids := t.exact_generic_specialization_args_from_callee(callee.value) {

				// The checker can lower an explicit zero-argument associated call such as
				// `Type.make[T]()` to the exact `Type[T].make` callee before this pass.
				// With no value arguments there is nothing to re-infer, but the exact
				// callee still needs a body materialized.
				if t.generic_specialization_progress_key(decl, exact_source_ids) !in t.generic_fn_spec_nodes {
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
	// arguments in `node.value`. The recovery helper prefers those semantic
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
	if callee.kind != .ident || !t.generic_callee_is_specialization(callee.value) {
		return none
	}
	if exact_source_ids := t.exact_generic_specialization_args_from_callee(callee.value) {
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
		arg := flat.source_type_name(arg_source_id)

		mut clean := trimmed_transform_text(arg)
		for clean.starts_with('&') || clean.starts_with('?') || clean.starts_with('!') {
			clean = trimmed_transform_text(clean[1..])
		}
		for clean.starts_with('[]') {
			clean = trimmed_transform_text(clean[2..])
		}
		if t.generic_callee_is_specialization(clean) {
			return true
		}
		if clean.contains('_T_') && !t.type_name_is_declared(clean) {
			return true
		}
		_, nested, is_app := generic_app_parts(clean)
		if is_app && t.generic_args_name_specialized_functions(flat.TypeTextList.from_names(nested).ids) {
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
		if trimmed_transform_text(flat.source_type_name(id)) != trimmed_transform_text(flat.source_type_name(right_ids[i])) {
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
	_, args, ok := generic_app_parts(receiver)
	if !ok || args.len == 0 || t.generic_args_have_placeholders(flat.TypeTextList.from_names(args).ids) {
		return none
	}
	return t.canonical_generic_specialization_args(flat.TypeTextList.from_names(args).ids).clone()
}

fn (mut t Transformer) generic_args_equal_ignoring_mut_storage(decl GenericFnDecl, exact_ids []u32, inferred_ids []u32) bool {
	if exact_ids.len != inferred_ids.len {
		return false
	}
	params := t.generic_fn_param_names(decl.node, decl.module)
	if params.len != exact_ids.len {
		return false
	}
	mut direct_mut_params := map[string]bool{}
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
			if is_generic_fn_placeholder_name(flat.source_type_name(generic_name)) {
				direct_mut_params[flat.source_type_name(generic_name)] = true
			}
		}
	}
	for i, exact_arg_source_id in exact_ids {
		exact_arg := flat.source_type_name(exact_arg_source_id)

		inferred_arg := trimmed_transform_text(flat.source_type_name(inferred_ids[i]))
		clean_exact := trimmed_transform_text(exact_arg)
		if inferred_arg == clean_exact {
			continue
		}
		if !direct_mut_params[params[i]] || inferred_arg != '&${clean_exact}' {
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
	for i in 0 .. decl.node.children_count {
		param := t.a.child_node(&decl.node, i)
		if param.kind != .param {
			if t.prefix_param_scan {
				break
			}
			continue
		}
		arg_id := t.generic_call_arg_id_for_param(node, param_idx, is_receiver) or {
			param_idx++
			continue
		}
		arg := t.a.nodes[int(arg_id)]
		inference_param_type := generic_inference_param_type(param)
		mut raw_arg_type := t.fn_value_type_name(arg_id) or { arg.typ }
		if !is_generic_fn_placeholder_name(flat.source_type_name(inference_param_type)) {
			raw_arg_type = t.generic_composite_inference_alias_type(raw_arg_type, decl.module)
		}
		if (param.is_mut || param.op == .amp || flat.source_type_shape(param.typ).has(.mutable_type))
			&& flat.source_type_shape(raw_arg_type).has(.pointer) {
			// A rewritten `mut value` call stores the C ABI address on either the
			// argument prefix or its cloned value identifier. Infer from the
			// language-level value by removing exactly that storage pointer; a
			// pointer-valued `T` still retains its own leading `&`.
			raw_arg_type = flat.source_type_id(flat.source_type_name(raw_arg_type)[1..])
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
	if callee.kind != .ident || !callee.value.contains('_T_') {
		return none
	}
	decl_key := t.specialized_plain_generic_call_decl_key(callee.value, module_name, decls) or {
		return none
	}
	decl := decls[decl_key] or { return none }
	if node.value.len > 0 {
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
	if node.value.len == 0 || node.children_count == 0 {
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
	if callee.kind == .ident && !callee.value.contains('.') {
		if (t.raw_var_type(callee.value)) != 0 {
			return none
		}
	}
	// Non-generic helpers cannot be specialized. Resolve a candidate before looking
	// up lexical bindings: synthesized callees have no source-parent index entry,
	// so an unnecessary lookup can scan the entire AST for every cloned helper call.
	key := t.unshadowed_generic_call_decl_key(id, node, callee, module_name, decls) or {
		return none
	}
	if callee.kind == .ident && !callee.value.contains('.') {
		// Collection, cloning and body pre-passes run before locals enter var_types.
		// During declaration-ordered specialization lowering, the live scope resolves
		// shadows without repeatedly scanning the whole AST for synthesized ancestors.
		// Recorded specialization names already identify generated function callees.
		if (!t.validating_generic_spec || !t.fn_body_locals_in_scope || t.in_monomorphize_scan
			|| t.cloning_generic_fn_depth > 0)
			&& !t.generic_callee_is_specialization(callee.value) {
			if _ := t.local_binding_before(callee.value, callee_id) {
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
		direct_key := generic_fn_decl_base_value(callee.value)
		if decl := decls[direct_key] {
			if t.generic_call_arg_count_matches_decl(node, decl) {
				return direct_key
			}
		}
		// Receiver calls lowered from a checker-selected concrete declaration use
		// an identifier callee. Keep that exact declaration instead of rematching
		// the call against an indexed generic receiver pattern.
		if t.plain_concrete_fn_known(callee.value, module_name, decls) {
			return none
		}
		// Synthesized method calls use an identifier callee and can encode only the
		// concrete generic arguments, not a structured receiver pattern such as
		// `Box[[]T]`. Match those declarations against the call's receiver value.
		if callee.value.contains('.') {
			method := callee.value.all_after_last('.')
			for key in t.generic_receiver_methods_by_name[method] {
				decl := decls[key] or { continue }
				if t.resolved_generic_decl_matches_callee_receiver(callee, node, decl,
					module_name) && t.generic_call_arg_count_matches_decl(node, decl) {
					return key
				}
			}
		}
		if key := t.generic_flat_receiver_call_decl_key(callee.value, module_name, decls) {
			if decl := decls[key] {
				if t.generic_call_arg_count_matches_decl(node, decl) {
					return key
				}
			}
		}
		if key := t.specialized_plain_generic_call_decl_key(callee.value, module_name, decls) {
			if decl := decls[key] {
				if t.generic_call_arg_count_matches_decl(node, decl) {
					return key
				}
			}
		}
		if t.local_concrete_fn_shadows_generic(callee.value, module_name, decls)
			|| t.plain_concrete_fn_known(callee.value, module_name, decls) {
			return none
		}
		for candidate in t.generic_plain_call_candidates(callee.value, module_name) {
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
		if static_key := t.generic_static_assoc_call_decl_key(base_id, callee.value, decls) {
			if decl := decls[static_key] {
				if t.generic_call_arg_count_matches_decl(node, decl) {
					return static_key
				}
			}
		}
		base := t.a.nodes[int(base_id)]
		if base.kind == .ident && t.ident_is_import_alias(base.value) {
			mod_name := t.import_alias_module(base.value)
			for candidate in ['${mod_name}.${callee.value}', '${base.value}.${callee.value}'] {
				key := generic_fn_decl_base_value(candidate)
				if decl := decls[key] {
					if !t.generic_call_arg_count_matches_decl(node, decl) {
						continue
					}
					return key
				}
			}
		}
		method_keys := t.generic_receiver_methods_by_name[callee.value] or { return none }
		mut base_type := flat.source_type_name(t.syntactic_static_call_return_type(base_id))
		if base_type.len == 0 {
			base_type = flat.source_type_name(t.node_type(base_id))
		}
		// Calls in a generic match branch must bind to the concrete variant, even
		// when the sum type declares a same-named generic method.
		if sc := t.find_smartcast(t.expr_key(base_id)) {
			target_type_source_id := t.smartcast_target_type(sc)
			target_type := flat.source_type_name(target_type_source_id)
			if target_type_source_id != 0 {
				base_type = target_type
			}
		}
		base_type = flat.source_type_name(transform_unshared_receiver_type(flat.source_type_id(base_type)))
		if base_type.starts_with('&') {
			base_type = base_type[1..]
		}
		mut receiver_types := [base_type]
		if base.kind == .ident {
			if declared_type_source_id := t.local_decl_type_before_pos(base.value, node) {
				declared_type := flat.source_type_name(declared_type_source_id)

				if declared_type !in receiver_types {
					receiver_types << declared_type
				}
			}
			if declared_type_source_id := t.local_decl_type_before(base.value, base_id) {
				declared_type := flat.source_type_name(declared_type_source_id)

				if declared_type !in receiver_types {
					receiver_types << declared_type
				}
			}
		}
		base_name, _, ok := generic_app_parts(base_type)
		if ok {
			key := t.generic_receiver_decl_key(base_name, callee.value, decls)
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
					if t.generic_receiver_decl_matches_type(flat.source_type_id(receiver_type), decl, module_name) {
						receiver_matches = true
						break
					}
					mut seen := map[u32]bool{}
					if t.generic_decl_matches_embedded_receiver(flat.source_type_id(receiver_type), decl, module_name, mut seen) {
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

fn (t &Transformer) generic_receiver_decl_matches_type(base_type_id u32, decl GenericFnDecl, module_name string) bool {
	decl_receiver := generic_fn_decl_base_value(decl.node.value).all_before_last('.')
	if decl_receiver.len == 0 {
		return false
	}
	mut clean_base := flat.source_type_name(transform_unshared_receiver_type(base_type_id))
	if clean_base.starts_with('&') {
		clean_base = clean_base[1..]
	}
	if clean_base.len == 0 {
		return false
	}
	base, _, ok := generic_app_parts(clean_base)
	if ok {
		clean_base = base
	}
	call_qualified := flat.source_type_name(t.normalize_type_in_module(flat.source_type_id(clean_base), module_name))
	decl_qualified := flat.source_type_name(t.normalize_type_in_module(flat.source_type_id(decl_receiver), decl.module))
	call_identity, _, call_is_generic := generic_app_parts(call_qualified)
	decl_identity, _, decl_is_generic := generic_app_parts(decl_qualified)
	qualified_call_base := if call_is_generic { call_identity } else { call_qualified }
	qualified_decl_base := if decl_is_generic { decl_identity } else { decl_qualified }
	if qualified_call_base.contains('.') && qualified_decl_base.contains('.')
		&& qualified_call_base != qualified_decl_base {
		// Receiver short names are not unique across modules. Once both sides can
		// be resolved, do not let `json2.Decoder` match `decoder2.Decoder` merely
		// because both declarations are named `Decoder`.
		return false
	}
	mut call_receivers := []string{}
	for candidate in [clean_base,
		flat.source_type_name(t.normalize_type_alias(flat.source_type_id(clean_base))),
		flat.source_type_name(t.normalize_type_in_module(flat.source_type_id(clean_base), module_name))] {
		mut roots := [candidate]
		generic_base, _, is_generic := generic_app_parts(candidate)
		if is_generic && generic_base.len > 0 {
			roots << generic_base
		}
		for root in roots {
			if root.len > 0 && root !in call_receivers {
				call_receivers << root
			}
			if root.contains('.') {
				short := root.all_after_last('.')
				if short.len > 0 && short !in call_receivers {
					call_receivers << short
				}
			}
		}
	}
	mut decl_receivers := []string{}
	for candidate in [decl_receiver,
		flat.source_type_name(t.normalize_type_alias(flat.source_type_id(decl_receiver))),
		flat.source_type_name(t.normalize_type_in_module(flat.source_type_id(decl_receiver), decl.module))] {
		mut roots := [candidate]
		generic_base, _, is_generic := generic_app_parts(candidate)
		if is_generic && generic_base.len > 0 {
			roots << generic_base
		}
		for root in roots {
			if root.len > 0 && root !in decl_receivers {
				decl_receivers << root
			}
			if root.contains('.') {
				short := root.all_after_last('.')
				if short.len > 0 && short !in decl_receivers {
					decl_receivers << short
				}
			} else if decl.module.len > 0 && decl.module !in ['main', 'builtin'] {
				qualified := '${decl.module}.${root}'
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
	decl_value := generic_fn_decl_base_value(decl.node.value)
	_, method := flat.decode_static_type_method_name(decl_value) or { return false }
	callee_id := t.a.child(&node, 0)
	callee := t.a.nodes[int(callee_id)]
	if callee.kind != .selector || callee.children_count == 0 {
		return false
	}
	if callee.value !in generic_call_name_spellings(method) {
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
		if base.value == 'C' || t.ident_is_import_alias(base.value) {
			return candidates
		}
		candidates << flat.source_type_id(base.value)
	} else if base.kind == .selector && base.children_count > 0 {
		inner := t.a.child_node(&base, 0)
		if inner.kind == .ident {
			if t.ident_is_import_alias(inner.value) {
				mod_name := t.import_alias_module(inner.value)
				candidates << flat.source_type_id('${mod_name}.${base.value}')
			}
			candidates << flat.source_type_id('${inner.value}.${base.value}')
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
	if t.generic_struct_operator_call_seen(name)
		&& t.generic_flat_receiver_operator_decl_exists(receiver, method, module_name, decls) {
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

fn (t &Transformer) generic_flat_receiver_operator_decl_exists(receiver string, method string, module_name string, decls map[string]GenericFnDecl) bool {
	op_method := generic_operator_symbol_for_c_method(method) or { return false }
	method_keys := t.generic_receiver_methods_by_name[op_method] or { return false }
	for key in method_keys {
		decl := decls[key] or { continue }
		decl_receiver := generic_fn_decl_base_value(decl.node.value).all_before_last('.')
		if decl_receiver.len == 0 {
			continue
		}
		if generic_flat_receiver_matches(receiver, decl_receiver, module_name, decl.module) {
			return true
		}
	}
	return false
}

fn generic_operator_symbol_for_c_method(method string) ?string {
	return match method {
		'plus' { '+' }
		'minus' { '-' }
		'mul' { '*' }
		'div' { '/' }
		'mod' { '%' }
		'eq' { '==' }
		'ne' { '!=' }
		'lt' { '<' }
		'gt' { '>' }
		'le' { '<=' }
		'ge' { '>=' }
		else { none }
	}
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
	if recorded_source_ids := t.recorded_generic_specialization_args(callee.value) {
		if recorded_source_ids.len == params.len {
			// The specialized callee name is already bound to these canonical concrete
			// arguments. Re-resolving a bare main type in the caller's module can turn
			// `fill_T_Item` into `fill_T_other__Item` during the refresh scan.
			return recorded_source_ids.clone()
		}
	}
	if !callee.value.contains('_T_') {
		return none
	}
	suffix := callee.value.all_after('_T_')
	if suffix.len == 0 {
		return none
	}
	if params.len != 1 {
		return none
	}
	arg := generic_type_arg_from_suffix_with_containers(suffix)
	if arg.len == 0 {
		return none
	}
	return [t.generic_arg_for_call_and_decl_module(flat.source_type_id(arg), module_name, decl.module)]
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
	mut bases := []string{}
	mut receiver_roots := [decl_receiver]
	decl_base, _, decl_is_generic := generic_app_parts(decl_receiver)
	if decl_is_generic && decl_base.len > 0 {
		receiver_roots << decl_base
	}
	for root in receiver_roots {
		if root.len == 0 || root in bases {
			continue
		}
		bases << root
		if decl_module != '' && decl_module != 'main' && decl_module != 'builtin'
			&& !root.contains('.') {
			bases << '${decl_module}.${root}'
		}
		if module_name != '' && module_name != 'main' && module_name != 'builtin'
			&& !root.contains('.') {
			bases << '${module_name}.${root}'
		}
		if root.contains('.') {
			short_receiver := root.all_after_last('.')
			bases << short_receiver
			bases << '${root.all_before_last('.')}.${short_receiver}'
		}
	}
	for base in bases {
		if base.len == 0 {
			continue
		}
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
		&& (t.local_concrete_fn_shadows_generic(callee.value, module_name, decls)
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
		&& !t.local_concrete_fn_shadows_generic(callee.value, module_name, decls) {
		for candidate in t.generic_plain_call_candidates(callee.value, module_name) {
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
		if declared_type_source_id := t.local_decl_type_before_pos(base.value, node) {
			if t.generic_receiver_decl_matches_type(declared_type_source_id, decl, module_name) {
				return true
			}
		}
		if declared_type_source_id := t.local_decl_type_before(base.value, base_id) {
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
		lookup_type = flat.source_type_id(trimmed_transform_text(flat.source_type_name(lookup_type)[4..]))
	}
	if lookup_type in seen {
		return false
	}
	seen[lookup_type] = true
	if lookup_type !in t.embedded_fields && flat.source_type_shape(lookup_type).has(.dotted) {
		short_type := flat.source_type_id(flat.source_type_name(lookup_type).all_after_last('.'))
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
		owner = base.value
	} else if base.kind == .selector && base.children_count > 0 {
		inner := t.a.child_node(base, 0)
		if inner.kind == .ident {
			owner = '${inner.value}.${base.value}'
		}
	}
	if owner == '' {
		return u32(0)
	}
	name := '${owner}.${callee.value}'
	for candidate in [name, transform_qualified_fn_name(t.cur_module, name)] {
		if ret_source_id := t.fn_ret_types.type_id(candidate) {
			return ret_source_id
		}
		if !isnil(t.tc) {
			if typ := t.tc.fn_ret_types[candidate] {
				ret_name := typ.name()
				if ret_name.len > 0 && ret_name != 'unknown' && ret_name != 'void' {
					return flat.source_type_id(ret_name)
				}
			}
		}
	}
	if callee.value == 'new' {
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
		return (t.var_type(base.value)) == 0 && !t.ident_is_import_alias(base.value)
	}
	if base.kind == .selector && base.children_count > 0 {
		inner := t.a.child_node(&base, 0)
		if inner.kind != .ident || (t.var_type(inner.value)) != 0 {
			return false
		}
		return t.ident_is_import_alias(inner.value) || v3_type_name_starts_upper(inner.value)
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
	param_type := flat.source_type_name(param_type_id)
	if param_type_id == u32(0) {
		return false
	}
	if _ := t.params_struct_type_name(param_type_id) {
		return true
	}
	if !param_type.contains('.') && decl_module != '' && decl_module != 'main'
		&& decl_module != 'builtin' {
		if _ := t.params_struct_type_name(flat.source_type_id('${decl_module}.${param_type}')) {
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
	if node.value.len > 0 {
		return normalize_generic_args(flat.TypeTextList.from_names(split_generic_args(node.value)).ids, module_name).clone()
	}
	if node.children_count == 0 {
		return none
	}
	fn_node := t.a.child_node(&node, 0)
	if fn_node.kind != .index || fn_node.children_count < 2 || fn_node.value == 'range' {
		return none
	}
	type_arg := t.generic_call_type_args_name(fn_node)
	if type_arg.len == 0 {
		return none
	}
	return normalize_generic_args(flat.TypeTextList.from_names(split_generic_args(type_arg)).ids, module_name).clone()
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
	if t.expected_expr_node == int(_id) && t.expected_expr_type != 0 {
		t.infer_generic_return_type_args(decl, t.generic_inference_expected_type(t.expected_expr_type), mut inferred, receiver_params)
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
				t.infer_generic_variadic_call_arg(child.typ, elem_param_type, arg_id, arg_ids.len == 1, mut inferred)
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
		if !is_generic_fn_placeholder_name(flat.source_type_name(inference_param_type)) {
			inferred_arg_type = t.generic_composite_inference_alias_type(inferred_arg_type,
				call_module)
		}
		if child.is_mut && child.op == .amp && flat.source_type_shape(inferred_arg_type).has(.pointer) {
			// The explicit `mut p &T` marker addresses the caller's pointer slot;
			// it is ABI indirection, not part of T's inferred source type.
			inferred_arg_type = flat.source_type_id(flat.source_type_name(inferred_arg_type)[1..])
		}
		if is_recv_param && t.call_is_selector_form(node)
			&& t.generic_receiver_needs_decl_type_fallback(child.typ, inferred_arg_type) {
			recv := t.a.nodes[int(arg_id)]
			if recv.kind == .ident {
				lookup_before := if node.pos.offset > 0 { node } else { recv }
				if decl_type_source_id := t.local_decl_type_before_pos(recv.value, lookup_before) {
					inferred_arg_type = t.generic_receiver_decl_inference_type(child.typ, decl_type_source_id, call_module)
				} else if decl_type_source_id := t.local_decl_type_before(recv.value, _id) {
					inferred_arg_type = t.generic_receiver_decl_inference_type(child.typ, decl_type_source_id, call_module)
				}
			}
		}
		mut defer_numeric_literal := false
		arg_kind := t.a.nodes[int(arg_id)].kind
		if arg_kind in [.int_literal, .float_literal]
			&& is_generic_fn_placeholder_name(flat.source_type_name(inference_param_type)) {
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
		if (child.is_mut || child.op == .amp || flat.source_type_shape(child.typ).has(.mutable_type))
			&& flat.source_type_shape(inferred_arg_type).has(.pointer) {
			// A mutable parameter's leading pointer is its storage ABI, whether the
			// generic placeholder is direct (`T`) or nested (`Container[T]`). A `mut`
			// argument that is a field/selector (`mut g.cells`) arrives here still
			// carrying that `&`, unlike a plain `mut ident` whose value node already
			// dropped it — strip it so `[]T` matches `[]Cell`, not `&[]Cell`. Mirrors
			// infer_generic_call_args_from_params.
			inferred_arg_type = flat.source_type_id(flat.source_type_name(inferred_arg_type)[1..])
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
		arg := flat.source_type_name(arg_source_id)
		if arg_source_id != 0 && arg != name && !t.generic_arg_is_unresolved(arg_source_id) {
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
		receiver := owner_name_view(node.value)
		base, args, _ := generic_app_parts(receiver)
		mut candidates := [base]
		if !base.contains('.') && module_name !in ['', 'main', 'builtin'] {
			candidates = ['${module_name}.${base}', base]
		}
		for candidate in candidates {
			params := t.tc.struct_generic_params[candidate] or { continue }
			for arg in args {
				if arg in params && arg !in receiver_params {
					receiver_params << arg
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
		t.collect_generic_param_names_from_type(receiver, decl.module, mut receiver_params)
	}
	for i in 0 .. decl.node.children_count {
		child := t.a.child_node(&decl.node, i)
		if child.kind == .param {
			t.collect_generic_param_names_from_type(child.typ_text(), decl.module, mut receiver_params)
			break
		}
	}
	return receiver_params
}

fn (mut t Transformer) infer_generic_struct_init_args(param_type_id u32, arg_id flat.NodeId, mut inferred map[string]u32) {
	param_type := flat.source_type_name(param_type_id)
	if int(arg_id) < 0 || int(arg_id) >= t.a.nodes.len {
		return
	}
	arg := t.a.nodes[int(arg_id)]
	if arg.kind != .struct_init {
		return
	}
	param_base, param_args, is_generic_struct := generic_app_parts(trimmed_transform_text(param_type))
	if !is_generic_struct || param_args.len == 0 {
		return
	}
	info := t.lookup_struct_info(flat.source_type_id(param_base)) or { return }
	for i in 0 .. arg.children_count {
		field := t.a.child_node(&arg, i)
		if field.kind != .field_init || field.children_count == 0 {
			continue
		}
		field_name := if field.value.len > 0 {
			field.value
		} else if i < info.fields.len {
			info.fields[i].name
		} else {
			continue
		}
		mut field_type := ''
		for struct_field in info.fields {
			if struct_field.name == field_name {
				field_type = struct_field.typ_text()
				break
			}
		}
		if field_type.len == 0 {
			continue
		}
		value_id := t.a.child(field, 0)
		value_type_source_id_2 := t.generic_call_arg_type_for_inference(value_id)
		if value_type_source_id_2 != 0 {
			infer_generic_type_args(flat.source_type_id(field_type), value_type_source_id_2, mut inferred)
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
	param_type := flat.source_type_name(param_type_id)
	if int(arg_id) < 0 || int(arg_id) >= t.a.nodes.len {
		return
	}
	if t.a.nodes[int(arg_id)].kind != .field_init {
		return
	}
	mut struct_param_type := t.normalize_type_alias_chain(flat.source_type_id(trimmed_transform_text(param_type)))
	if flat.source_type_shape(struct_param_type).has(.pointer) {
		if flat.source_type_name(struct_param_type)[1..].starts_with('&') {
			return
		}
		struct_param_type = flat.source_type_id(flat.source_type_name(struct_param_type)[1..])
	}
	param_base, param_args, is_generic_struct := generic_app_parts(flat.source_type_name(struct_param_type))
	if !is_generic_struct || param_args.len == 0 {
		return
	}
	info := t.lookup_struct_info(flat.source_type_id(param_base)) or { return }
	// The struct declares its fields in its own generic parameter names
	// (`struct Params[A] { a []A }`) while the call must infer the function's
	// parameters (`fn f[T](p Params[T])`). Rebase the field types onto the
	// applied args so `[]A` becomes `[]T` before inference.
	struct_params := t.struct_generic_params_for_base(param_base)
	mut field_idx := 0
	for i in 0 .. int(node.children_count) {
		field := t.a.child_node(&node, i)
		if field.kind != .field_init {
			continue
		}
		field_name := if field.value.len > 0 {
			field.value
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
		if struct_params.len > 0 && struct_params != param_args {
			field_type = substitute_generic_type_text_with_params(field_type, flat.TypeTextList.from_names(param_args).ids, struct_params)
		}
		value_id := t.a.child(field, 0)
		value_type_source_id_3 := t.generic_call_arg_type_for_inference(value_id)
		if value_type_source_id_3 != 0 {
			infer_generic_type_args(field_type, value_type_source_id_3, mut inferred)
		}
	}
}

fn (t &Transformer) struct_generic_params_for_base(base string) []string {
	if isnil(t.tc) {
		return []string{}
	}
	if params := t.tc.struct_generic_params[base] {
		return params
	}
	short := base.all_after_last('.')
	if params := t.tc.struct_generic_params[short] {
		return params
	}
	return []string{}
}

fn (t &Transformer) infer_generic_sum_variant_args(param_type_id u32, arg_type_id u32, mut inferred map[string]u32) {
	param_type := flat.source_type_name(param_type_id)
	arg_type := flat.source_type_name(arg_type_id)
	param := trimmed_transform_text(param_type)
	arg := trimmed_transform_text(arg_type)
	if param.len == 0 || arg.len == 0 {
		return
	}
	param_base, param_args, is_generic_sum := generic_app_parts(param)
	if !is_generic_sum || param_args.len == 0 {
		return
	}
	variants := t.concrete_sum_variants_for_candidate(flat.source_type_id(param))
	if variants.len == 0 {
		return
	}
	mut matched_concrete_variant := false
	for variant in variants {
		if is_generic_fn_placeholder_name(trimmed_transform_text(flat.source_type_name(variant))) {
			continue
		}
		before_len := inferred.len
		infer_generic_type_args(variant, flat.source_type_id(arg), mut inferred)
		if inferred.len == before_len {
			t.infer_generic_receiver_suffix_args(variant, flat.source_type_id(arg), mut inferred)
		}
		if inferred.len > before_len {
			return
		}
		if t.variant_names_match(variant, flat.source_type_id(arg)) {
			matched_concrete_variant = true
		}
	}
	arg_is_sum_base := arg == param_base
		|| arg.all_after_last('.') == param_base.all_after_last('.')
		|| t.resolve_sum_name(flat.source_type_id(arg)) == t.resolve_sum_name(flat.source_type_id(param_base))
	for variant in variants {
		if !is_generic_fn_placeholder_name(trimmed_transform_text(flat.source_type_name(variant))) || arg_is_sum_base {
			continue
		}
		before_len := inferred.len
		infer_generic_type_args(variant, flat.source_type_id(arg), mut inferred)
		if inferred.len == before_len {
			t.infer_generic_receiver_suffix_args(variant, flat.source_type_id(arg), mut inferred)
		}
		if inferred.len > before_len {
			return
		}
	}
	if !matched_concrete_variant {
		return
	}
	for param_arg in param_args {
		if is_generic_fn_placeholder_name(param_arg) && param_arg !in inferred {
			inferred[param_arg] = flat.source_type_id(arg)
		}
	}
}

fn (mut t Transformer) infer_generic_sum_literal_args(param_type_id u32, arg_id flat.NodeId, mut inferred map[string]u32) {
	param_type := flat.source_type_name(param_type_id)
	param := trimmed_transform_text(param_type)
	if int(arg_id) < 0 || int(arg_id) >= t.a.nodes.len {
		return
	}
	arg := t.a.nodes[int(arg_id)]
	if arg.kind != .struct_init {
		return
	}
	_, param_args, is_generic_sum := generic_app_parts(param)
	if !is_generic_sum || param_args.len == 0 {
		return
	}
	variants := t.concrete_sum_variants_for_candidate(flat.source_type_id(param))
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
			if t.sum_field_name(variant) == field.value {
				t.infer_generic_sum_variant_args(flat.source_type_id(param), value_type_source_id_4, mut inferred)
				return
			}
		}
	}
}

fn (t &Transformer) generic_arg_for_call_and_decl_module(arg_id u32, call_module string, decl_module string) u32 {
	arg_comparison_type_id_10 := arg_id
	arg_source_id_8 := arg_comparison_type_id_10
	arg := flat.source_type_name(arg_source_id_8)
	if call_module in ['', 'main'] && !arg.contains('.') {
		if info := t.structs[trimmed_transform_text(arg)] {
			if info.module in ['', 'main'] {
				return t.lock_colliding_main_generic_type_text(flat.source_type_id(arg), decl_module)
			}
		}
	}
	if t.substituted_type_belongs_to_main_generic(arg_source_id_8)
		&& (call_module in ['', 'main'] || t.current_specialization_has_generic_arg(arg_source_id_8)) {
		return arg_id
	}
	if t.current_specialization_has_generic_arg(arg_source_id_8) {
		base, args, is_generic := generic_provenance_app_parts(arg_id)
		if is_generic && !arg.starts_with('map[') {
			mut scoped_args := []string{cap: args.len}
			for nested_arg in args {
				scoped_args << flat.source_type_name(t.generic_arg_for_call_and_decl_module(nested_arg, call_module, decl_module))
			}
			scoped_base := flat.source_type_name(t.generic_arg_for_call_and_decl_module(base, call_module, decl_module))
			return flat.source_type_id('${scoped_base}[${scoped_args.join(', ')}]')
		}
	}
	// A bare program (main) type alias that collides by short name with a type in the
	// callee module (e.g. a user `RawHtml` alias against `veb.RawHtml`) must be kept as the
	// caller's own type: rebasing it into the callee module would merge the two into one
	// specialization, so `veb.filter_html` could no longer tell user data from trusted
	// `veb.RawHtml` and would skip escaping it. Only same-named collisions are preserved.
	if call_module in ['', 'main'] && !arg.contains('.') && !isnil(t.tc)
		&& (t.tc.type_aliases.contains(arg) || arg in t.tc.structs || t.tc.sum_types.contains(arg)
			|| arg in t.tc.enum_names || arg in t.tc.interface_names) {
		qname := '${decl_module}.${arg}'
		if t.tc.type_aliases.contains(qname) || qname in t.tc.structs || t.tc.sum_types.contains(qname)
			|| qname in t.tc.enum_names || qname in t.tc.interface_names {
			return arg_id
		}
	}
	if t.generic_type_text_contains_alias(arg_source_id_8, call_module) {
		if t.generic_type_text_contains_fixed_array_alias(arg_source_id_8, call_module) {
			call_normalized_source_id := t.normalize_type_in_module(arg_id, call_module)
			call_normalized_comparison_type_id := call_normalized_source_id

			if call_normalized_comparison_type_id != arg_comparison_type_id_10 {
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
	clean := flat.source_type_name(clean_id)
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
	receiver := flat.source_type_name(receiver_source_id)
	if receiver_source_id == 0 {
		return false
	}
	_, parsed_args, is_generic := generic_app_parts(receiver)
	if is_generic {
		for current_arg in parsed_args {
			if trimmed_transform_text(current_arg) == clean {
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
	return flat.TypeTextList.from_names(generic_type_container_components_bytes(flat.source_type_name(type_id))).ids
}

fn generic_type_container_components_bytes(typ string) []string {
	clean := trimmed_transform_text(typ)
	for prefix in ['mut ', 'shared ', 'atomic ', '...', '[]', '?', '!', '&', 'chan ', 'thread '] {
		if clean.starts_with(prefix) {
			return [trimmed_transform_text(clean[prefix.len..])]
		}
	}
	if clean.starts_with('map[') {
		end := generic_matching_bracket(clean, 3)
		if end > 3 && end + 1 < clean.len {
			return [trimmed_transform_text(clean[4..end]), trimmed_transform_text(clean[end + 1..])]
		}
	} else if clean.starts_with('[') {
		end := generic_matching_bracket(clean, 0)
		if end > 0 && end + 1 < clean.len {
			return [trimmed_transform_text(clean[end + 1..])]
		}
	} else if clean.starts_with('(') && clean.ends_with(')') && clean.contains(',') {
		return split_generic_args(clean[1..clean.len - 1])
	} else if clean.starts_with('fn(') || clean.starts_with('fn (') {
		if params, ret := fn_type_text_parts(clean) {
			mut components := params.clone()
			components << ret
			return components
		}
	}
	return []string{}
}

fn generic_provenance_app_parts(type_id u32) (u32, []u32, bool) {
	base, args, ok := generic_provenance_app_parts_bytes(flat.source_type_name(type_id))
	return flat.source_type_id(base), flat.TypeTextList.from_names(args).ids, ok
}

fn generic_provenance_app_parts_bytes(typ string) (string, []string, bool) {
	// A bracket inside an option, map or function belongs to that container;
	// it does not make the entire spelling a named generic application.
	if generic_type_container_components_bytes(typ).len > 0 {
		return '', []string{}, false
	}
	return generic_app_parts(typ)
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
	typ := flat.source_type_name(typ_id)
	mut clean := trimmed_transform_text(typ)
	if clean.len == 0 {
		return
	}
	if clean.starts_with('main.') && !t.ident_is_import_alias('main') {
		clean = clean['main.'.len..]
	}
	for prefix in ['mut ', 'shared ', 'atomic ', '...', '[]', '?', '!', '&', 'chan ', 'thread '] {
		if clean.starts_with(prefix) {
			t.collect_caller_main_type_names(flat.source_type_id(clean[prefix.len..]), mut types_in_scope)
			return
		}
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		if params, ret := fn_type_text_parts(clean) {
			for param in params {
				t.collect_caller_main_type_names(flat.source_type_id(param), mut types_in_scope)
			}
			t.collect_caller_main_type_names(flat.source_type_id(ret), mut types_in_scope)
		}
		return
	}
	if clean.starts_with('map[') {
		end := generic_matching_bracket(clean, 3)
		if end > 3 && end + 1 < clean.len {
			t.collect_caller_main_type_names(flat.source_type_id(clean[4..end]), mut types_in_scope)
			t.collect_caller_main_type_names(flat.source_type_id(clean[end + 1..]), mut types_in_scope)
		}
		return
	}
	if clean.starts_with('[') {
		end := generic_matching_bracket(clean, 0)
		if end > 0 && end + 1 < clean.len {
			t.collect_caller_main_type_names(flat.source_type_id(clean[end + 1..]), mut types_in_scope)
		}
		return
	}
	if clean.starts_with('(') && clean.ends_with(')') && clean.contains(',') {
		for part in split_generic_args(clean[1..clean.len - 1]) {
			t.collect_caller_main_type_names(flat.source_type_id(part), mut types_in_scope)
		}
		return
	}
	base, generic_args, is_generic := generic_app_parts(clean)
	if is_generic {
		for generic_arg in generic_args {
			t.collect_caller_main_type_names(flat.source_type_id(generic_arg), mut types_in_scope)
		}
		clean = base
	}
	if !clean.contains('.') && t.type_name_is_declared(clean) {
		types_in_scope[flat.source_type_id(clean)] = true
	}
}

fn (t &Transformer) collect_specialization_main_types(typ_id u32, mut types_in_scope map[u32]bool, mut seen map[u32]bool) {
	typ := flat.source_type_name(typ_id)
	mut clean := trimmed_transform_text(typ)
	if clean.len == 0 {
		return
	}
	clean_id := flat.source_type_id(clean)
	if seen[clean_id] {
		return
	}
	seen[clean_id] = true
	// `main.` is a temporary disambiguation lock on a caller-owned type. Strip it
	// while building the provenance closure so nested substitutions can recognize
	// the same type by its normal bare spelling in an imported generic body.
	if clean.starts_with('main.') && !t.ident_is_import_alias('main') {
		clean = clean['main.'.len..]
	}
	components := generic_type_container_components(flat.source_type_id(clean))
	if components.len > 0 {
		for component in components {
			t.collect_specialization_main_types(component, mut types_in_scope, mut seen)
		}
		return
	}
	base, generic_args, is_generic := generic_app_parts(clean)
	if is_generic {
		for generic_arg in generic_args {
			t.collect_specialization_main_types(flat.source_type_id(generic_arg), mut types_in_scope, mut seen)
		}
	}
	struct_name := if is_generic { base } else { clean }
	if struct_name.contains('.') {
		return
	}
	if !isnil(t.tc) {
		if alias_target_source_id := t.tc.type_aliases.type_id(struct_name) {
			types_in_scope[flat.source_type_id(struct_name)] = true
			t.collect_specialization_main_types(alias_target_source_id, mut types_in_scope, mut seen)
			return
		}
	}
	if variants_source_ids := t.sum_types.type_ids(struct_name) {
		types_in_scope[flat.source_type_id(struct_name)] = true
		for variant_source_id in variants_source_ids {
			variant_source_id_2 := variant_source_id

			t.collect_specialization_main_types(variant_source_id_2, mut types_in_scope, mut seen)
		}
		return
	}
	if struct_name in t.enum_types {
		types_in_scope[flat.source_type_id(struct_name)] = true
		return
	}
	info := t.structs[struct_name] or { return }
	if info.module !in ['', 'main'] {
		return
	}
	types_in_scope[flat.source_type_id(struct_name)] = true
	params := if is_generic {
		t.generic_struct_param_names_for_base(struct_name)
	} else {
		[]string{}
	}
	for field in info.fields {
		field_type := (if params.len == generic_args.len && params.len > 0 {
			substitute_generic_type_text_with_params(field.typ, flat.TypeTextList.from_names(generic_args).ids, params)
		} else {
			field.typ
		})
		t.collect_specialization_main_types(field_type, mut types_in_scope, mut seen)
	}
}

fn (t &Transformer) generic_arg_for_decl_module(arg_id u32, module_name string) u32 {
	arg := flat.source_type_name(arg_id)
	if t.substituted_type_belongs_to_main_generic(arg_id)
		&& t.current_specialization_has_generic_arg(arg_id) {
		return arg_id
	}
	if t.current_specialization_has_generic_arg(arg_id) {
		base, args, is_generic := generic_provenance_app_parts(arg_id)
		if is_generic && !arg.starts_with('map[') {
			mut scoped_args := []string{cap: args.len}
			for nested_arg in args {
				scoped_args << flat.source_type_name(t.generic_arg_for_decl_module(nested_arg, module_name))
			}
			scoped_base := flat.source_type_name(t.generic_arg_for_decl_module(base, module_name))
			return flat.source_type_id('${scoped_base}[${scoped_args.join(', ')}]')
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

fn strip_decl_module_from_generic_arg(arg string, module_name string) string {
	clean := trimmed_transform_text(arg)
	if clean.len == 0 || module_name == '' {
		return clean
	}
	if clean.starts_with('&') {
		return '&' + strip_decl_module_from_generic_arg(clean[1..], module_name)
	}
	if clean.starts_with('mut ') {
		return 'mut ' + strip_decl_module_from_generic_arg(clean[4..], module_name)
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		return clean[..1] + strip_decl_module_from_generic_arg(clean[1..], module_name)
	}
	if clean.starts_with('...') {
		return '...' + strip_decl_module_from_generic_arg(clean[3..], module_name)
	}
	if clean.starts_with('[]') {
		return '[]' + strip_decl_module_from_generic_arg(clean[2..], module_name)
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := strip_decl_module_from_generic_arg(clean[4..bracket_end], module_name)
			val := strip_decl_module_from_generic_arg(clean[bracket_end + 1..], module_name)
			return 'map[${key}]${val}'
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return clean[..bracket_end + 1] + strip_decl_module_from_generic_arg(clean[bracket_end + 1..], module_name)
		}
	}
	base, args, ok := generic_app_parts(clean)
	if ok {
		stripped_base := strip_decl_module_from_generic_base(base, module_name)
		mut stripped_args := []string{cap: args.len}
		for generic_arg in args {
			stripped_args << strip_decl_module_from_generic_arg(generic_arg, module_name)
		}
		return '${stripped_base}[${stripped_args.join(', ')}]'
	}
	return strip_decl_module_from_generic_base(clean, module_name)
}

fn strip_decl_module_from_generic_base(name string, module_name string) string {
	prefix := '${module_name}.'
	if !name.starts_with(prefix) {
		return name
	}
	short := name[prefix.len..]
	if short.starts_with('Array_') {
		return name
	}
	return short
}

fn (mut t Transformer) build_generic_alias_name_index() {
	mut names := map[string]bool{}
	if !isnil(t.tc) {
		for alias, _ in t.tc.type_aliases.ids {
			names[alias] = true
			names[alias.all_after_last('.')] = true
		}
	}
	t.generic_alias_names = names.move()
}

fn (t &Transformer) generic_arg_is_alias_name(arg string, module_name string) bool {
	clean := trimmed_transform_text(arg)
	if clean.len == 0 || isnil(t.tc) {
		return false
	}
	if t.tc.type_aliases.contains(clean) {
		return true
	}
	if clean.starts_with('main.') && clean[5..] in t.tc.type_aliases.ids {
		return true
	}
	if !clean.contains('.') {
		scoped := if module_name != '' && module_name != 'main' && module_name != 'builtin' {
			'${module_name}.${clean}'
		} else {
			clean
		}
		if t.tc.type_aliases.contains(scoped) {
			return true
		}
		// A concrete type declared in the call's module is authoritative over an
		// imported alias with the same short name.
		if scoped in t.tc.structs || t.tc.sum_types.contains(scoped) || scoped in t.tc.enum_names
			|| scoped in t.tc.interface_names {
			return false
		}
	}
	return t.generic_alias_names[clean]
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
	return t.generic_type_text_contains_alias_bytes(flat.source_type_name(typ_id), module_name)
}

fn (t &Transformer) generic_type_text_contains_alias_bytes(typ string, module_name string) bool {
	clean := trimmed_transform_text(typ)
	if clean.len == 0 {
		return false
	}
	if t.generic_arg_is_alias_name(clean, module_name) {
		return true
	}
	if clean.starts_with('&') {
		return t.generic_type_text_contains_alias_bytes(clean[1..], module_name)
	}
	if clean.starts_with('mut ') {
		return t.generic_type_text_contains_alias_bytes(clean[4..], module_name)
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		return t.generic_type_text_contains_alias_bytes(clean[1..], module_name)
	}
	if clean.starts_with('...') {
		return t.generic_type_text_contains_alias_bytes(clean[3..], module_name)
	}
	if clean.starts_with('[]') {
		return t.generic_type_text_contains_alias_bytes(clean[2..], module_name)
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			return t.generic_type_text_contains_alias_bytes(clean[4..bracket_end], module_name)
				|| t.generic_type_text_contains_alias_bytes(clean[bracket_end + 1..], module_name)
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return t.generic_type_text_contains_alias_bytes(clean[bracket_end + 1..], module_name)
		}
	}
	base, args, ok := generic_app_parts(clean)
	if ok {
		if t.generic_arg_is_alias_name(base, module_name) {
			return true
		}
		for arg in args {
			if t.generic_type_text_contains_alias_bytes(arg, module_name) {
				return true
			}
		}
	}
	return false
}

fn (t &Transformer) generic_type_text_contains_fixed_array_alias(typ_id u32, module_name string) bool {
	return t.generic_type_text_contains_fixed_array_alias_bytes(flat.source_type_name(typ_id), module_name)
}

fn (t &Transformer) generic_type_text_contains_fixed_array_alias_bytes(typ string, module_name string) bool {
	clean := trimmed_transform_text(typ)
	if clean.len == 0 {
		return false
	}
	if t.generic_arg_is_alias_name(clean, module_name) {
		return t.is_fixed_array_type(t.normalize_type_in_module(flat.source_type_id(clean), module_name))
	}
	if clean.starts_with('&') {
		return t.generic_type_text_contains_fixed_array_alias_bytes(clean[1..], module_name)
	}
	if clean.starts_with('mut ') {
		return t.generic_type_text_contains_fixed_array_alias_bytes(clean[4..], module_name)
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		return t.generic_type_text_contains_fixed_array_alias_bytes(clean[1..], module_name)
	}
	if clean.starts_with('...') {
		return t.generic_type_text_contains_fixed_array_alias_bytes(clean[3..], module_name)
	}
	if clean.starts_with('[]') {
		return t.generic_type_text_contains_fixed_array_alias_bytes(clean[2..], module_name)
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			return t.generic_type_text_contains_fixed_array_alias_bytes(clean[4..bracket_end], module_name)
				|| t.generic_type_text_contains_fixed_array_alias_bytes(clean[bracket_end + 1..], module_name)
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return t.generic_type_text_contains_fixed_array_alias_bytes(clean[bracket_end + 1..], module_name)
		}
	}
	_, args, ok := generic_app_parts(clean)
	if ok {
		for arg in args {
			if t.generic_type_text_contains_fixed_array_alias_bytes(arg, module_name) {
				return true
			}
		}
	}
	return false
}

fn (t &Transformer) qualify_generic_arg_for_decl_module(arg_id u32, module_name string) u32 {
	clean_id := t.trimmed_type_id(arg_id)
	clean := flat.source_type_name(clean_id)
	if clean.len == 0 {
		return clean_id
	}
	for current_arg in t.active_specialization_args.ids {
		current_clean_id := t.trimmed_type_id(current_arg)
		current_clean := flat.source_type_name(current_clean_id)
		if current_clean_id != 0 && current_clean_id != clean_id && c_name(current_clean) == clean {
			return current_clean_id
		}
	}
	if clean.starts_with('&') {
		return flat.source_type_id('&' + flat.source_type_name(t.qualify_generic_arg_for_decl_module(flat.source_type_id(clean[1..]), module_name)))
	}
	if clean.starts_with('mut ') {
		return flat.source_type_id('mut ' + flat.source_type_name(t.qualify_generic_arg_for_decl_module(flat.source_type_id(clean[4..]), module_name)))
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		return flat.source_type_id(clean[..1] + flat.source_type_name(t.qualify_generic_arg_for_decl_module(flat.source_type_id(clean[1..]), module_name)))
	}
	if clean.starts_with('...') {
		return flat.source_type_id('...' + flat.source_type_name(t.qualify_generic_arg_for_decl_module(flat.source_type_id(clean[3..]), module_name)))
	}
	if clean.starts_with('[]') {
		return flat.source_type_id('[]' + flat.source_type_name(t.qualify_generic_arg_for_decl_module(flat.source_type_id(clean[2..]), module_name)))
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := flat.source_type_name(t.qualify_generic_arg_for_decl_module(flat.source_type_id(clean[4..bracket_end]), module_name))
			val := flat.source_type_name(t.qualify_generic_arg_for_decl_module(flat.source_type_id(clean[bracket_end + 1..]), module_name))
			return flat.source_type_id('map[${key}]${val}')
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return flat.source_type_id(clean[..bracket_end + 1] + flat.source_type_name(t.qualify_generic_arg_for_decl_module(flat.source_type_id(clean[bracket_end + 1..]), module_name)))
		}
	}
	if clean.contains('.') || types.is_builtin_type_name(clean) {
		return clean_id
	}
	if t.generic_arg_module_owns_type(clean, module_name) {
		return flat.source_type_id('${module_name}.${clean}')
	}
	if qualified_source_id := t.qualified_types.type_id(clean) {
		return qualified_source_id
	}
	return clean_id
}

fn (t &Transformer) generic_arg_module_owns_type(name string, module_name string) bool {
	if module_name == '' || module_name == 'main' || module_name == 'builtin' {
		return false
	}
	qname := '${module_name}.${name}'
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
			typ_source_id := t.var_type(node.value)
			typ_source_id_3 := typ_source_id

			if typ_source_id != 0 {
				raw_typ_source_id := t.raw_var_type(node.value)
				raw_typ_source_id_2 := raw_typ_source_id

				return if raw_typ_source_id != 0 { raw_typ_source_id_2 } else { typ_source_id_3 }
			}
			if decl_type_source_id := t.local_decl_type_before(node.value, id) {
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
					return flat.source_type_id('[]${flat.source_type_name(elem_type)}')
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
			if node.value.len > 0 {
				return flat.source_type_id(node.value)
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
	if !t.generic_arg_is_alias_name(flat.source_type_name(alias_name), module_name) {
		return none
	}
	return flat.source_type_id('[]${flat.source_type_name(t.generic_qualified_alias_name(alias_name, module_name))}')
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
	if node.kind in [.cast_expr, .as_expr] && node.value.len > 0 {
		return flat.source_type_id(node.value)
	}
	if node.kind == .call && node.children_count > 0 {
		name := flat.source_type_name(t.generic_call_type_arg_name(t.a.child(&node, 0)))
		if name.len > 0 {
			return flat.source_type_id(name)
		}
	}
	for candidate in alias_name_candidates(node) {
		if candidate.len > 0 {
			return flat.source_type_id(candidate)
		}
	}
	return none
}

fn (t &Transformer) generic_qualified_alias_name(name_id u32, module_name string) u32 {
	clean_id := t.trimmed_type_id(name_id)
	if clean_id == 0 || isnil(t.tc) {
		return clean_id
	}
	clean := flat.source_type_name(clean_id)
	if t.tc.type_aliases.contains(clean) {
		return clean_id
	}
	if !flat.source_type_shape(clean_id).has(.dotted) && module_name != '' && module_name != 'main'
		&& module_name != 'builtin' {
		qname := '${module_name}.${clean}'
		if t.tc.type_aliases.contains(qname) {
			return flat.source_type_id(qname)
		}
	}
	if flat.source_type_shape(clean_id).has(.dotted) {
		return clean_id
	}
	mut found := u32(0)
	for alias, _ in t.tc.type_aliases.ids {
		if alias.all_after_last('.') != clean {
			continue
		}
		alias_id := flat.source_type_id(alias)
		if found != 0 && found != alias_id {
			return clean_id
		}
		found = alias_id
	}
	return if found != 0 { found } else { clean_id }
}

fn (mut t Transformer) generic_call_arg_type_for_inference(id flat.NodeId) u32 {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return u32(0)
	}
	node := t.a.nodes[int(id)]
	if node.kind == .string_literal
		&& !(node.children_count == 1 && node.value in ['__v3_comptime_zero', '__v3_comptime_new']) {
		// Raw and JavaScript string literals keep their quote mode in `typ` as
		// parser metadata. Generic inference still sees their semantic V type.
		return flat.source_type_string
	}
	if node.kind == .field_init && node.typ_text().all_after_last('.').starts_with('AnonStruct_') {
		return t.normalize_type_in_module(node.typ, t.node_module_or(int(id), t.cur_module))
	}
	if node.kind == .prefix && node.op == .amp && node.children_count > 0 {
		child_id := t.a.child(&node, 0)
		child := t.a.nodes[int(child_id)]
		inner_source_id := t.generic_call_arg_type_for_inference(child_id)
		inner := flat.source_type_name(inner_source_id)
		if inner_source_id != 0 {
			// `mut value` is lowered to an address for the C calling convention, but
			// generic inference still sees the value's language-level type. A source
			// `&value` is a real pointer expression and retains the extra indirection.
			if child.is_mut {
				return inner_source_id
			}
			if child.kind == .ident && t.mut_value_ident_nodes[int(child_id)]
				&& inner.starts_with('&') && !flat.source_type_shape(t.raw_var_type(child.value)).has(.pointer) {
				// A cloned `mut value T` parameter carries `&Concrete` on its identifier
				// as storage metadata. Source `&value` denotes `&Concrete`, not
				// `&&Concrete`; consume the storage pointer before applying the source one.
				return inner_source_id
			}
			return flat.source_type_id('&${inner}')
		}
	}
	if node.kind == .selector && node.children_count > 0 {
		base_id := t.a.child(&node, 0)
		base_type_source_id := t.generic_call_arg_type_for_inference(base_id)

		if base_type_source_id != 0 {
			lookup_type_source_id := t.trim_pointer_type(t.normalize_type_alias(base_type_source_id))

			if field_type_source_id := t.lookup_struct_field_type(lookup_type_source_id, node.value) {
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
		base_type := flat.source_type_name(base_type_source_id_2)
		if base_type.starts_with('[]') || base_type_source_id_2 == flat.source_type_string {
			return base_type_source_id_2
		}
		if t.is_fixed_array_type(base_type_source_id_2) {
			return flat.source_type_id('[]${fixed_array_elem_type_bytes(base_type)}')
		}
	}
	if node.kind in [.array_literal, .fn_literal, .lambda_expr] {
		typ_source_id_2 := t.generic_arg_expr_type(id)

		if typ_source_id_2 != 0 {
			return typ_source_id_2
		}
	}
	if node.kind == .ident {
		if node.value.starts_with('__') && generic_inference_arg_type_usable(node.typ)
			&& !t.generic_arg_is_unresolved(t.normalize_type_alias(node.typ)) {
			// Lowered expression temporaries carry their concrete type on the generated
			// identifier. The checker's expression cache belongs to the original tree and
			// can only report the unspecialized container (`map`, `array`) here.
			return t.generic_inference_argument_type(node.typ, t.node_module_or(int(id), t.cur_module))
		}
		if t.mut_value_ident_nodes[int(id)] && generic_inference_arg_type_usable(node.typ) {
			return node.typ
		}
		if sc := t.find_smartcast(node.value) {
			target_source_id := t.smartcast_target_type(sc)
			if generic_inference_arg_type_usable(target_source_id)
				&& !t.generic_arg_is_unresolved(t.normalize_type_alias(target_source_id)) {
				return t.canonical_generic_specialization_arg(target_source_id)
			}
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
		var_typ_source_id := t.var_type(node.value)

		raw_var_typ_source_id := t.raw_var_type(node.value)
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
				checked_name := checked.name()
				if generic_inference_arg_type_usable(flat.source_type_id(checked_name))
					&& !t.generic_arg_is_unresolved(t.normalize_type_alias(flat.source_type_id(checked_name))) {
					return t.generic_inference_argument_type(flat.source_type_id(checked_name), t.node_module_or(int(id),
						t.cur_module))
				}
			}
		}
		if scoped_var_typ != 0 {
			return t.generic_inference_argument_type(scoped_var_typ, t.node_module_or(int(id), t.cur_module))
		}
		if decl_type_source_id := t.local_decl_type_before(node.value, id) {
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
	typ := flat.source_type_name(typ_id)
	clean := trimmed_transform_text(typ)
	clean_id := if clean == typ { typ_id } else { flat.source_type_id(clean) }
	return decl_type_is_usable(clean_id) && clean_id != flat.source_type_void
}

fn (t &Transformer) generic_inference_argument_type(typ_id u32, module_name string) u32 {
	typ := flat.source_type_name(typ_id)
	clean := trimmed_transform_text(typ)
	clean_id := if clean == typ { typ_id } else { flat.source_type_id(clean) }
	if t.current_specialization_has_generic_arg(clean_id) {
		locked := t.lock_colliding_main_generic_type_text(clean_id, module_name)
		if locked != clean_id {
			return locked
		}
	}
	if t.generic_type_text_contains_alias(clean_id, module_name) {
		return t.generic_arg_for_call_and_decl_module(clean_id, module_name, module_name)
	}
	return t.generic_inference_alias_target(clean_id, module_name)
}

fn (t &Transformer) generic_receiver_decl_inference_type(param_type_id u32, decl_type_id u32, module_name string) u32 {
	param_type := flat.source_type_name(param_type_id)
	decl_type := flat.source_type_name(decl_type_id)
	mut param := trimmed_transform_text(param_type)
	for param.starts_with('&') || param.starts_with('mut ') {
		param = if param.starts_with('mut ') {
			trimmed_transform_text(param[4..])
		} else {
			trimmed_transform_text(param[1..])
		}
	}
	mut declared := trimmed_transform_text(decl_type)
	for declared.starts_with('&') || declared.starts_with('mut ') {
		declared = if declared.starts_with('mut ') {
			trimmed_transform_text(declared[4..])
		} else {
			trimmed_transform_text(declared[1..])
		}
	}
	param_base, _, param_is_generic := generic_app_parts(param)
	decl_base, _, decl_is_generic := generic_app_parts(declared)
	if param_is_generic && decl_is_generic
		&& param_base.all_after_last('.') == decl_base.all_after_last('.')
		&& t.generic_arg_is_alias_name(decl_base, module_name) {
		return t.generic_inference_argument_type(decl_type_id, module_name)
	}
	return t.generic_inference_alias_target(decl_type_id, module_name)
}

fn (mut t Transformer) generic_receiver_needs_decl_type_fallback(param_type_id u32, arg_type_id u32) bool {
	param_type := flat.source_type_name(param_type_id)
	if !generic_inference_arg_type_usable(arg_type_id) {
		return true
	}
	if !param_type.contains('[') {
		return false
	}
	mut inferred := map[string]u32{}
	probe_arg_type := generic_arg_type_for_param(param_type_id, arg_type_id)
	if probe_arg_type != 0 {
		infer_generic_type_args(param_type_id, probe_arg_type, mut inferred)
		t.infer_generic_receiver_suffix_args(param_type_id, probe_arg_type, mut inferred)
		t.infer_generic_embedded_receiver_args(param_type_id, probe_arg_type, mut inferred)
	}
	if inferred.len == 0 {
		return true
	}
	for name, arg_source_id in inferred {
		arg := flat.source_type_name(arg_source_id)

		if arg == name || t.generic_arg_is_unresolved(arg_source_id) {
			return true
		}
	}
	return false
}

fn (t &Transformer) generic_inference_alias_target(typ_id u32, module_name string) u32 {
	typ := flat.source_type_name(typ_id)
	clean := trimmed_transform_text(typ)
	if clean.len == 0 {
		return flat.source_type_id(clean)
	}
	// Specialization keys use bare main types. Keep their caller provenance when
	// inferring a nested call instead of resolving a homonym in the callee module.
	if t.substituted_type_belongs_to_main_generic(flat.source_type_id(clean))
		&& t.current_specialization_has_generic_arg(flat.source_type_id(clean)) {
		return flat.source_type_id(clean)
	}
	if t.current_specialization_has_generic_arg(flat.source_type_id(clean)) {
		base, args, is_generic := generic_provenance_app_parts(flat.source_type_id(clean))
		if is_generic && !clean.starts_with('map[') {
			mut scoped_args := []string{cap: args.len}
			for nested_arg in args {
				scoped_args << flat.source_type_name(t.generic_inference_alias_target(nested_arg, module_name))
			}
			scoped_base := flat.source_type_name(t.generic_inference_alias_target(base, module_name))
			return flat.source_type_id('${scoped_base}[${scoped_args.join(', ')}]')
		}
	}
	if target_source_id := t.direct_generic_inference_alias_target(flat.source_type_id(clean), module_name) {
		return target_source_id
	}
	normalized_source_id := t.normalize_type_in_module(flat.source_type_id(clean), module_name)
	normalized := flat.source_type_name(normalized_source_id)
	if normalized_source_id != 0 && normalized != clean && !t.generic_arg_is_unresolved(normalized_source_id) {
		return normalized_source_id
	}
	return flat.source_type_id(clean)
}

fn (t &Transformer) direct_generic_inference_alias_target(typ_id u32, module_name string) ?u32 {
	typ := flat.source_type_name(typ_id)
	if isnil(t.tc) {
		return none
	}
	if target_source_id := t.tc.type_aliases.type_id(typ) {
		return t.normalize_type_in_module(target_source_id, module_name)
	}
	if module_name != '' && !typ.contains('.') {
		qtyp := '${module_name}.${typ}'
		if target_source_id := t.tc.type_aliases.type_id(qtyp) {
			return t.normalize_type_in_module(target_source_id, module_name)
		}
	}
	if !typ.contains('.') {
		mut unique_target := ''
		for name, target_id in t.tc.type_aliases.ids {
			target := flat.source_type_name(target_id)
			if name != typ && !name.ends_with('.${typ}') {
				continue
			}
			if unique_target != '' && unique_target != target {
				return none
			}
			unique_target = target
		}
		if unique_target.len > 0 {
			return t.normalize_type_in_module(flat.source_type_id(unique_target), module_name)
		}
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
		if lhs.kind != .ident || lhs.value != name {
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
	if lhs.kind != .ident || lhs.value != name {
		return none
	}
	rhs_id := t.a.child(node, 1)
	rhs := t.a.node(rhs_id)
	if rhs.kind == .call {
		mut raw_type := t.raw_call_decl_return_type(rhs_id, *rhs) or { u32(0) }
		if raw_type == 0 && rhs.children_count > 0 {
			callee := t.a.child_node(rhs, 0)
			if callee.kind == .ident {
				raw_type = t.raw_return_type_for_fn_name(callee.value, *rhs) or { u32(0) }
			}
		}
		if raw_type != 0 {
			base, _, is_generic_alias := generic_app_parts(trimmed_transform_text(flat.source_type_name(raw_type)))
			if is_generic_alias && t.generic_arg_is_alias_name(base, t.cur_module) {
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
		if lhs.kind != .ident || lhs.value != name {
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
		if lhs.kind != .ident || lhs.value != name {
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

fn (t &Transformer) infer_generic_receiver_suffix_args(param_type_id u32, arg_type_id u32, mut inferred map[string]u32) {
	param_type := flat.source_type_name(param_type_id)
	arg_type := flat.source_type_name(arg_type_id)
	param := trimmed_transform_text(param_type).trim_left('&')
	arg := trimmed_transform_text(arg_type).trim_left('&')
	base, param_args, ok := generic_app_parts(param)
	if !ok || param_args.len == 0 || arg.len == 0 {
		return
	}
	if args_source_ids := t.recorded_type_specialization_args(flat.source_type_id(arg)) {
		t.infer_generic_receiver_recorded_args(param_args, args_source_ids, mut inferred)
		return
	}
	mut suffix := ''
	for prefix in [base, c_name(base)] {
		if arg.starts_with('${prefix}_') {
			suffix = arg[prefix.len + 1..]
			break
		}
	}
	if suffix.len == 0 {
		short := base.all_after_last('.')
		arg_short := if arg.contains('.') { arg.all_after_last('.') } else { arg }
		for candidate in [arg, arg_short, c_name(arg), c_name(arg_short)] {
			for prefix in [short, c_name(short)] {
				if candidate.starts_with('${prefix}_') {
					suffix = candidate[prefix.len + 1..]
					break
				}
			}
			if suffix.len > 0 {
				break
			}
		}
	}
	if suffix.len == 0 {
		return
	}
	mut assigned := false
	for i, param_arg in param_args {
		if !is_generic_fn_placeholder_name(param_arg) || param_arg in inferred {
			continue
		}
		decoded := generic_type_arg_from_receiver_suffix(suffix)
		if decoded.len == 0 {
			continue
		}
		if i == 0 || !assigned {
			inferred[param_arg] = flat.source_type_id(decoded)
			assigned = true
		}
	}
}

fn (t &Transformer) infer_generic_receiver_recorded_args(param_args []string, args_ids []u32, mut inferred map[string]u32) {
	mut arg_idx := 0
	for param_arg in param_args {
		if !is_generic_fn_placeholder_name(param_arg) || param_arg in inferred {
			continue
		}
		if arg_idx >= args_ids.len {
			return
		}
		inferred[param_arg] = args_ids[arg_idx]
		arg_idx++
	}
}

fn (t &Transformer) infer_generic_embedded_receiver_args(param_type_id u32, arg_type_id u32, mut inferred map[string]u32) {
	param_type := flat.source_type_name(param_type_id)
	arg_type := flat.source_type_name(arg_type_id)
	param := trimmed_transform_text(param_type).trim_left('&')
	base, _, ok := generic_app_parts(param)
	if !ok || base.len == 0 {
		return
	}
	mut receiver_type := trimmed_transform_text(arg_type).trim_left('&')
	receiver_base, _, receiver_is_generic := generic_app_parts(receiver_type)
	if receiver_is_generic {
		receiver_type = receiver_base
	}
	info := t.structs[receiver_type] or {
		short := if receiver_type.contains('.') {
			receiver_type.all_after_last('.')
		} else {
			receiver_type
		}
		t.structs[short] or { return }
	}
	for field in info.fields {
		field_type := trimmed_transform_text(field.typ_text()).trim_left('&')
		field_base, _, field_is_generic := generic_app_parts(field_type)
		if !field_is_generic {
			continue
		}
		if field_base == base || field_base.all_after_last('.') == base.all_after_last('.') {
			infer_generic_type_args(flat.source_type_id(param), flat.source_type_id(field_type), mut inferred)
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
	infer_generic_type_args_bytes(flat.source_type_name(param_type_id), flat.source_type_name(arg_type_id), mut inferred)
}

fn infer_generic_type_args_bytes(param_type string, arg_type string, mut inferred map[string]u32) {
	param := trimmed_transform_text(param_type)
	arg := trimmed_transform_text(arg_type)
	if param.len == 0 || arg.len == 0 {
		return
	}
	if arg == 'unknown' || arg == 'generic' {
		return
	}
	if param.starts_with('...') {
		infer_generic_type_args_bytes(param[3..], arg.trim_left('[]'), mut inferred)
		return
	}
	if is_generic_fn_placeholder_name(param) {
		// A bare generic struct literal can initially carry its open checked type
		// (`Response[T]`). `T -> T` is not inference; leave the slot open so the
		// literal's concrete field values can infer it below.
		if param !in inferred && arg != param {
			inferred[param] = flat.source_type_id(arg)
		}
		return
	}
	if param.starts_with('&') {
		// A `&T` / `mut T` parameter binds to a by-value argument too (the arg type
		// carries no `&`), so strip the reference from the parameter regardless of
		// whether the argument is itself a reference.
		arg_inner := if arg.starts_with('&') { arg[1..] } else { arg }
		infer_generic_type_args_bytes(param[1..], arg_inner, mut inferred)
		return
	}
	if param.starts_with('mut ') {
		infer_generic_type_args_bytes(param[4..], arg.trim_left('&'), mut inferred)
		return
	}
	if param.starts_with('[]') && arg.starts_with('[]') {
		infer_generic_type_args_bytes(param[2..], arg[2..], mut inferred)
		return
	}
	if param.starts_with('[') && arg.starts_with('[') {
		p_end := generic_matching_bracket(param, 0)
		a_end := generic_matching_bracket(arg, 0)
		if p_end < param.len && a_end < arg.len {
			infer_generic_type_args_bytes(param[p_end + 1..], arg[a_end + 1..], mut inferred)
		}
		return
	}
	if param.starts_with('[') && arg.ends_with(']') && arg.contains('[') {
		p_len := fixed_array_len_text(param)
		a_len := fixed_array_len_text(arg)
		if p_len == a_len {
			infer_generic_type_args_bytes(fixed_array_elem_type_bytes(param), fixed_array_elem_type_bytes(arg), mut inferred)
		}
		return
	}
	if param.starts_with('?') {
		// A `?T` parameter also binds a plain by-value argument (the arg is
		// auto-promoted to the option), so strip the `?` from the parameter
		// regardless of whether the argument itself carries one.
		arg_inner := if arg.starts_with('?') { arg[1..] } else { arg }
		infer_generic_type_args_bytes(param[1..], arg_inner, mut inferred)
		return
	}
	if param.starts_with('!') {
		arg_inner := if arg.starts_with('!') { arg[1..] } else { arg }
		infer_generic_type_args_bytes(param[1..], arg_inner, mut inferred)
		return
	}
	if param.starts_with('(') && param.ends_with(')') && arg.starts_with('(')
		&& arg.ends_with(')') && param.contains(',') && arg.contains(',') {
		param_parts := split_generic_args(param[1..param.len - 1])
		arg_parts := split_generic_args(arg[1..arg.len - 1])
		if param_parts.len == arg_parts.len {
			for i, part in param_parts {
				infer_generic_type_args_bytes(part, arg_parts[i], mut inferred)
			}
		}
		return
	}
	if param.starts_with('fn') && arg.starts_with('fn') {
		infer_generic_fn_type_args_bytes(param, arg, mut inferred)
		return
	}
	if param.starts_with('map[') && arg.starts_with('map[') {
		p_end := generic_matching_bracket(param, 3)
		a_end := generic_matching_bracket(arg, 3)
		if p_end < param.len && a_end < arg.len {
			infer_generic_type_args_bytes(param[4..p_end], arg[4..a_end], mut inferred)
			infer_generic_type_args_bytes(param[p_end + 1..], arg[a_end + 1..], mut inferred)
		}
		return
	}
	p_base, p_args, p_ok := generic_app_parts(param)
	if p_ok {
		a_base, a_args, a_ok := generic_app_parts(arg)
		if p_ok && a_ok && p_base.all_after_last('.') == a_base.all_after_last('.') {
			for i, p_arg in p_args {
				if i < a_args.len {
					infer_generic_type_args_bytes(p_arg, a_args[i], mut inferred)
				}
			}
		}
	}
}

fn infer_generic_fn_type_args_bytes(param string, arg string, mut inferred map[string]u32) {
	p_params, p_ret := fn_type_text_parts(param) or { return }
	a_params, a_ret := fn_type_text_parts(arg) or { return }
	for i, p_param in p_params {
		if i >= a_params.len {
			break
		}
		infer_generic_type_args_bytes(generic_fn_type_param_payload(p_param), generic_fn_type_param_payload(a_params[i]), mut inferred)
	}
	if p_ret.len > 0 && a_ret.len > 0 {
		infer_generic_type_args_bytes(p_ret, a_ret, mut inferred)
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
		return node.value
	}
	if node.kind == .selector && node.value == 'name' && node.children_count > 0 {
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
	return t.clone_generic_node_from(node, args_ids, true, u32(0))
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
	return t.clone_generic_node_from(header, args_ids, true, u32(0))
}

@[direct_array_access]
fn (mut t Transformer) clone_generic_node(id flat.NodeId, args_ids []u32) flat.NodeId {
	return t.clone_generic_node_with_return_context(id, args_ids, u32(0))
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
					value_type := value_types[i].name()
					if lhs.kind == .ident && lhs.value != '_' && !t.generic_arg_is_unresolved(flat.source_type_id(value_type)) {
						t.set_node_typ(int(lhs_id), flat.source_type_id(value_type))
						t.set_var_type_with_raw(lhs.value, t.normalize_type_alias(flat.source_type_id(value_type)), flat.source_type_id(value_type))
					}
				}
			}
		}
	}
	return clone_id
}

fn (mut t Transformer) clone_specialized_comptime_new_marker(node flat.Node, target_id u32) flat.NodeId {
	target := flat.source_type_name(target_id)
	type_expr := t.a.add_node(flat.Node{
		kind:  .ident
		value: target
	})
	start := t.a.children.len
	t.a.children << type_expr
	marker := t.a.add_node(flat.Node{
		kind:           .string_literal
		pos:            node.pos
		value:          node.value
		typ:            node.typ
		children_start: start
		children_count: 1
	})
	// Keep the deferred marker pointer-typed when call arguments inspect it before transforming it.
	return t.make_cast(flat.source_type_id('&${target}'), marker, flat.source_type_id('&${target}'))
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
	return flat.source_type_id(flat.source_type_name(type_id).trim_string_left('...'))
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
	for i in 0 .. decl.node.children_count {
		param := t.a.child_node(&decl.node, i)
		if param.kind != .param {
			if t.prefix_param_scan { break }
			continue
		}
		arg_id := t.generic_call_arg_id_for_param(node, param_idx, is_receiver) or {
			param_idx++
			continue
		}
		mut arg_type := t.generic_clone_argument_type(arg_id, args_ids)
		if decl_type_is_usable(arg_type) && !t.generic_arg_is_unresolved(arg_type) {
			param_type := generic_inference_param_type(param)
			if (param.is_mut || (is_receiver && param_idx == 0 && !flat.source_type_name(param_type).starts_with('&')))
				&& flat.source_type_shape(arg_type).has(.pointer) {
				arg_type = flat.source_type_id(flat.source_type_name(arg_type)[1..])
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
		arg_source_id := inferred[name] or { return flat.TypeTextList.from_names([]string{}).ids }

		if t.generic_arg_is_unresolved(arg_source_id) {
			return flat.TypeTextList.from_names([]string{}).ids
		}
		call_args << t.inherited_generic_arg_for_decl_module(arg_source_id, decl.module, args_ids)
	}
	return call_args.clone()
}

fn (mut t Transformer) generic_clone_call_param_types(node flat.Node, args_ids []u32, return_context_id u32) []u32 {
	if node.kind != .call || node.children_count < 2 || t.skip_generics
		|| return_context_id == 0 || t.cloning_comptime_for_depth > 0 {
		return flat.TypeTextList.from_names([]string{}).ids
	}
	decls := t.cached_generic_fn_decls()
	key := t.generic_call_decl_key(flat.empty_node, node, t.cur_module, decls) or { return flat.TypeTextList.from_names([]string{}).ids }
	decl := decls[key] or { return flat.TypeTextList.from_names([]string{}).ids }
	call_args_source_ids := t.generic_clone_call_type_args(node, decl, args_ids, return_context_id)

	if call_args_source_ids.len == 0 { return flat.TypeTextList.from_names([]string{}).ids }
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
			expected = flat.source_type_id(trimmed_transform_text(flat.source_type_name(expected)[1..]))
		}
		result_type := types.unalias_type(t.tc.parse_resolution_type(expected))
		if node.kind == .prefix {
			if node.children_count != 1 || child_index != 0 {
				return false
			}
			return match node.op {
				.not { result_type == types.Type(types.bool_) }
				.plus, .minus { result_type.is_integer() || result_type.is_float() }
				.bit_not { result_type.is_integer() || result_type is types.Enum }
				else { false }
			}
		}
		if node.children_count != 2 {
			return false
		}
		if !types.is_builtin_type_name(flat.source_type_name(expected)) {
			if _ := t.struct_operator_call_info_any(expected, node.op) {
				return false
			}
		}
		for i in 0 .. node.children_count {
			operand_name_source_id := t.node_type(t.a.child(&node, i))
			operand_name := flat.source_type_name(operand_name_source_id)
			operand_type := types.unalias_type(t.tc.parse_resolution_type(operand_name_source_id))
			// Pointer arithmetic and overloaded operators need their operand types,
			// which can differ from the enclosing return type.
			if operand_type is types.Pointer || operand_type is types.Struct {
				return false
			}
			if !types.is_builtin_type_name(operand_name) {
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
		if result_type == types.Type(types.bool_) {
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
		&& node.value in ['__v3_comptime_zero', '__v3_comptime_new'] {
		if target_source_id := t.generic_comptime_type_expr(t.a.child(&node, 0), args_ids) {
			return if node.value == '__v3_comptime_new' {
				t.clone_specialized_comptime_new_marker(node, target_source_id)
			} else {
				t.comptime_zero_value(target_source_id)
			}
		}
	}
	if node.kind == .selector && node.children_count > 0 {
		base_id := t.a.child(&node, 0)
		if node.value == 'name' {
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
		if node.value in ['idx', 'typ', 'key_type', 'value_type', 'element_type', 'payload_type',
			'pointee_type', 'indirections']
			&& t.selector_base_is_comptime_type_value(base_id) {
			if concrete_source_id := t.generic_comptime_type_expr_for_member(base_id, node.value, args_ids) {
				concrete := flat.source_type_name(concrete_source_id)

				if node.value == 'indirections' {
					return t.make_int_literal(generic_type_indirections(flat.source_type_id(concrete)))
				}
				target := if node.value in ['idx', 'typ'] {
					concrete_source_id
				} else {
					t.generic_comptime_type_member(concrete_source_id, node.value) or { u32(0) }
				}
				if target != 0 {
					return t.make_int_literal(t.comptime_field_type_id(target, t.cur_module))
				}
			}
		}
		if node.value == 'variant_types' && t.selector_base_is_comptime_type_value(base_id) {
			if concrete_source_id := t.generic_comptime_type_expr(base_id, args_ids) {
				resolved := flat.source_type_name(t.resolve_sum_name(concrete_source_id))
				if variants_source_ids := t.sum_types.type_ids(resolved) {
					mut ids := []flat.NodeId{cap: variants_source_ids.len}
					for variant_source_id in variants_source_ids {
						variant_source_id_3 := variant_source_id

						ids << t.make_int_literal(t.comptime_field_type_id(variant_source_id_3, t.cur_module))
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
		cond := t.subst_comptime_type_condition(node.value, args_ids)
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
		&& !generic_text_contains_param(node.typ_text(), t.active_generic_params) {
		return t.a.add_node(node)
	}
	// Calls nested inside a `$for` body must not be specialized during the clone: the loop
	// variable's members are not resolved yet, so mono would infer a garbage type argument and
	// emit an uncalled, uncompilable specialization. The comptime unroll (or its skip) handles
	// them later against the concrete field types.
	is_comptime_for := node.kind == .comptime_for
	if is_comptime_for {
		t.cloning_comptime_for_depth++
		loop_var, _ := comptime_for_parts(node.value)
		t.cloning_comptime_for_vars << loop_var
	}
	argument_types_source_ids := t.generic_clone_call_param_types(node, args_ids, return_context_id)

	scratch_start := t.generic_clone_children.len
	for _ in 0 .. node.children_count {
		t.generic_clone_children << flat.empty_node
	}
	for i in 0 .. node.children_count {
		mut match_smartcasts := 0
		// Receiver calls are retargeted while the generic tree is cloned. Preserve
		// the branch narrowing here so they are not bound to the enclosing sum type.
		if node.kind == .match_stmt && i > 0 {
			match_expr_id := t.a.child(&node, 0)
			branch := t.a.child_node(&node, i)
			for sc in t.match_branch_type_contexts(match_expr_id, branch) {
				variant := t.resolve_substituted_type_text(t.subst_type(sc.variant_name, args_ids))
				sum_type := t.resolve_substituted_type_text(t.subst_type(sc.sum_type_name, args_ids))
				t.push_smartcast(sc.expr_name, variant, sum_type)
				match_smartcasts++
			}
		}
		child_context_source_id := t.generic_clone_child_return_context(node, i, return_context_id, argument_types_source_ids.clone())

		child := t.clone_generic_node_with_return_context(t.a.child(&node, i), args_ids,
			child_context_source_id)
		for _ in 0 .. match_smartcasts {
			t.pop_smartcast()
		}
		t.generic_clone_children[scratch_start + i] = child
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
	substituted_node_type := if (node.kind == .directive
		&& node.value == 'string_interp_format') || node.kind == .offsetof_expr {
		// `offsetof_expr.typ_text()` stores the field name, not a type annotation.
		node.typ_text()
	} else {
		flat.source_type_name(t.subst_type(node.typ, args_ids))
	}
	mut cloned_typ := (
		if substituted_node_type == node.typ_text() {
			node.typ
		} else if t.generic_type_text_contains_alias(flat.source_type_id(substituted_node_type), t.cur_module) {
			flat.source_type_id(substituted_node_type)
		} else if node.kind == .comptime_for {
			// Reflection needs the source-level generic application (`Box[int]`) so it
			// can substitute the receiver method's `T`. The storage spelling
			// (`Box_int`) intentionally loses those arguments.
			flat.source_type_id(substituted_node_type)
		} else if node.kind == .directive && node.value == 'string_interp_format' {
			flat.source_type_id(substituted_node_type)
		} else {
			t.resolve_substituted_type_text(flat.source_type_id(substituted_node_type))
		}
	)
	// The parser stores `?T{}` on struct_init.value while leaving typ empty. Give
	// the clone its concrete optional type so the empty-literal handling below can
	// mark it as a present zero value, matching generic `?T{}` semantics.
	if node.kind == .struct_init && cloned_typ == 0 && t.is_optional_type_name(flat.source_type_id(node.value)) {
		cloned_typ = t.resolve_substituted_type_text(t.subst_type(flat.source_type_id(node.value), args_ids))
	}
	// A parameter type produced by substituting a generic parameter can carry an
	// external program (main) type that collides by short name with a same-named
	// type in the specialization's module (a user `Context` embedding `veb.Context`).
	// Lock it to an explicit `main.` spelling so codegen (and the closure's field
	// accesses like `ctx.Context`) bind the program type, not the callee module's
	// homonym — mirroring the struct-init literal lock below.
	if substituted_node_type != node.typ_text() && node.kind != .directive {
		cloned_typ = t.lock_colliding_main_substitution_type_text(node.typ, cloned_typ, t.cur_module, t.active_generic_params)
	}
	if node.kind == .ident && t.cloning_generic_fn_depth > 0 {
		scoped_type_source_id := t.raw_var_type(node.value)
		scoped_type_source_id_4 := scoped_type_source_id

		if scoped_type_source_id != 0 && !t.generic_arg_is_unresolved(scoped_type_source_id) {
			cloned_typ = scoped_type_source_id_4
		}
	}
	if node.kind == .ident && t.mut_param_values[node.value] && flat.source_type_shape(cloned_typ).has(.pointer)
		&& !flat.source_type_shape(t.raw_var_type(node.value)).has(.pointer) {
		// A use of a `mut T` parameter is a T value even though the parameter's
		// storage annotation is `&T`. Keeping the pointer annotation on cloned
		// idents makes the later generic-call scan emit duplicate `&T`
		// specializations instead of reusing the value-type specialization. An
		// explicit `mut p &T` has a scoped `&T` rvalue type and must retain it.
		cloned_typ = flat.source_type_id(flat.source_type_name(cloned_typ)[1..])
	}
	if node.kind == .struct_init && node.value == '__v_option'
		&& t.is_optional_type_name(t.cur_fn_ret_type) {
		cloned_typ = t.cur_fn_ret_type
	}
	if node.kind == .array_init && node.value.len > 0 {
		array_substituted := flat.source_type_name(t.subst_type(flat.source_type_id(node.value), args_ids))
		array_locked := if array_substituted != node.value {
			t.lock_colliding_main_substitution_type_text_bytes(node.value, array_substituted, t.cur_module, t.active_generic_params)
		} else {
			array_substituted
		}
		array_value := if array_locked.contains('main.') {
			array_locked
		} else {
			flat.source_type_name(t.resolve_substituted_type_text(flat.source_type_id(array_locked)))
		}
		// A fixed-array init keeps its whole type in `value`, a dynamic one only the
		// element type. Once a type parameter is substituted, the shape has to come
		// from the unsubstituted spelling: `[]T{}` with `T = [7]u8` is `[][7]u8`.
		is_fixed_init := if array_substituted != node.value {
			node.value.starts_with('[') && !node.value.starts_with('[]')
		} else {
			t.is_fixed_array_type(flat.source_type_id(array_value))
				|| (array_value.starts_with('[') && !array_value.starts_with('[]'))
		}
		cloned_typ = (if is_fixed_init {
			flat.source_type_id(array_value)
		} else {
			flat.source_type_id('[]${array_value}')
		})
	} else if node.kind == .struct_init && node.value.len > 0 && node.value != '__v_option'
		&& !t.is_optional_type_name(flat.source_type_id(node.value)) {
		// The checker can annotate `T{}` with its surrounding optional/result
		// context. For a concrete clone the literal itself is authoritative.
		struct_subst := flat.source_type_name(t.subst_type(flat.source_type_id(node.value), args_ids))
		struct_subst_locked := if struct_subst != node.value {
			t.lock_colliding_main_substitution_type_text_bytes(node.value, struct_subst, t.cur_module, t.active_generic_params)
		} else {
			struct_subst
		}
		struct_value_pre := flat.source_type_name(t.resolve_substituted_type_text(flat.source_type_id(struct_subst_locked)))
		mut struct_value0 := struct_value_pre
		if struct_subst != node.value {
			// Only a `T{}` literal whose type came from substituting a generic
			// parameter can carry an external program (main) type. Lock a colliding
			// one so codegen builds that struct rather than the callee module's
			// homonym. A literal module type (`json2.Decoder{}` written as `Decoder{}`
			// inside json2) is left untouched — it is bare-keyed but not a lock target.
			struct_value0 = t.lock_colliding_main_substitution_type_text_bytes(node.value, struct_value_pre, t.cur_module, t.active_generic_params)
		}
		if struct_value0.contains('main.') {
			// Keep the explicit `main.` lock as the literal's type: parsing it back to
			// its bare program name here would let codegen rebase it into the callee
			// module (the very collision the lock prevents). The lock may be nested inside
			// a composite (`chan main.Context`, `[]main.Context`, `map[string]main.Context`),
			// not just a scalar `main.Context`, so match `main.` anywhere — a bare
			// `chan Context{}` literal would otherwise round-trip to the callee's homonym.
			cloned_typ = flat.source_type_id(struct_value0)
		} else {
			struct_value_source_id := t.normalize_type_alias(flat.source_type_id(struct_value0))
			struct_value_source_id_2 := struct_value_source_id

			parsed := t.tc.parse_resolution_type(struct_value_source_id)
			cloned_typ = (if t.is_fixed_array_type(flat.source_type_id(struct_value0)) {
				t.resolved_fixed_array_canonical_type(flat.source_type_id(struct_value0))
			} else if t.generic_arg_is_alias_name(struct_value0, t.cur_module) {
				flat.source_type_id(struct_value0)
			} else if parsed is types.Unknown {
				struct_value_source_id_2
			} else {
				specialized_signature_storage_type_name(parsed)
			})
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
		if field_type_source_id := t.lookup_struct_field_type(base_type_source_id, node.value) {
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
		raw_child_type := flat.source_type_name(t.generic_clone_decl_alias_type(children[0]))
		child_type := if t.generic_type_text_contains_alias(flat.source_type_id(raw_child_type), t.cur_module) {
			raw_child_type
		} else {
			flat.source_type_name(t.node_type(children[0]))
		}
		if node.op == .mul && child_type.starts_with('&') {
			cloned_typ = flat.source_type_id(child_type[1..])
		} else if node.op == .amp && child_type.len > 0 {
			cloned_typ = flat.source_type_id('&${child_type}')
		}
	}
	// Record the local's substituted type as the body is cloned: later sibling
	// clones (nested generic calls) infer their type args from these locals, and
	// without a binding the lookup falls back to an arena scan that can land in
	// an unrelated, previously cloned function with a same-named local.
	if node.kind == .decl_assign && node.children_count == 2 && t.cloning_comptime_for_depth == 0 {
		lhs_clone := t.a.nodes[int(children[0])]
		if lhs_clone.kind == .ident && lhs_clone.value.len > 0 {
			// Synthetic declarations created while transforming a generic template
			// carry their intended type on the declaration itself. Once substituted,
			// that concrete type is more precise than a helper call's broad checker type.
			has_concrete_substituted_type := substituted_node_type != node.typ_text()
				&& decl_type_is_usable(cloned_typ) && !t.generic_arg_is_unresolved(cloned_typ)
			rhs_node := t.a.nodes[int(children[1])]
			call_alias_type_source_id := t.generic_clone_decl_alias_type(children[1])
			call_alias_type := flat.source_type_name(call_alias_type_source_id)
			rhs_raw_typ := if call_alias_type_source_id != 0 {
				call_alias_type
			} else if t.generic_type_text_contains_alias(rhs_node.typ, t.cur_module) {
				rhs_node.typ_text()
			} else {
				''
			}
			mut rhs_typ := (if rhs_raw_typ.len > 0 {
				flat.source_type_id(rhs_raw_typ)
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
				normalized_lhs_typ := (if flat.source_type_name(lhs_typ).contains('main.') {
					lhs_typ
				} else {
					t.normalize_type_alias(lhs_typ)
				})
				t.set_var_type_with_raw(lhs_clone.value, normalized_lhs_typ, lhs_typ)
			}
		}
	}
	t.substitute_cloned_generic_call_type_args(node, mut children, args_ids)
	comptime_payload := if node.kind == .comptime_for {
		if substituted_node_type != node.typ_text() {
			flat.canonical_comptime_type_payload
		} else {
			node.payload
		}
	} else {
		u32(0)
	}
	if t.cloning_comptime_for_depth > 0 {
		// Inside a `$for` body: clone verbatim, no generic-call retargeting.
		mut comptime_value := t.subst_node_value(node, args_ids)
		// Lock a colliding caller type like the regular clone below: `T(v)` in a
		// `$for v in T.variants` body of json2 must keep a main `Any` distinct from
		// `json2.Any`.
		if node.kind in [.array_init, .map_init, .cast_expr, .as_expr]
			&& flat.source_type_name(t.subst_type(flat.source_type_id(node.value), args_ids)) != node.value {
			comptime_value = t.lock_colliding_main_substitution_type_text_bytes(node.value,
				comptime_value, t.cur_module, t.active_generic_params)
		}
		start2 := t.a.children.len
		for child in children {
			t.a.children << child
		}
		clone_id := t.a.add_node(flat.Node{
			kind:           node.kind
			op:             cloned_op
			pos:            node.pos
			children_start: start2
			children_count: flat.child_count(children.len)
			typ:            cloned_typ
			value:          comptime_value
			payload:        comptime_payload
			is_mut:         node.is_mut
			flags:          flat.clone_node_flags(node, false)
		})
		if node.kind == .ident && t.mut_param_values[node.value] {
			t.mut_value_ident_nodes[int(clone_id)] = true
		}
		t.generic_clone_children = t.generic_clone_children[..scratch_start]
		return clone_id
	}
	if node.kind == .struct_init && children.len == 0 && t.is_optional_type_name(cloned_typ)
		&& (node.value == '__v_option' || t.is_optional_type_name(flat.source_type_id(node.value))) {
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
	if node.kind == .call && children.len > 0
		&& (flat.source_type_shape(node.typ).has(.bracketed) || node.value.contains('[')) {
		type_text := (if flat.source_type_shape(node.typ).has(.bracketed) {
			node.typ
		} else {
			flat.source_type_id(node.value)
		})
		base, _, ok := generic_app_parts(flat.source_type_name(type_text))
		if ok {
			callee_id := children[0]
			mut callee := t.a.nodes[int(callee_id)]
			if callee.kind in [.ident, .selector] && callee.value == base.all_after_last('.') {
				t.set_node_value(int(callee_id), flat.source_type_name(t.subst_type(type_text, args_ids)))
			}
		}
	}
	mut cloned_value := if node.kind == .struct_init && cloned_typ != 0 {
		flat.source_type_name(cloned_typ)
	} else if is_root {
		specialized_generic_fn_value(node.value, args_ids)
	} else {
		t.subst_node_value(node, args_ids)
	}
	if node.kind in [.array_init, .map_init, .cast_expr, .as_expr] {
		substituted_value := flat.source_type_name(t.subst_type(flat.source_type_id(node.value), args_ids))
		if substituted_value != node.value {
			cloned_value = t.lock_colliding_main_substitution_type_text_bytes(node.value, cloned_value, t.cur_module, t.active_generic_params)
		}
	}
	if is_root && t.cur_module.len > 0 {
		t.a.add_node(flat.Node{
			kind:  .module_decl
			value: t.cur_module
		})
	}
	clone_id := t.a.add_node(flat.Node{
		kind:           node.kind
		op:             cloned_op
		pos:            node.pos
		children_start: start
		children_count: flat.child_count(children.len)
		typ:            final_typ
		value:          cloned_value
		payload:        comptime_payload
		is_mut:         node.is_mut
		flags:          flat.clone_node_flags(node, false)
	})
	if node.kind == .map_init && !isnil(t.tc) {
		map_type := (if cloned_value.starts_with('map[') {
			flat.source_type_id(cloned_value)
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
		if lhs.kind == .ident && lhs.value.len > 0 {
			t.specialization_decl_nodes_by_name[lhs.value] << int(clone_id)
		}
	}
	if node.kind == .param && cloned_value.len > 0 && cloned_typ != 0 {
		mut scoped_param_type := flat.source_type_name(cloned_typ)
		if node.is_mut && node.op != .amp && scoped_param_type.starts_with('&') {
			// The specialized signature keeps `mut value T` as pointer storage, but
			// expressions in its body see the language-level `T`. Do not overwrite
			// the value scope seeded by emit_generic_fn_specialization with `&T`.
			scoped_param_type = scoped_param_type[1..]
		}
		t.set_var_type(cloned_value, flat.source_type_id(scoped_param_type))
	}
	if node.kind == .ident && t.mut_param_values[node.value] {
		t.mut_value_ident_nodes[int(clone_id)] = true
	}
	// A literal of a generic struct without its type arguments, `Box{ item: a }`,
	// has the ones that the types of its fields give it in this instance, as
	// `Box[A]{}` has the ones that the substitution gives it: the check of the
	// body cannot tell them from `a A`, and every later reader of the type of the
	// literal took the bare `Box` (see infer_bare_generic_struct_init_type).
	if node.kind == .struct_init && !cloned_value.contains('[') {
		if inferred_source_id := t.infer_bare_generic_struct_init_type(t.a.nodes[int(clone_id)]) {
			inferred_source_id_2 := inferred_source_id
			inferred := flat.source_type_name(inferred_source_id_2)

			t.set_node_value(int(clone_id), inferred)
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
	if resolved_key is types.Struct && resolved_key.name() != 'any'
		&& (resolved_key.name() in t.tc.structs || resolved_key.name() in t.structs) {
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
		pointer := flat.source_type_name(t.generic_clone_decl_alias_type(t.a.child(&node, 0)))
		if pointer.starts_with('&') {
			return flat.source_type_id(pointer[1..])
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
		wrapped := flat.source_type_name(t.generic_clone_decl_alias_type(t.a.child(&node, 0)))
		if wrapped.starts_with('?') || wrapped.starts_with('!') {
			return flat.source_type_id(wrapped[1..])
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
	raw_iter_type := flat.source_type_name(t.generic_clone_decl_alias_type(container_id))
	iter_type := (if t.generic_type_text_contains_alias(flat.source_type_id(raw_iter_type), t.cur_module) {
		flat.source_type_id(raw_iter_type.trim_left('&'))
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
				flat.source_type_name(t.map_value_type(map_type))
			} else {
				flat.source_type_name(t.infer_for_in_elem_type(iter_type, node))
			}
			if node.op == .amp && value_type.len > 0 {
				value_type = '&${value_type}'
			}
			if value_type != '' {
				t.set_node_typ(int(val_id), flat.source_type_id(value_type))
				t.set_var_type(value.value, flat.source_type_id(value_type))
			}
		}
		return
	}
	if int(key_id) >= 0 {
		value := t.a.nodes[int(key_id)]
		value_type := if flat.source_type_shape(map_type).has(.map_type) {
			flat.source_type_name(t.map_value_type(map_type))
		} else {
			flat.source_type_name(t.infer_for_in_elem_type(iter_type, node))
		}
		if value_type.len > 0 {
			t.set_node_typ(int(key_id), flat.source_type_id(value_type))
			t.set_var_type(value.value, flat.source_type_id(value_type))
		}
	}
}

fn (t &Transformer) generic_comptime_base_type(id flat.NodeId, args_ids []u32) ?u32 {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		if is_generic_fn_placeholder_name(node.value) {
			idx := t.active_generic_param_index(node.value)
			if idx < args_ids.len {
				return args_ids[idx]
			}
		}
		substituted := flat.source_type_name(t.subst_type(flat.source_type_id(node.value), args_ids))
		if substituted != node.value && !t.generic_args_have_placeholders(flat.TypeTextList.from_names([
			substituted,
		]).ids) {
			return t.resolve_substituted_type_text(flat.source_type_id(substituted))
		}
		typ_source_id := t.raw_var_type(node.value)

		if typ_source_id != 0 && !t.generic_arg_is_unresolved(typ_source_id) {
			return typ_source_id
		}
	}
	if node.kind == .typeof_expr {
		if typ := generic_type_name_from_marker(node.value) {
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
			if child.kind == .ident && t.mut_param_values[child.value] {
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
	if node.kind == .array_init && node.value == '__v3_comptime_type_array'
		&& node.children_count == 1 {
		elem := flat.source_type_name(t.generic_comptime_type_expr(t.a.child(&node, 0), args_ids) or { return none })
		return flat.source_type_id('[]${elem}')
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
	return match node.value {
		'typ' {
			base_id
		}
		'unaliased_typ' {
			t.comptime_typeof_unaliased_type(base_source_id)
		}
		'payload_type', 'pointee_type', 'element_type', 'key_type', 'value_type' {
			t.generic_comptime_type_member(base_source_id, node.value)?
		}
		else {
			none
		}
	}
}

fn (mut t Transformer) generic_comptime_typeof_target(node flat.Node, args_ids []u32) ?u32 {
	if typ := generic_type_name_from_marker(node.value) {
		idx := t.active_generic_param_index(typ)
		if idx < args_ids.len {
			return args_ids[idx]
		}
	}
	if node.value.len > 0 && !node.value.starts_with(generic_type_name_marker_prefix)
		&& generic_text_contains_param(node.value, t.active_generic_params) {
		return t.resolve_substituted_type_text(t.subst_type(flat.source_type_id(node.value), args_ids))
	}
	if node.children_count == 0 {
		return none
	}
	child_id := t.a.child(&node, 0)
	child := t.a.nodes[int(child_id)]
	if child.kind == .selector && child.children_count > 0
		&& child.value in ['idx', 'key_type', 'value_type', 'element_type'] {
		base_id := t.a.child(&child, 0)
		if concrete_source_id := t.generic_comptime_type_expr(base_id, args_ids) {
			if child.value == 'idx' {
				return flat.source_type_int
			}
			return t.generic_comptime_type_member(concrete_source_id, child.value)
		}
	}
	target_source_id := t.generic_comptime_base_type(child_id, args_ids) or { return none }
	target := flat.source_type_name(target_source_id)
	if child.kind == .ident && t.mut_param_values[child.value] && !target.starts_with('&') {
		// A `mut value T` parameter is stored indirectly. `typeof(value)` observes
		// that pointer shape even though ordinary value uses infer T itself.
		return flat.source_type_id('&${target}')
	}
	return target_source_id
}

// generic_comptime_typeof_display_name retains the writing file's spelling for
// a composite type expression, while a direct parameter keeps its caller's name.
fn (t &Transformer) generic_comptime_typeof_display_name(node flat.Node, args_ids []u32, reflected string) string {
	if node.value.len > 0 && !node.value.starts_with(generic_type_name_marker_prefix)
		&& node.value !in t.active_generic_params {
		return generic_type_name_display(flat.source_type_name(t.subst_type(flat.source_type_id(node.value), args_ids)))
	}
	if node.children_count > 0 {
		child := t.a.child_node(&node, 0)
		if child.kind == .ident && child.value !in t.active_generic_params
			&& generic_text_contains_param(child.value, t.active_generic_params)
			&& (t.raw_var_type(child.value)) == 0 {
			return generic_type_name_display(flat.source_type_name(t.subst_type(flat.source_type_id(child.value), args_ids)))
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
				return flat.source_type_id(trimmed_transform_text(flat.source_type_name(clean)[1..]))
			}
		}
		'pointee_type' {
			pointee := if shape.has(.optional) {
				flat.source_type_id(trimmed_transform_text(flat.source_type_name(clean)[1..]))
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
		arg := flat.source_type_name(arg_source_id_10)
		if arg.len == 0 {
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
		if !t.generic_args_have_placeholders(flat.TypeTextList.from_names([arg]).ids)
			&& !generic_text_contains_param(arg, t.active_generic_params) {
			continue
		}
		substituted := flat.source_type_name(t.resolve_substituted_type_text(t.subst_type(arg_source_id_10, args_ids)))
		if substituted.len == 0 || substituted == arg
			|| t.generic_args_have_placeholders(flat.TypeTextList.from_names([substituted]).ids) {
			continue
		}
		children[i] = t.make_ident(substituted)
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
	text := flat.source_type_name(type_id)
	unlocked := type_text_without_main_locks_bytes(text)
	if unlocked == text {
		return type_id
	}
	return flat.source_type_id(unlocked)
}

fn type_text_without_main_locks_bytes(typ string) string {
	if !typ.contains('main.') {
		return typ
	}
	mut out := []u8{cap: typ.len}
	mut i := 0
	for i < typ.len {
		if i + 'main.'.len <= typ.len && typ[i..i + 'main.'.len] == 'main.'
			&& (i == 0 || (!typ[i - 1].is_letter() && !typ[i - 1].is_digit()
				&& typ[i - 1] !in [`_`, `.`])) {
			i += 'main.'.len
			continue
		}
		out << typ[i]
		i++
	}
	return out.bytestr()
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
	if callee.kind != .ident || callee.value != 'new_map' {
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
		&& t.receiver_selector_is_fn_field(t.trim_pointer_type(t.node_type(t.a.child(&base, 0))), base.value)
	if is_fn_field || t.generic_call_base_is_fn_value(base_id, base) {
		children[0] = base_id
	}
}

fn (mut t Transformer) retarget_cloned_map_key_storage_type(node flat.Node, mut children []flat.NodeId, cloned_typ_id u32) u32 {
	cloned_typ_comparison_type_id := cloned_typ_id

	if cloned_typ_id == u32(0) {
		return cloned_typ_id
	}
	mut name := ''
	if node.kind == .ident {
		name = node.value
	} else if node.kind == .decl_assign && children.len > 0 {
		lhs := t.a.nodes[int(children[0])]
		if lhs.kind == .ident {
			name = lhs.value
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
		expected := t.tc.parse_type(receiver_arg_id).name()
		given := t.tc.parse_type(call_args[i]).name()
		if expected == given { continue }
		t.tc.report_receiver_type_arg_mismatch(t.cur_file, node.pos, decl.node.value.all_after_last('.'),
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
			if callee.kind != .ident || callee.value.contains('[') {
				return u32(0)
			}
			if t.plain_concrete_callee_shadows_decl(callee.value, decl) {
				return u32(0)
			}
		}
		mut inferred := map[string]u32{}
		mut param_idx := 0
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
				raw_arg_type = flat.source_type_id(flat.source_type_name(raw_arg_type)[1..])
			}
			if (child.is_mut || flat.source_type_shape(child.typ).has(.mutable_type)) && flat.source_type_shape(raw_arg_type).has(.pointer)
				&& !is_forwarded_mut_value {
				// An ordinary pointer local passed to `mut T` has V's source-level
				// auto-dereferenced value semantics. A forwarded `mut T` parameter is
				// already represented by its semantic type and must keep a genuine `&T`.
				raw_arg_type = flat.source_type_id(flat.source_type_name(raw_arg_type)[1..])
			}
			if !is_generic_fn_placeholder_name(flat.source_type_name(inference_param_type)) {
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
					expected_return = flat.source_type_id(trimmed_transform_text(flat.source_type_name(expected_return)[1..]))
				}
				mut declared_return := trimmed_transform_text(decl.node.typ_text())
				for declared_return.starts_with('?') || declared_return.starts_with('!') {
					declared_return = trimmed_transform_text(declared_return[1..])
				}
				// An enclosing tuple result cannot infer a scalar method's receiver.
				// A scalar contextual result may infer method-level parameters, but a
				// receiver parameter must come from the actual receiver type.
				if !flat.source_type_name(expected_return).starts_with('(') || declared_return.starts_with('(') {
					inference_return := if trimmed_transform_text(decl.node.typ_text()).starts_with('!') {
						'!' + flat.source_type_name(expected_return)
					} else if trimmed_transform_text(decl.node.typ_text()).starts_with('?') {
						'?' + flat.source_type_name(expected_return)
					} else {
						flat.source_type_name(expected_return)
					}
					receiver_params := if is_receiver {
						t.generic_receiver_param_names(decl)
					} else {
						[]string{}
					}
					t.infer_generic_return_type_args(decl, flat.source_type_id(inference_return), mut return_inferred,
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
	spec_value := specialized_generic_fn_value(decl.node.value, concrete_call_args_source_ids)
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

			mut expected_type := trimmed_transform_text(flat.source_type_name(param_types_source_ids[0]))
			if expected_type.starts_with('mut ') {
				expected_type = trimmed_transform_text(expected_type[4..])
			}
			expected_type = expected_type.trim_left('&').trim_space()
			if expected_type.len > 0 {
				if embedded := t.embedded_receiver_base_for_type(receiver, base_type_source_id,
					flat.source_type_id(expected_type))
				{
					receiver = embedded
				}
			}
		}
		children[0] = t.make_ident(qualified_spec)
		children.insert(1, receiver)
	} else {
		children[0] = t.make_ident(qualified_spec)
	}
	t.append_missing_specialized_params_struct_args(mut children, param_types_source_ids.clone(), decl.module)
	return t.specialized_fn_return_type_text(decl, concrete_call_args_source_ids)
}

fn (t &Transformer) generic_composite_inference_alias_type(typ_id u32, module_name string) u32 {
	typ := flat.source_type_name(typ_id)
	if !t.generic_arg_is_alias_name(typ, module_name)
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
	if base.kind != .ident || (t.var_type(base.value)) != 0 {
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
	for candidate in t.generic_plain_call_candidates(base.value, t.cur_module) {
		decl := decls[candidate] or { continue }
		param_names := t.generic_fn_param_names(decl.node, decl.module)
		if param_names.len != call_args.len
			|| t.plain_concrete_callee_shadows_decl(base.value, decl) {
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
		spec_value := specialized_generic_fn_value(decl.node.value, concrete_args)
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
	if base.kind != .ident || !is_generic_fn_placeholder_name(base.value) {
		return u32(0)
	}
	idx := t.active_generic_param_index(base.value)
	if idx < 0 || idx >= args_ids.len {
		return u32(0)
	}
	owner := t.resolve_substituted_type_text(t.subst_type(args_ids[idx], args_ids))
	for type_name in t.static_assoc_type_candidates(owner) {
		static_fn := flat.encode_static_type_method_name(flat.source_type_name(type_name), callee.value)
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
		for i in 0 .. decl.node.children_count {
			child := t.a.child_node(&decl.node, i)
			if child.kind != .param {
				if t.prefix_param_scan {
					break
				}
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
				scoped_type_source_id := t.raw_var_type(clone_arg.value)
				scoped_type_source_id_2 := scoped_type_source_id
				scoped_type_source_id_3 := scoped_type_source_id_2

				if decl_type_is_usable(scoped_type_source_id_2) && !t.generic_arg_is_unresolved(scoped_type_source_id) {
					arg_type = scoped_type_source_id_3
				}
			}
			source_raw_type_source_id := t.generic_call_arg_type_for_inference(source_arg_id)
			source_raw_type := flat.source_type_name(source_raw_type_source_id)
			source_substituted_source_id := t.subst_type(source_raw_type_source_id, active_args_ids)
			source_has_alias := t.generic_type_text_contains_alias(source_substituted_source_id, t.cur_module)
			source_depends_on_active := generic_text_contains_param(source_raw_type, t.active_generic_params)
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
				arg_type = flat.source_type_id(flat.source_type_name(arg_type)[1..])
			}
			if (child.is_mut || flat.source_type_shape(child.typ).has(.mutable_type)) && flat.source_type_shape(arg_type).has(.pointer)
				&& !is_forwarded_mut_value {
				arg_type = flat.source_type_id(flat.source_type_name(arg_type)[1..])
			}
			if !is_generic_fn_placeholder_name(flat.source_type_name(inference_param_type)) {
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
	if !t.generic_callee_is_specialization(callee.value)
		&& t.plain_concrete_callee_shadows_decl(callee.value, decl) {
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
	if callee.kind != .ident || callee.value.len == 0 || args_ids.len == 0 {
		return false
	}
	spec_value := specialized_generic_fn_value(decl.node.value, args_ids)
	qualified_spec := transform_qualified_fn_name(decl.module, spec_value)
	return callee.value in [spec_value, qualified_spec, c_name(spec_value), c_name(qualified_spec)]
}

// specialized_generic_fn_type_text renders the `fn (params) ret` type that
// `explicit_generic_fn_value_specialization` writes onto a callee ident when it erases an explicit
// `fn_name[Type]` list into a specialization ident.
fn (mut t Transformer) specialized_generic_fn_type_text(decl GenericFnDecl, args_ids []u32) u32 {
	params := flat.TypeTextList{ ids: t.specialized_generic_call_param_type_texts(decl, args_ids) }.names()
	ret_type := flat.source_type_name(t.specialized_fn_return_type_text(decl, args_ids))
	return flat.source_type_id('fn (${params.join(', ')})${if ret_type.len > 0 && ret_type != 'void' {
		' ${ret_type}'
	} else {
		''
	}}')
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
	if callee.kind != .ident || callee.value.len == 0 {
		return none
	}
	recorded_source_ids := t.recorded_generic_specialization_args(callee.value) or { return none }
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
	return t.generic_specialization_progress_key(decl, args_ids) in t.generic_fn_spec_nodes
}

fn (t &Transformer) generic_specialization_in_progress(decl GenericFnDecl, args_ids []u32) bool {
	return t.generic_fn_specs_in_progress[t.generic_specialization_progress_key(decl, args_ids)]
}

fn (t &Transformer) generic_specialization_progress_key(decl GenericFnDecl, args_ids []u32) string {
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
	return callee.kind == .ident && ((callee.value.contains('[') && callee.value.contains(']'))
		|| t.generic_callee_is_specialization(callee.value))
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
		.call, .array_init, .map_init, .struct_init, .assoc, .cast_expr, .as_expr, .sizeof_expr,
		.offsetof_expr, .typeof_expr, .is_expr, .type_decl, .field_decl, .param {
			return substitute_generic_type_text_bytes(node.value, args_ids)
		}
		else {
			return node.value
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
		|| t.type_text_has_generic_placeholder_bytes(node.value, module_name) {
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
	if !node.value.contains('.') || isnil(t.tc) {
		return false
	}
	receiver := owner_name_view(node.value)
	base, args, ok := generic_app_parts(receiver)
	if !ok || args.len == 0 {
		return false
	}
	mut candidates := [base]
	if !base.contains('.') && module_name != '' && module_name !in ['main', 'builtin'] {
		candidates << '${module_name}.${base}'
	}
	for candidate in candidates {
		params := t.tc.struct_generic_params[candidate] or { continue }
		for arg in args {
			if trimmed_transform_text(arg) in params {
				return true
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

fn (mut t Transformer) node_subtree_has_generic_placeholder(id flat.NodeId, module_name string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if t.type_text_has_generic_placeholder(node.typ, module_name)
		|| t.type_text_has_generic_placeholder_bytes(node.value, module_name) {
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

fn (mut t Transformer) type_text_has_generic_placeholder(typ_id u32, module_name string) bool {
	return t.type_text_has_generic_placeholder_bytes(flat.source_type_name(typ_id), module_name)
}

// type_text_has_generic_placeholder_bytes parses source syntax and declaration payloads.
fn (mut t Transformer) type_text_has_generic_placeholder_bytes(typ string, module_name string) bool {
	if t.skip_generics {
		return false
	}
	if !generic_placeholder_prescreen(typ) {
		return false
	}
	clean := trimmed_transform_text(typ)
	if clean.len == 0 {
		return false
	}
	if clean == 'generic' {
		return true
	}
	if is_generic_fn_placeholder_name(clean) && !t.concrete_type_name_known(clean, module_name) {
		return true
	}
	if clean.starts_with('&') {
		return t.type_text_has_generic_placeholder_bytes(clean[1..], module_name)
	}
	if clean.starts_with('mut ') {
		return t.type_text_has_generic_placeholder_bytes(clean[4..], module_name)
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		return t.type_text_has_generic_placeholder_bytes(clean[1..], module_name)
	}
	if clean.starts_with('...') {
		return t.type_text_has_generic_placeholder_bytes(clean[3..], module_name)
	}
	if clean.starts_with('[]') {
		return t.type_text_has_generic_placeholder_bytes(clean[2..], module_name)
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			return t.type_text_has_generic_placeholder_bytes(clean[4..bracket_end], module_name)
				|| t.type_text_has_generic_placeholder_bytes(clean[bracket_end + 1..], module_name)
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return t.type_text_has_generic_placeholder_bytes(clean[bracket_end + 1..], module_name)
		}
	}
	_, args, ok := generic_app_parts(clean)
	if ok {
		for arg in args {
			if t.type_text_has_generic_placeholder_bytes(arg, module_name) {
				return true
			}
		}
	}
	return false
}

fn (mut t Transformer) concrete_type_name_known(name string, module_name string) bool {
	if types.is_builtin_type_name(name) || name in ['C', 'JS'] {
		return true
	}
	if t.is_known_main_struct_type_name(name) {
		return true
	}
	qname := if name.contains('.') || module_name == '' || module_name == 'main'
		|| module_name == 'builtin' {
		name
	} else {
		'${module_name}.${name}'
	}
	return name in t.structs || qname in t.structs || t.sum_types.contains(name)
		|| t.sum_types.contains(qname) || name in t.enum_types || qname in t.enum_types
		|| (!isnil(t.tc) && (t.tc.type_aliases.contains(name) || t.tc.type_aliases.contains(qname)
			|| name in t.tc.structs || qname in t.tc.structs || t.tc.sum_types.contains(name)
			|| t.tc.sum_types.contains(qname) || name in t.tc.enum_names
			|| qname in t.tc.enum_names || name in t.tc.interface_names
			|| qname in t.tc.interface_names))
}

fn (mut t Transformer) generic_fn_decl_key(node flat.Node, module_name string) string {
	base_value := if generic_fn_receiver_application_is_structured(node.value) {
		node.value
	} else {
		generic_fn_decl_base_value(node.value)
	}
	if module_name != '' && module_name != 'main' && module_name != 'builtin' {
		return '${module_name}.${base_value}'
	}
	return base_value
}

fn generic_fn_receiver_application_is_structured(value string) bool {
	if !value.contains('.') {
		return false
	}
	receiver := value.all_before_last('.')
	_, args, is_generic := generic_app_parts(receiver)
	if !is_generic || args.len == 0 {
		return false
	}
	return args.any(!is_generic_fn_placeholder_name(trimmed_transform_text(it)))
}

fn generic_fn_decl_base_value(value string) string {
	if receiver, method := flat.decode_static_type_method_name(value) {
		base, _, ok := generic_app_parts(receiver)
		if ok {
			return flat.encode_static_type_method_name(base, method)
		}
		return value
	}
	if !value.contains('.') {
		return value
	}
	receiver := value.all_before_last('.')
	method := value.all_after_last('.')
	base, _, ok := generic_app_parts(receiver)
	if ok {
		return '${base}.${method}'
	}
	return value
}

fn (t &Transformer) generic_decl_is_receiver_method(node flat.Node) bool {
	if !node.value.contains('.') || node.children_count == 0 {
		return false
	}
	first := t.a.child_node(&node, 0)
	if first.kind != .param || first.typ == 0 {
		return false
	}
	mut first_type := flat.source_type_id(trimmed_transform_text(first.typ_text()))
	for {
		if flat.source_type_shape(first_type).has(.mutable_type) {
			first_type = flat.source_type_id(trimmed_transform_text(flat.source_type_name(first_type)[4..]))
		} else if flat.source_type_shape(first_type).has(.shared_type) {
			first_type = flat.source_type_id(trimmed_transform_text(flat.source_type_name(first_type)[7..]))
		} else if flat.source_type_shape(first_type).has(.atomic_type) {
			first_type = flat.source_type_id(trimmed_transform_text(flat.source_type_name(first_type)[7..]))
		} else if flat.source_type_shape(first_type).has(.pointer) {
			first_type = flat.source_type_id(trimmed_transform_text(flat.source_type_name(first_type)[1..]))
		} else {
			break
		}
	}
	method_name := generic_fn_decl_base_value(node.value)
	clean_first := flat.source_type_name(t.normalize_type_alias(first_type))
	return receiver_param_matches_method_name(clean_first, method_name)
		|| receiver_param_matches_method_name(flat.source_type_name(first_type), method_name)
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
			t.collect_generic_param_names_from_type(child.typ_text(), decl.module, mut receiver_params)
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
	if node.value.contains('.') {
		for name in t.generic_receiver_param_names(GenericFnDecl{
			node:   node
			module: module_name
		}) {
			if name !in names {
				names << name
			}
		}
	}
	t.collect_generic_param_names_from_type(node.value, module_name, mut names)
	t.collect_generic_param_names_from_type(node.typ_text(), module_name, mut names)
	for i in 0 .. node.children_count {
		child := t.a.child_node(&node, i)
		if child.kind != .param {
			if t.prefix_param_scan {
				break
			}
			continue
		}
		t.collect_generic_param_names_from_type(child.typ_text(), module_name, mut names)
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
	t.collect_generic_param_names_from_type(node.typ_text(), module_name, mut names)
	t.collect_generic_param_names_from_type(node.value, module_name, mut names)
	for i in 0 .. node.children_count {
		t.collect_generic_param_names_from_node(t.a.child(&node, i), module_name, mut names)
	}
}

fn (mut t Transformer) collect_generic_param_names_from_type(typ string, module_name string, mut names []string) {
	clean := trimmed_transform_text(typ)
	if clean.len == 0 {
		return
	}
	if clean.starts_with('&') {
		t.collect_generic_param_names_from_type(clean[1..], module_name, mut names)
		return
	}
	if clean.starts_with('mut ') {
		t.collect_generic_param_names_from_type(clean[4..], module_name, mut names)
		return
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		t.collect_generic_param_names_from_type(clean[1..], module_name, mut names)
		return
	}
	if clean.starts_with('...') {
		t.collect_generic_param_names_from_type(clean[3..], module_name, mut names)
		return
	}
	if clean.starts_with('[]') {
		t.collect_generic_param_names_from_type(clean[2..], module_name, mut names)
		return
	}
	if is_generic_fn_placeholder_name(clean) && !t.concrete_type_name_known(clean, module_name) {
		if clean !in names {
			names << clean
		}
		return
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			t.collect_generic_param_names_from_type(clean[4..bracket_end], module_name, mut names)
			t.collect_generic_param_names_from_type(clean[bracket_end + 1..], module_name, mut names)
		}
		return
	}
	_, args, ok := generic_app_parts(clean)
	if ok {
		for arg in args {
			t.collect_generic_param_names_from_type(arg, module_name, mut names)
		}
		return
	}
	bracket := clean.index_u8(`[`)
	if bracket > 0 {
		bracket_end := generic_matching_bracket(clean, bracket)
		if bracket_end > bracket && bracket_end < clean.len {
			for arg in split_generic_args(clean[bracket + 1..bracket_end]) {
				t.collect_generic_param_names_from_type(arg, module_name, mut names)
			}
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
	for arg_source_id in args_ids {
		arg := flat.source_type_name(arg_source_id)

		result << flat.source_type_id(normalize_generic_arg(arg, module_name))
	}
	return result.clone()
}

fn normalize_generic_arg(arg string, module_name string) string {
	clean := trimmed_transform_text(arg)
	if clean.len == 0 {
		return clean
	}
	if clean.starts_with('&') {
		return '&' + normalize_generic_arg(clean[1..], module_name)
	}
	if clean.starts_with('[]') {
		return '[]' + normalize_generic_arg(clean[2..], module_name)
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		return clean[..1] + normalize_generic_arg(clean[1..], module_name)
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := normalize_generic_arg(clean[4..bracket_end], module_name)
			val := normalize_generic_arg(clean[bracket_end + 1..], module_name)
			return 'map[${key}]${val}'
		}
	}
	if clean.contains('.') || module_name == '' || module_name == 'main'
		|| module_name == 'builtin' || types.is_builtin_type_name(clean) {
		return clean
	}
	return clean
}

fn substitute_generic_fixed_array_type_text(clean string, args_ids []u32, params []string) ?string {
	if clean.starts_with('[]') || clean.starts_with('map[') || clean.starts_with('[')
		|| !clean.ends_with(']') {
		return none
	}
	open := clean.last_index('[') or { return none }
	if open <= 0 {
		return none
	}
	len_text := trimmed_transform_text(clean[open + 1..clean.len - 1])
	if !typeof_display_is_fixed_array_len_text(len_text) {
		return none
	}
	elem_text := trimmed_transform_text(clean[..open])
	if elem_text.len == 0 {
		return none
	}
	elem_sub := substitute_generic_type_text_with_params_bytes(elem_text, args_ids, params)
	len_sub := substitute_generic_expr_text_with_params(len_text, args_ids, params)
	return '${elem_sub}[${len_sub}]'
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
	typ := flat.source_type_name(typ_id)
	substituted := substitute_generic_type_text_bytes(typ, args_ids)
	return if substituted == typ { typ_id } else { flat.source_type_id(substituted) }
}

fn substitute_generic_type_text_bytes(typ string, args_ids []u32) string {
	clean := trimmed_transform_text(typ)
	if clean.len == 0 || args_ids.len == 0 {
		return typ
	}
	if fixed := substitute_generic_fixed_array_type_text(clean, args_ids, []string{}) {
		return fixed
	}
	if is_generic_fn_placeholder_name(clean) {
		idx := generic_param_index(clean)
		if idx < args_ids.len {
			return flat.source_type_name(args_ids[idx])
		}
		return clean
	}
	if clean.starts_with('&') {
		return '&' + substitute_generic_type_text_bytes(clean[1..], args_ids)
	}
	if clean.starts_with('mut ') {
		return 'mut ' + substitute_generic_type_text_bytes(clean[4..], args_ids)
	}
	if clean.starts_with('?') {
		return '?' + substitute_generic_type_text_bytes(clean[1..], args_ids)
	}
	if clean.starts_with('!') {
		return '!' + substitute_generic_type_text_bytes(clean[1..], args_ids)
	}
	if clean.starts_with('...') {
		return '...' + substitute_generic_type_text_bytes(clean[3..], args_ids)
	}
	if clean.starts_with('shared ') {
		return 'shared ' + substitute_generic_type_text_bytes(clean[7..], args_ids)
	}
	if clean.starts_with('[]') {
		return '[]' + substitute_generic_type_text_bytes(clean[2..], args_ids)
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := substitute_generic_type_text_bytes(clean[4..bracket_end], args_ids)
			val := substitute_generic_type_text_bytes(clean[bracket_end + 1..], args_ids)
			return 'map[${key}]${val}'
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			len_text := substitute_generic_expr_text(clean[1..bracket_end], args_ids)
			elem_text := substitute_generic_type_text_bytes(clean[bracket_end + 1..], args_ids)
			return '[${len_text}]${elem_text}'
		}
	}
	if clean.starts_with('(') && clean.ends_with(')') && clean.contains(',') {
		mut parts := []string{}
		for part in split_generic_args(clean[1..clean.len - 1]) {
			parts << substitute_generic_type_text_bytes(part, args_ids)
		}
		return '(' + parts.join(', ') + ')'
	}
	base, nested_args, ok := generic_app_parts(clean)
	if ok {
		mut resolved_args := []string{}
		for arg in nested_args {
			resolved_args << substitute_generic_type_text_bytes(arg, args_ids)
		}
		return '${base}[${resolved_args.join(', ')}]'
	}
	return clean
}

fn substitute_generic_type_text_with_params(typ_id u32, args_ids []u32, params []string) u32 {
	typ := flat.source_type_name(typ_id)
	substituted := substitute_generic_type_text_with_params_bytes(typ, args_ids, params)
	return if substituted == typ { typ_id } else { flat.source_type_id(substituted) }
}

@[direct_array_access]
fn substitute_generic_type_text_with_params_bytes(typ string, args_ids []u32, params []string) string {
	clean := trimmed_transform_text(typ)
	if clean.len == 0 || args_ids.len == 0 {
		return typ
	}
	if params.len > 0 && !generic_text_contains_param(clean, params) {
		return clean
	}
	if fixed := substitute_generic_fixed_array_type_text(clean, args_ids, params) {
		return fixed
	}
	for i, param in params {
		if clean == param {
			if i < args_ids.len {
				return flat.source_type_name(args_ids[i])
			}
			return clean
		}
	}
	if params.len == 0 && is_generic_fn_placeholder_name(clean) {
		idx := generic_param_index(clean)
		if idx < args_ids.len {
			return flat.source_type_name(args_ids[idx])
		}
		return clean
	}
	if clean.starts_with('&') {
		return '&' + substitute_generic_type_text_with_params_bytes(clean[1..], args_ids, params)
	}
	if clean.starts_with('mut ') {
		return 'mut ' + substitute_generic_type_text_with_params_bytes(clean[4..], args_ids, params)
	}
	if clean.starts_with('?') {
		inner := substitute_generic_type_text_with_params_bytes(clean[1..], args_ids, params)
		return if inner.starts_with('?') { inner } else { '?' + inner }
	}
	if clean.starts_with('!') {
		inner := substitute_generic_type_text_with_params_bytes(clean[1..], args_ids, params)
		return if inner.starts_with('!') { inner } else { '!' + inner }
	}
	if clean.starts_with('...') {
		return '...' + substitute_generic_type_text_with_params_bytes(clean[3..], args_ids, params)
	}
	if clean.starts_with('shared ') {
		return 'shared ' + substitute_generic_type_text_with_params_bytes(clean[7..], args_ids, params)
	}
	// A `chan T` / `thread T` payload is a generic parameter too; substitute it so the clone
	// emits `chan Context` / `thread Context` rather than leaving the placeholder `chan T`,
	// which later resolves the unsubstituted `T` to a bogus `int` channel/thread element.
	if clean.starts_with('chan ') {
		return 'chan ' + substitute_generic_type_text_with_params_bytes(clean[5..], args_ids, params)
	}
	if clean.starts_with('thread ') {
		return 'thread ' + substitute_generic_type_text_with_params_bytes(clean[7..], args_ids, params)
	}
	if clean.starts_with('[]') {
		return '[]' + substitute_generic_type_text_with_params_bytes(clean[2..], args_ids, params)
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := substitute_generic_type_text_with_params_bytes(clean[4..bracket_end], args_ids, params)
			val := substitute_generic_type_text_with_params_bytes(clean[bracket_end + 1..], args_ids, params)
			return 'map[${key}]${val}'
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			len_text := substitute_generic_expr_text_with_params(clean[1..bracket_end], args_ids, params)
			elem_text := substitute_generic_type_text_with_params_bytes(clean[bracket_end + 1..], args_ids, params)
			return '[${len_text}]${elem_text}'
		}
	}
	if clean.starts_with('(') && clean.ends_with(')') && clean.contains(',') {
		mut parts := []string{}
		for part in split_generic_args(clean[1..clean.len - 1]) {
			parts << substitute_generic_type_text_with_params_bytes(part, args_ids, params)
		}
		return '(' + parts.join(', ') + ')'
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		// Substitute the generic params inside a function-type parameter so a specialized
		// method body emits e.g. `fn (string) int`, not the placeholder `fn (T) int` (which
		// cgen would otherwise render as `fn (int) int`).
		if sub := subst_generic_fn_type_text(clean, args_ids, params) {
			return sub
		}
	}
	base, nested_args, ok := generic_app_parts(clean)
	if ok {
		mut resolved_args := []string{}
		for arg in nested_args {
			resolved_args << substitute_generic_type_text_with_params_bytes(arg, args_ids, params)
		}
		return '${base}[${resolved_args.join(', ')}]'
	}
	return clean
}

// subst_generic_fn_type_text substitutes generic params inside a `fn (...) ...` type text,
// recursing into each parameter type and the return type. Returns none when the signature
// is malformed (unbalanced parens).
fn subst_generic_fn_type_text(clean string, args_ids []u32, params []string) ?string {
	params_start := clean.index_u8(`(`) + 1
	mut depth := 1
	mut params_end := params_start
	for params_end < clean.len {
		if clean[params_end] == `(` {
			depth++
		} else if clean[params_end] == `)` {
			depth--
			if depth == 0 {
				break
			}
		}
		params_end++
	}
	if params_end >= clean.len {
		return none
	}
	params_str := clean[params_start..params_end]
	mut fn_parts := []string{}
	if trimmed_transform_text(params_str).len > 0 {
		mut pdepth := 0
		mut start := 0
		for i := 0; i < params_str.len; i++ {
			c := params_str[i]
			if c == `(` || c == `[` {
				pdepth++
			} else if c == `)` || c == `]` {
				pdepth--
			} else if c == `,` && pdepth == 0 {
				fn_parts << subst_generic_fn_type_param_text(params_str[start..i], args_ids, params)
				start = i + 1
			}
		}
		fn_parts << subst_generic_fn_type_param_text(params_str[start..], args_ids, params)
	}
	ret_str := trimmed_transform_text(clean[params_end + 1..])
	if ret_str.len > 0 {
		return 'fn(${fn_parts.join(', ')}) ${substitute_generic_type_text_with_params_bytes(ret_str, args_ids, params)}'
	}
	return 'fn(${fn_parts.join(', ')})'
}

fn subst_generic_fn_type_param_text(param string, args_ids []u32, params []string) string {
	mut text := trimmed_transform_text(param)
	mut is_mut := false
	if text.starts_with('mut ') {
		is_mut = true
		text = trimmed_transform_text(text[4..])
	}
	space := generic_top_level_space_index(text)
	if space > 0 {
		head := trimmed_transform_text(text[..space])
		tail := trimmed_transform_text(text[space + 1..])
		if generic_fn_type_param_head_is_name(head, tail) {
			sub := substitute_generic_type_text_with_params_bytes(tail, args_ids, params)
			if is_mut && sub.len > 0 {
				return 'mut ${head} ${sub}'
			}
			return '${head} ${sub}'
		}
	}
	sub := substitute_generic_type_text_with_params_bytes(text, args_ids, params)
	if is_mut && sub.len > 0 {
		return 'mut ${sub}'
	}
	return sub
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

// subst_node_value is the param-aware counterpart of substitute_generic_node_value.
fn (mut t Transformer) subst_node_value(node flat.Node, args_ids []u32) string {
	match node.kind {
		.ident {
			if node.value.contains('[') {
				return substitute_generic_type_text_with_params_bytes(node.value, args_ids, t.active_generic_params)
			}
			if flat.source_type_shape(node.typ).has(.bracketed) {
				base, _, ok := generic_app_parts(node.typ_text())
				if ok && node.value == base.all_after_last('.') {
					return flat.source_type_name(t.subst_type(node.typ, args_ids))
				}
			}
			return node.value
		}
		.selector {
			if node.value.contains('[') {
				return substitute_generic_type_text_with_params_bytes(node.value, args_ids, t.active_generic_params)
			}
			if flat.source_type_shape(node.typ).has(.bracketed) {
				old_field_type_comparison_type_id := t.trim_pointer_type(node.typ)

				new_field_type_comparison_type_id := t.trim_pointer_type(t.subst_type(node.typ, args_ids))

				if old_field_type_comparison_type_id != new_field_type_comparison_type_id
					&& t.sum_field_name(old_field_type_comparison_type_id) == node.value {
					return t.sum_field_name(new_field_type_comparison_type_id)
				}
			}
			return node.value
		}
		.is_expr {
			// A `$for variant in T.variants` body still contains the loop variable
			// while the generic function is cloned. Qualifying that bare name as a
			// module type (`variant` -> `json2.variant_text()`) prevents the later comptime
			// unroll from replacing it with each concrete sum variant.
			if node.value in t.cloning_comptime_for_vars {
				return node.value
			}
			return flat.source_type_name(t.resolve_substituted_type_text(flat.source_type_id(substitute_generic_type_text_with_params_bytes(node.value, args_ids, t.active_generic_params))))
		}
		.array_init, .map_init, .struct_init, .assoc, .cast_expr, .as_expr, .offsetof_expr {
			substituted := substitute_generic_type_text_with_params_bytes(node.value, args_ids, t.active_generic_params)
			if node.kind == .cast_expr && substituted != node.value
				&& t.generic_type_text_contains_alias(flat.source_type_id(substituted), t.cur_module) {
				// Qualify source names before substitution, keeping the alias intact so
				// caller types can be locked before alias expansion changes their scope.
				source := flat.source_type_name(t.qualify_generic_alias_cast_type_text(flat.source_type_id(node.value)))
				return t.lock_colliding_main_substitution_type_text_bytes(source, substitute_generic_type_text_with_params_bytes(source, args_ids, t.active_generic_params), t.cur_module, t.active_generic_params)
			}
			return flat.source_type_name(t.resolve_substituted_type_text(flat.source_type_id(substituted)))
		}
		.sizeof_expr, .typeof_expr {
			substituted := substitute_generic_type_text_with_params_bytes(node.value, args_ids, t.active_generic_params)
			if substituted == node.value {
				// `sizeof(local)`/`typeof(local)` carry an expression name in
				// `value`; resolving unchanged text as a type would qualify the
				// local into a bogus module symbol.
				return node.value
			}
			return flat.source_type_name(t.resolve_substituted_type_text(flat.source_type_id(substituted)))
		}
		.call {
			return substitute_generic_type_text_with_params_bytes(node.value, args_ids, t.active_generic_params)
		}
		.type_decl, .field_decl, .param {
			return substitute_generic_type_text_with_params_bytes(node.value, args_ids, t.active_generic_params)
		}
		.comptime_if {
			return t.subst_comptime_type_condition(node.value, args_ids)
		}
		.sql_expr {
			return t.subst_sql_expr_value(node.value, args_ids)
		}
		else {
			return node.value
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

fn (t &Transformer) qualify_generic_alias_cast_type_text(typ_id u32) u32 {
	typ := flat.source_type_name(typ_id)
	resolved := t.qualify_generic_alias_cast_type_text_bytes(typ)
	return if resolved == typ { typ_id } else { flat.source_type_id(resolved) }
}

fn (t &Transformer) qualify_generic_alias_cast_type_text_bytes(typ string) string {
	clean := trimmed_transform_text(typ)
	if clean.len == 0 || clean in t.active_generic_params || isnil(t.tc) {
		return clean
	}
	for prefix in ['mut ', 'shared ', 'atomic ', '...', '[]', '?', '!', '&', 'chan ', 'thread '] {
		if clean.starts_with(prefix) {
			return prefix + t.qualify_generic_alias_cast_type_text_bytes(clean[prefix.len..])
		}
	}
	if clean.starts_with('map[') {
		end := generic_matching_bracket(clean, 3)
		if end < clean.len - 1 {
			key := t.qualify_generic_alias_cast_type_text_bytes(clean[4..end])
			value := t.qualify_generic_alias_cast_type_text_bytes(clean[end + 1..])
			return 'map[${key}]${value}'
		}
	}
	if clean.starts_with('[') {
		end := generic_matching_bracket(clean, 0)
		if end > 1 && end < clean.len - 1 {
			return clean[..end + 1] + t.qualify_generic_alias_cast_type_text_bytes(clean[end + 1..])
		}
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		if params, ret := fn_type_text_parts(clean) {
			mut qualified_params := []string{cap: params.len}
			for param in params {
				qualified_params << t.qualify_generic_alias_cast_type_text_bytes(generic_fn_type_param_mode_payload(param))
			}
			qualified_ret := t.qualify_generic_alias_cast_type_text_bytes(ret)
			return if qualified_ret.len > 0 {
				'fn (${qualified_params.join(', ')}) ${qualified_ret}'
			} else {
				'fn (${qualified_params.join(', ')})'
			}
		}
	}
	if clean.starts_with('(') && clean.ends_with(')') && clean.contains(',') {
		mut qualified_parts := []string{}
		for part in split_generic_args(clean[1..clean.len - 1]) {
			qualified_parts << t.qualify_generic_alias_cast_type_text_bytes(part)
		}
		return '(' + qualified_parts.join(', ') + ')'
	}
	base, args, is_app := generic_app_parts(clean)
	if is_app {
		qualified_base := t.qualify_generic_alias_cast_type_text_bytes(base)
		mut qualified_args := []string{cap: args.len}
		for arg in args {
			qualified_args << t.qualify_generic_alias_cast_type_text_bytes(arg)
		}
		return '${qualified_base}[${qualified_args.join(', ')}]'
	}
	if imported_source_id := t.resolve_imported_type_name(flat.source_type_id(clean)) {
		imported := flat.source_type_name(imported_source_id)

		return imported
	}
	if selective := t.selective_signature_type_symbol(t.cur_file, clean) {
		return selective
	}
	if !clean.contains('.') && t.generic_arg_module_owns_type(clean, t.cur_module) {
		return '${t.cur_module}.${clean}'
	}
	return clean
}

fn (t &Transformer) resolve_substituted_type_text(typ_id u32) u32 {
	typ := flat.source_type_name(typ_id)
	resolved := t.resolve_substituted_type_text_bytes(typ)
	return if resolved == typ { typ_id } else { flat.source_type_id(resolved) }
}

fn (t &Transformer) resolve_substituted_type_text_bytes(typ string) string {
	clean := trimmed_transform_text(typ)
	if clean.len == 0 || isnil(t.tc) {
		return typ
	}
	if clean.starts_with('shared ') {
		return 'shared ' + t.resolve_substituted_type_text_bytes(clean[7..])
	}
	if clean.starts_with('atomic ') {
		return 'atomic ' + t.resolve_substituted_type_text_bytes(clean[7..])
	}
	if clean.starts_with('main.') && !t.ident_is_import_alias('main') {
		bare := clean['main.'.len..]
		if !bare.contains('.') && t.type_name_is_declared(bare) {
			return clean
		}
	}
	if t.substituted_type_belongs_to_main_generic(flat.source_type_id(clean)) {
		return clean
	}
	for prefix in ['mut ', '...', '[]', '?', '!', '&'] {
		if clean.starts_with(prefix) {
			return prefix + t.resolve_substituted_type_text_bytes(clean[prefix.len..])
		}
	}
	// Decompose map and fixed-array composites and resolve each component independently, so a
	// nested program (main) type is preserved bare (via the belongs-to-main check on the
	// recursion) instead of being rebased into the current module; the caller then locks the
	// bare nested name to an explicit `main.` spelling. Handled regardless of an UNRELATED
	// qualified component elsewhere (`map[other.Key]Context`, `[N]other.Box[Context]`): a `.` in
	// the key/base must not stop the bare value/element from being kept bare, or
	// parse_resolution_type rebases it into the callee module before the lock runs. Each
	// qualified component still resolves correctly on its own through the recursion, mirroring
	// the chan/thread/fn/generic_app handling below.
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len - 1 {
			key := t.resolve_substituted_type_text_bytes(clean[4..bracket_end])
			value := t.resolve_substituted_type_text_bytes(clean[bracket_end + 1..])
			return 'map[${key}]${value}'
		}
	}
	if clean.starts_with('[') && !clean.starts_with('[]') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end > 1 && bracket_end < clean.len - 1 {
			return clean[..bracket_end + 1] + t.resolve_substituted_type_text_bytes(clean[bracket_end + 1..])
		}
	}
	// `chan T` / `thread T` carry a single payload type; a bare program type there
	// (`chan Context` where the callee module also declares `Context`) must be resolved
	// independently and kept bare so the caller's lock can pin it to `main.`, instead of
	// parse_resolution_type rebasing the payload into the current module. Handled regardless
	// of a `.` elsewhere (`chan other.Box[Context]`), mirroring how the type checker qualifies
	// chan/thread payloads.
	if clean.starts_with('chan ') {
		return 'chan ' + t.resolve_substituted_type_text_bytes(clean[5..])
	}
	if clean.starts_with('thread ') {
		return 'thread ' + t.resolve_substituted_type_text_bytes(clean[7..])
	}
	// A function type resolves its parameter and return types independently (keeping a bare
	// program type bare) so the caller's lock can pin a nested `Context` to `main.` instead
	// of parse_resolution_type rebasing the whole `fn (Context)` into the current module.
	// Handled regardless of a `.` elsewhere in the signature so mixed types stay correct.
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		if params, ret := fn_type_text_parts(clean) {
			mut resolved_params := []string{cap: params.len}
			for param in params {
				resolved_params << t.resolve_substituted_type_text_bytes(param)
			}
			resolved_ret := t.resolve_substituted_type_text_bytes(ret)
			return if resolved_ret.len > 0 {
				'fn (${resolved_params.join(', ')}) ${resolved_ret}'
			} else {
				'fn (${resolved_params.join(', ')})'
			}
		}
	}
	base, args, is_generic_app := generic_app_parts(clean)
	if is_generic_app && args.len > 0 && !base.contains('.') && t.cur_module.len > 0
		&& t.cur_module !in ['main', 'builtin'] {
		qualified_base := '${t.cur_module}.${base}'
		if qualified_base in t.tc.struct_generic_params || qualified_base in t.tc.sum_generic_params {
			mut resolved_args := []string{cap: args.len}
			for arg in args {
				resolved_args << t.resolve_substituted_type_text_bytes(arg)
			}
			return '${qualified_base}[${resolved_args.join(', ')}]'
		}
	}
	parsed := t.tc.parse_resolution_type(flat.source_type_id(clean))
	if parsed is types.Unknown {
		return typ
	}
	resolved := parsed.name()
	if is_generic_app && args.len > 0 {
		resolved_base := if resolved.contains('[') { resolved.all_before('[') } else { resolved }
		// Re-resolve the arguments (keeping a bare program type bare) not only when the
		// base itself was requalified, but also when the base is already qualified
		// (`other.Box[Context]`): otherwise parse_resolution_type rebases the nested bare
		// `Context` into the current module before the caller's lock can pin it to `main.`.
		if resolved_base.len > 0 && (resolved_base != base || base.contains('.')) {
			mut resolved_args := []string{cap: args.len}
			for arg in args {
				resolved_args << t.resolve_substituted_type_text_bytes(arg)
			}
			return '${resolved_base}[${resolved_args.join(', ')}]'
		}
	}
	return resolved
}

fn (t &Transformer) lock_colliding_main_substitution_type_text(original_id u32, substituted_id u32, module_name string, generic_params []string) u32 {
	substituted := flat.source_type_name(substituted_id)
	locked := t.lock_colliding_main_substitution_type_text_bytes(flat.source_type_name(original_id), substituted, module_name, generic_params)
	return if locked == substituted { substituted_id } else { flat.source_type_id(locked) }
}

fn (t &Transformer) lock_colliding_main_substitution_type_text_bytes(original string, substituted string, module_name string, generic_params []string) string {
	source := trimmed_transform_text(original)
	concrete := trimmed_transform_text(substituted)
	if source == concrete && !generic_text_contains_param(source, generic_params) {
		return concrete
	}
	if source in generic_params {
		return t.lock_colliding_main_generic_type_text_bytes(concrete, module_name)
	}
	if source.starts_with('mut ') && concrete.starts_with('&') {
		return '&' + t.lock_colliding_main_substitution_type_text_bytes(source[4..], concrete[1..], module_name, generic_params)
	}
	for prefix in ['mut ', 'shared ', 'atomic ', '...', '[]', '?', '!', '&'] {
		if source.starts_with(prefix) && concrete.starts_with(prefix) {
			return prefix + t.lock_colliding_main_substitution_type_text_bytes(source[prefix.len..], concrete[prefix.len..], module_name, generic_params)
		}
	}
	for prefix in ['chan ', 'thread '] {
		if source.starts_with(prefix) && concrete.starts_with(prefix) {
			return prefix + t.lock_colliding_main_substitution_type_text_bytes(source[prefix.len..], concrete[prefix.len..], module_name, generic_params)
		}
	}
	if source.starts_with('fn(') || source.starts_with('fn (') {
		if concrete.starts_with('fn(') || concrete.starts_with('fn (') {
			if source_params, source_ret := fn_type_text_parts(source) {
				if concrete_params, concrete_ret := fn_type_text_parts(concrete) {
					if source_params.len == concrete_params.len {
						mut locked_params := []string{cap: concrete_params.len}
						for i, concrete_param in concrete_params {
							source_param := trimmed_transform_text(source_params[i])
							source_is_mut := source_param.starts_with('mut ')
							source_payload := generic_fn_type_param_payload(source_param)
							// Both payloads are already declared spellings: the mut
							// prefix is stripped separately and readded below, so the
							// leading & of a mut T instantiated with &Dog belongs
							// to T and must survive.
							concrete_payload := generic_fn_type_param_payload(concrete_param)
							locked := t.lock_colliding_main_substitution_type_text_bytes(source_payload, concrete_payload, module_name, generic_params)
							locked_params << if source_is_mut { 'mut ${locked}' } else { locked }
						}
						locked_ret := t.lock_colliding_main_substitution_type_text_bytes(source_ret, concrete_ret, module_name, generic_params)
						return if locked_ret.len > 0 {
							'fn (${locked_params.join(', ')}) ${locked_ret}'
						} else {
							'fn (${locked_params.join(', ')})'
						}
					}
				}
			}
		}
	}
	if source.starts_with('(') && source.ends_with(')') && source.contains(',')
		&& concrete.starts_with('(') && concrete.ends_with(')') && concrete.contains(',') {
		source_parts := split_generic_args(source[1..source.len - 1])
		concrete_parts := split_generic_args(concrete[1..concrete.len - 1])
		if source_parts.len == concrete_parts.len {
			mut locked_parts := []string{cap: concrete_parts.len}
			for i, concrete_part in concrete_parts {
				locked_parts << t.lock_colliding_main_substitution_type_text_bytes(source_parts[i], concrete_part, module_name, generic_params)
			}
			return '(' + locked_parts.join(', ') + ')'
		}
	}
	if source.starts_with('map[') && concrete.starts_with('map[') {
		source_end := generic_matching_bracket(source, 3)
		concrete_end := generic_matching_bracket(concrete, 3)
		if source_end < source.len - 1 && concrete_end < concrete.len - 1 {
			key := t.lock_colliding_main_substitution_type_text_bytes(source[4..source_end], concrete[4..concrete_end], module_name, generic_params)
			value := t.lock_colliding_main_substitution_type_text_bytes(source[source_end + 1..], concrete[concrete_end + 1..], module_name, generic_params)
			return 'map[${key}]${value}'
		}
	}
	if source.starts_with('[') && concrete.starts_with('[') {
		source_end := generic_matching_bracket(source, 0)
		concrete_end := generic_matching_bracket(concrete, 0)
		if source_end > 1 && source_end < source.len - 1 && concrete_end > 1
			&& concrete_end < concrete.len - 1 {
			elem := t.lock_colliding_main_substitution_type_text_bytes(source[source_end + 1..], concrete[concrete_end + 1..], module_name, generic_params)
			return concrete[..concrete_end + 1] + elem
		}
	}
	source_base, source_args, source_is_app := generic_app_parts(source)
	concrete_base, concrete_args, concrete_is_app := generic_app_parts(concrete)
	if source_is_app && concrete_is_app && source_base == concrete_base
		&& source_args.len == concrete_args.len {
		mut locked_args := []string{cap: concrete_args.len}
		mut args_locked := false
		for i, concrete_arg in concrete_args {
			locked := t.lock_colliding_main_substitution_type_text_bytes(source_args[i], concrete_arg, module_name, generic_params)
			if locked != trimmed_transform_text(concrete_arg) {
				args_locked = true
			}
			locked_args << locked
		}
		// Once an argument is locked (`main.Item`), the text is a qualified semantic
		// spelling, which codegen parses without the decl module context. A bare
		// decl-module base (`List`) would then lose its module (`List[Item]` instead of
		// `lib.List[Item]`), so qualify it too.
		base := if args_locked {
			t.qualify_decl_module_generic_base(concrete_base, module_name)
		} else {
			concrete_base
		}
		return '${base}[${locked_args.join(', ')}]'
	}
	return t.lock_colliding_main_generic_type_text_bytes(concrete, module_name)
}

// qualify_decl_module_generic_base qualifies a bare generic base declared in
// `module_name` (e.g. `List` -> `lib.List`); other bases are returned unchanged.
fn (t &Transformer) qualify_decl_module_generic_base(base string, module_name string) string {
	clean := trimmed_transform_text(base)
	if clean.contains('.') || module_name in ['', 'main', 'builtin'] {
		return clean
	}
	qname := '${module_name}.${clean}'
	if qname in t.structs || t.sum_types.contains(qname) || (!isnil(t.tc)
		&& (qname in t.tc.struct_generic_params || qname in t.tc.sum_generic_params
			|| qname in t.tc.interface_names || qname in t.tc.interface_generic_params
			|| t.tc.type_aliases.contains(qname) || qname in t.tc.type_alias_generic_params)) {
		return qname
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
	typ := flat.source_type_name(typ_id)
	locked := t.lock_colliding_main_generic_type_text_bytes(typ, module_name)
	return if locked == typ { typ_id } else { flat.source_type_id(locked) }
}

fn (t &Transformer) lock_colliding_main_generic_type_text_bytes(typ string, module_name string) string {
	clean := trimmed_transform_text(typ)
	if module_name == '' || module_name == 'main' {
		return clean
	}
	for prefix in ['mut ', 'shared ', 'atomic ', '...', '[]', '?', '!', '&'] {
		if clean.starts_with(prefix) {
			return prefix + t.lock_colliding_main_generic_type_text_bytes(clean[prefix.len..], module_name)
		}
	}
	// A function type hides its parameter and return types from the scalar checks below,
	// so a bare main type nested in `fn (T)` / `fn (T) R` (e.g. the callback of
	// `fn wrap[T](cb fn (T))` specialized with a colliding `Context`) would otherwise be
	// parsed in the callee module as that module's type. Recurse through each parameter and
	// the return type, rebuilding only when a component actually changed.
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		if params, ret := fn_type_text_parts(clean) {
			mut changed := false
			mut locked_params := []string{cap: params.len}
			for param in params {
				locked := t.lock_colliding_main_generic_type_text_bytes(param, module_name)
				if locked != trimmed_transform_text(param) {
					changed = true
				}
				locked_params << locked
			}
			locked_ret := t.lock_colliding_main_generic_type_text_bytes(ret, module_name)
			if locked_ret != ret {
				changed = true
			}
			if changed {
				return if locked_ret.len > 0 {
					'fn (${locked_params.join(', ')}) ${locked_ret}'
				} else {
					'fn (${locked_params.join(', ')})'
				}
			}
			return clean
		}
	}
	// `chan T` / `thread T` hide a bare program payload just like a composite: a colliding
	// `Context` in `chan Context` / `thread Context` must be locked to `main.`, or the callee
	// module rebases the payload to its own type. Recurse through the payload; an unchanged
	// payload rebuilds to a byte-identical spelling.
	if clean.starts_with('chan ') {
		return 'chan ' + t.lock_colliding_main_generic_type_text_bytes(clean[5..], module_name)
	}
	if clean.starts_with('thread ') {
		return 'thread ' + t.lock_colliding_main_generic_type_text_bytes(clean[7..], module_name)
	}
	if clean.starts_with('(') && clean.ends_with(')') && clean.contains(',') {
		mut locked_parts := []string{}
		for part in split_generic_args(clean[1..clean.len - 1]) {
			locked_parts << t.lock_colliding_main_generic_type_text_bytes(part, module_name)
		}
		return '(' + locked_parts.join(', ') + ')'
	}
	// A composite must lock its component types even behind a qualified base: a bare main
	// `Context` nested in `map[string]Context`, `[3]Context`, `Box[Context]` or
	// `other.Box[Context]` is never a struct/sum/enum key on its own, so without descending
	// the collision goes unlocked and the callee module rebases the nested name to its own
	// type. Decompose these before the broad qualified-name return below; an unchanged
	// component (e.g. the `user.LocalWriter` in `Box[user.LocalWriter]`) is rebuilt to a
	// byte-identical spelling, so the specialization's declared/referenced names stay in sync.
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len - 1 {
			key := t.lock_colliding_main_generic_type_text_bytes(clean[4..bracket_end], module_name)
			value := t.lock_colliding_main_generic_type_text_bytes(clean[bracket_end + 1..], module_name)
			return 'map[${key}]${value}'
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end > 1 && bracket_end < clean.len - 1 {
			return clean[..bracket_end + 1] + t.lock_colliding_main_generic_type_text_bytes(clean[bracket_end + 1..], module_name)
		}
	}
	// Lock the nested arguments and — when it is a bare program-module generic — the base too,
	// rebuilding the text only when something actually changed so a non-colliding application
	// keeps its exact original spelling.
	base, nested_args, is_generic_app := generic_app_parts(clean)
	if is_generic_app {
		mut changed := false
		mut locked_args := []string{cap: nested_args.len}
		for nested_arg in nested_args {
			locked := t.lock_colliding_main_generic_type_text_bytes(nested_arg, module_name)
			if locked != trimmed_transform_text(nested_arg) {
				changed = true
			}
			locked_args << locked
		}
		// The base may itself be a bare main-module generic (`Box[string]` where `main.Box`
		// exists) that collides with a same-named generic in the callee module (`mid.Box`).
		// Without locking it, the cloned signature parsed in the callee binds `Box` to its own
		// homonym, giving the wrong ABI/fields. Recurse so the scalar main-type lock at this
		// function's tail applies; an already-qualified base (`other.Box`) is returned verbatim.
		locked_base := t.lock_colliding_main_generic_type_text_bytes(base, module_name)
		if locked_base != trimmed_transform_text(base) {
			changed = true
		}
		if changed {
			return '${locked_base}[${locked_args.join(', ')}]'
		}
		return clean
	}
	// A remaining spelling that still carries a module qualifier is a simple qualified
	// type with no lockable bare component (e.g. `veb.Context`); return it verbatim.
	if clean.contains('.') {
		return clean
	}
	if is_generic_fn_placeholder_name(clean) {
		if info := t.structs[clean] {
			if info.module in ['', 'main'] {
				return 'main.' + clean
			}
		}
	}
	// An active specialization type has caller provenance: even when both main and the
	// callee declare this generic base, this spelling is the caller's main type. Check it
	// before filtering to concrete declaration kinds so caller-owned aliases keep their
	// identity too. Source-owned generic bases are preserved by
	// lock_colliding_main_substitution_type_text without reaching this branch.
	if t.active_specialization_main_types[flat.source_type_id(clean)] {
		qname := '${module_name}.${clean}'
		mut alias_target_needs_lock := false
		if !isnil(t.tc) {
			if alias_target_source_id := t.tc.type_aliases.type_id(clean) {
				alias_target := flat.source_type_name(alias_target_source_id)

				alias_target_needs_lock = alias_target != clean
					&& t.lock_colliding_main_generic_type_text_bytes(alias_target, module_name) != alias_target
			}
		}
		if qname in t.structs || t.sum_types.contains(qname) || qname in t.enum_types
			|| (!isnil(t.tc) && (qname in t.tc.struct_generic_params
				|| qname in t.tc.sum_generic_params))
			|| t.type_short_name_has_non_main_owner(clean) || alias_target_needs_lock {
			return 'main.' + clean
		}
	}
	if t.main_alias_needs_lock(clean, module_name) {
		return 'main.' + clean
	}
	if !(clean in t.structs || t.sum_types.contains(clean) || clean in t.enum_types) {
		return clean
	}
	if info := t.structs[clean] {
		if info.module in ['', 'main'] {
			if !isnil(t.tc) && clean in t.tc.struct_generic_params
				&& '${module_name}.${clean}' in t.tc.struct_generic_params {
				return clean
			}
			qname := '${module_name}.${clean}'
			if qname in t.structs || t.sum_types.contains(qname) || qname in t.enum_types
				|| t.type_short_name_has_non_main_owner(clean) {
				return 'main.' + clean
			}
			return clean
		}
	}
	// A main-module type substituted into an imported specialization needs an
	// explicit lock even when the callee has no homonym. Another imported module
	// can still make the bare short name ambiguous during the later C type pass.
	if !isnil(t.tc) && clean in t.structs {
		decl_module := t.tc.struct_modules[clean] or { '' }
		if decl_module !in ['', 'main'] {
			return clean
		}
		qname := '${module_name}.${clean}'
		if decl_module.len == 0 && clean in t.tc.struct_generic_params
			&& qname in t.tc.struct_generic_params {
			return clean
		}
	}
	qname := '${module_name}.${clean}'
	if qname in t.structs || t.sum_types.contains(qname) || qname in t.enum_types {
		return 'main.' + clean
	}
	return clean
}

// main_alias_needs_lock reports whether a bare program alias must be spelled `main.X`
// in `module_name`: its target is resolved where the alias is parsed, so a `Cells`
// alias of `[]Cell` would otherwise become `[]module_name.Cell` there.
fn (t &Transformer) main_alias_needs_lock(name string, module_name string) bool {
	if isnil(t.tc) {
		return false
	}
	alias_module := t.tc.type_alias_modules[name] or { return false }
	if alias_module !in ['', 'main'] {
		return false
	}
	target := flat.source_type_name(t.tc.type_aliases.type_id(name) or { return false })
	qname := '${module_name}.${name}'
	return t.tc.type_aliases.contains(qname) || qname in t.structs || t.sum_types.contains(qname)
		|| qname in t.enum_types || (target != name
		&& t.lock_colliding_main_generic_type_text_bytes(target, module_name) != target)
}

fn (t &Transformer) type_short_name_has_non_main_owner(name string) bool {
	if t.non_main_type_index_ready {
		return t.non_main_type_short_names[name]
	}
	// Small fixture transformers can call this helper before prepare_with_pre_scans().
	// Production transforms use the prebuilt O(1) index above.
	for candidate, info in t.structs {
		owner := if info.module.len > 0 { info.module } else { candidate.all_before_last('.') }
		if candidate.contains('.') && candidate.all_after_last('.') == name
			&& owner !in ['', 'main', 'builtin'] {
			return true
		}
	}
	for candidate, _ in t.sum_types.ids {
		if candidate.contains('.') && candidate.all_after_last('.') == name
			&& candidate.all_before_last('.') !in ['', 'main', 'builtin'] {
			return true
		}
	}
	for candidate, _ in t.enum_types {
		if candidate.contains('.') && candidate.all_after_last('.') == name
			&& candidate.all_before_last('.') !in ['', 'main', 'builtin'] {
			return true
		}
	}
	return false
}

fn (t &Transformer) substituted_type_belongs_to_main_generic(typ_id u32) bool {
	typ := flat.source_type_name(typ_id)
	clean := trimmed_transform_text(typ)
	components := generic_type_container_components(flat.source_type_id(clean))
	if components.len > 0 {
		for component in components {
			if t.substituted_type_belongs_to_main_generic(component) {
				return true
			}
		}
		return false
	}
	base, args, ok := generic_app_parts(clean)
	local_base := if ok { base } else { clean }
	if !isnil(t.tc) && !local_base.contains('.') && t.cur_module.len > 0
		&& t.cur_module !in ['main', 'builtin'] && !t.active_specialization_main_types[flat.source_type_id(local_base)] {
		qname := '${t.cur_module}.${local_base}'
		if qname in t.structs || t.sum_types.contains(qname) || qname in t.enum_types
			|| qname in t.tc.struct_generic_params || qname in t.tc.sum_generic_params {
			// A bare type written in an imported declaration belongs to that
			// declaration's module unless it is an active main-module
			// specialization argument. In particular, an alias target such as
			// `a.Inner[T]` must not be rebound to a colliding `main.Inner[T]`.
			return false
		}
	}
	if ok && base.contains('.') {
		for arg in args {
			if t.substituted_type_belongs_to_main_generic(flat.source_type_id(arg))
				&& t.current_specialization_has_generic_arg(flat.source_type_id(arg)) {
				return true
			}
		}
	}
	if !clean.contains('.') && (clean in t.structs || t.sum_types.contains(clean) || clean in t.enum_types) {
		return true
	}
	if !ok || base.contains('.') || isnil(t.tc) {
		return false
	}
	if base !in t.tc.struct_generic_params {
		return false
	}
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
		concrete := flat.source_type_name(t.resolve_substituted_type_text(t.subst_type(raw_type_source_id, args_ids)))
		mut suffix := ''
		mut replacement := ''
		if result[close + 1..].starts_with('.indirections') {
			suffix = '.indirections'
			replacement = generic_type_indirections(flat.source_type_id(concrete)).str()
		} else if result[close + 1..].starts_with('.idx') {
			suffix = '.idx'
			replacement = t.type_index_for_type_name(flat.source_type_id(concrete)).str()
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
	arg_type := flat.source_type_name(arg_type_id)
	mut actual := trimmed_transform_text(arg_type)
	param := trimmed_transform_text(flat.source_type_name(param_type_id))
	for wrapper in ['shared ', 'atomic '] {
		if actual.starts_with(wrapper) && !param.starts_with(wrapper) {
			actual = trimmed_transform_text(actual[wrapper.len..])
		}
	}
	return if actual == arg_type { arg_type_id } else { flat.source_type_id(actual) }
}

fn generic_inference_param_type(param &flat.Node) u32 {
	mut clean := trimmed_transform_text(param.typ_text())
	for clean.starts_with('shared ') || clean.starts_with('atomic ') {
		clean = trimmed_transform_text(clean[7..])
	}
	// `mut value T` is represented as `&T` for storage, so that synthetic
	// pointer must not become part of T. An explicitly declared `value &T`
	// keeps its pointer: inferring it from an `&int` argument must bind T to
	// `int`, not `&int`.
	if param.is_mut && param.op != .amp && clean.starts_with('&') {
		return flat.source_type_id(clean[1..])
	}
	if clean.starts_with('mut ') {
		return flat.source_type_id(trimmed_transform_text(clean[4..]))
	}
	return if clean == param.typ_text() { param.typ } else { flat.source_type_id(clean) }
}

fn (t &Transformer) generic_inference_expected_type(typ_id u32) u32 {
	if source_source_id := t.generic_specialized_source_type_name(typ_id) {
		return source_source_id
	}
	return typ_id
}

fn generic_type_indirections(typ_id u32) int {
	mut clean := trimmed_transform_text(flat.source_type_name(typ_id))
	mut count := 0
	// An option or result wrapper is not itself a level of indirection, but it does
	// not hide the pointer inside it either: `?&Tag` reports one, the same as the
	// `$for field` metadata does. Without this a `$if T.indirections != 0` branch is
	// never taken for an option-of-pointer generic argument.
	for clean.starts_with('?') || clean.starts_with('!') {
		clean = trimmed_transform_text(clean[1..])
	}
	if clean.starts_with('shared ') {
		count++
		clean = trimmed_transform_text(clean[7..])
	}
	for clean.starts_with('&') {
		count++
		clean = trimmed_transform_text(clean[1..])
	}
	return count
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
		concrete := flat.source_type_name(t.resolve_substituted_type_text(t.subst_type(flat.source_type_id(inner), args_ids)))
		if layout := t.comptime_type_layout(flat.source_type_id(concrete), []u32{}) {
			return layout.size.str()
		}
	}
	if reflected_type, reflected_member := generic_comptime_typeof_operand(clean) {
		substituted := flat.source_type_name(t.resolve_substituted_type_text(t.subst_type(flat.source_type_id(reflected_type), args_ids)))
		return match reflected_member {
			'idx' { t.comptime_field_type_id(flat.source_type_id(substituted), t.cur_module).str() }
			'unaliased_typ' {
				flat.source_type_name(t.comptime_typeof_unaliased_type(flat.source_type_id(substituted)))
			}
			'typ' { substituted + '.typ' }
			else { substituted }
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
		var_type := flat.source_type_name(var_type_source_id)
		if var_type_source_id != 0 {
			value_type := if t.mut_param_values[clean] {
				var_type.trim_string_left('&')
			} else {
				var_type
			}
			return flat.source_type_name(t.resolve_substituted_type_text(t.subst_type(flat.source_type_id(value_type), args_ids)))
		}
		if comptime_condition_is_unresolved_value_ident(clean) {
			return clean
		}
	}
	payload_suffix := '.unaliased_typ.payload_type'
	if clean.ends_with(payload_suffix) {
		base := clean[..clean.len - payload_suffix.len]
		substituted :=
			flat.source_type_name(t.comptime_normalize_type_alias_chain(t.resolve_substituted_type_text(t.subst_type(flat.source_type_id(base), args_ids))))
		if substituted.starts_with('?') || substituted.starts_with('!') {
			payload := trimmed_transform_text(substituted[1..])
			return t.comptime_field_type_id(flat.source_type_id(payload), t.cur_module).str()
		}
		return '0'
	}
	if clean.ends_with('.unaliased_typ') {
		base := clean[..clean.len - '.unaliased_typ'.len]
		substituted := flat.source_type_name(t.resolve_substituted_type_text(t.subst_type(flat.source_type_id(base), args_ids)))
		return flat.source_type_name(t.comptime_typeof_unaliased_type(flat.source_type_id(substituted)))
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
			fields := t.tc.structs[typ.name()] or { return none }
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
	if value.contains('.') {
		receiver := value.all_before_last('.')
		method := value.all_after_last('.')
		base, _, ok := generic_app_parts(receiver)
		if ok {
			return '${base}[${generic_type_args_short(args_ids)}].${method}'
		}
		return '${receiver}[${generic_type_args_short(args_ids)}].${method}'
	}
	// Free-function specializations need the canonical module-qualified type
	// arguments in their identity. Using only the last type component made
	// `keep[a.tast.Value]` and `keep[b.tast.Value]` both become `keep_T_Value`,
	// so one body/prototype was reused with the other module's ABI type.
	return '${value}_T_${generic_type_full_suffixes(args_ids)}'
}

fn generic_type_args_short(args_ids []u32) string {
	mut parts := []string{cap: args_ids.len}
	for arg_source_id in args_ids {
		arg := flat.source_type_name(arg_source_id)

		parts << generic_type_arg_short(arg)
	}
	return parts.join(', ')
}

fn generic_type_arg_short(type_arg string) string {
	clean := trimmed_transform_text(type_arg)
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		if params, ret := fn_type_text_parts(clean) {
			mut normalized_params := []string{cap: params.len}
			for param in params {
				trimmed := trimmed_transform_text(param)
				mut payload := generic_fn_type_param_payload(trimmed)
				if trimmed.starts_with('mut ') && !payload.starts_with('&') {
					payload = '&${payload}'
				}
				normalized_params << payload
			}
			normalized := if ret.len > 0 {
				'fn (${normalized_params.join(', ')}) ${ret}'
			} else {
				'fn (${normalized_params.join(', ')})'
			}
			return sanitize_full_type_name_fragment(normalized)
		}
	}
	if clean.starts_with('[]') {
		return 'Array_${generic_type_arg_short(clean[2..])}'
	}
	if clean.starts_with('&') {
		return 'ptr_${generic_type_arg_short(clean[1..])}'
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len - 1 {
			key := generic_type_arg_short(clean[4..bracket_end])
			value := generic_type_arg_short(clean[bracket_end + 1..])
			return 'Map_${key}_${value}'
		}
	}
	if clean.starts_with('?') {
		return 'Option_${generic_type_arg_short(clean[1..])}'
	}
	if clean.starts_with('!') {
		return 'Result_${generic_type_arg_short(clean[1..])}'
	}
	if fixed := generic_fixed_array_type_arg_short(clean) {
		return fixed
	}
	// Function-type args (`fn (A, B) R`) and other compound types cannot appear
	// verbatim in a C identifier and must not be naively shortened by the last
	// `.` (which would truncate `fn(...) mod.R` to `R)`). Keep module qualifiers
	// inside the sanitized fragment so same-named callback parameter types from
	// different modules cannot share one specialization.
	if clean.contains('(') || clean.contains(' ') {
		return sanitize_full_type_name_fragment(clean)
	}
	base, args, ok := generic_app_parts(clean)
	if ok {
		mut parts := [generic_type_arg_short(base)]
		for arg in args {
			parts << generic_type_arg_short(arg)
		}
		return parts.join('_')
	}
	if clean.contains('.') {
		return clean
	}
	return clean
}

fn generic_fixed_array_type_arg_short(type_arg string) ?string {
	clean := trimmed_transform_text(type_arg)
	if !clean.starts_with('[') {
		return none
	}
	close_idx := clean.index_u8(`]`)
	if close_idx <= 1 || close_idx + 1 >= clean.len {
		return none
	}
	len_text := trimmed_transform_text(clean[1..close_idx])
	elem_text := trimmed_transform_text(clean[close_idx + 1..])
	if len_text.len == 0 || elem_text.len == 0 {
		return none
	}
	elem := generic_type_arg_short(elem_text)
	return '${elem}_${len_text}'
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
		arg := flat.source_type_name(arg_source_id)

		parts << c_name(generic_type_arg_short(arg).replace('[]', 'Array_').replace('&', 'ptr_'))
	}
	return parts.join('_')
}

fn generic_type_full_suffixes(args_ids []u32) string {
	mut parts := []string{cap: args_ids.len}
	for arg_source_id in args_ids {
		arg := flat.source_type_name(arg_source_id)

		// Keep module qualification while giving composite types an explicit,
		// round-trippable shape. Raw `c_name(map[string]mod.Type)` concatenates
		// the key and value and cannot be decoded by a later transform stage.
		parts << c_name(generic_type_arg_short(arg))
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
	mut keys := []string{}
	if clean_base.contains('.') || module_name == '' || module_name == 'main'
		|| module_name == 'builtin' {
		keys << '${clean_base}[${recorded_args.join(', ')}]'
		keys << c_name('${clean_base}[${recorded_args.join(', ')}]')
		keys << '${clean_base}_${suffix}'
		keys << c_name('${clean_base}_${suffix}')
	}
	keys << '${qualified_base}[${recorded_args.join(', ')}]'
	keys << c_name('${qualified_base}[${recorded_args.join(', ')}]')
	keys << '${qualified_base}_${suffix}'
	keys << c_name('${qualified_base}_${suffix}')
	if qualified_base.contains('.') {
		base_module := qualified_base.all_before_last('.')
		short_base := qualified_base.all_after_last('.')
		keys << '${base_module}.${short_base}_${suffix}'
		keys << c_name('${base_module}.${short_base}_${suffix}')
	}
	for key in keys {
		if key.len > 0 && !t.has_recorded_generic_specialization_args(key) {
			t.generic_specialization_args.set_ids(key, recorded_args_source_ids)
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
			t.generic_specialization_args_log << name
		}
	}
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
	clean_id := t.trimmed_type_id(arg_id)
	clean_comparison_type_id_4 := clean_id
	clean := flat.source_type_name(clean_comparison_type_id_4)
	if clean_id == 0 {
		return clean_id
	}
	// `main.Type` is a temporary disambiguation lock used while cloning an imported
	// generic body. Generic specialization keys use the program module's normal
	// bare spelling; leaving the lock qualified here lets import resolution rebind
	// it to an unrelated same-named type.
	if !t.ident_is_import_alias('main') && clean.contains('main.') {
		unlocked := strip_main_type_locks(clean)
		if unlocked != clean {
			return t.canonical_generic_specialization_arg(flat.source_type_id(unlocked))
		}
	}
	// Inference inside an already-specialized body can observe the materialized
	// aggregate spelling (`Box_int`) instead of the source spelling (`Box[int]`).
	// Recover the recorded application before forming another specialization key;
	// otherwise both spellings clone the same C symbol with different type identity.
	if source_source_id := t.generic_specialized_source_type_name(clean_id) {
		source_comparison_type_id := source_source_id

		if source_comparison_type_id != clean_comparison_type_id_4 {
			return t.canonical_generic_specialization_arg(source_source_id)
		}
	}
	if clean.starts_with('map_') {
		if source_source_id := t.source_composite_type_from_c_name(clean) {
			source_source_id_2 := source_source_id

			return t.canonical_generic_specialization_arg(source_source_id_2)
		}
	}
	// Leading bracket forms still need length normalization or recovery of a
	// materialized generic application such as `[string]Box`.
	if !clean.starts_with('[') && t.substituted_type_belongs_to_main_generic(clean_comparison_type_id_4)
		&& t.current_specialization_has_generic_arg(clean_comparison_type_id_4) {
		return clean_id
	}
	if clean.starts_with('&') {
		return flat.source_type_id('&' + flat.source_type_name(t.canonical_generic_specialization_arg(flat.source_type_id(clean[1..]))))
	}
	if clean.starts_with('mut ') {
		return flat.source_type_id('mut ' + flat.source_type_name(t.canonical_generic_specialization_arg(flat.source_type_id(clean[4..]))))
	}
	if clean.starts_with('shared ') {
		return flat.source_type_id('shared ' + flat.source_type_name(t.canonical_generic_specialization_arg(flat.source_type_id(clean[7..]))))
	}
	if clean.starts_with('atomic ') {
		return flat.source_type_id('atomic ' + flat.source_type_name(t.canonical_generic_specialization_arg(flat.source_type_id(clean[7..]))))
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		return flat.source_type_id(clean[..1] + flat.source_type_name(t.canonical_generic_specialization_arg(flat.source_type_id(clean[1..]))))
	}
	if clean.starts_with('...') {
		return flat.source_type_id('...' + flat.source_type_name(t.canonical_generic_specialization_arg(flat.source_type_id(clean[3..]))))
	}
	if clean.starts_with('[]') {
		return flat.source_type_id('[]' + flat.source_type_name(t.canonical_generic_specialization_arg(flat.source_type_id(clean[2..]))))
	}
	// A fixed array can reach a second inference pass through its lowered C-name
	// spelling (`_3int`). Restore the source spelling before using it as a
	// specialization key, otherwise the duplicate key emits an invalid `_3int`
	// payload type instead of `[3]int`.
	if clean.starts_with('_') && !t.type_name_is_declared(clean) {
		mut len_end := 1
		for len_end < clean.len && clean[len_end].is_digit() {
			len_end++
		}
		if len_end > 1 && len_end < clean.len {
			encoded_elem := clean[len_end..]
			canonical_elem := flat.source_type_name(t.canonical_generic_specialization_arg(flat.source_type_id(encoded_elem)))
			if canonical_elem != encoded_elem || types.is_builtin_type_name(encoded_elem)
				|| t.type_name_is_declared(encoded_elem) {
				candidate := '[${clean[1..len_end]}]${canonical_elem}'
				if c_name(candidate) == clean {
					return flat.source_type_id(candidate)
				}
			}
		}
	}
	// Fixed-array arguments are shortened as `Elem_len` in generated free-function
	// names. A later global scan can observe that encoded spelling through the
	// specialized callback ABI and infer it as a fresh type argument. Restore the
	// source spelling before forming the specialization key, while preserving a
	// real user type that happens to use the same name.
	if fixed := t.canonical_short_fixed_array_arg(clean) {
		return flat.source_type_id(fixed)
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end > 1 && bracket_end < clean.len - 1 {
			len_text := trimmed_transform_text(clean[1..bracket_end])
			elem_text := trimmed_transform_text(clean[bracket_end + 1..])
			if !is_decimal_text(len_text) && t.generic_type_base_requires_args(flat.source_type_id(elem_text)) {
				return flat.source_type_id('${flat.source_type_name(t.canonical_generic_specialization_arg(flat.source_type_id(elem_text)))}[${flat.source_type_name(t.canonical_generic_specialization_arg(flat.source_type_id(len_text)))}]')
			}
			mut clean_len := len_text
			if !isnil(t.tc) {
				if value := t.tc.const_int_value(len_text, []string{}) {
					clean_len = value.str()
				}
			}
			return flat.source_type_id('[${clean_len}]' + flat.source_type_name(t.canonical_generic_specialization_arg(flat.source_type_id(clean[bracket_end + 1..]))))
		}
	}
	if fixed := t.canonical_suffix_fixed_array_arg(clean) {
		return flat.source_type_id(fixed)
	}
	if !t.type_name_is_declared(clean) {
		if fixed := generic_fixed_array_type_arg_from_short_suffix(clean) {
			return flat.source_type_id(fixed)
		}
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len - 1 {
			key := flat.source_type_name(t.canonical_generic_specialization_arg(flat.source_type_id(clean[4..bracket_end])))
			value := flat.source_type_name(t.canonical_generic_specialization_arg(flat.source_type_id(clean[bracket_end + 1..])))
			return flat.source_type_id('map[${key}]${value}')
		}
	}
	base, nested_args, is_generic_app := generic_app_parts(clean)
	if is_generic_app {
		mut canonical_args := []string{cap: nested_args.len}
		for nested_arg in nested_args {
			canonical_args << flat.source_type_name(t.canonical_generic_specialization_arg(flat.source_type_id(nested_arg)))
		}
		return flat.source_type_id('${base}[${canonical_args.join(', ')}]')
	}
	if t.generic_arg_is_alias_name(clean, '') {
		normalized_alias_source_id := t.normalize_type_alias(clean_id)
		normalized_alias_comparison_type_id := normalized_alias_source_id

		if normalized_alias_comparison_type_id != clean_comparison_type_id_4 && t.is_fixed_array_type(normalized_alias_source_id) {
			return t.canonical_generic_specialization_arg(normalized_alias_source_id)
		}
		return clean_id
	}
	// A checker-resolved qualified type is already a canonical semantic name.
	// Do not reinterpret its first component as an import alias from the current
	// source file (where the same spelling can intentionally name another module).
	if clean.contains('.') && (clean in t.structs || t.sum_types.contains(clean)
		|| clean in t.enum_types || (!isnil(t.tc) && (clean in t.tc.structs
		|| t.tc.sum_types.contains(clean) || clean in t.tc.enum_names
		|| clean in t.tc.interface_names))) {
		return clean_id
	}
	// Resolve an import-alias module prefix (`import x.json2 as json` makes
	// `json.Any` mean `json2.Any`); the alias text must not leak into
	// specialization keys/typedef names parsed later without file context.
	if clean.contains('.') {
		alias := clean.all_before('.')
		resolved_mod := t.import_alias_module(alias)
		if resolved_mod != alias && resolved_mod.len > 0 {
			resolved := '${resolved_mod}.${clean.all_after('.')}'
			normalized_resolved_source_id := t.normalize_type_alias(flat.source_type_id(resolved))
			normalized_resolved := flat.source_type_name(normalized_resolved_source_id)
			if normalized_resolved != resolved {
				return t.canonical_generic_specialization_arg(normalized_resolved_source_id)
			}
			return flat.source_type_id(resolved)
		}
		if !t.type_name_is_declared(clean) {
			short := clean.all_after_last('.')
			if qualified_source_id := t.qualified_types.type_id(short) {
				return qualified_source_id
			}
		}
	}
	normalized_source_id := t.normalize_type_alias(clean_id)
	normalized_comparison_type_id_5 := normalized_source_id

	if normalized_comparison_type_id_5 != clean_comparison_type_id_4 {
		return t.canonical_generic_specialization_arg(normalized_source_id)
	}
	if decoded_array := generic_array_type_arg_from_suffix(clean) {
		return flat.source_type_id(decoded_array)
	}
	return clean_id
}

fn generic_fixed_array_type_arg_from_short_suffix(suffix string) ?string {
	clean := trimmed_transform_text(suffix)
	split := clean.last_index_u8(`_`)
	if split <= 0 || split + 1 >= clean.len {
		return none
	}
	len_text := clean[split + 1..]
	if !is_decimal_text(len_text) {
		return none
	}
	elem := generic_type_arg_from_suffix_with_containers(clean[..split])
	if elem.len == 0 {
		return none
	}
	candidate := '[${len_text}]${elem}'
	if generic_type_arg_short(candidate) == clean {
		return candidate
	}
	return none
}

fn (t &Transformer) generic_type_base_requires_args(typ_id u32) bool {
	typ := flat.source_type_name(typ_id)
	clean := trimmed_transform_text(typ)
	if clean.len == 0 || clean.contains('[') || isnil(t.tc) {
		return false
	}
	return clean in t.tc.struct_generic_params || clean in t.tc.sum_generic_params
		|| clean in t.tc.type_alias_generic_params
		|| clean.all_after_last('.') in t.tc.struct_generic_params
		|| clean.all_after_last('.') in t.tc.sum_generic_params
		|| clean.all_after_last('.') in t.tc.type_alias_generic_params
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
	raw := flat.source_type_name(raw_id)
	source := source_composite_type_candidate_bytes(encoded, raw) or { return none }
	return if source == raw { raw_id } else { flat.source_type_id(source) }
}

fn source_composite_type_candidate_bytes(encoded string, raw string) ?string {
	clean := trimmed_transform_text(raw)
	if clean.len == 0 {
		return none
	}
	if clean.starts_with('map[') && c_name(clean) == encoded {
		return clean
	}
	for prefix in ['mut ', 'shared ', 'atomic ', '...', '[]', '?', '!', '&', 'chan '] {
		if clean.starts_with(prefix) {
			return source_composite_type_candidate_bytes(encoded, clean[prefix.len..])
		}
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end > 3 && bracket_end + 1 < clean.len {
			if source := source_composite_type_candidate_bytes(encoded, clean[4..bracket_end]) {
				return source
			}
			return source_composite_type_candidate_bytes(encoded, clean[bracket_end + 1..])
		}
	}
	return none
}

fn (t &Transformer) canonical_suffix_fixed_array_arg(arg string) ?string {
	clean := trimmed_transform_text(arg)
	if !clean.ends_with(']') {
		return none
	}
	open_idx := clean.last_index('[') or { return none }
	if open_idx <= 0 || open_idx + 1 >= clean.len - 1 {
		return none
	}
	elem := trimmed_transform_text(clean[..open_idx])
	len_text := trimmed_transform_text(clean[open_idx + 1..clean.len - 1])
	if elem.len == 0 || len_text.len == 0 {
		return none
	}
	if types.is_builtin_type_name(len_text) || len_text.contains(',') {
		return none
	}
	mut clean_len := len_text
	mut len_resolved := is_decimal_text(len_text)
	if !len_resolved && !isnil(t.tc) {
		if value := t.tc.const_int_value(len_text, []string{}) {
			clean_len = value.str()
			len_resolved = true
		}
	}
	_, _, is_generic_app := generic_app_parts(clean)
	if !len_resolved && is_generic_app && !types.is_builtin_type_name(elem) {
		return none
	}
	return '[${clean_len}]${flat.source_type_name(t.canonical_generic_specialization_arg(flat.source_type_id(elem)))}'
}

fn (t &Transformer) canonical_short_fixed_array_arg(arg string) ?string {
	clean := trimmed_transform_text(arg)
	if clean.len == 0 || t.type_name_is_declared(clean) {
		return none
	}
	split := clean.last_index_u8(`_`)
	if split <= 0 || split + 1 >= clean.len {
		return none
	}
	len_text := clean[split + 1..]
	if !is_decimal_text(len_text) {
		return none
	}
	encoded_elem := clean[..split]
	canonical_elem := flat.source_type_name(t.canonical_generic_specialization_arg(flat.source_type_id(encoded_elem)))
	if canonical_elem == encoded_elem && !types.is_builtin_type_name(encoded_elem)
		&& !t.type_name_is_declared(encoded_elem) {
		return none
	}
	candidate := '[${len_text}]${canonical_elem}'
	encoded := generic_fixed_array_type_arg_short(candidate) or { return none }
	if encoded != clean {
		return none
	}
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
		t.generic_specialization_args_parent.ids.len
	}
	if cache.local_len != t.generic_specialization_args.ids.len || cache.parent_len != parent_len || cache.parent != parent {
		cache.misses.clear()
		cache.name_misses.clear()
		cache.local_len = t.generic_specialization_args.ids.len
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
	if args := t.recorded_generic_specialization_args_uncached(flat.source_type_name(type_id)) {
		return args
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
		if t.generic_arg_is_unresolved(arg_id) { return true }
	}
	return false
}

fn (t &Transformer) generic_arg_is_unresolved(arg_id u32) bool {
	if t.skip_generics {
		// Anonymous literals still need materialization in nongeneric programs.
		// Skipping generic specialization must not make their provisional storage
		// type authoritative over the concrete literal type.
		return inferred_anonymous_struct_placeholder(flat.source_type_name(arg_id))
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
		result := t.generic_arg_is_unresolved_uncached(flat.source_type_name(arg_id))
		cache.put(arg_id, if result { i8(1) } else { i8(-1) })
		return result
	}
	return t.generic_arg_is_unresolved_uncached(flat.source_type_name(arg_id))
}

fn inferred_anonymous_struct_placeholder(typ string) bool {
	clean := trimmed_transform_text(typ)
	if clean.starts_with('[]') {
		return inferred_anonymous_struct_placeholder(clean[2..])
	}
	if clean.starts_with('&') || clean.starts_with('?') || clean.starts_with('!') {
		return inferred_anonymous_struct_placeholder(clean[1..])
	}
	if clean.starts_with('map[') {
		end := generic_matching_bracket(clean, 3)
		if end < clean.len {
			return inferred_anonymous_struct_placeholder(clean[4..end])
				|| inferred_anonymous_struct_placeholder(clean[end + 1..])
		}
	}
	if clean.starts_with('[') {
		end := generic_matching_bracket(clean, 0)
		if end < clean.len {
			return inferred_anonymous_struct_placeholder(clean[end + 1..])
		}
	}
	return clean.all_after_last('.') == 'struct'
}

fn (t &Transformer) generic_arg_is_unresolved_uncached(arg string) bool {
	clean := trimmed_transform_text(arg)
	if clean.len == 0 || clean.all_after_last('.') in ['unknown', 'void', 'generic', 'struct'] {
		return true
	}
	if clean.starts_with('(') && clean.ends_with(')') {
		items := split_fn_type_params(clean[1..clean.len - 1])
		if items.len == 0 {
			return true
		}
		for item in items {
			if t.generic_arg_is_unresolved(flat.source_type_id(item)) {
				return true
			}
		}
		return false
	}
	if clean.starts_with('&') {
		return t.generic_arg_is_unresolved(flat.source_type_id(clean[1..]))
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		return t.generic_arg_is_unresolved(flat.source_type_id(clean[1..]))
	}
	if clean.starts_with('...') {
		return t.generic_arg_is_unresolved(flat.source_type_id(clean[3..]))
	}
	if clean.starts_with('[]') {
		return t.generic_arg_is_unresolved(flat.source_type_id(clean[2..]))
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return t.generic_arg_is_unresolved(flat.source_type_id(clean[bracket_end + 1..]))
		}
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			return t.generic_arg_is_unresolved(flat.source_type_id(clean[4..bracket_end]))
				|| t.generic_arg_is_unresolved(flat.source_type_id(clean[bracket_end + 1..]))
		}
	}
	if is_generic_fn_placeholder_name(clean) {
		// Inspect the nominal name after its wrappers, so `[]main.T` keeps the
		// same concrete declaration as `main.T`.
		return !t.is_known_concrete_type_name(clean)
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		params, ret := fn_type_text_parts(clean) or { return true }
		for param in params {
			if t.generic_arg_is_unresolved(flat.source_type_id(generic_fn_type_param_payload(param))) {
				return true
			}
		}
		return ret.len > 0 && t.generic_arg_is_unresolved(flat.source_type_id(ret))
	}
	_, nested_args, ok := generic_app_parts(clean)
	if ok {
		for nested in nested_args {
			if t.generic_arg_is_unresolved(flat.source_type_id(nested)) {
				return true
			}
		}
	}
	return false
}

// is_known_concrete_type_name reports whether `name` refers to an already
// declared concrete type (struct, sum type, enum, interface or alias), so that
// one-letter type names like `A` are not mistaken for generic placeholders.
fn (t &Transformer) is_known_concrete_type_name(name string) bool {
	if t.type_name_is_declared(name) || t.is_known_main_struct_type_name(name) {
		return true
	}
	if !name.contains('.') {
		if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin'
			&& t.type_name_is_declared('${t.cur_module}.${name}') {
			return true
		}
		if t.type_name_is_declared('main.${name}') {
			return true
		}
	}
	return false
}

fn (t &Transformer) is_known_main_struct_type_name(name string) bool {
	if !name.starts_with('main.') || t.ident_is_import_alias('main') {
		return false
	}
	bare := name['main.'.len..]
	if info := t.structs[bare] {
		return info.module in ['', 'main']
	}
	return false
}

fn (t &Transformer) type_name_is_declared(name string) bool {
	if name in t.structs || t.sum_types.contains(name) || name in t.enum_types {
		return true
	}
	if !isnil(t.tc) {
		if name in t.tc.structs || t.tc.sum_types.contains(name) || name in t.tc.enum_names
			|| name in t.tc.interface_names || t.tc.type_aliases.contains(name) {
			return true
		}
	}
	return false
}

fn generic_type_arg_from_receiver_suffix(suffix string) string {
	clean := trimmed_transform_text(suffix)
	if clean.len == 0 || (clean.starts_with('Array_') && !clean.starts_with('Array_fixed_')) {
		return ''
	}
	return generic_type_arg_from_suffix(clean)
}

fn generic_array_type_arg_from_suffix(suffix string) ?string {
	clean := trimmed_transform_text(suffix)
	if !clean.starts_with('Array_') || clean.starts_with('Array_fixed_') {
		return none
	}
	elem_suffix := clean['Array_'.len..]
	if elem_suffix.len == 0 {
		return none
	}
	split := elem_suffix.last_index_u8(`_`)
	if split > 0 {
		len_text := elem_suffix[split + 1..]
		if len_text.len > 0 && is_decimal_text(len_text) {
			elem := generic_type_arg_from_suffix(elem_suffix[..split])
			if elem.len > 0 {
				return '[${len_text}]${elem}'
			}
		}
	}
	elem := generic_type_arg_from_suffix_with_containers(elem_suffix)
	if elem.len == 0 {
		return none
	}
	return '[]${elem}'
}

struct GenericTypeSuffixPart {
	typ u32
	len int
}

fn generic_type_arg_prefix_from_suffix(suffix string) ?GenericTypeSuffixPart {
	clean := trimmed_transform_text(suffix)
	if clean.len == 0 {
		return none
	}
	for encoded in ['string', 'bool', 'int', 'u8', 'u16', 'u32', 'u64', 'i8', 'i16', 'i32', 'i64',
		'f32', 'f64', 'v_int', 'v_u8', 'v_u16', 'v_u32', 'v_u64', 'v_i8', 'v_i16', 'v_i32', 'v_i64',
		'v_f32', 'v_f64'] {
		if clean == encoded || clean.starts_with('${encoded}_') {
			return GenericTypeSuffixPart{
				typ: flat.source_type_id(generic_type_arg_from_suffix(encoded))
				len: encoded.len
			}
		}
	}
	for prefix, marker in {
		'ptr_':        '&'
		'Option_':     '?'
		'__v_option_': '?'
		'Result_':     '!'
	} {
		if clean.starts_with(prefix) {
			inner := generic_type_arg_prefix_from_suffix(clean[prefix.len..]) or { return none }
			return GenericTypeSuffixPart{
				typ: flat.source_type_id('${marker}${inner.typ_text()}')
				len: prefix.len + inner.len
			}
		}
	}
	if clean.starts_with('Array_') && !clean.starts_with('Array_fixed_') {
		inner := generic_type_arg_prefix_from_suffix(clean['Array_'.len..]) or { return none }
		mut typ := flat.source_type_id('[]${inner.typ_text()}')
		mut consumed := 'Array_'.len + inner.len
		if consumed + 1 < clean.len && clean[consumed] == `_` {
			mut end := consumed + 1
			for end < clean.len && clean[end] >= `0` && clean[end] <= `9` {
				end++
			}
			if end > consumed + 1 && (end == clean.len || clean[end] == `_`) {
				typ = flat.source_type_id('[${clean[consumed + 1..end]}]${inner.typ_text()}')
				consumed = end
			}
		}
		return GenericTypeSuffixPart{
			typ: typ
			len: consumed
		}
	}
	if clean.starts_with('Map_') {
		entry_suffix := clean['Map_'.len..]
		key := generic_type_arg_prefix_from_suffix(entry_suffix) or { return none }
		separator := 'Map_'.len + key.len
		if separator >= clean.len || clean[separator] != `_` {
			return none
		}
		value := generic_type_arg_prefix_from_suffix(clean[separator + 1..]) or { return none }
		return GenericTypeSuffixPart{
			typ: flat.source_type_id('map[${key.typ_text()}]${value.typ_text()}')
			len: separator + 1 + value.len
		}
	}
	return none
}

fn generic_map_type_arg_from_suffix(suffix string) ?string {
	clean := trimmed_transform_text(suffix)
	if !clean.starts_with('Map_') {
		return none
	}
	entry_suffix := clean['Map_'.len..]
	if key := generic_type_arg_prefix_from_suffix(entry_suffix) {
		if key.len < entry_suffix.len && entry_suffix[key.len] == `_` {
			value := generic_type_arg_from_suffix_with_containers(entry_suffix[key.len + 1..])
			if value.len > 0 {
				return 'map[${key.typ_text()}]${value}'
			}
		}
	}
	split := entry_suffix.last_index_u8(`_`)
	if split <= 0 || split + 1 >= entry_suffix.len {
		return none
	}
	key := generic_type_arg_from_suffix_with_containers(entry_suffix[..split])
	value := generic_type_arg_from_suffix_with_containers(entry_suffix[split + 1..])
	if key.len == 0 || value.len == 0 {
		return none
	}
	return 'map[${key}]${value}'
}

fn (t &Transformer) generic_type_arg_from_suffix(suffix string) string {
	clean := trimmed_transform_text(suffix)
	if clean.len == 0 {
		return ''
	}
	return generic_type_arg_from_suffix(clean)
}

fn generic_type_arg_from_suffix(suffix string) string {
	clean := trimmed_transform_text(suffix)
	if clean.len == 0 {
		return ''
	}
	if clean.starts_with('ptr_') {
		inner := generic_type_arg_from_suffix_with_containers(clean['ptr_'.len..])
		if inner.len > 0 {
			return '&${inner}'
		}
	}
	if clean.starts_with('Option_') {
		inner := generic_type_arg_from_suffix_with_containers(clean['Option_'.len..])
		if inner.len > 0 {
			return '?${inner}'
		}
	}
	if clean.starts_with('__v_option_') {
		inner := generic_type_arg_from_suffix_with_containers(clean['__v_option_'.len..])
		if inner.len > 0 {
			return '?${inner}'
		}
	}
	if clean.starts_with('Result_') {
		inner := generic_type_arg_from_suffix_with_containers(clean['Result_'.len..])
		if inner.len > 0 {
			return '!${inner}'
		}
	}
	return match clean {
		'v_int' { 'int' }
		'v_u8' { 'u8' }
		'v_u16' { 'u16' }
		'v_u32' { 'u32' }
		'v_u64' { 'u64' }
		'v_i8' { 'i8' }
		'v_i16' { 'i16' }
		'v_i32' { 'i32' }
		'v_i64' { 'i64' }
		'v_f32' { 'f32' }
		'v_f64' { 'f64' }
		'Array_u8' { '[]u8' }
		else { clean.replace('__', '.') }
	}
}

fn generic_type_arg_from_suffix_with_containers(suffix string) string {
	if decoded := generic_array_type_arg_from_suffix(suffix) {
		return decoded
	}
	if decoded := generic_map_type_arg_from_suffix(suffix) {
		return decoded
	}
	return generic_type_arg_from_suffix(suffix)
}

fn generic_fn_spec_key(decl_key string, args_ids []u32) string {
	args := flat.TypeTextList{ ids: args_ids }.names()
	return '${decl_key}[${args.join(', ')}]'
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

// file_import_module returns the module that `alias` names through the imports
// of `file`. The checker's import table is complete before transform starts, so
// answers, including misses, are memoized by the parts of the table's key.
fn (t &Transformer) file_import_module(file string, alias string) ?string {
	if isnil(t.tc) {
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

fn specialized_signature_storage_type_name_bytes(typ types.Type) string {
	if typ is types.Alias {
		return typ.name()
	}
	if typ is types.OptionType {
		return '?' + specialized_signature_storage_type_name_bytes(typ.base_type)
	}
	if typ is types.ResultType {
		return '!' + specialized_signature_storage_type_name_bytes(typ.base_type)
	}
	if typ is types.Pointer {
		return '&' + specialized_signature_storage_type_name_bytes(typ.base_type)
	}
	if typ is types.ArrayFixed {
		len_text := if typ.len > 0 { typ.len.str() } else { typ.len_expr }
		return '[${len_text}]' + specialized_signature_storage_type_name_bytes(typ.elem_type)
	}
	return typ.name()
}
