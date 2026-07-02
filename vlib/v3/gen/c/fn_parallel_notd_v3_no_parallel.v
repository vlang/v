module c

import runtime
import strings
import v3.flat
import v3.types

const max_flat_cgen_jobs = 10
const min_flat_cgen_parallel_items = 1024

$if !windows {
	// FlatCgenChunkArgs represents flat cgen chunk args data used by c.
	struct FlatCgenChunkArgs {
		worker         voidptr
		work_items_ptr voidptr
	}

	// C.pthread_t declares C pthread t data used by c.
	@[typedef]
	struct C.pthread_t {}

	// C.pthread_create declares the C pthread_create symbol used by c.
	fn C.pthread_create(thread &C.pthread_t, attr voidptr, start_routine fn (voidptr) voidptr, arg voidptr) int

	// C.pthread_join declares the C pthread_join symbol used by c.
	fn C.pthread_join(thread C.pthread_t, retval voidptr) int

	// C.pthread_attr_init declares the C pthread_attr_init symbol used by c.
	fn C.pthread_attr_init(attr voidptr) int

	// C.pthread_attr_setstacksize declares the C pthread_attr_setstacksize symbol used by c.
	fn C.pthread_attr_setstacksize(attr voidptr, stacksize usize) int

	// C.pthread_attr_destroy declares the C pthread_attr_destroy symbol used by c.
	fn C.pthread_attr_destroy(attr voidptr) int

	// flat_cgen_chunk_thread supports flat cgen chunk thread handling for c.
	fn flat_cgen_chunk_thread(arg voidptr) voidptr {
		a := unsafe { &FlatCgenChunkArgs(arg) }
		mut w := unsafe { &FlatGen(a.worker) }
		items := unsafe { &[]FlatFnGenItem(a.work_items_ptr) }
		w.gen_fn_items(*items)
		return unsafe { nil }
	}
}

// gen_fns_dispatch emits fns dispatch output for c.
fn (mut g FlatGen) gen_fns_dispatch(no_parallel bool) {
	if no_parallel {
		g.gen_fns()
		g.gen_synthetic_main_after_fns()
		return
	}
	items := g.ensure_fn_gen_items()
	n_items := items.len
	n_jobs := flat_cgen_job_count(runtime.nr_jobs(), n_items)
	$if windows {
		g.gen_fn_items(items)
		g.gen_synthetic_main_after_fns()
		return
	} $else {
		if n_items < min_flat_cgen_parallel_items || n_jobs <= 1 {
			g.gen_fn_items(items)
			g.gen_synthetic_main_after_fns()
			return
		}
		g.parallel_used = true
		g.prepare_parallel_items(items)
		mut chunk_items := split_flat_cgen_items(items, n_jobs)
		chunk_count := chunk_items.len
		// chunk[0] is emitted by the master (this thread) directly into its own builder
		// — no worker clone. Only chunks[1..] get helper threads with a cloned FlatGen.
		// This drops one full FlatGen clone from the peak and uses the master thread that
		// would otherwise just block in join.
		thread_count := chunk_count - 1
		mut thread_ids := []C.pthread_t{len: thread_count}
		mut args := []FlatCgenChunkArgs{cap: thread_count}
		mut workers := []voidptr{cap: thread_count}

		// Helper workers keep the same temp-name base they had when every chunk was a
		// worker (worker_id ci+1 -> base (ci+2)*100_000), and the master adopts what was
		// worker 0's base. This keeps each chunk in its own disjoint _tN range AND makes
		// the output byte-identical to the all-workers version.
		for ci := 0; ci < thread_count; ci++ {
			w := g.new_parallel_worker(ci + 1)
			workers << voidptr(w)
		}
		for ci := 0; ci < thread_count; ci++ {
			args << FlatCgenChunkArgs{
				worker:         workers[ci]
				work_items_ptr: unsafe { voidptr(&chunk_items[ci + 1]) }
			}
		}

		attr_buf := [64]u8{}
		attr := unsafe { voidptr(&attr_buf[0]) }
		C.pthread_attr_init(attr)
		C.pthread_attr_setstacksize(attr, 64 * 1024 * 1024)
		for ci := 0; ci < thread_count; ci++ {
			C.pthread_create(unsafe { &thread_ids[ci] }, attr, flat_cgen_chunk_thread,
				unsafe { voidptr(&args[ci]) })
		}
		C.pthread_attr_destroy(attr)
		// Master emits chunk[0] into g.sb while the helper threads run, using worker 0's
		// old temp base so its emitted temps match the all-workers numbering.
		g.tmp_count = 100_000
		g.gen_fn_items(chunk_items[0])
		for ci := 0; ci < thread_count; ci++ {
			C.pthread_join(thread_ids[ci], unsafe { nil })
		}
		// Merge helper output after the master's chunk[0], in fixed order.
		for ci := 0; ci < thread_count; ci++ {
			w := unsafe { &FlatGen(workers[ci]) }
			g.merge_parallel_worker(w)
		}
		g.gen_synthetic_main_after_fns()
	}
}

// flat_cgen_job_count supports flat cgen job count handling for c.
fn flat_cgen_job_count(n_runtime_jobs int, n_items int) int {
	if n_runtime_jobs <= 0 || n_items <= 0 {
		return 0
	}
	mut n_jobs := n_runtime_jobs
	if n_jobs > max_flat_cgen_jobs {
		n_jobs = max_flat_cgen_jobs
	}
	if n_jobs > n_items {
		n_jobs = n_items
	}
	return n_jobs
}

// split_flat_cgen_items supports split flat cgen items handling for c.
fn split_flat_cgen_items(items []FlatFnGenItem, n_jobs int) [][]FlatFnGenItem {
	if n_jobs <= 0 || items.len == 0 {
		return [][]FlatFnGenItem{}
	}
	mut chunks := [][]FlatFnGenItem{}
	mut total_cost := 0
	for item in items {
		total_cost += item.cost
	}
	mut target_cost := total_cost / n_jobs
	if total_cost % n_jobs != 0 {
		target_cost++
	}
	if target_cost < 1 {
		target_cost = 1
	}
	mut current := []FlatFnGenItem{}
	mut current_cost := 0
	mut chunks_left := n_jobs
	for idx, item in items {
		remaining_items := items.len - idx
		if current.len > 0 && current_cost >= target_cost && chunks_left > 1
			&& remaining_items >= chunks_left {
			chunks << current
			current = []FlatFnGenItem{}
			current_cost = 0
			chunks_left--
		}
		current << item
		current_cost += item.cost
	}
	if current.len > 0 {
		chunks << current
	}
	return chunks
}

// prepare_parallel_items supports prepare parallel items handling for FlatGen.
fn (mut g FlatGen) prepare_parallel_items(items []FlatFnGenItem) {
	mut stack := []flat.NodeId{cap: 256}
	mut type_text_cache := map[string]bool{}
	for item in items {
		g.tc.cur_file = item.file
		g.tc.cur_module = item.module
		g.prepare_parallel_node(item.node_id, mut stack, mut type_text_cache)
	}
	if _ := g.ierror_interface_name() {
		g.intern_string('')
	}
	g.register_interface_strings()
}

// prepare_parallel_node supports prepare parallel node handling for FlatGen.
fn (mut g FlatGen) prepare_parallel_node(id flat.NodeId, mut stack []flat.NodeId, mut type_text_cache map[string]bool) {
	stack.clear()
	stack << id
	for stack.len > 0 {
		current_id := stack.pop()
		idx := int(current_id)
		if idx < 0 || idx >= g.a.nodes.len {
			continue
		}
		node := g.a.nodes[idx]
		if node.kind == .string_literal {
			g.intern_string(node.value)
		}
		if g.should_preseed_parallel_type_text_cached(node.typ, mut type_text_cache) {
			g.preseed_parallel_fn_ptr_type(g.tc.parse_type(node.typ))
		}
		if expr_type := g.parallel_cached_expr_type(current_id, node) {
			g.preseed_parallel_fn_ptr_type(expr_type)
		}
		for i := node.children_count - 1; i >= 0; i-- {
			child_id := g.a.children[node.children_start + i]
			if int(child_id) >= 0 {
				stack << child_id
			}
		}
	}
}

fn (g &FlatGen) parallel_cached_expr_type(id flat.NodeId, node flat.Node) ?types.Type {
	idx := int(id)
	if idx < 0 {
		return none
	}
	if g.tc.parallel_check_sparse {
		if t := g.tc.sparse_expr_type_values[idx] {
			return t
		}
		if node.kind == .call {
			if name := g.tc.sparse_resolved_call_names[idx] {
				if t := g.tc.fn_ret_types[name] {
					return t
				}
			}
		}
		return none
	}
	if idx < g.tc.expr_type_set.len && idx < g.tc.expr_type_values.len && g.tc.expr_type_set[idx] {
		return g.tc.expr_type_values[idx]
	}
	if node.kind == .call && idx < g.tc.resolved_call_set.len && idx < g.tc.resolved_call_names.len
		&& g.tc.resolved_call_set[idx] {
		name := g.tc.resolved_call_names[idx]
		if t := g.tc.fn_ret_types[name] {
			return t
		}
	}
	return none
}

fn (g &FlatGen) should_preseed_parallel_type_text_cached(typ string, mut cache map[string]bool) bool {
	if typ.len == 0 {
		return false
	}
	key := '${g.tc.cur_file}\n${g.tc.cur_module}\n${typ}'
	if key in cache {
		return cache[key]
	}
	should_preseed := g.should_preseed_parallel_type_text(typ)
	cache[key] = should_preseed
	return should_preseed
}

// should_preseed_parallel_type_text reports whether should preseed parallel type text applies in c.
fn (g &FlatGen) should_preseed_parallel_type_text(typ string) bool {
	if typ.len == 0 {
		return false
	}
	clean := g.parallel_base_type_text(typ)
	if clean.contains('fn(') || clean.contains('fn (') {
		return true
	}
	if clean in g.tc.type_aliases {
		return true
	}
	qtyp := g.tc.qualify_name(clean)
	return qtyp in g.tc.type_aliases
}

// parallel_base_type_text supports parallel base type text handling for FlatGen.
fn (g &FlatGen) parallel_base_type_text(typ string) string {
	mut clean := typ.trim_space()
	for clean.len > 0 {
		if clean.starts_with('shared ') {
			clean = clean[7..].trim_space()
		} else if clean[0] == `&` || clean[0] == `?` || clean[0] == `!` {
			clean = clean[1..].trim_space()
		} else if clean.starts_with('...') {
			clean = clean[3..].trim_space()
		} else if clean.starts_with('[]') {
			clean = clean[2..].trim_space()
		} else {
			break
		}
	}
	return clean
}

// preseed_parallel_fn_ptr_type supports preseed parallel fn ptr type handling for FlatGen.
fn (mut g FlatGen) preseed_parallel_fn_ptr_type(typ types.Type) {
	if typ is types.FnType {
		g.resolve_fn_ptr_type(g.fn_ptr_type_key(typ))
		for param in typ.params {
			g.preseed_parallel_fn_ptr_type(param)
		}
		g.preseed_parallel_fn_ptr_type(typ.return_type)
	} else if typ is types.Pointer {
		g.preseed_parallel_fn_ptr_type(typ.base_type)
	} else if typ is types.Array {
		g.preseed_parallel_fn_ptr_type(typ.elem_type)
	} else if typ is types.ArrayFixed {
		g.preseed_parallel_fn_ptr_type(typ.elem_type)
	} else if typ is types.Map {
		g.preseed_parallel_fn_ptr_type(typ.key_type)
		g.preseed_parallel_fn_ptr_type(typ.value_type)
	} else if typ is types.OptionType {
		g.preseed_parallel_fn_ptr_type(typ.base_type)
	} else if typ is types.ResultType {
		g.preseed_parallel_fn_ptr_type(typ.base_type)
	} else if typ is types.Alias {
		g.preseed_parallel_fn_ptr_type(typ.base_type)
	} else if typ is types.MultiReturn {
		for item in typ.types {
			g.preseed_parallel_fn_ptr_type(item)
		}
	}
}

// fn_ptr_type_key supports fn ptr type key handling for FlatGen.
fn (mut g FlatGen) fn_ptr_type_key(typ types.FnType) string {
	ret := if typ.return_type is types.Void { 'void' } else { g.tc.c_type(typ.return_type) }
	if typ.params.len == 0 {
		return 'fn_ptr:${ret}|void'
	}
	mut params := []string{}
	for param in typ.params {
		params << g.tc.c_type(param)
	}
	return 'fn_ptr:${ret}|${params.join(', ')}'
}

// new_parallel_worker builds a per-worker FlatGen for parallel codegen.
//
// The lookup tables populated before gen_fns_dispatch (in collect_gen_info,
// collect_interface_impls and the precompute_* passes) are READ-ONLY during codegen, so
// they are SHARED by reference instead of cloned — V maps/arrays are reference types and
// concurrent readers are safe. Only the state a worker actually mutates while emitting is
// kept private: the output builder; the string-literal table (interned during gen); the
// fn_ptr_types / needed_optional_types / emitted_* sets and the param_types_cache /
// array_method_cache memoization caches (all written during gen); the per-function
// cur_param_* scratch; and runtime_inits (kept private out of caution). This drops the
// bulk of each worker's clone cost — previously the whole table set was duplicated per
// worker and, under -gc none, never freed.
fn (g &FlatGen) new_parallel_worker(worker_id int) &FlatGen {
	return &FlatGen{
		sb:                             strings.new_builder(64_000)
		a:                              unsafe { g.a }
		used_fns:                       g.used_fns
		used_fn_names:                  g.used_fn_names
		test_files:                     g.test_files.clone()
		str_lits:                       g.str_lits.clone()
		str_lit_ids:                    g.str_lit_ids.clone()
		global_types:                   g.global_types
		enum_vals:                      g.enum_vals
		interfaces:                     g.interfaces
		const_vals:                     g.const_vals
		const_modules:                  g.const_modules
		const_init_order:               g.const_init_order
		fixed_storage_consts:           g.fixed_storage_consts
		global_modules:                 g.global_modules
		global_inits:                   g.global_inits
		global_init_order:              g.global_init_order
		iface_impls:                    g.iface_impls
		iface_type_ids:                 g.iface_type_ids
		ierror_method_emit_names:       g.ierror_method_emit_names
		ierror_stack_pointer_aliases:   []map[string]bool{}
		local_pointer_storage_by_owner: map[string]bool{}
		sum_name_lookup:                g.sum_name_lookup
		module_init_fns:                g.module_init_fns
		module_init_fn_modules:         g.module_init_fn_modules
		module_imports:                 g.module_imports
		libc_compat_fns:                g.libc_compat_fns.clone()
		tc:                             g.clone_parallel_type_checker()
		has_builtins:                   g.has_builtins
		tmp_count:                      (worker_id + 1) * 100_000
		line_start:                     true
		modules:                        g.modules
		fn_ptr_types:                   g.fn_ptr_types.clone()
		fixed_array_ret_wrappers:       g.fixed_array_ret_wrappers
		concrete_optional_abi_fns:      g.concrete_optional_abi_fns
		fn_decl_param_types:            g.fn_decl_param_types
		fn_decl_mut_receivers:          g.fn_decl_mut_receivers
		fn_decl_ret_types:              g.fn_decl_ret_types
		struct_decl_infos:              g.struct_decl_infos
		struct_decl_short_infos:        g.struct_decl_short_infos
		shared_type_names:              g.shared_type_names
		const_runtime_inits:            g.const_runtime_inits.clone()
		runtime_inits:                  g.runtime_inits.clone()
		compiler_vroot:                 g.compiler_vroot
		cur_param_names:                g.cur_param_names.clone()
		cur_param_type_values:          g.cur_param_type_values.clone()
		cur_param_types:                g.cur_param_types.clone()
		cur_concrete_optional_params:   g.cur_concrete_optional_params.clone()
		cur_mut_params:                 g.cur_mut_params.clone()
		cur_mut_param_owners:           g.cur_mut_param_owners.clone()
		cur_fn_ret:                     g.cur_fn_ret
		cur_fn_ret_is_optional:         g.cur_fn_ret_is_optional
		cur_fn_ret_base:                g.cur_fn_ret_base
		loop_label_depths:              map[string]int{}
		expected_expr_type:             g.expected_expr_type
		expected_enum:                  g.expected_enum
		needed_optional_types:          g.needed_optional_types.clone()
		emitted_optional_types:         g.emitted_optional_types.clone()
		emitted_fns:                    g.emitted_fns.clone()
		array_method_cache:             g.array_method_cache.clone()
		param_types_cache:              g.param_types_cache.clone()
		embedded_fields_by_type:        g.embedded_fields_by_type
		param_types_by_short:           g.param_types_by_short
		generic_method_candidates:      g.generic_method_candidates
		spawn_wrapper_names:            g.spawn_wrapper_names.clone()
		spawn_wrapper_defs:             g.spawn_wrapper_defs.clone()
		callback_wrapper_names:         g.callback_wrapper_names.clone()
		callback_wrapper_defs:          g.callback_wrapper_defs.clone()
	}
}

// clone_parallel_type_checker builds a per-worker TypeChecker for parallel codegen.
//
// During codegen the checker's lookup tables are READ-ONLY: cgen only ever assigns the
// scalar `cur_file`/`cur_module` fields, and the read paths it uses (expr_type, c_type,
// parse_type, resolve_type, cached_resolved_call) never write into the big maps — the only
// memoizing write is into `type_cache`, which is left nil here so workers take the uncached
// path. V maps and arrays are reference types, so the read-only tables are SHARED by
// reference (no `.clone()`), exactly like the already-shared `a` FlatAst. This avoids
// deep-copying the program-wide `expr_type_*`/`structs`/signature tables once per worker,
// which was the bulk of parallel cgen's extra RAM and serial setup time.
//
// Only genuinely per-worker mutable state is given its own copy: the scope chain (gen pushes
// child scopes) and `errors` (avoid a concurrent append race, though gen does not emit any).
fn (g &FlatGen) clone_parallel_type_checker() &types.TypeChecker {
	mut fs := types.new_scope(unsafe { nil })
	fs.names = g.tc.file_scope.names.clone()
	fs.types = g.tc.file_scope.types.clone()
	fs.generations = g.tc.file_scope.generations.clone()
	fs.next_generation = g.tc.file_scope.next_generation
	fs.lifetime = g.tc.file_scope.lifetime
	return &types.TypeChecker{
		a:                                  unsafe { g.tc.a }
		fn_ret_types:                       g.tc.fn_ret_types
		fn_param_types:                     g.tc.fn_param_types
		fn_ret_type_texts:                  g.tc.fn_ret_type_texts
		fn_param_type_texts:                g.tc.fn_param_type_texts
		fn_type_files:                      g.tc.fn_type_files
		fn_type_modules:                    g.tc.fn_type_modules
		fn_generic_params:                  g.tc.fn_generic_params
		specialized_generic_fns:            g.tc.specialized_generic_fns
		fn_variadic:                        g.tc.fn_variadic
		fn_implicit_veb_ctx:                g.tc.fn_implicit_veb_ctx
		c_variadic_fns:                     g.tc.c_variadic_fns
		structs:                            g.tc.structs
		struct_modules:                     g.tc.struct_modules
		struct_files:                       g.tc.struct_files
		struct_error_embeds_shadow_builtin: g.tc.struct_error_embeds_shadow_builtin
		struct_generic_params:              g.tc.struct_generic_params
		struct_field_c_abi_fns:             g.tc.struct_field_c_abi_fns
		unions:                             g.tc.unions
		type_aliases:                       g.tc.type_aliases
		type_alias_c_abi_fns:               g.tc.type_alias_c_abi_fns
		sum_types:                          g.tc.sum_types
		sum_generic_params:                 g.tc.sum_generic_params
		enum_names:                         g.tc.enum_names
		enum_fields:                        g.tc.enum_fields
		flag_enums:                         g.tc.flag_enums
		interface_names:                    g.tc.interface_names
		interface_fields:                   g.tc.interface_fields
		interface_embeds:                   g.tc.interface_embeds
		interface_abstract_methods:         g.tc.interface_abstract_methods
		c_globals:                          g.tc.c_globals
		const_types:                        g.tc.const_types
		const_exprs:                        g.tc.const_exprs
		const_modules:                      g.tc.const_modules
		const_files:                        g.tc.const_files
		const_suffixes:                     g.tc.const_suffixes
		imports:                            g.tc.imports
		file_imports:                       g.tc.file_imports
		file_selective_imports:             g.tc.file_selective_imports
		file_modules:                       g.tc.file_modules
		file_scope:                         fs
		cur_scope:                          fs
		scope_pool:                         []&types.Scope{}
		has_builtins:                       g.tc.has_builtins
		cur_module:                         g.tc.cur_module
		cur_file:                           g.tc.cur_file
		errors:                             g.tc.errors.clone()
		resolved_call_names:                g.tc.resolved_call_names
		resolved_call_set:                  g.tc.resolved_call_set
		resolved_fn_value_names:            g.tc.resolved_fn_value_names
		resolved_fn_value_set:              g.tc.resolved_fn_value_set
		statement_nodes:                    g.tc.statement_nodes
		expr_type_values:                   g.tc.expr_type_values
		expr_type_set:                      g.tc.expr_type_set
		checking_nodes:                     g.tc.checking_nodes
		diagnose_unknown_calls:             g.tc.diagnose_unknown_calls
		reject_unlowered_map_mutation:      g.tc.reject_unlowered_map_mutation
		diagnostic_files:                   g.tc.diagnostic_files
		selected_file_called_fns:           g.tc.selected_file_called_fns
		cur_fn_ret_type:                    g.tc.cur_fn_ret_type
		smartcasts:                         g.tc.smartcasts
		// Read-only map cgen uses to recover substituted signatures for generic-receiver
		// method values (`Box[int].method` as a callback); without it a parallel worker
		// sees an empty map and gen_method_value_closure falls through.
		generic_method_value_info: g.tc.generic_method_value_info
		params_structs:            g.tc.params_structs
	}
}

// merge_parallel_worker supports merge parallel worker handling for FlatGen.
fn (mut g FlatGen) merge_parallel_worker(w &FlatGen) {
	mut ww := unsafe { w }
	worker_output := ww.sb.str()
	if worker_output.len > 0 {
		g.sb.write_string(worker_output)
	}
	// The master builder has copied the worker output; release the worker buffers.
	unsafe {
		worker_output.free()
		ww.sb.free()
	}
	for opt_name, val_type in w.needed_optional_types {
		g.needed_optional_types[opt_name] = val_type
	}
	for encoded, name in w.fn_ptr_types {
		if encoded !in g.fn_ptr_types {
			g.fn_ptr_types[encoded] = name
		}
	}
	for name, enabled in w.libc_compat_fns {
		if enabled {
			g.libc_compat_fns[name] = true
		}
	}
	// Spawn wrappers (thread arg structs + trampoline fns) are generated on demand
	// inside fn bodies, so a worker that emits a `spawn` produces wrapper defs the
	// master must also emit. Deduplicate by their deterministic key/def.
	for key, name in w.spawn_wrapper_names {
		if key !in g.spawn_wrapper_names {
			g.spawn_wrapper_names[key] = name
		}
	}
	for def in w.spawn_wrapper_defs {
		if def !in g.spawn_wrapper_defs {
			g.spawn_wrapper_defs << def
		}
	}
	for key, name in w.callback_wrapper_names {
		if key !in g.callback_wrapper_names {
			g.callback_wrapper_names[key] = name
		}
	}
	for def in w.callback_wrapper_defs {
		if def !in g.callback_wrapper_defs {
			g.callback_wrapper_defs << def
		}
	}
}
