module c

import runtime
import strings
import v3.flat
import v3.types

const max_flat_cgen_jobs = 3
const min_flat_cgen_parallel_items = 1024

struct FlatFnGenItem {
	node_id flat.NodeId
	file    string
	module  string
	cost    int
}

$if !windows {
	struct FlatCgenChunkArgs {
		worker         voidptr
		work_items_ptr voidptr
	}

	@[typedef]
	struct C.pthread_t {}

	fn C.pthread_create(thread &C.pthread_t, attr voidptr, start_routine fn (voidptr) voidptr, arg voidptr) int
	fn C.pthread_join(thread C.pthread_t, retval voidptr) int
	fn C.pthread_attr_init(attr voidptr) int
	fn C.pthread_attr_setstacksize(attr voidptr, stacksize usize) int
	fn C.pthread_attr_destroy(attr voidptr) int

	fn flat_cgen_chunk_thread(arg voidptr) voidptr {
		a := unsafe { &FlatCgenChunkArgs(arg) }
		mut w := unsafe { &FlatGen(a.worker) }
		items := unsafe { &[]FlatFnGenItem(a.work_items_ptr) }
		w.gen_fn_items(*items)
		return unsafe { nil }
	}
}

fn (mut g FlatGen) gen_fns_dispatch(no_parallel bool) {
	if no_parallel {
		g.gen_fns()
		return
	}
	items := g.collect_fn_gen_items()
	n_items := items.len
	n_jobs := flat_cgen_job_count(runtime.nr_jobs(), n_items)
	$if windows {
		g.gen_fn_items(items)
		return
	} $else {
		if n_items < min_flat_cgen_parallel_items || n_jobs <= 1 {
			g.gen_fn_items(items)
			return
		}
		g.parallel_used = true
		g.prepare_parallel_items(items)
		mut chunk_items := split_flat_cgen_items(items, n_jobs)
		chunk_count := chunk_items.len
		mut thread_ids := []C.pthread_t{len: chunk_count}
		mut args := []FlatCgenChunkArgs{cap: chunk_count}
		mut workers := []voidptr{cap: chunk_count}

		for ci := 0; ci < chunk_count; ci++ {
			w := g.new_parallel_worker(ci)
			workers << voidptr(w)
		}
		for ci := 0; ci < chunk_count; ci++ {
			args << FlatCgenChunkArgs{
				worker:         workers[ci]
				work_items_ptr: unsafe { voidptr(&chunk_items[ci]) }
			}
		}

		attr_buf := [64]u8{}
		attr := unsafe { voidptr(&attr_buf[0]) }
		C.pthread_attr_init(attr)
		C.pthread_attr_setstacksize(attr, 64 * 1024 * 1024)
		for ci := 0; ci < chunk_count; ci++ {
			C.pthread_create(unsafe { &thread_ids[ci] }, attr, flat_cgen_chunk_thread,
				unsafe { voidptr(&args[ci]) })
		}
		C.pthread_attr_destroy(attr)
		for ci := 0; ci < chunk_count; ci++ {
			C.pthread_join(thread_ids[ci], unsafe { nil })
		}
		for ci := 0; ci < chunk_count; ci++ {
			w := unsafe { &FlatGen(workers[ci]) }
			g.merge_parallel_worker(w)
		}
	}
}

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

fn split_flat_cgen_items(items []FlatFnGenItem, n_jobs int) [][]FlatFnGenItem {
	mut chunks := [][]FlatFnGenItem{}
	if n_jobs <= 0 || items.len == 0 {
		return chunks
	}
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

fn (mut g FlatGen) collect_fn_gen_items() []FlatFnGenItem {
	mut items := []FlatFnGenItem{}
	mut cur_module := ''
	mut cur_file := ''
	for i in 0 .. g.a.nodes.len {
		node := g.a.nodes[i]
		kind_id := node_kind_id(node)
		if kind_id == 77 {
			cur_file = node.value
			g.tc.cur_file = cur_file
			cur_module = ''
			g.tc.cur_module = cur_module
			continue
		}
		if kind_id == 73 {
			cur_module = node.value
			g.tc.cur_file = cur_file
			g.tc.cur_module = cur_module
			continue
		}

		if kind_id != 61 {
			continue
		}
		if !g.should_emit_fn_node_in_module(node, i, cur_module) {
			continue
		}
		qfn := qualified_fn_name_in_module(cur_module, node.value)
		if g.emitted_fn_contains(qfn) {
			continue
		}
		g.emitted_fns[qfn] = true
		items << FlatFnGenItem{
			node_id: flat.NodeId(i)
			file:    cur_file
			module:  cur_module
			cost:    node.children_count + 1
		}
	}
	return items
}

fn (mut g FlatGen) gen_fn_items(items []FlatFnGenItem) {
	for item in items {
		if int(item.node_id) < 0 || int(item.node_id) >= g.a.nodes.len {
			continue
		}
		g.tc.cur_file = item.file
		g.tc.cur_module = item.module
		node := g.a.nodes[int(item.node_id)]
		g.gen_fn_in_module(node, item.module)
	}
}

fn (mut g FlatGen) prepare_parallel_items(items []FlatFnGenItem) {
	for item in items {
		g.tc.cur_file = item.file
		g.tc.cur_module = item.module
		g.prepare_parallel_node(item.node_id)
	}
	g.register_interface_strings()
}

fn (mut g FlatGen) prepare_parallel_node(id flat.NodeId) {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return
	}
	node := g.a.nodes[int(id)]
	if node.kind == .string_literal {
		g.intern_string(node.value)
	}
	if g.should_preseed_parallel_type_text(node.typ) {
		g.preseed_parallel_fn_ptr_type(g.tc.parse_type(node.typ))
	}
	if expr_type := g.tc.expr_type(id) {
		g.preseed_parallel_fn_ptr_type(expr_type)
	}
	for i in 0 .. node.children_count {
		g.prepare_parallel_node(g.a.child(&node, i))
	}
}

fn (g &FlatGen) should_preseed_parallel_type_text(typ string) bool {
	if typ.len == 0 {
		return false
	}
	clean := g.parallel_base_type_text(typ)
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		return true
	}
	if clean in g.tc.type_aliases {
		return true
	}
	qtyp := g.tc.qualify_name(clean)
	return qtyp in g.tc.type_aliases
}

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

fn (g &FlatGen) new_parallel_worker(worker_id int) &FlatGen {
	return &FlatGen{
		sb:                      strings.new_builder(64_000)
		a:                       unsafe { g.a }
		used_fns:                g.used_fns.clone()
		used_fn_names:           g.used_fn_names.clone()
		str_lits:                g.str_lits.clone()
		str_lit_ids:             g.str_lit_ids.clone()
		global_types:            g.global_types.clone()
		enum_vals:               g.enum_vals.clone()
		interfaces:              g.interfaces.clone()
		const_vals:              g.const_vals.clone()
		const_modules:           g.const_modules.clone()
		const_init_order:        g.const_init_order.clone()
		global_modules:          g.global_modules.clone()
		global_inits:            g.global_inits.clone()
		global_init_order:       g.global_init_order.clone()
		iface_impls:             g.iface_impls.clone()
		iface_type_ids:          g.iface_type_ids.clone()
		module_init_fns:         g.module_init_fns.clone()
		module_init_fn_modules:  g.module_init_fn_modules.clone()
		module_imports:          g.module_imports.clone()
		tc:                      g.clone_parallel_type_checker()
		has_builtins:            g.has_builtins
		tmp_count:               (worker_id + 1) * 100_000
		line_start:              true
		modules:                 g.modules.clone()
		fn_ptr_types:            g.fn_ptr_types.clone()
		fn_decl_param_types:     g.fn_decl_param_types.clone()
		struct_decl_infos:       g.struct_decl_infos.clone()
		struct_decl_short_infos: g.struct_decl_short_infos.clone()
		runtime_inits:           g.runtime_inits.clone()
		compiler_vroot:          g.compiler_vroot
		cur_param_names:         g.cur_param_names.clone()
		cur_param_type_values:   g.cur_param_type_values.clone()
		cur_param_types:         g.cur_param_types.clone()
		cur_fn_ret:              g.cur_fn_ret
		cur_fn_ret_is_optional:  g.cur_fn_ret_is_optional
		cur_fn_ret_base:         g.cur_fn_ret_base
		expected_expr_type:      g.expected_expr_type
		expected_enum:           g.expected_enum
		needed_optional_types:   g.needed_optional_types.clone()
		emitted_optional_types:  g.emitted_optional_types.clone()
		emitted_fns:             g.emitted_fns.clone()
		array_method_cache:      g.array_method_cache.clone()
	}
}

fn (g &FlatGen) clone_parallel_type_checker() &types.TypeChecker {
	mut fs := types.new_scope(unsafe { nil })
	fs.names = g.tc.file_scope.names.clone()
	fs.types = g.tc.file_scope.types.clone()
	return &types.TypeChecker{
		a:                             unsafe { g.tc.a }
		fn_ret_types:                  g.tc.fn_ret_types.clone()
		fn_param_types:                g.tc.fn_param_types.clone()
		fn_variadic:                   g.tc.fn_variadic.clone()
		structs:                       g.tc.structs.clone()
		unions:                        g.tc.unions.clone()
		type_aliases:                  g.tc.type_aliases.clone()
		sum_types:                     g.tc.sum_types.clone()
		enum_names:                    g.tc.enum_names.clone()
		enum_fields:                   g.tc.enum_fields.clone()
		flag_enums:                    g.tc.flag_enums.clone()
		interface_names:               g.tc.interface_names.clone()
		interface_fields:              g.tc.interface_fields.clone()
		interface_embeds:              g.tc.interface_embeds.clone()
		interface_abstract_methods:    g.tc.interface_abstract_methods.clone()
		c_globals:                     g.tc.c_globals.clone()
		const_types:                   g.tc.const_types.clone()
		const_exprs:                   g.tc.const_exprs.clone()
		const_modules:                 g.tc.const_modules.clone()
		const_suffixes:                g.tc.const_suffixes.clone()
		imports:                       g.tc.imports.clone()
		file_imports:                  g.tc.file_imports.clone()
		file_modules:                  g.tc.file_modules.clone()
		file_scope:                    fs
		cur_scope:                     fs
		scope_pool:                    []&types.Scope{}
		has_builtins:                  g.tc.has_builtins
		cur_module:                    g.tc.cur_module
		cur_file:                      g.tc.cur_file
		errors:                        g.tc.errors.clone()
		resolved_call_names:           g.tc.resolved_call_names.clone()
		resolved_call_set:             g.tc.resolved_call_set.clone()
		expr_type_values:              g.tc.expr_type_values.clone()
		expr_type_set:                 g.tc.expr_type_set.clone()
		checking_nodes:                g.tc.checking_nodes.clone()
		diagnose_unknown_calls:        g.tc.diagnose_unknown_calls
		reject_unlowered_map_mutation: g.tc.reject_unlowered_map_mutation
		diagnostic_files:              g.tc.diagnostic_files.clone()
		cur_fn_ret_type:               g.tc.cur_fn_ret_type
		smartcasts:                    g.tc.smartcasts.clone()
	}
}

fn (mut g FlatGen) merge_parallel_worker(w &FlatGen) {
	mut ww := unsafe { w }
	worker_output := ww.sb.str()
	if worker_output.len > 0 {
		g.sb.write_string(worker_output)
	}
	for opt_name, val_type in w.needed_optional_types {
		g.needed_optional_types[opt_name] = val_type
	}
	for encoded, name in w.fn_ptr_types {
		if encoded !in g.fn_ptr_types {
			g.fn_ptr_types[encoded] = name
		}
	}
}
