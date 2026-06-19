// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module cleanc

import runtime
import strings
import time
import v2.ast
import v2.token
import v2.types

const max_generic_struct_scan_jobs = 4

$if !windows {
	struct GenericStructScanChunkArgs {
		worker           voidptr // &Gen - pre-cloned worker
		file_indices_ptr voidptr // &[]int - worker-owned full-file indexes
	}

	@[typedef]
	struct C.pthread_t {}

	fn C.pthread_create(thread &C.pthread_t, attr voidptr, start_routine fn (voidptr) voidptr, arg voidptr) int
	fn C.pthread_join(thread C.pthread_t, retval voidptr) int
	fn C.pthread_attr_init(attr voidptr) int
	fn C.pthread_attr_setstacksize(attr voidptr, stacksize usize) int
	fn C.pthread_attr_destroy(attr voidptr) int

	fn generic_struct_scan_chunk_thread(arg voidptr) voidptr {
		a := unsafe { &GenericStructScanChunkArgs(arg) }
		mut w := unsafe { &Gen(a.worker) }
		indices := unsafe { &[]int(a.file_indices_ptr) }
		w.collect_generic_struct_bindings_file_indices(*indices)
		return unsafe { nil }
	}
}

fn (mut g Gen) has_explicit_str_method_for_c_type(c_type_name string) bool {
	old_file := g.cur_file_name
	old_module := g.cur_module
	old_import_modules := g.cur_import_modules.clone()
	if g.has_flat() {
		for i in 0 .. g.flat.files.len {
			fc := g.flat.file_cursor(i)
			g.set_file_cursor_module(fc)
			stmts := fc.stmts()
			for j in 0 .. stmts.len() {
				stmt := stmts.at(j)
				if stmt.kind() == .stmt_fn_decl && stmt.name() == 'str' {
					decl := stmt.fn_decl_signature()
					if decl.is_method
						&& g.method_decl_c_name_for_module(g.cur_module, decl) == '${c_type_name}__str' {
						g.cur_file_name = old_file
						g.cur_module = old_module
						g.cur_import_modules = old_import_modules.clone()
						return true
					}
				}
			}
		}
	} else {
		for file in g.files {
			g.set_file_module(file)
			for stmt in file.stmts {
				if stmt is ast.FnDecl {
					if stmt.is_method && stmt.name == 'str'
						&& g.method_decl_c_name_for_module(g.cur_module, stmt) == '${c_type_name}__str' {
						g.cur_file_name = old_file
						g.cur_module = old_module
						g.cur_import_modules = old_import_modules.clone()
						return true
					}
				}
			}
		}
	}
	g.cur_file_name = old_file
	g.cur_module = old_module
	g.cur_import_modules = old_import_modules.clone()
	return false
}

// collect_generic_struct_bindings scans all struct fields for GenericType
// instantiations (e.g. LinkedList[ValueInfo]) and records the concrete type
// bindings so that methods on generic structs can resolve their generic params.
fn (mut g Gen) collect_generic_struct_bindings() {
	stats_enabled := g.cgen_stats_enabled()
	stats_scope := g.cgen_stats_scope_label()
	mut stats_sw := time.new_stopwatch()
	mut stage_start := stats_sw.elapsed()
	prev_active_generic_types := g.active_generic_types.clone()
	prev_runtime_local_types := g.runtime_local_types.clone()
	prev_runtime_decl_types := g.runtime_decl_types.clone()
	prev_not_local_var_cache := g.not_local_var_cache.clone()
	prev_is_module_ident_cache := g.is_module_ident_cache.clone()
	prev_resolved_module_names := g.resolved_module_names.clone()
	prev_cur_fn_generic_params := g.cur_fn_generic_params.clone()
	prev_fn_name := g.cur_fn_name
	prev_fn_c_name := g.cur_fn_c_name
	g.active_generic_types = map[string]types.Type{}
	g.runtime_local_types = map[string]string{}
	g.runtime_decl_types = map[string]string{}
	g.not_local_var_cache = map[string]bool{}
	g.is_module_ident_cache = map[string]bool{}
	g.resolved_module_names = map[string]string{}
	g.cur_fn_generic_params = map[string]string{}
	g.cur_fn_name = ''
	g.cur_fn_c_name = ''
	defer {
		g.active_generic_types = prev_active_generic_types.clone()
		g.runtime_local_types = prev_runtime_local_types.clone()
		g.runtime_decl_types = prev_runtime_decl_types.clone()
		g.not_local_var_cache = prev_not_local_var_cache.clone()
		g.is_module_ident_cache = prev_is_module_ident_cache.clone()
		g.resolved_module_names = prev_resolved_module_names.clone()
		g.cur_fn_generic_params = prev_cur_fn_generic_params.clone()
		g.cur_fn_name = prev_fn_name
		g.cur_fn_c_name = prev_fn_c_name
	}
	if g.has_flat() {
		g.collect_generic_struct_bindings_flat()
	} else if !g.collect_generic_struct_bindings_file_scan_parallel() {
		g.collect_generic_struct_bindings_file_scan(g.files)
	}
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'setup.generic_struct_bindings.file_scan')
	if g.has_flat() {
		g.propagate_generic_struct_bindings_flat()
	} else {
		g.propagate_generic_struct_bindings_for_files(g.files)
	}
	stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'setup.generic_struct_bindings.propagate')
	if g.has_flat() {
		g.scan_receiver_generic_struct_bindings_flat()
	} else {
		g.scan_receiver_generic_struct_bindings_for_files(g.files)
	}
	_ = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
		'setup.generic_struct_bindings.receiver_scan')
}

fn (mut g Gen) collect_generic_struct_bindings_flat() {
	for i in 0 .. g.flat.files.len {
		fc := g.flat.file_cursor(i)
		g.set_file_cursor_module(fc)
		stmts := fc.stmts()
		for j in 0 .. stmts.len() {
			stmt := stmts.at(j)
			match stmt.kind() {
				.stmt_struct_decl {
					decl := stmt.struct_decl()
					for field in decl.fields {
						g.scan_expr_for_generic_types(field.typ)
					}
				}
				.stmt_fn_decl {
					decl := stmt.fn_decl_signature()
					if decl.receiver.typ !is ast.EmptyExpr {
						g.scan_expr_for_generic_types(decl.receiver.typ)
					}
					for param in decl.typ.params {
						g.scan_expr_for_generic_types(param.typ)
					}
					if decl.typ.return_type !is ast.EmptyExpr {
						g.scan_expr_for_generic_types(decl.typ.return_type)
					}
					if stmt.list_at(3).len() > 0 {
						g.scan_fn_body_cursor_for_generic_types_with_clean_locals(stmt, decl, '')
					}
				}
				else {}
			}
		}
	}
}

fn (mut g Gen) collect_generic_struct_bindings_file_scan(files []ast.File) {
	for file in files {
		g.collect_generic_struct_bindings_file(file)
	}
}

fn (mut g Gen) collect_generic_struct_bindings_file_indices(file_indices []int) {
	for file_idx in file_indices {
		if file_idx < 0 || file_idx >= g.files.len {
			continue
		}
		g.collect_generic_struct_bindings_file(g.files[file_idx])
	}
}

fn (mut g Gen) collect_generic_struct_bindings_file(file ast.File) {
	g.set_file_module(file)
	for stmt in file.stmts {
		if stmt is ast.StructDecl {
			for field in stmt.fields {
				g.scan_expr_for_generic_types(field.typ)
			}
		}
		// Also scan function bodies for generic type references
		// (e.g. LinkedList[StructFieldInfo]{} in decode_value)
		if stmt is ast.FnDecl {
			if stmt.receiver.typ !is ast.EmptyExpr {
				g.scan_expr_for_generic_types(stmt.receiver.typ)
			}
			for param in stmt.typ.params {
				g.scan_expr_for_generic_types(param.typ)
			}
			if stmt.typ.return_type !is ast.EmptyExpr {
				g.scan_expr_for_generic_types(stmt.typ.return_type)
			}
			g.scan_fn_body_for_generic_types_with_clean_locals(stmt, '')
		}
	}
}

fn (mut g Gen) propagate_generic_struct_bindings_for_files(files []ast.File) {
	// Propagation pass: for each generic struct with recorded bindings,
	// look for nested generic type references (e.g. Node[T] inside
	// LinkedList[T]) and record concrete bindings by substituting the
	// parent struct's known bindings for placeholder params.
	for file in files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if stmt is ast.StructDecl {
				if stmt.generic_params.len == 0 {
					continue
				}
				struct_c_name := g.get_struct_name(stmt)
				if struct_c_name !in g.generic_struct_bindings {
					continue
				}
				// Propagate for ALL instances (not just the primary binding)
				instances := g.generic_struct_instances[struct_c_name]
				for inst in instances {
					for field in stmt.fields {
						g.propagate_generic_bindings(field.typ, inst.bindings)
					}
				}
			}
		}
	}
}

fn (mut g Gen) propagate_generic_struct_bindings_flat() {
	for i in 0 .. g.flat.files.len {
		fc := g.flat.file_cursor(i)
		g.set_file_cursor_module(fc)
		stmts := fc.stmts()
		for j in 0 .. stmts.len() {
			stmt := stmts.at(j)
			if stmt.kind() != .stmt_struct_decl {
				continue
			}
			decl := stmt.struct_decl()
			if decl.generic_params.len == 0 {
				continue
			}
			struct_c_name := g.get_struct_name(decl)
			if struct_c_name !in g.generic_struct_bindings {
				continue
			}
			instances := g.generic_struct_instances[struct_c_name]
			for inst in instances {
				for field in decl.fields {
					g.propagate_generic_bindings(field.typ, inst.bindings)
				}
			}
		}
	}
}

fn (mut g Gen) scan_receiver_generic_struct_bindings_for_files(files []ast.File) {
	// Generic receiver methods can contain explicit calls like
	// `helper[T](...)` even when the receiver itself is emitted with v2's
	// fallback placeholder type. Rescan those bodies with concrete receiver
	// bindings. Uninstantiated receiver-generic methods should not invent
	// concrete helper calls from unrelated generic bindings.
	for file in files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if stmt is ast.FnDecl {
				recv_params := receiver_generic_param_names(stmt)
				if recv_params.len == 0 {
					continue
				}
				mut binding_sets := []map[string]types.Type{}
				all_bindings := g.get_all_receiver_generic_bindings(stmt)
				if all_bindings.len > 0 {
					binding_sets << all_bindings
				} else if bindings := g.get_receiver_generic_bindings(stmt) {
					binding_sets << bindings
				}
				if binding_sets.len == 0 {
					continue
				}
				prev_generic_types := g.active_generic_types.clone()
				for bindings in binding_sets {
					g.active_generic_types = bindings.clone()
					g.scan_fn_body_for_generic_types_with_clean_locals(stmt, '')
				}
				g.active_generic_types = prev_generic_types.clone()
			}
		}
	}
}

fn (mut g Gen) scan_receiver_generic_struct_bindings_flat() {
	for i in 0 .. g.flat.files.len {
		fc := g.flat.file_cursor(i)
		g.set_file_cursor_module(fc)
		stmts := fc.stmts()
		for j in 0 .. stmts.len() {
			stmt := stmts.at(j)
			if stmt.kind() != .stmt_fn_decl {
				continue
			}
			decl := stmt.fn_decl_signature()
			recv_params := receiver_generic_param_names(decl)
			if recv_params.len == 0 {
				continue
			}
			mut binding_sets := []map[string]types.Type{}
			all_bindings := g.get_all_receiver_generic_bindings(decl)
			if all_bindings.len > 0 {
				binding_sets << all_bindings
			} else if bindings := g.get_receiver_generic_bindings(decl) {
				binding_sets << bindings
			}
			if binding_sets.len == 0 {
				continue
			}
			prev_generic_types := g.active_generic_types.clone()
			for bindings in binding_sets {
				g.active_generic_types = bindings.clone()
				g.scan_fn_body_cursor_for_generic_types_with_clean_locals(stmt, decl, '')
			}
			g.active_generic_types = prev_generic_types.clone()
		}
	}
}

fn (mut g Gen) collect_generic_struct_bindings_file_scan_parallel() bool {
	if g.pref == unsafe { nil } || g.pref.no_parallel || g.files.len < 2 {
		return false
	}
	$if windows {
		return false
	} $else {
		stats_enabled := g.cgen_stats_enabled()
		stats_scope := g.cgen_stats_scope_label()
		mut stats_sw := time.new_stopwatch()
		mut stage_start := stats_sw.elapsed()
		n_files := g.files.len
		n_runtime_jobs := runtime.nr_jobs()
		n_jobs := generic_struct_scan_job_count(n_runtime_jobs, n_files)
		if n_jobs <= 1 {
			return false
		}

		mut thread_ids := []C.pthread_t{len: n_jobs}
		mut args := []GenericStructScanChunkArgs{cap: n_jobs}
		mut workers := []voidptr{cap: n_jobs}
		mut chunk_indices := [][]int{cap: n_jobs}
		for ci := 0; ci < n_jobs; ci++ {
			start := ci * n_files / n_jobs
			end := (ci + 1) * n_files / n_jobs
			mut indices := []int{cap: end - start}
			for file_idx := start; file_idx < end; file_idx++ {
				indices << file_idx
			}
			chunk_indices << indices
			w := g.new_generic_struct_scan_worker(ci)
			workers << voidptr(w)
		}
		stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
			'setup.generic_struct_bindings.worker_setup')
		for ci := 0; ci < n_jobs; ci++ {
			args << GenericStructScanChunkArgs{
				worker:           workers[ci]
				file_indices_ptr: unsafe { voidptr(&chunk_indices[ci]) }
			}
		}

		attr_buf := [64]u8{}
		attr := unsafe { voidptr(&attr_buf[0]) }
		C.pthread_attr_init(attr)
		C.pthread_attr_setstacksize(attr, 64 * 1024 * 1024)
		for ci := 0; ci < n_jobs; ci++ {
			C.pthread_create(unsafe { &thread_ids[ci] }, attr, generic_struct_scan_chunk_thread,
				unsafe { voidptr(&args[ci]) })
		}
		C.pthread_attr_destroy(attr)
		for ci := 0; ci < n_jobs; ci++ {
			C.pthread_join(thread_ids[ci], unsafe { nil })
		}
		stage_start = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
			'setup.generic_struct_bindings.worker_run')
		for ci := 0; ci < n_jobs; ci++ {
			w := unsafe { &Gen(workers[ci]) }
			g.merge_generic_struct_scan_worker(w)
		}
		_ = g.mark_cgen_step(stats_enabled, stats_scope, mut stats_sw, stage_start,
			'setup.generic_struct_bindings.worker_merge')
		return true
	}
}

fn generic_struct_scan_job_count(n_runtime_jobs int, n_files int) int {
	if n_runtime_jobs <= 0 || n_files <= 0 {
		return 0
	}
	mut n_jobs := n_runtime_jobs
	if n_jobs > max_generic_struct_scan_jobs {
		n_jobs = max_generic_struct_scan_jobs
	}
	if n_jobs > n_files {
		n_jobs = n_files
	}
	// Worker Gen clones are sizeable; avoid spending more time cloning than scanning
	// when the current bundle is already small because .vh caches did most work.
	if n_files < n_jobs * 4 {
		n_jobs = if n_files >= 4 { n_files / 4 } else { 1 }
	}
	return n_jobs
}

fn (g &Gen) new_generic_struct_scan_worker(worker_id int) &Gen {
	mut w := g.new_pass5_worker([], worker_id)
	w.generic_body_scan_cache = g.generic_body_scan_cache.clone()
	w.generic_spec_index = clone_generic_spec_index(g.generic_spec_index)
	w.late_generic_specs = clone_late_generic_specs(g.late_generic_specs)
	w.generic_struct_bindings = clone_generic_struct_bindings(g.generic_struct_bindings)
	w.generic_struct_instances = clone_generic_struct_instances(g.generic_struct_instances)
	w.active_generic_types = map[string]types.Type{}
	w.runtime_local_types = map[string]string{}
	w.runtime_decl_types = map[string]string{}
	w.not_local_var_cache = map[string]bool{}
	w.is_module_ident_cache = map[string]bool{}
	w.resolved_module_names = map[string]string{}
	w.cur_fn_generic_params = map[string]string{}
	w.cur_fn_name = ''
	w.cur_fn_c_name = ''
	return w
}

fn (mut g Gen) merge_generic_struct_scan_worker(w &Gen) {
	for struct_c_name, instances in w.generic_struct_instances {
		for inst in instances {
			g.merge_generic_struct_scan_instance(struct_c_name, inst)
		}
	}
	for key, bindings in w.generic_struct_bindings {
		if key !in g.generic_struct_bindings && key !in g.generic_struct_instances {
			g.generic_struct_bindings[key] = bindings.clone()
		}
	}
	for key, specs in w.late_generic_specs {
		for bindings in specs {
			g.record_late_generic_call_spec_unchecked(key, bindings)
		}
	}
	for key, scanned in w.generic_body_scan_cache {
		if scanned {
			g.generic_body_scan_cache[key] = true
		}
	}
	g.merge_generic_setup_alias_maps(w)
}

fn (mut g Gen) merge_generic_struct_scan_instance(struct_c_name string, inst GenericStructInstance) {
	mut instances := g.generic_struct_instances[struct_c_name]
	struct_base := if struct_c_name.contains('__') {
		struct_c_name.all_after_last('__')
	} else {
		struct_c_name
	}
	param_names := g.generic_struct_runtime_param_names(struct_base, struct_c_name)
	bindings_key := g.generic_struct_bindings_key(param_names, inst.bindings)
	for existing in instances {
		if g.generic_struct_instance_matches(existing, inst.params_key, bindings_key, param_names) {
			return
		}
	}
	mut c_name := inst.c_name
	if instances.len > 0 && c_name == struct_c_name {
		c_name = '${struct_c_name}_T_${inst.params_key}'
	}
	merged := GenericStructInstance{
		params_key: inst.params_key
		bindings:   inst.bindings.clone()
		c_name:     c_name
	}
	instances << merged
	g.generic_struct_instances[struct_c_name] = instances
	if struct_c_name !in g.generic_struct_bindings {
		g.generic_struct_bindings[struct_c_name] = merged.bindings.clone()
	}
}

fn (mut g Gen) merge_generic_setup_alias_maps(w &Gen) {
	for key, values in w.tuple_aliases {
		if key !in g.tuple_aliases {
			g.tuple_aliases[key] = values.clone()
		}
	}
	for key, value in w.array_aliases {
		if value {
			g.array_aliases[key] = true
		}
	}
	for key, value in w.map_aliases {
		if value {
			g.map_aliases[key] = true
		}
	}
	for key, value in w.result_aliases {
		if value {
			g.result_aliases[key] = true
		}
	}
	for key, value in w.option_aliases {
		if value {
			g.option_aliases[key] = true
		}
	}
	for key, value in w.collected_fixed_array_types {
		if key !in g.collected_fixed_array_types {
			g.collected_fixed_array_types[key] = value
		}
	}
	for key, value in w.collected_map_types {
		if key !in g.collected_map_types {
			g.collected_map_types[key] = value
		}
	}
}

fn (mut g Gen) scan_fn_body_for_generic_types_with_clean_locals(node ast.FnDecl, spec_name string) {
	prev_runtime_local_types := g.runtime_local_types.clone()
	prev_runtime_decl_types := g.runtime_decl_types.clone()
	prev_not_local_var_cache := g.not_local_var_cache.clone()
	prev_is_module_ident_cache := g.is_module_ident_cache.clone()
	prev_resolved_module_names := g.resolved_module_names.clone()
	prev_cur_fn_generic_params := g.cur_fn_generic_params.clone()
	prev_fn_name := g.cur_fn_name
	prev_fn_c_name := g.cur_fn_c_name
	g.runtime_local_types = map[string]string{}
	g.runtime_decl_types = map[string]string{}
	g.not_local_var_cache = map[string]bool{}
	g.is_module_ident_cache = map[string]bool{}
	g.resolved_module_names = map[string]string{}
	g.cur_fn_generic_params = map[string]string{}
	g.cur_fn_name = ''
	g.cur_fn_c_name = ''
	g.scan_fn_body_for_generic_types(node, spec_name)
	g.runtime_local_types = prev_runtime_local_types.clone()
	g.runtime_decl_types = prev_runtime_decl_types.clone()
	g.not_local_var_cache = prev_not_local_var_cache.clone()
	g.is_module_ident_cache = prev_is_module_ident_cache.clone()
	g.resolved_module_names = prev_resolved_module_names.clone()
	g.cur_fn_generic_params = prev_cur_fn_generic_params.clone()
	g.cur_fn_name = prev_fn_name
	g.cur_fn_c_name = prev_fn_c_name
}

fn (mut g Gen) scan_fn_body_cursor_for_generic_types_with_clean_locals(stmt ast.Cursor, decl ast.FnDecl, spec_name string) {
	prev_runtime_local_types := g.runtime_local_types.clone()
	prev_runtime_decl_types := g.runtime_decl_types.clone()
	prev_not_local_var_cache := g.not_local_var_cache.clone()
	prev_is_module_ident_cache := g.is_module_ident_cache.clone()
	prev_resolved_module_names := g.resolved_module_names.clone()
	prev_cur_fn_generic_params := g.cur_fn_generic_params.clone()
	prev_fn_name := g.cur_fn_name
	prev_fn_c_name := g.cur_fn_c_name
	g.runtime_local_types = map[string]string{}
	g.runtime_decl_types = map[string]string{}
	g.not_local_var_cache = map[string]bool{}
	g.is_module_ident_cache = map[string]bool{}
	g.resolved_module_names = map[string]string{}
	g.cur_fn_generic_params = map[string]string{}
	g.cur_fn_name = ''
	g.cur_fn_c_name = ''
	g.scan_fn_body_cursor_for_generic_types(stmt, decl, spec_name)
	g.runtime_local_types = prev_runtime_local_types.clone()
	g.runtime_decl_types = prev_runtime_decl_types.clone()
	g.not_local_var_cache = prev_not_local_var_cache.clone()
	g.is_module_ident_cache = prev_is_module_ident_cache.clone()
	g.resolved_module_names = prev_resolved_module_names.clone()
	g.cur_fn_generic_params = prev_cur_fn_generic_params.clone()
	g.cur_fn_name = prev_fn_name
	g.cur_fn_c_name = prev_fn_c_name
}

fn clone_tuple_aliases(src map[string][]string) map[string][]string {
	mut out := map[string][]string{}
	for key, values in src {
		out[key] = values.clone()
	}
	return out
}

fn clone_generic_spec_index(src map[string][]string) map[string][]string {
	mut out := map[string][]string{}
	for key, values in src {
		out[key] = values.clone()
	}
	return out
}

fn clone_late_generic_specs(src map[string][]map[string]types.Type) map[string][]map[string]types.Type {
	mut out := map[string][]map[string]types.Type{}
	for key, specs in src {
		mut cloned_specs := []map[string]types.Type{cap: specs.len}
		for spec in specs {
			cloned_specs << spec.clone()
		}
		out[key] = cloned_specs
	}
	return out
}

fn clone_generic_struct_bindings(src map[string]map[string]types.Type) map[string]map[string]types.Type {
	mut out := map[string]map[string]types.Type{}
	for key, bindings in src {
		out[key] = bindings.clone()
	}
	return out
}

fn clone_generic_struct_instances(src map[string][]GenericStructInstance) map[string][]GenericStructInstance {
	mut out := map[string][]GenericStructInstance{}
	for key, instances in src {
		mut cloned_instances := []GenericStructInstance{cap: instances.len}
		for inst in instances {
			cloned_instances << GenericStructInstance{
				params_key: inst.params_key
				bindings:   inst.bindings.clone()
				c_name:     inst.c_name
			}
		}
		out[key] = cloned_instances
	}
	return out
}

fn (s &GenericSetupSnapshot) clone() GenericSetupSnapshot {
	return GenericSetupSnapshot{
		ready:                       s.ready
		tuple_aliases:               clone_tuple_aliases(s.tuple_aliases)
		array_aliases:               s.array_aliases.clone()
		map_aliases:                 s.map_aliases.clone()
		result_aliases:              s.result_aliases.clone()
		option_aliases:              s.option_aliases.clone()
		collected_fixed_array_types: s.collected_fixed_array_types.clone()
		collected_map_types:         s.collected_map_types.clone()
		generic_body_scan_cache:     s.generic_body_scan_cache.clone()
		generic_spec_index:          clone_generic_spec_index(s.generic_spec_index)
		late_generic_specs:          clone_late_generic_specs(s.late_generic_specs)
		generic_struct_bindings:     clone_generic_struct_bindings(s.generic_struct_bindings)
		generic_struct_instances:    clone_generic_struct_instances(s.generic_struct_instances)
	}
}

pub fn (mut g Gen) use_generic_setup_snapshot(snapshot GenericSetupSnapshot) {
	g.generic_setup_snapshot = snapshot.clone()
	g.has_generic_setup_snapshot = snapshot.ready
}

pub fn (g &Gen) generic_setup_snapshot() GenericSetupSnapshot {
	return GenericSetupSnapshot{
		ready:                       true
		tuple_aliases:               clone_tuple_aliases(g.tuple_aliases)
		array_aliases:               g.array_aliases.clone()
		map_aliases:                 g.map_aliases.clone()
		result_aliases:              g.result_aliases.clone()
		option_aliases:              g.option_aliases.clone()
		collected_fixed_array_types: g.collected_fixed_array_types.clone()
		collected_map_types:         g.collected_map_types.clone()
		generic_body_scan_cache:     g.generic_body_scan_cache.clone()
		generic_spec_index:          clone_generic_spec_index(g.generic_spec_index)
		late_generic_specs:          clone_late_generic_specs(g.late_generic_specs)
		generic_struct_bindings:     clone_generic_struct_bindings(g.generic_struct_bindings)
		generic_struct_instances:    clone_generic_struct_instances(g.generic_struct_instances)
	}
}

fn (mut g Gen) apply_generic_setup_snapshot() {
	snapshot := g.generic_setup_snapshot
	g.tuple_aliases = clone_tuple_aliases(snapshot.tuple_aliases)
	g.array_aliases = snapshot.array_aliases.clone()
	g.map_aliases = snapshot.map_aliases.clone()
	g.result_aliases = snapshot.result_aliases.clone()
	g.option_aliases = snapshot.option_aliases.clone()
	g.collected_fixed_array_types = snapshot.collected_fixed_array_types.clone()
	g.collected_map_types = snapshot.collected_map_types.clone()
	g.generic_body_scan_cache = snapshot.generic_body_scan_cache.clone()
	g.generic_spec_index = clone_generic_spec_index(snapshot.generic_spec_index)
	g.late_generic_specs = clone_late_generic_specs(snapshot.late_generic_specs)
	g.generic_struct_bindings = clone_generic_struct_bindings(snapshot.generic_struct_bindings)
	g.generic_struct_instances = clone_generic_struct_instances(snapshot.generic_struct_instances)
}

// propagate_generic_bindings finds nested generic type references and records
// concrete bindings by substituting placeholder params from parent bindings.
fn (mut g Gen) propagate_generic_bindings(e ast.Expr, parent_bindings map[string]types.Type) {
	match e {
		ast.Type {
			if e is ast.GenericType {
				gt := e as ast.GenericType
				base_name := g.expr_type_to_c(gt.name)
				if gt.params.len > 0 {
					// Check if any param is a placeholder that can be resolved
					// via parent_bindings.
					mut all_concrete := true
					for param in gt.params {
						pname := param.name()
						if is_generic_placeholder_type_name(pname) && pname !in parent_bindings {
							all_concrete = false
							break
						}
					}
					if all_concrete && !g.generic_call_spec_scan_only {
						struct_base := if base_name.contains('__') {
							base_name.all_after_last('__')
						} else {
							base_name
						}
						g.record_generic_struct_bindings_with_parent(struct_base, base_name,
							gt.params, parent_bindings)
					}
				}
				// Recurse into params
				for param in gt.params {
					g.propagate_generic_bindings(param, parent_bindings)
				}
			}
			if e is ast.PointerType {
				g.propagate_generic_bindings(e.base_type, parent_bindings)
			}
		}
		ast.ModifierExpr {
			g.propagate_generic_bindings(e.expr, parent_bindings)
		}
		ast.PrefixExpr {
			g.propagate_generic_bindings(e.expr, parent_bindings)
		}
		ast.GenericArgOrIndexExpr {
			base_name := g.expr_type_to_c(e.lhs)
			arg_name := e.expr.name()
			if !g.generic_call_spec_scan_only {
				if is_generic_placeholder_type_name(arg_name) && arg_name in parent_bindings {
					struct_base := if base_name.contains('__') {
						base_name.all_after_last('__')
					} else {
						base_name
					}
					g.record_generic_struct_bindings_with_parent(struct_base, base_name, [
						e.expr,
					], parent_bindings)
				} else if !is_generic_placeholder_type_name(arg_name) {
					struct_base := if base_name.contains('__') {
						base_name.all_after_last('__')
					} else {
						base_name
					}
					g.record_generic_struct_bindings(struct_base, base_name, [e.expr])
				}
			}
		}
		ast.GenericArgs {
			base_name := g.expr_type_to_c(e.lhs)
			if e.args.len > 0 && !g.generic_call_spec_scan_only {
				struct_base := if base_name.contains('__') {
					base_name.all_after_last('__')
				} else {
					base_name
				}
				g.record_generic_struct_bindings_with_parent(struct_base, base_name, e.args,
					parent_bindings)
			}
		}
		else {}
	}
}

fn (mut g Gen) scan_expr_for_generic_types(e ast.Expr) {
	match e {
		ast.Ident {
			if e.name.contains('_T_') || e.name.ends_with('_T') {
				g.scan_generic_fn_value_for_specs(e)
				g.record_generic_struct_bindings_from_specialized_name(e.name)
			}
		}
		ast.Type {
			if e is ast.GenericType {
				gt := e as ast.GenericType
				base_name := g.expr_type_to_c(gt.name)
				if gt.params.len > 0 && !g.generic_call_spec_scan_only {
					struct_base := if base_name.contains('__') {
						base_name.all_after_last('__')
					} else {
						base_name
					}
					g.record_generic_struct_bindings(struct_base, base_name, gt.params)
				}
				// Also scan params recursively (e.g. Node[T] inside LinkedList[ValueInfo])
				for param in gt.params {
					g.scan_expr_for_generic_types(param)
				}
			}
			if e is ast.ArrayType {
				g.scan_expr_for_generic_types(e.elem_type)
			}
			if e is ast.MapType {
				g.scan_expr_for_generic_types(e.key_type)
				g.scan_expr_for_generic_types(e.value_type)
			}
			if e is ast.OptionType {
				g.scan_expr_for_generic_types(e.base_type)
			}
			if e is ast.ResultType {
				g.scan_expr_for_generic_types(e.base_type)
			}
			if e is ast.PointerType {
				g.scan_expr_for_generic_types(e.base_type)
			}
		}
		ast.ModifierExpr {
			g.scan_expr_for_generic_types(e.expr)
		}
		ast.PrefixExpr {
			g.scan_expr_for_generic_types(e.expr)
		}
		ast.GenericArgOrIndexExpr {
			// e.g. &Node[ValueInfo] → PrefixExpr { GenericArgOrIndexExpr { Ident("Node"), Ident("ValueInfo") } }
			g.scan_generic_fn_value_for_specs(e)
			base_name := g.expr_type_to_c(e.lhs)
			arg_name := e.expr.name()
			if !g.generic_call_spec_scan_only {
				struct_base := if base_name.contains('__') {
					base_name.all_after_last('__')
				} else {
					base_name
				}
				if is_generic_placeholder_type_name(arg_name) {
					if concrete := g.active_generic_types[arg_name] {
						g.record_generic_struct_bindings(struct_base, base_name, [
							ast.Expr(ast.Ident{
								name: g.types_type_to_c(concrete)
							}),
						])
					}
				} else {
					g.record_generic_struct_bindings(struct_base, base_name, [e.expr])
				}
			}
		}
		ast.GenericArgs {
			g.scan_generic_fn_value_for_specs(e)
			base_name := g.expr_type_to_c(e.lhs)
			if e.args.len > 0 && !g.generic_call_spec_scan_only {
				struct_base := if base_name.contains('__') {
					base_name.all_after_last('__')
				} else {
					base_name
				}
				mut concrete_args := []ast.Expr{cap: e.args.len}
				mut all_concrete := true
				for arg in e.args {
					arg_name := arg.name()
					if is_generic_placeholder_type_name(arg_name) {
						if concrete := g.active_generic_types[arg_name] {
							concrete_args << ast.Expr(ast.Ident{
								name: g.types_type_to_c(concrete)
							})
						} else {
							all_concrete = false
							break
						}
					} else {
						concrete_args << arg
					}
				}
				if all_concrete {
					g.record_generic_struct_bindings(struct_base, base_name, concrete_args)
				}
			}
		}
		ast.CallOrCastExpr {
			// LinkedList[StructFieldInfo]{} is parsed as CallOrCastExpr with
			// lhs = GenericArgOrIndexExpr or GenericArgs
			g.scan_expr_for_generic_types(e.lhs)
			g.scan_expr_for_generic_types(e.expr)
		}
		ast.CallExpr {
			g.scan_expr_for_generic_types(e.lhs)
			for arg in e.args {
				g.scan_expr_for_generic_types(arg)
				g.scan_expr_stmts_for_generic_types(arg)
			}
			g.scan_call_for_generic_fn_specs(e)
		}
		ast.InfixExpr {
			g.scan_expr_for_generic_types(e.lhs)
			g.scan_expr_for_generic_types(e.rhs)
			if e.op == .left_shift {
				g.remember_array_append_lhs_type_for_generic_scan(e.lhs, e.rhs)
			}
		}
		ast.OrExpr {
			g.scan_expr_for_generic_types(e.expr)
		}
		ast.ComptimeExpr {
			if e.expr is ast.IfExpr {
				g.scan_comptime_if_for_generic_types(e.expr)
				return
			}
			g.scan_expr_for_generic_types(e.expr)
		}
		ast.IfExpr {
			g.scan_expr_for_generic_types(e.cond)
		}
		ast.MatchExpr {
			g.scan_expr_for_generic_types(e.expr)
			for branch in e.branches {
				for cond in branch.cond {
					g.scan_expr_for_generic_types(cond)
				}
			}
		}
		ast.Tuple {
			mut elem_types := []string{cap: e.exprs.len}
			for expr in e.exprs {
				g.scan_expr_for_generic_types(expr)
				g.scan_expr_stmts_for_generic_types(expr)
				mut elem_type := g.get_expr_type(expr)
				if elem_type == '' || elem_type == 'int' {
					if raw := g.get_raw_type(expr) {
						elem_type = g.types_type_to_c(raw)
					}
				}
				if elem_type == '' {
					elem_type = 'int'
				}
				elem_types << elem_type
			}
			g.register_tuple_alias(elem_types)
		}
		ast.InitExpr {
			// e.g. LinkedList[StructFieldInfo]{} has .typ = GenericArgs
			g.scan_expr_for_generic_types(e.typ)
			for field in e.fields {
				g.scan_expr_for_generic_types(field.value)
			}
		}
		else {}
	}
}

fn (mut g Gen) record_generic_struct_bindings_from_specialized_name(raw_name string) {
	if !raw_name.contains('_T_') {
		return
	}
	struct_c_name := raw_name.all_before('_T_')
	if struct_c_name == '' {
		return
	}
	runtime_param_names := g.generic_struct_runtime_param_names(struct_c_name, struct_c_name)
	if runtime_param_names.len == 0 {
		return
	}
	arg_tokens := generic_call_embedded_type_arg_names_from_name(raw_name, runtime_param_names.len)
	if arg_tokens.len != runtime_param_names.len {
		return
	}
	mut bindings := map[string]types.Type{}
	mut param_c_names := []string{cap: arg_tokens.len}
	for i, param_name in runtime_param_names {
		concrete_c_name := generic_token_to_c_type(arg_tokens[i])
		if concrete_c_name == '' {
			return
		}
		concrete_type := g.concrete_type_from_call_arg_c_name(concrete_c_name) or { return }
		if type_contains_generic_placeholder(concrete_type)
			|| !g.generic_concrete_type_is_runtime_specializable(concrete_type) {
			return
		}
		bindings[param_name] = concrete_type
		param_c_names << mangle_alias_component(concrete_c_name)
	}
	if bindings.len != runtime_param_names.len
		|| !g.generic_specialization_belongs_to_emit_modules(bindings) {
		return
	}
	params_key := param_c_names.join('_')
	bindings_key := g.generic_struct_bindings_key(runtime_param_names, bindings)
	mut instances := g.generic_struct_instances[struct_c_name]
	for inst in instances {
		if inst.c_name == raw_name
			|| g.generic_struct_instance_matches(inst, params_key, bindings_key, runtime_param_names)
			|| g.generic_struct_instance_bindings_match(inst, bindings, runtime_param_names) {
			return
		}
	}
	instances << GenericStructInstance{
		params_key: params_key
		bindings:   bindings.clone()
		c_name:     raw_name
	}
	g.generic_struct_instances[struct_c_name] = instances
	if struct_c_name !in g.generic_struct_bindings {
		g.generic_struct_bindings[struct_c_name] = bindings.clone()
	}
}

fn (mut g Gen) generic_call_decl_from_lhs(lhs ast.Expr) ?ast.FnDecl {
	mut call_name := match lhs {
		ast.Ident {
			lhs.name
		}
		ast.SelectorExpr {
			lhs.rhs.name
		}
		ast.GenericArgOrIndexExpr {
			return g.generic_call_decl_from_lhs(lhs.lhs)
		}
		ast.GenericArgs {
			return g.generic_call_decl_from_lhs(lhs.lhs)
		}
		else {
			''
		}
	}

	if call_name.contains('_T_') {
		call_name = call_name.all_before('_T_')
	} else if call_name.ends_with('_T') {
		call_name = call_name[..call_name.len - 2]
	}

	if call_name == '' {
		return none
	}
	for candidate in generic_call_decl_candidates(call_name) {
		if info := g.generic_fn_decl_index[candidate] {
			if g.has_flat() {
				if info.file_idx < 0 || info.file_idx >= g.flat.files.len {
					continue
				}
				stmts := g.flat.file_cursor(info.file_idx).stmts()
				if info.stmt_idx < 0 || info.stmt_idx >= stmts.len() {
					continue
				}
				stmt := stmts.at(info.stmt_idx)
				if stmt.kind() == .stmt_fn_decl {
					return stmt.fn_decl_signature()
				}
			} else if info.file_idx >= 0 && info.file_idx < g.files.len {
				file := g.files[info.file_idx]
				if info.stmt_idx >= 0 && info.stmt_idx < file.stmts.len {
					stmt := file.stmts[info.stmt_idx]
					if stmt is ast.FnDecl {
						return stmt
					}
				}
			}
		}
	}
	if g.generic_fn_decl_index.len > 0 {
		return none
	}
	prev_module := g.cur_module
	prev_file_name := g.cur_file_name
	prev_active_generic_types := g.active_generic_types.clone()
	g.active_generic_types = map[string]types.Type{}
	defer {
		g.cur_module = prev_module
		g.cur_file_name = prev_file_name
		g.active_generic_types = prev_active_generic_types.clone()
	}
	if g.has_flat() {
		for i in 0 .. g.flat.files.len {
			fc := g.flat.file_cursor(i)
			g.set_file_cursor_module(fc)
			stmts := fc.stmts()
			for j in 0 .. stmts.len() {
				stmt := stmts.at(j)
				if stmt.kind() != .stmt_fn_decl || stmt.name() != call_name {
					continue
				}
				decl := stmt.fn_decl_signature()
				if g.generic_fn_param_names(decl).len > 0 {
					return decl
				}
			}
		}
		return none
	}
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == call_name
				&& g.generic_fn_param_names(stmt).len > 0 {
				return stmt
			}
		}
	}
	return none
}

fn (mut g Gen) generic_call_decl_from_lhs_cursor(lhs ast.Cursor) ?ast.FnDecl {
	mut call_name := generic_call_short_name_cursor(lhs)
	if call_name == '' {
		return none
	}
	for candidate in generic_call_decl_candidates(call_name) {
		if info := g.generic_fn_decl_index[candidate] {
			if g.has_flat() {
				if info.file_idx < 0 || info.file_idx >= g.flat.files.len {
					continue
				}
				stmts := g.flat.file_cursor(info.file_idx).stmts()
				if info.stmt_idx < 0 || info.stmt_idx >= stmts.len() {
					continue
				}
				stmt := stmts.at(info.stmt_idx)
				if stmt.kind() == .stmt_fn_decl {
					return stmt.fn_decl_signature()
				}
			} else if info.file_idx >= 0 && info.file_idx < g.files.len {
				file := g.files[info.file_idx]
				if info.stmt_idx >= 0 && info.stmt_idx < file.stmts.len {
					stmt := file.stmts[info.stmt_idx]
					if stmt is ast.FnDecl {
						return stmt
					}
				}
			}
		}
	}
	if g.generic_fn_decl_index.len > 0 {
		return none
	}
	prev_module := g.cur_module
	prev_file_name := g.cur_file_name
	prev_active_generic_types := g.active_generic_types.clone()
	g.active_generic_types = map[string]types.Type{}
	defer {
		g.cur_module = prev_module
		g.cur_file_name = prev_file_name
		g.active_generic_types = prev_active_generic_types.clone()
	}
	if g.has_flat() {
		for i in 0 .. g.flat.files.len {
			fc := g.flat.file_cursor(i)
			g.set_file_cursor_module(fc)
			stmts := fc.stmts()
			for j in 0 .. stmts.len() {
				stmt := stmts.at(j)
				if stmt.kind() != .stmt_fn_decl || stmt.name() != call_name {
					continue
				}
				decl := stmt.fn_decl_signature()
				if g.generic_fn_param_names(decl).len > 0 {
					return decl
				}
			}
		}
		return none
	}
	for file in g.files {
		g.set_file_module(file)
		for stmt in file.stmts {
			if stmt is ast.FnDecl && stmt.name == call_name
				&& g.generic_fn_param_names(stmt).len > 0 {
				return stmt
			}
		}
	}
	return none
}

fn generic_call_decl_candidates(call_name string) []string {
	mut candidates := []string{cap: 3}
	if call_name != '' {
		candidates << call_name
	}
	sanitized := sanitize_fn_ident(call_name)
	if sanitized != '' && sanitized !in candidates {
		candidates << sanitized
	}
	if call_name.contains('__') {
		short_name := call_name.all_after_last('__')
		if short_name != '' && short_name !in candidates {
			candidates << short_name
		}
	}
	return candidates
}

fn generic_call_short_name(lhs ast.Expr) string {
	mut name := match lhs {
		ast.Ident {
			lhs.name
		}
		ast.SelectorExpr {
			lhs.rhs.name
		}
		ast.GenericArgOrIndexExpr {
			generic_call_short_name(lhs.lhs)
		}
		ast.GenericArgs {
			generic_call_short_name(lhs.lhs)
		}
		else {
			''
		}
	}

	if name.contains('_T_') {
		name = name.all_before('_T_')
	} else if name.ends_with('_T') {
		name = name[..name.len - 2]
	}
	return name
}

fn generic_call_short_name_cursor(lhs ast.Cursor) string {
	mut name := match lhs.kind() {
		.expr_ident {
			lhs.name()
		}
		.expr_selector {
			lhs.edge(1).name()
		}
		.expr_generic_arg_or_index, .expr_generic_args {
			generic_call_short_name_cursor(lhs.edge(0))
		}
		else {
			''
		}
	}

	if name.contains('_T_') {
		name = name.all_before('_T_')
	} else if name.ends_with('_T') {
		name = name[..name.len - 2]
	}
	return name
}

fn generic_call_raw_name(lhs ast.Expr) string {
	return match lhs {
		ast.Ident {
			lhs.name
		}
		ast.SelectorExpr {
			lhs.rhs.name
		}
		ast.GenericArgOrIndexExpr {
			generic_call_raw_name(lhs.lhs)
		}
		ast.GenericArgs {
			generic_call_raw_name(lhs.lhs)
		}
		else {
			''
		}
	}
}

fn generic_call_raw_name_cursor(lhs ast.Cursor) string {
	return match lhs.kind() {
		.expr_ident {
			lhs.name()
		}
		.expr_selector {
			lhs.edge(1).name()
		}
		.expr_generic_arg_or_index, .expr_generic_args {
			generic_call_raw_name_cursor(lhs.edge(0))
		}
		else {
			''
		}
	}
}

fn generic_call_type_args(lhs ast.Expr) []ast.Expr {
	return match lhs {
		ast.GenericArgOrIndexExpr {
			if lhs.expr is ast.LifetimeExpr {
				[]ast.Expr{}
			} else {
				[lhs.expr]
			}
		}
		ast.GenericArgs {
			runtime_generic_args(lhs.args)
		}
		else {
			[]ast.Expr{}
		}
	}
}

fn generic_call_type_args_cursor(lhs ast.Cursor) []ast.Expr {
	match lhs.kind() {
		.expr_generic_arg_or_index {
			arg := lhs.edge(1)
			if arg.kind() == .expr_lifetime {
				return []ast.Expr{}
			}
			return [arg.type_expr()]
		}
		.expr_generic_args {
			mut args := []ast.Expr{cap: lhs.edge_count() - 1}
			for i in 1 .. lhs.edge_count() {
				arg := lhs.edge(i)
				if arg.kind() == .expr_lifetime {
					continue
				}
				args << arg.type_expr()
			}
			return args
		}
		else {
			return []ast.Expr{}
		}
	}
}

fn generic_call_embedded_type_arg_names_from_name(raw_name string, expected_count int) []string {
	if expected_count <= 0 {
		return []string{}
	}
	if !raw_name.contains('_T_') {
		return []string{}
	}
	suffix := raw_name.all_after('_T_')
	if suffix == '' {
		return []string{}
	}
	if expected_count == 1 {
		return [suffix]
	}
	parts := suffix.split('_')
	if parts.len != expected_count {
		return []string{}
	}
	return parts
}

fn generic_call_embedded_type_arg_names(lhs ast.Expr, expected_count int) []string {
	raw_name := match lhs {
		ast.Ident {
			lhs.name
		}
		ast.SelectorExpr {
			lhs.rhs.name
		}
		ast.GenericArgOrIndexExpr {
			return generic_call_embedded_type_arg_names(lhs.lhs, expected_count)
		}
		ast.GenericArgs {
			return generic_call_embedded_type_arg_names(lhs.lhs, expected_count)
		}
		else {
			''
		}
	}

	return generic_call_embedded_type_arg_names_from_name(raw_name, expected_count)
}

fn generic_call_embedded_type_arg_names_cursor(lhs ast.Cursor, expected_count int) []string {
	raw_name := generic_call_raw_name_cursor(lhs)
	return generic_call_embedded_type_arg_names_from_name(raw_name, expected_count)
}

fn (g &Gen) active_generic_bindings_matching_embedded_args(embedded_args []string, generic_params []string) ?map[string]types.Type {
	if embedded_args.len != generic_params.len || g.active_generic_types.len == 0 {
		return none
	}
	mut bindings := map[string]types.Type{}
	for i, param_name in generic_params {
		arg_name := embedded_args[i]
		concrete := if is_generic_placeholder_type_name(arg_name) {
			g.active_generic_types[arg_name] or { return none }
		} else {
			active := g.active_generic_types[param_name] or { return none }
			spec_token := g.generic_specialization_token_from_type(active)
			if !generic_token_matches_short_name(spec_token, arg_name) {
				return none
			}
			active
		}
		if !g.generic_concrete_type_is_runtime_specializable(concrete) {
			return none
		}
		bindings[param_name] = concrete
	}
	return bindings
}

fn (g &Gen) generic_concrete_type_is_runtime_specializable(typ types.Type) bool {
	if !generic_concrete_type_is_runtime_specializable(typ) {
		return false
	}
	concrete := normalize_generic_concrete_type(typ)
	c_name := g.types_type_to_c(concrete)
	if c_name != '' && g.generic_struct_primary_instance_is_concrete(c_name) {
		return true
	}
	if c_name != '' {
		if struct_typ := g.lookup_type_by_c_name_const(c_name) {
			if struct_typ is types.Struct
				&& type_contains_generic_placeholder(types.Type(struct_typ)) {
				return false
			}
		}
	}
	if c_name != '' && c_name in g.generic_struct_instances && c_name !in g.generic_struct_bindings {
		return false
	}
	return true
}

fn (g &Gen) active_generic_bindings_matching_embedded_suffix(lhs ast.Expr, generic_params []string) ?map[string]types.Type {
	embedded_args := generic_call_embedded_type_arg_names(lhs, generic_params.len)
	return g.active_generic_bindings_matching_embedded_args(embedded_args, generic_params)
}

fn (g &Gen) active_generic_bindings_matching_embedded_suffix_cursor(lhs ast.Cursor, generic_params []string) ?map[string]types.Type {
	embedded_args := generic_call_embedded_type_arg_names_cursor(lhs, generic_params.len)
	return g.active_generic_bindings_matching_embedded_args(embedded_args, generic_params)
}

fn (g &Gen) active_generic_bindings_matching_name_suffix(name string, generic_params []string) ?map[string]types.Type {
	embedded_args := generic_call_embedded_type_arg_names_from_name(name, generic_params.len)
	return g.active_generic_bindings_matching_embedded_args(embedded_args, generic_params)
}

fn (mut g Gen) bind_embedded_generic_type_args(lhs ast.Expr, generic_params []string, mut bindings map[string]types.Type) bool {
	embedded_args := generic_call_embedded_type_arg_names(lhs, generic_params.len)
	if embedded_args.len == 0 {
		return true
	}
	if embedded_args.len != generic_params.len {
		return bindings.len == generic_params.len
	}
	for i, param_name in generic_params {
		if param_name in bindings {
			continue
		}
		arg_name := embedded_args[i]
		if is_generic_placeholder_type_name(arg_name) {
			concrete := g.active_generic_types[arg_name] or { return false }
			bindings[param_name] = concrete
			continue
		}
		concrete := g.concrete_type_from_c_name(arg_name) or { return false }
		bindings[param_name] = concrete
	}
	return true
}

fn (mut g Gen) bind_embedded_generic_type_args_cursor(lhs ast.Cursor, generic_params []string, mut bindings map[string]types.Type) bool {
	embedded_args := generic_call_embedded_type_arg_names_cursor(lhs, generic_params.len)
	if embedded_args.len == 0 {
		return true
	}
	if embedded_args.len != generic_params.len {
		return bindings.len == generic_params.len
	}
	for i, param_name in generic_params {
		if param_name in bindings {
			continue
		}
		arg_name := embedded_args[i]
		if is_generic_placeholder_type_name(arg_name) {
			concrete := g.active_generic_types[arg_name] or { return false }
			bindings[param_name] = concrete
			continue
		}
		concrete := g.concrete_type_from_c_name(arg_name) or { return false }
		bindings[param_name] = concrete
	}
	return true
}

fn (mut g Gen) generic_type_arg_concrete_type(arg ast.Expr) ?types.Type {
	arg_name := arg.name()
	if is_generic_placeholder_type_name(arg_name) {
		if concrete := g.active_generic_types[arg_name] {
			resolved_concrete := g.concrete_type_with_active_generics(concrete)
			if !generic_concrete_type_is_runtime_specializable(resolved_concrete) {
				return none
			}
			return resolved_concrete
		}
		return none
	}
	c_name := g.expr_type_to_c(arg).trim_space().trim_right('*')
	concrete := g.concrete_type_from_c_name(c_name) or { return none }
	if !g.generic_concrete_type_is_runtime_specializable(concrete) {
		return none
	}
	return concrete
}

fn (mut g Gen) concrete_type_from_generic_call_arg(arg ast.Expr) ?types.Type {
	base_arg := if arg is ast.ModifierExpr { arg.expr } else { arg }
	if is_comptime_field_metadata_expr(base_arg, g.comptime_field_var) {
		return none
	}
	if base_arg is ast.SelectorExpr && is_comptime_selector_rhs_name(base_arg.rhs.name)
		&& type_has_valid_data(g.comptime_field_raw_type) {
		return g.comptime_field_raw_type
	}
	if active_concrete := g.active_generic_concrete_from_arg(base_arg) {
		return active_concrete
	}
	if base_arg is ast.Ident {
		if placeholder := g.cur_fn_generic_params[base_arg.name] {
			if placeholder !in g.active_generic_types {
				return none
			}
		}
	}
	mut c_name := ''
	if base_arg is ast.InitExpr {
		c_name = g.expr_type_to_c(base_arg.typ).trim_space()
	}
	if c_name == '' && base_arg is ast.CastExpr {
		c_name = g.expr_type_to_c(base_arg.typ).trim_space()
	}
	if (c_name == '' || c_name == 'int' || c_name == 'array' || c_name == 'map')
		&& base_arg is ast.SelectorExpr {
		c_name = g.selector_field_type(base_arg).trim_space()
		if c_name == '' || c_name == 'int' || c_name == 'array' || c_name == 'map' {
			c_name = g.selector_declared_field_type(base_arg).trim_space()
		}
	}
	mut found_local_arg_type := false
	if c_name == '' || c_name == 'int' {
		if base_arg is ast.Ident {
			c_name = (g.get_local_var_c_type(base_arg.name) or { '' }).trim_space()
			found_local_arg_type = c_name != ''
		}
	}
	if !found_local_arg_type && (c_name == '' || c_name == 'int') {
		c_name = g.get_expr_type(base_arg).trim_space()
	}
	if c_name == '' || c_name == 'int' {
		if raw := g.get_raw_type(base_arg) {
			return raw
		}
	}
	if c_name == '' || c_name == 'int' {
		return none
	}
	if concrete := g.active_generic_types[c_name] {
		return concrete
	}
	return g.concrete_type_from_call_arg_c_name(c_name)
}

fn (mut g Gen) concrete_type_from_generic_call_arg_cursor(arg ast.Cursor) ?types.Type {
	base_arg := generic_call_base_arg_cursor(arg)
	if is_comptime_field_metadata_expr_cursor(base_arg, g.comptime_field_var) {
		return none
	}
	if base_arg.kind() == .expr_selector && is_comptime_selector_rhs_name(base_arg.edge(1).name())
		&& type_has_valid_data(g.comptime_field_raw_type) {
		return g.comptime_field_raw_type
	}
	if active_concrete := g.active_generic_concrete_from_arg_cursor(base_arg) {
		return active_concrete
	}
	if base_arg.kind() == .expr_ident {
		if placeholder := g.cur_fn_generic_params[base_arg.name()] {
			if placeholder !in g.active_generic_types {
				return none
			}
		}
	}
	mut c_name := ''
	match base_arg.kind() {
		.expr_init {
			c_name = g.expr_type_to_c(base_arg.edge(0).type_expr()).trim_space()
		}
		.expr_cast {
			c_name = g.expr_type_to_c(base_arg.edge(0).type_expr()).trim_space()
		}
		.expr_as_cast {
			c_name = g.expr_type_to_c(base_arg.edge(1).type_expr()).trim_space()
		}
		else {}
	}

	if (c_name == '' || c_name == 'int' || c_name == 'array' || c_name == 'map')
		&& base_arg.kind() == .expr_selector {
		c_name = g.selector_declared_field_type_cursor_for_generic_scan(base_arg).trim_space()
	}

	mut found_local_arg_type := false
	if c_name == '' || c_name == 'int' {
		if base_arg.kind() == .expr_ident {
			c_name = (g.get_local_var_c_type(base_arg.name()) or { '' }).trim_space()
			found_local_arg_type = c_name != ''
		}
	}
	if !found_local_arg_type && (c_name == '' || c_name == 'int') {
		c_name = g.generic_scan_expr_cursor_c_type(base_arg).trim_space()
	}
	if !found_local_arg_type && (c_name == '' || c_name == 'int') {
		if raw := g.raw_type_from_generic_call_arg_cursor(base_arg) {
			return raw
		}
	}
	if c_name == '' || c_name == 'int' {
		return none
	}
	if concrete := g.active_generic_types[c_name] {
		return concrete
	}
	return g.concrete_type_from_call_arg_c_name(c_name)
}

fn generic_call_base_arg_cursor(arg ast.Cursor) ast.Cursor {
	if arg.kind() == .expr_modifier {
		return arg.edge(0)
	}
	return arg
}

fn (mut g Gen) active_generic_concrete_from_arg_cursor(arg ast.Cursor) ?types.Type {
	if g.active_generic_types.len == 0 {
		return none
	}
	if arg.kind() == .expr_prefix && unsafe { token.Token(int(arg.aux())) } == .amp {
		if concrete := g.active_generic_concrete_from_arg_cursor(arg.edge(0)) {
			return types.Type(types.Pointer{
				base_type: concrete
			})
		}
	}
	if arg.kind() == .expr_ident {
		if param_name := g.cur_fn_generic_params[arg.name()] {
			if concrete := g.active_generic_types[param_name] {
				return normalize_generic_concrete_type(concrete)
			}
		}
	}
	raw := g.raw_type_from_generic_call_arg_cursor(arg) or { return none }
	concrete := g.concrete_type_with_active_generics(raw)
	if type_contains_generic_placeholder(concrete) || concrete.name() == raw.name() {
		return none
	}
	return normalize_generic_concrete_type(concrete)
}

fn (mut g Gen) raw_type_from_generic_call_arg_cursor(arg ast.Cursor) ?types.Type {
	if g.env == unsafe { nil } || !arg.is_valid() {
		return none
	}
	if arg.kind() == .expr_ident {
		if cached := g.is_module_ident_cache[arg.name()] {
			if cached {
				return none
			}
		}
		if local_type := g.runtime_local_types[arg.name()] {
			if resolved := g.resolve_c_type_to_raw(local_type) {
				return resolved
			}
		}
		if mut fn_scope := g.ensure_cur_fn_scope() {
			if obj := fn_scope.lookup_parent(arg.name(), 0) {
				if obj is types.Module {
					return none
				}
				return obj.typ()
			}
		}
	}
	pos := arg.pos()
	if pos.is_valid() {
		if typ := g.env.get_expr_type(pos.id) {
			if type_has_valid_data(typ) {
				return typ
			}
		}
	}
	return none
}

fn is_comptime_field_metadata_expr(expr ast.Expr, field_var string) bool {
	if field_var == '' {
		return false
	}
	base_expr := if expr is ast.ModifierExpr { expr.expr } else { expr }
	if base_expr is ast.SelectorExpr && base_expr.lhs is ast.Ident
		&& (base_expr.lhs as ast.Ident).name == field_var
		&& !is_comptime_selector_rhs_name(base_expr.rhs.name) {
		return true
	}
	return false
}

fn is_comptime_field_metadata_expr_cursor(expr ast.Cursor, field_var string) bool {
	if field_var == '' {
		return false
	}
	base_expr := generic_call_base_arg_cursor(expr)
	if base_expr.kind() != .expr_selector {
		return false
	}
	lhs := base_expr.edge(0)
	rhs := base_expr.edge(1)
	return lhs.kind() == .expr_ident && lhs.name() == field_var
		&& !is_comptime_selector_rhs_name(rhs.name())
}

fn (mut g Gen) record_late_generic_call_spec_for_key(key string, bindings map[string]types.Type) {
	if key == '' || bindings.len == 0 {
		return
	}
	for _, concrete in bindings {
		if !g.generic_concrete_type_is_runtime_specializable(concrete) {
			return
		}
	}
	for existing in g.late_generic_specs[key] {
		if existing == bindings {
			return
		}
	}
	g.late_generic_specs[key] << bindings.clone()
	g.index_late_generic_spec_key(key)
}

fn (mut g Gen) record_late_generic_call_spec_unchecked(key string, bindings map[string]types.Type) {
	if key == '' || bindings.len == 0 {
		return
	}
	for existing in g.late_generic_specs[key] {
		if existing == bindings {
			return
		}
	}
	g.late_generic_specs[key] << bindings.clone()
	g.index_late_generic_spec_key(key)
}

fn (mut g Gen) record_late_generic_call_spec(key string, bindings map[string]types.Type) {
	g.record_late_generic_call_spec_for_key(key, bindings)
	dot_pos := key.last_index_u8(`.`)
	if dot_pos > 0 && dot_pos < key.len - 1 {
		short_key := key[dot_pos + 1..]
		if short_key != key {
			g.record_late_generic_call_spec_for_key(short_key, bindings)
		}
	}
}

fn (mut g Gen) record_generic_scan_call_name(lhs ast.Expr, arg_count int, generic_params []string, bindings map[string]types.Type) {
	base_name := g.resolve_call_name(lhs, arg_count)
	g.record_generic_scan_call_base_name(base_name, generic_params, bindings)
}

fn (mut g Gen) record_generic_scan_call_name_cursor(lhs ast.Cursor, arg_count int, generic_params []string, bindings map[string]types.Type) {
	base_name := g.resolve_call_name_cursor_for_generic_scan(lhs, arg_count)
	g.record_generic_scan_call_base_name(base_name, generic_params, bindings)
}

fn (mut g Gen) record_generic_scan_call_base_name(name string, generic_params []string, bindings map[string]types.Type) {
	if !g.collect_generic_scan_calls {
		return
	}
	if generic_params.len == 0 || bindings.len != generic_params.len {
		return
	}
	mut base_name := name
	if base_name == '' {
		return
	}
	if base_name.contains('_T_') {
		base_name = base_name.all_before('_T_')
	} else if base_name.ends_with('_T') {
		base_name = base_name[..base_name.len - 2]
	}
	base_name = normalize_duplicate_qualified_method_prefix(base_name)
	mut suffixes := []string{cap: generic_params.len}
	for param_name in generic_params {
		concrete := bindings[param_name] or { return }
		if !g.generic_concrete_type_is_runtime_specializable(concrete) {
			return
		}
		suffixes << g.generic_specialization_token_from_type(concrete)
	}
	if suffixes.len == generic_params.len {
		specialized_name := '${base_name}_T_${suffixes.join('_')}'
		g.generic_scan_called_names[specialized_name] = true
		if !base_name.contains('__') && g.cur_module != '' && g.cur_module != 'main'
			&& g.cur_module != 'builtin' {
			g.generic_scan_called_names['${g.cur_module}__${specialized_name}'] = true
		}
	}
}

fn (mut g Gen) resolve_call_name_cursor_for_generic_scan(lhs ast.Cursor, arg_count int) string {
	mut name := ''
	match lhs.kind() {
		.expr_generic_arg_or_index, .expr_generic_args {
			return g.resolve_call_name_cursor_for_generic_scan(lhs.edge(0), arg_count)
		}
		.expr_ident {
			name = sanitize_fn_ident(lhs.name())
		}
		.expr_selector {
			lhs_expr := lhs.edge(0)
			method_name := sanitize_fn_ident(lhs.edge(1).name())
			if lhs_expr.kind() == .expr_ident && lhs_expr.name() == 'C' {
				return method_name
			}
			if lhs_expr.kind() == .expr_ident && (g.is_type_name(lhs_expr.name())
				|| g.selector_lhs_is_static_type_ident(lhs_expr.name())) {
				return '${g.get_qualified_name(lhs_expr.name())}__${method_name}'
			}
			if mod_call_name := g.resolve_selector_module_call_name_cursor(lhs) {
				name = mod_call_name
			} else if qualified_method_name := g.resolved_qualified_selector_method_name(method_name) {
				name = qualified_method_name
			} else {
				mut base_type := g.method_receiver_base_type_cursor_for_generic_scan(lhs_expr)
				if base_type == '' {
					base_type =
						g.generic_scan_expr_cursor_c_type(lhs_expr).trim_space().trim_right('*')
				}
				name = '${base_type}__${method_name}'
				if receiver_type := g.get_receiver_expr_type_for_method_cursor(lhs_expr) {
					if concrete_method := g.resolve_method_on_concrete_type(receiver_type,
						method_name)
					{
						name = concrete_method
					}
				}
				if name !in g.fn_return_types && name !in g.fn_param_is_ptr {
					if raw_type := g.raw_type_from_generic_call_arg_cursor(lhs_expr) {
						raw_c_type := strip_pointer_type_name(g.types_type_to_c(raw_type))
						if raw_c_type != '' && raw_c_type != base_type {
							if raw_method := g.resolve_method_on_concrete_type(raw_c_type,
								method_name)
							{
								name = raw_method
							}
						}
					}
					if name !in g.fn_return_types && name !in g.fn_param_is_ptr {
						if alias_base := g.alias_base_c_type(base_type) {
							alias_name := '${alias_base}__${method_name}'
							if alias_name in g.fn_return_types || alias_name in g.fn_param_is_ptr {
								name = alias_name
							}
						}
					}
					if name !in g.fn_return_types && name !in g.fn_param_is_ptr {
						if map_method := g.map_runtime_method_name_cursor(lhs_expr, method_name) {
							name = map_method
						}
					}
				}
			}
		}
		else {}
	}

	if name == 'builtin__new_array_from_c_array_noscan' {
		name = 'new_array_from_c_array'
	}
	if name == 'builtin__array_push_noscan' {
		name = 'array__push'
	}
	if name == 'panic' {
		name = 'v_panic'
	}
	if name == 'voidptr__vbytes' {
		name = 'void__vbytes'
	}
	if name == 'int__bytestr' {
		name = 'Array_u8__bytestr'
	}
	if name.ends_with('__bytes') && name !in g.fn_return_types && name !in g.fn_param_is_ptr {
		if 'string__bytes' in g.fn_return_types || 'string__bytes' in g.fn_param_is_ptr {
			name = 'string__bytes'
		}
	}
	if is_c_runtime_function(name) {
		return name
	}
	if name != '' && g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin'
		&& !name.contains('__') {
		qualified := '${g.cur_module}__${name}'
		if qualified in g.fn_return_types || qualified in g.fn_param_is_ptr {
			return qualified
		}
		if name in g.fn_return_types || name in g.fn_param_is_ptr {
			return name
		}
		return qualified
	}
	return name
}

fn (mut g Gen) resolve_selector_module_call_name_cursor(lhs ast.Cursor) ?string {
	if lhs.kind() != .expr_selector {
		return none
	}
	lhs_ident := lhs.edge(0)
	if lhs_ident.kind() != .expr_ident || lhs_ident.name() == 'C' {
		return none
	}
	if _ := g.get_local_var_c_type(lhs_ident.name()) {
		return none
	}
	mod_name := g.resolve_module_name(lhs_ident.name())
	name := '${mod_name}__${sanitize_fn_ident(lhs.edge(1).name())}'
	if g.is_module_ident(lhs_ident.name()) || name in g.fn_return_types || name in g.fn_param_is_ptr
		|| g.is_module_local_fn(name) || g.has_specialized_fn_base(name)
		|| g.has_generic_fn_decl_by_base_name(name) {
		return name
	}
	return none
}

fn (mut g Gen) direct_known_c_type_for_expr_cursor(expr ast.Cursor) string {
	if !expr.is_valid() {
		return ''
	}
	match expr.kind() {
		.expr_paren, .expr_modifier {
			return g.direct_known_c_type_for_expr_cursor(expr.edge(0))
		}
		.expr_prefix {
			if unsafe { token.Token(int(expr.aux())) } == .mul {
				ptr_type := g.direct_known_c_type_for_expr_cursor(expr.edge(0))
				if ptr_type != '' {
					return strip_one_pointer_type_name(ptr_type)
				}
			}
		}
		.expr_cast {
			return g.expr_type_to_c(expr.edge(0).type_expr())
		}
		.expr_as_cast {
			return g.expr_type_to_c(expr.edge(1).type_expr())
		}
		.expr_ident {
			name := expr.name()
			if local_type := g.get_local_var_c_type(name) {
				return local_type
			}
			if const_type := g.const_types[name] {
				return const_type
			}
			if global_type := g.global_var_types[name] {
				return global_type
			}
			if g.cur_module != '' {
				qualified := '${g.cur_module}__${name}'
				if const_type := g.const_types[qualified] {
					return const_type
				}
				if global_type := g.global_var_types[qualified] {
					return global_type
				}
			}
		}
		else {}
	}

	return ''
}

fn (mut g Gen) selector_struct_name_cursor_for_generic_scan(expr ast.Cursor) string {
	direct_type := g.direct_known_c_type_for_expr_cursor(expr)
	if direct_type != '' {
		base := strip_pointer_type_name(direct_type)
		if base != '' && base != 'int' && base !in ['void', 'void*', 'voidptr'] {
			return base
		}
	}
	if expr.kind() == .expr_ident {
		if local_type := g.get_local_var_c_type(expr.name()) {
			base := strip_pointer_type_name(local_type)
			if base != '' && base != 'int' && base !in ['void', 'void*', 'voidptr'] {
				return base
			}
		}
	}
	if raw_type := g.raw_type_from_generic_call_arg_cursor(expr) {
		if !type_has_valid_data(raw_type) {
			return g.generic_scan_expr_cursor_c_type(expr).trim_right('*')
		}
		match raw_type {
			types.Pointer {
				if raw_type.base_type is types.Struct {
					return raw_type.base_type.name
				}
				if raw_type.base_type is types.Alias {
					return raw_type.base_type.name
				}
			}
			types.Struct {
				return raw_type.name
			}
			types.Alias {
				return raw_type.name
			}
			else {}
		}
	}
	return g.generic_scan_expr_cursor_c_type(expr).trim_right('*')
}

fn (mut g Gen) selector_declared_field_type_cursor_for_generic_scan(sel ast.Cursor) string {
	if sel.kind() != .expr_selector {
		return ''
	}
	rhs := sel.edge(1).name()
	if rhs == '' {
		return ''
	}
	lhs_expr := sel.edge(0)
	lhs_struct_name := g.selector_struct_name_cursor_for_generic_scan(lhs_expr)
	if lhs_struct_name != '' {
		if g.active_generic_types.len > 0 || lhs_struct_name in g.generic_struct_bindings
			|| lhs_struct_name in g.generic_struct_instances || lhs_struct_name.contains('_T_') {
			if field_type := g.lookup_struct_field_type_by_name(lhs_struct_name, rhs) {
				return field_type
			}
		}
		if field_type := g.lookup_struct_decl_field_type_by_name(lhs_struct_name, rhs) {
			return field_type
		}
		if field_type := g.lookup_struct_field_type_by_name(lhs_struct_name, rhs) {
			return field_type
		}
	}
	lhs_expr_type := g.generic_scan_expr_cursor_c_type(lhs_expr)
	if lhs_expr_type != '' && lhs_expr_type != 'int' {
		if field_type := g.lookup_struct_field_type_by_name(lhs_expr_type, rhs) {
			return field_type
		}
	}
	mut raw_resolved := ''
	if lhs_raw := g.raw_type_from_generic_call_arg_cursor(lhs_expr) {
		if field_type := selector_struct_field_type_from_type(lhs_raw, rhs) {
			resolved := g.types_type_to_c(field_type)
			if resolved != '' && resolved !in ['int', 'array'] {
				return resolved
			}
			raw_resolved = resolved
		}
	}
	return raw_resolved
}

fn (mut g Gen) method_receiver_base_type_cursor_for_generic_scan(expr ast.Cursor) string {
	mut base_expr := expr
	for base_expr.is_valid() && base_expr.kind() in [.expr_paren, .expr_modifier] {
		base_expr = base_expr.edge(0)
	}
	if !base_expr.is_valid() {
		return ''
	}
	if base_expr.kind() == .expr_prefix
		&& unsafe { token.Token(int(base_expr.aux())) } in [.amp, .mul] {
		return g.method_receiver_base_type_cursor_for_generic_scan(base_expr.edge(0))
	}
	direct_type := g.direct_known_c_type_for_expr_cursor(base_expr)
	if direct_type != '' && direct_type != 'int' {
		base := strip_pointer_type_name(direct_type)
		if base != '' && base != 'int' && base !in ['void', 'void*', 'voidptr'] {
			return base
		}
	}
	if base_expr.kind() == .expr_init {
		init_type := g.expr_type_to_c(base_expr.edge(0).type_expr())
		if init_type != '' && init_type != 'int' {
			return init_type
		}
	}
	if base_expr.kind() == .expr_ident {
		if local_type := g.get_local_var_c_type(base_expr.name()) {
			base := strip_pointer_type_name(local_type)
			if base != '' && base != 'int' {
				return base
			}
		}
	}
	if base_expr.kind() == .expr_selector {
		lhs_struct_name := g.selector_struct_name_cursor_for_generic_scan(base_expr.edge(0))
		if lhs_struct_name != '' {
			if declared_field_type := g.lookup_struct_field_type_by_name(lhs_struct_name,
				base_expr.edge(1).name())
			{
				base := strip_pointer_type_name(declared_field_type)
				if base != '' && base != 'int' && base !in ['void', 'void*', 'voidptr'] {
					return base
				}
			}
		}
		field_type := g.selector_declared_field_type_cursor_for_generic_scan(base_expr)
		if field_type != '' && field_type != 'int' {
			base := strip_pointer_type_name(field_type)
			if base in ['voidptr', 'void*'] {
				return 'void'
			}
			return base
		}
	}
	if raw_type := g.raw_type_from_generic_call_arg_cursor(base_expr) {
		match raw_type {
			types.Pointer {
				return g.types_type_to_c(raw_type.base_type)
			}
			else {
				raw_c_type := g.types_type_to_c(raw_type)
				if raw_c_type != 'void' {
					return raw_c_type
				}
			}
		}
	}
	mut receiver_type := g.generic_scan_expr_cursor_c_type(base_expr)
	if receiver_type.ends_with('*') {
		receiver_type = receiver_type[..receiver_type.len - 1]
	}
	if receiver_type in ['voidptr', 'void*'] {
		return 'void'
	}
	return receiver_type
}

fn (mut g Gen) get_receiver_expr_type_for_method_cursor(expr ast.Cursor) ?string {
	mut base_expr := expr
	for base_expr.is_valid() && base_expr.kind() in [.expr_paren, .expr_modifier] {
		base_expr = base_expr.edge(0)
	}
	if !base_expr.is_valid() {
		return none
	}
	if base_expr.kind() == .expr_prefix
		&& unsafe { token.Token(int(base_expr.aux())) } in [.amp, .mul] {
		if receiver_type := g.get_receiver_expr_type_for_method_cursor(base_expr.edge(0)) {
			return receiver_type
		}
	}
	if active_concrete := g.active_generic_concrete_from_arg_cursor(base_expr) {
		active_type := g.types_type_to_c(active_concrete).trim_space()
		if active_type != '' && active_type != 'int' && active_type != 'void' {
			return active_type.trim_right('*')
		}
	}
	if base_expr.kind() == .expr_ident {
		local_type := (g.get_local_var_c_type(base_expr.name()) or { '' }).trim_space()
		if local_type != '' && local_type != 'int' && local_type != 'void' {
			return local_type.trim_right('*')
		}
	}
	mut receiver_type := g.generic_scan_expr_cursor_c_type(base_expr).trim_space()
	if base_expr.kind() == .expr_selector {
		declared_type :=
			g.selector_declared_field_type_cursor_for_generic_scan(base_expr).trim_space()
		declared_base := strip_pointer_type_name(declared_type)
		receiver_base := strip_pointer_type_name(receiver_type)
		if declared_type != '' && declared_type != 'int' && declared_type != 'void'
			&& (receiver_type == '' || receiver_type == 'int'
			|| (declared_type.contains('_T_') && (!receiver_type.contains('_T_')
			|| declared_base != receiver_base))) {
			receiver_type = declared_type
		}
	}
	if (receiver_type == '' || receiver_type == 'int') && base_expr.kind() == .expr_ident {
		receiver_type = (g.get_local_var_c_type(base_expr.name()) or { '' }).trim_space()
	}
	if receiver_type == '' || receiver_type == 'int' {
		if raw := g.raw_type_from_generic_call_arg_cursor(base_expr) {
			receiver_type = g.types_type_to_c(raw).trim_space()
		}
	}
	if receiver_type.ends_with('*') {
		receiver_type = receiver_type[..receiver_type.len - 1]
	}
	if receiver_type == '' || receiver_type == 'int' {
		return none
	}
	return receiver_type
}

fn (mut g Gen) selector_receiver_is_map_cursor(receiver ast.Cursor) bool {
	if raw_type := g.raw_type_from_generic_call_arg_cursor(receiver) {
		match raw_type {
			types.Map {
				return true
			}
			types.Pointer {
				match raw_type.base_type {
					types.Map {
						return true
					}
					types.Alias {
						return raw_type.base_type.base_type is types.Map
					}
					else {}
				}
			}
			types.Alias {
				return raw_type.base_type is types.Map
			}
			else {}
		}
	}
	base_type := g.method_receiver_base_type_cursor_for_generic_scan(receiver)
	if c_type_is_map_value(base_type) {
		return true
	}
	expr_type := g.generic_scan_expr_cursor_c_type(receiver)
	return c_type_is_map_value(expr_type)
}

fn (mut g Gen) map_runtime_method_name_cursor(receiver ast.Cursor, method_name string) ?string {
	if method_name !in ['clone', 'keys', 'values'] || !g.selector_receiver_is_map_cursor(receiver) {
		return none
	}
	return 'map__${method_name}'
}

fn normalize_duplicate_qualified_method_prefix(name string) string {
	parts := name.split('__')
	if parts.len < 5 {
		return name
	}
	max_prefix_parts := parts.len / 2
	for prefix_parts := max_prefix_parts; prefix_parts >= 2; prefix_parts-- {
		mut matches := true
		for i := 0; i < prefix_parts; i++ {
			if parts[i] != parts[prefix_parts + i] {
				matches = false
				break
			}
		}
		if matches {
			return parts[prefix_parts..].join('__')
		}
	}
	return name
}

fn (mut g Gen) index_late_generic_spec_key(key string) {
	// Some generic function values are first discovered while emitting an earlier
	// function body. Keep the specialization index in sync immediately so a later
	// declaration in the same generation pass can emit the concrete body.
	mut fn_name := key
	bracket_idx := key.index_u8(`[`)
	if bracket_idx > 0 {
		fn_name = key[..bracket_idx]
	}
	dot_idx := fn_name.last_index_u8(`.`)
	if dot_idx > 0 && dot_idx < fn_name.len - 1 {
		short_name := fn_name[dot_idx + 1..]
		if short_name.len > 0 && key !in g.generic_spec_index[short_name] {
			g.generic_spec_index[short_name] << key
		}
	}
	double_underscore_idx := fn_name.last_index('__') or { -1 }
	if double_underscore_idx > 0 && double_underscore_idx < fn_name.len - 2 {
		short_name := fn_name[double_underscore_idx + 2..]
		if short_name.len > 0 && key !in g.generic_spec_index[short_name] {
			g.generic_spec_index[short_name] << key
		}
	}
	if fn_name.len > 0 && key !in g.generic_spec_index[fn_name] {
		g.generic_spec_index[fn_name] << key
	}
}

fn (mut g Gen) scan_call_for_generic_fn_specs(call ast.CallExpr) {
	decl := g.generic_call_decl_from_lhs(call.lhs) or { return }
	generic_params := g.generic_fn_param_names(decl)
	if generic_params.len == 0 {
		return
	}
	mut bindings := map[string]types.Type{}
	mut metadata_params := map[string]bool{}
	type_args := generic_call_type_args(call.lhs)
	mut has_unresolved_explicit_type_arg := false
	for i, param_name in generic_params {
		if i >= type_args.len {
			break
		}
		concrete := g.generic_type_arg_concrete_type(type_args[i]) or {
			has_unresolved_explicit_type_arg = true
			continue
		}
		bindings[param_name] = concrete
	}
	if has_unresolved_explicit_type_arg {
		return
	}
	if type_args.len == 0 {
		if active_bindings := g.active_generic_bindings_matching_embedded_suffix(call.lhs,
			generic_params)
		{
			for param_name, concrete in active_bindings {
				bindings[param_name] = concrete
			}
		}
	}
	if type_args.len == 0 && g.active_generic_types.len == 0
		&& !g.bind_embedded_generic_type_args(call.lhs, generic_params, mut bindings) {
		return
	}
	arg_offset := if decl.is_method && call.args.len == decl.typ.params.len + 1 { 1 } else { 0 }
	for i, param in decl.typ.params {
		arg_idx := i + arg_offset
		if arg_idx >= call.args.len || !expr_has_generic_placeholder(param.typ) {
			continue
		}
		arg := call.args[arg_idx]
		if is_comptime_field_metadata_expr(arg, g.comptime_field_var) {
			mut seen := map[string]bool{}
			mut names := []string{}
			collect_generic_placeholder_names_from_expr(param.typ, mut seen, mut names)
			for name in names {
				metadata_params[name] = true
			}
			continue
		}
		concrete := g.concrete_type_from_generic_call_arg(arg) or { continue }
		g.infer_generic_type_bindings_from_param(param.typ, g.concrete_type_for_generic_param(param,
			concrete), generic_params, mut bindings)
	}
	for param_name in generic_params {
		if param_name in bindings {
			continue
		}
		if param_name in metadata_params {
			continue
		}
		if concrete := g.active_generic_types[param_name] {
			bindings[param_name] = concrete
		}
	}
	if type_args.len == 0 && bindings.len != generic_params.len
		&& !g.bind_embedded_generic_type_args(call.lhs, generic_params, mut bindings) {
		raw_name := generic_call_raw_name(call.lhs)
		if !g.generic_call_name_has_placeholder_suffix(raw_name) {
			if specialized_name := g.try_specialize_generic_call_name(raw_name, call.args) {
				g.record_called_specialized_generic_name(specialized_name)
			}
			return
		}
		return_bindings := g.infer_generic_bindings_from_current_return(decl, generic_params) or {
			return
		}
		for param_name, concrete in return_bindings {
			if param_name !in bindings {
				bindings[param_name] = concrete
			}
		}
	}
	if bindings.len != generic_params.len {
		return
	}
	for _, concrete in bindings {
		if !g.generic_concrete_type_is_runtime_specializable(concrete) {
			return
		}
	}
	short_name := generic_call_short_name(call.lhs)
	keys := if short_name == '' {
		[]string{}
	} else {
		[short_name]
	}

	for key in keys {
		g.record_late_generic_call_spec(key, bindings)
	}
	g.record_generic_scan_call_name(call.lhs, call.args.len, generic_params, bindings)
}

fn generic_fn_value_base_cursor(expr ast.Cursor) ast.Cursor {
	if expr.kind() in [.expr_generic_args, .expr_generic_arg_or_index] {
		return expr.edge(0)
	}
	return ast.Cursor{}
}

fn (mut g Gen) scan_generic_struct_bindings_cursor(expr ast.Cursor) {
	if g.generic_call_spec_scan_only {
		return
	}
	match expr.kind() {
		.expr_generic_arg_or_index {
			base_name := g.expr_type_to_c(expr.edge(0).type_expr())
			arg := expr.edge(1)
			arg_name := arg.name()
			struct_base := if base_name.contains('__') {
				base_name.all_after_last('__')
			} else {
				base_name
			}
			if is_generic_placeholder_type_name(arg_name) {
				if concrete := g.active_generic_types[arg_name] {
					g.record_generic_struct_bindings(struct_base, base_name, [
						ast.Expr(ast.Ident{
							name: g.types_type_to_c(concrete)
						}),
					])
				}
			} else {
				g.record_generic_struct_bindings(struct_base, base_name, [
					arg.type_expr(),
				])
			}
		}
		.expr_generic_args {
			args := generic_call_type_args_cursor(expr)
			if args.len == 0 {
				return
			}
			base_name := g.expr_type_to_c(expr.edge(0).type_expr())
			struct_base := if base_name.contains('__') {
				base_name.all_after_last('__')
			} else {
				base_name
			}
			mut concrete_args := []ast.Expr{cap: args.len}
			mut all_concrete := true
			for arg in args {
				arg_name := arg.name()
				if is_generic_placeholder_type_name(arg_name) {
					if concrete := g.active_generic_types[arg_name] {
						concrete_args << ast.Expr(ast.Ident{
							name: g.types_type_to_c(concrete)
						})
					} else {
						all_concrete = false
						break
					}
				} else {
					concrete_args << arg
				}
			}
			if all_concrete {
				g.record_generic_struct_bindings(struct_base, base_name, concrete_args)
			}
		}
		else {}
	}
}

fn (mut g Gen) scan_generic_fn_value_cursor_for_specs(expr ast.Cursor) {
	decl := g.generic_call_decl_from_lhs_cursor(expr) or { return }
	generic_params := g.generic_fn_param_names(decl)
	if generic_params.len == 0 {
		return
	}
	mut bindings := map[string]types.Type{}
	type_args := generic_call_type_args_cursor(expr)
	mut has_unresolved_explicit_type_arg := false
	for i, param_name in generic_params {
		if i >= type_args.len {
			break
		}
		concrete := g.generic_type_arg_concrete_type(type_args[i]) or {
			has_unresolved_explicit_type_arg = true
			continue
		}
		bindings[param_name] = concrete
	}
	if has_unresolved_explicit_type_arg {
		return
	}
	for param_name in generic_params {
		if param_name in bindings {
			continue
		}
		if concrete := g.active_generic_types[param_name] {
			bindings[param_name] = concrete
		}
	}
	if type_args.len == 0 && bindings.len != generic_params.len
		&& !g.bind_embedded_generic_type_args_cursor(expr, generic_params, mut bindings) {
		return
	}
	if bindings.len != generic_params.len {
		return
	}
	for _, concrete in bindings {
		if !g.generic_concrete_type_is_runtime_specializable(concrete) {
			return
		}
	}
	short_name := generic_call_short_name_cursor(expr)
	if short_name == '' {
		return
	}
	g.record_late_generic_call_spec(short_name, bindings)
	g.record_generic_scan_call_name_cursor(generic_fn_value_base_cursor(expr), 0, generic_params,
		bindings)
}

fn (mut g Gen) scan_generic_fn_value_for_specs(expr ast.Expr) {
	decl := g.generic_call_decl_from_lhs(expr) or { return }
	generic_params := g.generic_fn_param_names(decl)
	if generic_params.len == 0 {
		return
	}
	mut bindings := map[string]types.Type{}
	type_args := generic_call_type_args(expr)
	mut has_unresolved_explicit_type_arg := false
	for i, param_name in generic_params {
		if i >= type_args.len {
			break
		}
		concrete := g.generic_type_arg_concrete_type(type_args[i]) or {
			has_unresolved_explicit_type_arg = true
			continue
		}
		bindings[param_name] = concrete
	}
	if has_unresolved_explicit_type_arg {
		return
	}
	// Generic function values inside generic functions do not have call
	// arguments to infer from. Substitute the surrounding function's concrete
	// bindings so a value like handler[A, X] emits handler_T_App_Context.
	for param_name in generic_params {
		if param_name in bindings {
			continue
		}
		if concrete := g.active_generic_types[param_name] {
			bindings[param_name] = concrete
		}
	}
	if type_args.len == 0 && bindings.len != generic_params.len
		&& !g.bind_embedded_generic_type_args(expr, generic_params, mut bindings) {
		return
	}
	if bindings.len != generic_params.len {
		return
	}
	for _, concrete in bindings {
		if !g.generic_concrete_type_is_runtime_specializable(concrete) {
			return
		}
	}
	short_name := generic_call_short_name(expr)
	if short_name == '' {
		return
	}
	g.record_late_generic_call_spec(short_name, bindings)
	g.record_generic_scan_call_name(generic_fn_value_base_expr(expr), 0, generic_params, bindings)
}

fn (mut g Gen) scan_comptime_for_for_generic_types(node ast.ForStmt) bool {
	if node.init !is ast.ForInStmt {
		return false
	}
	for_in := node.init as ast.ForInStmt
	if for_in.expr !is ast.SelectorExpr {
		return false
	}
	sel := for_in.expr as ast.SelectorExpr
	if sel.rhs.name != 'fields' {
		return false
	}
	type_name := sel.lhs.name()
	concrete := g.active_generic_types[type_name] or { return false }
	if concrete !is types.Struct {
		return false
	}
	struct_type := g.comptime_for_struct_type(concrete, concrete as types.Struct)
	prev_field_var := g.comptime_field_var
	prev_field_name := g.comptime_field_name
	prev_field_type := g.comptime_field_type
	prev_field_raw_type := g.comptime_field_raw_type
	prev_field_attrs := g.comptime_field_attrs
	prev_field_idx := g.comptime_field_idx
	defer {
		g.comptime_field_var = prev_field_var
		g.comptime_field_name = prev_field_name
		g.comptime_field_type = prev_field_type
		g.comptime_field_raw_type = prev_field_raw_type
		g.comptime_field_attrs = prev_field_attrs
		g.comptime_field_idx = prev_field_idx
	}
	g.comptime_field_var = for_in.value.name()
	for i, field in struct_type.fields {
		g.comptime_field_name = field.name
		g.comptime_field_type = g.types_type_to_c(field.typ)
		g.comptime_field_raw_type = field.typ
		g.comptime_field_attrs = g.comptime_field_attribute_strings(struct_type.name, field)
		g.comptime_field_idx = i
		g.scan_stmts_for_generic_types(node.stmts)
	}
	return true
}

// scan_stmts_for_generic_types walks statements to find generic
// type instantiations (e.g. LinkedList[StructFieldInfo]{} in function bodies).
fn generic_body_scan_bindings_key(bindings map[string]types.Type) string {
	if bindings.len == 0 {
		return ''
	}
	mut keys := bindings.keys()
	keys.sort()
	mut parts := []string{cap: keys.len}
	for key in keys {
		concrete := bindings[key] or { continue }
		parts << '${key}=${concrete.name()}'
	}
	return parts.join(',')
}

fn (mut g Gen) scan_fn_body_for_generic_types(node ast.FnDecl, spec_name string) {
	prev_cur_fn_ret_type := g.cur_fn_ret_type
	if ret_type := g.scan_fn_return_c_type(node, spec_name) {
		g.cur_fn_ret_type = ret_type
	}
	defer {
		g.cur_fn_ret_type = prev_cur_fn_ret_type
	}
	if node.pos.id <= 0 {
		if !g.stmts_may_need_generic_scan(node.stmts) {
			return
		}
		g.scan_stmts_for_generic_types(node.stmts)
		return
	}
	fn_key := node.pos.id.str()
	bindings_key := if spec_name.len > 0 {
		spec_name
	} else {
		generic_body_scan_bindings_key(g.active_generic_types)
	}
	mode_key := if g.generic_call_spec_scan_only { 'calls' } else { 'all' }
	cache_key := '${g.cur_module}:${fn_key}:${bindings_key}:${mode_key}'
	if cache_key in g.generic_body_scan_cache {
		if !g.collect_generic_scan_calls {
			return
		}
	} else {
		g.generic_body_scan_cache[cache_key] = true
	}
	if !g.stmts_may_need_generic_scan(node.stmts) {
		return
	}
	g.scan_stmts_for_generic_types(node.stmts)
}

fn (mut g Gen) scan_fn_body_cursor_for_generic_types(stmt ast.Cursor, decl ast.FnDecl, spec_name string) {
	prev_cur_fn_ret_type := g.cur_fn_ret_type
	if ret_type := g.scan_fn_return_c_type(decl, spec_name) {
		g.cur_fn_ret_type = ret_type
	}
	defer {
		g.cur_fn_ret_type = prev_cur_fn_ret_type
	}
	body := stmt.list_at(3)
	if decl.pos.id <= 0 {
		if !g.cursor_stmts_may_need_generic_scan(body) {
			return
		}
		g.scan_cursor_stmts_for_generic_types(body)
		return
	}
	fn_key := decl.pos.id.str()
	bindings_key := if spec_name.len > 0 {
		spec_name
	} else {
		generic_body_scan_bindings_key(g.active_generic_types)
	}
	mode_key := if g.generic_call_spec_scan_only { 'calls' } else { 'all' }
	cache_key := '${g.cur_module}:${fn_key}:${bindings_key}:${mode_key}'
	if cache_key in g.generic_body_scan_cache {
		if !g.collect_generic_scan_calls {
			return
		}
	} else {
		g.generic_body_scan_cache[cache_key] = true
	}
	if !g.cursor_stmts_may_need_generic_scan(body) {
		return
	}
	g.scan_cursor_stmts_for_generic_types(body)
}

fn (mut g Gen) scan_fn_return_c_type(node ast.FnDecl, spec_name string) ?string {
	if spec_name != '' {
		if ret := g.fn_return_types[spec_name] {
			return normalize_signature_type_name(ret, 'void')
		}
	}
	if node.typ.return_type !is ast.EmptyExpr {
		return normalize_signature_type_name(g.expr_type_to_c(node.typ.return_type), 'void')
	}
	return 'void'
}

fn (g &Gen) stmts_may_need_generic_scan(stmts []ast.Stmt) bool {
	for stmt in stmts {
		match stmt {
			ast.AssignStmt {
				for expr in stmt.lhs {
					if g.expr_may_need_generic_scan(expr) {
						return true
					}
				}
				for expr in stmt.rhs {
					if g.expr_may_need_generic_scan(expr) {
						return true
					}
				}
			}
			ast.ReturnStmt {
				for expr in stmt.exprs {
					if g.expr_may_need_generic_scan(expr) {
						return true
					}
				}
			}
			ast.ExprStmt {
				if g.expr_may_need_generic_scan(stmt.expr) {
					return true
				}
			}
			ast.ComptimeStmt {
				if g.stmts_may_need_generic_scan([stmt.stmt]) {
					return true
				}
			}
			ast.ForStmt {
				if g.expr_may_need_generic_scan(stmt.cond)
					|| g.stmts_may_need_generic_scan(stmt.stmts) {
					return true
				}
			}
			ast.ForInStmt {
				if g.expr_may_need_generic_scan(stmt.key)
					|| g.expr_may_need_generic_scan(stmt.value)
					|| g.expr_may_need_generic_scan(stmt.expr) {
					return true
				}
			}
			ast.BlockStmt {
				if g.stmts_may_need_generic_scan(stmt.stmts) {
					return true
				}
			}
			else {}
		}
	}
	return false
}

fn (g &Gen) cursor_stmts_may_need_generic_scan(stmts ast.CursorList) bool {
	for i in 0 .. stmts.len() {
		if g.cursor_stmt_may_need_generic_scan(stmts.at(i)) {
			return true
		}
	}
	return false
}

fn (g &Gen) cursor_stmt_may_need_generic_scan(stmt ast.Cursor) bool {
	if !stmt.is_valid() {
		return false
	}
	match stmt.kind() {
		.stmt_assign {
			lhs_len := stmt.extra_int()
			for i in 0 .. lhs_len {
				if g.cursor_expr_may_need_generic_scan(stmt.edge(i)) {
					return true
				}
			}
			for i in lhs_len .. stmt.edge_count() {
				if g.cursor_expr_may_need_generic_scan(stmt.edge(i)) {
					return true
				}
			}
		}
		.stmt_return {
			for i in 0 .. stmt.edge_count() {
				if g.cursor_expr_may_need_generic_scan(stmt.edge(i)) {
					return true
				}
			}
		}
		.stmt_expr {
			return g.cursor_expr_may_need_generic_scan(stmt.edge(0))
		}
		.stmt_comptime {
			return g.cursor_stmt_may_need_generic_scan(stmt.edge(0))
		}
		.stmt_for {
			init := stmt.edge(0)
			if init.kind() == .stmt_for_in && g.cursor_stmt_may_need_generic_scan(init) {
				return true
			}
			if g.cursor_expr_may_need_generic_scan(stmt.edge(1)) {
				return true
			}
			return g.cursor_stmts_may_need_generic_scan(stmt.for_body_list())
		}
		.stmt_for_in {
			return g.cursor_expr_may_need_generic_scan(stmt.edge(0))
				|| g.cursor_expr_may_need_generic_scan(stmt.edge(1))
				|| g.cursor_expr_may_need_generic_scan(stmt.edge(2))
		}
		.stmt_block, .stmt_defer {
			for i in 0 .. stmt.edge_count() {
				if g.cursor_stmt_may_need_generic_scan(stmt.edge(i)) {
					return true
				}
			}
		}
		else {}
	}

	return false
}

fn (g &Gen) cursor_call_lhs_may_need_generic_scan(lhs ast.Cursor) bool {
	call_name := generic_call_short_name_cursor(lhs)
	if call_name != '' {
		for candidate in generic_call_decl_candidates(call_name) {
			if candidate in g.generic_fn_decl_index {
				return true
			}
		}
	}
	return g.cursor_expr_may_need_generic_scan(lhs)
}

fn (g &Gen) cursor_expr_may_need_generic_scan(expr ast.Cursor) bool {
	if !expr.is_valid() {
		return false
	}
	match expr.kind() {
		.expr_ident {
			name := expr.name()
			return name.contains('_T_') || name.ends_with('_T')
		}
		.typ_anon_struct, .typ_array_fixed, .typ_array, .typ_channel, .typ_fn, .typ_generic,
		.typ_map, .typ_option, .typ_pointer, .typ_result, .typ_thread, .typ_tuple {
			return g.type_cursor_may_need_generic_scan(expr)
		}
		.expr_generic_arg_or_index, .expr_generic_args, .expr_tuple {
			return true
		}
		.expr_call {
			if g.cursor_call_lhs_may_need_generic_scan(expr.edge(0)) {
				return true
			}
			for i in 1 .. expr.edge_count() {
				if g.cursor_expr_may_need_generic_scan(expr.edge(i)) {
					return true
				}
			}
		}
		.expr_call_or_cast {
			return g.cursor_call_lhs_may_need_generic_scan(expr.edge(0))
				|| g.cursor_expr_may_need_generic_scan(expr.edge(1))
		}
		.expr_infix {
			return g.cursor_expr_may_need_generic_scan(expr.edge(0))
				|| g.cursor_expr_may_need_generic_scan(expr.edge(1))
		}
		.expr_postfix, .expr_prefix, .expr_paren, .expr_modifier, .expr_lambda, .expr_comptime {
			return g.cursor_expr_may_need_generic_scan(expr.edge(0))
		}
		.expr_assoc {
			if g.type_cursor_may_need_generic_scan(expr.edge(0))
				|| g.cursor_expr_may_need_generic_scan(expr.edge(1)) {
				return true
			}
			for i in 2 .. expr.edge_count() {
				if g.cursor_expr_may_need_generic_scan(expr.edge(i).edge(0)) {
					return true
				}
			}
		}
		.expr_if_guard {
			return g.cursor_stmt_may_need_generic_scan(expr.edge(0))
		}
		.expr_or {
			if g.cursor_expr_may_need_generic_scan(expr.edge(0)) {
				return true
			}
			for i in 1 .. expr.edge_count() {
				if g.cursor_stmt_may_need_generic_scan(expr.edge(i)) {
					return true
				}
			}
		}
		.expr_unsafe {
			for i in 0 .. expr.edge_count() {
				if g.cursor_stmt_may_need_generic_scan(expr.edge(i)) {
					return true
				}
			}
		}
		.expr_lock {
			packed := u32(expr.extra_int())
			lock_len := int(packed & 0xFFFF)
			rlock_len := int((packed >> 16) & 0xFFFF)
			for i in 0 .. (lock_len + rlock_len) {
				if g.cursor_expr_may_need_generic_scan(expr.edge(i)) {
					return true
				}
			}
			for i in (lock_len + rlock_len) .. expr.edge_count() {
				if g.cursor_stmt_may_need_generic_scan(expr.edge(i)) {
					return true
				}
			}
		}
		.expr_if {
			if g.cursor_expr_may_need_generic_scan(expr.edge(0)) {
				return true
			}
			for i in 2 .. expr.edge_count() {
				if g.cursor_stmt_may_need_generic_scan(expr.edge(i)) {
					return true
				}
			}
			return g.cursor_expr_may_need_generic_scan(expr.edge(1))
		}
		.expr_match {
			if g.cursor_expr_may_need_generic_scan(expr.edge(0)) {
				return true
			}
			for i in 1 .. expr.edge_count() {
				branch := expr.edge(i)
				conds := branch.list_at(0)
				for ci in 0 .. conds.len() {
					if g.cursor_expr_may_need_generic_scan(conds.at(ci)) {
						return true
					}
				}
				if g.cursor_stmts_may_need_generic_scan(branch.list_at(1)) {
					return true
				}
			}
		}
		.expr_select {
			if g.cursor_stmt_may_need_generic_scan(expr.edge(0))
				|| g.cursor_expr_may_need_generic_scan(expr.edge(1)) {
				return true
			}
			for i in 2 .. expr.edge_count() {
				if g.cursor_stmt_may_need_generic_scan(expr.edge(i)) {
					return true
				}
			}
		}
		.expr_string_inter {
			inters := expr.list_at(1)
			for i in 0 .. inters.len() {
				inter := inters.at(i)
				if g.cursor_expr_may_need_generic_scan(inter.edge(0))
					|| g.cursor_expr_may_need_generic_scan(inter.edge(1)) {
					return true
				}
			}
		}
		.expr_init {
			if g.type_cursor_may_need_generic_scan(expr.edge(0)) {
				return true
			}
			for i in 1 .. expr.edge_count() {
				if g.cursor_expr_may_need_generic_scan(expr.edge(i).edge(0)) {
					return true
				}
			}
		}
		.expr_array_init {
			if g.type_cursor_may_need_generic_scan(expr.edge(0)) {
				return true
			}
			for i in 1 .. expr.edge_count() {
				if g.cursor_expr_may_need_generic_scan(expr.edge(i)) {
					return true
				}
			}
		}
		.expr_map_init {
			if g.type_cursor_may_need_generic_scan(expr.edge(0)) {
				return true
			}
			for i in 1 .. expr.edge_count() {
				if g.cursor_expr_may_need_generic_scan(expr.edge(i)) {
					return true
				}
			}
		}
		.expr_index, .expr_cast, .expr_as_cast, .expr_range {
			for i in 0 .. expr.edge_count() {
				if g.cursor_expr_may_need_generic_scan(expr.edge(i)) {
					return true
				}
			}
		}
		.expr_keyword_operator {
			for i in 0 .. expr.edge_count() {
				if g.cursor_expr_may_need_generic_scan(expr.edge(i)) {
					return true
				}
			}
		}
		.expr_fn_literal {
			captured_len := expr.extra_int()
			for i in 0 .. captured_len {
				if g.cursor_expr_may_need_generic_scan(expr.edge(1 + i)) {
					return true
				}
			}
			for i in (1 + captured_len) .. expr.edge_count() {
				if g.cursor_stmt_may_need_generic_scan(expr.edge(i)) {
					return true
				}
			}
		}
		.expr_sql, .expr_selector {
			return g.cursor_expr_may_need_generic_scan(expr.edge(0))
		}
		else {}
	}

	return false
}

fn (g &Gen) type_cursor_may_need_generic_scan(typ ast.Cursor) bool {
	if !typ.is_valid() {
		return false
	}
	match typ.kind() {
		.typ_generic, .expr_generic_arg_or_index, .expr_generic_args {
			return true
		}
		.expr_ident {
			name := typ.name()
			return name.contains('_T_') || name.ends_with('_T')
		}
		.expr_modifier, .expr_prefix, .expr_paren {
			return g.type_cursor_may_need_generic_scan(typ.edge(0))
		}
		.typ_anon_struct {
			generic_params := typ.list_at(0)
			for i in 0 .. generic_params.len() {
				if g.cursor_expr_may_need_generic_scan(generic_params.at(i)) {
					return true
				}
			}
			embedded := typ.list_at(1)
			for i in 0 .. embedded.len() {
				if g.cursor_expr_may_need_generic_scan(embedded.at(i)) {
					return true
				}
			}
			fields := typ.list_at(2)
			for i in 0 .. fields.len() {
				field := fields.at(i)
				if g.cursor_expr_may_need_generic_scan(field.edge(0))
					|| g.cursor_expr_may_need_generic_scan(field.edge(1)) {
					return true
				}
			}
		}
		.typ_array_fixed {
			return g.type_cursor_may_need_generic_scan(typ.edge(0))
				|| g.type_cursor_may_need_generic_scan(typ.edge(1))
		}
		.typ_array, .typ_channel, .typ_option, .typ_pointer, .typ_result, .typ_thread {
			return g.type_cursor_may_need_generic_scan(typ.edge(0))
		}
		.typ_fn {
			generic_params := typ.list_at(0)
			for i in 0 .. generic_params.len() {
				if g.cursor_expr_may_need_generic_scan(generic_params.at(i)) {
					return true
				}
			}
			params := typ.list_at(1)
			for i in 0 .. params.len() {
				if g.cursor_expr_may_need_generic_scan(params.at(i).edge(0)) {
					return true
				}
			}
			return g.type_cursor_may_need_generic_scan(typ.edge(2))
		}
		.typ_map {
			return g.type_cursor_may_need_generic_scan(typ.edge(0))
				|| g.type_cursor_may_need_generic_scan(typ.edge(1))
		}
		.typ_tuple {
			for i in 0 .. typ.edge_count() {
				if g.type_cursor_may_need_generic_scan(typ.edge(i)) {
					return true
				}
			}
		}
		else {}
	}

	return false
}

fn (g &Gen) call_lhs_may_need_generic_scan(lhs ast.Expr) bool {
	call_name := generic_call_short_name(lhs)
	if call_name != '' {
		for candidate in generic_call_decl_candidates(call_name) {
			if candidate in g.generic_fn_decl_index {
				return true
			}
		}
	}
	return g.expr_may_need_generic_scan(lhs)
}

fn (g &Gen) expr_may_need_generic_scan(expr ast.Expr) bool {
	match expr {
		ast.Ident {
			return expr.name.contains('_T_') || expr.name.ends_with('_T')
		}
		ast.Type {
			return type_expr_may_need_generic_scan(expr)
		}
		ast.GenericArgOrIndexExpr {
			return true
		}
		ast.GenericArgs {
			return true
		}
		ast.CallOrCastExpr {
			return g.expr_may_need_generic_scan(expr.lhs) || g.expr_may_need_generic_scan(expr.expr)
		}
		ast.CallExpr {
			if g.call_lhs_may_need_generic_scan(expr.lhs) {
				return true
			}
			for arg in expr.args {
				if g.expr_may_need_generic_scan(arg) {
					return true
				}
			}
		}
		ast.InfixExpr {
			return g.expr_may_need_generic_scan(expr.lhs) || g.expr_may_need_generic_scan(expr.rhs)
		}
		ast.PostfixExpr {
			return g.expr_may_need_generic_scan(expr.expr)
		}
		ast.PrefixExpr {
			return g.expr_may_need_generic_scan(expr.expr)
		}
		ast.ParenExpr {
			return g.expr_may_need_generic_scan(expr.expr)
		}
		ast.ModifierExpr {
			return g.expr_may_need_generic_scan(expr.expr)
		}
		ast.AssocExpr {
			if type_expr_may_need_generic_scan(expr.typ) || g.expr_may_need_generic_scan(expr.expr) {
				return true
			}
			for field in expr.fields {
				if g.expr_may_need_generic_scan(field.value) {
					return true
				}
			}
		}
		ast.IfGuardExpr {
			return g.stmts_may_need_generic_scan([ast.Stmt(expr.stmt)])
		}
		ast.OrExpr {
			return g.expr_may_need_generic_scan(expr.expr)
				|| g.stmts_may_need_generic_scan(expr.stmts)
		}
		ast.UnsafeExpr {
			return g.stmts_may_need_generic_scan(expr.stmts)
		}
		ast.LockExpr {
			for lock_expr in expr.lock_exprs {
				if g.expr_may_need_generic_scan(lock_expr) {
					return true
				}
			}
			for lock_expr in expr.rlock_exprs {
				if g.expr_may_need_generic_scan(lock_expr) {
					return true
				}
			}
			return g.stmts_may_need_generic_scan(expr.stmts)
		}
		ast.IfExpr {
			if g.expr_may_need_generic_scan(expr.cond) || g.stmts_may_need_generic_scan(expr.stmts) {
				return true
			}
			return expr.else_expr !is ast.EmptyExpr && g.expr_may_need_generic_scan(expr.else_expr)
		}
		ast.MatchExpr {
			if g.expr_may_need_generic_scan(expr.expr) {
				return true
			}
			for branch in expr.branches {
				for cond in branch.cond {
					if g.expr_may_need_generic_scan(cond) {
						return true
					}
				}
				if g.stmts_may_need_generic_scan(branch.stmts) {
					return true
				}
			}
		}
		ast.ComptimeExpr {
			return g.expr_may_need_generic_scan(expr.expr)
		}
		ast.SelectExpr {
			return g.stmts_may_need_generic_scan([expr.stmt])
				|| g.stmts_may_need_generic_scan(expr.stmts)
				|| g.expr_may_need_generic_scan(expr.next)
		}
		ast.StringInterLiteral {
			for item in expr.inters {
				if g.expr_may_need_generic_scan(item.expr)
					|| g.expr_may_need_generic_scan(item.format_expr) {
					return true
				}
			}
		}
		ast.InitExpr {
			if type_expr_may_need_generic_scan(expr.typ) {
				return true
			}
			for field in expr.fields {
				if g.expr_may_need_generic_scan(field.value) {
					return true
				}
			}
		}
		ast.ArrayInitExpr {
			if type_expr_may_need_generic_scan(expr.typ) || g.expr_may_need_generic_scan(expr.len) {
				return true
			}
			if g.expr_may_need_generic_scan(expr.init) || g.expr_may_need_generic_scan(expr.cap)
				|| g.expr_may_need_generic_scan(expr.update_expr) {
				return true
			}
			for item in expr.exprs {
				if g.expr_may_need_generic_scan(item) {
					return true
				}
			}
		}
		ast.MapInitExpr {
			if type_expr_may_need_generic_scan(expr.typ) {
				return true
			}
			for key in expr.keys {
				if g.expr_may_need_generic_scan(key) {
					return true
				}
			}
			for value in expr.vals {
				if g.expr_may_need_generic_scan(value) {
					return true
				}
			}
		}
		ast.IndexExpr {
			return g.expr_may_need_generic_scan(expr.lhs) || g.expr_may_need_generic_scan(expr.expr)
		}
		ast.CastExpr {
			return type_expr_may_need_generic_scan(expr.typ)
				|| g.expr_may_need_generic_scan(expr.expr)
		}
		ast.AsCastExpr {
			return type_expr_may_need_generic_scan(expr.typ)
				|| g.expr_may_need_generic_scan(expr.expr)
		}
		ast.Tuple {
			return true
		}
		ast.FieldInit {
			return g.expr_may_need_generic_scan(expr.value)
		}
		ast.KeywordOperator {
			for arg in expr.exprs {
				if g.expr_may_need_generic_scan(arg) {
					return true
				}
			}
		}
		ast.RangeExpr {
			return g.expr_may_need_generic_scan(expr.start)
				|| g.expr_may_need_generic_scan(expr.end)
		}
		ast.FnLiteral {
			for captured in expr.captured_vars {
				if g.expr_may_need_generic_scan(captured) {
					return true
				}
			}
			return g.stmts_may_need_generic_scan(expr.stmts)
		}
		ast.LambdaExpr {
			return g.expr_may_need_generic_scan(expr.expr)
		}
		ast.SqlExpr {
			return g.expr_may_need_generic_scan(expr.expr)
		}
		else {}
	}

	return false
}

fn type_expr_may_need_generic_scan(expr ast.Expr) bool {
	match expr {
		ast.Type {
			if expr is ast.GenericType {
				return true
			}
			if expr is ast.ArrayType {
				return type_expr_may_need_generic_scan(expr.elem_type)
			}
			if expr is ast.ArrayFixedType {
				return type_expr_may_need_generic_scan(expr.elem_type)
					|| type_expr_may_need_generic_scan(expr.len)
			}
			if expr is ast.MapType {
				return type_expr_may_need_generic_scan(expr.key_type)
					|| type_expr_may_need_generic_scan(expr.value_type)
			}
			if expr is ast.OptionType {
				return type_expr_may_need_generic_scan(expr.base_type)
			}
			if expr is ast.ResultType {
				return type_expr_may_need_generic_scan(expr.base_type)
			}
			if expr is ast.PointerType {
				return type_expr_may_need_generic_scan(expr.base_type)
			}
			if expr is ast.FnType {
				for param in expr.params {
					if type_expr_may_need_generic_scan(param.typ) {
						return true
					}
				}
				return type_expr_may_need_generic_scan(expr.return_type)
			}
			return false
		}
		ast.GenericArgOrIndexExpr {
			return true
		}
		ast.GenericArgs {
			return true
		}
		ast.Ident {
			return expr.name.contains('_T_') || expr.name.ends_with('_T')
		}
		ast.ModifierExpr {
			return type_expr_may_need_generic_scan(expr.expr)
		}
		ast.PrefixExpr {
			return type_expr_may_need_generic_scan(expr.expr)
		}
		ast.ParenExpr {
			return type_expr_may_need_generic_scan(expr.expr)
		}
		else {
			return false
		}
	}
}

fn (mut g Gen) scan_stmts_for_generic_types(stmts []ast.Stmt) {
	for stmt in stmts {
		if stmt is ast.AssignStmt {
			for rhs in stmt.rhs {
				g.scan_expr_for_generic_types(rhs)
				g.scan_expr_stmts_for_generic_types(rhs)
			}
			for i, lhs in stmt.lhs {
				if i >= stmt.rhs.len {
					continue
				}
				lhs_name := generic_wrapped_ident_name(lhs)
				if lhs_name == '' {
					continue
				}

				mut rhs_type := g.get_expr_type(stmt.rhs[i]).trim_space()
				if generic_scan_type_is_unresolved(rhs_type) {
					if raw := g.get_raw_type(stmt.rhs[i]) {
						rhs_type = g.types_type_to_c(raw).trim_space()
					}
				}
				if generic_scan_type_is_unresolved(rhs_type) {
					rhs_type = (g.get_local_var_c_type(lhs_name) or { '' }).trim_space()
				}
				if generic_scan_type_is_unresolved(rhs_type) && stmt.rhs[i] is ast.ArrayInitExpr {
					array_init := stmt.rhs[i] as ast.ArrayInitExpr
					rhs_type = g.expr_type_to_c(array_init.typ).trim_space()
				}
				if !generic_scan_type_is_unresolved(rhs_type) {
					g.remember_runtime_local_type(lhs_name, rhs_type)
				} else if stmt.rhs[i] is ast.ArrayInitExpr {
					g.remember_runtime_current_local_type(lhs_name, 'array')
				}
			}
		} else if stmt is ast.ReturnStmt {
			for expr in stmt.exprs {
				g.scan_expr_for_generic_types(expr)
				g.scan_expr_stmts_for_generic_types(expr)
			}
		} else if stmt is ast.ExprStmt {
			g.scan_expr_for_generic_types(stmt.expr)
			// Recurse into IfExpr/MatchExpr bodies
			g.scan_expr_stmts_for_generic_types(stmt.expr)
		} else if stmt is ast.ComptimeStmt {
			if stmt.stmt is ast.ForStmt && g.scan_comptime_for_for_generic_types(stmt.stmt) {
				continue
			}
			if stmt.stmt is ast.ExprStmt && stmt.stmt.expr is ast.ComptimeExpr
				&& (stmt.stmt.expr as ast.ComptimeExpr).expr is ast.IfExpr {
				g.scan_comptime_if_for_generic_types((stmt.stmt.expr as ast.ComptimeExpr).expr as ast.IfExpr)
				continue
			}
			// Recurse into comptime $if/$for bodies (wraps a single stmt)
			g.scan_stmts_for_generic_types([stmt.stmt])
		} else if stmt is ast.ForStmt {
			g.scan_stmts_for_generic_types(stmt.stmts)
		} else if stmt is ast.BlockStmt {
			g.scan_stmts_for_generic_types(stmt.stmts)
		}
	}
}

fn (mut g Gen) scan_cursor_stmts_for_generic_types(stmts ast.CursorList) {
	for i in 0 .. stmts.len() {
		g.scan_cursor_stmt_for_generic_types(stmts.at(i))
	}
}

fn (mut g Gen) scan_cursor_stmt_for_generic_types(stmt ast.Cursor) {
	if !stmt.is_valid() {
		return
	}
	match stmt.kind() {
		.stmt_assign {
			lhs_len := stmt.extra_int()
			for i in lhs_len .. stmt.edge_count() {
				g.scan_expr_cursor_for_generic_types(stmt.edge(i))
			}
			for i in 0 .. lhs_len {
				rhs_idx := lhs_len + i
				if rhs_idx >= stmt.edge_count() {
					continue
				}
				g.remember_assign_cursor_runtime_local_type(stmt.edge(i), stmt.edge(rhs_idx))
			}
		}
		.stmt_return {
			for i in 0 .. stmt.edge_count() {
				g.scan_expr_cursor_for_generic_types(stmt.edge(i))
			}
		}
		.stmt_expr {
			g.scan_expr_cursor_for_generic_types(stmt.edge(0))
		}
		.stmt_comptime {
			inner := stmt.edge(0)
			if inner.kind() == .stmt_for && g.scan_comptime_for_cursor_for_generic_types(inner) {
				return
			}
			if inner.kind() == .stmt_expr {
				expr := inner.edge(0)
				if expr.kind() == .expr_comptime && expr.edge(0).kind() == .expr_if {
					g.scan_comptime_if_cursor_for_generic_types(expr.edge(0))
					return
				}
			}
			g.scan_cursor_stmt_for_generic_types(inner)
		}
		.stmt_for {
			g.scan_cursor_stmts_for_generic_types(stmt.for_body_list())
		}
		.stmt_block, .stmt_defer {
			for i in 0 .. stmt.edge_count() {
				g.scan_cursor_stmt_for_generic_types(stmt.edge(i))
			}
		}
		else {}
	}
}

fn generic_call_cursor_arg_count(call ast.Cursor) int {
	return if call.edge_count() > 1 { call.edge_count() - 1 } else { 0 }
}

fn generic_call_cursor_arg(call ast.Cursor, idx int) ast.Cursor {
	edge_idx := idx + 1
	if idx < 0 || edge_idx >= call.edge_count() {
		return ast.Cursor{}
	}
	return call.edge(edge_idx)
}

fn sum_payload_ident_name_from_cursor(expr ast.Cursor) ?string {
	if !expr.is_valid() {
		return none
	}
	match expr.kind() {
		.expr_ident {
			return expr.name()
		}
		.expr_as_cast, .expr_modifier, .expr_paren {
			return sum_payload_ident_name_from_cursor(expr.edge(0))
		}
		.expr_cast {
			return sum_payload_ident_name_from_cursor(expr.edge(1))
		}
		.expr_prefix {
			if unsafe { token.Token(int(expr.aux())) } == .mul {
				return sum_payload_ident_name_from_cursor(expr.edge(0))
			}
		}
		.expr_selector {
			lhs := expr.edge(0)
			rhs := expr.edge(1)
			if lhs.kind() == .expr_selector && lhs.edge(1).name() == '_data' {
				return sum_payload_ident_name_from_cursor(lhs.edge(0))
			}
			if rhs.name() == '_data' {
				return sum_payload_ident_name_from_cursor(lhs)
			}
		}
		else {}
	}

	return none
}

fn (mut g Gen) resolve_sum_payload_arg_type_name_cursor(arg ast.Cursor, candidate_type string) string {
	mut type_name := candidate_type.trim_space()
	if type_name == '' || type_name == 'int' {
		return type_name
	}
	ident_name := sum_payload_ident_name_from_cursor(arg) or { return type_name }
	decl_type :=
		(g.get_local_var_c_type(ident_name) or { return type_name }).trim_space().trim_right('*')
	if decl_type == '' || type_name == decl_type || type_name == decl_type + '*' {
		return type_name
	}
	mut ptr_suffix := ''
	for type_name.ends_with('*') {
		ptr_suffix += '*'
		type_name = type_name[..type_name.len - 1].trim_space()
	}
	variants := g.get_sum_type_variants_for(decl_type)
	if variants.len == 0 {
		return type_name + ptr_suffix
	}
	mut variant_field := g.sum_type_variant_field_name(decl_type, type_name)
	if variant_field !in variants {
		for variant in variants {
			if sum_type_variant_matches(variant, type_name) {
				variant_field = variant
				break
			}
		}
	}
	if variant_field !in variants {
		return type_name + ptr_suffix
	}
	payload_type := g.sum_type_variant_payload_type(decl_type, type_name, variant_field)
	if payload_type == '' {
		return type_name + ptr_suffix
	}
	return payload_type + ptr_suffix
}

fn (mut g Gen) get_comptime_selector_type_cursor(node ast.Cursor) string {
	if node.kind() != .expr_selector {
		return ''
	}
	lhs := node.edge(0)
	rhs_name := node.edge(1).name()
	if is_comptime_selector_rhs_name(rhs_name) {
		return g.comptime_field_type
	}
	if lhs.kind() == .expr_ident && lhs.name() == g.comptime_field_var {
		match rhs_name {
			'name' {
				return 'string'
			}
			'typ', 'unaliased_typ' {
				return 'int'
			}
			'attrs' {
				return 'Array_string'
			}
			'is_pub', 'is_mut', 'is_embed', 'is_shared', 'is_atomic', 'is_option', 'is_array',
			'is_map', 'is_chan', 'is_enum', 'is_struct', 'is_alias' {
				return 'bool'
			}
			else {}
		}
	}
	if lhs.kind() == .expr_selector && lhs.edge(0).kind() == .expr_ident
		&& lhs.edge(0).name() == g.comptime_field_var && lhs.edge(1).name() == 'name' {
		match rhs_name {
			'str' {
				return 'char*'
			}
			'len' {
				return 'int'
			}
			else {}
		}
	}
	return ''
}

fn (mut g Gen) generic_specialization_arg_type_name_cursor(arg ast.Cursor) string {
	mut base_arg := generic_call_base_arg_cursor(arg)
	for base_arg.is_valid() && base_arg.kind() == .expr_paren {
		base_arg = base_arg.edge(0)
	}
	if !base_arg.is_valid() {
		return ''
	}
	if base_arg.kind() == .expr_prefix && unsafe { token.Token(int(base_arg.aux())) } == .amp {
		inner_type_name := g.generic_specialization_arg_type_name_cursor(base_arg.edge(0))
		if inner_type_name != '' && inner_type_name != 'int' {
			return inner_type_name.trim_right('*') + '*'
		}
	}
	mut arg_type_name := ''
	if base_arg.kind() == .expr_selector {
		rhs := base_arg.edge(1)
		if is_comptime_selector_rhs_name(rhs.name()) {
			comptime_type := g.get_comptime_selector_type_cursor(base_arg).trim_space()
			if comptime_type != '' && comptime_type != 'int' {
				return comptime_type
			}
		}
		arg_type_name =
			g.selector_declared_field_type_cursor_for_generic_scan(base_arg).trim_space()
		if arg_type_name == '' || arg_type_name == 'array' {
			arg_type_name = g.generic_scan_expr_cursor_c_type(base_arg).trim_space()
		}
		lhs := base_arg.edge(0)
		if lhs.kind() == .expr_ident {
			if placeholder := g.cur_fn_generic_params[lhs.name()] {
				if concrete := g.active_generic_types[placeholder] {
					mut struct_type := g.resolve_type_to_struct(concrete)
					if struct_type.fields.len == 0 {
						struct_c_name := g.types_type_to_c(concrete).trim_space().trim_right('*')
						struct_type = g.lookup_struct_type_by_c_name(struct_c_name)
					}
					for field in struct_type.fields {
						if field.name == rhs.name() {
							arg_type_name = g.types_type_to_c(field.typ)
							break
						}
					}
				}
			}
		}
	}
	if arg_type_name == '' {
		if active_concrete := g.active_generic_concrete_from_arg_cursor(base_arg) {
			arg_type_name = g.generic_specialization_token_from_type(active_concrete)
		}
	}
	if arg_type_name == '' && base_arg.kind() == .expr_init {
		arg_type_name = g.expr_type_to_c(base_arg.edge(0).type_expr()).trim_space()
	}
	if arg_type_name == '' && base_arg.kind() == .expr_cast {
		arg_type_name = g.expr_type_to_c(base_arg.edge(0).type_expr()).trim_space()
	}
	if arg_type_name == '' || arg_type_name == 'int' {
		arg_type_name = g.generic_scan_expr_cursor_c_type(base_arg).trim_space()
	}
	if (arg_type_name == '' || arg_type_name == 'int') && base_arg.kind() == .expr_ident {
		arg_type_name = (g.get_local_var_c_type(base_arg.name()) or { '' }).trim_space()
	}
	if arg_type_name == '' || arg_type_name == 'int' {
		if raw := g.raw_type_from_generic_call_arg_cursor(base_arg) {
			arg_type_name = g.types_type_to_c(raw).trim_space()
		}
	}
	return if arg_type_name.trim_space() == 'void*' {
		'voidptr'
	} else {
		g.resolve_sum_payload_arg_type_name_cursor(base_arg, arg_type_name).trim_space()
	}
}

fn (mut g Gen) generic_call_has_concrete_arg_bindings_cursor(decl ast.FnDecl, call ast.Cursor, generic_params []string) bool {
	if generic_params.len == 0 || g.env == unsafe { nil } {
		return false
	}
	arg_count := generic_call_cursor_arg_count(call)
	mut bindings := map[string]types.Type{}
	if decl.is_method && arg_count > 0 && expr_has_generic_placeholder(decl.receiver.typ) {
		recv_arg := generic_call_base_arg_cursor(generic_call_cursor_arg(call, 0))
		if recv_arg.is_valid() {
			if concrete := g.concrete_type_from_generic_call_arg_cursor(recv_arg) {
				g.infer_generic_type_bindings_from_param(decl.receiver.typ,
					g.concrete_type_with_active_generics(concrete), generic_params, mut bindings)
			}
		}
	}
	arg_offset := if decl.is_method && arg_count == decl.typ.params.len + 1 { 1 } else { 0 }
	for i, param in decl.typ.params {
		arg_idx := i + arg_offset
		if arg_idx >= arg_count || !expr_has_generic_placeholder(param.typ) {
			continue
		}
		arg := generic_call_base_arg_cursor(generic_call_cursor_arg(call, arg_idx))
		if !arg.is_valid() {
			continue
		}
		concrete := g.concrete_type_from_generic_call_arg_cursor(arg) or { continue }
		g.infer_generic_type_bindings_from_param(param.typ, g.concrete_type_for_generic_param(param,
			concrete), generic_params, mut bindings)
	}
	for param_name in generic_params {
		concrete := bindings[param_name] or { return false }
		if !g.generic_concrete_type_is_runtime_specializable(concrete) {
			return false
		}
	}
	return true
}

fn (mut g Gen) try_specialize_generic_call_name_cursor(name string, call ast.Cursor) ?string {
	if name == '' {
		return none
	}
	if candidate := g.same_receiver_specialized_method_name(name) {
		return candidate
	}
	if name == 'copy' {
		return none
	}
	arg_count := generic_call_cursor_arg_count(call)
	is_struct_field_should_encode :=
		name in ['struct_field_should_encode_T', 'json2__struct_field_should_encode_T', 'struct_field_should_encode', 'json2__struct_field_should_encode']
		|| name.starts_with('struct_field_should_encode_T_')
		|| name.starts_with('json2__struct_field_should_encode_T_')
	if is_struct_field_should_encode && arg_count > 1 {
		arg_type_name :=
			g.generic_specialization_arg_type_name_cursor(generic_call_cursor_arg(call, 1))
		if arg_type_name != '' {
			g.record_late_single_generic_call_spec_from_c_type('struct_field_should_encode', 'T',
				arg_type_name)
			spec_token := generic_specialization_token_from_c_type_name(arg_type_name)
			for base_name in [g.qualify_local_call_name('struct_field_should_encode'),
				'json2__struct_field_should_encode', 'struct_field_should_encode'] {
				if candidate := g.find_specialized_call_name(base_name, spec_token) {
					return candidate
				}
				g.ensure_specialized_call_signature('${base_name}_T_${spec_token}')
				if candidate := g.find_specialized_call_name(base_name, spec_token) {
					return candidate
				}
			}
		}
	}
	is_encode_struct_field_value :=
		name in ['Encoder__encode_struct_field_value_T', 'json2__Encoder__encode_struct_field_value_T', 'Encoder__encode_struct_field_value', 'json2__Encoder__encode_struct_field_value']
		|| name.starts_with('Encoder__encode_struct_field_value_T_')
		|| name.starts_with('json2__Encoder__encode_struct_field_value_T_')
	if is_encode_struct_field_value && arg_count > 1 {
		arg_type_name :=
			g.generic_specialization_arg_type_name_cursor(generic_call_cursor_arg(call, 1))
		if arg_type_name != '' {
			g.record_late_single_generic_call_spec_from_c_type('encode_struct_field_value', 'T',
				arg_type_name)
			spec_token := generic_specialization_token_from_c_type_name(arg_type_name)
			for base_name in [
				g.qualify_local_call_name('Encoder__encode_struct_field_value'),
				'json2__Encoder__encode_struct_field_value',
				'Encoder__encode_struct_field_value',
			] {
				if candidate := g.find_specialized_call_name(base_name, spec_token) {
					return candidate
				}
				g.ensure_specialized_call_signature('${base_name}_T_${spec_token}')
				if candidate := g.find_specialized_call_name(base_name, spec_token) {
					return candidate
				}
			}
		}
	}
	if name.ends_with('_T') {
		base_name := generic_call_base_name_for_specialization(name)
		if base_name != '' {
			mut has_arg_bindings := false
			if generic_decl := g.find_generic_fn_decl_by_base_name(base_name) {
				has_arg_bindings = g.generic_call_has_concrete_arg_bindings_cursor(generic_decl,
					call, g.generic_fn_param_names(generic_decl))
			}
			cur_fn_specialized_name := if g.cur_fn_c_name != '' {
				g.cur_fn_c_name
			} else {
				g.cur_fn_name
			}
			if cur_fn_specialized_name.contains('_T_') && !has_arg_bindings {
				suffix := cur_fn_specialized_name.all_after('_T_')
				if suffix != '' {
					candidate := '${base_name}_T_${suffix}'
					g.ensure_specialized_call_signature(candidate)
					if candidate in g.fn_param_is_ptr || candidate in g.fn_return_types {
						return candidate
					}
					if !base_name.contains('__') {
						qualified := g.qualify_local_call_name(candidate)
						g.ensure_specialized_call_signature(qualified)
						if qualified != candidate
							&& (qualified in g.fn_param_is_ptr || qualified in g.fn_return_types) {
							return qualified
						}
					}
				}
			}
			if g.active_generic_types.len > 0 && !has_arg_bindings {
				if concrete := g.active_generic_types['T'] {
					spec_token := g.generic_specialization_token_from_type(concrete)
					candidate := '${base_name}_T_${spec_token}'
					g.ensure_specialized_call_signature(candidate)
					if candidate in g.fn_param_is_ptr || candidate in g.fn_return_types {
						return candidate
					}
					if !base_name.contains('__') {
						qualified := g.qualify_local_call_name(candidate)
						g.ensure_specialized_call_signature(qualified)
						if qualified != candidate
							&& (qualified in g.fn_param_is_ptr || qualified in g.fn_return_types) {
							return qualified
						}
					}
				}
			}
			if g.active_generic_types.len == 0 {
				if candidate := g.find_specialized_call_name(g.qualify_local_call_name(base_name),
					'f64')
				{
					return candidate
				}
				if candidate := g.find_specialized_call_name(base_name, 'f64') {
					return candidate
				}
			}
		}
	}
	if name in ['decode_value_T', 'json2__decode_value_T'] && arg_count > 1 {
		arg_type_name :=
			g.generic_specialization_arg_type_name_cursor(generic_call_cursor_arg(call, 1)).trim_space().trim_right('*')
		if arg_type_name != '' && arg_type_name != 'int' {
			if candidate := g.find_specialized_call_name(g.qualify_local_call_name('decode_value'),
				sanitize_generic_token_part(arg_type_name))
			{
				return candidate
			}
		}
	}
	if name in ['Encoder__encode_value_T', 'json2__Encoder__encode_value_T', 'Encoder__encode_value', 'json2__Encoder__encode_value']
		&& arg_count > 1 {
		arg_type_name :=
			g.generic_specialization_arg_type_name_cursor(generic_call_cursor_arg(call, 1)).trim_space().trim_right('*')
		if arg_type_name != '' && arg_type_name != 'int' {
			if candidate := g.find_specialized_call_name('json2__Encoder__encode_value',
				sanitize_generic_token_part(arg_type_name))
			{
				return candidate
			}
		}
	}
	if g.active_generic_types.len > 0 && name.contains('_T_') {
		base_name := name.all_before('_T_')
		mut parts := name.all_after('_T_').split('_')
		mut c_parts := parts.clone()
		mut changed := false
		for i, part in parts {
			if concrete := g.active_generic_types[part] {
				parts[i] = g.generic_specialization_token_from_type(concrete)
				c_type := g.types_type_to_c(concrete).trim_space().trim_right('*')
				c_parts[i] = if c_type == '' {
					parts[i]
				} else {
					sanitize_generic_token_part(c_type)
				}
				changed = true
			}
		}
		if changed {
			candidate := '${base_name}_T_${parts.join('_')}'
			if candidate in g.fn_param_is_ptr || candidate in g.fn_return_types {
				return candidate
			}
			g.ensure_specialized_call_signature(candidate)
			if candidate in g.fn_param_is_ptr || candidate in g.fn_return_types {
				return candidate
			}
			if !base_name.contains('__') {
				qualified := g.qualify_local_call_name(candidate)
				if qualified != candidate
					&& (qualified in g.fn_param_is_ptr || qualified in g.fn_return_types) {
					return qualified
				}
				g.ensure_specialized_call_signature(qualified)
				if qualified != candidate
					&& (qualified in g.fn_param_is_ptr || qualified in g.fn_return_types) {
					return qualified
				}
			}
			c_candidate := '${base_name}_T_${c_parts.join('_')}'
			if c_candidate != candidate
				&& (c_candidate in g.fn_param_is_ptr || c_candidate in g.fn_return_types) {
				return c_candidate
			}
			if c_candidate != candidate {
				g.ensure_specialized_call_signature(c_candidate)
				if c_candidate in g.fn_param_is_ptr || c_candidate in g.fn_return_types {
					return c_candidate
				}
			}
			if c_candidate != candidate && !base_name.contains('__') {
				qualified := g.qualify_local_call_name(c_candidate)
				if qualified != c_candidate
					&& (qualified in g.fn_param_is_ptr || qualified in g.fn_return_types) {
					return qualified
				}
				g.ensure_specialized_call_signature(qualified)
				if qualified != c_candidate
					&& (qualified in g.fn_param_is_ptr || qualified in g.fn_return_types) {
					return qualified
				}
			}
		}
	}
	if g.active_generic_types.len > 0 {
		mut parts := name.split('_')
		mut changed := false
		for i, part in parts {
			if concrete := g.active_generic_types[part] {
				parts[i] = 'T_${g.generic_specialization_token_from_type(concrete)}'
				changed = true
			}
		}
		if changed {
			candidate := parts.join('_')
			if candidate in g.fn_param_is_ptr || candidate in g.fn_return_types {
				return candidate
			}
		}
	}
	if !name.contains('__') {
		qualified := g.qualify_local_call_name(name)
		if qualified != name && (qualified in g.fn_param_is_ptr || qualified in g.fn_return_types)
			&& !g.has_generic_fn_decl_by_base_name(qualified) {
			return none
		}
	}
	mut skip_arg0_specialization := false
	arg0_lookup_name := if name.contains('_T_') {
		name.all_before('_T_')
	} else if name.ends_with('_T') {
		name[..name.len - 2]
	} else {
		name
	}
	if generic_decl := g.find_generic_fn_decl_by_base_name(arg0_lookup_name) {
		skip_arg0_specialization = generic_decl.is_method || generic_decl.typ.params.len == 0
			|| !expr_has_generic_placeholder(generic_decl.typ.params[0].typ)
	}
	if arg_count > 0 && !skip_arg0_specialization {
		mut arg_type_name :=
			g.generic_specialization_arg_type_name_cursor(generic_call_cursor_arg(call, 0))
		if arg_type_name != '' && arg_type_name != 'int' && arg_type_name.trim_space() != 'void*' {
			arg_type_name = g.resolve_sum_payload_arg_type_name_cursor(generic_call_cursor_arg(call, 0),
				arg_type_name)
		}
		arg_type_name = if arg_type_name.trim_space() == 'void*' {
			'voidptr'
		} else {
			arg_type_name.trim_right('*')
		}
		if arg_type_name != '' && arg_type_name != 'int' {
			mut candidate_types := [arg_type_name]
			if !arg_type_name.contains('__') && g.cur_module != '' && g.cur_module != 'main'
				&& g.cur_module != 'builtin' {
				candidate_types << '${g.cur_module}__${arg_type_name}'
				if arg_type_name.starts_with('Array_') && arg_type_name.len > 'Array_'.len {
					elem_type_name := arg_type_name['Array_'.len..]
					if !elem_type_name.contains('__') {
						candidate_types << 'Array_${g.cur_module}__${elem_type_name}'
					}
				}
			}
			for concrete_type_name in candidate_types {
				if candidate := g.find_specialized_call_name(name,
					sanitize_generic_token_part(concrete_type_name))
				{
					return candidate
				}
			}
		}
	}
	if name in ['decode_value', 'json2__decode_value'] && g.cur_fn_ret_type.starts_with('_result_') {
		result_base := g.cur_fn_ret_type['_result_'.len..]
		if candidate := g.find_specialized_call_name(g.qualify_local_call_name('decode_value'),
			sanitize_generic_token_part(result_base))
		{
			return candidate
		}
	}
	mut generic_base_name := generic_call_base_name_for_specialization(name)
	if qualified_name := g.qualified_generic_fn_base_name(generic_base_name) {
		generic_base_name = qualified_name
	}
	decl := g.find_generic_fn_decl_by_base_name(generic_base_name) or {
		if generic_base_name.contains('__') {
			g.find_generic_fn_decl_by_base_name(generic_base_name.all_after_last('__')) or {
				return none
			}
		} else {
			return none
		}
	}
	generic_params := g.generic_fn_param_names(decl)
	if generic_params.len == 0 || g.env == unsafe { nil } {
		return none
	}
	if name.contains('_T_') {
		if active_bindings := g.active_generic_bindings_matching_name_suffix(name, generic_params) {
			mut suffixes := []string{cap: generic_params.len}
			for param_name in generic_params {
				concrete := active_bindings[param_name] or { return none }
				suffixes << g.generic_specialization_token_from_type(concrete)
			}
			candidate := '${generic_base_name}_T_${suffixes.join('_')}'
			g.ensure_specialized_call_signature(candidate)
			if candidate in g.fn_param_is_ptr || candidate in g.fn_return_types {
				return candidate
			}
			specialized_candidate := g.specialized_fn_name(decl, active_bindings)
			if specialized_candidate != candidate {
				g.ensure_specialized_call_signature(specialized_candidate)
				if specialized_candidate in g.fn_param_is_ptr
					|| specialized_candidate in g.fn_return_types {
					return specialized_candidate
				}
			}
		}
	}
	has_arg_bindings := g.generic_call_has_concrete_arg_bindings_cursor(decl, call, generic_params)
	cur_fn_specialized_name := if g.cur_fn_c_name != '' { g.cur_fn_c_name } else { g.cur_fn_name }
	if cur_fn_specialized_name.contains('_T_') && !has_arg_bindings {
		suffix := cur_fn_specialized_name.all_after('_T_')
		if suffix != '' {
			candidate := '${generic_base_name}_T_${suffix}'
			if candidate in g.fn_param_is_ptr || candidate in g.fn_return_types {
				return candidate
			}
			if !generic_base_name.contains('__') {
				qualified := g.qualify_local_call_name(candidate)
				if qualified != candidate
					&& (qualified in g.fn_param_is_ptr || qualified in g.fn_return_types) {
					return qualified
				}
			}
		}
	}
	if g.active_generic_types.len > 0 && !has_arg_bindings {
		mut active_bindings := map[string]types.Type{}
		mut suffixes := []string{}
		for param_name in generic_params {
			concrete := g.active_generic_types[param_name] or { break }
			active_bindings[param_name] = concrete
			suffixes << g.generic_specialization_token_from_type(concrete)
		}
		if active_bindings.len == generic_params.len {
			candidate := g.specialized_fn_name(decl, active_bindings)
			g.ensure_specialized_call_signature(candidate)
			if candidate in g.fn_param_is_ptr || candidate in g.fn_return_types {
				return candidate
			}
			alt_candidate := '${generic_base_name}_T_${suffixes.join('_')}'
			g.ensure_specialized_call_signature(alt_candidate)
			if alt_candidate in g.fn_param_is_ptr || alt_candidate in g.fn_return_types {
				return alt_candidate
			}
		}
	}
	if g.generic_call_name_has_placeholder_suffix(name) {
		if return_bindings := g.infer_generic_bindings_from_current_return(decl, generic_params) {
			g.record_late_generic_call_spec(generic_base_name, return_bindings)
			candidate := g.specialized_fn_name(decl, return_bindings)
			g.ensure_specialized_call_signature(candidate)
			if candidate in g.fn_param_is_ptr || candidate in g.fn_return_types {
				return candidate
			}
			mut suffixes := []string{cap: generic_params.len}
			for param_name in generic_params {
				concrete := return_bindings[param_name] or { return none }
				suffixes << g.generic_specialization_token_from_type(concrete)
			}
			alt_candidate := '${generic_base_name}_T_${suffixes.join('_')}'
			g.ensure_specialized_call_signature(alt_candidate)
			if alt_candidate in g.fn_param_is_ptr || alt_candidate in g.fn_return_types {
				return alt_candidate
			}
		}
	}
	if generic_params.len == 1 {
		param_name := generic_params[0]
		arg_offset := if decl.is_method && arg_count == decl.typ.params.len + 1 { 1 } else { 0 }
		for i, param in decl.typ.params {
			arg_idx := i + arg_offset
			if arg_idx >= arg_count || !expr_has_generic_placeholder(param.typ) {
				continue
			}
			direct_placeholder := direct_generic_placeholder_name(param.typ)
			if direct_placeholder != param_name {
				continue
			}
			base_arg := generic_call_base_arg_cursor(generic_call_cursor_arg(call, arg_idx))
			mut arg_type_name := g.generic_specialization_arg_type_name_cursor(base_arg)
			if base_arg.kind() == .expr_init {
				arg_type_name = g.expr_type_to_c(base_arg.edge(0).type_expr()).trim_space()
			}
			if arg_type_name != '' && arg_type_name != 'int'
				&& arg_type_name.trim_space() != 'void*' {
				arg_type_name = g.resolve_sum_payload_arg_type_name_cursor(base_arg, arg_type_name)
			}
			arg_type_name = if arg_type_name.trim_space() == 'void*' {
				'voidptr'
			} else {
				arg_type_name.trim_space()
			}
			if arg_type_name == '' || arg_type_name == 'int' {
				continue
			}
			is_direct_array_param := param.typ is ast.Type && param.typ is ast.ArrayType
			mut candidate_types := [arg_type_name]
			if !arg_type_name.contains('__') && g.cur_module != '' && g.cur_module != 'main'
				&& g.cur_module != 'builtin' {
				candidate_types << '${g.cur_module}__${arg_type_name}'
				if is_direct_array_param && arg_type_name.starts_with('Array_')
					&& arg_type_name.len > 'Array_'.len {
					elem_type_name := arg_type_name['Array_'.len..]
					if !elem_type_name.contains('__') {
						candidate_types << 'Array_${g.cur_module}__${elem_type_name}'
					}
				}
			}
			for concrete_type_name in candidate_types {
				spec_token := generic_specialization_token_for_direct_param(param.typ,
					concrete_type_name)
				if candidate := g.find_specialized_call_name(name, spec_token) {
					return candidate
				}
				if !is_direct_array_param {
					continue
				}
				candidate_name := '${name}_T_${spec_token}'
				g.ensure_specialized_call_signature(candidate_name)
				if candidate := g.find_specialized_call_name(name, spec_token) {
					return candidate
				}
				if !name.contains('__') {
					qualified := g.qualify_local_call_name(candidate_name)
					g.ensure_specialized_call_signature(qualified)
					if candidate := g.find_specialized_call_name(name, spec_token) {
						return candidate
					}
				}
			}
		}
	}
	mut bindings := map[string]types.Type{}
	mut receiver_bindings := map[string]types.Type{}
	if decl.is_method && arg_count > 0 && expr_has_generic_placeholder(decl.receiver.typ) {
		recv_arg := generic_call_base_arg_cursor(generic_call_cursor_arg(call, 0))
		if recv_arg.is_valid() {
			if concrete := g.concrete_type_from_generic_call_arg_cursor(recv_arg) {
				g.infer_generic_type_bindings_from_param(decl.receiver.typ,
					g.concrete_type_with_active_generics(concrete),
					receiver_generic_param_names(decl), mut receiver_bindings)
			}
		}
	}
	arg_offset := if decl.is_method && arg_count == decl.typ.params.len + 1 { 1 } else { 0 }
	for i, param in decl.typ.params {
		arg_idx := i + arg_offset
		if arg_idx >= arg_count || !expr_has_generic_placeholder(param.typ) {
			continue
		}
		arg := generic_call_base_arg_cursor(generic_call_cursor_arg(call, arg_idx))
		if !arg.is_valid() {
			continue
		}
		concrete := g.concrete_type_from_generic_call_arg_cursor(arg) or { continue }
		g.infer_generic_type_bindings_from_param(param.typ, g.concrete_type_for_generic_param(param,
			concrete), generic_params, mut bindings)
	}
	if bindings.len != generic_params.len {
		return none
	}
	for _, concrete in bindings {
		if type_contains_generic_placeholder(concrete) {
			return none
		}
	}
	g.record_late_generic_call_spec(generic_base_name, bindings)
	mut suffixes := []string{}
	for param_name in generic_params {
		if concrete := bindings[param_name] {
			suffixes << g.generic_specialization_token_from_type(concrete)
		}
	}
	if suffixes.len > 0 {
		lookup_name := generic_base_name
		if candidate := g.find_specialized_call_name(lookup_name, suffixes.join('_')) {
			return candidate
		}
		suffix := suffixes.join('_')
		alt_candidate := '${lookup_name}_T_${suffix}'
		g.ensure_specialized_call_signature(alt_candidate)
		if alt_candidate in g.fn_param_is_ptr || alt_candidate in g.fn_return_types {
			return alt_candidate
		}
	}
	mut combined_bindings := receiver_bindings.clone()
	for param_name, concrete in bindings {
		combined_bindings[param_name] = concrete
	}
	candidate := g.specialized_fn_name(decl, combined_bindings)
	g.ensure_specialized_call_signature(candidate)
	if candidate in g.fn_param_is_ptr || candidate in g.fn_return_types {
		return candidate
	}
	if candidate.contains('__') {
		if known_candidate := g.find_known_qualified_fn_name(candidate.all_after_last('__')) {
			return known_candidate
		}
	}
	return candidate
}

fn (mut g Gen) scan_call_cursor_for_generic_fn_specs(call ast.Cursor) {
	lhs := call.edge(0)
	decl := g.generic_call_decl_from_lhs_cursor(lhs) or { return }
	generic_params := g.generic_fn_param_names(decl)
	if generic_params.len == 0 {
		return
	}
	mut bindings := map[string]types.Type{}
	mut metadata_params := map[string]bool{}
	type_args := generic_call_type_args_cursor(lhs)
	mut has_unresolved_explicit_type_arg := false
	for i, param_name in generic_params {
		if i >= type_args.len {
			break
		}
		concrete := g.generic_type_arg_concrete_type(type_args[i]) or {
			has_unresolved_explicit_type_arg = true
			continue
		}
		bindings[param_name] = concrete
	}
	if has_unresolved_explicit_type_arg {
		return
	}
	if type_args.len == 0 {
		if active_bindings := g.active_generic_bindings_matching_embedded_suffix_cursor(lhs,
			generic_params)
		{
			for param_name, concrete in active_bindings {
				bindings[param_name] = concrete
			}
		}
	}
	if type_args.len == 0 && g.active_generic_types.len == 0
		&& !g.bind_embedded_generic_type_args_cursor(lhs, generic_params, mut bindings) {
		return
	}
	arg_count := if call.edge_count() > 1 { call.edge_count() - 1 } else { 0 }
	arg_offset := if decl.is_method && arg_count == decl.typ.params.len + 1 { 1 } else { 0 }
	for i, param in decl.typ.params {
		arg_idx := i + arg_offset
		if arg_idx >= arg_count || !expr_has_generic_placeholder(param.typ) {
			continue
		}
		arg := call.edge(1 + arg_idx)
		if is_comptime_field_metadata_expr_cursor(arg, g.comptime_field_var) {
			mut seen := map[string]bool{}
			mut names := []string{}
			collect_generic_placeholder_names_from_expr(param.typ, mut seen, mut names)
			for name in names {
				metadata_params[name] = true
			}
			continue
		}
		concrete := g.concrete_type_from_generic_call_arg_cursor(arg) or { continue }
		g.infer_generic_type_bindings_from_param(param.typ, g.concrete_type_for_generic_param(param,
			concrete), generic_params, mut bindings)
	}
	for param_name in generic_params {
		if param_name in bindings {
			continue
		}
		if param_name in metadata_params {
			continue
		}
		if concrete := g.active_generic_types[param_name] {
			bindings[param_name] = concrete
		}
	}
	if type_args.len == 0 && bindings.len != generic_params.len
		&& !g.bind_embedded_generic_type_args_cursor(lhs, generic_params, mut bindings) {
		raw_name := generic_call_raw_name_cursor(lhs)
		if !g.generic_call_name_has_placeholder_suffix(raw_name) {
			if specialized_name := g.try_specialize_generic_call_name_cursor(raw_name, call) {
				g.record_called_specialized_generic_name(specialized_name)
			}
			return
		}
		return_bindings := g.infer_generic_bindings_from_current_return(decl, generic_params) or {
			return
		}
		for param_name, concrete in return_bindings {
			if param_name !in bindings {
				bindings[param_name] = concrete
			}
		}
	}
	if bindings.len != generic_params.len {
		return
	}
	for _, concrete in bindings {
		if !g.generic_concrete_type_is_runtime_specializable(concrete) {
			return
		}
	}
	short_name := generic_call_short_name_cursor(lhs)
	if short_name == '' {
		return
	}
	g.record_late_generic_call_spec(short_name, bindings)
	g.record_generic_scan_call_name_cursor(lhs, arg_count, generic_params, bindings)
}

fn (mut g Gen) scan_type_cursor_for_generic_types(expr ast.Cursor) {
	if !expr.is_valid() || !g.type_cursor_may_need_generic_scan(expr) {
		return
	}
	g.scan_expr_for_generic_types(expr.type_expr())
}

fn (mut g Gen) scan_if_expr_cursor_for_generic_types(expr ast.Cursor) {
	g.scan_expr_cursor_for_generic_types(expr.edge(0))
	narrow_name, narrow_type := g.generic_scan_narrowing_from_cond_cursor(expr.edge(0))
	mut prev_runtime_local_types := g.runtime_local_types.clone()
	if narrow_name != '' && narrow_type != '' {
		g.remember_runtime_local_type(narrow_name, narrow_type)
	}
	for i in 2 .. expr.edge_count() {
		g.scan_cursor_stmt_for_generic_types(expr.edge(i))
	}
	g.runtime_local_types = prev_runtime_local_types.move()
	else_expr := expr.edge(1)
	if else_expr.is_valid() && else_expr.kind() != .expr_empty {
		g.scan_expr_cursor_for_generic_types(else_expr)
	}
}

fn (mut g Gen) scan_match_expr_cursor_for_generic_types(expr ast.Cursor) {
	g.scan_expr_cursor_for_generic_types(expr.edge(0))
	for i in 1 .. expr.edge_count() {
		branch := expr.edge(i)
		conds := branch.list_at(0)
		for ci in 0 .. conds.len() {
			g.scan_expr_cursor_for_generic_types(conds.at(ci))
		}
		g.scan_cursor_stmts_for_generic_types(branch.list_at(1))
	}
}

fn (mut g Gen) scan_tuple_expr_cursor_for_generic_types(expr ast.Cursor) {
	mut elem_types := []string{cap: expr.edge_count()}
	for i in 0 .. expr.edge_count() {
		elem := expr.edge(i)
		g.scan_expr_cursor_for_generic_types(elem)
		mut elem_type := g.generic_scan_expr_cursor_c_type(elem)
		if elem_type == '' || elem_type == 'int' {
			if raw := g.get_expr_cursor_type_from_env(elem) {
				elem_type = raw
			}
		}
		if elem_type == '' {
			elem_type = 'int'
		}
		elem_types << elem_type
	}
	g.register_tuple_alias(elem_types)
}

fn (mut g Gen) scan_expr_cursor_for_generic_types(expr ast.Cursor) {
	if !expr.is_valid() || !g.cursor_expr_may_need_generic_scan(expr) {
		return
	}
	if expr.kind() == .expr_comptime && expr.edge(0).kind() == .expr_if {
		g.scan_comptime_if_cursor_for_generic_types(expr.edge(0))
		return
	}
	match expr.kind() {
		.expr_ident {
			name := expr.name()
			if name.contains('_T_') || name.ends_with('_T') {
				g.scan_generic_fn_value_for_specs(ast.Expr(ast.Ident{
					name: name
					pos:  expr.pos()
				}))
				g.record_generic_struct_bindings_from_specialized_name(name)
			}
		}
		.typ_anon_struct, .typ_array_fixed, .typ_array, .typ_channel, .typ_fn, .typ_generic,
		.typ_map, .typ_option, .typ_pointer, .typ_result, .typ_thread, .typ_tuple {
			g.scan_type_cursor_for_generic_types(expr)
		}
		.expr_generic_arg_or_index, .expr_generic_args {
			g.scan_generic_fn_value_cursor_for_specs(expr)
			g.scan_generic_struct_bindings_cursor(expr)
			for arg in generic_call_type_args_cursor(expr) {
				g.scan_expr_for_generic_types(arg)
			}
		}
		.expr_call {
			g.scan_expr_cursor_for_generic_types(expr.edge(0))
			for i in 1 .. expr.edge_count() {
				g.scan_expr_cursor_for_generic_types(expr.edge(i))
			}
			g.scan_call_cursor_for_generic_fn_specs(expr)
		}
		.expr_call_or_cast {
			g.scan_expr_cursor_for_generic_types(expr.edge(0))
			g.scan_expr_cursor_for_generic_types(expr.edge(1))
		}
		.expr_infix {
			g.scan_expr_cursor_for_generic_types(expr.edge(0))
			g.scan_expr_cursor_for_generic_types(expr.edge(1))
			op := unsafe { token.Token(int(expr.aux())) }
			if op == .left_shift {
				g.remember_array_append_lhs_type_for_generic_scan_cursor(expr.edge(0), expr.edge(1))
			}
		}
		.expr_postfix, .expr_prefix, .expr_paren, .expr_modifier, .expr_lambda, .expr_sql,
		.expr_selector, .expr_comptime {
			g.scan_expr_cursor_for_generic_types(expr.edge(0))
		}
		.expr_assoc {
			g.scan_type_cursor_for_generic_types(expr.edge(0))
			g.scan_expr_cursor_for_generic_types(expr.edge(1))
			for i in 2 .. expr.edge_count() {
				g.scan_expr_cursor_for_generic_types(expr.edge(i).edge(0))
			}
		}
		.expr_if_guard {
			g.scan_cursor_stmt_for_generic_types(expr.edge(0))
		}
		.expr_or {
			g.scan_expr_cursor_for_generic_types(expr.edge(0))
			for i in 1 .. expr.edge_count() {
				g.scan_cursor_stmt_for_generic_types(expr.edge(i))
			}
		}
		.expr_unsafe {
			for i in 0 .. expr.edge_count() {
				g.scan_cursor_stmt_for_generic_types(expr.edge(i))
			}
		}
		.expr_lock {
			packed := u32(expr.extra_int())
			lock_len := int(packed & 0xFFFF)
			rlock_len := int((packed >> 16) & 0xFFFF)
			for i in 0 .. (lock_len + rlock_len) {
				g.scan_expr_cursor_for_generic_types(expr.edge(i))
			}
			for i in (lock_len + rlock_len) .. expr.edge_count() {
				g.scan_cursor_stmt_for_generic_types(expr.edge(i))
			}
		}
		.expr_if {
			g.scan_if_expr_cursor_for_generic_types(expr)
		}
		.expr_match {
			g.scan_match_expr_cursor_for_generic_types(expr)
		}
		.expr_select {
			g.scan_cursor_stmt_for_generic_types(expr.edge(0))
			g.scan_expr_cursor_for_generic_types(expr.edge(1))
			for i in 2 .. expr.edge_count() {
				g.scan_cursor_stmt_for_generic_types(expr.edge(i))
			}
		}
		.expr_string_inter {
			inters := expr.list_at(1)
			for i in 0 .. inters.len() {
				inter := inters.at(i)
				g.scan_expr_cursor_for_generic_types(inter.edge(0))
				g.scan_expr_cursor_for_generic_types(inter.edge(1))
			}
		}
		.expr_init {
			g.scan_type_cursor_for_generic_types(expr.edge(0))
			for i in 1 .. expr.edge_count() {
				g.scan_expr_cursor_for_generic_types(expr.edge(i).edge(0))
			}
		}
		.expr_array_init {
			g.scan_type_cursor_for_generic_types(expr.edge(0))
			for i in 1 .. expr.edge_count() {
				g.scan_expr_cursor_for_generic_types(expr.edge(i))
			}
		}
		.expr_map_init {
			g.scan_type_cursor_for_generic_types(expr.edge(0))
			for i in 1 .. expr.edge_count() {
				g.scan_expr_cursor_for_generic_types(expr.edge(i))
			}
		}
		.expr_index, .expr_cast, .expr_as_cast, .expr_range, .expr_keyword_operator {
			for i in 0 .. expr.edge_count() {
				g.scan_expr_cursor_for_generic_types(expr.edge(i))
			}
		}
		.expr_fn_literal {
			captured_len := expr.extra_int()
			for i in 0 .. captured_len {
				g.scan_expr_cursor_for_generic_types(expr.edge(1 + i))
			}
			for i in (1 + captured_len) .. expr.edge_count() {
				g.scan_cursor_stmt_for_generic_types(expr.edge(i))
			}
		}
		.expr_tuple {
			g.scan_tuple_expr_cursor_for_generic_types(expr)
		}
		else {}
	}
}

fn (mut g Gen) scan_comptime_for_cursor_for_generic_types(node ast.Cursor) bool {
	if node.kind() != .stmt_for {
		return false
	}
	for_in := node.edge(0)
	if for_in.kind() != .stmt_for_in {
		return false
	}
	iter := for_in.edge(2)
	if iter.kind() != .expr_selector || iter.edge(1).name() != 'fields' {
		return false
	}
	type_name := iter.edge(0).name()
	concrete := g.active_generic_types[type_name] or { return false }
	if concrete !is types.Struct {
		return false
	}
	struct_type := g.comptime_for_struct_type(concrete, concrete as types.Struct)
	prev_field_var := g.comptime_field_var
	prev_field_name := g.comptime_field_name
	prev_field_type := g.comptime_field_type
	prev_field_raw_type := g.comptime_field_raw_type
	prev_field_attrs := g.comptime_field_attrs
	prev_field_idx := g.comptime_field_idx
	defer {
		g.comptime_field_var = prev_field_var
		g.comptime_field_name = prev_field_name
		g.comptime_field_type = prev_field_type
		g.comptime_field_raw_type = prev_field_raw_type
		g.comptime_field_attrs = prev_field_attrs
		g.comptime_field_idx = prev_field_idx
	}
	g.comptime_field_var = for_in.edge(1).name()
	for i, field in struct_type.fields {
		g.comptime_field_name = field.name
		g.comptime_field_type = g.types_type_to_c(field.typ)
		g.comptime_field_raw_type = field.typ
		g.comptime_field_attrs = g.comptime_field_attribute_strings(struct_type.name, field)
		g.comptime_field_idx = i
		g.scan_cursor_stmts_for_generic_types(node.for_body_list())
	}
	return true
}

// scan_expr_stmts_for_generic_types recurses into IfExpr/MatchExpr/ComptimeExpr bodies.
fn (mut g Gen) scan_expr_stmts_for_generic_types(e ast.Expr) {
	if e is ast.IfExpr {
		g.scan_expr_for_generic_types(e.cond)
		narrow_name, narrow_type := g.generic_scan_narrowing_from_cond(e.cond)
		mut prev_runtime_local_types := g.runtime_local_types.clone()
		if narrow_name != '' && narrow_type != '' {
			g.remember_runtime_local_type(narrow_name, narrow_type)
		}
		g.scan_stmts_for_generic_types(e.stmts)
		g.runtime_local_types = prev_runtime_local_types.move()
		if e.else_expr !is ast.EmptyExpr {
			g.scan_expr_for_generic_types(e.else_expr)
			g.scan_expr_stmts_for_generic_types(e.else_expr)
		}
	} else if e is ast.MatchExpr {
		g.scan_expr_for_generic_types(e.expr)
		for branch in e.branches {
			for cond in branch.cond {
				g.scan_expr_for_generic_types(cond)
			}
			g.scan_stmts_for_generic_types(branch.stmts)
		}
	} else if e is ast.ComptimeExpr {
		if e.expr is ast.IfExpr {
			g.scan_comptime_if_for_generic_types(e.expr)
			return
		}
		// $if / $for wrapped in ComptimeExpr
		g.scan_expr_for_generic_types(e.expr)
		g.scan_expr_stmts_for_generic_types(e.expr)
	} else if e is ast.OrExpr {
		g.scan_expr_for_generic_types(e.expr)
		g.scan_stmts_for_generic_types(e.stmts)
	} else if e is ast.UnsafeExpr {
		g.scan_stmts_for_generic_types(e.stmts)
	}
}

fn (mut g Gen) scan_comptime_if_cursor_for_generic_types(node ast.Cursor) {
	if node.kind() != .expr_if {
		return
	}
	if g.eval_comptime_cond_cursor(node.edge(0)) {
		g.scan_if_expr_cursor_body_for_generic_types(node)
		return
	}
	else_expr := node.edge(1)
	if else_expr.kind() == .expr_if {
		if else_expr.edge(0).kind() == .expr_empty {
			g.scan_if_expr_cursor_body_for_generic_types(else_expr)
		} else {
			g.scan_comptime_if_cursor_for_generic_types(else_expr)
		}
		return
	}
	if else_expr.is_valid() && else_expr.kind() != .expr_empty {
		g.scan_expr_cursor_for_generic_types(else_expr)
	}
}

fn (mut g Gen) scan_if_expr_cursor_body_for_generic_types(node ast.Cursor) {
	for i in 2 .. node.edge_count() {
		g.scan_cursor_stmt_for_generic_types(node.edge(i))
	}
}

fn (mut g Gen) generic_scan_narrowing_from_cond(cond ast.Expr) (string, string) {
	if cond is ast.InfixExpr && cond.op == .key_is {
		name := cond.lhs.name()
		if name == '' {
			return '', ''
		}
		c_type := g.expr_type_to_c(cond.rhs).trim_space()
		if c_type == '' || c_type == 'int' {
			return '', ''
		}
		return name, c_type
	}
	return '', ''
}

fn (mut g Gen) generic_scan_narrowing_from_cond_cursor(cond ast.Cursor) (string, string) {
	if !cond.is_valid() || cond.kind() != .expr_infix {
		return '', ''
	}
	op := unsafe { token.Token(int(cond.aux())) }
	if op != .key_is {
		return '', ''
	}
	name := generic_wrapped_ident_name_cursor(cond.edge(0))
	if name == '' {
		return '', ''
	}
	c_type := g.expr_type_to_c(cond.edge(1).type_expr()).trim_space()
	if c_type == '' || c_type == 'int' {
		return '', ''
	}
	return name, c_type
}

fn generic_scan_type_is_unresolved(typ string) bool {
	return typ == '' || typ == 'int' || typ == 'void' || typ == 'array' || typ == 'map'
}

fn (mut g Gen) generic_scan_expr_c_type(expr ast.Expr) string {
	base_expr := if expr is ast.ModifierExpr { expr.expr } else { expr }
	if base_expr is ast.StringLiteral || base_expr is ast.StringInterLiteral {
		return 'string'
	}
	if base_expr is ast.Ident {
		if local_type := g.get_local_var_c_type(base_expr.name) {
			return local_type.trim_space()
		}
	}
	if base_expr is ast.ArrayInitExpr {
		array_type := g.expr_type_to_c(base_expr.typ).trim_space()
		if !generic_scan_type_is_unresolved(array_type) {
			return array_type
		}
	}
	mut typ := g.get_expr_type(base_expr).trim_space()
	if generic_scan_type_is_unresolved(typ) {
		if raw := g.get_raw_type(base_expr) {
			typ = g.types_type_to_c(raw).trim_space()
		}
	}
	return typ
}

fn (mut g Gen) generic_scan_expr_cursor_c_type(expr ast.Cursor) string {
	mut base_expr := expr
	for base_expr.is_valid() && base_expr.kind() in [.expr_modifier, .expr_paren] {
		base_expr = base_expr.edge(0)
	}
	if !base_expr.is_valid() {
		return ''
	}
	match base_expr.kind() {
		.expr_string, .expr_string_inter {
			return 'string'
		}
		.expr_ident {
			if local_type := g.get_local_var_c_type(base_expr.name()) {
				return local_type.trim_space()
			}
		}
		.expr_array_init {
			array_type := g.expr_type_to_c(base_expr.edge(0).type_expr()).trim_space()
			if !generic_scan_type_is_unresolved(array_type) {
				return array_type
			}
		}
		.expr_cast {
			cast_type := g.expr_type_to_c(base_expr.edge(0).type_expr()).trim_space()
			if !generic_scan_type_is_unresolved(cast_type) {
				return cast_type
			}
		}
		.expr_as_cast {
			cast_type := g.expr_type_to_c(base_expr.edge(1).type_expr()).trim_space()
			if !generic_scan_type_is_unresolved(cast_type) {
				return cast_type
			}
		}
		else {}
	}

	if typ := g.get_expr_cursor_type_from_env(base_expr) {
		return typ.trim_space()
	}
	return ''
}

fn (mut g Gen) remember_assign_cursor_runtime_local_type(lhs ast.Cursor, rhs ast.Cursor) {
	if !rhs.is_valid() {
		return
	}
	lhs_name := generic_wrapped_ident_name_cursor(lhs)
	if lhs_name == '' {
		return
	}
	mut rhs_type := g.generic_scan_expr_cursor_c_type(rhs).trim_space()
	if generic_scan_type_is_unresolved(rhs_type) {
		rhs_type = (g.get_local_var_c_type(lhs_name) or { '' }).trim_space()
	}
	if generic_scan_type_is_unresolved(rhs_type) && rhs.kind() == .expr_array_init {
		rhs_type = g.expr_type_to_c(rhs.edge(0).type_expr()).trim_space()
	}
	if !generic_scan_type_is_unresolved(rhs_type) {
		g.remember_runtime_local_type(lhs_name, rhs_type)
	} else if rhs.kind() == .expr_array_init {
		g.remember_runtime_current_local_type(lhs_name, 'array')
	}
}

fn (mut g Gen) remember_array_append_lhs_type_for_generic_scan(lhs ast.Expr, rhs ast.Expr) {
	lhs_name := generic_wrapped_ident_name(lhs)
	if lhs_name == '' {
		return
	}
	lhs_type := g.generic_scan_expr_c_type(lhs).trim_space().trim_right('*')
	if lhs_type != 'array' && !lhs_type.starts_with('Array_') {
		return
	}
	elem_type := g.generic_scan_expr_c_type(rhs).trim_space().trim_right('*')
	if generic_scan_type_is_unresolved(elem_type) {
		return
	}
	g.remember_runtime_local_type(lhs_name, 'Array_' + mangle_alias_component(elem_type))
}

fn (mut g Gen) remember_array_append_lhs_type_for_generic_scan_cursor(lhs ast.Cursor, rhs ast.Cursor) {
	lhs_name := generic_wrapped_ident_name_cursor(lhs)
	if lhs_name == '' {
		return
	}
	lhs_type := g.generic_scan_expr_cursor_c_type(lhs).trim_space().trim_right('*')
	if lhs_type != 'array' && !lhs_type.starts_with('Array_') {
		return
	}
	elem_type := g.generic_scan_expr_cursor_c_type(rhs).trim_space().trim_right('*')
	if generic_scan_type_is_unresolved(elem_type) {
		return
	}
	g.remember_runtime_local_type(lhs_name, 'Array_' + mangle_alias_component(elem_type))
}

fn (mut g Gen) scan_comptime_if_for_generic_types(node ast.IfExpr) {
	if g.eval_comptime_cond(node.cond) {
		g.scan_stmts_for_generic_types(node.stmts)
		return
	}
	if node.else_expr is ast.IfExpr {
		else_if := node.else_expr as ast.IfExpr
		if else_if.cond is ast.EmptyExpr {
			g.scan_stmts_for_generic_types(else_if.stmts)
		} else {
			g.scan_comptime_if_for_generic_types(else_if)
		}
		return
	}
	if node.else_expr !is ast.EmptyExpr {
		g.scan_expr_for_generic_types(node.else_expr)
		g.scan_expr_stmts_for_generic_types(node.else_expr)
	}
}

fn (mut g Gen) struct_is_leaf(node ast.StructDecl) bool {
	if node.embedded.len > 0 {
		return false
	}
	env_struct := g.lookup_struct_type(node.name)
	if env_struct.fields.len == node.fields.len {
		for field in env_struct.fields {
			if g.struct_leaf_field_type(field.typ) {
				continue
			}
			return false
		}
		return true
	}
	for field in node.fields {
		if g.is_pointer_type(field.typ) {
			continue
		}
		match field.typ {
			ast.Ident {
				if field.typ.name in primitive_types {
					continue
				}
				return false
			}
			ast.Type {
				if field.typ is ast.ArrayFixedType {
					if field.typ.elem_type is ast.Ident
						&& (field.typ.elem_type as ast.Ident).name in primitive_types {
						continue
					}
				}
				return false
			}
			else {
				return false
			}
		}
	}
	return true
}

fn (mut g Gen) struct_leaf_field_type(t types.Type) bool {
	if !type_has_valid_data(t) {
		return false
	}
	if g.struct_leaf_pointer_type(t) {
		return true
	}
	match t {
		types.Primitive, types.Char, types.Rune, types.ISize, types.USize, types.Enum {
			return true
		}
		types.ArrayFixed {
			return g.struct_leaf_field_type(t.elem_type)
		}
		types.Alias {
			type_name := g.types_type_to_c(t)
			if type_name in primitive_types || type_name == 'bool' {
				return true
			}
			return g.struct_leaf_field_type(t.base_type)
		}
		else {
			return false
		}
	}
}

fn (g &Gen) struct_leaf_pointer_type(t types.Type) bool {
	if !type_has_valid_data(t) {
		return false
	}
	match t {
		types.Pointer {
			return true
		}
		types.Alias {
			return g.struct_leaf_pointer_type(t.base_type)
		}
		else {
			return false
		}
	}
}

fn (mut g Gen) emit_ready_option_result_structs() bool {
	mut emitted_any := false
	for _, ret_type in g.fn_return_types {
		if ret_type.starts_with('_option_') || ret_type.starts_with('_result_') {
			g.register_alias_type(ret_type)
		}
	}
	mut option_names := g.option_aliases.keys()
	option_names.sort()
	for name in option_names {
		if g.should_skip_shadowed_option_result_alias(name) {
			continue
		}
		if name in g.emitted_option_structs {
			continue
		}
		val_type := option_value_type(name)
		if !g.option_result_payload_ready(val_type) {
			continue
		}
		payload_type := g.option_result_payload_c_type(val_type)
		g.emit_option_result_forward_decl(name, val_type)
		g.sb.writeln('struct ${name} { u8 state; IError err; u8 data[sizeof(${payload_type}) > 1 ? sizeof(${payload_type}) : 1]; };')
		g.emitted_option_structs[name] = true
		emitted_any = true
	}
	mut result_names := g.result_aliases.keys()
	result_names.sort()
	for name in result_names {
		if g.should_skip_shadowed_option_result_alias(name) {
			continue
		}
		if name in g.emitted_result_structs {
			continue
		}
		val_type := g.result_value_type(name)
		if !g.option_result_payload_ready(val_type) {
			continue
		}
		payload_type := g.option_result_payload_c_type(val_type)
		g.emit_option_result_forward_decl(name, val_type)
		g.sb.writeln('struct ${name} { bool is_error; IError err; u8 data[sizeof(${payload_type}) > 1 ? sizeof(${payload_type}) : 1]; };')
		g.emitted_result_structs[name] = true
		emitted_any = true
	}
	return emitted_any
}

fn (mut g Gen) emit_option_result_structs() {
	for g.emit_ready_option_result_structs() {}
}

fn (mut g Gen) struct_fields_resolved(node ast.StructDecl) bool {
	real_generic_params := generic_param_names(node.generic_params)
	owner_name := g.get_struct_name(node)
	prev_active_generic_types := g.active_generic_types.clone()
	mut set_generic_context := false
	if real_generic_params.len > 0 && g.active_generic_types.len == 0 {
		bindings := g.generic_bindings_for_struct_decl(node) or { return false }
		g.active_generic_types = bindings.clone()
		set_generic_context = true
	}
	defer {
		if set_generic_context {
			g.active_generic_types = prev_active_generic_types.clone()
		}
	}

	// Check embedded types (used by value, not pointer)
	for emb in node.embedded {
		emb_name := g.field_type_name(emb)
		if emb_name != '' && emb_name !in primitive_types {
			emb_body_key := 'body_${emb_name}'
			emb_enum_key := 'enum_${emb_name}'
			emb_alias_key := 'alias_${emb_name}'
			if emb_body_key !in g.emitted_types && emb_enum_key !in g.emitted_types
				&& emb_alias_key !in g.emitted_types {
				return false
			}
		}
	}
	for field in node.fields {
		mut typ_name := if g.active_generic_types.len > 0 {
			g.expr_type_to_c(field.typ)
		} else {
			g.field_type_name(field.typ)
		}
		typ_name = g.qualify_owner_local_field_type(owner_name, typ_name)
		if typ_name == '' {
			continue
		}
		if raw_type := g.get_raw_type(field.typ) {
			match raw_type {
				types.Interface {
					if typ_name !in g.emitted_interface_bodies {
						return false
					}
					continue
				}
				types.Alias {
					if raw_type.base_type is types.Interface
						&& typ_name !in g.emitted_interface_bodies {
						return false
					}
				}
				else {}
			}
		}
		field_typ := field.typ
		if field_typ is ast.Type {
			if field_typ is ast.ArrayFixedType {
				fixed_typ := field_typ as ast.ArrayFixedType
				if !g.struct_field_fixed_array_elem_resolved(fixed_typ.elem_type) {
					return false
				}
				continue
			}
			if field_typ is ast.OptionType {
				opt_typ := field_typ as ast.OptionType
				mut base_name := if g.active_generic_types.len > 0 {
					g.expr_type_to_c(opt_typ.base_type)
				} else {
					g.field_type_name(opt_typ.base_type)
				}
				base_name = g.qualify_owner_local_field_type(owner_name, base_name)
				if base_name != '' && base_name != 'void'
					&& !g.option_result_payload_invalid(base_name) {
					wrapper_name := '_option_' + g.option_result_payload_alias_component(base_name)
					if wrapper_name !in g.emitted_option_structs {
						return false
					}
				}
			} else if field_typ is ast.ResultType {
				res_typ := field_typ as ast.ResultType
				mut base_name := if g.active_generic_types.len > 0 {
					g.expr_type_to_c(res_typ.base_type)
				} else {
					g.field_type_name(res_typ.base_type)
				}
				base_name = g.qualify_owner_local_field_type(owner_name, base_name)
				if base_name != '' && base_name != 'void'
					&& !g.option_result_payload_invalid(base_name) {
					wrapper_name := '_result_' + g.option_result_payload_alias_component(base_name)
					if wrapper_name !in g.emitted_result_structs {
						return false
					}
				}
			}
		}
		if typ_name.starts_with('_option_') {
			if typ_name !in g.emitted_option_structs {
				return false
			}
			continue
		}
		if typ_name.starts_with('_result_') {
			if typ_name !in g.emitted_result_structs {
				return false
			}
			continue
		}
		// Pointer types are fine with forward declarations
		if g.is_pointer_type(field.typ) {
			continue
		}
		// Primitive types are always resolved
		if typ_name in primitive_types {
			continue
		}
		if g.is_c_type_name(typ_name) {
			continue
		}
		if typ_name == 'string' || typ_name == 'builtin__string' {
			string_body_key := 'body_string'
			builtin_string_body_key := 'body_builtin__string'
			if string_body_key !in g.emitted_types && builtin_string_body_key !in g.emitted_types {
				return false
			}
			continue
		}
		if typ_name.starts_with('Array_fixed_') {
			typ_body_key := 'body_${typ_name}'
			typ_alias_key := 'alias_${typ_name}'
			if typ_body_key !in g.emitted_types && typ_alias_key !in g.emitted_types {
				return false
			}
			continue
		}
		if typ_name == 'array' || typ_name.starts_with('Array_') || typ_name in g.array_aliases {
			if 'body_array' !in g.emitted_types {
				return false
			}
			continue
		}
		if typ_name == 'map' || typ_name.starts_with('Map_') || typ_name in g.map_aliases {
			if 'body_map' !in g.emitted_types {
				return false
			}
			continue
		}
		// Check if this type's body has been emitted
		typ_body_key := 'body_${typ_name}'
		typ_enum_key := 'enum_${typ_name}'
		typ_alias_key := 'alias_${typ_name}'
		if typ_body_key !in g.emitted_types && typ_enum_key !in g.emitted_types
			&& typ_alias_key !in g.emitted_types {
			return false
		}
	}
	return true
}

fn (mut g Gen) struct_field_fixed_array_elem_resolved(elem_type ast.Expr) bool {
	elem_name := if g.active_generic_types.len > 0 {
		g.expr_type_to_c(elem_type)
	} else {
		g.field_type_name(elem_type)
	}
	if elem_name == '' || elem_name in primitive_types || elem_name == 'bool' || elem_name == 'void'
		|| elem_name == 'void*' || elem_name == 'voidptr' {
		return true
	}
	if g.is_pointer_type(elem_type) || g.is_c_type_name(elem_name) {
		return true
	}
	if elem_name == 'string' || elem_name == 'builtin__string' {
		return 'body_string' in g.emitted_types || 'body_builtin__string' in g.emitted_types
	}
	if elem_name.starts_with('Array_') || elem_name == 'array' || elem_name in g.array_aliases {
		return 'body_array' in g.emitted_types
	}
	if elem_name.starts_with('Map_') || elem_name == 'map' || elem_name in g.map_aliases {
		return 'body_map' in g.emitted_types
	}
	return 'body_${elem_name}' in g.emitted_types || 'enum_${elem_name}' in g.emitted_types
		|| 'alias_${elem_name}' in g.emitted_types
}

fn (mut g Gen) generic_bindings_for_struct_decl(node ast.StructDecl) ?map[string]types.Type {
	struct_c_name := g.get_struct_name(node)
	if bindings := g.generic_struct_bindings[struct_c_name] {
		return bindings.clone()
	}
	if instances := g.generic_struct_instances[struct_c_name] {
		if instances.len > 0 {
			return instances[0].bindings.clone()
		}
	}
	return none
}

fn (mut g Gen) get_struct_name(node ast.StructDecl) string {
	if node.name.contains('__') {
		return node.name
	}
	if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
		return '${g.cur_module}__${node.name}'
	}
	return node.name
}

fn (g &Gen) qualify_owner_local_field_type(owner_type_name string, field_type string) string {
	if owner_type_name == '' || !owner_type_name.contains('__') || field_type == '' {
		return field_type
	}
	owner_mod := owner_type_name.all_before('__')
	if owner_mod == '' || owner_mod == 'main' || owner_mod == 'builtin' {
		return field_type
	}
	mut base := field_type.trim_space()
	mut suffix := ''
	for base.ends_with('*') {
		base = base[..base.len - 1]
		suffix += '*'
	}
	if base == '' || base.contains('.') || base in primitive_types
		|| base in ['bool', 'string', 'void*', 'voidptr', 'char*', 'charptr', 'u8*', 'byteptr'] {
		return field_type
	}
	if base.starts_with('Array_') || base.starts_with('Array_fixed_') || base.starts_with('Map_')
		|| base.starts_with('_option_') || base.starts_with('_result_') {
		return field_type
	}
	mut lookup_name := base
	if base.contains('_T_') {
		lookup_name = base.all_before('_T_')
	}
	if lookup_name.contains('__') {
		return field_type
	}
	if scope := g.env_scope(owner_mod) {
		if _ := scope.lookup_type(lookup_name) {
			return '${owner_mod}__${base}${suffix}'
		}
		if obj := scope.lookup(lookup_name) {
			if obj is types.Type {
				return '${owner_mod}__${base}${suffix}'
			}
		}
	}
	return field_type
}

fn (mut g Gen) gen_struct_decl(node ast.StructDecl) {
	// Skip C extern struct declarations
	if node.language == .c {
		return
	}
	// Generic structs are emitted only from concrete bindings recorded from
	// GenericType instantiations.
	prev_generic_types := g.active_generic_types.clone()
	real_generic_params := generic_param_names(node.generic_params)
	if real_generic_params.len > 0 {
		g.active_generic_types = g.generic_bindings_for_struct_decl(node) or { return }
	}
	defer {
		g.active_generic_types = prev_generic_types.clone()
	}

	name := g.get_struct_name(node)
	body_key := 'body_${name}'
	if body_key in g.emitted_types || body_key in g.pending_late_body_keys {
		return
	}
	if (name.contains('_T_') || name.contains('__T_'))
		&& g.struct_has_unresolved_generic_value_dependency(node, name) {
		g.emit_late_concrete_struct_decl(node, name)
		return
	}
	// For generic structs with active bindings, verify that all field types
	// are already emitted. This prevents the last-resort pass from emitting
	// a generic struct body before its concrete field types are defined.
	if real_generic_params.len > 0 && g.active_generic_types.len > 0 {
		if !g.struct_fields_resolved(node)
			|| g.struct_has_unresolved_generic_value_dependency(node, name) {
			return
		}
	}
	g.emitted_types[body_key] = true
	keyword := if node.is_union { 'union' } else { 'struct' }
	// Try to get the resolved struct type from the Environment
	env_struct := g.lookup_struct_type(node.name)
	for field in node.fields {
		field_type := g.struct_field_type_for_emit(name, field)
		if field_type.starts_with('Array_') {
			g.emit_array_alias_decl(field_type)
		}
		if field_type.starts_with('Map_') {
			g.emit_map_alias_decl(field_type)
		}
		field_base_type := field_type.trim_right('*')
		if field_base_type.contains('_T_') && !field_base_type.starts_with('Array_')
			&& !field_base_type.starts_with('Map_') && field_base_type !in g.emitted_types {
			g.emitted_types[field_base_type] = true
			g.sb.writeln('typedef struct ${field_base_type} ${field_base_type};')
		}
	}

	// Use named struct to match the forward declaration: typedef struct name name;
	is_packed := node.attributes.has('_pack')
	if is_packed {
		g.sb.writeln('#pragma pack(push, 1)')
	}
	g.sb.writeln('${keyword} ${name} {')
	// Embedded structs as fields
	for i, emb in node.embedded {
		emb_type := g.expr_type_to_c(emb)
		emb_field_name := if emb_type.contains('__') {
			emb_type.all_after_last('__')
		} else {
			emb_type
		}
		g.sb.writeln('\t${emb_type} ${emb_field_name};')
		if i < env_struct.embedded.len {
			mut embedded := env_struct.embedded[i]
			// For unions, the checker stores placeholder Struct objects with empty
			// fields. Look up the actual struct type from the environment,
			// following alias chains across modules.
			if embedded.fields.len == 0 && node.is_union {
				if embedded.name != '' {
					resolved := g.lookup_union_variant_struct(embedded.name)
					if resolved.fields.len > 0 {
						embedded = resolved
					}
				}
				if embedded.fields.len == 0 {
					// Try with the C type name from the AST
					resolved2 := g.lookup_union_variant_struct(emb_type)
					if resolved2.fields.len > 0 {
						embedded = resolved2
					}
				}
			}
			// Collect direct field names to avoid overriding with embedded fields
			mut direct_field_names := map[string]bool{}
			for field in node.fields {
				direct_field_names[field.name] = true
			}
			for ef in embedded.fields {
				if ef.name in direct_field_names {
					continue
				}
				key := name + '.' + ef.name
				g.embedded_field_owner[key] = emb_field_name
				embedded_field_type := g.types_type_to_c(ef.typ)
				g.struct_field_types[key] = embedded_field_type
				if name.contains('__') {
					short_key := name.all_after_last('__') + '.' + ef.name
					g.embedded_field_owner[short_key] = emb_field_name
					g.struct_field_types[short_key] = embedded_field_type
				}
			}
			// Recursively register fields from nested embedded structs.
			// E.g., if A embeds B and B embeds C with field f, register
			// A.f → B_owner.C_owner.f so that a.f generates a.B.C.f in C.
			emb_struct_info := g.lookup_struct_type_by_c_name(emb_type)
			for sub_emb in emb_struct_info.embedded {
				sub_emb_c_name := g.types_type_to_c(types.Type(sub_emb))
				sub_emb_field := if sub_emb_c_name.contains('__') {
					sub_emb_c_name.all_after_last('__')
				} else {
					sub_emb_c_name
				}
				// The embedded copy may have stale (empty) fields if the sub-struct
				// was processed after the parent by the checker. Re-lookup live type.
				mut live_sub := sub_emb
				if sub_emb.fields.len == 0 && sub_emb_c_name != '' {
					live_sub = g.lookup_struct_type_by_c_name(sub_emb_c_name)
				}
				for sf in live_sub.fields {
					if sf.name in direct_field_names {
						continue
					}
					sub_key := name + '.' + sf.name
					if sub_key !in g.embedded_field_owner {
						g.embedded_field_owner[sub_key] = '${emb_field_name}.${sub_emb_field}'
						sub_field_type := g.types_type_to_c(sf.typ)
						g.struct_field_types[sub_key] = sub_field_type
						if name.contains('__') {
							short_sub_key := name.all_after_last('__') + '.' + sf.name
							if short_sub_key !in g.embedded_field_owner {
								g.embedded_field_owner[short_sub_key] = '${emb_field_name}.${sub_emb_field}'
								g.struct_field_types[short_sub_key] = sub_field_type
							}
						}
					}
				}
			}
		}
	}
	// Regular fields
	mut has_shared_fields := false
	for field in node.fields {
		mut field_lookup_type := g.struct_field_type_for_emit(name, field)
		if fn_field_type := g.struct_fn_field_lookup_type_for_emit(field) {
			field_lookup_type = fn_field_type
		}
		field_v_name := if node.is_union && field_lookup_type.contains('__')
			&& (field.name == '' || field.name.contains('__')
			|| field.name == field_lookup_type
			|| field.name.all_after_last('__') == field_lookup_type.all_after_last('__')) {
			field_lookup_type.all_after_last('__')
		} else {
			field.name
		}
		field_name := escape_c_keyword(field_v_name)
		field_key := '${name}.${field_v_name}'
		g.struct_field_types[field_key] = field_lookup_type
		if name.contains('__') {
			short_field_key := '${name.all_after_last('__')}.${field_v_name}'
			g.struct_field_types[short_field_key] = field_lookup_type
		}
		if field.typ is ast.Type && field.typ is ast.ArrayFixedType {
			fixed_typ := field.typ as ast.ArrayFixedType
			elem_type := g.expr_type_to_c(fixed_typ.elem_type)
			// Use the resolved array size from the Environment if available
			mut resolved_len := -1
			for ef in env_struct.fields {
				if ef.name == field.name || ef.name == field_v_name {
					if ef.typ is types.ArrayFixed {
						resolved_len = ef.typ.len
					}
					break
				}
			}
			g.sb.write_string('\t${elem_type} ${field_name}[')
			if resolved_len > 0 {
				g.sb.write_string('${resolved_len}')
			} else if len_expr := g.const_expr_c_value_for_header(fixed_typ.len) {
				g.sb.write_string(len_expr)
			} else {
				g.expr(fixed_typ.len)
			}
			g.sb.writeln('];')
			continue
		}
		if field.typ is ast.Type {
			if field.typ is ast.FnType {
				if decl := g.struct_fn_field_decl_for_emit(field, field_name) {
					g.sb.writeln('\t${decl};')
				}
				continue
			}
		}
		// Check for shared modifier
		if field.typ is ast.ModifierExpr && field.typ.kind == .key_shared {
			has_shared_fields = true
		}
		field_type := field_lookup_type
		g.sb.writeln('\t${field_type} ${field_name};')
		// For union types, register the variant's sub-fields in embedded_field_owner
		// so that `box.x` resolves to `box.GgRect.x` via embedded_owner_for().
		if node.is_union {
			variant_struct := g.lookup_union_variant_struct(field_lookup_type)
			for vf in variant_struct.fields {
				vf_key := name + '.' + vf.name
				g.embedded_field_owner[vf_key] = field_name
				vf_type := g.types_type_to_c(vf.typ)
				g.struct_field_types[vf_key] = vf_type
				if name.contains('__') {
					vf_short_key := name.all_after_last('__') + '.' + vf.name
					g.embedded_field_owner[vf_short_key] = field_name
					g.struct_field_types[vf_short_key] = vf_type
				}
			}
		}
	}
	// Add mutex field for shared fields
	if has_shared_fields {
		g.sb.writeln('\tsync__RwMutex mtx;')
	}
	if node.embedded.len == 0 && node.fields.len == 0 {
		g.sb.writeln('\tu8 _dummy;')
	}
	g.sb.writeln('};')
	if is_packed {
		g.sb.writeln('#pragma pack(pop)')
	}
	// Emit fallback str macros for structs without explicit str() methods
	struct_str_fn := '${name}__str'
	has_explicit_str := g.has_explicit_str_method_for_c_type(name)
	if g.cache_bundle_name == 'virtuals' && g.should_emit_current_file() && !has_explicit_str {
		label := '${name}{}'
		g.late_struct_defs << 'string ${name}__str(${name} v) { return (string){.str = "${label}", .len = ${label.len}, .is_lit = 1}; }\n#define ${name}_str(v) ${name}__str(v)\n'
		g.fn_return_types[struct_str_fn] = 'string'
	} else if !has_explicit_str && struct_str_fn !in g.fn_return_types {
		label := '${name}{}'
		g.sb.writeln('#define ${name}__str(v) ((string){.str = "${label}", .len = ${label.len}, .is_lit = 1})')
		// Register the macro in fn_return_types so method resolution can find it
		g.fn_return_types[struct_str_fn] = 'string'
		struct_short_str_fn := '${name}_str'
		if struct_short_str_fn !in g.fn_return_types {
			g.sb.writeln('#define ${name}_str(v) ${name}__str(v)')
		}
	}
	g.sb.writeln('')
	// Generate SoA (Structure of Arrays) companion struct and helpers for @[soa] structs
	if env_struct.is_soa && env_struct.fields.len > 0 {
		g.gen_soa_companion(name, env_struct)
	}
	// Emit additional generic struct instantiations (e.g. Node[StructFieldInfo]
	// when Node[ValueInfo] was the primary binding).
	if node.generic_params.len > 0 {
		instances := g.generic_struct_instances[name]
		for inst in instances {
			if inst.c_name == name {
				continue // already emitted as primary
			}
			inst_body_key := 'body_${inst.c_name}'
			if inst_body_key in g.emitted_types || inst_body_key in g.pending_late_body_keys {
				continue
			}
			// Set active generic types to this instantiation's bindings
			prev_active := g.active_generic_types.clone()
			g.active_generic_types = inst.bindings.clone()
			if !g.struct_fields_resolved(node)
				|| g.struct_has_unresolved_generic_value_dependency(node, inst.c_name) {
				g.active_generic_types = prev_active.clone()
				g.emit_late_generic_struct(name, inst)
				continue
			}
			g.emitted_types[inst_body_key] = true
			// Emit typedef forward declarations for self-references and other
			// generic-instance field types (e.g. `next` field in linked-list
			// generic structs references the instance itself).
			// Note: don't mark these in g.emitted_types — per-bundle workers
			// inherit the map and would skip their own pass 1 typedef otherwise.
			g.sb.writeln('typedef struct ${inst.c_name} ${inst.c_name};')
			mut seen_field_typedef := map[string]bool{}
			for field in node.fields {
				ft := g.struct_field_type_for_emit(inst.c_name, field)
				fbase := ft.trim_right('*')
				if ft.starts_with('Array_') {
					g.emit_array_alias_decl(ft)
				}
				if fbase != inst.c_name && fbase.contains('_T_') && !fbase.starts_with('Array_')
					&& !fbase.starts_with('Map_') && fbase !in seen_field_typedef {
					seen_field_typedef[fbase] = true
					g.sb.writeln('typedef struct ${fbase} ${fbase};')
				}
				if ft.starts_with('Map_') {
					g.emit_map_alias_decl(ft)
				}
			}
			g.sb.writeln('${keyword} ${inst.c_name} {')
			for field in node.fields {
				field_type := g.struct_field_type_for_emit(inst.c_name, field)
				field_name := if field.name.len > 0 { field.name } else { 'value' }
				if decl := g.struct_fn_field_decl_for_emit(field, field_name) {
					g.sb.writeln('\t${decl};')
					g.struct_field_types['${inst.c_name}.${field_name}'] = g.struct_fn_field_lookup_type_for_emit(field) or {
						'void*'
					}
					continue
				}
				g.sb.writeln('\t${field_type} ${field_name};')
				// Register field types for this instantiation
				g.struct_field_types['${inst.c_name}.${field_name}'] = field_type
			}
			g.struct_field_lookup_cache = map[string]string{}
			g.struct_field_lookup_miss = map[string]bool{}
			if node.fields.len == 0 {
				g.sb.writeln('\tu8 _dummy;')
			}
			g.sb.writeln('};')
			// str macros — only emit fallback if no explicit str() method exists
			inst_str_fn := '${inst.c_name}__str'
			if inst_str_fn !in g.fn_return_types {
				inst_label := '${inst.c_name}{}'
				g.sb.writeln('#define ${inst.c_name}__str(v) ((string){.str = "${inst_label}", .len = ${inst_label.len}, .is_lit = 1})')
				g.sb.writeln('#define ${inst.c_name}_str(v) ${inst.c_name}__str(v)')
			}
			g.sb.writeln('')
			g.active_generic_types = prev_active.clone()
		}
	}
}

fn (mut g Gen) emit_late_concrete_struct_decl(node ast.StructDecl, name string) {
	body_key := 'body_${name}'
	if body_key in g.emitted_types || body_key in g.pending_late_body_keys {
		return
	}
	g.pending_late_body_keys[body_key] = true
	keyword := if node.is_union { 'union' } else { 'struct' }
	mut field_types := []string{cap: node.fields.len}
	for field in node.fields {
		field_type := g.struct_field_type_for_emit(name, field)
		field_types << field_type
		g.emit_late_generic_struct_dependency_for_type(field_type)
	}
	mut def := strings.new_builder(256)
	def.writeln('typedef ${keyword} ${name} ${name};')
	mut seen_field_typedef := map[string]bool{}
	for field_type in field_types {
		fbase := field_type.trim_right('*')
		if fbase != name && fbase.contains('_T_') && !fbase.starts_with('Array_')
			&& !fbase.starts_with('Map_') && fbase !in seen_field_typedef {
			seen_field_typedef[fbase] = true
			def.writeln('typedef struct ${fbase} ${fbase};')
		}
	}
	def.writeln('${keyword} ${name} {')
	for i, field in node.fields {
		field_type := if i < field_types.len {
			field_types[i]
		} else {
			g.struct_field_type_for_emit(name, field)
		}
		field_name := if field.name.len > 0 { escape_c_keyword(field.name) } else { 'value' }
		if decl := g.struct_fn_field_decl_for_emit(field, field_name) {
			def.writeln('\t${decl};')
			g.struct_field_types['${name}.${field.name}'] = g.struct_fn_field_lookup_type_for_emit(field) or {
				'void*'
			}
			continue
		}
		def.writeln('\t${field_type} ${field_name};')
		g.struct_field_types['${name}.${field.name}'] = field_type
	}
	if node.fields.len == 0 {
		def.writeln('\tu8 _dummy;')
	}
	def.writeln('};')
	label := '${name}{}'
	def.writeln('#define ${name}__str(v) ((string){.str = "${label}", .len = ${label.len}, .is_lit = 1})')
	def.writeln('#define ${name}_str(v) ${name}__str(v)')
	def.writeln('')
	g.late_struct_defs << def.str()
}

fn (mut g Gen) struct_has_unresolved_generic_value_dependency(node ast.StructDecl, owner_name string) bool {
	for field in node.fields {
		if g.is_pointer_type(field.typ) {
			continue
		}
		field_type := g.struct_field_type_for_emit(owner_name, field)
		if g.generic_struct_value_dependency_unresolved(field_type) {
			return true
		}
	}
	return false
}

fn (mut g Gen) struct_fn_field_decl_for_emit(field ast.FieldDecl, field_name string) ?string {
	if field.typ is ast.Type {
		if field.typ is ast.FnType {
			fn_type := field.typ as ast.FnType
			if g.fn_type_has_unresolved_generic_placeholder(fn_type) {
				return 'void* ${field_name}'
			}
			decl := c_fn_pointer_type_with_name(g.fn_type_c_type(fn_type), field_name)
			if decl != '' {
				return decl
			}
		}
	}
	return none
}

fn (mut g Gen) struct_fn_field_lookup_type_for_emit(field ast.FieldDecl) ?string {
	if field.typ is ast.Type {
		if field.typ is ast.FnType {
			fn_type := field.typ as ast.FnType
			if g.fn_type_has_unresolved_generic_placeholder(fn_type) {
				return 'void*'
			}
			return g.fn_type_c_type(fn_type)
		}
	}
	return none
}

fn (mut g Gen) fn_type_has_unresolved_generic_placeholder(fn_type ast.FnType) bool {
	for param in fn_type.params {
		if g.expr_has_unresolved_generic_placeholder(param.typ) {
			return true
		}
	}
	if fn_type.return_type !is ast.EmptyExpr {
		return g.expr_has_unresolved_generic_placeholder(fn_type.return_type)
	}
	return false
}

fn (mut g Gen) expr_has_unresolved_generic_placeholder(expr ast.Expr) bool {
	match expr {
		ast.Ident {
			return is_generic_placeholder_type_name(expr.name)
				&& expr.name !in g.active_generic_types
		}
		ast.PrefixExpr {
			return g.expr_has_unresolved_generic_placeholder(expr.expr)
		}
		ast.ModifierExpr {
			return g.expr_has_unresolved_generic_placeholder(expr.expr)
		}
		ast.GenericArgOrIndexExpr {
			return g.expr_has_unresolved_generic_placeholder(expr.lhs)
				|| g.expr_has_unresolved_generic_placeholder(expr.expr)
		}
		ast.GenericArgs {
			if g.expr_has_unresolved_generic_placeholder(expr.lhs) {
				return true
			}
			for arg in expr.args {
				if g.expr_has_unresolved_generic_placeholder(arg) {
					return true
				}
			}
			return false
		}
		ast.Type {
			match expr {
				ast.ArrayType {
					return g.expr_has_unresolved_generic_placeholder(expr.elem_type)
				}
				ast.ArrayFixedType {
					return g.expr_has_unresolved_generic_placeholder(expr.elem_type)
						|| g.expr_has_unresolved_generic_placeholder(expr.len)
				}
				ast.MapType {
					return g.expr_has_unresolved_generic_placeholder(expr.key_type)
						|| g.expr_has_unresolved_generic_placeholder(expr.value_type)
				}
				ast.OptionType {
					return g.expr_has_unresolved_generic_placeholder(expr.base_type)
				}
				ast.ResultType {
					return g.expr_has_unresolved_generic_placeholder(expr.base_type)
				}
				ast.PointerType {
					return g.expr_has_unresolved_generic_placeholder(expr.base_type)
				}
				ast.GenericType {
					if g.expr_has_unresolved_generic_placeholder(expr.name) {
						return true
					}
					for param in expr.params {
						if g.expr_has_unresolved_generic_placeholder(param) {
							return true
						}
					}
				}
				ast.FnType {
					return g.fn_type_has_unresolved_generic_placeholder(expr)
				}
				else {}
			}

			return false
		}
		else {
			return false
		}
	}
}

fn (mut g Gen) struct_field_type_for_emit(owner_name string, field ast.FieldDecl) string {
	first := g.qualify_owner_local_field_type(owner_name, g.expr_type_to_c(field.typ))
	second := g.qualify_owner_local_field_type(owner_name, g.expr_type_to_c(field.typ))
	if second != '' && second != first {
		return g.canonical_generic_struct_type_for_emit(second)
	}
	return g.canonical_generic_struct_type_for_emit(first)
}

fn (mut g Gen) canonical_generic_struct_type_for_emit(type_name string) string {
	mut base := type_name.trim_space()
	mut suffix := ''
	for base.ends_with('*') {
		base = base[..base.len - 1].trim_space()
		suffix += '*'
	}
	if base == '' || !base.contains('__T_') {
		return type_name
	}
	canonical := g.canonical_generic_struct_c_name_for_emit(base)
	if canonical == base {
		return type_name
	}
	return canonical + suffix
}

fn (mut g Gen) canonical_generic_struct_c_name_for_emit(c_name string) string {
	if c_name == '' || g.generic_struct_c_name_known_for_emit(c_name) {
		return c_name
	}
	if !c_name.contains('__T_') {
		return c_name
	}
	alt := c_name.replace('__T_', '_T_')
	if alt != c_name && g.generic_struct_c_name_known_for_emit(alt) {
		return alt
	}
	return c_name
}

fn (mut g Gen) generic_struct_c_name_known_for_emit(c_name string) bool {
	if c_name == '' {
		return false
	}
	if c_name in g.emitted_types || 'body_${c_name}' in g.emitted_types
		|| 'body_${c_name}' in g.pending_late_body_keys {
		return true
	}
	for _, instances in g.generic_struct_instances {
		for inst in instances {
			if inst.c_name == c_name {
				return true
			}
		}
	}
	if _ := g.find_struct_node_by_c_name(c_name) {
		return true
	}
	return false
}

fn (mut g Gen) generic_struct_value_dependency_unresolved(type_name string) bool {
	mut base := g.canonical_generic_struct_type_for_emit(type_name).trim_space()
	for base.ends_with('*') {
		base = base[..base.len - 1].trim_space()
	}
	if base == '' || !base.contains('_T_') || base.starts_with('Array_') || base.starts_with('Map_')
		|| base.starts_with('_option_') || base.starts_with('_result_') {
		return false
	}
	return 'body_${base}' !in g.emitted_types
}

fn (mut g Gen) gen_sum_type_decl(node ast.TypeDecl) {
	name := g.get_type_decl_name(node)
	body_key := 'body_${name}'
	if body_key in g.emitted_types {
		return
	}
	g.emitted_types[body_key] = true

	// Track variant names for sum type cast generation.
	mut variant_field_names := []string{cap: node.variants.len}
	if raw := g.lookup_type_by_c_name(name) {
		if raw is types.SumType {
			for variant_type in raw.variants {
				variant_c_name := g.types_type_to_c(variant_type)
				if variant_c_name != '' && variant_c_name != 'int' {
					variant_field_names << '_${variant_c_name}'
				}
			}
		}
	}
	if variant_field_names.len != node.variants.len {
		variant_field_names = []string{cap: node.variants.len}
		for i, variant in node.variants {
			variant_field_names << g.get_variant_field_name(variant, i)
		}
	}
	mut variant_names := []string{cap: variant_field_names.len}
	for vname in variant_field_names {
		variant_names << if vname.len > 1 && vname[0] == `_` { vname[1..] } else { vname }
	}
	g.sum_type_variants[name] = variant_names

	g.sb.writeln('struct ${name} {')
	g.sb.writeln('\tint _tag;')
	g.sb.writeln('\tunion {')
	for variant_name in variant_field_names {
		g.sb.writeln('\t\tvoid* ${variant_name};')
	}
	g.sb.writeln('\t} _data;')
	g.sb.writeln('};')
	g.sb.writeln('')
}

fn (mut g Gen) get_variant_field_name(variant ast.Expr, idx int) string {
	if variant is ast.Ident {
		return '_${variant.name}'
	} else if variant is ast.SelectorExpr {
		if variant.lhs is ast.Ident {
			return '_${variant.lhs.name}__${variant.rhs.name}'
		}
		return '_${variant.rhs.name}'
	} else if variant is ast.GenericArgOrIndexExpr {
		base := g.expr_type_to_c(variant.lhs)
		if base != '' && base != 'int' {
			return '_${base}'
		}
	} else if variant is ast.GenericArgs {
		base := g.expr_type_to_c(variant.lhs)
		if base != '' && base != 'int' {
			return '_${base}'
		}
	} else if variant is ast.Type {
		typ := variant as ast.Type
		if typ is ast.GenericType {
			base := g.expr_type_to_c(typ.name)
			if base != '' && base != 'int' {
				return '_${base}'
			}
		}
		if typ is ast.ArrayType {
			elem := mangle_alias_component(g.field_type_name(typ.elem_type))
			return '_Array_${elem}'
		}
		if typ is ast.MapType {
			key := mangle_alias_component(g.field_type_name(typ.key_type))
			val := mangle_alias_component(g.field_type_name(typ.value_type))
			return '_Map_${key}_${val}'
		}
	}
	return '_v${idx}'
}

fn (mut g Gen) infer_sum_variant_from_expr(type_name string, variants []string, expr ast.Expr) SumVariantMatch {
	// Handle module constants used as sum variants in selfhosted checker code
	// (e.g. char_, string_, void_, nil_, none_).
	if expr is ast.Ident {
		variant_hint := match expr.name {
			'char_' {
				'Char'
			}
			'string_' {
				'String'
			}
			'void_' {
				'Void'
			}
			'nil_' {
				'Nil'
			}
			'none_' {
				'None'
			}
			'rune_' {
				'Rune'
			}
			'usize_' {
				'USize'
			}
			'isize_' {
				'ISize'
			}
			'thread_' {
				'Thread'
			}
			'chan_' {
				'Channel'
			}
			'bool_', 'i8_', 'i16_', 'i32_', 'int_', 'i64_', 'u8_', 'u16_', 'u32_', 'u64_', 'f32_',
			'f64_', 'int_literal_', 'float_literal_' {
				'Primitive'
			}
			else {
				''
			}
		}

		if variant_hint != '' {
			for i, v in variants {
				if sum_type_variant_matches(v, variant_hint) {
					return SumVariantMatch{
						tag:          i
						field_name:   v
						is_primitive: variant_hint == 'Primitive'
						inner_type:   ''
					}
				}
			}
		}
	}

	// For InitExpr, infer from the struct type name
	if expr is ast.InitExpr {
		init_type := g.expr_type_to_c(expr.typ)
		mut resolved_init_type := init_type
		if (resolved_init_type == '' || resolved_init_type == 'int') && expr.typ is ast.Ident {
			resolved_init_type = expr.typ.name.replace('.', '__')
		}
		if resolved_init_type != '' {
			init_short := if resolved_init_type.contains('__') {
				resolved_init_type.all_after_last('__')
			} else {
				resolved_init_type
			}
			for i, v in variants {
				if v == init_short || v == resolved_init_type
					|| resolved_init_type.ends_with('__${v}')
					|| v.ends_with('__${resolved_init_type}') {
					return SumVariantMatch{
						tag:          i
						field_name:   v
						is_primitive: false
						inner_type:   resolved_init_type
					}
				}
			}
		}
	}
	// Generic fallback: if the expression is already a concrete AST sum variant
	// (e.g. ast.BasicLiteral being cast to ast.Expr), map it by runtime variant name.
	expr_variant := expr.type_name()
	if expr_variant != '' {
		expr_variant_c := expr_variant.replace('.', '__')
		expr_variant_short := if expr_variant.contains('.') {
			expr_variant.all_after_last('.')
		} else if expr_variant.contains('__') {
			expr_variant.all_after_last('__')
		} else {
			expr_variant
		}
		for i, v in variants {
			if sum_type_variant_matches(v, expr_variant_c)
				|| sum_type_variant_matches(v, expr_variant_short) {
				inner_type := if g.is_scalar_sum_payload_type(v)
					|| v in ['string', 'bool', 'voidptr', 'charptr', 'byteptr'] {
					v
				} else if type_name.contains('__') {
					'${type_name.all_before_last('__')}__${v}'
				} else {
					v
				}
				return SumVariantMatch{
					tag:          i
					field_name:   v
					is_primitive: g.is_scalar_sum_payload_type(v)
					inner_type:   inner_type
				}
			}
		}
	}
	return SumVariantMatch{
		tag: -1
	}
}

fn (g &Gen) is_scalar_sum_payload_type(type_name string) bool {
	return type_name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'bool', 'rune',
		'byte', 'usize', 'isize']
}

// gen_sum_wrapped_init_field checks if an InitExpr is a variant of the given
// sum type and wraps it accordingly. Returns true if wrapping was emitted.
fn (mut g Gen) gen_sum_wrapped_init_field(sum_type_name string, init_expr ast.InitExpr) bool {
	variants := g.get_sum_type_variants_for(sum_type_name)
	if variants.len == 0 {
		return false
	}
	init_type := g.expr_type_to_c(init_expr.typ)
	mut resolved := init_type
	if (resolved == '' || resolved == 'int') && init_expr.typ is ast.Ident {
		resolved = (init_expr.typ as ast.Ident).name.replace('.', '__')
	}
	if resolved == '' || resolved == sum_type_name {
		return false
	}
	init_short := if resolved.contains('__') { resolved.all_after_last('__') } else { resolved }
	for i, v in variants {
		if v == init_short || v == resolved || resolved.ends_with('__${v}')
			|| v.ends_with('__${resolved}') {
			g.gen_sum_type_wrap(sum_type_name, v, i, false, ast.Expr(init_expr), resolved)
			return true
		}
	}
	return false
}

fn (mut g Gen) gen_sum_wrapped_init_value_field(sum_type_name string, value ast.Expr) bool {
	if g.get_sum_type_variants_for(sum_type_name).len == 0 {
		return false
	}
	mut value_type := g.get_expr_type(value)
	if value is ast.Ident {
		if local_type := g.get_local_var_c_type(value.name) {
			if local_type != '' && local_type != 'int' {
				value_type = local_type
			}
		}
	}
	value_base := value_type.trim_right('*')
	if value_base == '' || value_base == 'int' || value_base == 'void'
		|| value_base == sum_type_name {
		return false
	}
	g.gen_type_cast_expr(sum_type_name, value)
	return true
}

fn (mut g Gen) gen_sum_type_wrap(type_name string, field_name string, tag int, is_primitive bool, expr ast.Expr, inner_type string) {
	_ = is_primitive
	g.sb.write_string('((${type_name}){._tag = ${tag}, ._data._${field_name} = ')
	resolved_type := g.resolve_sum_payload_storage_type(type_name, field_name, inner_type)
	if g.is_scalar_sum_payload_type(resolved_type) {
		// Keep scalar payloads encoded in pointer-size space. Smartcast extraction expects this.
		g.sb.write_string('((void*)((intptr_t)(')
		g.expr(expr)
		g.sb.write_string(')))')
	} else if inner_type == 'void*' {
		// The transformer pre-encodes payloads as void*-cast expressions:
		// `(voidptr)(intptr_t)(scalar)` for direct scalar aliases, or
		// `(voidptr)memdup(&struct, sizeof(...))` for boxed structs.
		// Emit them as-is rather than re-wrapping (double memdup leaks for
		// structs and dereferences NULL for scalars whose value is 0).
		if g.expr_is_voidptr_cast(expr) {
			g.expr(expr)
		} else if inner_expr := g.unwrap_addr_of_value_expr(expr) {
			// `&fn_call()` lowers to a statement-expression pointer to a temporary.
			// Copy by-value from the inner expression to avoid dangling addresses.
			g.tmp_counter++
			tmp_name := '_st${g.tmp_counter}'
			g.sb.write_string('((void*)({ ${resolved_type} ${tmp_name} = ')
			g.expr(inner_expr)
			g.sb.write_string('; memdup(&${tmp_name}, sizeof(${resolved_type})); }))')
		} else if g.sum_payload_expr_needs_temp(expr, resolved_type) {
			// Inner expr is a struct value (e.g. another sum type wrap or InitExpr).
			// memdup expects a pointer, so route through a temporary.
			g.tmp_counter++
			tmp_name := '_st${g.tmp_counter}'
			g.sb.write_string('((void*)({ ${resolved_type} ${tmp_name} = ')
			g.expr(expr)
			g.sb.write_string('; memdup(&${tmp_name}, sizeof(${resolved_type})); }))')
		} else {
			g.sb.write_string('((void*)memdup(')
			g.expr(expr)
			g.sb.write_string(', sizeof(${resolved_type})))')
		}
	} else {
		// Must heap-allocate (memdup) non-primitive sum type variants to avoid
		// dangling pointers to local variables that go out of scope.
		g.tmp_counter++
		tmp_name := '_st${g.tmp_counter}'
		g.sb.write_string('((void*)({ ${resolved_type} ${tmp_name} = ')
		g.expr(expr)
		g.sb.write_string('; memdup(&${tmp_name}, sizeof(${resolved_type})); }))')
	}
	g.sb.write_string('})')
}

fn (mut g Gen) sum_payload_expr_needs_temp(expr ast.Expr, resolved_type string) bool {
	unwrapped := strip_expr_wrappers(expr)
	return g.expr_yields_struct_value(expr) || g.expr_type_matches_value_type(expr, resolved_type)
		|| unwrapped is ast.SelectorExpr || unwrapped is ast.CallExpr
		|| g.sum_payload_expr_code_needs_temp(expr, resolved_type)
}

fn (mut g Gen) sum_payload_expr_code_needs_temp(expr ast.Expr, resolved_type string) bool {
	if resolved_type == '' || resolved_type.ends_with('*') || resolved_type in primitive_types {
		return false
	}
	saved_sb := g.sb
	g.sb = strings.new_builder(256)
	g.expr(expr)
	code := g.sb.str().trim_space()
	g.sb = saved_sb
	if code == '' || code.starts_with('&') {
		return false
	}
	return code.contains(')->') || code.contains(').')
}

fn (g &Gen) resolve_sum_payload_storage_type(sum_type_name string, variant_field string, inner_type string) string {
	mut resolved_type := inner_type
	if resolved_type.ends_with('__f32') {
		resolved_type = 'f32'
	} else if resolved_type.ends_with('__f64') {
		resolved_type = 'f64'
	}
	if resolved_type == '' || resolved_type == 'void*' || resolved_type == 'int'
		|| resolved_type == sum_type_name {
		return g.sum_type_variant_payload_type(sum_type_name, resolved_type, variant_field)
	}
	if !g.is_scalar_sum_payload_type(resolved_type)
		&& resolved_type !in ['string', 'bool', 'f32', 'f64', 'voidptr', 'charptr', 'byteptr', 'void*', 'char*', 'u8*'] {
		return g.sum_type_variant_payload_type(sum_type_name, resolved_type, variant_field)
	}
	return resolved_type
}

fn (g &Gen) get_sum_type_variants_for(type_name string) []string {
	sum_type := g.resolve_sum_type_name(type_name)
	if sum_type != '' {
		if variants := g.sum_type_variants[sum_type] {
			return variants
		}
	}
	lookup_name := if sum_type != '' { sum_type } else { type_name.trim_space().trim_right('*') }
	if raw := g.lookup_type_by_c_name_const(lookup_name) {
		if raw is types.SumType {
			mut variants := []string{cap: raw.variants.len}
			for variant in raw.variants {
				variants << g.types_type_to_c(variant)
			}
			return variants
		}
	}
	if lookup_name != type_name {
		if raw := g.lookup_type_by_c_name_const(type_name.trim_space().trim_right('*')) {
			if raw is types.SumType {
				mut variants := []string{cap: raw.variants.len}
				for variant in raw.variants {
					variants << g.types_type_to_c(variant)
				}
				return variants
			}
		}
	}
	return []string{}
}

fn (g &Gen) lookup_type_by_c_name_const(c_name string) ?types.Type {
	if g.env == unsafe { nil } || c_name == '' {
		return none
	}
	if c_name.contains('__') {
		parts := c_name.split('__')
		if parts.len == 2 {
			mod_name := parts[0]
			type_name := parts[1]
			if scope := g.env_scope(mod_name) {
				if typ := scope.lookup_type(type_name) {
					return typ
				}
			}
		}
	}
	mod_name := if g.cur_module != '' { g.cur_module } else { 'main' }
	short_name := if c_name.contains('__') { c_name.all_after_last('__') } else { c_name }
	mut modules_to_try := []string{}
	if !c_name.contains('__') && mod_name != 'main' && mod_name != 'builtin' {
		modules_to_try << 'main'
		modules_to_try << 'builtin'
	}
	modules_to_try << mod_name
	if 'main' !in modules_to_try {
		modules_to_try << 'main'
	}
	if 'builtin' !in modules_to_try {
		modules_to_try << 'builtin'
	}
	mut tried := map[string]bool{}
	for try_mod in modules_to_try {
		if tried[try_mod] {
			continue
		}
		tried[try_mod] = true
		if scope := g.env_scope(try_mod) {
			if typ := scope.lookup_type(short_name) {
				return typ
			}
		}
	}
	return none
}

fn (mut g Gen) sum_type_has_direct_variant(sum_type_name string, target_name string) bool {
	variants := g.get_sum_type_variants_for(sum_type_name)
	for variant in variants {
		if sum_type_variant_matches(variant, target_name) {
			return true
		}
	}
	if raw := g.lookup_type_by_c_name(sum_type_name) {
		if raw is types.SumType {
			for variant_type in raw.variants {
				variant_c_name := g.types_type_to_c(variant_type)
				if sum_type_variant_matches(variant_c_name, target_name) {
					return true
				}
			}
		}
	}
	return false
}

fn (g &Gen) resolve_sum_type_name(type_name string) string {
	sum_type := type_name.trim_space().trim_right('*')
	if sum_type == '' {
		return ''
	}
	if sum_type in g.sum_type_variants {
		return sum_type
	}
	// Composite array/map types are never themselves sum types — never fall
	// back to short-name matching, which would erroneously resolve e.g.
	// `Array_orm__Primitive` to the `orm__Primitive` sum type because of the
	// trailing `Primitive` segment.
	if sum_type.starts_with('Array_') || sum_type.starts_with('Map_') {
		return ''
	}
	if !sum_type.contains('__') {
		qualified_sum := g.get_qualified_name(sum_type)
		if qualified_sum in g.sum_type_variants {
			return qualified_sum
		}
	}
	short_sum := if sum_type.contains('__') { sum_type.all_after_last('__') } else { sum_type }
	if short_sum in g.sum_type_variants {
		return short_sum
	}
	mut found := ''
	for candidate, _ in g.sum_type_variants {
		if candidate.all_after_last('__') != short_sum {
			continue
		}
		if found != '' && found != candidate {
			return ''
		}
		found = candidate
	}
	return found
}

fn (g &Gen) sum_type_variant_field_name(sum_type_name string, variant_name string) string {
	mut resolved_sum_type := g.resolve_sum_type_name(sum_type_name)
	if resolved_sum_type == '' {
		resolved_sum_type = sum_type_name
	}
	variants := g.get_sum_type_variants_for(resolved_sum_type)
	qualified_variant := g.qualify_sum_type_variant_name(resolved_sum_type, variant_name)
	c_variant_name := variant_name.replace('.', '__')
	c_qualified_variant := qualified_variant.replace('.', '__')
	for variant in variants {
		c_variant := variant.replace('.', '__')
		if c_variant == c_variant_name || c_variant == c_qualified_variant {
			return variant
		}
	}
	for variant in variants {
		if sum_type_variant_matches(variant, variant_name)
			|| sum_type_variant_matches(variant, qualified_variant) {
			return variant
		}
	}
	if !variant_name.contains('__') && resolved_sum_type.contains('__')
		&& !g.is_scalar_sum_payload_type(variant_name)
		&& variant_name !in ['string', 'bool', 'voidptr', 'charptr', 'byteptr'] {
		prefix := resolved_sum_type.all_before_last('__')
		if prefix != '' {
			return '${prefix}__${variant_name}'
		}
	}
	return variant_name
}

fn sum_type_variant_match_name(name string) string {
	c_name := name.replace('.', '__')
	if c_name.starts_with('Array_') || c_name.starts_with('Array_fixed_')
		|| c_name.starts_with('Map_') || c_name.starts_with('_option_')
		|| c_name.starts_with('_result_') {
		return c_name
	}
	if c_name.contains('__') {
		return c_name.all_after_last('__')
	}
	return c_name
}

fn sum_type_variant_matches(variant string, target string) bool {
	c_variant := variant.replace('.', '__')
	c_target := target.replace('.', '__')
	if c_variant == c_target {
		return true
	}
	if c_variant == '' || c_target == '' {
		return false
	}
	if c_variant.contains('__') && c_target.contains('__') {
		return false
	}
	variant_short := sum_type_variant_match_name(c_variant)
	target_short := sum_type_variant_match_name(c_target)
	return variant_short == target_short || c_target.ends_with('__${variant_short}')
}

fn (g &Gen) qualify_sum_type_variant_name(sum_type_name string, variant_name string) string {
	if variant_name == '' || variant_name.contains('__') || !sum_type_name.contains('__') {
		return variant_name
	}
	prefix := sum_type_name.all_before_last('__')
	if prefix == '' {
		return variant_name
	}
	if variant_name.starts_with('Array_') && variant_name.len > 'Array_'.len {
		elem := variant_name['Array_'.len..]
		if !g.is_scalar_sum_payload_type(elem)
			&& elem !in ['string', 'bool', 'voidptr', 'charptr', 'byteptr'] {
			return 'Array_${prefix}__${elem}'
		}
	}
	if variant_name.starts_with('Map_') && variant_name.len > 'Map_'.len {
		rest := variant_name['Map_'.len..]
		split := rest.index_u8(`_`)
		if split >= 0 {
			key := rest[..split]
			value := rest[split + 1..]
			if value != '' && !value.contains('__') && !g.is_scalar_sum_payload_type(value)
				&& value !in ['string', 'bool', 'voidptr', 'charptr', 'byteptr'] {
				return 'Map_${key}_${prefix}__${value}'
			}
		}
	}
	return '${prefix}__${variant_name}'
}

fn (g &Gen) sum_type_variant_payload_type(sum_type_name string, target_type string, variant_field string) string {
	if variant_field == '' {
		return target_type
	}
	resolved_sum_type := g.resolve_sum_type_name(sum_type_name)
	sum_type := if resolved_sum_type != '' { resolved_sum_type } else { sum_type_name }
	if g.is_scalar_sum_payload_type(variant_field)
		|| variant_field in ['string', 'bool', 'f32', 'f64', 'voidptr', 'charptr', 'byteptr'] {
		return variant_field
	}
	if variant_field.starts_with('Array_') || variant_field.starts_with('Array_fixed_')
		|| variant_field.starts_with('Map_') || variant_field.starts_with('_option_')
		|| variant_field.starts_with('_result_') || variant_field.contains('__') {
		return variant_field
	}
	if target_type != '' && target_type != 'int' && target_type != 'void*'
		&& target_type != sum_type && target_type.contains('__') {
		return target_type
	}
	if sum_type.contains('__') {
		prefix := sum_type.all_before_last('__')
		if prefix != '' {
			return '${prefix}__${variant_field}'
		}
	}
	return variant_field
}

struct SumVariantTagStep {
	tag           int
	variant_field string
	payload_type  string
}

fn (mut g Gen) sum_variant_tag_path(sum_type_name string, target_name string, seen []string) ?[]SumVariantTagStep {
	sum_type := g.resolve_sum_type_name_for_common_field(sum_type_name)
	if sum_type == '' || sum_type in seen {
		return none
	}
	variants := g.get_sum_type_variants_for(sum_type)
	if variants.len == 0 {
		return none
	}
	for i, variant in variants {
		if sum_type_variant_matches(variant, target_name) {
			return [
				SumVariantTagStep{
					tag:           i
					variant_field: variant
					payload_type:  g.sum_type_variant_payload_type(sum_type, variant, variant)
				},
			]
		}
	}
	mut next_seen := seen.clone()
	next_seen << sum_type
	for i, variant in variants {
		payload_type := g.sum_type_variant_payload_type(sum_type, variant, variant)
		nested_sum_type := g.resolve_sum_type_name_for_common_field(payload_type)
		if nested_sum_type == '' {
			continue
		}
		mut child_path := g.sum_variant_tag_path(nested_sum_type, target_name, next_seen) or {
			continue
		}
		mut path := []SumVariantTagStep{cap: child_path.len + 1}
		path << SumVariantTagStep{
			tag:           i
			variant_field: variant
			payload_type:  nested_sum_type
		}
		path << child_path
		return path
	}
	return none
}

fn (mut g Gen) sum_data_variant_selector_field(sel ast.SelectorExpr) ?string {
	if !sel.rhs.name.starts_with('_') || sel.lhs !is ast.SelectorExpr {
		return none
	}
	lhs_sel := sel.lhs as ast.SelectorExpr
	if lhs_sel.rhs.name != '_data' {
		return none
	}
	mut sum_type := ''
	if raw_type := g.get_raw_type(lhs_sel.lhs) {
		match raw_type {
			types.SumType {
				sum_type = g.types_type_to_c(raw_type)
			}
			types.Pointer {
				if raw_type.base_type is types.SumType {
					sum_type = g.types_type_to_c(raw_type.base_type)
				}
			}
			types.Alias {
				if raw_type.base_type is types.SumType {
					sum_type = g.types_type_to_c(raw_type.base_type)
				}
			}
			else {}
		}
	}
	if sum_type == '' {
		sum_type = g.get_expr_type(lhs_sel.lhs).trim_space().trim_right('*')
	}
	if g.get_sum_type_variants_for(sum_type).len == 0 {
		return none
	}
	raw_variant := sel.rhs.name[1..]
	return '_${g.sum_type_variant_field_name(sum_type, raw_variant)}'
}

fn (g &Gen) is_sum_payload_expr(node ast.Expr, variant string) bool {
	match node {
		ast.SelectorExpr {
			return node.rhs.name == variant || node.rhs.name == '_${variant}'
		}
		ast.CastExpr {
			return g.is_sum_payload_expr(node.expr, variant)
		}
		ast.ParenExpr {
			return g.is_sum_payload_expr(node.expr, variant)
		}
		ast.PrefixExpr {
			return g.is_sum_payload_expr(node.expr, variant)
		}
		ast.ModifierExpr {
			return g.is_sum_payload_expr(node.expr, variant)
		}
		else {}
	}

	return false
}

fn (mut g Gen) gen_sum_variant_field_selector(node ast.SelectorExpr) bool {
	if node.rhs.name.starts_with('_') {
		return false
	}
	// If LHS is an as-cast, the cast already handles narrowing - skip double unwrap
	if node.lhs is ast.AsCastExpr {
		return false
	}
	mut lhs_sum_type := ''
	if node.lhs is ast.SelectorExpr {
		declared_lhs := g.selector_declared_field_type(node.lhs).trim_space().trim_right('*')
		if declared_lhs != '' && g.get_sum_type_variants_for(declared_lhs).len > 0 {
			lhs_sum_type = declared_lhs
		}
	}
	if raw_lhs := g.get_raw_type(node.lhs) {
		match raw_lhs {
			types.SumType {
				if lhs_sum_type == '' {
					lhs_sum_type = g.types_type_to_c(raw_lhs)
				}
			}
			types.Pointer {
				if lhs_sum_type == '' && raw_lhs.base_type is types.SumType {
					lhs_sum_type = g.types_type_to_c(raw_lhs.base_type)
				}
			}
			types.Alias {
				if lhs_sum_type == '' && raw_lhs.base_type is types.SumType {
					lhs_sum_type = g.types_type_to_c(raw_lhs.base_type)
				}
			}
			else {}
		}
	}
	if lhs_sum_type == '' {
		lhs_sum_type = g.get_expr_type(node.lhs)
	}
	mut variants := []string{}
	if vs := g.sum_type_variants[lhs_sum_type] {
		variants = vs.clone()
	} else if lhs_sum_type.contains('__') {
		short_sum := lhs_sum_type.all_after_last('__')
		if vs := g.sum_type_variants[short_sum] {
			variants = vs.clone()
		}
	} else {
		qualified_sum := g.get_qualified_name(lhs_sum_type)
		if vs := g.sum_type_variants[qualified_sum] {
			variants = vs.clone()
			lhs_sum_type = qualified_sum
		}
	}
	if variants.len == 0 {
		return false
	}
	mut matched_full := ''
	mut matched_field := ''
	for variant in variants {
		variant_short := sum_type_variant_match_name(variant)
		mut variant_full := variant
		if !variant_full.contains('__') && lhs_sum_type.contains('__')
			&& !g.is_scalar_sum_payload_type(variant_short)
			&& variant_short !in ['string', 'bool', 'voidptr', 'charptr', 'byteptr'] {
			prefix := lhs_sum_type.all_before_last('__')
			if prefix != '' {
				variant_full = '${prefix}__${variant_short}'
			}
		}
		key_full := '${variant_full}.${node.rhs.name}'
		key_short := '${variant_short}.${node.rhs.name}'
		has_field := key_full in g.struct_field_types || key_short in g.struct_field_types
			|| g.lookup_struct_field_type_by_name(variant_full, node.rhs.name) != none
			|| g.lookup_struct_field_type_by_name(variant_short, node.rhs.name) != none
		if has_field {
			if matched_full != '' && matched_full != variant_full {
				// Ambiguous field across multiple variants.
				return false
			}
			matched_full = variant_full
			matched_field = variant
		}
	}
	if matched_full == '' {
		return false
	}
	field_name := escape_c_keyword(node.rhs.name)
	owner := g.embedded_owner_for(matched_full, node.rhs.name)
	sep := if g.expr_is_pointer(node.lhs) { '->' } else { '.' }
	g.sb.write_string('(((${matched_full}*)(((')
	g.expr(node.lhs)
	g.sb.write_string(')${sep}_data._${matched_field})))')
	if owner != '' {
		g.sb.write_string('->${escape_c_keyword(owner)}.${field_name}')
	} else {
		g.sb.write_string('->${field_name}')
	}
	g.sb.write_string(')')
	return true
}

struct SumCommonFieldVariant {
	variant_field   string
	payload_type    string
	field_type      string
	owner           string
	nested_sum_type string
}

struct SumCommonFieldSelectorTarget {
	sum_type   string
	field_name string
	sum_expr   string
	is_ptr     bool
	infos      []SumCommonFieldVariant
}

struct SumCommonFieldAddressTarget {
	sum_type   string
	field_name string
	sum_expr   string
	is_ptr     bool
	infos      []SumCommonFieldVariant
	tail       []string
	value_type string
}

fn (mut g Gen) resolve_sum_type_name_for_common_field(type_name string) string {
	return g.resolve_sum_type_name(type_name)
}

fn (mut g Gen) sum_common_field_infos(sum_type_name string, field_name string, seen []string) ?[]SumCommonFieldVariant {
	sum_type := g.resolve_sum_type_name_for_common_field(sum_type_name)
	if sum_type == '' || sum_type in seen {
		return none
	}
	variants := g.get_sum_type_variants_for(sum_type)
	if variants.len == 0 {
		return none
	}
	mut next_seen := seen.clone()
	next_seen << sum_type
	mut infos := []SumCommonFieldVariant{cap: variants.len}
	mut common_field_type := ''
	for variant in variants {
		info := g.sum_common_field_info_for_variant(sum_type, variant, field_name, next_seen) or {
			return none
		}
		if common_field_type == '' {
			common_field_type = info.field_type
		} else if common_field_type != info.field_type {
			return none
		}
		infos << info
	}
	return infos
}

fn (mut g Gen) sum_common_field_info_for_variant(sum_type_name string, variant string, field_name string, seen []string) ?SumCommonFieldVariant {
	payload_type := g.sum_type_variant_payload_type(sum_type_name, variant, variant)
	if payload_type == '' || g.is_scalar_sum_payload_type(payload_type) {
		return none
	}
	if field_type := g.lookup_struct_field_type_by_name(payload_type, field_name) {
		return SumCommonFieldVariant{
			variant_field: variant
			payload_type:  payload_type
			field_type:    field_type
			owner:         g.embedded_owner_for(payload_type, field_name)
		}
	}
	nested_sum_type := g.resolve_sum_type_name_for_common_field(payload_type)
	if nested_sum_type != '' {
		nested_infos := g.sum_common_field_infos(nested_sum_type, field_name, seen) or {
			return none
		}
		if nested_infos.len == 0 {
			return none
		}
		return SumCommonFieldVariant{
			variant_field:   variant
			payload_type:    nested_sum_type
			field_type:      nested_infos[0].field_type
			nested_sum_type: nested_sum_type
		}
	}
	return none
}

fn (mut g Gen) sum_common_field_selector_target(node ast.SelectorExpr) ?SumCommonFieldSelectorTarget {
	if node.rhs.name.starts_with('_') {
		return none
	}
	if node.lhs is ast.AsCastExpr {
		return none
	}
	mut lhs_sum_type := ''
	if local_type := g.local_var_c_type_for_expr(node.lhs) {
		lhs_sum_type = g.resolve_sum_type_name_for_common_field(local_type)
		if lhs_sum_type == '' {
			return none
		}
	} else {
		if raw_lhs := g.get_raw_type(node.lhs) {
			match raw_lhs {
				types.SumType {
					lhs_sum_type = g.types_type_to_c(raw_lhs)
				}
				types.Pointer {
					if raw_lhs.base_type is types.SumType {
						lhs_sum_type = g.types_type_to_c(raw_lhs.base_type)
					}
				}
				types.Alias {
					if raw_lhs.base_type is types.SumType {
						lhs_sum_type = g.types_type_to_c(raw_lhs.base_type)
					}
				}
				else {}
			}
		}
		if lhs_sum_type == '' {
			lhs_sum_type = g.get_expr_type(node.lhs)
		}
	}
	if lhs_sum_type == '' {
		return none
	}
	lhs_sum_type = g.resolve_sum_type_name_for_common_field(lhs_sum_type)
	if lhs_sum_type == '' {
		return none
	}
	infos := g.sum_common_field_infos(lhs_sum_type, node.rhs.name, []string{}) or { return none }
	if infos.len == 0 {
		return none
	}
	lhs_code := g.expr_to_string(node.lhs)
	return SumCommonFieldSelectorTarget{
		sum_type:   lhs_sum_type
		field_name: node.rhs.name
		sum_expr:   lhs_code
		is_ptr:     g.expr_is_pointer(node.lhs)
		infos:      infos
	}
}

fn (mut g Gen) gen_sum_common_field_selector(node ast.SelectorExpr) bool {
	target := g.sum_common_field_selector_target(node) or { return false }
	g.emit_sum_common_field_expr(target.sum_type, target.field_name, target.sum_expr,
		target.is_ptr, target.infos)
	return true
}

fn (mut g Gen) emit_sum_common_field_expr(sum_type string, field_name string, sum_expr string, is_ptr bool, infos []SumCommonFieldVariant) {
	if infos.len == 0 {
		g.sb.write_string(zero_value_for_type('int'))
		return
	}
	field_type := infos[0].field_type
	g.tmp_counter++
	sum_tmp := '_sum_cf${g.tmp_counter}'
	value_tmp := '_field_cf${g.tmp_counter}'
	sum_decl_type := if is_ptr { '${sum_type}*' } else { sum_type }
	sep := if is_ptr { '->' } else { '.' }
	g.sb.write_string('({ ${sum_decl_type} ${sum_tmp} = ${sum_expr}; ${field_type} ${value_tmp} = ${zero_value_for_type(field_type)}; switch (${sum_tmp}${sep}_tag) {')
	for i, info in infos {
		g.sb.write_string('case ${i}: ${value_tmp} = ')
		g.emit_sum_common_field_variant_expr(info, field_name, sum_tmp, sep)
		g.sb.write_string('; break;')
	}
	g.sb.write_string('default: break;} ${value_tmp}; })')
}

fn (mut g Gen) gen_sum_common_field_assign(lhs ast.SelectorExpr, rhs ast.Expr) bool {
	target := g.sum_common_field_selector_target(lhs) or { return false }
	g.emit_sum_common_field_assign(target.sum_type, target.field_name, target.sum_expr,
		target.is_ptr, target.infos, rhs)
	return true
}

fn (mut g Gen) emit_sum_common_field_assign(sum_type string, field_name string, sum_expr string, is_ptr bool, infos []SumCommonFieldVariant, rhs ast.Expr) {
	field_type := infos[0].field_type
	g.tmp_counter++
	sum_tmp := '_sum_cf${g.tmp_counter}'
	value_tmp := '_field_cf${g.tmp_counter}'
	sum_decl_type := if is_ptr { '${sum_type}*' } else { sum_type }
	sep := if is_ptr { '->' } else { '.' }
	g.write_indent()
	g.sb.write_string('{ ${sum_decl_type} ${sum_tmp} = ${sum_expr}; ${field_type} ${value_tmp} = ')
	g.expr(rhs)
	g.sb.write_string('; switch (${sum_tmp}${sep}_tag) {')
	for i, info in infos {
		g.sb.write_string('case ${i}: ')
		g.emit_sum_common_field_variant_expr(info, field_name, sum_tmp, sep)
		g.sb.write_string(' = ${value_tmp}; break;')
	}
	g.sb.writeln('default: break;} }')
}

fn (mut g Gen) emit_sum_common_field_variant_expr(info SumCommonFieldVariant, field_name string, sum_tmp string, sep string) {
	payload_expr := '(((${info.payload_type}*)(${sum_tmp}${sep}_data._${info.variant_field})))'
	if info.nested_sum_type != '' {
		nested_infos := g.sum_common_field_infos(info.nested_sum_type, field_name, []string{}) or {
			g.sb.write_string(zero_value_for_type(info.field_type))
			return
		}
		g.emit_sum_common_field_expr(info.nested_sum_type, field_name, payload_expr, true,
			nested_infos)
		return
	}
	field_path := sum_common_field_path(info, field_name, [])
	g.sb.write_string('${payload_expr}->${field_path}')
}

fn (mut g Gen) gen_sum_common_field_selector_addr(node ast.SelectorExpr) bool {
	target := g.sum_common_field_address_target(node) or { return false }
	g.emit_sum_common_field_ptr_expr(target.sum_type, target.field_name, target.sum_expr,
		target.is_ptr, target.infos, target.tail, target.value_type)
	return true
}

fn (mut g Gen) sum_common_field_address_target(node ast.SelectorExpr) ?SumCommonFieldAddressTarget {
	mut tail := []string{}
	mut current := ast.Expr(node)
	for {
		if current is ast.SelectorExpr {
			if target := g.sum_common_field_selector_target(current) {
				mut value_type := g.selector_field_type(node)
				if value_type == '' {
					value_type = g.get_expr_type(ast.Expr(node))
				}
				if value_type == '' && tail.len == 0 && target.infos.len > 0 {
					value_type = target.infos[0].field_type
				}
				if value_type == '' {
					return none
				}
				return SumCommonFieldAddressTarget{
					sum_type:   target.sum_type
					field_name: target.field_name
					sum_expr:   target.sum_expr
					is_ptr:     target.is_ptr
					infos:      target.infos
					tail:       tail
					value_type: value_type
				}
			}
			tail.insert(0, current.rhs.name)
			current = unwrap_sum_common_field_address_base(current.lhs)
			continue
		}
		return none
	}
	return none
}

fn unwrap_sum_common_field_address_base(expr ast.Expr) ast.Expr {
	return match expr {
		ast.ParenExpr {
			unwrap_sum_common_field_address_base(expr.expr)
		}
		ast.ModifierExpr {
			unwrap_sum_common_field_address_base(expr.expr)
		}
		ast.CastExpr {
			unwrap_sum_common_field_address_base(expr.expr)
		}
		else {
			expr
		}
	}
}

fn (mut g Gen) emit_sum_common_field_ptr_expr(sum_type string, field_name string, sum_expr string, is_ptr bool, infos []SumCommonFieldVariant, tail []string, value_type string) {
	if infos.len == 0 {
		g.sb.write_string('NULL')
		return
	}
	g.tmp_counter++
	sum_tmp := '_sum_cf${g.tmp_counter}'
	value_tmp := '_field_cf${g.tmp_counter}'
	sum_decl_type := if is_ptr { '${sum_type}*' } else { sum_type }
	sep := if is_ptr { '->' } else { '.' }
	g.sb.write_string('({ ${sum_decl_type} ${sum_tmp} = ${sum_expr}; ${value_type}* ${value_tmp} = NULL; switch (${sum_tmp}${sep}_tag) {')
	for i, info in infos {
		g.sb.write_string('case ${i}: ${value_tmp} = ')
		g.emit_sum_common_field_variant_ptr_expr(info, field_name, sum_tmp, sep, tail, value_type)
		g.sb.write_string('; break;')
	}
	g.sb.write_string('default: break;} ${value_tmp}; })')
}

fn (mut g Gen) emit_sum_common_field_variant_ptr_expr(info SumCommonFieldVariant, field_name string, sum_tmp string, sep string, tail []string, value_type string) {
	payload_expr := '(((${info.payload_type}*)(${sum_tmp}${sep}_data._${info.variant_field})))'
	if info.nested_sum_type != '' {
		nested_infos := g.sum_common_field_infos(info.nested_sum_type, field_name, []string{}) or {
			g.sb.write_string('NULL')
			return
		}
		g.emit_sum_common_field_ptr_expr(info.nested_sum_type, field_name, payload_expr, true,
			nested_infos, tail, value_type)
		return
	}
	field_path := sum_common_field_path(info, field_name, tail)
	g.sb.write_string('&(${payload_expr}->${field_path})')
}

fn sum_common_field_path(info SumCommonFieldVariant, field_name string, tail []string) string {
	mut parts := []string{}
	if info.owner != '' {
		parts << escape_c_keyword(info.owner)
	}
	parts << escape_c_keyword(field_name)
	for part in tail {
		parts << escape_c_keyword(part)
	}
	return parts.join('.')
}

fn (mut g Gen) embedded_owner_for(struct_name string, field_name string) string {
	if struct_name == '' {
		return ''
	}
	mut lookup_name := struct_name
	if concrete := g.resolve_active_generic_type(strip_pointer_type_name(struct_name)) {
		// Generic veb helpers are emitted in the framework module, where a short
		// concrete name like `Context` can collide with `veb.Context`. Prefer the
		// active generic's concrete struct metadata before doing module-relative
		// name lookup, so promoted fields resolve through the app context embed.
		if concrete is types.Struct {
			for field in concrete.fields {
				if field.name == field_name {
					return ''
				}
			}
			if info := g.lookup_embedded_field_info_in_struct(concrete, field_name) {
				return info.owner
			}
		}
		lookup_name = g.types_type_to_c(concrete)
	}
	if active_concrete := g.active_generic_concrete_struct_for_c_name(lookup_name) {
		for field in active_concrete.fields {
			if field.name == field_name {
				return ''
			}
		}
		if info := g.lookup_embedded_field_info_in_struct(active_concrete, field_name) {
			return info.owner
		}
	}
	key := '${lookup_name}.${field_name}'
	if owner := g.embedded_field_owner[key] {
		return owner
	}
	if info := g.lookup_embedded_field_info(lookup_name, field_name) {
		return info.owner
	}
	return ''
}

fn (mut g Gen) active_generic_concrete_struct_for_c_name(c_name string) ?types.Struct {
	lookup_name := strip_pointer_type_name(c_name)
	if lookup_name == '' || g.active_generic_types.len == 0 {
		return none
	}
	for _, concrete in g.active_generic_types {
		concrete_c_name := strip_pointer_type_name(g.types_type_to_c(concrete))
		if concrete_c_name != lookup_name && short_type_name(concrete_c_name) != lookup_name {
			continue
		}
		if concrete is types.Struct {
			return concrete
		}
	}
	return none
}

fn (mut g Gen) is_fn_pointer_alias_type(type_name string) bool {
	if type_name == '' {
		return false
	}
	if type_name.contains('(*)') {
		return true
	}
	if raw_type := g.lookup_type_by_c_name(type_name) {
		if raw_type is types.Alias && raw_type.base_type is types.FnType {
			return true
		}
	}
	if g.env == unsafe { nil } {
		return type_name.ends_with('Fn')
	}
	mut candidates := []string{cap: 3}
	candidates << type_name
	if type_name.contains('__') {
		candidates << type_name.all_after_last('__')
	}
	for cand in candidates {
		if mut scope := g.env_scope(g.cur_module) {
			if obj := scope.lookup_parent(cand, 0) {
				typ := obj.typ()
				if typ is types.Alias {
					if typ.base_type is types.FnType {
						return true
					}
				}
			}
		}
		if mut scope := g.env_scope('builtin') {
			if obj := scope.lookup_parent(cand, 0) {
				typ := obj.typ()
				if typ is types.Alias {
					if typ.base_type is types.FnType {
						return true
					}
				}
			}
		}
		if cand.contains('__') {
			mod_name := cand.all_before('__')
			short_name := cand.all_after('__')
			if mut scope := g.env_scope(mod_name) {
				if obj := scope.lookup_parent(short_name, 0) {
					typ := obj.typ()
					if typ is types.Alias {
						if typ.base_type is types.FnType {
							return true
						}
					}
				}
			}
		}
	}
	return type_name.ends_with('Fn')
}

fn simd_vector_field_order(type_name string) []string {
	if type_name.ends_with('SimdFloat4') || type_name.ends_with('SimdInt4')
		|| type_name.ends_with('SimdU32_4') || type_name.ends_with('Vec4') {
		return ['x', 'y', 'z', 'w']
	}
	if type_name.ends_with('SimdFloat2') || type_name.ends_with('SimdUint2')
		|| type_name.ends_with('SimdI32_2') || type_name.ends_with('Vec2') {
		return ['x', 'y']
	}
	return []string{}
}

fn (mut g Gen) gen_simd_vector_init_expr(type_name string, fields []ast.FieldInit) bool {
	order := simd_vector_field_order(type_name)
	if order.len == 0 {
		return false
	}
	mut values := map[string]ast.Expr{}
	for field in fields {
		if field.name == '' {
			return false
		}
		values[field.name] = field.value
	}
	g.sb.write_string('((${type_name}){')
	for i, field_name in order {
		if i > 0 {
			g.sb.write_string(', ')
		}
		if value := values[field_name] {
			g.expr(value)
		} else {
			g.sb.write_string('0')
		}
	}
	g.sb.write_string('})')
	return true
}

fn unwrap_alias_type(typ types.Type) types.Type {
	if !type_has_valid_data(typ) {
		return typ
	}
	mut cur := typ
	for {
		if !type_has_valid_data(cur) {
			break
		}
		match cur {
			types.Alias {
				alias_type := cur as types.Alias
				cur = alias_type.base_type
			}
			else {
				break
			}
		}
	}
	return cur
}

fn struct_field_needs_explicit_default(field types.Field) bool {
	if field.default_expr !is ast.EmptyExpr {
		return true
	}
	field_type := unwrap_alias_type(field.typ)
	match field_type {
		types.Array, types.Map, types.OptionType, types.String {
			return true
		}
		types.Struct {
			return struct_type_needs_explicit_default(field_type)
		}
		else {}
	}

	return false
}

fn struct_type_needs_explicit_default(struct_type types.Struct) bool {
	for field in struct_type.fields {
		if struct_field_needs_explicit_default(field) {
			return true
		}
	}
	for emb in struct_type.embedded {
		if struct_type_needs_explicit_default(emb) {
			return true
		}
	}
	return false
}

fn map_int_key_width_from_c_type(type_name string) int {
	return match type_name {
		'i8', 'u8', 'byte', 'bool', 'char' { 1 }
		'i16', 'u16' { 2 }
		'i64', 'u64', 'f64', 'usize', 'isize' { 8 }
		else { 4 }
	}
}

fn map_runtime_key_fns_from_c_type(key_type_name string) (string, string, string, string) {
	if key_type_name == 'string' {
		return 'map_hash_string', 'map_eq_string', 'map_clone_string', 'map_free_string'
	}
	width := map_int_key_width_from_c_type(key_type_name)
	return 'map_hash_int_${width}', 'map_eq_int_${width}', 'map_clone_int_${width}', 'map_free_nop'
}

fn (mut g Gen) struct_default_field_is_direct(type_name string, field_name string) bool {
	if decl_info := g.find_struct_decl_info_by_c_name(type_name) {
		for field in decl_info.decl.fields {
			if field.name == field_name {
				return true
			}
		}
		return false
	}
	return true
}

fn embedded_struct_field_name(emb types.Struct) string {
	mut name := emb.name
	if name.contains('__') {
		name = name.all_after_last('__')
	}
	if name.contains('.') {
		name = name.all_after_last('.')
	}
	return name
}

fn (mut g Gen) resolve_embedded_struct(emb types.Struct) types.Struct {
	if emb.fields.len > 0 || emb.embedded.len > 0 || emb.name == '' {
		return emb
	}
	for lookup_name in [emb.name, emb.name.replace('.', '__')] {
		resolved := g.lookup_struct_type_by_c_name(lookup_name)
		if resolved.fields.len > 0 || resolved.embedded.len > 0 {
			return resolved
		}
	}
	return emb
}

fn (mut g Gen) resolve_struct_for_default_literal(struct_type types.Struct, type_name string) types.Struct {
	if struct_type.fields.len > 0 || struct_type.embedded.len > 0 {
		return struct_type
	}
	mut lookup_names := []string{}
	if type_name != '' {
		lookup_names << type_name
	}
	if struct_type.name != '' {
		lookup_names << struct_type.name
		lookup_names << struct_type.name.replace('.', '__')
	}
	for lookup_name in lookup_names {
		resolved := g.lookup_struct_type_by_c_name(lookup_name)
		if resolved.fields.len > 0 || resolved.embedded.len > 0 {
			return resolved
		}
	}
	return struct_type
}

fn (mut g Gen) write_struct_default_literal(struct_type types.Struct, type_name string) bool {
	resolved := g.resolve_struct_for_default_literal(struct_type, type_name)
	if resolved.fields.len == 0 && resolved.embedded.len == 0 {
		return false
	}
	g.sb.write_string('((${type_name}){')
	mut wrote_defaults := 0
	for field in resolved.fields {
		if !g.struct_default_field_is_direct(type_name, field.name) {
			continue
		}
		if !struct_field_needs_explicit_default(field) {
			continue
		}
		if wrote_defaults > 0 {
			g.sb.write_string(',')
		}
		g.sb.write_string('.${escape_c_keyword(field.name)} = ')
		if !g.write_struct_field_default_value(field, type_name) {
			g.sb.write_string('0')
		}
		wrote_defaults++
	}
	for emb in resolved.embedded {
		resolved_emb := g.resolve_embedded_struct(emb)
		emb_name := embedded_struct_field_name(resolved_emb)
		for field in resolved_emb.fields {
			if !struct_field_needs_explicit_default(field) {
				continue
			}
			if wrote_defaults > 0 {
				g.sb.write_string(',')
			}
			g.sb.write_string('.${escape_c_keyword(emb_name)}.${escape_c_keyword(field.name)} = ')
			emb_type_name := g.types_type_to_c(types.Type(resolved_emb))
			if !g.write_struct_field_default_value(field, emb_type_name) {
				g.sb.write_string('0')
			}
			wrote_defaults++
		}
	}
	if wrote_defaults == 0 {
		g.sb.write_string('0')
	}
	g.sb.write_string('})')
	return true
}

fn (mut g Gen) write_struct_field_default_value(field types.Field, owner_type_name string) bool {
	if field.default_expr !is ast.EmptyExpr {
		saved_module := g.cur_module
		owner_module := if owner_type_name.contains('__') {
			owner_type_name.all_before_last('__')
		} else {
			''
		}
		if owner_module != '' && owner_module != 'main' && owner_module != 'builtin' {
			g.cur_module = owner_module
		}
		defer {
			g.cur_module = saved_module
		}
		field_c_type := g.types_type_to_c(field.typ)
		if g.is_interface_type(field_c_type)
			&& g.gen_interface_cast(field_c_type, field.default_expr) {
			return true
		}
		g.expr(field.default_expr)
		return true
	}
	field_type := unwrap_alias_type(field.typ)
	if field_type is types.Array {
		array_type := field_type as types.Array
		elem_type := g.types_type_to_c(array_type.elem_type)
		g.sb.write_string('__new_array_with_default_noscan(0, 0, sizeof(${elem_type}), NULL)')
		return true
	}
	if field_type is types.Map {
		map_type := field_type as types.Map
		key_type := unwrap_alias_type(map_type.key_type)
		key_c_type := g.types_type_to_c(key_type)
		value_c_type := g.types_type_to_c(map_type.value_type)
		hash_fn, eq_fn, clone_fn, free_fn := map_runtime_key_fns_from_c_type(key_c_type)
		g.sb.write_string('new_map(sizeof(${key_c_type}), sizeof(${value_c_type}), (*&${hash_fn}), (*&${eq_fn}), (*&${clone_fn}), (*&${free_fn}))')
		return true
	}
	if field_type is types.String {
		g.sb.write_string(c_empty_v_string_expr())
		return true
	}
	if field_type is types.OptionType {
		option_type := g.known_unqualified_struct_literal_type(g.types_type_to_c(field.typ))
		if option_type.starts_with('_option_') {
			g.sb.write_string('(${option_type}){ .state = 2 }')
			return true
		}
	}
	if field_type is types.Struct {
		struct_type := field_type as types.Struct
		type_name := g.types_type_to_c(field_type)
		return g.write_struct_default_literal(struct_type, type_name)
	}
	return false
}

fn (mut g Gen) gen_none_literal_for_type(type_name string) bool {
	trimmed := type_name.trim_space()
	if trimmed == '' {
		return false
	}
	if trimmed.starts_with('_option_') {
		g.sb.write_string('(${trimmed}){ .state = 2 }')
		return true
	}
	if is_ierror_interface_name(trimmed) {
		g.sb.write_string('none__')
		return true
	}
	if is_type_name_pointer_like(trimmed) || trimmed in ['void*', 'voidptr', 'byteptr', 'charptr'] {
		g.sb.write_string('NULL')
		return true
	}
	return false
}

fn (g &Gen) unique_qualified_option_type(type_name string) string {
	if !type_name.starts_with('_option_') {
		return type_name
	}
	payload := type_name['_option_'.len..]
	if payload == '' || payload.contains('__') {
		return type_name
	}
	suffix := '__${payload}'
	mut matches := []string{}
	for name in g.option_aliases.keys() {
		if name.starts_with('_option_') && name['_option_'.len..].ends_with(suffix) {
			matches << name
		}
	}
	for name in g.emitted_option_structs.keys() {
		if name.starts_with('_option_') && name['_option_'.len..].ends_with(suffix)
			&& name !in matches {
			matches << name
		}
	}
	if matches.len == 0 {
		for key in g.emitted_types.keys() {
			if !key.starts_with('alias_') {
				continue
			}
			alias_name := key['alias_'.len..]
			if alias_name.ends_with(suffix) {
				matches << '_option_' + mangle_alias_component(alias_name)
			}
		}
	}
	if matches.len == 0 && g.cur_module != '' && g.cur_module !in ['main', 'builtin'] {
		qualified_payload := '${g.cur_module}__${payload}'
		if 'alias_${qualified_payload}' in g.emitted_types {
			matches << '_option_' + mangle_alias_component(qualified_payload)
		}
	}
	if matches.len == 1 {
		return matches[0]
	}
	return g.qualify_module_local_type_name(type_name)
}

fn option_payload_expr_can_be_contextually_typed_as_option(value ast.Expr) bool {
	unwrapped := strip_expr_wrappers(value)
	return match unwrapped {
		ast.BasicLiteral, ast.StringLiteral, ast.StringInterLiteral, ast.ArrayInitExpr {
			true
		}
		ast.SelectorExpr {
			unwrapped.lhs is ast.EmptyExpr
		}
		else {
			false
		}
	}
}

fn (mut g Gen) gen_option_wrapped_value_expr(option_type string, value ast.Expr) bool {
	expected_type := g.known_unqualified_struct_literal_type(option_type)
	if !expected_type.starts_with('_option_') {
		return false
	}
	if is_none_like_expr(value) {
		return g.gen_none_literal_for_type(expected_type)
	}
	value_type := option_value_type(expected_type)
	if value_type == '' || value_type == 'void' {
		return false
	}
	rhs_type := g.get_expr_type(value)
	contextual_payload := option_payload_expr_can_be_contextually_typed_as_option(value)
	if expected_type == '_option_string' && option_value_expr_is_builtin_option_string_clone(value) {
		g.expr(value)
		return true
	}
	if rhs_type == expected_type && !contextual_payload {
		g.expr(value)
		return true
	}
	if call_ret_type := g.option_result_value_expr_return_type(value) {
		if call_ret_type == expected_type && !contextual_payload {
			g.expr(value)
			return true
		}
		if (call_ret_type.starts_with('_option_') || call_ret_type.starts_with('_result_'))
			&& !contextual_payload {
			return false
		}
	}
	if (rhs_type.starts_with('_option_') || rhs_type.starts_with('_result_')) && !contextual_payload {
		return false
	}
	if value is ast.InitExpr && g.expr_type_to_c(value.typ) == expected_type {
		g.expr(value)
		return true
	}
	g.sb.write_string('({ ${expected_type} _opt = (${expected_type}){ .state = 2 }; ${value_type} _val = ')
	if value is ast.SelectorExpr {
		sel := value as ast.SelectorExpr
		if sel.lhs is ast.EmptyExpr && g.is_enum_type(value_type) {
			g.sb.write_string(g.enum_member_c_name(value_type, sel.rhs.name))
		} else if g.is_interface_type(value_type) && g.gen_interface_cast(value_type, value) {
		} else if !g.gen_auto_deref_value_param_arg(value_type, value) {
			g.expr(value)
		}
	} else if g.is_interface_type(value_type) && g.gen_interface_cast(value_type, value) {
	} else if !g.gen_auto_deref_value_param_arg(value_type, value) {
		g.expr(value)
	}
	g.sb.write_string('; _option_ok(&_val, (_option*)&_opt, sizeof(_val)); _opt; })')
	return true
}

fn option_string_clone_call_name(name string) bool {
	return name == 'builtin__Option_string__clone' || name == 'builtin____Option_string__clone'
		|| name.ends_with('__Option_string__clone')
}

fn option_value_expr_is_builtin_option_string_clone(value ast.Expr) bool {
	unwrapped := strip_expr_wrappers(value)
	match unwrapped {
		ast.CallExpr {
			if unwrapped.lhs is ast.Ident {
				return option_string_clone_call_name(unwrapped.lhs.name)
			}
		}
		ast.CallOrCastExpr {
			if unwrapped.lhs is ast.Ident {
				return option_string_clone_call_name(unwrapped.lhs.name)
			}
		}
		else {}
	}

	return false
}

fn (mut g Gen) option_result_value_expr_return_type(value ast.Expr) ?string {
	match value {
		ast.CallExpr {
			if ret := g.get_call_return_type(value.lhs, value.args) {
				if ret != '' && ret != 'int' {
					return ret
				}
			}
			if value.lhs is ast.Ident {
				name := sanitize_fn_ident(value.lhs.name)
				if option_string_clone_call_name(name) {
					return '_option_string'
				}
				if ret := g.fn_return_types[name] {
					return ret
				}
			}
		}
		ast.CallOrCastExpr {
			if value.lhs is ast.Ident {
				name := sanitize_fn_ident(value.lhs.name)
				if option_string_clone_call_name(name) {
					return '_option_string'
				}
				if ret := g.fn_return_types[name] {
					return ret
				}
			}
		}
		else {}
	}

	return none
}

fn (mut g Gen) gen_fixed_array_field_init_from_expr(fixed_type string, expr ast.Expr) bool {
	_, arr_len := parse_fixed_array_elem_type(fixed_type)
	if arr_len <= 0 {
		return false
	}
	if expr is ast.ArrayInitExpr && expr.exprs.len > 0 {
		g.sb.write_u8(`{`)
		for i in 0 .. arr_len {
			if i > 0 {
				g.sb.write_string(', ')
			}
			if i < expr.exprs.len {
				g.expr(expr.exprs[i])
			} else {
				g.sb.write_string('0')
			}
		}
		g.sb.write_u8(`}`)
		return true
	}
	g.sb.write_u8(`{`)
	for i in 0 .. arr_len {
		if i > 0 {
			g.sb.write_string(', ')
		}
		g.expr(expr)
		g.sb.write_string('[${i}]')
	}
	g.sb.write_u8(`}`)
	return true
}

fn (mut g Gen) init_field_expected_type(type_name string, env_struct types.Struct, field_name string) string {
	expected_key := '${type_name}.${field_name}'
	mut expected_field_type := g.struct_field_types[expected_key] or { '' }
	if expected_field_type == '' && type_name.contains('__') {
		short_type := type_name.all_after_last('__')
		short_expected_key := '${short_type}.${field_name}'
		expected_field_type = g.struct_field_types[short_expected_key] or { '' }
	}
	if expected_field_type != '' {
		return expected_field_type
	}
	if generic_field_type := g.generic_instance_field_type_for_init(type_name, field_name) {
		return generic_field_type
	}
	for field in env_struct.fields {
		if field.name == field_name {
			return g.types_type_to_c(field.typ)
		}
	}
	for emb in env_struct.embedded {
		resolved_emb := g.resolve_embedded_struct(emb)
		emb_name := embedded_struct_field_name(resolved_emb)
		if emb_name == field_name {
			return g.types_type_to_c(types.Type(resolved_emb))
		}
	}
	if info := g.lookup_embedded_field_info(type_name, field_name) {
		if info.field_type != '' {
			return info.field_type
		}
	}
	return ''
}

fn (mut g Gen) generic_instance_field_type_for_init(type_name string, field_name string) ?string {
	if type_name == '' || field_name == '' {
		return none
	}
	old_module := g.cur_module
	old_file := g.cur_file_name
	prev_active := g.active_generic_types.clone()
	defer {
		g.cur_module = old_module
		g.cur_file_name = old_file
		g.active_generic_types = prev_active.clone()
	}
	for struct_base, instances in g.generic_struct_instances {
		for inst in instances {
			if inst.c_name != type_name {
				continue
			}
			node := g.find_generic_struct_node(struct_base) or { return none }
			g.active_generic_types = inst.bindings.clone()
			for field in node.fields {
				if field.name != field_name {
					continue
				}
				if fn_field_type := g.struct_fn_field_lookup_type_for_emit(field) {
					return fn_field_type
				}
				return g.struct_field_type_for_emit(inst.c_name, field)
			}
		}
	}
	return none
}

fn (mut g Gen) gen_channel_init_expr(node ast.InitExpr) bool {
	mut is_channel := false
	mut elem_c_type := ''
	if node.typ is ast.Type && node.typ is ast.ChannelType {
		is_channel = true
		ch_type := node.typ as ast.ChannelType
		elem_c_type = g.expr_type_to_c(ch_type.elem_type)
	}
	if raw_type := g.get_raw_type(node.typ) {
		unwrapped := unwrap_alias_type(raw_type)
		if unwrapped is types.Channel {
			is_channel = true
			if elem_type := unwrapped.elem_type {
				elem_c_type = g.types_type_to_c(elem_type)
			}
		}
	}
	if !is_channel && g.expr_type_to_c(node.typ) != 'chan' {
		return false
	}
	if elem_c_type == '' {
		elem_c_type = 'void*'
	}
	g.force_emit_fn_names['sync__new_channel_st'] = true
	g.mark_called_fn_name('sync__new_channel_st')
	g.sb.write_string('sync__new_channel_st(')
	mut wrote_cap := false
	for field in node.fields {
		if field.name == 'cap' {
			g.expr(field.value)
			wrote_cap = true
			break
		}
	}
	if !wrote_cap && node.typ is ast.Type && node.typ is ast.ChannelType {
		ch_type := node.typ as ast.ChannelType
		if ch_type.cap !is ast.EmptyExpr {
			g.expr(ch_type.cap)
			wrote_cap = true
		}
	}
	if !wrote_cap {
		g.sb.write_string('0')
	}
	g.sb.write_string(', sizeof(${elem_c_type}) > 0 ? sizeof(${elem_c_type}) : 1)')
	return true
}

fn (mut g Gen) known_unqualified_struct_literal_type(type_name string) string {
	normalized_builtin := g.normalize_builtin_qualified_c_type(type_name)
	if normalized_builtin != type_name {
		return normalized_builtin
	}
	if type_name.starts_with('_option_') {
		return g.unique_qualified_option_type(type_name)
	}
	if type_name == '' || type_name.contains('__') || type_name.starts_with('Array_')
		|| type_name.starts_with('Map_') || type_name.starts_with('_result_')
		|| type_name in primitive_types || type_name in ['bool', 'string', 'void*', 'voidptr'] {
		return type_name
	}
	if type_name.contains('_T_') && !type_name.contains('__') {
		mod_name := if g.cur_module != '' && g.cur_module != 'main' && g.cur_module != 'builtin' {
			g.cur_module
		} else if g.cur_fn_c_name.contains('__') {
			g.cur_fn_c_name.all_before('__')
		} else {
			''
		}
		if mod_name != '' {
			base_name := type_name.all_before('_T_')
			if base_name != '' {
				if scope := g.env_scope(mod_name) {
					if _ := scope.lookup_type(base_name) {
						return '${mod_name}__${type_name}'
					}
				}
			}
		}
	}
	if 'body_${type_name}' in g.emitted_types {
		return type_name
	}
	if 'body_${type_name}' in g.pending_late_body_keys {
		return type_name
	}
	if 'alias_${type_name}' in g.emitted_types {
		return type_name
	}
	mut matched := ''
	mut match_count := 0
	suffix := '__${type_name}'
	for key, _ in g.emitted_types {
		if key.starts_with('body_') || key.starts_with('alias_') {
			prefix_len := if key.starts_with('body_') { 'body_'.len } else { 'alias_'.len }
			candidate := key[prefix_len..]
			if candidate.ends_with(suffix) {
				matched = candidate
				match_count++
				if match_count > 1 {
					return g.qualify_module_local_type_name(type_name)
				}
			}
		}
	}
	for key, _ in g.pending_late_body_keys {
		if key.starts_with('body_') {
			candidate := key['body_'.len..]
			if candidate.ends_with(suffix) {
				matched = candidate
				match_count++
				if match_count > 1 {
					return g.qualify_module_local_type_name(type_name)
				}
			}
		}
	}
	if match_count == 1 {
		return matched
	}
	return g.qualify_module_local_type_name(type_name)
}

fn (g &Gen) contextual_specialized_init_type(type_name string) string {
	if type_name == '' || g.cur_fn_ret_type == '' || type_name.starts_with('Array_')
		|| type_name.starts_with('Map_') || type_name.starts_with('_option_')
		|| type_name.starts_with('_result_') || type_name in primitive_types
		|| type_name in ['bool', 'string', 'void*', 'voidptr'] {
		return type_name
	}
	mut context_type := g.cur_fn_ret_type.trim_space()
	if context_type.starts_with('_option_') {
		context_type = option_value_type(context_type)
	} else if context_type.starts_with('_result_') {
		context_type = g.result_value_c_type(context_type)
	}
	context_type = strip_pointer_type_name(context_type)
	if context_type == '' {
		return type_name
	}
	if context_type.starts_with('Array_') || context_type.starts_with('Map_')
		|| context_type.starts_with('Tuple_') {
		return type_name
	}
	context_base := context_type.all_before('_T_')
	if context_type.contains('__') && !context_type.contains('_T_')
		&& short_type_name(context_type) == short_type_name(type_name) {
		return context_type
	}
	if !context_type.contains('_T_') {
		return type_name
	}
	type_base := if type_name.contains('_T_') { type_name.all_before('_T_') } else { type_name }
	if context_base == type_base || short_type_name(context_base) == short_type_name(type_base) {
		return context_type
	}
	return type_name
}

fn expected_init_expr_type_for_expr(expr ast.Expr, expected_type string) ?string {
	if expected_type == '' {
		return none
	}
	match expr {
		ast.InitExpr {
			return expected_type
		}
		ast.ParenExpr {
			return expected_init_expr_type_for_expr(expr.expr, expected_type)
		}
		ast.ModifierExpr {
			return expected_init_expr_type_for_expr(expr.expr, expected_type)
		}
		ast.CastExpr {
			return expected_init_expr_type_for_expr(expr.expr, expected_type)
		}
		ast.PrefixExpr {
			if expr.op == .amp {
				base_type := strip_pointer_type_name(expected_type)
				if base_type != '' && base_type != expected_type {
					return expected_init_expr_type_for_expr(expr.expr, base_type)
				}
			}
		}
		else {}
	}

	return none
}

fn (mut g Gen) expr_with_expected_init_type(expr ast.Expr, expected_type string) {
	init_expected_type := expected_init_expr_type_for_expr(expr, expected_type) or {
		g.expr(expr)
		return
	}
	saved_expected_type := g.expected_init_expr_type
	g.expected_init_expr_type = init_expected_type
	g.expr(expr)
	g.expected_init_expr_type = saved_expected_type
}

fn init_expr_can_use_expected_type(node ast.InitExpr, type_name string, expected_type string) bool {
	if expected_type == '' || expected_type.starts_with('Array_')
		|| expected_type.starts_with('Map_') || expected_type.starts_with('_option_')
		|| expected_type.starts_with('_result_') || expected_type in primitive_types
		|| expected_type in ['bool', 'string', 'void*', 'voidptr'] {
		return false
	}
	if node.typ !is ast.Ident {
		return false
	}
	ident := node.typ as ast.Ident
	ident_name := ident.name
	if ident_name == '' || ident_name.contains('.') || ident_name.contains('__') {
		return false
	}
	expected_base := strip_pointer_type_name(expected_type)
	type_base := strip_pointer_type_name(type_name)
	return short_type_name(expected_base) == ident_name
		|| is_generic_placeholder_type_name(ident_name)
		|| (type_base != '' && short_type_name(expected_base) == short_type_name(type_base))
}

fn generic_init_type_base_matches(resolved_type string, base_name string) bool {
	if resolved_type == '' || base_name == '' {
		return false
	}
	resolved_base := if resolved_type.contains('_T_') {
		resolved_type.all_before('_T_')
	} else {
		resolved_type
	}
	return resolved_base == base_name
		|| short_type_name(resolved_base) == short_type_name(base_name)
}

fn (mut g Gen) canonical_init_generic_type_name(typ ast.Expr, type_name string) string {
	if type_name == '' || !type_name.contains('_T_') {
		return type_name
	}
	match typ {
		ast.GenericArgOrIndexExpr {
			base_name := g.generic_type_lhs_to_c(typ.lhs)
			if generic_init_type_base_matches(type_name, base_name) {
				return g.resolve_generic_struct_c_name(base_name, [typ.expr])
			}
		}
		ast.GenericArgs {
			base_name := g.generic_type_lhs_to_c(typ.lhs)
			if generic_init_type_base_matches(type_name, base_name) {
				return g.resolve_generic_struct_c_name(base_name, typ.args)
			}
		}
		ast.Type {
			if typ is ast.GenericType {
				base_name := g.generic_type_lhs_to_c(typ.name)
				if generic_init_type_base_matches(type_name, base_name) {
					return g.resolve_generic_struct_c_name(base_name, typ.params)
				}
			}
		}
		else {}
	}

	return type_name
}

fn (mut g Gen) canonical_recorded_generic_type_name(type_name string) string {
	if type_name == '' || !type_name.contains('_T_') {
		return type_name
	}
	base_name := type_name.all_before('_T_')
	params_key := type_name.all_after('_T_')
	mut instances := g.generic_struct_instances[base_name]
	if instances.len == 0 && base_name.contains('__') {
		instances = g.generic_struct_instances[base_name.all_after_last('__')]
	} else if instances.len == 0 && !base_name.contains('__') && g.cur_module != ''
		&& g.cur_module != 'main' && g.cur_module != 'builtin' {
		instances = g.generic_struct_instances['${g.cur_module}__${base_name}']
	}
	for inst in instances {
		if inst.params_key == params_key {
			return inst.c_name
		}
	}
	return type_name
}

fn (mut g Gen) gen_init_expr(node ast.InitExpr) {
	expected_type := g.expected_init_expr_type
	g.expected_init_expr_type = ''
	mut type_name := g.expr_type_to_c(node.typ)
	type_name = g.canonical_init_generic_type_name(node.typ, type_name)
	type_name = g.canonical_recorded_generic_type_name(type_name)
	if node.typ is ast.Ident {
		if g.type_expr_has_metadata(node.typ) {
			// Position metadata is authoritative for synthesized type expressions.
		} else if fn_local_type := g.current_fn_module_local_type_name(node.typ.name) {
			type_name = fn_local_type
		}
	}
	mut used_expected_type := false
	if init_expr_can_use_expected_type(node, type_name, expected_type) {
		type_name = expected_type
		used_expected_type = true
	}
	if !used_expected_type {
		type_name = g.known_unqualified_struct_literal_type(type_name)
		type_name = g.contextual_specialized_init_type(type_name)
	}
	if g.gen_channel_init_expr(node) {
		return
	}
	if type_name.starts_with('_option_') && gen_init_expr_is_none_option(node) {
		g.sb.write_string('((${type_name}){ .state = 2 })')
		return
	}
	mut env_struct := types.Struct{}
	mut has_struct_defaults := false
	if raw_type := g.get_raw_type(node.typ) {
		unwrapped := unwrap_alias_type(raw_type)
		if unwrapped is types.Struct {
			env_struct = unwrapped
			has_struct_defaults = true
		}
	}
	if !has_struct_defaults && !type_name.starts_with('Array_') && !type_name.starts_with('Map_') {
		resolved := g.lookup_struct_type_by_c_name(type_name)
		if resolved.fields.len > 0 {
			env_struct = resolved
			has_struct_defaults = true
		}
	}
	if node.fields.len == 0 {
		if type_name.ends_with('*') {
			g.sb.write_string('0')
			return
		}
		if has_struct_defaults && (env_struct.fields.len > 0 || env_struct.embedded.len > 0) {
			if g.write_struct_default_literal(env_struct, type_name) {
				return
			}
		}
		if type_name.starts_with('Array_') {
			elem_c_type := type_name[6..] // e.g. Array_ast__Stmt → ast__Stmt
			g.sb.write_string('__new_array_with_default_noscan(0, 0, sizeof(${elem_c_type}), NULL)')
			return
		}
		g.sb.write_string('((${type_name}){0})')
		return
	}
	if g.gen_simd_vector_init_expr(type_name, node.fields) {
		return
	}
	if g.gen_specialized_sum_init_expr(type_name, node) {
		return
	}
	g.sb.write_string('((${type_name}){')
	mut wrote_fields := 0
	for field in node.fields {
		if wrote_fields > 0 {
			g.sb.write_string(',')
		}
		wrote_fields++
		if field.name == '' {
			g.expr(field.value)
			continue
		}
		if type_name in g.sum_type_variants && field.name.starts_with('_data._') {
			raw_variant_name := field.name.all_after('_data._')
			variant_name := g.sum_type_variant_field_name(type_name, raw_variant_name)
			field_name := '_data._${variant_name}'
			g.sb.write_string('.${field_name} = ')
			mut inner_type := g.get_expr_type(field.value)
			resolved_type := g.resolve_sum_payload_storage_type(type_name, variant_name, inner_type)
			if g.is_scalar_sum_payload_type(resolved_type) {
				// Keep scalar payloads encoded in pointer-size space.
				g.sb.write_string('((void*)((intptr_t)(')
				g.expr(field.value)
				g.sb.write_string(')))')
			} else if inner_type == 'void*' {
				// The transformer already lowered the payload to a void* expression
				// (either `(voidptr)(intptr_t)(scalar)` for direct scalar aliases or
				// `(voidptr)memdup(&struct, sizeof(...))` for boxed payloads). Trust
				// that encoding and emit it as-is. Re-wrapping with another `memdup`
				// would double-allocate for structs and dereference NULL for scalars.
				if g.expr_is_voidptr_cast(field.value) {
					g.expr(field.value)
				} else if inner_expr := g.unwrap_addr_of_value_expr(field.value) {
					// `&fn_call()` can point to a temporary; copy by-value from inner expr.
					g.tmp_counter++
					tmp_name := '_st${g.tmp_counter}'
					g.sb.write_string('((void*)({ ${resolved_type} ${tmp_name} = ')
					g.expr(inner_expr)
					g.sb.write_string('; memdup(&${tmp_name}, sizeof(${resolved_type})); }))')
				} else if g.sum_payload_expr_needs_temp(field.value, resolved_type) {
					g.tmp_counter++
					tmp_name := '_st${g.tmp_counter}'
					g.sb.write_string('((void*)({ ${resolved_type} ${tmp_name} = ')
					g.expr(field.value)
					g.sb.write_string('; memdup(&${tmp_name}, sizeof(${resolved_type})); }))')
				} else {
					g.sb.write_string('((void*)memdup(')
					g.expr(field.value)
					g.sb.write_string(', sizeof(${resolved_type})))')
				}
			} else {
				g.tmp_counter++
				tmp_name := '_st${g.tmp_counter}'
				g.sb.write_string('((void*)({ ${resolved_type} ${tmp_name} = ')
				g.expr(field.value)
				g.sb.write_string('; memdup(&${tmp_name}, sizeof(${resolved_type})); }))')
			}
			continue
		}
		owner := g.embedded_owner_for(type_name, field.name)
		field_name := escape_c_keyword(field.name)
		if owner != '' {
			g.sb.write_string('.${escape_c_keyword(owner)}.${field_name} = ')
		} else {
			g.sb.write_string('.${field_name} = ')
		}
		mut expected_field_type := g.init_field_expected_type(type_name, env_struct, field.name)
		expected_field_type = g.known_unqualified_struct_literal_type(expected_field_type)
		if expected_field_type.starts_with('Array_fixed_')
			&& g.gen_fixed_array_field_init_from_expr(expected_field_type, field.value) {
			continue
		}
		if is_none_like_expr(field.value) && g.gen_none_literal_for_type(expected_field_type) {
			continue
		}
		if expected_field_type.starts_with('_option_')
			&& g.gen_option_wrapped_value_expr(expected_field_type, field.value) {
			continue
		}
		if g.is_fn_pointer_alias_type(expected_field_type) {
			if field.value is ast.SelectorExpr {
				sel := field.value as ast.SelectorExpr
				if g.gen_bound_method_value_expr(sel, expected_field_type) {
					continue
				}
				if method_value_name := g.selector_method_value_name(sel) {
					g.sb.write_string('((${expected_field_type})${method_value_name})')
					continue
				}
			}
			if field.value is ast.Ident {
				g.sb.write_string('((${expected_field_type})')
				g.expr(field.value)
				g.sb.write_string(')')
				continue
			}
		}
		if expected_field_type in ['void*', 'voidptr'] && field.value is ast.SelectorExpr {
			sel := field.value as ast.SelectorExpr
			if g.gen_bound_method_value_expr(sel, 'void*') {
				continue
			}
			if method_value_name := g.selector_method_value_name(sel) {
				g.sb.write_string('((void*)${method_value_name})')
				continue
			}
		}
		// Disambiguate shorthand enum values in struct field initializers
		// using the field's declared enum type.
		if field.value is ast.SelectorExpr {
			sel := field.value as ast.SelectorExpr
			if sel.lhs is ast.EmptyExpr {
				expected_enum := expected_field_type
				if expected_enum != '' && g.is_enum_type(expected_enum) {
					g.sb.write_string(g.enum_member_c_name(expected_enum, sel.rhs.name))
					continue
				}
			}
		}
		if g.should_deref_init_field_value(type_name, field.name, expected_field_type, field.value) {
			g.sb.write_string('(*(')
			g.expr(field.value)
			g.sb.write_string('))')
			continue
		}
		// Auto-wrap concrete types into interface structs for interface-typed fields
		// (e.g. output_stream: log__stderr where output_stream is io__Writer and stderr is os__File)
		if expected_field_type != '' && g.is_interface_type(expected_field_type)
			&& g.gen_interface_cast(expected_field_type, field.value) {
			continue
		}
		// Pointer-to-interface fields (e.g. dd: &DrawDevice = &DrawDeviceContext{...})
		if expected_field_type != '' && expected_field_type.ends_with('*') {
			iface_base := expected_field_type.trim_right('*')
			if g.is_interface_type(iface_base) {
				rhs_type := g.get_expr_type(field.value)
				rhs_base := rhs_type.trim_right('*')
				if rhs_base != '' && rhs_base != 'int' && rhs_base != iface_base
					&& !g.is_interface_type(rhs_base) {
					if !g.gen_heap_interface_cast(iface_base, field.value) {
						g.expr(field.value)
					}
					continue
				}
			}
		}
		// Auto-wrap variant struct literals into sum type wrapping
		// (e.g. types.Struct{} -> types__Type{._tag = N, ._data._Struct = ...})
		// Only for InitExpr values (struct literals that are a variant of the sum type).
		if expected_field_type != '' && field.value is ast.InitExpr
			&& g.gen_sum_wrapped_init_field(expected_field_type, field.value) {
			continue
		}
		if expected_field_type != ''
			&& g.gen_sum_wrapped_init_value_field(expected_field_type, field.value) {
			continue
		}
		g.expr(field.value)
	}
	g.sb.write_string('})')
}

fn (mut g Gen) gen_specialized_sum_init_expr(type_name string, node ast.InitExpr) bool {
	variants := g.get_sum_type_variants_for(type_name)
	if variants.len == 0 {
		return false
	}
	if node.fields.len == 0 {
		return false
	}
	mut value := node.fields[0].value
	mut found_data_field := false
	for field in node.fields {
		if field.name.starts_with('_data._') {
			value = field.value
			found_data_field = true
			break
		}
	}
	if !found_data_field {
		return false
	}
	payload_value := g.unwrap_sum_init_payload_value(value)
	mut value_type := g.get_expr_type(payload_value)
	if local_type := g.local_var_c_type_for_expr(payload_value) {
		if local_type != '' && local_type != 'int' {
			value_type = local_type
		}
	}
	if value_type == '' {
		return false
	}
	mut wrap_value := payload_value
	mut tag, mut field_name := sum_type_variant_for_arg_type(value_type, variants)
	if tag < 0 && value_type.ends_with('*') {
		value_base := value_type.trim_right('*')
		tag, field_name = sum_type_variant_for_arg_type(value_base, variants)
		if tag >= 0 {
			value_type = value_base
			wrap_value = ast.PrefixExpr{
				op:   .mul
				expr: payload_value
			}
		}
	}
	if tag < 0 || value_type == type_name {
		return false
	}
	is_primitive :=
		value_type in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64', 'bool', 'rune', 'byte', 'usize', 'isize']
		|| value_type in g.primitive_type_aliases
	g.gen_sum_type_wrap(type_name, field_name, tag, is_primitive, wrap_value, value_type)
	return true
}

fn (mut g Gen) unwrap_sum_init_payload_value(expr ast.Expr) ast.Expr {
	match expr {
		ast.ParenExpr {
			return g.unwrap_sum_init_payload_value(expr.expr)
		}
		ast.ModifierExpr {
			return g.unwrap_sum_init_payload_value(expr.expr)
		}
		ast.CastExpr {
			cast_type := g.expr_type_to_c(expr.typ)
			if cast_type in ['void*', 'voidptr', 'intptr', 'intptr_t', 'isize', 'usize', 'int',
				'i64'] {
				return g.unwrap_sum_init_payload_value(expr.expr)
			}
		}
		else {}
	}

	return expr
}

fn gen_init_expr_is_none_option(node ast.InitExpr) bool {
	if node.fields.len != 1 {
		return false
	}
	field := node.fields[0]
	if field.name != 'state' {
		return false
	}
	if field.value is ast.BasicLiteral {
		return field.value.value == '2'
	}
	return false
}

fn (mut g Gen) should_deref_init_field_value(struct_type string, field_name string, expected_field_type string, value ast.Expr) bool {
	expected_key := '${struct_type}.${field_name}'
	mut expected := expected_field_type
	if expected == '' {
		expected = g.struct_field_types[expected_key] or { '' }
	}
	if expected == '' && struct_type.contains('__') {
		short_struct := struct_type.all_after_last('__')
		short_expected_key := '${short_struct}.${field_name}'
		expected = g.struct_field_types[short_expected_key] or { '' }
	}
	if expected == '' || is_type_name_pointer_like(expected) {
		return false
	}
	mut value_type := g.get_expr_type(value)
	if value is ast.Ident {
		if local_type := g.get_local_var_c_type(value.name) {
			if local_type != '' && local_type != 'int' {
				value_type = local_type
			}
		}
	}
	// When the value is a SelectorExpr with .data on a result/option variable
	// (e.g., _or_t48.data where _or_t48 is _result_SomeTypeptr), the generated
	// C code extracts the pointer from the result data area. The V-level type
	// says SomeType (non-pointer), but the actual C output is SomeType*.
	if !is_type_name_pointer_like(value_type) && value is ast.SelectorExpr {
		sel := value as ast.SelectorExpr
		if sel.rhs.name == 'data' {
			lhs_type := g.get_expr_type(sel.lhs)
			if lhs_type.starts_with('_result_') {
				inner := g.result_value_type(lhs_type)
				if is_type_name_pointer_like(inner) {
					value_type = inner
				}
			} else if lhs_type.starts_with('_option_') {
				inner := option_value_type(lhs_type)
				if is_type_name_pointer_like(inner) {
					value_type = inner
				}
			}
		}
	}
	// When the value is a CastExpr whose target type is a pointer (e.g., from
	// result/option unwrap like `new_socket()!` producing `&Socket`), the generated
	// C code will yield a pointer.
	if !is_type_name_pointer_like(value_type) && value is ast.CastExpr {
		cast_type := g.expr_type_to_c((value as ast.CastExpr).typ)
		if is_type_name_pointer_like(cast_type) {
			value_type = cast_type
		}
	}
	if !is_type_name_pointer_like(value_type) {
		call_ret_type := g.expr_pointer_return_type(value)
		if call_ret_type != '' {
			value_type = call_ret_type
		}
	}
	if !is_type_name_pointer_like(value_type) {
		return false
	}
	expected_base := strip_pointer_type_name(expected)
	value_base := strip_pointer_type_name(value_type)
	if expected_base == value_base {
		return true
	}
	return short_type_name(expected_base) == short_type_name(value_base)
}
