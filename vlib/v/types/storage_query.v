module types

import v.flat

const storage_query_probe_max_observations = 65536
const storage_query_probe_max_history_entries = 4096
const storage_query_probe_max_history_bytes = 4 * 1024 * 1024

struct StorageQueryResult {
	writes map[string][]int
	guards StorageQueryGuards
}

struct StorageQueryGuards {
	present []u64
	absent  []u64
}

struct StorageQueryUnion {
	entry_idx int
	guard_id  u64
	incoming  bool
	existing  bool
}

@[heap]
struct StorageQueryProbe {
	read_base              &TypeChecker = unsafe { nil }
	initial_error_count    int
	initial_node_count     int
	initial_children_count int
mut:
	recheck_requested                bool
	uncertain                        bool
	value_observations               int
	unguarded_reference_observations int
	// Positive prune stamps let nested and cached proofs pass ancestor dependencies to parents.
	prune_observations u64
	pruned_ancestors   map[int]u64
	history_no_local   map[int][]StorageQueryHistoryProof
	history_count      int
	history_bytes      int
}

struct StorageQueryHistoryProof {
	key                          string
	ancestors                    []int
	unguarded_reference_observed bool
}

fn (tc &TypeChecker) storage_query_probe_note_unguarded_reference() {
	if !isnil(tc.storage_query_probe) {
		mut probe := tc.storage_query_probe
		probe.unguarded_reference_observations++
	}
}

fn (tc &TypeChecker) storage_query_probe_record_pruned_ancestors(ids []int) {
	mut probe := tc.storage_query_probe
	if probe.prune_observations == max_u64 {
		probe.uncertain = true
		return
	}
	probe.prune_observations++
	for id in ids {
		if id !in probe.pruned_ancestors && probe.pruned_ancestors.len >= storage_query_probe_max_observations {
			probe.uncertain = true
			return
		}
		probe.pruned_ancestors[id] = probe.prune_observations
	}
}

fn (tc &TypeChecker) storage_query_probe_note_pruned_ancestors(ids []int) {
	if isnil(tc.storage_query_probe) || ids.len == 0 { return }
	mut probe := tc.storage_query_probe
	$if prealloc {
		if tc.storage_query_probe_scopes.len == 0 {
			probe.uncertain = true
			return
		}
		mut states := []voidptr{len: tc.storage_query_probe_scopes.len - 1, init: voidptr(0)}
		suspend_storage_query_scopes(tc.storage_query_probe_scopes, mut states)
		tc.storage_query_probe_record_pruned_ancestors(ids)
		resume_storage_query_scopes(tc.storage_query_probe_scopes, states)
	} $else {
		tc.storage_query_probe_record_pruned_ancestors(ids)
	}
}

fn (tc &TypeChecker) storage_query_probe_facts_unchanged() bool {
	probe := tc.storage_query_probe
	if isnil(probe) || probe.uncertain || probe.recheck_requested {
		return false
	}
	base := probe.read_base
	if isnil(base) || tc.errors.len != probe.initial_error_count
		|| tc.a.nodes.len != probe.initial_node_count || tc.a.children.len != probe.initial_children_count
		|| tc.fn_context.node_id != base.fn_context.node_id || voidptr(tc.cur_scope) != voidptr(base.cur_scope)
		|| tc.cur_file != base.cur_file || tc.cur_module != base.cur_module
		|| tc.smartcasts != base.smartcasts || tc.expected_expr_id != base.expected_expr_id
		|| tc.trust_checked_expr_types != base.trust_checked_expr_types
		|| tc.expected_expr_type != base.expected_expr_type || tc.cur_fn_ret_type != base.cur_fn_ret_type
		|| tc.channel_send_or_expr_id != base.channel_send_or_expr_id
		|| tc.fn_context.generic_params != base.fn_context.generic_params || tc.fn_context.return_type != base.fn_context.return_type
		|| tc.placeholder_types != base.placeholder_types || tc.type_param_texts != base.type_param_texts
		|| tc.type_params_expanding != base.type_params_expanding || tc.generic_decl_file != base.generic_decl_file {
		return false
	}
	// Sibling forks can discard private annotations. Cache only the unchanged checked facts.
	if tc.sparse_expr_type_values.len > 0 || tc.sparse_resolved_call_names.len > 0
		|| tc.sparse_resolved_fn_values.len > 0 || tc.fork_fn_value_writes.len > 0
		|| tc.sparse_statement_nodes.len > 0 {
		return false
	}
	if !isnil(tc.fork_overlay) && (tc.fork_overlay.resolved_call_names.len > 0
		|| tc.fork_overlay.resolved_fn_values.len > 0) {
		return false
	}
	return true
}

fn (tc &TypeChecker) storage_query_probe_checked_call_info(id flat.NodeId) ?CallInfo {
	if !tc.storage_query_probe_facts_unchanged() { return none }
	base := tc.storage_query_probe.read_base
	memo := base.body_resolve_memo
	idx := int(id)
	if !tc.memo_call_info || !base.memo_call_info || isnil(memo) || !memo.active
		|| idx < memo.lo || idx > memo.hi {
		return none
	}
	slot := idx & 2047
	if memo.call_generations[slot] != memo.call_generation || memo.call_ids[slot] != idx {
		return none
	}
	info := memo.call_infos[slot]
	// Keep mutable array descriptors private while borrowing the completed checked result.
	return CallInfo{ ...info, params: info.params.clone(), shared_params: info.shared_params.clone() }
}

fn (tc &TypeChecker) storage_query_probe_checked_type(id flat.NodeId) ?Type {
	if !tc.storage_query_probe_facts_unchanged() { return none }
	memo := tc.storage_query_probe.read_base.body_resolve_memo
	idx := int(id)
	if isnil(memo) || !memo.active || idx < memo.lo || idx > memo.hi {
		return none
	}
	mi := idx - memo.lo
	if memo.filled[mi] == 0 { return none }
	return memo.types[mi]
}

fn (tc &TypeChecker) storage_query_probe_history_key(id flat.NodeId, visited []flat.NodeId, local_sources map[string]flat.NodeId) ?string {
	if local_sources.len > 0 || !tc.storage_query_probe_facts_unchanged() { return none }
	mut ancestors := []int{cap: visited.len}
	for ancestor in visited {
		if int(ancestor) !in ancestors { ancestors << int(ancestor) }
	}
	ancestors.sort()
	return '${int(id)}:${ancestors}'
}

fn (tc &TypeChecker) storage_query_probe_cached_history(id flat.NodeId, visited []flat.NodeId, local_sources map[string]flat.NodeId) bool {
	key := tc.storage_query_probe_history_key(id, visited, local_sources) or { return false }
	mut probe := tc.storage_query_probe
	for proof in probe.history_no_local[int(id)] {
		if proof.key == key {
			// A cached child must not hide reference observations from its parent certificate.
			if proof.unguarded_reference_observed {
				probe.unguarded_reference_observations++
			} else {
				tc.storage_query_probe_note_pruned_ancestors(proof.ancestors)
			}
			return !probe.uncertain && !probe.recheck_requested
		}
		if !proof.unguarded_reference_observed && proof.ancestors.all(flat.NodeId(it) in visited) {
			// Keep only incoming ancestors that actually pruned this completed proof.
			tc.storage_query_probe_note_pruned_ancestors(proof.ancestors)
			return !probe.uncertain && !probe.recheck_requested
		}
	}
	return false
}

fn (tc &TypeChecker) storage_query_probe_store_history(id flat.NodeId, key string, ancestors []int, unguarded_reference_observed bool, estimated_bytes int) {
	mut probe := tc.storage_query_probe
	mut entries := unsafe { probe.history_no_local[int(id)] }
	entries << StorageQueryHistoryProof{ key: key.clone(), ancestors: ancestors.clone(), unguarded_reference_observed: unguarded_reference_observed }
	probe.history_no_local[int(id)] = entries
	probe.history_count++
	probe.history_bytes += estimated_bytes
}

fn (tc &TypeChecker) storage_query_probe_remember_history(id flat.NodeId, visited []flat.NodeId, local_sources map[string]flat.NodeId, unguarded_reference_observed bool, prunes_before u64) {
	key := tc.storage_query_probe_history_key(id, visited, local_sources) or { return }
	mut probe := tc.storage_query_probe
	for proof in probe.history_no_local[int(id)] {
		if proof.key == key { return }
	}
	mut ancestors := []int{cap: visited.len}
	for ancestor in visited {
		if (unguarded_reference_observed || probe.pruned_ancestors[int(ancestor)] > prunes_before)
			&& int(ancestor) !in ancestors {
			ancestors << int(ancestor)
		}
	}
	ancestors.sort()
	estimated_bytes := key.len + ancestors.len * int(sizeof(int)) + 2 * int(sizeof(StorageQueryHistoryProof)) + 4 * int(sizeof(voidptr))
	if probe.history_count >= storage_query_probe_max_history_entries
		|| estimated_bytes > storage_query_probe_max_history_bytes - probe.history_bytes {
		return
	}
	$if prealloc {
		if tc.storage_query_probe_scopes.len == 0 { return }
		mut states := []voidptr{len: tc.storage_query_probe_scopes.len - 1, init: voidptr(0)}
		// Retained keys and map growth must outlive every nested disposable observer.
		suspend_storage_query_scopes(tc.storage_query_probe_scopes, mut states)
		tc.storage_query_probe_store_history(id, key, ancestors, unguarded_reference_observed, estimated_bytes)
		resume_storage_query_scopes(tc.storage_query_probe_scopes, states)
	} $else {
		tc.storage_query_probe_store_history(id, key, ancestors, unguarded_reference_observed, estimated_bytes)
	}
}

fn (tc &TypeChecker) storage_query_probe_can_observe() bool {
	if isnil(tc.storage_query_probe) { return true }
	mut probe := tc.storage_query_probe
	if probe.recheck_requested || probe.uncertain { return false }
	if probe.value_observations >= storage_query_probe_max_observations {
		// Abandon the optional proof, never the original storage-source analysis.
		probe.uncertain = true
		return false
	}
	probe.value_observations++
	return true
}

@[heap]
struct StorageQueryTrace {
mut:
	guard_id u64
	guards   map[u64]bool
	complete bool = true
}

// A summary returns only paths and parameter indexes. Query state is disposable;
// memo entries retain the ancestor membership checks required to replay a result.
fn (tc &TypeChecker) param_storage_writes_for_decl(decl VisibleMutationFnDecl, target_param_idx int, mut visiting map[u64]bool) map[string][]int {
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		defer { unsafe { prealloc_scope_end(scope) } }
		needs_analysis := if param := tc.visible_mutation_fn_param(decl, target_param_idx) {
			param.is_mut
		} else {
			false
		}
		if !needs_analysis {
			parent := unsafe { prealloc_scope_suspend(scope) }
			empty := map[string][]int{}
			unsafe { prealloc_scope_resume(scope, parent) }
			return empty
		}
		mut cache := tc.visible_mutation_cache
		use_cache := !isnil(cache) && cache.storage_query
		guard_id := visible_mutation_cache_id(decl, target_param_idx)
		if guard_id in visiting {
			parent := unsafe { prealloc_scope_suspend(scope) }
			if use_cache {
				cache.record_storage_query_guard(guard_id, true)
			}
			empty := map[string][]int{}
			unsafe { prealloc_scope_resume(scope, parent) }
			return empty
		}
		cache_key := '${decl.mod}:${decl.idx}:${target_param_idx}'
		inherited_owner := use_cache && !isnil(cache.storage_query_owner)
		mut owner := if inherited_owner {
			cache.storage_query_owner
		} else {
			&VisibleMutationCache{ storage_query: true }
		}
		if inherited_owner {
			entries := owner.storage_query_results[cache_key]
			matched := storage_query_matching_index(entries, visiting)
			if matched >= 0 {
				cached := entries[matched]
				parent := unsafe { prealloc_scope_suspend(scope) }
				cache.record_storage_query_certificate(cached.guards)
				unsafe { prealloc_scope_resume(scope, parent) }
				// The outer memo arena outlives every nested query; consumers only read it.
				return cached.writes
			}
		}
		mut view := tc.fork_storage_query_view()
		view.visible_mutation_cache.storage_query_owner = owner
		mut scopes := if inherited_owner { cache.storage_query_scopes.clone() } else { []voidptr{} }
		scopes << scope
		view.visible_mutation_cache.storage_query_scopes = scopes
		view.visible_mutation_cache.storage_query_trace = &StorageQueryTrace{
			guard_id: guard_id
			guards:   {
				guard_id: false
			}
		}
		mut active := visiting.clone()
		result := view.param_storage_writes_for_decl_unscoped(decl, target_param_idx, mut active)
		trace := view.visible_mutation_cache.storage_query_trace
		// An active declaration also returns no paths, so this guard cannot affect an empty result.
		if trace.complete && result.len == 0 {
			trace.guards.delete(guard_id)
		}
		mut consensus := StorageQueryUnion{ entry_idx: -1 }
		if inherited_owner && trace.complete && trace.guards.len > 0 {
			if proof := owner.storage_query_union(cache_key, result, trace.guards) {
				consensus = proof
				if consensus.incoming {
					// Only this complete incoming proof is propagated to the parent.
					trace.guards.delete(consensus.guard_id)
				}
			}
		}
		// Keep the initial existing-entry proof separate from later incoming deletions.
		mut sharing_idx := consensus.entry_idx
		if consensus.entry_idx >= 0 && trace.guards.len > 0 {
			forward_idx := owner.storage_query_forward_union(cache_key, result, mut trace.guards)
			if forward_idx >= 0 { sharing_idx = forward_idx }
		}
		// Map iteration copies string keys, so estimate inside the disposable arena.
		certificate := if inherited_owner && trace.complete {
			storage_query_encode_guards(trace.guards)
		} else {
			StorageQueryGuards{}
		}
		mut estimated_bytes := if inherited_owner && trace.complete {
			storage_query_result_bytes(cache_key, result, certificate)
		} else {
			0
		}
		// Borrow immutable map descriptors; no copied payload is mutated or freed.
		mut retained_result := unsafe { result }
		mut clone_result := true
		if sharing_idx >= 0 {
			// The equality proof also permits borrowing this result after admission stops.
			retained_result = unsafe { owner.storage_query_results[cache_key][sharing_idx].writes }
			clone_result = false
			estimated_bytes = storage_query_entry_bytes(cache_key, certificate)
		} else if inherited_owner && trace.complete {
			entry_bytes := storage_query_entry_bytes(cache_key, certificate)
			if owner.storage_query_can_admit(entry_bytes) {
				if shared := owner.storage_query_shared_result(cache_key, result) {
					retained_result = unsafe { shared }
					clone_result = false
					estimated_bytes = entry_bytes
				}
			}
		}
		// Allocate suspension bookkeeping before selecting any older arena.
		mut states := []voidptr{len: if inherited_owner {
			cache.storage_query_scopes.len - 1
		} else {
			0
		}, init: voidptr(0)}
		parent := unsafe { prealloc_scope_suspend(scope) }
		if use_cache {
			cache.record_storage_query_guards(trace.guards, trace.complete)
		}
		if inherited_owner && trace.complete {
			// Only admitted memo payloads are allocated in the outer query's arena.
			suspend_storage_query_scopes(cache.storage_query_scopes, mut states)
			if consensus.entry_idx >= 0 {
				if !owner.broaden_storage_query_entry_to_guards(cache_key, consensus.entry_idx,
					trace.guards) && consensus.existing {
					owner.broaden_storage_query_entry(cache_key, consensus.entry_idx, consensus.guard_id)
				}
			}
			retained := owner.cache_storage_query_result(cache_key, retained_result, trace.guards,
				certificate, clone_result, estimated_bytes)
			resume_storage_query_scopes(cache.storage_query_scopes, states)
			if cached := retained {
				unsafe { prealloc_scope_resume(scope, parent) }
				return cached
			}
			if sharing_idx >= 0 {
				unsafe { prealloc_scope_resume(scope, parent) }
				return retained_result
			}
		}
		promoted := clone_storage_query_result(result)
		unsafe { prealloc_scope_resume(scope, parent) }
		return promoted
	} $else {
		return tc.param_storage_writes_for_decl_unscoped(decl, target_param_idx, mut visiting)
	}
}

fn suspend_storage_query_scopes(scopes []voidptr, mut states []voidptr) {
	$if prealloc {
		for i := scopes.len - 1; i > 0; i-- {
			states[i - 1] = unsafe { prealloc_scope_suspend(scopes[i]) }
		}
	}
}

fn resume_storage_query_scopes(scopes []voidptr, states []voidptr) {
	$if prealloc {
		for i in 1 .. scopes.len {
			unsafe {
				prealloc_scope_resume(scopes[i], states[i - 1])
			}
		}
	}
}

fn storage_query_encode_guards(guards map[u64]bool) StorageQueryGuards {
	mut present_count := 0
	for _, present in guards {
		if present { present_count++ }
	}
	mut present := []u64{len: present_count}
	mut absent := []u64{len: guards.len - present_count}
	mut present_idx := 0
	mut absent_idx := 0
	for id, expected in guards {
		if expected {
			present[present_idx] = id
			present_idx++
		} else {
			absent[absent_idx] = id
			absent_idx++
		}
	}
	return StorageQueryGuards{ present: present, absent: absent }
}

fn storage_query_guards_match(guards StorageQueryGuards, visiting map[u64]bool) bool {
	for id in guards.present {
		if id !in visiting { return false }
	}
	for id in guards.absent {
		if id in visiting { return false }
	}
	return true
}

fn storage_query_matching_index(entries []StorageQueryResult, visiting map[u64]bool) int {
	mut matched := -1
	mut conditions := 0
	for i, entry in entries {
		entry_conditions := entry.guards.present.len + entry.guards.absent.len
		if (matched < 0 || entry_conditions < conditions)
			&& storage_query_guards_match(entry.guards, visiting) {
			matched = i
			conditions = entry_conditions
			if conditions == 0 { return matched }
		}
	}
	return matched
}

fn storage_query_complementary_guard(left StorageQueryGuards, right map[u64]bool) ?u64 {
	if left.present.len + left.absent.len > right.len { return none }
	mut differences := 0
	mut complementary := u64(0)
	for id in left.present {
		actual := right[id] or { return none }
		if !actual {
			differences++
			complementary = id
			if differences > 1 { return none }
		}
	}
	for id in left.absent {
		actual := right[id] or { return none }
		if actual {
			differences++
			complementary = id
			if differences > 1 { return none }
		}
	}
	if differences == 1 { return complementary }
	return none
}

fn storage_query_existing_complementary_guard(existing StorageQueryGuards, incoming map[u64]bool) ?u64 {
	if incoming.len > existing.present.len + existing.absent.len { return none }
	mut found := 0
	mut differences := 0
	mut complementary := u64(0)
	for id in existing.present {
		if actual := incoming[id] {
			found++
			if !actual {
				differences++
				complementary = id
				if differences > 1 { return none }
			}
		}
	}
	for id in existing.absent {
		if actual := incoming[id] {
			found++
			if actual {
				differences++
				complementary = id
				if differences > 1 { return none }
			}
		}
	}
	if found == incoming.len && differences == 1 { return complementary }
	return none
}

fn (cache &VisibleMutationCache) storage_query_union(key string, result map[string][]int, guards map[u64]bool) ?StorageQueryUnion {
	paths := result.keys()
	for entry_idx, entry in cache.storage_query_results[key] {
		mut proof := StorageQueryUnion{ entry_idx: entry_idx }
		if complementary := storage_query_complementary_guard(entry.guards, guards) {
			proof = StorageQueryUnion{
				entry_idx: entry_idx
				guard_id:  complementary
				incoming:  true
				existing:  entry.guards.present.len + entry.guards.absent.len == guards.len
			}
		} else if complementary := storage_query_existing_complementary_guard(entry.guards,
			guards) {
			proof = StorageQueryUnion{
				entry_idx: entry_idx
				guard_id:  complementary
				existing:  true
			}
		} else {
			continue
		}
		if result.len != entry.writes.len { continue }
		if paths.len == 0 { return proof }
		$if prealloc {
			scope := unsafe { prealloc_scope_begin() }
			equal := storage_query_results_equal(paths, result, entry.writes)
			unsafe { prealloc_scope_end(scope) }
			if equal { return proof }
		} $else {
			if storage_query_results_equal(paths, result, entry.writes) {
				return proof
			}
		}
	}
	return none
}

fn (cache &VisibleMutationCache) storage_query_forward_union(key string, result map[string][]int, mut guards map[u64]bool) int {
	paths := result.keys()
	entries := cache.storage_query_results[key]
	mut matched := -1
	for guards.len > 0 {
		mut selected := -1
		mut guard_id := u64(0)
		for entry_idx, entry in entries {
			complementary := storage_query_complementary_guard(entry.guards, guards) or {
				continue
			}
			if result.len != entry.writes.len { continue }
			if paths.len > 0 {
				$if prealloc {
					scope := unsafe { prealloc_scope_begin() }
					equal := storage_query_results_equal(paths, result, entry.writes)
					unsafe { prealloc_scope_end(scope) }
					if !equal { continue }
				} $else {
					if !storage_query_results_equal(paths, result, entry.writes) {
						continue
					}
				}
			}
			selected = entry_idx
			guard_id = complementary
			break
		}
		if selected < 0 { break }
		// Each proof removes one incoming condition; retained certificates stay unchanged.
		guards.delete(guard_id)
		matched = selected
	}
	return matched
}

fn storage_query_remove_guard(mut ids []u64, guard_id u64) bool {
	for i, id in ids {
		if id != guard_id { continue }
		for j := i + 1; j < ids.len; j++ {
			ids[j - 1] = ids[j]
		}
		// These unique ROOT buffers have no slices; keep their original capacity charged.
		unsafe { ids.len-- }
		return true
	}
	return false
}

fn (mut cache VisibleMutationCache) broaden_storage_query_entry(key string, entry_idx int, guard_id u64) bool {
	// Borrow the existing array: replacing a map value could rehash even an existing key.
	mut entries := unsafe { cache.storage_query_results[key] }
	if entry_idx < 0 || entry_idx >= entries.len { return false }
	entry := entries[entry_idx]
	mut present := unsafe { entry.guards.present }
	mut absent := unsafe { entry.guards.absent }
	if !storage_query_remove_guard(mut present, guard_id)
		&& !storage_query_remove_guard(mut absent, guard_id) {
		return false
	}
	entries[entry_idx] = StorageQueryResult{
		writes: unsafe { entry.writes }
		guards: StorageQueryGuards{ present: unsafe { present }, absent: unsafe { absent } }
	}
	return true
}

fn storage_query_filter_guard_ids(mut ids []u64, guards map[u64]bool) {
	mut retained := 0
	for id in ids {
		if id in guards {
			ids[retained] = id
			retained++
		}
	}
	// The original unique buffer and its full capacity remain allocated and charged.
	unsafe { ids.len = retained }
}

fn (mut cache VisibleMutationCache) broaden_storage_query_entry_to_guards(key string, entry_idx int, guards map[u64]bool) bool {
	mut entries := unsafe { cache.storage_query_results[key] }
	if entry_idx < 0 || entry_idx >= entries.len { return false }
	entry := entries[entry_idx]
	mut present := unsafe { entry.guards.present }
	mut absent := unsafe { entry.guards.absent }
	mut found := 0
	for id in present {
		if expected := guards[id] {
			if !expected { return false }
			found++
		}
	}
	for id in absent {
		if expected := guards[id] {
			if expected { return false }
			found++
		}
	}
	// Validate the complete incoming proof before changing either retained buffer.
	if found != guards.len { return false }
	if found == present.len + absent.len { return true }
	storage_query_filter_guard_ids(mut present, guards)
	storage_query_filter_guard_ids(mut absent, guards)
	entries[entry_idx] = StorageQueryResult{
		writes: unsafe { entry.writes }
		guards: StorageQueryGuards{ present: unsafe { present }, absent: unsafe { absent } }
	}
	return true
}

fn (mut cache VisibleMutationCache) record_storage_query_certificate(guards StorageQueryGuards) {
	for id in guards.present { cache.record_storage_query_guard(id, true) }
	for id in guards.absent { cache.record_storage_query_guard(id, false) }
}

fn (mut cache VisibleMutationCache) record_storage_query_guards(guards map[u64]bool, complete bool) {
	if isnil(cache.storage_query_trace) { return }
	mut trace := cache.storage_query_trace
	trace.complete = trace.complete && complete
	for id, present in guards {
		cache.record_storage_query_guard(id, present)
	}
}

fn (mut cache VisibleMutationCache) record_storage_query_guard(id u64, present bool) {
	if isnil(cache.storage_query_trace) { return }
	mut trace := cache.storage_query_trace
	// This declaration was pushed by the parent, rather than its incoming context.
	if id == trace.guard_id { return }
	if previous := trace.guards[id] {
		if previous != present { trace.complete = false }
	} else {
		trace.guards[id] = present
	}
}

fn clone_storage_query_result(result map[string][]int) map[string][]int {
	mut promoted := map[string][]int{}
	for path, sources in result { promoted[path.clone()] = sources.clone() }
	return promoted
}

fn storage_query_guards_equal(left StorageQueryGuards, right map[u64]bool) bool {
	if left.present.len + left.absent.len != right.len { return false }
	for id in left.present {
		actual := right[id] or { return false }
		if !actual { return false }
	}
	for id in left.absent {
		actual := right[id] or { return false }
		if actual { return false }
	}
	return true
}

fn storage_query_result_bytes(key string, result map[string][]int, guards StorageQueryGuards) int {
	mut bytes := storage_query_entry_bytes(key, guards)
	for path, sources in result { bytes += path.len + sources.len * int(sizeof(int)) + 64 }
	return bytes
}

fn storage_query_entry_bytes(key string, guards StorageQueryGuards) int {
	// The entry includes both array headers; primitive buffers retain their exact lengths.
	return key.len + int(sizeof(StorageQueryResult)) +
		(guards.present.len + guards.absent.len) * int(sizeof(u64))
}

fn storage_query_results_equal(paths []string, result map[string][]int, candidate map[string][]int) bool {
	if paths.len != candidate.len { return false }
	// Storage paths map to deduplicated source sets; traversal order is not provenance.
	for path in paths {
		sources := result[path]
		candidate_sources := candidate[path] or { return false }
		if sources.len != candidate_sources.len { return false }
		for source in sources {
			if source !in candidate_sources { return false }
		}
	}
	return true
}

fn (cache &VisibleMutationCache) storage_query_shared_result(key string, result map[string][]int) ?map[string][]int {
	paths := result.keys()
	for entry in cache.storage_query_results[key] {
		if result.len != entry.writes.len { continue }
		if paths.len == 0 { return entry.writes }
		// Comparisons remain scratch-scoped when many variants share a payload.
		$if prealloc {
			scope := unsafe { prealloc_scope_begin() }
			equal := storage_query_results_equal(paths, result, entry.writes)
			unsafe { prealloc_scope_end(scope) }
			if equal { return entry.writes }
		} $else {
			if storage_query_results_equal(paths, result, entry.writes) {
				return entry.writes
			}
		}
	}
	return none
}

fn (cache &VisibleMutationCache) storage_query_can_admit(estimated_bytes int) bool {
	return cache.storage_query_count < 32768
		&& cache.storage_query_bytes + estimated_bytes <= 64 * 1024 * 1024
}

fn (mut cache VisibleMutationCache) cache_storage_query_result(key string, result map[string][]int, guards map[u64]bool, certificate StorageQueryGuards, clone_result bool, estimated_bytes int) ?map[string][]int {
	if !cache.storage_query_can_admit(estimated_bytes) { return none }
	mut entries := cache.storage_query_results[key] or { []StorageQueryResult{} }
	for entry in entries {
		if storage_query_guards_equal(entry.guards, guards) { return entry.writes }
	}
	writes := if clone_result { clone_storage_query_result(result) } else { result }
	entries << StorageQueryResult{
		writes: writes
		guards: StorageQueryGuards{
			present: certificate.present.clone()
			absent:  certificate.absent.clone()
		}
	}
	cache.storage_query_results[key] = entries
	cache.storage_query_bytes += estimated_bytes
	cache.storage_query_count++
	return writes
}

fn (tc &TypeChecker) fork_storage_query_view() &TypeChecker {
	return tc.fork_storage_query_view_with_annotations(true)
}

fn (tc &TypeChecker) fork_storage_query_view_with_annotations(copy_annotations bool) &TypeChecker {
	mut view := tc.fork_program_view(tc.a, map[int][]SymbolId{})
	view.transform_signature_maps_shared = true
	view.transform_struct_maps_shared = true
	view.type_interner = new_type_interner()
	view.symbols = new_symbol_interner()
	view.fork_overlay = &TransformForkOverlay{
		base_node_count: -1
	}
	if copy_annotations {
		if !isnil(tc.fork_overlay) {
			view.fork_overlay.resolved_call_names = tc.fork_overlay.resolved_call_names.clone()
			view.fork_overlay.resolved_fn_values = tc.fork_overlay.resolved_fn_values.clone()
		}
		view.sparse_resolved_fn_values = tc.sparse_resolved_fn_values.clone()
		view.fork_fn_value_writes = tc.fork_fn_value_writes.clone()
		view.sparse_expr_type_values = tc.sparse_expr_type_values.clone()
		view.sparse_resolved_call_names = tc.sparse_resolved_call_names.clone()
	}
	view.v_fn_semantic_names = tc.v_fn_semantic_names
	view.verbose = tc.verbose
	view.file_scope = tc.file_scope
	view.cur_scope = tc.cur_scope
	view.fn_context = clone_function_check_context(tc.fn_context)
	view.smartcasts = clone_smartcasts(tc.smartcasts)
	view.errors = tc.errors.clone()
	view.type_param_texts = tc.type_param_texts.clone()
	view.type_params_expanding = tc.type_params_expanding.clone()
	view.generic_decl_file = tc.generic_decl_file
	view.cur_fn_ret_type = tc.cur_fn_ret_type
	view.channel_send_or_expr_id = tc.channel_send_or_expr_id
	view.expected_expr_id = tc.expected_expr_id
	view.expected_expr_type = tc.expected_expr_type
	view.parallel_check_sparse = tc.parallel_check_sparse
	view.check_range_lo = tc.check_range_lo
	view.check_range_hi = tc.check_range_hi
	mut base := tc.type_cache
	if !isnil(base) && isnil(base.base) && base.local_fn_decl_indexed_len < tc.a.nodes.len {
		// A base cache freezes the declaration index. Keep cold-query AST scans.
		base = unsafe { nil }
	}
	view.type_cache = new_type_cache_with_base(!isnil(tc.type_cache) && tc.type_cache.parse_enabled,
		base)
	view.visible_mutation_cache = &VisibleMutationCache{
		base:          tc.visible_mutation_cache
		storage_query: true
	}
	return view
}

// Observation keeps the checked cache as a read-only base while new annotations stay private.
// Any request to check a node invalidates the proof before it can change the live checker.
fn (tc &TypeChecker) fork_storage_observation_view() &TypeChecker {
	// The outer observer reads original annotations through read_base; cloning
	// them here would only allocate maps that the private write sets replace.
	mut view := tc.fork_storage_query_view_with_annotations(!isnil(tc.storage_query_probe))
	view.storage_query_probe = if isnil(tc.storage_query_probe) {
		&StorageQueryProbe{
			read_base:              tc
			initial_error_count:    tc.errors.len
			initial_node_count:     tc.a.nodes.len
			initial_children_count: tc.a.children.len
		}
	} else {
		tc.storage_query_probe
	}
	view.parallel_check_sparse = true
	view.check_range_lo = -1
	view.check_range_hi = -1
	view.placeholder_types = tc.placeholder_types.clone()
	if isnil(tc.storage_query_probe) {
		// The base readers retain the caller's precedence and dense readable range.
		// Only annotations made during observation belong in these private maps.
		view.sparse_expr_type_values = map[int]Type{}
		view.sparse_resolved_call_names = map[int]string{}
		view.sparse_statement_nodes = map[int]bool{}
		view.fork_overlay.resolved_call_names = map[int]string{}
		view.sparse_resolved_fn_values = map[int]string{}
		view.fork_fn_value_writes = map[int]string{}
		view.fork_overlay.resolved_fn_values = map[int]string{}
	} else {
		view.sparse_statement_nodes = tc.sparse_statement_nodes.clone()
	}
	$if ownership ? {
		if isnil(tc.storage_query_probe) {
			view.ownership = ownership_clone_state_for_observation(tc.ownership)
		}
	}
	return view
}
