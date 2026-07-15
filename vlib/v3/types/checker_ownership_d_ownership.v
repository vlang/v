module types

import v3.flat
import v3.gen.c.naming

struct MovedVar {
	moved_to      string
	move_pos      flat.NodeId
	is_fn_call    bool
	suggest_clone bool = true
	fn_name       string
	type_name     string = 'string'
}

struct BorrowInfo {
	borrower string
	pos      flat.NodeId
	is_mut   bool
}

struct OwnershipBorrowConflict {
	name   string
	borrow BorrowInfo
}

struct OwnershipMovedConflict {
	name string
	info MovedVar
}

struct OwnershipBorrowerSnapshot {
	var_name string
	borrow   BorrowInfo
}

struct OwnershipKeySnapshot {
	name      string
	had_owned bool
	owned_pos flat.NodeId
	had_type  bool
	type_name string
	had_moved bool
	moved     MovedVar
}

struct OwnershipReturnParamSlot {
	param_idx int
	slot_idx  int
}

struct OwnershipReturnDescendant {
	slot_idx  int
	suffix    string
	type_name string
}

struct OwnershipReturnParamDescendant {
	param_idx     int
	slot_idx      int
	source_suffix string
	target_suffix string
}

struct OwnershipReturnParamArg {
	arg_id        flat.NodeId
	source_suffix string
}

struct OwnershipCallProjection {
	call_id flat.NodeId
	suffix  string
}

struct OwnershipConditionalMoveSource {
	source        string
	target_suffix string
}

struct OwnershipArrayShiftEntry {
	old_name  string
	new_name  string
	had_owned bool
	owned_pos flat.NodeId
	had_type  bool
	type_name string
	had_moved bool
	moved     MovedVar
}

struct OwnershipParamDescendant {
	param_idx int
	suffix    string
	type_name string
}

struct OwnershipCaptureBinding {
	name      string
	type_name string
	pos       flat.NodeId
}

// OwnershipDropEntry identifies one still-owned local whose concrete type
// implements Drop, or an optional/result wrapper whose payload does. Code
// generators consume these snapshots at lexical and function exits.
pub struct OwnershipDropEntry {
pub:
	name             string
	type_name        string
	optional_wrapper bool
}

struct OwnershipDropCandidate {
	name string
	pos  int
}

struct OwnershipDropTarget {
	type_name        string
	optional_wrapper bool
}

struct OwnershipMethodValueReceiverResult {
	consumed    bool
	borrow_name string
}

struct OwnershipFnScanItem {
	idx    int
	file   string
	module string
	name   string
}

struct OwnershipFrame {
	cur_fn          string
	owned_vars      map[string]flat.NodeId
	owned_var_types map[string]string
	moved_vars      map[string]MovedVar
	borrowed_vars   map[string][]BorrowInfo
	array_lengths   map[string]int
	fn_value_vars   map[string]string
	scope_frames    []OwnershipScopeFrame
	path_active     bool
}

struct OwnershipBranchGroup {
mut:
	base          OwnershipFrame
	branches      []OwnershipFrame
	continues     []OwnershipFrame
	saw_else      bool
	is_loop       bool
	value_context bool
	label         string
}

struct OwnershipNameSnapshot {
	had_owned bool
	owned_pos flat.NodeId
	had_type  bool
	type_name string
	had_moved bool
	moved     MovedVar
	borrows   []OwnershipBorrowerSnapshot
	children  []OwnershipKeySnapshot
	had_fn    bool
	fn_name   string
}

struct OwnershipScopeFrame {
mut:
	cur_fn      string
	is_fn_scope bool
	names       map[string]OwnershipNameSnapshot
	decl_order  []string
	defer_stmts []flat.NodeId
	scope_id    flat.NodeId = flat.NodeId(-1)
}

struct OwnershipState {
mut:
	owned_vars                       map[string]flat.NodeId
	owned_var_types                  map[string]string
	moved_vars                       map[string]MovedVar
	borrowed_vars                    map[string][]BorrowInfo
	ownership_fns                    map[string]bool
	ownership_fn_params              map[string]bool
	ownership_fn_returns_param       map[string][]int
	ownership_fn_return_params       map[string][]OwnershipReturnParamSlot
	ownership_fn_return_slots        map[string][]int
	ownership_fn_return_descs        map[string][]OwnershipReturnDescendant
	ownership_fn_return_param_descs  map[string][]OwnershipReturnParamDescendant
	ownership_fn_return_fn_values    map[string]string
	ownership_fn_literal_ret_types   map[string]Type
	ownership_fn_literal_param_types map[string][]Type
	ownership_fn_param_mut           map[string][]bool
	ownership_fn_param_descs         map[string][]OwnershipParamDescendant
	ownership_fn_param_desc_count    int
	owned_structs                    map[string]bool
	copy_structs                     map[string]bool
	drop_structs                     map[string]bool
	drop_at_fn_exit                  map[string][]OwnershipDropEntry
	drop_at_returns                  map[string][]OwnershipDropEntry
	drop_at_return_nodes             map[string][]OwnershipDropEntry
	drop_at_propagations             map[string][]OwnershipDropEntry
	drop_at_loop_controls            map[string][]OwnershipDropEntry
	drop_at_loop_iterations          map[string][]OwnershipDropEntry
	drop_at_scope_exit               map[string][]OwnershipDropEntry
	drop_return_counts               map[string]int
	drop_propagation_counts          map[string]int
	drop_loop_control_counts         map[string]int
	drop_loop_iteration_counts       map[string]int
	drop_scope_counts                map[string]int
	drop_type_names                  map[string]bool
	value_receiver_methods           map[string]bool
	owned_globals                    map[string]string
	array_lengths                    map[string]int
	ownership_fn_value_vars          map[string]string
	cur_fn                           string
	frames                           []OwnershipFrame
	branch_groups                    []OwnershipBranchGroup
	pending_value_branch_groups      []OwnershipBranchGroup
	pending_loop_label               string
	deferred_aggregate_consumption   map[int]int
	scope_frames                     []OwnershipScopeFrame
	suppressed_checks                int
	path_active                      bool
}

fn ownership_clone_name_snapshots(names map[string]OwnershipNameSnapshot) map[string]OwnershipNameSnapshot {
	mut cloned := map[string]OwnershipNameSnapshot{}
	for name, snap in names {
		cloned[name] = OwnershipNameSnapshot{
			had_owned: snap.had_owned
			owned_pos: snap.owned_pos
			had_type:  snap.had_type
			type_name: snap.type_name
			had_moved: snap.had_moved
			moved:     snap.moved
			borrows:   snap.borrows.clone()
			children:  snap.children.clone()
			had_fn:    snap.had_fn
			fn_name:   snap.fn_name
		}
	}
	return cloned
}

fn ownership_clone_scope_frames(frames []OwnershipScopeFrame) []OwnershipScopeFrame {
	mut cloned := []OwnershipScopeFrame{cap: frames.len}
	for frame in frames {
		cloned << OwnershipScopeFrame{
			cur_fn:      frame.cur_fn
			is_fn_scope: frame.is_fn_scope
			names:       ownership_clone_name_snapshots(frame.names)
			decl_order:  frame.decl_order.clone()
			defer_stmts: frame.defer_stmts.clone()
			scope_id:    frame.scope_id
		}
	}
	return cloned
}

fn new_ownership_state() &OwnershipState {
	return &OwnershipState{
		owned_vars:                       map[string]flat.NodeId{}
		owned_var_types:                  map[string]string{}
		moved_vars:                       map[string]MovedVar{}
		borrowed_vars:                    map[string][]BorrowInfo{}
		ownership_fns:                    map[string]bool{}
		ownership_fn_params:              map[string]bool{}
		ownership_fn_returns_param:       map[string][]int{}
		ownership_fn_return_params:       map[string][]OwnershipReturnParamSlot{}
		ownership_fn_return_slots:        map[string][]int{}
		ownership_fn_return_descs:        map[string][]OwnershipReturnDescendant{}
		ownership_fn_return_param_descs:  map[string][]OwnershipReturnParamDescendant{}
		ownership_fn_return_fn_values:    map[string]string{}
		ownership_fn_literal_ret_types:   map[string]Type{}
		ownership_fn_literal_param_types: map[string][]Type{}
		ownership_fn_param_mut:           map[string][]bool{}
		ownership_fn_param_descs:         map[string][]OwnershipParamDescendant{}
		ownership_fn_param_desc_count:    0
		owned_structs:                    map[string]bool{}
		copy_structs:                     map[string]bool{}
		drop_structs:                     map[string]bool{}
		drop_at_fn_exit:                  map[string][]OwnershipDropEntry{}
		drop_at_returns:                  map[string][]OwnershipDropEntry{}
		drop_at_return_nodes:             map[string][]OwnershipDropEntry{}
		drop_at_propagations:             map[string][]OwnershipDropEntry{}
		drop_at_loop_controls:            map[string][]OwnershipDropEntry{}
		drop_at_loop_iterations:          map[string][]OwnershipDropEntry{}
		drop_at_scope_exit:               map[string][]OwnershipDropEntry{}
		drop_return_counts:               map[string]int{}
		drop_propagation_counts:          map[string]int{}
		drop_loop_control_counts:         map[string]int{}
		drop_loop_iteration_counts:       map[string]int{}
		drop_scope_counts:                map[string]int{}
		drop_type_names:                  map[string]bool{}
		value_receiver_methods:           map[string]bool{}
		owned_globals:                    map[string]string{}
		array_lengths:                    map[string]int{}
		ownership_fn_value_vars:          map[string]string{}
		frames:                           []OwnershipFrame{}
		branch_groups:                    []OwnershipBranchGroup{}
		pending_value_branch_groups:      []OwnershipBranchGroup{}
		pending_loop_label:               ''
		deferred_aggregate_consumption:   map[int]int{}
		scope_frames:                     []OwnershipScopeFrame{}
		suppressed_checks:                0
		path_active:                      true
	}
}

fn (mut st OwnershipState) mark_fn_return_owned(fn_name string) {
	if fn_name.len == 0 || st.ownership_fns[fn_name] {
		return
	}
	st.ownership_fns[fn_name] = true
}

fn ownership_clone_int_lists(src map[string][]int) map[string][]int {
	mut out := map[string][]int{}
	for key, values in src {
		out[key] = values.clone()
	}
	return out
}

fn ownership_clone_return_param_slots(src map[string][]OwnershipReturnParamSlot) map[string][]OwnershipReturnParamSlot {
	mut out := map[string][]OwnershipReturnParamSlot{}
	for key, values in src {
		out[key] = values.clone()
	}
	return out
}

fn ownership_clone_return_descs(src map[string][]OwnershipReturnDescendant) map[string][]OwnershipReturnDescendant {
	mut out := map[string][]OwnershipReturnDescendant{}
	for key, values in src {
		out[key] = values.clone()
	}
	return out
}

fn ownership_clone_return_param_descs(src map[string][]OwnershipReturnParamDescendant) map[string][]OwnershipReturnParamDescendant {
	mut out := map[string][]OwnershipReturnParamDescendant{}
	for key, values in src {
		out[key] = values.clone()
	}
	return out
}

fn ownership_clone_param_descs(src map[string][]OwnershipParamDescendant) map[string][]OwnershipParamDescendant {
	mut out := map[string][]OwnershipParamDescendant{}
	for key, values in src {
		out[key] = values.clone()
	}
	return out
}

fn ownership_clone_bool_lists(src map[string][]bool) map[string][]bool {
	mut out := map[string][]bool{}
	for key, values in src {
		out[key] = values.clone()
	}
	return out
}

fn ownership_clone_state_for_parallel(src &OwnershipState) &OwnershipState {
	return &OwnershipState{
		owned_vars:                       map[string]flat.NodeId{}
		owned_var_types:                  map[string]string{}
		moved_vars:                       map[string]MovedVar{}
		borrowed_vars:                    map[string][]BorrowInfo{}
		ownership_fns:                    src.ownership_fns.clone()
		ownership_fn_params:              src.ownership_fn_params.clone()
		ownership_fn_returns_param:       ownership_clone_int_lists(src.ownership_fn_returns_param)
		ownership_fn_return_params:       ownership_clone_return_param_slots(src.ownership_fn_return_params)
		ownership_fn_return_slots:        ownership_clone_int_lists(src.ownership_fn_return_slots)
		ownership_fn_return_descs:        ownership_clone_return_descs(src.ownership_fn_return_descs)
		ownership_fn_return_param_descs:  ownership_clone_return_param_descs(src.ownership_fn_return_param_descs)
		ownership_fn_return_fn_values:    src.ownership_fn_return_fn_values.clone()
		ownership_fn_literal_ret_types:   map[string]Type{}
		ownership_fn_literal_param_types: map[string][]Type{}
		ownership_fn_param_mut:           ownership_clone_bool_lists(src.ownership_fn_param_mut)
		ownership_fn_param_descs:         ownership_clone_param_descs(src.ownership_fn_param_descs)
		ownership_fn_param_desc_count:    src.ownership_fn_param_desc_count
		owned_structs:                    src.owned_structs.clone()
		copy_structs:                     src.copy_structs.clone()
		drop_structs:                     src.drop_structs.clone()
		drop_at_fn_exit:                  map[string][]OwnershipDropEntry{}
		drop_at_returns:                  map[string][]OwnershipDropEntry{}
		drop_at_return_nodes:             map[string][]OwnershipDropEntry{}
		drop_at_propagations:             map[string][]OwnershipDropEntry{}
		drop_at_loop_controls:            map[string][]OwnershipDropEntry{}
		drop_at_loop_iterations:          map[string][]OwnershipDropEntry{}
		drop_at_scope_exit:               map[string][]OwnershipDropEntry{}
		drop_return_counts:               map[string]int{}
		drop_propagation_counts:          map[string]int{}
		drop_loop_control_counts:         map[string]int{}
		drop_loop_iteration_counts:       map[string]int{}
		drop_scope_counts:                map[string]int{}
		drop_type_names:                  map[string]bool{}
		value_receiver_methods:           src.value_receiver_methods.clone()
		owned_globals:                    src.owned_globals.clone()
		array_lengths:                    map[string]int{}
		ownership_fn_value_vars:          map[string]string{}
		frames:                           []OwnershipFrame{}
		branch_groups:                    []OwnershipBranchGroup{}
		pending_value_branch_groups:      []OwnershipBranchGroup{}
		pending_loop_label:               ''
		deferred_aggregate_consumption:   map[int]int{}
		scope_frames:                     []OwnershipScopeFrame{}
		suppressed_checks:                0
		path_active:                      true
	}
}

fn (mut tc TypeChecker) ownership_fork_for_parallel_check(src &TypeChecker) {
	if src.ownership == unsafe { nil } {
		tc.ownership = unsafe { nil }
		return
	}
	tc.ownership = ownership_clone_state_for_parallel(src.ownership)
}

fn ownership_merge_bool_map(mut dst map[string]bool, src map[string]bool) {
	for key, value in src {
		if value {
			dst[key] = true
		}
	}
}

fn ownership_merge_string_map(mut dst map[string]string, src map[string]string) {
	for key, value in src {
		if key !in dst {
			dst[key] = value
		}
	}
}

fn ownership_merge_int_lists(mut dst map[string][]int, src map[string][]int) {
	for key, values in src {
		mut merged := dst[key] or { []int{} }
		for value in values {
			if value !in merged {
				merged << value
			}
		}
		dst[key] = merged
	}
}

fn ownership_return_param_slot_in(values []OwnershipReturnParamSlot, needle OwnershipReturnParamSlot) bool {
	for value in values {
		if value.param_idx == needle.param_idx && value.slot_idx == needle.slot_idx {
			return true
		}
	}
	return false
}

fn ownership_merge_return_param_slots(mut dst map[string][]OwnershipReturnParamSlot, src map[string][]OwnershipReturnParamSlot) {
	for key, values in src {
		mut merged := dst[key] or { []OwnershipReturnParamSlot{} }
		for value in values {
			if !ownership_return_param_slot_in(merged, value) {
				merged << value
			}
		}
		dst[key] = merged
	}
}

fn ownership_return_desc_in(values []OwnershipReturnDescendant, needle OwnershipReturnDescendant) bool {
	for value in values {
		if value.slot_idx == needle.slot_idx && value.suffix == needle.suffix {
			return true
		}
	}
	return false
}

fn ownership_merge_return_descs(mut dst map[string][]OwnershipReturnDescendant, src map[string][]OwnershipReturnDescendant) {
	for key, values in src {
		mut merged := dst[key] or { []OwnershipReturnDescendant{} }
		for value in values {
			if !ownership_return_desc_in(merged, value) {
				merged << value
			}
		}
		dst[key] = merged
	}
}

fn ownership_return_param_desc_in(values []OwnershipReturnParamDescendant, needle OwnershipReturnParamDescendant) bool {
	for value in values {
		if value.param_idx == needle.param_idx && value.slot_idx == needle.slot_idx
			&& value.source_suffix == needle.source_suffix
			&& value.target_suffix == needle.target_suffix {
			return true
		}
	}
	return false
}

fn ownership_merge_return_param_descs(mut dst map[string][]OwnershipReturnParamDescendant, src map[string][]OwnershipReturnParamDescendant) {
	for key, values in src {
		mut merged := dst[key] or { []OwnershipReturnParamDescendant{} }
		for value in values {
			if !ownership_return_param_desc_in(merged, value) {
				merged << value
			}
		}
		dst[key] = merged
	}
}

fn ownership_param_desc_in(values []OwnershipParamDescendant, needle OwnershipParamDescendant) bool {
	for value in values {
		if value.param_idx == needle.param_idx && value.suffix == needle.suffix {
			return true
		}
	}
	return false
}

fn ownership_merge_param_descs(mut dst map[string][]OwnershipParamDescendant, src map[string][]OwnershipParamDescendant) int {
	mut added := 0
	for key, values in src {
		mut merged := dst[key] or { []OwnershipParamDescendant{} }
		for value in values {
			if !ownership_param_desc_in(merged, value) {
				merged << value
				added++
			}
		}
		dst[key] = merged
	}
	return added
}

fn ownership_merge_bool_lists(mut dst map[string][]bool, src map[string][]bool) {
	for key, values in src {
		if key !in dst {
			dst[key] = values.clone()
		}
	}
}

fn ownership_merge_type_map(mut dst map[string]Type, src map[string]Type) {
	for key, value in src {
		if key !in dst {
			dst[key] = value
		}
	}
}

fn ownership_merge_type_lists(mut dst map[string][]Type, src map[string][]Type) {
	for key, values in src {
		if key !in dst {
			dst[key] = values.clone()
		}
	}
}

fn ownership_merge_fn_value_returns(mut dst map[string]string, src map[string]string) {
	for key, value in src {
		if existing := dst[key] {
			if existing.len > 0 && value.len > 0 && existing != value {
				dst[key] = ''
			}
		} else {
			dst[key] = value
		}
	}
}

fn ownership_merge_drop_lists_by_name(mut dst map[string][]OwnershipDropEntry, src map[string][]OwnershipDropEntry) {
	for key, entries in src {
		dst[key] = entries.clone()
	}
}

fn ownership_merge_drop_entries(existing []OwnershipDropEntry, extra []OwnershipDropEntry) []OwnershipDropEntry {
	mut merged := existing.clone()
	mut names := map[string]bool{}
	for entry in merged {
		names[entry.name] = true
	}
	for entry in extra {
		if entry.name !in names {
			merged << entry
			names[entry.name] = true
		}
	}
	return merged
}

fn (mut tc TypeChecker) ownership_merge_parallel_check_worker(w &TypeChecker) {
	if tc.ownership == unsafe { nil } || w.ownership == unsafe { nil } {
		return
	}
	mut dst := tc.ownership
	src := w.ownership
	ownership_merge_bool_map(mut dst.ownership_fns, src.ownership_fns)
	ownership_merge_bool_map(mut dst.ownership_fn_params, src.ownership_fn_params)
	ownership_merge_int_lists(mut dst.ownership_fn_returns_param, src.ownership_fn_returns_param)
	ownership_merge_return_param_slots(mut dst.ownership_fn_return_params,
		src.ownership_fn_return_params)
	ownership_merge_int_lists(mut dst.ownership_fn_return_slots, src.ownership_fn_return_slots)
	ownership_merge_return_descs(mut dst.ownership_fn_return_descs, src.ownership_fn_return_descs)
	ownership_merge_return_param_descs(mut dst.ownership_fn_return_param_descs,
		src.ownership_fn_return_param_descs)
	ownership_merge_fn_value_returns(mut dst.ownership_fn_return_fn_values,
		src.ownership_fn_return_fn_values)
	ownership_merge_type_map(mut dst.ownership_fn_literal_ret_types,
		src.ownership_fn_literal_ret_types)
	ownership_merge_type_lists(mut dst.ownership_fn_literal_param_types,
		src.ownership_fn_literal_param_types)
	ownership_merge_bool_lists(mut dst.ownership_fn_param_mut, src.ownership_fn_param_mut)
	dst.ownership_fn_param_desc_count += ownership_merge_param_descs(mut dst.ownership_fn_param_descs,
		src.ownership_fn_param_descs)
	ownership_merge_bool_map(mut dst.owned_structs, src.owned_structs)
	ownership_merge_bool_map(mut dst.copy_structs, src.copy_structs)
	ownership_merge_bool_map(mut dst.drop_structs, src.drop_structs)
	ownership_merge_drop_lists_by_name(mut dst.drop_at_fn_exit, src.drop_at_fn_exit)
	ownership_merge_drop_lists_by_name(mut dst.drop_at_returns, src.drop_at_returns)
	ownership_merge_drop_lists_by_name(mut dst.drop_at_return_nodes, src.drop_at_return_nodes)
	ownership_merge_drop_lists_by_name(mut dst.drop_at_propagations, src.drop_at_propagations)
	ownership_merge_drop_lists_by_name(mut dst.drop_at_loop_controls, src.drop_at_loop_controls)
	ownership_merge_drop_lists_by_name(mut dst.drop_at_loop_iterations, src.drop_at_loop_iterations)
	ownership_merge_drop_lists_by_name(mut dst.drop_at_scope_exit, src.drop_at_scope_exit)
	ownership_merge_bool_map(mut dst.drop_type_names, src.drop_type_names)
	ownership_merge_bool_map(mut dst.value_receiver_methods, src.value_receiver_methods)
	ownership_merge_string_map(mut dst.owned_globals, src.owned_globals)
}

fn (mut tc TypeChecker) ownership_state() &OwnershipState {
	if tc.ownership == unsafe { nil } {
		tc.ownership = new_ownership_state()
	}
	return tc.ownership
}

fn (tc &TypeChecker) ownership_checks_suppressed() bool {
	return tc.ownership != unsafe { nil } && tc.ownership.suppressed_checks > 0
}

fn (tc &TypeChecker) ownership_effects_disabled() bool {
	return tc.ownership_checks_suppressed()
		|| (tc.ownership != unsafe { nil } && !tc.ownership.path_active)
}

fn (mut tc TypeChecker) ownership_begin_suppressed_checks() {
	mut st := tc.ownership_state()
	st.suppressed_checks++
}

fn (mut tc TypeChecker) ownership_end_suppressed_checks() {
	mut st := tc.ownership_state()
	if st.suppressed_checks > 0 {
		st.suppressed_checks--
	}
}

fn (mut tc TypeChecker) ownership_should_defer_aggregate_consumption(lhs_id flat.NodeId, op flat.Op) bool {
	if op != .assign {
		return false
	}
	lhs_name := tc.ownership_lhs_name(lhs_id)
	return lhs_name.len > 0 && lhs_name != '_'
}

fn (mut tc TypeChecker) ownership_begin_defer_aggregate_consumption(rhs_id flat.NodeId) {
	mut ids := []flat.NodeId{}
	tc.ownership_collect_deferred_aggregate_ids(rhs_id, mut ids)
	if ids.len == 0 {
		return
	}
	mut st := tc.ownership_state()
	for id in ids {
		key := int(id)
		st.deferred_aggregate_consumption[key] =
			(st.deferred_aggregate_consumption[key] or { 0 }) + 1
	}
}

fn (mut tc TypeChecker) ownership_end_defer_aggregate_consumption(rhs_id flat.NodeId) {
	mut ids := []flat.NodeId{}
	tc.ownership_collect_deferred_aggregate_ids(rhs_id, mut ids)
	if ids.len == 0 {
		return
	}
	mut st := tc.ownership_state()
	for id in ids {
		key := int(id)
		count := st.deferred_aggregate_consumption[key] or { 0 }
		if count <= 1 {
			st.deferred_aggregate_consumption.delete(key)
		} else {
			st.deferred_aggregate_consumption[key] = count - 1
		}
	}
}

fn (mut tc TypeChecker) ownership_check_node_with_deferred_aggregate_consumption(expr_id flat.NodeId) {
	tc.ownership_begin_defer_aggregate_consumption(expr_id)
	tc.check_node(expr_id)
	tc.ownership_end_defer_aggregate_consumption(expr_id)
}

fn (mut tc TypeChecker) ownership_check_node_with_aggregate_consumption_mode(expr_id flat.NodeId, should_defer bool) {
	if should_defer {
		tc.ownership_check_node_with_deferred_aggregate_consumption(expr_id)
		return
	}
	tc.check_node(expr_id)
}

fn (mut tc TypeChecker) ownership_should_defer_call_arg_aggregate_consumption(node flat.Node, info CallInfo, child_idx int) bool {
	if child_idx <= 0 || child_idx >= node.children_count {
		return false
	}
	if tc.a.child_node(&node, child_idx).kind == .field_init {
		return true
	}
	param_idx := tc.ownership_call_arg_decl_param_idx(info, child_idx)
	type_param_idx := param_idx + tc.ownership_call_arg_shift(node, info)
	variadic_elem_idx := tc.ownership_call_arg_variadic_elem_idx(info, type_param_idx)
	expected := tc.ownership_call_arg_expected_type(info, type_param_idx, variadic_elem_idx)
	return expected !is Void && expected !is Pointer
}

fn (tc &TypeChecker) ownership_aggregate_consumption_deferred(id flat.NodeId) bool {
	if tc.ownership == unsafe { nil } {
		return false
	}
	return int(id) in tc.ownership.deferred_aggregate_consumption
}

fn (tc &TypeChecker) ownership_collect_deferred_aggregate_ids(expr_id flat.NodeId, mut ids []flat.NodeId) {
	id := tc.ownership_unwrap_expr(expr_id)
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.array_literal, .array_init, .map_init, .struct_init, .assoc {
			ids << id
			tc.ownership_collect_deferred_aggregate_child_ids(node, mut ids)
		}
		.if_expr {
			if node.children_count > 1 {
				tc.ownership_collect_deferred_aggregate_ids(tc.branch_tail_expr_id(tc.a.child(&node,
					1)), mut ids)
			}
			if node.children_count > 2 {
				tc.ownership_collect_deferred_aggregate_ids(tc.branch_tail_expr_id(tc.a.child(&node,
					2)), mut ids)
			}
		}
		.match_stmt {
			for i in 1 .. node.children_count {
				branch_id := tc.a.child(&node, i)
				if !tc.valid_node_id(branch_id) {
					continue
				}
				if tc.a.nodes[int(branch_id)].kind == .match_branch {
					tc.ownership_collect_deferred_aggregate_ids(tc.branch_tail_expr_id(branch_id), mut
						ids)
				}
			}
		}
		.or_expr {
			if node.children_count >= 2 && node.value !in ['!', '?'] {
				tc.ownership_collect_deferred_aggregate_ids(tc.branch_tail_expr_id(tc.a.child(&node,
					1)), mut ids)
			}
		}
		else {}
	}
}

fn (tc &TypeChecker) ownership_collect_deferred_aggregate_child_ids(node flat.Node, mut ids []flat.NodeId) {
	match node.kind {
		.array_literal, .map_init {
			for i in 0 .. node.children_count {
				tc.ownership_collect_deferred_aggregate_ids(tc.a.child(&node, i), mut ids)
			}
		}
		.array_init, .struct_init, .assoc {
			for i in 0 .. node.children_count {
				child_id := tc.a.child(&node, i)
				child := tc.a.nodes[int(child_id)]
				if child.kind == .field_init && child.children_count > 0 {
					tc.ownership_collect_deferred_aggregate_ids(tc.a.child(&child, 0), mut ids)
				} else if child.kind != .field_init {
					tc.ownership_collect_deferred_aggregate_ids(child_id, mut ids)
				}
			}
		}
		else {}
	}
}

fn (mut tc TypeChecker) ownership_reset() {
	tc.ownership = new_ownership_state()
}

fn (mut tc TypeChecker) ownership_push_scope() {
	if tc.ownership_checks_suppressed() {
		return
	}
	mut st := tc.ownership_state()
	mut is_fn_scope := st.cur_fn.len > 0
	for i := st.scope_frames.len; i > 0; i-- {
		if st.scope_frames[i - 1].cur_fn == st.cur_fn {
			is_fn_scope = false
			break
		}
	}
	st.scope_frames << OwnershipScopeFrame{
		cur_fn:      st.cur_fn
		is_fn_scope: is_fn_scope
		names:       map[string]OwnershipNameSnapshot{}
		decl_order:  []string{}
		defer_stmts: []flat.NodeId{}
	}
}

fn (mut tc TypeChecker) ownership_mark_scope_node(id flat.NodeId) {
	if tc.ownership_checks_suppressed() {
		return
	}
	mut st := tc.ownership_state()
	if st.scope_frames.len > 0 {
		st.scope_frames[st.scope_frames.len - 1].scope_id = id
	}
}

fn (mut tc TypeChecker) ownership_check_defer_stmt(id flat.NodeId, node flat.Node) {
	if tc.ownership_effects_disabled() {
		tc.ownership_check_defer_body(node)
		return
	}
	frame := tc.ownership_snapshot_frame()
	tc.ownership_begin_suppressed_checks()
	tc.ownership_check_defer_body(node)
	tc.ownership_end_suppressed_checks()
	tc.ownership_restore_frame(frame)
	tc.ownership_register_defer_stmt(id)
}

fn (mut tc TypeChecker) ownership_register_defer_stmt(id flat.NodeId) {
	if tc.ownership_checks_suppressed() {
		return
	}
	defer_is_function := tc.valid_node_id(id) && tc.a.nodes[int(id)].value == 'function'
	mut st := tc.ownership_state()
	if st.scope_frames.len == 0 {
		return
	}
	mut scope_idx := st.scope_frames.len - 1
	if defer_is_function && st.cur_fn.len > 0 {
		for i := st.scope_frames.len; i > 0; i-- {
			idx := i - 1
			if st.scope_frames[idx].cur_fn != st.cur_fn {
				break
			}
			scope_idx = idx
		}
	}
	st.scope_frames[scope_idx].defer_stmts << id
}

fn (mut tc TypeChecker) ownership_run_scope_defers() {
	if tc.ownership_checks_suppressed() {
		return
	}
	mut defer_stmts := []flat.NodeId{}
	{
		mut st := tc.ownership_state()
		if st.scope_frames.len == 0 {
			return
		}
		scope_idx := st.scope_frames.len - 1
		defer_stmts = st.scope_frames[scope_idx].defer_stmts.clone()
		st.scope_frames[scope_idx].defer_stmts = []flat.NodeId{}
	}
	for i := defer_stmts.len; i > 0; i-- {
		defer_id := defer_stmts[i - 1]
		if !tc.valid_node_id(defer_id) {
			continue
		}
		defer_node := tc.a.nodes[int(defer_id)]
		tc.ownership_check_defer_body(defer_node)
	}
}

fn (mut tc TypeChecker) ownership_check_return_defers() {
	if tc.ownership_checks_suppressed() {
		return
	}
	st := tc.ownership_state()
	cur_fn := st.cur_fn
	if cur_fn.len == 0 {
		return
	}
	mut defer_stmts := []flat.NodeId{}
	for scope_idx := st.scope_frames.len; scope_idx > 0; scope_idx-- {
		scope := st.scope_frames[scope_idx - 1]
		if scope.cur_fn != cur_fn {
			continue
		}
		for i := scope.defer_stmts.len; i > 0; i-- {
			defer_stmts << scope.defer_stmts[i - 1]
		}
	}
	if defer_stmts.len == 0 {
		return
	}
	frame := tc.ownership_snapshot_frame()
	for defer_id in defer_stmts {
		if !tc.valid_node_id(defer_id) {
			continue
		}
		defer_node := tc.a.nodes[int(defer_id)]
		tc.ownership_check_defer_body(defer_node)
	}
	tc.ownership_restore_frame(frame)
}

fn (mut tc TypeChecker) ownership_check_defer_body(node flat.Node) {
	for i in 0 .. node.children_count {
		tc.check_node(tc.a.child(&node, i))
	}
}

fn (mut tc TypeChecker) ownership_pop_scope() {
	if tc.ownership_checks_suppressed() {
		return
	}
	mut frame := OwnershipScopeFrame{}
	{
		mut st := tc.ownership_state()
		if st.scope_frames.len == 0 {
			return
		}
		frame = st.scope_frames.pop()
	}
	if frame.is_fn_scope {
		entries := tc.ownership_live_drop_entries()
		if entries.len > 0 {
			mut st := tc.ownership_state()
			st.drop_at_fn_exit[frame.cur_fn] = entries
			tc.ownership_note_drop_types(frame.cur_fn, entries)
		}
	} else {
		tc.ownership_record_scope_drops(frame)
	}
	for name, snap in frame.names {
		tc.ownership_release_borrower(name)
		mut st := tc.ownership_state()
		mut saved_children := map[string]OwnershipKeySnapshot{}
		for child in snap.children {
			saved_children[child.name] = child
		}
		mut child_names := tc.ownership_child_state_keys(name)
		for child in snap.children {
			if child.name !in child_names {
				child_names << child.name
			}
		}
		child_names.sort()
		for child_name in child_names {
			child := saved_children[child_name] or {
				st.owned_vars.delete(child_name)
				st.owned_var_types.delete(child_name)
				st.moved_vars.delete(child_name)
				continue
			}
			if child.had_owned {
				st.owned_vars[child_name] = child.owned_pos
			} else {
				st.owned_vars.delete(child_name)
			}
			if child.had_type {
				st.owned_var_types[child_name] = child.type_name
			} else {
				st.owned_var_types.delete(child_name)
			}
			if child.had_moved {
				st.moved_vars[child_name] = child.moved
			} else {
				st.moved_vars.delete(child_name)
			}
		}
		if snap.had_owned {
			st.owned_vars[name] = snap.owned_pos
		} else {
			st.owned_vars.delete(name)
		}
		if snap.had_type {
			st.owned_var_types[name] = snap.type_name
		} else {
			st.owned_var_types.delete(name)
		}
		if snap.had_moved {
			st.moved_vars[name] = snap.moved
		} else {
			st.moved_vars.delete(name)
		}
		if snap.had_fn {
			st.ownership_fn_value_vars[name] = snap.fn_name
		} else {
			st.ownership_fn_value_vars.delete(name)
		}
		for saved in snap.borrows {
			mut borrows := st.borrowed_vars[saved.var_name] or { []BorrowInfo{} }
			borrows << saved.borrow
			st.borrowed_vars[saved.var_name] = borrows
		}
	}
}

fn (mut tc TypeChecker) ownership_record_scope_drops(frame OwnershipScopeFrame) {
	if int(frame.scope_id) < 0 || frame.cur_fn.len == 0 {
		return
	}
	mut st := tc.ownership_state()
	index := st.drop_scope_counts[frame.cur_fn] or { 0 }
	st.drop_scope_counts[frame.cur_fn] = index + 1
	mut entries := []OwnershipDropEntry{}
	for i := frame.decl_order.len; i > 0; i-- {
		name := frame.decl_order[i - 1]
		if entry := tc.ownership_live_drop_entry(name) {
			entries << entry
		}
	}
	if entries.len > 0 {
		st.drop_at_scope_exit['${frame.cur_fn}\x01${index}'] = entries
		tc.ownership_note_drop_types(frame.cur_fn, entries)
	}
}

fn (mut tc TypeChecker) ownership_live_drop_entry(name string) ?OwnershipDropEntry {
	if name.len == 0 || name.contains('.') || name.contains('[') {
		return none
	}
	st := tc.ownership_state()
	if name !in st.owned_vars || name in st.moved_vars {
		return none
	}
	type_name := st.owned_var_types[name] or { return none }
	target := tc.ownership_drop_target_for_type_name(type_name) or { return none }
	return OwnershipDropEntry{
		name:             name
		type_name:        target.type_name
		optional_wrapper: target.optional_wrapper
	}
}

fn (tc &TypeChecker) ownership_type_name_has_drop(type_name string) bool {
	if _ := tc.ownership_drop_target_for_type_name(type_name) {
		return true
	}
	return false
}

fn (tc &TypeChecker) ownership_drop_target_for_type_name(type_name string) ?OwnershipDropTarget {
	if tc.ownership == unsafe { nil } || type_name.len == 0 {
		return none
	}
	clean := type_name.trim_left('&')
	if clean.len > 1 && (clean[0] == `?` || clean[0] == `!`) {
		payload := clean[1..]
		if target := tc.ownership_drop_target_for_direct_type_name(payload, true) {
			return target
		}
		return none
	}
	if target := tc.ownership_drop_target_for_direct_type_name(clean, false) {
		return target
	}
	return none
}

fn (tc &TypeChecker) ownership_drop_target_for_direct_type_name(type_name string, optional_wrapper bool) ?OwnershipDropTarget {
	if tc.ownership_type_name_has_direct_drop(type_name) {
		return OwnershipDropTarget{
			type_name:        type_name
			optional_wrapper: optional_wrapper
		}
	}
	return tc.ownership_drop_target_for_resolved_type(tc.parse_type(type_name), optional_wrapper)
}

fn (tc &TypeChecker) ownership_drop_target_for_resolved_type(typ Type, optional_wrapper bool) ?OwnershipDropTarget {
	if typ is Alias {
		return tc.ownership_drop_target_for_resolved_type(typ.base_type, optional_wrapper)
	}
	if typ is OptionType {
		return tc.ownership_drop_target_for_resolved_type(typ.base_type, true)
	}
	if typ is ResultType {
		return tc.ownership_drop_target_for_resolved_type(typ.base_type, true)
	}
	type_name := typ.name()
	if tc.ownership_type_name_has_direct_drop(type_name) {
		return OwnershipDropTarget{
			type_name:        type_name
			optional_wrapper: optional_wrapper
		}
	}
	return none
}

fn (tc &TypeChecker) ownership_type_name_has_direct_drop(type_name string) bool {
	if tc.ownership == unsafe { nil } || type_name.len == 0 {
		return false
	}
	base := generic_base_name(type_name)
	return type_name in tc.ownership.drop_structs || base in tc.ownership.drop_structs
}

fn (mut tc TypeChecker) ownership_live_drop_entries() []OwnershipDropEntry {
	st := tc.ownership_state()
	mut candidates := []OwnershipDropCandidate{}
	for name, pos in st.owned_vars {
		if !name.contains('.') && !name.contains('[') {
			candidates << OwnershipDropCandidate{
				name: name
				pos:  int(pos)
			}
		}
	}
	candidates.sort(a.pos > b.pos)
	mut entries := []OwnershipDropEntry{}
	for candidate in candidates {
		if entry := tc.ownership_live_drop_entry(candidate.name) {
			entries << entry
		}
	}
	return entries
}

fn (mut tc TypeChecker) ownership_note_drop_types(fn_name string, entries []OwnershipDropEntry) {
	mut st := tc.ownership_state()
	for entry in entries {
		st.drop_type_names[entry.type_name] = true
		st.drop_type_names['${fn_name}\x01${entry.type_name}'] = true
	}
}

fn (mut tc TypeChecker) ownership_note_decl(name string) {
	if name.len == 0 || name == '_' {
		return
	}
	borrows := tc.ownership_borrower_snapshot(name)
	mut st := tc.ownership_state()
	if st.scope_frames.len == 0 {
		return
	}
	scope_idx := st.scope_frames.len - 1
	if name in st.scope_frames[scope_idx].names {
		return
	}
	children := tc.ownership_child_snapshots(name)
	st.scope_frames[scope_idx].names[name] = OwnershipNameSnapshot{
		had_owned: name in st.owned_vars
		owned_pos: st.owned_vars[name] or { flat.NodeId(-1) }
		had_type:  name in st.owned_var_types
		type_name: st.owned_var_types[name] or { '' }
		had_moved: name in st.moved_vars
		moved:     st.moved_vars[name] or { MovedVar{} }
		borrows:   borrows
		children:  children
		had_fn:    name in st.ownership_fn_value_vars
		fn_name:   st.ownership_fn_value_vars[name] or { '' }
	}
	st.scope_frames[scope_idx].decl_order << name
}

fn (mut tc TypeChecker) ownership_refresh_scope_snapshot(name string) {
	if name.len == 0 || name == '_' {
		return
	}
	mut st := tc.ownership_state()
	if st.scope_frames.len == 0 {
		return
	}
	scope_idx := st.scope_frames.len - 1
	snap := st.scope_frames[scope_idx].names[name] or { return }
	st.scope_frames[scope_idx].names[name] = OwnershipNameSnapshot{
		had_owned: name in st.owned_vars
		owned_pos: st.owned_vars[name] or { flat.NodeId(-1) }
		had_type:  name in st.owned_var_types
		type_name: st.owned_var_types[name] or { '' }
		had_moved: name in st.moved_vars
		moved:     st.moved_vars[name] or { MovedVar{} }
		borrows:   snap.borrows
		children:  tc.ownership_child_snapshots(name)
		had_fn:    name in st.ownership_fn_value_vars
		fn_name:   st.ownership_fn_value_vars[name] or { '' }
	}
}

fn (mut tc TypeChecker) ownership_child_snapshots(name string) []OwnershipKeySnapshot {
	if name.len == 0 {
		return []OwnershipKeySnapshot{}
	}
	st := tc.ownership_state()
	mut out := []OwnershipKeySnapshot{}
	for key in tc.ownership_child_state_keys(name) {
		out << OwnershipKeySnapshot{
			name:      key
			had_owned: key in st.owned_vars
			owned_pos: st.owned_vars[key] or { flat.NodeId(-1) }
			had_type:  key in st.owned_var_types
			type_name: st.owned_var_types[key] or { '' }
			had_moved: key in st.moved_vars
			moved:     st.moved_vars[key] or { MovedVar{} }
		}
	}
	return out
}

fn (mut tc TypeChecker) ownership_child_state_keys(name string) []string {
	if name.len == 0 {
		return []string{}
	}
	st := tc.ownership_state()
	mut seen := map[string]bool{}
	for key, _ in st.owned_vars {
		if ownership_storage_key_is_descendant(key, name) {
			seen[key] = true
		}
	}
	for key, _ in st.owned_var_types {
		if ownership_storage_key_is_descendant(key, name) {
			seen[key] = true
		}
	}
	for key, _ in st.moved_vars {
		if ownership_storage_key_is_descendant(key, name) {
			seen[key] = true
		}
	}
	mut keys := seen.keys()
	keys.sort()
	return keys
}

fn (mut tc TypeChecker) ownership_clear_descendant_state(name string) {
	mut st := tc.ownership_state()
	for key in tc.ownership_child_state_keys(name) {
		st.owned_vars.delete(key)
		st.owned_var_types.delete(key)
		st.moved_vars.delete(key)
	}
}

fn (mut tc TypeChecker) ownership_note_binding(name string, typ Type, pos flat.NodeId) {
	if tc.ownership_effects_disabled() {
		return
	}
	if name.len == 0 || name == '_' {
		return
	}
	tc.ownership_note_decl(name)
	tc.ownership_release_borrower(name)
	mut st := tc.ownership_state()
	st.moved_vars.delete(name)
	source_id := tc.ownership_guard_source_for_binding(pos, name)
	source_name := tc.ownership_expr_ident_name(source_id)
	source_participates := source_name.len > 0 && tc.ownership_storage_participates(source_name)
	if tc.ownership_type_is_owned(typ) || source_participates {
		if source_name.len > 0 {
			tc.ownership_reject_global_move(source_name, pos, name, false)
			if source_name in st.owned_vars {
				tc.ownership_move_var(source_name, name, pos, false, '', true)
			} else {
				_ := tc.ownership_move_overlapping_dynamic_storage(source_name, name, pos, false,
					'', true)
			}
		}
		tc.ownership_mark_owned(name, typ, pos)
		return
	}
	st.owned_vars.delete(name)
	st.owned_var_types.delete(name)
}

fn (mut tc TypeChecker) ownership_guard_source_for_binding(cond_id flat.NodeId, name string) flat.NodeId {
	if !tc.valid_node_id(cond_id) || name.len == 0 {
		return flat.empty_node
	}
	node := tc.a.nodes[int(cond_id)]
	if node.kind == .decl_assign && node.children_count >= 2 {
		lhs_id := tc.a.child(&node, 0)
		lhs := tc.a.nodes[int(lhs_id)]
		if lhs.kind == .ident && lhs.value == name {
			return tc.a.child(&node, 1)
		}
		return flat.empty_node
	}
	if node.kind == .infix && node.op == .logical_and && node.children_count >= 2 {
		left_source := tc.ownership_guard_source_for_binding(tc.a.child(&node, 0), name)
		if tc.valid_node_id(left_source) {
			return left_source
		}
		return tc.ownership_guard_source_for_binding(tc.a.child(&node, 1), name)
	}
	return flat.empty_node
}

fn (mut tc TypeChecker) ownership_after_collect() {
	mut st := tc.ownership_state()
	for qname, impls in tc.struct_implements {
		for iface in impls {
			short := iface.all_after_last('.')
			match short {
				'Owned' {
					st.owned_structs[qname] = true
				}
				'Copy' {
					st.copy_structs[qname] = true
				}
				'Drop' {
					st.drop_structs[qname] = true
					st.owned_structs[qname] = true
				}
				else {}
			}
		}
	}
	mut cur_module := ''
	for i in tc.top_level_idx {
		node := tc.a.nodes[i]
		match node.kind {
			.module_decl {
				cur_module = node.value
			}
			.global_decl {
				tc.ownership_collect_global_decl(cur_module, node)
			}
			.fn_decl {
				tc.ownership_collect_fn_signature(node)
				tc.ownership_collect_fn_param_mut(cur_module, node)
			}
			.c_fn_decl {
				tc.ownership_collect_c_fn_param_mut(node)
			}
			else {}
		}
	}
	fn_items := tc.ownership_fn_scan_items()
	tc.ownership_prescan_fn_returns(fn_items)
	tc.ownership_prescan_owned_call_params(fn_items)
	tc.ownership_collect_globals_after_prescan()
}

fn (mut tc TypeChecker) ownership_collect_globals_after_prescan() {
	mut cur_module := ''
	for i in tc.top_level_idx {
		node := tc.a.nodes[i]
		match node.kind {
			.module_decl {
				cur_module = node.value
			}
			.global_decl {
				tc.ownership_collect_global_decl(cur_module, node)
			}
			else {}
		}
	}
}

fn (mut tc TypeChecker) ownership_collect_global_decl(cur_module string, node flat.Node) {
	mut st := tc.ownership_state()
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		mut typ := tc.parse_type(field.typ)
		mut init_owned := false
		if field.children_count > 0 {
			init_id := tc.a.child(field, 0)
			if typ is Void {
				typ = tc.resolve_type(init_id)
			}
			init_owned = tc.ownership_expr_is_to_owned_call(init_id)
				|| tc.ownership_expr_is_owned_clone_call(init_id)
				|| tc.ownership_expr_is_ownership_call_in_module(init_id, cur_module)
		}
		if field.value.len > 0 {
			qname := ownership_qualify_name(cur_module, field.value)
			if tc.ownership_type_is_owned(typ) || init_owned {
				st.owned_globals[qname] = typ.name()
				if qname == field.value {
					st.owned_globals[field.value] = typ.name()
				}
			}
			if field.children_count > 0 {
				tc.ownership_collect_global_init_descendants(field.value, qname, tc.a.child(field,
					0))
			}
		}
	}
}

fn (mut tc TypeChecker) ownership_collect_global_init_descendants(name string, qname string, expr_id flat.NodeId) bool {
	return tc.ownership_collect_global_init_descendant(name, qname, '', expr_id)
}

fn (mut tc TypeChecker) ownership_collect_global_init_descendant(name string, qname string, suffix string, expr_id flat.NodeId) bool {
	if name.len == 0 || !tc.valid_node_id(expr_id) {
		return false
	}
	id := tc.ownership_unwrap_expr(expr_id)
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.array_literal {
			mut marked := false
			for i in 0 .. node.children_count {
				if tc.ownership_collect_global_init_descendant(name, qname, '${suffix}[${i}]', tc.a.child(&node,
					i))
				{
					marked = true
				}
			}
			return marked
		}
		.array_init {
			mut marked := false
			mut elem_idx := 0
			for i in 0 .. node.children_count {
				child_id := tc.a.child(&node, i)
				child := tc.a.nodes[int(child_id)]
				if child.kind == .field_init {
					if child.value == 'init' && child.children_count > 0 {
						if tc.ownership_collect_global_init_descendant(name, qname, '${suffix}[*]', tc.a.child(&child,
							0))
						{
							marked = true
						}
					}
					continue
				}
				if tc.ownership_collect_global_init_descendant(name, qname,
					'${suffix}[${elem_idx}]', child_id)
				{
					marked = true
				}
				elem_idx++
			}
			return marked
		}
		.map_init {
			mut marked := false
			for i := 0; i < node.children_count; i += 2 {
				key_id := tc.a.child(&node, i)
				if tc.ownership_collect_global_init_descendant(name, qname,
					ownership_map_key_storage_suffix(suffix), key_id)
				{
					marked = true
				}
				if i + 1 >= node.children_count {
					break
				}
				key_part := tc.ownership_index_key_part(key_id)
				if key_part.len == 0 {
					continue
				}
				if tc.ownership_collect_global_init_descendant(name, qname,
					'${suffix}[${key_part}]', tc.a.child(&node, i + 1))
				{
					marked = true
				}
			}
			return marked
		}
		.struct_init {
			init_type := tc.parse_type(node.value)
			return tc.ownership_collect_global_struct_init_descendants(name, qname, suffix, node,
				init_type, map[string]bool{})
		}
		.assoc {
			return tc.ownership_collect_global_assoc_init_descendants(name, qname, suffix, id,
				map[string]bool{})
		}
		else {}
	}

	marked_call_descendants := tc.ownership_collect_global_call_return_descendants(name, qname,
		suffix, id)
	if !tc.ownership_global_init_expr_is_owned(id, ownership_global_module_name(name, qname)) {
		return marked_call_descendants
	}
	type_name := tc.resolve_type(id).name()
	tc.ownership_mark_owned_global(name, qname, suffix, type_name)
	return true
}

fn (mut tc TypeChecker) ownership_mark_owned_global(name string, qname string, suffix string, type_name string) {
	mut st := tc.ownership_state()
	st.owned_globals[qname + suffix] = type_name
	if qname == name {
		st.owned_globals[name + suffix] = type_name
	}
}

fn (mut tc TypeChecker) ownership_collect_global_struct_init_descendants(name string, qname string, suffix string, node flat.Node, init_type Type, skip_fields map[string]bool) bool {
	fields := if init_type is Struct {
		tc.structs[init_type.name] or { []StructField{} }
	} else {
		[]StructField{}
	}
	mut marked := false
	mut explicit_fields := map[string]bool{}
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .field_init || field.children_count == 0 {
			continue
		}
		mut field_name := field.value
		if field_name.len == 0 && i < fields.len {
			field_name = fields[i].name
		}
		if field_name.len == 0 || field_name in skip_fields {
			continue
		}
		explicit_fields[field_name] = true
		if tc.ownership_collect_global_init_descendant(name, qname, '${suffix}.${field_name}', tc.a.child(field,
			0))
		{
			marked = true
		}
	}
	if init_type is Struct {
		if decl := tc.ownership_struct_decl_node(init_type.name) {
			for i in 0 .. decl.children_count {
				field := tc.a.child_node(decl, i)
				if field.kind != .field_decl || field.children_count == 0 || field.value.len == 0
					|| field.value in explicit_fields || field.value in skip_fields {
					continue
				}
				if tc.ownership_collect_global_init_descendant(name, qname,
					'${suffix}.${field.value}', tc.a.child(field, 0))
				{
					marked = true
				}
			}
		}
	}
	return marked
}

fn (mut tc TypeChecker) ownership_collect_global_assoc_init_descendants(name string, qname string, suffix string, id flat.NodeId, skip_fields map[string]bool) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind != .assoc || node.children_count == 0 {
		return false
	}
	init_type := tc.resolve_type(id)
	if init_type !is Struct {
		return false
	}
	fields := tc.structs[(init_type as Struct).name] or { []StructField{} }
	mut explicit_fields := map[string]bool{}
	for i in 1 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .field_init || field.children_count == 0 {
			continue
		}
		mut field_name := field.value
		field_idx := i - 1
		if field_name.len == 0 && field_idx < fields.len {
			field_name = fields[field_idx].name
		}
		if field_name.len > 0 {
			explicit_fields[field_name] = true
		}
	}
	mut base_skip_fields := skip_fields.clone()
	for field_name, _ in explicit_fields {
		base_skip_fields[field_name] = true
	}
	mut marked := false
	base_id := tc.a.child(&node, 0)
	if tc.ownership_collect_global_assoc_base_descendants(name, qname, suffix, base_id,
		base_skip_fields)
	{
		marked = true
	}
	for i in 1 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .field_init || field.children_count == 0 {
			continue
		}
		mut field_name := field.value
		field_idx := i - 1
		if field_name.len == 0 && field_idx < fields.len {
			field_name = fields[field_idx].name
		}
		if field_name.len == 0 || field_name in skip_fields {
			continue
		}
		if tc.ownership_collect_global_init_descendant(name, qname, '${suffix}.${field_name}', tc.a.child(field,
			0))
		{
			marked = true
		}
	}
	return marked
}

fn (mut tc TypeChecker) ownership_collect_global_assoc_base_descendants(name string, qname string, suffix string, base_id flat.NodeId, skip_fields map[string]bool) bool {
	base_name := tc.ownership_expr_ident_name(base_id)
	if base_name.len > 0 {
		return tc.ownership_collect_global_assoc_named_base_descendants(name, qname, suffix,
			base_name, skip_fields)
	}
	id := tc.ownership_unwrap_expr(base_id)
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.struct_init {
			init_type := tc.parse_type(node.value)
			return tc.ownership_collect_global_struct_init_descendants(name, qname, suffix, node,
				init_type, skip_fields)
		}
		.assoc {
			return tc.ownership_collect_global_assoc_init_descendants(name, qname, suffix, id,
				skip_fields)
		}
		else {
			return tc.ownership_collect_global_init_descendant(name, qname, suffix, base_id)
		}
	}
}

fn (mut tc TypeChecker) ownership_collect_global_call_return_descendants(name string, qname string, suffix string, expr_id flat.NodeId) bool {
	call_id := tc.ownership_unwrap_expr(expr_id)
	if !tc.valid_node_id(call_id) {
		return false
	}
	node := tc.a.nodes[int(call_id)]
	if node.kind != .call {
		return false
	}
	fn_name := tc.ownership_call_name_in_module(call_id, ownership_global_module_name(name, qname))
	if fn_name.len == 0 {
		return false
	}
	descs := tc.ownership_state().ownership_fn_return_descs[fn_name] or {
		[]OwnershipReturnDescendant{}
	}
	mut marked := false
	for desc in descs {
		if desc.slot_idx != 0 {
			continue
		}
		tc.ownership_mark_owned_global(name, qname, suffix + desc.suffix, desc.type_name)
		marked = true
	}
	return marked
}

fn (mut tc TypeChecker) ownership_collect_global_assoc_named_base_descendants(name string, qname string, suffix string, base_name string, skip_fields map[string]bool) bool {
	base_key := ownership_global_assoc_base_key(name, qname, base_name)
	mut marked := false
	st := tc.ownership_state()
	for source_name, type_name in st.owned_globals {
		if source_name != base_key && !ownership_storage_key_is_descendant(source_name, base_key) {
			continue
		}
		if source_name != base_key
			&& ownership_assoc_base_descendant_overridden(base_key, source_name, skip_fields) {
			continue
		}
		target_suffix := if source_name == base_key {
			suffix
		} else {
			suffix + source_name[base_key.len..]
		}
		tc.ownership_mark_owned_global(name, qname, target_suffix, type_name)
		marked = true
	}
	return marked
}

fn ownership_global_assoc_base_key(name string, qname string, base_name string) string {
	if base_name.len == 0 || base_name.contains('.') || qname == name {
		return base_name
	}
	module_name := qname.all_before_last('.')
	if module_name.len == 0 || module_name == qname {
		return base_name
	}
	return ownership_qualify_storage_key(module_name, base_name)
}

fn ownership_global_module_name(name string, qname string) string {
	if qname == name || !qname.contains('.') {
		return ''
	}
	return qname.all_before_last('.')
}

fn (mut tc TypeChecker) ownership_global_init_expr_is_owned(id flat.NodeId, module_name string) bool {
	return tc.ownership_expr_is_to_owned_call(id) || tc.ownership_expr_is_owned_clone_call(id)
		|| tc.ownership_expr_is_ownership_call_in_module(id, module_name)
		|| tc.ownership_type_is_owned(tc.resolve_type(id))
}

fn ownership_qualify_name(module_name string, name string) string {
	if module_name.len == 0 || module_name == 'main' || module_name == 'builtin'
		|| name.contains('.') {
		return name
	}
	return '${module_name}.${name}'
}

fn ownership_qualify_storage_key(module_name string, name string) string {
	if module_name.len == 0 || module_name == 'main' || module_name == 'builtin' || name.len == 0 {
		return name
	}
	mut root_end := name.len
	if dot_idx := name.index('.') {
		root_end = dot_idx
	}
	if bracket_idx := name.index('[') {
		if bracket_idx < root_end {
			root_end = bracket_idx
		}
	}
	if root_end <= 0 {
		return name
	}
	root := name[..root_end]
	if root == module_name {
		return name
	}
	return '${module_name}.${root}${name[root_end..]}'
}

fn ownership_qualify_fn_decl_name(module_name string, name string) string {
	if module_name.len == 0 || module_name == 'main' || module_name == 'builtin' {
		return name
	}
	if !name.contains('.') || name.starts_with('${module_name}.') {
		return ownership_qualify_name(module_name, name)
	}
	receiver := name.all_before_last('.')
	if receiver.contains('.') {
		return name
	}
	method := name.all_after_last('.')
	return '${module_name}.${receiver}.${method}'
}

fn (tc &TypeChecker) ownership_fn_scan_items() []OwnershipFnScanItem {
	mut items := []OwnershipFnScanItem{}
	mut cur_file := ''
	mut cur_module := ''
	for i in tc.top_level_idx {
		node := tc.a.nodes[i]
		match node.kind {
			.file {
				cur_file = node.value
				cur_module = tc.file_modules[cur_file] or { '' }
			}
			.module_decl {
				cur_module = node.value
			}
			.fn_decl {
				items << OwnershipFnScanItem{
					idx:    i
					file:   cur_file
					module: cur_module
					name:   ownership_qualify_fn_decl_name(cur_module, node.value)
				}
			}
			else {}
		}
	}
	return items
}

fn ownership_struct_implements(node flat.Node) []string {
	mut out := []string{}
	for part in ownership_struct_typ_parts(node.typ) {
		if part.starts_with('implements=') {
			for iface in part['implements='.len..].split('|') {
				clean := iface.trim_space()
				if clean.len > 0 {
					out << clean
				}
			}
		}
	}
	return out
}

fn ownership_struct_typ_parts(typ string) []string {
	mut parts := []string{}
	mut start := 0
	mut depth := 0
	for i, ch in typ {
		if ch == `[` {
			depth++
		} else if ch == `]` {
			if depth > 0 {
				depth--
			}
		} else if ch == `,` && depth == 0 {
			parts << typ[start..i]
			start = i + 1
		}
	}
	parts << typ[start..]
	return parts
}

fn (mut tc TypeChecker) ownership_collect_fn_signature(node flat.Node) {
	if !node.value.contains('.') || node.children_count == 0 {
		return
	}
	recv := tc.a.child_node(&node, 0)
	if recv.kind != .param || recv.typ.len == 0 || recv.typ.starts_with('&') || recv.op == .amp {
		return
	}
	short_type := recv.typ.all_after_last('.')
	method_name := node.value.all_after_last('.')
	tc.ownership_state().value_receiver_methods['${short_type}.${method_name}'] = true
	tc.ownership_state().value_receiver_methods['${recv.typ}.${method_name}'] = true
}

fn (mut tc TypeChecker) ownership_collect_fn_param_mut(module_name string, node flat.Node) {
	mut params := tc.ownership_node_param_mut_flags(node)
	params = tc.ownership_param_mut_with_implicit_veb_ctx(node, params)
	name := ownership_qualify_fn_decl_name(module_name, node.value)
	tc.ownership_register_fn_param_mut(name, params)
	if name != node.value && (node.value.contains('.') || module_name in ['', 'main', 'builtin']) {
		tc.ownership_register_fn_param_mut(node.value, params)
	}
}

fn (mut tc TypeChecker) ownership_collect_c_fn_param_mut(node flat.Node) {
	params := tc.ownership_node_param_mut_flags(node)
	tc.ownership_register_fn_param_mut(node.value, params)
	if !node.value.starts_with('C.') {
		tc.ownership_register_fn_param_mut('C.${node.value}', params)
	}
}

fn (tc &TypeChecker) ownership_node_param_mut_flags(node flat.Node) []bool {
	mut params := []bool{}
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		if child.kind == .param {
			params << (child.op == .amp || child.is_mut)
		}
	}
	return params
}

fn (tc &TypeChecker) ownership_param_mut_with_implicit_veb_ctx(node flat.Node, params []bool) []bool {
	if !tc.fn_needs_implicit_veb_ctx(node) {
		return params
	}
	insert_idx := tc.fn_implicit_veb_ctx_insert_index(node)
	mut result := []bool{cap: params.len + 1}
	for i, param in params {
		if i == insert_idx {
			result << false
		}
		result << param
	}
	if insert_idx >= params.len {
		result << false
	}
	return result
}

fn (mut tc TypeChecker) ownership_register_fn_param_mut(name string, params []bool) {
	tc.ownership_register_fn_param_mut_alias(name, params)
	lowered_name := naming.c_name(name)
	if lowered_name != name {
		tc.ownership_register_fn_param_mut_alias(lowered_name, params)
	}
	if name.ends_with('.str') {
		receiver := name.all_before_last('.')
		legacy_name := '${receiver}_str'
		if !legacy_name.contains('.') {
			tc.ownership_register_fn_param_mut_alias(legacy_name, params)
		}
	}
}

fn (mut tc TypeChecker) ownership_register_fn_param_mut_alias(name string, params []bool) {
	tc.ownership_state().ownership_fn_param_mut[name] = params.clone()
}

fn (mut tc TypeChecker) ownership_prescan_fn_returns(items []OwnershipFnScanItem) {
	mut changed := true
	for changed {
		before := tc.ownership_return_prescan_state_count()
		changed = false
		for item in items {
			tc.cur_file = item.file
			tc.cur_module = item.module
			node := tc.a.nodes[item.idx]
			if node.typ.len == 0 || node.typ == 'void' {
				continue
			}
			tc.ownership_prescan_fn_return_node(item.name, node)
		}
		changed = tc.ownership_return_prescan_state_count() != before
	}
}

fn (mut tc TypeChecker) ownership_return_prescan_state_count() int {
	st := tc.ownership_state()
	mut n := st.ownership_fns.len + st.ownership_fn_return_fn_values.len
	for _, values in st.ownership_fn_returns_param {
		n += values.len
	}
	for _, values in st.ownership_fn_return_params {
		n += values.len
	}
	for _, values in st.ownership_fn_return_slots {
		n += values.len
	}
	for _, values in st.ownership_fn_return_descs {
		n += values.len
	}
	for _, values in st.ownership_fn_return_param_descs {
		n += values.len
	}
	for _, value in st.ownership_fn_return_fn_values {
		n += value.len
	}
	return n
}

fn (mut tc TypeChecker) ownership_prescan_fn_return_node(fn_name string, fn_node flat.Node) {
	mut st := tc.ownership_state()
	saved_cur_fn := st.cur_fn
	saved_fn_value_vars := st.ownership_fn_value_vars.clone()
	st.cur_fn = fn_name
	st.ownership_fn_value_vars = map[string]string{}
	mut param_names := []string{}
	mut owned_locals := map[string]bool{}
	mut local_types := map[string]Type{}
	for i in 0 .. fn_node.children_count {
		child := tc.a.child_node(&fn_node, i)
		if child.kind == .param {
			param_names << child.value
			child_type := tc.parse_type(child.typ)
			if child.value.len > 0 {
				local_types[child.value] = child_type
			}
			if tc.ownership_type_is_owned(child_type) {
				key := '${fn_name}__param_${param_names.len - 1}'
				st.ownership_fn_params[key] = true
				owned_locals[child.value] = true
			}
		}
	}
	for i in 0 .. fn_node.children_count {
		child_id := tc.a.child(&fn_node, i)
		child := tc.a.nodes[int(child_id)]
		if child.kind == .param {
			continue
		}
		tc.ownership_prescan_returns_in(fn_name, child_id, param_names, mut owned_locals, mut
			local_types)
		if tc.ownership_stmt_definitely_exits_statement_sequence(child_id) {
			break
		}
	}
	st.cur_fn = saved_cur_fn
	st.ownership_fn_value_vars = saved_fn_value_vars.clone()
}

fn (mut tc TypeChecker) ownership_prescan_returned_fn_literal(fn_name string, node flat.Node) {
	literal_name := ownership_fn_literal_name(fn_name, node)
	tc.ownership_register_fn_literal_signature(literal_name, node)
	tc.ownership_prescan_fn_return_node(literal_name, node)
}

fn (tc &TypeChecker) ownership_fn_name(node flat.Node) string {
	return tc.qualify_fn_name(node.value)
}

fn (mut tc TypeChecker) ownership_prescan_returns_in(fn_name string, id flat.NodeId, param_names []string, mut owned_locals map[string]bool, mut local_types map[string]Type) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.nodes[int(id)]
	if node.kind in [.fn_literal, .lambda_expr] {
		return
	}
	if node.kind == .defer_stmt {
		return
	}
	if node.kind == .return_stmt {
		for i in 0 .. node.children_count {
			expr_id := tc.a.child(&node, i)
			clean_expr_id := tc.ownership_unwrap_expr(expr_id)
			mut is_returned_fn_literal := false
			if tc.valid_node_id(clean_expr_id) {
				expr_node := tc.a.nodes[int(clean_expr_id)]
				if expr_node.kind == .fn_literal {
					is_returned_fn_literal = true
					tc.ownership_prescan_returned_fn_literal(fn_name, expr_node)
				}
			}
			if fn_value := tc.ownership_fn_value_name_from_expr(expr_id) {
				tc.ownership_note_fn_return_fn_value(fn_name, fn_value)
			} else if fn_value := tc.ownership_fn_return_fn_value_from_call(expr_id) {
				tc.ownership_note_fn_return_fn_value(fn_name, fn_value)
			}
			tc.ownership_prescan_return_param_sources(fn_name, expr_id, i, param_names, local_types)
			tc.ownership_prescan_return_aggregate_literal_descendants(fn_name, i, '', expr_id,
				param_names, mut owned_locals, mut local_types)
			tc.ownership_prescan_add_return_owned_descendants_from_expr(fn_name, i, expr_id, mut
				owned_locals, local_types)
			mut return_owned_locals := owned_locals.clone()
			for pname in param_names {
				return_owned_locals.delete(pname)
			}
			mut return_local_types := local_types.clone()
			expr_owned := if is_returned_fn_literal {
				false
			} else {
				tc.ownership_prescan_expr_for_owned_calls(expr_id, mut return_owned_locals, mut
					return_local_types)
			}
			if tc.ownership_expr_is_to_owned_call(expr_id)
				|| tc.ownership_expr_is_ownership_call(expr_id) || expr_owned
				|| tc.ownership_type_is_owned(tc.resolve_type(expr_id)) {
				mut st := tc.ownership_state()
				st.mark_fn_return_owned(fn_name)
				call_name := tc.ownership_prescan_return_call_name(expr_id, local_types)
				for slot_idx in tc.ownership_return_slot_indices(expr_id, i, call_name) {
					tc.ownership_add_fn_return_slot(fn_name, slot_idx)
				}
			}
		}
		return
	}
	if node.kind == .block {
		tc.ownership_prescan_returns_in_sequence(fn_name, node, 0, param_names, mut owned_locals, mut
			local_types)
		return
	}
	if node.kind == .match_branch {
		body_start := if node.value == 'else' { 0 } else { node.value.int() }
		tc.ownership_prescan_returns_in_sequence(fn_name, node, body_start, param_names, mut
			owned_locals, mut local_types)
		return
	}
	if node.kind == .select_branch {
		tc.ownership_prescan_returns_in_sequence(fn_name, node,
			tc.ownership_select_branch_body_start(node), param_names, mut owned_locals, mut
			local_types)
		return
	}
	if node.kind == .if_expr {
		tc.ownership_prescan_if_returns_in(fn_name, node, param_names, mut owned_locals, mut
			local_types)
		return
	}
	if node.kind == .match_stmt {
		tc.ownership_prescan_match_returns_in(fn_name, node, param_names, mut owned_locals, mut
			local_types)
		return
	}
	if node.kind == .select_stmt {
		tc.ownership_prescan_select_returns_in(fn_name, node, param_names, mut owned_locals, mut
			local_types)
		return
	}
	if node.kind == .for_stmt {
		tc.ownership_prescan_for_returns_in(fn_name, node, param_names, mut owned_locals, mut
			local_types)
		return
	}
	if node.kind == .for_in_stmt {
		tc.ownership_prescan_for_in_returns_in(fn_name, node, param_names, mut owned_locals, mut
			local_types)
		return
	}
	if node.kind in [.decl_assign, .assign, .selector_assign, .index_assign] {
		tc.ownership_prescan_assign_for_owned_calls(node, mut owned_locals, mut local_types)
		return
	}
	for i in 0 .. node.children_count {
		tc.ownership_prescan_returns_in(fn_name, tc.a.child(&node, i), param_names, mut
			owned_locals, mut local_types)
	}
}

fn (mut tc TypeChecker) ownership_prescan_returns_in_sequence(fn_name string, node flat.Node, body_start int, param_names []string, mut owned_locals map[string]bool, mut local_types map[string]Type) {
	start := if body_start < 0 { 0 } else { body_start }
	for i in start .. node.children_count {
		stmt_id := tc.a.child(&node, i)
		tc.ownership_prescan_returns_in(fn_name, stmt_id, param_names, mut owned_locals, mut
			local_types)
		if tc.ownership_stmt_definitely_exits_statement_sequence(stmt_id) {
			return
		}
	}
}

fn (mut tc TypeChecker) ownership_prescan_if_returns_in(fn_name string, node flat.Node, param_names []string, mut owned_locals map[string]bool, mut local_types map[string]Type) {
	if node.children_count == 0 {
		return
	}
	_ := tc.ownership_prescan_expr_for_owned_calls(tc.a.child(&node, 0), mut owned_locals, mut
		local_types)
	base_owned := owned_locals.clone()
	base_types := local_types.clone()
	mut branch_owned := []map[string]bool{}
	mut branch_types := []map[string]Type{}
	if node.children_count > 1 {
		then_id := tc.a.child(&node, 1)
		mut then_owned := base_owned.clone()
		mut then_types := base_types.clone()
		tc.ownership_prescan_returns_in(fn_name, then_id, param_names, mut then_owned, mut
			then_types)
		if !tc.stmt_definitely_returns(then_id) {
			branch_owned << then_owned
			branch_types << then_types
		}
	}
	if node.children_count > 2 {
		else_id := tc.a.child(&node, 2)
		mut else_owned := base_owned.clone()
		mut else_types := base_types.clone()
		tc.ownership_prescan_returns_in(fn_name, else_id, param_names, mut else_owned, mut
			else_types)
		if !tc.stmt_definitely_returns(else_id) {
			branch_owned << else_owned
			branch_types << else_types
		}
	} else {
		branch_owned << base_owned
		branch_types << base_types
	}
	tc.ownership_prescan_merge_branch_states(base_owned, base_types, branch_owned, branch_types, mut
		owned_locals, mut local_types)
}

fn (mut tc TypeChecker) ownership_prescan_match_returns_in(fn_name string, node flat.Node, param_names []string, mut owned_locals map[string]bool, mut local_types map[string]Type) {
	if node.children_count == 0 {
		return
	}
	_ := tc.ownership_prescan_expr_for_owned_calls(tc.a.child(&node, 0), mut owned_locals, mut
		local_types)
	base_owned := owned_locals.clone()
	base_types := local_types.clone()
	mut branch_owned := []map[string]bool{}
	mut branch_types := []map[string]Type{}
	mut has_else := false
	for i in 1 .. node.children_count {
		branch_id := tc.a.child(&node, i)
		if !tc.valid_node_id(branch_id) {
			continue
		}
		branch := tc.a.nodes[int(branch_id)]
		if branch.kind != .match_branch {
			continue
		}
		if branch.value == 'else' {
			has_else = true
		}
		mut cur_owned := base_owned.clone()
		mut cur_types := base_types.clone()
		tc.ownership_prescan_returns_in(fn_name, branch_id, param_names, mut cur_owned, mut
			cur_types)
		if !tc.match_branch_definitely_returns(branch) {
			branch_owned << cur_owned
			branch_types << cur_types
		}
	}
	if !has_else && !tc.match_covers_all_variants(node) {
		branch_owned << base_owned
		branch_types << base_types
	}
	tc.ownership_prescan_merge_branch_states(base_owned, base_types, branch_owned, branch_types, mut
		owned_locals, mut local_types)
}

fn (mut tc TypeChecker) ownership_prescan_select_returns_in(fn_name string, node flat.Node, param_names []string, mut owned_locals map[string]bool, mut local_types map[string]Type) {
	base_owned := owned_locals.clone()
	base_types := local_types.clone()
	mut branch_owned := []map[string]bool{}
	mut branch_types := []map[string]Type{}
	for i in 0 .. node.children_count {
		branch_id := tc.a.child(&node, i)
		if !tc.valid_node_id(branch_id) {
			continue
		}
		branch := tc.a.nodes[int(branch_id)]
		if branch.kind != .select_branch {
			mut cur_owned := base_owned.clone()
			mut cur_types := base_types.clone()
			tc.ownership_prescan_returns_in(fn_name, branch_id, param_names, mut cur_owned, mut
				cur_types)
			if !tc.ownership_stmt_definitely_exits_statement_sequence(branch_id) {
				branch_owned << cur_owned
				branch_types << cur_types
			}
			continue
		}
		mut cur_owned := base_owned.clone()
		mut cur_types := base_types.clone()
		tc.ownership_prescan_returns_in(fn_name, branch_id, param_names, mut cur_owned, mut
			cur_types)
		if !tc.ownership_select_branch_definitely_exits_statement_sequence(branch) {
			branch_owned << cur_owned
			branch_types << cur_types
		}
	}
	tc.ownership_prescan_merge_branch_states(base_owned, base_types, branch_owned, branch_types, mut
		owned_locals, mut local_types)
}

fn (mut tc TypeChecker) ownership_prescan_for_returns_in(fn_name string, node flat.Node, param_names []string, mut owned_locals map[string]bool, mut local_types map[string]Type) {
	if node.children_count > 0 {
		init_id := tc.a.child(&node, 0)
		if tc.valid_node_id(init_id) && tc.a.nodes[int(init_id)].kind != .empty {
			tc.ownership_prescan_returns_in(fn_name, init_id, param_names, mut owned_locals, mut
				local_types)
		}
	}
	if node.children_count > 1 {
		cond_id := tc.a.child(&node, 1)
		if tc.valid_node_id(cond_id) && tc.a.nodes[int(cond_id)].kind != .empty {
			_ := tc.ownership_prescan_expr_for_owned_calls(cond_id, mut owned_locals, mut
				local_types)
		}
	}
	base_owned := owned_locals.clone()
	base_types := local_types.clone()
	mut branch_owned := []map[string]bool{}
	mut branch_types := []map[string]Type{}
	mut body_owned := base_owned.clone()
	mut body_types := base_types.clone()
	tc.ownership_prescan_returns_in_sequence(fn_name, node, 3, param_names, mut body_owned, mut
		body_types)
	if node.children_count > 2 && tc.ownership_statement_sequence_can_reach_loop_post(node, 3) {
		post_id := tc.a.child(&node, 2)
		if tc.valid_node_id(post_id) && tc.a.nodes[int(post_id)].kind != .empty {
			tc.ownership_prescan_returns_in(fn_name, post_id, param_names, mut body_owned, mut
				body_types)
		}
	}
	if !tc.ownership_statement_sequence_definitely_returns(node, 3) {
		branch_owned << body_owned
		branch_types << body_types
	}
	loop_may_skip_body := node.children_count > 1 && tc.a.child_node(&node, 1).kind != .empty
	if loop_may_skip_body {
		branch_owned << base_owned
		branch_types << base_types
	}
	tc.ownership_prescan_merge_branch_states(base_owned, base_types, branch_owned, branch_types, mut
		owned_locals, mut local_types)
}

fn (mut tc TypeChecker) ownership_prescan_for_in_returns_in(fn_name string, node flat.Node, param_names []string, mut owned_locals map[string]bool, mut local_types map[string]Type) {
	if node.children_count < 3 {
		return
	}
	header := node.value.int()
	container_id := tc.a.child(&node, 2)
	_ := tc.ownership_prescan_expr_for_owned_calls(container_id, mut owned_locals, mut local_types)
	if header == 4 && node.children_count > 3 {
		range_end_id := tc.a.child(&node, 3)
		_ := tc.ownership_prescan_expr_for_owned_calls(range_end_id, mut owned_locals, mut
			local_types)
	}
	base_owned := owned_locals.clone()
	base_types := local_types.clone()
	mut branch_owned := []map[string]bool{}
	mut branch_types := []map[string]Type{}
	mut body_owned := base_owned.clone()
	mut body_types := base_types.clone()
	tc.ownership_prescan_returns_in_sequence(fn_name, node, header, param_names, mut body_owned, mut
		body_types)
	if !tc.ownership_statement_sequence_definitely_returns(node, header) {
		branch_owned << body_owned
		branch_types << body_types
	}
	branch_owned << base_owned
	branch_types << base_types
	tc.ownership_prescan_merge_branch_states(base_owned, base_types, branch_owned, branch_types, mut
		owned_locals, mut local_types)
}

fn (mut tc TypeChecker) ownership_prescan_merge_branch_states(base_owned map[string]bool, base_types map[string]Type, branch_owned []map[string]bool, branch_types []map[string]Type, mut owned_locals map[string]bool, mut local_types map[string]Type) {
	if branch_owned.len == 0 {
		return
	}
	mut merged_owned := map[string]bool{}
	mut merged_types := base_types.clone()
	for i, owned in branch_owned {
		types := if i < branch_types.len { branch_types[i] } else { base_types }
		for name, is_owned in owned {
			if !is_owned || !ownership_prescan_branch_key_can_escape(name, base_owned, base_types) {
				continue
			}
			merged_owned[name] = true
			if typ := types[name] {
				merged_types[name] = typ
			}
		}
	}
	for name in owned_locals.keys() {
		owned_locals.delete(name)
	}
	for name, owned in merged_owned {
		owned_locals[name] = owned
	}
	for name in local_types.keys() {
		local_types.delete(name)
	}
	for name, typ in merged_types {
		local_types[name] = typ
	}
}

fn ownership_prescan_branch_key_can_escape(name string, base_owned map[string]bool, base_types map[string]Type) bool {
	if name in base_owned || name in base_types {
		return true
	}
	for base_name, _ in base_owned {
		if ownership_storage_key_is_descendant(name, base_name) {
			return true
		}
	}
	for base_name, _ in base_types {
		if ownership_storage_key_is_descendant(name, base_name) {
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) ownership_prescan_return_aggregate_literal_descendants(fn_name string, slot_idx int, base_suffix string, expr_id flat.NodeId, param_names []string, mut owned_locals map[string]bool, mut local_types map[string]Type) bool {
	if fn_name.len == 0 || slot_idx < 0 || !tc.valid_node_id(expr_id) {
		return false
	}
	id := tc.ownership_unwrap_expr(expr_id)
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.array_literal {
			mut marked := false
			for i in 0 .. node.children_count {
				elem_id := tc.a.child(&node, i)
				suffix := '${base_suffix}[${i}]'
				if tc.ownership_prescan_return_aggregate_literal_descendants(fn_name, slot_idx, suffix, elem_id, param_names, mut owned_locals, mut local_types)
					|| tc.ownership_prescan_add_return_param_descendant_from_expr(fn_name, slot_idx, suffix, elem_id, param_names)
					|| tc.ownership_prescan_add_return_descendant_from_expr(fn_name, slot_idx, suffix, elem_id, mut owned_locals, mut local_types) {
					marked = true
				}
			}
			return marked
		}
		.array_init {
			mut marked := false
			mut elem_idx := 0
			for i in 0 .. node.children_count {
				child_id := tc.a.child(&node, i)
				child := tc.a.nodes[int(child_id)]
				if child.kind == .field_init {
					if child.value == 'init' && child.children_count > 0 {
						init_id := tc.a.child(&child, 0)
						suffix := '${base_suffix}[*]'
						if tc.ownership_prescan_return_aggregate_literal_descendants(fn_name, slot_idx, suffix, init_id, param_names, mut owned_locals, mut local_types)
							|| tc.ownership_prescan_add_return_param_descendant_from_expr(fn_name, slot_idx, suffix, init_id, param_names)
							|| tc.ownership_prescan_add_return_descendant_from_expr(fn_name, slot_idx, suffix, init_id, mut owned_locals, mut local_types) {
							marked = true
						}
					}
					continue
				}
				suffix := '${base_suffix}[${elem_idx}]'
				if tc.ownership_prescan_return_aggregate_literal_descendants(fn_name, slot_idx, suffix, child_id, param_names, mut owned_locals, mut local_types)
					|| tc.ownership_prescan_add_return_param_descendant_from_expr(fn_name, slot_idx, suffix, child_id, param_names)
					|| tc.ownership_prescan_add_return_descendant_from_expr(fn_name, slot_idx, suffix, child_id, mut owned_locals, mut local_types) {
					marked = true
				}
				elem_idx++
			}
			return marked
		}
		.map_init {
			mut marked := false
			for i := 0; i < node.children_count; i += 2 {
				key_id := tc.a.child(&node, i)
				key_suffix := ownership_map_key_storage_suffix(base_suffix)
				if tc.ownership_prescan_return_aggregate_literal_descendants(fn_name, slot_idx, key_suffix, key_id, param_names, mut owned_locals, mut local_types)
					|| tc.ownership_prescan_add_return_param_descendant_from_expr(fn_name, slot_idx, key_suffix, key_id, param_names)
					|| tc.ownership_prescan_add_return_descendant_from_expr(fn_name, slot_idx, key_suffix, key_id, mut owned_locals, mut local_types) {
					marked = true
				} else {
					key_owned := tc.ownership_prescan_expr_for_owned_calls(key_id, mut
						owned_locals, mut local_types)
					if key_owned {
						tc.ownership_prescan_consume_local(key_id, mut owned_locals)
					}
				}
				if i + 1 >= node.children_count {
					break
				}
				key_part := tc.ownership_index_key_part(key_id)
				if key_part.len == 0 {
					value_id := tc.a.child(&node, i + 1)
					value_owned := tc.ownership_prescan_expr_for_owned_calls(value_id, mut
						owned_locals, mut local_types)
					if value_owned {
						tc.ownership_prescan_consume_local(value_id, mut owned_locals)
					}
					continue
				}
				value_id := tc.a.child(&node, i + 1)
				suffix := '${base_suffix}[${key_part}]'
				if tc.ownership_prescan_return_aggregate_literal_descendants(fn_name, slot_idx, suffix, value_id, param_names, mut owned_locals, mut local_types)
					|| tc.ownership_prescan_add_return_param_descendant_from_expr(fn_name, slot_idx, suffix, value_id, param_names)
					|| tc.ownership_prescan_add_return_descendant_from_expr(fn_name, slot_idx, suffix, value_id, mut owned_locals, mut local_types) {
					marked = true
				}
			}
			return marked
		}
		.struct_init {
			init_type := tc.parse_type(node.value)
			fields := if init_type is Struct {
				tc.structs[init_type.name] or { []StructField{} }
			} else {
				[]StructField{}
			}
			mut marked := false
			mut explicit_fields := map[string]bool{}
			for i in 0 .. node.children_count {
				field := tc.a.child_node(&node, i)
				if field.kind != .field_init || field.children_count == 0 {
					continue
				}
				mut field_name := field.value
				if field_name.len == 0 && i < fields.len {
					field_name = fields[i].name
				}
				if field_name.len == 0 {
					continue
				}
				explicit_fields[field_name] = true
				value_id := tc.a.child(field, 0)
				suffix := '${base_suffix}.${field_name}'
				if tc.ownership_prescan_return_aggregate_literal_descendants(fn_name, slot_idx, suffix, value_id, param_names, mut owned_locals, mut local_types)
					|| tc.ownership_prescan_add_return_param_descendant_from_expr(fn_name, slot_idx, suffix, value_id, param_names)
					|| tc.ownership_prescan_add_return_descendant_from_expr(fn_name, slot_idx, suffix, value_id, mut owned_locals, mut local_types) {
					marked = true
				}
			}
			if init_type is Struct {
				if decl := tc.ownership_struct_decl_node(init_type.name) {
					for i in 0 .. decl.children_count {
						field := tc.a.child_node(decl, i)
						if field.kind != .field_decl || field.children_count == 0
							|| field.value.len == 0 || field.value in explicit_fields {
							continue
						}
						default_id := tc.a.child(field, 0)
						suffix := '${base_suffix}.${field.value}'
						if tc.ownership_prescan_return_aggregate_literal_descendants(fn_name, slot_idx, suffix, default_id, param_names, mut owned_locals, mut local_types)
							|| tc.ownership_prescan_add_return_param_descendant_from_expr(fn_name, slot_idx, suffix, default_id, param_names)
							|| tc.ownership_prescan_add_return_descendant_from_expr(fn_name, slot_idx, suffix, default_id, mut owned_locals, mut local_types) {
							marked = true
						}
					}
				}
			}
			return marked
		}
		else {}
	}

	return false
}

fn (mut tc TypeChecker) ownership_prescan_add_return_owned_descendants_from_expr(fn_name string, slot_idx int, expr_id flat.NodeId, mut owned_locals map[string]bool, local_types map[string]Type) bool {
	if fn_name.len == 0 || slot_idx < 0 || !tc.valid_node_id(expr_id) {
		return false
	}
	source_name := tc.ownership_expr_ident_name(expr_id)
	if source_name.len == 0 {
		return false
	}
	mut marked := false
	for source in ownership_prescan_owned_descendant_names(source_name, owned_locals) {
		source_suffix := source[source_name.len..]
		type_name := (local_types[source] or { tc.resolve_type(expr_id) }).name()
		tc.ownership_add_fn_return_descendant(fn_name, slot_idx, source_suffix, type_name)
		owned_locals.delete(source)
		marked = true
	}
	return marked
}

fn (mut tc TypeChecker) ownership_prescan_add_return_param_descendant_from_expr(fn_name string, slot_idx int, target_suffix string, expr_id flat.NodeId, param_names []string) bool {
	if fn_name.len == 0 || slot_idx < 0 || target_suffix.len == 0 {
		return false
	}
	source_name := tc.ownership_expr_ident_name(expr_id)
	if source_name.len == 0 {
		return false
	}
	for pi, pname in param_names {
		if source_name == pname {
			tc.ownership_add_fn_return_param_descendant(fn_name, pi, slot_idx, '', target_suffix)
			return true
		}
		if ownership_storage_key_is_descendant(source_name, pname) {
			tc.ownership_add_fn_return_param_descendant(fn_name, pi, slot_idx,
				source_name[pname.len..], target_suffix)
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) ownership_prescan_add_return_descendant_from_expr(fn_name string, slot_idx int, suffix string, expr_id flat.NodeId, mut owned_locals map[string]bool, mut local_types map[string]Type) bool {
	if fn_name.len == 0 || slot_idx < 0 || suffix.len == 0 || !tc.valid_node_id(expr_id) {
		return false
	}
	expr_owned := tc.ownership_prescan_expr_for_owned_calls(expr_id, mut owned_locals, mut
		local_types)
	source_name := tc.ownership_expr_ident_name(expr_id)
	mut marked := false
	if source_name.len > 0 {
		for source in ownership_prescan_owned_descendant_names(source_name, owned_locals) {
			source_suffix := source[source_name.len..]
			type_name := (local_types[source] or { tc.resolve_type(expr_id) }).name()
			tc.ownership_add_fn_return_descendant(fn_name, slot_idx, suffix + source_suffix,
				type_name)
			owned_locals.delete(source)
			marked = true
		}
	}
	if expr_owned {
		type_name := if source_name.len > 0 {
			(local_types[source_name] or { tc.resolve_type(expr_id) }).name()
		} else {
			tc.resolve_type(expr_id).name()
		}
		tc.ownership_prescan_consume_local(expr_id, mut owned_locals)
		tc.ownership_add_fn_return_descendant(fn_name, slot_idx, suffix, type_name)
		marked = true
	}
	return marked
}

fn (mut tc TypeChecker) ownership_prescan_param_aggregate_literal_descendants(fn_name string, param_idx int, base_suffix string, expr_id flat.NodeId, mut owned_locals map[string]bool, mut local_types map[string]Type) bool {
	if fn_name.len == 0 || param_idx < 0 || !tc.valid_node_id(expr_id) {
		return false
	}
	id := tc.ownership_unwrap_expr(expr_id)
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.array_literal {
			mut marked := false
			for i in 0 .. node.children_count {
				elem_id := tc.a.child(&node, i)
				suffix := '${base_suffix}[${i}]'
				if tc.ownership_prescan_param_aggregate_literal_descendants(fn_name, param_idx, suffix, elem_id, mut owned_locals, mut local_types)
					|| tc.ownership_prescan_add_param_descendant_from_expr(fn_name, param_idx, suffix, elem_id, mut owned_locals, mut local_types) {
					marked = true
				}
			}
			return marked
		}
		.array_init {
			mut marked := false
			mut elem_idx := 0
			for i in 0 .. node.children_count {
				child_id := tc.a.child(&node, i)
				child := tc.a.nodes[int(child_id)]
				if child.kind == .field_init {
					if child.value == 'init' && child.children_count > 0 {
						init_id := tc.a.child(&child, 0)
						suffix := '${base_suffix}[*]'
						if tc.ownership_prescan_param_aggregate_literal_descendants(fn_name, param_idx, suffix, init_id, mut owned_locals, mut local_types)
							|| tc.ownership_prescan_add_param_descendant_from_expr(fn_name, param_idx, suffix, init_id, mut owned_locals, mut local_types) {
							marked = true
						}
					}
					continue
				}
				suffix := '${base_suffix}[${elem_idx}]'
				if tc.ownership_prescan_param_aggregate_literal_descendants(fn_name, param_idx, suffix, child_id, mut owned_locals, mut local_types)
					|| tc.ownership_prescan_add_param_descendant_from_expr(fn_name, param_idx, suffix, child_id, mut owned_locals, mut local_types) {
					marked = true
				}
				elem_idx++
			}
			return marked
		}
		.map_init {
			mut marked := false
			for i := 0; i < node.children_count; i += 2 {
				key_id := tc.a.child(&node, i)
				key_suffix := ownership_map_key_storage_suffix(base_suffix)
				if tc.ownership_prescan_param_aggregate_literal_descendants(fn_name, param_idx, key_suffix, key_id, mut owned_locals, mut local_types)
					|| tc.ownership_prescan_add_param_descendant_from_expr(fn_name, param_idx, key_suffix, key_id, mut owned_locals, mut local_types) {
					marked = true
				} else {
					key_owned := tc.ownership_prescan_expr_for_owned_calls(key_id, mut
						owned_locals, mut local_types)
					if key_owned {
						tc.ownership_prescan_consume_local(key_id, mut owned_locals)
					}
				}
				if i + 1 >= node.children_count {
					break
				}
				key_part := tc.ownership_index_key_part(key_id)
				value_id := tc.a.child(&node, i + 1)
				if key_part.len == 0 {
					value_owned := tc.ownership_prescan_expr_for_owned_calls(value_id, mut
						owned_locals, mut local_types)
					if value_owned {
						tc.ownership_prescan_consume_local(value_id, mut owned_locals)
					}
					continue
				}
				suffix := '${base_suffix}[${key_part}]'
				if tc.ownership_prescan_param_aggregate_literal_descendants(fn_name, param_idx, suffix, value_id, mut owned_locals, mut local_types)
					|| tc.ownership_prescan_add_param_descendant_from_expr(fn_name, param_idx, suffix, value_id, mut owned_locals, mut local_types) {
					marked = true
				}
			}
			return marked
		}
		.struct_init {
			init_type := tc.parse_type(node.value)
			fields := if init_type is Struct {
				tc.structs[init_type.name] or { []StructField{} }
			} else {
				[]StructField{}
			}
			mut marked := false
			mut explicit_fields := map[string]bool{}
			for i in 0 .. node.children_count {
				field := tc.a.child_node(&node, i)
				if field.kind != .field_init || field.children_count == 0 {
					continue
				}
				mut field_name := field.value
				if field_name.len == 0 && i < fields.len {
					field_name = fields[i].name
				}
				if field_name.len == 0 {
					continue
				}
				explicit_fields[field_name] = true
				value_id := tc.a.child(field, 0)
				suffix := '${base_suffix}.${field_name}'
				if tc.ownership_prescan_param_aggregate_literal_descendants(fn_name, param_idx, suffix, value_id, mut owned_locals, mut local_types)
					|| tc.ownership_prescan_add_param_descendant_from_expr(fn_name, param_idx, suffix, value_id, mut owned_locals, mut local_types) {
					marked = true
				}
			}
			if init_type is Struct {
				if decl := tc.ownership_struct_decl_node(init_type.name) {
					for i in 0 .. decl.children_count {
						field := tc.a.child_node(decl, i)
						if field.kind != .field_decl || field.children_count == 0
							|| field.value.len == 0 || field.value in explicit_fields {
							continue
						}
						default_id := tc.a.child(field, 0)
						suffix := '${base_suffix}.${field.value}'
						if tc.ownership_prescan_param_aggregate_literal_descendants(fn_name, param_idx, suffix, default_id, mut owned_locals, mut local_types)
							|| tc.ownership_prescan_add_param_descendant_from_expr(fn_name, param_idx, suffix, default_id, mut owned_locals, mut local_types) {
							marked = true
						}
					}
				}
			}
			return marked
		}
		else {}
	}

	return false
}

fn (mut tc TypeChecker) ownership_prescan_add_param_descendant_from_expr(fn_name string, param_idx int, suffix string, expr_id flat.NodeId, mut owned_locals map[string]bool, mut local_types map[string]Type) bool {
	if fn_name.len == 0 || param_idx < 0 || suffix.len == 0 || !tc.valid_node_id(expr_id) {
		return false
	}
	expr_owned := tc.ownership_prescan_expr_for_owned_calls(expr_id, mut owned_locals, mut
		local_types)
	source_name := tc.ownership_expr_ident_name(expr_id)
	mut marked := false
	if source_name.len > 0 {
		for source in ownership_prescan_owned_descendant_names(source_name, owned_locals) {
			source_suffix := source[source_name.len..]
			type_name := (local_types[source] or { tc.resolve_type(expr_id) }).name()
			tc.ownership_add_fn_param_descendant(fn_name, param_idx, suffix + source_suffix,
				type_name)
			owned_locals.delete(source)
			marked = true
		}
	}
	if expr_owned {
		type_name := if source_name.len > 0 {
			(local_types[source_name] or { tc.resolve_type(expr_id) }).name()
		} else {
			tc.resolve_type(expr_id).name()
		}
		tc.ownership_prescan_consume_local(expr_id, mut owned_locals)
		tc.ownership_add_fn_param_descendant(fn_name, param_idx, suffix, type_name)
		marked = true
	}
	return marked
}

fn (mut tc TypeChecker) ownership_prescan_return_param_sources(fn_name string, expr_id flat.NodeId, slot_idx int, param_names []string, local_types map[string]Type) {
	name := tc.ownership_expr_ident_name(expr_id)
	if name.len > 0 {
		for pi, pname in param_names {
			if name == pname {
				tc.ownership_add_fn_return_param(fn_name, pi)
				tc.ownership_add_fn_return_param_slot(fn_name, pi, slot_idx)
				continue
			}
			if ownership_storage_key_is_descendant(name, pname) {
				tc.ownership_add_fn_return_param_descendant(fn_name, pi, slot_idx,
					name[pname.len..], '')
			}
		}
		return
	}
	call_id := tc.ownership_unwrap_expr(expr_id)
	if !tc.valid_node_id(call_id) {
		return
	}
	node := tc.a.nodes[int(call_id)]
	if node.kind in [.if_expr, .match_stmt, .or_expr] {
		tc.ownership_prescan_conditional_return_param_sources(fn_name, call_id, slot_idx,
			param_names, local_types)
		return
	}
	if node.kind != .call {
		return
	}
	info := tc.ownership_prescan_call_info(node, local_types) or { return }
	param_slots := tc.ownership_state().ownership_fn_return_params[info.name] or {
		[]OwnershipReturnParamSlot{}
	}
	param_descs := tc.ownership_state().ownership_fn_return_param_descs[info.name] or {
		[]OwnershipReturnParamDescendant{}
	}
	for param_desc in param_descs {
		target_slot_idx := if tc.ownership_expr_is_multi_return(expr_id) {
			param_desc.slot_idx
		} else {
			slot_idx
		}
		tc.ownership_prescan_add_return_param_descendant_from_call_arg(fn_name, node, info,
			param_desc, target_slot_idx, param_names)
	}
	if param_slots.len > 0 {
		for param_slot in param_slots {
			target_slot_idx := if tc.ownership_expr_is_multi_return(expr_id) {
				param_slot.slot_idx
			} else {
				slot_idx
			}
			tc.ownership_prescan_add_return_param_from_call_arg(fn_name, node, info,
				param_slot.param_idx, target_slot_idx, param_names)
		}
		return
	}
	return_param_idxs := tc.ownership_state().ownership_fn_returns_param[info.name] or { []int{} }
	for callee_param_idx in return_param_idxs {
		tc.ownership_prescan_add_return_param_from_call_arg(fn_name, node, info, callee_param_idx,
			slot_idx, param_names)
	}
}

fn (mut tc TypeChecker) ownership_prescan_conditional_return_param_sources(fn_name string, id flat.NodeId, slot_idx int, param_names []string, local_types map[string]Type) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.if_expr {
			if node.children_count > 1 {
				then_tail := tc.branch_tail_expr_id(tc.a.child(&node, 1))
				tc.ownership_prescan_return_param_sources(fn_name, then_tail, slot_idx,
					param_names, local_types)
			}
			if node.children_count > 2 {
				else_id := tc.a.child(&node, 2)
				if tc.valid_node_id(else_id) && tc.a.nodes[int(else_id)].kind == .if_expr {
					tc.ownership_prescan_conditional_return_param_sources(fn_name, else_id,
						slot_idx, param_names, local_types)
				} else {
					else_tail := tc.branch_tail_expr_id(else_id)
					tc.ownership_prescan_return_param_sources(fn_name, else_tail, slot_idx,
						param_names, local_types)
				}
			}
		}
		.match_stmt {
			for i in 1 .. node.children_count {
				branch_id := tc.a.child(&node, i)
				if !tc.valid_node_id(branch_id) {
					continue
				}
				if tc.a.nodes[int(branch_id)].kind != .match_branch {
					continue
				}
				tail := tc.branch_tail_expr_id(branch_id)
				tc.ownership_prescan_return_param_sources(fn_name, tail, slot_idx, param_names,
					local_types)
			}
		}
		.or_expr {
			if node.children_count > 0 {
				tc.ownership_prescan_return_param_sources(fn_name, tc.a.child(&node, 0), slot_idx,
					param_names, local_types)
			}
			if node.children_count > 1 && node.value !in ['!', '?'] {
				fallback_tail := tc.branch_tail_expr_id(tc.a.child(&node, 1))
				tc.ownership_prescan_return_param_sources(fn_name, fallback_tail, slot_idx,
					param_names, local_types)
			}
		}
		else {}
	}
}

fn (mut tc TypeChecker) ownership_prescan_add_return_param_from_call_arg(fn_name string, node flat.Node, info CallInfo, callee_param_idx int, slot_idx int, param_names []string) {
	arg_id := tc.ownership_call_arg_for_return_param_info(node, info, callee_param_idx) or {
		return
	}
	arg_name := tc.ownership_expr_ident_name(arg_id)
	if arg_name.len == 0 {
		return
	}
	for pi, pname in param_names {
		if arg_name == pname {
			tc.ownership_add_fn_return_param(fn_name, pi)
			tc.ownership_add_fn_return_param_slot(fn_name, pi, slot_idx)
		}
	}
}

fn (mut tc TypeChecker) ownership_prescan_add_return_param_descendant_from_call_arg(fn_name string, node flat.Node, info CallInfo, callee_desc OwnershipReturnParamDescendant, slot_idx int, param_names []string) {
	source := tc.ownership_call_arg_for_return_param_source_info(node, info, callee_desc.param_idx,
		callee_desc.source_suffix) or { return }
	arg_name := tc.ownership_expr_ident_name(source.arg_id)
	if arg_name.len == 0 {
		return
	}
	for pi, pname in param_names {
		if arg_name == pname {
			tc.ownership_add_fn_return_param_descendant(fn_name, pi, slot_idx,
				source.source_suffix, callee_desc.target_suffix)
			return
		}
		if ownership_storage_key_is_descendant(arg_name, pname) {
			source_suffix := arg_name[pname.len..] + source.source_suffix
			tc.ownership_add_fn_return_param_descendant(fn_name, pi, slot_idx, source_suffix,
				callee_desc.target_suffix)
			return
		}
	}
}

fn (mut tc TypeChecker) ownership_prescan_return_call_name(expr_id flat.NodeId, local_types map[string]Type) string {
	call_id := tc.ownership_unwrap_expr(expr_id)
	if !tc.valid_node_id(call_id) {
		return ''
	}
	node := tc.a.nodes[int(call_id)]
	if node.kind != .call {
		return ''
	}
	info := tc.ownership_prescan_call_info(node, local_types) or { return '' }
	return info.name
}

fn (mut tc TypeChecker) ownership_return_slot_indices(expr_id flat.NodeId, fallback_slot_idx int, call_name string) []int {
	if call_name.len > 0 && tc.ownership_expr_is_multi_return(expr_id) {
		slots := tc.ownership_state().ownership_fn_return_slots[call_name] or { []int{} }
		if slots.len > 0 {
			return slots.clone()
		}
	}
	return [fallback_slot_idx]
}

fn (tc &TypeChecker) ownership_expr_is_multi_return(expr_id flat.NodeId) bool {
	expr_type := tc.resolve_type(expr_id)
	return expr_type is MultiReturn
}

fn (mut tc TypeChecker) ownership_add_fn_return_param(fn_name string, param_idx int) {
	mut st := tc.ownership_state()
	mut params := st.ownership_fn_returns_param[fn_name] or { []int{} }
	if param_idx !in params {
		params << param_idx
		st.ownership_fn_returns_param[fn_name] = params
	}
}

fn (mut tc TypeChecker) ownership_add_fn_return_param_slot(fn_name string, param_idx int, slot_idx int) {
	mut st := tc.ownership_state()
	mut slots := st.ownership_fn_return_params[fn_name] or { []OwnershipReturnParamSlot{} }
	for slot in slots {
		if slot.param_idx == param_idx && slot.slot_idx == slot_idx {
			return
		}
	}
	slots << OwnershipReturnParamSlot{
		param_idx: param_idx
		slot_idx:  slot_idx
	}
	st.ownership_fn_return_params[fn_name] = slots
}

fn (mut tc TypeChecker) ownership_add_fn_return_slot(fn_name string, slot_idx int) {
	mut st := tc.ownership_state()
	mut slots := st.ownership_fn_return_slots[fn_name] or { []int{} }
	if slot_idx !in slots {
		slots << slot_idx
		st.ownership_fn_return_slots[fn_name] = slots
	}
}

fn (mut tc TypeChecker) ownership_add_fn_return_descendant(fn_name string, slot_idx int, suffix string, type_name string) {
	if fn_name.len == 0 || slot_idx < 0 || suffix.len == 0 {
		return
	}
	mut st := tc.ownership_state()
	mut descs := st.ownership_fn_return_descs[fn_name] or { []OwnershipReturnDescendant{} }
	for desc in descs {
		if desc.slot_idx == slot_idx && desc.suffix == suffix {
			return
		}
	}
	descs << OwnershipReturnDescendant{
		slot_idx:  slot_idx
		suffix:    suffix
		type_name: if type_name.len > 0 { type_name } else { 'string' }
	}
	st.ownership_fn_return_descs[fn_name] = descs
}

fn (mut tc TypeChecker) ownership_add_fn_return_param_descendant(fn_name string, param_idx int, slot_idx int, source_suffix string, target_suffix string) {
	if fn_name.len == 0 || param_idx < 0 || slot_idx < 0
		|| (source_suffix.len == 0 && target_suffix.len == 0) {
		return
	}
	mut st := tc.ownership_state()
	mut descs := st.ownership_fn_return_param_descs[fn_name] or {
		[]OwnershipReturnParamDescendant{}
	}
	for desc in descs {
		if desc.param_idx == param_idx && desc.slot_idx == slot_idx
			&& desc.source_suffix == source_suffix && desc.target_suffix == target_suffix {
			return
		}
	}
	descs << OwnershipReturnParamDescendant{
		param_idx:     param_idx
		slot_idx:      slot_idx
		source_suffix: source_suffix
		target_suffix: target_suffix
	}
	st.ownership_fn_return_param_descs[fn_name] = descs
}

fn (mut tc TypeChecker) ownership_add_fn_param_descendant(fn_name string, param_idx int, suffix string, type_name string) {
	if fn_name.len == 0 || param_idx < 0 || suffix.len == 0 {
		return
	}
	mut st := tc.ownership_state()
	mut descs := st.ownership_fn_param_descs[fn_name] or { []OwnershipParamDescendant{} }
	for desc in descs {
		if desc.param_idx == param_idx && desc.suffix == suffix {
			return
		}
	}
	descs << OwnershipParamDescendant{
		param_idx: param_idx
		suffix:    suffix
		type_name: if type_name.len > 0 { type_name } else { 'string' }
	}
	st.ownership_fn_param_descs[fn_name] = descs
	st.ownership_fn_param_desc_count++
}

fn (mut tc TypeChecker) ownership_prescan_owned_call_params(items []OwnershipFnScanItem) {
	mut changed := true
	for changed {
		st := tc.ownership_state()
		before_params := st.ownership_fn_params.len
		before_descs := st.ownership_fn_param_desc_count
		changed = false
		for item in items {
			tc.cur_file = item.file
			tc.cur_module = item.module
			tc.ownership_prescan_fn_owned_call_params(item.name, tc.a.nodes[item.idx])
		}
		changed = st.ownership_fn_params.len != before_params
			|| st.ownership_fn_param_desc_count != before_descs
	}
}

fn (mut tc TypeChecker) ownership_prescan_fn_owned_call_params(fn_name string, fn_node flat.Node) {
	mut st := tc.ownership_state()
	saved_cur_fn := st.cur_fn
	saved_fn_value_vars := st.ownership_fn_value_vars.clone()
	st.cur_fn = fn_name
	st.ownership_fn_value_vars = map[string]string{}
	mut owned_locals := map[string]bool{}
	mut local_types := map[string]Type{}
	for i in 0 .. fn_node.children_count {
		child := tc.a.child_node(&fn_node, i)
		if child.kind != .param || child.value.len == 0 {
			continue
		}
		child_type := tc.parse_type(child.typ)
		local_types[child.value] = child_type
		key := '${fn_name}__param_${i}'
		if key in st.ownership_fn_params || tc.ownership_type_is_owned(child_type) {
			owned_locals[child.value] = true
		}
	}
	param_descs := st.ownership_fn_param_descs[fn_name] or { []OwnershipParamDescendant{} }
	for desc in param_descs {
		if desc.param_idx < 0 || desc.param_idx >= fn_node.children_count {
			continue
		}
		child := tc.a.child_node(&fn_node, desc.param_idx)
		if child.kind != .param || child.value.len == 0 {
			continue
		}
		key := child.value + desc.suffix
		owned_locals[key] = true
		local_types[key] = tc.parse_type(desc.type_name)
	}
	for i in 0 .. fn_node.children_count {
		child_id := tc.a.child(&fn_node, i)
		child := tc.a.nodes[int(child_id)]
		if child.kind == .param {
			continue
		}
		tc.ownership_prescan_node_for_owned_calls(child_id, mut owned_locals, mut local_types)
	}
	st.cur_fn = saved_cur_fn
	st.ownership_fn_value_vars = saved_fn_value_vars.clone()
}

fn (mut tc TypeChecker) ownership_prescan_fn_literal_owned_call_params(node flat.Node) bool {
	mut st := tc.ownership_state()
	before := st.ownership_fn_params.len
	before_descs := tc.ownership_fn_param_desc_count()
	fn_name := ownership_fn_literal_name(st.cur_fn, node)
	tc.ownership_register_fn_literal_signature(fn_name, node)
	saved_cur_fn := st.cur_fn
	saved_fn_value_vars := st.ownership_fn_value_vars.clone()
	st.cur_fn = fn_name
	st.ownership_fn_value_vars = map[string]string{}
	mut owned_locals := map[string]bool{}
	mut local_types := map[string]Type{}
	mut param_idx := 0
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		if child.kind != .param || child.value.len == 0 {
			continue
		}
		child_type := tc.parse_type(child.typ)
		local_types[child.value] = child_type
		key := '${fn_name}__param_${param_idx}'
		if key in st.ownership_fn_params || tc.ownership_type_is_owned(child_type) {
			owned_locals[child.value] = true
		}
		param_idx++
	}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.nodes[int(child_id)]
		if child.kind == .param || child.kind == .ident {
			continue
		}
		tc.ownership_prescan_node_for_owned_calls(child_id, mut owned_locals, mut local_types)
	}
	changed := st.ownership_fn_params.len != before
		|| tc.ownership_fn_param_desc_count() != before_descs
	st.cur_fn = saved_cur_fn
	st.ownership_fn_value_vars = saved_fn_value_vars.clone()
	return changed
}

fn (mut tc TypeChecker) ownership_fn_param_desc_count() int {
	st := tc.ownership_state()
	return st.ownership_fn_param_desc_count
}

fn (mut tc TypeChecker) ownership_prescan_node_for_owned_calls(id flat.NodeId, mut owned_locals map[string]bool, mut local_types map[string]Type) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.decl_assign, .assign, .selector_assign, .index_assign {
			tc.ownership_prescan_assign_for_owned_calls(node, mut owned_locals, mut local_types)
		}
		.fn_decl {
			return
		}
		else {
			_ := tc.ownership_prescan_expr_for_owned_calls(id, mut owned_locals, mut local_types)
		}
	}
}

fn (mut tc TypeChecker) ownership_prescan_assign_for_owned_calls(node flat.Node, mut owned_locals map[string]bool, mut local_types map[string]Type) {
	if tc.ownership_prescan_multi_return_assign_for_owned_calls(node, mut owned_locals, mut
		local_types)
	{
		return
	}
	mut i := 0
	for i + 1 < node.children_count {
		lhs_id := tc.a.child(&node, i)
		rhs_id := tc.a.child(&node, i + 1)
		lhs_name := tc.ownership_lhs_name(lhs_id)
		if lhs_name.len > 0 && lhs_name != '_'
			&& tc.ownership_prescan_assign_literal_to_name(lhs_name, rhs_id, mut owned_locals, mut local_types) {
			tc.ownership_track_fn_value_binding(lhs_name, rhs_id)
			i += 2
			continue
		}
		rhs_owned := tc.ownership_prescan_expr_for_owned_calls(rhs_id, mut owned_locals, mut
			local_types)
		rhs_name := tc.ownership_expr_ident_name(rhs_id)
		if rhs_owned && rhs_name.len > 0 {
			owned_locals.delete(rhs_name)
		}
		if lhs_name.len > 0 && lhs_name != '_' {
			local_types[lhs_name] = tc.resolve_type(rhs_id)
			tc.ownership_track_fn_value_binding(lhs_name, rhs_id)
			if rhs_name.len > 0 {
				tc.ownership_prescan_transfer_owned_descendants(rhs_name, lhs_name, mut
					owned_locals, mut local_types)
			}
			if rhs_owned {
				owned_locals[lhs_name] = true
			} else {
				owned_locals.delete(lhs_name)
			}
		}
		i += 2
	}
}

fn (mut tc TypeChecker) ownership_prescan_multi_return_assign_for_owned_calls(node flat.Node, mut owned_locals map[string]bool, mut local_types map[string]Type) bool {
	if node.children_count < 3 {
		return false
	}
	rhs_id := tc.a.child(&node, 1)
	rhs_type := tc.resolve_type(rhs_id)
	if rhs_type !is MultiReturn {
		return false
	}
	_ := tc.ownership_prescan_expr_for_owned_calls(rhs_id, mut owned_locals, mut local_types)
	mut lhs_ids := []flat.NodeId{}
	lhs_ids << tc.a.child(&node, 0)
	for i in 2 .. node.children_count {
		lhs_ids << tc.a.child(&node, i)
	}
	call_name := tc.ownership_prescan_return_call_name(rhs_id, local_types)
	owned_slots := tc.ownership_state().ownership_fn_return_slots[call_name] or { []int{} }
	return_descs := tc.ownership_state().ownership_fn_return_descs[call_name] or {
		[]OwnershipReturnDescendant{}
	}
	for i, lhs_id in lhs_ids {
		if i >= rhs_type.types.len {
			continue
		}
		lhs_name := tc.ownership_lhs_name(lhs_id)
		if lhs_name.len == 0 || lhs_name == '_' {
			continue
		}
		local_types[lhs_name] = rhs_type.types[i]
		mut marked := false
		if i in owned_slots || tc.ownership_type_is_owned(rhs_type.types[i]) {
			owned_locals[lhs_name] = true
			marked = true
		}
		for desc in return_descs {
			if desc.slot_idx != i {
				continue
			}
			target_name := lhs_name + desc.suffix
			owned_locals[target_name] = true
			local_types[target_name] = tc.parse_type(desc.type_name)
			marked = true
		}
		if !marked {
			owned_locals.delete(lhs_name)
		}
	}
	return true
}

fn (mut tc TypeChecker) ownership_prescan_mark_storage_from_expr(target_name string, expr_id flat.NodeId, mut owned_locals map[string]bool, mut local_types map[string]Type) bool {
	if target_name.len == 0 {
		return false
	}
	id := tc.ownership_unwrap_expr(expr_id)
	if !tc.valid_node_id(id) {
		return false
	}
	if tc.ownership_prescan_assign_literal_to_name(target_name, id, mut owned_locals, mut
		local_types)
	{
		return true
	}
	expr_owned := tc.ownership_prescan_expr_for_owned_calls(id, mut owned_locals, mut local_types)
	source_name := tc.ownership_expr_ident_name(id)
	mut marked := false
	if source_name.len > 0
		&& tc.ownership_prescan_transfer_owned_descendants(source_name, target_name, mut owned_locals, mut local_types) {
		marked = true
	}
	local_types[target_name] = tc.resolve_type(id)
	if expr_owned {
		tc.ownership_prescan_consume_local(id, mut owned_locals)
		owned_locals[target_name] = true
		return true
	}
	if !marked && !tc.ownership_prescan_has_owned_descendant(target_name, owned_locals) {
		owned_locals.delete(target_name)
	}
	return marked
}

fn (mut tc TypeChecker) ownership_prescan_assign_literal_to_name(lhs_name string, rhs_id flat.NodeId, mut owned_locals map[string]bool, mut local_types map[string]Type) bool {
	id := tc.ownership_unwrap_expr(rhs_id)
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .struct_init {
		init_type := tc.parse_type(node.value)
		fields := if init_type is Struct {
			tc.structs[init_type.name] or { []StructField{} }
		} else {
			[]StructField{}
		}
		mut explicit_fields := map[string]bool{}
		for i in 0 .. node.children_count {
			field := tc.a.child_node(&node, i)
			if field.kind != .field_init || field.children_count == 0 {
				continue
			}
			mut field_name := field.value
			if field_name.len == 0 && i < fields.len {
				field_name = fields[i].name
			}
			if field_name.len == 0 {
				continue
			}
			explicit_fields[field_name] = true
			value_id := tc.a.child(field, 0)
			target_name := '${lhs_name}.${field_name}'
			tc.ownership_prescan_mark_storage_from_expr(target_name, value_id, mut owned_locals, mut
				local_types)
		}
		if init_type is Struct {
			if decl := tc.ownership_struct_decl_node(init_type.name) {
				for i in 0 .. decl.children_count {
					field := tc.a.child_node(decl, i)
					if field.kind != .field_decl || field.children_count == 0
						|| field.value.len == 0 || field.value in explicit_fields {
						continue
					}
					target_name := '${lhs_name}.${field.value}'
					default_id := tc.a.child(field, 0)
					tc.ownership_prescan_mark_storage_from_expr(target_name, default_id, mut
						owned_locals, mut local_types)
				}
			}
		}
		local_types[lhs_name] = tc.resolve_type(id)
		if tc.ownership_type_is_owned(tc.resolve_type(id)) {
			owned_locals[lhs_name] = true
		} else {
			owned_locals.delete(lhs_name)
		}
		return true
	}
	if node.kind == .array_literal {
		for i in 0 .. node.children_count {
			elem_id := tc.a.child(&node, i)
			target_name := '${lhs_name}[${i}]'
			tc.ownership_prescan_mark_storage_from_expr(target_name, elem_id, mut owned_locals, mut
				local_types)
		}
		local_types[lhs_name] = tc.resolve_type(id)
		owned_locals.delete(lhs_name)
		return true
	}
	if node.kind == .array_init {
		mut elem_idx := 0
		for i in 0 .. node.children_count {
			child_id := tc.a.child(&node, i)
			child := tc.a.nodes[int(child_id)]
			if child.kind == .field_init {
				if child.value == 'init' && child.children_count > 0 {
					init_id := tc.a.child(&child, 0)
					target_name := '${lhs_name}[*]'
					tc.ownership_prescan_mark_storage_from_expr(target_name, init_id, mut
						owned_locals, mut local_types)
				}
				continue
			}
			target_name := '${lhs_name}[${elem_idx}]'
			tc.ownership_prescan_mark_storage_from_expr(target_name, child_id, mut owned_locals, mut
				local_types)
			elem_idx++
		}
		local_types[lhs_name] = tc.resolve_type(id)
		owned_locals.delete(lhs_name)
		return true
	}
	if node.kind == .map_init {
		for i := 0; i < node.children_count; i += 2 {
			key_id := tc.a.child(&node, i)
			key_target := ownership_map_key_storage_name(lhs_name)
			tc.ownership_prescan_mark_storage_from_expr(key_target, key_id, mut owned_locals, mut
				local_types)
			if i + 1 >= node.children_count {
				break
			}
			value_id := tc.a.child(&node, i + 1)
			key_part := tc.ownership_index_key_part(key_id)
			if key_part.len == 0 {
				value_owned := tc.ownership_prescan_expr_for_owned_calls(value_id, mut
					owned_locals, mut local_types)
				if value_owned {
					tc.ownership_prescan_consume_local(value_id, mut owned_locals)
				}
				continue
			}
			target_name := '${lhs_name}[${key_part}]'
			tc.ownership_prescan_mark_storage_from_expr(target_name, value_id, mut owned_locals, mut
				local_types)
		}
		local_types[lhs_name] = tc.resolve_type(id)
		owned_locals.delete(lhs_name)
		return true
	}
	if node.kind == .assoc {
		if node.children_count == 0 {
			return false
		}
		init_type := tc.resolve_type(id)
		if init_type !is Struct {
			return false
		}
		fields := tc.structs[(init_type as Struct).name] or { []StructField{} }
		mut explicit_fields := map[string]bool{}
		for i in 1 .. node.children_count {
			field := tc.a.child_node(&node, i)
			if field.kind != .field_init || field.children_count == 0 {
				continue
			}
			mut field_name := field.value
			field_idx := i - 1
			if field_name.len == 0 && field_idx < fields.len {
				field_name = fields[field_idx].name
			}
			if field_name.len > 0 {
				explicit_fields[field_name] = true
			}
		}
		tc.ownership_prescan_transfer_assoc_base_to_name(lhs_name, tc.a.child(&node, 0),
			explicit_fields, mut owned_locals, mut local_types)
		for i in 1 .. node.children_count {
			field := tc.a.child_node(&node, i)
			if field.kind != .field_init || field.children_count == 0 {
				continue
			}
			mut field_name := field.value
			field_idx := i - 1
			if field_name.len == 0 && field_idx < fields.len {
				field_name = fields[field_idx].name
			}
			if field_name.len == 0 {
				continue
			}
			target_name := '${lhs_name}.${field_name}'
			tc.ownership_prescan_mark_storage_from_expr(target_name, tc.a.child(field, 0), mut
				owned_locals, mut local_types)
		}
		local_types[lhs_name] = tc.resolve_type(id)
		if tc.ownership_type_is_owned(tc.resolve_type(id)) {
			owned_locals[lhs_name] = true
		} else {
			owned_locals.delete(lhs_name)
		}
		return true
	}
	return false
}

fn (mut tc TypeChecker) ownership_prescan_transfer_assoc_base_to_name(lhs_name string, base_id flat.NodeId, explicit_fields map[string]bool, mut owned_locals map[string]bool, mut local_types map[string]Type) bool {
	base_name := tc.ownership_expr_ident_name(base_id)
	if base_name.len == 0 {
		return tc.ownership_prescan_mark_storage_from_expr(lhs_name, base_id, mut owned_locals, mut
			local_types)
	}
	mut marked := false
	if base_name in owned_locals {
		source_type := local_types[base_name] or { tc.resolve_type(base_id) }
		owned_locals.delete(base_name)
		owned_locals[lhs_name] = true
		local_types[lhs_name] = source_type
		marked = true
	}
	for source_name in ownership_prescan_owned_descendant_names(base_name, owned_locals) {
		if ownership_assoc_base_descendant_overridden(base_name, source_name, explicit_fields) {
			continue
		}
		suffix := source_name[base_name.len..]
		target_name := lhs_name + suffix
		source_type := local_types[source_name] or { Type(String{}) }
		owned_locals.delete(source_name)
		owned_locals[target_name] = true
		local_types[target_name] = source_type
		marked = true
	}
	return marked
}

fn (mut tc TypeChecker) ownership_prescan_expr_for_owned_calls(id flat.NodeId, mut owned_locals map[string]bool, mut local_types map[string]Type) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	name := tc.ownership_expr_ident_name(id)
	if name.len > 0 && name in owned_locals {
		return true
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.ident {
			return node.value in owned_locals
		}
		.paren, .expr_stmt, .cast_expr {
			if node.children_count > 0 {
				return tc.ownership_prescan_expr_for_owned_calls(tc.a.child(&node, 0), mut
					owned_locals, mut local_types)
			}
			return false
		}
		.call {
			return tc.ownership_prescan_call_for_owned_calls(id, node, mut owned_locals, mut
				local_types)
		}
		.if_expr {
			return tc.ownership_prescan_if_expr_for_owned_calls(node, mut owned_locals, mut
				local_types)
		}
		.match_stmt {
			return tc.ownership_prescan_match_expr_for_owned_calls(node, mut owned_locals, mut
				local_types)
		}
		.or_expr {
			return tc.ownership_prescan_or_expr_for_owned_calls(node, mut owned_locals, mut
				local_types)
		}
		.fn_literal {
			_ := tc.ownership_prescan_fn_literal_owned_call_params(node)
			return false
		}
		.struct_init {
			for i in 0 .. node.children_count {
				field := tc.a.child_node(&node, i)
				if field.children_count > 0 {
					value_id := tc.a.child(field, 0)
					value_owned := tc.ownership_prescan_expr_for_owned_calls(value_id, mut
						owned_locals, mut local_types)
					if value_owned {
						tc.ownership_prescan_consume_local(value_id, mut owned_locals)
					}
				}
			}
			return tc.ownership_type_is_owned(tc.resolve_type(id))
		}
		.array_literal {
			for i in 0 .. node.children_count {
				elem_id := tc.a.child(&node, i)
				elem_owned := tc.ownership_prescan_expr_for_owned_calls(elem_id, mut owned_locals, mut
					local_types)
				if elem_owned {
					tc.ownership_prescan_consume_local(elem_id, mut owned_locals)
				}
			}
			return false
		}
		.map_init {
			for i in 0 .. node.children_count {
				elem_id := tc.a.child(&node, i)
				elem_owned := tc.ownership_prescan_expr_for_owned_calls(elem_id, mut owned_locals, mut
					local_types)
				if elem_owned {
					tc.ownership_prescan_consume_local(elem_id, mut owned_locals)
				}
			}
			return false
		}
		else {}
	}

	for i in 0 .. node.children_count {
		tc.ownership_prescan_node_for_owned_calls(tc.a.child(&node, i), mut owned_locals, mut
			local_types)
	}
	return false
}

fn (mut tc TypeChecker) ownership_prescan_if_expr_for_owned_calls(node flat.Node, mut owned_locals map[string]bool, mut local_types map[string]Type) bool {
	if node.children_count == 0 {
		return false
	}
	_ := tc.ownership_prescan_expr_for_owned_calls(tc.a.child(&node, 0), mut owned_locals, mut
		local_types)
	base_owned := owned_locals.clone()
	base_types := local_types.clone()
	mut branch_owned := []map[string]bool{}
	mut branch_types := []map[string]Type{}
	mut result_owned := false
	if node.children_count > 1 {
		then_id := tc.a.child(&node, 1)
		mut then_owned := base_owned.clone()
		mut then_types := base_types.clone()
		if tc.ownership_prescan_branch_tail_for_owned_calls(then_id, mut then_owned, mut then_types) {
			result_owned = true
		}
		if !tc.stmt_definitely_returns(then_id) {
			branch_owned << then_owned
			branch_types << then_types
		}
	}
	if node.children_count > 2 {
		else_id := tc.a.child(&node, 2)
		mut else_owned := base_owned.clone()
		mut else_types := base_types.clone()
		if tc.ownership_prescan_branch_tail_for_owned_calls(else_id, mut else_owned, mut else_types) {
			result_owned = true
		}
		if !tc.stmt_definitely_returns(else_id) {
			branch_owned << else_owned
			branch_types << else_types
		}
	} else {
		branch_owned << base_owned
		branch_types << base_types
	}
	tc.ownership_prescan_merge_branch_states(base_owned, base_types, branch_owned, branch_types, mut
		owned_locals, mut local_types)
	return result_owned
}

fn (mut tc TypeChecker) ownership_prescan_match_expr_for_owned_calls(node flat.Node, mut owned_locals map[string]bool, mut local_types map[string]Type) bool {
	if node.children_count == 0 {
		return false
	}
	_ := tc.ownership_prescan_expr_for_owned_calls(tc.a.child(&node, 0), mut owned_locals, mut
		local_types)
	base_owned := owned_locals.clone()
	base_types := local_types.clone()
	mut branch_owned := []map[string]bool{}
	mut branch_types := []map[string]Type{}
	mut result_owned := false
	mut has_else := false
	for i in 1 .. node.children_count {
		branch_id := tc.a.child(&node, i)
		if !tc.valid_node_id(branch_id) {
			continue
		}
		branch := tc.a.nodes[int(branch_id)]
		if branch.kind != .match_branch {
			continue
		}
		if branch.value == 'else' {
			has_else = true
		}
		mut cur_owned := base_owned.clone()
		mut cur_types := base_types.clone()
		if tc.ownership_prescan_branch_tail_for_owned_calls(branch_id, mut cur_owned, mut cur_types) {
			result_owned = true
		}
		if !tc.match_branch_definitely_returns(branch) {
			branch_owned << cur_owned
			branch_types << cur_types
		}
	}
	if !has_else && !tc.match_covers_all_variants(node) {
		branch_owned << base_owned
		branch_types << base_types
	}
	tc.ownership_prescan_merge_branch_states(base_owned, base_types, branch_owned, branch_types, mut
		owned_locals, mut local_types)
	return result_owned
}

fn (mut tc TypeChecker) ownership_prescan_or_expr_for_owned_calls(node flat.Node, mut owned_locals map[string]bool, mut local_types map[string]Type) bool {
	if node.children_count == 0 {
		return false
	}
	_ := tc.ownership_prescan_expr_for_owned_calls(tc.a.child(&node, 0), mut owned_locals, mut
		local_types)
	if node.children_count < 2 || node.value in ['!', '?'] {
		return false
	}
	base_owned := owned_locals.clone()
	base_types := local_types.clone()
	mut fallback_owned := base_owned.clone()
	mut fallback_types := base_types.clone()
	result_owned := tc.ownership_prescan_branch_tail_for_owned_calls(tc.a.child(&node, 1), mut
		fallback_owned, mut fallback_types)
	branch_owned := [base_owned, fallback_owned]
	branch_types := [base_types, fallback_types]
	tc.ownership_prescan_merge_branch_states(base_owned, base_types, branch_owned, branch_types, mut
		owned_locals, mut local_types)
	return result_owned
}

fn (mut tc TypeChecker) ownership_prescan_branch_tail_for_owned_calls(branch_id flat.NodeId, mut branch_owned_locals map[string]bool, mut branch_local_types map[string]Type) bool {
	if !tc.valid_node_id(branch_id) {
		return false
	}
	node := tc.a.nodes[int(branch_id)]
	if node.kind == .block {
		if node.children_count == 0 {
			return false
		}
		last_idx := node.children_count - 1
		for i in 0 .. last_idx {
			tc.ownership_prescan_node_for_owned_calls(tc.a.child(&node, i), mut
				branch_owned_locals, mut branch_local_types)
		}
		return tc.ownership_prescan_expr_for_owned_calls(tc.branch_tail_expr_id(branch_id), mut
			branch_owned_locals, mut branch_local_types)
	}
	if node.kind == .match_branch {
		body_start := if node.value == 'else' { 0 } else { node.value.int() }
		if node.children_count <= body_start {
			return false
		}
		last_idx := node.children_count - 1
		for i in body_start .. last_idx {
			tc.ownership_prescan_node_for_owned_calls(tc.a.child(&node, i), mut
				branch_owned_locals, mut branch_local_types)
		}
		return tc.ownership_prescan_expr_for_owned_calls(tc.branch_tail_expr_id(branch_id), mut
			branch_owned_locals, mut branch_local_types)
	}
	return tc.ownership_prescan_expr_for_owned_calls(branch_id, mut branch_owned_locals, mut
		branch_local_types)
}

fn (mut tc TypeChecker) ownership_prescan_params_field_arg(node flat.Node, info CallInfo, arg_node flat.Node, arg_id flat.NodeId, mut owned_locals map[string]bool, mut local_types map[string]Type) {
	field_name := arg_node.value
	arg_owned := tc.ownership_prescan_expr_for_owned_calls(arg_id, mut owned_locals, mut
		local_types)
	if field_name.len == 0 || info.name.len == 0 {
		if arg_owned {
			tc.ownership_prescan_consume_local(arg_id, mut owned_locals)
		}
		return
	}
	param_idx := tc.ownership_call_params_struct_decl_param_idx(node, info)
	if param_idx < 0 {
		if arg_owned {
			tc.ownership_prescan_consume_local(arg_id, mut owned_locals)
		}
		return
	}
	suffix := '.${field_name}'
	if arg_owned {
		arg_name := tc.ownership_expr_ident_name(arg_id)
		arg_type := if arg_name.len > 0 {
			local_types[arg_name] or { tc.resolve_type(arg_id) }
		} else {
			tc.resolve_type(arg_id)
		}
		tc.ownership_add_fn_param_descendant(info.name, param_idx, suffix, arg_type.name())
		tc.ownership_prescan_consume_local(arg_id, mut owned_locals)
	}
	arg_name := tc.ownership_expr_ident_name(arg_id)
	if arg_name.len > 0 {
		tc.ownership_prescan_transfer_owned_descendants_to_param_with_suffix(arg_name, info.name,
			param_idx, suffix, mut owned_locals, local_types)
	}
}

fn (mut tc TypeChecker) ownership_prescan_expr_is_owned_clone_call(id flat.NodeId, mut owned_locals map[string]bool, mut local_types map[string]Type) bool {
	call_id := tc.ownership_unwrap_expr(id)
	recv_id := tc.ownership_clone_receiver_id(call_id)
	if !tc.valid_node_id(recv_id) {
		return false
	}
	recv_name := tc.ownership_expr_ident_name(recv_id)
	if recv_name.len > 0 && recv_name in owned_locals {
		recv_type := local_types[recv_name] or { Type(String{}) }
		return tc.ownership_clone_result_is_owned(call_id, recv_type)
	}
	if !tc.ownership_prescan_expr_for_owned_calls(recv_id, mut owned_locals, mut local_types)
		&& !tc.ownership_type_is_owned(tc.resolve_type(recv_id)) {
		return false
	}
	return tc.ownership_clone_result_is_owned(call_id, tc.resolve_type(recv_id))
}

fn (mut tc TypeChecker) ownership_prescan_call_for_owned_calls(id flat.NodeId, node flat.Node, mut owned_locals map[string]bool, mut local_types map[string]Type) bool {
	if tc.ownership_expr_is_to_owned_call(id) {
		return true
	}
	if tc.ownership_prescan_expr_is_owned_clone_call(id, mut owned_locals, mut local_types) {
		return true
	}
	if tc.ownership_prescan_array_element_method_for_owned_calls(id, mut owned_locals, mut
		local_types)
	{
		return true
	}
	info := tc.ownership_prescan_call_info(node, local_types) or {
		fn_node := tc.a.child_node(&node, 0)
		if fn_node.kind == .selector && fn_node.children_count > 0 {
			_ := tc.ownership_prescan_expr_for_owned_calls(tc.a.child(fn_node, 0), mut
				owned_locals, mut local_types)
		}
		for i in 1 .. node.children_count {
			_ := tc.ownership_prescan_expr_for_owned_calls(tc.call_arg_value(tc.a.child(&node, i)), mut
				owned_locals, mut local_types)
		}
		return false
	}
	mut returns_owned := info.name in tc.ownership_state().ownership_fns
	return_param_idxs := tc.ownership_state().ownership_fn_returns_param[info.name] or { []int{} }
	arg_shift := tc.ownership_call_arg_shift(node, info)
	if info.has_receiver && node.children_count > 0 && info.params.len > 0 {
		fn_node := tc.a.child_node(&node, 0)
		if fn_node.kind == .selector && fn_node.children_count > 0 {
			recv_id := tc.a.child(fn_node, 0)
			recv_owned := tc.ownership_prescan_expr_for_owned_calls(recv_id, mut owned_locals, mut
				local_types)
			if 0 in return_param_idxs && recv_owned {
				returns_owned = true
			}
			if recv_owned && info.params[0] !is Pointer
				&& !tc.ownership_method_keeps_receiver(fn_node.value)
				&& !tc.ownership_string_builtin_keeps_receiver(recv_id, fn_node.value) {
				if info.name.len > 0 {
					tc.ownership_state().ownership_fn_params['${info.name}__param_0'] = true
				}
				tc.ownership_prescan_consume_local(recv_id, mut owned_locals)
			}
			recv_name := tc.ownership_expr_ident_name(recv_id)
			if recv_name.len > 0 && info.params[0] !is Pointer
				&& !tc.ownership_method_keeps_receiver(fn_node.value)
				&& !tc.ownership_string_builtin_keeps_receiver(recv_id, fn_node.value) {
				tc.ownership_prescan_transfer_owned_descendants_to_param(recv_name, info.name, 0, mut
					owned_locals, local_types)
			}
		}
	}
	for i in 1 .. node.children_count {
		arg_node := tc.a.child_node(&node, i)
		arg_id := tc.call_arg_value(tc.a.child(&node, i))
		if arg_node.kind == .field_init {
			tc.ownership_prescan_params_field_arg(node, info, arg_node, arg_id, mut owned_locals, mut
				local_types)
			continue
		}
		param_idx := tc.ownership_call_arg_decl_param_idx(info, i)
		type_param_idx := param_idx + arg_shift
		variadic_elem_idx := tc.ownership_call_arg_variadic_elem_idx(info, type_param_idx)
		target_param_idx := tc.ownership_call_arg_variadic_decl_param_idx(param_idx,
			variadic_elem_idx)
		target_suffix := ownership_call_arg_variadic_suffix(variadic_elem_idx)
		expected := tc.ownership_call_arg_expected_type(info, type_param_idx, variadic_elem_idx)
		if expected !is Void && expected !is Pointer && !tc.ownership_expr_is_borrow(arg_id)
			&& tc.ownership_prescan_param_aggregate_literal_descendants(info.name, target_param_idx, target_suffix, arg_id, mut owned_locals, mut local_types) {
			continue
		}
		arg_owned := tc.ownership_prescan_expr_for_owned_calls(arg_id, mut owned_locals, mut
			local_types)
		if param_idx in return_param_idxs && arg_owned {
			returns_owned = true
		}
		if !arg_owned || expected is Void {
			if expected !is Void && expected !is Pointer && !tc.ownership_expr_is_borrow(arg_id) {
				arg_name := tc.ownership_expr_ident_name(arg_id)
				if arg_name.len > 0 {
					tc.ownership_prescan_transfer_owned_descendants_to_param_with_suffix(arg_name,
						info.name, target_param_idx, target_suffix, mut owned_locals, local_types)
				}
			}
			continue
		}
		if expected is Pointer || tc.ownership_expr_is_borrow(arg_id) {
			continue
		}
		if info.name.len > 0 {
			if variadic_elem_idx >= 0 {
				arg_name := tc.ownership_expr_ident_name(arg_id)
				arg_type := if arg_name.len > 0 {
					local_types[arg_name] or { tc.resolve_type(arg_id) }
				} else {
					tc.resolve_type(arg_id)
				}
				tc.ownership_add_fn_param_descendant(info.name, target_param_idx, target_suffix,
					arg_type.name())
			} else {
				tc.ownership_state().ownership_fn_params['${info.name}__param_${param_idx}'] = true
			}
		}
		tc.ownership_prescan_consume_local(arg_id, mut owned_locals)
		arg_name := tc.ownership_expr_ident_name(arg_id)
		if arg_name.len > 0 {
			tc.ownership_prescan_transfer_owned_descendants_to_param_with_suffix(arg_name,
				info.name, target_param_idx, target_suffix, mut owned_locals, local_types)
		}
	}
	return returns_owned || tc.ownership_type_is_owned(tc.resolve_type(id))
}

fn (mut tc TypeChecker) ownership_prescan_array_element_method_for_owned_calls(id flat.NodeId, mut owned_locals map[string]bool, mut local_types map[string]Type) bool {
	call_id := tc.ownership_unwrap_expr(id)
	if !tc.valid_node_id(call_id) {
		return false
	}
	node := tc.a.nodes[int(call_id)]
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return false
	}
	method := fn_node.value
	if method !in ['first', 'last', 'pop', 'pop_left'] {
		return false
	}
	recv_id := tc.a.child(fn_node, 0)
	array_name := tc.ownership_expr_ident_name(recv_id)
	if array_name.len == 0 {
		return false
	}
	mut owned := false
	for source_key in ownership_prescan_array_element_method_source_keys(array_name, method,
		owned_locals) {
		if source_key in owned_locals {
			owned_locals.delete(source_key)
			owned = true
		}
		for source_name in ownership_prescan_owned_descendant_names(source_key, owned_locals) {
			owned_locals.delete(source_name)
			local_types.delete(source_name)
			owned = true
		}
	}
	return owned
}

fn ownership_prescan_array_element_method_source_keys(array_name string, method string, owned_locals map[string]bool) []string {
	match method {
		'first', 'pop_left' {
			return ['${array_name}[0]']
		}
		'last', 'pop' {
			dynamic_key := '${array_name}[*]'
			if dynamic_key in owned_locals
				|| ownership_prescan_owned_descendant_names(dynamic_key, owned_locals).len > 0 {
				return [dynamic_key]
			}
			mut max_idx := -1
			for name, _ in owned_locals {
				if idx := ownership_prescan_array_concrete_index(name, array_name) {
					if idx > max_idx {
						max_idx = idx
					}
				}
			}
			if max_idx >= 0 {
				return ['${array_name}[${max_idx}]']
			}
		}
		else {}
	}

	return []string{}
}

fn ownership_prescan_array_concrete_index(name string, array_name string) ?int {
	prefix := '${array_name}['
	if !name.starts_with(prefix) {
		return none
	}
	rest := name[prefix.len..]
	close_idx := rest.index(']') or { return none }
	idx_part := rest[..close_idx]
	if !idx_part.is_int() {
		return none
	}
	return idx_part.int()
}

fn (mut tc TypeChecker) ownership_prescan_call_info(node flat.Node, local_types map[string]Type) ?CallInfo {
	if node.children_count == 0 {
		return none
	}
	fn_node := tc.a.child_node(&node, 0)
	match fn_node.kind {
		.ident {
			if mapped := tc.ownership_state().ownership_fn_value_vars[fn_node.value] {
				if info := tc.ownership_fn_literal_call_info(mapped) {
					return info
				}
				if mapped in tc.fn_ret_types {
					return tc.call_info(mapped, false)
				}
			}
			qname := tc.qualify_fn_name(fn_node.value)
			if qname in tc.fn_ret_types {
				return tc.call_info(qname, false)
			}
			if fn_node.value in tc.fn_ret_types {
				return tc.call_info(fn_node.value, false)
			}
		}
		.selector {
			if fn_node.children_count == 0 {
				return none
			}
			base_node := tc.a.child_node(fn_node, 0)
			if base_node.kind != .ident {
				return none
			}
			if resolved_mod := tc.resolve_import_alias(base_node.value) {
				mod_name := '${resolved_mod}.${fn_node.value}'
				if mod_name in tc.fn_ret_types {
					return tc.call_info(mod_name, false)
				}
			}
			if base_node.value == tc.cur_module {
				mod_name := '${tc.cur_module}.${fn_node.value}'
				if mod_name in tc.fn_ret_types {
					return tc.call_info(mod_name, false)
				}
			}
			qbase := tc.qualify_name(base_node.value)
			static_name := '${qbase}.${fn_node.value}'
			if static_name in tc.fn_ret_types && (qbase in tc.structs
				|| qbase in tc.enum_names || qbase in tc.sum_types
				|| qbase in tc.interface_names || qbase in tc.type_aliases) {
				return tc.call_info(static_name, false)
			}
			if recv_type := local_types[base_node.value] {
				for mname in receiver_method_name_candidates(unwrap_pointer(recv_type),
					fn_node.value, tc.cur_module) {
					if mname !in tc.fn_ret_types {
						continue
					}
					if !tc.method_can_be_called_on_receiver(recv_type, fn_node.value, mname) {
						continue
					}
					return tc.call_info(mname, true)
				}
			}
		}
		else {}
	}

	return none
}

fn (tc &TypeChecker) ownership_call_arg_shift(node flat.Node, info CallInfo) int {
	ctx_count := if info.has_implicit_veb_ctx { 1 } else { 0 }
	if ctx_count == 0 {
		return 0
	}
	mut field_init_args := 0
	for i in 1 .. node.children_count {
		if tc.a.child_node(&node, i).kind == .field_init {
			field_init_args++
		}
	}
	collapsed := if field_init_args > 0 { 1 } else { 0 }
	recv_extra := if info.has_receiver { 1 } else { 0 }
	actual_count := node.children_count - 1 - field_init_args + collapsed + recv_extra
	if actual_count < info.params.len {
		return ctx_count
	}
	return 0
}

fn (tc &TypeChecker) ownership_call_arg_decl_param_idx(info CallInfo, child_idx int) int {
	return if info.has_receiver { child_idx } else { child_idx - 1 }
}

fn (tc &TypeChecker) ownership_call_arg_variadic_elem_idx(info CallInfo, type_param_idx int) int {
	if !info.is_variadic || info.params.len == 0 {
		return -1
	}
	variadic_param_idx := info.params.len - 1
	if type_param_idx < variadic_param_idx {
		return -1
	}
	return type_param_idx - variadic_param_idx
}

fn (tc &TypeChecker) ownership_call_arg_variadic_decl_param_idx(param_idx int, elem_idx int) int {
	if elem_idx < 0 {
		return param_idx
	}
	return param_idx - elem_idx
}

fn ownership_call_arg_variadic_suffix(elem_idx int) string {
	if elem_idx < 0 {
		return ''
	}
	return '[${elem_idx}]'
}

fn (tc &TypeChecker) ownership_call_arg_expected_type(info CallInfo, type_param_idx int, elem_idx int) Type {
	if elem_idx >= 0 && info.params.len > 0 {
		variadic_raw := info.params[info.params.len - 1]
		if variadic_raw is Array {
			return array_elem_type(variadic_raw)
		}
	}
	if type_param_idx >= 0 && type_param_idx < info.params.len {
		return info.params[type_param_idx]
	}
	return Type(void_)
}

fn (tc &TypeChecker) ownership_call_params_struct_decl_param_idx(node flat.Node, info CallInfo) int {
	if info.params.len == 0 {
		return -1
	}
	mut type_param_idx := -1
	for i := info.params.len - 1; i >= 0; i-- {
		if tc.is_params_struct_type(info.params[i]) {
			type_param_idx = i
			break
		}
	}
	if type_param_idx < 0 {
		return -1
	}
	return type_param_idx - tc.ownership_call_arg_shift(node, info)
}

fn (tc &TypeChecker) ownership_prescan_consume_local(expr_id flat.NodeId, mut owned_locals map[string]bool) {
	name := tc.ownership_expr_ident_name(expr_id)
	if name.len > 0 {
		owned_locals.delete(name)
	}
}

fn (tc &TypeChecker) ownership_prescan_transfer_owned_descendants(source_prefix string, target_prefix string, mut owned_locals map[string]bool, mut local_types map[string]Type) bool {
	if source_prefix.len == 0 || target_prefix.len == 0 || source_prefix == target_prefix {
		return false
	}
	mut moved_any := false
	for source_name in ownership_prescan_owned_descendant_names(source_prefix, owned_locals) {
		suffix := source_name[source_prefix.len..]
		target_name := target_prefix + suffix
		owned_locals.delete(source_name)
		owned_locals[target_name] = true
		local_types[target_name] = local_types[source_name] or { Type(String{}) }
		moved_any = true
	}
	return moved_any
}

fn (mut tc TypeChecker) ownership_prescan_transfer_owned_descendants_to_param(source_prefix string, fn_name string, param_idx int, mut owned_locals map[string]bool, local_types map[string]Type) bool {
	return tc.ownership_prescan_transfer_owned_descendants_to_param_with_suffix(source_prefix,
		fn_name, param_idx, '', mut owned_locals, local_types)
}

fn (mut tc TypeChecker) ownership_prescan_transfer_owned_descendants_to_param_with_suffix(source_prefix string, fn_name string, param_idx int, target_suffix string, mut owned_locals map[string]bool, local_types map[string]Type) bool {
	if source_prefix.len == 0 || fn_name.len == 0 || param_idx < 0 {
		return false
	}
	mut moved_any := false
	for source_name in ownership_prescan_owned_descendant_names(source_prefix, owned_locals) {
		suffix := source_name[source_prefix.len..]
		type_name := (local_types[source_name] or { Type(String{}) }).name()
		tc.ownership_add_fn_param_descendant(fn_name, param_idx, target_suffix + suffix, type_name)
		owned_locals.delete(source_name)
		moved_any = true
	}
	return moved_any
}

fn ownership_prescan_owned_descendant_names(prefix string, owned_locals map[string]bool) []string {
	if prefix.len == 0 {
		return []string{}
	}
	mut names := []string{}
	for name, owned in owned_locals {
		if owned && ownership_storage_key_is_descendant(name, prefix) {
			names << name
		}
	}
	names.sort()
	return names
}

fn (tc &TypeChecker) ownership_prescan_has_owned_descendant(prefix string, owned_locals map[string]bool) bool {
	for name, owned in owned_locals {
		if owned && ownership_storage_key_is_descendant(name, prefix) {
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) ownership_begin_fn(node flat.Node) {
	if tc.ownership_checks_suppressed() {
		return
	}
	mut st := tc.ownership_state()
	fn_name := tc.ownership_fn_name(node)
	st.frames << OwnershipFrame{
		cur_fn:          st.cur_fn
		owned_vars:      st.owned_vars.clone()
		owned_var_types: st.owned_var_types.clone()
		moved_vars:      st.moved_vars.clone()
		borrowed_vars:   st.borrowed_vars.clone()
		array_lengths:   st.array_lengths.clone()
		fn_value_vars:   st.ownership_fn_value_vars.clone()
		scope_frames:    ownership_clone_scope_frames(st.scope_frames)
		path_active:     st.path_active
	}
	st.cur_fn = fn_name
	st.drop_return_counts[fn_name] = 0
	st.drop_propagation_counts[fn_name] = 0
	st.drop_loop_control_counts[fn_name] = 0
	st.drop_loop_iteration_counts[fn_name] = 0
	st.drop_scope_counts[fn_name] = 0
	st.owned_vars = map[string]flat.NodeId{}
	st.owned_var_types = map[string]string{}
	st.moved_vars = map[string]MovedVar{}
	st.borrowed_vars = map[string][]BorrowInfo{}
	st.array_lengths = map[string]int{}
	st.ownership_fn_value_vars = map[string]string{}
	st.path_active = true
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		if child.kind != .param || child.value.len == 0 {
			continue
		}
		key := '${fn_name}__param_${i}'
		child_type := tc.parse_type(child.typ)
		if key in st.ownership_fn_params || tc.ownership_type_is_owned(child_type) {
			tc.ownership_mark_owned(child.value, child_type, tc.a.child(&node, i))
		}
	}
	param_descs := st.ownership_fn_param_descs[fn_name] or { []OwnershipParamDescendant{} }
	for desc in param_descs {
		if desc.param_idx < 0 || desc.param_idx >= node.children_count {
			continue
		}
		child_id := tc.a.child(&node, desc.param_idx)
		child := tc.a.nodes[int(child_id)]
		if child.kind != .param || child.value.len == 0 {
			continue
		}
		tc.ownership_mark_owned_name(child.value + desc.suffix, desc.type_name, child_id)
	}
}

fn (mut tc TypeChecker) ownership_begin_fn_literal(node flat.Node) {
	if tc.ownership_checks_suppressed() {
		return
	}
	mut st := tc.ownership_state()
	fn_name := ownership_fn_literal_name(st.cur_fn, node)
	captures := tc.ownership_consume_fn_literal_captures(node, fn_name)
	st.frames << OwnershipFrame{
		cur_fn:          st.cur_fn
		owned_vars:      st.owned_vars.clone()
		owned_var_types: st.owned_var_types.clone()
		moved_vars:      st.moved_vars.clone()
		borrowed_vars:   st.borrowed_vars.clone()
		array_lengths:   st.array_lengths.clone()
		fn_value_vars:   st.ownership_fn_value_vars.clone()
		scope_frames:    ownership_clone_scope_frames(st.scope_frames)
		path_active:     st.path_active
	}
	st.cur_fn = fn_name
	st.drop_return_counts[fn_name] = 0
	st.drop_scope_counts[fn_name] = 0
	st.owned_vars = map[string]flat.NodeId{}
	st.owned_var_types = map[string]string{}
	st.moved_vars = map[string]MovedVar{}
	st.borrowed_vars = map[string][]BorrowInfo{}
	st.array_lengths = map[string]int{}
	st.ownership_fn_value_vars = map[string]string{}
	st.path_active = true
	for capture in captures {
		tc.ownership_mark_owned_name(capture.name, capture.type_name, capture.pos)
	}
	mut param_idx := 0
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		if child.kind != .param || child.value.len == 0 {
			continue
		}
		child_type := tc.parse_type(child.typ)
		key := '${fn_name}__param_${param_idx}'
		if key in st.ownership_fn_params || tc.ownership_type_is_owned(child_type) {
			tc.ownership_mark_owned(child.value, child_type, tc.a.child(&node, i))
		}
		param_idx++
	}
}

fn (mut tc TypeChecker) ownership_consume_fn_literal_captures(node flat.Node, fn_name string) []OwnershipCaptureBinding {
	mut captures := []OwnershipCaptureBinding{}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.nodes[int(child_id)]
		if child.kind != .ident || child.value.len == 0 {
			continue
		}
		captures << tc.ownership_consume_fn_literal_capture(child.value, fn_name, child_id)
	}
	return captures
}

fn (mut tc TypeChecker) ownership_consume_fn_literal_capture(name string, fn_name string, pos flat.NodeId) []OwnershipCaptureBinding {
	mut captures := []OwnershipCaptureBinding{}
	st := tc.ownership_state()
	source_type := tc.resolve_type(pos)
	if name in st.owned_vars || tc.ownership_type_is_owned(source_type) {
		if name !in st.owned_vars {
			tc.ownership_mark_owned(name, source_type, pos)
		}
		tc.ownership_reject_global_move(name, pos, fn_name, false)
		type_name := tc.ownership_type_name_for_var(name)
		if tc.ownership_move_var_result(name, fn_name, pos, false, '', true) {
			captures << OwnershipCaptureBinding{
				name:      name
				type_name: type_name
				pos:       pos
			}
		}
	}
	for source_name in tc.ownership_owned_descendant_names(name) {
		type_name := tc.ownership_type_name_for_var(source_name)
		if tc.ownership_move_var_result(source_name, fn_name, pos, false, '', true) {
			captures << OwnershipCaptureBinding{
				name:      source_name
				type_name: type_name
				pos:       pos
			}
		}
	}
	return captures
}

fn ownership_fn_literal_name(cur_fn string, node flat.Node) string {
	return '${cur_fn}__fn_literal_${node.pos.id}_${node.pos.offset}'
}

fn (mut tc TypeChecker) ownership_begin_lambda_expr(node flat.Node) {
	if tc.ownership_checks_suppressed() {
		return
	}
	mut st := tc.ownership_state()
	fn_name := '${st.cur_fn}__lambda_${node.pos.id}_${node.pos.offset}'
	captures := tc.ownership_consume_lambda_captures(node, fn_name)
	st.frames << OwnershipFrame{
		cur_fn:          st.cur_fn
		owned_vars:      st.owned_vars.clone()
		owned_var_types: st.owned_var_types.clone()
		moved_vars:      st.moved_vars.clone()
		borrowed_vars:   st.borrowed_vars.clone()
		array_lengths:   st.array_lengths.clone()
		fn_value_vars:   st.ownership_fn_value_vars.clone()
		scope_frames:    ownership_clone_scope_frames(st.scope_frames)
		path_active:     st.path_active
	}
	st.cur_fn = fn_name
	st.owned_vars = map[string]flat.NodeId{}
	st.owned_var_types = map[string]string{}
	st.moved_vars = map[string]MovedVar{}
	st.borrowed_vars = map[string][]BorrowInfo{}
	st.array_lengths = map[string]int{}
	st.ownership_fn_value_vars = map[string]string{}
	st.path_active = true
	for capture in captures {
		tc.ownership_mark_owned_name(capture.name, capture.type_name, capture.pos)
	}
}

fn (mut tc TypeChecker) ownership_consume_lambda_captures(node flat.Node, fn_name string) []OwnershipCaptureBinding {
	if node.children_count == 0 {
		return []OwnershipCaptureBinding{}
	}
	mut params := map[string]bool{}
	for i in 0 .. node.children_count - 1 {
		child := tc.a.child_node(&node, i)
		if child.kind == .ident && child.value.len > 0 {
			params[child.value] = true
		}
	}
	body_id := tc.a.child(&node, node.children_count - 1)
	mut names := map[string]flat.NodeId{}
	tc.ownership_collect_lambda_capture_names(body_id, params, mut names)
	mut captures := []OwnershipCaptureBinding{}
	mut sorted_names := names.keys()
	sorted_names.sort()
	for name in sorted_names {
		captures << tc.ownership_consume_fn_literal_capture(name, fn_name, names[name])
	}
	return captures
}

fn (mut tc TypeChecker) ownership_collect_lambda_capture_names(id flat.NodeId, locals map[string]bool, mut names map[string]flat.NodeId) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.ident {
			if node.value.len > 0 && node.value !in locals {
				names[node.value] = id
			}
			return
		}
		.block {
			tc.ownership_collect_lambda_capture_sequence(node, 0, locals, mut names)
			return
		}
		.decl_assign {
			mut local_scope := locals.clone()
			tc.ownership_collect_lambda_decl_assign_captures(node, mut local_scope, mut names)
			return
		}
		.for_in_stmt {
			tc.ownership_collect_lambda_for_in_captures(node, locals, mut names)
			return
		}
		.for_stmt {
			tc.ownership_collect_lambda_for_captures(node, locals, mut names)
			return
		}
		.if_expr {
			tc.ownership_collect_lambda_if_captures(node, locals, mut names)
			return
		}
		.match_stmt {
			tc.ownership_collect_lambda_match_captures(node, locals, mut names)
			return
		}
		.fn_literal, .lambda_expr {
			return
		}
		.call {
			if node.children_count > 0 {
				callee_id := tc.a.child(&node, 0)
				callee := if tc.valid_node_id(callee_id) {
					tc.a.nodes[int(callee_id)]
				} else {
					flat.Node{}
				}
				if callee.kind == .selector && callee.children_count > 0 {
					tc.ownership_collect_lambda_capture_names(tc.a.child(callee, 0), locals, mut
						names)
				}
			}
			for i in 1 .. node.children_count {
				tc.ownership_collect_lambda_capture_names(tc.a.child(&node, i), locals, mut names)
			}
			return
		}
		.selector {
			if node.children_count > 0 {
				tc.ownership_collect_lambda_capture_names(tc.a.child(&node, 0), locals, mut names)
			}
			return
		}
		else {}
	}

	for i in 0 .. node.children_count {
		tc.ownership_collect_lambda_capture_names(tc.a.child(&node, i), locals, mut names)
	}
}

fn (mut tc TypeChecker) ownership_collect_lambda_capture_sequence(node flat.Node, start int, locals map[string]bool, mut names map[string]flat.NodeId) {
	mut local_scope := locals.clone()
	for i in start .. node.children_count {
		tc.ownership_collect_lambda_capture_stmt(tc.a.child(&node, i), mut local_scope, mut names)
	}
}

fn (mut tc TypeChecker) ownership_collect_lambda_capture_stmt(id flat.NodeId, mut locals map[string]bool, mut names map[string]flat.NodeId) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.decl_assign {
			tc.ownership_collect_lambda_decl_assign_captures(node, mut locals, mut names)
		}
		.for_in_stmt {
			tc.ownership_collect_lambda_for_in_captures(node, locals, mut names)
		}
		.for_stmt {
			tc.ownership_collect_lambda_for_captures(node, locals, mut names)
		}
		else {
			tc.ownership_collect_lambda_capture_names(id, locals, mut names)
		}
	}
}

fn (mut tc TypeChecker) ownership_collect_lambda_match_captures(node flat.Node, locals map[string]bool, mut names map[string]flat.NodeId) {
	if node.children_count == 0 {
		return
	}
	tc.ownership_collect_lambda_capture_names(tc.a.child(&node, 0), locals, mut names)
	for i in 1 .. node.children_count {
		branch_id := tc.a.child(&node, i)
		if !tc.valid_node_id(branch_id) {
			continue
		}
		branch := tc.a.nodes[int(branch_id)]
		if branch.kind != .match_branch {
			tc.ownership_collect_lambda_capture_names(branch_id, locals, mut names)
			continue
		}
		body_start := if branch.value == 'else' { 0 } else { branch.value.int() }
		for j in 0 .. body_start {
			tc.ownership_collect_lambda_capture_names(tc.a.child(&branch, j), locals, mut names)
		}
		mut branch_scope := locals.clone()
		for j in body_start .. branch.children_count {
			tc.ownership_collect_lambda_capture_stmt(tc.a.child(&branch, j), mut branch_scope, mut
				names)
		}
	}
}

fn (mut tc TypeChecker) ownership_collect_lambda_if_captures(node flat.Node, locals map[string]bool, mut names map[string]flat.NodeId) {
	if node.children_count == 0 {
		return
	}
	cond_id := tc.a.child(&node, 0)
	mut then_scope := locals.clone()
	tc.ownership_collect_lambda_condition_captures(cond_id, locals, mut then_scope, mut names)
	if node.children_count > 1 {
		tc.ownership_collect_lambda_capture_names(tc.a.child(&node, 1), then_scope, mut names)
	}
	if node.children_count > 2 {
		tc.ownership_collect_lambda_capture_names(tc.a.child(&node, 2), locals, mut names)
	}
}

fn (mut tc TypeChecker) ownership_collect_lambda_condition_captures(id flat.NodeId, locals map[string]bool, mut then_scope map[string]bool, mut names map[string]flat.NodeId) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .decl_assign {
		mut cond_scope := locals.clone()
		tc.ownership_collect_lambda_decl_assign_captures(node, mut cond_scope, mut names)
		for name, _ in cond_scope {
			if name !in locals {
				then_scope[name] = true
			}
		}
		return
	}
	if node.kind == .infix && node.op == .logical_and && node.children_count >= 2 {
		mut lhs_scope := locals.clone()
		tc.ownership_collect_lambda_condition_captures(tc.a.child(&node, 0), locals, mut lhs_scope, mut
			names)
		for name, _ in lhs_scope {
			if name !in locals {
				then_scope[name] = true
			}
		}
		tc.ownership_collect_lambda_condition_captures(tc.a.child(&node, 1), lhs_scope, mut
			then_scope, mut names)
		return
	}
	tc.ownership_collect_lambda_capture_names(id, locals, mut names)
}

fn (mut tc TypeChecker) ownership_collect_lambda_decl_assign_captures(node flat.Node, mut locals map[string]bool, mut names map[string]flat.NodeId) {
	if node.children_count == 0 {
		return
	}
	if node.children_count >= 3 {
		rhs_id := tc.a.child(&node, 1)
		if tc.resolve_type(rhs_id) is MultiReturn {
			tc.ownership_collect_lambda_capture_names(rhs_id, locals, mut names)
			tc.ownership_note_lambda_local_binding(tc.a.child(&node, 0), mut locals)
			for i in 2 .. node.children_count {
				tc.ownership_note_lambda_local_binding(tc.a.child(&node, i), mut locals)
			}
			return
		}
	}
	mut i := 0
	for i + 1 < node.children_count {
		lhs_id := tc.a.child(&node, i)
		rhs_id := tc.a.child(&node, i + 1)
		tc.ownership_collect_lambda_capture_names(rhs_id, locals, mut names)
		tc.ownership_note_lambda_local_binding(lhs_id, mut locals)
		i += 2
	}
}

fn (mut tc TypeChecker) ownership_collect_lambda_for_in_captures(node flat.Node, locals map[string]bool, mut names map[string]flat.NodeId) {
	if node.children_count < 3 {
		return
	}
	header := node.value.int()
	container_id := tc.a.child(&node, 2)
	tc.ownership_collect_lambda_capture_names(container_id, locals, mut names)
	mut loop_scope := locals.clone()
	tc.ownership_note_lambda_local_binding(tc.a.child(&node, 0), mut loop_scope)
	tc.ownership_note_lambda_local_binding(tc.a.child(&node, 1), mut loop_scope)
	for i in header .. node.children_count {
		tc.ownership_collect_lambda_capture_stmt(tc.a.child(&node, i), mut loop_scope, mut names)
	}
}

fn (mut tc TypeChecker) ownership_collect_lambda_for_captures(node flat.Node, locals map[string]bool, mut names map[string]flat.NodeId) {
	mut loop_scope := locals.clone()
	if node.children_count > 0 {
		init_id := tc.a.child(&node, 0)
		if tc.valid_node_id(init_id) {
			init := tc.a.nodes[int(init_id)]
			if init.kind == .decl_assign {
				tc.ownership_collect_lambda_decl_assign_captures(init, mut loop_scope, mut names)
			} else {
				tc.ownership_collect_lambda_capture_names(init_id, loop_scope, mut names)
			}
		}
	}
	if node.children_count > 1 {
		cond_id := tc.a.child(&node, 1)
		if tc.valid_node_id(cond_id) {
			tc.ownership_collect_lambda_capture_names(cond_id, loop_scope, mut names)
		}
	}
	if node.children_count > 2 {
		post_id := tc.a.child(&node, 2)
		if tc.valid_node_id(post_id) {
			tc.ownership_collect_lambda_capture_names(post_id, loop_scope, mut names)
		}
	}
	for i in 3 .. node.children_count {
		tc.ownership_collect_lambda_capture_stmt(tc.a.child(&node, i), mut loop_scope, mut names)
	}
}

fn (mut tc TypeChecker) ownership_note_lambda_local_binding(id flat.NodeId, mut locals map[string]bool) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .ident && node.value.len > 0 && node.value != '_' {
		locals[node.value] = true
	}
}

fn (mut tc TypeChecker) ownership_end_fn() {
	if tc.ownership_checks_suppressed() {
		return
	}
	mut st := tc.ownership_state()
	if st.frames.len == 0 {
		return
	}
	if st.cur_fn.len > 0 {
		entries := tc.ownership_live_drop_entries()
		if entries.len > 0 {
			existing := st.drop_at_fn_exit[st.cur_fn] or { []OwnershipDropEntry{} }
			merged := ownership_merge_drop_entries(existing, entries)
			st.drop_at_fn_exit[st.cur_fn] = merged
			tc.ownership_note_drop_types(st.cur_fn, merged)
		}
	}
	frame := st.frames.pop()
	st.cur_fn = frame.cur_fn
	st.owned_vars = frame.owned_vars.clone()
	st.owned_var_types = frame.owned_var_types.clone()
	st.moved_vars = frame.moved_vars.clone()
	st.borrowed_vars = frame.borrowed_vars.clone()
	st.array_lengths = frame.array_lengths.clone()
	st.ownership_fn_value_vars = frame.fn_value_vars.clone()
	st.scope_frames = ownership_clone_scope_frames(frame.scope_frames)
	st.path_active = frame.path_active
}

fn (mut tc TypeChecker) ownership_snapshot_frame() OwnershipFrame {
	st := tc.ownership_state()
	return OwnershipFrame{
		cur_fn:          st.cur_fn
		owned_vars:      st.owned_vars.clone()
		owned_var_types: st.owned_var_types.clone()
		moved_vars:      st.moved_vars.clone()
		borrowed_vars:   st.borrowed_vars.clone()
		array_lengths:   st.array_lengths.clone()
		fn_value_vars:   st.ownership_fn_value_vars.clone()
		scope_frames:    ownership_clone_scope_frames(st.scope_frames)
		path_active:     st.path_active
	}
}

fn (mut tc TypeChecker) ownership_restore_frame(frame OwnershipFrame) {
	mut st := tc.ownership_state()
	st.cur_fn = frame.cur_fn
	st.owned_vars = frame.owned_vars.clone()
	st.owned_var_types = frame.owned_var_types.clone()
	st.moved_vars = frame.moved_vars.clone()
	st.borrowed_vars = frame.borrowed_vars.clone()
	st.array_lengths = frame.array_lengths.clone()
	st.ownership_fn_value_vars = frame.fn_value_vars.clone()
	st.scope_frames = ownership_clone_scope_frames(frame.scope_frames)
	st.path_active = frame.path_active
}

fn (mut tc TypeChecker) ownership_begin_branch_group() {
	if tc.ownership_checks_suppressed() {
		return
	}
	tc.ownership_begin_branch_group_with_mode(false, false)
}

fn (mut tc TypeChecker) ownership_begin_value_branch_group() {
	if tc.ownership_checks_suppressed() {
		return
	}
	tc.ownership_begin_branch_group_with_mode(false, true)
}

fn (mut tc TypeChecker) ownership_begin_loop_branch_group() {
	if tc.ownership_checks_suppressed() {
		return
	}
	mut label := ''
	{
		mut st := tc.ownership_state()
		label = st.pending_loop_label
		st.pending_loop_label = ''
	}
	tc.ownership_begin_branch_group_with_label(true, false, label)
}

fn (mut tc TypeChecker) ownership_begin_branch_group_with_mode(is_loop bool, value_context bool) {
	tc.ownership_begin_branch_group_with_label(is_loop, value_context, '')
}

fn (mut tc TypeChecker) ownership_begin_branch_group_with_label(is_loop bool, value_context bool, label string) {
	if tc.ownership_checks_suppressed() {
		return
	}
	base := tc.ownership_snapshot_frame()
	tc.ownership_state().branch_groups << OwnershipBranchGroup{
		base:          base
		branches:      []OwnershipFrame{}
		continues:     []OwnershipFrame{}
		saw_else:      false
		is_loop:       is_loop
		value_context: value_context
		label:         label
	}
}

fn (mut tc TypeChecker) ownership_add_branch_group_base() {
	if tc.ownership_checks_suppressed() {
		return
	}
	mut st := tc.ownership_state()
	if st.branch_groups.len == 0 {
		return
	}
	group_idx := st.branch_groups.len - 1
	st.branch_groups[group_idx].branches << st.branch_groups[group_idx].base
}

fn (mut tc TypeChecker) ownership_note_branch_group_else() {
	if tc.ownership_checks_suppressed() {
		return
	}
	mut st := tc.ownership_state()
	if st.branch_groups.len == 0 {
		return
	}
	group_idx := st.branch_groups.len - 1
	st.branch_groups[group_idx].saw_else = true
}

fn (mut tc TypeChecker) ownership_add_branch_group_base_if_no_else() {
	if tc.ownership_checks_suppressed() {
		return
	}
	mut st := tc.ownership_state()
	if st.branch_groups.len == 0 {
		return
	}
	group_idx := st.branch_groups.len - 1
	if st.branch_groups[group_idx].saw_else {
		return
	}
	st.branch_groups[group_idx].branches << st.branch_groups[group_idx].base
}

fn (mut tc TypeChecker) ownership_begin_branch() {
	if tc.ownership_checks_suppressed() {
		return
	}
	mut base := OwnershipFrame{}
	{
		st := tc.ownership_state()
		if st.branch_groups.len == 0 {
			return
		}
		base = st.branch_groups[st.branch_groups.len - 1].base
	}
	tc.ownership_restore_frame(base)
}

fn (mut tc TypeChecker) ownership_end_branch(branch_id flat.NodeId) {
	if tc.ownership_checks_suppressed() {
		return
	}
	snapshot := tc.ownership_snapshot_frame()
	continues := snapshot.path_active && tc.ownership_branch_continues(branch_id)
	mut base := OwnershipFrame{}
	{
		mut st := tc.ownership_state()
		if st.branch_groups.len == 0 {
			return
		}
		mut group := st.branch_groups.pop()
		if continues {
			group.branches << snapshot
		}
		base = group.base
		st.branch_groups << group
	}
	tc.ownership_restore_frame(base)
}

fn (tc &TypeChecker) ownership_expr_may_consume_array_element_method(expr_id flat.NodeId) bool {
	call_id := tc.ownership_unwrap_expr(expr_id)
	if !tc.valid_node_id(call_id) {
		return false
	}
	node := tc.a.nodes[int(call_id)]
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	fn_node := tc.a.child_node(&node, 0)
	return fn_node.kind == .selector && fn_node.children_count > 0
		&& fn_node.value in ['first', 'last', 'pop', 'pop_left']
}

fn (mut tc TypeChecker) ownership_after_stmt_node(id flat.NodeId) {
	if tc.ownership_checks_suppressed() || !tc.valid_node_id(id) {
		return
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.expr_stmt {
			if node.children_count > 0 {
				expr_id := tc.a.child(&node, 0)
				if tc.ownership_expr_may_consume_array_element_method(expr_id) {
					tc.ownership_consume_array_element_method_result(expr_id,
						'discarded expression', id)
				}
			}
		}
		.label_stmt {
			tc.ownership_state().pending_loop_label = node.value
			return
		}
		.break_stmt {
			tc.ownership_add_loop_exit_snapshot(node.value)
			mut st := tc.ownership_state()
			st.pending_loop_label = ''
			st.path_active = false
			return
		}
		.continue_stmt {
			tc.ownership_add_loop_continue_snapshot(node.value)
			mut st := tc.ownership_state()
			st.pending_loop_label = ''
			st.path_active = false
			return
		}
		else {}
	}

	if tc.ownership != unsafe { nil } && tc.ownership.pending_loop_label.len > 0 {
		tc.ownership.pending_loop_label = ''
	}
}

fn ownership_loop_group_matches(group OwnershipBranchGroup, label string) bool {
	return group.is_loop && (label.len == 0 || group.label == label)
}

fn (mut tc TypeChecker) ownership_add_loop_exit_snapshot(label string) {
	if tc.ownership_effects_disabled() {
		return
	}
	snapshot := tc.ownership_snapshot_frame()
	tc.ownership_add_loop_branch_frame(snapshot, label)
}

fn (mut tc TypeChecker) ownership_add_loop_continue_snapshot(label string) {
	if tc.ownership_effects_disabled() {
		return
	}
	snapshot := tc.ownership_snapshot_frame()
	mut st := tc.ownership_state()
	for i := st.branch_groups.len; i > 0; i-- {
		group_idx := i - 1
		if ownership_loop_group_matches(st.branch_groups[group_idx], label) {
			base := st.branch_groups[group_idx].base
			tc.ownership_record_loop_control_drops(base, snapshot)
			st.branch_groups[group_idx].continues << ownership_frame_without_loop_locals(base,
				snapshot)
			return
		}
	}
}

fn (mut tc TypeChecker) ownership_add_loop_branch_frame(frame OwnershipFrame, label string) {
	if tc.ownership_checks_suppressed() {
		return
	}
	mut st := tc.ownership_state()
	for i := st.branch_groups.len; i > 0; i-- {
		group_idx := i - 1
		if ownership_loop_group_matches(st.branch_groups[group_idx], label) {
			base := st.branch_groups[group_idx].base
			if label.len > 0 {
				tc.ownership_record_labelled_loop_break_drops(base, frame)
			} else {
				tc.ownership_record_loop_control_drops(base, frame)
			}
			st.branch_groups[group_idx].branches << ownership_frame_without_loop_locals(base, frame)
			return
		}
	}
}

fn (mut tc TypeChecker) ownership_add_loop_continuation_frame(frame OwnershipFrame) {
	mut st := tc.ownership_state()
	for i := st.branch_groups.len; i > 0; i-- {
		group_idx := i - 1
		if st.branch_groups[group_idx].is_loop {
			st.branch_groups[group_idx].branches << frame
			return
		}
	}
}

fn (mut tc TypeChecker) ownership_record_current_loop_iteration_drops() {
	// Keep one snapshot for every syntactic loop, including an unreachable
	// iteration tail after an unconditional break or continue. C generation
	// consumes these snapshots in source order, so omitting an empty snapshot
	// shifts every later loop's destructor list.
	if tc.ownership_checks_suppressed() {
		return
	}
	snapshot := tc.ownership_snapshot_frame()
	mut base := OwnershipFrame{}
	mut found := false
	{
		st := tc.ownership_state()
		for i := st.branch_groups.len; i > 0; i-- {
			group_idx := i - 1
			if st.branch_groups[group_idx].is_loop {
				base = st.branch_groups[group_idx].base
				found = true
				break
			}
		}
	}
	if !found {
		return
	}
	entries := tc.ownership_drop_entries_since_frame(base, snapshot)
	mut st := tc.ownership_state()
	index := st.drop_loop_iteration_counts[st.cur_fn] or { 0 }
	st.drop_loop_iteration_counts[st.cur_fn] = index + 1
	st.drop_at_loop_iterations['${st.cur_fn}\x01${index}'] = entries
	tc.ownership_note_drop_types(st.cur_fn, entries)
}

fn (mut tc TypeChecker) ownership_record_loop_control_drops(base OwnershipFrame, snapshot OwnershipFrame) {
	entries := tc.ownership_drop_entries_since_frame(base, snapshot)
	tc.ownership_record_loop_control_entries(entries)
}

fn (mut tc TypeChecker) ownership_record_labelled_loop_break_drops(base OwnershipFrame, snapshot OwnershipFrame) {
	mut entries := tc.ownership_drop_entries_since_frame(base, snapshot)
	entries << tc.ownership_drop_entries_for_loop_base_scope(base, snapshot)
	tc.ownership_record_loop_control_entries(entries)
}

fn (mut tc TypeChecker) ownership_record_loop_control_entries(entries []OwnershipDropEntry) {
	mut st := tc.ownership_state()
	index := st.drop_loop_control_counts[st.cur_fn] or { 0 }
	st.drop_loop_control_counts[st.cur_fn] = index + 1
	st.drop_at_loop_controls['${st.cur_fn}\x01${index}'] = entries
	tc.ownership_note_drop_types(st.cur_fn, entries)
}

fn (mut tc TypeChecker) ownership_drop_entries_for_loop_base_scope(base OwnershipFrame, snapshot OwnershipFrame) []OwnershipDropEntry {
	if base.scope_frames.len == 0 {
		return []OwnershipDropEntry{}
	}
	scope := base.scope_frames[base.scope_frames.len - 1]
	mut entries := []OwnershipDropEntry{}
	for i := scope.decl_order.len; i > 0; i-- {
		name := scope.decl_order[i - 1]
		base_pos := base.owned_vars[name] or { continue }
		snapshot_pos := snapshot.owned_vars[name] or { continue }
		if snapshot_pos != base_pos || name in snapshot.moved_vars {
			continue
		}
		type_name := snapshot.owned_var_types[name] or { continue }
		if target := tc.ownership_drop_target_for_type_name(type_name) {
			entries << OwnershipDropEntry{
				name:             name
				type_name:        target.type_name
				optional_wrapper: target.optional_wrapper
			}
		}
	}
	return entries
}

fn (mut tc TypeChecker) ownership_drop_entries_since_frame(base OwnershipFrame, snapshot OwnershipFrame) []OwnershipDropEntry {
	mut candidates := []OwnershipDropCandidate{}
	for name, pos in snapshot.owned_vars {
		base_pos := base.owned_vars[name] or { flat.NodeId(-1) }
		if pos != base_pos && name !in snapshot.moved_vars && !name.contains('.')
			&& !name.contains('[') {
			candidates << OwnershipDropCandidate{
				name: name
				pos:  int(pos)
			}
		}
	}
	candidates.sort(a.pos > b.pos)
	mut entries := []OwnershipDropEntry{}
	for candidate in candidates {
		type_name := snapshot.owned_var_types[candidate.name] or { continue }
		if target := tc.ownership_drop_target_for_type_name(type_name) {
			entries << OwnershipDropEntry{
				name:             candidate.name
				type_name:        target.type_name
				optional_wrapper: target.optional_wrapper
			}
		}
	}
	return entries
}

fn ownership_frame_without_loop_locals(base OwnershipFrame, snapshot OwnershipFrame) OwnershipFrame {
	mut local_names := []string{}
	for name, pos in snapshot.owned_vars {
		base_pos := base.owned_vars[name] or { flat.NodeId(-1) }
		if pos != base_pos {
			local_names << name
		}
	}
	for name in snapshot.moved_vars.keys() {
		if name !in base.owned_vars && name !in local_names {
			local_names << name
		}
	}
	mut owned_vars := snapshot.owned_vars.clone()
	mut owned_var_types := snapshot.owned_var_types.clone()
	mut moved_vars := snapshot.moved_vars.clone()
	mut borrowed_vars := snapshot.borrowed_vars.clone()
	mut array_lengths := snapshot.array_lengths.clone()
	mut fn_value_vars := snapshot.fn_value_vars.clone()
	for name in local_names {
		owned_vars.delete(name)
		owned_var_types.delete(name)
		moved_vars.delete(name)
		borrowed_vars.delete(name)
		array_lengths.delete(name)
		fn_value_vars.delete(name)
	}
	return OwnershipFrame{
		cur_fn:          snapshot.cur_fn
		owned_vars:      owned_vars
		owned_var_types: owned_var_types
		moved_vars:      moved_vars
		borrowed_vars:   borrowed_vars
		array_lengths:   array_lengths
		fn_value_vars:   fn_value_vars
		scope_frames:    ownership_clone_scope_frames(base.scope_frames)
		path_active:     snapshot.path_active
	}
}

fn (mut tc TypeChecker) ownership_take_loop_continue_snapshots() []OwnershipFrame {
	if tc.ownership_checks_suppressed() {
		return []OwnershipFrame{}
	}
	mut st := tc.ownership_state()
	for i := st.branch_groups.len; i > 0; i-- {
		group_idx := i - 1
		if st.branch_groups[group_idx].is_loop {
			frames := st.branch_groups[group_idx].continues.clone()
			st.branch_groups[group_idx].continues = []OwnershipFrame{}
			return frames
		}
	}
	return []OwnershipFrame{}
}

fn (mut tc TypeChecker) ownership_merge_loop_continue_snapshots() {
	if tc.ownership_checks_suppressed() {
		return
	}
	frames := tc.ownership_take_loop_continue_snapshots()
	for frame in frames {
		tc.ownership_add_loop_continuation_frame(frame)
	}
}

fn (mut tc TypeChecker) ownership_apply_loop_continue_snapshots(post_id flat.NodeId) {
	if tc.ownership_checks_suppressed() {
		return
	}
	frames := tc.ownership_take_loop_continue_snapshots()
	if frames.len == 0 {
		return
	}
	saved := tc.ownership_snapshot_frame()
	for frame in frames {
		tc.ownership_restore_frame(frame)
		if tc.valid_node_id(post_id) {
			tc.check_node(post_id)
		}
		tc.ownership_add_loop_continuation_frame(tc.ownership_snapshot_frame())
	}
	tc.ownership_restore_frame(saved)
}

fn (mut tc TypeChecker) ownership_end_loop_branch(node flat.Node, body_start int) {
	if tc.ownership_checks_suppressed() {
		return
	}
	snapshot := tc.ownership_snapshot_frame()
	continues := snapshot.path_active
		&& !tc.ownership_statement_sequence_definitely_returns(node, body_start)
	mut base := OwnershipFrame{}
	{
		mut st := tc.ownership_state()
		if st.branch_groups.len == 0 {
			return
		}
		mut group := st.branch_groups.pop()
		if continues {
			group.branches << ownership_frame_without_loop_locals(group.base, snapshot)
		}
		base = group.base
		st.branch_groups << group
	}
	tc.ownership_restore_frame(base)
}

fn (tc &TypeChecker) ownership_statement_sequence_definitely_returns(node flat.Node, body_start int) bool {
	start := if body_start < 0 { 0 } else { body_start }
	if start >= node.children_count {
		return false
	}
	for i in start .. node.children_count {
		if tc.stmt_definitely_returns(tc.a.child(&node, i)) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) ownership_statement_sequence_may_break(node flat.Node, body_start int) bool {
	start := if body_start < 0 { 0 } else { body_start }
	if start >= node.children_count {
		return false
	}
	for i in start .. node.children_count {
		stmt_id := tc.a.child(&node, i)
		if tc.ownership_stmt_may_break(stmt_id) {
			return true
		}
		if tc.ownership_stmt_definitely_exits_before_loop_post(stmt_id) {
			return false
		}
	}
	return false
}

fn (tc &TypeChecker) ownership_statement_sequence_can_reach_loop_post(node flat.Node, body_start int) bool {
	start := if body_start < 0 { 0 } else { body_start }
	if start >= node.children_count {
		return true
	}
	for i in start .. node.children_count {
		if tc.ownership_stmt_definitely_exits_before_loop_post(tc.a.child(&node, i)) {
			return false
		}
	}
	return true
}

fn (tc &TypeChecker) ownership_statement_sequence_can_continue(node flat.Node, body_start int) bool {
	start := if body_start < 0 { 0 } else { body_start }
	if start >= node.children_count {
		return true
	}
	for i in start .. node.children_count {
		if tc.ownership_stmt_definitely_exits_statement_sequence(tc.a.child(&node, i)) {
			return false
		}
	}
	return true
}

fn (tc &TypeChecker) ownership_statement_sequence_definitely_breaks(node flat.Node, body_start int) bool {
	start := if body_start < 0 { 0 } else { body_start }
	if start >= node.children_count {
		return false
	}
	for i in start .. node.children_count {
		stmt_id := tc.a.child(&node, i)
		if tc.ownership_stmt_definitely_breaks_statement_sequence(stmt_id) {
			return true
		}
		if tc.ownership_stmt_definitely_exits_statement_sequence(stmt_id) {
			return false
		}
	}
	return false
}

fn (tc &TypeChecker) ownership_stmt_definitely_exits_statement_sequence(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.break_stmt, .continue_stmt, .return_stmt {
			return true
		}
		.for_stmt, .for_in_stmt, .fn_literal, .lambda_expr {
			return false
		}
		.block {
			return !tc.ownership_statement_sequence_can_continue(node, 0)
		}
		.if_expr {
			if node.children_count < 3 {
				return false
			}
			return tc.ownership_stmt_definitely_exits_statement_sequence(tc.a.child(&node, 1))
				&& tc.ownership_stmt_definitely_exits_statement_sequence(tc.a.child(&node, 2))
		}
		.match_stmt {
			if node.children_count < 2 {
				return false
			}
			mut has_else := false
			for i in 1 .. node.children_count {
				branch_id := tc.a.child(&node, i)
				if !tc.valid_node_id(branch_id) {
					return false
				}
				branch := tc.a.nodes[int(branch_id)]
				if branch.kind != .match_branch {
					return false
				}
				if branch.value == 'else' {
					has_else = true
				}
				if !tc.ownership_match_branch_definitely_exits_statement_sequence(branch) {
					return false
				}
			}
			return has_else || tc.match_covers_all_variants(node)
		}
		else {}
	}

	return false
}

fn (tc &TypeChecker) ownership_stmt_definitely_breaks_statement_sequence(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.break_stmt, .continue_stmt {
			return true
		}
		.return_stmt, .for_stmt, .for_in_stmt, .fn_literal, .lambda_expr {
			return false
		}
		.block {
			return tc.ownership_statement_sequence_definitely_breaks(node, 0)
		}
		.if_expr {
			if node.children_count < 3 {
				return false
			}
			return tc.ownership_stmt_definitely_breaks_statement_sequence(tc.a.child(&node, 1))
				&& tc.ownership_stmt_definitely_breaks_statement_sequence(tc.a.child(&node, 2))
		}
		.match_stmt {
			if node.children_count < 2 {
				return false
			}
			mut has_else := false
			for i in 1 .. node.children_count {
				branch_id := tc.a.child(&node, i)
				if !tc.valid_node_id(branch_id) {
					return false
				}
				branch := tc.a.nodes[int(branch_id)]
				if branch.kind != .match_branch {
					return false
				}
				if branch.value == 'else' {
					has_else = true
				}
				if !tc.ownership_match_branch_definitely_breaks_statement_sequence(branch) {
					return false
				}
			}
			return has_else || tc.match_covers_all_variants(node)
		}
		else {}
	}

	return false
}

fn (tc &TypeChecker) ownership_stmt_may_break(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.break_stmt {
			return true
		}
		.for_stmt, .for_in_stmt, .fn_literal, .lambda_expr {
			return false
		}
		.block {
			return tc.ownership_statement_sequence_may_break(node, 0)
		}
		.if_expr {
			for i in 1 .. node.children_count {
				if tc.ownership_stmt_may_break(tc.a.child(&node, i)) {
					return true
				}
			}
			return false
		}
		.match_stmt {
			for i in 1 .. node.children_count {
				branch_id := tc.a.child(&node, i)
				if !tc.valid_node_id(branch_id) {
					continue
				}
				branch := tc.a.nodes[int(branch_id)]
				if branch.kind == .match_branch && tc.ownership_match_branch_may_break(branch) {
					return true
				}
			}
			return false
		}
		else {}
	}

	for i in 0 .. node.children_count {
		if tc.ownership_stmt_may_break(tc.a.child(&node, i)) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) ownership_stmt_definitely_exits_before_loop_post(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.break_stmt, .return_stmt {
			return true
		}
		.continue_stmt {
			return false
		}
		.for_stmt, .for_in_stmt, .fn_literal, .lambda_expr {
			return false
		}
		.block {
			return !tc.ownership_statement_sequence_can_reach_loop_post(node, 0)
		}
		.if_expr {
			if node.children_count < 3 {
				return false
			}
			return tc.ownership_stmt_definitely_exits_before_loop_post(tc.a.child(&node, 1))
				&& tc.ownership_stmt_definitely_exits_before_loop_post(tc.a.child(&node, 2))
		}
		.match_stmt {
			if node.children_count < 2 {
				return false
			}
			mut has_else := false
			for i in 1 .. node.children_count {
				branch_id := tc.a.child(&node, i)
				if !tc.valid_node_id(branch_id) {
					return false
				}
				branch := tc.a.nodes[int(branch_id)]
				if branch.kind != .match_branch {
					return false
				}
				if branch.value == 'else' {
					has_else = true
				}
				if !tc.ownership_match_branch_definitely_exits_before_loop_post(branch) {
					return false
				}
			}
			return has_else || tc.match_covers_all_variants(node)
		}
		else {}
	}

	return false
}

fn (tc &TypeChecker) ownership_match_branch_may_break(branch flat.Node) bool {
	body_start := if branch.value == 'else' { 0 } else { branch.value.int() }
	return tc.ownership_statement_sequence_may_break(branch, body_start)
}

fn (tc &TypeChecker) ownership_match_branch_definitely_exits_before_loop_post(branch flat.Node) bool {
	body_start := if branch.value == 'else' { 0 } else { branch.value.int() }
	return !tc.ownership_statement_sequence_can_reach_loop_post(branch, body_start)
}

fn (tc &TypeChecker) ownership_match_branch_definitely_exits_statement_sequence(branch flat.Node) bool {
	body_start := if branch.value == 'else' { 0 } else { branch.value.int() }
	return !tc.ownership_statement_sequence_can_continue(branch, body_start)
}

fn (tc &TypeChecker) ownership_match_branch_definitely_breaks_statement_sequence(branch flat.Node) bool {
	body_start := if branch.value == 'else' { 0 } else { branch.value.int() }
	return tc.ownership_statement_sequence_definitely_breaks(branch, body_start)
}

fn (mut tc TypeChecker) ownership_bind_for_in_vars(key_id flat.NodeId, val_id flat.NodeId, container_id flat.NodeId, has_val bool) {
	if tc.ownership_effects_disabled() {
		return
	}
	container_name := tc.ownership_expr_ident_name(container_id)
	clean := unwrap_pointer(tc.resolve_type(container_id))
	if clean is Map && has_val {
		key_name := tc.ownership_lhs_name(key_id)
		key_source := if container_name.len > 0 {
			ownership_map_key_storage_name(container_name)
		} else {
			''
		}
		tc.ownership_bind_for_in_var_from_storage(key_name, key_source, key_id)
	}
	target_id := if has_val { val_id } else { key_id }
	target_name := tc.ownership_lhs_name(target_id)
	source_name := if container_name.len > 0 { '${container_name}[*]' } else { '' }
	tc.ownership_bind_for_in_var_from_storage(target_name, source_name, target_id)
}

fn (mut tc TypeChecker) ownership_bind_for_in_var_from_storage(target_name string, source_name string, target_id flat.NodeId) bool {
	if target_name.len == 0 || target_name == '_' {
		return false
	}
	tc.ownership_note_decl(target_name)
	tc.ownership_release_borrower(target_name)
	moved_types := if source_name.len > 0 {
		tc.ownership_move_overlapping_dynamic_storage(source_name, target_name, target_id, false,
			'', true)
	} else {
		[]string{}
	}
	tc.ownership_clear_descendant_state(target_name)
	{
		mut st := tc.ownership_state()
		st.owned_vars.delete(target_name)
		st.owned_var_types.delete(target_name)
		st.moved_vars.delete(target_name)
		st.ownership_fn_value_vars.delete(target_name)
	}
	if moved_types.len == 0 {
		return false
	}
	tc.ownership_mark_owned_name(target_name, moved_types[0], target_id)
	return true
}

fn (tc &TypeChecker) ownership_branch_continues(branch_id flat.NodeId) bool {
	if !tc.valid_node_id(branch_id) {
		return true
	}
	node := tc.a.nodes[int(branch_id)]
	if node.kind == .match_branch {
		return !tc.ownership_match_branch_definitely_exits_statement_sequence(node)
	}
	if node.kind == .select_branch {
		return !tc.ownership_select_branch_definitely_exits_statement_sequence(node)
	}
	return !tc.ownership_stmt_definitely_exits_statement_sequence(branch_id)
}

fn (tc &TypeChecker) ownership_branch_breaks_loop(branch_id flat.NodeId) bool {
	if !tc.valid_node_id(branch_id) {
		return false
	}
	node := tc.a.nodes[int(branch_id)]
	if node.kind == .match_branch {
		return tc.ownership_match_branch_definitely_breaks_statement_sequence(node)
	}
	if node.kind == .select_branch {
		return tc.ownership_select_branch_definitely_breaks_statement_sequence(node)
	}
	return tc.ownership_stmt_definitely_breaks_statement_sequence(branch_id)
}

fn (tc &TypeChecker) ownership_select_branch_body_start(branch flat.Node) int {
	return if branch.value == 'recv' && branch.children_count >= 2 { 2 } else { 0 }
}

fn (tc &TypeChecker) ownership_select_branch_definitely_exits_statement_sequence(branch flat.Node) bool {
	return !tc.ownership_statement_sequence_can_continue(branch,
		tc.ownership_select_branch_body_start(branch))
}

fn (tc &TypeChecker) ownership_select_branch_definitely_breaks_statement_sequence(branch flat.Node) bool {
	return tc.ownership_statement_sequence_definitely_breaks(branch,
		tc.ownership_select_branch_body_start(branch))
}

fn (mut tc TypeChecker) ownership_end_branch_group() {
	if tc.ownership_checks_suppressed() {
		return
	}
	mut group := OwnershipBranchGroup{}
	{
		mut st := tc.ownership_state()
		if st.branch_groups.len == 0 {
			return
		}
		group = st.branch_groups.pop()
	}
	tc.ownership_restore_frame(group.base)
	tc.ownership_merge_branch_array_lengths(group)
	tc.ownership_merge_branch_fn_value_vars(group)
	tc.ownership_merge_branch_owned(group)
	tc.ownership_merge_branch_scope_frames(group)
	if group.branches.len == 0 {
		tc.ownership_state().path_active = false
	}
	if group.value_context {
		tc.ownership_state().pending_value_branch_groups << group
		return
	}
	tc.ownership_merge_branch_moved(group)
	tc.ownership_merge_branch_borrows(group)
	if group.is_loop {
		merged := tc.ownership_snapshot_frame()
		tc.ownership_restore_frame(ownership_frame_without_loop_locals(group.base, merged))
	}
}

fn (mut tc TypeChecker) ownership_merge_branch_scope_frames(group OwnershipBranchGroup) {
	if group.branches.len == 0 {
		return
	}
	mut st := tc.ownership_state()
	for branch in group.branches {
		for idx, branch_scope in branch.scope_frames {
			if idx >= st.scope_frames.len || idx >= group.base.scope_frames.len {
				continue
			}
			if st.scope_frames[idx].cur_fn != branch_scope.cur_fn
				|| group.base.scope_frames[idx].cur_fn != branch_scope.cur_fn {
				continue
			}
			base_defers := group.base.scope_frames[idx].defer_stmts
			for defer_id in branch_scope.defer_stmts {
				if defer_id in base_defers || defer_id in st.scope_frames[idx].defer_stmts {
					continue
				}
				st.scope_frames[idx].defer_stmts << defer_id
			}
		}
	}
}

fn (mut tc TypeChecker) ownership_merge_branch_moved(group OwnershipBranchGroup) {
	mut st := tc.ownership_state()
	for branch in group.branches {
		for name, info in branch.moved_vars {
			if name in group.base.owned_vars || name in group.base.moved_vars
				|| name in branch.owned_vars {
				st.moved_vars[name] = info
			}
		}
	}
}

fn (mut tc TypeChecker) ownership_merge_branch_borrows(group OwnershipBranchGroup) {
	mut st := tc.ownership_state()
	for branch in group.branches {
		for name, borrows in branch.borrowed_vars {
			if name !in group.base.owned_vars && name !in group.base.borrowed_vars
				&& !tc.ownership_storage_participates(name) {
				continue
			}
			mut merged := st.borrowed_vars[name] or { []BorrowInfo{} }
			for borrow in borrows {
				mut exists := false
				for existing in merged {
					if existing.borrower == borrow.borrower && existing.pos == borrow.pos
						&& existing.is_mut == borrow.is_mut {
						exists = true
						break
					}
				}
				if !exists {
					merged << borrow
				}
			}
			if merged.len > 0 {
				st.borrowed_vars[name] = merged
			}
		}
	}
}

fn (mut tc TypeChecker) ownership_flush_value_branch_moves() {
	mut groups := []OwnershipBranchGroup{}
	{
		mut st := tc.ownership_state()
		if st.pending_value_branch_groups.len == 0 {
			return
		}
		groups = st.pending_value_branch_groups.clone()
		st.pending_value_branch_groups = []OwnershipBranchGroup{}
	}
	if tc.ownership_effects_disabled() {
		return
	}
	for group in groups {
		tc.ownership_merge_branch_moved(group)
		tc.ownership_merge_branch_borrows(group)
	}
}

fn (mut tc TypeChecker) ownership_merge_branch_array_lengths(group OwnershipBranchGroup) {
	if group.branches.len == 0 {
		return
	}
	mut names := map[string]bool{}
	for name, _ in group.base.array_lengths {
		names[name] = true
	}
	for branch in group.branches {
		for name, _ in branch.array_lengths {
			names[name] = true
		}
	}
	mut st := tc.ownership_state()
	for name, _ in names {
		mut has_candidate := false
		mut candidate := 0
		mut all_same := true
		for branch in group.branches {
			length := branch.array_lengths[name] or {
				all_same = false
				break
			}
			if !has_candidate {
				candidate = length
				has_candidate = true
				continue
			}
			if length != candidate {
				all_same = false
				break
			}
		}
		if all_same && has_candidate {
			st.array_lengths[name] = candidate
		} else {
			st.array_lengths.delete(name)
		}
	}
}

fn (mut tc TypeChecker) ownership_merge_branch_fn_value_vars(group OwnershipBranchGroup) {
	if group.branches.len == 0 {
		return
	}
	mut names := map[string]bool{}
	for name, _ in group.base.fn_value_vars {
		names[name] = true
	}
	for branch in group.branches {
		for name, _ in branch.fn_value_vars {
			names[name] = true
		}
	}
	mut st := tc.ownership_state()
	for name, _ in names {
		mut has_candidate := false
		mut candidate := ''
		mut all_same := true
		for branch in group.branches {
			fn_name := branch.fn_value_vars[name] or {
				all_same = false
				break
			}
			if !has_candidate {
				candidate = fn_name
				has_candidate = true
				continue
			}
			if fn_name != candidate {
				all_same = false
				break
			}
		}
		if all_same && has_candidate {
			st.ownership_fn_value_vars[name] = candidate
		} else {
			st.ownership_fn_value_vars.delete(name)
		}
	}
}

fn (mut tc TypeChecker) ownership_merge_branch_owned(group OwnershipBranchGroup) {
	tc.ownership_clear_branch_overwritten_owned(group)
	for branch in group.branches {
		for name, pos in branch.owned_vars {
			if name in branch.moved_vars {
				continue
			}
			type_name := branch.owned_var_types[name] or { 'string' }
			tc.ownership_mark_owned_name(name, type_name, pos)
		}
	}
}

fn (mut tc TypeChecker) ownership_clear_branch_overwritten_owned(group OwnershipBranchGroup) {
	if group.branches.len == 0 {
		return
	}
	mut st := tc.ownership_state()
	for name, _ in group.base.owned_vars {
		mut cleared := true
		for branch in group.branches {
			if name in branch.owned_vars || name in branch.moved_vars {
				cleared = false
				break
			}
		}
		if cleared {
			st.owned_vars.delete(name)
			st.owned_var_types.delete(name)
			st.moved_vars.delete(name)
		}
	}
}

fn (mut tc TypeChecker) ownership_check_ident(id flat.NodeId, _node flat.Node) {
	if tc.ownership_effects_disabled() {
		return
	}
	tc.ownership_check_expr(id)
}

fn (mut tc TypeChecker) ownership_check_expr(id flat.NodeId) {
	if tc.ownership_effects_disabled() {
		return
	}
	name := tc.ownership_expr_ident_name(id)
	if name.len == 0 {
		return
	}
	if moved := tc.ownership_moved_conflict(name) {
		tc.ownership_report_moved(moved.name, moved.info, id)
	}
}

fn (mut tc TypeChecker) ownership_after_decl_assign(lhs_id flat.NodeId, rhs_id flat.NodeId, lhs_type Type, assign_id flat.NodeId) {
	if tc.ownership_effects_disabled() {
		return
	}
	lhs_name := tc.ownership_lhs_name(lhs_id)
	if lhs_name.len == 0 {
		return
	}
	if lhs_name == '_' {
		tc.ownership_consume_expr(rhs_id, 'blank identifier', assign_id)
		return
	}
	tc.ownership_note_decl(lhs_name)
	if tc.ownership_assign_shadowing_same_name(lhs_name, rhs_id, lhs_type, assign_id) {
		tc.ownership_track_fn_value_binding(lhs_name, rhs_id)
		return
	}
	tc.ownership_assign_to_name(lhs_name, rhs_id, lhs_type, assign_id)
	tc.ownership_track_fn_value_binding(lhs_name, rhs_id)
}

fn (mut tc TypeChecker) ownership_after_multi_return_decl_assign(lhs_ids []flat.NodeId, rhs_id flat.NodeId, rhs_type MultiReturn, assign_id flat.NodeId) {
	if tc.ownership_effects_disabled() {
		return
	}
	tc.ownership_after_multi_return_assign_impl(lhs_ids, rhs_id, rhs_type, assign_id, true)
}

fn (mut tc TypeChecker) ownership_after_multi_return_assign(lhs_ids []flat.NodeId, rhs_id flat.NodeId, rhs_type MultiReturn, assign_id flat.NodeId) {
	if tc.ownership_effects_disabled() {
		return
	}
	tc.ownership_after_multi_return_assign_impl(lhs_ids, rhs_id, rhs_type, assign_id, false)
}

fn (mut tc TypeChecker) ownership_after_multi_return_assign_impl(lhs_ids []flat.NodeId, rhs_id flat.NodeId, rhs_type MultiReturn, assign_id flat.NodeId, is_decl bool) {
	if tc.ownership_effects_disabled() {
		return
	}
	fn_name := tc.ownership_call_name(rhs_id)
	owned_slots := tc.ownership_state().ownership_fn_return_slots[fn_name] or { []int{} }
	return_param_slots := tc.ownership_state().ownership_fn_return_params[fn_name] or {
		[]OwnershipReturnParamSlot{}
	}
	call_id := tc.ownership_unwrap_expr(rhs_id)
	for i, lhs_id in lhs_ids {
		if i >= rhs_type.types.len {
			continue
		}
		lhs_name := tc.ownership_lhs_name(lhs_id)
		if lhs_name.len == 0 || lhs_name == '_' {
			continue
		}
		if is_decl {
			tc.ownership_note_decl(lhs_name)
		} else {
			tc.ownership_check_reassign(lhs_name, assign_id)
		}
		tc.ownership_release_borrower(lhs_name)
		{
			mut st := tc.ownership_state()
			st.moved_vars.delete(lhs_name)
		}
		tc.ownership_clear_descendant_state(lhs_name)
		if i in owned_slots || tc.ownership_type_is_owned(rhs_type.types[i]) {
			tc.ownership_mark_owned(lhs_name, rhs_type.types[i], assign_id)
			continue
		}
		marked_return_desc := tc.ownership_mark_return_descendants_from_call(lhs_name, fn_name, i,
			assign_id)
		mut marked_from_param := false
		mut marked_param_desc := false
		if !tc.valid_node_id(call_id) {
			marked_from_param = false
		} else {
			call_node := tc.a.nodes[int(call_id)]
			if call_node.kind == .call {
				marked_param_desc = tc.ownership_mark_return_param_descendants_from_call(lhs_name,
					call_id, call_node, fn_name, i, assign_id)
				for slot in return_param_slots {
					if slot.slot_idx == i
						&& tc.ownership_mark_from_return_param(lhs_name, call_id, call_node, fn_name, slot.param_idx, assign_id) {
						marked_from_param = true
						break
					}
				}
			}
		}
		if !marked_from_param && !marked_return_desc && !marked_param_desc {
			mut st := tc.ownership_state()
			st.owned_vars.delete(lhs_name)
			st.owned_var_types.delete(lhs_name)
		}
	}
}

fn (mut tc TypeChecker) ownership_after_assign(lhs_id flat.NodeId, rhs_id flat.NodeId, lhs_type Type, _rhs_type Type, op flat.Op, assign_id flat.NodeId) {
	if tc.ownership_effects_disabled() {
		return
	}
	lhs_name := tc.ownership_lhs_name(lhs_id)
	if op != .assign {
		if lhs_name.len > 0 {
			tc.ownership_check_expr(lhs_id)
			tc.ownership_check_reassign(lhs_name, assign_id)
		}
		return
	}
	if lhs_name.len == 0 {
		tc.ownership_consume_expr(rhs_id, 'assignment target', assign_id)
		return
	}
	if lhs_name == '_' {
		tc.ownership_consume_expr(rhs_id, 'blank identifier', assign_id)
		return
	}
	tc.ownership_check_reassign(lhs_name, assign_id)
	tc.ownership_assign_to_name(lhs_name, rhs_id, lhs_type, assign_id)
	tc.ownership_track_fn_value_binding(lhs_name, rhs_id)
}

fn (mut tc TypeChecker) ownership_after_assign_pairs(lhs_ids []flat.NodeId, rhs_ids []flat.NodeId, lhs_types []Type, rhs_types []Type, op flat.Op, assign_id flat.NodeId) {
	if lhs_ids.len <= 1 || op != .assign {
		for i, lhs_id in lhs_ids {
			if i >= rhs_ids.len || i >= lhs_types.len || i >= rhs_types.len {
				continue
			}
			tc.ownership_after_assign(lhs_id, rhs_ids[i], lhs_types[i], rhs_types[i], op, assign_id)
		}
		return
	}
	mut temp_names := []string{cap: lhs_ids.len}
	for i, lhs_id in lhs_ids {
		if i >= rhs_ids.len || i >= lhs_types.len {
			temp_names << ''
			continue
		}
		lhs_name := tc.ownership_lhs_name(lhs_id)
		if lhs_name.len == 0 {
			tc.ownership_consume_expr(rhs_ids[i], 'assignment target', assign_id)
			temp_names << ''
			continue
		}
		if lhs_name == '_' {
			tc.ownership_consume_expr(rhs_ids[i], 'blank identifier', assign_id)
			temp_names << ''
			continue
		}
		temp_name := tc.ownership_multi_assign_temp_name(assign_id, i)
		tc.ownership_assign_to_name(temp_name, rhs_ids[i], lhs_types[i], assign_id)
		temp_names << temp_name
	}
	for i, lhs_id in lhs_ids {
		if i >= temp_names.len || i >= rhs_ids.len || i >= lhs_types.len {
			continue
		}
		lhs_name := tc.ownership_lhs_name(lhs_id)
		if lhs_name.len == 0 || lhs_name == '_' {
			continue
		}
		tc.ownership_check_reassign(lhs_name, assign_id)
		tc.ownership_commit_multi_assign_temp(temp_names[i], lhs_name, assign_id)
		tc.ownership_track_fn_value_binding(lhs_name, rhs_ids[i])
	}
	for temp_name in temp_names {
		tc.ownership_clear_temp_storage(temp_name)
	}
}

fn (mut tc TypeChecker) ownership_multi_assign_temp_name(assign_id flat.NodeId, idx int) string {
	return '${tc.ownership_state().cur_fn}__multi_assign_${int(assign_id)}_${idx}'
}

fn (mut tc TypeChecker) ownership_commit_multi_assign_temp(temp_name string, lhs_name string, assign_id flat.NodeId) {
	if temp_name.len == 0 || lhs_name.len == 0 {
		return
	}
	tc.ownership_release_borrower(lhs_name)
	mut st := tc.ownership_state()
	st.moved_vars.delete(lhs_name)
	tc.ownership_clear_descendant_state(lhs_name)
	if length := st.array_lengths[temp_name] {
		st.array_lengths[lhs_name] = length
	} else {
		st.array_lengths.delete(lhs_name)
	}
	mut marked := false
	if temp_name in st.owned_vars {
		type_name := tc.ownership_type_name_for_var(temp_name)
		tc.ownership_mark_owned_name(lhs_name, type_name, assign_id)
		marked = true
	}
	for owned_name in tc.ownership_owned_descendant_names(temp_name) {
		suffix := owned_name[temp_name.len..]
		tc.ownership_mark_owned_name(lhs_name + suffix, tc.ownership_type_name_for_var(owned_name),
			assign_id)
		marked = true
	}
	if !marked {
		st.owned_vars.delete(lhs_name)
		st.owned_var_types.delete(lhs_name)
	}
	borrow_snapshots := tc.ownership_borrower_snapshot(temp_name)
	tc.ownership_release_borrower(temp_name)
	for snap in borrow_snapshots {
		borrower := snap.borrow.borrower
		target_borrower := if ownership_storage_key_is_descendant(borrower, temp_name) {
			lhs_name + borrower[temp_name.len..]
		} else {
			lhs_name
		}
		tc.ownership_add_borrow(snap.var_name, target_borrower, snap.borrow.pos, snap.borrow.is_mut)
	}
}

fn (mut tc TypeChecker) ownership_assign_to_name(lhs_name string, rhs_id flat.NodeId, lhs_type Type, assign_id flat.NodeId) {
	tc.ownership_release_borrower(lhs_name)
	mut st := tc.ownership_state()
	st.moved_vars.delete(lhs_name)
	rhs_name := tc.ownership_expr_ident_name(rhs_id)
	if lhs_name != rhs_name {
		tc.ownership_clear_descendant_state(lhs_name)
	}
	if tc.ownership_expr_is_borrow(rhs_id) {
		st.owned_vars.delete(lhs_name)
		st.owned_var_types.delete(lhs_name)
		name := tc.ownership_borrowed_name(rhs_id)
		if name.len > 0 {
			tc.ownership_add_borrow(name, lhs_name, assign_id, false)
		}
		return
	}
	if tc.ownership_alias_borrower(lhs_name, rhs_name, assign_id) {
		return
	}
	if tc.ownership_mark_borrow_from_call_return(lhs_name, rhs_id, assign_id) {
		return
	}
	tc.ownership_update_array_length(lhs_name, rhs_id)
	tc.ownership_mark_struct_literal_fields(lhs_name, rhs_id, assign_id)
	tc.ownership_mark_array_literal_elements(lhs_name, rhs_id, assign_id)
	tc.ownership_mark_array_init_elements(lhs_name, rhs_id, assign_id)
	tc.ownership_mark_map_literal_entries(lhs_name, rhs_id, assign_id)
	if tc.ownership_mark_from_conditional_expr(lhs_name, rhs_id, lhs_type, assign_id) {
		return
	}
	if tc.ownership_mark_from_call(lhs_name, rhs_id, assign_id) {
		return
	}
	lhs_owned := tc.ownership_type_is_owned(lhs_type)
	if rhs_name.len > 0 {
		tc.ownership_reject_global_move(rhs_name, assign_id, lhs_name, false)
		tc.ownership_transfer_owned_descendants(rhs_name, lhs_name, assign_id)
		if rhs_name in st.owned_vars {
			tc.ownership_move_var(rhs_name, lhs_name, assign_id, false, '', true)
			tc.ownership_mark_owned(lhs_name, tc.ownership_type_for_var(rhs_name, lhs_type),
				assign_id)
			return
		}
		moved_types := tc.ownership_move_overlapping_dynamic_storage(rhs_name, lhs_name, assign_id,
			false, '', true)
		if moved_types.len > 0 {
			tc.ownership_mark_owned_name(lhs_name, moved_types[0], assign_id)
			return
		}
		rhs_type := tc.resolve_type(rhs_id)
		rhs_owned := tc.ownership_type_is_owned(rhs_type)
		if rhs_owned || lhs_owned {
			source_type := if rhs_owned { rhs_type } else { lhs_type }
			tc.ownership_mark_owned(rhs_name, source_type, assign_id)
			tc.ownership_move_var(rhs_name, lhs_name, assign_id, false, '', true)
			tc.ownership_mark_owned(lhs_name, tc.ownership_type_for_var(rhs_name, source_type),
				assign_id)
			return
		}
	}
	if lhs_owned || tc.ownership_type_is_owned(tc.resolve_type(rhs_id)) {
		tc.ownership_mark_owned(lhs_name, lhs_type, assign_id)
		return
	}
	if lhs_name != rhs_name {
		st.owned_vars.delete(lhs_name)
		st.owned_var_types.delete(lhs_name)
	}
}

fn (mut tc TypeChecker) ownership_assign_shadowing_same_name(lhs_name string, rhs_id flat.NodeId, lhs_type Type, assign_id flat.NodeId) bool {
	rhs_name := tc.ownership_expr_ident_name(rhs_id)
	if lhs_name.len == 0 || lhs_name != rhs_name {
		return false
	}
	mut st := tc.ownership_state()
	tc.ownership_reject_global_move(rhs_name, assign_id, lhs_name, false)
	if rhs_name in st.owned_vars {
		source_type := tc.ownership_type_for_var(rhs_name, lhs_type)
		if tc.ownership_move_var_result(rhs_name, lhs_name, assign_id, false, '', true) {
			tc.ownership_refresh_scope_snapshot(lhs_name)
			tc.ownership_mark_owned(lhs_name, source_type, assign_id)
		}
		return true
	}
	mut moved_descendants := map[string]string{}
	for source_name in tc.ownership_owned_descendant_names(rhs_name) {
		type_name := tc.ownership_type_name_for_var(source_name)
		if tc.ownership_move_var_result(source_name, lhs_name, assign_id, false, '', true) {
			moved_descendants[source_name] = type_name
		}
	}
	if moved_descendants.len > 0 {
		tc.ownership_refresh_scope_snapshot(lhs_name)
		for source_name, type_name in moved_descendants {
			tc.ownership_mark_owned_name(source_name, type_name, assign_id)
		}
		return true
	}
	rhs_type := tc.resolve_type(rhs_id)
	rhs_owned := tc.ownership_type_is_owned(rhs_type)
	lhs_owned := tc.ownership_type_is_owned(lhs_type)
	if rhs_owned || lhs_owned {
		source_type := if rhs_owned { rhs_type } else { lhs_type }
		tc.ownership_mark_owned(rhs_name, source_type, assign_id)
		if tc.ownership_move_var_result(rhs_name, lhs_name, assign_id, false, '', true) {
			tc.ownership_refresh_scope_snapshot(lhs_name)
			tc.ownership_mark_owned(lhs_name, source_type, assign_id)
		}
		return true
	}
	return false
}

fn (mut tc TypeChecker) ownership_track_fn_value_binding(lhs_name string, rhs_id flat.NodeId) {
	if lhs_name.len == 0 || lhs_name == '_' {
		return
	}
	mut st := tc.ownership_state()
	if fn_name := tc.ownership_fn_value_name_from_expr(rhs_id) {
		st.ownership_fn_value_vars[lhs_name] = fn_name
		return
	}
	if fn_name := tc.ownership_fn_return_fn_value_from_call(rhs_id) {
		st.ownership_fn_value_vars[lhs_name] = fn_name
		return
	}
	st.ownership_fn_value_vars.delete(lhs_name)
}

fn (mut tc TypeChecker) ownership_note_fn_return_fn_value(fn_name string, value_name string) {
	if fn_name.len == 0 || value_name.len == 0 {
		return
	}
	mut st := tc.ownership_state()
	if existing := st.ownership_fn_return_fn_values[fn_name] {
		if existing.len == 0 || existing == value_name {
			return
		}
		st.ownership_fn_return_fn_values[fn_name] = ''
		return
	}
	st.ownership_fn_return_fn_values[fn_name] = value_name
}

fn (mut tc TypeChecker) ownership_fn_return_fn_value_from_call(id flat.NodeId) ?string {
	call_id := tc.ownership_unwrap_expr(id)
	if !tc.valid_node_id(call_id) {
		return none
	}
	node := tc.a.nodes[int(call_id)]
	if node.kind != .call {
		return none
	}
	call_name := tc.ownership_call_name(call_id)
	if call_name.len == 0 {
		return none
	}
	fn_value := tc.ownership_state().ownership_fn_return_fn_values[call_name] or { return none }
	if fn_value.len == 0 {
		return none
	}
	return fn_value
}

fn (mut tc TypeChecker) ownership_fn_value_name_from_expr(id flat.NodeId) ?string {
	if name := tc.resolved_fn_value_name(id) {
		return name
	}
	clean_id := tc.ownership_unwrap_expr(id)
	if !tc.valid_node_id(clean_id) {
		return none
	}
	node := tc.a.nodes[int(clean_id)]
	match node.kind {
		.fn_literal {
			fn_name := ownership_fn_literal_name(tc.ownership_state().cur_fn, node)
			tc.ownership_register_fn_literal_signature(fn_name, node)
			return fn_name
		}
		.ident {
			if mapped := tc.ownership_state().ownership_fn_value_vars[node.value] {
				return mapped
			}
		}
		.cast_expr {
			if node.children_count > 0 {
				return tc.ownership_fn_value_name_from_expr(tc.a.child(&node, 0))
			}
		}
		else {}
	}

	if tc.fn_value_shadowed_by_value(node) {
		return none
	}
	return tc.fn_value_key(node)
}

fn (mut tc TypeChecker) ownership_fn_literal_call_info(fn_name string) ?CallInfo {
	if tc.ownership == unsafe { nil } {
		return none
	}
	ret_type := tc.ownership.ownership_fn_literal_ret_types[fn_name] or { return none }
	params := tc.ownership.ownership_fn_literal_param_types[fn_name] or { []Type{} }
	return CallInfo{
		name:         fn_name
		params:       params.clone()
		return_type:  ret_type
		params_known: true
	}
}

fn (mut tc TypeChecker) ownership_register_fn_literal_signature(fn_name string, node flat.Node) {
	if fn_name.len == 0 || fn_name in tc.fn_ret_types {
		return
	}
	mut params := []Type{}
	mut shared_params := []bool{}
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		if child.kind == .param {
			params << tc.parse_type(child.typ)
			shared_params << param_type_text_is_shared(child.typ)
		}
	}
	ret_type := tc.parse_type(node.typ)
	if !tc.parallel_check_sparse {
		tc.register_fn_signature(fn_name, ret_type, params, shared_params, false, false)
	} else {
		mut st := tc.ownership_state()
		if fn_name in st.ownership_fn_literal_ret_types {
			return
		}
		st.ownership_fn_literal_ret_types[fn_name] = ret_type
		st.ownership_fn_literal_param_types[fn_name] = params.clone()
	}
	tc.ownership_register_fn_param_mut(fn_name, tc.ownership_node_param_mut_flags(node))
}

fn (mut tc TypeChecker) ownership_consume_array_init_expr(node flat.Node) {
	if node.kind != .array_init {
		return
	}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.nodes[int(child_id)]
		if child.kind == .field_init {
			if child.value == 'init' && child.children_count > 0 {
				init_id := tc.a.child(&child, 0)
				tc.ownership_consume_expr(init_id, 'array element', init_id)
			}
			continue
		}
		tc.ownership_consume_expr(child_id, 'array element', child_id)
	}
}

fn (mut tc TypeChecker) ownership_update_array_length(lhs_name string, rhs_id flat.NodeId) {
	if lhs_name.len == 0 {
		return
	}
	mut st := tc.ownership_state()
	id := tc.ownership_unwrap_expr(rhs_id)
	if tc.valid_node_id(id) {
		node := tc.a.nodes[int(id)]
		if node.kind == .array_literal {
			st.array_lengths[lhs_name] = node.children_count
			return
		}
		if node.kind == .array_init {
			length := tc.ownership_array_init_length(node) or {
				st.array_lengths.delete(lhs_name)
				return
			}
			st.array_lengths[lhs_name] = length
			return
		}
	}
	rhs_name := tc.ownership_expr_ident_name(rhs_id)
	if rhs_name.len > 0 {
		if length := st.array_lengths[rhs_name] {
			st.array_lengths[lhs_name] = length
			return
		}
	}
	if lhs_name != rhs_name {
		st.array_lengths.delete(lhs_name)
	}
}

fn (tc &TypeChecker) ownership_array_init_length(node flat.Node) ?int {
	mut element_count := 0
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		if child.kind != .field_init {
			element_count++
			continue
		}
		if child.value != 'len' {
			continue
		}
		if child.children_count == 0 {
			return none
		}
		len_id := tc.a.child(child, 0)
		if !tc.valid_node_id(len_id) {
			return none
		}
		len_node := tc.a.nodes[int(len_id)]
		if len_node.kind != .int_literal || !len_node.value.is_int() {
			return none
		}
		return len_node.value.int()
	}
	return element_count
}

fn (mut tc TypeChecker) ownership_transfer_owned_descendants(source_prefix string, target_prefix string, pos flat.NodeId) bool {
	if source_prefix.len == 0 || target_prefix.len == 0 || source_prefix == target_prefix {
		return false
	}
	mut moved_any := false
	for source_name in tc.ownership_owned_descendant_names(source_prefix) {
		suffix := source_name[source_prefix.len..]
		target_name := target_prefix + suffix
		type_name := tc.ownership_type_name_for_var(source_name)
		if tc.ownership_move_var_result(source_name, target_name, pos, false, '', true) {
			tc.ownership_mark_owned_name(target_name, type_name, pos)
			moved_any = true
		}
	}
	return moved_any
}

fn (mut tc TypeChecker) ownership_move_owned_descendants(source_prefix string, target string, pos flat.NodeId, is_fn_call bool, fn_name string, suggest_clone bool) bool {
	if source_prefix.len == 0 {
		return false
	}
	mut moved_any := false
	for source_name in tc.ownership_owned_descendant_names(source_prefix) {
		if tc.ownership_move_var_result(source_name, target, pos, is_fn_call, fn_name,
			suggest_clone)
		{
			moved_any = true
		}
	}
	return moved_any
}

fn (mut tc TypeChecker) ownership_transfer_owned_descendants_to_param(source_prefix string, fn_name string, param_idx int, pos flat.NodeId) bool {
	return tc.ownership_transfer_owned_descendants_to_param_with_suffix(source_prefix, fn_name,
		param_idx, '', pos)
}

fn (mut tc TypeChecker) ownership_transfer_owned_descendants_to_param_with_suffix(source_prefix string, fn_name string, param_idx int, target_suffix string, pos flat.NodeId) bool {
	if source_prefix.len == 0 || fn_name.len == 0 || param_idx < 0 {
		return false
	}
	mut moved_any := false
	for source_name in tc.ownership_owned_descendant_names(source_prefix) {
		suffix := source_name[source_prefix.len..]
		type_name := tc.ownership_type_name_for_var(source_name)
		if tc.ownership_move_var_result(source_name, fn_name, pos, true, fn_name, true) {
			tc.ownership_add_fn_param_descendant(fn_name, param_idx, target_suffix + suffix,
				type_name)
			moved_any = true
		}
	}
	return moved_any
}

fn (mut tc TypeChecker) ownership_owned_descendant_names(prefix string) []string {
	if prefix.len == 0 {
		return []string{}
	}
	st := tc.ownership_state()
	mut names := []string{}
	for name, _ in st.owned_vars {
		if ownership_storage_key_is_descendant(name, prefix) {
			names << name
		}
	}
	names.sort()
	mut deduped := []string{cap: names.len}
	for name in names {
		mut covered_by_dynamic := false
		for existing in deduped {
			if ownership_storage_key_has_dynamic_index(existing)
				&& ownership_storage_keys_overlap(name, existing) {
				covered_by_dynamic = true
				break
			}
		}
		if !covered_by_dynamic {
			deduped << name
		}
	}
	return deduped
}

fn (mut tc TypeChecker) ownership_mark_array_literal_elements(lhs_name string, rhs_id flat.NodeId, pos flat.NodeId) bool {
	return tc.ownership_mark_array_literal_elements_with_mode(lhs_name, rhs_id, pos, true)
}

fn (mut tc TypeChecker) ownership_mark_array_literal_elements_with_mode(lhs_name string, rhs_id flat.NodeId, pos flat.NodeId, clear_unowned bool) bool {
	if lhs_name.len == 0 {
		return false
	}
	id := tc.ownership_unwrap_expr(rhs_id)
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind != .array_literal {
		return false
	}
	mut marked := false
	tc.ownership_state().array_lengths[lhs_name] = node.children_count
	for i in 0 .. node.children_count {
		elem_id := tc.a.child(&node, i)
		elem_key := '${lhs_name}[${i}]'
		elem_type := tc.resolve_type(elem_id)
		if tc.ownership_mark_storage_from_expr_with_mode(elem_key, elem_id, elem_type, pos,
			clear_unowned)
		{
			marked = true
			continue
		}
		if !clear_unowned {
			continue
		}
		mut st := tc.ownership_state()
		st.owned_vars.delete(elem_key)
		st.owned_var_types.delete(elem_key)
		st.moved_vars.delete(elem_key)
	}
	return marked
}

fn (mut tc TypeChecker) ownership_mark_array_init_elements(lhs_name string, rhs_id flat.NodeId, pos flat.NodeId) bool {
	return tc.ownership_mark_array_init_elements_with_mode(lhs_name, rhs_id, pos, true)
}

fn (mut tc TypeChecker) ownership_mark_array_init_elements_with_mode(lhs_name string, rhs_id flat.NodeId, pos flat.NodeId, clear_unowned bool) bool {
	if lhs_name.len == 0 {
		return false
	}
	id := tc.ownership_unwrap_expr(rhs_id)
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind != .array_init {
		return false
	}
	if length := tc.ownership_array_init_length(node) {
		tc.ownership_state().array_lengths[lhs_name] = length
	}
	mut marked := false
	mut elem_idx := 0
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.nodes[int(child_id)]
		if child.kind == .field_init {
			if child.value == 'init' && child.children_count > 0 {
				init_id := tc.a.child(&child, 0)
				elem_key := '${lhs_name}[*]'
				elem_type := tc.resolve_type(init_id)
				if tc.ownership_mark_storage_from_expr_with_mode(elem_key, init_id, elem_type, pos,
					clear_unowned)
				{
					marked = true
					continue
				}
				if clear_unowned {
					mut st := tc.ownership_state()
					st.owned_vars.delete(elem_key)
					st.owned_var_types.delete(elem_key)
					st.moved_vars.delete(elem_key)
				}
			}
			continue
		}
		elem_key := '${lhs_name}[${elem_idx}]'
		elem_idx++
		elem_type := tc.resolve_type(child_id)
		if tc.ownership_mark_storage_from_expr_with_mode(elem_key, child_id, elem_type, pos,
			clear_unowned)
		{
			marked = true
			continue
		}
		if !clear_unowned {
			continue
		}
		mut st := tc.ownership_state()
		st.owned_vars.delete(elem_key)
		st.owned_var_types.delete(elem_key)
		st.moved_vars.delete(elem_key)
	}
	return marked
}

fn (mut tc TypeChecker) ownership_mark_array_append_expr(array_id flat.NodeId, elem_id flat.NodeId, pos flat.NodeId) {
	array_name := tc.ownership_expr_ident_name(array_id)
	if array_name.len == 0 {
		tc.ownership_consume_expr(elem_id, 'array append', pos)
		return
	}
	tc.ownership_check_reassign(array_name, pos)
	if unwrap_pointer(tc.resolve_type(elem_id)) is Array {
		tc.ownership_mark_array_append_array(array_name, elem_id, pos)
		return
	}
	tc.ownership_mark_array_append_element(array_name, array_id, elem_id, pos)
}

fn (mut tc TypeChecker) ownership_mark_array_append_element(array_name string, array_id flat.NodeId, elem_id flat.NodeId, pos flat.NodeId) {
	elem_key := tc.ownership_array_append_key(array_name)
	elem_type := tc.ownership_array_append_elem_type(array_id, elem_id)
	if tc.ownership_mark_storage_from_expr(elem_key, elem_id, elem_type, pos) {
		return
	}
	mut st := tc.ownership_state()
	st.owned_vars.delete(elem_key)
	st.owned_var_types.delete(elem_key)
	st.moved_vars.delete(elem_key)
}

fn (mut tc TypeChecker) ownership_mark_array_insert_prepend_arg(node flat.Node, call_name string, param_idx int, arg_id flat.NodeId, pos flat.NodeId) bool {
	if (call_name == 'array.insert' && param_idx != 2)
		|| (call_name == 'array.prepend' && param_idx != 1) {
		return false
	}
	if call_name !in ['array.insert', 'array.prepend'] {
		return false
	}
	recv_id := tc.ownership_call_receiver_id(node) or { return false }
	array_name := tc.ownership_expr_ident_name(recv_id)
	if array_name.len == 0 {
		return false
	}
	tc.ownership_check_reassign(array_name, pos)
	elem_type := tc.ownership_array_append_elem_type(recv_id, arg_id)
	temp_name := '${tc.ownership_state().cur_fn}__array_insert_${int(pos)}'
	marked := tc.ownership_mark_storage_from_expr(temp_name, arg_id, elem_type, pos)
	if insert_idx := tc.ownership_array_insert_prepend_index(node, call_name) {
		tc.ownership_shift_array_elements_for_insert(array_name, insert_idx)
	}
	elem_key := tc.ownership_array_insert_prepend_key(array_name, node, call_name)
	tc.ownership_commit_multi_assign_temp(temp_name, elem_key, pos)
	tc.ownership_clear_temp_storage(temp_name)
	tc.ownership_update_array_length_after_array_insert(array_name)
	return marked
}

fn (mut tc TypeChecker) ownership_array_insert_prepend_key(array_name string, node flat.Node, call_name string) string {
	if insert_idx := tc.ownership_array_insert_prepend_index(node, call_name) {
		return '${array_name}[${insert_idx}]'
	}
	return '${array_name}[*]'
}

fn (mut tc TypeChecker) ownership_array_insert_prepend_index(node flat.Node, call_name string) ?int {
	if call_name == 'array.prepend' {
		return 0
	}
	if call_name != 'array.insert' || node.children_count <= 1 {
		return none
	}
	index_id := tc.call_arg_value(tc.a.child(&node, 1))
	index := tc.ownership_index_key_part(index_id)
	if index.len == 0 || !index.is_int() {
		return none
	}
	return index.int()
}

fn (mut tc TypeChecker) ownership_update_array_length_after_array_insert(array_name string) {
	mut st := tc.ownership_state()
	if length := st.array_lengths[array_name] {
		st.array_lengths[array_name] = length + 1
	}
}

fn (mut tc TypeChecker) ownership_shift_array_elements_for_insert(array_name string, insert_idx int) {
	if array_name.len == 0 || insert_idx < 0 {
		return
	}
	mut st := tc.ownership_state()
	mut names := map[string]bool{}
	for name, _ in st.owned_vars {
		if _ := ownership_shifted_array_storage_key(name, array_name, insert_idx) {
			names[name] = true
		}
	}
	for name, _ in st.owned_var_types {
		if _ := ownership_shifted_array_storage_key(name, array_name, insert_idx) {
			names[name] = true
		}
	}
	for name, _ in st.moved_vars {
		if _ := ownership_shifted_array_storage_key(name, array_name, insert_idx) {
			names[name] = true
		}
	}
	mut entries := []OwnershipArrayShiftEntry{}
	for name, _ in names {
		new_name := ownership_shifted_array_storage_key(name, array_name, insert_idx) or {
			continue
		}
		entries << OwnershipArrayShiftEntry{
			old_name:  name
			new_name:  new_name
			had_owned: name in st.owned_vars
			owned_pos: st.owned_vars[name] or { flat.empty_node }
			had_type:  name in st.owned_var_types
			type_name: st.owned_var_types[name] or { '' }
			had_moved: name in st.moved_vars
			moved:     st.moved_vars[name] or { MovedVar{} }
		}
	}
	for entry in entries {
		st.owned_vars.delete(entry.old_name)
		st.owned_var_types.delete(entry.old_name)
		st.moved_vars.delete(entry.old_name)
	}
	for entry in entries {
		if entry.had_owned {
			st.owned_vars[entry.new_name] = entry.owned_pos
		}
		if entry.had_type {
			st.owned_var_types[entry.new_name] = entry.type_name
		}
		if entry.had_moved {
			st.moved_vars[entry.new_name] = entry.moved
		}
	}
}

fn ownership_shifted_array_storage_key(name string, array_name string, insert_idx int) ?string {
	if name.len == 0 || array_name.len == 0 || insert_idx < 0
		|| !ownership_storage_key_is_descendant(name, array_name) {
		return none
	}
	suffix := name[array_name.len..]
	index_part, rest := ownership_split_first_index_suffix(suffix)
	idx := ownership_index_segment_int(index_part) or { return none }
	if idx < insert_idx {
		return none
	}
	return '${array_name}[${idx + 1}]${rest}'
}

fn (mut tc TypeChecker) ownership_shift_array_elements_after_pop_left(array_name string) {
	if array_name.len == 0 {
		return
	}
	mut st := tc.ownership_state()
	mut names := map[string]bool{}
	for name, _ in st.owned_vars {
		if _ := ownership_pop_left_shifted_array_storage_key(name, array_name) {
			names[name] = true
		}
	}
	for name, _ in st.owned_var_types {
		if _ := ownership_pop_left_shifted_array_storage_key(name, array_name) {
			names[name] = true
		}
	}
	for name, _ in st.moved_vars {
		if _ := ownership_pop_left_shifted_array_storage_key(name, array_name) {
			names[name] = true
		}
	}
	mut entries := []OwnershipArrayShiftEntry{}
	for name, _ in names {
		new_name := ownership_pop_left_shifted_array_storage_key(name, array_name) or { continue }
		entries << OwnershipArrayShiftEntry{
			old_name:  name
			new_name:  new_name
			had_owned: name in st.owned_vars
			owned_pos: st.owned_vars[name] or { flat.empty_node }
			had_type:  name in st.owned_var_types
			type_name: st.owned_var_types[name] or { '' }
			had_moved: name in st.moved_vars
			moved:     st.moved_vars[name] or { MovedVar{} }
		}
	}
	for entry in entries {
		st.owned_vars.delete(entry.old_name)
		st.owned_var_types.delete(entry.old_name)
		st.moved_vars.delete(entry.old_name)
	}
	for entry in entries {
		if entry.had_owned {
			st.owned_vars[entry.new_name] = entry.owned_pos
		}
		if entry.had_type {
			st.owned_var_types[entry.new_name] = entry.type_name
		}
		if entry.had_moved {
			st.moved_vars[entry.new_name] = entry.moved
		}
	}
}

fn ownership_pop_left_shifted_array_storage_key(name string, array_name string) ?string {
	if name.len == 0 || array_name.len == 0
		|| !ownership_storage_key_is_descendant(name, array_name) {
		return none
	}
	suffix := name[array_name.len..]
	index_part, rest := ownership_split_first_index_suffix(suffix)
	idx := ownership_index_segment_int(index_part) or { return none }
	if idx <= 0 {
		return none
	}
	return '${array_name}[${idx - 1}]${rest}'
}

fn (mut tc TypeChecker) ownership_mark_array_append_array(array_name string, rhs_id flat.NodeId, pos flat.NodeId) bool {
	if array_name.len == 0 {
		return false
	}
	rhs_id_unwrapped := tc.ownership_unwrap_expr(rhs_id)
	if !tc.valid_node_id(rhs_id_unwrapped) {
		return false
	}
	node := tc.a.nodes[int(rhs_id_unwrapped)]
	dst_len := tc.ownership_state().array_lengths[array_name] or { -1 }
	mut source_len := -1
	mut marked := false
	if node.kind == .array_literal {
		source_len = node.children_count
		for i in 0 .. node.children_count {
			elem_id := tc.a.child(&node, i)
			elem_key := ownership_array_append_target_key(array_name, dst_len, i)
			elem_type := tc.resolve_type(elem_id)
			if tc.ownership_mark_storage_from_expr(elem_key, elem_id, elem_type, pos) {
				marked = true
				continue
			}
			mut st := tc.ownership_state()
			st.owned_vars.delete(elem_key)
			st.owned_var_types.delete(elem_key)
			st.moved_vars.delete(elem_key)
		}
		tc.ownership_update_array_length_after_array_append(array_name, dst_len, source_len)
		return marked
	}
	source_name := tc.ownership_expr_ident_name(rhs_id_unwrapped)
	if source_name.len == 0 {
		tc.ownership_consume_expr(rhs_id_unwrapped, 'array append', pos)
		mut st := tc.ownership_state()
		st.array_lengths.delete(array_name)
		return false
	}
	source_len = tc.ownership_state().array_lengths[source_name] or { -1 }
	for source in tc.ownership_owned_descendant_names(source_name) {
		suffix := source[source_name.len..]
		source_index, rest := ownership_split_first_index_suffix(suffix)
		if source_index.len == 0 {
			continue
		}
		target_key :=
			ownership_array_append_target_key_from_segment(array_name, dst_len, source_index) + rest
		type_name := tc.ownership_type_name_for_var(source)
		if tc.ownership_move_var_result(source, target_key, pos, false, '', true) {
			tc.ownership_mark_owned_name(target_key, type_name, pos)
			marked = true
		}
	}
	tc.ownership_update_array_length_after_array_append(array_name, dst_len, source_len)
	return marked
}

fn (mut tc TypeChecker) ownership_update_array_length_after_array_append(array_name string, dst_len int, source_len int) {
	mut st := tc.ownership_state()
	if dst_len >= 0 && source_len >= 0 {
		st.array_lengths[array_name] = dst_len + source_len
		return
	}
	st.array_lengths.delete(array_name)
}

fn ownership_array_append_target_key(array_name string, dst_len int, source_index int) string {
	if array_name.len == 0 || dst_len < 0 || source_index < 0 {
		return '${array_name}[*]'
	}
	return '${array_name}[${dst_len + source_index}]'
}

fn ownership_array_append_target_key_from_segment(array_name string, dst_len int, source_index string) string {
	idx := ownership_index_segment_int(source_index) or { return '${array_name}[*]' }
	return ownership_array_append_target_key(array_name, dst_len, idx)
}

fn ownership_map_key_storage_name(map_name string) string {
	return ownership_map_key_storage_suffix(map_name)
}

fn ownership_map_key_storage_suffix(base_suffix string) string {
	return '${base_suffix}.__key[*]'
}

fn ownership_split_first_index_suffix(suffix string) (string, string) {
	if suffix.len == 0 || !suffix.starts_with('[') {
		return '', suffix
	}
	close_idx := suffix.index(']') or { return '', suffix }
	return suffix[..close_idx + 1], suffix[close_idx + 1..]
}

fn ownership_index_segment_int(segment string) ?int {
	if segment.len < 3 || segment[0] != `[` || segment[segment.len - 1] != `]` {
		return none
	}
	inner := segment[1..segment.len - 1]
	if !inner.is_int() {
		return none
	}
	return inner.int()
}

fn (mut tc TypeChecker) ownership_array_append_elem_type(array_id flat.NodeId, elem_id flat.NodeId) Type {
	clean := unwrap_pointer(tc.resolve_type(array_id))
	if clean is Array {
		return array_elem_type(clean)
	}
	return tc.resolve_type(elem_id)
}

fn (mut tc TypeChecker) ownership_array_append_key(array_name string) string {
	mut st := tc.ownership_state()
	idx := st.array_lengths[array_name] or { return '${array_name}[*]' }
	st.array_lengths[array_name] = idx + 1
	return '${array_name}[${idx}]'
}

fn (mut tc TypeChecker) ownership_mark_map_literal_entries(lhs_name string, rhs_id flat.NodeId, pos flat.NodeId) bool {
	return tc.ownership_mark_map_literal_entries_with_mode(lhs_name, rhs_id, pos, true)
}

fn (mut tc TypeChecker) ownership_mark_map_literal_entries_with_mode(lhs_name string, rhs_id flat.NodeId, pos flat.NodeId, clear_unowned bool) bool {
	if lhs_name.len == 0 {
		return false
	}
	id := tc.ownership_unwrap_expr(rhs_id)
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind != .map_init {
		return false
	}
	mut marked := false
	for i := 0; i < node.children_count; i += 2 {
		key_id := tc.a.child(&node, i)
		key_entry := ownership_map_key_storage_name(lhs_name)
		key_type := tc.resolve_type(key_id)
		if tc.ownership_mark_storage_from_expr_with_mode(key_entry, key_id, key_type, pos,
			clear_unowned)
		{
			marked = true
		}
		key_part := tc.ownership_index_key_part(key_id)
		if key_part.len == 0 || i + 1 >= node.children_count {
			continue
		}
		value_id := tc.a.child(&node, i + 1)
		entry_key := '${lhs_name}[${key_part}]'
		value_type := tc.resolve_type(value_id)
		if tc.ownership_mark_storage_from_expr_with_mode(entry_key, value_id, value_type, pos,
			clear_unowned)
		{
			marked = true
			continue
		}
		if !clear_unowned {
			continue
		}
		mut st := tc.ownership_state()
		st.owned_vars.delete(entry_key)
		st.owned_var_types.delete(entry_key)
		st.moved_vars.delete(entry_key)
	}
	return marked
}

fn (mut tc TypeChecker) ownership_mark_struct_literal_fields(lhs_name string, rhs_id flat.NodeId, pos flat.NodeId) bool {
	return tc.ownership_mark_struct_literal_fields_with_mode(lhs_name, rhs_id, pos, true)
}

fn (mut tc TypeChecker) ownership_mark_struct_literal_fields_with_mode(lhs_name string, rhs_id flat.NodeId, pos flat.NodeId, clear_unowned bool) bool {
	if lhs_name.len == 0 {
		return false
	}
	id := tc.ownership_unwrap_expr(rhs_id)
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .assoc {
		return tc.ownership_mark_assoc_literal_fields_with_mode(lhs_name, id, pos, clear_unowned)
	}
	if node.kind != .struct_init {
		return false
	}
	init_type := tc.parse_type(node.value)
	if init_type !is Struct {
		return false
	}
	fields := tc.structs[(init_type as Struct).name] or { []StructField{} }
	mut marked := false
	mut explicit_fields := map[string]bool{}
	for i in 0 .. node.children_count {
		field_id := tc.a.child(&node, i)
		field := tc.a.nodes[int(field_id)]
		if field.kind != .field_init || field.children_count == 0 {
			continue
		}
		mut field_name := field.value
		if field_name.len == 0 && i < fields.len {
			field_name = fields[i].name
		}
		if field_name.len == 0 {
			continue
		}
		explicit_fields[field_name] = true
		field_key := '${lhs_name}.${field_name}'
		value_id := tc.a.child(&field, 0)
		field_type := if field_name.len > 0 {
			tc.struct_field_type((init_type as Struct).name, field_name) or {
				tc.resolve_type(value_id)
			}
		} else {
			tc.resolve_type(value_id)
		}
		if tc.ownership_mark_storage_from_expr_with_mode(field_key, value_id, field_type, pos,
			clear_unowned)
		{
			marked = true
			continue
		}
		if !clear_unowned {
			continue
		}
		mut st := tc.ownership_state()
		st.owned_vars.delete(field_key)
		st.owned_var_types.delete(field_key)
		st.moved_vars.delete(field_key)
	}
	if decl := tc.ownership_struct_decl_node((init_type as Struct).name) {
		for i in 0 .. decl.children_count {
			field := tc.a.child_node(decl, i)
			if field.kind != .field_decl || field.children_count == 0 || field.value.len == 0 {
				continue
			}
			if field.value in explicit_fields {
				continue
			}
			field_key := '${lhs_name}.${field.value}'
			default_id := tc.a.child(field, 0)
			field_type := tc.struct_field_type((init_type as Struct).name, field.value) or {
				tc.parse_type(field.typ)
			}
			if tc.ownership_mark_storage_from_expr_with_mode(field_key, default_id, field_type,
				pos, clear_unowned)
			{
				marked = true
				continue
			}
			if !clear_unowned {
				continue
			}
			mut st := tc.ownership_state()
			st.owned_vars.delete(field_key)
			st.owned_var_types.delete(field_key)
			st.moved_vars.delete(field_key)
		}
	}
	return marked
}

fn (mut tc TypeChecker) ownership_mark_assoc_literal_fields_with_mode(lhs_name string, rhs_id flat.NodeId, pos flat.NodeId, clear_unowned bool) bool {
	if lhs_name.len == 0 {
		return false
	}
	id := tc.ownership_unwrap_expr(rhs_id)
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind != .assoc || node.children_count == 0 {
		return false
	}
	init_type := tc.resolve_type(id)
	if init_type !is Struct {
		return false
	}
	fields := tc.structs[(init_type as Struct).name] or { []StructField{} }
	mut explicit_fields := map[string]bool{}
	for i in 1 .. node.children_count {
		field_id := tc.a.child(&node, i)
		field := tc.a.nodes[int(field_id)]
		if field.kind != .field_init || field.children_count == 0 {
			continue
		}
		mut field_name := field.value
		field_idx := i - 1
		if field_name.len == 0 && field_idx < fields.len {
			field_name = fields[field_idx].name
		}
		if field_name.len > 0 {
			explicit_fields[field_name] = true
		}
	}
	base_id := tc.a.child(&node, 0)
	base_name := tc.ownership_expr_ident_name(base_id)
	mut marked := false
	if base_name.len > 0 {
		tc.ownership_reject_global_move(base_name, pos, lhs_name, false)
		mut st := tc.ownership_state()
		if base_name in st.owned_vars {
			source_type := tc.ownership_type_for_var(base_name, init_type)
			if tc.ownership_move_var_result(base_name, lhs_name, pos, false, '', true) {
				tc.ownership_mark_owned(lhs_name, source_type, pos)
				marked = true
			}
		}
		if tc.ownership_transfer_assoc_base_descendants(base_name, lhs_name, explicit_fields, pos) {
			marked = true
		}
	} else if tc.ownership_mark_storage_from_expr_with_mode(lhs_name, base_id, init_type, pos,
		clear_unowned)
	{
		marked = true
	}
	for i in 1 .. node.children_count {
		field_id := tc.a.child(&node, i)
		field := tc.a.nodes[int(field_id)]
		if field.kind != .field_init || field.children_count == 0 {
			continue
		}
		mut field_name := field.value
		field_idx := i - 1
		if field_name.len == 0 && field_idx < fields.len {
			field_name = fields[field_idx].name
		}
		if field_name.len == 0 {
			continue
		}
		field_key := '${lhs_name}.${field_name}'
		if clear_unowned {
			tc.ownership_clear_temp_storage(field_key)
		}
		value_id := tc.a.child(&field, 0)
		field_type := tc.struct_field_type((init_type as Struct).name, field_name) or {
			tc.resolve_type(value_id)
		}
		if tc.ownership_mark_storage_from_expr_with_mode(field_key, value_id, field_type, pos,
			clear_unowned)
		{
			marked = true
			continue
		}
		if clear_unowned {
			tc.ownership_clear_temp_storage(field_key)
		}
	}
	return marked
}

fn (mut tc TypeChecker) ownership_transfer_assoc_base_descendants(source_prefix string, target_prefix string, explicit_fields map[string]bool, pos flat.NodeId) bool {
	if source_prefix.len == 0 || target_prefix.len == 0 || source_prefix == target_prefix {
		return false
	}
	mut moved_any := false
	for source_name in tc.ownership_owned_descendant_names(source_prefix) {
		if ownership_assoc_base_descendant_overridden(source_prefix, source_name, explicit_fields) {
			continue
		}
		suffix := source_name[source_prefix.len..]
		target_name := target_prefix + suffix
		type_name := tc.ownership_type_name_for_var(source_name)
		if tc.ownership_move_var_result(source_name, target_name, pos, false, '', true) {
			tc.ownership_mark_owned_name(target_name, type_name, pos)
			moved_any = true
		}
	}
	return moved_any
}

fn ownership_assoc_base_descendant_overridden(source_prefix string, source_name string, explicit_fields map[string]bool) bool {
	for field_name, _ in explicit_fields {
		if ownership_storage_keys_overlap(source_name, '${source_prefix}.${field_name}') {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) ownership_struct_decl_node(struct_name string) ?flat.Node {
	target := generic_base_name(struct_name)
	mut cur_module := ''
	for i in tc.top_level_idx {
		node := tc.a.nodes[i]
		match node.kind {
			.module_decl {
				cur_module = node.value
			}
			.struct_decl {
				qname := ownership_qualify_name(cur_module, node.value)
				module_qname := if cur_module.len > 0 {
					'${cur_module}.${node.value}'
				} else {
					node.value
				}
				if target == generic_base_name(qname) || target == generic_base_name(module_qname)
					|| target == generic_base_name(node.value) {
					return node
				}
			}
			else {}
		}
	}
	return none
}

fn (mut tc TypeChecker) ownership_mark_aggregate_literal_storage(target_name string, expr_id flat.NodeId, pos flat.NodeId, clear_unowned bool) bool {
	if !tc.valid_node_id(expr_id) {
		return false
	}
	node := tc.a.nodes[int(expr_id)]
	match node.kind {
		.array_literal {
			return tc.ownership_mark_array_literal_elements_with_mode(target_name, expr_id, pos,
				clear_unowned)
		}
		.array_init {
			return tc.ownership_mark_array_init_elements_with_mode(target_name, expr_id, pos,
				clear_unowned)
		}
		.map_init {
			return tc.ownership_mark_map_literal_entries_with_mode(target_name, expr_id, pos,
				clear_unowned)
		}
		.struct_init {
			return tc.ownership_mark_struct_literal_fields_with_mode(target_name, expr_id, pos,
				clear_unowned)
		}
		.assoc {
			return tc.ownership_mark_assoc_literal_fields_with_mode(target_name, expr_id, pos,
				clear_unowned)
		}
		else {}
	}

	return false
}

fn (mut tc TypeChecker) ownership_mark_storage_from_expr(target_name string, expr_id flat.NodeId, target_type Type, pos flat.NodeId) bool {
	return tc.ownership_mark_storage_from_expr_with_mode(target_name, expr_id, target_type, pos,
		true)
}

fn (mut tc TypeChecker) ownership_mark_storage_from_expr_with_mode(target_name string, expr_id flat.NodeId, target_type Type, pos flat.NodeId, clear_unowned bool) bool {
	if !tc.valid_node_id(expr_id) {
		return false
	}
	id := tc.ownership_unwrap_expr(expr_id)
	if !tc.valid_node_id(id) {
		return false
	}
	if tc.ownership_mark_aggregate_literal_storage(target_name, id, pos, clear_unowned) {
		return true
	}
	borrow_name := tc.ownership_borrowed_name(id)
	if borrow_name.len > 0 {
		mut st := tc.ownership_state()
		st.owned_vars.delete(target_name)
		st.owned_var_types.delete(target_name)
		st.moved_vars.delete(target_name)
		tc.ownership_add_borrow(borrow_name, target_name, pos, false)
		return false
	}
	source_name := tc.ownership_expr_ident_name(id)
	if source_name.len > 0 {
		if moved := tc.ownership_moved_conflict(source_name) {
			tc.ownership_report_moved(moved.name, moved.info, id)
			return false
		}
	}
	if source_name.len > 0 && source_name in tc.ownership_state().owned_vars {
		tc.ownership_reject_global_move(source_name, pos, target_name, false)
		tc.ownership_move_var(source_name, target_name, pos, false, '', true)
		tc.ownership_mark_owned(target_name, tc.ownership_type_for_var(source_name, target_type),
			pos)
		return true
	}
	if source_name.len > 0 {
		moved_types := tc.ownership_move_overlapping_dynamic_storage(source_name, target_name, pos,
			false, '', true)
		if moved_types.len > 0 {
			tc.ownership_mark_owned_name(target_name, moved_types[0], pos)
			return true
		}
	}
	if tc.ownership_mark_from_conditional_expr(target_name, id, target_type, pos) {
		return true
	}
	if tc.ownership_mark_from_call(target_name, id, pos) {
		return true
	}
	if tc.ownership_expr_is_to_owned_call(id) || tc.ownership_expr_is_owned_clone_call(id)
		|| tc.ownership_expr_is_ownership_call(id)
		|| tc.ownership_type_is_owned(tc.resolve_type(id)) {
		tc.ownership_mark_owned(target_name, tc.resolve_type(id), pos)
		return true
	}
	return false
}

fn (mut tc TypeChecker) ownership_alias_borrower(lhs_name string, rhs_name string, pos flat.NodeId) bool {
	if lhs_name.len == 0 || rhs_name.len == 0 {
		return false
	}
	st := tc.ownership_state()
	mut aliases := []OwnershipBorrowerSnapshot{}
	for var_name, borrows in st.borrowed_vars {
		for borrow in borrows {
			if borrow.borrower == rhs_name {
				aliases << OwnershipBorrowerSnapshot{
					var_name: var_name
					borrow:   borrow
				}
			}
		}
	}
	if aliases.len == 0 {
		return false
	}
	{
		mut st_mut := tc.ownership_state()
		st_mut.owned_vars.delete(lhs_name)
		st_mut.owned_var_types.delete(lhs_name)
	}
	for alias in aliases {
		tc.ownership_add_borrow(alias.var_name, lhs_name, pos, alias.borrow.is_mut)
	}
	return true
}

fn (mut tc TypeChecker) ownership_mark_from_conditional_expr(lhs_name string, rhs_id flat.NodeId, lhs_type Type, pos flat.NodeId) bool {
	cond_id := tc.ownership_unwrap_expr(rhs_id)
	if !tc.valid_node_id(cond_id) {
		return false
	}
	node := tc.a.nodes[int(cond_id)]
	if node.kind !in [.if_expr, .match_stmt, .or_expr] {
		return false
	}
	mut moved_sources := []OwnershipConditionalMoveSource{}
	mut marked := tc.ownership_collect_conditional_result(lhs_name, cond_id, lhs_type, pos, mut
		moved_sources)
	for move in moved_sources {
		target_name := lhs_name + move.target_suffix
		tc.ownership_reject_global_move(move.source, pos, target_name, false)
		tc.ownership_move_var(move.source, target_name, pos, false, '', true)
		tc.ownership_mark_owned(target_name, tc.ownership_type_for_var(move.source, lhs_type), pos)
		marked = true
	}
	tc.ownership_flush_value_branch_moves()
	return marked
}

fn (mut tc TypeChecker) ownership_collect_conditional_result(lhs_name string, if_id flat.NodeId, lhs_type Type, pos flat.NodeId, mut moved_sources []OwnershipConditionalMoveSource) bool {
	if !tc.valid_node_id(if_id) {
		return false
	}
	node := tc.a.nodes[int(if_id)]
	match node.kind {
		.if_expr {
			mut marked := false
			if node.children_count > 1 {
				then_tail := tc.branch_tail_expr_id(tc.a.child(&node, 1))
				if tc.ownership_collect_expr_result(lhs_name, then_tail, lhs_type, pos, mut
					moved_sources)
				{
					marked = true
				}
			}
			if node.children_count > 2 {
				else_id := tc.a.child(&node, 2)
				if tc.valid_node_id(else_id) && tc.a.nodes[int(else_id)].kind == .if_expr {
					if tc.ownership_collect_conditional_result(lhs_name, else_id, lhs_type, pos, mut
						moved_sources)
					{
						marked = true
					}
				} else {
					else_tail := tc.branch_tail_expr_id(else_id)
					if tc.ownership_collect_expr_result(lhs_name, else_tail, lhs_type, pos, mut
						moved_sources)
					{
						marked = true
					}
				}
			}
			return marked
		}
		.match_stmt {
			mut marked := false
			for i in 1 .. node.children_count {
				branch_id := tc.a.child(&node, i)
				if !tc.valid_node_id(branch_id) {
					continue
				}
				if tc.a.nodes[int(branch_id)].kind != .match_branch {
					continue
				}
				tail := tc.branch_tail_expr_id(branch_id)
				if tc.ownership_collect_expr_result(lhs_name, tail, lhs_type, pos, mut
					moved_sources)
				{
					marked = true
				}
			}
			return marked
		}
		.or_expr {
			if node.children_count < 2 || node.value in ['!', '?'] {
				return false
			}
			tail := tc.branch_tail_expr_id(tc.a.child(&node, 1))
			return tc.ownership_collect_expr_result(lhs_name, tail, lhs_type, pos, mut
				moved_sources)
		}
		else {}
	}

	return tc.ownership_collect_expr_result(lhs_name, if_id, lhs_type, pos, mut moved_sources)
}

fn (mut tc TypeChecker) ownership_collect_expr_result(lhs_name string, expr_id flat.NodeId, lhs_type Type, pos flat.NodeId, mut moved_sources []OwnershipConditionalMoveSource) bool {
	if !tc.valid_node_id(expr_id) {
		return false
	}
	id := tc.ownership_unwrap_expr(expr_id)
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind in [.if_expr, .match_stmt, .or_expr] {
		return tc.ownership_collect_conditional_result(lhs_name, id, lhs_type, pos, mut
			moved_sources)
	}
	if tc.ownership_mark_aggregate_literal_storage(lhs_name, id, pos, false) {
		return true
	}
	name := tc.ownership_expr_ident_name(id)
	if name.len > 0 && tc.ownership_collect_named_conditional_move_sources(name, mut moved_sources) {
		return true
	}
	if tc.ownership_mark_from_call(lhs_name, id, pos) {
		return true
	}
	if tc.ownership_expr_is_to_owned_call(id) || tc.ownership_expr_is_owned_clone_call(id)
		|| tc.ownership_expr_is_ownership_call(id)
		|| tc.ownership_type_is_owned(tc.resolve_type(id)) {
		tc.ownership_mark_owned(lhs_name, tc.resolve_type(id), pos)
		return true
	}
	return false
}

fn (mut tc TypeChecker) ownership_collect_named_conditional_move_sources(name string, mut moved_sources []OwnershipConditionalMoveSource) bool {
	if name.len == 0 {
		return false
	}
	mut marked := false
	if name in tc.ownership_state().owned_vars {
		ownership_add_conditional_move_source(mut moved_sources, name, '')
		marked = true
	}
	for owned_name in tc.ownership_owned_descendant_names(name) {
		ownership_add_conditional_move_source(mut moved_sources, owned_name, owned_name[name.len..])
		marked = true
	}
	return marked
}

fn ownership_add_conditional_move_source(mut moved_sources []OwnershipConditionalMoveSource, source string, target_suffix string) {
	if source.len == 0 {
		return
	}
	for moved in moved_sources {
		if moved.source == source && moved.target_suffix == target_suffix {
			return
		}
	}
	moved_sources << OwnershipConditionalMoveSource{
		source:        source
		target_suffix: target_suffix
	}
}

fn (mut tc TypeChecker) ownership_after_params_field_arg(node flat.Node, info CallInfo, arg_node flat.Node, arg_id flat.NodeId, pos flat.NodeId, call_name string, mut call_borrows []string) {
	field_name := arg_node.value
	if field_name.len == 0 || info.name.len == 0 {
		return
	}
	param_idx := tc.ownership_call_params_struct_decl_param_idx(node, info)
	if param_idx < 0 {
		return
	}
	suffix := '.${field_name}'
	borrow_name := tc.ownership_borrowed_name(arg_id)
	if borrow_name.len > 0 {
		if moved := tc.ownership_moved_conflict(borrow_name) {
			tc.ownership_report_moved(moved.name, moved.info, arg_id)
			return
		}
		if tc.ownership_storage_participates(borrow_name) {
			tc.ownership_add_borrow(borrow_name, call_name, pos, false)
			call_borrows << borrow_name
			return
		}
	}
	tc.ownership_mark_fn_param_descendant_from_expr(info.name, param_idx, suffix, arg_id, pos)
	arg_name := tc.ownership_expr_ident_name(arg_id)
	if arg_name.len > 0 {
		tc.ownership_transfer_owned_descendants_to_param_with_suffix(arg_name, info.name,
			param_idx, suffix, pos)
	}
}

fn (mut tc TypeChecker) ownership_consume_conditional_call_arg(fn_name string, param_idx int, suffix string, arg_id flat.NodeId, target_type Type, pos flat.NodeId) bool {
	if fn_name.len == 0 || param_idx < 0 {
		return false
	}
	id := tc.ownership_unwrap_expr(arg_id)
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind !in [.if_expr, .match_stmt, .or_expr] {
		return false
	}
	tmp_name := '${fn_name}__conditional_arg_${param_idx}_${int(pos)}'
	mut moved_sources := []OwnershipConditionalMoveSource{}
	mut marked := tc.ownership_collect_conditional_result(tmp_name, id, target_type, pos, mut
		moved_sources)
	for move in moved_sources {
		tc.ownership_reject_global_move(move.source, pos, fn_name, true)
		if tc.ownership_move_var_result(move.source, fn_name, pos, true, fn_name, true) {
			tc.ownership_mark_call_param_owned(fn_name, param_idx, suffix + move.target_suffix,
				tc.ownership_type_name_for_var(move.source))
			marked = true
		}
	}
	if tmp_name in tc.ownership_state().owned_vars {
		tc.ownership_mark_call_param_owned(fn_name, param_idx, suffix,
			tc.ownership_type_name_for_var(tmp_name))
		marked = true
	}
	for owned_name in tc.ownership_owned_descendant_names(tmp_name) {
		source_suffix := owned_name[tmp_name.len..]
		tc.ownership_mark_call_param_owned(fn_name, param_idx, suffix + source_suffix,
			tc.ownership_type_name_for_var(owned_name))
		marked = true
	}
	tc.ownership_clear_temp_storage(tmp_name)
	tc.ownership_flush_value_branch_moves()
	return marked
}

fn (mut tc TypeChecker) ownership_mark_call_param_owned(fn_name string, param_idx int, suffix string, type_name string) {
	if suffix.len > 0 {
		tc.ownership_add_fn_param_descendant(fn_name, param_idx, suffix, type_name)
		return
	}
	mut st := tc.ownership_state()
	st.ownership_fn_params['${fn_name}__param_${param_idx}'] = true
}

fn (mut tc TypeChecker) ownership_clear_temp_storage(name string) {
	mut st := tc.ownership_state()
	st.owned_vars.delete(name)
	st.owned_var_types.delete(name)
	st.moved_vars.delete(name)
	st.array_lengths.delete(name)
	tc.ownership_clear_descendant_state(name)
}

fn (mut tc TypeChecker) ownership_mark_fn_param_aggregate_literal_descendants(fn_name string, param_idx int, base_suffix string, expr_id flat.NodeId, pos flat.NodeId) bool {
	if fn_name.len == 0 || param_idx < 0 || !tc.valid_node_id(expr_id) {
		return false
	}
	id := tc.ownership_unwrap_expr(expr_id)
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.array_literal {
			mut marked := false
			for i in 0 .. node.children_count {
				elem_id := tc.a.child(&node, i)
				suffix := '${base_suffix}[${i}]'
				if tc.ownership_mark_fn_param_aggregate_literal_descendants(fn_name, param_idx, suffix, elem_id, pos)
					|| tc.ownership_mark_fn_param_descendant_from_expr(fn_name, param_idx, suffix, elem_id, pos) {
					marked = true
				}
			}
			return marked
		}
		.array_init {
			mut marked := false
			mut elem_idx := 0
			for i in 0 .. node.children_count {
				child_id := tc.a.child(&node, i)
				child := tc.a.nodes[int(child_id)]
				if child.kind == .field_init {
					if child.value == 'init' && child.children_count > 0 {
						init_id := tc.a.child(&child, 0)
						suffix := '${base_suffix}[*]'
						if tc.ownership_mark_fn_param_aggregate_literal_descendants(fn_name, param_idx, suffix, init_id, pos)
							|| tc.ownership_mark_fn_param_descendant_from_expr(fn_name, param_idx, suffix, init_id, pos) {
							marked = true
						}
					}
					continue
				}
				suffix := '${base_suffix}[${elem_idx}]'
				if tc.ownership_mark_fn_param_aggregate_literal_descendants(fn_name, param_idx, suffix, child_id, pos)
					|| tc.ownership_mark_fn_param_descendant_from_expr(fn_name, param_idx, suffix, child_id, pos) {
					marked = true
				}
				elem_idx++
			}
			return marked
		}
		.map_init {
			mut marked := false
			for i := 0; i < node.children_count; i += 2 {
				if i + 1 >= node.children_count {
					continue
				}
				key_id := tc.a.child(&node, i)
				key_suffix := ownership_map_key_storage_suffix(base_suffix)
				if tc.ownership_mark_fn_param_aggregate_literal_descendants(fn_name, param_idx, key_suffix, key_id, pos)
					|| tc.ownership_mark_fn_param_descendant_from_expr(fn_name, param_idx, key_suffix, key_id, pos) {
					marked = true
				}
				key_part := tc.ownership_index_key_part(key_id)
				if key_part.len == 0 {
					continue
				}
				value_id := tc.a.child(&node, i + 1)
				suffix := '${base_suffix}[${key_part}]'
				if tc.ownership_mark_fn_param_aggregate_literal_descendants(fn_name, param_idx, suffix, value_id, pos)
					|| tc.ownership_mark_fn_param_descendant_from_expr(fn_name, param_idx, suffix, value_id, pos) {
					marked = true
				}
			}
			return marked
		}
		.struct_init {
			init_type := tc.parse_type(node.value)
			if init_type !is Struct {
				return false
			}
			fields := tc.structs[(init_type as Struct).name] or { []StructField{} }
			mut marked := false
			for i in 0 .. node.children_count {
				field_id := tc.a.child(&node, i)
				field := tc.a.nodes[int(field_id)]
				if field.kind != .field_init || field.children_count == 0 {
					continue
				}
				mut field_name := field.value
				if field_name.len == 0 && i < fields.len {
					field_name = fields[i].name
				}
				if field_name.len == 0 {
					continue
				}
				value_id := tc.a.child(&field, 0)
				suffix := '${base_suffix}.${field_name}'
				if tc.ownership_mark_fn_param_aggregate_literal_descendants(fn_name, param_idx, suffix, value_id, pos)
					|| tc.ownership_mark_fn_param_descendant_from_expr(fn_name, param_idx, suffix, value_id, pos) {
					marked = true
				}
			}
			return marked
		}
		.assoc {
			init_type := tc.resolve_type(id)
			if init_type !is Struct || node.children_count == 0 {
				return false
			}
			fields := tc.structs[(init_type as Struct).name] or { []StructField{} }
			mut marked := false
			mut explicit_fields := map[string]bool{}
			for i in 1 .. node.children_count {
				field_id := tc.a.child(&node, i)
				field := tc.a.nodes[int(field_id)]
				if field.kind != .field_init || field.children_count == 0 {
					continue
				}
				mut field_name := field.value
				field_idx := i - 1
				if field_name.len == 0 && field_idx < fields.len {
					field_name = fields[field_idx].name
				}
				if field_name.len > 0 {
					explicit_fields[field_name] = true
				}
			}
			base_id := tc.a.child(&node, 0)
			if tc.ownership_mark_fn_param_assoc_base_descendants(fn_name, param_idx, base_suffix,
				base_id, explicit_fields, pos)
			{
				marked = true
			}
			for i in 1 .. node.children_count {
				field_id := tc.a.child(&node, i)
				field := tc.a.nodes[int(field_id)]
				if field.kind != .field_init || field.children_count == 0 {
					continue
				}
				mut field_name := field.value
				field_idx := i - 1
				if field_name.len == 0 && field_idx < fields.len {
					field_name = fields[field_idx].name
				}
				if field_name.len == 0 {
					continue
				}
				value_id := tc.a.child(&field, 0)
				suffix := '${base_suffix}.${field_name}'
				if tc.ownership_mark_fn_param_aggregate_literal_descendants(fn_name, param_idx, suffix, value_id, pos)
					|| tc.ownership_mark_fn_param_descendant_from_expr(fn_name, param_idx, suffix, value_id, pos) {
					marked = true
				}
			}
			return marked
		}
		else {}
	}

	return false
}

fn (mut tc TypeChecker) ownership_mark_fn_param_assoc_base_descendants(fn_name string, param_idx int, base_suffix string, base_id flat.NodeId, explicit_fields map[string]bool, pos flat.NodeId) bool {
	base_name := tc.ownership_expr_ident_name(base_id)
	if base_name.len == 0 {
		if tc.ownership_mark_fn_param_aggregate_literal_descendants(fn_name, param_idx,
			base_suffix, base_id, pos)
		{
			return true
		}
		return tc.ownership_mark_fn_param_descendant_from_expr(fn_name, param_idx, base_suffix,
			base_id, pos)
	}
	mut marked := false
	mut st := tc.ownership_state()
	if base_name in st.owned_vars {
		tc.ownership_reject_global_move(base_name, pos, fn_name, true)
		type_name := tc.ownership_type_name_for_var(base_name)
		if tc.ownership_move_var_result(base_name, fn_name, pos, true, fn_name, true) {
			tc.ownership_mark_call_param_owned(fn_name, param_idx, base_suffix, type_name)
			marked = true
		}
	}
	for source_name in tc.ownership_owned_descendant_names(base_name) {
		if ownership_assoc_base_descendant_overridden(base_name, source_name, explicit_fields) {
			continue
		}
		suffix := base_suffix + source_name[base_name.len..]
		type_name := tc.ownership_type_name_for_var(source_name)
		if tc.ownership_move_var_result(source_name, fn_name, pos, true, fn_name, true) {
			tc.ownership_add_fn_param_descendant(fn_name, param_idx, suffix, type_name)
			marked = true
		}
	}
	return marked
}

fn (mut tc TypeChecker) ownership_mark_fn_param_descendant_from_expr(fn_name string, param_idx int, suffix string, expr_id flat.NodeId, pos flat.NodeId) bool {
	if fn_name.len == 0 || param_idx < 0 || suffix.len == 0 || !tc.valid_node_id(expr_id) {
		return false
	}
	id := tc.ownership_unwrap_expr(expr_id)
	if !tc.valid_node_id(id) {
		return false
	}
	source_name := tc.ownership_expr_ident_name(id)
	if source_name.len > 0 && source_name in tc.ownership_state().owned_vars {
		tc.ownership_reject_global_move(source_name, pos, fn_name, true)
		if moved := tc.ownership_state().moved_vars[source_name] {
			tc.ownership_report_moved(source_name, moved, id)
			return false
		}
		tc.ownership_move_var(source_name, fn_name, pos, true, fn_name, true)
		tc.ownership_add_fn_param_descendant(fn_name, param_idx, suffix,
			tc.ownership_type_name_for_var(source_name))
		return true
	}
	if source_name.len > 0
		&& tc.ownership_transfer_owned_descendants_to_param_with_suffix(source_name, fn_name, param_idx, suffix, pos) {
		return true
	}
	if tc.ownership_expr_is_to_owned_call(id) || tc.ownership_expr_is_owned_clone_call(id)
		|| tc.ownership_expr_is_ownership_call(id)
		|| tc.ownership_type_is_owned(tc.resolve_type(id)) {
		tc.ownership_add_fn_param_descendant(fn_name, param_idx, suffix, tc.resolve_type(id).name())
		return true
	}
	return false
}

fn (mut tc TypeChecker) ownership_mark_return_aggregate_literal_descendants(fn_name string, slot_idx int, base_suffix string, expr_id flat.NodeId, pos flat.NodeId) bool {
	if fn_name.len == 0 || slot_idx < 0 || !tc.valid_node_id(expr_id) {
		return false
	}
	id := tc.ownership_unwrap_expr(expr_id)
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.array_literal {
			mut marked := false
			for i in 0 .. node.children_count {
				elem_id := tc.a.child(&node, i)
				suffix := '${base_suffix}[${i}]'
				if tc.ownership_mark_return_aggregate_literal_descendants(fn_name, slot_idx, suffix, elem_id, pos)
					|| tc.ownership_add_return_descendant_from_expr(fn_name, slot_idx, suffix, elem_id, pos) {
					marked = true
				}
			}
			return marked
		}
		.array_init {
			mut marked := false
			mut elem_idx := 0
			for i in 0 .. node.children_count {
				child_id := tc.a.child(&node, i)
				child := tc.a.nodes[int(child_id)]
				if child.kind == .field_init {
					if child.value == 'init' && child.children_count > 0 {
						init_id := tc.a.child(&child, 0)
						suffix := '${base_suffix}[*]'
						if tc.ownership_mark_return_aggregate_literal_descendants(fn_name, slot_idx, suffix, init_id, pos)
							|| tc.ownership_add_return_descendant_from_expr(fn_name, slot_idx, suffix, init_id, pos) {
							marked = true
						}
					}
					continue
				}
				suffix := '${base_suffix}[${elem_idx}]'
				if tc.ownership_mark_return_aggregate_literal_descendants(fn_name, slot_idx, suffix, child_id, pos)
					|| tc.ownership_add_return_descendant_from_expr(fn_name, slot_idx, suffix, child_id, pos) {
					marked = true
				}
				elem_idx++
			}
			return marked
		}
		.map_init {
			mut marked := false
			for i := 0; i < node.children_count; i += 2 {
				if i + 1 >= node.children_count {
					continue
				}
				key_id := tc.a.child(&node, i)
				key_suffix := ownership_map_key_storage_suffix(base_suffix)
				if tc.ownership_mark_return_aggregate_literal_descendants(fn_name, slot_idx, key_suffix, key_id, pos)
					|| tc.ownership_add_return_descendant_from_expr(fn_name, slot_idx, key_suffix, key_id, pos) {
					marked = true
				}
				key_part := tc.ownership_index_key_part(key_id)
				if key_part.len == 0 {
					continue
				}
				value_id := tc.a.child(&node, i + 1)
				suffix := '${base_suffix}[${key_part}]'
				if tc.ownership_mark_return_aggregate_literal_descendants(fn_name, slot_idx, suffix, value_id, pos)
					|| tc.ownership_add_return_descendant_from_expr(fn_name, slot_idx, suffix, value_id, pos) {
					marked = true
				}
			}
			return marked
		}
		.struct_init {
			init_type := tc.parse_type(node.value)
			if init_type !is Struct {
				return false
			}
			fields := tc.structs[(init_type as Struct).name] or { []StructField{} }
			mut marked := false
			mut explicit_fields := map[string]bool{}
			for i in 0 .. node.children_count {
				field_id := tc.a.child(&node, i)
				field := tc.a.nodes[int(field_id)]
				if field.kind != .field_init || field.children_count == 0 {
					continue
				}
				mut field_name := field.value
				if field_name.len == 0 && i < fields.len {
					field_name = fields[i].name
				}
				if field_name.len == 0 {
					continue
				}
				explicit_fields[field_name] = true
				value_id := tc.a.child(&field, 0)
				suffix := '${base_suffix}.${field_name}'
				if tc.ownership_mark_return_aggregate_literal_descendants(fn_name, slot_idx, suffix, value_id, pos)
					|| tc.ownership_add_return_descendant_from_expr(fn_name, slot_idx, suffix, value_id, pos) {
					marked = true
				}
			}
			if decl := tc.ownership_struct_decl_node((init_type as Struct).name) {
				for i in 0 .. decl.children_count {
					field := tc.a.child_node(decl, i)
					if field.kind != .field_decl || field.children_count == 0
						|| field.value.len == 0 || field.value in explicit_fields {
						continue
					}
					default_id := tc.a.child(field, 0)
					suffix := '${base_suffix}.${field.value}'
					if tc.ownership_mark_return_aggregate_literal_descendants(fn_name, slot_idx, suffix, default_id, pos)
						|| tc.ownership_add_return_descendant_from_expr(fn_name, slot_idx, suffix, default_id, pos) {
						marked = true
					}
				}
			}
			return marked
		}
		.assoc {
			init_type := tc.resolve_type(id)
			if init_type !is Struct || node.children_count == 0 {
				return false
			}
			fields := tc.structs[(init_type as Struct).name] or { []StructField{} }
			mut marked := false
			mut explicit_fields := map[string]bool{}
			for i in 1 .. node.children_count {
				field_id := tc.a.child(&node, i)
				field := tc.a.nodes[int(field_id)]
				if field.kind != .field_init || field.children_count == 0 {
					continue
				}
				mut field_name := field.value
				field_idx := i - 1
				if field_name.len == 0 && field_idx < fields.len {
					field_name = fields[field_idx].name
				}
				if field_name.len > 0 {
					explicit_fields[field_name] = true
				}
			}
			base_id := tc.a.child(&node, 0)
			if tc.ownership_mark_return_assoc_base_descendants(fn_name, slot_idx, base_suffix,
				base_id, explicit_fields, pos)
			{
				marked = true
			}
			for i in 1 .. node.children_count {
				field_id := tc.a.child(&node, i)
				field := tc.a.nodes[int(field_id)]
				if field.kind != .field_init || field.children_count == 0 {
					continue
				}
				mut field_name := field.value
				field_idx := i - 1
				if field_name.len == 0 && field_idx < fields.len {
					field_name = fields[field_idx].name
				}
				if field_name.len == 0 {
					continue
				}
				value_id := tc.a.child(&field, 0)
				suffix := '${base_suffix}.${field_name}'
				if tc.ownership_mark_return_aggregate_literal_descendants(fn_name, slot_idx, suffix, value_id, pos)
					|| tc.ownership_add_return_descendant_from_expr(fn_name, slot_idx, suffix, value_id, pos) {
					marked = true
				}
			}
			return marked
		}
		else {}
	}

	return false
}

fn (mut tc TypeChecker) ownership_mark_return_assoc_base_descendants(fn_name string, slot_idx int, base_suffix string, base_id flat.NodeId, explicit_fields map[string]bool, pos flat.NodeId) bool {
	base_name := tc.ownership_expr_ident_name(base_id)
	if base_name.len == 0 {
		if tc.ownership_mark_return_aggregate_literal_descendants(fn_name, slot_idx, base_suffix,
			base_id, pos)
		{
			return true
		}
		return tc.ownership_add_return_descendant_from_expr(fn_name, slot_idx, base_suffix,
			base_id, pos)
	}
	mut marked := false
	mut st := tc.ownership_state()
	if base_name in st.owned_vars {
		tc.ownership_reject_global_move(base_name, pos, fn_name, true)
		type_name := tc.ownership_type_name_for_var(base_name)
		if tc.ownership_move_var_result(base_name, fn_name, pos, true, fn_name, false) {
			if base_suffix.len == 0 {
				st.mark_fn_return_owned(fn_name)
				tc.ownership_add_fn_return_slot(fn_name, slot_idx)
			} else {
				tc.ownership_add_fn_return_descendant(fn_name, slot_idx, base_suffix, type_name)
			}
			marked = true
		}
	}
	for source_name in tc.ownership_owned_descendant_names(base_name) {
		if ownership_assoc_base_descendant_overridden(base_name, source_name, explicit_fields) {
			continue
		}
		suffix := base_suffix + source_name[base_name.len..]
		type_name := tc.ownership_type_name_for_var(source_name)
		if tc.ownership_move_var_result(source_name, fn_name, pos, true, fn_name, false) {
			tc.ownership_add_fn_return_descendant(fn_name, slot_idx, suffix, type_name)
			marked = true
		}
	}
	return marked
}

fn (mut tc TypeChecker) ownership_add_return_descendant_from_expr(fn_name string, slot_idx int, suffix string, expr_id flat.NodeId, pos flat.NodeId) bool {
	if fn_name.len == 0 || slot_idx < 0 || suffix.len == 0 || !tc.valid_node_id(expr_id) {
		return false
	}
	tmp_name := '${fn_name}__return_${slot_idx}${suffix}'
	if !tc.ownership_mark_storage_from_expr(tmp_name, expr_id, tc.resolve_type(expr_id), pos) {
		return false
	}
	type_name := tc.ownership_type_name_for_var(tmp_name)
	{
		mut st := tc.ownership_state()
		st.owned_vars.delete(tmp_name)
		st.owned_var_types.delete(tmp_name)
		st.moved_vars.delete(tmp_name)
	}
	tc.ownership_add_fn_return_descendant(fn_name, slot_idx, suffix, type_name)
	return true
}

fn (mut tc TypeChecker) ownership_after_call(id flat.NodeId, node flat.Node, info CallInfo) {
	if tc.ownership_effects_disabled() {
		return
	}
	mut st := tc.ownership_state()
	call_name := if info.name.len > 0 { info.name } else { tc.ownership_call_name(id) }
	mut call_borrows := []string{}
	if info.has_receiver && node.children_count > 0 && info.params.len > 0 {
		fn_node := tc.a.child_node(&node, 0)
		if fn_node.kind == .selector && fn_node.children_count > 0 {
			recv_id := tc.a.child(fn_node, 0)
			recv_name := tc.ownership_expr_ident_name(recv_id)
			if !tc.ownership_method_keeps_receiver(fn_node.value)
				&& !tc.ownership_array_builtin_keeps_receiver(recv_id, fn_node.value)
				&& !tc.ownership_string_builtin_keeps_receiver(recv_id, fn_node.value) {
				if info.params[0] is Pointer {
					if recv_name.len > 0 && tc.ownership_storage_participates(recv_name) {
						tc.ownership_add_borrow(recv_name, call_name, id, tc.ownership_call_param_is_mut(call_name,
							0))
						call_borrows << recv_name
					}
				} else {
					if recv_name.len > 0 {
						tc.ownership_reject_global_move(recv_name, id, call_name, true)
						if recv_name in st.owned_vars {
							tc.ownership_move_var(recv_name, call_name, id, true, call_name, true)
						}
						tc.ownership_transfer_owned_descendants_to_param(recv_name, call_name, 0,
							id)
					} else {
						_ := tc.ownership_consume_conditional_call_arg(call_name, 0, '', recv_id,
							info.params[0], id)
					}
				}
			}
		}
	}
	for i in 1 .. node.children_count {
		arg_node := tc.a.child_node(&node, i)
		arg_id := tc.call_arg_value(tc.a.child(&node, i))
		if arg_node.kind == .field_init {
			tc.ownership_after_params_field_arg(node, info, arg_node, arg_id, id, call_name, mut
				call_borrows)
			continue
		}
		param_idx := tc.ownership_call_arg_decl_param_idx(info, i)
		type_param_idx := param_idx + tc.ownership_call_arg_shift(node, info)
		variadic_elem_idx := tc.ownership_call_arg_variadic_elem_idx(info, type_param_idx)
		target_param_idx := tc.ownership_call_arg_variadic_decl_param_idx(param_idx,
			variadic_elem_idx)
		target_suffix := ownership_call_arg_variadic_suffix(variadic_elem_idx)
		expected := tc.ownership_call_arg_expected_type(info, type_param_idx, variadic_elem_idx)
		if tc.ownership_mark_array_insert_prepend_arg(node, call_name, param_idx, arg_id, id) {
			continue
		}
		borrow_param_idx := if variadic_elem_idx >= 0 { info.params.len - 1 } else { type_param_idx }
		borrow_name := if expected is Pointer {
			explicit := tc.ownership_borrowed_name(arg_id)
			if explicit.len > 0 {
				explicit
			} else {
				tc.ownership_expr_ident_name(arg_id)
			}
		} else {
			tc.ownership_borrowed_name(arg_id)
		}
		if borrow_name.len > 0 {
			if moved := tc.ownership_moved_conflict(borrow_name) {
				tc.ownership_report_moved(moved.name, moved.info, arg_id)
				continue
			}
		}
		if borrow_name.len > 0 && tc.ownership_storage_participates(borrow_name) {
			tc.ownership_add_borrow(borrow_name, call_name, id, tc.ownership_call_param_is_mut(call_name,
				borrow_param_idx))
			call_borrows << borrow_name
			continue
		}
		method_value := tc.ownership_consume_method_value_receiver(arg_id, call_name, id)
		if method_value.consumed {
			if method_value.borrow_name.len > 0 {
				call_borrows << method_value.borrow_name
			}
			continue
		}
		arg_name := tc.ownership_expr_ident_name(arg_id)
		if arg_name.len == 0 {
			if expected !is Void && expected !is Pointer {
				if tc.ownership_consume_array_element_method_result(arg_id, call_name, id) {
					continue
				}
				if tc.ownership_consume_conditional_call_arg(call_name, target_param_idx,
					target_suffix, arg_id, expected, id)
				{
					continue
				}
				tc.ownership_mark_fn_param_aggregate_literal_descendants(call_name,
					target_param_idx, target_suffix, arg_id, id)
			}
			continue
		}
		if moved := tc.ownership_moved_conflict(arg_name) {
			tc.ownership_report_moved(moved.name, moved.info, arg_id)
			continue
		}
		tc.ownership_reject_global_move(arg_name, id, call_name, true)
		if arg_name in st.owned_vars {
			type_name := tc.ownership_type_name_for_var(arg_name)
			tc.ownership_move_var(arg_name, call_name, id, true, call_name, true)
			if variadic_elem_idx >= 0 {
				tc.ownership_add_fn_param_descendant(call_name, target_param_idx, target_suffix,
					type_name)
			} else {
				key := '${call_name}__param_${param_idx}'
				st.ownership_fn_params[key] = true
			}
		} else {
			moved_types := tc.ownership_move_overlapping_dynamic_storage(arg_name, call_name, id,
				true, call_name, true)
			if moved_types.len > 0 {
				if variadic_elem_idx >= 0 {
					tc.ownership_add_fn_param_descendant(call_name, target_param_idx,
						target_suffix, moved_types[0])
				} else {
					key := '${call_name}__param_${param_idx}'
					st.ownership_fn_params[key] = true
				}
			}
		}
		if expected !is Void && expected !is Pointer {
			tc.ownership_transfer_owned_descendants_to_param_with_suffix(arg_name, call_name,
				target_param_idx, target_suffix, id)
		}
	}
	for name in call_borrows {
		tc.ownership_release_borrow(name, call_name)
	}
}

fn (mut tc TypeChecker) ownership_consume_method_value_receiver(arg_id flat.NodeId, call_name string, pos flat.NodeId) OwnershipMethodValueReceiverResult {
	clean_id := tc.ownership_unwrap_expr(arg_id)
	if !tc.valid_node_id(clean_id) {
		return OwnershipMethodValueReceiverResult{}
	}
	node := tc.a.nodes[int(clean_id)]
	if node.kind != .selector || node.children_count == 0 || !tc.expr_is_method_value(clean_id) {
		return OwnershipMethodValueReceiverResult{}
	}
	recv_id := tc.a.child(&node, 0)
	recv_name := tc.ownership_expr_ident_name(recv_id)
	if recv_name.len == 0 {
		return OwnershipMethodValueReceiverResult{
			consumed: true
		}
	}
	info := tc.ownership_method_value_call_info(node, recv_id) or {
		return OwnershipMethodValueReceiverResult{
			consumed: true
		}
	}
	if info.params.len == 0 || tc.ownership_method_keeps_receiver(node.value) {
		return OwnershipMethodValueReceiverResult{
			consumed: true
		}
	}
	if info.params[0] is Pointer {
		if tc.ownership_storage_participates(recv_name) {
			tc.ownership_add_borrow(recv_name, call_name, pos, tc.ownership_call_param_is_mut(info.name,
				0))
			return OwnershipMethodValueReceiverResult{
				consumed:    true
				borrow_name: recv_name
			}
		}
		return OwnershipMethodValueReceiverResult{
			consumed: true
		}
	}
	st := tc.ownership_state()
	tc.ownership_reject_global_move(recv_name, pos, call_name, true)
	if recv_name in st.owned_vars {
		tc.ownership_move_var(recv_name, call_name, pos, true, call_name, true)
	} else {
		recv_type := tc.resolve_type(recv_id)
		if tc.ownership_type_is_owned(recv_type) {
			tc.ownership_mark_owned(recv_name, recv_type, pos)
			tc.ownership_move_var(recv_name, call_name, pos, true, call_name, true)
		} else {
			_ :=
				tc.ownership_move_owned_descendants(recv_name, call_name, pos, true, call_name, true)
		}
	}
	return OwnershipMethodValueReceiverResult{
		consumed: true
	}
}

fn (mut tc TypeChecker) ownership_method_value_call_info(node flat.Node, recv_id flat.NodeId) ?CallInfo {
	if node.kind != .selector || node.value.len == 0 {
		return none
	}
	recv_type := tc.resolve_type(recv_id)
	for method_name in receiver_method_name_candidates(unwrap_pointer(recv_type), node.value,
		tc.cur_module) {
		if method_name !in tc.fn_param_types {
			continue
		}
		if !tc.method_can_be_called_on_receiver(recv_type, node.value, method_name) {
			continue
		}
		return tc.call_info(method_name, true)
	}
	if clean := struct_type_from_type(unwrap_pointer(recv_type)) {
		if info := tc.resolve_generic_struct_method(clean.name, node.value) {
			return info
		}
	}
	return none
}

fn (mut tc TypeChecker) ownership_call_param_is_mut(fn_name string, param_idx int) bool {
	if param_idx < 0 {
		return false
	}
	params := tc.ownership_state().ownership_fn_param_mut[fn_name] or { return false }
	if param_idx >= params.len {
		return false
	}
	return params[param_idx]
}

fn (mut tc TypeChecker) ownership_after_return(id flat.NodeId, node flat.Node) {
	if tc.ownership_effects_disabled() {
		return
	}
	mut st := tc.ownership_state()
	if st.cur_fn.len == 0 {
		return
	}
	for i in 0 .. node.children_count {
		expr_id := tc.a.child(&node, i)
		name := tc.ownership_expr_ident_name(expr_id)
		if fn_value := tc.ownership_fn_value_name_from_expr(expr_id) {
			tc.ownership_note_fn_return_fn_value(st.cur_fn, fn_value)
		} else if fn_value := tc.ownership_fn_return_fn_value_from_call(expr_id) {
			tc.ownership_note_fn_return_fn_value(st.cur_fn, fn_value)
		}
		tc.ownership_mark_return_aggregate_literal_descendants(st.cur_fn, i, '', expr_id, id)
		tc.ownership_move_conditional_return_sources(st.cur_fn, i, expr_id, id)
		if tc.ownership_mark_return_from_array_element_method(st.cur_fn, i, expr_id, id) {
			continue
		}
		if name.len > 0 {
			tc.ownership_reject_global_move(name, expr_id, st.cur_fn, true)
			for slot_idx in tc.ownership_return_slot_indices(expr_id, i, '') {
				tc.ownership_mark_return_owned_descendants(st.cur_fn, slot_idx, name, id)
			}
			if name in st.owned_vars {
				st.mark_fn_return_owned(st.cur_fn)
				for slot_idx in tc.ownership_return_slot_indices(expr_id, i, '') {
					tc.ownership_add_fn_return_slot(st.cur_fn, slot_idx)
				}
				tc.ownership_move_var(name, st.cur_fn, id, true, st.cur_fn, false)
			}
		}
		if name.len > 0 && name !in st.owned_vars {
			moved_types := tc.ownership_move_overlapping_dynamic_storage(name, st.cur_fn, id, true,
				st.cur_fn, false)
			if moved_types.len > 0 {
				st.mark_fn_return_owned(st.cur_fn)
				for slot_idx in tc.ownership_return_slot_indices(expr_id, i, '') {
					tc.ownership_add_fn_return_slot(st.cur_fn, slot_idx)
				}
			}
		}
		if tc.ownership_expr_is_to_owned_call(expr_id)
			|| tc.ownership_expr_is_owned_clone_call(expr_id)
			|| tc.ownership_expr_is_ownership_call(expr_id)
			|| tc.ownership_type_is_owned(tc.resolve_type(expr_id)) {
			st.mark_fn_return_owned(st.cur_fn)
			call_name := tc.ownership_call_name(expr_id)
			for slot_idx in tc.ownership_return_slot_indices(expr_id, i, call_name) {
				tc.ownership_add_fn_return_slot(st.cur_fn, slot_idx)
			}
		}
	}
	entries := tc.ownership_live_drop_entries()
	return_index := st.drop_return_counts[st.cur_fn] or { 0 }
	st.drop_return_counts[st.cur_fn] = return_index + 1
	if entries.len > 0 {
		st.drop_at_returns['${st.cur_fn}\x01${return_index}'] = entries
		st.drop_at_return_nodes['${st.cur_fn}\x01${int(id)}'] = entries
		tc.ownership_note_drop_types(st.cur_fn, entries)
	}
	tc.ownership_check_return_defers()
	tc.ownership_state().path_active = false
}

fn (mut tc TypeChecker) ownership_record_propagation_drops() {
	if tc.ownership_effects_disabled() {
		return
	}
	mut st := tc.ownership_state()
	if st.cur_fn.len == 0 {
		return
	}
	entries := tc.ownership_live_drop_entries()
	index := st.drop_propagation_counts[st.cur_fn] or { 0 }
	st.drop_propagation_counts[st.cur_fn] = index + 1
	if entries.len > 0 {
		st.drop_at_propagations['${st.cur_fn}\x01${index}'] = entries
		tc.ownership_note_drop_types(st.cur_fn, entries)
	}
}

fn (mut tc TypeChecker) ownership_mark_return_from_array_element_method(fn_name string, slot_idx int, expr_id flat.NodeId, pos flat.NodeId) bool {
	if fn_name.len == 0 || slot_idx < 0 {
		return false
	}
	call_id := tc.ownership_unwrap_expr(expr_id)
	if !tc.valid_node_id(call_id) {
		return false
	}
	node := tc.a.nodes[int(call_id)]
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return false
	}
	method := fn_node.value
	if method !in ['first', 'last', 'pop', 'pop_left'] {
		return false
	}
	recv_id := tc.a.child(fn_node, 0)
	if unwrap_pointer(tc.resolve_type(recv_id)) !is Array {
		return false
	}
	array_name := tc.ownership_expr_ident_name(recv_id)
	if array_name.len == 0 {
		return false
	}
	source_key := tc.ownership_array_element_method_source_key(array_name, method)
	if source_key.len == 0 {
		return false
	}
	mut marked := false
	mut st := tc.ownership_state()
	for return_slot in tc.ownership_return_slot_indices(expr_id, slot_idx, '') {
		for source_name in tc.ownership_owned_descendant_names(source_key) {
			suffix := source_name[source_key.len..]
			type_name := tc.ownership_type_name_for_var(source_name)
			if tc.ownership_move_var_result(source_name, fn_name, pos, true, fn_name, false) {
				tc.ownership_add_fn_return_descendant(fn_name, return_slot, suffix, type_name)
				marked = true
			}
		}
		if source_key in st.owned_vars {
			if tc.ownership_move_var_result(source_key, fn_name, pos, true, fn_name, false) {
				tc.ownership_add_fn_return_slot(fn_name, return_slot)
				st.mark_fn_return_owned(fn_name)
				marked = true
			}
		} else {
			moved_types := tc.ownership_move_overlapping_dynamic_storage(source_key, fn_name, pos,
				true, fn_name, false)
			if moved_types.len > 0 {
				tc.ownership_add_fn_return_slot(fn_name, return_slot)
				st.mark_fn_return_owned(fn_name)
				marked = true
			}
		}
	}
	if marked {
		st.mark_fn_return_owned(fn_name)
	}
	tc.ownership_update_array_length_after_element_method(array_name, method)
	tc.ownership_clear_removed_array_element_after_method(array_name, source_key, method)
	return marked
}

fn (mut tc TypeChecker) ownership_move_conditional_return_sources(fn_name string, slot_idx int, expr_id flat.NodeId, pos flat.NodeId) bool {
	if fn_name.len == 0 || slot_idx < 0 {
		return false
	}
	cond_id := tc.ownership_unwrap_expr(expr_id)
	if !tc.valid_node_id(cond_id) {
		return false
	}
	cond_node := tc.a.nodes[int(cond_id)]
	if cond_node.kind !in [.if_expr, .match_stmt, .or_expr] {
		return false
	}
	tmp_name := '${fn_name}__return_conditional_${slot_idx}_${int(pos)}'
	mut moved_sources := []OwnershipConditionalMoveSource{}
	mut marked := tc.ownership_collect_conditional_result(tmp_name, cond_id,
		tc.resolve_type(cond_id), pos, mut moved_sources)
	mut st := tc.ownership_state()
	for move in moved_sources {
		tc.ownership_reject_global_move(move.source, pos, fn_name, true)
		if tc.ownership_move_var_result(move.source, fn_name, pos, true, fn_name, false) {
			st.mark_fn_return_owned(fn_name)
			type_name := tc.ownership_type_name_for_var(move.source)
			for return_slot in tc.ownership_return_slot_indices(expr_id, slot_idx, '') {
				if move.target_suffix.len == 0 {
					tc.ownership_add_fn_return_slot(fn_name, return_slot)
				} else {
					tc.ownership_add_fn_return_descendant(fn_name, return_slot, move.target_suffix,
						type_name)
				}
			}
			marked = true
		}
	}
	if tmp_name in st.owned_vars {
		st.mark_fn_return_owned(fn_name)
		for return_slot in tc.ownership_return_slot_indices(expr_id, slot_idx, '') {
			tc.ownership_add_fn_return_slot(fn_name, return_slot)
		}
		marked = true
	}
	for owned_name in tc.ownership_owned_descendant_names(tmp_name) {
		suffix := owned_name[tmp_name.len..]
		type_name := tc.ownership_type_name_for_var(owned_name)
		for return_slot in tc.ownership_return_slot_indices(expr_id, slot_idx, '') {
			tc.ownership_add_fn_return_descendant(fn_name, return_slot, suffix, type_name)
		}
		marked = true
	}
	tc.ownership_clear_temp_storage(tmp_name)
	tc.ownership_flush_value_branch_moves()
	return marked
}

fn (mut tc TypeChecker) ownership_mark_return_owned_descendants(fn_name string, slot_idx int, source_prefix string, pos flat.NodeId) bool {
	if fn_name.len == 0 || slot_idx < 0 || source_prefix.len == 0 {
		return false
	}
	mut moved_any := false
	for source_name in tc.ownership_owned_descendant_names(source_prefix) {
		suffix := source_name[source_prefix.len..]
		type_name := tc.ownership_type_name_for_var(source_name)
		if tc.ownership_move_var_result(source_name, fn_name, pos, true, fn_name, false) {
			tc.ownership_add_fn_return_descendant(fn_name, slot_idx, suffix, type_name)
			moved_any = true
		}
	}
	return moved_any
}

fn (mut tc TypeChecker) ownership_consume_expr(expr_id flat.NodeId, target string, at flat.NodeId) {
	if tc.ownership_effects_disabled() {
		return
	}
	if tc.ownership_consume_array_element_method_result(expr_id, target, at) {
		return
	}
	mut st := tc.ownership_state()
	name := tc.ownership_expr_ident_name(expr_id)
	if name.len == 0 {
		_ := tc.ownership_consume_conditional_expr(expr_id, target, at)
		return
	}
	tc.ownership_reject_global_move(name, at, target, false)
	if moved := tc.ownership_moved_conflict(name) {
		tc.ownership_report_moved(moved.name, moved.info, at)
		return
	}
	if name !in st.owned_vars {
		_ := tc.ownership_move_overlapping_dynamic_storage(name, target, at, false, '', false)
		_ := tc.ownership_consume_owned_descendants(name, target, at)
		return
	}
	tc.ownership_move_var(name, target, at, false, '', false)
}

fn (mut tc TypeChecker) ownership_consume_owned_descendants(source_prefix string, target string, at flat.NodeId) bool {
	if source_prefix.len == 0 {
		return false
	}
	mut moved_any := false
	for source_name in tc.ownership_owned_descendant_names(source_prefix) {
		if tc.ownership_move_var_result(source_name, target, at, false, '', false) {
			moved_any = true
		}
	}
	return moved_any
}

fn (mut tc TypeChecker) ownership_consume_conditional_expr(expr_id flat.NodeId, target string, at flat.NodeId) bool {
	id := tc.ownership_unwrap_expr(expr_id)
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind !in [.if_expr, .match_stmt, .or_expr] {
		return false
	}
	tmp_name := '${tc.ownership_state().cur_fn}__conditional_sink_${int(at)}'
	mut moved_sources := []OwnershipConditionalMoveSource{}
	mut consumed := tc.ownership_collect_conditional_result(tmp_name, id, tc.resolve_type(id), at, mut
		moved_sources)
	for move in moved_sources {
		tc.ownership_reject_global_move(move.source, at, target, false)
		if tc.ownership_move_var_result(move.source, target, at, false, '', false) {
			consumed = true
		}
	}
	if tmp_name in tc.ownership_state().owned_vars
		|| tc.ownership_owned_descendant_names(tmp_name).len > 0 {
		consumed = true
	}
	tc.ownership_clear_temp_storage(tmp_name)
	tc.ownership_flush_value_branch_moves()
	return consumed
}

fn (mut tc TypeChecker) ownership_consume_array_element_method_result(expr_id flat.NodeId, target string, pos flat.NodeId) bool {
	call_id := tc.ownership_unwrap_expr(expr_id)
	if !tc.valid_node_id(call_id) {
		return false
	}
	node := tc.a.nodes[int(call_id)]
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return false
	}
	method := fn_node.value
	if method !in ['first', 'last', 'pop', 'pop_left'] {
		return false
	}
	recv_id := tc.a.child(fn_node, 0)
	if unwrap_pointer(tc.resolve_type(recv_id)) !is Array {
		return false
	}
	array_name := tc.ownership_expr_ident_name(recv_id)
	if array_name.len == 0 {
		return false
	}
	source_key := tc.ownership_array_element_method_source_key(array_name, method)
	if source_key.len == 0 {
		return false
	}
	tc.ownership_move_owned_descendants(source_key, target, pos, false, '', false)
	mut st := tc.ownership_state()
	if source_key in st.owned_vars {
		tc.ownership_reject_global_move(source_key, pos, target, false)
		tc.ownership_move_var(source_key, target, pos, false, '', false)
	} else {
		_ := tc.ownership_move_overlapping_dynamic_storage(source_key, target, pos, false, '',
			false)
	}
	tc.ownership_update_array_length_after_element_method(array_name, method)
	tc.ownership_clear_removed_array_element_after_method(array_name, source_key, method)
	return true
}

fn (mut tc TypeChecker) ownership_mark_return_descendants_from_call(lhs_name string, fn_name string, slot_idx int, pos flat.NodeId) bool {
	if lhs_name.len == 0 || fn_name.len == 0 || slot_idx < 0 {
		return false
	}
	descs := tc.ownership_state().ownership_fn_return_descs[fn_name] or {
		[]OwnershipReturnDescendant{}
	}
	mut marked := false
	for desc in descs {
		if desc.slot_idx != slot_idx {
			continue
		}
		tc.ownership_mark_owned_name(lhs_name + desc.suffix, desc.type_name, pos)
		marked = true
	}
	return marked
}

fn (mut tc TypeChecker) ownership_mark_return_param_descendants_from_call(lhs_name string, call_id flat.NodeId, node flat.Node, fn_name string, slot_idx int, pos flat.NodeId) bool {
	if lhs_name.len == 0 || fn_name.len == 0 || slot_idx < 0 {
		return false
	}
	descs := tc.ownership_state().ownership_fn_return_param_descs[fn_name] or {
		[]OwnershipReturnParamDescendant{}
	}
	mut marked := false
	for desc in descs {
		if desc.slot_idx != slot_idx {
			continue
		}
		if tc.ownership_mark_from_return_param_descendant(lhs_name + desc.target_suffix, call_id,
			node, fn_name, desc, pos)
		{
			marked = true
		}
	}
	return marked
}

fn (mut tc TypeChecker) ownership_mark_from_return_param_descendant(target_name string, call_id flat.NodeId, node flat.Node, fn_name string, desc OwnershipReturnParamDescendant, pos flat.NodeId) bool {
	if target_name.len == 0 || desc.param_idx < 0 {
		return false
	}
	source := tc.ownership_call_arg_for_return_param_source(call_id, node, desc.param_idx,
		desc.source_suffix) or { return false }
	arg_id := source.arg_id
	source_suffix := source.source_suffix
	arg_name := tc.ownership_expr_ident_name(arg_id)
	if source_suffix.len == 0 {
		if arg_name.len == 0 {
			return tc.ownership_mark_storage_from_expr(target_name, arg_id,
				tc.resolve_type(arg_id), pos)
		}
		if arg_name in tc.ownership_state().owned_vars {
			tc.ownership_mark_owned(target_name, tc.ownership_type_for_name(arg_name,
				tc.resolve_type(arg_id)), pos)
			return true
		}
		if info := tc.ownership_state().moved_vars[arg_name] {
			if info.is_fn_call && info.fn_name == fn_name {
				tc.ownership_mark_owned_name(target_name, info.type_name, pos)
				return true
			}
		}
		return false
	}
	if arg_name.len == 0 {
		return tc.ownership_mark_from_return_param_arg_projection(target_name, arg_id,
			source_suffix, fn_name, pos)
	}
	source_name := arg_name + source_suffix
	if source_name in tc.ownership_state().owned_vars {
		tc.ownership_mark_owned_name(target_name, tc.ownership_type_name_for_var(source_name), pos)
		return true
	}
	if info := tc.ownership_state().moved_vars[source_name] {
		if info.is_fn_call && info.fn_name == fn_name {
			tc.ownership_mark_owned_name(target_name, info.type_name, pos)
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) ownership_mark_from_return_param_arg_projection(target_name string, arg_id flat.NodeId, source_suffix string, fn_name string, pos flat.NodeId) bool {
	if target_name.len == 0 || source_suffix.len == 0 || !tc.valid_node_id(arg_id) {
		return false
	}
	id := tc.ownership_unwrap_expr(arg_id)
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .call
		&& tc.ownership_mark_return_projection_from_call_suffix(target_name, id, source_suffix, pos) {
		return true
	}
	if source_suffix.starts_with('.') {
		field_name, rest := ownership_split_first_field_suffix(source_suffix)
		if field_name.len == 0 {
			return false
		}
		if field_id := tc.ownership_aggregate_field_expr(id, field_name) {
			if rest.len == 0 {
				return tc.ownership_mark_from_return_param_source_expr(target_name, field_id,
					fn_name, pos)
			}
			return tc.ownership_mark_from_return_param_arg_projection(target_name, field_id, rest,
				fn_name, pos)
		}
	}
	index_part, rest := ownership_split_first_index_suffix(source_suffix)
	if index_part.len > 0 {
		if elem_id := tc.ownership_aggregate_index_expr(id, index_part) {
			if rest.len == 0 {
				return tc.ownership_mark_from_return_param_source_expr(target_name, elem_id,
					fn_name, pos)
			}
			return tc.ownership_mark_from_return_param_arg_projection(target_name, elem_id, rest,
				fn_name, pos)
		}
	}
	return false
}

fn (mut tc TypeChecker) ownership_mark_from_return_param_source_expr(target_name string, expr_id flat.NodeId, fn_name string, pos flat.NodeId) bool {
	if target_name.len == 0 || !tc.valid_node_id(expr_id) {
		return false
	}
	id := tc.ownership_unwrap_expr(expr_id)
	if !tc.valid_node_id(id) {
		return false
	}
	source_name := tc.ownership_expr_ident_name(id)
	if source_name.len > 0 {
		if info := tc.ownership_state().moved_vars[source_name] {
			if (info.is_fn_call && info.fn_name == fn_name) || info.move_pos == id {
				tc.ownership_mark_owned_name(target_name, info.type_name, pos)
				return true
			}
			tc.ownership_report_moved(source_name, info, id)
			return false
		}
		if source_name in tc.ownership_state().owned_vars {
			tc.ownership_reject_global_move(source_name, pos, target_name, false)
			type_name := tc.ownership_type_name_for_var(source_name)
			if tc.ownership_move_var_result(source_name, target_name, pos, false, '', true) {
				tc.ownership_mark_owned_name(target_name, type_name, pos)
				return true
			}
		}
		moved_types := tc.ownership_move_overlapping_dynamic_storage(source_name, target_name, pos,
			false, '', true)
		if moved_types.len > 0 {
			tc.ownership_mark_owned_name(target_name, moved_types[0], pos)
			return true
		}
	}
	if tc.ownership_mark_from_conditional_expr(target_name, id, tc.resolve_type(id), pos) {
		return true
	}
	if tc.ownership_mark_from_call(target_name, id, pos) {
		return true
	}
	if tc.ownership_expr_is_to_owned_call(id) || tc.ownership_expr_is_owned_clone_call(id)
		|| tc.ownership_expr_is_ownership_call(id)
		|| tc.ownership_type_is_owned(tc.resolve_type(id)) {
		tc.ownership_mark_owned(target_name, tc.resolve_type(id), pos)
		return true
	}
	return false
}

fn (mut tc TypeChecker) ownership_aggregate_field_expr(id flat.NodeId, field_name string) ?flat.NodeId {
	if field_name.len == 0 || !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.struct_init {
			init_type := tc.parse_type(node.value)
			if init_type !is Struct {
				return none
			}
			fields := tc.structs[(init_type as Struct).name] or { []StructField{} }
			for i in 0 .. node.children_count {
				field_id := tc.a.child(&node, i)
				field := tc.a.nodes[int(field_id)]
				if field.kind != .field_init || field.children_count == 0 {
					continue
				}
				mut current_name := field.value
				if current_name.len == 0 && i < fields.len {
					current_name = fields[i].name
				}
				if current_name == field_name {
					return tc.a.child(&field, 0)
				}
			}
			if decl := tc.ownership_struct_decl_node((init_type as Struct).name) {
				for i in 0 .. decl.children_count {
					field := tc.a.child_node(decl, i)
					if field.kind == .field_decl && field.value == field_name
						&& field.children_count > 0 {
						return tc.a.child(field, 0)
					}
				}
			}
		}
		.assoc {
			init_type := tc.resolve_type(id)
			if init_type !is Struct || node.children_count == 0 {
				return none
			}
			fields := tc.structs[(init_type as Struct).name] or { []StructField{} }
			for i in 1 .. node.children_count {
				field_id := tc.a.child(&node, i)
				field := tc.a.nodes[int(field_id)]
				if field.kind != .field_init || field.children_count == 0 {
					continue
				}
				mut current_name := field.value
				field_idx := i - 1
				if current_name.len == 0 && field_idx < fields.len {
					current_name = fields[field_idx].name
				}
				if current_name == field_name {
					return tc.a.child(&field, 0)
				}
			}
			return tc.ownership_aggregate_field_expr(tc.a.child(&node, 0), field_name)
		}
		else {}
	}

	return none
}

fn (mut tc TypeChecker) ownership_aggregate_index_expr(id flat.NodeId, index_part string) ?flat.NodeId {
	if index_part.len == 0 || !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.array_literal {
			idx := ownership_index_segment_int(index_part) or { return none }
			if idx >= 0 && idx < node.children_count {
				return tc.a.child(&node, idx)
			}
		}
		.array_init {
			if index_part == '[*]' {
				for i in 0 .. node.children_count {
					child_id := tc.a.child(&node, i)
					child := tc.a.nodes[int(child_id)]
					if child.kind == .field_init && child.value == 'init'
						&& child.children_count > 0 {
						return tc.a.child(&child, 0)
					}
				}
			}
			idx := ownership_index_segment_int(index_part) or { return none }
			mut elem_idx := 0
			for i in 0 .. node.children_count {
				child_id := tc.a.child(&node, i)
				child := tc.a.nodes[int(child_id)]
				if child.kind == .field_init {
					continue
				}
				if elem_idx == idx {
					return child_id
				}
				elem_idx++
			}
		}
		.map_init {
			for i := 0; i + 1 < node.children_count; i += 2 {
				key_id := tc.a.child(&node, i)
				if '[${tc.ownership_index_key_part(key_id)}]' == index_part {
					return tc.a.child(&node, i + 1)
				}
			}
		}
		else {}
	}

	return none
}

fn ownership_split_first_field_suffix(suffix string) (string, string) {
	if suffix.len < 2 || suffix[0] != `.` {
		return '', suffix
	}
	for i in 1 .. suffix.len {
		if suffix[i] == `.` || suffix[i] == `[` {
			return suffix[1..i], suffix[i..]
		}
	}
	return suffix[1..], ''
}

fn (mut tc TypeChecker) ownership_mark_return_projection_from_call(lhs_name string, rhs_id flat.NodeId, pos flat.NodeId) bool {
	projection := tc.ownership_call_projection(rhs_id) or { return false }
	if projection.suffix.len == 0 {
		return false
	}
	return tc.ownership_mark_return_projection_from_call_suffix(lhs_name, projection.call_id,
		projection.suffix, pos)
}

fn (mut tc TypeChecker) ownership_mark_return_projection_from_call_suffix(lhs_name string, call_id flat.NodeId, projection_suffix string, pos flat.NodeId) bool {
	call_node := tc.a.nodes[int(call_id)]
	mut fn_name := tc.ownership_call_name(call_id)
	if info := tc.resolve_call_info(call_id, call_node) {
		if info.name.len > 0 {
			fn_name = info.name
		}
	}
	if fn_name.len == 0 {
		return false
	}
	descs := tc.ownership_state().ownership_fn_return_descs[fn_name] or {
		[]OwnershipReturnDescendant{}
	}
	mut marked := false
	for desc in descs {
		if desc.slot_idx != 0 {
			continue
		}
		if desc.suffix == projection_suffix
			|| ownership_storage_keys_dynamic_overlap(desc.suffix, projection_suffix)
			|| ownership_storage_keys_dynamic_overlap(projection_suffix, desc.suffix) {
			tc.ownership_mark_owned_name(lhs_name, desc.type_name, pos)
			marked = true
			continue
		}
		if ownership_storage_key_is_descendant(desc.suffix, projection_suffix) {
			tc.ownership_mark_owned_name(lhs_name + desc.suffix[projection_suffix.len..],
				desc.type_name, pos)
			marked = true
		}
	}
	param_descs := tc.ownership_state().ownership_fn_return_param_descs[fn_name] or {
		[]OwnershipReturnParamDescendant{}
	}
	for desc in param_descs {
		if desc.slot_idx != 0 {
			continue
		}
		if desc.target_suffix == projection_suffix
			|| ownership_storage_keys_dynamic_overlap(desc.target_suffix, projection_suffix)
			|| ownership_storage_keys_dynamic_overlap(projection_suffix, desc.target_suffix) {
			if tc.ownership_mark_from_return_param_descendant(lhs_name, call_id, call_node,
				fn_name, desc, pos)
			{
				marked = true
			}
			continue
		}
		if ownership_storage_key_is_descendant(desc.target_suffix, projection_suffix) {
			target_name := lhs_name + desc.target_suffix[projection_suffix.len..]
			if tc.ownership_mark_from_return_param_descendant(target_name, call_id, call_node,
				fn_name, desc, pos)
			{
				marked = true
			}
		}
	}
	return marked
}

fn (tc &TypeChecker) ownership_call_projection(id flat.NodeId) ?OwnershipCallProjection {
	clean_id := tc.ownership_unwrap_expr(id)
	if !tc.valid_node_id(clean_id) {
		return none
	}
	node := tc.a.nodes[int(clean_id)]
	match node.kind {
		.call {
			return OwnershipCallProjection{
				call_id: clean_id
				suffix:  ''
			}
		}
		.selector {
			if node.children_count == 0 || node.value.len == 0 || !valid_string_data(node.value) {
				return none
			}
			base := tc.ownership_call_projection(tc.a.child(&node, 0)) or { return none }
			return OwnershipCallProjection{
				call_id: base.call_id
				suffix:  base.suffix + '.${node.value}'
			}
		}
		.index {
			if node.children_count < 2 {
				return none
			}
			key_part := tc.ownership_index_key_part(tc.a.child(&node, 1))
			if key_part.len == 0 {
				return none
			}
			base := tc.ownership_call_projection(tc.a.child(&node, 0)) or { return none }
			return OwnershipCallProjection{
				call_id: base.call_id
				suffix:  base.suffix + '[${key_part}]'
			}
		}
		else {}
	}

	return none
}

fn (mut tc TypeChecker) ownership_mark_from_call(lhs_name string, rhs_id flat.NodeId, pos flat.NodeId) bool {
	if !tc.valid_node_id(rhs_id) {
		return false
	}
	if tc.ownership_mark_return_projection_from_call(lhs_name, rhs_id, pos) {
		return true
	}
	call_id := tc.ownership_unwrap_expr(rhs_id)
	if !tc.valid_node_id(call_id) {
		return false
	}
	node := tc.a.nodes[int(call_id)]
	if node.kind != .call {
		return false
	}
	if tc.ownership_expr_is_to_owned_call(call_id) {
		tc.ownership_mark_owned(lhs_name, Type(String{}), pos)
		return true
	}
	if tc.ownership_expr_is_owned_clone_call(call_id) {
		tc.ownership_mark_owned(lhs_name, tc.resolve_type(call_id), pos)
		return true
	}
	if tc.ownership_mark_from_array_element_method(lhs_name, node, pos) {
		return true
	}
	fn_name := tc.ownership_call_name(call_id)
	if tc.ownership_mark_return_param_descendants_from_call(lhs_name, call_id, node, fn_name, 0,
		pos)
	{
		return true
	}
	if tc.ownership_mark_return_descendants_from_call(lhs_name, fn_name, 0, pos) {
		return true
	}
	if fn_name in tc.ownership_state().ownership_fns {
		tc.ownership_mark_owned(lhs_name, tc.resolve_type(call_id), pos)
		return true
	}
	return_param_idxs := tc.ownership_state().ownership_fn_returns_param[fn_name] or { []int{} }
	for param_idx in return_param_idxs {
		if tc.ownership_mark_from_return_param(lhs_name, call_id, node, fn_name, param_idx, pos) {
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) ownership_mark_from_array_element_method(lhs_name string, node flat.Node, pos flat.NodeId) bool {
	if lhs_name.len == 0 || node.children_count == 0 {
		return false
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return false
	}
	method := fn_node.value
	if method !in ['first', 'last', 'pop', 'pop_left'] {
		return false
	}
	recv_id := tc.a.child(fn_node, 0)
	if unwrap_pointer(tc.resolve_type(recv_id)) !is Array {
		return false
	}
	array_name := tc.ownership_expr_ident_name(recv_id)
	if array_name.len == 0 {
		return false
	}
	source_key := tc.ownership_array_element_method_source_key(array_name, method)
	if source_key.len == 0 {
		return false
	}
	mut marked := tc.ownership_transfer_owned_descendants(source_key, lhs_name, pos)
	mut st := tc.ownership_state()
	if source_key in st.owned_vars {
		type_name := tc.ownership_type_name_for_var(source_key)
		if tc.ownership_move_var_result(source_key, lhs_name, pos, false, '', true) {
			tc.ownership_mark_owned_name(lhs_name, type_name, pos)
			marked = true
		}
	} else {
		moved_types := tc.ownership_move_overlapping_dynamic_storage(source_key, lhs_name, pos,
			false, '', true)
		if moved_types.len > 0 {
			tc.ownership_mark_owned_name(lhs_name, moved_types[0], pos)
			marked = true
		}
	}
	tc.ownership_update_array_length_after_element_method(array_name, method)
	tc.ownership_clear_removed_array_element_after_method(array_name, source_key, method)
	return marked
}

fn (mut tc TypeChecker) ownership_array_element_method_source_key(array_name string, method string) string {
	match method {
		'first', 'pop_left' {
			return '${array_name}[0]'
		}
		'last', 'pop' {
			if length := tc.ownership_state().array_lengths[array_name] {
				if length > 0 {
					return '${array_name}[${length - 1}]'
				}
			}
			return '${array_name}[*]'
		}
		else {}
	}

	return ''
}

fn (mut tc TypeChecker) ownership_update_array_length_after_element_method(array_name string, method string) {
	if method !in ['pop', 'pop_left'] {
		return
	}
	mut st := tc.ownership_state()
	if length := st.array_lengths[array_name] {
		if length > 0 {
			st.array_lengths[array_name] = length - 1
		}
	}
}

fn (mut tc TypeChecker) ownership_clear_removed_array_element_after_method(array_name string, source_key string, method string) {
	if source_key.len == 0 || method !in ['pop', 'pop_left'] {
		return
	}
	tc.ownership_clear_descendant_state(source_key)
	mut st := tc.ownership_state()
	st.owned_vars.delete(source_key)
	st.owned_var_types.delete(source_key)
	st.moved_vars.delete(source_key)
	if method == 'pop_left' {
		tc.ownership_shift_array_elements_after_pop_left(array_name)
	}
}

fn (mut tc TypeChecker) ownership_mark_borrow_from_call_return(lhs_name string, rhs_id flat.NodeId, pos flat.NodeId) bool {
	call_id := tc.ownership_unwrap_expr(rhs_id)
	if !tc.valid_node_id(call_id) {
		return false
	}
	node := tc.a.nodes[int(call_id)]
	if node.kind != .call {
		return false
	}
	info := tc.resolve_call_info(call_id, node) or { return false }
	return_type := tc.resolve_type(call_id)
	if info.return_type !is Pointer && return_type !is Pointer {
		return false
	}
	call_name := if info.name.len > 0 { info.name } else { tc.ownership_call_name(call_id) }
	return_param_idxs := tc.ownership_state().ownership_fn_returns_param[call_name] or { []int{} }
	for param_idx in return_param_idxs {
		arg_id := tc.ownership_call_arg_for_return_param_info(node, info, param_idx) or { continue }
		mut borrow_name := tc.ownership_borrowed_name(arg_id)
		if borrow_name.len == 0 {
			borrow_name = tc.ownership_expr_ident_name(arg_id)
		}
		if borrow_name.len == 0 || !tc.ownership_storage_participates(borrow_name) {
			continue
		}
		mut st := tc.ownership_state()
		st.owned_vars.delete(lhs_name)
		st.owned_var_types.delete(lhs_name)
		tc.ownership_add_borrow(borrow_name, lhs_name, pos, tc.ownership_call_param_is_mut(call_name,
			param_idx))
		return true
	}
	return false
}

fn (mut tc TypeChecker) ownership_mark_from_return_param(lhs_name string, call_id flat.NodeId, node flat.Node, fn_name string, param_idx int, pos flat.NodeId) bool {
	if param_idx < 0 {
		return false
	}
	arg_id := tc.ownership_call_arg_for_return_param(call_id, node, param_idx) or { return false }
	arg_name := tc.ownership_expr_ident_name(arg_id)
	if arg_name.len == 0 {
		return tc.ownership_mark_storage_from_expr(lhs_name, arg_id, tc.resolve_type(arg_id), pos)
	}
	if arg_name in tc.ownership_state().owned_vars {
		tc.ownership_mark_owned(lhs_name, tc.ownership_type_for_name(arg_name,
			tc.resolve_type(arg_id)), pos)
		return true
	}
	if info := tc.ownership_state().moved_vars[arg_name] {
		if info.is_fn_call && info.fn_name == fn_name {
			tc.ownership_mark_owned_name(lhs_name, info.type_name, pos)
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) ownership_call_arg_for_return_param(call_id flat.NodeId, node flat.Node, param_idx int) ?flat.NodeId {
	info := tc.resolve_call_info(call_id, node) or {
		arg_child_idx := param_idx + 1
		if arg_child_idx < node.children_count {
			return tc.call_arg_value(tc.a.child(&node, arg_child_idx))
		}
		return none
	}
	return tc.ownership_call_arg_for_return_param_info(node, info, param_idx)
}

fn (mut tc TypeChecker) ownership_call_arg_for_return_param_source(call_id flat.NodeId, node flat.Node, param_idx int, source_suffix string) ?OwnershipReturnParamArg {
	info := tc.resolve_call_info(call_id, node) or {
		arg_child_idx := param_idx + 1
		if arg_child_idx < node.children_count {
			return OwnershipReturnParamArg{
				arg_id:        tc.call_arg_value(tc.a.child(&node, arg_child_idx))
				source_suffix: source_suffix
			}
		}
		return none
	}
	return tc.ownership_call_arg_for_return_param_source_info(node, info, param_idx, source_suffix)
}

fn (tc &TypeChecker) ownership_call_arg_for_return_param_info(node flat.Node, info CallInfo, param_idx int) ?flat.NodeId {
	source := tc.ownership_call_arg_for_return_param_source_info(node, info, param_idx, '') or {
		return none
	}
	return source.arg_id
}

fn (tc &TypeChecker) ownership_call_arg_for_return_param_source_info(node flat.Node, info CallInfo, param_idx int, source_suffix string) ?OwnershipReturnParamArg {
	if info.has_receiver {
		if param_idx == 0 {
			fn_node := tc.a.child_node(&node, 0)
			if fn_node.kind == .selector && fn_node.children_count > 0 {
				return OwnershipReturnParamArg{
					arg_id:        tc.a.child(fn_node, 0)
					source_suffix: source_suffix
				}
			}
			return none
		}
	}
	if source_suffix.starts_with('.') {
		params_idx := tc.ownership_call_params_struct_decl_param_idx(node, info)
		if params_idx == param_idx {
			field_name, rest := ownership_split_first_field_suffix(source_suffix)
			if field_name.len > 0 {
				for i in 1 .. node.children_count {
					arg_node := tc.a.child_node(&node, i)
					if arg_node.kind == .field_init && arg_node.value == field_name
						&& arg_node.children_count > 0 {
						return OwnershipReturnParamArg{
							arg_id:        tc.call_arg_value(tc.a.child(&node, i))
							source_suffix: rest
						}
					}
				}
			}
		}
	}
	arg_shift := tc.ownership_call_arg_shift(node, info)
	for i in 1 .. node.children_count {
		arg_node := tc.a.child_node(&node, i)
		if arg_node.kind == .field_init {
			continue
		}
		raw_param_idx := tc.ownership_call_arg_decl_param_idx(info, i)
		type_param_idx := raw_param_idx + arg_shift
		variadic_elem_idx := tc.ownership_call_arg_variadic_elem_idx(info, type_param_idx)
		target_param_idx := tc.ownership_call_arg_variadic_decl_param_idx(raw_param_idx,
			variadic_elem_idx)
		if target_param_idx != param_idx {
			continue
		}
		target_suffix := ownership_call_arg_variadic_suffix(variadic_elem_idx)
		if target_suffix.len > 0 {
			if source_suffix == target_suffix {
				return OwnershipReturnParamArg{
					arg_id:        tc.call_arg_value(tc.a.child(&node, i))
					source_suffix: ''
				}
			}
			if ownership_storage_key_is_descendant(source_suffix, target_suffix) {
				return OwnershipReturnParamArg{
					arg_id:        tc.call_arg_value(tc.a.child(&node, i))
					source_suffix: source_suffix[target_suffix.len..]
				}
			}
			continue
		}
		return OwnershipReturnParamArg{
			arg_id:        tc.call_arg_value(tc.a.child(&node, i))
			source_suffix: source_suffix
		}
	}
	return none
}

fn (tc &TypeChecker) ownership_expr_is_ownership_call(id flat.NodeId) bool {
	name := tc.ownership_call_name(id)
	return name.len > 0 && name in tc.ownership.ownership_fns
}

fn (tc &TypeChecker) ownership_expr_is_ownership_call_in_module(id flat.NodeId, module_name string) bool {
	name := tc.ownership_call_name_in_module(id, module_name)
	return name.len > 0 && name in tc.ownership.ownership_fns
}

fn (tc &TypeChecker) ownership_call_name_in_module(id flat.NodeId, module_name string) string {
	call_id := tc.ownership_unwrap_expr(id)
	idx := int(call_id)
	if idx >= 0 && idx < tc.resolved_call_set.len && tc.resolved_call_set[idx] {
		return tc.resolved_call_names[idx]
	}
	if !tc.valid_node_id(call_id) {
		return ''
	}
	node := tc.a.nodes[idx]
	if node.kind != .call || node.children_count == 0 {
		return ''
	}
	fn_node := tc.a.child_node(&node, 0)
	match fn_node.kind {
		.ident {
			if tc.ownership != unsafe { nil } {
				if mapped := tc.ownership.ownership_fn_value_vars[fn_node.value] {
					return mapped
				}
			}
			qname := ownership_qualify_name(module_name, fn_node.value)
			if qname in tc.fn_ret_types || qname in tc.ownership.ownership_fns {
				return qname
			}
			return fn_node.value
		}
		.selector {
			if fn_node.children_count > 0 {
				base_node := tc.a.child_node(fn_node, 0)
				if base_node.kind == .ident {
					if resolved_mod := tc.resolve_import_alias(base_node.value) {
						mod_name := '${resolved_mod}.${fn_node.value}'
						if mod_name in tc.fn_ret_types || mod_name in tc.ownership.ownership_fns {
							return mod_name
						}
					}
					if base_node.value == module_name {
						mod_name := '${module_name}.${fn_node.value}'
						if mod_name in tc.fn_ret_types || mod_name in tc.ownership.ownership_fns {
							return mod_name
						}
					}
				}
			}
			return fn_node.value
		}
		else {}
	}

	return ''
}

fn (tc &TypeChecker) ownership_unwrap_expr(id flat.NodeId) flat.NodeId {
	mut cur := id
	for tc.valid_node_id(cur) {
		node := tc.a.nodes[int(cur)]
		if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
			next := tc.a.child(&node, 0)
			if next == cur {
				break
			}
			cur = next
			continue
		}
		break
	}
	return cur
}

fn (tc &TypeChecker) ownership_expr_is_to_owned_call(id flat.NodeId) bool {
	call_id := tc.ownership_unwrap_expr(id)
	if !tc.valid_node_id(call_id) {
		return false
	}
	node := tc.a.nodes[int(call_id)]
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.value != 'to_owned' || fn_node.children_count == 0 {
		return false
	}
	recv_id := tc.a.child(fn_node, 0)
	return tc.ownership_type_is_string(tc.resolve_type(recv_id))
		|| tc.ownership_type_is_owned(tc.resolve_type(call_id))
}

fn (mut tc TypeChecker) ownership_expr_is_owned_clone_call(id flat.NodeId) bool {
	call_id := tc.ownership_unwrap_expr(id)
	recv_id := tc.ownership_clone_receiver_id(call_id)
	if !tc.valid_node_id(recv_id) {
		return false
	}
	name := tc.ownership_expr_ident_name(recv_id)
	if name.len > 0 && name in tc.ownership.owned_vars {
		return tc.ownership_clone_result_is_owned(call_id, tc.ownership_type_for_name(name,
			Type(String{})))
	}
	if !tc.ownership_expr_produces_owned_value(recv_id) {
		return false
	}
	return tc.ownership_clone_result_is_owned(call_id, tc.resolve_type(recv_id))
}

fn (mut tc TypeChecker) ownership_expr_produces_owned_value(id flat.NodeId) bool {
	return tc.ownership_expr_is_to_owned_call(id) || tc.ownership_expr_is_owned_clone_call(id)
		|| tc.ownership_expr_is_ownership_call(id)
		|| tc.ownership_type_is_owned(tc.resolve_type(id))
}

fn (mut tc TypeChecker) ownership_clone_result_is_owned(call_id flat.NodeId, recv_type Type) bool {
	if tc.ownership_type_is_string(recv_type) {
		return true
	}
	ret_type := tc.resolve_type(call_id)
	return tc.ownership_type_is_owned(ret_type)
}

fn (tc &TypeChecker) ownership_clone_receiver_name(id flat.NodeId) string {
	recv_id := tc.ownership_clone_receiver_id(id)
	if !tc.valid_node_id(recv_id) {
		return ''
	}
	return tc.ownership_expr_ident_name(recv_id)
}

fn (tc &TypeChecker) ownership_clone_receiver_id(id flat.NodeId) flat.NodeId {
	call_id := tc.ownership_unwrap_expr(id)
	if !tc.valid_node_id(call_id) {
		return flat.empty_node
	}
	node := tc.a.nodes[int(call_id)]
	if node.kind != .call || node.children_count == 0 {
		return flat.empty_node
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.value != 'clone' || fn_node.children_count == 0 {
		return flat.empty_node
	}
	return tc.a.child(fn_node, 0)
}

fn (tc &TypeChecker) ownership_expr_is_borrow(id flat.NodeId) bool {
	clean_id := tc.ownership_unwrap_expr(id)
	if !tc.valid_node_id(clean_id) {
		return false
	}
	node := tc.a.nodes[int(clean_id)]
	return node.kind == .prefix && node.op == .amp
}

fn (tc &TypeChecker) ownership_borrowed_name(id flat.NodeId) string {
	clean_id := tc.ownership_unwrap_expr(id)
	if !tc.valid_node_id(clean_id) {
		return ''
	}
	node := tc.a.nodes[int(clean_id)]
	if node.kind == .prefix && node.op == .amp && node.children_count > 0 {
		return tc.ownership_expr_ident_name(tc.a.child(&node, 0))
	}
	return ''
}

fn (tc &TypeChecker) ownership_expr_ident_name(id flat.NodeId) string {
	if !tc.valid_node_id(id) {
		return ''
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.ident {
			return node.value
		}
		.paren, .expr_stmt, .cast_expr {
			if node.children_count > 0 {
				return tc.ownership_expr_ident_name(tc.a.child(&node, 0))
			}
		}
		.selector {
			if node.children_count > 0 && node.value.len > 0 && valid_string_data(node.value) {
				base_id := tc.a.child(&node, 0)
				base_node := tc.a.nodes[int(base_id)]
				base := if base_node.kind == .ident {
					if _ := tc.cur_scope.lookup(base_node.value) {
						tc.ownership_expr_ident_name(base_id)
					} else {
						tc.resolve_import_alias(base_node.value) or {
							tc.ownership_expr_ident_name(base_id)
						}
					}
				} else {
					tc.ownership_expr_ident_name(base_id)
				}
				if base.len > 0 {
					return '${base}.${node.value}'
				}
			}
		}
		.index {
			if node.children_count >= 2 {
				base := tc.ownership_expr_ident_name(tc.a.child(&node, 0))
				index := tc.ownership_index_key_part(tc.a.child(&node, 1))
				if base.len > 0 && index.len > 0 {
					return '${base}[${index}]'
				}
			}
		}
		else {}
	}

	return ''
}

fn (tc &TypeChecker) ownership_index_key_part(id flat.NodeId) string {
	if !tc.valid_node_id(id) {
		return ''
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.int_literal, .bool_literal, .char_literal, .string_literal, .enum_val {
			if node.value.len > 0 && valid_string_data(node.value) {
				return node.value
			}
		}
		.ident {
			return '*'
		}
		.paren, .expr_stmt, .cast_expr {
			if node.children_count > 0 {
				return tc.ownership_index_key_part(tc.a.child(&node, 0))
			}
		}
		else {}
	}

	return '*'
}

fn (tc &TypeChecker) ownership_lhs_name(id flat.NodeId) string {
	if !tc.valid_node_id(id) {
		return ''
	}
	return tc.ownership_expr_ident_name(id)
}

fn (tc &TypeChecker) ownership_call_receiver_id(node flat.Node) ?flat.NodeId {
	if node.kind != .call || node.children_count == 0 {
		return none
	}
	fn_node := tc.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return none
	}
	return tc.a.child(fn_node, 0)
}

fn (tc &TypeChecker) ownership_call_name(id flat.NodeId) string {
	call_id := tc.ownership_unwrap_expr(id)
	idx := int(call_id)
	if idx >= 0 && idx < tc.resolved_call_set.len && tc.resolved_call_set[idx] {
		return tc.resolved_call_names[idx]
	}
	if !tc.valid_node_id(call_id) {
		return ''
	}
	node := tc.a.nodes[idx]
	if node.kind != .call || node.children_count == 0 {
		return ''
	}
	fn_node := tc.a.child_node(&node, 0)
	match fn_node.kind {
		.ident {
			if tc.ownership != unsafe { nil } {
				if mapped := tc.ownership.ownership_fn_value_vars[fn_node.value] {
					return mapped
				}
			}
			return fn_node.value
		}
		.selector {
			return fn_node.value
		}
		else {}
	}

	return ''
}

fn (mut tc TypeChecker) ownership_mark_owned(name string, typ Type, pos flat.NodeId) {
	tc.ownership_mark_owned_name(name, typ.name(), pos)
}

fn (mut tc TypeChecker) ownership_mark_owned_name(name string, type_name string, pos flat.NodeId) {
	if name.len == 0 || name == '_' {
		return
	}
	mut st := tc.ownership_state()
	st.owned_vars[name] = pos
	st.owned_var_types[name] = if type_name.len > 0 { type_name } else { 'string' }
	st.moved_vars.delete(name)
}

fn (mut tc TypeChecker) ownership_type_name_for_var(name string) string {
	return tc.ownership_state().owned_var_types[name] or { 'string' }
}

fn (mut tc TypeChecker) ownership_move_var(name string, target string, pos flat.NodeId, is_fn_call bool, fn_name string, suggest_clone bool) {
	_ := tc.ownership_move_var_result(name, target, pos, is_fn_call, fn_name, suggest_clone)
}

fn (mut tc TypeChecker) ownership_move_var_result(name string, target string, pos flat.NodeId, is_fn_call bool, fn_name string, suggest_clone bool) bool {
	mut st := tc.ownership_state()
	if moved := tc.ownership_moved_conflict(name) {
		// Re-moving the exact same value to the same target at the same node is one logical
		// move recorded twice, not a use-after-move. This happens when a returned struct's
		// owned descendant (e.g. `args.positional[*]`) is both marked as a return descendant
		// and swept again as overlapping dynamic storage in the same `return`. Treat it as a
		// no-op instead of reporting a spurious `use of moved value`.
		if moved.name == name && moved.info.moved_to == target && moved.info.move_pos == pos {
			return false
		}
		tc.ownership_report_moved(moved.name, moved.info, pos)
		return false
	}
	if conflict := tc.ownership_borrow_conflict(name) {
		msg := if conflict.name == name {
			'cannot move `${name}` because it is borrowed by `${conflict.borrow.borrower}`'
		} else {
			'cannot move `${name}` because `${conflict.name}` is borrowed by `${conflict.borrow.borrower}`'
		}
		tc.record_error(.assignment_mismatch, msg, pos)
		return false
	}
	tname := st.owned_var_types[name] or { 'string' }
	st.owned_vars.delete(name)
	st.owned_var_types.delete(name)
	st.moved_vars[name] = MovedVar{
		moved_to:      target
		move_pos:      pos
		is_fn_call:    is_fn_call
		fn_name:       if fn_name.len > 0 { fn_name } else { target }
		type_name:     tname
		suggest_clone: suggest_clone
	}
	return true
}

fn (mut tc TypeChecker) ownership_moved_conflict(name string) ?OwnershipMovedConflict {
	st := tc.ownership_state()
	if info := st.moved_vars[name] {
		return OwnershipMovedConflict{
			name: name
			info: info
		}
	}
	for moved_name, info in st.moved_vars {
		if moved_name == name {
			continue
		}
		if ownership_storage_keys_overlap(name, moved_name) {
			return OwnershipMovedConflict{
				name: moved_name
				info: info
			}
		}
	}
	return none
}

fn (mut tc TypeChecker) ownership_move_overlapping_dynamic_storage(source_name string, target string, pos flat.NodeId, is_fn_call bool, fn_name string, suggest_clone bool) []string {
	mut moved_types := []string{}
	for owned_name in tc.ownership_owned_dynamic_overlap_names(source_name) {
		type_name := tc.ownership_type_name_for_var(owned_name)
		if tc.ownership_move_var_result(owned_name, target, pos, is_fn_call, fn_name, suggest_clone) {
			moved_types << type_name
		}
	}
	return moved_types
}

fn (mut tc TypeChecker) ownership_owned_dynamic_overlap_names(source_name string) []string {
	if source_name.len == 0 {
		return []string{}
	}
	st := tc.ownership_state()
	source_dynamic := ownership_storage_key_has_dynamic_index(source_name)
	mut names := []string{}
	for owned_name, _ in st.owned_vars {
		owned_dynamic := ownership_storage_key_has_dynamic_index(owned_name)
		if !source_dynamic && !owned_dynamic {
			continue
		}
		if ownership_storage_keys_overlap(source_name, owned_name) {
			names << owned_name
		}
	}
	names.sort()
	return names
}

fn (mut tc TypeChecker) ownership_report_moved(name string, info MovedVar, id flat.NodeId) {
	tname := if info.type_name.len > 0 { info.type_name } else { 'string' }
	mut msg := 'use of moved value: `${name}`'
	msg += '; move occurs because `${name}` has type `${tname}`, which does not implement `Copy`'
	if info.is_fn_call {
		msg += '; value moved into function `${info.fn_name}`'
	} else {
		msg += '; value moved to `${info.moved_to}`'
	}
	if info.suggest_clone {
		msg += '; consider cloning the value'
	}
	tc.record_error(.assignment_mismatch, msg, id)
}

fn (mut tc TypeChecker) ownership_check_reassign(name string, pos flat.NodeId) {
	if conflict := tc.ownership_borrow_conflict(name) {
		msg := if conflict.name == name {
			'cannot assign to `${name}` because it is borrowed by `${conflict.borrow.borrower}`'
		} else {
			'cannot assign to `${name}` because `${conflict.name}` is borrowed by `${conflict.borrow.borrower}`'
		}
		tc.record_error(.assignment_mismatch, msg, pos)
	}
}

fn (mut tc TypeChecker) ownership_borrow_conflict(name string) ?OwnershipBorrowConflict {
	st := tc.ownership_state()
	if borrows := st.borrowed_vars[name] {
		if borrows.len > 0 {
			return OwnershipBorrowConflict{
				name:   name
				borrow: borrows[0]
			}
		}
	}
	for borrowed_name, borrows in st.borrowed_vars {
		if borrowed_name == name || borrows.len == 0 {
			continue
		}
		if ownership_storage_keys_overlap(name, borrowed_name) {
			return OwnershipBorrowConflict{
				name:   borrowed_name
				borrow: borrows[0]
			}
		}
	}
	return none
}

fn (mut tc TypeChecker) ownership_storage_participates(name string) bool {
	if name.len == 0 {
		return false
	}
	st := tc.ownership_state()
	if name in st.owned_vars {
		return true
	}
	for owned_name, _ in st.owned_vars {
		if ownership_storage_keys_overlap(name, owned_name) {
			return true
		}
	}
	return false
}

fn ownership_storage_keys_overlap(a string, b string) bool {
	return a == b || ownership_storage_key_is_descendant(a, b)
		|| ownership_storage_key_is_descendant(b, a) || ownership_storage_keys_dynamic_overlap(a, b)
		|| ownership_storage_keys_dynamic_overlap(b, a)
}

fn ownership_storage_key_is_descendant(child string, parent string) bool {
	if child.len <= parent.len || !child.starts_with(parent) {
		return false
	}
	next := child[parent.len]
	return next == `.` || next == `[`
}

fn ownership_storage_key_has_dynamic_index(name string) bool {
	return name.contains('[*]')
}

fn ownership_storage_keys_dynamic_overlap(pattern string, key string) bool {
	marker_idx := pattern.index('[*]') or { return false }
	prefix := pattern[..marker_idx]
	key_prefix := '${prefix}['
	if !key.starts_with(key_prefix) {
		return false
	}
	key_rest := key[key_prefix.len..]
	key_close_idx := key_rest.index(']') or { return false }
	pattern_after := pattern[marker_idx + 3..]
	key_after := key_rest[key_close_idx + 1..]
	if pattern_after.len == 0 || key_after.len == 0 {
		return true
	}
	return ownership_storage_keys_overlap(pattern_after, key_after)
}

fn (mut tc TypeChecker) ownership_add_borrow(var_name string, borrower string, pos flat.NodeId, is_mut bool) {
	if conflict := tc.ownership_overlapping_borrow_conflict(var_name, is_mut) {
		if is_mut {
			msg := if conflict.borrow.is_mut {
				'cannot borrow `${var_name}` as mutable because `${conflict.name}` is already borrowed as mutable'
			} else {
				'cannot borrow `${var_name}` as mutable because `${conflict.name}` is borrowed as immutable'
			}
			tc.record_error(.assignment_mismatch,
				'${msg}; previous borrow by `${conflict.borrow.borrower}`', pos)
			return
		}
		tc.record_error(.assignment_mismatch,
			'cannot borrow `${var_name}` as immutable because `${conflict.name}` is borrowed as mutable by `${conflict.borrow.borrower}`',
			pos)
		return
	}
	mut st := tc.ownership_state()
	if existing := st.borrowed_vars[var_name] {
		if is_mut && existing.len > 0 {
			borrow := existing[0]
			msg := if borrow.is_mut {
				'cannot borrow `${var_name}` as mutable more than once'
			} else {
				'cannot borrow `${var_name}` as mutable because it is also borrowed as immutable'
			}
			tc.record_error(.assignment_mismatch,
				'${msg}; previous borrow by `${borrow.borrower}`', pos)
			return
		}
		if !is_mut {
			for borrow in existing {
				if borrow.is_mut {
					tc.record_error(.assignment_mismatch,
						'cannot borrow `${var_name}` as immutable because it is borrowed as mutable by `${borrow.borrower}`',
						pos)
					return
				}
			}
		}
		mut updated := existing.clone()
		updated << BorrowInfo{
			borrower: borrower
			pos:      pos
			is_mut:   is_mut
		}
		st.borrowed_vars[var_name] = updated
		return
	}
	st.borrowed_vars[var_name] = [
		BorrowInfo{
			borrower: borrower
			pos:      pos
			is_mut:   is_mut
		},
	]
}

fn (mut tc TypeChecker) ownership_overlapping_borrow_conflict(var_name string, is_mut bool) ?OwnershipBorrowConflict {
	st := tc.ownership_state()
	for borrowed_name, borrows in st.borrowed_vars {
		if borrowed_name == var_name || borrows.len == 0 {
			continue
		}
		if !ownership_storage_keys_overlap(var_name, borrowed_name) {
			continue
		}
		for borrow in borrows {
			if is_mut || borrow.is_mut {
				return OwnershipBorrowConflict{
					name:   borrowed_name
					borrow: borrow
				}
			}
		}
	}
	return none
}

fn (mut tc TypeChecker) ownership_release_borrow(var_name string, borrower string) {
	mut st := tc.ownership_state()
	existing := st.borrowed_vars[var_name] or { return }
	mut remaining := []BorrowInfo{}
	mut removed := false
	for borrow in existing {
		if !removed && borrow.borrower == borrower {
			removed = true
			continue
		}
		remaining << borrow
	}
	if remaining.len > 0 {
		st.borrowed_vars[var_name] = remaining
	} else {
		st.borrowed_vars.delete(var_name)
	}
}

fn (mut tc TypeChecker) ownership_borrower_snapshot(borrower string) []OwnershipBorrowerSnapshot {
	if borrower.len == 0 {
		return []OwnershipBorrowerSnapshot{}
	}
	st := tc.ownership_state()
	mut out := []OwnershipBorrowerSnapshot{}
	for var_name, borrows in st.borrowed_vars {
		for borrow in borrows {
			if borrow.borrower == borrower
				|| ownership_storage_key_is_descendant(borrow.borrower, borrower) {
				out << OwnershipBorrowerSnapshot{
					var_name: var_name
					borrow:   borrow
				}
			}
		}
	}
	return out
}

fn (mut tc TypeChecker) ownership_release_borrower(borrower string) {
	if borrower.len == 0 {
		return
	}
	mut st := tc.ownership_state()
	mut emptied := []string{}
	for var_name, borrows in st.borrowed_vars {
		mut remaining := []BorrowInfo{}
		for borrow in borrows {
			if borrow.borrower != borrower
				&& !ownership_storage_key_is_descendant(borrow.borrower, borrower) {
				remaining << borrow
			}
		}
		if remaining.len > 0 {
			st.borrowed_vars[var_name] = remaining
		} else {
			emptied << var_name
		}
	}
	for var_name in emptied {
		st.borrowed_vars.delete(var_name)
	}
}

fn (mut tc TypeChecker) ownership_reject_global_move(name string, pos flat.NodeId, target string, is_call bool) {
	if tname := tc.ownership_owned_global_type_name(name) {
		action := if is_call { 'move into function' } else { 'move to' }
		tc.record_error(.assignment_mismatch,
			'cannot move owned global `${name}` of type `${tname}`; attempted ${action} `${target}`',
			pos)
	}
}

fn (mut tc TypeChecker) ownership_owned_global_type_name(name string) ?string {
	st := tc.ownership_state()
	if tname := st.owned_globals[name] {
		return tname
	}
	for global_name, tname in st.owned_globals {
		if ownership_storage_key_is_descendant(global_name, name) {
			return tname
		}
	}
	qname := ownership_qualify_storage_key(tc.cur_module, name)
	if qname != name {
		if tname := st.owned_globals[qname] {
			return tname
		}
		for global_name, tname in st.owned_globals {
			if ownership_storage_key_is_descendant(global_name, qname) {
				return tname
			}
		}
	}
	return none
}

fn ownership_marker_name_candidates(name string) []string {
	base := generic_base_name(name)
	mut out := []string{}
	out << name
	if base != name {
		out << base
	}
	return out
}

fn (tc &TypeChecker) ownership_type_is_owned(typ Type) bool {
	if typ is Pointer || typ is Void || typ is Unknown {
		return false
	}
	if typ is Alias {
		return tc.ownership_type_is_owned(typ.base_type)
	}
	if typ is OptionType {
		return tc.ownership_type_is_owned(typ.base_type)
	}
	if typ is ResultType {
		return tc.ownership_type_is_owned(typ.base_type)
	}
	if typ is Struct {
		base := generic_base_name(typ.name)
		if typ.name in tc.ownership.copy_structs
			|| (base != typ.name && base in tc.ownership.copy_structs) {
			return false
		}
		if typ.name in tc.ownership.owned_structs
			|| (base != typ.name && base in tc.ownership.owned_structs) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) ownership_type_is_string(typ Type) bool {
	clean := unwrap_pointer(typ)
	if clean is String {
		return true
	}
	if clean is Alias {
		return tc.ownership_type_is_string(clean.base_type)
	}
	return false
}

// ownership_drop_entries_at_return returns the destructor snapshot recorded
// after the return value has been moved out of the current function.
pub fn (tc &TypeChecker) ownership_drop_entries_at_return(fn_name string, index int) []OwnershipDropEntry {
	if tc.ownership == unsafe { nil } {
		return []OwnershipDropEntry{}
	}
	return (tc.ownership.drop_at_returns['${fn_name}\x01${index}'] or { []OwnershipDropEntry{} }).clone()
}

// ownership_drop_entries_at_return_node returns the destructor snapshot recorded
// for the original return node, used by transformer-expanded return paths.
pub fn (tc &TypeChecker) ownership_drop_entries_at_return_node(fn_name string, id flat.NodeId) []OwnershipDropEntry {
	if tc.ownership == unsafe { nil } {
		return []OwnershipDropEntry{}
	}
	return (tc.ownership.drop_at_return_nodes['${fn_name}\x01${int(id)}'] or {
		[]OwnershipDropEntry{}
	}).clone()
}

pub fn (tc &TypeChecker) ownership_drop_entries_at_propagation(fn_name string, index int) []OwnershipDropEntry {
	if tc.ownership == unsafe { nil } {
		return []OwnershipDropEntry{}
	}
	return (tc.ownership.drop_at_propagations['${fn_name}\x01${index}'] or {
		[]OwnershipDropEntry{}
	}).clone()
}

pub fn (tc &TypeChecker) ownership_drop_entries_at_loop_control(fn_name string, index int) []OwnershipDropEntry {
	if tc.ownership == unsafe { nil } {
		return []OwnershipDropEntry{}
	}
	return (tc.ownership.drop_at_loop_controls['${fn_name}\x01${index}'] or {
		[]OwnershipDropEntry{}
	}).clone()
}

pub fn (tc &TypeChecker) ownership_drop_entries_at_loop_iteration(fn_name string, index int) []OwnershipDropEntry {
	if tc.ownership == unsafe { nil } {
		return []OwnershipDropEntry{}
	}
	return (tc.ownership.drop_at_loop_iterations['${fn_name}\x01${index}'] or {
		[]OwnershipDropEntry{}
	}).clone()
}

// ownership_drop_entries_at_scope_exit returns destructors for locals declared
// in one lexical block and still owned at its normal exit.
pub fn (tc &TypeChecker) ownership_drop_entries_at_scope_exit(fn_name string, index int) []OwnershipDropEntry {
	if tc.ownership == unsafe { nil } {
		return []OwnershipDropEntry{}
	}
	return (tc.ownership.drop_at_scope_exit['${fn_name}\x01${index}'] or { []OwnershipDropEntry{} }).clone()
}

// ownership_drop_entries_at_fn_exit returns destructors for parameters and
// function-scope locals still owned at the implicit function exit.
pub fn (tc &TypeChecker) ownership_drop_entries_at_fn_exit(fn_name string) []OwnershipDropEntry {
	if tc.ownership == unsafe { nil } {
		return []OwnershipDropEntry{}
	}
	return (tc.ownership.drop_at_fn_exit[fn_name] or { []OwnershipDropEntry{} }).clone()
}

// ownership_drop_type_names returns the concrete receiver types whose Drop
// methods are referenced by compiler-generated destructor calls.
pub fn (tc &TypeChecker) ownership_drop_type_names() []string {
	if tc.ownership == unsafe { nil } {
		return []string{}
	}
	mut names := []string{}
	for name, _ in tc.ownership.drop_type_names {
		if !name.contains('\x01') {
			names << name
		}
	}
	names.sort()
	return names
}

// inherit_ownership_codegen_metadata_from shares the immutable ownership
// snapshots with a parallel code-generation checker fork.
pub fn (mut tc TypeChecker) inherit_ownership_codegen_metadata_from(src &TypeChecker) {
	if src.ownership == unsafe { nil } {
		tc.ownership = unsafe { nil }
		return
	}
	mut cloned := *src.ownership
	cloned.scope_frames = []OwnershipScopeFrame{}
	cloned.frames = []OwnershipFrame{}
	cloned.branch_groups = []OwnershipBranchGroup{}
	cloned.pending_value_branch_groups = []OwnershipBranchGroup{}
	cloned.suppressed_checks++
	tc.ownership = &cloned
}

fn (tc &TypeChecker) match_covers_all_variants(node flat.Node) bool {
	return tc.match_covers_all_enum_variants(node)
		|| tc.ownership_match_covers_all_sum_variants(node)
}

fn (tc &TypeChecker) ownership_match_covers_all_sum_variants(node flat.Node) bool {
	if node.children_count < 2 {
		return false
	}
	subject_type := unwrap_pointer(tc.resolve_type(tc.a.child(&node, 0)))
	mut sum_name := ''
	if subject_type is SumType {
		sum_name = subject_type.name
	} else {
		return false
	}
	variants := tc.sum_types[sum_name] or { return false }
	if variants.len == 0 {
		return false
	}
	mut covered := map[string]bool{}
	for i in 1 .. node.children_count {
		branch := tc.a.child_node(&node, i)
		if branch.kind != .match_branch {
			return false
		}
		if branch.value == 'else' {
			return true
		}
		n_conds := branch.value.int()
		for j in 0 .. n_conds {
			cond := tc.a.child_node(branch, j)
			pattern := tc.match_type_pattern(cond) or { continue }
			pattern_short := short_type_name(pattern)
			qpattern := tc.qualify_name(pattern)
			for variant in variants {
				if variant == pattern || variant == qpattern
					|| short_type_name(variant) == pattern_short {
					covered[variant] = true
				}
			}
		}
	}
	for variant in variants {
		if variant !in covered {
			return false
		}
	}
	return true
}

fn (mut tc TypeChecker) ownership_type_for_var(name string, fallback Type) Type {
	tname := tc.ownership_state().owned_var_types[name] or { return fallback }
	if tname == 'string' {
		return Type(String{})
	}
	return tc.parse_type(tname)
}

fn (mut tc TypeChecker) ownership_type_for_name(name string, fallback Type) Type {
	return tc.ownership_type_for_var(name, fallback)
}

fn (tc &TypeChecker) ownership_method_keeps_receiver(method_name string) bool {
	short_method := method_name.all_after_last('.')
	if short_method in ['clone', 'to_owned', 'str'] {
		return true
	}
	return false
}

fn (mut tc TypeChecker) ownership_array_builtin_keeps_receiver(recv_id flat.NodeId, method_name string) bool {
	if method_name !in ['first', 'last', 'pop', 'pop_left', 'insert', 'prepend'] {
		return false
	}
	return unwrap_pointer(tc.resolve_type(recv_id)) is Array
}

// ownership_string_builtin_keeps_receiver reports whether a method call whose receiver is a
// builtin `string` should borrow rather than move the receiver. Builtin string methods
// (`contains`, `starts_with`, `split`, `to_upper`, ...) only read the receiver; none consume
// the string's heap buffer, so a `string` receiver passed by value borrows just like Rust's
// `&self` string methods. User-defined by-value string methods still move the receiver.
fn (mut tc TypeChecker) ownership_string_builtin_keeps_receiver(recv_id flat.NodeId, method_name string) bool {
	if unwrap_pointer(tc.resolve_type(recv_id)) !is String {
		return false
	}
	return tc.ownership_fn_declared_in_builtin('string.${method_name}')
}

fn (tc &TypeChecker) ownership_fn_declared_in_builtin(fn_name string) bool {
	file := tc.fn_type_files[fn_name] or { return false }
	normalized := file.replace('\\', '/')
	return normalized.starts_with('vlib/builtin/')
		|| normalized.contains('/vlib/builtin/')
		|| (normalized.contains('/v3_module_cache_') && normalized.ends_with('.vh')
		&& normalized.all_after_last('/').starts_with('builtin_'))
}
