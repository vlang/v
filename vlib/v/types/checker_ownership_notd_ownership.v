module types

import v.flat

struct OwnershipState {}

pub struct OwnershipDropEntry {
pub:
	name             string
	type_name        string
	optional_wrapper bool
}

// ownership_drop_entries_at_return returns no destructor entries when ownership checking is disabled.
pub fn (tc &TypeChecker) ownership_drop_entries_at_return(_ string, _ int) []OwnershipDropEntry {
	return []OwnershipDropEntry{}
}

// ownership_drop_entries_at_return_node returns no destructor entries when ownership checking is disabled.
pub fn (tc &TypeChecker) ownership_drop_entries_at_return_node(_ string, _ flat.NodeId) []OwnershipDropEntry {
	return []OwnershipDropEntry{}
}

// ownership_drop_entries_at_propagation returns no destructor entries when ownership checking is disabled.
pub fn (tc &TypeChecker) ownership_drop_entries_at_propagation(_ string, _ int) []OwnershipDropEntry {
	return []OwnershipDropEntry{}
}

// ownership_drop_entries_at_loop_control returns no destructor entries when ownership checking is disabled.
pub fn (tc &TypeChecker) ownership_drop_entries_at_loop_control(_ string, _ int) []OwnershipDropEntry {
	return []OwnershipDropEntry{}
}

// ownership_drop_entries_at_loop_iteration returns no destructor entries when ownership checking is disabled.
pub fn (tc &TypeChecker) ownership_drop_entries_at_loop_iteration(_ string, _ int) []OwnershipDropEntry {
	return []OwnershipDropEntry{}
}

// ownership_drop_entries_at_scope_exit returns no destructor entries when ownership checking is disabled.
pub fn (tc &TypeChecker) ownership_drop_entries_at_scope_exit(_ string, _ int) []OwnershipDropEntry {
	return []OwnershipDropEntry{}
}

// ownership_drop_entries_at_fn_exit returns no destructor entries when ownership checking is disabled.
pub fn (tc &TypeChecker) ownership_drop_entries_at_fn_exit(_ string) []OwnershipDropEntry {
	return []OwnershipDropEntry{}
}

// ownership_drop_type_names returns no drop receiver types when ownership checking is disabled.
pub fn (tc &TypeChecker) ownership_drop_type_names() []string {
	return []string{}
}

// ownership_drop_value_type_names_by_fn returns no per-function drop types when ownership checking is disabled.
pub fn (tc &TypeChecker) ownership_drop_value_type_names_by_fn() map[string][]string {
	return map[string][]string{}
}

// ownership_has_return_node reports no recorded return-node cleanup when ownership checking is disabled.
pub fn (tc &TypeChecker) ownership_has_return_node(_ string, _ flat.NodeId) bool {
	return false
}

// ownership_type_requires_drop reports that no type requires ownership drop handling when ownership checking is disabled.
pub fn (tc &TypeChecker) ownership_type_requires_drop(_ Type) bool {
	return false
}

// ownership_type_requires_destruction reports that no type requires ownership destruction when ownership checking is disabled.
pub fn (tc &TypeChecker) ownership_type_requires_destruction(_ Type) bool {
	return false
}

// ownership_expr_moves_storage reports no storage moves when ownership checking is disabled.
pub fn (tc &TypeChecker) ownership_expr_moves_storage(_ flat.NodeId, _ flat.NodeId) bool {
	return false
}

// ownership_expr_creates_owned_value reports no owned-value creation when ownership checking is disabled.
pub fn (tc &TypeChecker) ownership_expr_creates_owned_value(_ flat.NodeId) bool {
	return false
}

// ownership_index_read_moves_value reports no moving index reads when ownership checking is disabled.
pub fn (tc &TypeChecker) ownership_index_read_moves_value(_ flat.NodeId) bool {
	return false
}

// ownership_receiver_alias_arg_is_cloned reports no receiver clone metadata when ownership is disabled.
pub fn (tc &TypeChecker) ownership_receiver_alias_arg_is_cloned(_ flat.NodeId) bool {
	return false
}

// ownership_expr_is_borrowed_projection reports no borrowed projections when ownership checking is disabled.
pub fn (tc &TypeChecker) ownership_expr_is_borrowed_projection(_ flat.NodeId) bool {
	return false
}

// ownership_expr_clones_borrowed_storage reports no clone metadata when ownership is disabled.
pub fn (tc &TypeChecker) ownership_expr_clones_borrowed_storage(_ flat.NodeId) bool {
	return false
}

// ownership_guard_read_moves_value reports no moving guard reads when ownership checking is disabled.
pub fn (tc &TypeChecker) ownership_guard_read_moves_value(_ flat.NodeId) bool {
	return false
}

// ownership_assignment_reinitializes_moved_value reports no moved-value reinitialization when ownership checking is disabled.
pub fn (tc &TypeChecker) ownership_assignment_reinitializes_moved_value(_ flat.NodeId) bool {
	return false
}

// ownership_fn_value_returns_owned reports no owned function-value returns when ownership checking is disabled.
pub fn (tc &TypeChecker) ownership_fn_value_returns_owned(_ flat.NodeId, _ string, _ string) bool {
	return false
}

// ownership_call_result_source_args is empty when ownership analysis is disabled.
pub fn (mut tc TypeChecker) ownership_call_result_source_args(_ flat.NodeId) []flat.NodeId {
	return []flat.NodeId{}
}

// ownership_call_result_sources is empty when ownership analysis is disabled.
pub fn (mut tc TypeChecker) ownership_call_result_sources(_ flat.NodeId) []OwnershipCallResultSource {
	return []OwnershipCallResultSource{}
}

// ownership_default_clone_missing_method reports no missing clone method when ownership checking is disabled.
pub fn (tc &TypeChecker) ownership_default_clone_missing_method(_ Type) ?string {
	return none
}

// inherit_ownership_codegen_metadata_from is a no-op when ownership checking is disabled.
pub fn (mut tc TypeChecker) inherit_ownership_codegen_metadata_from(_ &TypeChecker) {}
