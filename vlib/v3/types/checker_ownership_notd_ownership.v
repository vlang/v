module types

import v3.flat

struct OwnershipState {}

pub struct OwnershipDropEntry {
pub:
	name             string
	type_name        string
	optional_wrapper bool
}

pub fn (tc &TypeChecker) ownership_drop_entries_at_return(_ string, _ int) []OwnershipDropEntry {
	return []OwnershipDropEntry{}
}

pub fn (tc &TypeChecker) ownership_drop_entries_at_return_node(_ string, _ flat.NodeId) []OwnershipDropEntry {
	return []OwnershipDropEntry{}
}

pub fn (tc &TypeChecker) ownership_drop_entries_at_propagation(_ string, _ int) []OwnershipDropEntry {
	return []OwnershipDropEntry{}
}

pub fn (tc &TypeChecker) ownership_drop_entries_at_loop_control(_ string, _ int) []OwnershipDropEntry {
	return []OwnershipDropEntry{}
}

pub fn (tc &TypeChecker) ownership_drop_entries_at_loop_iteration(_ string, _ int) []OwnershipDropEntry {
	return []OwnershipDropEntry{}
}

pub fn (tc &TypeChecker) ownership_drop_entries_at_scope_exit(_ string, _ int) []OwnershipDropEntry {
	return []OwnershipDropEntry{}
}

pub fn (tc &TypeChecker) ownership_drop_entries_at_fn_exit(_ string) []OwnershipDropEntry {
	return []OwnershipDropEntry{}
}

pub fn (tc &TypeChecker) ownership_drop_type_names() []string {
	return []string{}
}

pub fn (tc &TypeChecker) ownership_drop_value_type_names_by_fn() map[string][]string {
	return map[string][]string{}
}

pub fn (tc &TypeChecker) ownership_has_return_node(_ string, _ flat.NodeId) bool {
	return false
}

pub fn (tc &TypeChecker) ownership_type_requires_drop(_ Type) bool {
	return false
}

pub fn (tc &TypeChecker) ownership_type_requires_destruction(_ Type) bool {
	return false
}

pub fn (tc &TypeChecker) ownership_expr_moves_storage(_ flat.NodeId, _ flat.NodeId) bool {
	return false
}

pub fn (tc &TypeChecker) ownership_expr_creates_owned_value(_ flat.NodeId) bool {
	return false
}

pub fn (tc &TypeChecker) ownership_index_read_moves_value(_ flat.NodeId) bool {
	return false
}

// ownership_receiver_alias_arg_is_cloned reports no receiver clone metadata when ownership is disabled.
pub fn (tc &TypeChecker) ownership_receiver_alias_arg_is_cloned(_ flat.NodeId) bool {
	return false
}

pub fn (tc &TypeChecker) ownership_expr_is_borrowed_projection(_ flat.NodeId) bool {
	return false
}

// ownership_expr_clones_borrowed_storage reports no clone metadata when ownership is disabled.
pub fn (tc &TypeChecker) ownership_expr_clones_borrowed_storage(_ flat.NodeId) bool {
	return false
}

pub fn (tc &TypeChecker) ownership_guard_read_moves_value(_ flat.NodeId) bool {
	return false
}

pub fn (tc &TypeChecker) ownership_assignment_reinitializes_moved_value(_ flat.NodeId) bool {
	return false
}

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

pub fn (tc &TypeChecker) ownership_default_clone_missing_method(_ Type) ?string {
	return none
}

pub fn (mut tc TypeChecker) inherit_ownership_codegen_metadata_from(_ &TypeChecker) {}
