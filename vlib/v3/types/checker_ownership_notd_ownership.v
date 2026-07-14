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

pub fn (mut tc TypeChecker) inherit_ownership_codegen_metadata_from(_ &TypeChecker) {}
