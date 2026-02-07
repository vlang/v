// BEAM map implementation
// V maps are represented as Erlang maps on BEAM
//
// IMPORTANT: Most map methods are intercepted by the BEAM codegen
// (core_map_method in core_exprs.v) and emitted as direct Erlang BIF calls:
//   keys()   -> maps:keys(Map)
//   values() -> maps:values(Map)
//   clone()  -> identity (maps are immutable on BEAM)
//   delete() -> maps:remove(Key, Map)
//
// Map mutation (m[k] = v) is handled by codegen as maps:put(K, V, Map).
// Map access (m[k]) is handled by codegen as maps:get(K, Map).
//
// These stubs exist to satisfy the V type checker.
module builtin

// map is the internal representation of a V `map` type.
// On BEAM: maps are native Erlang maps (immutable, hash-based)
struct map {
pub:
	len int
}

// clear clears the map
// On BEAM: maps are immutable, clearing returns empty map #{}
// Codegen should intercept this and emit maps:new()
pub fn (mut m map) clear() {
	// No-op stub - BEAM codegen handles this as #{}
	// Cannot reassign m to empty map from V code since `map{}` is deprecated syntax
}

// move returns the map and resets the original
// On BEAM: maps are immutable, move is identity
// Ownership semantics don't apply on BEAM (GC handles everything)
pub fn (mut m map) move() map {
	return m
}

// reserve is a no-op on BEAM (Erlang maps resize automatically)
pub fn (mut m map) reserve(meta_bytes u32) {
}

// delete removes a key from the map
// Codegen intercepts: maps:remove(Key, Map)
pub fn (mut m map) delete(key voidptr) {
}

// keys returns all keys as an array
// Codegen intercepts: maps:keys(Map)
pub fn (m &map) keys() array {
	return array{}
}

// values returns all values as an array
// Codegen intercepts: maps:values(Map)
pub fn (m &map) values() array {
	return array{}
}

// clone returns a copy of the map
// Codegen intercepts: identity (maps are immutable on BEAM)
pub fn (m &map) clone() map {
	return *m
}

// free is a no-op on BEAM (garbage collected)
pub fn (m &map) free() {
}
