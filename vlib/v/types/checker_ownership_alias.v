module types

// This file is compiled with and without `-d ownership`, so that its pure helpers can be
// unit tested by the regular compiler. A `-d ownership` build selects the ownership
// compiler, which would also run ownership analysis over the whole `types` module.

const ownership_unknown_pointer_index_alias = '<unknown-index-alias>'

// ownership_alias_chain_borrows_indexed_storage reports whether `rhs_name` reaches indexed
// storage (or an unknown index alias) through the recorded pointer index aliases.
fn ownership_alias_chain_borrows_indexed_storage(aliases map[string]string, rhs_name string) bool {
	// Follow the complete alias chain (`arr -> val -> t[k]`). Cycles represent unresolved
	// alias state, so treat them conservatively as borrowed storage.
	mut cur := rhs_name
	mut seen := map[string]bool{}
	for {
		if seen[cur] {
			return true
		}
		seen[cur] = true
		source := aliases[cur] or { return false }
		if source == ownership_unknown_pointer_index_alias {
			return true
		}
		if source.contains('[') {
			return true
		}
		cur = source
	}
	return false
}
