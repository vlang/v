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

// ownership_return_param_call_source composes a callee's returned parameter path behind the
// projection of the argument passed for that parameter: when `f` returns `.name` of its
// parameter, `f(p.next)` returns `.next.name` of `p`. The second result reports a prefix
// source, which aliases storage at or below the returned path rather than that exact path.
//
// Through recursion the argument projection is prepended again every round (`.next.name`,
// `.next.next.name`, ...), so the return fixed point would never converge, and it would grow
// exponentially for a function that recurses through several fields. So a direct recursive
// call, or a call whose returned path already contains the argument projection (the path went
// around a call cycle), yields a prefix source at the argument projection. That bounds every
// composed path by the argument projections of the call sites it passes through; acyclic call
// chains keep their exact paths.
fn ownership_return_param_call_source(arg_suffix string, callee_suffix string, callee_is_prefix bool, is_recursive_call bool) (string, bool) {
	if is_recursive_call || ownership_storage_suffix_contains_projection(callee_suffix, arg_suffix) {
		return arg_suffix, true
	}
	return arg_suffix + callee_suffix, callee_is_prefix
}

// ownership_storage_suffix_contains_projection reports whether the storage suffix `suffix`
// contains all projections of `part`: `.next` is in `.left.next.name`, but not in `.nextval`.
fn ownership_storage_suffix_contains_projection(suffix string, part string) bool {
	if part.len == 0 {
		return false
	}
	mut start := 0
	for {
		idx := suffix.index_after(part, start) or { return false }
		end := idx + part.len
		if end == suffix.len || suffix[end] in [`.`, `[`] {
			return true
		}
		start = idx + 1
	}
	return false
}
