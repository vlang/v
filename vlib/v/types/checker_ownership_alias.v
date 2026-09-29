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

// ownership_return_param_call_source composes, in function `caller`, a callee's returned
// parameter path behind the projection of the argument passed for that parameter: when the
// callee returns `.name` of its parameter, `callee(p.next)` returns `.next.name` of `p`. It also
// returns whether the result is a prefix source, which aliases storage at or below the path
// rather than that exact path, and the functions that composed the path (`callee_via` lists
// those of the callee's path).
//
// Around a call cycle the argument projection is prepended again every round (`.next.name`,
// `.next.next.name`, ...), so the return fixed point would never converge, and it would grow
// exponentially for a function that recurses through several fields. A cycle shows as a
// callee path that `caller` already composed; such a call yields a prefix source at the
// argument projection. Every other composition extends `via` by a function that is not in it
// yet, which bounds the exact paths, and keeps acyclic call chains exact.
fn ownership_return_param_call_source(caller string, arg_suffix string, callee_suffix string, callee_is_prefix bool, callee_via []string) (string, bool, []string) {
	if caller in callee_via {
		return arg_suffix, true, [caller]
	}
	return arg_suffix + callee_suffix, callee_is_prefix, ownership_return_param_via(callee_via,
		caller)
}

// ownership_return_param_via returns `via` extended by `caller`, unless it already lists it.
fn ownership_return_param_via(via []string, caller string) []string {
	if caller in via {
		return via
	}
	mut out := []string{cap: via.len + 1}
	out << via
	out << caller
	return out
}
