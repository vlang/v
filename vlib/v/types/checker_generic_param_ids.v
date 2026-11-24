module types

import v.flat

// generic_param_ids interns the generic parameter names a declaration spells
// (`node.generic_params()`), in their declared order. The checker stores type
// parameters as spelling IDs from this AST boundary on.
pub fn generic_param_ids(names []string) []u32 {
	if names.len == 0 {
		return []u32{}
	}
	mut ids := []u32{cap: names.len}
	for name in names {
		ids << flat.source_type_id(name)
	}
	return ids
}

// generic_param_texts decodes type parameter spelling IDs for a consumer
// that still works on parameter names as text.
pub fn generic_param_texts(ids []u32) []string {
	if ids.len == 0 {
		return []string{}
	}
	mut names := []string{cap: ids.len}
	for id in ids {
		names << flat.source_type_name(id)
	}
	return names
}

fn clone_owned_id_list_map(source map[u32][]u32) map[u32][]u32 {
	mut result := map[u32][]u32{}
	result.reserve(u32(source.len))
	for key, values in source {
		result[key] = values.clone()
	}
	return result
}

fn clone_owned_string_id_list_map(source map[string][]u32) map[string][]u32 {
	mut result := map[string][]u32{}
	result.reserve(u32(source.len))
	for key, values in source {
		result[key.clone()] = values.clone()
	}
	return result
}

// generic_param_index_of_name returns the position of the type parameter
// spelled `name` in `param_ids`, or -1. A spelling that was never interned
// cannot name a declared parameter.
fn generic_param_index_of_name(param_ids []u32, name string) int {
	if param_ids.len == 0 {
		return -1
	}
	id := flat.source_type_existing_id(name) or { return -1 }
	return param_ids.index(id)
}

// generic_param_ids_contain_name reports whether `name` spells one of `param_ids`.
fn generic_param_ids_contain_name(param_ids []u32, name string) bool {
	return generic_param_index_of_name(param_ids, name) >= 0
}

// generic_param_ids_join spells type parameter IDs for a diagnostic, `T, U`.
fn generic_param_ids_join(ids []u32, sep string) string {
	return generic_param_texts(ids).join(sep)
}

// generic_param_id_set indexes type parameter IDs for membership tests.
fn generic_param_id_set(ids []u32) map[u32]bool {
	mut params := map[u32]bool{}
	for id in ids {
		params[id] = true
	}
	return params
}

// generic_param_ids_from_set lists a set of type parameter IDs ordered by
// their spellings, the order declaration-inferred parameter lists use.
fn generic_param_ids_from_set(params map[u32]bool) []u32 {
	mut ids := []u32{cap: params.len}
	for id, _ in params {
		ids << id
	}
	sort_generic_param_ids(mut ids)
	return ids
}

// sort_generic_param_ids orders type parameter IDs by their spellings.
fn sort_generic_param_ids(mut ids []u32) {
	if ids.len < 2 {
		return
	}
	ids.sort_with_compare(fn (a &u32, b &u32) int {
		a_name := flat.source_type_name(*a)
		b_name := flat.source_type_name(*b)
		if a_name < b_name {
			return -1
		}
		if a_name > b_name {
			return 1
		}
		return 0
	})
}

// generic_param_mask_ranges_min_nodes is the size from which an AST records
// the generic parameter letters of its nodes as ranges instead of per node.
const generic_param_mask_ranges_min_nodes = 65536

// GenericParamMaskRanges records, for the nodes of generic function
// declarations, the mask of one-letter generic parameters of the declaration
// that encloses them (see generic_param_letter_mask). Few declarations are
// generic, so it keeps their node ranges in place of a mask for every node.
struct GenericParamMaskRanges {
mut:
	// The nodes [0, nodes) are covered: one outside every range has no
	// parameter. Zero while the checker keeps a mask per node instead.
	nodes  int
	starts []i32 // ascending
	ends   []i32 // exclusive
	masks  []u32
	// Two ranges overlap, so a node takes the masks of all that hold it.
	overlapping bool
}

// add gives the nodes [start, end) the parameter letters of `mask` too.
fn (mut r GenericParamMaskRanges) add(start int, end int, mask u32) {
	if mask == 0 || end <= start {
		return
	}
	n := r.starts.len
	if n == 0 || start >= int(r.ends[n - 1]) {
		r.starts << i32(start)
		r.ends << i32(end)
		r.masks << mask
		return
	}
	// The first range that does not start before this one.
	mut lo := 0
	mut hi := n
	for lo < hi {
		mid := (lo + hi) >> 1
		if int(r.starts[mid]) < start {
			lo = mid + 1
		} else {
			hi = mid
		}
	}
	if lo < n && int(r.starts[lo]) == start && int(r.ends[lo]) == end {
		r.masks[lo] |= mask
		return
	}
	if (lo > 0 && int(r.ends[lo - 1]) > start) || (lo < n && int(r.starts[lo]) < end) {
		r.overlapping = true
	}
	r.starts.insert(lo, i32(start))
	r.ends.insert(lo, i32(end))
	r.masks.insert(lo, mask)
}

// mask_at returns the parameter letters of the declaration enclosing node `idx`.
@[direct_array_access]
fn (r &GenericParamMaskRanges) mask_at(idx int) u32 {
	n := r.starts.len
	if n == 0 {
		return 0
	}
	if r.overlapping {
		mut mask := u32(0)
		for i in 0 .. n {
			if idx >= int(r.starts[i]) && idx < int(r.ends[i]) {
				mask |= r.masks[i]
			}
		}
		return mask
	}
	// The last range that starts at or before the node.
	mut lo := 0
	mut hi := n
	for lo < hi {
		mid := (lo + hi) >> 1
		if int(r.starts[mid]) <= idx {
			lo = mid + 1
		} else {
			hi = mid
		}
	}
	if lo > 0 && idx < int(r.ends[lo - 1]) {
		return r.masks[lo - 1]
	}
	return 0
}

// generic_param_letter_mask returns the enclosing-parameter mask bit of a one
// capital letter type parameter, `T`, or 0 for any other spelling. The bit is
// the letter's own, so the mask needs the spelling's text.
fn generic_param_letter_mask(param u32) u32 {
	if !source_type_is_bare_generic_param(param) {
		return 0
	}
	return u32(1) << u32(flat.source_type_name(param)[0] - `A`)
}

// generic_param_layout_name spells a generic declaration applied to its own
// type parameters without spaces, `Tree[T,U]`.
fn generic_param_layout_name(base_id u32, params []u32) u32 {
	mut parts := []u32{cap: params.len * 2 + 2}
	parts << base_id
	parts << checker_source_open_bracket
	for i, param in params {
		if i > 0 {
			parts << checker_source_comma
		}
		parts << param
	}
	parts << checker_source_close_bracket
	return flat.source_type_composition(.concat, parts)
}

// generic_param_constraint_named reports whether the source name `name`
// spells a type parameter that `constraints` constrains.
fn generic_param_constraint_named(constraints map[u32]GenericConstraint, name string) bool {
	id := flat.source_type_existing_id(name) or { return false }
	return id in constraints
}

// generic_param_set_has_name reports whether the source text `name` spells one
// of the type parameters of `params`.
fn generic_param_set_has_name(params map[u32]bool, name string) bool {
	id := flat.source_type_existing_id(name) or { return false }
	return id in params
}

// generic_param_bound_named reports whether the source text `name` spells a
// type parameter that `bindings` gives a type.
fn generic_param_bound_named(bindings map[u32]u32, name string) bool {
	id := flat.source_type_existing_id(name) or { return false }
	return id in bindings
}
