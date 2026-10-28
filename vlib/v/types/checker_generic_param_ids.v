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

// The one capital letter spellings `A`..`Z`, indexed by letter.
const generic_param_letter_ids = generic_param_letter_spellings()

fn generic_param_letter_spellings() []u32 {
	mut ids := []u32{cap: 26}
	for letter := u8(`A`); letter <= `Z`; letter++ {
		ids << flat.source_type_id(letter.ascii_str())
	}
	return ids
}

// generic_param_letter_mask returns the enclosing-parameter mask bit of a one
// capital letter type parameter, or 0 for any other spelling.
fn generic_param_letter_mask(param u32) u32 {
	idx := generic_param_letter_ids.index(param)
	if idx < 0 {
		return 0
	}
	return u32(1) << u32(idx)
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
