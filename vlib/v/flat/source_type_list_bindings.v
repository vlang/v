module flat

// SourceTypeListBindings stores ordered source spelling IDs by source spelling ID.
pub struct SourceTypeListBindings {
pub mut:
	ids map[u32][]u32
}

// from_names indexes lists supplied at a source integration boundary.
pub fn SourceTypeListBindings.from_names(values map[string][]string) SourceTypeListBindings {
	mut result := SourceTypeListBindings{}
	for name, texts in values {
		result.set(source_type_id(name), texts)
	}
	return result
}

// type_ids returns the numeric values of a present binding.
pub fn (bindings &SourceTypeListBindings) type_ids(name u32) ?[]u32 {
	return bindings.ids[name] or { return none }
}

// get resolves a present binding at a text-consuming integration boundary.
pub fn (bindings &SourceTypeListBindings) get(name u32) ?[]string {
	ids := bindings.type_ids(name) or { return none }
	mut result := []string{cap: ids.len}
	for id in ids {
		result << source_type_name(id)
	}
	return result
}

// value resolves a list, returning an empty list for an absent binding.
pub fn (bindings &SourceTypeListBindings) value(name u32) []string {
	return bindings.get(name) or { []string{} }
}

// set indexes a list while preserving its order and empty entries.
pub fn (mut bindings SourceTypeListBindings) set(name u32, texts []string) {
	mut ids := []u32{cap: texts.len}
	for text in texts {
		ids << source_type_id(text)
	}
	bindings.ids[name] = ids
}

// set_ids copies a list of stable identities without rendering their spellings.
pub fn (mut bindings SourceTypeListBindings) set_ids(name u32, ids []u32) {
	bindings.ids[name] = ids.clone()
}

// append indexes additional type spellings at the end of a binding.
pub fn (mut bindings SourceTypeListBindings) append(name u32, texts []string) {
	for text in texts {
		bindings.ids[name] << source_type_id(text)
	}
}

// append_ids copies additional identities into a binding without resolving spellings.
pub fn (mut bindings SourceTypeListBindings) append_ids(name u32, ids []u32) {
	if ids.len == 0 {
		return
	}
	bindings.ids[name] << ids
}

// contains reports whether a binding exists, including an explicitly empty list.
pub fn (bindings &SourceTypeListBindings) contains(name u32) bool {
	return name in bindings.ids
}

// keys returns source spelling IDs in insertion order.
pub fn (bindings &SourceTypeListBindings) keys() []u32 {
	return bindings.ids.keys()
}

// clone copies the map and its numeric lists for an independent writer.
pub fn (bindings &SourceTypeListBindings) clone() SourceTypeListBindings {
	mut result := SourceTypeListBindings{}
	result.ids.reserve(u32(bindings.ids.len))
	for name, ids in bindings.ids {
		result.ids[name] = ids.clone()
	}
	return result
}

// clone_owned copies lookup storage and numeric lists into the current allocation arena.
pub fn (bindings &SourceTypeListBindings) clone_owned() SourceTypeListBindings {
	mut result := SourceTypeListBindings{}
	result.ids.reserve(u32(bindings.ids.len))
	for name, ids in bindings.ids {
		result.ids[name] = ids.clone()
	}
	return result
}

// delete removes a declaration binding.
pub fn (mut bindings SourceTypeListBindings) delete(name u32) {
	bindings.ids.delete(name)
}

// clear removes all declaration bindings.
pub fn (mut bindings SourceTypeListBindings) clear() {
	bindings.ids.clear()
}

// reserve reserves declaration slots without changing their values.
pub fn (mut bindings SourceTypeListBindings) reserve(count u32) {
	bindings.ids.reserve(count)
}
