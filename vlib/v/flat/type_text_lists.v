module flat

// TypeTextListBindings stores ordered lists of source type IDs by declaration name.
pub struct TypeTextListBindings {
pub mut:
	ids map[string][]u32
}

// from_names indexes lists supplied at a source integration boundary.
pub fn TypeTextListBindings.from_names(values map[string][]string) TypeTextListBindings {
	mut result := TypeTextListBindings{}
	for name, texts in values {
		result.set(name, texts)
	}
	return result
}

// type_ids returns the numeric values of a present binding.
pub fn (bindings &TypeTextListBindings) type_ids(name string) ?[]u32 {
	return bindings.ids[name] or { return none }
}

// get resolves a present binding at a text-consuming integration boundary.
pub fn (bindings &TypeTextListBindings) get(name string) ?[]string {
	ids := bindings.type_ids(name) or { return none }
	mut result := []string{cap: ids.len}
	for id in ids {
		result << source_type_name(id)
	}
	return result
}

// value resolves a list, returning an empty list for an absent binding.
pub fn (bindings &TypeTextListBindings) value(name string) []string {
	return bindings.get(name) or { []string{} }
}

// set indexes a list while preserving its order and empty entries.
pub fn (mut bindings TypeTextListBindings) set(name string, texts []string) {
	mut ids := []u32{cap: texts.len}
	for text in texts {
		ids << source_type_id(text)
	}
	bindings.ids[name] = ids
}

// set_ids copies a list of stable identities without rendering their spellings.
pub fn (mut bindings TypeTextListBindings) set_ids(name string, ids []u32) {
	bindings.ids[name] = ids.clone()
}

// append indexes additional type spellings at the end of a binding.
pub fn (mut bindings TypeTextListBindings) append(name string, texts []string) {
	for text in texts {
		bindings.ids[name] << source_type_id(text)
	}
}

// append_ids copies additional identities into a binding without resolving spellings.
pub fn (mut bindings TypeTextListBindings) append_ids(name string, ids []u32) {
	if ids.len == 0 {
		return
	}
	bindings.ids[name] << ids
}

// contains reports whether a binding exists, including an explicitly empty list.
pub fn (bindings &TypeTextListBindings) contains(name string) bool {
	return name in bindings.ids
}

// keys returns declaration names in insertion order.
pub fn (bindings &TypeTextListBindings) keys() []string {
	return bindings.ids.keys()
}

// clone copies the map and its numeric lists for an independent writer.
pub fn (bindings &TypeTextListBindings) clone() TypeTextListBindings {
	mut result := TypeTextListBindings{}
	result.ids.reserve(u32(bindings.ids.len))
	for name, ids in bindings.ids {
		result.ids[name] = ids.clone()
	}
	return result
}

// clone_owned also promotes declaration keys into the current allocation arena.
pub fn (bindings &TypeTextListBindings) clone_owned() TypeTextListBindings {
	mut result := TypeTextListBindings{}
	result.ids.reserve(u32(bindings.ids.len))
	for name, ids in bindings.ids {
		result.ids[name.clone()] = ids.clone()
	}
	return result
}

// delete removes a declaration binding.
pub fn (mut bindings TypeTextListBindings) delete(name string) {
	bindings.ids.delete(name)
}

// clear removes all declaration bindings.
pub fn (mut bindings TypeTextListBindings) clear() {
	bindings.ids.clear()
}

// reserve reserves declaration slots without changing their values.
pub fn (mut bindings TypeTextListBindings) reserve(count u32) {
	bindings.ids.reserve(count)
}
