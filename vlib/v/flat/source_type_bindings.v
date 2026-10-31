module flat

// SourceTypeBindings stores source spelling IDs keyed by source spelling IDs.
pub struct SourceTypeBindings {
pub mut:
	ids map[u32]u32
}

// from_names indexes source spellings supplied at a compiler integration boundary.
pub fn SourceTypeBindings.from_names(values map[string]string) SourceTypeBindings {
	mut bindings := SourceTypeBindings{}
	for name, text in values {
		bindings.set(source_type_id(name), text)
	}
	return bindings
}

// get returns the spelling of a present binding, including an explicitly empty one.
pub fn (bindings &SourceTypeBindings) get(name u32) ?string {
	id := bindings.ids[name] or { return none }
	return source_type_name(id)
}

// value returns a binding's spelling, or an empty spelling when it is absent.
pub fn (bindings &SourceTypeBindings) value(name u32) string {
	return bindings.get(name) or { '' }
}

// value_id returns a binding's source identity, or zero when it is absent.
@[inline]
pub fn (bindings &SourceTypeBindings) value_id(name u32) u32 {
	return bindings.ids[name] or { 0 }
}

// type_id returns the numeric source-spelling identity of a present binding.
pub fn (bindings &SourceTypeBindings) type_id(name u32) ?u32 {
	return bindings.ids[name] or { return none }
}

// set_id stores a source-spelling identity without formatting its spelling.
pub fn (mut bindings SourceTypeBindings) set_id(name u32, id u32) {
	bindings.ids[name] = id
}

// set interns a source spelling and records its identity for the binding.
pub fn (mut bindings SourceTypeBindings) set(name u32, text string) {
	bindings.ids[name] = source_type_id(text)
}

// contains reports whether a binding exists, including one with an empty spelling.
pub fn (bindings &SourceTypeBindings) contains(name u32) bool {
	return name in bindings.ids
}

// keys returns source spelling IDs in insertion order.
pub fn (bindings &SourceTypeBindings) keys() []u32 {
	return bindings.ids.keys()
}

// values resolves indexed values at an integration boundary.
pub fn (bindings &SourceTypeBindings) values() []string {
	mut result := []string{cap: bindings.ids.len}
	for _, id in bindings.ids {
		result << source_type_name(id)
	}
	return result
}

// names exports spelling values for an integration that still consumes text.
pub fn (bindings &SourceTypeBindings) names() map[string]string {
	mut result := map[string]string{}
	result.reserve(u32(bindings.ids.len))
	for name, id in bindings.ids {
		result[source_type_name(name)] = source_type_name(id)
	}
	return result
}

// reserve reserves binding slots without changing their identities.
pub fn (mut bindings SourceTypeBindings) reserve(count u32) {
	bindings.ids.reserve(count)
}

// delete removes a binding without changing the identities of other spellings.
pub fn (mut bindings SourceTypeBindings) delete(name u32) {
	bindings.ids.delete(name)
}

// clone copies the numeric binding map for an independent writer.
pub fn (bindings &SourceTypeBindings) clone() SourceTypeBindings {
	return SourceTypeBindings{
		ids: bindings.ids.clone()
	}
}

// clone_owned copies lookup storage into the current arena.
// Source spelling IDs already outlive every disposable worker arena.
pub fn (bindings &SourceTypeBindings) clone_owned() SourceTypeBindings {
	mut result := SourceTypeBindings{}
	result.ids.reserve(u32(bindings.ids.len))
	for name, id in bindings.ids {
		result.ids[name] = id
	}
	return result
}

// clear removes bindings without changing source spelling IDs.
pub fn (mut bindings SourceTypeBindings) clear() {
	bindings.ids.clear()
}

// move transfers numeric binding storage.
pub fn (mut bindings SourceTypeBindings) move() SourceTypeBindings {
	return SourceTypeBindings{
		ids: bindings.ids.move()
	}
}

// free releases the numeric binding map.
pub fn (mut bindings SourceTypeBindings) free() {
	unsafe { bindings.ids.free() }
}

// fork copies bindings for an independent writer.
pub fn (bindings &SourceTypeBindings) fork() SourceTypeBindings {
	return bindings.clone()
}
