module flat

// Source spellings are distinct from checked TypeId values: an unresolved
// generic or nominal alias must retain exactly the spelling inference uses.
// TypeTextTable caches process-wide source spelling identities for one compiler worker.
@[heap]
pub struct TypeTextTable {
mut:
	recent       [128]u32
	recent_texts [128]string
}

// intern returns the numeric identity of text, caching its process-owned spelling.
pub fn (mut table TypeTextTable) intern(text string) u32 {
	if text.len == 0 {
		return 0
	}
	// Pointer reuse is safe: every hit compares the current bytes with the
	// immutable process-owned spelling, never with a borrowed input header.
	slot := int((uint(text.str) >> 4 ^ uint(text.len)) & 127)
	recent := table.recent[slot]
	if recent != 0 && table.recent_texts[slot] == text {
		return recent
	}
	id := source_type_id(text)
	canonical := source_type_name(id)
	// The global interner owns these immutable bytes for the process lifetime.
	// Cache only a nonfreeing header, including when the table is copied or dropped.
	table.recent_texts[slot] = unsafe {
		canonical.str.vstring_literal_with_len(canonical.len)
	}
	table.recent[slot] = id
	return id
}

// text resolves a source-spelling identity; zero represents an empty spelling.
@[inline]
pub fn (table &TypeTextTable) text(id u32) string {
	return source_type_name(id)
}

// TypeTextBindings stores source type spellings by numeric identity within one table.
pub struct TypeTextBindings {
pub:
	texts &TypeTextTable = &TypeTextTable{}
pub mut:
	ids map[string]u32
}

// from_names indexes source spellings supplied at a compiler integration boundary.
pub fn TypeTextBindings.from_names(values map[string]string) TypeTextBindings {
	mut bindings := TypeTextBindings{}
	for name, text in values {
		bindings.set(name, text)
	}
	return bindings
}

// get returns the spelling of a present binding, including an explicitly empty one.
pub fn (bindings &TypeTextBindings) get(name string) ?string {
	id := bindings.ids[name] or { return none }
	return bindings.texts.text(id)
}

// value returns a binding's spelling, or an empty spelling when it is absent.
pub fn (bindings &TypeTextBindings) value(name string) string {
	return bindings.get(name) or { '' }
}

// value_id returns a binding's source identity, or zero when it is absent.
@[inline]
pub fn (bindings &TypeTextBindings) value_id(name string) u32 {
	return bindings.ids[name] or { 0 }
}

// type_id returns the numeric source-spelling identity of a present binding.
pub fn (bindings &TypeTextBindings) type_id(name string) ?u32 {
	return bindings.ids[name] or { return none }
}

// set_id stores an identity from this binding table without formatting its spelling.
pub fn (mut bindings TypeTextBindings) set_id(name string, id u32) {
	bindings.ids[name] = id
}

// set interns a source spelling and records its identity for the binding.
pub fn (mut bindings TypeTextBindings) set(name string, text string) {
	mut table := bindings.texts
	bindings.ids[name] = table.intern(text)
}

// contains reports whether a binding exists, including one with an empty spelling.
pub fn (bindings &TypeTextBindings) contains(name string) bool {
	return name in bindings.ids
}

// keys returns the declaration or binding names in insertion order.
pub fn (bindings &TypeTextBindings) keys() []string {
	return bindings.ids.keys()
}

// values resolves indexed values at an integration boundary.
pub fn (bindings &TypeTextBindings) values() []string {
	mut result := []string{cap: bindings.ids.len}
	for _, id in bindings.ids {
		result << source_type_name(id)
	}
	return result
}

// names exports spelling values for an integration that still consumes text.
pub fn (bindings &TypeTextBindings) names() map[string]string {
	mut result := map[string]string{}
	result.reserve(u32(bindings.ids.len))
	for name, id in bindings.ids {
		result[name] = source_type_name(id)
	}
	return result
}

// reserve reserves binding slots without changing their identities.
pub fn (mut bindings TypeTextBindings) reserve(count u32) {
	bindings.ids.reserve(count)
}

// delete removes a binding without changing the identities of other spellings.
pub fn (mut bindings TypeTextBindings) delete(name string) {
	bindings.ids.delete(name)
}

// clone copies the bindings while sharing their source-spelling table.
pub fn (bindings &TypeTextBindings) clone() TypeTextBindings {
	return TypeTextBindings{
		texts: bindings.texts
		ids:   bindings.ids.clone()
	}
}

// clone_owned copies binding keys and lookup storage into the current arena.
// Indexed spellings already outlive every disposable worker arena.
pub fn (bindings &TypeTextBindings) clone_owned() TypeTextBindings {
	mut result := TypeTextBindings{}
	result.ids.reserve(u32(bindings.ids.len))
	for name, id in bindings.ids {
		result.ids[name.clone()] = id
	}
	return result
}

// clear removes bindings while preserving the spelling table and existing IDs.
pub fn (mut bindings TypeTextBindings) clear() {
	bindings.ids.clear()
}

// move transfers binding storage while preserving its spelling namespace.
pub fn (mut bindings TypeTextBindings) move() TypeTextBindings {
	return TypeTextBindings{
		texts: bindings.texts
		ids:   bindings.ids.move()
	}
}

// free releases the binding map; the spelling table may still serve other scopes.
pub fn (mut bindings TypeTextBindings) free() {
	unsafe { bindings.ids.free() }
}

// NodeTypeTextBindings stores source type spellings by numeric identity within one table.
pub struct NodeTypeTextBindings {
pub:
	texts &TypeTextTable = &TypeTextTable{}
pub mut:
	ids map[int]u32
}

// get returns the spelling of a present binding, including an explicitly empty one.
pub fn (bindings &NodeTypeTextBindings) get(node int) ?string {
	id := bindings.ids[node] or { return none }
	return bindings.texts.text(id)
}

// value returns a node binding's spelling, or an empty spelling when it is absent.
pub fn (bindings &NodeTypeTextBindings) value(node int) string {
	return bindings.get(node) or { '' }
}

// value_id returns a node binding's source identity, or zero when it is absent.
@[inline]
pub fn (bindings &NodeTypeTextBindings) value_id(node int) u32 {
	return bindings.ids[node] or { 0 }
}

// type_id returns the numeric source-spelling identity of a present node binding.
pub fn (bindings &NodeTypeTextBindings) type_id(node int) ?u32 {
	return bindings.ids[node] or { return none }
}

// set_id records an existing source identity without resolving its spelling.
pub fn (mut bindings NodeTypeTextBindings) set_id(node int, id u32) {
	bindings.ids[node] = id
}

// set interns a source spelling and records its identity for the binding.
pub fn (mut bindings NodeTypeTextBindings) set(node int, text string) {
	mut table := bindings.texts
	bindings.ids[node] = table.intern(text)
}

// contains reports whether a binding exists, including one with an empty spelling.
pub fn (bindings &NodeTypeTextBindings) contains(node int) bool {
	return node in bindings.ids
}

// fork copies bindings and the private lookup cache for an independent worker.
// The immutable spelling identities remain valid across workers.
pub fn (bindings &TypeTextBindings) fork() TypeTextBindings {
	return TypeTextBindings{
		texts: bindings.texts.fork()
		ids:   bindings.ids.clone()
	}
}

// fork copies the private lookup cache. All spellings use process-wide identities.
pub fn (table &TypeTextTable) fork() &TypeTextTable {
	return &TypeTextTable{
		recent:       table.recent
		recent_texts: table.recent_texts
	}
}
