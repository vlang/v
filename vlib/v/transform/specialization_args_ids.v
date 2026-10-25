module transform

import v.flat

// SpecializationArgsKey distinguishes source applications from actual declaration
// and generated C symbols. The same bytes still share the former first-write slot.
struct SpecializationArgsKey {
	type_id u32
	name    string
}

struct SpecializationArgsBindings {
mut:
	names map[string][]u32
	types map[u32][]u32
	// Callee lookup may present the exact bytes of a source application. Buckets
	// retain only IDs; full byte equality resolves collisions at that boundary.
	type_text_buckets map[u64][]u32
	order             []SpecializationArgsKey
	len               int
}

fn specialization_args_text_hash(text string) u64 {
	mut hash := u64(14695981039346656037)
	for c in text.bytes() { hash = (hash ^ u64(c)) * u64(1099511628211) }
	return hash
}

fn (bindings &SpecializationArgsBindings) source_key_for_name(name string) u32 {
	bucket := bindings.type_text_buckets[specialization_args_text_hash(name)] or { return 0 }
	for id in bucket {
		if flat.source_type_name(id) == name { return id }
	}
	return 0
}

fn (bindings &SpecializationArgsBindings) type_ids(name string) ?[]u32 {
	if ids := bindings.names[name] { return ids }
	type_id := bindings.source_key_for_name(name)
	if type_id != 0 { return bindings.types[type_id] or { return none } }
	return none
}

fn (bindings &SpecializationArgsBindings) source_ids(type_id u32) ?[]u32 {
	if ids := bindings.types[type_id] { return ids }
	// A generated declaration can have the same spelling as the requested type.
	return bindings.names[flat.source_type_name(type_id)] or { return none }
}

fn (bindings &SpecializationArgsBindings) key_ids(key SpecializationArgsKey) ?[]u32 {
	if key.type_id != 0 { return bindings.source_ids(key.type_id) }
	return bindings.type_ids(key.name)
}

fn (bindings &SpecializationArgsBindings) contains(name string) bool {
	return name in bindings.names || bindings.source_key_for_name(name) != 0
}

fn (bindings &SpecializationArgsBindings) contains_key(key SpecializationArgsKey) bool {
	if key.type_id != 0 {
		return key.type_id in bindings.types || flat.source_type_name(key.type_id) in bindings.names
	}
	return bindings.contains(key.name)
}

fn (mut bindings SpecializationArgsBindings) set_ids(name string, ids []u32) {
	if name in bindings.names {
		bindings.names[name] = ids.clone()
		return
	}
	type_id := bindings.source_key_for_name(name)
	if type_id != 0 {
		bindings.types[type_id] = ids.clone()
		return
	}
	owned_name := name.clone()
	bindings.names[owned_name] = ids.clone()
	bindings.order << SpecializationArgsKey{ name: owned_name }
	bindings.len++
}

fn (mut bindings SpecializationArgsBindings) set_source_ids(type_id u32, ids []u32) {
	if type_id in bindings.types {
		bindings.types[type_id] = ids.clone()
		return
	}
	name := flat.source_type_name(type_id)
	if name in bindings.names {
		bindings.names[name] = ids.clone()
		return
	}
	bindings.types[type_id] = ids.clone()
	bindings.type_text_buckets[specialization_args_text_hash(name)] << type_id
	bindings.order << SpecializationArgsKey{ type_id: type_id }
	bindings.len++
}

fn (mut bindings SpecializationArgsBindings) set_key_ids(key SpecializationArgsKey, ids []u32) {
	if key.type_id != 0 {
		bindings.set_source_ids(key.type_id, ids)
	} else {
		bindings.set_ids(key.name, ids)
	}
}

fn (bindings &SpecializationArgsBindings) keys() []SpecializationArgsKey {
	return bindings.order
}

fn (bindings &SpecializationArgsBindings) clone() SpecializationArgsBindings {
	mut result := SpecializationArgsBindings{}
	for key in bindings.order {
		ids := bindings.key_ids(key) or { continue }
		result.set_key_ids(key, ids)
	}
	return result
}
