module c

import v.flat

// TypeNameLookupCache retains source and C type spellings as stable identities.
// It owns its lookup state so parallel code generation can populate it independently.
@[heap]
struct TypeNameLookupCache {
mut:
	texts      flat.TypeTextTable
	entries    map[u32]u32
	last_name  u32
	last_value u32
	last_valid bool
}

fn (mut cache TypeNameLookupCache) get(name string) ?string {
	if cache.last_valid && flat.source_type_name(cache.last_name) == name {
		return flat.source_type_name(cache.last_value)
	}
	id := cache.texts.intern(name)
	if value := cache.entries[id] {
		cache.last_name = id
		cache.last_value = value
		cache.last_valid = true
		return flat.source_type_name(value)
	}
	return none
}

fn (mut cache TypeNameLookupCache) put(name string, value string) {
	id := cache.texts.intern(name)
	value_id := cache.texts.intern(value)
	cache.entries[id] = value_id
	cache.last_name = id
	cache.last_value = value_id
	cache.last_valid = true
}

fn scratch_type_name_lookup_cache(cache &TypeNameLookupCache) &TypeNameLookupCache {
	if isnil(cache) {
		return unsafe { nil }
	}
	return &TypeNameLookupCache{}
}
