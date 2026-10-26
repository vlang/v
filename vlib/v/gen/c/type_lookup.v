module c

// TypeNameLookupCache retains source and C type spellings as stable identities.
// It owns its lookup state so parallel code generation can populate it independently.
@[heap]
struct TypeNameLookupCache {
mut:
	entries    map[u32]u32
	last_name  u32
	last_value u32
	last_valid bool
}

fn scratch_type_name_lookup_cache(cache &TypeNameLookupCache) &TypeNameLookupCache {
	if isnil(cache) {
		return unsafe { nil }
	}
	return &TypeNameLookupCache{}
}

fn (mut cache TypeNameLookupCache) get_id(id u32) ?u32 {
	if cache.last_valid && cache.last_name == id { return cache.last_value }
	if value := cache.entries[id] {
		cache.last_name = id
		cache.last_value = value
		cache.last_valid = true
		return value
	}
	return none
}

fn (mut cache TypeNameLookupCache) put_id(id u32, value u32) {
	cache.entries[id] = value
	cache.last_name = id
	cache.last_value = value
	cache.last_valid = true
}
