module transform

import v.flat

// TypeRecentCache keeps source-type keys and results indexed between queries.
// Module and field names are symbols; their spelling is owned by the cache.
struct TypeRecentCache {
mut:
	modules [1024]string
	fields  [1024]string
	types   [1024]u32
	results [1024]u32
	states  [1024]i8
}

fn (mut cache TypeRecentCache) get(module_name string, id u32, field string) (i8, u32) {
	slot := int(id & 1023)
	if cache.states[slot] != 0 && cache.types[slot] == id
		&& cache.modules[slot] == module_name && cache.fields[slot] == field {
		return cache.states[slot], cache.results[slot]
	}
	return 0, 0
}

fn (mut cache TypeRecentCache) put(module_name string, id u32, field string, state i8, result u32) {
	slot := int(id & 1023)
	cache.modules[slot] = module_name.clone()
	cache.fields[slot] = field.clone()
	cache.types[slot] = id
	cache.results[slot] = result
	cache.states[slot] = state
}

// FieldTypeCache combines a source type ID with a module/field lookup context.
// The context contains declaration symbols; type spellings stay numeric.
struct FieldTypeCache {
mut:
	entries  map[u64]u32
	misses   map[u64]bool
	contexts flat.TypeTextTable
	recent   &TypeRecentCache = unsafe { nil }
}

fn (mut cache FieldTypeCache) recent_cache() &TypeRecentCache {
	if isnil(cache.recent) {
		cache.recent = &TypeRecentCache{}
	}
	return cache.recent
}
