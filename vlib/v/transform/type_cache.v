module transform

import v.flat

// TypeRecentCache keeps source-type keys and results indexed between queries.
// Module and field names are symbols; their spelling is owned by the cache.
struct TypeRecentCache {
mut:
	texts   flat.TypeTextTable
	modules [1024]string
	fields  [1024]string
	types   [1024]u32
	results [1024]u32
	states  [1024]i8
}

fn (mut cache TypeRecentCache) get(module_name string, name string, field string) (i8, string) {
	id := cache.texts.intern(name)
	slot := int(id & 1023)
	if cache.states[slot] != 0 && cache.types[slot] == id
		&& cache.modules[slot] == module_name && cache.fields[slot] == field {
		return cache.states[slot], flat.source_type_name(cache.results[slot])
	}
	return 0, ''
}

fn (mut cache TypeRecentCache) put(module_name string, name string, field string, state i8, result string) {
	id := cache.texts.intern(name)
	slot := int(id & 1023)
	cache.modules[slot] = module_name.clone()
	cache.fields[slot] = field.clone()
	cache.types[slot] = id
	cache.results[slot] = cache.texts.intern(result)
	cache.states[slot] = state
}
