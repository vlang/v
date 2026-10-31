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

// type_recent_slot mixes the field spelling into the slot, so lookups of
// several fields of one type do not evict each other.
@[inline]
fn type_recent_slot(id u32, field string) int {
	if field.len == 0 {
		return int(id & 1023)
	}
	mix := id ^ (u32(field.len) << 3) ^ (u32(unsafe { field.str[0] }) << 5) ^ (u32(unsafe { field.str[field.len - 1] }) << 1)
	return int((mix ^ (mix >> 10)) & 1023)
}

fn (mut cache TypeRecentCache) get(module_name string, id u32, field string) (i8, u32) {
	slot := type_recent_slot(id, field)
	if cache.states[slot] != 0 && cache.types[slot] == id
		&& cache.modules[slot] == module_name && cache.fields[slot] == field {
		return cache.states[slot], cache.results[slot]
	}
	return 0, 0
}

fn (mut cache TypeRecentCache) put(module_name string, id u32, field string, state i8, result u32) {
	slot := type_recent_slot(id, field)
	// Keep the owned spellings when the slot already holds equal ones.
	if cache.modules[slot] != module_name {
		cache.modules[slot] = module_name.clone()
	}
	if cache.fields[slot] != field {
		cache.fields[slot] = field.clone()
	}
	cache.types[slot] = id
	cache.results[slot] = result
	cache.states[slot] = state
}

// field_type_cache_miss marks a cached failed field lookup in FieldTypeModuleCache.
const field_type_cache_miss = u32(0xffffffff)

// FieldTypeModuleCache holds the field lookups made from one module, keyed by
// the owner source type ID (high half) and the field name ID (low half).
struct FieldTypeModuleCache {
mut:
	entries map[u64]u32 // field_type_cache_miss for a known miss
}

// FieldTypeCache combines a source type ID with a module/field lookup context.
// The context contains declaration symbols; type spellings stay numeric.
struct FieldTypeCache {
mut:
	contexts flat.TypeTextTable
	recent   &TypeRecentCache = unsafe { nil }
	// module_name owns the spelling of the module whose lookups module_entries holds.
	module_name    string
	module_entries &FieldTypeModuleCache = unsafe { nil }
	modules        map[string]&FieldTypeModuleCache
}

// module_cache returns the lookups made from `module_name`, switching the
// current module only when the caller's module changes.
fn (mut cache FieldTypeCache) module_cache(module_name string) &FieldTypeModuleCache {
	if !isnil(cache.module_entries) && cache.module_name == module_name {
		return cache.module_entries
	}
	mut entries := cache.modules[module_name] or {
		created := &FieldTypeModuleCache{}
		cache.modules[module_name] = created
		created
	}
	cache.module_name = module_name.clone()
	cache.module_entries = entries
	return entries
}

fn (mut cache FieldTypeCache) recent_cache() &TypeRecentCache {
	if isnil(cache.recent) {
		cache.recent = &TypeRecentCache{}
	}
	return cache.recent
}
