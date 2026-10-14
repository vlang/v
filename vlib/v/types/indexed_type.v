module types

import v.flat

// semantic_type_id interns a semantic value in this checker view's type table.
// The resulting identity must only be used with this view or a view sharing its table.
pub fn (tc &TypeChecker) semantic_type_id(value Type) ?TypeId {
	if isnil(tc.type_interner) {
		return none
	}
	id, _ := tc.intern_type(value)
	return id
}

fn (tc &TypeChecker) indexed_type_cache() &IndexedTypeCache {
	mut cache := unsafe { tc.type_cache }
	if isnil(cache.indexed_types) || cache.indexed_types.owner != tc.type_interner {
		cache.indexed_types = &IndexedTypeCache{
			owner: tc.type_interner
		}
	}
	return cache.indexed_types
}

@[inline]
fn (tc &TypeChecker) invalidate_checked_type_id(idx int) {
	if idx < 0 || isnil(tc.type_cache) || isnil(tc.type_cache.indexed_types) {
		return
	}
	mut cache := unsafe { tc.type_cache.indexed_types }
	slot := idx & 4095
	if cache.set[slot] && cache.nodes[slot] == idx {
		cache.set[slot] = false
	}
}

// checked_expr_type_id returns the interned identity of a recorded expression type.
// The identity belongs to this checker view's interner; do not pass it to another view.
// It follows the visibility of checked node metadata, without resolving a new type.
@[direct_array_access]
pub fn (tc &TypeChecker) checked_expr_type_id(node flat.NodeId) ?TypeId {
	idx := int(node)
	if idx < 0 || isnil(tc.type_interner) {
		return none
	}
	dense := isnil(tc.storage_query_probe) && idx < tc.expr_type_set.len
		&& tc.expr_type_set[idx] && (!tc.parallel_check_sparse || tc.in_check_range(idx))
	if dense && !isnil(tc.type_cache) {
		cache := tc.indexed_type_cache()
		slot := idx & 4095
		if cache.set[slot] && cache.nodes[slot] == idx {
			return cache.types[slot]
		}
		typ := tc.expr_type_values[idx]
		id, _ := tc.intern_type(typ)
		mut writable := unsafe { &IndexedTypeCache(cache) }
		writable.nodes[slot] = idx
		writable.types[slot] = id
		writable.set[slot] = true
		return id
	}
	// Sparse/probe metadata may change as a query moves between scope views.
	// Leave those compatibility paths uncached until they carry their own IDs.
	typ := tc.cached_expr_type(node) or { return none }
	id, _ := tc.intern_type(typ)
	return id
}

// type_by_id returns the canonical semantic type for an identity in this checker view.
pub fn (tc &TypeChecker) type_by_id(id TypeId) ?Type {
	if isnil(tc.type_interner) {
		return none
	}
	mut interner := unsafe { tc.type_interner }
	interner.lock.rlock()
	defer { interner.lock.runlock() }
	if u32(id) >= u32(interner.types.len) {
		return none
	}
	return interner.types[int(id)]
}

// type_name_by_id returns the source spelling ID of an interned semantic type.
pub fn (tc &TypeChecker) type_name_by_id(id TypeId) u32 {
	if isnil(tc.type_interner) { return flat.source_type_id('unknown') }
	if isnil(tc.type_cache) {
		mut interner := unsafe { tc.type_interner }
		return interner.source_type_id(id)
	}
	mut cache := tc.indexed_type_cache()
	slot := int(u32(id) & 4095)
	if cache.name_set[slot] && cache.name_ids[slot] == id { return cache.names[slot] }
	mut interner := unsafe { tc.type_interner }
	name := interner.source_type_id(id)
	// Invalid IDs may become valid after growth; don't retain their placeholder.
	if name != flat.source_type_id('unknown') {
		cache.name_ids[slot] = id
		cache.names[slot] = name
		cache.name_set[slot] = true
	}
	return name
}

// c_type_by_id returns the C representation of an identity in this checker view.
// Cached representations are read by numeric identity without rehashing a Type value.
pub fn (tc &TypeChecker) c_type_by_id(id TypeId) ?string {
	if isnil(tc.type_interner) {
		return none
	}
	if cached := tc.cached_c_type_by_id(id) {
		return cached
	}
	canonical := tc.type_by_id(id) or { return none }
	return tc.canonical_c_type(id, canonical)
}

// parse_type_id resolves an interned source spelling to a semantic identity in this checker view.
// Source imports and generic bindings follow parse_type; builtin scalar IDs can
// be carried across source contexts without repeating contextual cache lookups.
pub fn (tc &TypeChecker) parse_type_id(text u32) ?TypeId {
	if isnil(tc.type_interner) {
		return none
	}
	value := tc.parse_type(text)
	if tc.type_param_texts.ids.len == 0 && !isnil(tc.type_cache)
		&& tc.type_cache.alias_parse_stack.len == 0 {
		cache := tc.type_cache.parsed_scalars
		if !isnil(cache) && cache.owner == tc.type_interner && text != 0 {
			slot := parsed_scalar_slot(text)
			if cache.texts[slot] == text {
				return cache.ids[slot]
			}
		}
	}
	id, _ := tc.intern_type(value)
	return id
}

@[inline]
fn scalar_builtin_type_name(text string) bool {
	return is_builtin_type_name(text) && text != 'array' && text != 'map'
}

@[direct_array_access; inline]
fn parsed_scalar_slot(source_id u32) int {
	return int(source_id & 127)
}

@[direct_array_access; inline]
fn (tc &TypeChecker) parsed_scalar_type(text u32) ?Type {
	cache := tc.type_cache.parsed_scalars
	if isnil(cache) || cache.owner != tc.type_interner || text == 0 {
		return none
	}
	slot := parsed_scalar_slot(text)
	if cache.texts[slot] == text {
		return cache.values[slot]
	}
	return none
}

// Only exact builtin spellings enter this cache. Their meanings do not depend
// on imports, the current module, resolution mode or function-local scopes.
fn (tc &TypeChecker) remember_parsed_scalar_type(text u32, id TypeId, value Type) {
	if isnil(tc.type_interner) {
		return
	}
	mut type_cache := unsafe { tc.type_cache }
	if isnil(type_cache.parsed_scalars) || type_cache.parsed_scalars.owner != tc.type_interner {
		type_cache.parsed_scalars = &ParsedScalarTypeCache{
			owner:  tc.type_interner
			// Unfilled slots are guarded by their nonempty text key.
			values: unsafe { []Type{len: 128} }
		}
	}
	mut cache := type_cache.parsed_scalars
	slot := parsed_scalar_slot(text)
	cache.texts[slot] = text
	cache.ids[slot] = id
	cache.values[slot] = value
}
