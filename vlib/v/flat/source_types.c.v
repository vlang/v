@[has_globals]
module flat

import hash

// Source spellings have process-wide identities, like declaration payloads.
// Immutable chunks let nodes carry their IDs across worker and arena boundaries.
// Hash slots publish immutable entries; only insertion takes the writer lock.
const source_type_chunk_bits = 10
const source_type_chunk_size = 1 << source_type_chunk_bits
const source_type_chunk_count = 16384

struct SourceTypeEntry {
	text  string
	hash  u64
	shape SourceTypeShape
}

// SourceTypeShape records spelling syntax once when a type is interned.
@[flag]
pub enum SourceTypeShape {
	dotted
	bracketed
	wrapped
	typeof_name
	typeof_expr
}

fn source_type_spelling_shape(text string) SourceTypeShape {
	mut shape := unsafe { SourceTypeShape(0) } // The empty set has no syntax flags.
	if text.contains('.') { shape.set(.dotted) }
	if text.contains('[') { shape.set(.bracketed) }
	if text.starts_with('&') || text.starts_with('?') || text.starts_with('!') {
		shape.set(.wrapped)
	}
	if text.contains('typeof') { shape.set(.typeof_name) }
	if text.contains('typeof(') { shape.set(.typeof_expr) }
	return shape
}

struct SourceTypeTable {
mut:
	chunks [source_type_chunk_count]voidptr
	count  u32
	index  &SourceTypeIndex = unsafe { nil }
}

struct SourceTypeIndex {
	slots    &u32 = unsafe { nil }
	capacity u32
}

__global g_source_types &SourceTypeTable
__global g_source_types_lock u32

fn source_type_lock() {
	for {
		mut expected := u32(0)
		if C.atomic_compare_exchange_strong_u32(&g_source_types_lock, &expected, 1) {
			return
		}
	}
}

@[inline]
fn source_type_table_load() &SourceTypeTable {
	$if x32 {
		return unsafe { &SourceTypeTable(voidptr(C.atomic_load_u32(voidptr(&g_source_types)))) }
	} $else {
		return unsafe { &SourceTypeTable(voidptr(C.atomic_load_u64(voidptr(&g_source_types)))) }
	}
}

fn source_type_table_publish(table &SourceTypeTable) {
	$if x32 {
		C.atomic_store_u32(voidptr(&g_source_types), u32(voidptr(table)))
	} $else {
		C.atomic_store_u64(voidptr(&g_source_types), u64(voidptr(table)))
	}
}

@[inline]
fn source_type_index_load(table &SourceTypeTable) &SourceTypeIndex {
	$if x32 {
		return unsafe { &SourceTypeIndex(voidptr(C.atomic_load_u32(voidptr(&table.index)))) }
	} $else {
		return unsafe { &SourceTypeIndex(voidptr(C.atomic_load_u64(voidptr(&table.index)))) }
	}
}

fn source_type_index_publish(table &SourceTypeTable, index &SourceTypeIndex) {
	$if x32 {
		C.atomic_store_u32(voidptr(&table.index), u32(voidptr(index)))
	} $else {
		C.atomic_store_u64(voidptr(&table.index), u64(voidptr(index)))
	}
}

@[inline]
fn source_type_entry(table &SourceTypeTable, id u32) &SourceTypeEntry {
	idx := id - 1
	chunk := payload_ptr_get(voidptr(table), usize(idx >> source_type_chunk_bits))
	// Published IDs address immutable entries in nonmoving chunks.
	return unsafe { &SourceTypeEntry(chunk) + (idx & (source_type_chunk_size - 1)) }
}

fn (mut table SourceTypeTable) grow() {
	previous := source_type_index_load(table)
	capacity := if isnil(previous) { u32(2048) } else { previous.capacity * 2 }
	// The hash index holds integers only; its storage belongs to the process,
	// independently of the calling worker's disposable allocation arena.
	slots := unsafe { &u32(C.calloc(capacity, sizeof(u32))) }
	if isnil(slots) {
		panic('could not allocate the source type index')
	}
	for id in u32(1) .. table.count + 1 {
		entry := source_type_entry(table, id)
		mut slot := u32(entry.hash) & (capacity - 1)
		unsafe {
			for slots[slot] != 0 {
				slot = (slot + 1) & (capacity - 1)
			}
			slots[slot] = id
		}
	}
	// A reader can still be probing any previous index. Keep old generations for
	// the process lifetime; geometric growth retains less than twice the latest
	// slot allocation. Neither the index nor its slots belong to a worker arena.
	mut index := unsafe { &SourceTypeIndex(node_payload_alloc_zeroed(sizeof(SourceTypeIndex))) }
	if isnil(index) {
		panic('could not allocate the source type index')
	}
	unsafe { *index = SourceTypeIndex{ slots: slots, capacity: capacity } }
	source_type_index_publish(table, index)
}

fn source_type_find(table &SourceTypeTable, index &SourceTypeIndex, text string, hashed u64) u32 {
	if isnil(index) {
		return 0
	}
	mut slot := u32(hashed) & (index.capacity - 1)
	for {
		// An acquired slot makes the entry's bytes and chunk pointer visible.
		id := unsafe { C.atomic_load_u32(&index.slots[slot]) }
		if id == 0 {
			return 0
		}
		entry := source_type_entry(table, id)
		if entry.hash == hashed && entry.text == text {
			return id
		}
		slot = (slot + 1) & (index.capacity - 1)
	}
}

// source_type_id interns an owned source spelling and returns its stable u32 ID.
// Zero denotes an empty spelling. IDs are spelling identities, not checked types:
// resolving an unqualified name still requires its module and import context.
// Existing spellings are read without taking the insertion lock.
pub fn source_type_id(text string) u32 {
	if text.len == 0 {
		return 0
	}
	hashed := hash.sum64_string(text, 0)
	mut table := source_type_table_load()
	if !isnil(table) {
		id := source_type_find(table, source_type_index_load(table), text, hashed)
		if id != 0 {
			return id
		}
	}
	source_type_lock()
	defer { C.atomic_store_u32(&g_source_types_lock, 0) }
	table = source_type_table_load()
	if isnil(table) {
		// Scanned, uncollectable storage keeps strings reachable with GC, and
		// raw storage in prealloc builds survives all disposable worker arenas.
		table = unsafe { &SourceTypeTable(node_payload_alloc_zeroed(sizeof(SourceTypeTable))) }
		if isnil(table) {
			panic('could not allocate the source type table')
		}
		source_type_table_publish(table)
	}
	mut index := source_type_index_load(table)
	if isnil(index) || (table.count + 1) * 4 >= index.capacity * 3 {
		table.grow()
		index = source_type_index_load(table)
	}
	// Another writer may have inserted the spelling, or grown the index, while
	// this caller waited for the lock. Probe the current generation again.
	mut slot := u32(hashed) & (index.capacity - 1)
	for {
		id := unsafe { C.atomic_load_u32(&index.slots[slot]) }
		if id == 0 {
			break
		}
		entry := source_type_entry(table, id)
		if entry.hash == hashed && entry.text == text {
			return id
		}
		slot = (slot + 1) & (index.capacity - 1)
	}
	idx := table.count
	chunk_idx := idx >> source_type_chunk_bits
	if chunk_idx >= source_type_chunk_count {
		panic('too many source type spellings')
	}
	mut chunk := payload_ptr_get(voidptr(table), usize(chunk_idx))
	if isnil(chunk) {
		chunk = node_payload_alloc_zeroed(source_type_chunk_size * sizeof(SourceTypeEntry))
		if isnil(chunk) {
			panic('could not allocate a source type chunk')
		}
		payload_ptr_set(voidptr(table), usize(chunk_idx), chunk)
	}
	bytes := node_payload_alloc_zeroed(usize(text.len) + 1)
	if isnil(bytes) {
		panic('could not allocate a source type spelling')
	}
	id := idx + 1
	// The reserved entry is private until the release store publishes its ID.
	unsafe {
		vmemcpy(bytes, text.str, text.len)
		entry := &SourceTypeEntry(chunk) + (idx & (source_type_chunk_size - 1))
		*entry = SourceTypeEntry{
			text:  tos(bytes, text.len)
			hash:  hashed
			shape: source_type_spelling_shape(text)
		}
	}
	C.atomic_store_u32(&table.count, id)
	unsafe { C.atomic_store_u32(&index.slots[slot], id) }
	return id
}

// source_type_name resolves a source spelling ID at a text boundary.
@[inline]
pub fn source_type_name(id u32) string {
	if id == 0 {
		return ''
	}
	table := source_type_table_load()
	if isnil(table) || id > C.atomic_load_u32(&table.count) {
		panic('invalid source type ID')
	}
	return source_type_entry(table, id).text
}

// source_type_shape returns syntax properties without resolving spelling bytes.
@[inline]
pub fn source_type_shape(id u32) SourceTypeShape {
	if id == 0 { return unsafe { SourceTypeShape(0) } }
	table := source_type_table_load()
	if isnil(table) || id > C.atomic_load_u32(&table.count) {
		panic('invalid source type ID')
	}
	return source_type_entry(table, id).shape
}
