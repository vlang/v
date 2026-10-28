@[has_globals]
module flat

import hash

// Source spellings have process-wide identities, like declaration payloads.
// Immutable chunks let nodes carry their IDs across worker and arena boundaries.
// Hash slots publish immutable entries; only insertion takes the writer lock.
const source_type_chunk_bits = 10
const source_type_chunk_size = 1 << source_type_chunk_bits
const source_type_chunk_count = 16384
const source_type_read_cache_size = 4096

// Builtin spellings are interned once; hot paths carry their numeric identities.
pub const source_type_void = source_type_id('void')
pub const source_type_unknown = source_type_id('unknown')
pub const source_type_generic = source_type_id('generic')
pub const source_type_nil = source_type_id('nil')
pub const source_type_none = source_type_id('none')
pub const source_type_string = source_type_id('string')
pub const source_type_char = source_type_id('char')
pub const source_type_rune = source_type_id('rune')
pub const source_type_isize = source_type_id('isize')
pub const source_type_usize = source_type_id('usize')
pub const source_type_uint = source_type_id('uint')
pub const source_type_voidptr = source_type_id('voidptr')
pub const source_type_byteptr = source_type_id('byteptr')
pub const source_type_charptr = source_type_id('charptr')
pub const source_type_bool = source_type_id('bool')
pub const source_type_int = source_type_id('int')
pub const source_type_i8 = source_type_id('i8')
pub const source_type_i16 = source_type_id('i16')
pub const source_type_i32 = source_type_id('i32')
pub const source_type_i64 = source_type_id('i64')
pub const source_type_u8 = source_type_id('u8')
pub const source_type_u16 = source_type_id('u16')
pub const source_type_u32 = source_type_id('u32')
pub const source_type_u64 = source_type_id('u64')
pub const source_type_f32 = source_type_id('f32')
pub const source_type_f64 = source_type_id('f64')

struct SourceTypeEntry {
	bytes &u8
	len   int
	hash  u32
	shape SourceTypeShape
	id    u32
	query SourceTypeQuery
}

// The bytes are process-owned; the borrowed string needs no stored ownership flag.
@[inline]
fn (entry &SourceTypeEntry) text() string {
	return unsafe { string{ str: entry.bytes, len: entry.len } }
}

// SourceTypeShape records spelling syntax once when a type is interned.
@[flag]
pub enum SourceTypeShape {
	dotted
	bracketed
	wrapped
	typeof_name
	typeof_expr
	builtin_alias
	optional
	pointer
	shared_type
	variadic
	padded
	array_type
	map_type
	leading_bracket
	option_type
	result_type
	mutable_type
	atomic_type
	channel_type
	thread_type
	function_compact
	function_spaced
	pointer_array
	pointer_map
	pointer_option
	pointer_chain
	bare_generic
	anonymous_struct
	generic_c_name
	context_independent
	unusable_decl
	shared_parameter_candidate
}

fn source_type_spelling_shape(text string) SourceTypeShape {
	mut shape := unsafe { SourceTypeShape(0) } // The empty set has no syntax flags.
	if text.contains('.') { shape.set(.dotted) }
	if text.contains('[') { shape.set(.bracketed) }
	if text.starts_with('&') || text.starts_with('?') || text.starts_with('!') {
		shape.set(.wrapped)
	}
	if text.starts_with('?') || text.starts_with('!') { shape.set(.optional) }
	if text.starts_with('&') { shape.set(.pointer) }
	if text.starts_with('shared ') { shape.set(.shared_type) }
	if text.starts_with('...') { shape.set(.variadic) }
	if text.starts_with('[]') { shape.set(.array_type) }
	if text.starts_with('map[') { shape.set(.map_type) }
	if text.starts_with('[') { shape.set(.leading_bracket) }
	if text.starts_with('?') { shape.set(.option_type) }
	if text.starts_with('!') { shape.set(.result_type) }
	if text.starts_with('mut ') { shape.set(.mutable_type) }
	if text.starts_with('atomic ') { shape.set(.atomic_type) }
	if text.starts_with('chan ') { shape.set(.channel_type) }
	if text.starts_with('thread ') { shape.set(.thread_type) }
	if text.starts_with('fn(') { shape.set(.function_compact) }
	if text.starts_with('fn (') { shape.set(.function_spaced) }
	if text.len > 0 && text[0] in [`s`, ` `, `\t`, `\n`, `\r`] {
		shape.set(.shared_parameter_candidate)
	}
	if text.starts_with('&[]') { shape.set(.pointer_array) }
	if text.starts_with('&map[') { shape.set(.pointer_map) }
	if text.starts_with('&?') { shape.set(.pointer_option) }
	if text.starts_with('&&') { shape.set(.pointer_chain) }
	if text.len > 0 && (source_type_space(text[0]) || source_type_space(text[text.len - 1])) {
		shape.set(.padded)
	}
	if text.contains('typeof') { shape.set(.typeof_name) }
	if text.len >= 8 && text.contains('typeof(') { shape.set(.typeof_expr) }
	if text in ['bool', 'string', 'void', 'int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32',
		'u64', 'f32', 'f64', 'rune', 'uint', 'isize', 'usize', 'voidptr', 'byteptr', 'charptr'] {
		shape.set(.builtin_alias)
	}
	if text == 'struct' || text.ends_with('.struct') { shape.set(.anonymous_struct) }
	if text.contains('_T_') { shape.set(.generic_c_name) }
	if source_type_spelling_context_independent(text) { shape.set(.context_independent) }
	if source_type_spelling_unusable_decl(text, shape) { shape.set(.unusable_decl) }
	mut start := 0
	mut end := text.len
	for start < end && source_type_space(text[start]) { start++ }
	for end > start && source_type_space(text[end - 1]) { end-- }
	if end - start == 1 && text[start] >= `A` && text[start] <= `Z` && text[start] != `C` {
		shape.set(.bare_generic)
	}
	return shape
}

// Classify plain raw names together: these have no wrappers, padding, or
// delimiters. All other spellings retain the complete legacy classifiers.
@[direct_array_access]
fn source_type_spelling_flags(text string) (SourceTypeShape, SourceTypeQuery) {
	mut shape := unsafe { SourceTypeShape(0) }
	mut query := unsafe { SourceTypeQuery(0) }
	if text.len == 0 {
		shape = source_type_spelling_shape(text)
		return shape, source_type_spelling_query(text, shape)
	}
	for i, c in text {
		if c == `.` {
			shape.set(.dotted)
		} else if !source_type_query_ident(c) {
			shape = source_type_spelling_shape(text)
			return shape, source_type_spelling_query(text, shape)
		}
		if c >= `A` && c <= `Z`
			&& (i == 0 || !source_type_query_ident(text[i - 1]))
			&& (i + 1 == text.len || !source_type_query_ident(text[i + 1])) {
			query.set(.generic_token)
		}
	}
	query.set(.complete)
	builtin := match text.len {
		2 { text in ['i8', 'u8'] }
		3 { text in ['int', 'i16', 'i32', 'i64', 'u16', 'u32', 'u64', 'f32', 'f64'] }
		4 { text in ['bool', 'void', 'rune', 'uint'] }
		5 { text in ['isize', 'usize'] }
		6 { text == 'string' }
		7 { text in ['voidptr', 'byteptr', 'charptr'] }
		else { false }
	}
	if builtin {
		shape.set(.builtin_alias)
	} else {
		query.set(.struct_candidate)
	}
	if builtin || text in ['char', 'nil', 'none', 'i128', 'u128'] {
		shape.set(.context_independent)
	}
	if text.contains('typeof') { shape.set(.typeof_name) }
	if text.contains('_T_') { shape.set(.generic_c_name) }
	if text.contains('generic') { query.set(.generic_word) }
	unknown := text.contains('unknown')
	if unknown { query.set(.unknown_fragment) }
	if unknown || text in ['array', 'map', 'struct']
		|| (text.starts_with('Option') && (text.len == 'Option'.len || text['Option'.len] == `_`))
		|| (text.starts_with('__v_option') && (text.len == '__v_option'.len
			|| text['__v_option'.len] == `_`))
		|| (text.starts_with('Result') && (text.len == 'Result'.len || text['Result'.len] == `_`)) {
		shape.set(.unusable_decl)
	}
	if text.contains('Arc_') { query.set(.map_arc_key) }
	last := text.len - 1
	if (text.len == 1 || text[last - 1] == `.`) {
		if text[last] >= `A` && text[last] <= `Z` { query.set(.generic_fn_name) }
		if text[last] in [`T`, `U`, `V`, `K`] { query.set(.generic_name) }
	}
	if text.len == 1 && text[0] >= `A` && text[0] <= `Z` && text[0] != `C` {
		shape.set(.bare_generic)
	}
	if text == 'chan' { query.set(.channel) }
	if text == 'struct' || text.ends_with('.struct') { shape.set(.anonymous_struct) }
	if text.ends_with('.thread') { query.set(.thread_suffix) }
	if text.ends_with('_Ctx') { query.set(.ctx_suffix) }
	match text[0] {
		`.` {
			if text.starts_with('...') { shape.set(.variadic) }
		}
		`s` { shape.set(.shared_parameter_candidate) }
		`C` {
			if text.starts_with('C.') { query.set(.c_prefix) }
		}
		`m` {
			if text.starts_with('main.') { query.set(.main_prefix) }
		}
		`A` {
			if text.starts_with('Array_') { query.set(.c_array_prefix) }
			if text.starts_with('AnonStruct_') { query.set(.anon_struct_prefix) }
			if text.starts_with('AnonUnion_') { query.set(.anon_union_prefix) }
		}
		`F` {
			if text.starts_with('FixedArray_') { query.set(.c_fixed_array_prefix) }
		}
		`M` {
			if text.starts_with('Map_') { query.set(.c_map_prefix) }
		}
		`t` {
			if text.starts_with('thread') { query.set(.thread_prefix) }
		}
		`_` {
			if text.starts_with('__vf_thread_') { query.set(.c_thread_prefix) }
		}
		`J` {
			if text.starts_with('JS.') { query.set(.js_prefix) }
		}
		else {}
	}
	return shape, query
}

// Declaration fallback rejects these provisional spellings before alias resolution.
// Only ASCII spaces are removed from wrapper names; raw-name checks stay exact.
fn source_type_spelling_unusable_decl(text string, shape SourceTypeShape) bool {
	if text in ['unknown', 'array', 'map', 'struct'] || text.contains('unknown')
		|| shape.has(.typeof_expr) {
		return true
	}
	mut pos := 0
	for pos < text.len && text[pos] == ` ` {
		pos++
	}
	if pos == text.len {
		return false
	}
	token := match text[pos] {
		`O` { 'Option' }
		`_` { '__v_option' }
		`R` { 'Result' }
		else { return false }
	}
	for c in token {
		for pos < text.len && text[pos] == ` ` {
			pos++
		}
		if pos == text.len || text[pos] != c {
			return false
		}
		pos++
	}
	for pos < text.len && text[pos] == ` ` {
		pos++
	}
	return pos == text.len || text[pos] == `_`
}

fn source_type_space(c u8) bool {
	return c == ` ` || c == `\n` || c == `\t` || c == `\v` || c == `\f` || c == `\r`
}

// Resolution can bypass declaration imports only for builtin leaves and these
// exact wrappers. Do not trim: callers decide whether padded spellings are valid.
@[manualfree]
fn source_type_spelling_context_independent(text string) bool {
	if text in ['', '!', '?'] {
		return true
	}
	mut start := 0
	for start < text.len {
		if text[start] in [`&`, `?`, `!`] {
			start++
		} else if text[start] == `[` && start + 1 < text.len && text[start + 1] == `]` {
			start += 2
		} else if text[start] == `[` {
			mut end := start + 1
			for end < text.len && text[end] >= `0` && text[end] <= `9` {
				end++
			}
			if end == start + 1 || end >= text.len || text[end] != `]` {
				return false
			}
			start = end + 1
		} else if start + 4 < text.len && text[start] == `m` && text[start + 1] == `a`
			&& text[start + 2] == `p` && text[start + 3] == `[` {
			mut end := start + 4
			mut depth := 1
			for end < text.len {
				if text[end] == `[` { depth++ }
				if text[end] == `]` {
					depth--
					if depth == 0 { break }
				}
				end++
			}
			if end >= text.len {
				return false
			}
			key := unsafe { text.substr_unsafe(start + 4, end) }
			if !source_type_spelling_context_independent(key) {
				return false
			}
			start = end + 1
		} else {
			break
		}
	}
	if text.len - start > 7 {
		return false
	}
	leaf := unsafe { text.substr_unsafe(start, text.len) }
	// Keep the builtin leaves accepted by types.is_builtin_type_name, excluding
	// the open container names `map` and `array`, which still require resolution.
	return match leaf.len {
		2 { leaf in ['i8', 'u8'] }
		3 { leaf in ['int', 'i16', 'i32', 'i64', 'u16', 'u32', 'u64', 'f32', 'f64', 'nil'] }
		4 { leaf in ['bool', 'char', 'i128', 'rune', 'u128', 'uint', 'void', 'none'] }
		5 { leaf in ['isize', 'usize'] }
		6 { leaf == 'string' }
		7 { leaf in ['voidptr', 'charptr', 'byteptr'] }
		else { false }
	}
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

// A whole program uses a few hundred thousand spellings; a small direct-mapped
// cache keeps evicting entries that are about to be asked for again.
const source_type_recent_slots = 131072

__global g_source_type_recent_entries [source_type_recent_slots]voidptr
__global g_source_type_read_entries [source_type_read_cache_size]voidptr

fn source_type_lock() {
	for {
		mut expected := u32(0)
		if C.atomic_compare_exchange_strong_u32(&g_source_types_lock, &expected, 1) {
			return
		}
		// Readers need no lock; contending writers only retry once it is free.
		for C.atomic_load_u32(&g_source_types_lock) != 0 {
			C.cpu_relax()
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
	chunk := payload_ptr_get(voidptr(table), uint(idx >> source_type_chunk_bits))
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
		mut slot := entry.hash & (capacity - 1)
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

fn source_type_find(table &SourceTypeTable, index &SourceTypeIndex, text string, hashed u32) u32 {
	if isnil(index) {
		return 0
	}
	mut slot := hashed & (index.capacity - 1)
	for {
		// An acquired slot makes the entry's bytes and chunk pointer visible.
		id := unsafe { C.atomic_load_u32(&index.slots[slot]) }
		if id == 0 {
			return 0
		}
		entry := source_type_entry(table, id)
		if entry.hash == hashed && entry.text() == text {
			return id
		}
		slot = (slot + 1) & (index.capacity - 1)
	}
}

// source_type_existing_id returns an interned spelling ID without inserting it.
// The empty spelling is always present as zero. Borrowed input bytes are not retained.
@[direct_array_access; inline]
pub fn source_type_existing_id(text string) ?u32 {
	if text.len == 0 {
		return u32(0)
	}
	slot := source_type_recent_slot(text)
	cached := source_type_recent_entry(slot)
	if !isnil(cached) && cached.text() == text {
		return cached.id
	}
	table := source_type_table_load()
	if isnil(table) {
		return none
	}
	id := source_type_find(table, source_type_index_load(table), text, u32(hash.sum64_string(text,
		0)))
	if id == 0 {
		return none
	}
	entry := source_type_entry(table, id)
	$if x32 {
		C.atomic_store_u32(voidptr(&g_source_type_recent_entries[slot]), u32(voidptr(entry)))
	} $else {
		C.atomic_store_u64(voidptr(&g_source_type_recent_entries[slot]), u64(voidptr(entry)))
	}
	return id
}

// source_type_id interns an owned source spelling and returns its stable u32 ID.
// Zero denotes an empty spelling. IDs are spelling identities, not checked types:
// resolving an unqualified name still requires its module and import context.
// Existing spellings are read without taking the insertion lock.
@[direct_array_access; inline]
pub fn source_type_id(text string) u32 {
	if text.len == 0 {
		return 0
	}
	// Only immutable process-owned entries are cached. Inputs can be borrowed from
	// a disposable arena, so every hit still checks the canonical spelling.
	slot := source_type_recent_slot(text)
	cached := source_type_recent_entry(slot)
	if !isnil(cached) && cached.text() == text {
		return cached.id
	}
	id := source_type_id_uncached(text)
	entry := source_type_entry(source_type_table_load(), id)
	$if x32 {
		C.atomic_store_u32(voidptr(&g_source_type_recent_entries[slot]), u32(voidptr(entry)))
	} $else {
		C.atomic_store_u64(voidptr(&g_source_type_recent_entries[slot]), u64(voidptr(entry)))
	}
	return id
}

@[direct_array_access; inline]
fn source_type_recent_entry(slot int) &SourceTypeEntry {
	$if x32 {
		return unsafe { &SourceTypeEntry(voidptr(C.atomic_load_u32(voidptr(&g_source_type_recent_entries[slot])))) }
	} $else {
		return unsafe { &SourceTypeEntry(voidptr(C.atomic_load_u64(voidptr(&g_source_type_recent_entries[slot])))) }
	}
}

@[direct_array_access; inline]
fn source_type_recent_slot(text string) int {
	// Equal spellings share a slot even when callers borrow different buffers.
	// The complete canonical spelling is still checked before accepting a hit.
	mut key := u64(text.len)
	if text.len >= 8 {
		mut first := u64(0)
		mut last := u64(0)
		// Word copies stay within the borrowed slice, including unaligned views.
		unsafe {
			vmemcpy(&first, text.str, 8)
			vmemcpy(&last, text.str + text.len - 8, 8)
		}
		key ^= first ^ (last * u64(0x9e3779b97f4a7c15))
	} else {
		key ^= (u64(text[0]) << 8) ^ (u64(text[text.len >> 1]) << 16) ^ (u64(text[text.len - 1]) << 24)
	}
	key = (key ^ (key >> 32)) * u64(0x9e3779b97f4a7c15)
	return int((key >> 32) & u64(source_type_recent_slots - 1))
}

fn source_type_id_uncached(text string) u32 {
	if text.len == 0 {
		return 0
	}
	// The fingerprint filters candidates; complete spelling equality resolves collisions.
	hashed := u32(hash.sum64_string(text, 0))
	mut table := source_type_table_load()
	if !isnil(table) {
		id := source_type_find(table, source_type_index_load(table), text, hashed)
		if id != 0 {
			return id
		}
	}
	// Spelling classification is independent of the shared table. Keep it out
	// of the insertion critical section so other writers can publish meanwhile.
	shape, query := source_type_spelling_flags(text)
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
	mut slot := hashed & (index.capacity - 1)
	for {
		id := unsafe { C.atomic_load_u32(&index.slots[slot]) }
		if id == 0 {
			break
		}
		entry := source_type_entry(table, id)
		if entry.hash == hashed && entry.text() == text {
			return id
		}
		slot = (slot + 1) & (index.capacity - 1)
	}
	idx := table.count
	chunk_idx := idx >> source_type_chunk_bits
	if chunk_idx >= source_type_chunk_count {
		panic('too many source type spellings')
	}
	mut chunk := payload_ptr_get(voidptr(table), uint(chunk_idx))
	if isnil(chunk) {
		chunk = node_payload_alloc_zeroed(source_type_chunk_size * sizeof(SourceTypeEntry))
		if isnil(chunk) {
			panic('could not allocate a source type chunk')
		}
		payload_ptr_set(voidptr(table), uint(chunk_idx), chunk)
	}
	bytes := node_payload_alloc_zeroed(uint(text.len) + 1)
	if isnil(bytes) {
		panic('could not allocate a source type spelling')
	}
	id := idx + 1
	// The reserved entry is private until the release store publishes its ID.
	unsafe {
		vmemcpy(bytes, text.str, text.len)
		entry := &SourceTypeEntry(chunk) + (idx & (source_type_chunk_size - 1))
		*entry = SourceTypeEntry{
			bytes: &u8(bytes)
			len:   text.len
			hash:  hashed
			shape: shape
			id:    id
			query: query
		}
	}
	C.atomic_store_u32(&table.count, id)
	unsafe { C.atomic_store_u32(&index.slots[slot], id) }
	return id
}

// source_type_read_entry caches immutable process-owned entries by their published ID.
@[direct_array_access; inline]
fn source_type_read_entry(id u32) &SourceTypeEntry {
	slot := int(id & u32(source_type_read_cache_size - 1))
	cached := $if x32 {
		unsafe { &SourceTypeEntry(voidptr(C.atomic_load_u32(voidptr(&g_source_type_read_entries[slot])))) }
	} $else {
		unsafe { &SourceTypeEntry(voidptr(C.atomic_load_u64(voidptr(&g_source_type_read_entries[slot])))) }
	}
	if !isnil(cached) && cached.id == id {
		return cached
	}
	return source_type_read_entry_miss(id, slot)
}

// Keep publication validation and cache insertion out of the hot lookup body.
@[direct_array_access; noinline]
fn source_type_read_entry_miss(id u32, slot int) &SourceTypeEntry {
	table := source_type_table_load()
	if isnil(table) || id > C.atomic_load_u32(&table.count) {
		panic('invalid source type ID')
	}
	entry := source_type_entry(table, id)
	// Only a checked published ID reaches this store. Entries never move or expire,
	// so a matching ID remains valid across index growth and disposable worker arenas.
	$if x32 {
		C.atomic_store_u32(voidptr(&g_source_type_read_entries[slot]), u32(voidptr(entry)))
	} $else {
		C.atomic_store_u64(voidptr(&g_source_type_read_entries[slot]), u64(voidptr(entry)))
	}
	return entry
}

// source_type_name resolves a source spelling ID at a text boundary.
@[inline]
pub fn source_type_name(id u32) string {
	if id == 0 {
		return ''
	}
	return source_type_read_entry(id).text()
}

// source_type_shape returns syntax properties without resolving spelling bytes.
@[inline]
pub fn source_type_shape(id u32) SourceTypeShape {
	if id == 0 { return unsafe { SourceTypeShape(0) } }
	return source_type_read_entry(id).shape
}

// source_type_query returns context-free query results without decoding a spelling.
@[inline]
pub fn source_type_query(id u32) SourceTypeQuery {
	if id == 0 { return .complete }
	return source_type_read_entry(id).query
}
