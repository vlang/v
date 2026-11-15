module types

import sync

// SymbolId is the stable identity of a resolved declaration name in one
// compilation. Zero denotes no symbol.
pub type SymbolId = u32

@[heap]
struct SymbolInterner {
mut:
	lock   &sync.Mutex = unsafe { nil }
	names  []string
	ids    map[string]SymbolId
	recent [128]SymbolId
}

// symbol_recent_slot selects the recent slot of a non-empty name from its
// bytes: the same name arrives from many allocations (call keys are spelled
// per call), which a slot keyed by the pointer missed every time.
@[direct_array_access; inline]
fn symbol_recent_slot(name string) int {
	mut h := u32(name.len) * 0x9e3779b1 ^ u32(name[0]) * 0x85ebca6b
	h ^= u32(name[name.len - 1]) * 0xc2b2ae35 ^ u32(name[name.len / 2]) * 0x27d4eb2f
	return int((h ^ (h >> 15)) & 127)
}

fn new_symbol_interner() &SymbolInterner {
	return &SymbolInterner{
		lock: sync.new_mutex()
		ids:  map[string]SymbolId{}
	}
}

fn (mut i SymbolInterner) intern(name string) (SymbolId, string) {
	if name.len == 0 {
		return SymbolId(0), ''
	}
	i.lock.lock()
	defer {
		i.lock.unlock()
	}
	// Cache identities only: input storage can be a disposable or reused buffer.
	// Checking the canonical bytes makes pointer reuse and hash collisions safe.
	slot := symbol_recent_slot(name)
	recent := i.recent[slot]
	if recent != 0 {
		canonical := i.names[int(recent) - 1]
		if canonical.len == name.len && (canonical.str == name.str || canonical == name) {
			return recent, canonical
		}
	}
	if id := i.ids[name] {
		i.recent[slot] = id
		return id, i.names[int(id) - 1]
	}
	id := SymbolId(i.names.len + 1)
	i.names << name.clone()
	canonical := i.names.last()
	i.ids[canonical] = id
	i.recent[slot] = id
	return id, canonical
}

fn (mut i SymbolInterner) name(id SymbolId) string {
	idx := int(id) - 1
	i.lock.lock()
	defer {
		i.lock.unlock()
	}
	if idx < 0 || idx >= i.names.len {
		return ''
	}
	return i.names[idx]
}

fn (mut i SymbolInterner) len() int {
	i.lock.lock()
	defer {
		i.lock.unlock()
	}
	return i.names.len
}

fn (mut i SymbolInterner) reserve(headroom int) {
	if headroom <= 0 {
		return
	}
	unsafe { i.names.grow_cap(headroom) }
	i.ids.reserve(u32(i.ids.len + headroom))
}

fn (mut i SymbolInterner) promote_from(start int, scope voidptr) {
	i.lock.lock()
	defer {
		i.lock.unlock()
	}
	_ = start
	_ = scope
	// Forks may return an existing canonical spelling whose bytes came from a
	// retained worker arena rather than the outer scope. Rebuild the complete
	// table while every arena is still alive so both names and index storage are
	// owned by the caller's current compilation arena.
	mut names := []string{cap: i.names.len}
	mut ids := map[string]SymbolId{}
	for idx, name in i.names {
		names << name.clone()
		ids[names.last()] = SymbolId(idx + 1)
	}
	i.names = names
	i.ids = ids.move()
}
