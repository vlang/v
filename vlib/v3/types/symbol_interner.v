module types

import sync

// SymbolId is the stable identity of a resolved declaration name in one
// compilation. Zero denotes no symbol.
pub type SymbolId = u32

@[heap]
struct SymbolInterner {
mut:
	lock  &sync.Mutex = unsafe { nil }
	names []string
	ids   map[string]SymbolId
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
	if id := i.ids[name] {
		return id, i.names[int(id) - 1]
	}
	canonical := name.clone()
	id := SymbolId(i.names.len + 1)
	i.names << canonical
	i.ids[canonical] = id
	return id, i.names.last()
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
		canonical := name.clone()
		names << canonical
		ids[canonical] = SymbolId(idx + 1)
	}
	i.names = names
	i.ids = ids.move()
}
