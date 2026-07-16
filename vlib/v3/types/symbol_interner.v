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
	names_backing_scoped := scoped_transform_owns(scope, i.names.data)
	first := if start < 0 { 0 } else { start }
	for idx in first .. i.names.len {
		name := i.names[idx]
		if name.len == 0 || !scoped_transform_owns(scope, name.str) {
			continue
		}
		i.ids.delete(name)
		canonical := name.clone()
		i.names[idx] = canonical
		i.ids[canonical] = SymbolId(idx + 1)
	}
	if names_backing_scoped {
		i.names = i.names.clone()
	}
	// Keep the hash index itself out of the disposable transform arena too;
	// an insertion may have rehashed it despite the pre-transform reserve.
	i.ids = i.ids.clone()
}
