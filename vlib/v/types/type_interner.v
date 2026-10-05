module types

import sync

// TypeId is the stable identity of a canonical semantic type in one
// compilation. Type values remain the public compatibility representation;
// caches and equality-heavy internals can use this compact identity.
pub type TypeId = u32

@[heap]
struct TypeInterner {
mut:
	lock    &sync.RwMutex = unsafe { nil }
	types   []&Type
	names   []string
	buckets map[u64]TypeId
}

fn new_type_interner() &TypeInterner {
	return &TypeInterner{
		lock:    sync.new_rwmutex()
		buckets: map[u64]TypeId{}
	}
}

fn (mut i TypeInterner) intern_locked(t &Type, hash u64) (TypeId, &Type) {
	mut key := hash
	for {
		if id := i.buckets[key] {
			if int(id) < 0 || int(id) >= i.types.len {
				panic('corrupt semantic type interner: id ${id} outside ${i.types.len} types')
			}
			candidate := i.types[int(id)]
			if semantic_types_equal(candidate, t) {
				return id, candidate
			}
			key = type_hash_tag(key, 0x5bd1e995)
			continue
		}
		break
	}
	owned := i.own_value(t)
	for {
		if key !in i.buckets {
			break
		}
		key = type_hash_tag(key, 0x5bd1e995)
	}
	id := TypeId(i.types.len)
	i.types << &Type(owned)
	i.names << ''
	i.buckets[key] = id
	return id, i.types[int(id)]
}

fn (mut i TypeInterner) own_value(t &Type) Type {
	return match t {
		Array {
			_, element := i.intern_locked(t.elem_type, semantic_type_hash(t.elem_type))
			Type(Array{ elem_type: element })
		}
		ArrayFixed {
			_, element := i.intern_locked(t.elem_type, semantic_type_hash(t.elem_type))
			Type(ArrayFixed{
				elem_type: element
				len:       t.len
				len_expr:  t.len_expr.clone()
			})
		}
		Channel {
			_, element := i.intern_locked(t.elem_type, semantic_type_hash(t.elem_type))
			Type(Channel{ elem_type: element, is_mut: t.is_mut })
		}
		Map {
			_, key := i.intern_locked(t.key_type, semantic_type_hash(t.key_type))
			_, value := i.intern_locked(t.value_type, semantic_type_hash(t.value_type))
			Type(Map{ key_type: key, value_type: value })
		}
		Pointer {
			_, base := i.intern_locked(t.base_type, semantic_type_hash(t.base_type))
			Type(Pointer{ base_type: base })
		}
		FnType {
			mut params := []FnParam{cap: t.params.len}
			for param in t.params {
				_, typ := i.intern_locked(param.typ, semantic_type_hash(param.typ))
				params << FnParam{ typ: typ, is_mut: param.is_mut }
			}
			_, result := i.intern_locked(t.return_type, semantic_type_hash(t.return_type))
			Type(FnType{ params: params, return_type: result })
		}
		OptionType {
			_, base := i.intern_locked(t.base_type, semantic_type_hash(t.base_type))
			Type(OptionType{ base_type: base })
		}
		ResultType {
			_, base := i.intern_locked(t.base_type, semantic_type_hash(t.base_type))
			Type(ResultType{ base_type: base })
		}
		Alias {
			_, base := i.intern_locked(t.base_type, semantic_type_hash(t.base_type))
			Type(Alias{ name: t.name.clone(), base_type: base })
		}
		MultiReturn {
			mut types := []Type{cap: t.types.len}
			for typ in t.types {
				_, value := i.intern_locked(typ, semantic_type_hash(typ))
				types << *value
			}
			Type(MultiReturn{ types: types })
		}
		else {
			clone_owned_type(t)
		}
	}
}

// probe returns the canonical copy when t is already interned. Readers must
// synchronize with table growth even when they do not insert a missing type.
fn (i &TypeInterner) probe(t &Type) ?&Type {
	// The semantic lookup is read-only; only its synchronization state is mutable.
	// Probes share a read lock, and interned types never change, so the hash
	// and the comparisons run outside it: parallel probes do not queue.
	mut guard := unsafe { i.lock }
	mut key := semantic_type_hash(t)
	for {
		guard.rlock()
		id := i.buckets[key] or {
			guard.runlock()
			return none
		}
		if int(id) < 0 || int(id) >= i.types.len {
			guard.runlock()
			return none
		}
		candidate := i.types[int(id)]
		guard.runlock()
		if semantic_types_equal(candidate, t) {
			return candidate
		}
		key = type_hash_tag(key, 0x5bd1e995)
	}
	return none
}

// probe_frozen requires an immutable table and immutable semantic payloads
// until every reader joins. Normal callers must use the synchronized probe.
fn (i &TypeInterner) probe_frozen(t &Type) ?&Type {
	mut key := semantic_type_hash(t)
	for {
		id := i.buckets[key] or { return none }
		if int(id) < 0 || int(id) >= i.types.len {
			return none
		}
		candidate := i.types[int(id)]
		if semantic_types_equal(candidate, t) {
			return candidate
		}
		key = type_hash_tag(key, 0x5bd1e995)
	}
	return none
}

fn (mut i TypeInterner) name(id TypeId) string {
	i.lock.lock()
	defer {
		i.lock.unlock()
	}
	if int(id) < 0 || int(id) >= i.types.len {
		return 'unknown'
	}
	if i.names[int(id)].len == 0 {
		i.names[int(id)] = i.types[int(id)].name()
	}
	return i.names[int(id)]
}

fn (mut i TypeInterner) canonicalize(t &Type) (TypeId, &Type) {
	hash := semantic_type_hash(t)
	i.lock.lock()
	defer {
		i.lock.unlock()
	}
	return i.intern_locked(t, hash)
}

fn (mut i TypeInterner) len() int {
	i.lock.lock()
	defer {
		i.lock.unlock()
	}
	return i.types.len
}

fn (mut i TypeInterner) reserve(headroom int) {
	if headroom <= 0 {
		return
	}
	i.lock.lock()
	defer { i.lock.unlock() }
	unsafe {
		i.types.grow_cap(headroom)
		i.names.grow_cap(headroom)
	}
	i.buckets.reserve(u32(i.buckets.len + headroom))
}

fn (mut i TypeInterner) promote_from(start int, scope voidptr) {
	i.lock.lock()
	defer {
		i.lock.unlock()
	}
	_ = start
	_ = scope
	// Canonical types can contain strings originating in retained worker arenas,
	// not only additions owned by the outer transform scope. Deep-copy the full
	// stable-id table before any of those arenas are released.
	previous := i.types
	i.types = []&Type{cap: previous.len}
	for typ in previous {
		owned := i.own_value(typ)
		i.types << &Type(owned)
	}
	mut owned_names := []string{cap: i.names.len}
	for name in i.names {
		owned_names << name.clone()
	}
	i.names = owned_names
	// A scoped insertion can rehash even after the caller reserves headroom.
	// Rebuild the index after leaving the scope so its backing storage cannot
	// remain owned by the disposable transform arena.
	i.buckets = i.buckets.clone()
}

pub fn semantic_type_hash(t &Type) u64 {
	mut hash := u64(14_695_981_039_346_656_037)
	match t {
		Void {
			return type_hash_tag(hash, 1)
		}
		Unknown {
			hash = type_hash_tag(hash, 2)
			return type_hash_string(hash, t.reason)
		}
		Primitive {
			hash = type_hash_tag(hash, 3)
			hash = type_hash_tag(hash, int(t.props))
			return type_hash_tag(hash, int(t.size))
		}
		String {
			return type_hash_tag(hash, 4)
		}
		Char {
			return type_hash_tag(hash, 5)
		}
		Rune {
			return type_hash_tag(hash, 6)
		}
		ISize {
			return type_hash_tag(hash, 7)
		}
		USize {
			return type_hash_tag(hash, 8)
		}
		Nil {
			return type_hash_tag(hash, 9)
		}
		None {
			return type_hash_tag(hash, 10)
		}
		Array {
			hash = type_hash_tag(hash, 11)
			return type_hash_child(hash, t.elem_type)
		}
		ArrayFixed {
			hash = type_hash_tag(hash, 12)
			hash = type_hash_child(hash, t.elem_type)
			hash = type_hash_tag(hash, t.len)
			return type_hash_string(hash, t.len_expr)
		}
		Channel {
			hash = type_hash_tag(hash, 13)
			return type_hash_child(hash, t.elem_type)
		}
		Map {
			hash = type_hash_tag(hash, 14)
			hash = type_hash_child(hash, t.key_type)
			return type_hash_child(hash, t.value_type)
		}
		Pointer {
			hash = type_hash_tag(hash, 15)
			return type_hash_child(hash, t.base_type)
		}
		FnType {
			hash = type_hash_tag(hash, 16)
			hash = type_hash_tag(hash, t.params.len)
			for idx in 0 .. t.params.len {
				hash = type_hash_tag(hash, int(t.params[idx].is_mut))
				hash = type_hash_child(hash, t.params[idx].typ)
			}
			return type_hash_child(hash, t.return_type)
		}
		OptionType {
			hash = type_hash_tag(hash, 17)
			return type_hash_child(hash, t.base_type)
		}
		ResultType {
			hash = type_hash_tag(hash, 18)
			return type_hash_child(hash, t.base_type)
		}
		Struct {
			hash = type_hash_tag(hash, 19)
			return type_hash_string(hash, t.name)
		}
		Interface {
			hash = type_hash_tag(hash, 20)
			return type_hash_string(hash, t.name)
		}
		Enum {
			hash = type_hash_tag(hash, 21)
			hash = type_hash_string(hash, t.name)
			return type_hash_tag(hash, int(t.is_flag))
		}
		SumType {
			hash = type_hash_tag(hash, 22)
			return type_hash_string(hash, t.name)
		}
		Alias {
			hash = type_hash_tag(hash, 23)
			hash = type_hash_string(hash, t.name)
			return type_hash_child(hash, t.base_type)
		}
		MultiReturn {
			hash = type_hash_tag(hash, 24)
			hash = type_hash_tag(hash, t.types.len)
			for i in 0 .. t.types.len {
				hash = type_hash_child(hash, &t.types[i])
			}
			return hash
		}
	}
}

@[inline]
fn type_hash_tag(initial u64, value int) u64 {
	mut hash := initial ^ u64(value)
	hash *= u64(1_099_511_628_211)
	return hash
}

fn type_hash_string(initial u64, value string) u64 {
	mut hash := type_hash_tag(initial, value.len)
	for idx in 0 .. value.len {
		hash ^= u64(value[idx])
		hash *= u64(1_099_511_628_211)
	}
	return hash
}

@[inline]
fn type_hash_child(initial u64, child &Type) u64 {
	mut hash := initial ^ semantic_type_hash(child)
	hash *= u64(1_099_511_628_211)
	return hash
}

pub fn semantic_types_equal(a &Type, b &Type) bool {
	if voidptr(a) == voidptr(b) {
		return true
	}
	match a {
		Void {
			return b is Void
		}
		Unknown {
			if b !is Unknown {
				return false
			}
			return a.reason == b.reason
		}
		Primitive {
			if b !is Primitive {
				return false
			}
			return a.props == b.props && a.size == b.size
		}
		String {
			return b is String
		}
		Char {
			return b is Char
		}
		Rune {
			return b is Rune
		}
		ISize {
			return b is ISize
		}
		USize {
			return b is USize
		}
		Nil {
			return b is Nil
		}
		None {
			return b is None
		}
		Array {
			if b !is Array {
				return false
			}
			return semantic_types_equal(a.elem_type, b.elem_type)
		}
		ArrayFixed {
			if b !is ArrayFixed {
				return false
			}
			return a.len == b.len && a.len_expr == b.len_expr
				&& semantic_types_equal(a.elem_type, b.elem_type)
		}
		Channel {
			if b !is Channel {
				return false
			}
			return a.is_mut == b.is_mut && semantic_types_equal(a.elem_type, b.elem_type)
		}
		Map {
			if b !is Map {
				return false
			}
			return semantic_types_equal(a.key_type, b.key_type)
				&& semantic_types_equal(a.value_type, b.value_type)
		}
		Pointer {
			if b !is Pointer {
				return false
			}
			return semantic_types_equal(a.base_type, b.base_type)
		}
		FnType {
			if b !is FnType {
				return false
			}
			if a.params.len != b.params.len || !semantic_types_equal(a.return_type, b.return_type) {
				return false
			}
			for idx in 0 .. a.params.len {
				if a.params[idx].is_mut != b.params[idx].is_mut
					|| !semantic_types_equal(a.params[idx].typ, b.params[idx].typ) {
					return false
				}
			}
			return true
		}
		OptionType {
			if b !is OptionType {
				return false
			}
			return semantic_types_equal(a.base_type, b.base_type)
		}
		ResultType {
			if b !is ResultType {
				return false
			}
			return semantic_types_equal(a.base_type, b.base_type)
		}
		Struct {
			if b !is Struct {
				return false
			}
			return a.name == b.name
		}
		Interface {
			if b !is Interface {
				return false
			}
			return a.name == b.name
		}
		Enum {
			if b !is Enum {
				return false
			}
			return a.name == b.name && a.is_flag == b.is_flag
		}
		SumType {
			if b !is SumType {
				return false
			}
			return a.name == b.name
		}
		Alias {
			if b !is Alias {
				return false
			}
			return a.name == b.name && semantic_types_equal(a.base_type, b.base_type)
		}
		MultiReturn {
			if b !is MultiReturn {
				return false
			}
			if a.types.len != b.types.len {
				return false
			}
			for idx in 0 .. a.types.len {
				if !semantic_types_equal(&a.types[idx], &b.types[idx]) {
					return false
				}
			}
			return true
		}
	}
}

pub fn (tc &TypeChecker) fn_type(params []Type, result &Type, mutability []bool) FnType {
	mut signature := []FnParam{cap: params.len}
	for i in 0 .. params.len {
		_, typ := tc.intern_type(params[i])
		signature << FnParam{
			typ:    typ
			is_mut: i < mutability.len && mutability[i]
		}
	}
	_, return_type := tc.intern_type(result)
	return FnType{
		params:      signature
		return_type: return_type
	}
}
