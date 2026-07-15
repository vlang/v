module types

import sync

// TypeId is the stable identity of a canonical semantic type in one
// compilation. Type values remain the public compatibility representation;
// caches and equality-heavy internals can use this compact identity.
pub type TypeId = u32

@[heap]
struct TypeInterner {
mut:
	lock    &sync.Mutex = unsafe { nil }
	types   []Type
	names   []string
	buckets map[u64]TypeId
}

fn new_type_interner() &TypeInterner {
	return &TypeInterner{
		lock:    sync.new_mutex()
		buckets: map[u64]TypeId{}
	}
}

fn (mut i TypeInterner) intern_locked(t Type) (TypeId, Type) {
	mut key := semantic_type_hash(t)
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
	id := TypeId(i.types.len)
	i.types << t
	i.names << ''
	i.buckets[key] = id
	return id, i.types[int(id)]
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

fn (mut i TypeInterner) canonicalize(t Type) (TypeId, Type) {
	i.lock.lock()
	defer {
		i.lock.unlock()
	}
	return i.intern_locked(t)
}

fn (mut i TypeInterner) len() int {
	i.lock.lock()
	defer {
		i.lock.unlock()
	}
	return i.types.len
}

fn semantic_type_hash(t Type) u64 {
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
			for param in t.params {
				hash = type_hash_child(hash, param)
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
			for item in t.types {
				hash = type_hash_child(hash, item)
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
fn type_hash_child(initial u64, child Type) u64 {
	mut hash := initial ^ semantic_type_hash(child)
	hash *= u64(1_099_511_628_211)
	return hash
}

fn semantic_types_equal(a Type, b Type) bool {
	match a {
		Void {
			return b is Void
		}
		Unknown {
			if b !is Unknown {
				return false
			}
			bb := b as Unknown
			return a.reason == bb.reason
		}
		Primitive {
			if b !is Primitive {
				return false
			}
			bb := b as Primitive
			return a.props == bb.props && a.size == bb.size
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
			bb := b as Array
			return semantic_types_equal(a.elem_type, bb.elem_type)
		}
		ArrayFixed {
			if b !is ArrayFixed {
				return false
			}
			bb := b as ArrayFixed
			return a.len == bb.len && a.len_expr == bb.len_expr
				&& semantic_types_equal(a.elem_type, bb.elem_type)
		}
		Channel {
			if b !is Channel {
				return false
			}
			bb := b as Channel
			return semantic_types_equal(a.elem_type, bb.elem_type)
		}
		Map {
			if b !is Map {
				return false
			}
			bb := b as Map
			return semantic_types_equal(a.key_type, bb.key_type)
				&& semantic_types_equal(a.value_type, bb.value_type)
		}
		Pointer {
			if b !is Pointer {
				return false
			}
			bb := b as Pointer
			return semantic_types_equal(a.base_type, bb.base_type)
		}
		FnType {
			if b !is FnType {
				return false
			}
			bb := b as FnType
			if a.params.len != bb.params.len || !semantic_types_equal(a.return_type, bb.return_type) {
				return false
			}
			for idx, param in a.params {
				if !semantic_types_equal(param, bb.params[idx]) {
					return false
				}
			}
			return true
		}
		OptionType {
			if b !is OptionType {
				return false
			}
			bb := b as OptionType
			return semantic_types_equal(a.base_type, bb.base_type)
		}
		ResultType {
			if b !is ResultType {
				return false
			}
			bb := b as ResultType
			return semantic_types_equal(a.base_type, bb.base_type)
		}
		Struct {
			if b !is Struct {
				return false
			}
			bb := b as Struct
			return a.name == bb.name
		}
		Interface {
			if b !is Interface {
				return false
			}
			bb := b as Interface
			return a.name == bb.name
		}
		Enum {
			if b !is Enum {
				return false
			}
			bb := b as Enum
			return a.name == bb.name && a.is_flag == bb.is_flag
		}
		SumType {
			if b !is SumType {
				return false
			}
			bb := b as SumType
			return a.name == bb.name
		}
		Alias {
			if b !is Alias {
				return false
			}
			bb := b as Alias
			return a.name == bb.name && semantic_types_equal(a.base_type, bb.base_type)
		}
		MultiReturn {
			if b !is MultiReturn {
				return false
			}
			bb := b as MultiReturn
			if a.types.len != bb.types.len {
				return false
			}
			for idx, item in a.types {
				if !semantic_types_equal(item, bb.types[idx]) {
					return false
				}
			}
			return true
		}
	}
}
