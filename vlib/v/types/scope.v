module types

import os
import v.flat

// Scope represents scope data used by types. Bindings are keyed by the
// interned id of their name (see flat.Node.name), as Odin's ScopeMap is keyed
// by InternedString: a lookup compares ids instead of hashing the spelling.
@[heap]
pub struct Scope {
pub mut:
	parent       &Scope = unsafe { nil }
	names        []string
	ids          []u32
	types        []Type
	name_indexes map[u32]int
	// name_mask is a conservative bloom filter over this scope's own binding
	// ids: a cleared bit proves the name is absent, letting chain walks skip
	// the per-level probe (most lookups walk many levels that do not bind the
	// name at all).
	name_mask        u64
	fast_lookup      bool
	fast_generation  u32
	fast_ids         [32]u32
	fast_indexes     [32]u32
	fast_generations [32]u32
	generations      []i32
	storage_keys     []string
	next_generation  int
	lifetime         int
}

pub struct ScopeBindingOwner {
	scope       &Scope = unsafe { nil }
	index       int    = -1
	generation  int
	lifetime    int
	name        string
	name_id     u32
	storage_key string
}

// scope_name_bit is the bloom filter bit of a name spelled as text, which the
// import tables of a file use.
@[direct_array_access; inline]
fn scope_name_bit(name string) u64 {
	if name.len == 0 {
		return u64(1)
	}
	return u64(1) << ((u32(name[0]) * 33 + u32(name[name.len - 1]) * 7 + u32(name.len)) & 63)
}

@[inline]
fn scope_id_bit(id u32) u64 {
	return u64(1) << ((id ^ (id >> 6)) & 63)
}

// scope_linear_names is how many bindings a scope finds by scanning its ids.
// A scope indexes its names in name_indexes only once it has more.
const scope_linear_names = 8

@[direct_array_access; inline]
fn (s &Scope) own_binding_index(id u32) int {
	if s.fast_lookup {
		slot := int(id & 31)
		if s.fast_generations[slot] == s.fast_generation && s.fast_ids[slot] == id {
			return int(s.fast_indexes[slot])
		}
	}
	if s.ids.len <= scope_linear_names {
		return s.scan_own_binding_index(id)
	}
	$if ownership ? {
		return s.scan_own_binding_index(id)
	}
	if i := s.name_indexes[id] {
		return i
	}
	return -1
}

// scan_own_binding_index finds `id` among the ids of a scope that has not
// indexed them (see scope_linear_names).
@[direct_array_access]
fn (s &Scope) scan_own_binding_index(id u32) int {
	for i := s.ids.len - 1; i >= 0; i-- {
		if s.ids[i] == id {
			return i
		}
	}
	return -1
}

@[direct_array_access; inline]
fn (mut s Scope) remember_fast_binding(id u32, index int) {
	if !s.fast_lookup {
		return
	}
	slot := int(id & 31)
	s.fast_ids[slot] = id
	s.fast_indexes[slot] = u32(index)
	s.fast_generations[slot] = s.fast_generation
}

// scope_fast_lookup_enabled is read once: getenv takes the process-wide
// environment lock, which parallel workers creating scopes contended on.
const scope_fast_lookup_enabled = os.getenv('V3_NO_SCOPE_DIRECT') == ''

// new_scope returns a reusable type-checker scope with an optional parent.
pub fn new_scope(parent &Scope) &Scope {
	unsafe {
		return &Scope{
			parent:          parent
			fast_lookup:     scope_fast_lookup_enabled
			fast_generation: 1
		}
	}
}

// reset retargets a pooled scope, clearing its bindings while keeping the
// backing storage capacity so reuse does not reallocate.
pub fn (mut s Scope) reset(parent &Scope) {
	s.parent = parent
	s.names.clear()
	s.ids.clear()
	s.types.clear()
	s.storage_keys.clear()
	// Scopes are pooled: the lifetime distinguishes bindings of a previous
	// occupant from the new one, so binding identity stays exact.
	s.lifetime++
	s.name_mask = 0
	s.fast_generation++
	if s.fast_generation == 0 {
		s.fast_generations = [32]u32{}
		s.fast_generation = 1
	}
	if s.name_indexes.len > 0 {
		s.name_indexes.clear()
	}
	$if ownership ? {
		s.generations.clear()
		s.next_generation = 0
	}
}

// binding_scope returns the nearest scope from this one up to, but not
// including, `stop` (the whole chain when `stop` is nil) that binds `id`, and
// the index of that binding.
@[direct_array_access; inline]
fn (s &Scope) binding_scope(id u32, stop &Scope) (&Scope, int) {
	if id == 0 {
		return unsafe { nil }, -1
	}
	bit := scope_id_bit(id)
	// Only the local pointer is reassigned; the scopes remain read-only.
	mut scope := unsafe { &Scope(s) }
	for scope != unsafe { nil } && voidptr(scope) != voidptr(stop) {
		if scope.name_mask & bit != 0 {
			i := scope.own_binding_index(id)
			if i >= 0 {
				return scope, i
			}
		}
		scope = scope.parent
	}
	return unsafe { nil }, -1
}

// contains_id reports whether this scope or a parent binds the name `id`.
pub fn (s &Scope) contains_id(id u32) bool {
	_, i := s.binding_scope(id, unsafe { nil })
	return i >= 0
}

// lookup_id returns the nearest visible type binding for the name `id`.
@[direct_array_access]
pub fn (s &Scope) lookup_id(id u32) ?Type {
	scope, i := s.binding_scope(id, unsafe { nil })
	if i < 0 {
		return none
	}
	return scope.types[i]
}

// lookup_before_id returns the nearest binding for `id` in the scopes from this
// one up to, but not including, `stop` (the whole chain when `stop` is nil).
@[direct_array_access]
pub fn (s &Scope) lookup_before_id(id u32, stop &Scope) ?Type {
	scope, i := s.binding_scope(id, stop)
	if i < 0 {
		return none
	}
	return scope.types[i]
}

// binds_own_id reports whether this scope itself, not a parent, binds `id`.
@[inline]
pub fn (s &Scope) binds_own_id(id u32) bool {
	if id == 0 || s.name_mask & scope_id_bit(id) == 0 {
		return false
	}
	return s.own_binding_index(id) >= 0
}

// lookup_owner_id returns the nearest scope that owns a visible binding for `id`.
@[direct_array_access]
pub fn (s &Scope) lookup_owner_id(id u32) ?ScopeBindingOwner {
	scope, i := s.binding_scope(id, unsafe { nil })
	if i < 0 {
		return none
	}
	return ScopeBindingOwner{
		scope:       scope
		index:       i
		generation:  if i < scope.generations.len { scope.generations[i] } else { 0 }
		lifetime:    scope.lifetime
		name:        scope.names[i]
		name_id:     id
		storage_key: scope.storage_keys[i]
	}
}

// bound_type returns the current type of this binding while its scope is live.
// It returns none after scope reuse or replacement of an ownership-mode binding.
@[inline]
pub fn (owner &ScopeBindingOwner) bound_type() ?Type {
	if owner.scope == unsafe { nil } || owner.lifetime != owner.scope.lifetime
		|| owner.index < 0 || owner.index >= owner.scope.types.len {
		return none
	}
	$if ownership ? {
		if owner.index >= owner.scope.generations.len
			|| owner.scope.generations[owner.index] != owner.generation {
			return none
		}
	} $else {
		if owner.index >= owner.scope.ids.len || owner.scope.ids[owner.index] != owner.name_id {
			return none
		}
	}
	return owner.scope.types[owner.index]
}

// storage_key returns a stable key for this binding owner while its scope is live.
pub fn (owner ScopeBindingOwner) storage_key() string {
	if owner.scope == unsafe { nil } || owner.index < 0 {
		return ''
	}
	return owner.storage_key
}

fn scope_binding_storage_key(scope &Scope, lifetime int, index int, generation int) string {
	$if !ownership ? {
		// The name alone would collapse every same-named binding in a
		// function to one storage entry; keep the exact binding identity.
		// Spelled as `${voidptr(scope)}:${lifetime}:${index}`, without the
		// interpolation machinery: every scope binding gets one.
		return scope_binding_storage_key_text(u64(voidptr(scope)), lifetime, index)
	}
	return '${voidptr(scope)}:${lifetime}:${index}:${generation}'
}

// scope_binding_storage_key_text writes the lowercase hex digits of `ptr` (no
// leading zeros, `0` for zero), then `:lifetime:index` in decimal.
@[direct_array_access]
fn scope_binding_storage_key_text(ptr u64, lifetime int, index int) string {
	if lifetime < 0 || index < 0 {
		return '${voidptr(ptr)}:${lifetime}:${index}'
	}
	mut buf := [64]u8{}
	mut n := 0
	mut shift := 60
	for shift > 0 && (ptr >> u64(shift)) & 0xf == 0 {
		shift -= 4
	}
	for shift >= 0 {
		digit := u8((ptr >> u64(shift)) & 0xf)
		buf[n] = if digit < 10 { `0` + digit } else { `a` + digit - 10 }
		n++
		shift -= 4
	}
	buf[n] = `:`
	n++
	n += write_storage_key_decimal(unsafe { &buf[n] }, lifetime)
	buf[n] = `:`
	n++
	n += write_storage_key_decimal(unsafe { &buf[n] }, index)
	// The key owns a standalone allocation, like an interpolated string.
	mut out := unsafe { malloc_noscan(n + 1) }
	unsafe {
		vmemcpy(out, &buf[0], n)
		out[n] = 0
	}
	return unsafe { tos(out, n) }
}

// write_storage_key_decimal writes the decimal digits of `value` (at least 0)
// to `dst`, which has room for 20 bytes, and returns how many it wrote.
@[direct_array_access]
fn write_storage_key_decimal(dst &u8, value int) int {
	mut digits := [20]u8{}
	mut count := 0
	mut rest := value
	for {
		digits[count] = u8(`0` + rest % 10)
		count++
		rest /= 10
		if rest == 0 {
			break
		}
	}
	for i in 0 .. count {
		unsafe {
			dst[i] = digits[count - 1 - i]
		}
	}
	return count
}

// belongs_to_scope reports whether this binding owner was declared directly in `scope`.
pub fn (owner ScopeBindingOwner) belongs_to_scope(scope &Scope) bool {
	return owner.scope != unsafe { nil } && scope != unsafe { nil }
		&& voidptr(owner.scope) == voidptr(scope) && owner.lifetime == scope.lifetime
}

// belongs_to_scope_chain_until reports whether this binding owner was declared
// between `scope` and `stop`, inclusive.
pub fn (owner ScopeBindingOwner) belongs_to_scope_chain_until(scope &Scope, stop &Scope) bool {
	if owner.scope == unsafe { nil } || scope == unsafe { nil } || stop == unsafe { nil } {
		return false
	}
	// Only the local pointer is reassigned; the scopes remain read-only.
	mut cur := unsafe { &Scope(scope) }
	for cur != unsafe { nil } {
		if owner.belongs_to_scope(cur) {
			return true
		}
		if voidptr(cur) == voidptr(stop) {
			return false
		}
		cur = cur.parent
	}
	return false
}

// nearest_binding_owned_by_id reports whether the nearest visible binding for
// the name `id` belongs to `owner`.
pub fn (s &Scope) nearest_binding_owned_by_id(id u32, owner ScopeBindingOwner) bool {
	if owner.scope == unsafe { nil } || owner.index < 0 {
		return false
	}
	scope, i := s.binding_scope(id, unsafe { nil })
	if i < 0 {
		return false
	}
	$if ownership ? {
		if i >= scope.generations.len || scope.generations[i] != owner.generation {
			return false
		}
	}
	return voidptr(scope) == voidptr(owner.scope) && scope.lifetime == owner.lifetime
		&& i == owner.index
}

// keep_string returns `text` itself. `arr << s` stores a copy of an addressable
// string `s`; strings are immutable, so pushing `keep_string(s)` stores `s`.
@[inline]
fn keep_string(text string) string {
	return text
}

// insert_id records or updates a type binding for the name `id`, spelled `name`.
pub fn (mut s Scope) insert_id(id u32, name string, typ Type) {
	_ := s.insert_with_owner_id(id, name, typ)
}

// insert_with_owner_id records or updates a type binding and returns the exact
// binding identity now visible for the name `id`, spelled `name`.
pub fn (mut s Scope) insert_with_owner_id(id u32, name string, typ Type) ScopeBindingOwner {
	existing := if s.name_mask & scope_id_bit(id) != 0 { s.own_binding_index(id) } else { -1 }
	if existing >= 0 {
		s.types[existing] = typ
		s.remember_fast_binding(id, existing)
		$if ownership ? {
			s.next_generation++
			s.generations[existing] = s.next_generation
			s.storage_keys[existing] = scope_binding_storage_key(s, s.lifetime, existing,
				s.generations[existing])
		}
		return ScopeBindingOwner{
			scope:       s
			index:       existing
			generation:  if existing < s.generations.len { s.generations[existing] } else { 0 }
			lifetime:    s.lifetime
			name:        s.names[existing]
			name_id:     id
			storage_key: s.storage_keys[existing]
		}
	}
	// Strings are immutable: the binding keeps the caller's name instead of a
	// pushed copy of it.
	s.names << keep_string(name)
	s.ids << id
	s.types << typ
	s.name_mask |= scope_id_bit(id)
	index := s.ids.len - 1
	$if !ownership ? {
		if index == scope_linear_names {
			for i, bound in s.ids {
				s.name_indexes[bound] = i
			}
		} else if index > scope_linear_names {
			s.name_indexes[id] = index
		}
	}
	s.remember_fast_binding(id, index)
	mut generation := 0
	$if ownership ? {
		s.next_generation++
		s.generations << s.next_generation
		generation = s.next_generation
	}
	s.storage_keys << scope_binding_storage_key(s, s.lifetime, index, generation)
	return ScopeBindingOwner{
		scope:       s
		index:       index
		generation:  generation
		lifetime:    s.lifetime
		name:        name
		name_id:     id
		storage_key: s.storage_keys[index]
	}
}
