module types

// Scope represents scope data used by types.
@[heap]
pub struct Scope {
pub mut:
	parent          &Scope = unsafe { nil }
	names           []string
	types           []Type
	name_indexes    map[string]int
	generations     []int
	next_generation int
	lifetime        int
}

pub struct ScopeBindingOwner {
	scope      &Scope = unsafe { nil }
	index      int    = -1
	generation int
	lifetime   int
	name       string
}

// new_scope returns a reusable type-checker scope with an optional parent.
pub fn new_scope(parent &Scope) &Scope {
	unsafe {
		return &Scope{
			parent: parent
		}
	}
}

// reset retargets a pooled scope, clearing its bindings while keeping the
// backing storage capacity so reuse does not reallocate. Without this clear
// a pooled scope's arrays accumulate every binding ever inserted across all
// generations, turning lookup/insert into an O(n) scan over dead entries.
pub fn (mut s Scope) reset(parent &Scope) {
	s.parent = parent
	s.names.clear()
	s.types.clear()
	// Scopes are pooled: the lifetime distinguishes bindings of a previous
	// occupant from the new one, so binding identity (storage_key,
	// nearest_binding_owned_by) stays exact in every build mode.
	s.lifetime++
	$if !ownership ? {
		s.name_indexes.clear()
	}
	$if ownership ? {
		s.generations.clear()
		s.next_generation = 0
	}
}

// lookup returns the nearest visible type binding for `name`.
pub fn (s &Scope) lookup(name string) ?Type {
	if name.len == 0 {
		return none
	}
	$if !ownership ? {
		// Only the local pointer is reassigned; the scopes remain read-only.
		mut scope := unsafe { &Scope(s) }
		for scope != unsafe { nil } {
			if i := scope.name_indexes[name] {
				return scope.types[i]
			}
			scope = scope.parent
		}
		return none
	}
	for i := s.names.len - 1; i >= 0; i-- {
		if s.names[i] == name {
			return s.types[i]
		}
	}
	if s.parent != unsafe { nil } {
		return s.parent.lookup(name)
	}
	return none
}

// lookup_owner returns the nearest scope that owns a visible binding for `name`.
pub fn (s &Scope) lookup_owner(name string) ?ScopeBindingOwner {
	if name.len == 0 {
		return none
	}
	$if !ownership ? {
		// Only the local pointer is reassigned; the scopes remain read-only.
		mut scope := unsafe { &Scope(s) }
		for scope != unsafe { nil } {
			if i := scope.name_indexes[name] {
				return ScopeBindingOwner{
					scope:    scope
					index:    i
					lifetime: scope.lifetime
					name:     name
				}
			}
			scope = scope.parent
		}
		return none
	}
	for i := s.names.len - 1; i >= 0; i-- {
		if s.names[i] == name {
			return ScopeBindingOwner{
				scope:      s
				index:      i
				generation: s.generations[i]
				lifetime:   s.lifetime
			}
		}
	}
	if s.parent != unsafe { nil } {
		return s.parent.lookup_owner(name)
	}
	return none
}

// storage_key returns a stable key for this binding owner while its scope is live.
pub fn (owner ScopeBindingOwner) storage_key() string {
	if owner.scope == unsafe { nil } || owner.index < 0 {
		return ''
	}
	$if !ownership ? {
		// The name alone would collapse every same-named binding in a
		// function to one storage entry; keep the exact binding identity.
		return '${voidptr(owner.scope)}:${owner.lifetime}:${owner.index}'
	}
	return '${voidptr(owner.scope)}:${owner.lifetime}:${owner.index}:${owner.generation}'
}

// nearest_binding_owned_by reports whether the nearest visible binding for
// `name` belongs to `owner`.
pub fn (s &Scope) nearest_binding_owned_by(name string, owner ScopeBindingOwner) bool {
	if name.len == 0 || owner.scope == unsafe { nil } || owner.index < 0 {
		return false
	}
	$if !ownership ? {
		if i := s.name_indexes[name] {
			return s == owner.scope && s.lifetime == owner.lifetime && i == owner.index
		}
		if s.parent != unsafe { nil } {
			return s.parent.nearest_binding_owned_by(name, owner)
		}
		return false
	}
	for i := s.names.len - 1; i >= 0; i-- {
		if s.names[i] == name {
			return s == owner.scope && s.lifetime == owner.lifetime && i == owner.index
				&& s.generations[i] == owner.generation
		}
	}
	if s.parent != unsafe { nil } {
		return s.parent.nearest_binding_owned_by(name, owner)
	}
	return false
}

// insert records or updates a type binding in this scope.
pub fn (mut s Scope) insert(name string, typ Type) {
	_ := s.insert_with_owner(name, typ)
}

// insert_with_owner records or updates a type binding and returns the exact
// binding identity now visible for `name`.
pub fn (mut s Scope) insert_with_owner(name string, typ Type) ScopeBindingOwner {
	$if !ownership ? {
		if i := s.name_indexes[name] {
			s.types[i] = typ
			return ScopeBindingOwner{
				scope:    s
				index:    i
				lifetime: s.lifetime
				name:     name
			}
		}
		s.names << name
		s.types << typ
		s.name_indexes[name] = s.names.len - 1
		return ScopeBindingOwner{
			scope:    s
			index:    s.names.len - 1
			lifetime: s.lifetime
			name:     name
		}
	}
	for i := s.names.len - 1; i >= 0; i-- {
		if s.names[i] == name {
			s.types[i] = typ
			s.next_generation++
			s.generations[i] = s.next_generation
			return ScopeBindingOwner{
				scope:      s
				index:      i
				generation: s.generations[i]
				lifetime:   s.lifetime
			}
		}
	}
	s.next_generation++
	s.names << name
	s.types << typ
	s.generations << s.next_generation
	index := s.names.len - 1
	return ScopeBindingOwner{
		scope:      s
		index:      index
		generation: s.next_generation
		lifetime:   s.lifetime
	}
}
