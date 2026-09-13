module types

// CachedName keeps a resolved name behind one pointer in the dense node caches.
// Most AST nodes have no resolved name, so empty slots need no string header.
@[heap]
pub struct CachedName {
pub:
	value string
}

// cached_name boxes an immutable name in the current allocation arena.
@[inline]
pub fn cached_name(value string) &CachedName {
	return &CachedName{ value: value }
}

// promote_cached_name moves a cached header and its bytes out of a scope that
// has been left but not yet freed. Either allocation may belong to that scope.
pub fn promote_cached_name(name &CachedName, scope voidptr) &CachedName {
	$if prealloc {
		if !isnil(name) && unsafe {
			prealloc_scope_owns(scope, name) || prealloc_scope_owns(scope, name.value.str)
		} {
			return cached_name(name.value.clone())
		}
	}
	return name
}
