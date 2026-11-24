module arena

// LifetimeToken identifies an arena lifetime without keeping its storage alive.
// All addresses in the same arena share a lifetime until reset or free.
// A token is neither an allocation identifier nor proof of initialized storage.
@[noinit]
pub struct LifetimeToken {
	epoch u64
}

// lifetime_token returns the lifetime of the live arena chunk containing ptr.
// It returns none for nil, stack, heap, and other non-arena addresses. The query
// reads allocator metadata only and never dereferences ptr.
pub fn lifetime_token(ptr voidptr) ?LifetimeToken {
	epoch := v_arena_lifetime_token(ptr)
	if epoch == 0 {
		return none
	}
	return LifetimeToken{ epoch: epoch }
}

// matches reports whether ptr belongs to a chunk from this token's arena
// lifetime. It is false after reset or free, including when an address is reused.
// It does not dereference ptr, pin its storage, or synchronize access to payloads.
// Callers must synchronize reset/free with any subsequent use of a live pointer.
pub fn (token LifetimeToken) matches(ptr voidptr) bool {
	return token.epoch != 0 && v_arena_lifetime_token(ptr) == token.epoch
}
