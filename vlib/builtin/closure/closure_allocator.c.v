module closure

// Process-owned bookkeeping uses the configured ordinary allocator rather
// than a caller's scoped arena. Never keep this scope around a user drop.
// This does not change the separate -prealloc allocation policy: arena itself
// is unsupported with -prealloc, and no prealloc compatibility is added here.
@[inline]
fn closure_allocator_begin() voidptr {
	$if builtin_arena ? {
		return unsafe { v_arena_suspend() }
	}
	return unsafe { nil }
}

@[inline]
fn closure_allocator_end(previous voidptr) {
	$if builtin_arena ? {
		unsafe { v_arena_resume(previous) }
	}
}
