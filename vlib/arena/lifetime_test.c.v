import arena

fn test_lifetime_token_rejects_non_arena_addresses() {
	stack_value := 42
	heap_value := unsafe { malloc(32) }
	defer { unsafe { free(heap_value) } }
	assert arena.lifetime_token(unsafe { nil }) == none
	assert arena.lifetime_token(&stack_value) == none
	assert arena.lifetime_token(heap_value) == none
}

fn test_lifetime_token_survives_pop_and_distinguishes_arenas() {
	mut outer := arena.new()
	mut inner := arena.new()
	defer {
		inner.free()
		outer.free()
	}
	outer.push()
	first := unsafe { malloc(8) }
	second := unsafe { malloc(32) }
	outer_token := arena.lifetime_token(first) or { panic('missing outer lifetime') }
	assert outer_token.matches(first)
	assert outer_token.matches(second)
	assert outer_token.matches(unsafe { first + 1 })
	inner.push()
	nested := unsafe { malloc(16) }
	inner_token := arena.lifetime_token(nested) or { panic('missing inner lifetime') }
	assert inner_token.matches(nested)
	assert !outer_token.matches(nested)
	assert !inner_token.matches(first)
	inner.pop()
	outer.pop()
	assert outer_token.matches(first)
	assert inner_token.matches(nested)
	inner.free()
	assert !inner_token.matches(nested)
	assert outer_token.matches(first)
	assert !outer_token.matches(unsafe { nil })
}

fn test_lifetime_token_reset_invalidates_retained_chunk_and_reused_address() {
	mut storage := arena.new(chunk_size: 1024)
	defer { storage.free() }
	storage.push()
	address := unsafe { malloc(16) }
	original := arena.lifetime_token(address) or { panic('missing original lifetime') }
	storage.pop()
	for _ in 0 .. 8 {
		before_reset := arena.lifetime_token(address) or { panic('missing retained lifetime') }
		storage.reset()
		assert !before_reset.matches(address)
		assert !original.matches(address)
		storage.push()
		reused := unsafe { malloc(16) }
		storage.pop()
		assert reused == address
		current := arena.lifetime_token(reused) or { panic('missing reused lifetime') }
		assert current.matches(reused)
		assert !before_reset.matches(reused)
		assert !original.matches(reused)
	}
}

fn test_lifetime_token_reset_invalidates_released_and_retained_chunks() {
	mut storage := arena.new(chunk_size: 128)
	defer { storage.free() }
	storage.push()
	first := unsafe { malloc(32) }
	large := unsafe { malloc(1024) }
	first_token := arena.lifetime_token(first) or { panic('missing first lifetime') }
	large_token := arena.lifetime_token(large) or { panic('missing large lifetime') }
	storage.pop()
	storage.reset()
	assert !first_token.matches(first)
	assert !large_token.matches(large)
	storage.free()
	assert arena.lifetime_token(first) == none
	assert arena.lifetime_token(large) == none
}

fn check_lifetime_after_release(address voidptr, token arena.LifetimeToken, ready chan bool,
	finished chan bool) {
	assert token.matches(address)
	ready <- true
	_ := <-finished
	assert !token.matches(address)
	assert arena.lifetime_token(address) == none
}

fn test_lifetime_token_can_query_after_owner_is_freed_on_another_thread() {
	mut storage := arena.new()
	ready := chan bool{}
	finished := chan bool{}
	storage.push()
	address := unsafe { malloc(64) }
	token := arena.lifetime_token(address) or { panic('missing lifetime') }
	storage.pop()
	worker := spawn check_lifetime_after_release(address, token, ready, finished)
	_ := <-ready
	storage.free()
	finished <- true
	worker.wait()
}

fn churn_lifetime_registry() {
	for _ in 0 .. 1000 {
		mut storage := arena.new(chunk_size: 128)
		storage.push()
		_ = unsafe { malloc(32) }
		storage.pop()
		storage.reset()
		storage.free()
	}
}

fn test_lifetime_token_remains_valid_during_other_threads_registry_updates() {
	mut storage := arena.new()
	storage.push()
	address := unsafe { malloc(64) }
	token := arena.lifetime_token(address) or { panic('missing lifetime') }
	storage.pop()
	worker := spawn churn_lifetime_registry()
	for _ in 0 .. 10000 {
		assert token.matches(address)
	}
	worker.wait()
	storage.free()
	assert !token.matches(address)
}

fn test_lifetime_tokens_survive_registry_growth_and_middle_removal() {
	mut owners := []arena.Arena{cap: 48}
	mut addresses := []voidptr{cap: 48}
	mut tokens := []arena.LifetimeToken{cap: 48}
	// The registry starts with room for 16 ranges. Keep enough live owners to
	// force multiple replacements, then remove entries from its middle.
	for _ in 0 .. 48 {
		storage := arena.new(chunk_size: 128)
		storage.push()
		address := unsafe { malloc(16) }
		storage.pop()
		token := arena.lifetime_token(address) or { panic('missing lifetime') }
		owners << storage
		addresses << address
		tokens << token
	}
	for i in 0 .. owners.len {
		assert tokens[i].matches(addresses[i])
	}
	for i := 1; i < owners.len; i += 2 {
		owners[i].free()
		assert !tokens[i].matches(addresses[i])
	}
	for i := 0; i < owners.len; i += 2 {
		assert tokens[i].matches(addresses[i])
		owners[i].free()
		assert !tokens[i].matches(addresses[i])
	}
}

fn test_lifetime_tokens_do_not_match_new_arena_owners() {
	mut previous := []arena.LifetimeToken{}
	for _ in 0 .. 64 {
		mut storage := arena.new(chunk_size: 128)
		storage.push()
		address := unsafe { malloc(16) }
		storage.pop()
		current := arena.lifetime_token(address) or { panic('missing lifetime') }
		assert current.matches(address)
		for old in previous {
			assert !old.matches(address)
		}
		previous << current
		storage.free()
		assert !current.matches(address)
	}
}
