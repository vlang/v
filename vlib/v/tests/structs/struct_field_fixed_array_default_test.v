import fixed_array_default_mod { Request }

// `id [4]u64 = [...]!` used to be emitted as `.id = (u64[4]){...}` inside the
// compound literal. An array member is not assignable there, so the literal
// decayed to a pointer and the C compiler initialized `id[0]` with it:
//   error: incompatible pointer to integer conversion initializing 'u64'
//          with an expression of type 'u64[4]'
// The default has to be copied after the literal closes, like an explicitly
// set fixed-array field.

fn test_imported_struct_keeps_its_fixed_array_field_default() {
	req := Request{
		response: unsafe { nil }
	}
	assert req.id[0] == u64(0xc7b1dd30df4c8b88)
	assert req.id[1] == u64(0x0a82e883a194f07b)
	assert req.id[2] == u64(0x48dcf1cb8ad2b852)
	assert req.id[3] == u64(0x63984e959a98244b)
	assert req.tags == [7, 8, 9]!
	assert req.revision == 0
}

fn test_explicitly_set_field_still_wins_over_the_default() {
	req := Request{
		tags:     [1, 2, 3]!
		revision: 5
		response: unsafe { nil }
	}
	assert req.tags == [1, 2, 3]!
	assert req.revision == 5
	// The untouched fixed-array default is still applied.
	assert req.id[0] == u64(0xc7b1dd30df4c8b88)
}

fn test_heap_initializer_keeps_the_default() {
	req := &Request{
		response: unsafe { nil }
	}
	assert req.id[3] == u64(0x63984e959a98244b)
	assert req.tags == [7, 8, 9]!
}
