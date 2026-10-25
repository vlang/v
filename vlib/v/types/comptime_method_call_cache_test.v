module types

import v.flat

fn test_comptime_method_call_cache_preserves_declaration_and_ordered_arguments() {
	mut calls := map[u64][]ComptimeMethodCallInstance{}
	decl := flat.NodeId(1)
	other_decl := flat.NodeId(2)
	integer := flat.source_type_id('int')
	text := flat.source_type_id('string')
	callback := flat.source_type_id('fn (int, string)')
	assert remember_comptime_method_call(mut calls, decl, [integer, text])
	assert !remember_comptime_method_call(mut calls, decl, [integer, text])
	assert remember_comptime_method_call(mut calls, decl, [text, integer])
	assert remember_comptime_method_call(mut calls, decl, [integer, integer])
	assert remember_comptime_method_call(mut calls, decl, [integer])
	assert remember_comptime_method_call(mut calls, decl, [callback])
	assert remember_comptime_method_call(mut calls, decl, [integer, text, integer])
	assert remember_comptime_method_call(mut calls, other_decl, [integer, text])
	assert remember_comptime_method_call(mut calls, decl, []u32{})
	assert !remember_comptime_method_call(mut calls, decl, []u32{})
	assert remember_comptime_method_call(mut calls, decl, [u32(0)])
	assert !remember_comptime_method_call(mut calls, decl, [u32(0)])
	mut instances := 0
	for _, bucket in calls {
		instances += bucket.len
	}
	assert instances == 9
}

fn test_comptime_method_call_cache_verifies_hash_collisions_and_fork_snapshots() {
	mut calls := map[u64][]ComptimeMethodCallInstance{}
	decl := flat.NodeId(1)
	other_decl := flat.NodeId(2)
	integer := flat.source_type_id('int')
	text := flat.source_type_id('string')
	mut args := [integer, text]
	key := comptime_method_call_key(decl, args)
	// Seed nonmatching instances in the requested hash bucket to force collisions
	// without relying on a particular hash or adding a test-only cache API.
	calls[key] = [
		ComptimeMethodCallInstance{
			decl_id: other_decl
			args:    [integer, text]
		},
		ComptimeMethodCallInstance{
			decl_id: decl
			args:    [text, integer]
		},
		ComptimeMethodCallInstance{
			decl_id: decl
			args:    [integer]
		},
	]
	mut fork := clone_checked_comptime_method_calls(calls)
	assert remember_comptime_method_call(mut fork, decl, args)
	assert !remember_comptime_method_call(mut fork, decl, args)
	assert calls[key].len == 3
	assert fork[key].len == 4
	args[0] = text
	assert fork[key][3].args == [integer, text]
	mut nested := clone_checked_comptime_method_calls(fork)
	assert !remember_comptime_method_call(mut nested, decl, [integer, text])
	assert remember_comptime_method_call(mut calls, decl, [integer, text])
	assert remember_comptime_method_call(mut fork, other_decl, [text])
	assert remember_comptime_method_call(mut nested, other_decl, [text])
	fork.clear()
	calls.clear()
	assert !remember_comptime_method_call(mut nested, decl, [integer, text])
	assert !remember_comptime_method_call(mut nested, other_decl, [text])
	assert remember_comptime_method_call(mut calls, decl, [integer, text])
}
