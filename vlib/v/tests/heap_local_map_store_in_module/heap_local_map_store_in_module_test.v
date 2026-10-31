module main

import heapstore

// A local that was moved to the heap is stored into a map by value. The record type
// belongs to another module, so its name is qualified where the store is lowered.
fn test_heap_moved_local_is_stored_in_a_map_by_value() {
	mut keep := []&heapstore.Record{}
	mut env := heapstore.Env{}
	heapstore.declared_record(mut env, mut keep)
	assert heapstore.record_read_from_map(mut env, mut keep)
	local := heapstore.record_in_local_map(mut keep)
	assert env.bindings['declared'].n == 2
	assert env.bindings['read'].n == 3
	assert local['local'].n == 5
	assert keep.map(it.n) == [2, 3, 5]
}

fn test_record_updated_through_a_field_call_is_stored_by_value() {
	mut env := heapstore.Env{}
	env.bindings['a'] = heapstore.Record{
		n: 1
	}
	assert heapstore.append_to_record('a', heapstore.Record{ n: 7 }, mut env)
	assert heapstore.append_to_record('a', heapstore.Record{ n: 8 }, mut env)
	assert !heapstore.append_to_record('missing', heapstore.Record{ n: 9 }, mut env)
	assert env.bindings['a'].n == 1
	assert env.bindings['a'].elements.map(it.n) == [7, 8]
}
