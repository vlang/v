module cleanc

fn test_pass5_worker_clones_global_type_lookup() {
	mut g := Gen.new([])
	g.global_var_types['main__settings'] = 'main__Settings'
	mut worker := g.new_pass5_worker([], 0)
	assert worker.global_var_types['main__settings'] == 'main__Settings'
	g.global_var_types['main__settings'] = 'main__OtherSettings'
	assert worker.global_var_types['main__settings'] == 'main__Settings'
}

fn test_specialized_receiver_method_index_tracks_ambiguity() {
	mut g := Gen.new([])
	g.fn_return_types['Box_T_i32__get'] = 'int'
	g.remember_specialized_fn_base('Box_T_i32__get')
	got := g.resolve_specialized_receiver_method('Box', 'get') or { '' }
	assert got == 'Box_T_i32__get'

	g.fn_return_types['Box_T_string__get'] = 'string'
	g.remember_specialized_fn_base('Box_T_string__get')
	if _ := g.resolve_specialized_receiver_method('Box', 'get') {
		assert false
	} else {
		assert true
	}
}

// When a file is split across pass-5 workers, a non-owning worker blocks the
// file's fns but must still emit its own assigned slice. The owner-scoped bypass
// allows exactly the fns owned by the slice's file (explicit_slice_file) and
// nothing transitively reached from another file.
fn test_explicit_slice_emit_allows_is_scoped_to_owning_file() {
	mut g := Gen.new([])
	g.fn_owner_file['fn_foo'] = 3 // foo is owned by file 3
	g.fn_owner_file['fn_bar'] = 5 // bar is owned by a different file
	g.blocked_fn_keys['fn_foo'] = true
	g.blocked_fn_keys['fn_bar'] = true

	// Not in an explicit slice: nothing is unblocked.
	assert !g.explicit_slice_emit_allows('fn_foo')
	assert !g.explicit_slice_emit_allows('fn_bar')

	// Emitting file 3's slice: only file 3's fns are unblocked.
	g.explicit_slice_active = true
	g.explicit_slice_file = 3
	assert g.explicit_slice_emit_allows('fn_foo')
	assert !g.explicit_slice_emit_allows('fn_bar') // owned by file 5 -> stays blocked
	assert !g.explicit_slice_emit_allows('fn_unknown') // no recorded owner -> blocked

	// Switching to file 5's slice flips which fn is allowed.
	g.explicit_slice_file = 5
	assert !g.explicit_slice_emit_allows('fn_foo')
	assert g.explicit_slice_emit_allows('fn_bar')
}
