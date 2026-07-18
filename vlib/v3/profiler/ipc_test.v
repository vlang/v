module profiler

import os
import time

// A snapshot must round-trip file paths that contain commas: the record format
// puts the path last and the reader rejoins the trailing fields, so a comma in
// the path can no longer be mistaken for extra columns.
fn test_snapshot_roundtrips_file_paths_with_commas() {
	init_profiler_state()
	profiler_enable()
	reset()

	// Record an allocation whose source path contains commas.
	ptr := profiler_alloc_with_location(64, '/tmp/weird,path/with,commas.v', 42)
	assert ptr != unsafe { nil }

	path := os.join_path(os.temp_dir(),
		'v3_profiler_ipc_test_${os.getpid()}_${time.sys_mono_now()}.dat')
	defer {
		os.rm(path) or {}
	}
	write_snapshot_to(path)

	snap := read_snapshot_from(path) or {
		assert false, 'snapshot did not parse'
		return
	}
	assert snap.allocs.len >= 1
	last := snap.allocs[snap.allocs.len - 1]
	assert last.file == '/tmp/weird,path/with,commas.v'
	assert last.line == 42
	assert last.size == 64
}

// init_profiler_state must be idempotent: repeated calls keep the same mutex and
// do not wipe state out from under an active profiling session.
fn test_init_profiler_state_is_idempotent() {
	init_profiler_state()
	mu_before := voidptr(get_state().mu)
	init_profiler_state()
	init_profiler_state()
	assert voidptr(get_state().mu) == mu_before
}
