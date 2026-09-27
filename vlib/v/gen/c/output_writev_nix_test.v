module c

import os

fn writev_test_segments(count int) []string {
	mut segments := []string{cap: count}
	for i in 0 .. count {
		segments << 'segment ${i}\n'
	}
	return segments
}

fn test_vectored_output_matches_sequential_layout() {
	path := os.join_path(os.vtmp_dir(), 'cgen_writev_layout_${os.getpid()}.c')
	defer {
		os.rm(path) or {}
	}
	segments := writev_test_segments(1500)
	write_c_output_vectored(path, 'prefix\n'.bytes(), segments, 'tail\n', '/* unit */\n')!
	expected := 'prefix\n' + segments.map('/* unit */\n' + it).join('') + 'tail\n'
	assert os.read_file(path)! == expected
}

fn test_vectored_output_shrinks_batches_the_host_rejects() {
	// No host accepts this many vectors in one call (IOV_MAX is at most a few
	// thousand), so every first attempt fails with EINVAL and must be retried in
	// smaller batches without losing or duplicating any piece.
	path := os.join_path(os.vtmp_dir(), 'cgen_writev_shrink_${os.getpid()}.c')
	defer {
		os.rm(path) or {}
	}
	segments := writev_test_segments(40_000)
	write_c_output_vectored_batches(path, 'prefix\n'.bytes(), segments, 'tail\n', '', 1 << 20)!
	expected := 'prefix\n' + segments.join('') + 'tail\n'
	assert os.read_file(path)! == expected
}
