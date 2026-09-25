module c

import os

// cache_program_file_matches must accept a program file under any spelling that
// resolves to it, and resolve each written path at most once per memo.
fn test_cache_program_file_matches_resolves_each_written_path_once() {
	root := os.join_path(os.temp_dir(), 'v_cache_program_file_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	program := os.join_path(root, 'main.v')
	other := os.join_path(root, 'other.v')
	os.write_file(program, 'module main\n')!
	os.write_file(other, 'module main\n')!
	program_files := {
		os.real_path(program): true
	}
	// This spelling only matches after resolution (join_path would drop the `.`).
	dotted := root + os.path_separator + '.' + os.path_separator + 'main.v'
	assert !program_files[dotted]
	mut memo := map[string]bool{}
	assert cache_program_file_matches(program_files, dotted, mut memo)
	assert !cache_program_file_matches(program_files, other, mut memo)
	assert memo == {
		dotted: true
		other:  false
	}
	// A memoized answer is used as is; the path is not resolved again.
	mut seeded := {
		other: true
	}
	assert cache_program_file_matches(program_files, other, mut seeded)
	// Without program files nothing matches, and nothing is resolved or memoized.
	mut empty_memo := map[string]bool{}
	assert !cache_program_file_matches(map[string]bool{}, other, mut empty_memo)
	assert empty_memo.len == 0
}
