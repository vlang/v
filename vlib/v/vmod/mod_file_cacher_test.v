module vmod

import os

// The module shares one private cacher, so every lookup in the process sees the
// same answer for the same folder, and a fresh one must not inherit that.
fn test_get_cache_returns_the_shared_cacher() {
	first := get_cache()
	assert first != unsafe { nil }
	assert get_cache() == first
	// A second call is the same object, so a lookup recorded on it is visible.
	root := @VEXEROOT
	answer := first.get_by_folder(root)
	assert answer.vmod_file != ''
	assert first.get_by_folder(root) == answer
	// A cacher built by hand is a different one and starts empty.
	other := new_mod_file_cacher()
	assert other != first
	assert other.get_by_folder(root) == answer
}

fn test_new_mod_file_cacher_starts_with_no_recorded_folders() {
	root := os.join_path(os.vtmp_dir(), 'vmod_cacher_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'project')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, '.v.mod.stop'), '') or { panic(err) }
	os.write_file(os.join_path(root, 'project', 'v.mod'), '') or { panic(err) }
	mut cacher := new_mod_file_cacher()
	answer := cacher.get_by_folder(os.join_path(root, 'project'))
	assert answer.vmod_file == os.real_path(os.join_path(root, 'project', 'v.mod'))
	// A folder inside the project resolves to the same v.mod.
	nested := os.join_path(root, 'project', 'sub')
	os.mkdir_all(nested) or { panic(err) }
	assert cacher.get_by_folder(nested).vmod_file == answer.vmod_file
	// A folder above it resolves to nothing, and the two answers differ.
	assert cacher.get_by_folder(root).vmod_file == ''
}

fn test_mod_file_cacher_stops_at_the_stop_marker_and_vcs_directories() {
	root := os.join_path(os.vtmp_dir(), 'vmod_cacher_stop_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	outer := os.join_path_single(root, 'outer')
	inner := os.join_path_single(outer, 'inner')
	os.mkdir_all(inner) or { panic(err) }
	os.write_file(os.join_path_single(root, 'v.mod'), '') or { panic(err) }
	// Without a stop marker the walk reaches the outer v.mod.
	mut swallowing := new_mod_file_cacher()
	assert swallowing.get_by_folder(inner).vmod_file == os.real_path(os.join_path_single(root,
		'v.mod'))
	// A `.git` directory ends the walk, so nothing above it is seen.
	os.mkdir_all(os.join_path_single(outer, '.git')) or { panic(err) }
	mut stopping := new_mod_file_cacher()
	assert stopping.get_by_folder(inner).vmod_file == ''
	// `.v.mod.stop` ends it the same way.
	os.rmdir_all(os.join_path_single(outer, '.git')) or { panic(err) }
	os.write_file(os.join_path_single(outer, '.v.mod.stop'), '') or { panic(err) }
	mut marked := new_mod_file_cacher()
	assert marked.get_by_folder(inner).vmod_file == ''
}
