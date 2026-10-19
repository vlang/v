module flat

import os
import v.token

// real_source_path must answer exactly like os.real_path, from the table for
// parsed sources and directly for anything else, and must never add to the
// table: threads read it concurrently once it is built.
fn test_real_source_path_matches_os_real_path_and_never_grows_the_table() {
	root := os.join_path(os.vtmp_dir(), 'v3_flat_source_paths_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'src')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	parsed := os.join_path(root, 'src', 'main.v')
	other := os.join_path(root, 'src', 'other.v')
	os.write_file(parsed, 'module main\n')!
	os.write_file(other, 'module main\n')!
	// A spelling that only resolution turns into the canonical path.
	parsed_spelling := root + os.path_separator + 'src' + os.path_separator + '.' +
		os.path_separator + 'main.v'
	mut file_set := token.FileSet.new()
	mut a := FlatAst.new()
	a.source_files[1] = file_set.add_file(parsed_spelling, 12)
	a.source_files[2] = file_set.add_file(parsed_spelling, 12)
	assert a.real_source_path(parsed_spelling) == os.real_path(parsed_spelling)

	a.resolve_source_paths()
	assert a.resolved_source_paths.len == 1
	assert a.resolved_source_paths[parsed_spelling] == os.real_path(parsed_spelling)
	assert a.real_source_path(parsed_spelling) == os.real_path(parsed)
	assert a.real_source_path(other) == os.real_path(other)
	assert a.real_source_path(os.join_path(root, 'missing.v')) == os.real_path(os.join_path(root,
		'missing.v'))
	assert a.resolved_source_paths.len == 1
}

// Paths the owning thread records before resolve_source_paths seed the table,
// and resolve_source_paths reuses them instead of resolving those files again.
fn test_record_source_path_seeds_the_table_for_resolve_source_paths() {
	root := os.join_path(os.vtmp_dir(), 'v3_flat_record_source_path_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'module main\n')!
	mut a := FlatAst.new()
	assert a.record_source_path(source) == os.real_path(source)
	assert a.resolved_source_paths.len == 1
	assert a.record_source_path(source) == os.real_path(source)
	assert a.resolved_source_paths.len == 1
	// Mark the recorded answer so a second resolution would be visible.
	a.resolved_source_paths[source] = 'recorded'
	mut file_set := token.FileSet.new()
	a.source_files[1] = file_set.add_file(source, 12)
	a.resolve_source_paths()
	assert a.resolved_source_paths[source] == 'recorded'
}

// Once resolve_source_paths has frozen the table, nothing adds to it any more,
// because other threads read it without locks. Late calls still answer like
// os.real_path.
fn test_resolve_source_paths_freezes_the_table() {
	root := os.join_path(os.vtmp_dir(), 'v3_flat_frozen_source_paths_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	parsed := os.join_path(root, 'main.v')
	late := os.join_path(root, 'late.v')
	os.write_file(parsed, 'module main\n')!
	os.write_file(late, 'module main\n')!
	mut file_set := token.FileSet.new()
	mut a := FlatAst.new()
	a.source_files[1] = file_set.add_file(parsed, 12)
	a.resolve_source_paths()
	assert a.resolved_source_paths.len == 1
	assert a.record_source_path(late) == os.real_path(late)
	assert a.resolved_source_paths.len == 1
	// A file parsed after the freeze is not added by a second call either.
	a.source_files[2] = file_set.add_file(late, 12)
	a.resolve_source_paths()
	assert a.resolved_source_paths.len == 1
	assert late !in a.resolved_source_paths
}

// A frozen source-path table stays read-only for listed files too.
fn test_record_listed_source_path_does_not_grow_a_frozen_table() {
	root := os.join_path(os.vtmp_dir(), 'v3_flat_listed_frozen_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	late := os.join_path(root, 'late.v')
	os.write_file(late, 'module main\n')!
	mut a := FlatAst.new()
	a.resolve_source_paths()
	assert a.record_listed_source_path(late, os.real_path(root)) == os.real_path(late)
	assert a.resolved_source_paths.len == 0
}

// A listed entry resolves through the real path of its directory, and like
// os.real_path through its target when the entry is a symbolic link.
fn test_record_listed_source_path_matches_os_real_path() {
	root := os.join_path(os.vtmp_dir(), 'v3_flat_listed_source_paths_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'real')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'real', 'main.v'), 'module main\n')!
	os.write_file(os.join_path(root, 'real', 'target.v'), 'module main\n')!
	// The listed directory is reached through a link, and one entry is a link.
	linked_dir := os.join_path(root, 'linked')
	os.symlink(os.join_path(root, 'real'), linked_dir) or { return }
	os.symlink(os.join_path(root, 'real', 'target.v'), os.join_path(root, 'real', 'alias.v')) or {
		return
	}
	real_dir := os.real_path(linked_dir)
	mut a := FlatAst.new()
	for name in ['main.v', 'alias.v', 'missing.v'] {
		path := os.join_path(linked_dir, name)
		assert a.record_listed_source_path(path, real_dir) == os.real_path(path)
		assert a.resolved_source_paths[path] == os.real_path(path)
	}
}

fn test_record_listed_source_path_in_matches_os_real_path() {
	root := os.join_path(os.vtmp_dir(), 'v3_flat_listed_source_paths_in_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'real')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'real', 'main.v'), 'module main\n')!
	os.write_file(os.join_path(root, 'real', 'target.v'), 'module main\n')!
	os.mkdir_all(os.join_path(root, 'other')) or { panic(err) }
	os.write_file(os.join_path(root, 'other', 'elsewhere.v'), 'module main\n')!
	// The listed directory is reached through a link, and two entries are links:
	// one to a sibling entry and one out of the directory.
	linked_dir := os.join_path(root, 'linked')
	os.symlink(os.join_path(root, 'real'), linked_dir) or { return }
	os.symlink(os.join_path(root, 'real', 'target.v'), os.join_path(root, 'real', 'alias.v')) or {
		return
	}
	os.symlink(os.join_path(root, 'other', 'elsewhere.v'), os.join_path(root, 'real', 'outside.v')) or {
		return
	}
	real_dir := os.real_path(linked_dir)
	symlinks := scan_listed_symlinks(linked_dir)
	$if !windows {
		assert symlinks.known
		assert symlinks.names.len == 2
	}
	mut a := FlatAst.new()
	for name in ['main.v', 'target.v', 'alias.v', 'outside.v'] {
		path := os.join_path(linked_dir, name)
		assert a.record_listed_source_path_in(path, real_dir, &symlinks) == os.real_path(path)
		assert a.resolved_source_paths[path] == os.real_path(path)
	}
}
