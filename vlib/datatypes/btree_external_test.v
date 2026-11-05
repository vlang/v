import os

fn test_btree_iterator_from_importing_cached_module() {
	// Module-cache staging repeats the generated filename. Avoid the test
	// session's extra directory level so Windows paths remain below MAX_PATH.
	root := os.join_path(os.temp_dir(), 'btree_iterator_import_${os.getpid()}')
	os.mkdir(root)!
	previous_cache := os.getenv('V3CACHE')
	os.setenv('V3CACHE', os.join_path(root, 'cache'), true)
	defer {
		if previous_cache == '' {
			os.unsetenv('V3CACHE')
		} else {
			os.setenv('V3CACHE', previous_cache, true)
		}
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'main.v')
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'btree_iterator_import' }")!
	// First cache the library through range_keys, which consumes its iterator internally.
	os.write_file(path, 'import datatypes
fn main() {
	mut tree := datatypes.new_btree[int, int]()
	tree.set(7, 11)
	assert tree.range_keys(7, 7) == [7]
}
')!
	warm := os.exec([@VEXE, '-cc', @CCOMPILER, 'run', path])
	assert warm.exit_code == 0, warm.output
	// The importing program then needs a concrete optional entry return from next.
	os.write_file(path, 'import datatypes
fn main() {
	mut tree := datatypes.new_btree[int, int]()
	tree.set(7, 11)
	mut snapshot := tree.iterate(7, 7)
	tree.set(7, 99)
	assert tree.delete(7)
	entry := snapshot.next() or { panic("missing captured entry") }
	assert entry.key == 7 && entry.value == 11
	assert snapshot.next() == none
}
')!
	result := os.exec([@VEXE, '-cc', @CCOMPILER, 'run', path])
	assert result.exit_code == 0, result.output
}
