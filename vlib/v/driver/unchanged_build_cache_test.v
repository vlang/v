module driver

import os

fn test_unchanged_build_cache_restores_and_invalidates_source_change() {
	root := os.join_path(os.vtmp_dir(), 'v3_unchanged_build_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	source_dir := os.join_path(root, 'source')
	cache_dir := os.join_path(root, 'cache')
	source := os.join_path(source_dir, 'main.v')
	executable := os.join_path(root, 'app')
	destination := os.join_path(root, 'restored')
	os.mkdir_all(source_dir)!
	os.write_file(source, 'fn main() {}\n')!
	os.write_file(executable, 'executable bytes')!
	assert v3_publish_unchanged_build(cache_dir, 'test', executable, [source], [source_dir])
	assert v3_restore_unchanged_build(cache_dir, 'test', destination)
	assert os.read_file(destination)! == 'executable bytes'
	os.write_file(source, 'fn main() { println("changed") }\n')!
	assert !v3_restore_unchanged_build(cache_dir, 'test', destination)
}

fn test_unchanged_build_cache_invalidates_new_source_file() {
	root := os.join_path(os.vtmp_dir(), 'v3_unchanged_build_new_file_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	source_dir := os.join_path(root, 'source')
	cache_dir := os.join_path(root, 'cache')
	source := os.join_path(source_dir, 'main.v')
	executable := os.join_path(root, 'app')
	destination := os.join_path(root, 'restored')
	os.mkdir_all(source_dir)!
	os.write_file(source, 'fn main() {}\n')!
	os.write_file(executable, 'executable bytes')!
	assert v3_publish_unchanged_build(cache_dir, 'test', executable, [source], [source_dir])
	os.write_file(os.join_path(source_dir, 'added.v'), 'fn added() {}\n')!
	assert !v3_restore_unchanged_build(cache_dir, 'test', destination)
}
