module driver

import os
import v.modulecache

fn test_program_link_inputs_are_the_files_and_library_candidates_of_a_link() {
	root := os.join_path(os.vtmp_dir(), 'v3_driver_program_link_inputs_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	first := os.join_path(root, 'first')
	second := os.join_path(root, 'second')
	linker := os.join_path(root, 'linker')
	for dir in [first, second, linker] {
		os.mkdir_all(dir)!
	}
	object := os.join_path(root, 'module.o')
	archive := os.join_path(second, 'libfoo.a')
	runtime := os.join_path(linker, 'libruntime.a')
	for file in [object, archive, runtime, os.join_path(linker, 'notes.txt')] {
		os.write_file(file, 'x')!
	}
	gone := os.join_path(root, 'gone.o')
	files, missing := v3_program_link_inputs(['-std=gnu11', '-o', 'out', 'src.c', object, gone,
		'-L${first}', '-L', second, '-lfoo', '-l', 'bar', '-I${root}', '-Wl,-rpath,${root}'],
		linker)
	assert files == [runtime, object, archive].sorted()
	// The link reads `libfoo.a` of the second directory because no library of that
	// name is in the first, or before it in the second.
	for candidate in [gone, os.join_path(first, 'libfoo.dylib'), os.join_path(first, 'libfoo.a'),
		os.join_path(second, 'libfoo.dylib'), os.join_path(first, 'libbar.a'),
		os.join_path(second, 'libbar.so')] {
		assert candidate in missing, candidate
	}
	assert archive !in missing
	// What is relative to the build directory is made anew by every build.
	assert files.all(os.is_abs_path(it)) && missing.all(os.is_abs_path(it))
}

fn test_files_keep_identities_only_while_they_are_the_files_that_were_read() {
	root := os.join_path(os.vtmp_dir(), 'v3_driver_file_identities_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() {}\n')!
	identities := [modulecache.file_metadata_signature(source)]
	if identities[0] == '' {
		// This file system cannot tell a later edit apart: nothing is kept for it.
		assert !v3_files_keep_identities([source], identities)
		return
	}
	assert v3_files_keep_identities([source], identities)
	assert !v3_files_keep_identities([source, source], identities)
	assert !v3_files_keep_identities([source], [''])
	os.write_file(source, 'fn main() { println(1) }\n')!
	assert !v3_files_keep_identities([source], identities)
}

fn test_program_executable_link_signature_tells_builds_apart() {
	base := v3_program_executable_link_signature(['-lm'], false, ['strict=false'])
	assert base == v3_program_executable_link_signature(['-lm'], false, ['strict=false'])
	assert base != v3_program_executable_link_signature(['-lm', '-lz'], false, ['strict=false'])
	assert base != v3_program_executable_link_signature(['-lm'], true, ['strict=false'])
	assert base != v3_program_executable_link_signature(['-lm'], false, ['strict=true'])
}
