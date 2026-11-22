module fastc

import os
import v.cmdexec

// Nested FastC builds select their own C compiler; a GCC choice inherited from the
// outer runner's VFLAGS (sanitizer and musl jobs) is not a FastC compiler.
fn testsuite_begin() {
	os.unsetenv('VFLAGS')
	os.unsetenv('VOSARGS')
}

fn test_large_embedded_files_link_from_a_separate_object() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_payload_object_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'other')) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	payload := []u8{len: fastc_payload_unit_min_bytes + 3, init: u8(index % 251)}
	mut sum := u64(0)
	for b in payload {
		sum += b
	}
	os.write_file_array(os.join_path(root, 'payload.bin'), payload) or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), r"module main

import other

fn main() {
	bytes := $embed_file('payload.bin').to_bytes()
	mut sum := u64(0)
	for b in bytes {
		sum += b
	}
	println('${bytes.len + other.offset()} ${sum}')
}
") or {
		panic(err)
	}
	// The first build compiles without caches, the second publishes the payload object
	// and the third reuses it. A different offset keeps the executable cache cold.
	cached_program := os.join_path(root, 'program')
	cache_dir := os.join_path(os.vtmp_dir(), 'v3_fastc_unit_cache')
	prefix := fastc_payload_cache_prefix(cached_program)
	for run, flags in [['-nocache'], []string{}, []string{}] {
		os.write_file(os.join_path(root, 'other', 'other.v'), 'module other\n\npub fn offset() int {\n\treturn ${run}\n}\n') or {
			panic(err)
		}
		program := if run == 0 { os.join_path(root, 'program_0') } else { cached_program }
		mut args := ['-b', 'fastc']
		args << flags
		args << ['-o', program, root]
		build := cmdexec.run(@VEXE, args)
		assert build.exit_code == 0, build.output
		result := cmdexec.run(program, [])
		assert result.exit_code == 0, result.output
		assert result.output.trim_space() == '${payload.len + run} ${sum}'
		if run > 0 {
			cached := (os.ls(cache_dir) or { []string{} }).filter(it.starts_with(prefix))
			assert cached.len == 1, cached.str()
		}
	}
	for name in os.ls(cache_dir) or { []string{} } {
		if name.starts_with(prefix) {
			os.rm(os.join_path(cache_dir, name)) or {}
		}
	}
}

// A new payload object of an output replaces the previous ones of that output only.
fn test_stale_payload_objects_of_the_same_output_are_removed() {
	dir := os.join_path(os.vtmp_dir(), 'fastc_payload_prune_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer { os.rmdir_all(dir) or {} }
	prefix := fastc_payload_cache_prefix('/tmp/a/program')
	other := fastc_payload_cache_prefix('/tmp/b/program')
	assert prefix != other
	for name in ['${prefix}old.o', '${prefix}new.o', '${other}old.o', 'unit_x.o'] {
		os.write_file(os.join_path(dir, name), 'x') or { panic(err) }
	}
	fastc_remove_stale_payload_objects(dir, prefix, os.join_path(dir, '${prefix}new.o'))
	mut left := os.ls(dir) or { panic(err) }
	left.sort()
	mut want := ['${prefix}new.o', '${other}old.o', 'unit_x.o']
	want.sort()
	assert left == want
}
