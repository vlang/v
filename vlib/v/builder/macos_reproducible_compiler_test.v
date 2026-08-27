module builder

import crypto.sha256
import os
import os.filelock

const macos_reproducible_compiler_vexe = @VEXE

fn create_macos_debug_cache_entry(cache_dir string, name string, contents string, last_used i64) !string {
	object_dir := os.join_path(cache_dir, name)
	os.mkdir_all(object_dir)!
	object_path := os.join_path(object_dir, 'v-compiler.o')
	os.write_file(object_path, contents)!
	os.utime(object_dir, last_used, last_used)!
	return object_path
}

fn test_macos_debug_compiler_cache_is_bounded() {
	$if !macos {
		return
	}
	cache_dir := os.join_path(os.vtmp_dir(), 'macos_debug_cache_${os.getpid()}')
	os.rmdir_all(cache_dir) or {}
	os.mkdir_all(cache_dir)!
	defer {
		os.rmdir_all(cache_dir) or {}
	}
	retained := create_macos_debug_cache_entry(cache_dir, 'retained', '123456', 10)!
	obsolete := create_macos_debug_cache_entry(cache_dir, 'obsolete', '123456', 20)!
	newest := create_macos_debug_cache_entry(cache_dir, 'newest', '123456', 30)!
	abandoned_temporary := os.join_path(cache_dir, 'v-compiler.123456.tmp')
	os.write_file(abandoned_temporary, 'temporary object')!
	os.write_file(abandoned_temporary + '.rsp', 'response file')!
	clang_temporary := os.join_path(cache_dir, 'v-compiler.123456-abcd.tmp.tmp')
	os.write_file(clang_temporary, 'active clang temporary object')!
	orphaned_response_temporary := os.join_path(cache_dir, 'v-compiler.123457.tmp')
	os.write_file(orphaned_response_temporary + '.rsp', 'orphaned response file')!
	active_temporary := os.join_path(cache_dir, 'v-compiler.123458.tmp')
	os.write_file(active_temporary, 'active temporary object')!
	os.write_file(active_temporary + '.rsp', 'active response file')!
	mut active_temporary_lock := filelock.new(active_temporary + '.lock')
	active_temporary_lock.acquire()!
	defer {
		active_temporary_lock.release()
	}

	prune_reproducible_macos_debug_compiler_cache(cache_dir, retained, 12)

	assert os.is_file(retained)
	assert !os.exists(obsolete)
	assert os.is_file(newest)
	assert !os.exists(abandoned_temporary)
	assert !os.exists(abandoned_temporary + '.rsp')
	assert os.is_file(clang_temporary)
	assert !os.exists(orphaned_response_temporary + '.rsp')
	assert os.is_file(active_temporary)
	assert os.is_file(active_temporary + '.rsp')
}

fn test_macos_debug_compiler_cache_recreates_entry_directory_before_store() {
	$if !macos {
		return
	}
	cache_dir := os.join_path(os.vtmp_dir(), 'macos_debug_cache_store_${os.getpid()}')
	os.rmdir_all(cache_dir) or {}
	os.mkdir_all(cache_dir)!
	defer {
		os.rmdir_all(cache_dir) or {}
	}
	temporary_object := os.join_path(cache_dir, 'v-compiler.123456.tmp')
	object_dir := os.join_path(cache_dir, 'content-hash')
	object_path := os.join_path(object_dir, 'v-compiler.o')
	os.write_file(temporary_object, 'compiler object')!
	os.mkdir_all(object_dir)!
	mut cache_entry_lock := filelock.new(object_dir + '.lock')
	cache_entry_lock.acquire()!
	defer {
		cache_entry_lock.release()
	}
	// Simulate a pruner removing the entry while the producer waited for this lock.
	os.rmdir_all(object_dir)!
	store_reproducible_macos_debug_compiler_object(temporary_object, object_dir, object_path)!

	assert os.read_file(object_path)! == 'compiler object'
	assert !os.exists(temporary_object)
}

fn test_macos_debug_compiler_cache_lock_reopens_replaced_sidecar() {
	$if !macos {
		return
	}
	cache_dir := os.join_path(os.vtmp_dir(), 'macos_debug_cache_lock_${os.getpid()}')
	os.rmdir_all(cache_dir) or {}
	os.mkdir_all(cache_dir)!
	defer {
		os.rmdir_all(cache_dir) or {}
	}
	lock_path := os.join_path(cache_dir, 'content-hash.lock')
	mut first_owner := filelock.new(lock_path)
	first_owner.acquire()!
	mut waiter := filelock.new(lock_path)
	assert !waiter.try_acquire()
	first_owner.release()

	mut replacement_owner := filelock.new(lock_path)
	replacement_owner.acquire()!
	assert !waiter.try_acquire()
	replacement_owner.release()
	assert waiter.try_acquire()
	waiter.release()
}

fn test_macos_debug_compiler_build_is_reproducible() {
	$if !macos {
		return
	}
	test_dir := os.join_path(os.vtmp_dir(), 'macos_reproducible_compiler_${os.getpid()}')
	compiler_dir := os.join_path(test_dir, 'cmd', 'v')
	os.rmdir_all(test_dir) or {}
	os.mkdir_all(compiler_dir)!
	defer {
		os.rmdir_all(test_dir) or {}
	}
	source_path := os.join_path(compiler_dir, 'v.v')
	os.write_file(source_path, 'fn main() {\n\tprintln(42)\n}\n')!
	object_output := os.join_path(test_dir, 'compiler.o')
	object_cmd := '${os.quoted_path(macos_reproducible_compiler_vexe)} -old-compiler -g -keepc -o ${os.quoted_path(object_output)} ${os.quoted_path(source_path)}'
	object_result := os.execute(object_cmd)
	assert object_result.exit_code == 0, object_result.output
	assert os.is_file(object_output)
	assert os.file_size(object_output) > 0
	assert !os.exists(object_output + '.dSYM')
	mut binary_hashes := []string{}
	build_names := ['first', 'second', 'no_rsp']
	build_flags := ['', '', '-no-rsp']
	for i, name in build_names {
		extra_flags := build_flags[i]
		output_path := os.join_path(test_dir, name)
		cmd := '${os.quoted_path(macos_reproducible_compiler_vexe)} -old-compiler -g -keepc ${extra_flags} -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}'
		result := os.execute(cmd)
		assert result.exit_code == 0, result.output
		binary_hashes << sha256.hexhash(os.read_file(output_path)!)
	}
	spaced_vtmp := os.join_path(test_dir, 'temporary files with spaces')
	os.mkdir_all(spaced_vtmp)!
	old_vtmp := os.getenv_opt('VTMP')
	os.setenv('VTMP', spaced_vtmp, true)
	spaced_output := os.join_path(test_dir, 'spaced_vtmp')
	spaced_cmd := '${os.quoted_path(macos_reproducible_compiler_vexe)} -old-compiler -g -keepc -o ${os.quoted_path(spaced_output)} ${os.quoted_path(source_path)}'
	spaced_result := os.execute(spaced_cmd)
	if previous_vtmp := old_vtmp {
		os.setenv('VTMP', previous_vtmp, true)
	} else {
		os.unsetenv('VTMP')
	}
	assert spaced_result.exit_code == 0, spaced_result.output
	binary_hashes << sha256.hexhash(os.read_file(spaced_output)!)
	assert binary_hashes.all(it == binary_hashes[0]), binary_hashes.str()
	mut cdebug_hashes := []string{}
	for name in ['cdebug_first', 'cdebug_second'] {
		output_path := os.join_path(test_dir, name)
		cmd := '${os.quoted_path(macos_reproducible_compiler_vexe)} -old-compiler -cg -keepc -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}'
		result := os.execute(cmd)
		assert result.exit_code == 0, result.output
		cdebug_hashes << sha256.hexhash(os.read_file(output_path)!)
	}
	assert cdebug_hashes[0] == cdebug_hashes[1], cdebug_hashes.str()

	first_output := os.join_path(test_dir, 'first')
	nm_result := os.execute('nm -ap ${os.quoted_path(first_output)}')
	assert nm_result.exit_code == 0, nm_result.output
	oso_lines := nm_result.output.split_into_lines().filter(it.contains(' OSO '))
	assert oso_lines.len > 0, nm_result.output
	debug_object := oso_lines[0].all_after(' OSO ').trim_space()
	assert os.is_file(debug_object), debug_object

	dsym_path := first_output + '.dSYM'
	assert os.is_dir(dsym_path)
	os.rmdir_all(dsym_path)!
	dsymutil_result :=
		os.execute('dsymutil -o ${os.quoted_path(dsym_path)} ${os.quoted_path(first_output)}')
	assert dsymutil_result.exit_code == 0, dsymutil_result.output
	assert os.is_dir(dsym_path)

	compressed_output := os.join_path(test_dir, 'compressed')
	compressed_cmd := '${os.quoted_path(macos_reproducible_compiler_vexe)} -old-compiler -g -keepc -compress -o ${os.quoted_path(compressed_output)} ${os.quoted_path(source_path)}'
	compressed_result := os.execute(compressed_cmd)
	assert compressed_result.exit_code == 0, compressed_result.output
	assert os.is_dir(compressed_output + '.dSYM')
	codesign_result := os.execute('codesign --verify --strict ${os.quoted_path(compressed_output)}')
	assert codesign_result.exit_code == 0, codesign_result.output
}
