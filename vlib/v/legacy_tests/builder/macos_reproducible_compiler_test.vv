module builder

import crypto.sha256
import os
import os.filelock

const macos_reproducible_compiler_vexe = @VEXE

fn put_macho_test_u32(mut data []u8, offset int, value u32) {
	data[offset] = u8(value)
	data[offset + 1] = u8(value >> 8)
	data[offset + 2] = u8(value >> 16)
	data[offset + 3] = u8(value >> 24)
}

fn put_macho_test_u32_be(mut data []u8, offset int, value u32) {
	data[offset] = u8(value >> 24)
	data[offset + 1] = u8(value >> 16)
	data[offset + 2] = u8(value >> 8)
	data[offset + 3] = u8(value)
}

fn macho_test_thin_binary(seed u8) []u8 {
	mut binary := []u8{len: 80, init: seed + u8(index)}
	put_macho_test_u32(mut binary, 0, 0xfeedfacf)
	put_macho_test_u32(mut binary, 16, 2)
	put_macho_test_u32(mut binary, 20, 32)
	put_macho_test_u32(mut binary, 32, 1)
	put_macho_test_u32(mut binary, 36, 8)
	put_macho_test_u32(mut binary, 40, 0x1b)
	put_macho_test_u32(mut binary, 44, 24)
	return binary
}

fn test_normalize_macho_uuid_is_content_derived_and_idempotent() {
	mut binary := macho_test_thin_binary(0)
	mut normalized_input := binary.clone()
	for i in 48 .. 64 {
		normalized_input[i] = 0
	}
	mut expected_uuid := sha256.sum(normalized_input)
	expected_uuid[6] = (expected_uuid[6] & 0x0f) | 0x30
	expected_uuid[8] = (expected_uuid[8] & 0x3f) | 0x80

	normalize_macho_uuid(mut binary)!
	assert binary[48..64] == expected_uuid[..16]
	first_uuid := binary[48..64].clone()
	normalize_macho_uuid(mut binary)!
	assert binary[48..64] == first_uuid
}

fn test_normalize_macho_uuid_normalizes_each_universal_slice() {
	mut first := macho_test_thin_binary(1)
	mut second := macho_test_thin_binary(17)
	normalize_macho_uuid(mut first)!
	normalize_macho_uuid(mut second)!

	mut universal := []u8{len: 208}
	put_macho_test_u32_be(mut universal, 0, 0xcafebabe)
	put_macho_test_u32_be(mut universal, 4, 2)
	put_macho_test_u32_be(mut universal, 16, 48)
	put_macho_test_u32_be(mut universal, 20, 80)
	put_macho_test_u32_be(mut universal, 36, 128)
	put_macho_test_u32_be(mut universal, 40, 80)
	copy(mut universal[48..128], macho_test_thin_binary(1))
	copy(mut universal[128..208], macho_test_thin_binary(17))

	normalize_macho_uuid(mut universal)!
	assert universal[48..128] == first
	assert universal[128..208] == second
	first_uuid := universal[96..112].clone()
	second_uuid := universal[176..192].clone()
	normalize_macho_uuid(mut universal)!
	assert universal[96..112] == first_uuid
	assert universal[176..192] == second_uuid
}

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
	mut cache_entry_lock := new_reproducible_macos_debug_cache_entry_lock(object_dir)!
	cache_entry_lock.acquire()!
	defer {
		cache_entry_lock.release()
	}
	// Simulate a pruner removing the entry while the producer waited for this lock.
	os.rmdir_all(object_dir)!
	store_reproducible_macos_debug_compiler_object(temporary_object, object_dir, object_path)!

	assert os.read_file(object_path)! == 'compiler object'
	assert !os.exists(temporary_object)
	assert os.is_file(object_dir + '.lock')
}

fn test_macos_debug_compiler_cache_lock_uses_persistent_inode() {
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
	object_dir := os.join_path(cache_dir, 'content-hash')
	mut first_owner := new_reproducible_macos_debug_cache_entry_lock(object_dir)!
	first_owner.acquire()!
	lock_stat := os.stat(lock_path)!
	first_owner.release()
	assert os.is_file(lock_path)
	after_first_release := os.stat(lock_path)!
	assert after_first_release.dev == lock_stat.dev
	assert after_first_release.inode == lock_stat.inode
	mut next_owner := new_reproducible_macos_debug_cache_entry_lock(object_dir)!
	next_owner.acquire()!
	next_owner.release()
	assert os.is_file(lock_path)
	after_next_release := os.stat(lock_path)!
	assert after_next_release.dev == lock_stat.dev
	assert after_next_release.inode == lock_stat.inode
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
	first_codesign_result :=
		os.execute('codesign --verify --strict ${os.quoted_path(first_output)}')
	assert first_codesign_result.exit_code == 0, first_codesign_result.output
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
