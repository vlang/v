module modulecache

import os
import time

fn test_file_metadata_signature_is_scoped_to_compiler_build() {
	signature := file_metadata_signature(@FILE)
	assert signature.len > 0
	assert signature.starts_with('${@VCURRENTHASH}:')
}

// A same-size edit in place keeps the file identity and size, and restoring the
// old modification time afterwards hides the edit from the write time as well.
// Only the change time still moves (restoring the time is itself a change), so
// the signature must include it.
fn test_file_metadata_signature_sees_an_edit_behind_a_restored_mtime() {
	dir := os.join_path(os.vtmp_dir(), 'v3_modulecache_restored_mtime_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	path := os.join_path(dir, 'input.v')
	os.write_file(path, 'First')!
	old := time.utc().unix() - 600
	os.utime(path, old, old)!
	before := file_metadata_signature(path)
	assert before.len > 0
	// Step past the timestamp clock granularity (a scheduler tick on Windows, a
	// jiffy on Linux) so the edit cannot share the recorded change time.
	time.sleep(100 * time.millisecond)
	os.write_file(path, 'Other')!
	os.utime(path, old, old)!
	assert os.file_size(path) == 5
	assert os.file_last_mod_unix(path) == old
	assert file_metadata_signature(path) != before
}

// A whole-second modification time comes from a file system with coarse
// timestamps (FAT and exFAT keep 2 second steps, HFS+ 1 second steps). While it
// is that recent, a same-size edit can keep identical metadata, so the metadata
// must not identify the file yet.
fn test_coarse_mtime_is_recent_only_within_the_timestamp_steps() {
	now := i64(1_790_000_000)
	assert coarse_mtime_is_recent(u64(now), 0, now)
	assert coarse_mtime_is_recent(u64(now - coarse_mtime_recent_seconds + 1), 0, now)
	assert !coarse_mtime_is_recent(u64(now - coarse_mtime_recent_seconds), 0, now)
	// A clock ahead of this machine's, or a future timestamp, proves nothing yet.
	assert coarse_mtime_is_recent(u64(now + 3600), 0, now)
	// A sub-second part means fine-grained timestamps, which a later edit changes.
	assert !coarse_mtime_is_recent(u64(now), 1, now)
	assert !coarse_mtime_is_recent(u64(now + 3600), 500, now)
}

fn test_file_metadata_helper_uses_generated_u64_abi() {
	header_path := os.join_path(@VEXEROOT, 'vlib', 'v', 'modulecache', 'file_metadata.c')
	header := os.read_file(header_path) or { panic(err) }
	signature := 'static int v3_modulecache_file_metadata(const char *path, u64 *device, u64 *inode,
	u64 *size, u64 *mtime_seconds, u64 *mtime_nanoseconds,
	u64 *ctime_seconds, u64 *ctime_nanoseconds)'
	assert header.count(signature) == 3
	assert !header.contains('uint64_t *')

	cc := os.find_abs_path_of_executable('cc') or { return }
	temp_dir := os.join_path(os.temp_dir(), 'v3_modulecache_file_metadata_${os.getpid()}')
	os.mkdir_all(temp_dir) or { panic(err) }
	defer {
		os.rmdir_all(temp_dir) or {}
	}

	header_include := header_path.replace('\\', '/')
	aliases := ['typedef unsigned long long u64;', 'typedef uint64_t u64;']
	branches := ['', '#undef __APPLE__\n#undef __linux__']
	for alias_index, alias in aliases {
		for branch_index, branch in branches {
			source_path := os.join_path(temp_dir, 'abi_${alias_index}_${branch_index}.c')
			source := '#include <stdint.h>
${alias}
${branch}
#include "${header_include}"

int main(void) {
	u64 device = 0;
	u64 inode = 0;
	u64 size = 0;
	u64 mtime_seconds = 0;
	u64 mtime_nanoseconds = 0;
	u64 ctime_seconds = 0;
	u64 ctime_nanoseconds = 0;
	return v3_modulecache_file_metadata("", &device, &inode, &size, &mtime_seconds,
		&mtime_nanoseconds, &ctime_seconds, &ctime_nanoseconds);
}
'
			os.write_file(source_path, source) or { panic(err) }
			result :=
				os.execute('${os.quoted_path(cc)} -D_DEFAULT_SOURCE -std=c99 -fsyntax-only -Werror=incompatible-pointer-types ${os.quoted_path(source_path)}')
			assert result.exit_code == 0, result.output
		}
	}
}
