module driver

import os

const short_root = 'C:\\Users\\u\\AppData\\Local\\v3cc'

// dir_of_len returns a build directory path of exactly `len` bytes.
fn dir_of_len(len int) string {
	prefix := 'C:\\out\\'
	suffix := '\\.a.exe.v3cc.123'
	return prefix + 'x'.repeat(len - prefix.len - suffix.len) + suffix
}

fn test_short_build_dir_boundary_is_the_short_root_len() {
	below := dir_of_len(v3_cc_dir_short_root_len - 1)
	assert below.len == 239
	dir, moved := v3_short_build_dir_for(below, true, short_root)
	assert !moved
	assert dir == below
	at := dir_of_len(v3_cc_dir_short_root_len)
	assert at.len == 240
	moved_dir, moved_at := v3_short_build_dir_for(at, true, short_root)
	assert moved_at
	assert moved_dir.starts_with(short_root + '\\')
}

fn test_short_build_dir_moves_a_long_dir_only_on_a_windows_host() {
	long := dir_of_len(400)
	moved_dir, moved := v3_short_build_dir_for(long, true, short_root)
	assert moved
	// The name is a digest, so it is short however long the output's name is, and it is
	// the same for the same directory.
	assert moved_dir.len == short_root.len + 1 + 16
	again, _ := v3_short_build_dir_for(long, true, short_root)
	assert again == moved_dir
	other, _ := v3_short_build_dir_for(dir_of_len(401), true, short_root)
	assert other != moved_dir
	// Only Windows has the MAX_PATH limit, so any other host keeps the directory.
	kept_dir, kept := v3_short_build_dir_for(long, false, short_root)
	assert !kept
	assert kept_dir == long
}

fn test_short_build_dir_stays_without_a_usable_root() {
	long := dir_of_len(400)
	for root in ['', 'C:\\' + 'r'.repeat(v3_cc_dir_short_root_len)] {
		dir, moved := v3_short_build_dir_for(long, true, root)
		assert !moved
		assert dir == long
	}
}

fn test_short_build_root_is_per_user() {
	$if windows {
		root := v3_short_build_root()
		assert root == os.getenv('LOCALAPPDATA').trim_right('\\/') + '\\v3cc'
		assert !root.to_lower().starts_with(os.getenv('SystemDrive').to_lower() + '\\v3cc')
	}
}

fn test_mkdir_all_shared_accepts_a_dir_that_already_exists() ! {
	dir := os.join_path(os.vtmp_dir(), 'v3_mkdir_all_shared_${os.getpid()}', 'nested')
	defer {
		os.rmdir_all(os.dir(dir)) or {}
	}
	v3_mkdir_all_shared(dir)!
	v3_mkdir_all_shared(dir)!
	assert os.is_dir(dir)
}

fn mkdir_all_shared_ok(dir string) bool {
	v3_mkdir_all_shared(dir) or { return false }
	return true
}

// test_mkdir_all_shared_survives_builds_creating_the_same_parent runs many creations at once
// under one parent that does not exist yet, the way parallel builds create the short root.
fn test_mkdir_all_shared_survives_builds_creating_the_same_parent() {
	base := os.join_path(os.vtmp_dir(), 'v3_mkdir_all_race_${os.getpid()}')
	defer {
		os.rmdir_all(base) or {}
	}
	for round in 0 .. 20 {
		parent := os.join_path(base, 'round_${round}', 'shared_root')
		mut threads := []thread bool{}
		for i in 0 .. 24 {
			threads << spawn mkdir_all_shared_ok(os.join_path(parent, 'build_${i}'))
		}
		results := threads.wait()
		assert results.all(it), 'round ${round}: a concurrent creation failed'
	}
}
