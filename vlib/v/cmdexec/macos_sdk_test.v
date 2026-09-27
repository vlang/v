module cmdexec

import os

fn test_macos_sdk_cache_requires_matching_selection_and_existing_sdk() {
	test_dir := os.join_path(os.temp_dir(), 'v3_macos_sdk_cache_${os.getpid()}')
	os.rmdir_all(test_dir) or {}
	os.mkdir_all(test_dir) or { panic(err) }
	defer {
		os.rmdir_all(test_dir) or {}
	}
	sdk_dir := os.join_path_single(test_dir, 'MacOSX.sdk')
	cache_path := os.join_path_single(test_dir, 'cache')
	os.mkdir_all(sdk_dir) or { panic(err) }
	macos_sdk_write_cache(cache_path, 'selected-a', sdk_dir)
	assert macos_sdk_read_cache(cache_path, 'selected-a') == sdk_dir
	assert macos_sdk_read_cache(cache_path, 'selected-b') == ''
	os.rmdir_all(sdk_dir) or { panic(err) }
	assert macos_sdk_read_cache(cache_path, 'selected-a') == ''
}
