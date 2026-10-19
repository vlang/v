module driver

import os
import v.modulecache

fn test_cached_collision_identity_requires_a_valid_header_for_the_same_sources() {
	root := os.join_path(os.vtmp_dir(), 'v3_cache_collision_identity_${os.getpid()}')
	saved := os.getenv_opt('V3CACHE')
	os.setenv('V3CACHE', os.join_path(root, 'cache'), true)
	defer {
		if value := saved {
			os.setenv('V3CACHE', value, true)
		} else {
			os.unsetenv('V3CACHE')
		}
		os.rmdir_all(root) or {}
	}
	for mode in ['valid', 'disabled', 'missing', 'corrupt', 'stale', 'other_directory'] {
		source := os.join_path(root, mode, 'first', 'internal', 'internal.v')
		other := os.join_path(root, mode, 'second', 'internal', 'internal.v')
		os.mkdir_all(os.dir(source))!
		os.mkdir_all(os.dir(other))!
		os.write_file(source, 'module internal\npub fn value() int { return 1 }\n')!
		os.write_file(other, 'module internal\npub fn value() int { return 2 }\n')!
		writer := modulecache.new_manager(root, mode, true, '', '')
		entry := writer.write_header('internal', [source], 'module internal\npub fn value() int\n')!
		if mode == 'missing' {
			os.rm(entry.header)!
		} else if mode == 'corrupt' {
			os.write_file(entry.header, 'not a cached interface')!
		} else if mode == 'stale' {
			os.write_file(source, 'module internal\npub fn different() string { return "changed" }\n')!
		}
		reader := modulecache.new_manager(root, mode, mode != 'disabled', '', '')
		files := if mode == 'other_directory' { [other] } else { [source] }
		assert v3_cached_collision_identity(&reader, 'first.internal', 'internal', files) == if mode == 'valid' {
			'internal'
		} else {
			'first.internal'
		}, mode
		// A module already using its full name never adopts another identity.
		assert v3_cached_collision_identity(&reader, 'first.internal', 'first.internal', files) == 'first.internal'
	}
}
