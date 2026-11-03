module driver

import os

fn test_fastc_native_cache_hashes_every_dependency_before_reuse() ! {
	$if !skip_fastc ? {
		root := os.join_path(os.vtmp_dir(), 'fastc_native_manifest_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		library := os.join_path(root, 'native.dylib')
		os.write_file(library, 'native cache fixture')!
		mut dependencies := []string{}
		for i in 0 .. 72 {
			path := os.join_path(root, 'input_${i}.h')
			os.write_file(path, 'original')!
			dependencies << path
		}
		manifest := os.join_path(root, 'native.manifest')
		mut stats := CObjectCacheStats{}
		write_c_object_manifest(manifest, library, dependencies, mut stats)!
		for jobs in [1, 8] {
			cached := v3_fastc_native_cached_library(manifest, jobs) or { panic('valid native manifest rejected') }
			assert cached.library == library
			assert cached.dependencies == dependencies
		}
		// The last worker's input changes without changing its length.
		os.write_file(dependencies.last(), 'modified')!
		assert v3_fastc_native_cached_library(manifest, 1) == none
		assert v3_fastc_native_cached_library(manifest, 8) == none
		os.write_file(manifest, 'format=v3-c-object-manifest-v1\nobject=${library}\ndependency=\t\n')!
		assert v3_fastc_native_cached_library(manifest, 8) == none
	}
}
