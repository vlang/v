import os
import v.vcache

const (
	vcache_folder = os.join_path(os.temp_dir(), 'v', 'cache_folder')
)

fn check_cache_entry_fpath_invariants(x string, extension string) {
	a := x.replace(vcache_folder + os.path_separator, '').split(os.path_separator)
	assert a.len > 0
	assert a[0].len == 2
	assert a[1].len > 32
	assert os.file_ext(a[1]) == extension
	assert a[1][0..2] == a[0]
}

fn testsuite_begin() {
	os.setenv('VCACHE', vcache_folder, true)
	// eprintln('testsuite_begin, vcache_folder = $vcache_folder')
	os.rmdir_all(vcache_folder) or {}
	vcache.new_cache_manager([])
	assert os.is_dir(vcache_folder)
}

fn test_save_and_load() {
	mut cm := vcache.new_cache_manager([])
	x := cm.save('.txt', 'first/cache/entry', 'hello') or {
		assert false
		''
	}
	check_cache_entry_fpath_invariants(x, '.txt')
}

fn test_different_options_should_produce_different_cache_entries_for_same_key_and_content() {
	mut cm1 := vcache.new_cache_manager([])
	mut cm2 := vcache.new_cache_manager(['-cc tcc'])
	mut cm3 := vcache.new_cache_manager(['-cc gcc'])
	x := cm1.save('.txt', 'first/cache/entry', 'hello') or {
		assert false
		''
	}
	y := cm2.save('.txt', 'first/cache/entry', 'hello') or {
		assert false
		''
	}
	z := cm3.save('.txt', 'first/cache/entry', 'hello') or {
		assert false
		''
	}
	check_cache_entry_fpath_invariants(x, '.txt')
	check_cache_entry_fpath_invariants(y, '.txt')
	check_cache_entry_fpath_invariants(z, '.txt')
}

fn test_exists() {
	mut cm := vcache.new_cache_manager([])
	cm.exists('.o', 'abc') or { assert true }
	//
	x := cm.save('.x', 'abc', '') or {
		assert false
		''
	}
	cm.exists('.o', 'abc') or { assert true }
	//
	y := cm.save('.o', 'zbc', '') or {
		assert false
		''
	}
	cm.exists('.o', 'abc') or { assert true }
	//
	z := cm.save('.o', 'abc', '') or {
		assert false
		''
	}
	cm.exists('.o', 'abc') or { assert false }
	//
	assert os.is_file(x)
	assert os.is_file(y)
	assert os.is_file(z)
	assert x != y
	assert x != z
	assert y != z
}

fn test_readme_exists_and_is_readable() {
	vcache.new_cache_manager([])
	freadme := os.join_path(vcache_folder, 'README.md')
	assert os.is_file(freadme)
	x := os.read_file(freadme) or {
		assert false
		''
	}
	assert x.len > 0
	assert x.starts_with('This folder contains cached build artifacts')
}

fn testsuite_end() {
	os.chdir(os.wd_at_startup) or {}
	os.rmdir_all(vcache_folder) or {}
	assert !os.is_dir(vcache_folder)
}
