module builder

import os
import v.cflag

fn test_msvc_string_flags_uses_cached_thirdparty_obj_path() {
	obj_file := os.join_path(@VEXEROOT, 'thirdparty', 'mbedtls', 'library', 'bignum.o')
	mut builder := new_builder_for_args(['-cc', 'msvc', '-m32', hello_world_example()])
	cached_obj := builder.pref.cache_manager.mod_postfix_with_key2cpath('mbedtls', '.o',
		os.real_path(obj_file))
	expected_obj := builder.msvc_thirdparty_obj_path('mbedtls', obj_file, cached_obj)
	sflags := builder.msvc_string_flags([
		cflag.CFlag{
			mod:    'mbedtls'
			value:  obj_file
			cached: cached_obj
		},
	])
	assert sflags.other_flags == ['"${expected_obj}"']
}

fn test_msvc_string_flags_rewrites_obj_flags_through_cached_path() {
	test_dir := os.join_path(os.vtmp_dir(), 'builder_msvc_obj_flag_test_${os.getpid()}')
	obj_file := os.join_path(test_dir, 'gc.obj')
	os.mkdir_all(test_dir) or { panic(err) }
	os.write_file(obj_file, '') or { panic(err) }
	defer {
		os.rmdir_all(test_dir) or {}
	}
	mut builder := new_builder_for_args(['-cc', 'msvc', '-m64', hello_world_example()])
	builder.table.cflags = [
		cflag.CFlag{
			mod:   'builtin'
			value: obj_file
		},
	]
	flags := builder.get_os_cflags()
	expected_cached := builder.pref.cache_manager.mod_postfix_with_key2cpath('builtin', '.obj',
		os.real_path(obj_file))
	assert flags.len == 1
	assert flags[0].cached == expected_cached
	expected_obj := builder.msvc_thirdparty_obj_path('builtin', obj_file, expected_cached)
	sflags := builder.msvc_string_flags(flags)
	assert sflags.other_flags == ['"${expected_obj}"']
}
