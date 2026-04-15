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
