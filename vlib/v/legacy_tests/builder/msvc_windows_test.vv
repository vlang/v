module builder

import os
import v.cflag
import v.pref

fn test_msvc_string_flags_uses_cached_thirdparty_obj_path() {
	obj_file := os.join_path(@VEXEROOT, 'thirdparty', 'mbedtls', 'library', 'bignum.o')
	mut builder := msvc_new_builder_for_args(['-cc', 'msvc', '-m32', msvc_hello_world_example()])
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
	mut builder := msvc_new_builder_for_args(['-cc', 'msvc', '-m64', msvc_hello_world_example()])
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

fn test_msvc_ordered_pkgconfig_linker_args_routes_static_paths_and_libs() {
	mut builder := msvc_new_builder_for_args(['-cc', 'msvc', '-m64', msvc_hello_world_example()])
	lib_dir := os.join_path(os.getwd(), 'msvc_pkgconfig_static_libs')
	builder.table.parse_pkgconfig_link_flags(['-L${lib_dir}', '-lissue74_public', '-lissue74_private'],
		'main', builder.pref.compile_defines_all) or { panic(err) }

	assert builder.get_os_cflags() == []
	assert builder.msvc_ordered_pkgconfig_linker_args() == [
		'/LIBPATH:"${os.real_path(lib_dir)}"',
		'/LIBPATH:"${os.real_path(os.join_path(lib_dir, 'msvc'))}"',
		'issue74_public.lib',
		'issue74_private.lib',
	]
}

fn test_msvc_ordered_pkgconfig_linker_args_preserves_segment_order_and_repetition() {
	mut builder := msvc_new_builder_for_args(['-cc', 'msvc', '-m64', msvc_hello_world_example()])
	builder.table.parse_pkgconfig_link_flags(['-lissue74_a', '-lissue74_b', '-lissue74_a'], 'main',
		builder.pref.compile_defines_all) or { panic(err) }
	builder.table.parse_pkgconfig_link_flags(['-lissue74_c', '-lissue74_a'], 'main',
		builder.pref.compile_defines_all) or { panic(err) }

	assert builder.msvc_ordered_pkgconfig_linker_args() == [
		'issue74_a.lib',
		'issue74_b.lib',
		'issue74_a.lib',
		'issue74_c.lib',
		'issue74_a.lib',
	]
}

fn test_msvc_ordered_pkgconfig_linker_args_preserves_single_segment_token_order() {
	mut builder := msvc_new_builder_for_args(['-cc', 'msvc', '-m64', msvc_hello_world_example()])
	lib_dir := os.join_path(os.getwd(), 'msvc_pkgconfig_ordered_libs')
	builder.table.parse_pkgconfig_link_flags(['-L${lib_dir}', 'issue74_direct.LIB', '-lissue74_a',
		'-Wl,--start-group', '-lissue74_b', 'issue74_repeat.LIB', '-lissue74_a', '-Wl,--end-group',
		'issue74_tail.LIB'], 'main', builder.pref.compile_defines_all) or { panic(err) }

	args := builder.msvc_ordered_pkgconfig_linker_args()
	assert args == [
		'/LIBPATH:"${os.real_path(lib_dir)}"',
		'/LIBPATH:"${os.real_path(os.join_path(lib_dir, 'msvc'))}"',
		'issue74_direct.LIB',
		'issue74_a.lib',
		'issue74_b.lib',
		'issue74_repeat.LIB',
		'issue74_a.lib',
		'issue74_tail.LIB',
	]
	assert !args.any(it.contains('start-group') || it.contains('end-group'))
}

fn test_msvc_ordered_pkgconfig_linker_args_preserves_native_slash_flags() {
	mut builder := msvc_new_builder_for_args(['-cc', 'msvc', '-m64', msvc_hello_world_example()])
	output_dir := os.join_path(os.getwd(), 'msvc pkgconfig native flags')
	pdb_path := os.join_path(output_dir, 'issue74 output.pdb')
	direct_lib := os.join_path(output_dir, 'issue74 direct.LIB')
	builder.table.parse_pkgconfig_link_flags(['-lissue74_a', '/NODEFAULTLIB', '/OPT:REF',
		'/PDB:"${pdb_path}"', '/WHOLEARCHIVE:issue74_whole.lib', '"${direct_lib}"',
		'-Wl,--start-group', '--as-needed', '-lissue74_b', '-Wl,--end-group', '/OPT:REF',
		'-lissue74_a'], 'main', builder.pref.compile_defines_all) or { panic(err) }

	args := builder.msvc_ordered_pkgconfig_linker_args()
	assert args == [
		'issue74_a.lib',
		'/NODEFAULTLIB',
		'/OPT:REF',
		'/PDB:"${pdb_path}"',
		'/WHOLEARCHIVE:issue74_whole.lib',
		'"${direct_lib}"',
		'issue74_b.lib',
		'/OPT:REF',
		'issue74_a.lib',
	]
	assert !args.any(it.contains('start-group') || it.contains('end-group') || it == '--as-needed')
}

fn test_msvc_ordered_pkgconfig_linker_args_quotes_only_decoded_slash_option_operands() {
	pdb_path := r'C:\Issue74 Output\app.pdb'
	whole_archive_path := r'C:\Issue74 Libraries\whole.lib'
	decoded_pdb := '/PDB:${pdb_path}'
	decoded_whole_archive := '/WHOLEARCHIVE:${whole_archive_path}'
	quoted_pdb := '/PDB:"${pdb_path}"'
	equals_pdb := '/PDB=${pdb_path}'

	assert quote_spaced_ordered_pkgconfig_operand(cflag.CFlag{
		value: decoded_pdb
	}, true).value == '/PDB:"${pdb_path}"'
	assert quote_spaced_ordered_pkgconfig_operand(cflag.CFlag{
		value: decoded_whole_archive
	}, true).value == '/WHOLEARCHIVE:"${whole_archive_path}"'
	assert quote_spaced_ordered_pkgconfig_operand(cflag.CFlag{
		value: quoted_pdb
	}, true).value == quoted_pdb
	assert quote_spaced_ordered_pkgconfig_operand(cflag.CFlag{
		value: equals_pdb
	}, true).value == equals_pdb
	assert quote_spaced_ordered_pkgconfig_operand(cflag.CFlag{
		value: '/OPT:REF'
	}, true).value == '/OPT:REF'

	mut builder := msvc_new_builder_for_args(['-cc', 'msvc', '-m64', msvc_hello_world_example()])
	builder.table.parse_pkgconfig_link_flags([
		decoded_pdb,
		decoded_whole_archive,
		quoted_pdb,
		'/OPT:REF',
		'-lVersion',
	], 'main', builder.pref.compile_defines_all) or { panic(err) }
	assert builder.msvc_ordered_pkgconfig_linker_args() == [
		'/PDB:"${pdb_path}"',
		'/WHOLEARCHIVE:"${whole_archive_path}"',
		quoted_pdb,
		'/OPT:REF',
		'Version.lib',
	]
}

fn test_msvc_ordered_pkgconfig_linker_args_preserves_quoted_paths_with_spaces() {
	mut builder := msvc_new_builder_for_args(['-cc', 'msvc', '-m64', msvc_hello_world_example()])
	lib_dir := os.join_path(os.getwd(), 'msvc pkgconfig quoted libs')
	direct_lib := os.join_path(lib_dir, 'issue74 direct.LIB')
	builder.table.parse_pkgconfig_link_flags(['-L"${lib_dir}"', '"${direct_lib}"', '-lissue74_a',
		'"${direct_lib}"', 'issue74_plain.LIB'], 'main', builder.pref.compile_defines_all) or {
		panic(err)
	}

	assert builder.msvc_ordered_pkgconfig_linker_args() == [
		'/LIBPATH:"${os.real_path(lib_dir)}"',
		'/LIBPATH:"${os.real_path(os.join_path(lib_dir, 'msvc'))}"',
		'"${direct_lib}"',
		'issue74_a.lib',
		'"${direct_lib}"',
		'issue74_plain.LIB',
	]
}

fn test_msvc_ordered_pkgconfig_linker_args_preserves_decoded_paths_with_spaces() {
	mut builder := msvc_new_builder_for_args(['-cc', 'msvc', '-m64', msvc_hello_world_example()])
	lib_dir := os.join_path(os.getwd(), 'msvc pkgconfig decoded libs')
	direct_lib := os.join_path(lib_dir, 'issue74 direct.lib')
	builder.table.parse_pkgconfig_link_flags(['-L${lib_dir}', direct_lib, '-lVersion', direct_lib,
		'-lVersion'], 'main', builder.pref.compile_defines_all) or { panic(err) }

	assert builder.msvc_ordered_pkgconfig_linker_args() == [
		'/LIBPATH:"${os.real_path(lib_dir)}"',
		'/LIBPATH:"${os.real_path(os.join_path(lib_dir, 'msvc'))}"',
		'"${direct_lib}"',
		'Version.lib',
		'"${direct_lib}"',
		'Version.lib',
	]
}

fn test_msvc_ordered_pkgconfig_linker_args_ignores_spaced_gnu_linker_control() {
	mut builder := msvc_new_builder_for_args(['-cc', 'msvc', '-m64', msvc_hello_world_example()])
	builder.table.parse_pkgconfig_link_flags([
		r'-Wl,-rpath,C:\issue74 runtime libs',
		'-lVersion',
	], 'main', builder.pref.compile_defines_all) or { panic(err) }

	assert builder.msvc_ordered_pkgconfig_linker_args() == ['Version.lib']
}

fn test_msvc_ordered_pkgconfig_linker_args_keeps_dynamic_flags_on_legacy_path() {
	mut builder := msvc_new_builder_for_args(['-cc', 'msvc', '-m64', msvc_hello_world_example()])
	lib_dir := os.join_path(os.getwd(), 'msvc_pkgconfig_dynamic_libs')
	builder.table.parse_cflag_with_link_segment('/NODEFAULTLIB', 'main',
		builder.pref.compile_defines_all) or { panic(err) }
	builder.table.parse_cflag_with_link_segment('/OPT:NOREF', 'main',
		builder.pref.compile_defines_all) or { panic(err) }
	builder.table.parse_cflag_with_link_segment('-L${lib_dir} -lissue74_dynamic', 'main',
		builder.pref.compile_defines_all) or { panic(err) }
	builder.table.parse_pkgconfig_link_flags(['/OPT:REF', '-lissue74_static'], 'main',
		builder.pref.compile_defines_all) or { panic(err) }

	legacy := builder.msvc_string_flags(builder.get_os_cflags())
	assert legacy.lib_paths == [
		'/LIBPATH:"${os.real_path(lib_dir)}"',
		'/LIBPATH:"${os.real_path(os.join_path(lib_dir, 'msvc'))}"',
	]
	assert legacy.real_libs == ['issue74_dynamic.lib']
	assert legacy.other_flags == ['/NODEFAULTLIB', '/OPT:NOREF']
	assert builder.msvc_ordered_pkgconfig_linker_args() == ['/OPT:REF', 'issue74_static.lib']
}

fn msvc_new_builder_for_args(args []string) Builder {
	mut full_args := ['']
	full_args << args
	prefs, _ := pref.parse_args_and_show_errors([], full_args, false)
	return new_builder(prefs)
}

fn msvc_hello_world_example() string {
	return os.join_path(@VEXEROOT, 'examples', 'hello_world.v')
}
