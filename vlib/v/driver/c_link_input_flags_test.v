module driver

import os
import time
import v.pref

fn test_joined_compile_flags_are_not_native_input_files() {
	for prefix in ['-I', '-isystem', '-iquote', '-DPLUGIN=', '-U'] {
		for suffix in ['.o', '.obj', '.c', '.cc', '.cpp', '.m', '.mm', '.a', '.so', '.so.1',
			'.dylib', '.dll', '.lib', '.tbd'] {
			flag := '${prefix}folder with spaces/input${suffix}'
			assert !c_flag_is_object_file(flag), flag
			assert !c_flag_is_c_source_file(flag), flag
			assert !c_flag_token_is_link_only(flag), flag
			assert c_object_compile_flags([flag]) == [flag], flag
			assert c_dylib_link_flags([flag]).len == 0, flag
			assert tcc_native_c_source_flags([flag]).len == 0, flag
		}
	}
}

fn test_positional_native_inputs_and_link_options_keep_their_roles() {
	for input in ['unit.o', 'folder with spaces/unit.obj', './-unit.o'] {
		assert c_flag_is_object_file(input), input
		assert c_object_compile_flags([input]).len == 0, input
		assert c_dylib_link_flags([input]) == [input], input
	}
	for suffix in ['.c', '.cc', '.cpp', '.m', '.mm'] {
		input := 'folder with spaces/unit${suffix}'
		assert c_flag_is_c_source_file(input), input
		assert c_object_compile_flags([input]).len == 0, input
	}
	for input in ['lib.a', 'lib.so', 'lib.so.1', 'lib.dylib', 'lib.dll', 'lib.lib', 'lib.tbd',
		'-lfoo', '-Llibrary.a', '-Wl,-rpath,library.so', '-shared'] {
		assert c_flag_token_is_link_only(input), input
		assert c_object_compile_flags([input]).len == 0, input
		assert c_dylib_link_flags([input]) == [input], input
	}
	assert tcc_native_c_source_flags(['-DNAME=not_a_source.c', 'real.c']) == ['real.c']
	assert tcc_native_c_source_flags(['-x', 'c', '-Iinclude.c', 'extensionless', '-x', 'none']) == [
		'-x', 'c', 'extensionless', '-x', 'none',
	]
}

fn c_link_operand_options() []string {
	return ['-I', '-L', '-F', '-D', '-U', '-include', '-imacros', '-isystem', '-iquote',
		'-idirafter', '-iprefix', '-iwithprefix', '-iwithprefixbefore', '-isysroot', '--sysroot',
		'-target', '-arch', '-framework', '-weak_framework', '-Xlinker', '-force_load', '-o', '-MF',
		'-MT', '-MQ', '-l', '-weak_library', '-x']
}

fn test_native_input_selection_consumes_option_operands_once() {
	for option in c_link_operand_options() {
		for operand in ['value.o', 'value.mm', 'folder with spaces/value.obj', '-x', ''] {
			flags := [option, operand, 'real.o']
			assert c_link_input_indices(flags) == [2], flags.str()
		}
		assert c_link_input_indices([option]).len == 0, option
	}
	assert c_link_input_indices(['main.c', '-I', 'include.o', '-x', 'c++', 'unit.cpp', '-x',
		'none', 'support.o', '-l', 'library.mm', '-DNAME=macro.obj', '']) == [0, 5, 8]
	assert !c_link_flags_use_cpp_language(['-l', 'library.cpp'])
	assert !c_link_flags_use_objective_c_language(['-weak_library', 'library.mm'])
}

fn test_option_only_link_preparation_does_not_probe_or_compile() {
	for option in c_link_operand_options() {
		for operand in ['missing.o', 'missing.mm', 'missing path.obj', '-x', ''] {
			flags := [option, operand]
			mut stats := CObjectCacheStats{}
			prepared := prepare_c_flags_for_link(flags, [], [], false, '', [], pref.host_target(),
				'v_c_link_flags_nonexistent_compiler', false, '', mut stats)!
			assert prepared == flags, flags.str()
			assert stats.requests == 0, flags.str()
			assert stats.compiler_versions.len == 0, flags.str()
			assert stats.link_plan_signature == '', flags.str()
		}
	}
	for flag in ['-DNAME=missing.o', '-DNAME=missing.obj', '-DNAME=missing.mm', '-Iinclude.mm'] {
		mut stats := CObjectCacheStats{}
		assert prepare_c_flags_for_link([flag], [], [], false, '', [], pref.host_target(),
			'v_c_link_flags_nonexistent_compiler', false, '', mut stats)! == [flag]
		assert stats.requests == 0
		assert stats.compiler_versions.len == 0
	}
}

fn test_link_plan_tracks_objects_not_object_named_option_operands() {
	root := os.join_path(os.vtmp_dir(), 'v link manifest ${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir(root)!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	object := os.join_path(root, 'real.o')
	operand := os.join_path(root, 'operand.obj')
	manifest := os.join_path(root, 'link.manifest')
	// These tests inspect manifest bookkeeping, not object file contents.
	os.write_file(object, 'real input')!
	os.write_file(operand, 'option operand')!
	mut flags := [object]
	for option in c_link_operand_options() {
		flags << [option, operand]
	}
	stats := CObjectCacheStats{
		requests:       1
		direct_objects: 1
	}
	write_c_link_plan(manifest, flags, stats)!
	payload := os.read_file(manifest)!
	assert payload.split_into_lines().filter(it.starts_with('object=')) == ['object=${object}']
	mut read_stats := CObjectCacheStats{}
	plan := valid_c_link_plan(manifest, mut read_stats) or { panic('invalid link manifest') }
	assert plan.flags == flags
	assert plan.requests == 1
	assert plan.direct_objects == 1
	// A file used only as an option operand must not become a required object.
	os.rm(operand)!
	assert valid_c_link_plan(manifest, mut read_stats) != none
	os.write_file(manifest, payload.replace('v3-c-link-plan-v4', 'v3-c-link-plan-v3'))!
	assert valid_c_link_plan(manifest, mut read_stats) == none
	os.write_file(manifest, payload)!
	os.rm(object)!
	assert valid_c_link_plan(manifest, mut read_stats) == none
}

fn test_link_preparation_preserves_option_pairs_beside_real_objects() {
	root := os.join_path(os.vtmp_dir(), 'v link inputs ${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir(root)!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	object := os.join_path(root, 'real.o')
	os.write_file(object, 'existing object input')!
	compiler := os.join_path(root, 'nonexistent-compiler')
	flags := ['-D', 'VALUE=not_an_object.o', '-include', 'not_a_source.mm', '-Xlinker', '-x',
		'-Iinclude.mm', object, '-L', 'not_an_object.obj']
	// Seed the identity so this existing-object test needs no external compiler.
	mut stats := CObjectCacheStats{
		compiler_versions: {
			compiler: 'fixture compiler identity'
		}
	}
	target := pref.host_target()
	cache_dir := os.join_path(os.vtmp_dir(), 'v3_thirdparty_objs')
	manifest := c_link_plan_path(cache_dir, flags, c_object_compile_support_flags(flags), false,
		'', [], target, compiler, false, mut stats)
	defer {
		os.rm(manifest) or {}
	}
	prepared := prepare_c_flags_for_link(flags, [], [], false, '', [], target, compiler, false,
		root, mut stats)!
	assert prepared == flags
	assert stats.requests == 1
	assert stats.direct_objects == 1
	assert stats.dependency_scans == 0
	assert stats.temporary_objects.len == 0
	plan := valid_c_link_plan(manifest, mut stats) or { panic('invalid mixed-input manifest') }
	assert plan.flags == flags
	assert plan.requests == 1
}
