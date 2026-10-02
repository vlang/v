module driver

import os
import time
import v.cmdexec
import v.pref

fn test_dependency_objects_precede_imported_libraries_in_link_command() {
	for target_os in ['windows', 'linux', 'macos'] {
		plan := v3_c_compiler_flag_plan(V3CCompilerFlagOptions{
			target_os:    target_os
			c_compiler:   'gcc'
			dependencies: ['first.o', '-L', 'library directory', '-limported', '-I', 'include.o',
				'second.obj', '-Wl,--start-group', 'libfirst.a', '-lsecond', '-Wl,--end-group',
				'third.c']
		})
		args := plan.compiler_args('output', ['main.c', 'cached.o'], ['support.o'])
		assert args.index('support.o') < args.index('first.o')
		assert args.index('first.o') < args.index('second.obj')
		assert args.index('second.obj') < args.index('third.c')
		assert args.index('third.c') < args.index('-limported')
		assert args[args.index('-L') + 1] == 'library directory'
		assert args[args.index('-I') + 1] == 'include.o'
		assert args.index('-limported') < args.index('-Wl,--start-group')
		assert args.index('-Wl,--start-group') < args.index('libfirst.a')
		assert args.index('libfirst.a') < args.index('-lsecond')
		assert args.index('-lsecond') < args.index('-Wl,--end-group')
	}
}

fn test_dependency_reordering_preserves_explicit_native_source_languages() {
	flags := ['-lfirst', '-x', 'c++', 'extensionless', '-D', 'SOURCE=operand.o', '-x', 'none',
		'support.o', '-l', 'second', '-x', 'objective-c', 'implementation.m', '-x', 'none', 'liblast.a']
	assert c_link_dependency_flags(flags) == [
		'-x',
		'c++',
		'extensionless',
		'-x',
		'none',
		'-x',
		'none',
		'support.o',
		'-x',
		'none',
		'-x',
		'objective-c',
		'implementation.m',
		'-x',
		'none',
		'-lfirst',
		'-D',
		'SOURCE=operand.o',
		'-l',
		'second',
		'-x',
		'none',
		'liblast.a',
		'-x',
		'none',
	]
	assert c_link_dependency_flags(['-xc++', 'cpp.c', '-xnone', '-lfirst']) == [
		'-x',
		'c++',
		'cpp.c',
		'-x',
		'none',
		'-lfirst',
		'-x',
		'none',
	]
	assert c_link_dependency_flags(['-x', 'c++', 'cpp.c', '-xnone', 'plain.c', '-lfirst']) == [
		'-x',
		'c++',
		'cpp.c',
		'-x',
		'none',
		'-x',
		'none',
		'plain.c',
		'-x',
		'none',
		'-lfirst',
		'-x',
		'none',
	]
}

fn test_imported_library_resolves_symbols_from_later_native_object() {
	compiler := os.find_abs_path_of_executable($if windows { 'gcc' } $else { 'cc' }) or {
		return
	}
	archiver := os.find_abs_path_of_executable('ar') or { return }
	root := os.join_path(os.vtmp_dir(), 'v_imported_library_${os.getpid()}_${time.now().unix_nano()}')
	archive_dir := os.join_path(root, 'archive')
	consumer_dir := os.join_path(root, 'consumer')
	os.mkdir_all(archive_dir)!
	os.mkdir_all(consumer_dir)!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	library_source := os.join_path(archive_dir, 'answer.c')
	library_object := os.join_path(archive_dir, 'answer.o')
	library := os.join_path(archive_dir, 'libv3_order.a')
	consumer_source := os.join_path(consumer_dir, 'consumer.c')
	consumer_object := os.join_path(consumer_dir, 'consumer.o')
	os.write_file(library_source, 'int v3_archive_answer(void) { return 42; }\n')!
	os.write_file(consumer_source, 'int v3_archive_answer(void);\nint v3_archive_consumer(void) { return v3_archive_answer(); }\n')!
	for pair in [[library_source, library_object], [consumer_source, consumer_object]] {
		compiled := cmdexec.run(compiler, ['-c', '-o', pair[1], pair[0]])
		assert compiled.exit_code == 0, compiled.output
	}
	archived := cmdexec.run(archiver, ['rcs', library, library_object])
	assert archived.exit_code == 0, archived.output
	os.write_file(os.join_path(archive_dir, 'archive.c.v'), 'module archive\n#flag -L @DIR\n#flag -lv3_order\npub fn marker() int { return 41 }\n')!
	os.write_file(os.join_path(consumer_dir, 'consumer.c.v'), 'module consumer\n#flag @DIR/consumer.o\nfn C.v3_archive_consumer() int\npub fn answer() int { return C.v3_archive_consumer() }\n')!
	source := os.join_path(root, 'main.v')
	output := os.join_path(root, 'main' + $if windows { '.exe' } $else { '' })
	os.write_file(source, 'import archive\nimport consumer\nfn main() {\n assert archive.marker() == 41\n assert consumer.answer() == 42\n}\n')!
	vexe := if os.base(@VEXE) in ['v1_fallback', 'v1_fallback.exe'] {
		os.join_path(@VEXEROOT, 'v' + $if windows { '.exe' } $else { '' })
	} else {
		@VEXE
	}
	built := cmdexec.run(vexe, ['-new-compiler', '-nocache', '-gc', 'none', '-cc', compiler, '-showcc',
		'-o', output, source])
	assert built.exit_code == 0, built.output
	link_line := built.output.split_into_lines().filter(it.contains('-lv3_order')).last()
	assert (link_line.index(consumer_object) or { -1 }) >= 0, link_line
	assert (link_line.index(consumer_object) or { -1 }) < (link_line.index('-lv3_order') or { -1 }), link_line
	ran := cmdexec.run(output, [])
	assert ran.exit_code == 0, ran.output
}

fn test_reordered_native_sources_keep_joined_and_reset_languages() {
	compiler := os.find_abs_path_of_executable($if windows { 'gcc' } $else { 'cc' }) or {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v_link_languages_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	cpp_source := os.join_path(root, 'template.c')
	c_source := os.join_path(root, 'plain.c')
	output := os.join_path(root, 'main' + $if windows { '.exe' } $else { '' })
	entry := $if windows { 'wmain' } $else { 'main' }
	os.write_file(cpp_source, 'template<int N> int answer() { return N; }\nextern "C" int plain_answer(void);\nint ${entry}(void) { return answer<42>() == plain_answer() ? 0 : 1; }\n')!
	os.write_file(c_source, 'int class(void) { return 42; }\nint plain_answer(void) { return class(); }\n')!
	for options in [
		V3CCompilerFlagOptions{
			dependencies: ['-xc++', cpp_source, '-xnone', c_source]
		},
		V3CCompilerFlagOptions{
			dependencies: ['-x', 'c++', cpp_source, '-xnone', c_source]
		},
		V3CCompilerFlagOptions{
			dependencies:  ['-xc++']
			link_ld_flags: [cpp_source, '-xnone', c_source]
		},
		V3CCompilerFlagOptions{
			dependencies:  ['-x', 'c++']
			link_ld_flags: [cpp_source, '-x', 'none', c_source]
		},
		V3CCompilerFlagOptions{
			dependencies:  ['-xc++', cpp_source, '-xnone']
			link_ld_flags: [c_source]
		},
		V3CCompilerFlagOptions{
			dependencies:  ['-xc++', cpp_source]
			link_ld_flags: ['-x', 'none', c_source]
		},
		V3CCompilerFlagOptions{
			dependencies:  ['-xc++', cpp_source, '-x', 'c']
			link_ld_flags: [c_source]
		},
		V3CCompilerFlagOptions{
			environment_c_flags: ['-xc++']
			dependencies:        ['-xnone', c_source, '-xc++', cpp_source, '-xnone']
		},
	] {
		plan := v3_c_compiler_flag_plan(V3CCompilerFlagOptions{
			...options
			target_os:  os.user_os()
			c_compiler: compiler
		})
		built := cmdexec.run(compiler, plan.compiler_args(output, [], []))
		assert built.exit_code == 0, built.output
		ran := cmdexec.run(output, [])
		assert ran.exit_code == 0, ran.output
	}
	if archiver := os.find_abs_path_of_executable('ar') {
		object := os.join_path(root, 'plain.o')
		library := os.join_path(root, 'libplain.a')
		compiled := cmdexec.run(compiler, ['-c', '-o', object, c_source])
		assert compiled.exit_code == 0, compiled.output
		archived := cmdexec.run(archiver, ['rcs', library, object])
		assert archived.exit_code == 0, archived.output
		plan := v3_c_compiler_flag_plan(V3CCompilerFlagOptions{
			target_os:           os.user_os()
			c_compiler:          compiler
			environment_c_flags: ['-xc++']
			dependencies:        ['-xnone', library]
		})
		built := cmdexec.run(compiler, plan.compiler_args(output, [cpp_source], []))
		assert built.exit_code == 0, built.output
		ran := cmdexec.run(output, [])
		assert ran.exit_code == 0, ran.output
	}
}

fn test_trailing_native_language_reaches_sources_passed_through_ldflags() {
	compiler := os.find_abs_path_of_executable($if windows { 'gcc' } $else { 'cc' }) or {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v_trailing_language_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	source := os.join_path(root, 'main.v')
	cpp_source := os.join_path(root, 'template.c')
	c_source := os.join_path(root, 'plain.c')
	output := os.join_path(root, 'main' + $if windows { '.exe' } $else { '' })
	os.write_file(source, 'fn main() {}\n')!
	os.write_file(cpp_source, 'template<int N> int answer() { return N; }\nextern "C" int template_answer(void) { return answer<42>(); }\n')!
	os.write_file(c_source, 'int class(void) { return 42; }\n')!
	vexe := if os.base(@VEXE) in ['v1_fallback', 'v1_fallback.exe'] {
		os.join_path(@VEXEROOT, 'v' + $if windows { '.exe' } $else { '' })
	} else {
		@VEXE
	}
	for flags in [['-x c++', cpp_source], ['-xc++', cpp_source], ['-x c++ -x none', c_source],
		['-xc++ -xnone', c_source]] {
		built := cmdexec.run(vexe, ['-new-compiler', '-no-std', '-nocache', '-gc', 'none', '-cc',
			compiler, '-cflags', flags[0], '-ldflags', os.quoted_path(flags[1]), '-o', output,
			source])
		assert built.exit_code == 0, built.output
		ran := cmdexec.run(output, [])
		assert ran.exit_code == 0, ran.output
	}
}

fn test_joined_compile_flags_are_not_native_input_files() {
	for prefix in ['-I', '-isystem', '-iquote', '-DPLUGIN=', '-U'] {
		for suffix in ['.o', '.obj', '.c', '.cc', '.cpp', '.m', '.mm', '.a', '.so', '.so.1', '.dylib',
			'.dll', '.lib', '.tbd'] {
			flag := '${prefix}folder with spaces/input${suffix}'
			assert !c_flag_is_object_file(flag), flag
			assert !c_flag_is_c_source_file(flag), flag
			assert !c_flag_token_is_link_only(flag), flag
			assert c_object_compile_flags([flag]) == [flag], flag
			assert c_dylib_link_flags([flag]).len == 0, flag
			assert tcc_native_c_source_flags([flag]).len == 0, flag
			assert c_link_dependency_flags([flag]) == [flag], flag
		}
	}
}

fn test_sysroot_flags_reach_both_compile_and_link_steps() {
	for flags in [['--sysroot=/opt/cross-sdk.a'], ['--sysroot', '/opt/cross-sdk.a']] {
		assert c_object_compile_flags(flags) == flags
		assert tcc_cached_main_flags(flags) == flags
		assert c_dylib_link_flags(flags) == flags
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
		'-lfoo', '-Llibrary.a', '-Wl,-rpath,library.so', '-T/path/script.so', '-shared'] {
		assert c_flag_token_is_link_only(input), input
		assert c_object_compile_flags([input]).len == 0, input
		assert c_dylib_link_flags([input]) == [input], input
	}
	assert tcc_native_c_source_flags(['-DNAME=not_a_source.c', 'real.c']) == ['real.c']
	assert tcc_native_c_source_flags(['-x', 'c', '-Iinclude.c', 'extensionless', '-x', 'none']) == [
		'-x',
		'c',
		'extensionless',
		'-x',
		'none',
	]
}

fn c_link_operand_options() []string {
	return ['-I', '-L', '-F', '-D', '-U', '-include', '-imacros', '-isystem', '-iquote', '-idirafter',
		'-iprefix', '-iwithprefix', '-iwithprefixbefore', '-isysroot', '--sysroot', '-target',
		'-arch', '-framework', '-weak_framework', '-Xlinker', '-force_load', '-o', '-MF', '-MT',
		'-MQ', '-l', '-weak_library', '-x']
}

fn test_joined_isysroot_reaches_cached_dylib_link() {
	for flags in [['-isysroot/opt/MacSDK.a'], ['-isysroot', '/opt/MacSDK.a']] {
		assert c_dylib_link_flags(flags) == flags
		assert c_object_compile_flags(flags) == flags
		assert tcc_cached_main_flags(flags) == flags
	}
}

fn test_native_input_selection_consumes_option_operands_once() {
	for option in c_link_operand_options() {
		for operand in ['value.o', 'value.mm', 'folder with spaces/value.obj', '-x', ''] {
			flags := [option, operand, 'real.o']
			assert c_link_input_indices(flags) == [2], flags.str()
			if option != '-x' {
				assert c_link_dependency_flags(flags) == ['real.o', option, operand], flags.str()
			}
		}
		assert c_link_input_indices([option]).len == 0, option
	}
	assert c_link_input_indices(['main.c', '-I', 'include.o', '-x', 'c++', 'unit.cpp', '-x', 'none',
		'support.o', '-l', 'library.mm', '-DNAME=macro.obj', '']) == [0, 5, 8]
	assert !c_link_flags_use_cpp_language(['-l', 'library.cpp'])
	assert !c_link_flags_use_objective_c_language(['-weak_library', 'library.mm'])
}

fn test_option_only_link_preparation_does_not_probe_or_compile() {
	for option in c_link_operand_options() {
		for operand in ['missing.o', 'missing.mm', 'missing path.obj', '-x', ''] {
			flags := [option, operand]
			mut stats := CObjectCacheStats{}
			prepared := prepare_c_flags_for_link(flags, [], [], [], false, false, '', [],
				pref.host_target(),
				'v_c_link_flags_nonexistent_compiler', false, '', mut stats)!
			assert prepared == flags, flags.str()
			assert stats.requests == 0, flags.str()
			assert stats.compiler_versions.len == 0, flags.str()
			assert stats.link_plan_signature == '', flags.str()
		}
	}
	for flag in ['-DNAME=missing.o', '-DNAME=missing.obj', '-DNAME=missing.mm', '-Iinclude.mm'] {
		mut stats := CObjectCacheStats{}
		assert prepare_c_flags_for_link([flag], [], [], [], false, false, '', [],
			pref.host_target(),
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
	os.write_file(manifest, payload.replace('v3-c-link-plan-v5', 'v3-c-link-plan-v4'))!
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
	object_flags := CObjectFlagPlan{
		primary_compiler: compiler
		common_flags:     c_object_compile_support_flags(flags)
	}
	manifest := c_link_plan_path(cache_dir, flags, &object_flags, false, false, '', [],
		target, compiler, false, mut stats)
	defer {
		os.rm(manifest) or {}
	}
	prepared := prepare_c_flags_for_link(flags, [], [], [], false, false, '', [], target,
		compiler, false, root, mut stats)!
	assert prepared == flags
	assert stats.requests == 1
	assert stats.direct_objects == 1
	assert stats.dependency_scans == 0
	assert stats.temporary_objects.len == 0
	plan := valid_c_link_plan(manifest, mut stats) or { panic('invalid mixed-input manifest') }
	assert plan.flags == flags
	assert plan.requests == 1
}

fn test_c_source_language_flags_preserve_filename_case_and_explicit_languages() {
	flags := ['long C source/my_test_cshim.c', 'native.o', 'cpp_source.C', 'source.cpp']
	expected := ['-x', 'c', flags[0], '-x', 'none', 'native.o', 'cpp_source.C', 'source.cpp']
	assert c_source_language_flags(flags) == expected
	assert c_source_language_flags(expected) == expected
	for option in c_link_operand_options() {
		assert c_source_language_flags([option, 'operand.c']) == [option, 'operand.c']
	}
	for language in ['c', 'c++', 'objective-c', 'objective-c++'] {
		explicit := ['-x', language, 'source.c', 'source.C']
		assert c_source_language_flags(explicit) == explicit
		joined := ['-x${language}', 'source.c', 'source.C']
		assert c_source_language_flags(joined) == joined
	}
	assert c_source_language_flags(['-xc++', 'cpp.c', '-xnone', 'plain.c']) == ['-xc++', 'cpp.c',
		'-xnone', '-x', 'c', 'plain.c', '-x', 'none']
	assert c_source_language_flags(['-Xlinker', '-xc++', 'plain.c']) == ['-Xlinker', '-xc++', '-x',
		'c', 'plain.c', '-x', 'none']
	assert c_source_language_flags(['-x', 'none', 'source.c']) == ['-x', 'none', '-x', 'c', 'source.c',
		'-x', 'none']
	assert c_source_language('source.c', '') == 'c'
	assert c_source_language('source.C', '') == 'c++'
	assert c_source_language('source.c', 'c++') == 'c++'
	assert c_source_language('source.C', 'c') == 'c'
	assert c_flag_is_c_source_file('source.C')
	assert c_link_flags_use_cpp_language(['source.C'])
	assert !c_link_flags_use_cpp_language(['-x', 'c', 'source.C'])
	mut stats := CObjectCacheStats{}
	prepared := prepare_c_flags_for_link(flags.filter(it != 'native.o'), [], [], [], false, false, '', [],
		pref.host_target(), 'missing-compiler', false, '', mut stats)!
	mut prepared_expected := expected.filter(it != 'native.o')
	prepared_expected << cpp_runtime_link_flag(pref.host_target())
	assert prepared == prepared_expected
}

fn test_joined_languages_match_separated_driver_decisions() {
	for language in ['c', 'c++', 'objective-c', 'objective-c++', 'none'] {
		joined := ['-x${language}', 'source.c', '-xnone', 'following.c']
		separated := ['-x', language, 'source.c', '-x', 'none', 'following.c']
		assert c_link_flags_use_non_c_language(joined) == c_link_flags_use_non_c_language(separated)
		assert c_link_flags_use_cpp_language(joined) == c_link_flags_use_cpp_language(separated)
		assert c_link_flags_use_objective_c_language(joined) == c_link_flags_use_objective_c_language(separated)
		assert c_flags_need_objective_c(joined) == c_flags_need_objective_c(separated)
		assert c_object_compile_flags(joined) == c_object_compile_flags(separated)
		assert c_dylib_link_flags(joined) == c_dylib_link_flags(separated)
		assert tcc_native_c_source_flags(joined) == tcc_native_c_source_flags(separated)
	}
	assert c_link_flags_use_cpp_language(['-xc++', 'source.c'])
	assert c_link_flags_use_objective_c_language(['-xobjective-c', 'source.c'])
	assert !c_link_flags_use_cpp_language(['-Xlinker', '-xc++', 'source.c'])
	assert !c_flags_need_objective_c(['-Xlinker', '-xobjective-c', 'source.c'])
	assert !c_link_flags_use_cpp_language(['-xc++', '-xnone', 'source.c'])
}
