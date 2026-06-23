module builder

import os
import v2.abi
import v2.ast
import v2.gen.x64
import v2.pref

fn test_native_x64_windows_selects_coff_and_windows_abi() {
	assert is_windows_x64_native_target(.x64, 'windows')
	assert native_x64_object_format_for_os('windows') == x64.ObjectFormat.coff
	assert native_x64_codegen_abi_for_os('windows') == x64.X64Abi.windows
	assert native_x64_lowering_abi_for_os('windows') == abi.X64Abi.windows
}

fn test_native_x64_non_windows_keeps_existing_object_formats_and_sysv() {
	assert !is_windows_x64_native_target(.x64, 'linux')
	assert native_x64_object_format_for_os('linux') == x64.ObjectFormat.elf
	assert native_x64_codegen_abi_for_os('linux') == x64.X64Abi.sysv
	assert native_x64_lowering_abi_for_os('linux') == abi.X64Abi.sysv

	assert !is_windows_x64_native_target(.x64, 'macos')
	assert native_x64_object_format_for_os('macos') == x64.ObjectFormat.macho
	assert native_x64_codegen_abi_for_os('macos') == x64.X64Abi.sysv
	assert native_x64_lowering_abi_for_os('macos') == abi.X64Abi.sysv
	assert is_macos_native_target('macos')
	assert is_macos_native_target('darwin')
	assert native_x64_object_format_for_os('darwin') == x64.ObjectFormat.macho
	assert native_x64_codegen_abi_for_os('darwin') == x64.X64Abi.sysv
	assert native_x64_lowering_abi_for_os('darwin') == abi.X64Abi.sysv
}

fn test_minimal_x64_runtime_and_macos_tiny_object_targets() {
	old_linux_tiny := os.getenv_opt('V2_X64_LINUX_TINY')
	os.unsetenv('V2_X64_LINUX_TINY')
	defer {
		if old := old_linux_tiny {
			os.setenv('V2_X64_LINUX_TINY', old, true)
		} else {
			os.unsetenv('V2_X64_LINUX_TINY')
		}
	}

	mut prefs := pref.new_preferences()
	prefs.backend = .x64
	prefs.arch = .x64

	prefs.target_os = 'linux'
	linux_builder := new_builder(&prefs)
	assert !linux_builder.uses_minimal_windows_x64_runtime()
	assert linux_builder.uses_minimal_linux_x64_runtime()
	assert linux_builder.uses_minimal_x64_runtime()
	assert !linux_builder.uses_minimal_linux_x64_runtime_roots()
	assert !linux_builder.uses_minimal_x64_runtime_roots()

	os.setenv('V2_X64_LINUX_TINY', '1', true)
	linux_tiny_builder := new_builder(&prefs)
	assert linux_tiny_builder.uses_minimal_linux_x64_runtime()
	assert linux_tiny_builder.uses_minimal_linux_x64_runtime_roots()
	assert linux_tiny_builder.uses_minimal_x64_runtime_roots()
	os.unsetenv('V2_X64_LINUX_TINY')

	prefs.target_os = 'macos'
	macos_builder := new_builder(&prefs)
	assert !macos_builder.uses_minimal_windows_x64_runtime()
	assert !macos_builder.uses_minimal_linux_x64_runtime()
	assert !macos_builder.uses_minimal_x64_runtime()
	assert !macos_builder.uses_minimal_x64_runtime_roots()
	assert macos_builder.uses_macos_x64_tiny_object(.x64)
	assert !macos_builder.uses_macos_x64_tiny_object(.arm64)

	macos_tiny_builder := new_builder(&prefs)
	assert !macos_tiny_builder.uses_minimal_x64_runtime()
	assert !macos_tiny_builder.uses_minimal_x64_runtime_roots()
	assert macos_tiny_builder.uses_macos_x64_tiny_object(.x64)
	assert !macos_tiny_builder.uses_macos_x64_tiny_object(.arm64)

	prefs.macos_tiny = false
	macos_no_tiny_builder := new_builder(&prefs)
	assert !macos_no_tiny_builder.uses_macos_x64_tiny_object(.x64)

	prefs.macos_tiny = true
	prefs.arch = .auto
	macos_auto_arch_builder := new_builder(&prefs)
	assert !macos_auto_arch_builder.uses_minimal_x64_runtime_roots()
	assert macos_auto_arch_builder.uses_macos_x64_tiny_object(.x64)
	assert !macos_auto_arch_builder.uses_macos_x64_tiny_object(.arm64)
	prefs.arch = .x64

	prefs.target_os = 'darwin'
	darwin_builder := new_builder(&prefs)
	assert !darwin_builder.uses_minimal_windows_x64_runtime()
	assert !darwin_builder.uses_minimal_linux_x64_runtime()
	assert !darwin_builder.uses_minimal_x64_runtime()
	assert !darwin_builder.uses_minimal_x64_runtime_roots()
	assert darwin_builder.uses_macos_x64_tiny_object(.x64)

	prefs.target_os = 'windows'
	windows_builder := new_builder(&prefs)
	assert windows_builder.uses_minimal_windows_x64_runtime()
	assert !windows_builder.uses_minimal_linux_x64_runtime()
	assert windows_builder.uses_minimal_x64_runtime()
	assert windows_builder.uses_minimal_x64_runtime_roots()
	assert !windows_builder.uses_macos_x64_tiny_object(.x64)
}

fn test_macos_tiny_candidate_source_snapshot_is_auto_by_default_with_opt_out() {
	mut prefs := pref.new_preferences()
	prefs.backend = .x64
	prefs.arch = .auto
	prefs.target_os = 'macos'

	mut hosted_builder := new_builder(&prefs)
	hosted_builder.files = [
		ast.File{
			mod:  'main'
			name: 'main.v'
		},
	]
	hosted_builder.prepare_macos_tiny_candidate_source_files()
	assert hosted_builder.macos_tiny_candidate_source_files.len == 1
	assert hosted_builder.macos_tiny_candidate_source_files[0].name == 'main.v'

	prefs.macos_tiny = false
	mut opt_out_builder := new_builder(&prefs)
	opt_out_builder.files = [
		ast.File{
			mod:  'main'
			name: 'main.v'
		},
	]
	opt_out_builder.prepare_macos_tiny_candidate_source_files()
	assert opt_out_builder.macos_tiny_candidate_source_files.len == 0
	assert !opt_out_builder.uses_minimal_x64_runtime_roots()
}

fn test_macos_tiny_candidate_source_snapshot_uses_flat_when_enabled() {
	mut prefs := pref.new_preferences()
	prefs.backend = .x64
	prefs.arch = .auto
	prefs.target_os = 'macos'

	source_files := [
		ast.File{
			mod:   'main'
			name:  'flat_main.v'
			stmts: [
				ast.Stmt(ast.FnDecl{
					name: 'main'
				}),
			]
		},
	]
	mut tiny_builder := new_builder(&prefs)
	tiny_builder.files = [
		ast.File{
			mod:  'stale'
			name: 'stale.v'
		},
	]
	tiny_builder.flat = ast.flatten_files(source_files)
	tiny_builder.prepare_macos_tiny_candidate_source_files()
	assert tiny_builder.macos_tiny_candidate_source_files.len == 0
	assert tiny_builder.macos_tiny_candidate_source_flat.files.len == source_files.len
	assert tiny_builder.macos_tiny_candidate_source_flat.file_name(tiny_builder.macos_tiny_candidate_source_flat.files[0]) == 'flat_main.v'
	assert tiny_builder.macos_tiny_candidate_source_flat.string_at(tiny_builder.macos_tiny_candidate_source_flat.files[0].mod_idx) == 'main'
}

fn test_macos_tiny_candidate_native_mir_build_policy_is_sequential_only_for_candidate() {
	mut prefs := pref.new_preferences()
	prefs.backend = .x64
	prefs.arch = .x64
	prefs.target_os = 'macos'
	prefs.no_parallel = false
	prefs.hot_fn = ''
	builder := new_builder(&prefs)

	assert !builder.native_mir_build_sequential('')
	assert builder.native_mir_build_sequential(macos_tiny_candidate_graph_label)

	prefs.no_parallel = true
	no_parallel_builder := new_builder(&prefs)
	assert no_parallel_builder.native_mir_build_sequential('')
}

fn test_macos_tiny_link_command_adds_hygiene_only_for_tiny_object() {
	normal := macos_native_link_command('/tmp/out', 'main.o', '/SDK Path', 'x86_64', false, '')
	tiny := macos_native_link_command('/tmp/out', 'main.o', '/SDK Path', 'x86_64', true, '')

	assert normal == 'ld -o ${os.quoted_path('/tmp/out')} ${os.quoted_path('main.o')} -lSystem -syslibroot ${os.quoted_path('/SDK Path')} -e _main -arch x86_64 -platform_version macos 11.0.0 11.0.0'
	assert tiny == '${normal} -dead_strip -x -S'
	assert !normal.contains('-dead_strip')
	assert !normal.contains(' -x')
	assert !normal.contains(' -S')
}

fn test_macos_sdk_path_from_xcrun_output_accepts_simple_sdk_path() {
	output := '/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX12.3.sdk\n'
	assert macos_sdk_path_from_xcrun_output(output)! == '/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX12.3.sdk'
}

fn test_macos_sdk_path_from_xcrun_output_ignores_diagnostics_before_sdk_path() {
	output :=
		'2026-06-07 16:19:11.428 xcodebuild[1002:7567] Requested but did not find extension point\n' +
		'2026-06-07 16:19:14.880 xcodebuild[1004:7681] Requested but did not find extension point\n' +
		'/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX12.3.sdk\n'
	assert macos_sdk_path_from_xcrun_output(output)! == '/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX12.3.sdk'
}

fn test_macos_sdk_path_from_xcrun_output_handles_empty_lines_spaces_and_crlf() {
	output := ' \r\n\t\r\n  /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk  \r\n'
	assert macos_sdk_path_from_xcrun_output(output)! == '/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk'
}

fn test_macos_sdk_path_from_xcrun_output_rejects_missing_sdk_path() {
	path := macos_sdk_path_from_xcrun_output('xcodebuild diagnostic only\nnot a path\n') or {
		assert err.msg().contains('could not find a clean macOS SDK path')
		return
	}
	assert false, 'expected missing SDK path error, got ${path}'
}

fn test_macos_sdk_path_from_xcrun_output_preserves_spaces_inside_path() {
	output := '/Applications/Xcode With Spaces.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX12.3.sdk\n'
	assert macos_sdk_path_from_xcrun_output(output)! == '/Applications/Xcode With Spaces.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX12.3.sdk'
}

fn test_macos_sdk_path_from_xcrun_output_rejects_relative_or_non_macos_sdk_lines() {
	output := 'relative/MacOSX12.3.sdk\n/Applications/Xcode.app/Contents/Developer/SDKs/iPhoneOS.sdk\n/Applications/Xcode.app/Contents/Developer/SDKs/NotMacOSX.sdk\n'
	path := macos_sdk_path_from_xcrun_output(output) or {
		assert err.msg().contains('could not find a clean macOS SDK path')
		return
	}
	assert false, 'expected invalid SDK path lines to be rejected, got ${path}'
}

fn test_macos_sdk_path_from_xcrun_output_uses_last_valid_sdk_line() {
	output := '/Applications/Old.app/Contents/Developer/SDKs/MacOSX11.0.sdk\n' +
		'/Applications/New.app/Contents/Developer/SDKs/MacOSX12.3.sdk\n'
	assert macos_sdk_path_from_xcrun_output(output)! == '/Applications/New.app/Contents/Developer/SDKs/MacOSX12.3.sdk'
}

fn test_validate_macos_sdk_path_for_native_link_accepts_libsystem_tbd() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_macos_sdk_tbd_${os.getpid()}')
	sdk_path := os.join_path(tmp_dir, 'MacOSX.test.sdk')
	lib_dir := os.join_path(sdk_path, 'usr', 'lib')
	os.mkdir_all(lib_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	os.write_file(os.join_path(lib_dir, 'libSystem.tbd'), '') or { panic(err) }
	validate_macos_sdk_path_for_native_link(sdk_path)!
}

fn test_validate_macos_sdk_path_for_native_link_accepts_libsystem_dylib() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_macos_sdk_dylib_${os.getpid()}')
	sdk_path := os.join_path(tmp_dir, 'MacOSX.test.sdk')
	lib_dir := os.join_path(sdk_path, 'usr', 'lib')
	os.mkdir_all(lib_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	os.write_file(os.join_path(lib_dir, 'libSystem.dylib'), '') or { panic(err) }
	validate_macos_sdk_path_for_native_link(sdk_path)!
}

fn test_validate_macos_sdk_path_for_native_link_rejects_sdk_without_libsystem() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_macos_sdk_missing_libsystem_${os.getpid()}')
	sdk_path := os.join_path(tmp_dir, 'MacOSX.test.sdk')
	os.mkdir_all(os.join_path(sdk_path, 'usr', 'lib')) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	validate_macos_sdk_path_for_native_link(sdk_path) or {
		assert err.msg().contains('missing usr/lib/libSystem')
		return
	}
	assert false, 'expected SDK without libSystem to be rejected'
}

fn test_validate_macos_sdk_path_for_native_link_rejects_missing_sdk_dir() {
	missing_sdk := os.join_path(os.vtmp_dir(), 'v2_missing_macos_sdk_${os.getpid()}',
		'MacOSX.test.sdk')
	validate_macos_sdk_path_for_native_link(missing_sdk) or {
		assert err.msg().contains('does not exist')
		return
	}
	assert false, 'expected missing SDK directory to be rejected'
}

fn test_native_link_commands_append_directive_link_flags() {
	linux := linux_native_link_command('cc', '/tmp/out', 'main.o', '-lm')
	macos := macos_native_link_command('/tmp/out', 'main.o', '/SDK Path', 'x86_64', false, '-lm')

	assert linux == 'cc ${os.quoted_path('main.o')} -o ${os.quoted_path('/tmp/out')} -no-pie -lm'
	assert macos == 'ld -o ${os.quoted_path('/tmp/out')} ${os.quoted_path('main.o')} -lm -lSystem -syslibroot ${os.quoted_path('/SDK Path')} -e _main -arch x86_64 -platform_version macos 11.0.0 11.0.0'
}

fn test_linux_native_link_command_uses_configured_driver_and_link_flags() {
	cmd := linux_native_link_command('custom-cc', '/tmp/out', 'main.o', '-fopenmp')

	assert cmd.starts_with('custom-cc ${os.quoted_path('main.o')}'), cmd
	assert cmd.contains('-fopenmp'), cmd
}

fn test_native_link_commands_keep_wl_for_linux_and_translate_for_macos_ld() {
	linux := linux_native_link_command('cc', '/tmp/out', 'main.o', '-Wl,-rpath,/tmp/lib')
	macos := macos_native_link_command('/tmp/out', 'main.o', '/SDK Path', 'x86_64', false,
		'-Wl,-rpath,@loader_path/lib')

	assert linux.contains('-Wl,-rpath,/tmp/lib'), linux
	assert macos.contains(' -rpath @loader_path/lib '), macos
	assert !macos.contains('-Wl,'), macos
}

fn test_native_link_commands_translate_xlinker_for_macos_ld() {
	macos := macos_native_link_command('/tmp/out', 'main.o', '/SDK Path', 'x86_64', false,
		'-Xlinker -rpath -Xlinker @loader_path/lib')

	assert macos.contains(' -rpath @loader_path/lib '), macos
	assert !macos.contains('-Xlinker'), macos
}

fn test_macos_native_link_command_keeps_framework_search_paths() {
	macos := macos_native_link_command('/tmp/out', 'main.o', '/SDK Path', 'x86_64', false,
		'-F /tmp/Fw -F/opt/Fw -framework Foo')

	assert macos.contains(' -F /tmp/Fw -F/opt/Fw -framework Foo '), macos
}

fn test_split_compile_and_link_flags_duplicates_dual_use_driver_flags() {
	compile_flags, link_flags :=
		split_compile_and_link_flags('-I include -fopenmp -fopenmp=libomp -pthread helper.c')

	assert compile_flags == '-I include -fopenmp -fopenmp=libomp -pthread'
	assert link_flags == '-fopenmp -fopenmp=libomp -pthread helper.c'
}

fn test_split_compile_and_link_flags_keeps_framework_search_paths_dual_use() {
	compile_flags, link_flags :=
		split_compile_and_link_flags('-F /tmp/Fw -F/opt/Fw -framework Foo helper.m')

	assert compile_flags == '-F /tmp/Fw -F/opt/Fw'
	assert link_flags == '-F /tmp/Fw -F/opt/Fw -framework Foo helper.m'
}

fn test_macos_native_ld_rejects_dual_use_driver_link_flags() {
	validate_macos_native_ld_link_flags('-lm')!
	validate_macos_native_ld_link_flags('-fopenmp') or {
		assert err.msg().contains('macOS native ld cannot consume driver linker flags'), err.msg()
		return
	}
	assert false, 'expected macOS native ld driver flag diagnostic'
}

fn test_native_linux_tiny_link_allows_runtime_system_libs_when_user_flags_are_empty() {
	assert native_link_flags_allow_builtin_linux_tiny('', '')
	assert native_link_flags_allow_builtin_linux_tiny('   ', '')
	assert native_link_flags_allow_builtin_linux_tiny('-lpthread -lm -ldl -lc', '')
	assert native_link_flags_allow_builtin_linux_tiny('-l pthread -l m -l dl -l c', '')
}

fn test_native_linux_tiny_link_blocks_user_external_link_flags() {
	for user_link_flags in ['-lm', '-Wl,-rpath,/tmp/lib', '/tmp/helper.c', '-fopenmp', '-pthread',
		'-F/tmp/Frameworks'] {
		assert !native_link_flags_allow_builtin_linux_tiny('-lpthread -lm -ldl', user_link_flags), user_link_flags
	}
}

fn test_native_linux_tiny_link_blocks_external_link_inputs_in_global_flags() {
	assert !native_link_flags_allow_builtin_linux_tiny('-Xlinker -rpath', '')
	assert !native_link_flags_allow_builtin_linux_tiny('-L/tmp', '')
	assert !native_link_flags_allow_builtin_linux_tiny('/tmp/helper.o', '')
	assert !native_link_flags_allow_builtin_linux_tiny('/tmp/libhelper.a', '')
	assert !native_link_flags_allow_builtin_linux_tiny('-framework Foundation', '')
	assert !native_link_flags_allow_builtin_linux_tiny('-lfoo', '')
}

fn test_native_external_link_inputs_replace_sources_with_objects_in_order() {
	inputs := native_external_link_inputs('-L/tmp/lib /tmp/foo.c -lfoo /tmp/bar.m /tmp/baz.o',
		'/tmp/out')!

	assert inputs.source_files == ['/tmp/foo.c', '/tmp/bar.m']
	assert inputs.object_files.len == 2
	assert inputs.link_flags == '-L/tmp/lib ${os.quoted_path(inputs.object_files[0])} -lfoo ${os.quoted_path(inputs.object_files[1])} /tmp/baz.o'
	assert !inputs.link_flags.contains('/tmp/foo.c')
	assert !inputs.link_flags.contains('/tmp/bar.m')
}

fn test_native_external_link_inputs_reject_unsupported_c_family_sources() {
	for source in ['/tmp/foo.cc', '/tmp/foo.cpp', '/tmp/foo.cxx', '/tmp/foo.mm'] {
		native_external_link_inputs(source, '/tmp/out') or {
			assert err.msg().starts_with('x64: unsupported backend feature: '), err.msg()
			continue
		}
		assert false, 'expected ${source} source to be rejected'
	}
}

fn test_native_external_link_inputs_reject_quoted_source_tokens() {
	native_external_link_inputs('"foo.c"', '/tmp/out') or {
		assert err.msg().starts_with('x64: unsupported backend feature: quoted #flag source path'), err.msg()

		return
	}
	assert false, 'expected quoted source token to be rejected'
}

fn test_macos_native_link_command_uses_rewritten_external_objects() {
	tmp_dir := os.real_path(os.vtmp_dir())
	foo_c := os.join_path(tmp_dir, 'foo.c')
	bar_m := os.join_path(tmp_dir, 'bar.m')
	out_path := os.join_path(tmp_dir, 'out')
	inputs := native_external_link_inputs('${foo_c} -framework Foundation ${bar_m}', out_path)!
	cmd := macos_native_link_command(out_path, 'main.o', '/SDK Path', 'x86_64', false,
		inputs.link_flags)

	assert cmd.contains(os.quoted_path(inputs.object_files[0])), cmd
	assert cmd.contains(os.quoted_path(inputs.object_files[1])), cmd
	assert cmd.contains('-framework Foundation'), cmd
	assert !cmd.contains(foo_c), cmd
	assert !cmd.contains(bar_m), cmd
}

fn test_native_external_source_compile_command_adds_macos_sdk_and_arch() {
	cmd := native_external_source_compile_command('cc', '/tmp/foo.c', '/tmp/foo.o',
		'-I /tmp/include -DHELPER', '/SDK Path', 'x86_64', 'macos')

	assert cmd.starts_with('cc -c '), cmd
	assert cmd.contains('-isysroot ${os.quoted_path('/SDK Path')}'), cmd
	assert cmd.contains('-arch x86_64'), cmd
	assert cmd.contains('-I /tmp/include -DHELPER'), cmd
	assert cmd.contains(os.quoted_path('/tmp/foo.c')), cmd
	assert cmd.contains('-o ${os.quoted_path('/tmp/foo.o')}'), cmd
}

fn test_native_external_source_compile_command_keeps_linux_compile_flags() {
	cmd := native_external_source_compile_command('cc', '/tmp/foo.c', '/tmp/foo.o',
		'-I /tmp/include -DHELPER', '', '', 'linux')

	assert cmd.starts_with('cc -c '), cmd
	assert !cmd.contains('-isysroot'), cmd
	assert !cmd.contains('-arch'), cmd
	assert cmd.contains('-I /tmp/include -DHELPER'), cmd
	assert cmd.contains(os.quoted_path('/tmp/foo.c')), cmd
	assert cmd.contains('-o ${os.quoted_path('/tmp/foo.o')}'), cmd
}

fn test_native_external_source_compiler_uses_pref_ccompiler() {
	mut prefs := pref.new_preferences()
	prefs.ccompiler = 'custom-native-cc'
	mut b := new_builder(&prefs)

	assert b.native_external_source_compiler('macos') == 'custom-native-cc'
}

fn test_native_external_source_compiler_uses_v2cc_when_pref_is_empty() {
	old_v2cc := os.getenv_opt('V2CC')
	os.setenv('V2CC', 'env-native-cc', true)
	defer {
		if old := old_v2cc {
			os.setenv('V2CC', old, true)
		} else {
			os.unsetenv('V2CC')
		}
	}

	mut prefs := pref.new_preferences()
	prefs.ccompiler = ''
	mut b := new_builder(&prefs)

	assert b.native_external_source_compiler('macos') == 'env-native-cc'
}

fn test_native_external_source_compiler_defaults_to_cc_for_macos() {
	old_v2cc := os.getenv_opt('V2CC')
	os.unsetenv('V2CC')
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_builder_native_external_cc_${os.getpid()}')
	tcc_dir := os.join_path(tmp_dir, 'thirdparty', 'tcc')
	os.mkdir_all(tcc_dir) or { panic(err) }
	os.write_file(os.join_path(tcc_dir, 'tcc.exe'), '') or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
		if old := old_v2cc {
			os.setenv('V2CC', old, true)
		} else {
			os.unsetenv('V2CC')
		}
	}

	mut prefs := pref.Preferences{
		vroot: tmp_dir
	}
	mut b := new_builder(&prefs)

	assert b.native_external_source_compiler('macos') == 'cc'
	assert b.native_external_source_compiler('darwin') == 'cc'
}

fn test_native_linux_hosted_link_compiler_pref_v2cc_default_order() {
	old_v2cc := os.getenv_opt('V2CC')
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_builder_native_linux_hosted_cc_${os.getpid()}')
	tcc_dir := os.join_path(tmp_dir, 'thirdparty', 'tcc')
	os.mkdir_all(tcc_dir) or { panic(err) }
	os.write_file(os.join_path(tcc_dir, 'tcc.exe'), '') or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
		if old := old_v2cc {
			os.setenv('V2CC', old, true)
		} else {
			os.unsetenv('V2CC')
		}
	}

	os.setenv('V2CC', 'env-hosted-cc', true)
	mut pref_prefs := pref.new_preferences()
	pref_prefs.ccompiler = 'pref-hosted-cc'
	pref_builder := new_builder(&pref_prefs)
	assert pref_builder.native_linux_hosted_link_compiler() == 'pref-hosted-cc'

	mut env_prefs := pref.new_preferences()
	env_prefs.ccompiler = ''
	env_builder := new_builder(&env_prefs)
	assert env_builder.native_linux_hosted_link_compiler() == 'env-hosted-cc'

	os.unsetenv('V2CC')
	mut default_prefs := pref.Preferences{
		vroot: tmp_dir
	}
	default_builder := new_builder(&default_prefs)
	assert default_builder.native_linux_hosted_link_compiler() == 'cc'
}

fn test_native_link_flags_from_sources_are_not_global() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_builder_native_link_flags_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	no_flag_path := os.join_path(tmp_dir, 'no_flag.v')
	math_flag_path := os.join_path(tmp_dir, 'math_flag.v')
	os.write_file(no_flag_path, 'module main\n') or { panic(err) }
	os.write_file(math_flag_path, 'module main\n#flag -lm\n') or { panic(err) }

	mut prefs := pref.new_preferences()
	prefs.backend = .x64
	prefs.arch = .x64
	prefs.target_os = 'linux'
	prefs.skip_builtin = true

	mut no_flag_builder := new_builder(&prefs)
	no_flag_builder.files = [
		ast.File{
			name: no_flag_path
		},
	]
	assert no_flag_builder.native_link_flags_from_sources() == ''

	mut math_flag_builder := new_builder(&prefs)
	math_flag_builder.files = [
		ast.File{
			name: math_flag_path
		},
	]
	assert math_flag_builder.native_link_flags_from_sources() == '-lm'
}

fn test_native_user_link_flags_ignore_internal_vlib_runtime_flags() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_builder_native_user_link_flags_${os.getpid()}')
	vlib_os_dir := os.join_path(tmp_dir, 'vlib', 'os')
	os.mkdir_all(vlib_os_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	main_path := os.join_path(tmp_dir, 'main.v')
	internal_path := os.join_path(vlib_os_dir, 'signal_linux.c.v')
	os.write_file(main_path, 'module main\n#flag -lm\n') or { panic(err) }
	os.write_file(internal_path, 'module os\n#flag -lpthread\n') or { panic(err) }

	mut prefs := pref.Preferences{
		backend:      .x64
		arch:         .x64
		target_os:    'linux'
		vroot:        tmp_dir
		skip_builtin: true
	}

	mut b := new_builder(&prefs)
	b.user_files = [main_path]
	b.files = [
		ast.File{
			name: main_path
		},
		ast.File{
			name: internal_path
		},
	]
	_, all_link_flags := b.native_compile_and_link_flags_from_sources()
	_, user_link_flags := b.native_user_compile_and_link_flags_from_sources()

	assert all_link_flags.fields().sorted() == ['-lm', '-lpthread']
	assert user_link_flags == '-lm'
	assert !native_link_flags_allow_builtin_linux_tiny(all_link_flags, user_link_flags)

	b.user_files = []
	b.files = [
		ast.File{
			name: internal_path
		},
	]
	_, internal_only_link_flags := b.native_compile_and_link_flags_from_sources()
	_, no_user_link_flags := b.native_user_compile_and_link_flags_from_sources()

	assert internal_only_link_flags == '-lpthread'
	assert no_user_link_flags == ''
	assert native_link_flags_allow_builtin_linux_tiny(internal_only_link_flags, no_user_link_flags)
}

fn test_native_compile_and_link_flags_from_sources_keep_compile_flags_for_source_inputs() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_builder_native_source_flags_${os.getpid()}')
	include_dir := os.join_path(tmp_dir, 'include')
	os.mkdir_all(include_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	main_path := os.join_path(tmp_dir, 'main.v')
	c_path := os.join_path(tmp_dir, 'helper.c')
	os.write_file(c_path, 'int helper(void) { return 7; }\n') or { panic(err) }
	os.write_file(main_path, 'module main
#flag -I include -DHELPER -fopenmp -pthread helper.c -Wl,-rpath,/tmp/native-lib -lm
') or {
		panic(err)
	}

	mut prefs := pref.new_preferences()
	prefs.backend = .x64
	prefs.arch = .x64
	prefs.target_os = 'linux'
	prefs.skip_builtin = true

	mut b := new_builder(&prefs)
	b.files = [
		ast.File{
			name: main_path
		},
	]
	compile_flags, link_flags := b.native_compile_and_link_flags_from_sources()
	expected_include_dir := os.real_path(include_dir)
	expected_c_path := os.real_path(c_path)

	assert compile_flags == '-I ${expected_include_dir} -DHELPER -fopenmp -pthread'
	assert link_flags == '-fopenmp -pthread ${expected_c_path} -Wl,-rpath,/tmp/native-lib -lm'
}

fn test_native_x64_requires_ssa_optimization() {
	mut prefs := pref.new_preferences()
	assert prefs.no_optimize
	builder := new_builder(&prefs)
	assert builder.native_backend_requires_ssa_optimization(.x64)
	assert !builder.native_backend_requires_ssa_optimization(.arm64)
}
