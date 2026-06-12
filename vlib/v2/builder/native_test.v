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
	linux := linux_native_link_command('/tmp/out', 'main.o', '-lm')
	macos := macos_native_link_command('/tmp/out', 'main.o', '/SDK Path', 'x86_64', false, '-lm')

	assert linux == 'cc ${os.quoted_path('main.o')} -o ${os.quoted_path('/tmp/out')} -no-pie -lm'
	assert macos == 'ld -o ${os.quoted_path('/tmp/out')} ${os.quoted_path('main.o')} -lm -lSystem -syslibroot ${os.quoted_path('/SDK Path')} -e _main -arch x86_64 -platform_version macos 11.0.0 11.0.0'
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

fn test_native_x64_requires_ssa_optimization() {
	mut prefs := pref.new_preferences()
	assert prefs.no_optimize
	builder := new_builder(&prefs)
	assert builder.native_backend_requires_ssa_optimization(.x64)
	assert !builder.native_backend_requires_ssa_optimization(.arm64)
}
