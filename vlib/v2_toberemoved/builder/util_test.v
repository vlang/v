// vtest build: macos
module builder

import os
import v2.ast
import v2.pref

fn test_get_v_files_from_dir_uses_target_os() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_builder_target_os_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	os.write_file(os.join_path(tmp_dir, 'common.v'), 'module test') or { panic(err) }
	os.write_file(os.join_path(tmp_dir, 'platform_linux.v'), 'module test') or { panic(err) }
	os.write_file(os.join_path(tmp_dir, 'platform_macos.v'), 'module test') or { panic(err) }

	macos_files := get_v_files_from_dir(tmp_dir, []string{}, 'mac')
	macos_names := macos_files.map(os.file_name(it))
	assert 'common.v' in macos_names
	assert 'platform_macos.v' in macos_names
	assert 'platform_linux.v' !in macos_names

	linux_files := get_v_files_from_dir(tmp_dir, []string{}, 'linux')
	linux_names := linux_files.map(os.file_name(it))
	assert 'common.v' in linux_names
	assert 'platform_linux.v' in linux_names
	assert 'platform_macos.v' !in linux_names
}

fn test_get_v_files_from_dir_uses_d_and_notd_suffixes() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_builder_define_suffix_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	os.write_file(os.join_path(tmp_dir, 'common.v'), 'module test') or { panic(err) }
	os.write_file(os.join_path(tmp_dir, 'ssl_d_use_openssl.v'), 'module test') or { panic(err) }
	os.write_file(os.join_path(tmp_dir, 'ssl_notd_use_openssl.v'), 'module test') or { panic(err) }

	default_names := get_v_files_from_dir(tmp_dir, []string{}, 'mac').map(os.file_name(it))
	assert 'common.v' in default_names
	assert 'ssl_d_use_openssl.v' !in default_names
	assert 'ssl_notd_use_openssl.v' in default_names

	openssl_names := get_v_files_from_dir(tmp_dir, ['use_openssl'], 'mac').map(os.file_name(it))
	assert 'common.v' in openssl_names
	assert 'ssl_d_use_openssl.v' in openssl_names
	assert 'ssl_notd_use_openssl.v' !in openssl_names
}

fn test_get_v_files_from_dir_skips_prealloc_without_prealloc_flag() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_builder_prealloc_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	os.write_file(os.join_path(tmp_dir, 'common.v'), 'module test') or { panic(err) }
	os.write_file(os.join_path(tmp_dir, 'prealloc.c.v'), 'module test') or { panic(err) }

	default_names := get_v_files_from_dir(tmp_dir, []string{}, 'mac').map(os.file_name(it))
	assert 'common.v' in default_names
	assert 'prealloc.c.v' !in default_names

	prealloc_names := get_v_files_from_dir(tmp_dir, ['prealloc'], 'mac').map(os.file_name(it))
	assert 'common.v' in prealloc_names
	assert 'prealloc.c.v' in prealloc_names
}

fn test_get_v_files_from_dir_returns_lexically_sorted_files() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_builder_sorted_v_files_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	os.write_file(os.join_path(tmp_dir, 'foo.v'), 'module test') or { panic(err) }
	os.write_file(os.join_path(tmp_dir, 'foo.c.v'), 'module test') or { panic(err) }
	os.write_file(os.join_path(tmp_dir, 'bar.v'), 'module test') or { panic(err) }
	os.write_file(os.join_path(tmp_dir, 'ignore.txt'), 'not v') or { panic(err) }

	names := get_v_files_from_dir(tmp_dir, []string{}, 'linux').map(os.file_name(it))
	assert names == ['bar.v', 'foo.c.v', 'foo.v']
	assert names.index('foo.c.v') or { -1 } < names.index('foo.v') or { -1 }
}

fn test_flag_helpers_use_target_os() {
	assert flag_os_matches('macos', 'mac')
	assert flag_os_matches('bsd', 'mac')
	assert flag_os_matches('bsd', 'freebsd')
	assert !flag_os_matches('linux', 'mac')
	assert comptime_cond_matches('macos', 'mac')
	assert comptime_cond_matches('linux || bsd', 'mac')
	assert !comptime_cond_matches('linux', 'mac')
	assert comptime_cond_matches('macos && !linux', 'mac')
	macos_flag := parse_flag_directive_line('#flag macos -framework Cocoa', '/tmp/source.v', 'mac') or {
		''
	}
	linux_flag := parse_flag_directive_line('#flag linux -lm', '/tmp/source.v', 'mac') or { '' }
	assert macos_flag == '-framework Cocoa'
	assert linux_flag == ''
}

fn test_flag_helpers_expand_when_first_existing() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_builder_when_first_${os.getpid()}')
	existing_dir := os.join_path(tmp_dir, 'openssl_include')
	os.mkdir_all(existing_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	missing_dir := os.join_path(tmp_dir, 'missing')
	flag := parse_flag_directive_line("#flag darwin -I\$when_first_existing('${missing_dir}','${existing_dir}')", os.join_path(tmp_dir,
		'source.v'), 'mac') or { '' }
	assert flag == '-I${existing_dir}'
	none_flag := parse_flag_directive_line("#flag darwin -I\$when_first_existing('${missing_dir}')", os.join_path(tmp_dir,
		'source.v'), 'mac') or { '' }
	assert none_flag == ''
}

fn test_split_compile_and_link_flags_moves_c_sources_to_link_step() {
	compile_flags, link_flags :=
		split_compile_and_link_flags('-I /tmp/include -DMYFLAG thirdparty/sqlite/sqlite3.c -L/tmp/lib -lsqlite3 foo.o')
	assert compile_flags == '-I /tmp/include -DMYFLAG'
	assert link_flags == 'thirdparty/sqlite/sqlite3.c -L/tmp/lib -lsqlite3 foo.o'
}

fn test_sanitize_header_source_preserves_public_module_storage() {
	source := 'module state

pub __global (
	mut count int
	pub mut shared int
)

pub __global mut total int
'
	sanitized := sanitize_header_source(source, map[string]string{})
	assert sanitized.contains('pub __global (')
	assert sanitized.contains('mut count int')
	assert sanitized.contains('pub mut shared int')
	assert sanitized.contains('pub __global mut total int')
}

fn test_build_module_header_ast_preserves_module_storage_flags() {
	mut prefs := pref.new_preferences()
	mut b := new_builder(&prefs)
	source_file := ast.File{
		mod:   'state'
		name:  'state.v'
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: 'state'
			}),
			ast.Stmt(ast.GlobalDecl{
				is_public: true
				fields:    [
					ast.FieldDecl{
						name:      'private_count'
						typ:       ast.Ident{
							name: 'int'
						}
						is_mut:    true
						is_public: false
					},
					ast.FieldDecl{
						name:      'shared_count'
						typ:       ast.Ident{
							name: 'int'
						}
						is_mut:    true
						is_public: true
					},
				]
			}),
		]
	}
	header := b.build_module_header_ast([source_file], 'state') or {
		panic('missing module storage header')
	}
	mut found := false
	for stmt in header.stmts {
		if stmt is ast.GlobalDecl {
			found = true
			assert stmt.is_public
			assert stmt.fields.len == 2
			assert stmt.fields[0].name == 'private_count'
			assert stmt.fields[0].is_mut
			assert !stmt.fields[0].is_public
			assert stmt.fields[1].name == 'shared_count'
			assert stmt.fields[1].is_mut
			assert stmt.fields[1].is_public
		}
	}
	assert found
}

fn test_cflags_need_objc_mode_only_for_objc_inputs() {
	assert cflags_need_objc_mode('-framework Cocoa')
	assert cflags_need_objc_mode('/tmp/clipboard_darwin.m -framework Foundation')
	assert !cflags_need_objc_mode('-I /tmp/include -lssl -lcrypto /tmp/sqlite3.c')
	assert !cflags_need_objc_mode('-I /tmp/project.m/include')
}

fn test_file_has_incompatible_os_suffix_windows() {
	assert pref.file_has_incompatible_os_suffix('time_solaris.c.v', 'windows')
	assert pref.file_has_incompatible_os_suffix('time_freebsd.c.v', 'windows')
	assert pref.file_has_incompatible_os_suffix('time_openbsd.c.v', 'windows')
	assert pref.file_has_incompatible_os_suffix('time_netbsd.c.v', 'windows')
	assert pref.file_has_incompatible_os_suffix('time_dragonfly.c.v', 'windows')
	assert pref.file_has_incompatible_os_suffix('time_nix.c.v', 'windows')
	assert !pref.file_has_incompatible_os_suffix('time_windows.c.v', 'windows')
}

fn test_file_has_incompatible_os_suffix_non_windows_targets() {
	assert !pref.file_has_incompatible_os_suffix('time_solaris.c.v', 'solaris')
	assert !pref.file_has_incompatible_os_suffix('time_freebsd.c.v', 'freebsd')
	assert !pref.file_has_incompatible_os_suffix('time_bsd.c.v', 'macos')
	assert pref.file_has_incompatible_os_suffix('time_bsd.c.v', 'linux')
	assert !pref.file_has_incompatible_os_suffix('time_darwin.c.v', 'macos')
	assert !pref.file_has_incompatible_os_suffix('time_macos.c.v', 'darwin')
	assert !pref.file_has_incompatible_os_suffix('time_android_outside_termux.c.v', 'android')
	assert pref.file_has_incompatible_os_suffix('time_windows.c.v', 'linux')
	assert pref.file_has_incompatible_os_suffix('time_android_outside_termux.c.v', 'linux')
}

fn test_default_cc_uses_tcc_when_available() {
	vroot := os.dir(os.dir(@FILE))
	tcc_path := os.join_path(vroot, 'thirdparty', 'tcc', 'tcc.exe')
	cc := default_cc(vroot)
	if os.exists(tcc_path) {
		assert cc.contains('tcc')
	} else {
		assert cc == 'cc'
	}
}

fn test_cc_recompile_flags_from_cmd_keeps_include_paths() {
	cmd := 'cc -I /tmp/include -I/tmp/other -Dfoo=1 -x objective-c -std=gnu11 -fwrapv "/tmp/main.c" -x none "/tmp/lib.o" -L/tmp/lib -lm -o "app"'
	flags := cc_recompile_flags_from_cmd(cmd)
	assert flags.contains('-I /tmp/include')
	assert flags.contains('-I/tmp/other')
	assert flags.contains('-Dfoo=1')
	assert flags.contains('-x objective-c')
	assert flags.contains('-std=gnu11')
	assert flags.contains('-fwrapv')
	assert !flags.contains('/tmp/lib.o')
	assert !flags.contains('-L/tmp/lib')
	assert !flags.contains('-lm')
	assert !flags.contains('-o')
}
