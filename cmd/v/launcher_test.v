module main

import os

fn test_compiler_selection_flags_are_not_forwarded() {
	assert clean_compiler_selection_flags(['-silent', '-new-compiler', 'main.v']) == [
		'-silent',
		'main.v',
	]
	assert clean_compiler_selection_flags(['-old-compiler', 'run', 'main.v']) == [
		'run',
		'main.v',
	]
}

fn test_launcher_finds_the_source_root() {
	root := find_vroot(@FILE) or { panic(err) }
	assert os.is_file(os.join_path(root, 'GNUmakefile'))
	assert os.is_dir(os.join_path(root, 'vlib', 'v'))
	directory_root := find_vroot(root) or { panic(err) }
	assert directory_root == root
}

fn test_launcher_finds_external_commands() {
	index, command := find_command(['-cc', 'clang', 'fmt', '-w', 'main.v'])
	assert index == 2
	assert command == 'fmt'
	missing_index, missing := find_command(['-silent', 'main.v'])
	assert missing_index == -1
	assert missing == ''
	program_arg_index, program_arg := find_command(['run', 'main.v', 'fmt'])
	assert program_arg_index == -1
	assert program_arg == ''
	option_value_index, option_value := find_command(['-o', 'fmt', 'main.v'])
	assert option_value_index == -1
	assert option_value == ''
}

fn test_json_quote_escapes_report_content() {
	assert json_quote('a\n"b"\\c\t') == '"a\\n\\"b\\"\\\\c\\t"'
}

fn test_v1_fallback_installer_uses_last_v1_snapshot() {
	root := find_vroot(@FILE) or { panic(err) }
	installer := os.read_file(os.join_path(root, 'cmd', 'tools', 'install_v1_fallback.sh')) or {
		panic(err)
	}
	assert installer.contains('fallback_revision=0613e1f6fc68573f5b679e406ce58a00d6ebeb30')
	assert installer.contains('fallback_vc_revision=e658629fc4bd59826bd7637cde498d0c6236b14b')
	assert installer.contains('oldv_workdir=\$cache_parent/sources/\${fallback_short_revision}_\${fallback_vc_short_revision}')
	assert installer.contains('oldv_source_dir=\$oldv_workdir/v_at_\${fallback_revision}_vc_\${fallback_vc_revision}')
	assert installer.contains('lock_dir=\$oldv_workdir.lock')
	assert installer.contains('lock_owner_file=\$lock_dir/owner')
	assert installer.contains('lock_timeout_seconds=600')
	assert installer.contains('while ! mkdir "$lock_dir" 2>/dev/null; do')
	assert installer.contains('kill -0 "$lock_owner_pid" 2>/dev/null')
	assert installer.contains('mv "$lock_dir" "$stale_lock" 2>/dev/null')
	assert installer.contains('Timed out waiting for the V1 fallback cache lock')
	assert installer.contains('rmdir "$lock_dir" 2>/dev/null || true')
	assert installer.contains("VFLAGS= OLDV_VFLAGS='-d v1_fallback'")
	assert installer.contains('set -- "$@" --vccommit "$fallback_vc_revision"')
	assert installer.contains('set -- "$@" "$fallback_revision"')
	assert installer.contains("config --get-regexp '^remote\\..*\\.promisor$'")
	assert installer.contains('[ -z "$promisor_config" ] || return 1')
	assert installer.contains('rev-parse --is-shallow-repository')
	assert installer.contains('[ "$is_shallow" = false ] || return 1')
	assert installer.contains('show-ref --verify --quiet refs/heads/master || return 1')
	assert installer.contains('oldv_copy=\'copy /Y .\\v.exe "%V1_FALLBACK_TARGET%" >NUL\'')
	assert !installer.contains('V1_FALLBACK_ROOT_TARGET')
	assert !installer.contains('releases/download')
	oldv := os.read_file(os.join_path(root, 'cmd', 'tools', 'oldv.v')) or { panic(err) }
	assert oldv.contains("workpath_commit := if context.commit_vc == '' {")
	assert oldv.contains("'\${context.commit_v}_vc_\${context.commit_vc}'")
	assert oldv.contains('normalized_workpath_for_commit(context.vgo.workdir, workpath_commit)')
}

fn test_gnumake_avoids_duplicate_windows_executable_suffix_for_fallback_bootstrap() {
	root := find_vroot(@FILE) or { panic(err) }
	makefile := os.read_file(os.join_path(root, 'GNUmakefile')) or { panic(err) }
	assert makefile.contains('V1_FALLBACK_BOOTSTRAP = \$(if \$(EXE_EXT),\$(if \$(filter %\$(EXE_EXT),\$(VEXE)),\$(VEXE),\$(VEXE)\$(EXE_EXT)),\$(VEXE))')
	assert makefile.contains("install_v1_fallback.sh '\$(V1_FALLBACK_BOOTSTRAP)' '\$(V1_FALLBACK_EXE)'")
	assert !makefile.contains("install_v1_fallback.sh '\$(VEXE)\$(EXE_EXT)'")
}

fn test_gnumake_uses_absolute_legacy_paths_for_fallback() {
	root := find_vroot(@FILE) or { panic(err) }
	makefile := os.read_file(os.join_path(root, 'GNUmakefile')) or { panic(err) }
	assert makefile.contains('LEGACYLIBS := \$(abspath \$(VROOT)/thirdparty/legacy)')
}

fn test_portable_make_uses_the_pinned_v1_fallback_installer() {
	root := find_vroot(@FILE) or { panic(err) }
	makefile := os.read_file(os.join_path(root, 'Makefile')) or { panic(err) }
	assert makefile.contains('VEXE ?= ./v')
	assert makefile.contains('V1_FALLBACK_EXE ?= ./v1_fallback')
	v1_recipe := makefile.all_after('\nv1:\n').all_before('\ncheck:\n')
	assert v1_recipe.contains("install_v1_fallback.sh '\$(VEXE)' '\$(V1_FALLBACK_EXE)'")
	assert !v1_recipe.contains('vc/v.c')
	assert !v1_recipe.contains('candidate=./v1_fallback')
}

fn test_cached_fallback_root_is_preferred_when_installed() {
	root := find_vroot(@FILE) or { panic(err) }
	fallback := os.join_path(root, v1_fallback_binary + $if windows { '.exe' } $else { '' })
	root_file := fallback + '.vroot'
	if !os.is_executable(fallback) || !os.is_file(root_file) {
		return
	}
	resolved := ensure_v1_fallback('test') or { panic(err) }
	assert os.dir(resolved) == os.read_file(root_file)!.trim_space()
	assert os.is_dir(os.join_path(os.dir(resolved), 'vlib', 'json2'))
}

fn test_fallback_installer_writes_a_native_windows_root() {
	root := find_vroot(@FILE) or { panic(err) }
	source := os.read_file(os.join_path(root, 'cmd', 'tools', 'install_v1_fallback.sh'))!
	writer := source.all_after('write_candidate_root() {').all_before('\n}\n\nlocal_git_repo()')
	assert writer.contains('MSYS*|MINGW*')
	assert writer.contains('cygpath -w')
	assert writer.contains('pwd -W')
	assert source.count('write_candidate_root "$candidate_root" || return 1') == 1
}
