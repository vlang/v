module main

import v.pref

fn test_macos_v3_relevant_command_only_selects_supported_native_c_builds() {
	$if macos {
		mut prefs := &pref.Preferences{
			path:      'main.v'
			backend:   .c
			os:        .macos
			is_script: true
		}
		assert is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_run = true
		assert is_macos_v3_relevant_command('run', prefs)
		prefs.os = .linux
		assert !is_macos_v3_relevant_command('run', prefs)
		prefs.os = .macos
		prefs.path = 'cmd/v'
		assert !is_macos_v3_relevant_command('cmd/v', prefs)
		prefs.path = 'cmd/tools/vfmt.v'
		assert !is_macos_v3_relevant_command('cmd/tools/vfmt.v', prefs)
		prefs.path = 'main.v'
		prefs.is_cstrict = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_cstrict = false
		prefs.is_test = true
		assert !is_macos_v3_relevant_command('test', prefs)
		prefs.is_test = false
		prefs.autofree = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.autofree = false
		prefs.is_prod = true
		assert !is_macos_v3_relevant_command('main.v', prefs)
		prefs.is_prod = false
		prefs.path = 'version'
		assert !is_macos_v3_relevant_command('version', prefs)
	}
}

fn test_macos_v3_default_executable_excludes_temporary_self_hosted_compilers() {
	$if macos {
		assert is_macos_v3_default_executable('/tmp/v')
		assert is_macos_v3_default_executable('/tmp/vnew')
		assert !is_macos_v3_default_executable('/tmp/v2')
		assert !is_macos_v3_default_executable('/tmp/vstrict1')
		assert !is_macos_v3_default_executable('/tmp/vp')
	}
}
