import os
import os.font

fn env_snapshot(name string) (string, bool) {
	val := os.getenv_opt(name) or { return '', false }
	return val, true
}

fn restore_env(name string, value string, existed bool) {
	if existed {
		os.setenv(name, value, true)
	} else {
		os.unsetenv(name)
	}
}

fn normalize_test_path(path string) string {
	return path.replace(os.path_separator, '/')
}

fn test_tmpdir() {
	t := os.temp_dir()
	assert t.len > 0
	assert os.is_dir(t)
	tfile := t + os.path_separator + 'tmpfile.txt'
	os.rm(tfile) or {} // just in case
	tfile_content := 'this is a temporary file'
	os.write_file(tfile, tfile_content) or { panic(err) }
	tfile_content_read := os.read_file(tfile) or { panic(err) }
	assert tfile_content_read == tfile_content
	os.rm(tfile) or { panic(err) }
}

fn test_ensure_folder_is_writable() {
	tmp := os.temp_dir()
	os.ensure_folder_is_writable(tmp) or {
		eprintln('err: ${err}')
		assert false
	}
}

fn test_expand_tilde_to_home() {
	os.setenv('HOME', '/tmp/home/folder', true)
	os.setenv('USERPROFILE', r'\tmp\home\folder', true)

	home_test := os.join_path(os.home_dir(), 'test', 'tilde', 'expansion')
	home_expansion_test := os.expand_tilde_to_home(os.join_path('~', 'test', 'tilde', 'expansion'))
	assert home_test == home_expansion_test
	assert os.expand_tilde_to_home('~') == os.home_dir()
}

fn test_config_dir() {
	cdir := os.config_dir()!
	assert cdir.len > 0
	adir := '${cdir}/test-v-config'
	os.mkdir_all(adir)!
	os.rmdir(adir)!
	assert os.is_dir(cdir)
}

fn test_data_dir_prefers_platform_location() {
	xdg_data_home, had_xdg_data_home := env_snapshot('XDG_DATA_HOME')
	defer {
		restore_env('XDG_DATA_HOME', xdg_data_home, had_xdg_data_home)
	}
	$if windows {
		local_app_data, had_local_app_data := env_snapshot('LocalAppData')
		userprofile, had_userprofile := env_snapshot('USERPROFILE')
		test_root := os.join_path(os.temp_dir(), 'v_data_dir_windows_test_${os.getpid()}')
		defer {
			restore_env('LocalAppData', local_app_data, had_local_app_data)
			restore_env('USERPROFILE', userprofile, had_userprofile)
			os.rmdir_all(test_root) or {}
		}
		expected := os.join_path(test_root, 'LocalAppData')
		os.setenv('XDG_DATA_HOME', os.join_path(test_root, 'XdgDataHome'), true)
		os.setenv('LocalAppData', expected, true)
		os.setenv('USERPROFILE', os.join_path(test_root, 'UserProfile'), true)
		assert os.data_dir() == expected
		assert os.is_dir(expected)
	} $else {
		test_root := os.join_path(os.temp_dir(), 'v_data_dir_xdg_test_${os.getpid()}')
		defer {
			os.rmdir_all(test_root) or {}
		}
		expected := os.join_path(test_root, 'XdgDataHome')
		os.setenv('XDG_DATA_HOME', expected, true)
		assert os.data_dir() == expected
		assert os.is_dir(expected)
	}
}

fn test_vmodules_dir_without_home_falls_back_to_vtmp() {
	home, had_home := env_snapshot('HOME')
	userprofile, had_userprofile := env_snapshot('USERPROFILE')
	vmodules, had_vmodules := env_snapshot('VMODULES')
	defer {
		restore_env('HOME', home, had_home)
		restore_env('USERPROFILE', userprofile, had_userprofile)
		restore_env('VMODULES', vmodules, had_vmodules)
	}
	os.unsetenv('HOME')
	os.unsetenv('USERPROFILE')
	os.unsetenv('VMODULES')
	assert os.vmodules_dir() == os.join_path_single(os.vtmp_dir(), '.vmodules')
}

fn test_font_default_prefers_user_config_dir_font() ! {
	vui_font, had_vui_font := env_snapshot('VUI_FONT')
	defer {
		restore_env('VUI_FONT', vui_font, had_vui_font)
	}
	os.unsetenv('VUI_FONT')
	$if windows {
		app_data, had_app_data := env_snapshot('AppData')
		test_root := os.join_path(os.temp_dir(), 'v_font_config_windows_test_${os.getpid()}')
		defer {
			restore_env('AppData', app_data, had_app_data)
			os.rmdir_all(test_root) or {}
		}
		os.setenv('AppData', test_root, true)
		font_dir := os.join_path(test_root, 'v', 'fonts')
		expected := os.join_path(font_dir, 'UserFallback.ttf')
		os.mkdir_all(font_dir)!
		os.write_file(expected, 'fallback')!
		assert normalize_test_path(font.default()) == normalize_test_path(expected)
	} $else $if macos || darwin || ios {
		home, had_home := env_snapshot('HOME')
		userprofile, had_userprofile := env_snapshot('USERPROFILE')
		test_root := os.join_path(os.temp_dir(), 'v_font_config_macos_test_${os.getpid()}')
		defer {
			restore_env('HOME', home, had_home)
			restore_env('USERPROFILE', userprofile, had_userprofile)
			os.rmdir_all(test_root) or {}
		}
		os.setenv('HOME', test_root, true)
		os.unsetenv('USERPROFILE')
		font_dir := os.join_path(test_root, 'Library', 'Application Support', 'v', 'fonts')
		expected := os.join_path(font_dir, 'UserFallback.ttf')
		os.mkdir_all(font_dir)!
		os.write_file(expected, 'fallback')!
		assert normalize_test_path(font.default()) == normalize_test_path(expected)
	} $else {
		xdg_config_home, had_xdg_config_home := env_snapshot('XDG_CONFIG_HOME')
		home, had_home := env_snapshot('HOME')
		test_root := os.join_path(os.temp_dir(), 'v_font_config_xdg_test_${os.getpid()}')
		defer {
			restore_env('XDG_CONFIG_HOME', xdg_config_home, had_xdg_config_home)
			restore_env('HOME', home, had_home)
			os.rmdir_all(test_root) or {}
		}
		os.setenv('XDG_CONFIG_HOME', test_root, true)
		os.setenv('HOME', test_root, true)
		font_dir := os.join_path(test_root, 'v', 'fonts')
		expected := os.join_path(font_dir, 'UserFallback.ttf')
		os.mkdir_all(font_dir)!
		os.write_file(expected, 'fallback')!
		assert normalize_test_path(font.default()) == normalize_test_path(expected)
	}
}
