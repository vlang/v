import os

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
