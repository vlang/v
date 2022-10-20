import os

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
		eprintln('err: $err')
		assert false
	}
}

fn test_expand_tilde_to_home() {
	os.setenv('HOME', '/tmp/home/folder', true)
	os.setenv('USERPROFILE', '/tmp/home/folder', true)
	//
	home_test := os.join_path(os.home_dir(), 'test', 'tilde', 'expansion')
	home_expansion_test := os.expand_tilde_to_home(os.join_path('~', 'test', 'tilde',
		'expansion'))
	assert home_test == home_expansion_test
	assert os.expand_tilde_to_home('~') == os.home_dir()
}

fn test_config_dir() {
	cdir := os.config_dir()!
	assert cdir.len > 0
	adir := '$cdir/test-v-config'
	os.mkdir_all(adir)!
	os.rmdir(adir)!
	assert os.is_dir(cdir)
}
