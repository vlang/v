import os

fn snapshot_env(name string) (string, bool) {
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

fn test_distro_temp_and_module_paths() {
	// The root branch deliberately uses /var paths, so only assert the
	// unprivileged behaviour here.
	if os.getuid() == 0 {
		return
	}
	names := ['VTMP', 'XDG_CACHE_HOME', 'HOME', 'TMPDIR', 'VMODULES']
	mut saved := map[string](string, bool){}
	for name in names {
		saved[name] = snapshot_env(name)
	}
	defer {
		for name in names {
			val, existed := saved[name]
			restore_env(name, val, existed)
		}
	}

	root := os.join_path(os.temp_dir(), 'v_distro_paths_${os.getpid()}')
	home := os.join_path(root, 'home')
	cache := os.join_path(root, 'cache')
	tmpdir := os.join_path(root, 'tmp')
	os.mkdir_all(home)!
	os.mkdir_all(cache)!
	os.mkdir_all(tmpdir)!
	defer {
		os.rmdir_all(root) or {}
	}

	os.unsetenv('VMODULES')
	os.setenv('HOME', home, true)
	os.setenv('XDG_CACHE_HOME', cache, true)
	os.unsetenv('VTMP')
	assert os.vtmp_dir() == os.join_path(cache, 'v', 'tmp')
	assert os.vmodules_dir() == os.join_path(home, '.vmodules')

	os.unsetenv('XDG_CACHE_HOME')
	os.setenv('HOME', os.join_path(root, 'missing-home'), true)
	os.setenv('TMPDIR', tmpdir, true)
	os.unsetenv('VTMP')
	assert os.vtmp_dir() == os.join_path(tmpdir, 'v_${os.getuid()}')
	os.unsetenv('VTMP')
	assert os.vmodules_dir() == os.join_path(os.vtmp_dir(), '.vmodules')
}
