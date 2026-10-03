module main

import os

// A make fixture is a file the lookup has to accept on this platform: Windows
// decides by extension, while a Unix host needs the executable bit on a name
// with no suffix.
fn write_make_fixture(dir string, name string) !string {
	path := os.join_path(dir, name + $if windows { '.bat' } $else { '' })
	$if windows {
		os.write_file(path, '@exit /b 0\n')!
	} $else {
		os.write_file(path, '#!/bin/sh\nexit 0\n')!
		os.chmod(path, 0o755)!
	}
	return path
}

// find_make_with_fixtures points PATH at a temporary directory holding one
// fixture per name, asks find_make() what it makes of them, and returns that
// answer together with the directory the fixtures live in.
fn find_make_with_fixtures(names ...string) !(?string, string) {
	root := os.join_path(os.vtmp_dir(), 'v1_fallback_find_make_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	for name in names {
		write_make_fixture(root, name)!
	}
	previous := os.getenv_opt('PATH')
	os.setenv('PATH', root, true)
	defer {
		if value := previous {
			os.setenv('PATH', value, true)
		} else {
			os.unsetenv('PATH')
		}
	}
	return find_make(), root
}

// fixture_base is the file name find_make() is expected to report for `name`.
fn fixture_base(name string) string {
	return name + $if windows { '.bat' } $else { '' }
}

fn test_find_make_prefers_make_over_gmake_and_mingw32_make() ! {
	result, root := find_make_with_fixtures('make', 'gmake', 'mingw32-make')!
	found := result or { panic('make was not found next to gmake and mingw32-make') }
	assert os.base(found) == fixture_base('make'), found
	assert os.dir(found) == root, found
}

fn test_find_make_falls_back_to_gmake() ! {
	result, root := find_make_with_fixtures('gmake')!
	found := result or { panic('gmake was not found on its own') }
	assert os.base(found) == fixture_base('gmake'), found
	assert os.dir(found) == root, found
}

fn test_find_make_uses_mingw32_make_on_windows_only() ! {
	result, _ := find_make_with_fixtures('mingw32-make')!
	// MSYS2's make is what a stock Windows install has; on a Unix host the same
	// name is a Windows cross-make, and building the fallback with it would
	// cross-compile the compatibility compiler instead of running it.
	$if windows {
		found := result or { panic('mingw32-make was not found on Windows') }
		assert os.base(found) == fixture_base('mingw32-make'), found
	} $else {
		assert result == none
	}
}

fn test_find_make_reports_nothing_when_path_has_no_make() ! {
	result, _ := find_make_with_fixtures()!
	assert result == none
}
