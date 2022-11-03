import os
import io.util

const (
	// tfolder will contain all the temporary files/subfolders made by
	// the different tests. It would be removed in testsuite_end(), so
	// individual os tests do not need to clean up after themselves.
	tfolder = os.join_path(os.vtmp_dir(), 'v', 'tests', 'io_util_test')
)

fn testsuite_begin() {
	eprintln('testsuite_begin, tfolder = $tfolder')
	os.rmdir_all(tfolder) or {}
	assert !os.is_dir(tfolder)
	os.mkdir_all(tfolder) or { panic(err) }
	os.chdir(tfolder) or {}
	assert os.is_dir(tfolder)
}

fn testsuite_end() {
	os.chdir(os.wd_at_startup) or {}
	os.rmdir_all(tfolder) or {}
	assert !os.is_dir(tfolder)
	// eprintln('testsuite_end  , tfolder = $tfolder removed.')
}

fn test_temp_file() {
	// Test defaults
	mut f, mut path := util.temp_file() or {
		assert false
		return
	}
	mut prev_path := path
	defer {
		f.close()
	}
	assert os.is_file(path)
	assert f.is_opened
	// Test pattern
	f.close()
	f, path = util.temp_file(
		pattern: 'some_*_test.file'
	) or {
		assert false
		return
	}
	assert path != prev_path
	assert os.is_file(path)
	assert f.is_opened
	mut filename := os.file_name(path)
	assert filename.contains('_test.file')
	// Check for 9 digits where the wildcard is placed in the pattern
	for i, c in filename {
		if i > 4 && i <= 4 + 9 {
			assert c.is_digit()
		}
	}
	// Test custom path
	prev_path = path
	f.close()
	f, path = util.temp_file(
		path: tfolder
	) or {
		assert false
		return
	}
	assert path != prev_path
	assert os.is_file(path)
	assert path.contains(tfolder)
	assert f.is_opened
	filename = os.file_name(path)
	for c in filename {
		assert c.is_digit()
	}
}

fn test_temp_dir() {
	// Test defaults
	mut path := util.temp_dir() or {
		assert false
		return
	}
	assert os.is_dir(path)
	os.ensure_folder_is_writable(path) or {
		assert false
		return
	}
	mut prev_path := path
	// Test pattern
	path = util.temp_dir(
		pattern: 'some_*_test_dir'
	) or {
		assert false
		return
	}
	assert path != prev_path
	assert os.is_dir(path)
	mut filename := os.file_name(path)
	assert filename.contains('_test_dir')
	// Check for 9 digits where the wildcard is placed in the pattern
	for i, c in filename {
		if i > 4 && i <= 4 + 9 {
			assert c.is_digit()
		}
	}
	// Test custom path
	prev_path = path
	path = util.temp_dir(
		path: tfolder
	) or {
		assert false
		return
	}
	assert path != prev_path
	assert os.is_dir(path)
	os.ensure_folder_is_writable(path) or {
		assert false
		return
	}
	assert path.contains(tfolder)
	filename = os.file_name(path)
	for c in filename {
		assert c.is_digit()
	}
}
