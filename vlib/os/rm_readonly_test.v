import os

const ro_folder = os.join_path(os.vtmp_dir(), 'os_rm_readonly_tests')

fn testsuite_begin() {
	os.rmdir_all(ro_folder) or {}
	assert !os.is_dir(ro_folder)
	os.mkdir_all(ro_folder)!
	assert os.is_dir(ro_folder)
}

fn testsuite_end() {
	os.rmdir_all(ro_folder) or {}
}

// A read-only file can be deleted on Windows. `_wremove` refuses it, so `rm`
// clears FILE_ATTRIBUTE_READONLY first. POSIX `unlink` never needed this,
// because it only requires write permission on the containing directory.
fn test_rm_deletes_a_read_only_file() {
	target := os.join_path(ro_folder, 'read_only.txt')
	os.write_file(target, 'read only payload')!
	// git marks its loose objects read-only, so this is the mode that matters.
	os.chmod(target, 0o444)!
	assert !os.is_writable(target)
	os.rm(target) or {
		assert false, 'os.rm should delete a read-only file, but got: ${err}'
	}
	assert !os.exists(target), 'os.rm reported success, but `${target}` is still there'
}

// `rmdir_all` walks into a tree whose files are read-only, which is what a git
// clone looks like on Windows: every object under `.git/objects/` is read-only.
// Before the fix the walk refused every file, then reported the trailing
// `rmdir(path)` failure ("directory not empty"), which named neither the file
// that failed nor the reason.
fn test_rmdir_all_removes_a_tree_with_read_only_files() {
	deep := os.join_path(ro_folder, 'objects', 'a1', 'b2')
	os.mkdir_all(deep)!
	for p in [os.join_path(deep, 'abcdef1234'), os.join_path(ro_folder, 'objects', 'top')] {
		os.write_file(p, 'blob')!
		os.chmod(p, 0o444)!
	}
	os.rmdir_all(os.join_path(ro_folder, 'objects')) or {
		assert false, 'os.rmdir_all should remove read-only files, but got: ${err}'
	}
	assert !os.exists(os.join_path(ro_folder, 'objects'))
}

// Windows holds the current directory open, so its removal reports the child
// that failed before the parent reports that it is not empty.
fn test_rmdir_all_reports_the_entry_that_failed_not_its_parent() {
	$if windows {
		top := os.join_path(ro_folder, 'top')
		bottom := os.join_path(top, 'bottom')
		os.mkdir_all(bottom)!
		os.write_file(os.join_path(bottom, 'leaf'), 'x')!
		previous := os.getwd()
		os.chdir(bottom)!
		defer {
			os.chdir(previous) or { panic(err) }
		}
		os.rmdir_all(top) or {
			assert err.msg().contains('bottom'), 'rmdir_all reported ${err.msg()}, which does not name `bottom`'
			return
		}
		assert false, 'os.rmdir_all of a tree with an undeletable directory unexpectedly succeeded'
	}
}

fn test_rmdir_all_keeps_the_first_file_deletion_error() {
	$if !windows {
		if os.geteuid() == 0 {
			return
		}
		dir := os.join_path(ro_folder, 'cannot_remove')
		target := os.join_path(dir, 'blocked.txt')
		os.mkdir_all(dir)!
		os.write_file(target, 'kept')!
		os.chmod(dir, 0o555)!
		defer {
			os.chmod(dir, 0o755) or {}
			os.rmdir_all(dir) or {}
		}
		os.rmdir_all(dir) or {
			assert os.is_permission_denied(err), err.msg()
			assert err.msg().contains(target), err.msg()
			assert os.exists(target)
			return
		}
		assert false, 'removing a file from a nonwritable directory should fail'
	}
}
