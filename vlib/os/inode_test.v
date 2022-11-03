import os

const (
	// tfolder will contain all the temporary files/subfolders made by
	// the different tests. It would be removed in testsuite_end(), so
	// individual os tests do not need to clean up after themselves.
	tfolder = os.join_path(os.vtmp_dir(), 'v', 'tests', 'inode_test')
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
	os.rmdir_all(tfolder) or { panic(err) }
	assert !os.is_dir(tfolder)
}

fn test_inode_file_type() {
	filename := './test1.txt'
	mut file := os.open_file(filename, 'w', 0o600) or { return }
	file.close()
	mode := os.inode(filename)
	os.rm(filename) or { panic(err) }
	assert mode.typ == .regular
}

fn test_inode_file_owner_permission() {
	filename := './test2.txt'
	mut file := os.open_file(filename, 'w', 0o600) or { return }
	file.close()
	mode := os.inode(filename)
	os.rm(filename) or {}
	assert mode.owner.read
	assert mode.owner.write
	assert !mode.owner.execute
}

fn test_inode_file_permissions_bitmask() {
	if os.user_os() == 'windows' {
		println('> skipping ${@FN} on windows')
		return
	}
	filename := './test3.txt'
	mut file := os.open_file(filename, 'w', 0o641) or { return }
	file.close()
	mode := os.inode(filename)
	os.rm(filename) or {}
	assert mode.bitmask() == 0o641
}
