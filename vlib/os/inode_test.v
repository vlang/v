module os

const (
	// tfolder will contain all the temporary files/subfolders made by
	// the different tests. It would be removed in testsuite_end(), so
	// individual os tests do not need to clean up after themselves.
	tfolder = join_path(temp_dir(), 'v', 'tests', 'inode_test')
)

fn testsuite_begin() {
	eprintln('testsuite_begin, tfolder = $os.tfolder')
	rmdir_all(os.tfolder) or {}
	assert !is_dir(os.tfolder)
	mkdir_all(os.tfolder) or { panic(err) }
	chdir(os.tfolder) or {}
	assert is_dir(os.tfolder)
}

fn testsuite_end() {
	chdir(wd_at_startup) or {}
	rmdir_all(os.tfolder) or { panic(err) }
	assert !is_dir(os.tfolder)
}

fn test_inode_file_type() {
	filename := './test1.txt'
	mut file := open_file(filename, 'w', 0o600) or { return }
	file.close()
	mode := inode(filename)
	rm(filename) or { panic(err) }
	assert mode.typ == .regular
}

fn test_inode_file_owner_permission() {
	filename := './test2.txt'
	mut file := open_file(filename, 'w', 0o600) or { return }
	file.close()
	mode := inode(filename)
	rm(filename) or {}
	assert mode.owner.read
	assert mode.owner.write
	assert !mode.owner.execute
}

fn test_inode_file_permissions_bitmask() {
	if user_os() == 'windows' {
		println('> skipping ${@FN} on windows')
		return
	}
	filename := './test3.txt'
	mut file := open_file(filename, 'w', 0o641) or { return }
	file.close()
	mode := inode(filename)
	rm(filename) or {}
	assert mode.bitmask() == 0o641
}
