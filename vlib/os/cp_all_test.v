module os

fn cp_link_test_base() string {
	return join_path(vtmp_dir(), 'os_cp_all_symlink_${getpid()}')
}

// `cp -r` copies symlinks found in the tree as symlinks; cp_all must do the
// same rather than materializing the link target's (possibly outside-the-tree)
// contents into the destination.
fn test_cp_all_copies_directory_symlinks_as_links() {
	base := cp_link_test_base()
	defer {
		rmdir_all(base) or { panic('cleanup failed: ${err}') }
	}
	outside := join_path(base, 'vault')
	mkdir_all(outside) or { panic('${err}') }
	write_file(join_path(outside, 'vault.txt'), 'outside-vault-data')!
	tree := join_path(base, 'tree')
	mkdir_all(tree) or { panic('${err}') }
	write_file(join_path(tree, 'a.txt'), 'A')!
	symlink(outside, join_path(tree, 'link')) or {
		// some filesystems (e.g. Windows without symlink privileges) do not
		// allow the creation of the links this test needs
		eprintln('skipping: cannot create a symlink here: ${err}')
		return
	}
	dst := join_path(base, 'dst')

	cp_all(tree, dst, false)!

	assert is_link(join_path(dst, 'link'))
	// the reflections of the tree itself are plain files
	assert exists(join_path(dst, 'a.txt'))
	assert !exists(join_path(dst, 'vault.txt'))
	// the copied link still points at the vault
	$if !windows {
		link_target := readlink(join_path(dst, 'link')) or { panic('${err}') }
		assert link_target == outside
	}
}

// `cp -r` also copies file symlinks as symlinks
fn test_cp_all_copies_file_symlinks_as_links() {
	base := cp_link_test_base()
	defer {
		rmdir_all(base) or { panic('cleanup failed: ${err}') }
	}
	outside_doc := join_path(base, 'doc.txt')
	mkdir_all(base) or { panic('${err}') }
	write_file(outside_doc, 'bytes')!
	tree := join_path(base, 'tree')
	mkdir_all(tree) or { panic('${err}') }
	symlink(outside_doc, join_path(tree, 'flink')) or {
		eprintln('skipping: cannot create a symlink here: ${err}')
		return
	}
	dst := join_path(base, 'dst')

	cp_all(tree, dst, false)!

	assert is_link(join_path(dst, 'flink'))
	$if !windows {
		target := readlink(join_path(dst, 'flink')) or { panic('${err}') }
		assert target == outside_doc
	}
}

// a dangling link is copied as a dangling link
fn test_cp_all_copies_dangling_symlinks_as_links() {
	base := cp_link_test_base()
	defer {
		rmdir_all(base) or { panic('cleanup failed: ${err}') }
	}
	tree := join_path(base, 'tree')
	mkdir_all(tree) or { panic('${err}') }
	dangling := join_path(tree, 'dangling')
	symlink(join_path(base, 'nowhere'), dangling) or {
		eprintln('skipping: cannot create a symlink here: ${err}')
		return
	}
	dst := join_path(base, 'dst')

	cp_all(tree, dst, false)!

	assert is_link(join_path(dst, 'dangling'))
	assert !exists(join_path(dst, 'dangling'))
}

fn test_cp_all_refuses_non_overwrite_conflicts_and_recopies_overwrite() {
	base := cp_link_test_base()
	defer {
		rmdir_all(base) or { panic('cleanup failed: ${err}') }
	}
	outside := join_path(base, 'vault')
	mkdir_all(outside) or { panic('${err}') }
	write_file(join_path(outside, 'vault.txt'), 'outside-vault-data')!
	tree := join_path(base, 'tree')
	mkdir_all(tree) or { panic('${err}') }
	write_file(join_path(tree, 'a.txt'), 'A')!
	symlink(outside, join_path(tree, 'link')) or {
		eprintln('skipping: cannot create a symlink here: ${err}')
		return
	}
	dst := join_path(base, 'dst')

	// the first pass fills dst
	cp_all(tree, dst, false)!
	// without overwrite, a second pass reports the existing destination
	// entries with the same message the single-file copy uses
	mut second_msg := ''
	cp_all(tree, dst, false) or { second_msg = err.msg() }
	assert second_msg == 'Destination file path already exist'
	// with overwrite, the copied links are refreshed as links
	cp_all(tree, dst, true)!
	assert is_link(join_path(dst, 'link'))
	assert exists(join_path(dst, 'a.txt'))
	// and the vault is still not pulled into dst
	assert !exists(join_path(dst, 'vault.txt'))
	assert exists(join_path(outside, 'vault.txt'))
}
