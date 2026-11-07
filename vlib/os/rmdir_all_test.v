module os

fn rm_test_base(suffix string) string {
	return os.join_path(vtmp_dir(), 'os_rmdir_all_${suffix}_${getpid()}')
}

// `rm -rf` on a symlink unlinks the link and leaves the target's contents
// untouched; rmdir_all must do the same instead of enumerating and deleting
// everything reachable *through* the link (which lies outside the tree the
// caller named), after which `rmdir(path)` fails with `Not a directory`.
fn test_rmdir_all_on_a_symlinked_root_unlinks_the_link() {
	base := rm_test_base('root')
	defer {
		rmdir_all(base) or { panic('cleanup failed: ${err}') }
	}
	outside := join_path(base, 'vault')
	mkdir_all(outside) or { panic('${err}') }
	write_file(join_path(outside, 'victim.txt'), 'IMPORTANT')!
	link := join_path(base, 'rootlink')
	symlink(outside, link) or {
		// some filesystems (e.g. Windows without symlink privileges) do not
		// allow the creation of the links this test needs
		eprintln('skipping: cannot create a symlink here: ${err}')
		return
	}

	rmdir_all(link) or { panic('rmdir_all on a symlinked root must succeed: ${err}') }

	assert !is_link(link)
	assert exists(join_path(outside, 'victim.txt'))
}

// the pre-existing child guard stays: links below the root are unlinked,
// their referents are untouched, and everything else is deleted
fn test_rmdir_all_keeps_target_of_a_child_link() {
	base := rm_test_base('child')
	defer {
		rmdir_all(base) or { panic('cleanup failed: ${err}') }
	}
	outside := join_path(base, 'vault')
	mkdir_all(outside) or { panic('${err}') }
	write_file(join_path(outside, 'victim.txt'), 'IMPORTANT')!
	tree := join_path(base, 'tree')
	mkdir_all(tree) or { panic('${err}') }
	write_file(join_path(tree, 'a.txt'), 'A')!
	symlink(outside, join_path(tree, 'link')) or {
		eprintln('skipping: cannot create a symlink here: ${err}')
		return
	}

	rmdir_all(tree) or { panic('${err}') }

	assert !exists(tree)
	// the child link was inside the tree; the vault (and its contents) are
	// outside the tree and untouched
	assert exists(join_path(outside, 'victim.txt'))
}

fn test_rmdir_all_still_removes_regular_trees() {
	base := rm_test_base('plain')
	defer {
		rmdir_all(base) or { panic('cleanup failed: ${err}') }
	}
	deep_a := join_path(base, 'a', 'b', 'c')
	mkdir_all(deep_a) or { panic('${err}') }
	write_file(join_path(deep_a, 'x.txt'), 'X')!

	rmdir_all(join_path(base, 'a')) or { panic('${err}') }

	assert !exists(join_path(base, 'a'))
}
