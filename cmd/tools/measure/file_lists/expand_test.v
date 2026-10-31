module file_lists

import os

// expand_files_test covers the file-list expansion used by the `measure` speed
// tools (`scanner_speed.v`, `parser_speed.v`, `fmt_speed.v`).
// The callers' `main()`/`process_files()` are not unit testable: they print a
// report and measure wall-clock time.

fn fl_make_file(path string, content string) {
	os.write_file(path, content) or { panic(err) }
}

// fl_tree builds `<vroot>/file_lists_probe_<pid>` and returns its path.
fn fl_tree() !string {
	root := os.join_path(os.vtmp_dir(), 'file_lists_probe_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	return root
}

fn test_expand_files_skips_empty_and_missing_entries() {
	res := expand_files(['', 'definitely_missing_${os.getpid()}.v'])!
	assert res == []
}

fn test_expand_files_keeps_plain_files_verbatim() {
	res := expand_files(['LICENSE', 'README.md', 'v.mod'])!
	assert res == ['LICENSE', 'README.md', 'v.mod']
}

fn test_expand_files_does_not_deduplicate() {
	res := expand_files(['LICENSE', 'LICENSE'])!
	assert res == ['LICENSE', 'LICENSE']
}

fn test_expand_files_collects_v_and_vsh_from_a_folder() {
	root := fl_tree()!
	defer {
		os.rmdir_all(root) or {}
	}
	fl_make_file(os.join_path(root, 'a.v'), 'module a')
	fl_make_file(os.join_path(root, 'b.vsh'), 'x')
	fl_make_file(os.join_path(root, 'notes.txt'), 'x')
	os.mkdir_all(os.join_path(root, 'sub'))!
	fl_make_file(os.join_path(root, 'sub', 'd.v'), 'module d')
	mut got := expand_files([root])!.map(os.file_name(it))
	// Folder order is platform dependent; the set of collected files is not.
	got.sort()
	assert got == ['a.v', 'b.vsh', 'd.v']
}

fn test_expand_files_reads_an_at_list() {
	root := fl_tree()!
	defer {
		os.rmdir_all(root) or {}
	}
	fl_make_file(os.join_path(root, 'list.lst'), 'one.v\ntwo.vsh\n\nnope.v\n')
	fl_make_file(os.join_path(root, 'one.v'), 'module one')
	fl_make_file(os.join_path(root, 'two.vsh'), 'x')
	cwd := os.getwd()
	os.chdir(root) or { panic(err) }
	defer {
		os.chdir(cwd) or {}
	}
	res := expand_files(['@list.lst'])!
	assert res == ['one.v', 'two.vsh']
}

fn test_expand_files_expands_a_nested_at_list() {
	root := fl_tree()!
	defer {
		os.rmdir_all(root) or {}
	}
	fl_make_file(os.join_path(root, 'outer.lst'), '@inner.lst\nthree.v\n')
	fl_make_file(os.join_path(root, 'inner.lst'), 'one.v\n')
	fl_make_file(os.join_path(root, 'one.v'), 'module one')
	fl_make_file(os.join_path(root, 'three.v'), 'module three')
	cwd := os.getwd()
	os.chdir(root) or { panic(err) }
	defer {
		os.chdir(cwd) or {}
	}
	res := expand_files(['@outer.lst'])!
	assert res == ['one.v', 'three.v']
}

// NOTE: `expand_files` derives the list path from `files[0]`, not from the entry
// it is iterating, so two `@list` arguments expand the first list twice and the
// second one is never opened. This is the current behaviour and is asserted
// here, not fixed.
fn test_expand_files_uses_the_first_argument_for_every_at_entry() {
	root := fl_tree()!
	defer {
		os.rmdir_all(root) or {}
	}
	fl_make_file(os.join_path(root, 'l1.lst'), 'one.v\n')
	fl_make_file(os.join_path(root, 'l2.lst'), 'two.v\n')
	fl_make_file(os.join_path(root, 'one.v'), 'module one')
	fl_make_file(os.join_path(root, 'two.v'), 'module two')
	cwd := os.getwd()
	os.chdir(root) or { panic(err) }
	defer {
		os.chdir(cwd) or {}
	}
	res := expand_files(['@l1.lst', '@l2.lst'])!
	assert res == ['one.v', 'one.v']
}

fn test_expand_files_reports_a_missing_list_file() {
	root := fl_tree()!
	defer {
		os.rmdir_all(root) or {}
	}
	if res := expand_files(['@missing.lst']) {
		assert false, 'expected an error, got ${res}'
	} else {
		assert err.msg().starts_with('failed to open file'), err.msg()
	}
}
