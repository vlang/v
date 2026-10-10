import os

const tfolder = os.join_path(os.vtmp_dir(), 'os_ls_order_test_${os.getpid()}')

// Mixed case, dot-prefixed and non-ASCII names, like the directory of
// https://github.com/vlang/v/issues/30047, which NTFS, APFS, ext4 and tmpfs each
// list in a different order. No two names differ only by case, and the non-ASCII
// ones have no decomposed Unicode form, so a case-insensitive or a normalising
// filesystem stores every name exactly as it is written here.
const file_names = ['.hidden', '.hidden.txt', 'Bcase.txt', 'Zcase.txt', 'a b.txt', 'acase.txt',
	'straße.txt', 'жук.md']
const dir_name = 'adir'
const nested_file = 'inner.txt'

fn testsuite_begin() {
	os.rmdir_all(tfolder) or {}
	os.mkdir_all(os.join_path(tfolder, dir_name))!
	for name in file_names {
		os.write_file(os.join_path(tfolder, name), name)!
	}
	os.write_file(os.join_path(tfolder, dir_name, nested_file), nested_file)!
}

fn testsuite_end() {
	os.rmdir_all(tfolder) or {}
}

// sorted returns a sorted copy of `names`. os.ls and the walks built on it promise
// which entries they report, not the order, so every comparison here is made on
// sorted lists and none of them depends on the filesystem under os.vtmp_dir().
fn sorted(names []string) []string {
	mut res := names.clone()
	res.sort()
	return res
}

// in_tfolder returns the paths of `names` inside tfolder, as the walks report them.
fn in_tfolder(names []string) []string {
	return names.map(os.join_path(tfolder, it))
}

struct Visits {
mut:
	paths []string
}

fn (mut v Visits) add(path string) {
	v.paths << path
}

fn test_ls_returns_every_entry_once_without_dot_entries() {
	entries := os.ls(tfolder)!
	assert '.' !in entries
	assert '..' !in entries
	mut expected := file_names.clone()
	expected << dir_name
	assert sorted(entries) == sorted(expected)
}

fn test_ls_result_can_be_sorted_into_a_stable_order() {
	mut entries := os.ls(tfolder)!
	entries.sort()
	// Byte order: the dot, then upper case, then lower case, then the multi-byte names.
	assert entries == ['.hidden', '.hidden.txt', 'Bcase.txt', 'Zcase.txt', 'a b.txt', 'acase.txt',
		'adir', 'straße.txt', 'жук.md']
}

fn test_walk_ext_returns_the_files_that_ls_lists() {
	// walk_ext reports files only, and descends into `adir`.
	nested := os.join_path(tfolder, dir_name, nested_file)
	mut with_hidden := in_tfolder(file_names)
	with_hidden << nested
	assert sorted(os.walk_ext(tfolder, '', hidden: true)) == sorted(with_hidden)
	// Without `hidden: true`, the names that start with a dot are left out.
	mut visible := in_tfolder(file_names.filter(!it.starts_with('.')))
	visible << nested
	assert sorted(os.walk_ext(tfolder, '')) == sorted(visible)
	assert os.walk_ext(tfolder, '.md') == in_tfolder(['жук.md'])
}

fn test_walk_and_walk_with_context_visit_what_ls_lists() {
	nested := os.join_path(tfolder, dir_name, nested_file)
	mut files := in_tfolder(file_names)
	files << nested
	// walk calls back for files only.
	mut walked := Visits{}
	os.walk(tfolder, walked.add)
	assert sorted(walked.paths) == sorted(files)
	// walk_with_context calls back for the directories below the root as well.
	mut everything := files.clone()
	everything << os.join_path(tfolder, dir_name)
	mut seen := Visits{}
	os.walk_with_context(tfolder, &seen, fn (ctx voidptr, path string) {
		// The context arrives as a voidptr, so it has to be cast back.
		mut visits := unsafe { &Visits(ctx) }
		visits.add(path)
	})
	assert sorted(seen.paths) == sorted(everything)
}
