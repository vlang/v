module os

// walk_kinds_tree makes a folder with a folder `sub` (holding `in_sub.txt`), a file `file.txt`,
// a link `link` to `sub`, a link `file_link` to the file and an empty folder `empty` in it.
fn walk_kinds_tree(name string) !string {
	folder := join_path(vtmp_dir(), '${name}_${getpid()}')
	rmdir_all(folder) or {}
	mkdir_all(join_path_single(folder, 'sub'))!
	mkdir(join_path_single(folder, 'empty'))!
	write_file(join_path(folder, 'sub', 'in_sub.txt'), 'x')!
	write_file(join_path_single(folder, 'file.txt'), 'x')!
	symlink(join_path_single(folder, 'sub'), join_path_single(folder, 'link'))!
	symlink(join_path_single(folder, 'file.txt'), join_path_single(folder, 'file_link'))!
	return folder
}

// entered_names is what walk_enter reports for `path` when its folder listed it as `typ`:
// whether it is a directory that is no link, and the names it added with their kinds, sorted.
fn entered_names(path string, typ u8) (bool, []string) {
	mut remaining := []WalkListed{}
	is_dir := walk_enter(WalkListed{ path: path, typ: typ }, mut remaining)
	mut names := remaining.map('${file_name(it.path)}:${it.typ}')
	names.sort()
	return is_dir, names
}

const walk_test_type_file = u8(8)
const walk_test_type_link = u8(10)

// The listing of a folder says what each entry is, and the walk carries that with the path.
fn test_walk_enter_adds_the_entries_with_what_the_listing_says_they_are() {
	folder := walk_kinds_tree('walk_enter_kinds')!
	defer {
		rmdir_all(folder) or {}
	}
	is_dir, names := entered_names(folder, walk_type_dir)
	assert is_dir
	// A file system that does not say what an entry is gives 0 for each of them.
	if names.all(it.ends_with(':0')) {
		eprintln('> ${@FN}: the file system of ${folder} does not say what an entry is')
		assert names == ['empty:0', 'file.txt:0', 'file_link:0', 'link:0', 'sub:0']
		return
	}
	assert names == ['empty:4', 'file.txt:8', 'file_link:10', 'link:10', 'sub:4']
	mut remaining := []WalkListed{}
	assert walk_enter(WalkListed{ path: folder, typ: walk_type_dir }, mut remaining)
	assert remaining.all(it.path.starts_with(folder + path_separator))
	// An empty folder is entered, and adds nothing.
	empty_is_dir, in_empty := entered_names(join_path_single(folder, 'empty'), walk_type_dir)
	assert empty_is_dir
	assert in_empty == []
}

// What the listing called a directory is opened without following a link, so what it has
// become by then decides, and a link is never listed.
fn test_walk_enter_opens_what_was_listed_as_a_directory_without_following_a_link() {
	folder := walk_kinds_tree('walk_enter_dir')!
	defer {
		rmdir_all(folder) or {}
	}
	mut is_dir, mut names := entered_names(join_path_single(folder, 'sub'), walk_type_dir)
	assert is_dir
	assert names.len == 1 && names[0].starts_with('in_sub.txt:')
	// It has become a link to a directory: an entry, and what is behind it is not listed.
	is_dir, names = entered_names(join_path_single(folder, 'link'), walk_type_dir)
	assert !is_dir
	assert names == []
	// It has become a file, a link to a file, or nothing.
	for name in ['file.txt', 'file_link', 'missing'] {
		is_dir, names = entered_names(join_path_single(folder, name), walk_type_dir)
		assert !is_dir, name
		assert names == [], name
	}
}

// What the listing called a file or a link is an entry, and is not looked at again: also when
// it has become a directory by the time the walk reaches it.
fn test_walk_enter_does_not_look_again_at_what_was_listed_as_no_directory() {
	folder := walk_kinds_tree('walk_enter_listed')!
	defer {
		rmdir_all(folder) or {}
	}
	for typ in [walk_test_type_file, walk_test_type_link, u8(1), u8(2), u8(6), u8(12)] {
		is_dir, names := entered_names(join_path_single(folder, 'sub'), typ)
		assert !is_dir, 'listed as ${typ}'
		assert names == [], 'listed as ${typ}'
	}
}

// Where the listing does not say what an entry is, and for the root of a walk, the path is
// looked up by name, as every entry was before.
fn test_walk_enter_looks_up_by_name_what_the_listing_did_not_say() {
	folder := walk_kinds_tree('walk_enter_unknown')!
	defer {
		rmdir_all(folder) or {}
	}
	mut is_dir, mut names := entered_names(join_path_single(folder, 'sub'), walk_type_unknown)
	assert is_dir
	assert names.len == 1 && names[0].starts_with('in_sub.txt:')
	for name in ['link', 'file.txt', 'file_link', 'missing'] {
		is_dir, names = entered_names(join_path_single(folder, name), walk_type_unknown)
		assert !is_dir, name
		assert names == [], name
	}
}

// A directory that may not be listed is still a directory: it is passed over, and is not
// reported as an entry.
fn test_walk_enter_passes_over_a_directory_that_may_not_be_opened() {
	if getuid() == 0 {
		eprintln('> skipping ${@FN}: the superuser may open every directory')
		return
	}
	folder := walk_kinds_tree('walk_enter_denied')!
	sub := join_path_single(folder, 'sub')
	chmod(sub, 0o000)!
	defer {
		chmod(sub, 0o700) or {}
		rmdir_all(folder) or {}
	}
	for typ in [walk_type_dir, walk_type_unknown] {
		is_dir, names := entered_names(sub, typ)
		assert is_dir, 'listed as ${typ}'
		assert names == [], 'listed as ${typ}'
	}
}

// open_descriptors is the number of file descriptors this process holds.
fn open_descriptors() int {
	return (ls('/proc/self/fd') or { panic(err) }).len
}

// Each directory a walk lists is opened. One that is not closed again stays open for the life
// of the process, and no result of the walk shows it.
fn test_a_walk_closes_every_directory_it_opens() {
	folder := walk_kinds_tree('walk_enter_descriptors')!
	defer {
		rmdir_all(folder) or {}
	}
	nothing := fn (path string) {}
	walk(folder, nothing)
	before := open_descriptors()
	mut added := []WalkListed{}
	for _ in 0 .. 200 {
		walk(folder, nothing)
		walk_with_context(folder, unsafe { nil }, fn (ctx voidptr, path string) {})
		// What fails to open as a directory leaves nothing open either.
		assert !walk_enter(WalkListed{ path: join_path_single(folder, 'link'), typ: walk_type_dir }, mut
			added)
		assert !walk_enter(WalkListed{ path: join_path_single(folder, 'file.txt'), typ: walk_type_dir }, mut
			added)
	}
	assert open_descriptors() == before
}

// Both walks report the same paths, in the same order, as a walk that looks every entry up by
// name, which is what they did before.
fn test_walks_report_what_a_walk_that_looks_every_entry_up_reports() {
	folder := walk_kinds_tree('walk_enter_whole')!
	defer {
		rmdir_all(folder) or {}
	}
	mkdir_all(join_path(folder, 'sub', 'deeper', 'deepest'))!
	write_file(join_path(folder, 'sub', 'deeper', 'deepest', 'bottom.txt'), 'x')!
	mut by_name_files := []string{}
	mut by_name_all := []string{}
	mut stack := [folder]
	mut first := true
	for stack.len > 0 {
		cur := stack.pop()
		if !first {
			by_name_all << cur
		}
		first = false
		if !walk_is_dir_by_name(cur) {
			by_name_files << cur
			continue
		}
		names := ls(cur)!
		for i := names.len - 1; i >= 0; i-- {
			stack << cur + path_separator + names[i]
		}
	}
	mut files := []string{}
	mut files_ref := &files
	walk(folder, fn [mut files_ref] (path string) {
		files_ref << path
	})
	mut all := []string{}
	walk_with_context(folder, &all, fn (ctx voidptr, path string) {
		mut paths := unsafe { &[]string(ctx) }
		paths << path
	})
	assert files == by_name_files
	assert all == by_name_all
	assert files.len == 5
	assert all.len == 9
}
