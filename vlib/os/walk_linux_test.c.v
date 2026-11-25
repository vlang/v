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

// listing_says_kinds returns whether the file system of `folder` says what each entry of it is.
// It reads the listing itself, so that it does not depend on what walk_enter makes of it.
fn listing_says_kinds(folder string) bool {
	listing := C.opendir(&char(folder.str))
	assert !isnil(listing), folder
	mut entries := 0
	mut unknown := 0
	for {
		ent := C.readdir(listing)
		if isnil(ent) {
			break
		}
		entries++
		if ent.d_type == 0 {
			unknown++
		}
	}
	C.closedir(listing)
	assert entries >= 2, folder
	return unknown == 0
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
	if !listing_says_kinds(folder) {
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
	if _ := ls(sub) {
		eprintln('> skipping ${@FN}: the file system of ${folder} still lets ${sub} be listed')
		return
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
	// Only the names `.` and `..` are left out of a listing: a name that starts with them is
	// an entry like any other.
	mkdir(join_path_single(folder, '.hidden_dir'))!
	write_file(join_path(folder, '.hidden_dir', 'in_hidden.txt'), 'x')!
	mkdir(join_path_single(folder, '..two_dots_dir'))!
	write_file(join_path(folder, '..two_dots_dir', '.x'), 'x')!
	write_file(join_path_single(folder, '.hidden'), 'x')!
	write_file(join_path_single(folder, '..two_dots'), 'x')!
	write_file(join_path_single(folder, '...'), 'x')!
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
	assert files.len == 10
	assert all.len == 16
	for name in ['.hidden', '..two_dots', '...', '.hidden_dir/in_hidden.txt', '..two_dots_dir/.x'] {
		assert join_path_single(folder, name) in files, name
	}
	// Every separator at the end of the root is left out of the reported paths.
	mut from_slashes := []string{}
	mut from_slashes_ref := &from_slashes
	walk(folder + '///', fn [mut from_slashes_ref] (path string) {
		from_slashes_ref << path
	})
	assert from_slashes == files
	mut all_from_slashes := []string{}
	walk_with_context(folder + '///', &all_from_slashes, fn (ctx voidptr, path string) {
		mut paths := unsafe { &[]string(ctx) }
		paths << path
	})
	assert all_from_slashes == all
}

// A directory that can not be opened because the process has no file descriptor left is still
// a directory: it is passed over like one that may not be opened, and not reported.
fn test_walks_pass_over_what_they_have_no_descriptor_left_to_open() {
	folder := walk_kinds_tree('walk_enter_no_descriptor')!
	defer {
		rmdir_all(folder) or {}
	}
	mut held := []File{}
	for held.len < 100000 {
		held << open('/dev/null') or { break }
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
	mut added := []WalkListed{}
	sub_is_dir := walk_enter(WalkListed{ path: join_path_single(folder, 'sub'), typ: walk_type_dir }, mut
		added)
	held_count := held.len
	for mut f in held {
		f.close()
	}
	if held_count == 100000 {
		eprintln('> skipping ${@FN}: the process may open more than ${held_count} files')
		return
	}
	assert files == []
	assert all == []
	assert sub_is_dir
	assert added.len == 0
	// With descriptors to spare again, the same walk lists the folder.
	walk(folder, fn [mut files_ref] (path string) {
		files_ref << path
	})
	assert files.len == 4
}

// WalkSwap is a walk callback that, at the first path it is given, replaces a file of `root`
// by a folder of the same name with a file in it: the file it was given when `itself` is set,
// else the other of `one.txt` and `two.txt`.
struct WalkSwap {
mut:
	root   string
	itself bool
	target string
	paths  []string
}

fn (mut s WalkSwap) add(path string) {
	s.paths << path
	if s.target != '' {
		return
	}
	one := join_path_single(s.root, 'one.txt')
	two := join_path_single(s.root, 'two.txt')
	s.target = if s.itself {
		path
	} else if path == one {
		two
	} else {
		one
	}
	rm(s.target) or { panic(err) }
	mkdir(s.target) or { panic(err) }
	write_file(join_path_single(s.target, 'born.txt'), 'x') or { panic(err) }
}

fn walk_swap_with_context(ctx voidptr, path string) {
	mut s := unsafe { &WalkSwap(ctx) }
	s.add(path)
}

// An entry that its folder listed as a file is reported once and not entered when it has
// become a folder: both when the callback changed another entry that the walk had still to
// reach, and when `walk_with_context` passed the callback the very entry that it changed.
// Where the listing does not say what an entry is, the entry is looked up when the walk reaches
// it, and entered.
fn test_walks_do_not_enter_what_was_listed_as_a_file_and_became_a_folder() {
	root := join_path(vtmp_dir(), 'walk_enter_swap_${getpid()}')
	defer {
		rmdir_all(root) or {}
	}
	for with_context in [false, true] {
		for itself in [false, true] {
			if itself && !with_context {
				// `walk` is done with a file once it has passed it to the callback.
				continue
			}
			rmdir_all(root) or {}
			mkdir_all(root)!
			write_file(join_path_single(root, 'one.txt'), '1')!
			write_file(join_path_single(root, 'two.txt'), '2')!
			says_kinds := listing_says_kinds(root)
			mut s := &WalkSwap{
				root:   root
				itself: itself
			}
			if with_context {
				walk_with_context(root, s, walk_swap_with_context)
			} else {
				walk(root, s.add)
			}
			born := join_path_single(s.target, 'born.txt')
			what := 'with_context: ${with_context}, itself: ${itself}, says_kinds: ${says_kinds}'
			if says_kinds {
				assert s.paths.filter(it == s.target).len == 1, what
				assert born !in s.paths, what
				assert s.paths.len == 2, what
			} else {
				// `walk` reports no folder that it enters.
				assert s.paths.filter(it == s.target).len == int(with_context), what
				assert born in s.paths, what
			}
		}
	}
}
