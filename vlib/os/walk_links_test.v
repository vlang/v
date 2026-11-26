import os

const troot = os.join_path(os.vtmp_dir(), 'os_walk_links_test_${os.getpid()}')

fn testsuite_begin() {
	os.rmdir_all(troot) or {}
	os.mkdir_all(os.join_path(troot, 'sub', 'deeper'))!
	os.mkdir_all(os.join_path(troot, 'empty'))!
	os.mkdir_all(os.join_path(troot, 'target'))!
	os.write_file(os.join_path(troot, 'a.txt'), 'a')!
	os.write_file(os.join_path(troot, 'sub', 'b.txt'), 'b')!
	os.write_file(os.join_path(troot, 'sub', 'deeper', 'c.txt'), 'c')!
	os.write_file(os.join_path(troot, 'target', 'inside.txt'), 'inside')!
}

fn testsuite_end() {
	// The link goes first, so that removing the tree can not reach through it.
	remove_dir_link(os.join_path(troot, 'link'))
	$if windows {
		// What a failed test can leave behind, and rmdir_all can not remove: a folder that may
		// not be listed, and names that end in a dot or a space.
		os.exec(['icacls', os.join_path(troot, 'denied', 'locked'), '/remove:d', '*S-1-1-0'])
		os.exec(['cmd', '/c', 'rmdir', '/s', '/q', r'\\?\' + os.join_path(troot, 'twins')])
	}
	os.rmdir_all(troot) or {}
}

// make_dir_link makes `link` a link to the directory `target`, and reports whether it could.
// On Windows it makes a junction, which unlike a symlink needs no privileges.
fn make_dir_link(target string, link string) bool {
	$if windows {
		return os.exec(['cmd', '/c', 'mklink', '/J', link, target]).exit_code == 0
	} $else {
		os.symlink(target, link) or { return false }
		return true
	}
}

fn remove_dir_link(link string) {
	$if windows {
		os.rmdir(link) or {}
	} $else {
		os.rm(link) or {}
	}
}

struct Seen {
mut:
	paths []string
}

fn (mut s Seen) add(path string) {
	s.paths << path
}

fn collect_with_context(ctx voidptr, path string) {
	mut seen := unsafe { &Seen(ctx) }
	seen.add(path)
}

// reference_walk visits `dir` the way os.walk and os.walk_with_context document it:
// entries in the order of os.ls, links reported and never followed.
fn reference_walk(dir string, with_dirs bool, mut out []string) {
	for name in os.ls(dir) or { return } {
		path := os.join_path_single(dir, name)
		if os.is_dir(path) && !os.is_link(path) {
			if with_dirs {
				out << path
			}
			reference_walk(path, with_dirs, mut out)
		} else {
			out << path
		}
	}
}

fn in_troot(parts ...string) string {
	mut path := troot
	for part in parts {
		path = os.join_path_single(path, part)
	}
	return path
}

fn test_walks_report_a_directory_link_without_following_it() {
	link := in_troot('link')
	if !make_dir_link(in_troot('target'), link) {
		eprintln('> skipping ${@FN}: could not create a directory link in ${troot}')
		return
	}
	assert os.is_link(link)
	behind_link := os.join_path_single(link, 'inside.txt')
	assert os.exists(behind_link)

	mut walked := Seen{}
	os.walk(troot, walked.add)
	assert link in walked.paths
	assert behind_link !in walked.paths
	mut files := [in_troot('a.txt'), in_troot('sub', 'b.txt'), in_troot('sub', 'deeper', 'c.txt'),
		in_troot('target', 'inside.txt'), link]
	files.sort()
	mut got := walked.paths.clone()
	got.sort()
	assert got == files

	mut seen := Seen{}
	os.walk_with_context(troot, &seen, collect_with_context)
	assert link in seen.paths
	assert behind_link !in seen.paths
	mut everything := files.clone()
	everything << [in_troot('sub'), in_troot('sub', 'deeper'), in_troot('empty'), in_troot('target')]
	everything.sort()
	mut got_all := seen.paths.clone()
	got_all.sort()
	assert got_all == everything

	// A link as the root is reported by walk as the single entry, and is not listed.
	mut from_link := Seen{}
	os.walk(link, from_link.add)
	assert from_link.paths == [link]
	mut ctx_from_link := Seen{}
	os.walk_with_context(link, &ctx_from_link, collect_with_context)
	assert ctx_from_link.paths == []
}

fn test_walks_visit_entries_in_the_order_of_ls() {
	mut expected_files := []string{}
	reference_walk(troot, false, mut expected_files)
	mut walked := Seen{}
	os.walk(troot, walked.add)
	assert walked.paths == expected_files

	mut expected_all := []string{}
	reference_walk(troot, true, mut expected_all)
	mut seen := Seen{}
	os.walk_with_context(troot, &seen, collect_with_context)
	assert seen.paths == expected_all
}

fn test_walks_ignore_what_is_not_a_directory() {
	mut nothing := Seen{}
	os.walk('', nothing.add)
	os.walk(in_troot('a.txt'), nothing.add)
	os.walk(in_troot('missing'), nothing.add)
	os.walk_with_context('', &nothing, collect_with_context)
	os.walk_with_context(in_troot('a.txt'), &nothing, collect_with_context)
	os.walk_with_context(in_troot('missing'), &nothing, collect_with_context)
	assert nothing.paths == []
}

// Changer is a walk callback that changes the tree once, at the first path it is given:
// it removes the folder `d1` or `d2` of `root`, whichever the walk has not reached yet, and
// with `link_to` set puts a link to that folder in its place.
struct Changer {
mut:
	root    string
	link_to string
	other   string // the folder that was changed
	linked  bool
	paths   []string
}

fn (mut c Changer) add(path string) {
	c.paths << path
	if c.other != '' {
		return
	}
	d1 := os.join_path_single(c.root, 'd1')
	d2 := os.join_path_single(c.root, 'd2')
	c.other = if path == d1 || path.starts_with(d1 + os.path_separator) { d2 } else { d1 }
	os.rmdir_all(c.other) or { panic(err) }
	if c.link_to != '' {
		c.linked = make_dir_link(c.link_to, c.other)
	}
}

fn change_with_context(ctx voidptr, path string) {
	mut c := unsafe { &Changer(ctx) }
	c.add(path)
}

// two_folders makes `root` hold the folders d1 and d2, with a file in each, and returns
// a folder outside of `root`, with a file that no walk of `root` may report.
fn two_folders(root string) !string {
	outside := root + '_outside'
	os.rmdir_all(root) or {}
	os.rmdir_all(outside) or {}
	os.mkdir_all(os.join_path(root, 'd1'))!
	os.mkdir_all(os.join_path(root, 'd2'))!
	os.mkdir_all(outside)!
	os.write_file(os.join_path(root, 'd1', 'inner.txt'), '1')!
	os.write_file(os.join_path(root, 'd2', 'inner.txt'), '2')!
	os.write_file(os.join_path(outside, 'secret.txt'), 's')!
	return outside
}

// The walk has listed d1 and d2 as folders by the time the callback runs. One of them then
// becomes a link, and the walk must see that when it gets there.
fn test_walks_do_not_enter_a_folder_that_became_a_link_during_the_walk() {
	root := in_troot('swap')
	for with_context in [false, true] {
		outside := two_folders(root)!
		mut c := &Changer{
			root:    root
			link_to: outside
		}
		if with_context {
			os.walk_with_context(root, c, change_with_context)
		} else {
			os.walk(root, c.add)
		}
		remove_dir_link(c.other)
		if !c.linked {
			eprintln('> skipping ${@FN}: could not create a directory link in ${root}')
			return
		}
		assert c.paths.filter(it.ends_with('secret.txt')) == [], 'with_context: ${with_context}'
		// The link itself is an entry of the walk, like any other link.
		assert c.paths.filter(it == c.other) == [c.other], 'with_context: ${with_context}'
		assert c.paths.filter(it.starts_with(c.other + os.path_separator)) == [], 'with_context: ${with_context}'
	}
}

// A folder that is gone by the time the walk reaches it is still passed to the callback,
// once, as the entry that its parent listed.
fn test_walks_report_a_folder_that_was_removed_during_the_walk() {
	root := in_troot('gone')
	for with_context in [false, true] {
		two_folders(root)!
		mut c := &Changer{
			root: root
		}
		if with_context {
			os.walk_with_context(root, c, change_with_context)
		} else {
			os.walk(root, c.add)
		}
		assert c.paths.filter(it == c.other) == [c.other], 'with_context: ${with_context}'
		assert c.paths.filter(it.starts_with(c.other + os.path_separator)) == [], 'with_context: ${with_context}'
	}
}

// Swapper is a walk callback that, at the first path it is given, replaces the other of the
// files `one.txt` and `two.txt` of `root` by a folder of that name, with a file in it.
struct Swapper {
mut:
	root  string
	other string // the file that became a folder
	paths []string
}

fn (mut s Swapper) add(path string) {
	s.paths << path
	if s.other != '' {
		return
	}
	one := os.join_path_single(s.root, 'one.txt')
	two := os.join_path_single(s.root, 'two.txt')
	s.other = if path == one { two } else { one }
	os.rm(s.other) or { panic(err) }
	os.mkdir(s.other) or { panic(err) }
	os.write_file(os.join_path_single(s.other, 'born.txt'), 'x') or { panic(err) }
}

fn swap_with_context(ctx voidptr, path string) {
	mut s := unsafe { &Swapper(ctx) }
	s.add(path)
}

// What the doc comments of walk and walk_with_context say about a file that the callback
// replaces by a folder before the walk reaches it: on Windows it is still reported as the
// entry that its folder listed, and not entered; elsewhere it is looked at again and entered.
fn test_walks_handle_a_file_that_became_a_folder_as_documented() {
	root := in_troot('became_folder')
	for with_context in [false, true] {
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		os.write_file(os.join_path_single(root, 'one.txt'), '1')!
		os.write_file(os.join_path_single(root, 'two.txt'), '2')!
		mut s := &Swapper{
			root: root
		}
		if with_context {
			os.walk_with_context(root, s, swap_with_context)
		} else {
			os.walk(root, s.add)
		}
		born := os.join_path_single(s.other, 'born.txt')
		$if windows {
			assert s.paths.filter(it == s.other) == [s.other], 'with_context: ${with_context}'
			assert born !in s.paths, 'with_context: ${with_context}'
		} $else {
			assert born in s.paths, 'with_context: ${with_context}'
		}
	}
}

// LinkSwapper is a walk callback that, at the first path it is given, replaces the other of
// the links `one` and `two` of `root` by a real folder of that name, with a file in it.
struct LinkSwapper {
mut:
	root  string
	other string // the link that became a folder
	paths []string
}

fn (mut s LinkSwapper) add(path string) {
	s.paths << path
	if s.other != '' {
		return
	}
	one := os.join_path_single(s.root, 'one')
	two := os.join_path_single(s.root, 'two')
	s.other = if path == one { two } else { one }
	remove_dir_link(s.other)
	os.mkdir(s.other) or { panic(err) }
	os.write_file(os.join_path_single(s.other, 'born.txt'), 'x') or { panic(err) }
}

fn link_swap_with_context(ctx voidptr, path string) {
	mut s := unsafe { &LinkSwapper(ctx) }
	s.add(path)
}

// What the doc comments of walk and walk_with_context say about an entry that was listed as a
// link and that the callback replaces by a folder before the walk reaches it: on Windows it is
// reported as the entry that its folder listed, and not entered; elsewhere it is looked at
// again and entered. What is behind the links is never reported.
fn test_walks_handle_a_link_that_became_a_folder_as_documented() {
	root := in_troot('link_became_folder')
	target := in_troot('link_became_folder_target')
	os.mkdir_all(target)!
	os.write_file(os.join_path_single(target, 'behind.txt'), 'behind')!
	one := os.join_path_single(root, 'one')
	two := os.join_path_single(root, 'two')
	defer {
		// The links go first, so that removing the tree can not reach through them.
		remove_dir_link(one)
		remove_dir_link(two)
	}
	for with_context in [false, true] {
		remove_dir_link(one)
		remove_dir_link(two)
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		linked := make_dir_link(target, one) && make_dir_link(target, two)
		$if windows {
			assert linked
		}
		if !linked {
			return
		}
		mut s := &LinkSwapper{
			root: root
		}
		if with_context {
			os.walk_with_context(root, s, link_swap_with_context)
		} else {
			os.walk(root, s.add)
		}
		born := os.join_path_single(s.other, 'born.txt')
		assert s.paths.filter(os.file_name(it) == 'behind.txt') == [], 'with_context: ${with_context}'
		$if windows {
			assert s.paths.filter(it == s.other) == [s.other], 'with_context: ${with_context}'
			assert born !in s.paths, 'with_context: ${with_context}'
		} $else {
			assert born in s.paths, 'with_context: ${with_context}'
		}
	}
}

// A file whose name ends in a dot or a space, beside a folder that has the name without that
// ending: under a plain path Windows reads both names as the folder. The listing says that
// each is a file, so the walk reports each once and lists nothing under its name.
fn test_walks_report_a_file_that_a_plain_path_reads_as_the_folder_beside_it() {
	$if !windows {
		return
	}
	root := in_troot('file_twin')
	sep := os.path_separator
	extended := r'\\?\' + root
	os.mkdir_all(os.join_path(root, 'x'))!
	defer {
		os.exec(['cmd', '/c', 'rmdir', '/s', '/q', extended])
	}
	os.write_file(os.join_path(root, 'x', 'in_x.txt'), 'in the folder x')!
	dotted := root + sep + 'x.'
	spaced := root + sep + 'x '
	os.write_file(r'\\?\' + dotted, 'the file x-dot')!
	os.write_file(r'\\?\' + spaced, 'the file x-space')!
	mut listed := os.ls(root)!
	listed.sort()
	assert listed == ['x', 'x ', 'x.']
	for with_context in [false, true] {
		mut seen := Seen{}
		if with_context {
			os.walk_with_context(root, &seen, collect_with_context)
		} else {
			os.walk(root, seen.add)
		}
		assert seen.paths.filter(it == dotted) == [dotted], 'with_context: ${with_context}'
		assert seen.paths.filter(it == spaced) == [spaced], 'with_context: ${with_context}'
		in_x := root + sep + 'x' + sep + 'in_x.txt'
		assert seen.paths.filter(os.file_name(it) == 'in_x.txt') == [in_x], 'with_context: ${with_context}'
	}
}

// A folder with more entries than one call for a part of its listing returns: every entry is
// reported, once, and in the order of os.ls.
fn test_walks_report_every_entry_of_a_folder_that_is_listed_in_several_parts() {
	root := in_troot('many')
	os.mkdir_all(root)!
	for i in 0 .. 1500 {
		os.write_file(os.join_path_single(root, 'file_with_a_long_enough_name_${i:04}.txt'), '')!
	}
	listed := os.ls(root)!
	assert listed.len == 1500
	expected := listed.map(os.join_path_single(root, it))
	for with_context in [false, true] {
		mut seen := Seen{}
		if with_context {
			os.walk_with_context(root, &seen, collect_with_context)
		} else {
			os.walk(root, seen.add)
		}
		assert seen.paths == expected, 'with_context: ${with_context}'
	}
}

// A plain path loses a space at the end of its last part, and keeps it in the middle. So the
// folder `gsp ` is read as `gsp` when it is looked at by name, while what is in it is read as
// being in `gsp `. The walk opens such a folder the way its entries are read: each folder is
// listed as itself, whether or not a folder with the name without the space is beside it.
fn test_walks_list_a_folder_whose_name_ends_in_a_space_as_itself() {
	$if !windows {
		return
	}
	root := in_troot('space_twin')
	sep := os.path_separator
	extended := r'\\?\' + root
	os.mkdir_all(os.join_path(root, 'gsp'))!
	defer {
		os.exec(['cmd', '/c', 'rmdir', '/s', '/q', extended])
	}
	for folder in ['gsp ', 'solo '] {
		assert os.exec(['cmd', '/c', 'mkdir', extended + sep + folder]).exit_code == 0
	}
	in_plain := root + sep + 'gsp' + sep + 'in_plain_gsp.txt'
	in_spaced := root + sep + 'gsp ' + sep + 'in_gsp_space.txt'
	in_solo := root + sep + 'solo ' + sep + 'in_solo_space.txt'
	os.write_file(in_plain, 'p')!
	os.write_file(r'\\?\' + in_spaced, 's')!
	os.write_file(r'\\?\' + in_solo, 'o')!
	for with_context in [false, true] {
		mut seen := Seen{}
		if with_context {
			os.walk_with_context(root, &seen, collect_with_context)
		} else {
			os.walk(root, seen.add)
		}
		mut files := seen.paths.filter(os.file_name(it).starts_with('in_'))
		files.sort()
		mut expected := [in_plain, in_spaced, in_solo]
		expected.sort()
		assert files == expected, 'with_context: ${with_context}'
	}
}

// Windows reads a plain path and the `\\?\` form of the same text differently: under a plain
// path a name loses a dot or a space at its end, so `foo.` is read as `foo`. A walk has to
// look at a folder and list it under one spelling, or it lists another folder than the one it
// looked at. Here `foo` and `foo.` are two folders with different files. The walk can only
// reach `foo`, twice, and must never report what is in the folders it can not open.
fn test_walks_do_not_list_a_folder_under_another_spelling_than_they_looked_at() {
	$if !windows {
		return
	}
	root := in_troot('twins')
	sep := os.path_separator
	extended := r'\\?\' + root
	os.mkdir_all(os.join_path(root, 'foo'))!
	defer {
		os.exec(['cmd', '/c', 'rmdir', '/s', '/q', extended])
	}
	// Each folder is made on its own: `mkdir` with a nested path drops the dot.
	for folder in ['foo${sep}sub.', 'foo.', 'foo.${sep}sub.'] {
		assert os.exec(['cmd', '/c', 'mkdir', extended + sep + folder]).exit_code == 0
	}
	os.write_file(extended + '${sep}foo${sep}in_plain.txt', 'p')!
	os.write_file(extended + '${sep}foo${sep}sub.${sep}below_plain.txt', 'a')!
	os.write_file(extended + '${sep}foo.${sep}in_dotted.txt', 'd')!
	os.write_file(extended + '${sep}foo.${sep}sub.${sep}below_dotted.txt', 'b')!
	for with_context in [false, true] {
		mut seen := Seen{}
		if with_context {
			os.walk_with_context(root, &seen, collect_with_context)
		} else {
			os.walk(root, seen.add)
		}
		names := seen.paths.map(os.file_name(it))
		// What only the `\\?\` spelling reaches stays unreported.
		assert 'in_dotted.txt' !in names, 'with_context: ${with_context}'
		assert 'below_dotted.txt' !in names, 'with_context: ${with_context}'
		assert 'below_plain.txt' !in names, 'with_context: ${with_context}'
		// `foo` is listed, under both names that Windows reads as `foo`.
		assert root + '${sep}foo${sep}in_plain.txt' in seen.paths
		assert root + '${sep}foo.${sep}in_plain.txt' in seen.paths
		// `sub.` can not be opened under its plain path, so it is one entry, as it always was.
		assert root + '${sep}foo${sep}sub.' in seen.paths
		assert root + '${sep}foo.${sep}sub.' in seen.paths
	}
}

// A folder that may not be listed is passed over: walk reports nothing for it, and
// walk_with_context reports the folder itself, which its parent listed.
fn test_walks_pass_over_a_folder_they_may_not_list() {
	$if !windows {
		return
	}
	root := in_troot('denied')
	locked := os.join_path_single(root, 'locked')
	os.mkdir_all(locked)!
	os.write_file(os.join_path_single(locked, 'in.txt'), 'x')!
	visible := os.join_path_single(root, 'visible.txt')
	os.write_file(visible, 'x')!
	// Deny everyone the right to list the folder; its attributes stay readable.
	denied := os.exec(['icacls', locked, '/deny', '*S-1-1-0:(RD)'])
	defer {
		os.exec(['icacls', locked, '/remove:d', '*S-1-1-0'])
	}
	mut listable := denied.exit_code != 0
	if _ := os.ls(locked) {
		listable = true
	}
	if listable {
		eprintln('> skipping ${@FN}: could not make ${locked} unlistable')
		return
	}
	mut walked := Seen{}
	os.walk(root, walked.add)
	assert walked.paths == [visible]
	mut seen := Seen{}
	os.walk_with_context(root, &seen, collect_with_context)
	mut got := seen.paths.clone()
	got.sort()
	assert got == [locked, visible]
}

// A folder that a process has as its current folder is held open by it. The walk must still
// be able to list it.
fn test_walks_list_the_current_folder_of_the_process() {
	old := os.getwd()
	os.chdir(in_troot('sub'))!
	defer {
		os.chdir(old) or {}
	}
	for with_context in [false, true] {
		mut seen := Seen{}
		if with_context {
			os.walk_with_context(in_troot('sub'), &seen, collect_with_context)
		} else {
			os.walk(in_troot('sub'), seen.add)
		}
		assert in_troot('sub', 'b.txt') in seen.paths, 'with_context: ${with_context}'
		assert in_troot('sub', 'deeper', 'c.txt') in seen.paths, 'with_context: ${with_context}'
	}
}

// Grower is a walk_with_context callback that puts a file into the folder `grow` at the moment
// it is called with that folder.
struct Grower {
mut:
	grow  string
	paths []string
}

fn grow_with_context(ctx voidptr, path string) {
	mut g := unsafe { &Grower(ctx) }
	g.paths << path
	if path == g.grow {
		os.write_file(os.join_path_single(path, 'born_in_grow.txt'), 'x') or { panic(err) }
	}
}

// walk_with_context calls the callback for a folder before it lists that folder, so what the
// callback puts into the folder it was given is reported too.
fn test_walk_with_context_reports_a_folder_before_it_lists_it() {
	root := in_troot('grows')
	grow := os.join_path_single(root, 'grow')
	os.rmdir_all(root) or {}
	os.mkdir_all(grow)!
	os.write_file(os.join_path_single(grow, 'old_in_grow.txt'), 'x')!
	mut g := &Grower{
		grow: grow
	}
	os.walk_with_context(root, g, grow_with_context)
	mut got := g.paths.clone()
	got.sort()
	assert got == [grow, os.join_path_single(grow, 'born_in_grow.txt'),
		os.join_path_single(grow, 'old_in_grow.txt')]
}

// A tree deeper than MAX_PATH (260 characters) on Windows, where every single name is short.
fn test_walks_reach_below_a_path_longer_than_260_characters() {
	long_root := in_troot('long')
	defer {
		os.rmdir_all(long_root) or {}
	}
	mut dir := long_root
	mut files := []string{}
	mut dirs := []string{}
	for i in 0 .. 12 {
		dir = os.join_path_single(dir, 'level_${i:02}_' + 'x'.repeat(31))
		file := os.join_path_single(dir, 'file_${i:02}.txt')
		os.mkdir_all(dir) or {
			eprintln('> skipping ${@FN}: this process can not create a path of ${dir.len} characters')
			return
		}
		os.write_file(file, 'x') or {
			eprintln('> skipping ${@FN}: this process can not create a path of ${file.len} characters')
			return
		}
		dirs << dir
		files << file
	}
	assert dir.len > 260 + troot.len
	// Compared sorted: whether a directory lists its file or its folder first is up to
	// the filesystem.
	files.sort()
	mut walked := Seen{}
	os.walk(long_root, walked.add)
	mut got_files := walked.paths.clone()
	got_files.sort()
	assert got_files == files
	mut seen := Seen{}
	os.walk_with_context(long_root, &seen, collect_with_context)
	mut everything := files.clone()
	everything << dirs
	everything.sort()
	mut got := seen.paths.clone()
	got.sort()
	assert got == everything
}

fn test_walk_accepts_a_trailing_separator_and_forward_slashes() {
	mut plain := Seen{}
	os.walk(troot, plain.add)
	mut trailing := Seen{}
	os.walk(troot + os.path_separator, trailing.add)
	assert trailing.paths == plain.paths
	// On Windows a root written with forward slashes is reported with backslashes;
	// elsewhere the replacement changes nothing.
	mut forward := Seen{}
	os.walk(troot.replace('\\', '/'), forward.add)
	assert forward.paths == plain.paths
	// A forward slash at the end is cut like any other separator.
	mut forward_trailing := Seen{}
	os.walk(troot.replace('\\', '/') + '/', forward_trailing.add)
	assert forward_trailing.paths == plain.paths
}
