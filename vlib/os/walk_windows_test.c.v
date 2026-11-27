module os

// Preserve the SDK calling convention and pointer types at the indirect call.
#define v_os_walk_test_handle_count(address, process, count) (((BOOL (WINAPI *)(HANDLE, PDWORD))(address))((process), (count)))

fn C.v_os_walk_test_handle_count(address voidptr, process voidptr, count &C.DWORD) i32

// The empty path with a separator after it is the root of the current drive, which no walk
// was asked for.
fn test_walk_list_does_not_open_the_empty_path() {
	mut added := []WalkPending{}
	assert walk_list(WalkPending{ path: '', listed_as_dir: true }, mut added) == .other
	assert added.len == 0
}

struct WalkSeen {
mut:
	paths []string
}

fn (mut s WalkSeen) add(path string) {
	s.paths << path
}

fn walk_seen_add(ctx voidptr, path string) {
	mut s := unsafe { &WalkSeen(ctx) }
	s.add(path)
}

// both_walks returns what walk and what walk_with_context report for `root`, each sorted.
fn both_walks(root string) ([]string, []string) {
	mut files := &WalkSeen{}
	walk(root, files.add)
	mut all := &WalkSeen{}
	walk_with_context(root, all, walk_seen_add)
	files.paths.sort()
	all.paths.sort()
	return files.paths, all.paths
}

// A root that is nothing but separators has no name left once they are cut. The walks report
// nothing for it, and do not call the callback with an empty path.
fn test_walks_of_a_root_that_is_only_separators_report_nothing() {
	for root in ['\\', '/', '//', '\\/'] {
		files, all := both_walks(root)
		assert files == [], 'root: ${root}'
		assert all == [], 'root: ${root}'
	}
}

// The root is opened the way an entry is, so a name is read the same in both places: a folder
// named `NUL`, and one whose name ends in a space, are walked as a root as they are below one,
// and a file is no root, also when a plain path reads its name as the folder beside it.
fn test_walks_open_their_root_as_they_open_an_entry() {
	folder := join_path(vtmp_dir(), 'walk_roots_${getpid()}')
	extended := r'\\?\' + folder
	sep := path_separator
	rmdir_all(folder) or {}
	mkdir_all(join_path_single(folder, 'x'))!
	defer {
		exec(['cmd', '/c', 'rmdir', '/s', '/q', extended])
	}
	in_x := join_path(folder, 'x', 'in_x.txt')
	write_file(in_x, 'x')!
	file_twin := folder + sep + 'x '
	write_file(r'\\?\' + file_twin, 'the file x-space')!
	in_solo := folder + sep + 'solo ' + sep + 'in_solo_space.txt'
	in_nul := folder + sep + 'NUL' + sep + 'in_nul.txt'
	for name in ['solo ', 'NUL'] {
		assert exec(['cmd', '/c', 'mkdir', extended + sep + name]).exit_code == 0
	}
	write_file(r'\\?\' + in_solo, 's')!
	write_file(r'\\?\' + in_nul, 'n')!
	// Below a root: each folder is entered, and the file is one entry.
	mut files, mut all := both_walks(folder)
	mut expected := [in_x, file_twin, in_solo, in_nul]
	expected.sort()
	assert files == expected
	// As the root itself.
	files, all = both_walks(folder + sep + 'solo ')
	assert files == [in_solo]
	assert all == [in_solo]
	files, all = both_walks(folder + sep + 'NUL')
	assert files == [in_nul]
	assert all == [in_nul]
	files, all = both_walks(file_twin)
	assert files == []
	assert all == []
	// A device name alone, as a relative root, is the folder of that name.
	assert exec(['cmd', '/c', 'mkdir', extended + sep + 'CON']).exit_code == 0
	write_file(extended + sep + 'CON' + sep + 'in_con.txt', 'c')!
	old := getwd()
	chdir(folder)!
	files, all = both_walks('CON')
	chdir(old)!
	assert files == ['CON' + sep + 'in_con.txt']
	assert all == ['CON' + sep + 'in_con.txt']
}

// A link to a file is no directory, so it is no root: nothing is walked and nothing is reported,
// also when the link is reached through a share, where Windows opens it under `name\`.
fn test_walks_do_not_take_a_link_to_a_file_for_a_root() {
	folder := join_path(vtmp_dir(), 'walk_file_link_${getpid()}')
	rmdir_all(folder) or {}
	mkdir_all(folder)!
	defer {
		rmdir_all(folder) or {}
	}
	target := join_path_single(folder, 'target_of_link.txt')
	write_file(target, 'x')!
	symlink(target, join_path_single(folder, 'link_to_file')) or {
		eprintln('> skipping ${@FN}: could not create a symbolic link in ${folder}')
		return
	}
	mut files, mut all := both_walks(join_path_single(folder, 'link_to_file'))
	assert files == []
	assert all == []
	if folder.len < 3 || folder[1] != `:` {
		return
	}
	share := r'\\localhost\' + folder[..1] + '$' + folder[2..]
	if !is_dir(share) {
		eprintln('> skipping the rest of ${@FN}: ${share} can not be reached')
		return
	}
	files, all = both_walks(join_path_single(share, 'link_to_file'))
	assert files == []
	assert all == []
	// Below a root it is one entry, like any other link.
	files, all = both_walks(share)
	assert files == [join_path_single(share, 'link_to_file'),
		join_path_single(share, 'target_of_link.txt')]
	// The handle that the look at the link took is closed again.
	shared_link := WalkPending{
		path:          join_path_single(share, 'link_to_file')
		listed_as_dir: true
	}
	mut added := []WalkPending{}
	before := open_handles()
	for _ in 0 .. 200 {
		assert walk_list(shared_link, mut added) == .other
	}
	// When nothing says what the opened link is, it is no directory either: neither way of
	// listing it works.
	assert walk_list_with(shared_link, mut added, 3, walk_info_first) == .other
	assert added.len == 0
	grown := open_handles() - before
	assert grown < 100, 'the process holds ${grown} more handles after 200 looks at the link'
	// The link is no root either when it may not be opened.
	denied := exec(['icacls', join_path_single(folder, 'link_to_file'), '/L', '/deny', '*S-1-1-0:(RD)'])
	defer {
		exec(['icacls', join_path_single(folder, 'link_to_file'), '/L', '/remove:d', '*S-1-1-0'])
	}
	if denied.exit_code == 0 {
		files, all = both_walks(join_path_single(share, 'link_to_file'))
		assert files == []
		assert all == []
	}
}

// A folder that another handle holds open with the right to delete it, or to write to it, can
// only be opened by who shares that right.
fn test_walks_list_a_folder_that_another_handle_may_delete_or_write_to() {
	folder := join_path(vtmp_dir(), 'walk_held_${getpid()}')
	rmdir_all(folder) or {}
	mkdir_all(folder)!
	defer {
		rmdir_all(folder) or {}
	}
	in_held := join_path_single(folder, 'in_held.txt')
	write_file(in_held, 'x')!
	// DELETE, and FILE_ADD_FILE, which is a right to write: each is held in turn.
	for access in [u32(0x00010000), u32(0x00000002)] {
		wpath := folder.to_wide()
		// Shared with everyone, and FILE_FLAG_BACKUP_SEMANTICS to open a directory.
		held := C.CreateFileW(wpath, access, u32(7), unsafe { nil }, u32(C.OPEN_EXISTING),
			u32(0x02000000), unsafe { nil })
		unsafe { free(wpath) }
		assert held != invalid_handle_value, 'access: ${access}'
		files, all := both_walks(folder)
		C.CloseHandle(held)
		assert files == [in_held], 'access: ${access}'
		assert all == [in_held], 'access: ${access}'
	}
}

// To list a folder the walk asks for the right to list it, and not for every right to read
// it: a folder that denies another read right is still listed.
fn test_walks_list_a_folder_that_denies_another_read_right() {
	folder := join_path(vtmp_dir(), 'walk_rights_${getpid()}')
	rmdir_all(folder) or {}
	mkdir_all(folder)!
	defer {
		rmdir_all(folder) or {}
	}
	in_folder := join_path_single(folder, 'in_folder.txt')
	write_file(in_folder, 'x')!
	// Deny everyone the right to read extended attributes.
	denied := exec(['icacls', folder, '/deny', '*S-1-1-0:(REA)'])
	defer {
		exec(['icacls', folder, '/remove:d', '*S-1-1-0'])
	}
	if denied.exit_code != 0 {
		eprintln('> skipping ${@FN}: icacls could not change ${folder}')
		return
	}
	files, all := both_walks(folder)
	assert files == [in_folder]
	assert all == [in_folder]
}

// listed_names is what walk_list reports for `path`: what it found it to be, and the names
// of the entries it added, sorted.
fn listed_names(path string, listed_as_dir bool) (WalkKind, []string) {
	mut remaining := []WalkPending{}
	kind := walk_list(WalkPending{ path: path, listed_as_dir: listed_as_dir }, mut remaining)
	mut names := remaining.map(file_name(it.path))
	names.sort()
	return kind, names
}

fn test_walk_list_enters_only_what_was_listed_as_a_folder_and_still_is_one() {
	folder := join_path(vtmp_dir(), 'walk_list_${getpid()}')
	rmdir_all(folder) or {}
	mkdir_all(join_path_single(folder, 'target'))!
	defer {
		rmdir_all(folder) or {}
	}
	file := join_path_single(folder, 'file.txt')
	write_file(file, 'x')!
	write_file(join_path(folder, 'target', 'in_target.txt'), 'x')!
	link := join_path_single(folder, 'link')
	assert exec(['cmd', '/c', 'mklink', '/J', link, join_path_single(folder, 'target')]).exit_code == 0
	defer {
		// Before the folder is removed, so that removing it can not reach through the link.
		rmdir(link) or {}
	}
	mut kind, mut names := listed_names(folder, true)
	assert kind == .entered
	assert names == ['file.txt', 'link', 'target']
	// What the listing called a file is reported as one, and is not opened.
	kind, names = listed_names(folder, false)
	assert kind == .other
	assert names == []
	// What the listing called a folder is opened, and asked what it is now: a file,
	kind, names = listed_names(file, true)
	assert kind == .other
	assert names == []
	// nothing any more,
	kind, names = listed_names(join_path_single(folder, 'missing'), true)
	assert kind == .other
	assert names == []
	// or a link, which is neither entered nor listed,
	kind, names = listed_names(link, true)
	assert kind == .link
	assert names == []
	// also when the link itself may not be listed, so that it can not be opened.
	assert exec(['icacls', link, '/L', '/deny', '*S-1-1-0:(RD)']).exit_code == 0
	defer {
		exec(['icacls', link, '/L', '/remove:d', '*S-1-1-0'])
	}
	kind, names = listed_names(link, true)
	assert kind == .link
	assert names == []
}

// Every folder lists itself as `.` and its parent as `..`. A walk that kept either would enter
// the same folder again and again, so the listing must leave both out.
fn test_walk_list_leaves_out_the_folder_itself_and_its_parent() {
	folder := join_path(vtmp_dir(), 'walk_entries_${getpid()}')
	rmdir_all(folder) or {}
	mkdir_all(join_path_single(folder, 'sub'))!
	defer {
		rmdir_all(folder) or {}
	}
	write_file(join_path_single(folder, 'file.txt'), 'x')!
	// A junction to a folder: the listing calls it a directory and a link.
	link := join_path_single(folder, 'link')
	assert exec(['cmd', '/c', 'mklink', '/J', link, join_path_single(folder, 'sub')]).exit_code == 0
	defer {
		// Before the folder is removed, so that removing it can not reach through the link.
		rmdir(link) or {}
	}
	mut remaining := []WalkPending{}
	assert walk_list(WalkPending{ path: folder, listed_as_dir: true }, mut remaining) == .entered
	mut names := remaining.map(file_name(it.path))
	names.sort()
	assert names == ['file.txt', 'link', 'sub']
	// Only the folder that is not a link is what the listing called a folder.
	assert remaining.filter(it.listed_as_dir).map(file_name(it.path)) == ['sub']
	// Each entry is the folder, a separator and the name.
	assert remaining.all(it.path == folder + path_separator + file_name(it.path))
}

// A plain path loses a space at the end of its last part, so `gsp ` is read as `gsp` there,
// while in the paths of what is in `gsp ` the space stays. The walk opens `gsp ` the way those
// paths are read, so it lists the folder that its entries are in.
fn test_walk_list_opens_a_name_that_ends_in_a_space_as_its_entries_are_read() {
	folder := join_path(vtmp_dir(), 'walk_space_${getpid()}')
	extended := r'\\?\' + folder
	rmdir_all(folder) or {}
	mkdir_all(join_path_single(folder, 'gsp'))!
	defer {
		exec(['cmd', '/c', 'rmdir', '/s', '/q', extended])
	}
	spaced := folder + path_separator + 'gsp '
	assert exec(['cmd', '/c', 'mkdir', r'\\?\' + spaced]).exit_code == 0
	write_file(join_path(folder, 'gsp', 'in_plain_gsp.txt'), 'x')!
	write_file(r'\\?\' + spaced + path_separator + 'in_gsp_space.txt', 'x')!
	mut kind, mut names := listed_names(spaced, true)
	assert kind == .entered
	assert names == ['in_gsp_space.txt']
	kind, names = listed_names(join_path_single(folder, 'gsp'), true)
	assert kind == .entered
	assert names == ['in_plain_gsp.txt']
}

// `C:\` is cut to `C:`, which by itself is the current folder of that drive. The walk has to
// open the root of the drive, whatever the current folder is: here it is a link, which a walk
// that looked at the current folder would report as one entry and stop.
fn test_walk_list_reads_a_drive_as_its_root_and_not_as_its_current_folder() {
	folder := join_path(vtmp_dir(), 'walk_drive_${getpid()}')
	if folder.len < 3 || folder[1] != `:` {
		return
	}
	drive := folder[..2]
	rmdir_all(folder) or {}
	mkdir_all(join_path_single(folder, 'target'))!
	link := join_path_single(folder, 'link')
	assert exec(['cmd', '/c', 'mklink', '/J', link, join_path_single(folder, 'target')]).exit_code == 0
	old := getwd()
	chdir(link)!
	defer {
		chdir(old) or {}
		rmdir(link) or {}
		rmdir_all(folder) or {}
	}
	mut remaining := []WalkPending{}
	assert walk_list(WalkPending{ path: drive, listed_as_dir: true }, mut remaining) == .entered
	assert remaining.len > 0
	assert remaining.all(it.path.starts_with(drive + path_separator) && it.path.len > 3)
	// The first part of the test folder's own path is one of the entries of the root.
	top := folder[..3] + folder[3..].all_before(path_separator)
	assert remaining.any(it.path.to_lower() == top.to_lower())
}

// open_handles is the number of handles this process holds.
fn open_handles() int {
	// The bundled TCC import library omits GetProcessHandleCount, so it is looked up by name,
	// as in exec_handles_windows_test.c.v.
	kernel32 := C.GetModuleHandleA(c'kernel32.dll')
	assert kernel32 != 0
	address := C.GetProcAddress(kernel32, c'GetProcessHandleCount')
	assert address != unsafe { nil }
	mut count := C.DWORD(0)
	assert C.v_os_walk_test_handle_count(address, C.GetCurrentProcess(), &count) != 0
	return int(count)
}

// Each folder a walk lists opens a handle to it. One that is not closed again stays open for
// the life of the process, and no result of the walk shows it.
fn test_a_walk_closes_the_handle_of_every_folder_it_lists() {
	root := join_path(vtmp_dir(), 'walk_handles_${getpid()}')
	rmdir_all(root) or {}
	for name in ['a', 'b', 'c', 'd'] {
		mkdir_all(join_path(root, name, 'inner'))!
		write_file(join_path(root, name, 'inner', 'file.txt'), 'x')!
	}
	defer {
		rmdir_all(root) or {}
	}
	count := fn (path string) {}
	// One walk first, so that whatever the first use of anything opens is open already.
	walk(root, count)
	before := open_handles()
	rounds := 100
	// A link that is taken for a folder is opened as well, and not entered.
	link := join_path_single(vtmp_dir(), 'walk_handles_link_${getpid()}')
	assert exec(['cmd', '/c', 'mklink', '/J', link, root]).exit_code == 0
	defer {
		rmdir(link) or {}
	}
	mut added := []WalkPending{}
	for _ in 0 .. rounds {
		walk(root, count)
		walk_with_context(root, unsafe { nil }, fn (ctx voidptr, path string) {})
		assert walk_list(WalkPending{ path: link, listed_as_dir: true }, mut added) == .link
	}
	// 9 folders are listed per walk: 1800 handles would stay open if none were closed.
	grown := open_handles() - before
	assert grown < rounds, 'the process holds ${grown} more handles after ${2 * rounds} walks'
}

// walk_test_open opens `path` the way walk_list_with does. The caller closes the handle.
fn walk_test_open(path string) voidptr {
	wpath := (path + path_separator).to_wide()
	handle := C.CreateFileW(wpath, walk_access_list, walk_share_all, unsafe { nil }, u32(C.OPEN_EXISTING),
		walk_open_flags, unsafe { nil })
	unsafe { free(wpath) }
	assert handle != invalid_handle_value, path
	return handle
}

// walk_test_tree makes a folder with a folder `sub`, a file `file.txt` and a link `link` to
// `sub` in it. The caller removes it with walk_test_tree_remove.
fn walk_test_tree(name string) !string {
	folder := join_path(vtmp_dir(), '${name}_${getpid()}')
	rmdir(join_path_single(folder, 'link')) or {}
	rmdir_all(folder) or {}
	mkdir_all(join_path_single(folder, 'sub'))!
	write_file(join_path(folder, 'sub', 'in_sub.txt'), 'x')!
	write_file(join_path_single(folder, 'file.txt'), 'x')!
	assert exec(['cmd', '/c', 'mklink', '/J', join_path_single(folder, 'link'),
		join_path_single(folder, 'sub')]).exit_code == 0
	return folder
}

fn walk_test_tree_remove(folder string) {
	// The link first, so that removing the folder can not reach through it.
	rmdir(join_path_single(folder, 'link')) or {}
	rmdir_all(folder) or {}
}

// Some file systems and servers do not answer every way of asking what an open handle is. Each
// way that fails must hand over to the next one, and none may be taken for "not a directory".
fn test_walk_attributes_asks_another_way_when_one_fails() {
	folder := walk_test_tree('walk_attributes')!
	defer {
		walk_test_tree_remove(folder)
	}
	directory := u32(C.FILE_ATTRIBUTE_DIRECTORY)
	link_bits := directory | windows_file_attribute_reparse_point
	for name, want in {
		'sub':  directory
		'link': link_bits
	} {
		path := join_path_single(folder, name)
		wpath := (path + path_separator).to_wide()
		handle := walk_test_open(path)
		// Each of the three ways alone gives the same answer.
		for first_way in 0 .. 3 {
			got := walk_attributes(handle, wpath, first_way)
			assert got & link_bits == want, '${name}, from way ${first_way}: ${got:x}'
		}
		// With no way left, nothing is known.
		assert walk_attributes(handle, wpath, 3) == walk_attributes_unknown, name
		C.CloseHandle(handle)
		// Through a handle that answers nothing, the two ways that use it fail, and the look by
		// name still tells what the path is.
		got := walk_attributes(unsafe { nil }, wpath, 0)
		assert got & link_bits == want, '${name}, by name after two failures: ${got:x}'
		unsafe { free(wpath) }
	}
	// A folder that was renamed after it was opened is no longer found by its name. The two ways
	// that ask through the handle still answer, each of them alone.
	sub := join_path_single(folder, 'sub')
	wsub := (sub + path_separator).to_wide()
	handle := walk_test_open(sub)
	rename(sub, join_path_single(folder, 'renamed_sub'))!
	assert walk_attributes(handle, wsub, 0) & link_bits == directory
	assert walk_attributes(handle, wsub, 1) & link_bits == directory
	assert walk_attributes(handle, wsub, 2) == walk_attributes_unknown
	C.CloseHandle(handle)
	unsafe { free(wsub) }
	// Where the look by name fails as well, nothing is known.
	missing := (join_path_single(folder, 'missing') + path_separator).to_wide()
	assert walk_attributes(unsafe { nil }, missing, 0) == walk_attributes_unknown
	unsafe { free(missing) }
}

fn walk_test_names(remaining []WalkPending) []string {
	return remaining.map('${file_name(it.path)}:${it.listed_as_dir}')
}

// A listing that ends, and a listing that the file system does not offer, both stop with a
// failed call. Only the first is the end of the folder.
fn test_walk_list_handle_tells_the_end_of_a_folder_from_a_listing_that_is_not_offered() {
	assert walk_file_info_address() != unsafe { nil }
	folder := walk_test_tree('walk_list_handle')!
	defer {
		walk_test_tree_remove(folder)
	}
	mkdir(join_path_single(folder, 'empty'))!
	mut handle := walk_test_open(folder)
	mut remaining := []WalkPending{}
	assert walk_list_handle(handle, folder, walk_info_first, mut remaining)
	mut names := walk_test_names(remaining)
	names.sort()
	assert names == ['empty:true', 'file.txt:false', 'link:false', 'sub:true']
	assert remaining.all(it.path.starts_with(folder + path_separator))
	C.CloseHandle(handle)
	// A class that no file system knows fails at the first call, as one that this file system
	// does not offer would: nothing is added, and the caller is told to list another way.
	handle = walk_test_open(folder)
	remaining.clear()
	assert !walk_list_handle(handle, folder, 9999, mut remaining)
	assert remaining.len == 0
	C.CloseHandle(handle)
	// An empty folder was listed, with nothing in it.
	empty := join_path_single(folder, 'empty')
	handle = walk_test_open(empty)
	assert walk_list_handle(handle, empty, walk_info_first, mut remaining)
	assert remaining.len == 0
	C.CloseHandle(handle)
	// A handle that can not be listed at all is no listed folder either.
	assert !walk_list_handle(unsafe { nil }, folder, walk_info_first, mut remaining)
	assert remaining.len == 0
}

// The listing by name is what a walk falls back to, so it must add what the listing through
// the handle adds, in the same order.
fn test_walk_list_by_name_adds_what_the_listing_through_the_handle_adds() {
	folder := walk_test_tree('walk_list_by_name')!
	defer {
		walk_test_tree_remove(folder)
	}
	for i in 0 .. 40 {
		write_file(join_path_single(folder, 'n${i:02}_${'y'.repeat(i)}.txt'), 'x')!
	}
	handle := walk_test_open(folder)
	mut through_handle := []WalkPending{}
	assert walk_list_handle(handle, folder, walk_info_first, mut through_handle)
	C.CloseHandle(handle)
	mut by_name := []WalkPending{}
	assert walk_list_by_name(folder, mut by_name)
	assert by_name.len == 43
	assert walk_test_names(by_name) == walk_test_names(through_handle)
	assert by_name.map(it.path) == through_handle.map(it.path)
	// What can not be listed by name adds nothing, and says so.
	mut none_added := []WalkPending{}
	assert !walk_list_by_name(join_path_single(folder, 'missing'), mut none_added)
	assert !walk_list_by_name(join_path_single(folder, 'file.txt'), mut none_added)
	assert none_added.len == 0
}

// A folder whose listing through the handle is not offered, and one that does not say what it
// is, are still folders: they are listed, and the walk gets every entry in the usual order.
fn test_walk_list_lists_a_folder_that_fails_the_look_or_the_listing_through_its_handle() {
	folder := walk_test_tree('walk_list_with')!
	defer {
		walk_test_tree_remove(folder)
	}
	mut usual := []WalkPending{}
	assert walk_list(WalkPending{ path: folder, listed_as_dir: true }, mut usual) == .entered
	assert usual.len == 3
	// The listing through the handle is not offered: the folder is listed by name.
	mut by_name := []WalkPending{}
	assert walk_list_with(WalkPending{ path: folder, listed_as_dir: true }, mut by_name,
		0, 9999) == .entered
	assert walk_test_names(by_name) == walk_test_names(usual)
	// Nothing says what the open folder is: it is listed all the same.
	mut unknown := []WalkPending{}
	assert walk_list_with(WalkPending{ path: folder, listed_as_dir: true }, mut unknown,
		3, walk_info_first) == .entered
	assert walk_test_names(unknown) == walk_test_names(usual)
	// Both at once: nothing shows that it is a directory, and it is not looked up by name,
	// which would follow a link.
	mut both := []WalkPending{}
	assert walk_list_with(WalkPending{ path: folder, listed_as_dir: true }, mut both,
		3, 9999) == .other
	assert both.len == 0
	// A look that works still decides: a link is not entered whichever way it was asked, nor
	// however it would be listed.
	link := join_path_single(folder, 'link')
	mut none_added := []WalkPending{}
	for first_way in 0 .. 3 {
		assert walk_list_with(WalkPending{ path: link, listed_as_dir: true }, mut none_added,
			first_way, 9999) == .link, 'from way ${first_way}'
	}
	assert none_added.len == 0
	// A link that does not say what it is in any way is listed through its own handle, which
	// was opened without following it: that lists the link and not what it leads to.
	assert walk_list_with(WalkPending{ path: link, listed_as_dir: true }, mut none_added,
		3, walk_info_first) == .entered
	assert none_added.len == 0
	assert walk_list_with(WalkPending{ path: link, listed_as_dir: true }, mut none_added,
		3, 9999) == .other
	assert none_added.len == 0
}

// The listing by name opens a search handle of its own, which a walk that falls back to it
// must close like the handle of the folder.
fn test_a_listing_by_name_closes_its_search_handle() {
	folder := walk_test_tree('walk_by_name_handles')!
	defer {
		walk_test_tree_remove(folder)
	}
	mut added := []WalkPending{}
	pending := WalkPending{
		path:          folder
		listed_as_dir: true
	}
	assert walk_list_with(pending, mut added, 0, 9999) == .entered
	before := open_handles()
	rounds := 200
	for _ in 0 .. rounds {
		assert walk_list_with(pending, mut added, 0, 9999) == .entered
	}
	grown := open_handles() - before
	assert grown < rounds / 2, 'the process holds ${grown} more handles after ${rounds} listings'
}
