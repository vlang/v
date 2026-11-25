module os

fn C.fdopendir(fd i32) voidptr

// What the listing of a directory says an entry is (`d_type`): a directory, or nothing, which
// is what some file systems say for every entry. Every other value is something that a walk
// reports and does not enter: a file, a link, a FIFO, a socket or a device.
const walk_type_unknown = u8(0)
const walk_type_dir = u8(4)

// WalkListed is a path that walk or walk_with_context still has to visit.
struct WalkListed {
	path string
	// typ is what the listing of its directory said the entry is. It is walk_type_unknown for
	// the root of a walk, and for an entry of a file system that does not say.
	// It can be out of date by the time the path is visited, see walk_enter.
	typ u8
}

// walk_is_dir_by_name returns whether `path` names a directory that is no link, without
// following a link.
fn walk_is_dir_by_name(path string) bool {
	kind := kind_of_existing_path(path)
	return kind.is_dir && !kind.is_link
}

// walk_enter appends the entries of the directory `item` to `remaining`, in reverse listing
// order, so that popping them visits them in the order that `ls` reports. It returns whether
// `item` is a directory that is no link: true also for one that may not be listed, for which
// nothing is added, and false for everything that a walk reports as an entry.
// The listing says what kind each entry is, so only an entry of unknown kind is looked up by
// name, which is where a walk used to spend most of its time. An entry that was listed as
// anything else than a directory is not looked at again.
// A directory is opened without following a link, and listed through what was opened. So what
// is listed is what was opened, also when the callback or another process replaces the
// directory by a link before the walk reaches it: the open fails then, and the path is looked
// up by name to tell what it has become.
fn walk_enter(item WalkListed, mut remaining []WalkListed) bool {
	if item.typ == walk_type_unknown {
		if !walk_is_dir_by_name(item.path) {
			return false
		}
	} else if item.typ != walk_type_dir {
		return false
	}
	fd := C.open(&char(item.path.str), C.O_RDONLY | C.O_DIRECTORY | C.O_NOFOLLOW | C.O_CLOEXEC)
	if fd < 0 {
		// It is no directory any more, it is gone, or it may not be opened. Nothing is listed,
		// so nothing depends on this look by name: it only tells a directory that is passed
		// over from what is reported as an entry.
		return walk_is_dir_by_name(item.path)
	}
	dir := C.fdopendir(fd)
	if isnil(dir) {
		C.close(fd)
		return true
	}
	first := remaining.len
	for {
		ent := C.readdir(dir)
		if isnil(ent) {
			break
		}
		unsafe {
			name := &u8(&ent.d_name[0])
			if name[0] == 0 || (name[0] == `.` && name[1] == 0)
				|| (name[0] == `.` && name[1] == `.` && name[2] == 0) {
				continue
			}
			// The name is read where the listing keeps it, which is until the next entry is
			// read. Joining it to the path copies it.
			remaining << WalkListed{
				path: item.path + path_separator + tos(name, vstrlen(name))
				typ:  ent.d_type
			}
		}
	}
	// Closing the listing closes `fd` as well.
	C.closedir(dir)
	mut lo := first
	mut hi := remaining.len - 1
	for lo < hi {
		remaining[lo], remaining[hi] = remaining[hi], remaining[lo]
		lo++
		hi--
	}
	return true
}

// walk_linux is the implementation of `walk` on Linux.
fn walk_linux(path string, f fn (string)) {
	if path == '' || !is_dir(path) {
		return
	}
	mut remaining := []WalkListed{cap: 1000}
	remaining << WalkListed{
		path: path.trim_right(path_separator)
	}
	for remaining.len > 0 {
		cur := remaining.pop()
		if !walk_enter(cur, mut remaining) {
			f(cur.path)
		}
	}
}

// walk_with_context_linux is the implementation of `walk_with_context` on Linux.
fn walk_with_context_linux(path string, context voidptr, fcb FnWalkContextCB) {
	// A root that is no directory, or a link to one, adds nothing, and the root is not reported.
	mut remaining := []WalkListed{cap: 1000}
	root := WalkListed{
		path: path.trim_right(path_separator)
	}
	walk_enter(root, mut remaining)
	for remaining.len > 0 {
		cur := remaining.pop()
		// The callback comes first, so that the walk lists what the callback left.
		fcb(context, cur.path)
		walk_enter(cur, mut remaining)
	}
}
