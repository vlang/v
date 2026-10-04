import os

const tfolder = os.join_path(os.vtmp_dir(), 'os_walk_dir_tests')

// The tree every test walks. Kept small and fixed so the expected visit order can
// be written out literally.
const tree = [
	'a.txt',
	'b.txt',
	'dir1/c.txt',
	'dir1/deep/d.txt',
	'dir2/e.txt',
]

fn build_tree() {
	for f in tree {
		p := os.join_path_single(tfolder, f.replace('/', os.path_separator))
		os.mkdir_all(os.dir(p)) or { panic(err) }
		os.write_file(p, 'x') or { panic(err) }
	}
	os.mkdir_all(os.join_path_single(tfolder, 'empty')) or { panic(err) }
}

fn testsuite_begin() {
	os.rmdir_all(tfolder) or {}
	assert !os.is_dir(tfolder)
	os.mkdir_all(tfolder)!
	os.chdir(tfolder)!
	build_tree()
	assert os.is_dir(tfolder)
}

fn testsuite_end() {
	os.chdir(os.dir(tfolder)) or {}
	os.rmdir_all(tfolder) or {}
}

// rel_to_tfolder trims the test's own directory off a walked path, so the
// expectations can be written as short literals. Every walk here starts at
// tfolder or somewhere inside it.
fn rel_to_tfolder(path string) string {
	if path.len <= tfolder.len {
		return '.'
	}
	return path[tfolder.len..].trim_left(os.path_separator)
}

// Collector accumulates what walk_dir reported. Its `cb` method is handed to
// walk_dir as a method value, which writes into the Collector it was taken from,
// so the fields can be read once the walk returns.
struct Collector {
mut:
	visits      []string
	dirs        []string
	files       []string
	links       []string
	other       []string
	failed      []string
	err_codes   []int  // err.code() of every report that carried an error
	last        string // the path reported by the previous call
	prune       string // a directory name to veto with .skip_dir
	stop_at     int    // stop once this many entries have been reported
	stop_on_err bool   // stop at the first report that carries an error
	calls       int    // how many times the callback ran, including the one that stops
	deepest     int    // most separators seen in a reported directory path
	sep_byte    u8
}

fn (mut c Collector) cb(path string, entry os.WalkDirEntry) os.WalkDirAction {
	c.calls++
	c.visits << rel_to_tfolder(path)
	// Only a directory that could not be listed is reported twice in a row.
	second_report := path == c.last
	c.last = path
	if entry.is_dir {
		mut level := 0
		for ch in path {
			if ch == c.sep_byte {
				level++
			}
		}
		if level > c.deepest {
			c.deepest = level
		}
	}
	if err := entry.err {
		c.failed << path
		c.err_codes << err.code()
		if second_report {
			// The listing failure is attached to the directory as it was first
			// reported.
			assert entry.is_dir, 'a directory that could not be listed lost is_dir: ${path}'
			assert entry.typ == .directory, 'a directory that could not be listed reported ${entry.typ}: ${path}'
		} else {
			// A path that could not be stat'ed must not look like a directory, or a
			// callback could descend into it.
			assert !entry.is_dir, 'a failed entry reported is_dir for ${path}'
			assert entry.typ == .unknown, 'a failed entry reported ${entry.typ} for ${path}'
		}
		return if c.stop_on_err { .stop } else { .proceed }
	}
	assert !second_report, '${path} was reported twice without an error'
	match entry.typ {
		.directory {
			c.dirs << entry.name
		}
		.regular {
			c.files << entry.name
		}
		.symbolic_link {
			assert !entry.is_dir, 'a symlink reported is_dir: ${path}'
			c.links << entry.name
		}
		else {
			c.other << entry.name
		}
	}
	if c.prune != '' && entry.is_dir && entry.name == c.prune {
		return .skip_dir
	}
	if c.stop_at > 0 && c.calls >= c.stop_at {
		return .stop
	}
	return .proceed
}

fn walk_all(mut c Collector) {
	os.walk_dir(tfolder, c.cb) or { panic(err) }
}

fn walk_root(mut c Collector, root string) {
	os.walk_dir(root, c.cb) or { panic(err) }
}

fn test_walk_dir_visits_everything_in_lexical_order() {
	mut c := Collector{}
	walk_all(mut c)
	sep := os.path_separator
	assert c.visits.len == 10, 'got ${c.visits.len} visits: ${c.visits}'
	assert c.visits[0] == '.', 'the root itself is reported first, got "${c.visits[0]}"'
	assert c.visits[1] == 'a.txt'
	assert c.visits[2] == 'b.txt'
	assert c.visits[3] == 'dir1'
	assert c.visits[4] == 'dir1${sep}c.txt'
	assert c.visits[5] == 'dir1${sep}deep'
	assert c.visits[6] == 'dir1${sep}deep${sep}d.txt'
	assert c.visits[7] == 'dir2'
	assert c.visits[8] == 'dir2${sep}e.txt'
	assert c.visits[9] == 'empty'
	// Nothing failed, and the callback was not asked anything extra.
	assert c.failed.len == 0, 'unexpected read failures: ${c.failed}'
	assert c.calls == 10
}

// os.walk never reported directories at all, which is what made pruning a subtree
// impossible with it.
fn test_walk_dir_reports_directories_and_their_types() {
	mut c := Collector{}
	walk_all(mut c)
	// Five directories including the root itself, and five regular files.
	assert c.dirs.len == 5, 'dirs = ${c.dirs}'
	assert c.dirs.contains('dir1') && c.dirs.contains('dir2') && c.dirs.contains('empty')
	assert c.files.len == 5, 'files = ${c.files}'
	assert c.other.len == 0, 'unexpected other types: ${c.other}'
}

// The whole point of the callback: a vetoed subtree is never read, and its
// siblings still are.
fn test_walk_dir_skip_dir_prunes_only_that_subtree() {
	prune := os.join_path_single(tfolder, 'prune_me')
	os.mkdir_all(os.join_path_single(prune, 'inner')) or { panic(err) }
	os.write_file(os.join_path_single(prune, 'inside.txt'), 'x') or { panic(err) }
	os.write_file(os.join_path_single(os.join_path_single(prune, 'inner'), 'deep.txt'),
		'x') or { panic(err) }
	mut c := Collector{
		prune: 'prune_me'
	}
	walk_all(mut c)
	sep := os.path_separator
	assert c.visits.contains('prune_me'), 'the pruned directory is still reported'
	for v in c.visits {
		assert !v.starts_with('prune_me${sep}'), 'walked into the pruned subtree: ${v}'
	}
	assert c.visits.contains('dir1')
	assert c.visits.contains('dir2${sep}e.txt')
	os.rmdir_all(prune) or { panic(err) }
}

// Vetoing the root itself leaves nothing below it visited.
fn test_walk_dir_skip_dir_on_the_root_stops_below_it() {
	mut c := Collector{
		prune: 'os_walk_dir_tests'
	}
	walk_all(mut c)
	assert c.visits.len == 1, 'visits = ${c.visits}'
	assert c.visits[0] == '.'
}

fn test_walk_dir_stop_ends_the_walk() {
	// A tree of its own, so the expectation does not depend on where a marker
	// happens to sort among the other tests' entries.
	stop_root := os.join_path_single(tfolder, 'stop_root')
	os.mkdir_all(stop_root) or { panic(err) }
	os.write_file(os.join_path_single(stop_root, 'one.txt'), 'x') or { panic(err) }
	os.mkdir_all(os.join_path_single(stop_root, 'two')) or { panic(err) }
	os.write_file(os.join_path_single(stop_root, 'three.txt'), 'x') or { panic(err) }

	mut c := Collector{
		stop_at: 2
	}
	walk_root(mut c, stop_root)
	// The second callback invocation is the one that asks to stop, so two entries
	// were reported and nothing under the stopped point was read. This matches Go,
	// where the callback that returns SkipAll has already been given its entry.
	assert c.calls == 2, 'callback ran ${c.calls} times, visits = ${c.visits}'
	assert c.visits.len == 2, 'visits = ${c.visits}'
	// visits are recorded relative to tfolder, which is the walk root's parent here
	assert c.visits[0] == 'stop_root'
	assert c.visits[1] == 'stop_root${os.path_separator}one.txt', "lexical order reaches one.txt before two, got '${c.visits[1]}'"
	assert !c.visits.contains('stop_root${os.path_separator}two')
	assert !c.visits.contains('stop_root${os.path_separator}three.txt')

	os.rmdir_all(stop_root) or { panic(err) }
}

// Symlinks are reported, never followed: a link to a directory and a link back to
// an ancestor are each reported once, as links, and nothing beneath them is
// visited.
fn test_walk_dir_does_not_follow_symlinks() {
	$if !windows {
		links_root := os.join_path_single(tfolder, 'links_root')
		real := os.join_path_single(links_root, 'real')
		os.mkdir_all(real) or { panic(err) }
		defer {
			os.rmdir_all(links_root) or {}
		}
		os.write_file(os.join_path_single(real, 'f.txt'), 'x') or { panic(err) }
		os.symlink(real, os.join_path_single(links_root, 'to_real')) or { panic(err) }
		os.symlink(tfolder, os.join_path_single(links_root, 'loop')) or { panic(err) }

		mut c := Collector{}
		walk_root(mut c, links_root)
		sep := os.path_separator
		assert c.visits == ['links_root', 'links_root${sep}loop', 'links_root${sep}real',
			'links_root${sep}real${sep}f.txt', 'links_root${sep}to_real'], 'visits = ${c.visits}'
		assert c.links == ['loop', 'to_real']
		assert c.dirs == ['links_root', 'real']
		assert c.failed.len == 0, 'unexpected read failures: ${c.failed}'
	}
}

// A directory that cannot be listed is reported a second time, with the error,
// and .stop from that report ends the walk.
fn test_walk_dir_reports_a_directory_it_cannot_list() {
	$if !windows {
		if os.geteuid() == 0 {
			// root lists a directory whatever its permissions say
			return
		}
		locked_root := os.join_path_single(tfolder, 'locked_root')
		locked := os.join_path_single(locked_root, 'aa')
		os.mkdir_all(os.join_path_single(locked, 'inside')) or { panic(err) }
		os.mkdir_all(os.join_path_single(locked_root, 'zz')) or { panic(err) }
		os.chmod(locked, 0o000) or { panic(err) }
		defer {
			os.chmod(locked, 0o755) or {}
			os.rmdir_all(locked_root) or {}
		}
		sep := os.path_separator

		mut c := Collector{}
		walk_root(mut c, locked_root)
		assert c.visits == ['locked_root', 'locked_root${sep}aa', 'locked_root${sep}aa',
			'locked_root${sep}zz'], 'visits = ${c.visits}'
		assert c.failed == [locked]
		assert c.err_codes == [int(C.EACCES)]

		mut s := Collector{
			stop_on_err: true
		}
		walk_root(mut s, locked_root)
		assert s.visits == ['locked_root', 'locked_root${sep}aa', 'locked_root${sep}aa'], '.stop on the error report did not end the walk, visits = ${s.visits}'
	}
}

// A root that does not exist is reported to the callback, like any other entry
// that cannot be read, rather than failing the call.
fn test_walk_dir_reports_a_missing_root() {
	missing := os.join_path_single(tfolder, 'missing')
	mut c := Collector{}
	os.walk_dir(missing, c.cb) or {
		assert false, 'walk_dir failed for a missing root: ${err}'
		return
	}
	assert c.visits == ['missing'], 'visits = ${c.visits}'
	assert c.failed == [missing]
	assert c.err_codes.len == 1 && c.err_codes[0] != 0, 'err_codes = ${c.err_codes}'
}

fn test_walk_dir_rejects_an_empty_root() {
	mut c := Collector{}
	os.walk_dir('', c.cb) or {
		assert c.calls == 0, 'the callback ran for an empty root'
		return
	}
	assert false, 'walk_dir accepted an empty root'
}

// walk_dir iterates rather than recursing, so depth costs no stack. A recursive
// implementation would overflow well before this many levels.
fn test_walk_dir_handles_a_deep_tree() {
	// Windows caps a path at MAX_PATH, so the achievable depth there is a
	// property of the filesystem rather than of walk_dir. POSIX has no such cap,
	// which is where the deep case is actually meaningful.
	mut depth := 0
	$if windows {
		depth = 25
	} $else {
		depth = 400
	}
	mut deep := tfolder
	for _ in 0 .. depth {
		deep = os.join_path_single(deep, 'd')
	}
	os.mkdir_all(deep) or { panic(err) }
	mut c := Collector{
		sep_byte: os.path_separator[0]
	}
	walk_all(mut c)
	// The count includes the temporary directory's own separators, so only the
	// descent into the 'd' chain is asserted here.
	assert c.deepest >= depth, 'walk reached ${c.deepest} separators, expected at least ${depth}'
	assert c.failed.len == 0, 'unexpected read failures deep in the tree: ${c.failed}'
	mut cur := deep
	for _ in 0 .. depth {
		cur = os.dir(cur)
	}
	os.rmdir_all(cur) or {}
}
