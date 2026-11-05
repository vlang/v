module main

import os
import os.filelock
import time

struct WipeCacheOptions {
	dry_run           bool
	verbose           bool
	vcache            bool
	vtmp              bool
	v3cache           bool
	tools             bool
	explicit_selector bool
	show_help         bool
}

const wipe_cache_help_text = 'Remove V cache folders (VCACHE, VTMP, V3CACHE, and compiled tool caches).
Stop and return a nonzero status if a cache folder cannot be removed or recreated.
Leave the entries of tools that another process has locked right now (a build in progress)
in place and name them in the output. Lock files that cannot be opened are repaired, so they can be locked again.
Entries of a tool whose lock cannot be created, for example because the filesystem has no hard
links or a directory is in the way, are kept.

Usage:
  v wipe-cache [options]

Options:
  -n, --dry-run   Show what would be wiped without deleting any files.
  -v, --verbose   Show detailed output, skipped paths, and tool cache diagnostics.
      --tools     Wipe only compiled tool caches (VTOOLS_CACHE_DIR, ~/.cache/v/tools,
                  <VTMP>/tools, ~/.vtools, ~/.v/tools, and moved-aside .untrusted-* folders).
      --vcache    Wipe only the V module/object cache (VCACHE or ~/.vmodules/.cache).
      --vtmp      Wipe only the V temporary folder (VTMP), keeping tool entries a build holds.
      --v3cache   Wipe only the V3 module and object caches (V3CACHE).
  -h, --help      Display this help message.'

fn main() {
	passed := os.args[1..]
	command_args := if passed.len > 0 && passed[0] == 'wipe-cache' {
		passed[1..]
	} else {
		passed
	}
	opts := parse_wipe_cache_options(command_args) or {
		eprintln(err.msg())
		eprintln(wipe_cache_help_text)
		exit(1)
	}
	if opts.show_help {
		println(wipe_cache_help_text)
		return
	}
	lines := wipe_caches(opts) or {
		eprintln(err.msg())
		exit(1)
	}
	for line in lines {
		println(line)
	}
}

fn parse_wipe_cache_options(command_args []string) !WipeCacheOptions {
	mut dry_run := false
	mut verbose := false
	mut vcache := false
	mut vtmp := false
	mut v3cache := false
	mut tools := false
	mut explicit_selector := false
	mut show_help := false
	for arg in command_args {
		match arg {
			'-n', '-dry-run', '--dry-run' {
				dry_run = true
			}
			'-v', '-verbose', '--verbose' {
				verbose = true
			}
			'-tools', '--tools' {
				tools = true
				explicit_selector = true
			}
			'-vcache', '--vcache' {
				vcache = true
				explicit_selector = true
			}
			'-vtmp', '--vtmp' {
				vtmp = true
				explicit_selector = true
			}
			'-v3cache', '--v3cache' {
				v3cache = true
				explicit_selector = true
			}
			'-h', '-help', '--help' {
				show_help = true
			}
			else {
				return error('v wipe-cache: unknown option `${arg}`')
			}
		}
	}
	if !explicit_selector {
		vcache = true
		vtmp = true
		v3cache = true
		tools = true
	}
	return WipeCacheOptions{
		dry_run:           dry_run
		verbose:           verbose
		vcache:            vcache
		vtmp:              vtmp
		v3cache:           v3cache
		tools:             tools
		explicit_selector: explicit_selector
		show_help:         show_help
	}
}

fn tool_cache_default_candidates() []string {
	mut candidates := [
		os.join_path(os.cache_dir(), 'v', 'tools'),
		os.join_path(os.vtmp_dir(), 'tools'),
	]
	home := os.home_dir().trim_space()
	if home != '' {
		candidates << os.join_path(home, '.vtools')
		candidates << os.join_path(home, '.v', 'tools')
	}
	return candidates
}

fn append_unique_wipe_target(path string, mut targets []string, mut seen map[string]bool) {
	trimmed := path.trim_space()
	if trimmed == '' {
		return
	}
	key := os.abs_path(trimmed)
	if seen[key] {
		return
	}
	seen[key] = true
	targets << trimmed
}

fn append_untrusted_tool_cache_siblings(candidate string, mut targets []string, mut seen map[string]bool) {
	parent := os.dir(candidate)
	if !os.is_dir(parent) {
		return
	}
	prefix := '${os.file_name(candidate)}.untrusted-'
	mut entries := os.ls(parent) or { return }
	entries.sort()
	for entry in entries {
		if entry.starts_with(prefix) {
			append_unique_wipe_target(os.join_path(parent, entry), mut targets, mut seen)
		}
	}
}

fn tool_cache_wipe_targets() []string {
	mut targets := []string{}
	mut seen := map[string]bool{}
	if configured := os.getenv_opt('VTOOLS_CACHE_DIR') {
		trimmed := configured.trim_space()
		if trimmed != '' {
			append_unique_wipe_target(trimmed, mut targets, mut seen)
			append_untrusted_tool_cache_siblings(trimmed, mut targets, mut seen)
		}
	}
	for candidate in tool_cache_default_candidates() {
		append_unique_wipe_target(candidate, mut targets, mut seen)
		append_untrusted_tool_cache_siblings(candidate, mut targets, mut seen)
	}
	return targets
}

fn is_v3_cache_subdir(name string) bool {
	if name in ['v3_thirdparty_objs', 'v3_fastc_unit_cache', 'v3_fastc_link_cache', 'v3_crun_cache'] {
		return true
	}
	for prefix in ['v3_module_cache_', 'v3_test_cache_'] {
		if name.starts_with(prefix) {
			suffix := name[prefix.len..]
			if suffix.len > 0 && suffix.bytes().all(it.is_hex_digit()) {
				return true
			}
		}
	}
	return false
}

// is_cache_artifact_of reports whether `name` is the entry directory of a cache entry of
// `tool`. The tool name alone is not enough to tell them apart, because one tool's name can
// be a prefix of another's (`v bug` and `v bug-report`), so the content address itself has
// to be matched. Keep in sync with cmd/v/toolcache.v.
fn is_cache_artifact_of(name string, tool string) bool {
	if !name.starts_with('${tool}-') {
		return false
	}
	rest := name[tool.len + 1..]
	if rest.len < 64 {
		return false
	}
	for character in rest[..64] {
		if !character.is_hex_digit() {
			return false
		}
	}
	suffix := rest[64..]
	return suffix == '' || suffix.starts_with('.')
}

// tool_cache_lock_file_suffix ends the name of this user's persistent build lock for one
// tool inside a tool cache root; the tool's name sits between the leading dot and it.
// Keep in sync with `tool_cache_lock_path_for_uid` and cmd/tools/vwipe-cache.v.
fn tool_cache_lock_file_suffix() string {
	return '.${os.getuid()}.toolcache.lock'
}

// tool_cache_entry_tool returns the tool a cache entry directory `<tool>-<64 hex>[.<n>]` belongs
// to, or none when `name` is not shaped like one. A tool name may itself contain dashes, so every
// dash is tried as the separator.
fn tool_cache_entry_tool(name string) ?string {
	for i, c in name {
		if c == `-` && i > 0 && is_cache_artifact_of(name, name[..i]) {
			return name[..i]
		}
	}
	return none
}

// ToolCacheWipeLocks tracks, for one tool cache root, which build locks the wipe holds, which
// tools another process has locked, and which lock files cannot be opened at all. The lock files
// are persistent mutexes (see `tool_cache_lock`): usable ones are opened, created only by `claim`,
// and never removed, so a builder blocked on one while the wipe holds it simply proceeds once the wipe
// releases it.
struct ToolCacheWipeLocks {
	root string
mut:
	busy       []string
	held       []string
	stale      []string
	unlockable []string
	locks      []filelock.FileLock
}

// probe takes the build lock of `tool` in this root unless it was already probed. No lock file
// means no builder can be at work: every build creates the lock file before it touches the entry
// directory. A lock file that cannot even be opened for writing (read-only attribute, mode 0400)
// is stale rather than held: no builder can lock it either, so nothing is being built with it.
fn (mut locks ToolCacheWipeLocks) probe(tool string) {
	if tool in locks.busy || tool in locks.held || tool in locks.stale {
		return
	}
	lock_path := os.join_path(locks.root, '.${tool}${tool_cache_lock_file_suffix()}')
	if os.is_link(lock_path) || !os.is_file(lock_path) {
		return
	}
	mut build_lock := filelock.new_file(lock_path, mode: .exclusive)
	if build_lock.try_acquire() {
		locks.held << tool
		locks.locks << build_lock
		return
	}
	// `try_acquire` fails both when another process holds the lock and when the file cannot be
	// opened; only the first one means a build is in flight.
	mut probe_file := os.open_file(lock_path, 'r+') or {
		locks.stale << tool
		return
	}
	probe_file.close()
	locks.busy << tool
}

// claim takes the build lock of `tool` before its entries are removed. When the tool has no lock
// file yet, one is created first, so that a builder either finds the lock taken or waits on it. A
// tool whose lock cannot be taken, because it could not be created or its path is a link, is
// recorded as unlockable and its entries are kept rather than removed without a lock. A dry run
// creates nothing. It returns the path of a lock file it created, or nothing.
fn (mut locks ToolCacheWipeLocks) claim(tool string, dry_run bool) string {
	lock_path := os.join_path(locks.root, '.${tool}${tool_cache_lock_file_suffix()}')
	mut created := ''
	if !dry_run && !os.exists(lock_path) && !os.is_link(lock_path) {
		created = locks.create_lock(tool, lock_path)
	}
	locks.probe(tool)
	if !dry_run && tool !in locks.held && tool !in locks.busy && tool !in locks.unlockable {
		locks.unlockable << tool
	}
	return created
}

// create_lock makes the missing lock file of `tool` by hard linking a temporary file to `lock_path`.
// A link fails when that path exists and never follows a link found there, so a link planted at the
// lock path cannot redirect the create. It returns the new path, or nothing when the file could not
// be made, for instance on a filesystem without hard links.
fn (mut locks ToolCacheWipeLocks) create_lock(tool string, lock_path string) string {
	staged := os.join_path(locks.root, '.${tool}.${os.getpid()}-${time.now().unix_nano()}.tmp')
	mut staged_file := os.open_file(staged, 'a', 0o600) or { return '' }
	staged_file.close()
	os.link(staged, lock_path) or {
		os.rm(staged) or {}
		return ''
	}
	os.rm(staged) or {}
	return lock_path
}

// release gives every acquired build lock back, after the wipe is done.
fn (mut locks ToolCacheWipeLocks) release() {
	for mut build_lock in locks.locks {
		build_lock.release()
	}
	locks.locks = []
	locks.held = []
}

// repair_stale_lock sets a stale lock file to mode 0600, which makes it lockable again. It returns
// an error that names the file and the cause when that fails.
fn repair_stale_lock(path string) ! {
	os.chmod(path, 0o600) or {
		return error('cannot repair the lock file ${path}: ${err}')
	}
}

// wipe_except_text names the tools whose entries a wipe kept, and why, for its summary line.
fn wipe_except_text(busy []string, unlockable []string) string {
	mut text := ''
	if busy.len > 0 {
		text += ', except for the tools another process has locked right now: ${busy.join(', ')}'
	}
	if unlockable.len > 0 {
		text += ', except for the tools whose lock could not be created: ${unlockable.join(', ')}'
	}
	return text
}

// wipe_tool_cache_directory wipes one tool cache root like `wipe_cache_directory`, except that
// the entries of tools another process has locked at that moment are left in place, and the
// output names those tools. Such a lock means a build is in flight: on Windows the compiler still
// has its staging directory open and removal fails with a sharing violation; on POSIX removal
// would succeed and pull the build's files out from under it. The locks the wipe can take are
// held until it is done, so a build starting meanwhile waits instead of racing it. Usable lock
// files stay. A lock file that cannot be opened is repaired to mode 0600 instead of removed, so a
// builder can lock it again: lock files are never unlinked. An entry of a tool with no lock file is
// removed only while its lock is held: a missing lock file is created first, so a builder either
// finds the lock taken or waits on it, and a tool whose lock cannot be created is kept. Every entry is attempted and the first failure is reported
// afterwards, as `os.rmdir_all` does. The root itself is removed when
// nothing in it was kept. If that removal fails, the failure is an error unless a build that
// started during the wipe put new entries in the root; those are its own, and are reported.
fn wipe_tool_cache_directory(path string, label string, opts WipeCacheOptions, mut lines []string) ! {
	if os.is_link(path) || !os.is_dir(path) {
		return wipe_cache_directory(path, label, false, opts, mut lines)
	}
	mut locks := ToolCacheWipeLocks{
		root: path
	}
	defer {
		locks.release()
	}
	mut names := os.ls(path) or {
		return error('v wipe-cache: could not list ${label} folder ${path}: ${err}')
	}
	names.sort()
	suffix := tool_cache_lock_file_suffix()
	mut details := []string{}
	mut kept := []string{}
	mut first_error := ''
	for name in names {
		full := os.join_path(path, name)
		// Another wipe may have removed this entry since the listing.
		if !os.exists(full) && !os.is_link(full) {
			continue
		}
		if name.starts_with('.') && name.ends_with(suffix) && name.len > suffix.len + 1
			&& os.is_file(full) && !os.is_link(full) {
			tool := name[1..name.len - suffix.len]
			locks.probe(tool)
			if tool in locks.stale && !opts.dry_run {
				// Repaired rather than removed: lock files are never unlinked, so no builder can be left
				// holding an inode that the wipe took away.
				repair_stale_lock(full) or {
					if first_error == '' {
						first_error = err.msg()
					}
				}
				locks.stale = locks.stale.filter(it != tool)
				locks.probe(tool)
			}
			kept << full
			continue
		} else if entry_tool := tool_cache_entry_tool(name) {
			created := locks.claim(entry_tool, opts.dry_run)
			if created != '' {
				kept << created
			}
			if entry_tool in locks.busy || entry_tool in locks.unlockable {
				kept << full
				continue
			}
		}
		if opts.dry_run {
			details << '  would remove ${full}'
			continue
		}
		details << '  removing ${full}'
		if os.is_dir(full) && !os.is_link(full) {
			os.rmdir_all(full) or {
				if first_error == '' && (os.exists(full) || os.is_link(full)) {
					first_error = '${err}'
				}
			}
		} else {
			os.rm(full) or {
				if first_error == '' && (os.exists(full) || os.is_link(full)) {
					first_error = '${err}'
				}
			}
		}
	}
	for path_kept in kept {
		details << if opts.dry_run { '  would keep ${path_kept}' } else { '  keeping ${path_kept}' }
	}
	mut busy := locks.busy.clone()
	busy.sort()
	mut unlockable := locks.unlockable.clone()
	unlockable.sort()
	except := wipe_except_text(busy, unlockable)
	if opts.dry_run {
		// Same order as `wipe_cache_directory`: the summary first, then what it consists of.
		lines << 'Would wipe ${label} folder ${path}${except}.'
		if opts.verbose {
			lines << details
		}
		return
	}
	if opts.verbose {
		lines << details
	}
	if first_error != '' {
		return error('v wipe-cache: could not remove ${label} folder ${path}: ${first_error}')
	}
	mut build_started := ''
	if kept.len == 0 {
		os.rmdir(path) or {
			// Only a build that started during the wipe can have put new entries in the root;
			// anything else that stops the removal is a failure of the wipe.
			mut fresh := os.ls(path) or { []string{} }
			fresh = fresh.filter(it !in names)
			if fresh.len == 0 {
				return error('v wipe-cache: could not remove ${label} folder ${path}: ${err}')
			}
			build_started = '${label} folder ${path} was emptied, but a build started during the wipe kept ${fresh.len} entries in it.'
		}
	}
	if build_started != '' {
		lines << build_started
	} else {
		lines << '${label} folder ${path} was wiped${except}.'
	}
}

fn wipe_cache_directory(path string, label string, recreate bool, opts WipeCacheOptions, mut lines []string) ! {
	if opts.dry_run {
		if os.is_link(path) {
			lines << 'Would wipe ${label} folder ${path} (symbolic link).'
		} else if os.is_dir(path) {
			lines << 'Would wipe ${label} folder ${path}.'
			if opts.verbose {
				mut entries := os.ls(path) or {
					return error('v wipe-cache: could not list ${label} folder ${path}: ${err}')
				}
				entries.sort()
				for entry in entries {
					lines << '  would remove ${os.join_path(path, entry)}'
				}
			}
		} else if os.exists(path) {
			lines << 'Would wipe ${label} folder ${path}.'
		} else {
			lines << 'Would wipe ${label} folder ${path} (does not exist).'
		}
		return
	}
	if os.is_link(path) {
		if opts.verbose {
			lines << '  removing symbolic link ${path}'
		}
		os.rm(path) or {
			return error('v wipe-cache: could not remove ${label} folder ${path}: ${err}')
		}
	} else if os.is_dir(path) {
		if opts.verbose {
			mut entries := os.ls(path) or {
				return error('v wipe-cache: could not list ${label} folder ${path}: ${err}')
			}
			entries.sort()
			for entry in entries {
				lines << '  removing ${os.join_path(path, entry)}'
			}
		}
		os.rmdir_all(path) or {
			return error('v wipe-cache: could not remove ${label} folder ${path}: ${err}')
		}
	} else if os.exists(path) {
		os.rm(path) or {
			return error('v wipe-cache: could not remove ${label} folder ${path}: ${err}')
		}
	} else if opts.verbose {
		lines << '  ${path} does not exist'
	}
	if recreate {
		os.mkdir_all(path) or {
			return error('v wipe-cache: could not create ${label} folder ${path}: ${err}')
		}
	}
	lines << '${label} folder ${path} was wiped.'
}

// wipe_vtmp_directory wipes the V tmp folder. Its tool cache folder is wiped lock-aware, so
// entries and lock files that a running build holds survive, and the rest of the folder is
// removed around them.
fn wipe_vtmp_directory(opts WipeCacheOptions, mut lines []string) ! {
	vtmp := os.vtmp_dir()
	tools_dir := os.join_path(vtmp, 'tools')
	if !os.is_link(vtmp) && os.is_dir(vtmp) && (os.exists(tools_dir) || os.is_link(tools_dir)) {
		if !opts.tools {
			wipe_tool_cache_directory(tools_dir, 'V tool cache', opts, mut lines)!
		}
		if os.exists(tools_dir) || os.is_link(tools_dir) {
			if opts.dry_run {
				lines << 'Would wipe V tmp.c and tests folder ${vtmp}, except ${tools_dir}.'
				return
			}
			entries := os.ls(vtmp) or {
				return error('v wipe-cache: could not list V tmp.c and tests folder ${vtmp}: ${err}')
			}
			for entry in entries {
				path := os.join_path(vtmp, entry)
				if path == tools_dir {
					continue
				}
				if os.is_dir(path) && !os.is_link(path) {
					os.rmdir_all(path) or {
						return error('v wipe-cache: could not remove ${path}: ${err}')
					}
				} else {
					os.rm(path) or { return error('v wipe-cache: could not remove ${path}: ${err}') }
				}
			}
			lines << 'V tmp.c and tests folder ${vtmp} was wiped, except ${tools_dir}, which keeps entries in use.'
			return
		}
	}
	wipe_cache_directory(vtmp, 'V tmp.c and tests', true, opts, mut lines)!
}

fn wipe_caches(opts WipeCacheOptions) ![]string {
	mut lines := []string{}
	if opts.vcache {
		cache_path := os.getenv_opt('VCACHE') or { os.join_path(os.vmodules_dir(), '.cache') }
		wipe_cache_directory(cache_path, 'V cache', true, opts, mut lines)!
	}
	mut tool_lines := []string{}
	if opts.tools {
		mut wiped_any_tool_cache := false
		for target in tool_cache_wipe_targets() {
			label := if os.file_name(target).contains('.untrusted-') {
				'Moved-aside V tool cache'
			} else {
				'V tool cache'
			}
			if os.exists(target) || os.is_link(target) {
				wiped_any_tool_cache = true
				wipe_tool_cache_directory(target, label, opts, mut tool_lines)!
			} else if opts.verbose {
				tool_lines << '${label} folder ${target} does not exist, skipping.'
			}
		}
		if opts.explicit_selector && !opts.vcache && !opts.vtmp && !opts.v3cache
			&& !wiped_any_tool_cache && !opts.verbose {
			if opts.dry_run {
				tool_lines << 'No existing V tool cache folders to wipe.'
			} else {
				tool_lines << 'V tool cache folders were already clean.'
			}
		}
	}
	if opts.vtmp {
		wipe_vtmp_directory(opts, mut lines)!
	}
	if opts.v3cache {
		vtmp_path := os.vtmp_dir()
		configured_v3 := os.getenv('V3CACHE').trim_space()
		if configured_v3 != '' && os.abs_path(configured_v3) != os.abs_path(vtmp_path) {
			wipe_cache_directory(configured_v3, 'V3 cache', true, opts, mut lines)!
		} else if !opts.vtmp {
			mut v3_dirs := []string{}
			if os.is_dir(vtmp_path) {
				mut entries := os.ls(vtmp_path) or {
					return error('v wipe-cache: could not list V3 cache folders in ${vtmp_path}: ${err}')
				}
				entries.sort()
				for entry in entries {
					if is_v3_cache_subdir(entry) {
						candidate := os.join_path(vtmp_path, entry)
						if os.is_dir(candidate) || os.is_link(candidate) {
							v3_dirs << candidate
						}
					}
				}
			}
			if v3_dirs.len > 0 {
				for dir in v3_dirs {
					wipe_cache_directory(dir, 'V3 cache', false, opts, mut lines)!
				}
			} else if opts.dry_run {
				lines << 'Would wipe V3 cache folders in ${vtmp_path} (none exist).'
			} else {
				lines << 'V3 cache folders in ${vtmp_path} were already clean.'
			}
		}
	}
	lines << tool_lines
	return lines
}
