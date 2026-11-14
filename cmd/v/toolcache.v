// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import crypto.sha256
import os
import os.filelock
import time
import v.util
import v.vmod

// The external `cmd/tools/*` programs are compiled once and then cached, so that
// repeated invocations of `v fmt`, `v vet`, `v doctor`, ... do not pay for a full
// recompilation of the tool every single time. This matters a lot for `v test-cleancode`,
// which runs `v fmt -verify` and `v vet` once per repository file: without a cache,
// each of those ~10000 invocations would rebuild the tool from scratch.
//
// A cache entry is content addressed by `tool_cache_key/4`, which covers the V executable,
// the tool's own sources and the compiler flags the tool is built with. That alone is not
// enough, because a tool also depends on every vlib module it imports. Those inputs are
// recorded in a manifest, built from the `-dump-files` source closure that the compiler
// emits while producing the binary, and revalidated on every lookup.
//
// The cache lives under the user's V cache directory, never inside the source tree.

const tool_cache_manifest_version = 'v3-tool-cache-4'
const tool_cache_disable_env = 'VTOOLS_NO_CACHE'
const tool_cache_dir_env = 'VTOOLS_CACHE_DIR'
const tool_cache_verbose_env = 'VTOOLS_CACHE_VERBOSE'
const tool_cache_field_separator = '\x1f'
// marks a cached binary that had to be renamed out of the way instead of being overwritten,
// because Windows was still executing it; `prune_stale_tool_binaries_locked` collects these later
const tool_cache_replaced_marker = '.replaced.'
const tool_cache_stage_prefix = '.v-toolcache-stage-'

// ToolCacheEntry describes where a single compiled `cmd/tools/` program is cached.
struct ToolCacheEntry {
	name string // the tool source name, i.e. `vfmt`
	// `<vroot>/cmd/tools/vfmt.v`, or a directory holding the tool's `.v` files
	source string
	vroot  string // the V source tree the tool is built from
	// the content addressed directory holding this entry. The hash lives here rather than
	// in the executable's name so that the executable can keep the tool's own name: tools
	// read `os.file_name(os.executable())` and use it as their displayed name and as their
	// own cache directory, and a hashed name would rename the tool and make every rebuild
	// download its assets again.
	dir    string
	binary string // the cached executable, named exactly like the tool
	// the build inputs recorded for `binary`
	manifest string
	// set when the current compiler is known to not be able to build the tool
	unbuildable string
	// the build inputs recorded for `unbuildable`
	unbuildable_manifest string
	// the compiler flags that `binary` was produced with
	build_args []string
}

// tool_exe_suffix returns the platform specific executable suffix.
fn tool_exe_suffix() string {
	$if windows {
		return '.exe'
	} $else {
		return ''
	}
}

// staged_tool_binary_path returns the output path inside a private staging directory while
// keeping the cached tool's exact executable name and platform suffix.
fn staged_tool_binary_path(stage_dir string, binary string) string {
	return os.join_path(stage_dir, os.file_name(binary))
}

// tool_cache_is_disabled reports whether the user asked for the tools to always be rebuilt.
fn tool_cache_is_disabled() bool {
	return os.getenv(tool_cache_disable_env).trim_space().to_lower() in ['1', 'true', 'yes', 'on']
}

// tool_cache_is_verbose reports whether every cache decision should be explained on stderr.
fn tool_cache_is_verbose() bool {
	return os.getenv(tool_cache_verbose_env).trim_space().to_lower() in ['1', 'true', 'yes', 'on']
}

// tool_cache_default_candidates returns the default tool cache directories tried in order when
// `VTOOLS_CACHE_DIR` is unset or unusable. Besides the XDG/platform cache and `VTMP`, home
// directories are tried last so that systems where `~/.cache` is
// group-writable or `/tmp` is owned by another account still have a private default candidate.
fn tool_cache_default_candidates() []string {
	mut candidates := [
		os.join_path(os.cache_dir(), 'v', 'tools'),
		os.join_path(os.vtmp_dir(), 'tools'),
	]
	private_home := tool_cache_home_candidate()
	if private_home == '' {
		return candidates
	}
	if private_home !in candidates {
		candidates << private_home
	}
	fallback := os.join_path(os.home_dir().trim_space(), '.v', 'tools')
	if fallback !in candidates {
		candidates << fallback
	}
	return candidates
}

// ToolCacheRejection records why one candidate folder was not used for the tool cache.
struct ToolCacheRejection {
	candidate string
	problems  []string
}

// tool_cache_dir returns a writable directory for the compiled tools, creating it on demand.
// It deliberately lives outside of the V source tree, next to the other V caches. When no
// candidate can be used, the error names each of them, and why it was rejected.
fn tool_cache_dir() !string {
	mut candidates := []string{}
	if custom := os.getenv_opt(tool_cache_dir_env) {
		if custom.trim_space() != '' {
			candidates << custom.trim_space()
		}
	}
	// Explicit paths keep the custom-root policy, even when they name a default.
	mut default_candidates := []string{}
	for candidate in tool_cache_default_candidates() {
		if !candidates.any(os.abs_path(it) == os.abs_path(candidate)) {
			default_candidates << candidate
		}
	}
	candidates << default_candidates
	mut rejected := []ToolCacheRejection{}
	for candidate in candidates {
		problems := tool_cache_candidate_problems(candidate, candidate in default_candidates)
		if problems.len > 0 {
			rejected << ToolCacheRejection{
				candidate: candidate
				problems:  problems
			}
			continue
		}
		if tool_cache_is_verbose() {
			for rejection in rejected {
				eprintln('> not using the tool cache folder `${rejection.candidate}`: ${rejection.problems.join('; ')}')
			}
		}
		return os.real_path(candidate)
	}
	return error(tool_cache_unavailable_message(rejected))
}

// tool_cache_candidate_problems prepares one candidate folder for the tool cache, creating it
// when needed, and returns why it cannot be used, or nothing when it can.
fn tool_cache_candidate_problems(candidate string, is_default bool) []string {
	if is_default {
		problems := prepare_default_tool_cache_root(candidate)
		if problems.len > 0 {
			return problems
		}
	}
	if !os.is_dir(candidate) {
		os.mkdir_all(os.dir(candidate)) or {
			return ['cannot create `${os.dir(candidate)}`: ${err.msg()}']
		}
		// Create the folder so that only the current user can access it (mode 0700).
		// With the default mode, a umask like 002 would let the whole group write to it.
		// `tool_cache_root_can_stage` would then reject the folder, and every tool would
		// be recompiled on each run instead of being cached.
		os.mkdir(candidate, mode: 0o700) or {
			if !os.is_dir(candidate) {
				return ['cannot create it: ${err.msg()}']
			}
		}
	}
	if !directory_is_writable(candidate) {
		return ['the current user cannot create files in it']
	}
	problem := tool_cache_root_problem(candidate)
	if problem != '' {
		return [problem]
	}
	return []
}

// tool_cache_unavailable_message explains why no tool cache folder can be used, and how to
// get one: every caller that needs the cache reports it the same way.
fn tool_cache_unavailable_message(rejected []ToolCacheRejection) string {
	mut lines := ['no usable tool cache folder was found:']
	for rejection in rejected {
		lines << '  - `${rejection.candidate}`:'
		for problem in rejection.problems {
			lines << '      ${problem}'
		}
	}
	$if windows {
		lines << 'The tool cache folder has to belong to you, and no other account may be allowed to change it.'
		lines << 'Fix its permissions, or set ${tool_cache_dir_env} to a folder that only you can change.'
	} $else {
		lines << 'The tool cache folder, and every folder above it up to the first one that also holds the V'
		lines << 'executable, has to belong to you or to root, and nobody else may write to it unless it has the'
		lines << 'sticky bit. Otherwise someone else could replace the tools that V runs.'
		lines << 'Fix the permissions listed above, or set ${tool_cache_dir_env} to a folder that only you can'
		lines << 'write to (V does not check the folders above that one), for example:'
		lines << '  mkdir -p -m 700 "\$HOME/.vtools" && export ${tool_cache_dir_env}="\$HOME/.vtools"'
	}
	return lines.join('\n')
}

// prepare_default_tool_cache_root makes sure that nobody else can change a default cache
// folder, or what is inside it. It returns why the folder cannot be used, or nothing when it
// can.
//
// The parent is V's own folder (`~/.cache/v`), the `VTMP` folder (`/tmp/v_<uid>` by default), or
// the home folder, for `~/.vtools`. It has to be private, along with every folder above it, or
// someone else could swap the cache folder for their own one at any time; the one exception is
// described at `tool_cache_folder_common_with_compiler`. V tightens `~/.cache/v` itself, when
// the current user owns it.
//
// A cache folder that others could write to earlier may hold entries that someone else put
// there, and fixing its permissions afterwards does not make those entries trustworthy. So
// such a folder is moved aside and never read again, and a new, empty one is created instead.
// A symlink is moved aside too: it could have been planted while the parent was writable,
// and the folder it points to lives under parents that were never checked.
fn prepare_default_tool_cache_root(candidate string) []string {
	parent := os.dir(candidate)
	os.mkdir_all(parent, mode: 0o700) or { return ['cannot create `${parent}`: ${err.msg()}'] }
	// A `VTMP` folder set by the user may be shared on purpose, so it is left as it is.
	if !tool_cache_root_can_stage(parent) && (parent == os.join_path(os.cache_dir(), 'v')
		|| parent == os.join_path(os.home_dir(), '.v')) {
		make_tool_cache_root_private(parent)
	}
	if !tool_cache_parents_are_trusted(parent) {
		problems := tool_cache_parent_problems(parent)
		if problems.len == 0 {
			return ['a folder above `${parent}` can be changed by someone else']
		}
		return problems
	}
	if os.is_link(candidate) || (os.is_dir(candidate) && !tool_cache_root_can_stage(candidate)) {
		$if windows {
			// Privacy is decided by ACLs there, and a new folder would get the same ACL.
			if os.is_link(candidate) {
				return ['it is a symbolic link']
			}
			return [tool_cache_root_problem(candidate)]
		}
		aside := '${candidate}.untrusted-${time.now().unix()}-${os.getpid()}'
		// If this fails, e.g. because another V process just did the same, the check after
		// the folder is created again decides. A symlink that is still there is never used.
		os.rename(candidate, aside) or {
			if os.is_link(candidate) {
				return ['it is a symbolic link, and it could not be moved aside: ${err.msg()}']
			}
			return []
		}
		if tool_cache_is_verbose() {
			eprintln('> moved the untrusted tool cache folder `${candidate}` aside, to `${aside}`')
		}
	}
	return []
}

// tool_cache_parent_problems explains why `tool_cache_parents_are_trusted` rejects `path`. It
// walks up from `path`, both as written and with symlinks resolved, like the check does, and
// lists every folder that does not pass `tool_cache_root_can_stage`, nearest first, for as long
// as the check still rejects the folders from there upwards. So all of them can be fixed at once.
// The walk stops at the folders that the check tolerates for `path`, see
// `tool_cache_parent_is_tolerated`: they do not need to be fixed.
fn tool_cache_parent_problems(path string) []string {
	mut problems := []string{}
	for start in [os.real_path(path), os.abs_path(path)] {
		mut current := start
		for !tool_cache_parents_are_trusted(current) && !tool_cache_parent_is_tolerated(start, current) {
			problem := tool_cache_root_problem(current)
			if problem != '' && problem !in problems {
				problems << problem
			}
			parent := os.dir(current)
			if parent == current {
				break
			}
			current = parent
		}
	}
	return problems
}

// directory_is_writable reports whether this process can actually create files in a
// directory. Existing and being a directory does not answer that: a cache left behind by an
// earlier `sudo` run belongs to another user, a `VTOOLS_CACHE_DIR` can point at a read-only
// mount, and an ACL can deny what the mode bits allow. Writing a probe is the only reliable
// test, and without it an unwritable first candidate is returned and every tool build then
// fails at staging, rather than falling through to the temporary directory that would work.
fn directory_is_writable(directory string) bool {
	probe := os.join_path(directory, '.tool-cache-probe.${os.getpid()}')
	os.write_file(probe, '') or { return false }
	os.rm(probe) or {}
	return true
}

// tool_cache_home_candidate returns the private home tool cache folder, `~/.vtools`, or an empty
// string without a home folder. It is for the hosts where the folders above both other defaults
// can be changed by someone else: with a umask of 002, `~/.cache` is writable by its group, and
// in some containers `/tmp` belongs to another user. The home folder itself normally is private.
// V creates the folder with mode 0700, and never changes the permissions of the home folder.
fn tool_cache_home_candidate() string {
	home := os.home_dir().trim_space()
	if home == '' {
		return ''
	}
	return os.join_path(home, '.vtools')
}

// tool_key_sources returns the files that make up the tool itself, in a stable order.
// A tool can either be a single `.v` file, or a whole directory. Test files and the
// tool's own `tests/` fixtures are irrelevant to the produced binary, so they are skipped.
fn tool_key_sources(tool_source string) []string {
	if !os.is_dir(tool_source) {
		return [tool_source]
	}
	mut files := []string{}
	collect_tool_key_sources(tool_source, mut files)
	files.sort()
	return files
}

fn collect_tool_key_sources(directory string, mut files []string) {
	entries := os.ls(directory) or { return }
	for entry in entries {
		if entry.starts_with('.') || entry == 'tests' || entry == 'testdata' {
			continue
		}
		path := os.join_path(directory, entry)
		if os.is_dir(path) {
			collect_tool_key_sources(path, mut files)
			continue
		}
		if entry.ends_with('_test.v') {
			continue
		}
		files << path
	}
}

// file_stamp returns a cheap identity for a path: its modification time and size.
fn file_stamp(path string) string {
	attributes := os.stat(path) or { return file_stamp_missing }
	return '${attributes.mtime}${tool_cache_field_separator}${attributes.size}'
}

// boundary_marker_stamp records only whether a marker exists; its contents and directory
// metadata do not change module resolution.
fn boundary_marker_stamp(path string) string {
	return if os.exists(path) { 'present' } else { 'missing' }
}

// file_stamp_missing is the stamp of a path that does not exist. A build input can legitimately
// be absent -- `$embed_file` records where its asset should be so that restoring it invalidates
// the build -- and an absent path is a stable value rather than an error.
const file_stamp_missing = 'missing'

// dir_stamp digests the names of the V sources in a directory. It exists to catch the one
// change a per file stamp cannot see: a source file appearing in, or disappearing from, a
// module the tool already imports. Hashing the names rather than using the directory's
// modification time keeps it undisturbed by unrelated files that land next to the sources,
// such as the `*.tmp.c` leftovers of a compilation.
fn dir_stamp(path string) string {
	mut names := os.ls(path) or { return 'missing' }
	names = names.filter(it.ends_with('.v') || it.ends_with('.vv') || it.ends_with('.vsh'))
	names.sort()
	return sha256.hexhash(names.join('\n'))
}

// module_root_stamp digests the names of the directories that live directly inside a directory.
// Failure manifests use it to detect a new directory in a recursively searched `v.mod` subdir.
// It also remains readable for manifests written before exact module-directory stamps were
// introduced.
fn module_root_stamp(path string) string {
	entries := os.ls(path) or { return 'missing' }
	mut names := entries.filter(os.is_dir(os.join_path(path, it)))
	names.sort()
	return sha256.hexhash(names.join('\n'))
}

// module_directory_stamp records whether one exact module path is a directory. Unlike a
// parent-directory listing, it is unaffected when an unrelated sibling module is added.
fn module_directory_stamp(path string) string {
	if os.is_dir(path) {
		return 'directory:${os.real_path(path)}'
	}
	if os.exists(path) {
		return 'not-directory'
	}
	return file_stamp_missing
}

// binary_identity identifies one published executable, so that a manifest can name the exact
// build it describes. Every build stages a fresh file and a rename carries its native identity
// over, so two builds of the same key never share one. Windows needs its native volume and file
// index because the CRT inode is not reliable there. Hashing the contents is the
// collision-resistant fallback for filesystems that do not expose a usable identity.
fn binary_identity(path string) string {
	$if windows {
		if identity := windows_binary_file_identity(path) {
			return 'windows:${identity}'
		}
	} $else {
		attributes := os.stat(path) or { return file_stamp_missing }
		if attributes.inode != 0 {
			return 'stat:${attributes.dev}:${attributes.inode}:${attributes.size}:${attributes.mtime}'
		}
	}
	contents := os.read_bytes(path) or { return file_stamp_missing }
	return 'sha256:${sha256.sum256(contents).hex()}'
}

// tool_application_manifest_identity detects sidecar edits even within one timestamp tick.
fn tool_application_manifest_identity(path string) string {
	contents := os.read_bytes(path) or { return file_stamp_missing }
	return 'sha256:${sha256.sum256(contents).hex()}'
}

// last_modified returns the modification time of a path, or the largest possible time when
// it cannot be determined, so that an unreadable input always counts as changed.
fn last_modified(path string) i64 {
	attributes := os.stat(path) or { return max_i64 }
	return attributes.mtime
}

// ambient_build_variables are the environment variables that change what a tool build
// produces without being part of its command line. `CFLAGS` and `LDFLAGS` are applied by the
// driver to native compilation and linking; `VCOVDIR` turns on coverage instrumentation. The
// `PKG_CONFIG_*` ones decide what `$pkgconfig(...)` and `#pkgconfig` resolve to, which
// selects whole native branches: `vlib/db/sqlite/sqlite.c.v` picks between the system SQLite
// and the bundled amalgamation that way, and neither choice changes a single source stamp.
// `VMODULES` moves the second module search root, which decides which copy of a module an
// import resolves to, so it selects sources without changing any recorded path.
const ambient_build_variables = ['CFLAGS', 'LDFLAGS', 'VCOVDIR', 'PKG_CONFIG_PATH', 'PKG_CONFIG_LIBDIR',
	'PKG_CONFIG_SYSROOT_DIR', 'VMODULES']

// tool_cache_key derives the content address of a cached tool binary. Everything that can
// change the produced executable without being visible in the recorded source manifest has
// to be part of it: the V executable, the tool's own sources, and the build flags.
fn tool_cache_key(vexe string, tool_name string, tool_sources []string, build_args []string) string {
	mut parts := []string{}
	parts << tool_cache_manifest_version
	parts << 'vexe${tool_cache_field_separator}${vexe}${tool_cache_field_separator}${file_stamp(vexe)}'
	parts << 'tool${tool_cache_field_separator}${tool_name}'
	parts << 'args${tool_cache_field_separator}${build_args.join(tool_cache_field_separator)}'
	parts << 'vflags${tool_cache_field_separator}${os.getenv('VFLAGS')}'
	// The child compiler inherits the environment, and the driver reads these directly from
	// it rather than from `build_args`, so they change the produced binary without appearing
	// anywhere else in the identity. A tool first built with a sanitizer, an extra macro or a
	// coverage directory must not be handed back once that setting is gone.
	for name in ambient_build_variables {
		parts << 'env${tool_cache_field_separator}${name}${tool_cache_field_separator}${os.getenv(name)}'
	}
	// Which `pkg-config` is on PATH decides the same branches, and swapping it (a cross
	// toolchain's wrapper, or simply installing one) leaves every environment variable above
	// untouched. Stamping the resolved binary catches that without keying on all of PATH,
	// which changes between shells and would defeat the cache outright.
	parts << 'pkgconfig${tool_cache_field_separator}${pkgconfig_executable_stamp()}'
	for source in tool_sources {
		parts << 'src${tool_cache_field_separator}${source}${tool_cache_field_separator}${file_stamp(source)}'
	}
	return sha256.hexhash(parts.join('\n'))
}

// pkgconfig_executable_stamp identifies the `pkg-config` that a build would use. Resolving it
// costs a short PATH scan and one stat, so it stays cheap enough for a launcher that runs once
// per file. A `.pc` file appearing or disappearing without any of this changing is still not
// detected; catching that would mean running `pkg-config` on every single tool launch.
fn pkgconfig_executable_stamp() string {
	path := os.find_abs_path_of_executable('pkg-config') or { return 'missing' }
	return '${path}${tool_cache_field_separator}${file_stamp(path)}'
}

// tool_cache_entry_dir_for_uid keeps private cache entries from different accounts distinct
// when they intentionally share a sticky cache root.
fn tool_cache_entry_dir_for_uid(directory string, tool_name string, key string, uid int) string {
	return os.join_path(directory, '${tool_name}-${key}.${uid}')
}

// tool_cache_entry locates the cache slot for a tool. When no cache is usable, the error says
// which folders were tried, and why each one was rejected.
fn tool_cache_entry(vexe string, vroot string, tool_name string, tool_source string, build_args []string) !ToolCacheEntry {
	directory := tool_cache_dir()!
	key := tool_cache_key(vexe, tool_name, tool_key_sources(tool_source), build_args)
	entry_dir := tool_cache_entry_dir_for_uid(directory, tool_name, key, os.getuid())
	binary := os.join_path(entry_dir, '${tool_name}${tool_exe_suffix()}')
	return ToolCacheEntry{
		name:                 tool_name
		source:               tool_source
		vroot:                vroot
		dir:                  entry_dir
		binary:               binary
		manifest:             os.join_path(entry_dir, 'inputs')
		unbuildable:          os.join_path(entry_dir, 'unbuildable')
		unbuildable_manifest: os.join_path(entry_dir, 'unbuildable.inputs')
		build_args:           build_args.clone()
	}
}

// tool_cache_stale_reason returns an empty string when the cached binary is still usable,
// and otherwise describes the input that no longer matches what it was built from.
fn tool_cache_stale_reason(entry ToolCacheEntry) string {
	if !os.is_executable(entry.binary) {
		return 'there is no cached binary at `${entry.binary}`'
	}
	return recorded_inputs_changed(entry.manifest)
}

// recorded_inputs_changed returns an empty string when every input written to `manifest_path`
// still matches what it was when the manifest was recorded, and otherwise names the first one
// that does not. It is used both for a cached binary and for a cached build failure.
fn recorded_inputs_changed(manifest_path string) string {
	manifest := os.read_file(manifest_path) or {
		return 'the recorded inputs at `${manifest_path}` are missing'
	}
	return recorded_input_contents_changed(manifest, manifest_path)
}

// recorded_input_contents_changed also validates manifests read through a pinned directory.
fn recorded_input_contents_changed(manifest string, manifest_path string) string {
	lines := manifest.split_into_lines()
	if lines.len < 2 || lines[0] != tool_cache_manifest_version
		|| !lines[1].starts_with('started${tool_cache_field_separator}') {
		return 'the recorded inputs at `${manifest_path}` are not readable'
	}
	started := lines[1].all_after(tool_cache_field_separator).i64()
	for line in lines[2..] {
		if line == '' {
			continue
		}
		fields := line.split(tool_cache_field_separator)
		if fields.len < 3 {
			return 'the recorded inputs at `${manifest_path}` are not readable'
		}
		kind, path, recorded := fields[0], fields[1], fields[2..].join(tool_cache_field_separator)
		current := match kind {
			'd' { dir_stamp(path) }
			'e' { boundary_marker_stamp(path) }
			'm' { module_root_stamp(path) }
			'p' { module_directory_stamp(path) }
			'b' { binary_identity(path) }
			'a' { tool_application_manifest_identity(path) }
			else { file_stamp(path) }
		}
		if current != recorded {
			return '`${path}` changed (recorded `${recorded}`, found `${current}`)'
		}
		if kind != 'f' {
			continue
		}
		if current == file_stamp_missing {
			// The input was absent when it was recorded and still is. `last_modified` cannot
			// date a file that does not exist and reports the end of time, which would make
			// the check below call it changed on every single lookup and rebuild forever.
			continue
		}
		// `os.stat` only resolves modification times down to the second, so a file that was
		// rewritten within the same second the build started can carry an unchanged stamp.
		// Anything that is not strictly older than the build is therefore treated as changed:
		// at worst that costs one extra recompilation, while the alternative would be handing
		// out a binary built from sources that no longer exist.
		modified := last_modified(path)
		if modified >= started {
			return '`${path}` was modified at ${modified}, which is not older than the build that started at ${started}'
		}
	}
	return ''
}

// unbuildable_tool_failure returns the compiler output of a previous, already recorded,
// failed build of the tool, but only while that failure still applies to the current sources.
// Without the second half of that condition a single broken intermediate state of some vlib
// module would permanently pin the tool to the compatibility compiler.
fn unbuildable_tool_failure(entry ToolCacheEntry) ?string {
	details := os.read_file(entry.unbuildable) or { return none }
	if recorded_inputs_changed(entry.unbuildable_manifest) != '' {
		return none
	}
	return details
}

// c_stage_failure_markers are the ways the driver reports that the generated C could not be
// compiled or linked. The C stage only runs once the whole V source closure has been accepted,
// so a failure carrying one of these is about the toolchain and not about the tool's sources.
const c_stage_failure_markers = ['C compilation failed:', 'C compilation error (from ',
	'failed parallel C compilation', 'failed to link after parallel C compilation',
	'was not found while linking the generated program']

// build_failure_is_source_dependent reports whether a failed tool build is explained by the
// tool's V sources, and may therefore be cached against them. Anything else -- a C compiler
// that was not installed at the time, a transient linker failure, a build the OOM killer cut
// short, a child process that never started -- has to be retried on the next invocation,
// because nothing in the recorded manifest would ever invalidate it.
fn build_failure_is_source_dependent(details string) bool {
	text := details.trim_space()
	if text == '' {
		// A build that produced no diagnostic at all was not a rejection of the sources.
		return false
	}
	for marker in c_stage_failure_markers {
		if text.contains(marker) {
			return false
		}
	}
	for line in text.split_into_lines() {
		if line_is_v_diagnostic(line) {
			return true
		}
	}
	return false
}

// line_is_v_diagnostic reports whether a line is a V compiler diagnostic, i.e. has the
// `<file>:<line>:<column>: error: ...` shape that the frontend gives every one of them.
// The position is what makes the test meaningful: without it any output that merely
// contains the word `error` would pass, including a C compiler's own.
fn line_is_v_diagnostic(line string) bool {
	mut head := ''
	if index := line.index(': error: ') {
		head = line[..index]
	} else if index := line.index(': builder error: ') {
		head = line[..index]
	} else {
		return false
	}
	if !head.contains(':') {
		return false
	}
	column := head.all_after_last(':')
	rest := head.all_before_last(':')
	if !rest.contains(':') {
		return false
	}
	return is_all_digits(column) && is_all_digits(rest.all_after_last(':'))
}

fn is_all_digits(text string) bool {
	if text == '' {
		return false
	}
	for character in text {
		if !character.is_digit() {
			return false
		}
	}
	return true
}

// unresolved_import_modules returns the modules that a failed build could not resolve at all.
// Such a module contributed no source file to the dump, because there was nothing to read, so
// stamping where it would have lived is the only way a later invocation notices it came back.
fn unresolved_import_modules(details string) []string {
	marker := 'cannot import module "'
	mut names := map[string]bool{}
	mut rest := details
	for {
		index := rest.index(marker) or { break }
		rest = rest[index + marker.len..]
		end := rest.index('"') or { break }
		name := rest[..end].trim_space()
		rest = rest[end + 1..]
		if name.len > 0 {
			names[name] = true
		}
	}
	mut result := names.keys()
	result.sort()
	return result
}

// module_search_roots returns the directories an import is resolved against, in the order
// `pref` searches them. An explicit `-path` replaces the defaults and supports the same
// placeholders as the driver. Without one, the tree's own `vlib` and every `~/.vmodules`
// entry are searched. A missing module can reappear in any of those roots.
fn module_search_roots(vroot string, build_args []string) []string {
	mut spec := ''
	for index, argument in build_args {
		if argument == '-path' && index + 1 < build_args.len {
			spec = build_args[index + 1]
		}
	}
	if spec != '' {
		mut roots := []string{}
		for path in spec.replace('|', os.path_delimiter).split(os.path_delimiter) {
			match path {
				'@vlib' { roots << os.join_path(vroot, 'vlib') }
				'@vmodules' { roots << os.vmodules_paths() }
				else { roots << path.replace('@vroot', vroot) }
			}
		}
		return roots
	}
	mut roots := [os.join_path(vroot, 'vlib')]
	for path in os.vmodules_paths() {
		clean := path.trim_space()
		if clean.len > 0 && clean !in roots {
			roots << clean
		}
	}
	return roots
}

fn append_module_search_root(path string, mut roots []string, mut seen map[string]bool) {
	if path.trim_space() == '' {
		return
	}
	root := os.real_path(path)
	if !seen[root] {
		seen[root] = true
		roots << root
	}
}

fn failure_importer_dirs(entry ToolCacheEntry, source_files []string) []string {
	mut importer_dirs := map[string]bool{}
	if entry.source != '' {
		source_dir := if os.is_dir(entry.source) {
			os.real_path(entry.source)
		} else {
			os.dir(os.real_path(entry.source))
		}
		importer_dirs[source_dir] = true
	}
	for source in source_files {
		if source.ends_with('.v') || source.ends_with('.vv') || source.ends_with('.vsh') {
			importer_dirs[os.dir(os.real_path(source))] = true
		}
	}
	mut result := importer_dirs.keys()
	result.sort()
	return result
}

// failure_module_search_roots covers the importer-local and project/ancestor locations that
// V3 tries in addition to the configured global roots. A failed transitive import can come
// from any V source already read before the failure, so each such importer contributes roots.
fn failure_module_search_roots(entry ToolCacheEntry, source_files []string) []string {
	mut roots := []string{}
	mut seen := map[string]bool{}
	importer_dirs := failure_importer_dirs(entry, source_files)
	for importer_dir in importer_dirs {
		append_module_search_root(importer_dir, mut roots, mut seen)
		append_module_search_root(os.join_path(importer_dir, 'modules'), mut roots, mut seen)
	}
	// project_root_for_files resolves to the surrounding v.mod root. Cached tools are sources
	// inside the V project, whose root is already carried explicitly by the cache entry.
	if entry.vroot != '' {
		append_module_search_root(entry.vroot, mut roots, mut seen)
		append_module_search_root(os.join_path(entry.vroot, 'modules'), mut roots, mut seen)
	}
	for root in module_search_roots(entry.vroot, entry.build_args) {
		append_module_search_root(root, mut roots, mut seen)
	}
	// Finally mirror the sibling-project fallback that walks upward from each importer.
	for importer_dir in importer_dirs {
		mut current := importer_dir
		for {
			append_module_search_root(current, mut roots, mut seen)
			append_module_search_root(os.join_path(current, 'modules'), mut roots, mut seen)
			parent := os.parent_dir(current)
			if parent == '' || parent == current {
				break
			}
			current = parent
		}
	}
	return roots
}

// vmod_manifest_inputs returns every path inspected while finding the manifest that would own
// an existing module directory. Ancestor module resolution consults these paths before
// accepting a module outside the importer's project, so changing the declaration or creating
// a nearer manifest has to invalidate a cached failure.
fn vmod_manifest_inputs(directory string) []string {
	if !os.is_dir(directory) {
		return []
	}
	mut inputs := []string{}
	mut current := os.real_path(directory)
	for {
		manifest := os.join_path(current, 'v.mod')
		inputs << manifest
		if os.is_file(manifest) {
			break
		}
		parent := os.parent_dir(current)
		if parent == '' || parent == current {
			break
		}
		current = parent
	}
	return inputs
}

fn module_alias_target_from_source(source string) ?string {
	marker := '@[alias'
	marker_pos := source.index(marker) or { return none }
	mut rest := source[marker_pos + marker.len..].trim_space()
	if !rest.starts_with(':') {
		return none
	}
	rest = rest[1..].trim_space()
	if rest.len < 2 || rest[0] !in [`'`, `"`] {
		return none
	}
	quote := rest[0]
	mut end := 1
	for end < rest.len && rest[end] != quote {
		end++
	}
	if end >= rest.len {
		return none
	}
	target := rest[1..end].clone()
	if target == '' {
		return none
	}
	rest = rest[end + 1..].trim_space()
	if !rest.starts_with(']') {
		return none
	}
	rest = rest[1..].trim_space()
	if !rest.starts_with('module ') {
		return none
	}
	return target
}

// module_alias_targets returns every parseable alias target the resolver checks for an import.
// Targets that are currently absent or empty still matter because either can later become a
// usable module without changing the alias declaration itself.
fn module_alias_targets(search_root string, module_name string, mut manifest_inputs map[string]bool) []string {
	parts := module_name.split('.')
	mut targets := []string{}
	for part_count := parts.len; part_count > 0; part_count-- {
		alias_dir := os.join_path(search_root, parts[..part_count].join(os.path_separator))
		alias_file := os.join_path(alias_dir, 'alias.v')
		if !os.is_file(alias_file) {
			continue
		}
		source := os.read_file(alias_file) or { continue }
		mut target := module_alias_target_from_source(source) or { continue }
		if target.contains('@VMODROOT') {
			vmod_inputs := vmod_manifest_inputs(alias_dir)
			if vmod_inputs.len == 0 || !os.is_file(vmod_inputs.last()) {
				continue
			}
			for input in vmod_inputs {
				manifest_inputs[input] = true
			}
			vmod_root := os.dir(vmod_inputs.last())
			target = target.replace('@VMODROOT', vmod_root)
		}
		if !os.is_abs_path(target) {
			target = os.join_path(alias_dir, target)
		}
		if part_count < parts.len {
			target = os.join_path(target, parts[part_count..].join(os.path_separator))
		}
		targets << os.real_path(target)
	}
	return targets
}

// append_module_source_tree_inputs records the directory structure and V source names below
// one recursively searched `v.mod` subdir. Nested manifests stop that search, just as they do
// in the module resolver; recording absent manifests detects a newly introduced boundary too.
fn append_module_source_tree_inputs(source_root string, directory string, mut module_dirs map[string]bool, mut module_roots map[string]bool, mut module_source_dirs map[string]bool, mut module_manifests map[string]bool, mut seen_dirs map[string]bool) {
	module_dirs[directory] = true
	if !os.is_dir(directory) {
		return
	}
	real_directory := os.real_path(directory)
	if seen_dirs[real_directory] {
		return
	}
	seen_dirs[real_directory] = true
	nested_manifest := os.join_path(real_directory, 'v.mod')
	if real_directory != source_root {
		module_manifests[nested_manifest] = true
		if os.is_file(nested_manifest) {
			return
		}
	}
	module_roots[real_directory] = true
	module_source_dirs[real_directory] = true
	for entry in os.ls(real_directory) or { return } {
		entry_path := os.join_path(real_directory, entry)
		if os.is_dir(entry_path) {
			append_module_source_tree_inputs(source_root, entry_path, mut module_dirs, mut module_roots, mut module_source_dirs, mut module_manifests, mut seen_dirs)
		}
	}
}

// append_vmod_module_source_inputs records the alternate source root and recursive subdirs
// that can make an existing, otherwise empty module directory usable.
fn append_vmod_module_source_inputs(module_dir string, mut module_dirs map[string]bool, mut module_roots map[string]bool, mut module_source_dirs map[string]bool, mut module_manifests map[string]bool) {
	if !os.is_dir(module_dir) {
		return
	}
	real_module_dir := os.real_path(module_dir)
	manifest_path := os.join_path(real_module_dir, 'v.mod')
	module_manifests[manifest_path] = true
	if !os.is_file(manifest_path) {
		return
	}
	if (os.read_file(manifest_path) or { return }).trim_space() == '' {
		return
	}
	manifest := vmod.from_file(manifest_path) or { return }
	source_root_path := manifest.source_root(real_module_dir)
	module_dirs[source_root_path] = true
	if !os.is_dir(source_root_path) {
		return
	}
	source_root := os.real_path(source_root_path)
	module_source_dirs[source_root] = true
	for subdir in manifest.unknown['subdirs'] or { [] } {
		mut seen_dirs := map[string]bool{}
		append_module_source_tree_inputs(source_root, os.join_path_single(source_root, subdir), mut module_dirs, mut module_roots, mut module_source_dirs, mut module_manifests, mut seen_dirs)
	}
}

fn nearest_vmod_root(directory string) string {
	inputs := vmod_manifest_inputs(directory)
	if inputs.len > 0 && os.is_file(inputs.last()) {
		return os.dir(inputs.last())
	}
	return ''
}

fn path_is_within_directory(path string, directory string) bool {
	mut real_path := os.real_path(path).replace('\\', '/')
	mut real_directory := os.real_path(directory).replace('\\', '/')
	$if windows {
		real_path = real_path.to_lower()
		real_directory = real_directory.to_lower()
	}
	if real_path == real_directory {
		return true
	}
	return if real_directory.ends_with('/') {
		real_path.starts_with(real_directory)
	} else {
		real_path.starts_with(real_directory + '/')
	}
}

// failure_module_manifest_inputs mirrors the ancestor resolver's manifest probes. These are
// separate from ordinary module roots: only candidates outside an importer's v.mod project
// have their owning manifest checked before they can satisfy an import.
fn failure_module_manifest_inputs(entry ToolCacheEntry, source_files []string, module_name string) []string {
	module_path := module_name.replace('.', os.path_separator)
	mut inputs := map[string]bool{}
	importer_dirs := failure_importer_dirs(entry, source_files)
	for importer_dir in importer_dirs {
		importer_root := nearest_vmod_root(importer_dir)
		if importer_root == '' {
			continue
		}
		mut current := importer_dir
		for {
			for candidate in [os.join_path(current, module_path),
				os.join_path(current, 'modules', module_path)] {
				if path_is_within_directory(candidate, importer_root) {
					continue
				}
				for manifest in vmod_manifest_inputs(candidate) {
					inputs[manifest] = true
				}
			}
			parent := os.parent_dir(current)
			if parent == '' || parent == current {
				break
			}
			current = parent
		}
	}
	mut result := inputs.keys()
	result.sort()
	return result
}

// encode_unbuildable_tool_manifest records the inputs that caused a failed build, so that
// the failing compilation is not repeated on every invocation, while fixing any of those
// inputs still makes it be retried.
fn encode_unbuildable_tool_manifest(entry ToolCacheEntry, dumped string, started i64, details string) string {
	source_files := (os.read_file(dumped) or { '' }).split_into_lines().filter(it != '')
	mut manifest := encode_tool_cache_manifest(source_files, started)
	// The compiler reports what it read even for most failures, but the one failure it
	// cannot describe that way is an import it could not resolve at all, because the module
	// has no files to stamp. Recording the module roots as well is what makes the tool be
	// retried as soon as a missing module reappears.
	search_roots := failure_module_search_roots(entry, source_files)
	mut module_dirs := map[string]bool{}
	mut module_roots := map[string]bool{}
	mut module_source_dirs := map[string]bool{}
	mut module_alias_files := map[string]bool{}
	mut module_manifests := map[string]bool{}
	for module_name in unresolved_import_modules(details) {
		// Stamp each exact module directory, then stamp the final directory's V source names:
		// an existing empty directory becomes usable when its first `.v` file appears. Every
		// root the compiler would have searched counts because the module may reappear there.
		for search_root in search_roots {
			mut path := search_root
			parts := module_name.split('.')
			for index, part in parts {
				if part.trim_space() == '' {
					break
				}
				path = os.join_path(path, part)
				module_dirs[path] = true
				module_alias_files[os.join_path(path, 'alias.v')] = true
				if index == parts.len - 1 {
					module_source_dirs[path] = true
					append_vmod_module_source_inputs(path, mut module_dirs, mut module_roots, mut module_source_dirs, mut module_manifests)
				}
			}
			for target in module_alias_targets(search_root, module_name, mut module_manifests) {
				module_dirs[target] = true
				module_source_dirs[target] = true
			}
		}
		for module_manifest in failure_module_manifest_inputs(entry, source_files, module_name) {
			module_manifests[module_manifest] = true
		}
	}
	mut sorted_alias_files := module_alias_files.keys()
	sorted_alias_files.sort()
	for alias_file in sorted_alias_files {
		manifest += 'f${tool_cache_field_separator}${alias_file}${tool_cache_field_separator}${file_stamp(alias_file)}\n'
	}
	mut sorted_manifests := module_manifests.keys()
	sorted_manifests.sort()
	for module_manifest in sorted_manifests {
		manifest += 'f${tool_cache_field_separator}${module_manifest}${tool_cache_field_separator}${file_stamp(module_manifest)}\n'
	}
	mut sorted_dirs := module_dirs.keys()
	sorted_dirs.sort()
	for directory in sorted_dirs {
		manifest += 'p${tool_cache_field_separator}${directory}${tool_cache_field_separator}${module_directory_stamp(directory)}\n'
	}
	mut sorted_roots := module_roots.keys()
	sorted_roots.sort()
	for directory in sorted_roots {
		manifest += 'm${tool_cache_field_separator}${directory}${tool_cache_field_separator}${module_root_stamp(directory)}\n'
	}
	mut sorted_source_dirs := module_source_dirs.keys()
	sorted_source_dirs.sort()
	for directory in sorted_source_dirs {
		manifest += 'd${tool_cache_field_separator}${directory}${tool_cache_field_separator}${dir_stamp(directory)}\n'
	}
	return manifest
}

// encode_tool_cache_manifest turns the `-dump-files` source closure into the manifest that
// `tool_cache_stale_reason` revalidates. Besides the files themselves it records the directories
// that contain them, so that a source file added to an imported module is detected too.
fn encode_tool_cache_manifest(source_files []string, started i64) string {
	mut lines := []string{}
	lines << tool_cache_manifest_version
	lines << 'started${tool_cache_field_separator}${started}'
	mut directories := map[string]bool{}
	mut boundaries := map[string]bool{}
	for file in source_files {
		if file == '' {
			continue
		}
		lines << 'f${tool_cache_field_separator}${file}${tool_cache_field_separator}${file_stamp(file)}'
		directories[os.dir(file)] = true
		mut directory := os.real_path(os.dir(file))
		for {
			for marker in util.project_boundary_markers {
				boundaries[os.join_path(directory, marker)] = true
			}
			parent := os.dir(directory)
			if parent == directory {
				break
			}
			directory = parent
		}
	}
	mut names := directories.keys()
	names.sort()
	for name in names {
		lines << 'd${tool_cache_field_separator}${name}${tool_cache_field_separator}${dir_stamp(name)}'
	}
	mut marker_paths := boundaries.keys()
	marker_paths.sort()
	for path in marker_paths {
		lines << 'e${tool_cache_field_separator}${path}${tool_cache_field_separator}${boundary_marker_stamp(path)}'
	}
	return lines.join('\n') + '\n'
}

// is_cache_artifact_of reports whether `name` is the entry directory of a cache entry of
// `tool`. The tool name alone is not enough to tell them apart, because one tool's name can
// be a prefix of another's (`v bug` and `v bug-report`), so the content address itself has
// to be matched.
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

// tool_cache_lock_path_for_uid namespaces one tool's persistent mutex in a sticky shared
// cache root. Otherwise the first account leaves an inode that every other account rejects.
fn tool_cache_lock_path_for_uid(entry ToolCacheEntry, uid int) string {
	return os.join_path(os.dir(entry.dir), '.${entry.name}.${uid}.toolcache.lock')
}

// tool_cache_lock_path names the persistent mutex shared by this user's cache keys for one
// tool. Windows cache roots have private ACLs, so its synthetic UID 0 remains owner-private.
fn tool_cache_lock_path(entry ToolCacheEntry) string {
	return tool_cache_lock_path_for_uid(entry, os.getuid())
}

// tool_cache_lock opens the persistent lock file without removing it between owners.
fn tool_cache_lock(entry ToolCacheEntry) !filelock.FileLock {
	path := tool_cache_lock_path(entry)
	ensure_tool_cache_lock_file(path)!
	return filelock.new_file(path, mode: .exclusive)
}

// prune_stale_tool_binaries_locked drops invalid cache entries of the same tool while its
// build lock is held, retaining fresh builds for other compiler flags and checkouts sharing
// the cache directory. Unlinking a running executable is safe on POSIX: the process keeps its
// own already opened image.
fn prune_stale_tool_binaries_locked(entry ToolCacheEntry) {
	// A binary that Windows would not let us overwrite while it was still being executed was
	// renamed aside instead. Open the entry without following links and enumerate/remove its
	// children through that pinned directory, rather than through its replaceable pathname.
	cache_entry := open_tool_cache_entry_dir(entry.dir) or { return }
	cache_entry.prune_replaced_binaries()
	cache_entry.close()
	directory := os.dir(entry.dir)
	keep := os.file_name(entry.dir)
	entries := os.ls(directory) or { return }
	for name in entries {
		if !is_cache_artifact_of(name, entry.name) {
			continue
		}
		if name == keep {
			continue
		}
		path := os.join_path(directory, name)
		if !os.is_link(path) && os.is_dir(path) {
			// Check ownership and pin the sibling before reading its metadata. A FIFO or
			// symlink planted as a manifest must not block pruning while the lock is held.
			other_dir := open_tool_cache_entry_dir(path) or { continue }
			other := ToolCacheEntry{
				name:                 entry.name
				dir:                  path
				binary:               os.join_path(path, entry.name + tool_exe_suffix())
				manifest:             os.join_path(path, 'inputs')
				unbuildable:          os.join_path(path, 'unbuildable')
				unbuildable_manifest: os.join_path(path, 'unbuildable.inputs')
			}
			// A different content key can still be in use by another checkout or
			// flag combination. Its recorded inputs decide whether it is stale.
			if other_dir.has_fresh_build(other) {
				other_dir.close()
				continue
			}
			other_dir.remove_all_contents()
			other_dir.close()
			os.rmdir(path) or {}
			continue
		}
		prune_stale_tool_artifact(path)
	}
}

// has_fresh_build reads only regular metadata files from the pinned, owned entry.
fn (cache_entry ToolCacheEntryDir) has_fresh_build(entry ToolCacheEntry) bool {
	if os.is_executable(entry.binary) {
		if manifest := cache_entry.read_metadata('inputs') {
			if recorded_input_contents_changed(manifest, entry.manifest) == '' {
				return true
			}
		}
	}
	if _ := cache_entry.read_metadata('unbuildable') {
		if manifest := cache_entry.read_metadata('unbuildable.inputs') {
			return recorded_input_contents_changed(manifest, entry.unbuildable_manifest) == ''
		}
	}
	return false
}

fn prune_stale_tool_artifact(path string) {
	if os.is_link(path) {
		// Never recurse through a cache-shaped symlink. Windows removes directory links
		// with RemoveDirectory and file links with remove, so try both non-recursive forms.
		os.rm(path) or {}
		os.rmdir(path) or {}
	} else if !os.is_dir(path) {
		// Legacy cache entries were flat executables and sidecars, so they need unlinking
		// rather than directory removal.
		os.rm(path) or {}
	} else {
		// Pin the directory before traversing it. An entry owned by another account in a
		// sticky shared cache is deliberately left alone; a link substituted at any point
		// is unlinked as a leaf by the platform implementation below.
		stale_entry := open_tool_cache_entry_dir(path) or { return }
		stale_entry.remove_all_contents()
		stale_entry.close()
		os.rmdir(path) or {}
	}
}

// create_tool_cache_stage_dir makes a unique, private directory for compiler output on the
// cache filesystem, so completed files can be installed with an atomic rename.
fn create_tool_cache_stage_dir(parent string) !string {
	if !os.is_dir(parent) {
		return error('cannot find a staging directory for the tool cache')
	}
	for attempt in 0 .. 16 {
		token := unique_cache_path_token(attempt)
		path := os.join_path(parent, '${tool_cache_stage_prefix}${os.getuid()}-${token}')
		os.mkdir(path, mode: 0o700) or { continue }
		return path
	}
	return error('cannot create a private temporary directory for the tool cache')
}

fn unique_cache_path_token(attempt int) string {
	return '${os.getpid()}-${time.now().unix_nano()}-${attempt}'
}

// build_tool_binary compiles the tool into its cache slot and records its source closure.
// It returns the compiler output when the build failed.
fn build_tool_binary(vexe string, entry ToolCacheEntry) !string {
	mut build_lock := tool_cache_lock(entry)!
	if !build_lock.wait_acquire(5 * time.minute) {
		return error('timed out waiting to build the cached `${entry.name}` tool')
	}
	defer {
		build_lock.release()
	}
	// Another process may have completed this exact build while this process waited.
	if tool_cache_stale_reason(entry) == '' {
		return ''
	}
	if details := unbuildable_tool_failure(entry) {
		return error(details)
	}
	cache_entry := open_tool_cache_entry_dir(entry.dir)!
	defer {
		cache_entry.close()
	}
	cache_entry.prune_abandoned_stages()
	stage_parent := cache_entry.stage_parent(entry.dir)!
	stage_dir := create_tool_cache_stage_dir(stage_parent)!
	stage_entry := open_tool_cache_entry_dir(stage_dir)!
	defer {
		stage_entry.close()
		os.rmdir_all(stage_dir) or {}
	}
	staged := staged_tool_binary_path(stage_dir, entry.binary)
	dumped := os.join_path(stage_dir, 'sources')
	started := time.now().unix()
	mut build_args := entry.build_args.clone()
	build_args << ['-dump-files', dumped, '-o', staged, entry.source]
	mut environment := os.environ()
	// The build must either succeed with the current compiler, or fail loudly. Letting the
	// child silently retry with the compatibility compiler would cache a binary that does
	// not correspond to this V executable at all.
	environment[v3_no_fallback_env] = '1'
	// `build_args` already carries everything `VFLAGS` contributed to this invocation.
	// Leaving the variable set would make the child prepend all of it a second time.
	environment['VFLAGS'] = ''
	mut process := os.new_process(vexe)
	process.set_args(build_args)
	process.set_environment(environment)
	process.set_redirect_stdio_merged()
	// Drain the pipe while the child is still running. Waiting first deadlocks as soon as
	// the build produces more output than the pipe buffer holds -- a few hundred V
	// diagnostics, or a C compiler erroring out, is enough: the child blocks writing while
	// the parent blocks waiting, and `v fmt`/`v vet` hang forever. `stdout_slurp` reads
	// until the child closes its end, so it returns once the build is over.
	process.run()
	output := process.stdout_slurp()
	process.wait()
	code := process.code
	failure := process.err
	process.close()
	if code != 0 || !os.is_file(staged) {
		os.rm(staged) or {}
		mut details := if output.trim_space() != '' { output } else { failure }
		if details.trim_space() == '' && code == 0 {
			details = 'compiler exited successfully but did not produce `${staged}`'
		}
		// Only a failure the tool's own V sources explain may be cached against them. A C
		// toolchain that was missing, out of memory or momentarily broken says nothing about
		// those sources, and the manifest does not describe it, so recording it would replay
		// the same error until a source file happened to change.
		if build_failure_is_source_dependent(details) {
			unbuildable_manifest := os.join_path(stage_dir, 'unbuildable.inputs')
			unbuildable_details := os.join_path(stage_dir, 'unbuildable')
			manifest := encode_unbuildable_tool_manifest(entry, dumped, started, details)
			os.write_file(unbuildable_manifest, manifest) or {}
			os.write_file(unbuildable_details, details) or {}
			// Publish the details before the manifest that makes them reusable. A concurrent
			// reader must never see a new manifest vouch for details that were not installed.
			if cache_entry.publish(unbuildable_details, os.file_name(entry.unbuildable)) {
				cache_entry.publish(unbuildable_manifest, os.file_name(entry.unbuildable_manifest))
			}
		}
		return error(details)
	}
	source_files := os.read_file(dumped) or { '' }.split_into_lines()
	// Identify the executable this manifest describes, taken from the staged file before it
	// is published: a rename carries the identity across unchanged. Publishing the binary and
	// the manifest are two separate steps, so two builds racing over one key can interleave
	// and leave one build's executable paired with the other's manifest. Recording the pairing
	// lets a reader see that and rebuild, instead of running stale code that a fresh manifest
	// vouches for.
	staged_sidecar := '${staged}.manifest'
	sidecar := '${entry.binary}.manifest'
	manifest := encode_tool_cache_manifest(source_files, started) + 'b${tool_cache_field_separator}${entry.binary}${tool_cache_field_separator}${binary_identity(staged)}\n' + 'a${tool_cache_field_separator}${sidecar}${tool_cache_field_separator}${tool_application_manifest_identity(staged_sidecar)}\n'
	// Publish the application manifest through the same pinned directory as the
	// executable. Track absence too, so a removed or replaced sidecar is a cache miss.
	if os.is_file(staged_sidecar) {
		if !cache_entry.publish(staged_sidecar, os.file_name(sidecar)) {
			return error('cannot install the application manifest of `${entry.name}`')
		}
	} else {
		cache_entry.remove(os.file_name(sidecar))
	}
	// Publish the executable before its manifest. A lookup requires both, so the worst that
	// a concurrent reader can observe is "binary present, manifest not updated yet", which
	// is simply treated as a miss. The reverse order could hand out a stale binary.
	if !cache_entry.publish(staged, os.file_name(entry.binary)) {
		return error('cannot install the compiled `${entry.name}` into `${entry.binary}`')
	}
	staged_manifest := os.join_path(stage_dir, 'inputs')
	os.write_file(staged_manifest, manifest) or {
		return error('cannot record the inputs of `${entry.name}`: ${err}')
	}
	cache_entry.publish(staged_manifest, os.file_name(entry.manifest))
	cache_entry.remove(os.file_name(entry.unbuildable))
	cache_entry.remove(os.file_name(entry.unbuildable_manifest))
	prune_stale_tool_binaries_locked(entry)
	return ''
}

// exec_cached_tool hands the process over to the cached tool binary. On POSIX this is a
// plain `execvp`, so the tool inherits the standard streams untouched and its exit status
// and signals are reported to the caller's shell verbatim.
@[noreturn]
fn exec_cached_tool(executable string, tool_args []string) {
	$if windows {
		exit(run_windows_cached_tool(executable, tool_args))
	} $else {
		os.execvp(executable, tool_args) or {
			eprintln('cannot start `${executable}`: ${err}')
			exit(1)
		}
	}
	// `execvp` replaces this process, so it only ever returns through the `or` block above.
	// The explicit exit keeps both the current and the bootstrap compiler satisfied that
	// every path out of this `@[noreturn]` function ends in one.
	exit(1)
}

// run_windows_cached_tool waits for the child before wiping caches. Windows can keep the
// child's image locked for a moment after the wait returns; `rmdir_all_settling` retries past
// that. Only a successful updater requests the private exit code.
fn run_windows_cached_tool(executable string, tool_args []string) int {
	// Private handshake; keep in sync with cmd/tools/vup.v.
	deferred_wipe_exit_code := 80
	is_updater := os.file_name(executable).to_lower_ascii() == 'vup.exe'
	mut process := os.new_process(executable)
	process.set_args(tool_args)
	if is_updater {
		mut environment := os.environ()
		environment['VUP_DEFER_CACHE_WIPE'] = '1'
		process.set_environment(environment)
	}
	process.wait()
	code := process.code
	process.close()
	if !is_updater || code != deferred_wipe_exit_code {
		return code
	}
	// This process retains the caller's working directory and cache environment.
	opts := parse_wipe_cache_options([], []) or {
		eprintln(err.msg())
		return 1
	}
	lines := wipe_caches(opts) or {
		eprintln('v up: wiping caches failed: ${err}')
		return 1
	}
	for line in lines {
		println(line)
	}
	return 0
}

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

A tool cache folder is wiped entry by entry. The entries of a tool that another process has
locked right now (a build in progress) stay, and the output names the tool. Lock files stay too,
so the folder stays after a wipe. A lock file that cannot be opened is repaired, so that it can
be locked again, unless another account could swap it for a link: then it is left as it is, and
the entries of its tool are kept. Entries of a tool whose lock file cannot be made, or is no
regular file, are kept; a dry run assumes it can be made.

In a tool cache folder that another account can write to, only what you own is removed. In one
that is yours alone, everything is, whoever owns it, except lock files. Run as root, the wipe
takes each entry under the build lock of the user who built it.

A folder V moved aside as <folder>.untrusted-<seconds>-<pid> is wiped the same way; once nothing
in it is kept, the lock files the wipe holds and then the folder itself are removed too. Any
other <folder>.untrusted-* folder is wiped like a tool cache folder.

A tool cache folder inside VCACHE, VTMP or V3CACHE gets the same treatment when that folder is
wiped, whichever options select it. This covers the folders listed under --tools. A tool cache
folder that only the VTOOLS_CACHE_DIR of another process names is not known to the wipe, and is
removed with the folder it lies in.

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

fn parse_wipe_cache_options(prefix_args []string, command_args []string) !WipeCacheOptions {
	mut dry_run := false
	mut verbose := false
	mut vcache := false
	mut vtmp := false
	mut v3cache := false
	mut tools := false
	mut explicit_selector := false
	mut show_help := false
	for arg in prefix_args {
		if arg in ['-v', '-verbose', '--verbose'] {
			verbose = true
		}
	}
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

// tool_cache_wipe_targets returns every configured and default tool cache folder, together with
// any `*.untrusted-*` folders moved aside by `prepare_default_tool_cache_root`.
fn tool_cache_wipe_targets() []string {
	mut targets := []string{}
	mut seen := map[string]bool{}
	if configured := os.getenv_opt(tool_cache_dir_env) {
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

// inspect_tool_cache_candidates reports the status of each tool cache candidate without
// creating or modifying any directory on disk.
fn inspect_tool_cache_candidates() []string {
	mut lines := []string{}
	if configured := os.getenv_opt(tool_cache_dir_env) {
		trimmed := configured.trim_space()
		if trimmed != '' {
			status := if !os.exists(trimmed) && !os.is_link(trimmed) {
				'does not exist (will be created on demand)'
			} else if !os.is_dir(trimmed) {
				'rejected: is not a directory'
			} else if !directory_is_writable(trimmed) {
				'rejected: directory is not writable'
			} else if !tool_cache_root_can_stage(trimmed) {
				'rejected: ${tool_cache_root_problem(trimmed)}'
			} else {
				'usable (exists)'
			}
			lines << '  - `${trimmed}` (${tool_cache_dir_env}): ${status}'
		}
	}
	for candidate in tool_cache_default_candidates() {
		parent := os.dir(candidate)
		mut existing_parent := parent
		for !os.is_dir(existing_parent) {
			next_parent := os.dir(existing_parent)
			if next_parent == existing_parent {
				break
			}
			existing_parent = next_parent
		}
		mut status := 'usable (will be created on demand)'
		if os.is_dir(existing_parent) {
			problems := tool_cache_parent_problems(existing_parent)
			if problems.len > 0 {
				reason := problems.join('; ')
				status = 'rejected: ${reason}'
			}
		}
		if !status.starts_with('rejected:') {
			if os.is_link(candidate) {
				status = 'untrusted symbolic link (will be moved aside on next tool build)'
			} else if os.is_dir(candidate) && !tool_cache_root_can_stage(candidate) {
				status = 'untrusted (${tool_cache_root_problem(candidate)}; will be moved aside on next tool build)'
			} else if os.is_dir(candidate) {
				status = 'usable (exists)'
			}
		}
		lines << '  - `${candidate}`: ${status}'
	}
	return lines
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

// ToolCacheWipeKind says what a name in a folder being wiped is. A link is never followed.
enum ToolCacheWipeKind {
	gone
	file
	folder
	link
	other
}

// ToolCacheWipeItem describes one name in a folder being wiped: what it is, who owns it, and how
// many names the file has.
struct ToolCacheWipeItem {
	kind  ToolCacheWipeKind
	owner u32
	links u64
}

// ToolCacheWipeLockState says what came of a wipe's attempt to take a build lock.
enum ToolCacheWipeLockState {
	// absent: nothing has the lock file's name.
	absent
	// held: the wipe holds the lock.
	held
	// busy: another process holds the lock, such as a build.
	busy
	// unopenable: the lock file cannot be opened for writing, so no build can lock it either.
	unopenable
	// unsafe_name: the name is taken by something that is no regular file.
	unsafe_name
}

// tool_cache_lock_ending ends the name of every build lock (see `tool_cache_lock_path_for_uid`).
const tool_cache_lock_ending = '.toolcache.lock'

// tool_cache_wipe_gone is the code of the error `open_tool_cache_wipe_dir` returns when nothing is
// at the path: another wipe can remove a folder first, which is no failure of this one.
const tool_cache_wipe_gone = 2

// ToolCacheLockName is the name of a build lock, `.<tool>.<uid>.toolcache.lock`, taken apart.
struct ToolCacheLockName {
	tool string
	uid  u32
}

// tool_cache_lock_name names the build lock of `tool` for the user `uid`, as
// `tool_cache_lock_path_for_uid` does for every id below 2^31. Above that, `os.getuid()` can give
// a build a negative number, and the tool cache then refuses the lock file it has just made as
// not the user's (see `ensure_tool_cache_lock_file`), so no build holds a lock there.
fn tool_cache_lock_name(tool string, uid u32) string {
	return '.${tool}.${uid}${tool_cache_lock_ending}'
}

// tool_cache_user_id reads `text` as a user id the way a build writes one into a name: decimal
// digits with no leading zero, within 32 bits. Anything else is none.
fn tool_cache_user_id(text string) ?u32 {
	if text.len == 0 || text.len > 10 || !text.bytes().all(it.is_digit()) {
		return none
	}
	value := text.u64()
	if value > 0xffffffff || value.str() != text {
		return none
	}
	return u32(value)
}

// tool_cache_lock_name_parts takes the name of a build lock apart, or returns none when `name` is
// not shaped like one: a dot, a tool, a dot, a user id, and `.toolcache.lock`.
fn tool_cache_lock_name_parts(name string) ?ToolCacheLockName {
	if !name.starts_with('.') || !name.ends_with(tool_cache_lock_ending) {
		return none
	}
	if name.len < tool_cache_lock_ending.len + 4 {
		return none
	}
	middle := name[1..name.len - tool_cache_lock_ending.len]
	at := middle.last_index('.') or { return none }
	if at == 0 {
		return none
	}
	uid := tool_cache_user_id(middle[at + 1..]) or { return none }
	return ToolCacheLockName{
		tool: middle[..at]
		uid:  uid
	}
}

// ToolCacheEntryName is the name of a cache entry, `<tool>-<64 hex>[.<uid>]`, taken apart.
struct ToolCacheEntryName {
	tool    string
	has_uid bool
	uid     u32
}

// tool_cache_entry_name_parts takes the name of a cache entry apart, or returns none when `name`
// is not shaped like one. A tool name may itself contain dashes, so every dash is tried as the
// separator. The user id is that of the account whose build made the entry (see
// `tool_cache_entry_dir_for_uid`); legacy entries and their sidecar files carry none.
fn tool_cache_entry_name_parts(name string) ?ToolCacheEntryName {
	for i, c in name {
		if c == `-` && i > 0 && is_cache_artifact_of(name, name[..i]) {
			suffix := name[i + 65..]
			if suffix.len > 1 {
				if uid := tool_cache_user_id(suffix[1..]) {
					return ToolCacheEntryName{
						tool:    name[..i]
						has_uid: true
						uid:     uid
					}
				}
			}
			return ToolCacheEntryName{
				tool: name[..i]
			}
		}
	}
	return none
}

// tool_cache_wipe_is_superuser reports whether the wipe runs as the POSIX superuser, which may
// remove any user's files and take any user's build lock.
fn tool_cache_wipe_is_superuser(me u32) bool {
	$if windows {
		return false
	} $else {
		return me == 0
	}
}

// tool_cache_wipe_takes_posix is the one rule for what a wipe removes outside Windows. The user
// `me` runs the wipe, in a folder that `folder_owner` owns with the permission bits `folder_mode`,
// and `owner` owns the item. In a folder that only `me` can write to, everything is the wipe's,
// whoever owns it, as it is for the superuser anywhere. In a folder that another account can write
// to, only what `me` owns is: what others own there is guarded by their build locks, which this
// wipe cannot take.
fn tool_cache_wipe_takes_posix(me u32, folder_owner u32, folder_mode u32, owner u32) bool {
	if me == 0 || owner == me {
		return true
	}
	return folder_owner == me && folder_mode & 0o022 == 0
}

// tool_cache_wipe_takes reports whether the wipe removes an item (see
// `tool_cache_wipe_takes_posix`). On Windows privacy is decided by ACLs, which the tool cache
// checks when it picks a folder, so everything in a tool cache folder is the wipe's.
fn tool_cache_wipe_takes(me u32, folder_owner u32, folder_mode u32, owner u32) bool {
	$if windows {
		return true
	} $else {
		return tool_cache_wipe_takes_posix(me, folder_owner, folder_mode, owner)
	}
}

// tool_cache_lock_is_repairable reports whether a lock file that cannot be opened may be set to
// mode 0600, a change that follows links. It must be a regular file. Outside Windows, no other
// account may be able to swap it meanwhile: its folder belongs to this user or to root, and others
// cannot write to it, or only with the sticky bit; and the file belongs to this user, or the wipe
// runs as the superuser, and has no other name, so that a hard link cannot carry the change to a
// file outside the wipe.
fn tool_cache_lock_is_repairable(me u32, folder_owner u32, folder_mode u32, item ToolCacheWipeItem) bool {
	if item.kind != .file {
		return false
	}
	$if windows {
		return true
	} $else {
		if folder_owner !in [me, u32(0)] || (folder_mode & 0o022 != 0 && folder_mode & 0o1000 == 0) {
			return false
		}
		return (item.owner == me || me == 0) && item.links == 1
	}
}

// ToolCacheWipeLocks tracks, for one tool cache folder, which build locks the wipe holds, which
// tools another process has locked, and which lock files cannot be used. The lock files are
// persistent mutexes (see `tool_cache_lock`): they are opened, created only by `claim`, and stay,
// so a builder blocked on one while the wipe holds it simply proceeds once the wipe lets go. Each
// lock file is opened once at most while the wipe holds it, because closing any descriptor of a
// file gives up the POSIX lock on it.
struct ToolCacheWipeLocks {
	// dry is set for a dry run, which probes build locks, keeps none of them, and changes nothing.
	dry          bool
	root         string
	me           u32
	folder_owner u32
	folder_mode  u32
mut:
	states     map[string]ToolCacheWipeLockState
	held       []ToolCacheWipeLock
	held_names []string
	busy       []string
	unlockable []string
	unopenable []string
	// created lists the lock files that `claim` made.
	created []string
	// vanishing lists the names a dry run would remove, which a real run no longer finds.
	vanishing   map[string]bool
	first_error string
}

// probe takes the build lock of `tool` for the user `uid`, unless it was probed already, and
// returns what came of it. No lock file means no build is at work: every build creates its lock
// file before it touches an entry. A lock file that cannot be opened for writing is held by no
// build either; it is set to mode 0600 and tried again when that is safe (see
// `tool_cache_lock_is_repairable`), and otherwise left as it is.
fn (mut locks ToolCacheWipeLocks) probe(dir ToolCacheWipeDir, tool string, uid u32) ToolCacheWipeLockState {
	name := tool_cache_lock_name(tool, uid)
	if name in locks.states {
		return locks.states[name]
	}
	mut state := ToolCacheWipeLockState.absent
	if !(locks.dry && locks.vanishing[name]) {
		mut taken := dir.try_lock(name)
		state = taken.state
		if state == .unopenable
			&& tool_cache_lock_is_repairable(locks.me, locks.folder_owner, locks.folder_mode, dir.item(name)) {
			if locks.dry {
				// A dry run reports what a real run does, and that one repairs the file and locks it.
				state = .held
			} else {
				dir.repair_lock_file(name) or {
					if locks.first_error == '' {
						locks.first_error = 'cannot repair the lock file ${os.join_path(locks.root, name)}: ${err.msg()}'
					}
				}
				taken = dir.try_lock(name)
				state = taken.state
			}
		}
		if taken.state == .held {
			if locks.dry {
				// A dry run must not keep a builder waiting.
				taken.release()
			} else {
				locks.held << taken
				locks.held_names << name
			}
		}
	}
	match state {
		.busy {
			if tool !in locks.busy {
				locks.busy << tool
			}
		}
		.unopenable {
			if tool !in locks.unopenable {
				locks.unopenable << tool
			}
		}
		.unsafe_name {
			if tool !in locks.unlockable {
				locks.unlockable << tool
			}
		}
		else {}
	}
	locks.states[name] = state
	return state
}

// claim takes the build lock of `tool` for the user `uid` before an entry of that tool is removed,
// and reports whether the entry may go. When the tool has no lock file, one is made first, so that
// a builder either finds the lock taken or waits on it. A tool whose lock is taken by another
// process, cannot be made, or is no regular file keeps its entries. A dry run makes nothing and
// assumes that a real run can make the file.
fn (mut locks ToolCacheWipeLocks) claim(dir ToolCacheWipeDir, tool string, uid u32) bool {
	name := tool_cache_lock_name(tool, uid)
	mut state := locks.probe(dir, tool, uid)
	if state == .absent {
		if locks.dry {
			return true
		}
		if dir.create_lock_file(name, uid) {
			locks.created << name
		}
		locks.states.delete(name)
		state = locks.probe(dir, tool, uid)
		if state == .absent {
			// It was not made and is still not there, so nothing guards the entries of the tool.
			if tool !in locks.unlockable {
				locks.unlockable << tool
			}
			locks.states[name] = .unsafe_name
			return false
		}
	}
	return state == .held
}

// release gives every held build lock back, after the wipe of the folder is done.
fn (mut locks ToolCacheWipeLocks) release() {
	for mut held in locks.held {
		held.release()
	}
	locks.held = []
	locks.held_names = []
}

// wipe_except_text names the tools whose entries a wipe kept, and why, and counts the kept files and
// folders of other users, for its summary line.
fn wipe_except_text(busy []string, unlockable []string, unopenable []string, others int) string {
	mut text := ''
	if busy.len > 0 {
		text += ', except for the tools another process has locked right now: ${busy.join(', ')}'
	}
	if unlockable.len > 0 {
		text += ', except for the tools whose lock file could not be made or is no regular file: ${unlockable.join(', ')}'
	}
	if unopenable.len > 0 {
		text += ', except for the tools whose lock file cannot be opened and was left as it is: ${unopenable.join(', ')}'
	}
	if others > 0 {
		text += ', except for ${others} files and folders of other users'
	}
	return text
}

// ToolCacheWipeRun carries what one run of `v wipe-cache` knows: the tool cache folders it was
// told about, and which of them it has wiped. A wipe protects the builds in those folders only: the
// one `VTOOLS_CACHE_DIR` names, the default ones, and the `.untrusted-*` folders next to them (see
// `tool_cache_wipe_targets`). A tool cache folder that only another process's `VTOOLS_CACHE_DIR`
// names is ordinary content of the folder it lies in.
struct ToolCacheWipeRun {
	// targets holds the real paths of the tool cache folders this wipe knows.
	targets []string
mut:
	// wiped holds the real paths of the tool cache folders this run has wiped, so that a folder
	// which another wiped folder holds is wiped once.
	wiped []string
	// gone holds the real paths of the tool cache folders this run removed, or would remove in a
	// dry run, so that the summary of the folder around them does not list them as kept.
	gone []string
}

// new_tool_cache_wipe_run starts a run with the tool cache folders the environment names now.
fn new_tool_cache_wipe_run() ToolCacheWipeRun {
	return ToolCacheWipeRun{
		targets: tool_cache_wipe_targets().map(tool_cache_real_path(it))
	}
}

// tool_cache_real_path resolves the links in `path` as far as `path` exists, and keeps the rest as
// it is written, so that a tool cache folder which is not there yet has one spelling too.
fn tool_cache_real_path(path string) string {
	mut existing := os.abs_path(path)
	mut rest := []string{}
	for !os.exists(existing) && !os.is_link(existing) {
		parent := os.dir(existing)
		if parent == existing || parent == '' || parent == '.' {
			break
		}
		rest.insert(0, os.file_name(existing))
		existing = parent
	}
	mut real := os.real_path(existing)
	for part in rest {
		real = os.join_path(real, part)
	}
	return real
}

// path_holds reports whether `path` is `folder` or lies inside it.
fn path_holds(folder string, path string) bool {
	return path == folder || path.starts_with(folder.trim_right(os.path_separator) + os.path_separator)
}

// roots_below lists where the tool cache folders this wipe knows lie in the folder at `path`, each
// as a path below it, and '' for the folder itself. The parts of such a path are spelled as the
// folders on disk list them, since the wipe finds them by the names a listing gives.
fn (run ToolCacheWipeRun) roots_below(path string) []string {
	real := tool_cache_real_path(path)
	mut below := []string{}
	for target in run.targets {
		if !path_holds(real, target) {
			continue
		}
		relative := tool_cache_spelled_as_listed(real, target[real.len..].trim_left(os.path_separator))
		if relative !in below {
			below << relative
		}
	}
	return below
}

// tool_cache_spelled_as_listed respells `relative`, a path below `folder`, the way the folders on
// disk list its parts, as far as they exist. On a filesystem that ignores case, a path reaches a
// folder in any spelling, and the wipe must not depend on `os.real_path` giving the listed one (on
// Windows it does). A part is respelled only when a listed name differs from it in case alone and
// is the same file by device and inode; V reports no inode on Windows, where the name decides.
fn tool_cache_spelled_as_listed(folder string, relative string) string {
	if relative == '' {
		return relative
	}
	mut current := folder
	mut parts := []string{}
	for part in relative.split(os.path_separator) {
		mut spelled := part
		names := os.ls(current) or { []string{} }
		if part !in names {
			if wanted := os.lstat(os.join_path(current, part)) {
				for name in names {
					if name.to_lower() != part.to_lower() {
						continue
					}
					listed := os.lstat(os.join_path(current, name)) or { continue }
					if listed.dev == wanted.dev && listed.inode == wanted.inode {
						spelled = name
						break
					}
				}
			}
		}
		parts << spelled
		current = os.join_path(current, spelled)
	}
	return parts.join(os.path_separator)
}

// tool_cache_wipe_label names a tool cache folder in the output, as the wipe always has: one whose
// name holds `.untrusted-` is a folder V moved aside.
fn tool_cache_wipe_label(path string) string {
	if os.file_name(path).contains('.untrusted-') {
		return 'Moved-aside V tool cache'
	}
	return 'V tool cache'
}

// is_moved_aside_tool_cache_name reports whether `name` is named as V names a tool cache folder it
// moves aside: `<folder>.untrusted-<seconds>-<pid>` (see `prepare_default_tool_cache_root`).
fn is_moved_aside_tool_cache_name(name string) bool {
	at := name.last_index('.untrusted-') or { return false }
	parts := name[at + '.untrusted-'.len..].split('-')
	if at == 0 || parts.len != 2 {
		return false
	}
	for part in parts {
		if part.len == 0 || !part.bytes().all(it.is_digit()) {
			return false
		}
	}
	return true
}

// is_moved_aside_tool_cache_root reports whether `path` is a tool cache folder that V moved aside,
// in which nothing is built any more: its name has the shape `is_moved_aside_tool_cache_name`
// checks, and it neither is nor holds one of the folders V builds tools in (`VTOOLS_CACHE_DIR` or a
// default folder), which a user may have named or placed that way.
fn is_moved_aside_tool_cache_root(path string) bool {
	if !is_moved_aside_tool_cache_name(os.file_name(path)) {
		return false
	}
	mut used := tool_cache_default_candidates()
	if configured := os.getenv_opt(tool_cache_dir_env) {
		used << configured.trim_space()
	}
	real := tool_cache_real_path(path)
	return !used.any(it != '' && path_holds(real, tool_cache_real_path(it)))
}

// wipe_item removes `name` from the open folder `dir`, spelled `path`. When `name` is one of the
// tool cache folders this wipe knows, listed in `below` as paths under `path`, it is wiped as one
// instead; when it leads to one, it is cleared around that one. It reports whether `name` stays.
fn (mut run ToolCacheWipeRun) wipe_item(dir ToolCacheWipeDir, path string, name string, below []string, opts WipeCacheOptions, mut details []string, mut lines []string) !bool {
	full := os.join_path(path, name)
	mut is_root := name in below
	if !is_root {
		// A folder that a build moved aside from a tool cache folder this wipe knows is one too,
		// whenever it appears: it is known by its name beside that folder, not by a listing made
		// when the wipe started (see `append_untrusted_tool_cache_siblings`).
		if at := name.index('.untrusted-') {
			is_root = at > 0 && name[..at] in below
		}
	}
	prefix := name + os.path_separator
	deeper := below.filter(it.starts_with(prefix)).map(it[prefix.len..])
	if (is_root || deeper.len > 0) && dir.item(name).kind == .folder {
		child := dir.open_folder(name) or {
			if err.code() == tool_cache_wipe_gone {
				// Another wipe removed it since it was looked at, so nothing of it stays.
				return false
			}
			return error('could not open ${full}: ${err.msg()}')
		}
		defer {
			child.close()
		}
		if is_root {
			run.wipe_open_tool_cache_directory(child, full, opts, mut lines)!
		} else {
			run.clear_around_tool_roots(child, full, deeper, opts, mut details, mut lines)!
			if !opts.dry_run {
				// It goes when nothing in it stayed; a folder that is not empty refuses.
				dir.remove_folder(name) or {}
			}
		}
		return dir.item(name).kind != .gone
	}
	if opts.dry_run {
		details << '  would remove ${full}'
		return false
	}
	details << '  removing ${full}'
	dir.remove_tree(name) or { return error('${path}${os.path_separator}${err.msg()}') }
	return false
}

// clear_around_tool_roots removes all that is in the open folder `dir`, spelled `path`, except the
// tool cache folders this wipe knows, at `below` under `path`, and the folders that lead to them.
// Those tool cache folders are wiped as such. They are known by their paths, not by what is on disk
// when the wipe starts, so a build that creates one of them meanwhile finds it left alone. Every
// item is attempted, and the first failure is reported afterwards.
fn (mut run ToolCacheWipeRun) clear_around_tool_roots(dir ToolCacheWipeDir, path string, below []string, opts WipeCacheOptions, mut details []string, mut lines []string) ! {
	names := dir.names() or { return error('could not list ${path}: ${err.msg()}') }
	mut first_error := ''
	for name in names {
		run.wipe_item(dir, path, name, below, opts, mut details, mut lines) or {
			if first_error == '' {
				first_error = err.msg()
			}
			true
		}
	}
	if first_error != '' {
		return error(first_error)
	}
}

// wipe_tool_cache_directory wipes the tool cache folder at `path` (see
// `wipe_open_tool_cache_directory`). What is at `path` but is no real folder, a link for example,
// is removed as it is, like any other cache folder.
fn (mut run ToolCacheWipeRun) wipe_tool_cache_directory(path string, opts WipeCacheOptions, mut lines []string) ! {
	label := tool_cache_wipe_label(path)
	if os.is_link(path) || !os.is_dir(path) {
		return wipe_cache_directory(path, label, false, opts, mut lines)
	}
	dir := open_tool_cache_wipe_dir(path) or {
		if err.code() == tool_cache_wipe_gone {
			// Another wipe removed it meanwhile, so there is nothing left to do.
			return
		}
		return error('v wipe-cache: could not open ${label} folder ${path}: ${err.msg()}')
	}
	defer {
		dir.close()
	}
	run.wipe_open_tool_cache_directory(dir, path, opts, mut lines)!
}

// wipe_open_tool_cache_directory wipes the tool cache folder that `dir` holds open, spelled `path`,
// entry by entry, and only through `dir`: a build that moves the folder aside meanwhile leaves the
// wipe in the folder it opened, and the new folder at `path` untouched.
//
// The entries of a tool go only while the wipe holds that tool's build lock. A tool another process
// has locked (a build in flight) keeps its entries, and the output names it: on Windows the compiler
// still has its staging folder open and removal fails with a sharing violation; on POSIX removal
// would pull the build's files out from under it. The locks stay held until the folder is done, so
// a build that starts meanwhile waits. A missing lock file is made first. Lock files of any user
// stay, since unlinking one would let two builds lock two different files; one that cannot be
// opened is set to mode 0600 instead. What another account owns stays in a folder that account can
// write to (see `tool_cache_wipe_takes`). Run as the superuser, the wipe takes each entry under the
// lock of the user who built it.
//
// A folder V moved aside (see `is_moved_aside_tool_cache_root`) is one nobody builds in any more: it
// is wiped the same way, and once nothing in it is kept, its lock files and the folder go too. Any
// other tool cache folder is removed only when the wipe leaves it empty, which lock files prevent.
//
// Every item is attempted, and the first failure is reported afterwards, as `os.rmdir_all` does.
fn (mut run ToolCacheWipeRun) wipe_open_tool_cache_directory(dir ToolCacheWipeDir, path string, opts WipeCacheOptions, mut lines []string) ! {
	real := tool_cache_real_path(path)
	if real in run.wiped {
		return
	}
	run.wiped << real
	label := tool_cache_wipe_label(path)
	moved_aside := is_moved_aside_tool_cache_root(path)
	me := u32(os.getuid())
	superuser := tool_cache_wipe_is_superuser(me)
	folder_owner, folder_mode := dir.owner_and_mode()
	below := run.roots_below(path).filter(it != '')
	mut locks := ToolCacheWipeLocks{
		dry:          opts.dry_run
		root:         path
		me:           me
		folder_owner: folder_owner
		folder_mode:  folder_mode
	}
	defer {
		locks.release()
	}
	names := dir.names() or {
		return error('v wipe-cache: could not list ${label} folder ${path}: ${err.msg()}')
	}
	mut details := []string{}
	mut kept := []string{}
	mut lock_files := []string{}
	mut entries := []string{}
	mut first_error := ''
	mut others := 0
	// First everything that is no tool entry. What is in the way of a lock file, a link for example,
	// is gone before the entries are claimed, whatever the order of the names.
	for name in names {
		full := os.join_path(path, name)
		item := dir.item(name)
		if item.kind == .gone {
			// Another wipe removed it since the listing.
			continue
		}
		if lock_name := tool_cache_lock_name_parts(name) {
			if item.kind == .file {
				if lock_name.uid == me || superuser {
					locks.probe(dir, lock_name.tool, lock_name.uid)
				}
				lock_files << name
				continue
			}
		}
		if !tool_cache_wipe_takes(me, folder_owner, folder_mode, item.owner) {
			others++
			kept << full
			continue
		}
		if _ := tool_cache_entry_name_parts(name) {
			entries << name
			continue
		}
		stays := run.wipe_item(dir, path, name, below, opts, mut details, mut lines) or {
			if first_error == '' {
				first_error = err.msg()
			}
			true
		}
		if stays {
			kept << full
		} else if opts.dry_run {
			locks.vanishing[name] = true
		}
	}
	// Then the tool entries, each under the build lock that guards it.
	for name in entries {
		full := os.join_path(path, name)
		item := dir.item(name)
		if item.kind == .gone {
			continue
		}
		entry := tool_cache_entry_name_parts(name) or { continue }
		mut guard := me
		mut guarded := true
		if superuser {
			guard = if entry.has_uid { entry.uid } else { item.owner }
		} else if entry.has_uid && entry.uid != me {
			// No build of this user works in an entry of another user, and no lock of this user
			// guards it: in a folder that is the wipe's alone it goes like any other content.
			guarded = false
		}
		if guarded && !locks.claim(dir, entry.tool, guard) {
			kept << full
			continue
		}
		stays := run.wipe_item(dir, path, name, below, opts, mut details, mut lines) or {
			if first_error == '' {
				first_error = err.msg()
			}
			true
		}
		if stays {
			kept << full
		}
	}
	nothing_kept := kept.len == 0 && locks.busy.len == 0 && locks.unlockable.len == 0
		&& locks.unopenable.len == 0
	for name in locks.created {
		if name !in lock_files {
			lock_files << name
		}
	}
	// The folder itself goes when the wipe leaves it empty.
	mut folder_goes := nothing_kept
	$if windows {
		if moved_aside && nothing_kept && !opts.dry_run {
			// The locks are let go before their files are removed: Windows may keep the name of a
			// removed file until its last handle is closed (`DeleteFile` is documented to delete
			// on close), and the folder would then not come empty in this run.
			locks.release()
		}
	}
	for name in lock_files {
		full := os.join_path(path, name)
		if moved_aside && nothing_kept && locks.states[name] == .held {
			// Nobody builds in a folder V moved aside, so its lock files go, while the wipe holds them.
			if opts.dry_run {
				details << '  would remove ${full}'
			} else {
				if name !in locks.created {
					// One that this wipe made a moment ago was never there for the user.
					details << '  removing ${full}'
				}
				dir.remove_file(name) or {}
			}
			continue
		}
		folder_goes = false
		details << if opts.dry_run { '  would keep ${full}' } else { '  keeping ${full}' }
	}
	for path_kept in kept {
		details << if opts.dry_run { '  would keep ${path_kept}' } else { '  keeping ${path_kept}' }
	}
	mut busy := locks.busy.clone()
	busy.sort()
	mut unlockable := locks.unlockable.clone()
	unlockable.sort()
	mut unopenable := locks.unopenable.clone()
	unopenable.sort()
	except := wipe_except_text(busy, unlockable, unopenable, others)
	if opts.dry_run {
		// Same order as `wipe_cache_directory`: the summary first, then what it consists of.
		lines << 'Would wipe ${label} folder ${path}${except}.'
		if opts.verbose {
			lines << details
		}
		if folder_goes {
			run.gone << real
		}
		return
	}
	if opts.verbose {
		lines << details
	}
	if first_error == '' {
		first_error = locks.first_error
	}
	if first_error != '' {
		if first_error.starts_with('v wipe-cache:') {
			return error(first_error)
		}
		return error('v wipe-cache: could not remove ${label} folder ${path}: ${first_error}')
	}
	if !dir.is_at(path) {
		lines << '${label} folder ${path} was moved aside by a build during the wipe. The wipe finished in the folder it had opened${except}; the new folder at ${path} was not touched.'
		return
	}
	// `os.rmdir` refuses a folder that is not empty, so a build that put something in meanwhile
	// keeps it. The removal is by path, since nothing removes a folder by the handle on it: a build
	// that moves the folder aside between the check above and the removal, and makes a new one, loses
	// that new folder while it is still empty, and fails to make its lock file there once.
	if folder_goes {
		remaining := dir.names() or { [''] }
		if remaining.len == 0 {
			os.rmdir(path) or {}
		}
		if !os.exists(path) && !os.is_link(path) {
			run.gone << real
		}
	}
	lines << '${label} folder ${path} was wiped${except}.'
}

// remove_cache_folder removes the folder at `path` and all that is in it, as `os.rmdir_all` does,
// except that whatever is gone already counts as removed: another wipe can be at work in the same
// folder. Every item is attempted, and the first failure is returned afterwards.
fn remove_cache_folder(path string) ! {
	dir := open_tool_cache_wipe_dir(path) or {
		if err.code() == tool_cache_wipe_gone {
			return
		}
		return err
	}
	defer {
		dir.close()
	}
	mut first_error := ''
	names := dir.names() or {
		first_error = err.msg()
		[]string{}
	}
	for name in names {
		dir.remove_tree(name) or {
			if first_error == '' {
				first_error = os.join_path(path, err.msg())
			}
		}
	}
	if first_error != '' {
		return error(first_error)
	}
	// Another wipe can remove the folder first, and make it anew at once. So a removal that fails is
	// tried again: it then finds the folder gone, or removes the new and empty one, as that wipe
	// would. Only a folder that is still the one opened here, and still there, is a failure.
	mut problem := ''
	for _ in 0 .. 3 {
		problem = ''
		os.rmdir(path) or { problem = err.msg() }
		if problem == '' || !dir.is_at(path) || (!os.exists(path) && !os.is_link(path)) {
			return
		}
	}
	return error(problem)
}

fn wipe_cache_directory(path string, label string, recreate bool, opts WipeCacheOptions, mut lines []string) ! {
	if opts.dry_run {
		if os.is_link(path) {
			lines << 'Would wipe ${label} folder ${path} (symbolic link).'
		} else if os.is_dir(path) {
			lines << 'Would wipe ${label} folder ${path}.'
			if opts.verbose {
				mut entries := os.ls(path) or {
					// A folder that a wipe removed meanwhile has nothing left to list.
					if os.exists(path) || os.is_link(path) {
						return error('v wipe-cache: could not list ${label} folder ${path}: ${err}')
					}
					[]string{}
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
			// What another wipe removed first counts as removed, here and below.
			if os.exists(path) || os.is_link(path) {
				return error('v wipe-cache: could not remove ${label} folder ${path}: ${err}')
			}
		}
	} else if os.is_dir(path) {
		if opts.verbose {
			mut entries := os.ls(path) or {
				// A folder that another wipe removed meanwhile has nothing left to list.
				if os.exists(path) || os.is_link(path) {
					return error('v wipe-cache: could not list ${label} folder ${path}: ${err}')
				}
				[]string{}
			}
			entries.sort()
			for entry in entries {
				lines << '  removing ${os.join_path(path, entry)}'
			}
		}
		remove_cache_folder(path) or {
			return error('v wipe-cache: could not remove ${label} folder ${path}: ${err}')
		}
	} else if os.exists(path) {
		os.rm(path) or {
			if os.exists(path) || os.is_link(path) {
				return error('v wipe-cache: could not remove ${label} folder ${path}: ${err}')
			}
		}
	} else if opts.verbose {
		lines << '  ${path} does not exist'
	}
	if recreate {
		// Another wipe can remove or make the folder in the same moment, which fails one attempt and
		// not the next. A folder that cannot be made fails every time.
		mut problem := ''
		for _ in 0 .. 5 {
			problem = ''
			os.mkdir_all(path) or { problem = err.msg() }
			if problem == '' || os.is_dir(path) {
				problem = ''
				break
			}
		}
		if problem != '' {
			return error('v wipe-cache: could not create ${label} folder ${path}: ${problem}')
		}
	}
	lines << '${label} folder ${path} was wiped.'
}

// wipe_folder_around_tool_roots wipes a cache folder that can hold tool cache folders this wipe
// knows, such as `<VTMP>/tools` in `VTMP`. Each of those is wiped as a tool cache folder, so the
// entries and lock files a running build holds survive, and everything else in the folder is
// removed around them. A folder that holds none of them is wiped whole, like any cache folder. A
// failure does not stop the rest: everything is attempted, and the first failure is reported.
fn (mut run ToolCacheWipeRun) wipe_folder_around_tool_roots(path string, label string, recreate bool, opts WipeCacheOptions, mut lines []string) ! {
	if os.is_link(path) || !os.is_dir(path) {
		return wipe_cache_directory(path, label, recreate, opts, mut lines)
	}
	below := run.roots_below(path)
	if below.len == 0 {
		return wipe_cache_directory(path, label, recreate, opts, mut lines)
	}
	dir := open_tool_cache_wipe_dir(path) or {
		if err.code() == tool_cache_wipe_gone {
			// Another wipe removed it meanwhile; what is left to do is to make it again.
			return wipe_cache_directory(path, label, recreate, opts, mut lines)
		}
		return error('v wipe-cache: could not open ${label} folder ${path}: ${err.msg()}')
	}
	defer {
		dir.close()
	}
	if '' in below {
		// The folder is a tool cache folder itself, which holds all there is to wipe.
		run.wipe_open_tool_cache_directory(dir, path, opts, mut lines)!
		lines << if opts.dry_run {
			'Would wipe ${label} folder ${path}, which is a tool cache folder too.'
		} else {
			'${label} folder ${path} was wiped, as the tool cache folder it also is.'
		}
		return
	}
	mut details := []string{}
	mut problem := ''
	run.clear_around_tool_roots(dir, path, below, opts, mut details, mut lines) or {
		problem = err.msg()
	}
	kept := below.map(os.join_path(path, it)).filter((os.exists(it) || os.is_link(it))
		&& tool_cache_real_path(it) !in run.gone)
	kept_text := kept.join(', ')
	if opts.dry_run {
		lines << if kept.len > 0 {
			'Would wipe ${label} folder ${path}, except ${kept_text}.'
		} else {
			'Would wipe ${label} folder ${path}.'
		}
		if opts.verbose {
			lines << details
		}
		return
	}
	if opts.verbose {
		lines << details
	}
	if problem != '' {
		if problem.starts_with('v wipe-cache:') {
			return error(problem)
		}
		return error('v wipe-cache: could not remove ${label} folder ${path}: ${problem}')
	}
	if kept.len > 0 {
		lines << '${label} folder ${path} was wiped, except the tool cache folders in it: ${kept_text}.'
		return
	}
	if !recreate {
		// Emptied in place. `os.rmdir` refuses when a build put a tool cache folder in meanwhile.
		os.rmdir(path) or {}
	}
	lines << '${label} folder ${path} was wiped.'
}

// wipe_caches returns cleanup output, stopping at the first filesystem error.
fn wipe_caches(opts WipeCacheOptions) ![]string {
	mut lines := []string{}
	mut run := new_tool_cache_wipe_run()
	if opts.vcache {
		cache_path := os.getenv_opt('VCACHE') or { os.join_path(os.vmodules_dir(), '.cache') }
		run.wipe_folder_around_tool_roots(cache_path, 'V cache', true, opts, mut lines)!
	}
	mut tool_lines := []string{}
	if opts.tools {
		if opts.dry_run || opts.verbose {
			tool_lines << 'Tool cache candidates:'
			tool_lines << inspect_tool_cache_candidates()
		}
		mut wiped_any_tool_cache := false
		for target in tool_cache_wipe_targets() {
			if os.exists(target) || os.is_link(target) {
				wiped_any_tool_cache = true
				run.wipe_tool_cache_directory(target, opts, mut tool_lines)!
			} else if opts.verbose {
				tool_lines << '${tool_cache_wipe_label(target)} folder ${target} does not exist, skipping.'
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
		run.wipe_folder_around_tool_roots(os.vtmp_dir(), 'V tmp.c and tests', true, opts, mut
			lines)!
	}
	if opts.v3cache {
		vtmp_path := os.vtmp_dir()
		configured_v3 := os.getenv('V3CACHE').trim_space()
		if configured_v3 != '' && os.abs_path(configured_v3) != os.abs_path(vtmp_path) {
			run.wipe_folder_around_tool_roots(configured_v3, 'V3 cache', true, opts, mut lines)!
		} else if !opts.vtmp {
			mut v3_dirs := []string{}
			if os.is_dir(vtmp_path) {
				mut entries := os.ls(vtmp_path) or {
					// A folder that another wipe removed meanwhile has nothing left to list.
					if os.exists(vtmp_path) || os.is_link(vtmp_path) {
						return error('v wipe-cache: could not list V3 cache folders in ${vtmp_path}: ${err}')
					}
					[]string{}
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
					run.wipe_folder_around_tool_roots(dir, 'V3 cache', false, opts, mut lines)!
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

fn run_wipe_cache(args []string, command_index int) {
	prefix_args := if command_index > 0 { args[..command_index] } else { []string{} }
	command_args := if command_index >= 0 && command_index + 1 < args.len {
		args[command_index + 1..]
	} else {
		[]string{}
	}
	opts := parse_wipe_cache_options(prefix_args, command_args) or {
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
