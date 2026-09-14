// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import crypto.sha256
import crypto.rand as crypto_rand
import os
import time

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

const tool_cache_manifest_version = 'v3-tool-cache-1'
const tool_cache_disable_env = 'VTOOLS_NO_CACHE'
const tool_cache_dir_env = 'VTOOLS_CACHE_DIR'
const tool_cache_verbose_env = 'VTOOLS_CACHE_VERBOSE'
const tool_cache_field_separator = '\x1f'
// marks a cached binary that had to be renamed out of the way instead of being overwritten,
// because Windows was still executing it; `prune_stale_tool_binaries` collects these later
const tool_cache_replaced_marker = '.replaced.'

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

// tool_cache_is_disabled reports whether the user asked for the tools to always be rebuilt.
fn tool_cache_is_disabled() bool {
	return os.getenv(tool_cache_disable_env).trim_space().to_lower() in ['1', 'true', 'yes', 'on']
}

// tool_cache_is_verbose reports whether every cache decision should be explained on stderr.
fn tool_cache_is_verbose() bool {
	return os.getenv(tool_cache_verbose_env).trim_space().to_lower() in ['1', 'true', 'yes', 'on']
}

// tool_cache_dir returns a writable directory for the compiled tools, creating it on demand.
// It deliberately lives outside of the V source tree, next to the other V caches.
fn tool_cache_dir() ?string {
	mut candidates := []string{}
	if custom := os.getenv_opt(tool_cache_dir_env) {
		if custom.trim_space() != '' {
			candidates << custom.trim_space()
		}
	}
	candidates << os.join_path(os.cache_dir(), 'v', 'tools')
	candidates << os.join_path(os.vtmp_dir(), 'tools')
	for candidate in candidates {
		if !os.is_dir(candidate) {
			os.mkdir_all(candidate) or { continue }
		}
		if os.is_dir(candidate) && directory_is_writable(candidate)
			&& tool_cache_root_can_stage(candidate) {
			return os.real_path(candidate)
		}
	}
	return none
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

// module_root_stamp digests the names of the modules that live directly inside a directory.
// It is only used to revalidate a recorded build failure, so paying for an `is_dir` call per
// entry is fine here: it is what makes "the module you could not import came back" visible.
fn module_root_stamp(path string) string {
	entries := os.ls(path) or { return 'missing' }
	mut names := entries.filter(os.is_dir(os.join_path(path, it)))
	names.sort()
	return sha256.hexhash(names.join('\n'))
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
const ambient_build_variables = ['CFLAGS', 'LDFLAGS', 'VCOVDIR', 'PKG_CONFIG_PATH',
	'PKG_CONFIG_LIBDIR', 'PKG_CONFIG_SYSROOT_DIR', 'VMODULES']

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

// tool_cache_entry locates the cache slot for a tool, or none when no cache is usable.
fn tool_cache_entry(vexe string, vroot string, tool_name string, tool_source string, build_args []string) ?ToolCacheEntry {
	directory := tool_cache_dir()?
	key := tool_cache_key(vexe, tool_name, tool_key_sources(tool_source), build_args)
	entry_dir := os.join_path(directory, '${tool_name}-${key}')
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

// tool_cache_is_fresh reports whether `entry.binary` can be executed as is, i.e. whether
// every source file it was built from is still exactly the way it was at build time.
fn tool_cache_is_fresh(entry ToolCacheEntry) bool {
	return tool_cache_stale_reason(entry) == ''
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
			'm' { module_root_stamp(path) }
			'b' { binary_identity(path) }
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

// record_unbuildable_tool remembers a failed build together with the inputs that caused it,
// so that the failing compilation is not repeated on every invocation, while fixing any of
// those inputs still makes it be retried.
fn encode_unbuildable_tool_manifest(entry ToolCacheEntry, dumped string, started i64, details string) string {
	source_files := (os.read_file(dumped) or { '' }).split_into_lines().filter(it != '')
	mut manifest := encode_tool_cache_manifest(source_files, started)
	// The compiler reports what it read even for most failures, but the one failure it
	// cannot describe that way is an import it could not resolve at all, because the module
	// has no files to stamp. Recording the module roots as well is what makes the tool be
	// retried as soon as a missing module reappears.
	search_roots := module_search_roots(entry.vroot, entry.build_args)
	mut module_roots := map[string]bool{}
	mut module_source_dirs := map[string]bool{}
	for root in search_roots {
		module_roots[root] = true
	}
	for module_name in unresolved_import_modules(details) {
		// A module-root stamp records its *direct* child directories only, so `db.sqlite`
		// could be removed and restored without `vlib` itself changing. Stamp each ancestor,
		// then stamp the final module directory's V source names: an existing empty directory
		// becomes usable when its first `.v` file appears. Every root the compiler would have
		// searched counts because the module may reappear in any of them.
		for search_root in search_roots {
			mut path := search_root
			parts := module_name.split('.')
			for index, part in parts {
				if part.trim_space() == '' {
					break
				}
				path = os.join_path(path, part)
				if index == parts.len - 1 {
					module_source_dirs[path] = true
				} else {
					module_roots[path] = true
				}
			}
		}
	}
	mut sorted_roots := module_roots.keys()
	sorted_roots.sort()
	for root in sorted_roots {
		manifest += 'm${tool_cache_field_separator}${root}${tool_cache_field_separator}${module_root_stamp(root)}\n'
	}
	mut sorted_source_dirs := module_source_dirs.keys()
	sorted_source_dirs.sort()
	for directory in sorted_source_dirs {
		manifest += 'd${tool_cache_field_separator}${directory}${tool_cache_field_separator}${dir_stamp(directory)}\n'
	}
	return manifest
}

fn record_unbuildable_tool(entry ToolCacheEntry, dumped string, started i64, details string) {
	manifest := encode_unbuildable_tool_manifest(entry, dumped, started, details)
	os.write_file(entry.unbuildable_manifest, manifest) or { return }
	os.write_file(entry.unbuildable, details) or {}
}

// encode_tool_cache_manifest turns the `-dump-files` source closure into the manifest that
// `tool_cache_is_fresh` revalidates. Besides the files themselves it records the directories
// that contain them, so that a source file added to an imported module is detected too.
fn encode_tool_cache_manifest(source_files []string, started i64) string {
	mut lines := []string{}
	lines << tool_cache_manifest_version
	lines << 'started${tool_cache_field_separator}${started}'
	mut directories := map[string]bool{}
	for file in source_files {
		if file == '' {
			continue
		}
		lines << 'f${tool_cache_field_separator}${file}${tool_cache_field_separator}${file_stamp(file)}'
		directories[os.dir(file)] = true
	}
	mut names := directories.keys()
	names.sort()
	for name in names {
		lines << 'd${tool_cache_field_separator}${name}${tool_cache_field_separator}${dir_stamp(name)}'
	}
	return lines.join('\n') + '\n'
}

// publish_atomically moves `staged` over `destination` in a single step, so that a
// concurrently running V process either sees the previous file or the new one, but never
// a half written one, and never has the executable it is starting truncated underneath it.
// `destination` regularly already exists: a tool whose own sources and compiler are
// unchanged keeps its cache key, so a rebuild triggered by an imported vlib module lands
// in the very same slot. Replacing it is what `replace_file_atomically` is for; a plain
// `os.rename` would fail there on Windows.
fn publish_atomically(staged string, destination string) bool {
	if !replace_file_atomically(staged, destination) {
		os.rm(staged) or {}
		return false
	}
	return true
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

// prune_stale_tool_binaries drops the cache entries of previous builds of the same tool.
// Unlinking an executable that another process is currently running is safe on POSIX: that
// process keeps its own already opened image.
fn prune_stale_tool_binaries(entry ToolCacheEntry) {
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
			stale_entry := open_tool_cache_entry_dir(path) or { continue }
			stale_entry.remove_all_contents()
			stale_entry.close()
			os.rmdir(path) or {}
		}
	}
}

// create_tool_cache_stage_dir makes an unpredictable, private directory for compiler output
// on the cache filesystem, so completed files can be installed with an atomic rename.
fn create_tool_cache_stage_dir(parent string) !string {
	if !os.is_dir(parent) {
		return error('cannot find a staging directory for the tool cache')
	}
	for _ in 0 .. 16 {
		token := crypto_rand.bytes(16)!.hex()
		path := os.join_path(parent, '.v-toolcache-stage-${os.getuid()}-${token}')
		os.mkdir(path, mode: 0o700) or { continue }
		return path
	}
	return error('cannot create a private temporary directory for the tool cache')
}

// build_tool_binary compiles the tool into its cache slot and records its source closure.
// It returns the compiler output when the build failed.
fn build_tool_binary(vexe string, entry ToolCacheEntry) !string {
	cache_entry := open_tool_cache_entry_dir(entry.dir)!
	defer {
		cache_entry.close()
	}
	stage_parent := cache_entry.stage_parent(entry.dir)!
	stage_dir := create_tool_cache_stage_dir(stage_parent)!
	stage_entry := open_tool_cache_entry_dir(stage_dir)!
	defer {
		stage_entry.close()
		os.rmdir_all(stage_dir) or {}
	}
	staged := os.join_path(stage_dir, os.file_name(entry.binary))
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
		details := if output.trim_space() != '' { output } else { failure }
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
	manifest := encode_tool_cache_manifest(source_files, started) + 'b${tool_cache_field_separator}${entry.binary}${tool_cache_field_separator}${binary_identity(staged)}\n'
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
	prune_stale_tool_binaries(entry)
	return ''
}

// exec_cached_tool hands the process over to the cached tool binary. On POSIX this is a
// plain `execvp`, so the tool inherits the standard streams untouched and its exit status
// and signals are reported to the caller's shell verbatim.
@[noreturn]
fn exec_cached_tool(executable string, tool_args []string) {
	$if windows {
		mut process := os.new_process(executable)
		process.set_args(tool_args)
		process.wait()
		code := process.code
		process.close()
		exit(code)
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
