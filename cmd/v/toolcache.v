// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import crypto.sha256
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
	binary string // the cached executable
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
		if os.is_dir(candidate) {
			return candidate
		}
	}
	return none
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
	attributes := os.stat(path) or { return 'missing' }
	return '${attributes.mtime}${tool_cache_field_separator}${attributes.size}'
}

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

// last_modified returns the modification time of a path, or the largest possible time when
// it cannot be determined, so that an unreadable input always counts as changed.
fn last_modified(path string) i64 {
	attributes := os.stat(path) or { return max_i64 }
	return attributes.mtime
}

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
	for source in tool_sources {
		parts << 'src${tool_cache_field_separator}${source}${tool_cache_field_separator}${file_stamp(source)}'
	}
	return sha256.hexhash(parts.join('\n'))
}

// tool_cache_entry locates the cache slot for a tool, or none when no cache is usable.
fn tool_cache_entry(vexe string, vroot string, tool_name string, tool_source string, build_args []string) ?ToolCacheEntry {
	directory := tool_cache_dir()?
	key := tool_cache_key(vexe, tool_name, tool_key_sources(tool_source), build_args)
	binary := os.join_path(directory, '${tool_name}-${key}${tool_exe_suffix()}')
	return ToolCacheEntry{
		name:                 tool_name
		source:               tool_source
		vroot:                vroot
		binary:               binary
		manifest:             binary + '.inputs'
		unbuildable:          binary + '.unbuildable'
		unbuildable_manifest: binary + '.unbuildable.inputs'
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
			else { file_stamp(path) }
		}
		if current != recorded {
			return '`${path}` changed (recorded `${recorded}`, found `${current}`)'
		}
		if kind != 'f' {
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

// record_unbuildable_tool remembers a failed build together with the inputs that caused it,
// so that the failing compilation is not repeated on every invocation, while fixing any of
// those inputs still makes it be retried.
fn record_unbuildable_tool(entry ToolCacheEntry, dumped string, started i64, details string) {
	source_files := (os.read_file(dumped) or { '' }).split_into_lines().filter(it != '')
	mut manifest := encode_tool_cache_manifest(source_files, started)
	// The compiler reports what it read even for most failures, but the one failure it
	// cannot describe that way is an import it could not resolve at all, because the module
	// has no files to stamp. Recording the module roots as well is what makes the tool be
	// retried as soon as a missing module reappears.
	for root in [os.join_path(entry.vroot, 'vlib'), os.join_path(entry.vroot, 'vlib', 'v')] {
		manifest += 'm${tool_cache_field_separator}${root}${tool_cache_field_separator}${module_root_stamp(root)}\n'
	}
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

// is_cache_artifact_of reports whether `name` belongs to a cache entry of `tool`. The tool
// name alone is not enough to tell them apart, because one tool's name can be a prefix of
// another's (`v bug` and `v bug-report`), so the content address itself has to be matched.
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
	directory := os.dir(entry.binary)
	keep := os.file_name(entry.binary)
	entries := os.ls(directory) or { return }
	for name in entries {
		if !is_cache_artifact_of(name, entry.name) {
			continue
		}
		if name.contains(tool_cache_replaced_marker) {
			// A binary that Windows would not let us overwrite while it was still being
			// executed was renamed aside instead. It shares the current entry's prefix, so
			// the `keep` test below would spare it forever; once the process that held it
			// has exited this is the only thing that ever deletes it.
			os.rm(os.join_path(directory, name)) or {}
			continue
		}
		if name.starts_with(keep) {
			continue
		}
		os.rm(os.join_path(directory, name)) or {}
	}
}

// build_tool_binary compiles the tool into its cache slot and records its source closure.
// It returns the compiler output when the build failed.
fn build_tool_binary(vexe string, entry ToolCacheEntry) !string {
	unique := '${os.getpid()}'
	staged := '${entry.binary}.staged.${unique}'
	dumped := '${entry.binary}.sources.${unique}'
	defer {
		os.rm(dumped) or {}
	}
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
	process.wait()
	code := process.code
	output := process.stdout_slurp()
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
			record_unbuildable_tool(entry, dumped, started, details)
		}
		return error(details)
	}
	source_files := os.read_file(dumped) or { '' }.split_into_lines()
	manifest := encode_tool_cache_manifest(source_files, started)
	// Publish the executable before its manifest. A lookup requires both, so the worst that
	// a concurrent reader can observe is "binary present, manifest not updated yet", which
	// is simply treated as a miss. The reverse order could hand out a stale binary.
	if !publish_atomically(staged, entry.binary) {
		return error('cannot install the compiled `${entry.name}` into `${entry.binary}`')
	}
	staged_manifest := '${entry.manifest}.staged.${unique}'
	os.write_file(staged_manifest, manifest) or {
		return error('cannot record the inputs of `${entry.name}`: ${err}')
	}
	publish_atomically(staged_manifest, entry.manifest)
	os.rm(entry.unbuildable) or {}
	os.rm(entry.unbuildable_manifest) or {}
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
