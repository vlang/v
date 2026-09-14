// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import time

// `vtimeout` is used as the probe tool: it is small enough to compile quickly, and
// `v timeout <seconds> <command>` both succeeds and terminates on its own.
const probe_tool = 'vtimeout'

fn toolcache_test_dir(name string) string {
	directory := os.join_path(os.vtmp_dir(), 'v_toolcache_test', '${name}_${os.getpid()}')
	os.rmdir_all(directory) or {}
	os.mkdir_all(directory) or { panic(err) }
	return directory
}

// cached_entry_dirs returns the content addressed entry directories of `tool`. Each holds one
// build: the executable, named exactly like the tool, plus that build's bookkeeping files.
fn cached_entry_dirs(directory string, tool string) []string {
	names := os.ls(directory) or { return [] }
	mut found := names.filter(is_cache_artifact_of(it, tool)
		&& os.is_dir(os.join_path(directory, it)))
	found.sort()
	return found
}

// This is the property the whole cache exists for: `v test-cleancode` invokes `v fmt` and
// `v vet` once per repository file, so the tool must be compiled once and then reused.
fn test_a_second_invocation_of_a_tool_reuses_the_compiled_binary() {
	vexe := @VEXE
	if !os.is_executable(vexe) {
		eprintln('> skipping, no V executable at `${vexe}`')
		return
	}
	cache := toolcache_test_dir('reuse')
	defer {
		os.rmdir_all(cache) or {}
	}
	os.setenv(tool_cache_dir_env, cache, true)
	defer {
		os.unsetenv(tool_cache_dir_env)
	}
	command := '${os.quoted_path(vexe)} timeout 60 ${os.quoted_path(vexe)} version'

	first := os.execute(command)
	assert first.exit_code == 0, first.output
	entries := cached_entry_dirs(cache, probe_tool)
	assert entries.len == 1, 'expected a single cached `${probe_tool}`, got ${entries}'
	entry_dir := os.join_path(cache, entries[0])
	binary := os.join_path(entry_dir, probe_tool + tool_exe_suffix())
	// The hash lives in the directory name, never in the executable's: tools read
	// `os.file_name(os.executable())` and use it as their own name and cache directory.
	assert os.file_name(binary) == probe_tool + tool_exe_suffix()
	assert os.is_executable(binary), 'expected an executable at `${binary}`'
	before := os.stat(binary)!

	// Every rebuild prunes the earlier entries of the same tool, so a decoy shaped like the
	// entry of an older build, still being there afterwards, proves that the second
	// invocation did not recompile anything.
	decoy := os.join_path(cache, '${probe_tool}-' + 'a'.repeat(64))
	os.mkdir_all(decoy)!
	os.write_file(os.join_path(decoy, probe_tool), 'decoy')!

	probe := ToolCacheEntry{
		name:     probe_tool
		dir:      entry_dir
		binary:   binary
		manifest: os.join_path(entry_dir, 'inputs')
	}
	reason := tool_cache_stale_reason(probe)
	assert reason == '', 'the just built tool is already considered stale: ${reason}'

	second := os.execute(command)
	assert second.exit_code == 0, second.output
	assert os.exists(decoy), 'the tool was recompiled although none of its inputs changed'
	after := os.stat(binary)!
	assert after.inode == before.inode, 'the cached binary was replaced'
	assert after.mtime == before.mtime
	assert after.size == before.size
	remaining := cached_entry_dirs(cache, probe_tool).filter(it != os.file_name(decoy))
	assert remaining == entries, 'a second entry appeared for the same tool: ${remaining}'
}

fn test_the_cache_key_covers_the_tool_sources_the_compiler_and_the_flags() {
	directory := toolcache_test_dir('key')
	defer {
		os.rmdir_all(directory) or {}
	}
	vexe := os.join_path(directory, 'v')
	os.write_file(vexe, 'a pretend V executable')!
	source := os.join_path(directory, 'vdemo.v')
	os.write_file(source, 'module main\n')!

	baseline := tool_cache_key(vexe, 'vdemo', [source], ['-cc', 'clang'])
	assert baseline == tool_cache_key(vexe, 'vdemo', [source], ['-cc', 'clang']), 'the key must be stable'

	assert tool_cache_key(vexe, 'vdemo', [source], ['-cc', 'gcc']) != baseline, 'build flags must be part of the key'
	assert tool_cache_key(vexe, 'vother', [source], ['-cc', 'clang']) != baseline, 'the tool name must be part of the key'

	// Touching the tool's own source must never let the previous binary be reused.
	os.write_file(source, 'module main\n\nfn main() {}\n')!
	assert tool_cache_key(vexe, 'vdemo', [source], ['-cc', 'clang']) != baseline, 'the tool source must be part of the key'

	// Neither may a rebuilt V executable, which is what makes `v self` pick up new tools.
	os.write_file(source, 'module main\n')!
	assert tool_cache_key(vexe, 'vdemo', [source], ['-cc', 'clang']) == baseline
	os.write_file(vexe, 'a different pretend V executable')!
	assert tool_cache_key(vexe, 'vdemo', [source], ['-cc', 'clang']) != baseline, 'the V executable must be part of the key'
}

fn test_the_cache_key_covers_vflags() {
	directory := toolcache_test_dir('vflags')
	defer {
		os.rmdir_all(directory) or {}
	}
	vexe := os.join_path(directory, 'v')
	os.write_file(vexe, 'a pretend V executable')!
	source := os.join_path(directory, 'vdemo.v')
	os.write_file(source, 'module main\n')!
	previous := os.getenv('VFLAGS')
	defer {
		os.setenv('VFLAGS', previous, true)
	}

	os.setenv('VFLAGS', '-cc clang', true)
	baseline := tool_cache_key(vexe, 'vdemo', [source], [])
	os.setenv('VFLAGS', '-cc clang -prod', true)
	assert tool_cache_key(vexe, 'vdemo', [source], []) != baseline, 'VFLAGS must be part of the key'
}

// fresh_cache_fixture builds a cache entry whose recorded inputs are all still valid.
fn fresh_cache_fixture(name string) (ToolCacheEntry, string) {
	directory := toolcache_test_dir(name)
	binary := os.join_path(directory, 'vdemo-0123456789')
	os.write_file(binary, 'a pretend compiled tool') or { panic(err) }
	os.chmod(binary, 0o755) or { panic(err) }
	module_dir := os.join_path(directory, 'demomod')
	os.mkdir_all(module_dir) or { panic(err) }
	dependency := os.join_path(module_dir, 'demomod.v')
	os.write_file(dependency, 'module demomod\n') or { panic(err) }
	// The build has to look strictly newer than the sources it consumed, exactly like a
	// real build does. `os.stat` only reports whole seconds, so wait for the next one.
	time.sleep(1100 * time.millisecond)
	entry := ToolCacheEntry{
		name:        'vdemo'
		binary:      binary
		manifest:    binary + '.inputs'
		unbuildable: binary + '.unbuildable'
	}
	manifest := encode_tool_cache_manifest([dependency], time.now().unix())
	os.write_file(entry.manifest, manifest) or { panic(err) }
	assert tool_cache_is_fresh(entry), 'the fixture itself must start out fresh'
	return entry, dependency
}

fn test_an_edited_dependency_invalidates_the_cache() {
	entry, dependency := fresh_cache_fixture('edited')
	defer {
		os.rmdir_all(os.dir(entry.binary)) or {}
	}
	os.write_file(dependency, 'module demomod\n\npub fn added() {}\n')!
	assert !tool_cache_is_fresh(entry), 'editing an imported module must force a rebuild'
}

fn test_a_removed_dependency_invalidates_the_cache() {
	entry, dependency := fresh_cache_fixture('removed')
	defer {
		os.rmdir_all(os.dir(entry.binary)) or {}
	}
	os.rm(dependency)!
	assert !tool_cache_is_fresh(entry), 'deleting an imported source must force a rebuild'
}

// A file added to an already imported module changes what the tool compiles to, even though
// none of the recorded files changed. The recorded directory timestamps catch that.
fn test_a_new_file_in_an_imported_module_invalidates_the_cache() {
	entry, dependency := fresh_cache_fixture('added')
	defer {
		os.rmdir_all(os.dir(entry.binary)) or {}
	}
	os.write_file(os.join_path(os.dir(dependency), 'extra.v'), 'module demomod\n')!
	assert !tool_cache_is_fresh(entry), 'a new module source must force a rebuild'
}

fn test_a_missing_or_damaged_manifest_invalidates_the_cache() {
	entry, _ := fresh_cache_fixture('manifest')
	defer {
		os.rmdir_all(os.dir(entry.binary)) or {}
	}
	os.write_file(entry.manifest, 'some-other-format\n')!
	assert !tool_cache_is_fresh(entry), 'an unreadable manifest must force a rebuild'
	os.rm(entry.manifest)!
	assert !tool_cache_is_fresh(entry), 'a missing manifest must force a rebuild'
}

fn test_a_missing_binary_invalidates_the_cache() {
	entry, _ := fresh_cache_fixture('binary')
	defer {
		os.rmdir_all(os.dir(entry.binary)) or {}
	}
	os.rm(entry.binary)!
	assert !tool_cache_is_fresh(entry), 'a missing binary must force a rebuild'
}

// Sources that are rewritten within the same second as the build started keep their
// modification time and size, so the recorded build time is what makes them detectable.
fn test_a_dependency_touched_during_the_build_invalidates_the_cache() {
	directory := toolcache_test_dir('racing')
	defer {
		os.rmdir_all(directory) or {}
	}
	binary := os.join_path(directory, 'vdemo-0123456789')
	os.write_file(binary, 'a pretend compiled tool')!
	os.chmod(binary, 0o755)!
	dependency := os.join_path(directory, 'demomod.v')
	os.write_file(dependency, 'module demomod\n')!
	entry := ToolCacheEntry{
		name:     'vdemo'
		binary:   binary
		manifest: binary + '.inputs'
	}
	os.write_file(entry.manifest, encode_tool_cache_manifest([dependency], time.now().unix()))!
	assert !tool_cache_is_fresh(entry)
}

fn test_tool_key_sources_ignores_tests_of_directory_tools() {
	directory := toolcache_test_dir('sources')
	defer {
		os.rmdir_all(directory) or {}
	}
	tool := os.join_path(directory, 'vdemo')
	os.mkdir_all(os.join_path(tool, 'tests'))!
	os.write_file(os.join_path(tool, 'vdemo.v'), 'module main\n')!
	os.write_file(os.join_path(tool, 'helper.v'), 'module main\n')!
	os.write_file(os.join_path(tool, 'vdemo_test.v'), 'module main\n')!
	os.write_file(os.join_path(tool, 'tests', 'fixture.v'), 'module main\n')!

	sources := tool_key_sources(tool).map(os.file_name(it))
	assert sources == ['helper.v', 'vdemo.v'], 'got ${sources}'

	single := os.join_path(directory, 'vsingle.v')
	os.write_file(single, 'module main\n')!
	assert tool_key_sources(single) == [single]
}

// One tool's name can be a prefix of another's, so pruning the entries of `v bug` must not
// throw away the ones belonging to `v bug-report`.
fn test_cache_artifacts_of_one_tool_are_not_mistaken_for_anothers() {
	key := 'a'.repeat(64)
	assert is_cache_artifact_of('vbug-${key}', 'vbug')
	assert is_cache_artifact_of('vbug-${key}.inputs', 'vbug')
	assert is_cache_artifact_of('vbug-${key}.exe', 'vbug')
	assert is_cache_artifact_of('vbug-${key}.staged.42', 'vbug')
	assert !is_cache_artifact_of('vbug-report-${key}', 'vbug')
	assert is_cache_artifact_of('vbug-report-${key}', 'vbug-report')
	assert !is_cache_artifact_of('vbug-notahash', 'vbug')
	assert !is_cache_artifact_of('vbugged-${key}', 'vbug')
}

// A tool that failed to build must not be pinned to the compatibility compiler forever: a
// broken intermediate state of any vlib module it imports has to be retried once it is fixed.
fn test_a_recorded_build_failure_is_retried_once_its_inputs_change() {
	directory := toolcache_test_dir('failure')
	defer {
		os.rmdir_all(directory) or {}
	}
	dependency := os.join_path(directory, 'demomod.v')
	os.write_file(dependency, 'module demomod\n')!
	dumped := os.join_path(directory, 'sources')
	os.write_file(dumped, dependency + '\n')!
	entry := unbuildable_fixture(directory, 'a')
	// The recorded build has to look strictly newer than the sources it read.
	time.sleep(1100 * time.millisecond)
	record_unbuildable_tool(entry, dumped, time.now().unix(), 'the compiler said no')

	recorded := unbuildable_tool_failure(entry) or { '' }
	assert recorded == 'the compiler said no', 'a just recorded failure must be reused'

	os.write_file(dependency, 'module demomod\n\npub fn fixed() {}\n')!
	if again := unbuildable_tool_failure(entry) {
		assert false, 'fixing an input must make the build be retried, got `${again}`'
	}
}

// When an import cannot be resolved at all there is no source closure to record, so the
// module roots are what has to invalidate the recorded failure.
fn test_a_recorded_build_failure_is_retried_once_a_missing_module_reappears() {
	directory := toolcache_test_dir('missingmodule')
	defer {
		os.rmdir_all(directory) or {}
	}
	os.mkdir_all(os.join_path(directory, 'vlib', 'v', 'pref'))!
	dumped := os.join_path(directory, 'sources')
	os.write_file(dumped, '')!
	entry := unbuildable_fixture(directory, 'b')
	record_unbuildable_tool(entry, dumped, time.now().unix(), 'cannot import module "v.ast"')

	recorded := unbuildable_tool_failure(entry) or { '' }
	assert recorded.contains('v.ast'), 'a just recorded failure must be reused'

	os.mkdir_all(os.join_path(directory, 'vlib', 'v', 'ast'))!
	if again := unbuildable_tool_failure(entry) {
		assert false, 'restoring the missing module must make the build be retried, got `${again}`'
	}
}

fn unbuildable_fixture(directory string, filler string) ToolCacheEntry {
	binary := os.join_path(directory, 'vdemo-' + filler.repeat(64))
	return ToolCacheEntry{
		name:                 'vdemo'
		vroot:                directory
		binary:               binary
		manifest:             binary + '.inputs'
		unbuildable:          binary + '.unbuildable'
		unbuildable_manifest: binary + '.unbuildable.inputs'
	}
}

// A build that failed because of the environment rather than the tool's own sources must not
// be recorded: nothing in the manifest describes the toolchain, so the failure would be
// replayed on every later invocation even after the environment was repaired.
fn test_only_source_dependent_build_failures_are_cached() {
	// V frontend diagnostics: these are explained by the sources, and are cacheable.
	assert build_failure_is_source_dependent('/v/cmd/tools/vdemo.v:3:1: error: unknown type `Foo`')
	assert build_failure_is_source_dependent('/v/cmd/tools/vdemo.v:2:1: builder error: cannot import module "missing" (not found)')
	assert build_failure_is_source_dependent('warming up\n/v/x.v:12:5: error: undefined ident: `y`\n')

	// C stage failures: the whole V source closure was already accepted by the time these
	// happen, so they say nothing about it.
	assert !build_failure_is_source_dependent('C compilation failed:\nsrc.c:12:3: error: expected `;`')
	assert !build_failure_is_source_dependent('C compilation error (from clang):\nsrc.c:9:1: error: x')
	assert !build_failure_is_source_dependent('failed parallel C compilation\nsrc.c:4:4: error: x')
	assert !build_failure_is_source_dependent('failed to link after parallel C compilation')
	assert !build_failure_is_source_dependent('builder error:\n==================\nC library `sqlite3` was not found while linking the generated program.')

	// No diagnostic at all: a child that never started, or one the OOM killer cut short.
	assert !build_failure_is_source_dependent('')
	assert !build_failure_is_source_dependent('   \n  ')
	assert !build_failure_is_source_dependent('signal 9')
	assert !build_failure_is_source_dependent('exec failed: No such file or directory')
	// Prose that merely contains the word, with no source position, is not a diagnostic.
	assert !build_failure_is_source_dependent('clang: error: no such file or directory')
}

fn test_v_diagnostic_lines_require_a_source_position() {
	assert line_is_v_diagnostic('/a/b.v:1:2: error: boom')
	assert line_is_v_diagnostic('/a/b.v:10:20: builder error: boom')
	assert !line_is_v_diagnostic('error: boom')
	assert !line_is_v_diagnostic('/a/b.v: error: boom')
	assert !line_is_v_diagnostic('/a/b.v:x:y: error: boom')
	assert !line_is_v_diagnostic('cc: error: boom')
	assert !line_is_v_diagnostic('just a line')
}

// A rebuild triggered by a changed vlib dependency keeps the tool's cache key, so it has to
// be installable over the binary already sitting in that slot.
fn test_publishing_replaces_an_existing_binary() {
	directory := toolcache_test_dir('replace')
	defer {
		os.rmdir_all(directory) or {}
	}
	destination := os.join_path(directory, 'vdemo-' + 'a'.repeat(64))
	staged := destination + '.staged.1'
	os.write_file(destination, 'old') or { panic(err) }
	os.write_file(staged, 'new') or { panic(err) }

	assert publish_atomically(staged, destination)
	assert os.read_file(destination) or { '' } == 'new'
	assert !os.exists(staged)
}

// The binaries that Windows only let us rename out of the way share the current entry's
// prefix, so pruning has to collect them explicitly or they would accumulate forever.
fn test_pruning_collects_binaries_that_were_replaced_while_in_use() {
	directory := toolcache_test_dir('prune_replaced')
	defer {
		os.rmdir_all(directory) or {}
	}
	key := 'a'.repeat(64)
	entry_dir := os.join_path(directory, 'vdemo-${key}')
	os.mkdir_all(entry_dir) or { panic(err) }
	binary := os.join_path(entry_dir, 'vdemo')
	entry := ToolCacheEntry{
		name:     'vdemo'
		vroot:    directory
		dir:      entry_dir
		binary:   binary
		manifest: os.join_path(entry_dir, 'inputs')
	}
	os.write_file(binary, 'current') or { panic(err) }
	os.write_file(entry.manifest, 'manifest') or { panic(err) }
	displaced := '${binary}${tool_cache_replaced_marker}4242'
	os.write_file(displaced, 'previous') or { panic(err) }
	stale := os.join_path(directory, 'vdemo-' + 'b'.repeat(64))
	os.mkdir_all(stale) or { panic(err) }
	os.write_file(os.join_path(stale, 'vdemo'), 'older build') or { panic(err) }

	prune_stale_tool_binaries(entry)

	assert os.exists(binary), 'the current binary must be kept'
	assert os.exists(entry.manifest), 'the current manifest must be kept'
	assert !os.exists(displaced), 'a binary replaced while in use must be collected'
	assert !os.exists(stale), 'a previous build must be collected'
}

// Before cache entries became directories, each binary and its manifest lived directly in
// the cache root. Directory-only pruning silently left those legacy files behind forever.
fn test_pruning_collects_legacy_flat_cache_files() {
	directory := toolcache_test_dir('prune_legacy')
	defer {
		os.rmdir_all(directory) or {}
	}
	current_key := 'a'.repeat(64)
	entry_dir := os.join_path(directory, 'vdemo-${current_key}')
	os.mkdir_all(entry_dir)!
	entry := ToolCacheEntry{
		name: 'vdemo'
		dir:  entry_dir
	}
	stale_key := 'b'.repeat(64)
	legacy_binary := os.join_path(directory, 'vdemo-${stale_key}')
	legacy_manifest := legacy_binary + '.inputs'
	current_legacy_sidecar := os.join_path(directory, 'vdemo-${current_key}.inputs')
	os.write_file(legacy_binary, 'old binary')!
	os.write_file(legacy_manifest, 'old manifest')!
	os.write_file(current_legacy_sidecar, 'old current-key manifest')!

	prune_stale_tool_binaries(entry)

	assert os.is_dir(entry_dir), 'the current entry directory must be kept'
	assert !os.exists(legacy_binary), 'a legacy cached executable must be collected'
	assert !os.exists(legacy_manifest), 'a legacy manifest must be collected'
	assert !os.exists(current_legacy_sidecar), 'only the exact current directory may be kept'
}

// A cache-shaped symlink can point outside the cache. Recursive removal must never traverse
// it, even when the cache directory is shared with an untrusted account.
fn test_pruning_unlinks_stale_symlinks_without_touching_their_targets() {
	directory := toolcache_test_dir('prune_symlink')
	defer {
		os.rmdir_all(directory) or {}
	}
	entry_dir := os.join_path(directory, 'vdemo-' + 'a'.repeat(64))
	os.mkdir_all(entry_dir)!
	entry := ToolCacheEntry{
		name: 'vdemo'
		dir:  entry_dir
	}
	target := os.join_path(directory, 'outside')
	os.mkdir_all(target)!
	payload := os.join_path(target, 'must-survive')
	os.write_file(payload, 'safe')!
	stale_link := os.join_path(directory, 'vdemo-' + 'b'.repeat(64))
	os.symlink(target, stale_link) or {
		eprintln('> skipping symlink pruning test: ${err}')
		return
	}

	prune_stale_tool_binaries(entry)

	assert !os.is_link(stale_link), 'the stale cache symlink must be unlinked'
	assert os.read_file(payload)! == 'safe', 'the symlink target must not be traversed'
}

// The current entry path is predictable from the tool inputs. A shared cache must reject a
// link planted there instead of staging the executable and manifest through it.
fn test_building_rejects_a_symlinked_current_entry() {
	directory := toolcache_test_dir('current_symlink')
	defer {
		os.rmdir_all(directory) or {}
	}
	target := os.join_path(directory, 'outside')
	os.mkdir_all(target)!
	entry_dir := os.join_path(directory, 'vdemo-' + 'a'.repeat(64))
	os.symlink(target, entry_dir) or {
		eprintln('> skipping current-entry symlink test: ${err}')
		return
	}
	entry := ToolCacheEntry{
		name:     'vdemo'
		dir:      entry_dir
		binary:   os.join_path(entry_dir, 'vdemo' + tool_exe_suffix())
		manifest: os.join_path(entry_dir, 'inputs')
	}

	build_tool_binary('', entry) or {
		assert err.msg().contains('symbolic link'), 'unexpected error: ${err}'
		assert os.ls(target)! == [], 'nothing may be written through the cache-entry link'
		return
	}
	assert false, 'a symlinked current cache entry must be rejected'
}

// The entry pathname can change after it has been checked when the cache root is shared.
// POSIX publication has to stay bound to the opened directory, while Windows keeps the
// pathname from being renamed until the entry handle is closed.
fn test_pinned_entry_publication_cannot_be_redirected() {
	directory := toolcache_test_dir('pinned_entry')
	defer {
		os.rmdir_all(directory) or {}
	}
	entry_dir := os.join_path(directory, 'vdemo-' + 'a'.repeat(64))
	os.mkdir(entry_dir)!
	cache_entry := open_tool_cache_entry_dir(entry_dir)!
	defer {
		cache_entry.close()
	}

	original_dir := entry_dir + '.original'
	mut pathname_was_replaced := true
	os.rename(entry_dir, original_dir) or { pathname_was_replaced = false }
	outside := os.join_path(directory, 'outside')
	os.mkdir(outside)!
	if pathname_was_replaced {
		os.symlink(outside, entry_dir)!
	}
	staged := os.join_path(directory, 'staged-inputs')
	os.write_file(staged, 'the manifest')!

	assert cache_entry.publish(staged, 'inputs')
	assert os.ls(outside)! == [], 'publication must not follow a replacement symlink'
	installed := if pathname_was_replaced {
		os.join_path(original_dir, 'inputs')
	} else {
		os.join_path(entry_dir, 'inputs')
	}
	assert os.read_file(installed)! == 'the manifest'
}

// A single-file tool can pull in a sibling asset with `$embed_file`, whose bytes end up
// inside the compiled binary. `cmd/tools/vgret.v` does exactly that with its
// `vgret.defaults.toml`. The asset is not a V source, so only the compiler can report it,
// and without it the cache would keep running a binary built from an older configuration.
fn test_an_embedded_asset_is_recorded_and_invalidates_the_cache() {
	vexe := @VEXE
	if !os.is_executable(vexe) {
		eprintln('> skipping, no V executable at `${vexe}`')
		return
	}
	directory := toolcache_test_dir('embedded_asset')
	defer {
		os.rmdir_all(directory) or {}
	}
	asset := os.join_path(directory, 'asset.txt')
	os.write_file(asset, 'first revision')!
	source := os.join_path(directory, 'vdemo.v')
	os.write_file(source, "module main\n\nconst asset = \$embed_file('asset.txt')\n\nfn main() {\n\tprintln(asset.len)\n}\n")!

	dumped := os.join_path(directory, 'sources.txt')
	binary := os.join_path(directory, 'vdemo-' + 'a'.repeat(64))
	build :=
		os.execute('${os.quoted_path(vexe)} -dump-files ${os.quoted_path(dumped)} -o ${os.quoted_path(binary)} ${os.quoted_path(source)}')
	assert build.exit_code == 0, build.output

	recorded := (os.read_file(dumped) or { '' }).split_into_lines().filter(it != '')
	assert os.real_path(asset) in recorded, 'the embedded asset must be a recorded build input, got ${recorded.filter(!it.contains('/vlib/'))}'

	// The recorded closure is what the manifest revalidates, so editing the asset has to
	// make the cached binary stale even though every `.v` file is untouched.
	entry := ToolCacheEntry{
		name:     'vdemo'
		binary:   binary
		manifest: binary + '.inputs'
	}
	time.sleep(1100 * time.millisecond)
	os.write_file(entry.manifest, encode_tool_cache_manifest(recorded, time.now().unix()))!
	assert tool_cache_is_fresh(entry), 'the freshly built tool must not start out stale'

	os.write_file(asset, 'second revision, a different length')!
	assert !tool_cache_is_fresh(entry), 'editing an embedded asset must force a rebuild'
}

// `CFLAGS`, `LDFLAGS` and `VCOVDIR` are read straight from the environment by the child
// compiler, not passed through `build_args`, so nothing else in the cache identity records
// them. A tool built with a sanitizer or an extra macro must not be handed back once that
// setting is gone.
fn test_the_cache_key_covers_ambient_native_flags() {
	directory := toolcache_test_dir('ambient')
	defer {
		os.rmdir_all(directory) or {}
	}
	vexe := os.join_path(directory, 'v')
	os.write_file(vexe, 'a pretend V executable')!
	source := os.join_path(directory, 'vdemo.v')
	os.write_file(source, 'module main\n')!

	mut previous := map[string]string{}
	for name in ambient_build_variables {
		previous[name] = os.getenv(name)
		os.setenv(name, '', true)
	}
	defer {
		for name, value in previous {
			os.setenv(name, value, true)
		}
	}

	baseline := tool_cache_key(vexe, 'vdemo', [source], [])
	for name in ambient_build_variables {
		os.setenv(name, '-DSOMETHING=1', true)
		assert tool_cache_key(vexe, 'vdemo', [source], []) != baseline, '${name} must be part of the key'
		os.setenv(name, '', true)
		assert tool_cache_key(vexe, 'vdemo', [source], []) == baseline, 'clearing ${name} must restore the key'
	}
}

// A tool that compiles local C sources or includes local headers has to be rebuilt when any
// of them changes. `vlib/db/sqlite/sqlite.c.v` is the real case: it builds
// `thirdparty/sqlite/sqlite3.c` and includes `sqlite3.h`, neither of which is a V source.
fn test_a_native_input_is_recorded_and_invalidates_the_cache() {
	vexe := @VEXE
	if !os.is_executable(vexe) {
		eprintln('> skipping, no V executable at `${vexe}`')
		return
	}
	directory := toolcache_test_dir('native_input')
	defer {
		os.rmdir_all(directory) or {}
	}
	os.write_file(os.join_path(directory, 'v.mod'), 'Module { name: "nativedemo" }\n')!
	header := os.join_path(directory, 'helper.h')
	os.write_file(header, '#ifndef HELPER_H\n#define HELPER_H\nint native_double(int x);\n#endif\n')!
	native_source := os.join_path(directory, 'helper.c')
	os.write_file(native_source, '#include "helper.h"\nint native_double(int x) { return x * 2; }\n')!
	source := os.join_path(directory, 'vdemo.v')
	os.write_file(source, 'module main\n\n#flag -I@VMODROOT\n#flag @VMODROOT/helper.c\n#include "helper.h"\n\nfn C.native_double(int) int\n\nfn main() {\n\tprintln(C.native_double(21))\n}\n')!

	dumped := os.join_path(directory, 'sources.txt')
	binary := os.join_path(directory, 'vdemo-' + 'a'.repeat(64))
	build :=
		os.execute('${os.quoted_path(vexe)} -dump-files ${os.quoted_path(dumped)} -o ${os.quoted_path(binary)} ${os.quoted_path(source)}')
	assert build.exit_code == 0, build.output

	recorded := (os.read_file(dumped) or { '' }).split_into_lines().filter(it != '')
	assert os.real_path(native_source) in recorded, 'the compiled C source must be recorded, got ${recorded}'
	assert os.real_path(header) in recorded, 'the included C header must be recorded, got ${recorded}'

	entry := ToolCacheEntry{
		name:     'vdemo'
		binary:   binary
		manifest: binary + '.inputs'
	}
	time.sleep(1100 * time.millisecond)
	os.write_file(entry.manifest, encode_tool_cache_manifest(recorded, time.now().unix()))!
	assert tool_cache_is_fresh(entry), 'the freshly built tool must not start out stale'

	os.write_file(native_source, '#include "helper.h"\nint native_double(int x) { return x * 3; }\n')!
	assert !tool_cache_is_fresh(entry), 'editing a compiled C source must force a rebuild'
}

// A build that writes more than the pipe buffer holds must not deadlock: the child blocks
// writing while the parent blocks in `wait()`, and `v fmt`/`v vet` then hang forever. The
// launcher has to drain the pipe while the child is still running.
fn test_a_tool_build_with_large_output_does_not_deadlock() {
	vexe := @VEXE
	if !os.is_executable(vexe) {
		eprintln('> skipping, no V executable at `${vexe}`')
		return
	}
	directory := toolcache_test_dir('large_output')
	defer {
		os.rmdir_all(directory) or {}
	}
	// Several thousand distinct diagnostics, comfortably past any pipe buffer.
	mut lines := ['module main\n']
	for i in 0 .. 4000 {
		lines << 'fn broken_${i}() { undefined_call_${i}() }'
	}
	source := os.join_path(directory, 'vdemo.v')
	os.write_file(source, lines.join('\n'))!

	entry := ToolCacheEntry{
		name:     'vdemo'
		source:   source
		vroot:    directory
		binary:   os.join_path(directory, 'vdemo-' + 'a'.repeat(64))
		manifest: os.join_path(directory, 'vdemo-' + 'a'.repeat(64)) + '.inputs'
	}
	// The build must fail, but it must *return*. Before draining concurrently this call
	// never came back at all.
	output := build_tool_binary(vexe, entry) or { err.msg() }
	assert output.len > 0, 'the failing build has to report something'
	assert !os.exists(entry.binary), 'a failed build must not install a binary'
}

// An unwritable cache directory has to be passed over, not returned: staging into it fails
// for every tool, while the next candidate would have worked.
fn test_an_unwritable_cache_directory_is_skipped() {
	if os.getuid() == 0 {
		eprintln('> skipping, root can write into a read-only directory')
		return
	}
	directory := toolcache_test_dir('unwritable')
	defer {
		os.chmod(os.join_path(directory, 'locked'), 0o755) or {}
		os.rmdir_all(directory) or {}
	}
	locked := os.join_path(directory, 'locked')
	os.mkdir_all(locked)!
	os.chmod(locked, 0o500)!
	assert !directory_is_writable(locked), 'a read-only directory must not pass the probe'

	writable := os.join_path(directory, 'open')
	os.mkdir_all(writable)!
	assert directory_is_writable(writable), 'a normal directory must pass the probe'
	// The probe must not leave anything behind that a later listing would trip over.
	assert os.ls(writable)! == []

	previous := os.getenv(tool_cache_dir_env)
	defer {
		os.setenv(tool_cache_dir_env, previous, true)
	}
	os.setenv(tool_cache_dir_env, locked, true)
	chosen := tool_cache_dir() or { '' }
	assert chosen != locked, 'the unwritable candidate must not be chosen'
	assert chosen == '' || directory_is_writable(chosen), 'the chosen cache has to be writable'
}

// `$pkgconfig(...)` and `#pkgconfig` select whole native branches, so the pkg-config
// environment decides what a tool is built against without touching a single source stamp.
fn test_the_cache_key_covers_the_pkgconfig_environment() {
	directory := toolcache_test_dir('pkgconfig')
	defer {
		os.rmdir_all(directory) or {}
	}
	vexe := os.join_path(directory, 'v')
	os.write_file(vexe, 'a pretend V executable')!
	source := os.join_path(directory, 'vdemo.v')
	os.write_file(source, 'module main\n')!

	for name in ['PKG_CONFIG_PATH', 'PKG_CONFIG_LIBDIR', 'PKG_CONFIG_SYSROOT_DIR'] {
		assert name in ambient_build_variables, '${name} has to be part of the cache identity'
	}

	previous := os.getenv('PKG_CONFIG_PATH')
	defer {
		os.setenv('PKG_CONFIG_PATH', previous, true)
	}
	os.setenv('PKG_CONFIG_PATH', '', true)
	baseline := tool_cache_key(vexe, 'vdemo', [source], [])
	os.setenv('PKG_CONFIG_PATH', '/opt/custom/lib/pkgconfig', true)
	assert tool_cache_key(vexe, 'vdemo', [source], []) != baseline, 'PKG_CONFIG_PATH must be part of the key'
	os.setenv('PKG_CONFIG_PATH', '', true)
	assert tool_cache_key(vexe, 'vdemo', [source], []) == baseline, 'clearing it must restore the key'
}

// Tools read `os.file_name(os.executable())` and use it as their displayed name and as their
// own cache directory -- `cmd/tools/vshader.v` keys `~/.cache/v/<name>` on it, so a hashed
// executable name made every compiler change re-download `sokol-shdc`. The hash belongs in
// the directory, never in the executable's name.
fn test_a_cached_tool_keeps_its_own_executable_name() {
	vexe := @VEXE
	if !os.is_executable(vexe) {
		eprintln('> skipping, no V executable at `${vexe}`')
		return
	}
	cache := toolcache_test_dir('exe_name')
	defer {
		os.rmdir_all(cache) or {}
	}
	os.setenv(tool_cache_dir_env, cache, true)
	defer {
		os.unsetenv(tool_cache_dir_env)
	}
	run := os.execute('${os.quoted_path(vexe)} timeout 60 ${os.quoted_path(vexe)} version')
	assert run.exit_code == 0, run.output

	entries := cached_entry_dirs(cache, probe_tool)
	assert entries.len == 1, 'expected a single cached `${probe_tool}`, got ${entries}'
	// The directory carries the content address ...
	assert entries[0].starts_with('${probe_tool}-')
	assert entries[0].len > probe_tool.len + 1 + 60
	// ... and the executable inside it carries only the tool's name.
	binary := os.join_path(cache, entries[0], probe_tool + tool_exe_suffix())
	assert os.is_executable(binary), 'expected an executable at `${binary}`'
	assert os.file_name(binary) == probe_tool + tool_exe_suffix()
	assert !os.file_name(binary).contains('-')
}

fn test_unresolved_import_modules_are_read_from_the_failure() {
	assert unresolved_import_modules('x.v:2:1: builder error: cannot import module "db.sqlite" (not found)') == [
		'db.sqlite',
	]
	// Several unresolved imports, reported in a stable order and without duplicates.
	many := 'cannot import module "b.c" (not found)\ncannot import module "a" (not found)\ncannot import module "b.c" (not found)'
	assert unresolved_import_modules(many) == ['a', 'b.c']
	// A failure that is not about imports contributes nothing.
	assert unresolved_import_modules('x.v:1:1: error: unknown type `Foo`') == []
	assert unresolved_import_modules('') == []
}

// A module that could not be resolved contributed no source file to the manifest, so the only
// record of it is where it would have lived. Stamping just `vlib` and `vlib/v` caught a module
// removed directly under them and nothing deeper: `db.sqlite` could be removed and restored
// with both of those unchanged, replaying the recorded failure forever.
fn test_a_restored_nested_module_invalidates_a_recorded_failure() {
	directory := toolcache_test_dir('nested_module')
	defer {
		os.rmdir_all(directory) or {}
	}
	// A vroot whose `vlib/db` exists but whose `vlib/db/sqlite` does not, which is exactly
	// the state a removed nested module leaves behind.
	os.mkdir_all(os.join_path(directory, 'vlib', 'v'))!
	os.mkdir_all(os.join_path(directory, 'vlib', 'db'))!
	binary := os.join_path(directory, 'vdemo-' + 'a'.repeat(64))
	entry := ToolCacheEntry{
		name:                 'vdemo'
		vroot:                directory
		dir:                  binary
		binary:               os.join_path(binary, 'vdemo')
		manifest:             os.join_path(binary, 'inputs')
		unbuildable:          os.join_path(binary, 'unbuildable')
		unbuildable_manifest: os.join_path(binary, 'unbuildable.inputs')
	}
	os.mkdir_all(entry.dir)!
	dumped := os.join_path(directory, 'sources.txt')
	os.write_file(dumped, '')!
	details := 'x.v:2:1: builder error: cannot import module "db.sqlite" (not found)'

	time.sleep(1100 * time.millisecond)
	record_unbuildable_tool(entry, dumped, time.now().unix(), details)
	recorded := os.read_file(entry.unbuildable_manifest)!
	assert recorded.contains(os.join_path(directory, 'vlib', 'db', 'sqlite')), 'the missing module path must be stamped, got:\n${recorded}'
	assert unresolved_import_modules(details) == ['db.sqlite']
	// While the module is still absent the failure stands, or every invocation would pay
	// for the same failing compilation again.
	assert unbuildable_tool_failure(entry) != none, 'the failure must stand while the module is missing'

	// The premise of the bug: the two fixed roots record their *direct* children, so
	// restoring a module one level deeper leaves both of them reading exactly as before.
	// Without the module's own ancestors in the manifest nothing would ever notice.
	vlib_root := os.join_path(directory, 'vlib')
	v_root := os.join_path(directory, 'vlib', 'v')
	vlib_before := module_root_stamp(vlib_root)
	v_before := module_root_stamp(v_root)

	os.mkdir_all(os.join_path(directory, 'vlib', 'db', 'sqlite'))!

	assert module_root_stamp(vlib_root) == vlib_before, '`vlib` alone cannot see this change'
	assert module_root_stamp(v_root) == v_before, '`vlib/v` alone cannot see this change'
	assert unbuildable_tool_failure(entry) == none, 'restoring a nested module must retry the build'
}

// `vlib` is only the first place an import is resolved against; `$VMODULES` (`~/.vmodules`
// by default) is the second. A module that failed to resolve can come back in either, so a
// recorded failure has to watch both or it is replayed forever in the `~/.vmodules` case.
fn test_a_restored_vmodules_module_invalidates_a_recorded_failure() {
	directory := toolcache_test_dir('vmodules_module')
	defer {
		os.rmdir_all(directory) or {}
	}
	vmodules := os.join_path(directory, 'vmodules')
	os.mkdir_all(os.join_path(vmodules, 'acme'))!
	os.mkdir_all(os.join_path(directory, 'vlib', 'v'))!

	previous := os.getenv('VMODULES')
	os.setenv('VMODULES', vmodules, true)
	defer {
		os.setenv('VMODULES', previous, true)
	}
	assert vmodules in module_search_roots(directory, []), 'the vmodules root has to be searched'

	entry_dir := os.join_path(directory, 'vdemo-' + 'a'.repeat(64))
	entry := ToolCacheEntry{
		name:                 'vdemo'
		vroot:                directory
		dir:                  entry_dir
		binary:               os.join_path(entry_dir, 'vdemo')
		manifest:             os.join_path(entry_dir, 'inputs')
		unbuildable:          os.join_path(entry_dir, 'unbuildable')
		unbuildable_manifest: os.join_path(entry_dir, 'unbuildable.inputs')
	}
	os.mkdir_all(entry.dir)!
	dumped := os.join_path(directory, 'sources.txt')
	os.write_file(dumped, '')!
	details := 'x.v:2:1: builder error: cannot import module "acme.widget" (not found)'

	time.sleep(1100 * time.millisecond)
	record_unbuildable_tool(entry, dumped, time.now().unix(), details)
	recorded := os.read_file(entry.unbuildable_manifest)!
	assert recorded.contains(os.join_path(vmodules, 'acme', 'widget')), 'the vmodules path must be stamped, got:\n${recorded}'
	assert unbuildable_tool_failure(entry) != none, 'the failure must stand while the module is missing'

	// It reappears under `~/.vmodules`, not under `vlib`, so nothing rooted at the tree can
	// see it: only the vmodules ancestors make the build be retried.
	vlib_root := os.join_path(directory, 'vlib')
	vlib_before := module_root_stamp(vlib_root)
	os.mkdir_all(os.join_path(vmodules, 'acme', 'widget'))!
	assert module_root_stamp(vlib_root) == vlib_before, '`vlib` cannot see a vmodules change'
	assert unbuildable_tool_failure(entry) == none, 'restoring a vmodules module must retry the build'
}

// A missing module can already have a directory while containing no usable sources. Its
// direct child directories do not change when the first source appears, so the final module
// path needs a V-source-name stamp rather than another module-root stamp.
fn test_adding_the_first_source_to_a_missing_module_invalidates_a_recorded_failure() {
	directory := toolcache_test_dir('empty_missing_module')
	defer {
		os.rmdir_all(directory) or {}
	}
	module_dir := os.join_path(directory, 'vlib', 'db', 'sqlite')
	os.mkdir_all(module_dir)!
	entry_dir := os.join_path(directory, 'vdemo-' + 'a'.repeat(64))
	entry := ToolCacheEntry{
		name:                 'vdemo'
		vroot:                directory
		dir:                  entry_dir
		unbuildable:          os.join_path(entry_dir, 'unbuildable')
		unbuildable_manifest: os.join_path(entry_dir, 'unbuildable.inputs')
	}
	os.mkdir_all(entry.dir)!
	dumped := os.join_path(directory, 'sources.txt')
	os.write_file(dumped, '')!
	details := 'x.v:2:1: builder error: cannot import module "db.sqlite" (not found)'
	record_unbuildable_tool(entry, dumped, time.now().unix(), details)
	assert unbuildable_tool_failure(entry) != none, 'an empty module directory is still missing'
	before := dir_stamp(module_dir)

	os.write_file(os.join_path(module_dir, 'sqlite.v'), 'module sqlite\n')!

	assert dir_stamp(module_dir) != before, 'the module source stamp must see the first `.v` file'
	assert unbuildable_tool_failure(entry) == none, 'adding the first source must retry the build'
}

// `-path` replaces the default vlib/vmodules search roots. A dependency restored below one
// of those explicit roots must invalidate the failure recorded for that exact invocation.
fn test_a_restored_module_under_an_explicit_path_invalidates_a_recorded_failure() {
	directory := toolcache_test_dir('explicit_missing_module')
	defer {
		os.rmdir_all(directory) or {}
	}
	explicit_root := os.join_path(directory, 'private_modules')
	os.mkdir_all(os.join_path(explicit_root, 'acme'))!
	assert module_search_roots(directory, ['-path', explicit_root]) == [explicit_root]
	entry_dir := os.join_path(directory, 'vdemo-' + 'a'.repeat(64))
	entry := ToolCacheEntry{
		name:                 'vdemo'
		vroot:                directory
		dir:                  entry_dir
		unbuildable:          os.join_path(entry_dir, 'unbuildable')
		unbuildable_manifest: os.join_path(entry_dir, 'unbuildable.inputs')
		build_args:           ['-path', explicit_root]
	}
	os.mkdir_all(entry.dir)!
	dumped := os.join_path(directory, 'sources.txt')
	os.write_file(dumped, '')!
	details := 'x.v:2:1: builder error: cannot import module "acme.widget" (not found)'
	record_unbuildable_tool(entry, dumped, time.now().unix(), details)
	recorded := os.read_file(entry.unbuildable_manifest)!
	module_dir := os.join_path(explicit_root, 'acme', 'widget')
	assert recorded.contains(module_dir), 'the explicit module path must be stamped, got:\n${recorded}'
	assert !recorded.contains(os.join_path(directory, 'vlib', 'acme', 'widget'))
	assert unbuildable_tool_failure(entry) != none, 'the failure must stand while the module is missing'

	os.mkdir_all(module_dir)!
	os.write_file(os.join_path(module_dir, 'widget.v'), 'module widget\n')!

	assert unbuildable_tool_failure(entry) == none, 'restoring an explicit-path module must retry the build'
}

// `VMODULES` decides which copy of a module an import resolves to, so it selects sources
// without changing any path already recorded in a manifest.
fn test_the_cache_key_covers_vmodules() {
	directory := toolcache_test_dir('vmodules_key')
	defer {
		os.rmdir_all(directory) or {}
	}
	vexe := os.join_path(directory, 'v')
	os.write_file(vexe, 'a pretend V executable')!
	source := os.join_path(directory, 'vdemo.v')
	os.write_file(source, 'module main\n')!

	assert 'VMODULES' in ambient_build_variables
	previous := os.getenv('VMODULES')
	defer {
		os.setenv('VMODULES', previous, true)
	}
	os.setenv('VMODULES', '', true)
	baseline := tool_cache_key(vexe, 'vdemo', [source], [])
	os.setenv('VMODULES', os.join_path(directory, 'elsewhere'), true)
	assert tool_cache_key(vexe, 'vdemo', [source], []) != baseline, 'VMODULES must be part of the key'
	os.setenv('VMODULES', '', true)
	assert tool_cache_key(vexe, 'vdemo', [source], []) == baseline, 'clearing it must restore the key'
}

// `$embed_file` on an asset that is not there compiles: the binary simply carries no payload.
// Recording where the asset should have been is the only thing that lets a later invocation
// notice it was restored, so the path has to reach the manifest precisely while it is absent.
fn test_a_missing_embedded_asset_is_recorded_and_invalidates_the_cache() {
	vexe := @VEXE
	if !os.is_executable(vexe) {
		eprintln('> skipping, no V executable at `${vexe}`')
		return
	}
	directory := toolcache_test_dir('missing_embed')
	defer {
		os.rmdir_all(directory) or {}
	}
	os.write_file(os.join_path(directory, 'v.mod'), 'Module { name: "embdemo" }\n')!
	asset := os.join_path(directory, 'config.toml')
	source := os.join_path(directory, 'vdemo.v')
	os.write_file(source, "module main\n\nconst asset = \$embed_file('config.toml')\n\nfn main() {\n\tprintln(asset.len)\n}\n")!
	assert !os.exists(asset), 'the asset must be absent for this build'

	dumped := os.join_path(directory, 'sources.txt')
	binary := os.join_path(directory, 'vdemo')
	build :=
		os.execute('${os.quoted_path(vexe)} -prod -dump-files ${os.quoted_path(dumped)} -o ${os.quoted_path(binary)} ${os.quoted_path(source)}')
	assert build.exit_code == 0, build.output

	recorded := (os.read_file(dumped) or { '' }).split_into_lines().filter(it != '')
	assert os.real_path(asset) in recorded || asset in recorded, 'the absent asset must be recorded, got ${recorded.filter(!it.contains('/vlib/'))}'

	entry := ToolCacheEntry{
		name:     'vdemo'
		binary:   binary
		manifest: binary + '.inputs'
	}
	time.sleep(1100 * time.millisecond)
	os.write_file(entry.manifest, encode_tool_cache_manifest(recorded, time.now().unix()))!
	// An input that was absent when recorded and still is must read as unchanged. `os.stat`
	// cannot date it, so a freshness check that trusted `last_modified` here would call it
	// changed on every lookup and rebuild the tool forever.
	assert tool_cache_is_fresh(entry), 'a still absent asset must not force a rebuild'
	assert tool_cache_is_fresh(entry), 'the answer has to be stable across lookups'

	// Restoring it has to invalidate the binary that was built without a payload.
	os.write_file(asset, 'key = "value"\n')!
	assert !tool_cache_is_fresh(entry), 'restoring an embedded asset must force a rebuild'
}

fn test_a_missing_input_stamps_as_missing() {
	directory := toolcache_test_dir('missing_stamp')
	defer {
		os.rmdir_all(directory) or {}
	}
	absent := os.join_path(directory, 'not-there')
	assert file_stamp(absent) == file_stamp_missing
	os.write_file(absent, 'now it is')!
	assert file_stamp(absent) != file_stamp_missing, 'appearing has to change the stamp'
}

// Publishing the executable and publishing the manifest are two separate steps, so two
// processes rebuilding one key across a source edit can interleave: the newer build installs
// its binary, the older one replaces it and publishes its old manifest, then the newer one
// publishes its manifest. That leaves the older executable vouched for by a current, fresh
// manifest, and every later invocation runs stale code. The manifest names the exact build it
// describes so a reader can see the mismatch.
fn test_a_binary_from_another_build_is_not_trusted_by_a_fresh_manifest() {
	entry, _ := fresh_cache_fixture('interleaved')
	defer {
		os.rmdir_all(os.dir(entry.binary)) or {}
	}
	// The fixture's manifest has no binary line, so pair it the way a real build does.
	paired := (os.read_file(entry.manifest) or { '' }) + 'b${tool_cache_field_separator}${entry.binary}${tool_cache_field_separator}${binary_identity(entry.binary)}\n'
	os.write_file(entry.manifest, paired)!
	assert tool_cache_is_fresh(entry), 'a binary and the manifest that describes it must pair'

	// Another build of the same key replaces the executable. Its sources are identical, so
	// nothing else in the manifest can notice; only the pairing can.
	other := entry.binary + '.other'
	os.write_file(other, 'a different build of the same key')!
	os.chmod(other, 0o755)!
	os.mv(other, entry.binary)!

	assert !tool_cache_is_fresh(entry), 'an executable from another build must not be reused'
	reason := tool_cache_stale_reason(entry)
	assert reason.contains(entry.binary), 'the reason has to name the binary, got `${reason}`'
}

fn test_binary_identity_separates_two_builds() {
	directory := toolcache_test_dir('identity')
	defer {
		os.rmdir_all(directory) or {}
	}
	first := os.join_path(directory, 'first')
	os.write_file(first, 'build one')!
	identity := binary_identity(first)
	assert identity != file_stamp_missing
	// Re-reading the same file has to give the same answer, or every lookup would rebuild.
	assert binary_identity(first) == identity

	// A separately created file is a different build, even with identical contents.
	second := os.join_path(directory, 'second')
	os.write_file(second, 'build one')!
	assert binary_identity(second) != identity, 'two builds must not share an identity'

	assert binary_identity(os.join_path(directory, 'absent')) == file_stamp_missing
}
