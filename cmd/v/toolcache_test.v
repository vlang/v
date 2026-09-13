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

// cached_binaries returns the cached executables of `tool`, without the bookkeeping files.
fn cached_binaries(directory string, tool string) []string {
	names := os.ls(directory) or { return [] }
	mut found := names.filter(it.starts_with('${tool}-') && !it.contains('.inputs')
		&& !it.contains('.unbuildable') && !it.contains('.staged') && !it.contains('.sources'))
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
	binaries := cached_binaries(cache, probe_tool)
	assert binaries.len == 1, 'expected a single cached `${probe_tool}`, got ${binaries}'
	binary := os.join_path(cache, binaries[0])
	before := os.stat(binary)!

	// Every rebuild prunes the earlier entries of the same tool, so a decoy shaped like the
	// entry of an older build, still being there afterwards, proves that the second
	// invocation did not recompile anything.
	decoy := os.join_path(cache, '${probe_tool}-' + 'a'.repeat(64))
	os.write_file(decoy, 'decoy')!

	probe := ToolCacheEntry{
		name:     probe_tool
		binary:   binary
		manifest: binary + '.inputs'
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
	remaining := cached_binaries(cache, probe_tool).filter(it != os.file_name(decoy))
	assert remaining == binaries, 'a second entry appeared for the same tool: ${remaining}'
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
