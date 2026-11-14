// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import os.filelock
import time
import v.cmdexec
import v.util

// `vtimeout` is used as the probe tool: it is small enough to compile quickly, and
// `v timeout <seconds> <command>` both succeeds and terminates on its own.
const probe_tool = 'vtimeout'
const toolcache_prune_probe_env = 'VTEST_TOOLCACHE_PRUNE_PROBE'
const toolcache_sidecar_probe_env = 'VTEST_TOOLCACHE_SIDECAR_PROBE'
const toolcache_updater_probe_env = 'VTEST_TOOLCACHE_UPDATER_PROBE'
const vup_defer_cache_wipe_env = 'VUP_DEFER_CACHE_WIPE'
const vup_deferred_wipe_exit_code = 80
const toolcache_lock_holder_env = 'VTEST_TOOLCACHE_LOCK_HOLDER'
const toolcache_lock_release_env = 'VTEST_TOOLCACHE_LOCK_RELEASE'
const toolcache_wipe_child_env = 'VTEST_TOOLCACHE_WIPE_CHILD'

// Reuse the test executable for a bounded pruning probe. Reading a planted FIFO used to
// block indefinitely while holding the tool's lock, so run that work in a child process.
fn testsuite_begin() {
	if lock_path := os.getenv_opt(toolcache_lock_holder_env) {
		hold_tool_cache_lock_until_released(lock_path, os.getenv(toolcache_lock_release_env))
	}
	if start_path := os.getenv_opt(toolcache_wipe_child_env) {
		// One of several wipes that start at the same moment, each in a process of its own.
		deadline := time.now().add(10 * time.second)
		for time.now() < deadline && !os.exists(start_path) {
			time.sleep(time.millisecond)
		}
		lines := wipe_caches(parse_wipe_cache_options([], []) or { panic(err) }) or {
			eprintln(err.msg())
			exit(1)
		}
		println(lines.join('\n'))
		exit(0)
	}
	if mode := os.getenv_opt(toolcache_updater_probe_env) {
		if mode == 'ordinary' {
			assert os.getenv(vup_defer_cache_wipe_env) == ''
			exit(vup_deferred_wipe_exit_code)
		}
		assert os.getenv(vup_defer_cache_wipe_env) == '1'
		assert os.is_file(os.executable())
		match mode {
			'help' { exit(0) }
			'failure' { exit(7) }
			else { exit(vup_deferred_wipe_exit_code) }
		}
	}
	if mode := os.getenv_opt(toolcache_sidecar_probe_env) {
		// Act as a compiler that produces an executable and optionally a sidecar.
		mut output := ''
		mut dumped := ''
		for i, arg in os.args {
			if arg == '-o' { output = os.args[i + 1] }
			if arg == '-dump-files' { dumped = os.args[i + 1] }
		}
		os.cp(os.executable(), output)!
		os.chmod(output, 0o755)!
		if mode == 'sidecar' {
			os.write_file('${output}.manifest', 'application manifest')!
		}
		os.write_file(dumped, os.args.last() + '\n')!
		os.write_file(os.args.last() + '.stage', os.dir(output))!
		exit(0)
	}
	if directory := os.getenv_opt(toolcache_prune_probe_env) {
		prune_stale_tool_binaries(ToolCacheEntry{
			name: 'vdemo'
			dir:  os.join_path(directory, 'vdemo-' + 'a'.repeat(64))
		})
		exit(0)
	}
}

fn test_cached_tool_publishes_and_tracks_its_application_manifest() ! {
	directory := toolcache_test_dir('application_manifest')
	defer { os.rmdir_all(directory) or {} }
	previous := os.getenv_opt(toolcache_sidecar_probe_env)
	defer {
		if value := previous {
			os.setenv(toolcache_sidecar_probe_env, value, true)
		} else {
			os.unsetenv(toolcache_sidecar_probe_env)
		}
	}
	source := os.join_path(directory, 'vdemo.v')
	os.write_file(source, 'module main\n')!
	// Source timestamps must be older than the recorded build start.
	time.sleep(1100 * time.millisecond)
	cache := os.join_path(directory, 'vdemo-' + 'a'.repeat(64))
	entry := ToolCacheEntry{
		name:                 'vdemo'
		source:               source
		vroot:                directory
		dir:                  cache
		binary:               os.join_path(cache, 'vdemo' + tool_exe_suffix())
		manifest:             os.join_path(cache, 'inputs')
		unbuildable:          os.join_path(cache, 'unbuildable')
		unbuildable_manifest: os.join_path(cache, 'unbuildable.inputs')
	}
	os.setenv(toolcache_sidecar_probe_env, 'sidecar', true)
	build_tool_binary(os.executable(), entry)!
	sidecar := '${entry.binary}.manifest'
	assert os.read_file(sidecar)! == 'application manifest'
	assert !os.exists(os.read_file(source + '.stage')!)
	assert tool_cache_is_fresh(entry), tool_cache_stale_reason(entry)
	os.write_file(sidecar, 'application manifesT')!
	assert !tool_cache_is_fresh(entry)
	build_tool_binary(os.executable(), entry)!
	assert os.read_file(sidecar)! == 'application manifest'
	os.rm(sidecar)!
	assert !tool_cache_is_fresh(entry)
	// Rebuilding without a sidecar removes the old one and records its absence.
	os.write_file(sidecar, 'stale manifest')!
	os.setenv(toolcache_sidecar_probe_env, 'none', true)
	build_tool_binary(os.executable(), entry)!
	assert !os.exists(sidecar)
	assert tool_cache_is_fresh(entry), tool_cache_stale_reason(entry)
	os.write_file(sidecar, 'unexpected manifest')!
	assert !tool_cache_is_fresh(entry)
}

fn toolcache_prune_probe(directory string) os.Result {
	previous := os.getenv_opt(toolcache_prune_probe_env)
	os.setenv(toolcache_prune_probe_env, directory, true)
	defer {
		if value := previous {
			os.setenv(toolcache_prune_probe_env, value, true)
		} else {
			os.unsetenv(toolcache_prune_probe_env)
		}
	}
	return cmdexec.run_with_timeout(os.executable(), [], 5000)
}

// tool_cache_is_fresh reports whether `entry.binary` can be executed as is, i.e. whether
// every source file it was built from is still exactly the way it was at build time.
fn tool_cache_is_fresh(entry ToolCacheEntry) bool {
	return tool_cache_stale_reason(entry) == ''
}

// record_unbuildable_tool writes a failed build and its inputs for cache test fixtures.
fn record_unbuildable_tool(entry ToolCacheEntry, dumped string, started i64, details string) {
	manifest := encode_unbuildable_tool_manifest(entry, dumped, started, details)
	os.write_file(entry.unbuildable_manifest, manifest) or { return }
	os.write_file(entry.unbuildable, details) or {}
}

// prune_stale_tool_binaries drops the cache entries of previous builds of the same tool.
// Unlinking an executable that another process is currently running is safe on POSIX: that
// process keeps its own already opened image.
fn prune_stale_tool_binaries(entry ToolCacheEntry) {
	mut build_lock := tool_cache_lock(entry) or { return }
	if !build_lock.try_acquire() {
		return
	}
	defer {
		build_lock.release()
	}
	prune_stale_tool_binaries_locked(entry)
}

fn toolcache_test_dir(name string) string {
	mut directory := os.join_path(os.vtmp_dir(), 'v_toolcache_test', '${name}_${os.getpid()}')
	$if windows {
		// The MSVC probe is not longPathAware. Leave room for a complete cache
		// key, staging directory, executable name, and application manifest.
		directory = os.join_path(os.vtmp_dir(), 'vtc_${os.getpid()}_${name}')
	}
	os.rmdir_all(directory) or {}
	os.mkdir_all(directory) or { panic(err) }
	// Make the test folder private (0700). The cache refuses folders that the group can
	// write to, and a umask like 002 would create them that way.
	os.chmod(directory, 0o700) or { panic(err) }
	return directory
}

// private_toolcache_test_dir prepares a root that the real launcher can select. Windows
// chmod does not change ACLs, and an elevated process can create an Administrators-owned
// directory even when its token user is the account that will run the cached tool.
fn private_toolcache_test_dir(name string) string {
	directory := toolcache_test_dir(name)
	$if windows {
		system32 := os.join_path(os.getenv('SystemRoot'), 'System32')
		identity := os.exec([os.join_path(system32, 'whoami.exe'), '/user', '/fo', 'csv', '/nh'])
		assert identity.exit_code == 0, identity.output
		sid := identity.output.trim_space().all_after_last(',').trim('"')
		assert sid.starts_with('S-1-') && sid[4..].bytes().all(it.is_digit() || it == `-`), identity.output
		icacls := os.join_path(system32, 'icacls.exe')
		for arguments in [
			['/setowner', '*${sid}'],
			['/grant:r', '*${sid}:(OI)(CI)F'],
			['/inheritancelevel:r'],
		] {
			mut command := [icacls, directory]
			command << arguments
			result := os.exec(command)
			assert result.exit_code == 0, result.output
		}
	}
	assert tool_cache_root_can_stage(directory), tool_cache_root_problem(directory)
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
	cache := private_toolcache_test_dir('reuse')
	defer {
		os.rmdir_all(cache) or {}
	}
	os.setenv(tool_cache_dir_env, cache, true)
	defer {
		os.unsetenv(tool_cache_dir_env)
	}
	command := '${os.quoted_path(vexe)} timeout 60 ${os.quoted_path(vexe)} version'

	first := os.exec([vexe, 'timeout', '60', vexe, 'version'])
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

	// Every rebuild prunes invalid earlier entries of the same tool, so a decoy shaped like the
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

	second := os.exec([vexe, 'timeout', '60', vexe, 'version'])
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
	// A cached tool is named like the tool plus the platform suffix, because that
	// is what `os.is_executable` accepts on this platform.
	binary := os.join_path(directory, 'vdemo-0123456789' + tool_exe_suffix())
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

fn test_project_boundary_markers_invalidate_the_cache() {
	for marker_name in util.project_boundary_markers {
		entry, dependency := fresh_cache_fixture('boundary_${marker_name}')
		defer { os.rmdir_all(os.dir(entry.binary)) or {} }
		marker := os.join_path(os.dir(os.dir(dependency)), marker_name)
		if marker_name == '.v.mod.stop' {
			os.write_file(marker, '')!
		} else {
			os.mkdir(marker)!
		}
		assert !tool_cache_is_fresh(entry), 'adding ${marker_name} must force a rebuild'
		time.sleep(1100 * time.millisecond)
		os.write_file(entry.manifest, encode_tool_cache_manifest([dependency], time.now().unix()))!
		assert tool_cache_is_fresh(entry)
		if marker_name == '.v.mod.stop' {
			os.write_file(marker, 'marker contents changed')!
			assert tool_cache_is_fresh(entry), 'editing ${marker_name} must not force a rebuild'
			os.rm(marker)!
		} else {
			os.write_file(os.join_path(marker, 'index.lock'), '')!
			assert tool_cache_is_fresh(entry), 'editing ${marker_name} must not force a rebuild'
			os.rmdir_all(marker)!
		}
		assert !tool_cache_is_fresh(entry), 'removing ${marker_name} must force a rebuild'
	}
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

fn test_cache_entry_directories_are_namespaced_by_uid() {
	key := 'a'.repeat(64)
	first := tool_cache_entry_dir_for_uid('/cache', 'vbug', key, 1000)
	second := tool_cache_entry_dir_for_uid('/cache', 'vbug', key, 1001)
	assert first != second
	assert os.file_name(first) == 'vbug-${key}.1000'
	assert is_cache_artifact_of(os.file_name(first), 'vbug')
}

fn test_staged_tool_binary_path_keeps_the_platform_executable_suffix() {
	binary := os.join_path('/cache', 'vdemo' + tool_exe_suffix())
	staged := staged_tool_binary_path('/stage', binary)
	assert staged == os.join_path('/stage', os.file_name(binary))
	$if windows {
		assert staged.ends_with('.exe')
	}
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

	cache_entry := open_tool_cache_entry_dir(directory)!
	defer {
		cache_entry.close()
	}
	assert cache_entry.publish(staged, os.file_name(destination))
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

fn test_pruning_preserves_other_fresh_keys_and_failures_until_their_inputs_change() {
	directory := toolcache_test_dir('prune_fresh_keys')
	defer { os.rmdir_all(directory) or {} }
	mut dependencies := []string{}
	mut entries := []ToolCacheEntry{}
	for index, key in ['a', 'b', 'c'] {
		module_dir := os.join_path(directory, 'module_${index}')
		os.mkdir(module_dir)!
		dependency := os.join_path(module_dir, 'source.v')
		os.write_file(dependency, 'module module_${index}\n')!
		dependencies << dependency
		entry_dir := os.join_path(directory, 'vdemo-' + key.repeat(64))
		os.mkdir(entry_dir)!
		entry := ToolCacheEntry{
			name:                 'vdemo'
			dir:                  entry_dir
			binary:               os.join_path(entry_dir, 'vdemo' + tool_exe_suffix())
			manifest:             os.join_path(entry_dir, 'inputs')
			unbuildable:          os.join_path(entry_dir, 'unbuildable')
			unbuildable_manifest: os.join_path(entry_dir, 'unbuildable.inputs')
		}
		entries << entry
		if index < 2 {
			os.write_file(entry.binary, 'compiled tool ${index}')!
			os.chmod(entry.binary, 0o755)!
		} else {
			os.write_file(entry.unbuildable, 'error: rejected source')!
		}
	}
	time.sleep(1100 * time.millisecond)
	for index, entry in entries {
		manifest := encode_tool_cache_manifest([dependencies[index]], time.now().unix())
		os.write_file(if index < 2 { entry.manifest } else { entry.unbuildable_manifest }, manifest)!
	}

	prune_stale_tool_binaries(entries[0])
	assert tool_cache_is_fresh(entries[0])
	assert tool_cache_is_fresh(entries[1]), 'building one key must preserve another usable key'
	assert unbuildable_tool_failure(entries[2])? == 'error: rejected source'
	prune_stale_tool_binaries(entries[1])
	assert tool_cache_is_fresh(entries[0]), 'alternating keys must reuse both cached builds'

	for index in [1, 2] {
		os.write_file(dependencies[index], 'module module_${index}\nfn changed() {}\n')!
	}
	prune_stale_tool_binaries(entries[0])
	assert !os.exists(entries[1].dir), 'a changed source must make its old binary collectible'
	assert !os.exists(entries[2].dir), 'a changed source must make its old failure collectible'
}

fn test_tool_cache_lock_path_is_persistent_between_owners() {
	directory := toolcache_test_dir('persistent_lock')
	defer {
		os.rmdir_all(directory) or {}
	}
	entry_dir := os.join_path(directory, 'vdemo-' + 'a'.repeat(64))
	os.mkdir(entry_dir)!
	entry := ToolCacheEntry{
		name: 'vdemo'
		dir:  entry_dir
	}
	assert tool_cache_lock_path_for_uid(entry, 1000) != tool_cache_lock_path_for_uid(entry, 1001), 'accounts sharing a sticky cache root need separate mutex pathnames'
	assert tool_cache_lock_path(entry) == tool_cache_lock_path_for_uid(entry, os.getuid())
	mut first_owner := tool_cache_lock(entry)!
	first_owner.acquire()!
	first_owner.release()
	assert os.is_file(tool_cache_lock_path(entry)), 'the shared mutex pathname must remain stable between owners'
	mut next_owner := tool_cache_lock(entry)!
	assert next_owner.try_acquire()
	next_owner.release()
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

fn test_pruning_rejects_fifo_metadata_without_blocking() {
	$if windows {
		return
	}
	directory := toolcache_test_dir('prune_fifo_metadata')
	defer { os.rmdir_all(directory) or {} }
	os.mkdir(os.join_path(directory, 'vdemo-' + 'a'.repeat(64)))!
	manifest := '${tool_cache_manifest_version}\nstarted${tool_cache_field_separator}${time.now().unix()}\n'
	mut siblings := []string{}
	for index, metadata in ['inputs', 'unbuildable', 'unbuildable.inputs'] {
		sibling := os.join_path(directory, 'vdemo-' + ['b', 'c', 'd'][index].repeat(64))
		os.mkdir(sibling)!
		siblings << sibling
		if metadata == 'inputs' {
			binary := os.join_path(sibling, 'vdemo')
			os.write_file(binary, 'compiled tool')!
			os.chmod(binary, 0o755)!
		} else {
			os.write_file(os.join_path(sibling, 'unbuildable'), 'source error')!
			os.write_file(os.join_path(sibling, 'unbuildable.inputs'), manifest)!
			os.rm(os.join_path(sibling, metadata))!
		}
		fifo := cmdexec.run('mkfifo', [os.join_path(sibling, metadata)])
		assert fifo.exit_code == 0, fifo.output
	}

	result := toolcache_prune_probe(directory)
	assert result.exit_code == 0, 'pruning FIFO metadata must return promptly: ${result.output}'
	for sibling in siblings {
		assert !os.exists(sibling), 'unsafe metadata cannot preserve `${sibling}`'
	}
}

fn test_pruning_does_not_follow_metadata_symlinks() {
	directory := toolcache_test_dir('prune_metadata_links')
	defer { os.rmdir_all(directory) or {} }
	os.mkdir(os.join_path(directory, 'vdemo-' + 'a'.repeat(64)))!
	manifest := '${tool_cache_manifest_version}\nstarted${tool_cache_field_separator}${time.now().unix()}\n'
	target := os.join_path(directory, 'outside-metadata')
	os.write_file(target, manifest)!
	mut siblings := []string{}
	for index, metadata in ['inputs', 'unbuildable', 'unbuildable.inputs'] {
		sibling := os.join_path(directory, 'vdemo-' + ['b', 'c', 'd'][index].repeat(64))
		os.mkdir(sibling)!
		siblings << sibling
		if metadata == 'inputs' {
			binary := os.join_path(sibling, 'vdemo' + tool_exe_suffix())
			os.write_file(binary, 'compiled tool')!
			os.chmod(binary, 0o755)!
		} else {
			os.write_file(os.join_path(sibling, 'unbuildable'), 'source error')!
			os.write_file(os.join_path(sibling, 'unbuildable.inputs'), manifest)!
			os.rm(os.join_path(sibling, metadata))!
		}
		os.symlink(target, os.join_path(sibling, metadata)) or {
			eprintln('> skipping metadata symlink test: ${err}')
			return
		}
	}

	result := toolcache_prune_probe(directory)
	assert result.exit_code == 0, result.output
	for sibling in siblings {
		assert !os.exists(sibling), 'metadata links must not make `${sibling}` fresh'
	}
	assert os.read_file(target)! == manifest, 'the metadata link target must remain untouched'
}

fn test_pruning_skips_foreign_entries_and_rejects_foreign_metadata() {
	$if windows {
		return
	}
	if os.getuid() != 0 {
		eprintln('> skipping foreign-owner pruning test: changing ownership requires root')
		return
	}
	directory := toolcache_test_dir('prune_foreign_metadata')
	defer { os.rmdir_all(directory) or {} }
	os.mkdir(os.join_path(directory, 'vdemo-' + 'a'.repeat(64)))!
	foreign_entry := os.join_path(directory, 'vdemo-' + 'b'.repeat(64))
	os.mkdir(foreign_entry)!
	foreign_fifo := cmdexec.run('mkfifo', [os.join_path(foreign_entry, 'unbuildable')])
	assert foreign_fifo.exit_code == 0, foreign_fifo.output
	original_mode := os.stat(foreign_entry)!.mode
	os.chown(foreign_entry, 1, -1)!
	owned_entry := os.join_path(directory, 'vdemo-' + 'c'.repeat(64))
	os.mkdir(owned_entry)!
	manifest := '${tool_cache_manifest_version}\nstarted${tool_cache_field_separator}${time.now().unix()}\n'
	os.write_file(os.join_path(owned_entry, 'unbuildable.inputs'), manifest)!
	details := os.join_path(owned_entry, 'unbuildable')
	os.write_file(details, 'source error')!
	os.chown(details, 1, -1)!

	result := toolcache_prune_probe(directory)
	assert result.exit_code == 0, 'foreign entries must be skipped before reading: ${result.output}'
	assert os.is_dir(foreign_entry), 'another account owns the entry and its cleanup'
	assert os.stat(foreign_entry)!.mode == original_mode, 'ownership checks must precede chmod'
	assert !os.exists(owned_entry), 'foreign metadata must not preserve a cached failure'
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
		os.symlink(outside, entry_dir) or {
			eprintln('> skipping ${@FN}: ${err}')
			return
		}
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

fn test_pinned_stale_pruning_cannot_be_redirected() {
	directory := toolcache_test_dir('pinned_prune')
	defer {
		os.rmdir_all(directory) or {}
	}
	stale_dir := os.join_path(directory, 'vdemo-' + 'a'.repeat(64))
	os.mkdir(stale_dir)!
	os.write_file(os.join_path(stale_dir, 'cached-binary'), 'stale')!
	stale_entry := open_tool_cache_entry_dir(stale_dir)!
	defer {
		stale_entry.close()
	}

	original_dir := stale_dir + '.original'
	mut pathname_was_replaced := true
	os.rename(stale_dir, original_dir) or { pathname_was_replaced = false }
	outside := os.join_path(directory, 'outside')
	os.mkdir(outside)!
	must_survive := os.join_path(outside, 'must-survive')
	os.write_file(must_survive, 'safe')!
	if pathname_was_replaced {
		os.symlink(outside, stale_dir) or {
			eprintln('> skipping ${@FN}: ${err}')
			return
		}
	}

	stale_entry.remove_all_contents()
	assert os.read_file(must_survive)! == 'safe', 'recursive pruning must not follow a replacement symlink'
	cleaned_dir := if pathname_was_replaced { original_dir } else { stale_dir }
	assert os.ls(cleaned_dir)! == []
}

fn test_pinned_metadata_read_cannot_be_redirected() {
	directory := toolcache_test_dir('pinned_metadata')
	defer { os.rmdir_all(directory) or {} }
	entry_dir := os.join_path(directory, 'vdemo-' + 'a'.repeat(64))
	os.mkdir(entry_dir)!
	os.write_file(os.join_path(entry_dir, 'inputs'), 'original manifest')!
	cache_entry := open_tool_cache_entry_dir(entry_dir)!
	defer { cache_entry.close() }
	original_dir := entry_dir + '.original'
	mut pathname_was_replaced := true
	os.rename(entry_dir, original_dir) or { pathname_was_replaced = false }
	if pathname_was_replaced {
		os.mkdir(entry_dir)!
		os.write_file(os.join_path(entry_dir, 'inputs'), 'replacement manifest')!
	}
	assert cache_entry.read_metadata('inputs')? == 'original manifest'
	assert cache_entry.read_metadata('missing') == none
}

fn test_staging_directory_is_on_the_cache_filesystem() {
	directory := toolcache_test_dir('stage_filesystem')
	defer {
		os.rmdir_all(directory) or {}
	}
	entry_dir := os.join_path(directory, 'vdemo-' + 'a'.repeat(64))
	os.mkdir(entry_dir)!
	cache_entry := open_tool_cache_entry_dir(entry_dir)!
	defer {
		cache_entry.close()
	}
	stage_parent := cache_entry.stage_parent(entry_dir)!
	stage_dir := create_tool_cache_stage_dir(stage_parent)!
	defer {
		os.rmdir_all(stage_dir) or {}
	}

	assert os.dir(stage_dir) == stage_parent
	assert os.stat(stage_dir)!.dev == os.stat(entry_dir)!.dev
	os.write_file(os.join_path(stage_dir, 'partial-build'), 'abandoned')!
	cache_entry.prune_abandoned_stages()
	assert !os.exists(stage_dir), 'a stage left by an interrupted build must be reclaimed'
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
	binary := os.join_path(directory, 'vdemo-' + 'a'.repeat(64) + tool_exe_suffix())
	build :=
		os.exec([vexe, '-dump-files', '${dumped}', '-o', binary, source])
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
	binary := os.join_path(directory, 'vdemo-' + 'a'.repeat(64) + tool_exe_suffix())
	build :=
		os.exec([vexe, '-dump-files', '${dumped}', '-o', binary, source])
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
	entry_dir := os.join_path(directory, 'vdemo-' + 'a'.repeat(64))

	entry := ToolCacheEntry{
		name:     'vdemo'
		source:   source
		vroot:    directory
		dir:      entry_dir
		binary:   os.join_path(entry_dir, 'vdemo' + tool_exe_suffix())
		manifest: os.join_path(entry_dir, 'inputs')
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

fn test_a_non_sticky_shared_cache_cannot_hold_staged_outputs() {
	$if windows {
		return
	}
	directory := toolcache_test_dir('shared_stage_root')
	defer {
		os.chmod(directory, 0o700) or {}
		os.rmdir_all(directory) or {}
	}
	os.chmod(directory, 0o777)!
	assert !tool_cache_root_can_stage(directory)
	os.chmod(directory, 0o1777)!
	assert tool_cache_root_can_stage(directory), 'a sticky shared cache protects user-owned staging directories'
}

// Regression test: with a umask like 002, the cache folder used to be group writable. The
// cache then refused to use it, so `v doc`, `v fmt` etc. recompiled their tool on every run.
fn test_a_group_writable_own_cache_root_is_made_private() {
	$if windows {
		return
	}
	directory := toolcache_test_dir('group_writable_root')
	defer {
		os.chmod(directory, 0o700) or {}
		os.rmdir_all(directory) or {}
	}
	os.chmod(directory, 0o775)!
	assert !tool_cache_root_can_stage(directory)
	make_tool_cache_root_private(directory)
	assert os.stat(directory)!.mode & 0o777 == 0o755
	assert tool_cache_root_can_stage(directory)
	os.chmod(directory, 0o1777)!
	make_tool_cache_root_private(directory)
	assert os.stat(directory)!.mode & 0o7777 == 0o1777, 'sticky shared roots are left alone'
}

// Someone who can write to the parent folder could replace the cache folder with a symlink
// between the ownership check and the permission change. The repair must never follow such
// a symlink, so the permissions of the unrelated target have to stay exactly as they were.
fn test_making_a_cache_root_private_does_not_follow_a_replacing_symlink() {
	$if windows {
		return
	}
	directory := toolcache_test_dir('replaced_root')
	defer {
		os.rmdir_all(directory) or {}
	}
	private_file := os.join_path(directory, 'private_file')
	os.write_file(private_file, 'secret')!
	os.chmod(private_file, 0o660)!
	other_folder := os.join_path(directory, 'other_folder')
	os.mkdir(other_folder)!
	os.chmod(other_folder, 0o775)!
	for target in [private_file, other_folder] {
		before := os.stat(target)!.mode & 0o7777
		root := os.join_path(directory, 'tools')
		os.symlink(target, root)!
		make_tool_cache_root_private(root)
		os.rm(root)!
		assert os.stat(target)!.mode & 0o7777 == before, 'the repair followed a symlink to `${target}`'
	}
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
	cache := private_toolcache_test_dir('exe_name')
	defer {
		os.rmdir_all(cache) or {}
	}
	os.setenv(tool_cache_dir_env, cache, true)
	defer {
		os.unsetenv(tool_cache_dir_env)
	}
	run := os.exec([vexe, 'timeout', '60', vexe, 'version'])
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
	vlib_root := os.join_path(directory, 'vlib')
	os.mkdir_all(os.join_path(vlib_root, 'unrelated'))!
	assert unbuildable_tool_failure(entry) != none, 'an unrelated sibling module must not invalidate the failure'

	// The premise of the bug: the two fixed roots record their *direct* children, so
	// restoring a module one level deeper leaves both of them reading exactly as before.
	// Without the module's own ancestors in the manifest nothing would ever notice.
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

// A module directory can redirect its sources with `base_url` and recursively search named
// `subdirs`. Adding a source anywhere in that configured tree must retry a cached failure.
fn test_adding_a_source_under_a_vmod_subdir_invalidates_a_recorded_failure() {
	directory := toolcache_test_dir('vmod_subdir_missing_module')
	defer {
		os.rmdir_all(directory) or {}
	}
	module_dir := os.join_path(directory, 'modules', 'acme', 'widget')
	source_root := os.join_path(module_dir, 'src')
	nested_dir := os.join_path(source_root, 'parts', 'deep')
	os.mkdir_all(nested_dir)!
	module_manifest := os.join_path(module_dir, 'v.mod')
	os.write_file(module_manifest, "Module {\n\tname: 'widget'\n\tbase_url: 'src'\n\tsubdirs: ['parts']\n}\n")!
	entry_dir := os.join_path(directory, 'cache', 'vdemo-' + 'a'.repeat(64))
	entry := ToolCacheEntry{
		name:                 'vdemo'
		vroot:                directory
		dir:                  entry_dir
		unbuildable:          os.join_path(entry_dir, 'unbuildable')
		unbuildable_manifest: os.join_path(entry_dir, 'unbuildable.inputs')
		build_args:           ['-path', os.join_path(directory, 'modules')]
	}
	os.mkdir_all(entry.dir)!
	dumped := os.join_path(directory, 'sources.txt')
	os.write_file(dumped, '')!
	details := 'x.v:2:1: builder error: cannot import module "acme.widget" (not found)'
	time.sleep(1100 * time.millisecond)
	record_unbuildable_tool(entry, dumped, time.now().unix(), details)
	recorded := os.read_file(entry.unbuildable_manifest)!
	real_module_manifest := os.real_path(module_manifest)
	real_nested_dir := os.real_path(nested_dir)
	assert recorded.contains('f${tool_cache_field_separator}${real_module_manifest}${tool_cache_field_separator}')
	assert recorded.contains('d${tool_cache_field_separator}${real_nested_dir}${tool_cache_field_separator}${dir_stamp(real_nested_dir)}')
	assert unbuildable_tool_failure(entry) != none, 'the failure must stand while the source tree is empty'
	before := dir_stamp(nested_dir)

	os.write_file(os.join_path(nested_dir, 'widget.v'), 'module widget\n')!

	assert dir_stamp(nested_dir) != before, 'the recursive source stamp must see the first `.v` file'
	assert unbuildable_tool_failure(entry) == none, 'adding a source under a declared subdir must retry the build'
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

fn test_a_restored_importer_local_module_invalidates_a_recorded_failure() {
	directory := toolcache_test_dir('local_missing_module')
	defer {
		os.rmdir_all(directory) or {}
	}
	project_root := os.join_path(directory, 'project')
	tool_dir := os.join_path(project_root, 'cmd', 'demo')
	local_modules := os.join_path(tool_dir, 'modules')
	os.mkdir_all(os.join_path(local_modules, 'acme'))!
	source := os.join_path(tool_dir, 'main.v')
	os.write_file(source, 'module main\n\nimport acme.widget\n')!
	entry_dir := os.join_path(directory, 'cache', 'vdemo-' + 'a'.repeat(64))
	entry := ToolCacheEntry{
		name:                 'vdemo'
		source:               source
		vroot:                project_root
		dir:                  entry_dir
		unbuildable:          os.join_path(entry_dir, 'unbuildable')
		unbuildable_manifest: os.join_path(entry_dir, 'unbuildable.inputs')
		build_args:           ['-path', os.join_path(directory, 'explicit')]
	}
	os.mkdir_all(entry.dir)!
	dumped := os.join_path(directory, 'sources.txt')
	os.write_file(dumped, '')!
	details := '${source}:3:1: builder error: cannot import module "acme.widget" (not found)'
	record_unbuildable_tool(entry, dumped, time.now().unix(), details)
	recorded := os.read_file(entry.unbuildable_manifest)!
	assert recorded.contains(os.join_path(tool_dir, 'acme', 'widget'))
	assert recorded.contains(os.join_path(local_modules, 'acme', 'widget'))
	assert recorded.contains(os.join_path(project_root, 'acme', 'widget'))
	reason := recorded_inputs_changed(entry.unbuildable_manifest)
	assert reason == '', 'the just-recorded local-module failure is stale: ${reason}'
	assert unbuildable_tool_failure(entry) != none

	module_dir := os.join_path(local_modules, 'acme', 'widget')
	os.mkdir_all(module_dir)!
	os.write_file(os.join_path(module_dir, 'widget.v'), 'module widget\n')!
	assert unbuildable_tool_failure(entry) == none, 'restoring an importer-local module must retry the build'
}

fn test_adding_an_alias_at_a_module_prefix_invalidates_a_recorded_failure() {
	directory := toolcache_test_dir('missing_alias')
	defer {
		os.rmdir_all(directory) or {}
	}
	search_root := os.join_path(directory, 'modules')
	alias_dir := os.join_path(search_root, 'foo')
	os.mkdir_all(alias_dir)!
	canonical := os.join_path(directory, 'canonical')
	os.mkdir_all(os.join_path(canonical, 'bar'))!
	os.write_file(os.join_path(canonical, 'bar', 'bar.v'), 'module bar\n')!
	entry_dir := os.join_path(directory, 'cache', 'vdemo-' + 'a'.repeat(64))
	entry := ToolCacheEntry{
		name:                 'vdemo'
		vroot:                directory
		dir:                  entry_dir
		unbuildable:          os.join_path(entry_dir, 'unbuildable')
		unbuildable_manifest: os.join_path(entry_dir, 'unbuildable.inputs')
		build_args:           ['-path', search_root]
	}
	os.mkdir_all(entry.dir)!
	dumped := os.join_path(directory, 'sources.txt')
	os.write_file(dumped, '')!
	details := 'x.v:2:1: builder error: cannot import module "foo.bar" (not found)'
	record_unbuildable_tool(entry, dumped, time.now().unix(), details)
	alias_file := os.join_path(alias_dir, 'alias.v')
	recorded_alias_file := os.join_path(os.real_path(search_root), 'foo', 'alias.v')
	recorded := os.read_file(entry.unbuildable_manifest)!
	assert recorded.contains('f${tool_cache_field_separator}${recorded_alias_file}${tool_cache_field_separator}${file_stamp_missing}')
	assert unbuildable_tool_failure(entry) != none

	os.write_file(alias_file, "@[alias: '${canonical}'] module foo\n")!
	assert unbuildable_tool_failure(entry) == none, 'adding an alias at an ancestor prefix must retry the build'
}

fn test_restoring_an_existing_alias_target_invalidates_a_recorded_failure() {
	directory := toolcache_test_dir('missing_alias_target')
	defer {
		os.rmdir_all(directory) or {}
	}
	search_root := os.join_path(directory, 'modules')
	alias_dir := os.join_path(search_root, 'foo')
	os.mkdir_all(alias_dir)!
	project_manifest := os.join_path(directory, 'v.mod')
	os.write_file(project_manifest, "Module { name: 'alias_target_test' }\n")!
	os.write_file(os.join_path(alias_dir, 'alias.v'), "@[alias: '@VMODROOT/canonical'] module foo\n")!
	target := os.join_path(os.real_path(directory), 'canonical', 'bar')

	entry_dir := os.join_path(directory, 'cache', 'vdemo-' + 'a'.repeat(64))
	entry := ToolCacheEntry{
		name:                 'vdemo'
		vroot:                directory
		dir:                  entry_dir
		unbuildable:          os.join_path(entry_dir, 'unbuildable')
		unbuildable_manifest: os.join_path(entry_dir, 'unbuildable.inputs')
		build_args:           ['-path', search_root]
	}
	os.mkdir_all(entry.dir)!
	dumped := os.join_path(directory, 'sources.txt')
	os.write_file(dumped, '')!
	details := 'x.v:2:1: builder error: cannot import module "foo.bar" (not found)'
	time.sleep(1100 * time.millisecond)
	record_unbuildable_tool(entry, dumped, time.now().unix(), details)
	mut alias_manifests := map[string]bool{}
	assert module_alias_targets(os.real_path(search_root), 'foo.bar', mut alias_manifests) == [
		target,
	]
	assert os.real_path(project_manifest) in alias_manifests
	recorded := os.read_file(entry.unbuildable_manifest)!
	assert recorded.contains('d${tool_cache_field_separator}${target}${tool_cache_field_separator}${dir_stamp(target)}')
	assert unbuildable_tool_failure(entry) != none

	os.mkdir_all(target)!
	os.write_file(os.join_path(target, 'bar.v'), 'module bar\n')!
	assert unbuildable_tool_failure(entry) == none, 'restoring an alias target must retry module resolution'
}

// Ancestor resolution rejects a sibling module whose nearest v.mod declares another name.
// Editing that manifest can make an otherwise unchanged directory resolve successfully.
fn test_changing_an_ancestor_module_manifest_invalidates_a_recorded_failure() {
	directory := toolcache_test_dir('ancestor_module_manifest')
	defer {
		os.rmdir_all(directory) or {}
	}
	project_root := os.join_path(directory, 'importer')
	tool_dir := os.join_path(project_root, 'cmd', 'demo')
	os.mkdir_all(tool_dir)!
	os.write_file(os.join_path(project_root, 'v.mod'), "Module {\n\tname: 'importer'\n}\n")!
	source := os.join_path(tool_dir, 'main.v')
	os.write_file(source, 'module main\n\nimport foo.bar\n')!

	sibling_root := os.join_path(directory, 'foo')
	module_dir := os.join_path(sibling_root, 'bar')
	os.mkdir_all(module_dir)!
	os.write_file(os.join_path(module_dir, 'bar.v'), 'module bar\n')!
	module_manifest_path := os.join_path(sibling_root, 'v.mod')
	os.write_file(module_manifest_path, "Module {\n\tname: 'not_foo'\n}\n")!
	module_manifest := os.join_path(os.real_path(sibling_root), 'v.mod')

	entry_dir := os.join_path(directory, 'cache', 'vdemo-' + 'a'.repeat(64))
	entry := ToolCacheEntry{
		name:                 'vdemo'
		source:               source
		vroot:                project_root
		dir:                  entry_dir
		unbuildable:          os.join_path(entry_dir, 'unbuildable')
		unbuildable_manifest: os.join_path(entry_dir, 'unbuildable.inputs')
		build_args:           ['-path', os.join_path(directory, 'explicit')]
	}
	os.mkdir_all(entry.dir)!
	dumped := os.join_path(directory, 'sources.txt')
	os.write_file(dumped, '')!
	details := '${source}:3:1: builder error: cannot import module "foo.bar" (not found)'
	time.sleep(1100 * time.millisecond)
	record_unbuildable_tool(entry, dumped, time.now().unix(), details)
	recorded := os.read_file(entry.unbuildable_manifest)!
	assert vmod_manifest_inputs(module_dir) == [
		os.join_path(os.real_path(module_dir), 'v.mod'),
		module_manifest,
	]
	assert recorded.contains('f${tool_cache_field_separator}${module_manifest}${tool_cache_field_separator}${file_stamp(module_manifest)}')
	reason := recorded_inputs_changed(entry.unbuildable_manifest)
	assert reason == '', 'the just-recorded ancestor-module failure is stale: ${reason}'
	assert unbuildable_tool_failure(entry) != none

	os.write_file(module_manifest_path, "Module {\n\tname: 'foo'\n}\n")!
	assert unbuildable_tool_failure(entry) == none, 'changing the owning manifest must retry ancestor module resolution'
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
	binary := os.join_path(directory, 'vdemo' + tool_exe_suffix())
	mut build_args := [vexe, '-prod']
	$if tinyc {
		// Keep production embedding coverage when the test runner uses TinyCC.
		cc := $if windows { 'gcc' } $else { 'cc' }
		build_args << ['-cc', cc]
	}
	build := os.exec([...build_args, '-dump-files', dumped, '-o', binary, source])
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

// trusted_toolcache_test_dir creates a private folder directly in the system temporary folder.
// Default cache roots are only used when every folder above them is private too, which the
// folders of the test runner are not, when they were created with a umask like 002.
fn trusted_toolcache_test_dir(name string) ?string {
	directory := os.join_path(os.temp_dir(), 'v_toolcache_${name}_${os.getpid()}')
	os.rmdir_all(directory) or {}
	os.mkdir(directory, mode: 0o700) or { return none }
	if !tool_cache_parents_are_trusted(directory) {
		os.rmdir_all(directory) or {}
		return none
	}
	return directory
}

// with_default_tool_cache_roots points all default cache folders into `directory`, by way of
// `XDG_CACHE_HOME`, `VTMP`, and `HOME`, while `body` runs. `body` gets V`s own folder, `<directory>/xdg/v`.
fn with_default_tool_cache_roots(directory string, body fn (string) !) ! {
	names := ['XDG_CACHE_HOME', 'VTMP', 'HOME', 'USERPROFILE', tool_cache_dir_env,
		tool_cache_disable_env]
	mut previous := map[string]?string{}
	for name in names {
		previous[name] = os.getenv_opt(name)
	}
	defer {
		for name, value in previous {
			if v := value {
				os.setenv(name, v, true)
			} else {
				os.unsetenv(name)
			}
		}
	}
	xdg := os.join_path(directory, 'xdg')
	vtmp := os.join_path(directory, 'vtmp')
	home := os.join_path(directory, 'home')
	os.mkdir(xdg, mode: 0o700)!
	os.mkdir(vtmp, mode: 0o700)!
	os.mkdir(home, mode: 0o700)!
	os.setenv('XDG_CACHE_HOME', xdg, true)
	os.setenv('VTMP', vtmp, true)
	os.setenv('HOME', home, true)
	os.setenv('USERPROFILE', home, true)
	os.unsetenv(tool_cache_dir_env)
	os.unsetenv(tool_cache_disable_env)
	body(os.join_path(xdg, 'v'))!
}

// A default cache folder that others could write to may contain entries that someone else
// planted. It must be moved aside and replaced by a new, empty and private folder, and V's own
// parent folder must be made private too, so that the new folder can not be swapped later.
fn test_a_formerly_writable_default_cache_root_is_replaced_not_reused() {
	$if windows {
		return
	}
	directory := trusted_toolcache_test_dir('formerly_writable_root') or {
		eprintln('> skipping, the system temporary folder is not private enough')
		return
	}
	defer {
		os.rmdir_all(directory) or {}
	}
	with_default_tool_cache_roots(directory, fn (v_dir string) ! {
		old_root := os.join_path(v_dir, 'tools')
		os.mkdir_all(os.join_path(old_root, 'planted'))!
		os.chmod(v_dir, 0o775)!
		os.chmod(old_root, 0o775)!
		chosen := tool_cache_dir() or { panic('no usable tool cache directory') }
		assert chosen == os.real_path(old_root), 'the default location should still be used'
		assert os.ls(chosen)! == [], 'the new cache folder must start empty'
		assert os.stat(chosen)!.mode & 0o777 == 0o700
		assert os.stat(v_dir)!.mode & 0o777 == 0o755, 'V`s own parent folder must be private'
		moved := os.ls(v_dir)!.filter(it.starts_with('tools.untrusted-'))
		assert moved.len == 1, 'the old folder must be moved aside, got ${os.ls(v_dir)!}'
		assert os.is_dir(os.join_path(v_dir, moved[0], 'planted'))
		// A private folder is trusted from then on, and is reused as it is.
		assert tool_cache_dir() or { '' } == chosen
		assert os.ls(v_dir)!.filter(it.starts_with('tools.untrusted-')).len == 1
	})!
}

// A default cache folder that is a symlink may have been planted while V's own folder was
// writable by others. Its target is private, but a folder above the target is not, so someone
// else could swap the target at any time. The symlink must be moved aside, not followed.
fn test_a_symlinked_default_cache_root_is_replaced_not_followed() {
	$if windows {
		return
	}
	directory := trusted_toolcache_test_dir('symlinked_root') or {
		eprintln('> skipping, the system temporary folder is not private enough')
		return
	}
	defer {
		os.rmdir_all(directory) or {}
	}
	with_default_tool_cache_roots(directory, fn [directory] (v_dir string) ! {
		writable := os.join_path(directory, 'gw')
		victim := os.join_path(writable, 'victimdir')
		os.mkdir(writable)!
		os.chmod(writable, 0o777)!
		os.mkdir(victim, mode: 0o700)!
		os.chmod(victim, 0o700)!
		os.mkdir(v_dir)!
		os.chmod(v_dir, 0o775)!
		link := os.join_path(v_dir, 'tools')
		os.symlink(victim, link)!
		chosen := tool_cache_dir() or { panic('no usable tool cache directory') }
		assert chosen != os.real_path(victim), 'the symlinked cache folder must not be followed'
		assert !os.is_link(link)
		assert chosen == os.real_path(link), 'the default location should still be used'
		assert os.dir(chosen) == os.real_path(v_dir)
		assert os.stat(chosen)!.mode & 0o777 == 0o700
		assert os.stat(v_dir)!.mode & 0o777 == 0o755, 'V`s own parent folder must be private'
		moved := os.ls(v_dir)!.filter(it.starts_with('tools.untrusted-'))
		assert moved.len == 1, 'the symlink must be moved aside, got ${os.ls(v_dir)!}'
		assert os.is_link(os.join_path(v_dir, moved[0]))
		assert os.ls(victim)! == [], 'nothing may be written through the symlink'
	})!
}

// When V's own folder is a symlink, the folder holding that symlink must be private too, or
// someone else could point the symlink at their own folder at any time. Checking only the
// folders above the symlink's target is not enough.
fn test_a_symlinked_v_folder_inside_a_writable_cache_folder_is_not_used() {
	$if windows {
		return
	}
	directory := trusted_toolcache_test_dir('symlinked_v_folder') or {
		eprintln('> skipping, the system temporary folder is not private enough')
		return
	}
	defer {
		os.rmdir_all(directory) or {}
	}
	with_default_tool_cache_roots(directory, fn [directory] (v_dir string) ! {
		target := os.join_path(directory, 'private_v')
		os.mkdir(target, mode: 0o700)!
		os.chmod(target, 0o700)!
		os.symlink(target, v_dir)!
		cache := os.dir(v_dir)
		os.chmod(cache, 0o777)!
		chosen := tool_cache_dir() or { panic('no usable tool cache directory') }
		assert chosen == os.real_path(os.join_path(directory, 'vtmp', 'tools')), 'the symlinked V folder must not be used'
		assert os.ls(target)! == [], 'nothing may be written through the symlink'
		assert os.stat(cache)!.mode & 0o777 == 0o777, 'a folder that is not V`s own must be left as it is'
		// With a private folder holding it, the symlinked V folder is used as before.
		os.chmod(cache, 0o700)!
		assert tool_cache_dir() or { '' } == os.real_path(os.join_path(target, 'tools'))
	})!
}

// When both `~/.cache` and `VTMP` live under untrusted parents, `~/.v/tools` directly under a
// private `$HOME` provides a usable default cache without modifying `~/.cache`.
fn test_home_dot_v_tools_is_used_when_cache_and_vtmp_are_untrusted() {
	candidates := tool_cache_default_candidates()
	assert candidates.len == 4
	assert candidates[2].ends_with('.vtools')
	assert candidates[3].ends_with(os.join_path('.v', 'tools'))
	$if windows {
		return
	}
	directory := trusted_toolcache_test_dir('home_dot_v_fallback') or {
		eprintln('> skipping, the system temporary folder is not private enough')
		return
	}
	defer {
		os.rmdir_all(directory) or {}
	}
	with_default_tool_cache_roots(directory, fn [directory] (v_dir string) ! {
		cache := os.dir(v_dir)
		vtmp := os.join_path(directory, 'vtmp')
		home := os.join_path(directory, 'home')
		os.chmod(cache, 0o775)!
		os.chmod(vtmp, 0o775)!
		os.write_file(os.join_path(home, '.vtools'), 'occupied')!
		dot_v := os.join_path(home, '.v')
		os.mkdir_all(dot_v)!
		os.chmod(dot_v, 0o775)!
		chosen := tool_cache_dir() or { panic('no usable tool cache directory') }
		expected := os.real_path(os.join_path(dot_v, 'tools'))
		assert chosen == expected, 'expected `${expected}`, got `${chosen}`'
		assert os.stat(dot_v)!.mode & 0o777 == 0o755, '`~/.v` is V`s own folder and must be tightened'
		assert os.stat(chosen)!.mode & 0o777 == 0o700
		assert os.stat(cache)!.mode & 0o777 == 0o775, '`~/.cache` must be left as it is'
	})!
}

// When every candidate fails, `tool_cache_entry` lists each rejected candidate and
// shows how to configure a private `VTOOLS_CACHE_DIR`.
fn test_tool_cache_unavailable_error_lists_rejected_candidates_and_workaround() {
	formatted := tool_cache_unavailable_message([
		ToolCacheRejection{ candidate: '/home/user/.cache/v/tools', problems: ['is writable by group/others without the sticky bit'] },
		ToolCacheRejection{ candidate: '/tmp/v_1000/tools', problems: ['is owned by user id 65534, not by the current user or root'] },
	])
	assert formatted.contains('no usable tool cache folder was found:')
	assert formatted.contains('is writable by group/others without the sticky bit')
	assert formatted.contains('is owned by user id 65534, not by the current user or root')
	$if windows {
		assert formatted.contains('The tool cache folder has to belong to you, and no other account may be allowed to change it.')
		assert formatted.contains('Fix its permissions, or set VTOOLS_CACHE_DIR to a folder that only you can change.')
		return
	}
	assert formatted.contains('export VTOOLS_CACHE_DIR="\$HOME/.vtools"')
	directory := trusted_toolcache_test_dir('unavailable_diagnostics') or {
		eprintln('> skipping, the system temporary folder is not private enough')
		return
	}
	defer {
		os.rmdir_all(directory) or {}
	}
	with_default_tool_cache_roots(directory, fn [directory] (v_dir string) ! {
		cache := os.dir(v_dir)
		vtmp := os.join_path(directory, 'vtmp')
		home := os.join_path(directory, 'home')
		os.chmod(cache, 0o775)!
		os.chmod(vtmp, 0o775)!
		os.chmod(home, 0o775)!
		if entry := tool_cache_entry('v', directory, 'v3_ownership', directory, []) {
			assert false, 'expected no usable cache, got `${entry.dir}`'
		} else {
			message := err.msg()
			assert message.contains('no usable tool cache folder was found:'), message
			assert message.contains(os.join_path(v_dir, 'tools')), message
			assert message.contains(os.join_path(vtmp, 'tools')), message
			assert message.contains(os.join_path(home, '.v', 'tools')), message
			assert message.contains('without the sticky bit'), message
			assert message.contains('export VTOOLS_CACHE_DIR="\$HOME/.vtools"'), message
		}
	})!
}

// An entry planted in a formerly writable cache folder, under the exact name and with a manifest
// that looks fresh, must never be executed. The real tool must be built and reused instead.
fn test_an_entry_planted_in_a_formerly_writable_root_is_never_executed() {
	$if windows {
		return
	}
	vexe := os.real_path(@VEXE)
	if !os.is_executable(vexe) {
		eprintln('> skipping, no V executable at `${vexe}`')
		return
	}
	vroot := find_vroot(vexe) or {
		eprintln('> skipping, no V source tree for `${vexe}`')
		return
	}
	source := find_external_tool_source(os.join_path(vroot, 'cmd', 'tools', probe_tool)) or {
		panic('cannot find the `${probe_tool}` source')
	}
	directory := trusted_toolcache_test_dir('planted_entry') or {
		eprintln('> skipping, the system temporary folder is not private enough')
		return
	}
	defer {
		os.rmdir_all(directory) or {}
	}
	marker := os.join_path(directory, 'planted_entry_was_executed')
	with_default_tool_cache_roots(directory, fn [vexe, source, marker] (v_dir string) ! {
		old_root := os.join_path(v_dir, 'tools')
		os.mkdir_all(old_root)!
		os.chmod(v_dir, 0o775)!
		os.chmod(old_root, 0o775)!
		key := tool_cache_key(vexe, probe_tool, tool_key_sources(source), external_tool_build_args(probe_tool,
			[]))
		entry_dir := tool_cache_entry_dir_for_uid(old_root, probe_tool, key, os.getuid())
		os.mkdir(entry_dir)!
		planted := os.join_path(entry_dir, probe_tool)
		os.write_file(planted, '#!/bin/sh\necho planted > ${os.quoted_path(marker)}\n')!
		os.chmod(planted, 0o755)!
		os.write_file(os.join_path(entry_dir, 'inputs'), '${tool_cache_manifest_version}\nstarted${tool_cache_field_separator}1\n')!

		first := os.exec([vexe, 'timeout', '60', vexe, 'version'])
		assert first.exit_code == 0, first.output
		assert !os.exists(marker), 'the planted entry was executed'
		os.setenv(tool_cache_verbose_env, '1', true)
		defer {
			os.unsetenv(tool_cache_verbose_env)
		}
		second := os.exec([vexe, 'timeout', '60', vexe, 'version'])
		assert second.exit_code == 0, second.output
		assert second.output.contains('> reusing the cached `${probe_tool}`'), second.output
		assert !os.exists(marker), 'the planted entry was executed'
	})!
}

fn test_windows_entry_pin_blocks_rename_until_handle_is_closed() {
	$if !windows {
		return
	}
	directory := toolcache_test_dir('windows_entry_pin')
	defer { os.rmdir_all(directory) or {} }
	entry_dir := os.join_path(directory, 'vdemo-' + 'a'.repeat(64))
	os.mkdir(entry_dir)!
	entry := open_tool_cache_entry_dir(entry_dir)!
	moved_dir := entry_dir + '.moved'
	mut renamed_while_open := true
	os.rename(entry_dir, moved_dir) or { renamed_while_open = false }
	entry.close()
	assert !renamed_while_open, 'an open tool-cache entry must deny rename access'
	os.rename(entry_dir, moved_dir)!
	assert os.is_dir(moved_dir)
	assert !os.exists(entry_dir)
}

fn test_wipe_cache_options_and_selective_cleanup() {
	default_opts := parse_wipe_cache_options([], [])!
	assert default_opts.vcache && default_opts.vtmp && default_opts.v3cache && default_opts.tools
	assert !default_opts.dry_run && !default_opts.verbose && !default_opts.show_help

	selective_opts := parse_wipe_cache_options(['-v'], ['--tools', '-n'])!
	assert selective_opts.tools && !selective_opts.vcache && !selective_opts.vtmp
		&& !selective_opts.v3cache
	assert selective_opts.dry_run && selective_opts.verbose

	if _ := parse_wipe_cache_options([], ['--unknown-flag']) {
		assert false, 'expected unknown option error'
	} else {
		assert err.msg().contains('unknown option `--unknown-flag`')
	}

	directory := toolcache_test_dir('wipe_cache')
	defer {
		os.rmdir_all(directory) or {}
	}
	vcache_dir := os.join_path(directory, 'vcache')
	v3cache_dir := os.join_path(directory, 'v3cache')
	previous_vcache := os.getenv_opt('VCACHE')
	previous_v3cache := os.getenv_opt('V3CACHE')
	defer {
		if v := previous_vcache {
			os.setenv('VCACHE', v, true)
		} else {
			os.unsetenv('VCACHE')
		}
		if v := previous_v3cache {
			os.setenv('V3CACHE', v, true)
		} else {
			os.unsetenv('V3CACHE')
		}
	}
	os.setenv('VCACHE', vcache_dir, true)
	os.setenv('V3CACHE', v3cache_dir, true)

	with_default_tool_cache_roots(directory, fn [directory, vcache_dir, v3cache_dir] (_ string) ! {
		cache_v_dir := os.join_path(os.cache_dir(), 'v')
		vtmp_dir := os.join_path(directory, 'vtmp')
		home_dir := os.join_path(directory, 'home')
		cache_tools := os.join_path(cache_v_dir, 'tools')
		home_tools := os.join_path(home_dir, '.v', 'tools')
		untrusted_tools := os.join_path(cache_v_dir, 'tools.untrusted-12345')

		os.mkdir_all(vcache_dir)!
		os.write_file(os.join_path(vcache_dir, 'obj.o'), 'x')!
		os.mkdir_all(v3cache_dir)!
		os.write_file(os.join_path(v3cache_dir, 'mod.o'), 'x')!
		os.mkdir_all(os.join_path(vtmp_dir, 'v3_module_cache_deadbeef'))!
		os.write_file(os.join_path(vtmp_dir, 'keep.txt'), 'keep')!
		os.mkdir_all(cache_tools)!
		os.write_file(os.join_path(cache_tools, 'vfmt'), 'bin')!
		os.mkdir_all(home_tools)!
		os.write_file(os.join_path(home_tools, 'vvet'), 'bin')!
		os.mkdir_all(untrusted_tools)!
		os.write_file(os.join_path(untrusted_tools, 'old'), 'bin')!

		// 1. `--dry-run --verbose` reports targets and candidates without deleting anything.
		dry_opts := parse_wipe_cache_options([], ['--dry-run', '--verbose'])!
		dry_output := wipe_caches(dry_opts)!.join('\n')
		assert dry_output.contains('Tool cache candidates:'), dry_output
		assert dry_output.contains('Would wipe V cache folder ${vcache_dir}.'), dry_output
		assert dry_output.contains('Would wipe V tool cache folder ${cache_tools}.'), dry_output
		assert dry_output.contains('Would wipe Moved-aside V tool cache folder ${untrusted_tools}.'), dry_output
		assert os.is_file(os.join_path(cache_tools, 'vfmt'))
		assert os.is_file(os.join_path(home_tools, 'vvet'))
		assert os.is_file(os.join_path(untrusted_tools, 'old'))

		// 2. `--tools` wipes only tool caches and `.untrusted-*` folders.
		tools_opts := parse_wipe_cache_options([], ['--tools'])!
		tools_output := wipe_caches(tools_opts)!.join('\n')
		assert tools_output.contains('V tool cache folder ${cache_tools} was wiped.'), tools_output
		assert tools_output.contains('V tool cache folder ${home_tools} was wiped.'), tools_output
		assert tools_output.contains('Moved-aside V tool cache folder ${untrusted_tools} was wiped.'), tools_output
		assert !os.exists(cache_tools)
		assert !os.exists(home_tools)
		assert !os.exists(untrusted_tools)
		assert os.is_file(os.join_path(vcache_dir, 'obj.o'))
		assert os.is_file(os.join_path(v3cache_dir, 'mod.o'))
		assert os.is_file(os.join_path(vtmp_dir, 'keep.txt'))

		// 3. `--v3cache` with `V3CACHE` unset wipes only `v3_*` directories inside `VTMP`.
		os.unsetenv('V3CACHE')
		v3_opts := parse_wipe_cache_options([], ['--v3cache'])!
		v3_output := wipe_caches(v3_opts)!.join('\n')
		assert v3_output.contains('V3 cache folder ${os.join_path(vtmp_dir, 'v3_module_cache_deadbeef')} was wiped.'), v3_output
		assert !os.exists(os.join_path(vtmp_dir, 'v3_module_cache_deadbeef'))
		assert os.is_file(os.join_path(vtmp_dir, 'keep.txt'))

		// 4. Default `wipe-cache` wipes all remaining caches.
		os.setenv('V3CACHE', v3cache_dir, true)
		os.mkdir_all(cache_tools)!
		all_opts := parse_wipe_cache_options([], [])!
		all_output := wipe_caches(all_opts)!.join('\n')
		assert all_output.contains('V cache folder ${vcache_dir} was wiped.'), all_output
		assert all_output.contains('V tmp.c and tests folder ${vtmp_dir} was wiped.'), all_output
		assert all_output.contains('V3 cache folder ${v3cache_dir} was wiped.'), all_output
		assert all_output.contains('V tool cache folder ${cache_tools} was wiped.'), all_output
		assert !os.exists(os.join_path(vcache_dir, 'obj.o'))
		assert !os.exists(os.join_path(vtmp_dir, 'keep.txt'))
		assert !os.exists(os.join_path(v3cache_dir, 'mod.o'))
		assert !os.exists(cache_tools)
	})!
}

// This runs on every host; on Windows the copied child holds the actual image lock.
fn test_cached_updater_wipes_caches_only_after_a_successful_child_exits() ! {
	directory := toolcache_test_dir('deferred_updater_wipe')
	defer { os.rmdir_all(directory) or {} }
	mut previous := map[string]?string{}
	for name in ['VCACHE', 'V3CACHE', vup_defer_cache_wipe_env, toolcache_updater_probe_env] {
		previous[name] = os.getenv_opt(name)
	}
	defer {
		for name, value in previous {
			if v := value { os.setenv(name, v, true) } else { os.unsetenv(name) }
		}
	}
	os.unsetenv(vup_defer_cache_wipe_env)
	with_default_tool_cache_roots(directory, fn [directory] (_ string) ! {
		cache := os.join_path(directory, 'modules')
		os.setenv('VCACHE', cache, true)
		os.setenv('V3CACHE', os.join_path(directory, 'v3cache'), true)
		// In particular, VTMP cleanup must not run while its cached child is alive.
		updater := os.join_path(os.vtmp_dir(), 'tools', 'cached-updater', 'vup.exe')
		// Each mode uses the same probe; request mode removes it with the cache.
		os.mkdir_all(os.dir(updater))!
		os.cp(os.executable(), updater)!
		os.chmod(updater, 0o755)!
		for mode in ['help', 'failure', 'request'] {
			os.mkdir_all(cache)!
			marker := os.join_path(cache, 'object.o')
			os.write_file(marker, 'cached object')!
			os.setenv(toolcache_updater_probe_env, mode, true)
			code := run_windows_cached_tool(updater, [])
			assert code == if mode == 'failure' { 7 } else { 0 }
			assert os.exists(updater) == (mode != 'request')
			assert os.exists(marker) == (mode != 'request')
		}
		// A real cleanup failure must still reach the launcher's exit status.
		os.mkdir_all(os.dir(updater))!
		os.cp(os.executable(), updater)!
		os.chmod(updater, 0o755)!
		blocker := os.join_path(directory, 'not_a_directory')
		os.write_file(blocker, 'file')!
		os.setenv('VCACHE', os.join_path(blocker, 'cache'), true)
		assert run_windows_cached_tool(updater, []) == 1
		assert os.exists(updater)
		// Other cached tools retain their exit status and do not request cleanup.
		ordinary := os.join_path(directory, 'vtimeout.exe')
		os.cp(os.executable(), ordinary)!
		os.chmod(ordinary, 0o755)!
		os.setenv(toolcache_updater_probe_env, 'ordinary', true)
		assert run_windows_cached_tool(ordinary, []) == vup_deferred_wipe_exit_code
		assert os.exists(updater)
	})!
}

// hold_tool_cache_lock_until_released acts as a compiler in the middle of a tool build: it
// takes the tool's build lock, reports that, and keeps it until `release_path` appears.
@[noreturn]
fn hold_tool_cache_lock_until_released(lock_path string, release_path string) {
	mut build_lock := filelock.new_file(lock_path, mode: .exclusive)
	build_lock.acquire() or {
		eprintln(err.msg())
		exit(2)
	}
	println('locked')
	os.flush()
	deadline := time.now().add(30 * time.second)
	for time.now() < deadline && !os.exists(release_path) {
		time.sleep(10 * time.millisecond)
	}
	build_lock.release()
	exit(0)
}

// start_tool_cache_lock_holder runs the lock holder above in a child process and returns
// once the child reports that it holds `lock_path`. The child lets go when `release_path`
// appears; keep that outside any directory the test wipes.
fn start_tool_cache_lock_holder(lock_path string, release_path string) !&os.Process {
	mut p := os.new_process(os.executable())
	mut env := os.environ()
	env[toolcache_lock_holder_env] = lock_path
	env[toolcache_lock_release_env] = release_path
	p.set_environment(env)
	p.set_redirect_stdio()
	p.run()
	mut output := ''
	deadline := time.now().add(10 * time.second)
	for time.now() < deadline {
		output += p.stdout_read()
		if output.contains('locked') {
			return p
		}
		if !p.is_alive() {
			break
		}
		time.sleep(10 * time.millisecond)
	}
	p.signal_kill()
	p.wait()
	p.close()
	return error('the lock holder did not take ${lock_path}: ${output}')
}

// with_wipe_test_caches points every folder that `v wipe-cache` touches into `directory` while
// `body` runs: the default tool cache folders (see `with_default_tool_cache_roots`), `VCACHE` and
// `V3CACHE`. `body` gets V's own cache folder, `<directory>/xdg/v`.
fn with_wipe_test_caches(directory string, body fn (string) !) ! {
	mut previous := map[string]?string{}
	for name in ['VCACHE', 'V3CACHE'] {
		previous[name] = os.getenv_opt(name)
	}
	defer {
		for name, value in previous {
			if v := value {
				os.setenv(name, v, true)
			} else {
				os.unsetenv(name)
			}
		}
	}
	os.setenv('VCACHE', os.join_path(directory, 'vcache'), true)
	os.setenv('V3CACHE', os.join_path(directory, 'v3cache'), true)
	with_default_tool_cache_roots(directory, body)!
}

// wipe_output runs `v wipe-cache` with `arguments` in this process, and returns what it prints.
fn wipe_output(arguments ...string) !string {
	return wipe_caches(parse_wipe_cache_options([], arguments)!)!.join('\n')
}

// tool_cache_test_entry makes a cache entry of `tool` for this user in the tool cache folder
// `root`, with the tool's executable in it, and returns the entry's folder. `key` is one hex digit.
fn tool_cache_test_entry(root string, tool string, key string) !string {
	entry := os.join_path(root, '${tool}-${key.repeat(64)}.${os.getuid()}')
	os.mkdir_all(entry)!
	os.write_file(os.join_path(entry, tool + tool_exe_suffix()), 'bin')!
	return entry
}

// tool_cache_test_root makes the tool cache folder `root`, private to this user whatever the umask
// is, as the tool cache makes its own folders.
fn tool_cache_test_root(root string) ! {
	os.mkdir_all(root)!
	os.chmod(root, 0o700)!
}

// tool_cache_test_lock returns the path of this user's build lock of `tool` in `root`.
fn tool_cache_test_lock(root string, tool string) string {
	return os.join_path(root, tool_cache_lock_name(tool, u32(os.getuid())))
}

// toolcache_test_tree lists everything below `folder`, without following links.
fn toolcache_test_tree(folder string) []string {
	mut found := []string{}
	for name in os.ls(folder) or { []string{} } {
		path := os.join_path(folder, name)
		found << path
		if os.is_dir(path) && !os.is_link(path) {
			found << toolcache_test_tree(path)
		}
	}
	found.sort()
	return found
}

// toolcache_test_detail_paths returns the paths in the detail lines of a wipe's verbose output
// that start with `prefix`, such as `  removing `.
fn toolcache_test_detail_paths(output string, prefix string) []string {
	mut paths := []string{}
	for line in output.split_into_lines() {
		if line.starts_with(prefix) {
			paths << line[prefix.len..]
		}
	}
	paths.sort()
	return paths
}

fn test_wipe_cache_leaves_the_tools_being_built_alone() ! {
	directory := toolcache_test_dir('wipe_cache_busy')
	defer {
		os.rmdir_all(directory) or {}
	}
	release_path := os.join_path(directory, 'lock.release')
	with_wipe_test_caches(directory, fn [release_path] (cache_v_dir string) ! {
		tools := os.join_path(cache_v_dir, 'tools')
		tool_cache_test_root(tools)!
		busy_entry := tool_cache_test_entry(tools, 'vtest', 'a')!
		staged_binary := os.join_path(busy_entry, '${tool_cache_stage_prefix}0-1-2-0', 'vtest' +
			tool_exe_suffix())
		os.mkdir_all(os.dir(staged_binary))!
		os.write_file(staged_binary, 'half built')!
		idle_entry := tool_cache_test_entry(tools, 'vfmt', 'b')!
		// `v test-all` is a different tool whose name starts with the busy one.
		prefixed_entry := tool_cache_test_entry(tools, 'vtest-all', 'c')!
		prefixed_lock := tool_cache_test_lock(tools, 'vtest-all')
		// An entry from before entries carried a user id is guarded by this user's lock as well.
		legacy_entry := os.join_path(tools, 'vtest-' + 'd'.repeat(64))
		os.mkdir_all(legacy_entry)!
		// The entry of another user is guarded by no lock of this user: in a folder that is this
		// user's alone it goes like any other content, and no lock file is made for it.
		foreign_entry := os.join_path(tools, 'vdoc-' + 'e'.repeat(64) + '.${os.getuid() + 1}')
		os.mkdir_all(foreign_entry)!
		busy_lock := tool_cache_test_lock(tools, 'vtest')
		idle_lock := tool_cache_test_lock(tools, 'vfmt')
		// A folder that is merely named like a lock file is ordinary content.
		lock_shaped_dir := tool_cache_test_lock(tools, 'vfoo')
		os.mkdir_all(lock_shaped_dir)!
		os.write_file(busy_lock, '')!
		os.write_file(idle_lock, '')!

		mut holder := start_tool_cache_lock_holder(busy_lock, release_path)!
		defer {
			os.write_file(release_path, '') or {}
			holder.wait()
			holder.close()
		}
		dry := wipe_output('--tools', '--dry-run', '--verbose')!
		assert dry.contains('Would wipe V tool cache folder ${tools}, except for the tools another process has locked right now: vtest.'), dry
		assert dry.contains('  would keep ${busy_entry}'), dry
		assert dry.contains('  would keep ${legacy_entry}'), dry
		assert dry.contains('  would remove ${idle_entry}'), dry
		assert dry.contains('  would remove ${foreign_entry}'), dry
		assert os.is_dir(idle_entry)
		// A dry run makes no lock file.
		assert !os.exists(prefixed_lock)

		output := wipe_output('--tools', '--verbose')!
		assert output.contains('V tool cache folder ${tools} was wiped, except for the tools another process has locked right now: vtest.'), output
		assert output.contains('  keeping ${busy_entry}'), output
		assert output.contains('  keeping ${busy_lock}'), output
		assert output.contains('  removing ${idle_entry}'), output
		assert os.is_file(staged_binary)
		assert os.is_dir(legacy_entry), output
		assert !os.exists(idle_entry)
		assert !os.exists(prefixed_entry)
		assert !os.exists(foreign_entry), output
		assert !os.exists(tool_cache_test_lock(tools, 'vdoc')), output
		assert !os.exists(lock_shaped_dir)
		// Lock files are persistent mutexes and stay, whether held or not. A tool that had none got
		// one before its entry went.
		assert os.is_file(busy_lock)
		assert os.is_file(idle_lock)
		assert os.is_file(prefixed_lock)

		// Once the build is over, nothing holds the lock and the entry goes too; the lock files
		// keep the folder in place.
		os.write_file(release_path, '')!
		holder.wait()
		released_output := wipe_output('--tools')!
		assert released_output.contains('V tool cache folder ${tools} was wiped.'), released_output
		assert !os.exists(busy_entry)
		assert !os.exists(legacy_entry)
		assert os.is_file(busy_lock)
		assert os.is_dir(tools)
	})!
}

// A tool cache folder that `VTOOLS_CACHE_DIR` puts inside `VTMP` is wiped as a tool cache folder by
// the wipe of `VTMP` too, with the default options and with `--vtmp` alone.
fn test_wipe_cache_keeps_a_live_build_in_a_configured_folder_inside_vtmp() ! {
	directory := toolcache_test_dir('wipe_cache_vtmp_root')
	defer {
		os.rmdir_all(directory) or {}
	}
	release_path := os.join_path(directory, 'lock.release')
	with_wipe_test_caches(directory, fn [directory, release_path] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		custom := os.join_path(vtmp, 'custom-tools')
		os.setenv(tool_cache_dir_env, custom, true)
		tool_cache_test_root(custom)!
		busy_entry := tool_cache_test_entry(custom, 'vtest', 'a')!
		staged_binary := os.join_path(busy_entry, '${tool_cache_stage_prefix}0-1-2-0', 'vtest')
		os.mkdir_all(os.dir(staged_binary))!
		os.write_file(staged_binary, 'half built')!
		busy_lock := tool_cache_test_lock(custom, 'vtest')
		os.write_file(busy_lock, '')!
		mut holder := start_tool_cache_lock_holder(busy_lock, release_path)!
		defer {
			os.write_file(release_path, '') or {}
			holder.wait()
			holder.close()
		}
		for arguments in [[]string{}, ['--vtmp']] {
			idle_entry := tool_cache_test_entry(custom, 'vfmt', 'b')!
			temporary := os.join_path(vtmp, 'a.tmp.c')
			session := os.join_path(vtmp, 'tsession_1')
			os.write_file(temporary, 'c')!
			os.mkdir_all(os.join_path(session, 'deeper'))!
			os.write_file(os.join_path(session, 'deeper', 'file'), 'x')!
			output := wipe_output(...arguments)!
			assert os.is_file(staged_binary), output
			assert os.is_file(busy_lock), output
			assert !os.exists(idle_entry), output
			assert !os.exists(temporary), output
			assert !os.exists(session), output
			assert output.contains('V tool cache folder ${custom} was wiped, except for the tools another process has locked right now: vtest.'), output
			assert output.contains('V tmp.c and tests folder ${vtmp} was wiped, except the tool cache folders in it: ${custom}.'), output
		}
	})!
}

// The tool cache folders a wipe knows are known by their paths, as deep as they lie, and whether
// they exist yet or not.
fn test_known_tool_cache_folders_are_found_below_a_folder_by_path() ! {
	directory := toolcache_test_dir('wipe_cache_roots_below')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		deep := os.join_path(vtmp, 'a', 'b', 'tools')
		os.setenv(tool_cache_dir_env, deep, true)
		run := new_tool_cache_wipe_run()
		below := run.roots_below(vtmp)
		assert os.join_path('a', 'b', 'tools') in below
		assert 'tools' in below
		assert run.roots_below(os.join_path(vtmp, 'a')) == [os.join_path('b', 'tools')]
		assert run.roots_below(deep) == ['']
		assert run.roots_below(os.join_path(directory, 'unrelated')).len == 0

		// The folders that lead to such a folder stay, and what lies beside them goes.
		entry := tool_cache_test_entry(deep, 'vfmt', 'a')!
		lock_path := tool_cache_test_lock(deep, 'vfmt')
		os.write_file(lock_path, '')!
		os.write_file(os.join_path(vtmp, 'a', 'beside'), 'x')!
		os.mkdir_all(os.join_path(vtmp, 'a', 'b', 'beside', 'deeper'))!
		dry := wipe_output('--vtmp', '--dry-run', '--verbose')!
		assert dry.contains('Would wipe V tmp.c and tests folder ${vtmp}, except ${deep}.'), dry
		assert dry.contains('  would remove ${os.join_path(vtmp, 'a', 'beside')}'), dry
		output := wipe_output('--vtmp')!
		assert output.contains('V tmp.c and tests folder ${vtmp} was wiped, except the tool cache folders in it: ${deep}.'), output
		assert !os.exists(entry)
		assert os.is_file(lock_path)
		assert !os.exists(os.join_path(vtmp, 'a', 'beside'))
		assert !os.exists(os.join_path(vtmp, 'a', 'b', 'beside'))
	})!
}

// On a filesystem that ignores case, a tool cache folder is known by the spelling that a listing
// of its parent gives, whatever spelling `VTOOLS_CACHE_DIR` uses.
fn test_a_known_tool_cache_folder_is_spelled_as_its_parent_lists_it() ! {
	directory := toolcache_test_dir('wipe_cache_spelling')
	defer {
		os.rmdir_all(directory) or {}
	}
	os.mkdir(os.join_path(directory, 'CaseProbe'))!
	if !os.exists(os.join_path(directory, 'caseprobe')) {
		// This filesystem tells the two spellings apart, so a name that is not listed is not there.
		assert tool_cache_spelled_as_listed(directory, 'caseprobe') == 'caseprobe'
		return
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		listed := os.join_path(vtmp, 'Deep', 'Custom-Tools')
		tool_cache_test_root(listed)!
		// A folder that a listing gives first is not mistaken for it.
		os.mkdir_all(os.join_path(vtmp, 'Deep', 'Another'))!
		assert tool_cache_spelled_as_listed(vtmp, os.join_path('deep', 'custom-tools')) == os.join_path('Deep',
			'Custom-Tools')
		// A part that is not there yet keeps the spelling it was given.
		assert tool_cache_spelled_as_listed(vtmp, os.join_path('deep', 'missing')) == os.join_path('Deep',
			'missing')
		os.setenv(tool_cache_dir_env, os.join_path(vtmp, 'deep', 'custom-tools'), true)
		entry := tool_cache_test_entry(listed, 'vfmt', 'a')!
		lock_path := tool_cache_test_lock(listed, 'vfmt')
		os.write_file(lock_path, '')!
		output := wipe_output('--vtmp')!
		// It was wiped as the tool cache folder it is, so its lock file is still there.
		assert !os.exists(entry), output
		assert os.is_file(lock_path), output
	})!
}

// A tool cache folder that only the `VTOOLS_CACHE_DIR` of another process names is not known to
// the wipe: it goes with the folder it lies in, as it always has.
fn test_a_tool_cache_folder_the_wipe_does_not_know_is_ordinary_content() ! {
	directory := toolcache_test_dir('wipe_cache_unknown_root')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		unknown := os.join_path(vtmp, 'tools-of-another-process')
		tool_cache_test_root(unknown)!
		tool_cache_test_entry(unknown, 'vfmt', 'a')!
		os.write_file(tool_cache_test_lock(unknown, 'vfmt'), '')!
		// So is a folder named like one that V moved aside, beside no folder the wipe knows.
		stranger := os.join_path(vtmp, 'elsewhere.untrusted-kept')
		tool_cache_test_root(stranger)!
		os.write_file(tool_cache_test_lock(stranger, 'vfmt'), '')!
		output := wipe_output('--vtmp')!
		assert output.contains('V tmp.c and tests folder ${vtmp} was wiped.'), output
		assert !os.exists(unknown)
		assert !os.exists(stranger), output
		assert !output.contains(stranger), output
	})!
}

// A tool cache folder inside another one is wiped as a tool cache folder, and once.
fn test_a_known_tool_cache_folder_inside_another_is_wiped_as_one() ! {
	directory := toolcache_test_dir('wipe_cache_nested_roots')
	defer {
		os.rmdir_all(directory) or {}
	}
	release_path := os.join_path(directory, 'lock.release')
	with_wipe_test_caches(directory, fn [release_path] (cache_v_dir string) ! {
		outer := os.join_path(cache_v_dir, 'tools')
		inner := os.join_path(outer, 'nested', 'tools')
		os.setenv(tool_cache_dir_env, inner, true)
		tool_cache_test_root(inner)!
		busy_entry := tool_cache_test_entry(inner, 'vtest', 'a')!
		busy_lock := tool_cache_test_lock(inner, 'vtest')
		os.write_file(busy_lock, '')!
		inner_idle := tool_cache_test_entry(inner, 'vfmt', 'b')!
		outer_idle := tool_cache_test_entry(outer, 'vdoc', 'c')!
		beside := os.join_path(outer, 'nested', 'beside')
		os.write_file(beside, 'x')!
		mut holder := start_tool_cache_lock_holder(busy_lock, release_path)!
		defer {
			os.write_file(release_path, '') or {}
			holder.wait()
			holder.close()
		}
		output := wipe_output('--tools')!
		assert os.is_dir(busy_entry), output
		assert os.is_file(busy_lock), output
		assert !os.exists(inner_idle), output
		assert !os.exists(outer_idle), output
		assert !os.exists(beside), output
		inner_line := 'V tool cache folder ${inner} was wiped, except for the tools another process has locked right now: vtest.'
		assert output.contains(inner_line), output
		assert output.count(inner_line) == 1, output
	})!
}

// A build that moves the folder aside while the wipe runs, and builds in a new folder at the same
// path, leaves the wipe in the folder it opened: nothing in the new folder is touched.
fn test_a_wipe_stays_in_the_folder_it_opened() ! {
	$if windows {
		// No build moves a tool cache folder aside on Windows.
		return
	}
	directory := toolcache_test_dir('wipe_cache_moved_meanwhile')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn (cache_v_dir string) ! {
		tools := os.join_path(cache_v_dir, 'tools')
		tool_cache_test_root(tools)!
		entry := tool_cache_test_entry(tools, 'vfmt', 'a')!
		entry_name := os.file_name(entry)
		lock_name := tool_cache_lock_name('vfmt', u32(os.getuid()))
		opts := parse_wipe_cache_options([], ['--tools'])!
		mut run := new_tool_cache_wipe_run()
		dir := open_tool_cache_wipe_dir(tools)!
		defer {
			dir.close()
		}
		// Now a build moves the folder aside, and builds the same tool in a new one at its path.
		aside := tools + '.untrusted-1700000000-42'
		os.rename(tools, aside)!
		os.mkdir(tools, mode: 0o700)!
		new_entry := tool_cache_test_entry(tools, 'vfmt', 'a')!
		assert new_entry == entry
		mut lines := []string{}
		run.wipe_open_tool_cache_directory(dir, tools, opts, mut lines)!
		output := lines.join('\n')
		// The entry of the folder that was opened is gone, under a lock taken in that folder.
		assert !os.exists(os.join_path(aside, entry_name)), output
		assert os.is_file(os.join_path(aside, lock_name)), output
		// The new folder has all it had, and got no lock file from the wipe.
		assert os.is_file(os.join_path(new_entry, 'vfmt' + tool_exe_suffix())), output
		assert os.ls(tools)! == [entry_name], output
		assert output.contains('V tool cache folder ${tools} was moved aside by a build during the wipe.'), output
		assert output.contains('the new folder at ${tools} was not touched.'), output
	})!
}

fn test_lock_names_are_taken_apart_exactly() {
	plain := tool_cache_lock_name_parts('.vfmt.1000.toolcache.lock') or {
		panic('a lock name was not recognised')
	}
	assert plain.tool == 'vfmt'
	assert plain.uid == 1000
	dotted := tool_cache_lock_name_parts('.v.fmt.0.toolcache.lock') or {
		panic('a lock name with a dot in the tool was not recognised')
	}
	assert dotted.tool == 'v.fmt'
	assert dotted.uid == 0
	shortest := tool_cache_lock_name_parts('.a.7.toolcache.lock') or {
		panic('the shortest lock name was not recognised')
	}
	assert shortest.tool == 'a'
	assert shortest.uid == 7
	largest := tool_cache_lock_name_parts('.vfmt.4294967295.toolcache.lock') or {
		panic('the largest user id was not recognised')
	}
	assert largest.uid == 4294967295
	assert tool_cache_lock_name('vfmt', 1000) == '.vfmt.1000.toolcache.lock'
	// Names that only resemble a lock name: no tool, no user id, a user id no build writes.
	for name in ['.toolcache.lock', '..toolcache.lock', '.vfmt.toolcache.lock', '..7.toolcache.lock',
		'.vfmt..toolcache.lock', '.vfmt.x7.toolcache.lock', '.vfmt.7x.toolcache.lock',
		'.vfmt.-7.toolcache.lock', '.vfmt.007.toolcache.lock', '.vfmt.4294967296.toolcache.lock',
		'.vfmt.99999999999.toolcache.lock', 'vfmt.7.toolcache.lock', '.vfmt.7.toolcache.lock.bak',
		'.vfmt.7.toolcache.locks', '.vfmt.7.toolcache', '', 
		// A sign that no number follows, a second sign, a plus sign.
		'.vfmt.-.toolcache.lock', '.vfmt.--7.toolcache.lock', '.vfmt.+7.toolcache.lock',
		// An empty tool before a user id, in a name long enough to hold both.
		'..1000.toolcache.lock', 
		// Another ending of the same length, after a tool and a user id.
		'.vfmt.7' + 'x'.repeat(tool_cache_lock_ending.len)] {
		if _ := tool_cache_lock_name_parts(name) {
			assert false, '`${name}` is no lock name'
		}
	}
}

fn test_entry_names_are_taken_apart_exactly() {
	hex := 'a'.repeat(64)
	entry := tool_cache_entry_name_parts('vfmt-${hex}.1000') or { panic('an entry name was not recognised') }
	assert entry.tool == 'vfmt'
	assert entry.has_uid
	assert entry.uid == 1000
	// A tool name can hold dashes; the content address decides where it ends.
	dashed := tool_cache_entry_name_parts('vtest-all-${hex}.0') or {
		panic('an entry name with a dash in the tool was not recognised')
	}
	assert dashed.tool == 'vtest-all'
	assert dashed.has_uid
	assert dashed.uid == 0
	// A legacy entry, a sidecar file, and a user id no build writes carry no user id.
	for name in ['vfmt-${hex}', 'vfmt-${hex}.inputs', 'vfmt-${hex}.007', 'vfmt-${hex}.-7',
		'vfmt-${hex}.4294967296', 'vfmt-${hex}.'] {
		plain := tool_cache_entry_name_parts(name) or { panic('`${name}` was not recognised') }
		assert plain.tool == 'vfmt', name
		assert !plain.has_uid, name
	}
	for name in ['vfmt', 'vfmt-', 'vfmt-${hex[..63]}', '-${hex}', 'vfmt-${hex}x', 'vfmt_${hex}',
		'vfmt-' + 'g'.repeat(64), ''] {
		if _ := tool_cache_entry_name_parts(name) {
			assert false, '`${name}` is no entry name'
		}
	}
}

// The wipe spells a lock file and an entry exactly as a build does, and reads both names back to
// the tool and the user they belong to: for the user who runs the tests, and for every user id
// below 2^31 (the tool cache works for no other, see `tool_cache_lock_name`).
fn test_the_wipe_names_locks_and_entries_as_a_build_does() {
	hex := 'a'.repeat(64)
	own := ToolCacheEntry{
		name: 'vfmt'
		dir:  tool_cache_entry_dir_for_uid('root', 'vfmt', hex, os.getuid())
	}
	assert tool_cache_lock_name(own.name, u32(os.getuid())) == os.file_name(tool_cache_lock_path(own))
	for uid in [0, 1000, 65534, int(max_i32)] {
		entry := ToolCacheEntry{
			name: 'vtest-all'
			dir:  tool_cache_entry_dir_for_uid('root', 'vtest-all', hex, uid)
		}
		lock_name := os.file_name(tool_cache_lock_path_for_uid(entry, uid))
		assert tool_cache_lock_name(entry.name, u32(uid)) == lock_name
		lock_parts := tool_cache_lock_name_parts(lock_name) or {
			panic('the lock name `${lock_name}` of a build was not recognised')
		}
		assert lock_parts.tool == entry.name, lock_name
		assert lock_parts.uid == u32(uid), lock_name
		entry_name := os.file_name(entry.dir)
		entry_parts := tool_cache_entry_name_parts(entry_name) or {
			panic('the entry name `${entry_name}` of a build was not recognised')
		}
		assert entry_parts.tool == entry.name, entry_name
		assert entry_parts.has_uid, entry_name
		assert entry_parts.uid == u32(uid), entry_name
	}
}

// What a wipe removes outside Windows follows one rule, from who runs it, whose folder it is and
// who can write to that folder.
fn test_a_wipe_takes_what_is_its_own_to_take() {
	me := u32(1000)
	// In a folder that is this user's alone, everything goes, whoever owns it.
	for owner in [me, u32(0), u32(65534)] {
		assert tool_cache_wipe_takes_posix(me, me, 0o700, owner)
		assert tool_cache_wipe_takes_posix(me, me, 0o755, owner)
	}
	// In a folder that another account can write to, only what this user owns goes, with the
	// sticky bit or without it.
	for mode in [u32(0o777), u32(0o1777), u32(0o775), u32(0o757), u32(0o1775), u32(0o720)] {
		assert tool_cache_wipe_takes_posix(me, me, mode, me)
		assert !tool_cache_wipe_takes_posix(me, me, mode, 0)
		assert !tool_cache_wipe_takes_posix(me, me, mode, 65534)
	}
	// So it is in a folder that another account owns.
	assert tool_cache_wipe_takes_posix(me, 0, 0o1777, me)
	assert !tool_cache_wipe_takes_posix(me, 0, 0o755, 0)
	assert !tool_cache_wipe_takes_posix(me, 2000, 0o700, 2000)
	// The superuser takes everything, everywhere.
	for owner in [u32(0), me, u32(65534)] {
		assert tool_cache_wipe_takes_posix(0, me, 0o700, owner)
		assert tool_cache_wipe_takes_posix(0, me, 0o1777, owner)
		assert tool_cache_wipe_takes_posix(0, 0, 0o755, owner)
	}
}

// A lock file that cannot be opened is set to mode 0600 only when it is a regular file that no
// other account can swap for something else meanwhile.
fn test_a_lock_file_is_repaired_only_where_nobody_else_can_swap_it() {
	me := u32(1000)
	file := ToolCacheWipeItem{
		kind:  .file
		owner: me
		links: 1
	}
	assert tool_cache_lock_is_repairable(me, me, 0o700, file)
	for kind in [ToolCacheWipeKind.gone, .folder, .link, .other] {
		assert !tool_cache_lock_is_repairable(me, me, 0o700, ToolCacheWipeItem{
			kind:  kind
			owner: me
			links: 1
		}), kind.str()
	}
	$if !windows {
		// The folder is this user's or root's, and others cannot write to it, or only with the
		// sticky bit.
		assert tool_cache_lock_is_repairable(me, me, 0o755, file)
		assert tool_cache_lock_is_repairable(me, me, 0o1777, file)
		assert tool_cache_lock_is_repairable(me, 0, 0o1777, file)
		assert !tool_cache_lock_is_repairable(me, 2000, 0o700, file)
		assert !tool_cache_lock_is_repairable(me, 2000, 0o1777, file)
		for mode in [u32(0o777), u32(0o770), u32(0o702), u32(0o720)] {
			assert !tool_cache_lock_is_repairable(me, me, mode, file), mode.str()
			assert !tool_cache_lock_is_repairable(me, 0, mode, file), mode.str()
		}
		// The file is this user's and has one name.
		assert !tool_cache_lock_is_repairable(me, me, 0o700, ToolCacheWipeItem{
			kind:  .file
			owner: 2000
			links: 1
		})
		assert !tool_cache_lock_is_repairable(me, me, 0o700, ToolCacheWipeItem{
			kind:  .file
			owner: me
			links: 2
		})
		// The superuser repairs the file of any user, in a folder that is root's.
		assert tool_cache_lock_is_repairable(0, 0, 0o755, ToolCacheWipeItem{
			kind:  .file
			owner: 2000
			links: 1
		})
		assert !tool_cache_lock_is_repairable(0, 2000, 0o700, ToolCacheWipeItem{
			kind:  .file
			owner: 2000
			links: 1
		})
	}
}

fn test_wipe_cache_tells_lock_files_from_other_names() ! {
	directory := toolcache_test_dir('wipe_cache_names')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn (cache_v_dir string) ! {
		tools := os.join_path(cache_v_dir, 'tools')
		tool_cache_test_root(tools)!
		// The bare ending holds neither a tool nor a user id; it used to crash the wipe.
		others := ['.toolcache.lock', '..toolcache.lock', '.vfmt.toolcache.lock',
			'.vfmt.007.toolcache.lock', '.vfmt.x.toolcache.lock', 'plain']
		for name in others {
			os.write_file(os.join_path(tools, name), '')!
		}
		// The lock file of another user stays, like every lock file: unlinking one would let two
		// builds lock two different files.
		foreign_lock := os.join_path(tools, tool_cache_lock_name('vfmt', u32(os.getuid()) + 1))
		os.write_file(foreign_lock, '')!
		own_lock := tool_cache_test_lock(tools, 'vdoc')
		os.write_file(own_lock, '')!
		output := wipe_output('--tools', '--verbose')!
		for name in others {
			assert !os.exists(os.join_path(tools, name)), '${name}: ${output}'
		}
		assert os.is_file(foreign_lock), output
		assert os.is_file(own_lock), output
		assert output.contains('  keeping ${foreign_lock}'), output
		assert output.contains('  keeping ${own_lock}'), output
		assert output.contains('V tool cache folder ${tools} was wiped.'), output
	})!
}

fn test_wipe_cache_repairs_a_lock_file_that_cannot_be_opened() ! {
	$if !windows {
		if os.geteuid() == 0 {
			// The superuser opens a read-only file for writing anyway, so nothing needs a repair.
			return
		}
	}
	directory := toolcache_test_dir('wipe_cache_repair')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn (cache_v_dir string) ! {
		tools := os.join_path(cache_v_dir, 'tools')
		tool_cache_test_root(tools)!
		lock_path := tool_cache_test_lock(tools, 'vfmt')
		// A file that can only be read, one that can only be written, and one that cannot be opened
		// at all: each is repaired another way.
		modes := $if windows {
			[0o400]
		} $else {
			[0o400, 0o200, 0o000]
		}
		for mode in modes {
			entry := tool_cache_test_entry(tools, 'vfmt', 'a')!
			os.write_file(lock_path, '')!
			os.chmod(lock_path, mode)!
			defer {
				os.chmod(lock_path, 0o600) or {}
			}
			as_it_was := os.stat(lock_path)!.mode
			// A dry run tells what a real run does, and leaves the file as it is.
			dry := wipe_output('--tools', '--dry-run', '--verbose')!
			assert dry.contains('Would wipe V tool cache folder ${tools}.'), '${mode:o}: ${dry}'
			assert dry.contains('  would remove ${entry}'), '${mode:o}: ${dry}'
			assert os.stat(lock_path)!.mode == as_it_was, '${mode:o}'
			assert os.is_dir(entry)

			output := wipe_output('--tools')!
			assert output.contains('V tool cache folder ${tools} was wiped.'), '${mode:o}: ${output}'
			assert !os.exists(entry), '${mode:o}'
			assert os.is_file(lock_path)
			assert os.is_writable(lock_path), '${mode:o}'
			// A build can lock it again.
			mut build_lock := filelock.new_file(lock_path, mode: .exclusive)
			assert build_lock.try_acquire(), '${mode:o}'
			build_lock.release()
		}
	})!
}

// A lock file that cannot be opened and has a second name is left as it is: a change of its mode
// would reach a file outside the wipe. The entries of its tool stay.
fn test_a_lock_file_with_a_second_name_is_left_as_it_is() ! {
	$if windows {
		return
	}
	if os.geteuid() == 0 {
		return
	}
	directory := toolcache_test_dir('wipe_cache_no_repair')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (cache_v_dir string) ! {
		tools := os.join_path(cache_v_dir, 'tools')
		tool_cache_test_root(tools)!
		entry := tool_cache_test_entry(tools, 'vfmt', 'a')!
		idle_entry := tool_cache_test_entry(tools, 'vdoc', 'b')!
		lock_path := tool_cache_test_lock(tools, 'vfmt')
		os.write_file(lock_path, '')!
		os.chmod(lock_path, 0o400)!
		defer {
			os.chmod(lock_path, 0o600) or {}
		}
		os.link(lock_path, os.join_path(directory, 'second-name'))!
		for arguments in [['--tools', '--dry-run'], ['--tools']] {
			output := wipe_output(...arguments)!
			assert output.contains('except for the tools whose lock file cannot be opened and was left as it is: vfmt.'), output
			assert os.is_dir(entry), output
			assert !os.is_writable(lock_path), output
		}
		assert !os.exists(idle_entry)
	})!
}

// What is in the way of a lock file is removed as it is, without going through a link, and the
// tool then gets a real lock file before its entry goes.
fn test_a_link_at_a_lock_path_is_removed_and_not_followed() ! {
	$if windows {
		return
	}
	directory := toolcache_test_dir('wipe_cache_lock_link')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (cache_v_dir string) ! {
		tools := os.join_path(cache_v_dir, 'tools')
		tool_cache_test_root(tools)!
		entry := tool_cache_test_entry(tools, 'vfmt', 'a')!
		target := os.join_path(directory, 'elsewhere')
		os.write_file(target, 'keep')!
		os.chmod(target, 0o400)!
		lock_path := tool_cache_test_lock(tools, 'vfmt')
		os.symlink(target, lock_path)!
		dry := wipe_output('--tools', '--dry-run', '--verbose')!
		assert dry.contains('  would remove ${lock_path}'), dry
		assert dry.contains('  would remove ${entry}'), dry
		assert os.is_link(lock_path)
		output := wipe_output('--tools', '--verbose')!
		assert output.contains('  removing ${lock_path}'), output
		assert !os.exists(entry), output
		assert !os.is_link(lock_path), output
		assert os.is_file(lock_path), output
		assert os.read_file(target)! == 'keep'
		assert !os.is_writable(target)
	})!
}

// A folder V moved aside is wiped like any tool cache folder while a build holds a lock in it, and
// goes whole, lock files included, once none does.
fn test_a_folder_v_moved_aside_goes_whole_once_no_build_holds_a_lock_in_it() ! {
	directory := toolcache_test_dir('wipe_cache_moved_aside')
	defer {
		os.rmdir_all(directory) or {}
	}
	release_path := os.join_path(directory, 'lock.release')
	with_wipe_test_caches(directory, fn [release_path] (cache_v_dir string) ! {
		aside := os.join_path(cache_v_dir, 'tools.untrusted-1700000000-42')
		tool_cache_test_root(aside)!
		assert is_moved_aside_tool_cache_root(aside)
		busy_entry := tool_cache_test_entry(aside, 'vtest', 'a')!
		idle_entry := tool_cache_test_entry(aside, 'vfmt', 'b')!
		busy_lock := tool_cache_test_lock(aside, 'vtest')
		idle_lock := tool_cache_test_lock(aside, 'vfmt')
		os.write_file(busy_lock, '')!
		os.write_file(idle_lock, '')!
		planted := os.join_path(aside, 'planted')
		os.write_file(planted, 'x')!
		mut holder := start_tool_cache_lock_holder(busy_lock, release_path)!
		defer {
			os.write_file(release_path, '') or {}
			holder.wait()
			holder.close()
		}
		output := wipe_output('--tools')!
		assert output.contains('Moved-aside V tool cache folder ${aside} was wiped, except for the tools another process has locked right now: vtest.'), output
		assert os.is_dir(busy_entry), output
		assert os.is_file(busy_lock), output
		assert !os.exists(idle_entry), output
		assert !os.exists(planted), output
		// While something is kept, the lock files stay too.
		assert os.is_file(idle_lock), output
		// A folder with the name of that one inside a folder being wiped is found by that name
		// whenever it appears, here after the wipe has looked for the folders it knows.
		opts := parse_wipe_cache_options([], ['--vtmp'])!
		mut run := new_tool_cache_wipe_run()
		vtmp := os.vtmp_dir()
		late := os.join_path(vtmp, 'tools.untrusted-1700000001-43')
		tool_cache_test_root(late)!
		late_entry := tool_cache_test_entry(late, 'vtest', 'c')!
		late_idle := tool_cache_test_entry(late, 'vdoc', 'd')!
		late_lock := tool_cache_test_lock(late, 'vtest')
		os.write_file(late_lock, '')!
		late_release := release_path + '.late'
		mut late_holder := start_tool_cache_lock_holder(late_lock, late_release)!
		mut late_lines := []string{}
		run.wipe_folder_around_tool_roots(vtmp, 'V tmp.c and tests', true, opts, mut late_lines)!
		late_output := late_lines.join('\n')
		assert os.is_dir(late_entry), late_output
		assert os.is_file(late_lock), late_output
		assert !os.exists(late_idle), late_output
		os.write_file(late_release, '')!
		late_holder.wait()
		late_holder.close()

		os.write_file(release_path, '')!
		holder.wait()
		dry := wipe_output('--tools', '--dry-run', '--verbose')!
		assert dry.contains('Would wipe Moved-aside V tool cache folder ${aside}.'), dry
		assert dry.contains('  would remove ${busy_lock}'), dry
		assert os.is_dir(busy_entry)
		released := wipe_output('--tools')!
		assert released.contains('Moved-aside V tool cache folder ${aside} was wiped.'), released
		assert !os.exists(aside), released
	})!
}

fn test_moved_aside_names_have_one_shape() {
	assert is_moved_aside_tool_cache_name('tools.untrusted-1700000000-42')
	assert is_moved_aside_tool_cache_name('my.tools.untrusted-1-2')
	for name in ['tools', 'tools.untrusted-', 'tools.untrusted-1', 'tools.untrusted-1-',
		'tools.untrusted--2', 'tools.untrusted-1-2-3', 'tools.untrusted-a-2', 'tools.untrusted-1-b',
		'.untrusted-1-2', 'tools.untrusted-1-2.bak', 'tools.untrusted-12345'] {
		assert !is_moved_aside_tool_cache_name(name), name
	}
}

// A dry run removes nothing, makes no lock file, and lists exactly what a real run then removes.
fn test_a_dry_run_changes_nothing_and_tells_what_a_real_run_removes() ! {
	directory := toolcache_test_dir('wipe_cache_dry_run')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (cache_v_dir string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		tools := os.join_path(cache_v_dir, 'tools')
		custom := os.join_path(vtmp, 'custom-tools')
		aside := os.join_path(vtmp, 'tools.untrusted-1700000000-42')
		os.setenv(tool_cache_dir_env, custom, true)
		for root in [tools, custom, aside] {
			tool_cache_test_root(root)!
			tool_cache_test_entry(root, 'vfmt', 'a')!
			tool_cache_test_entry(root, 'vdoc', 'b')!
			os.write_file(tool_cache_test_lock(root, 'vfmt'), '')!
			os.write_file(os.join_path(root, 'plain'), 'x')!
		}
		os.write_file(os.join_path(vtmp, 'a.tmp.c'), 'c')!
		os.mkdir_all(os.join_path(vtmp, 'tsession_1', 'deeper'))!
		os.mkdir_all(os.join_path(directory, 'vcache', 'sub'))!
		os.write_file(os.join_path(directory, 'vcache', 'sub', 'obj.o'), 'o')!
		before := toolcache_test_tree(directory)
		dry := wipe_output('--dry-run', '--verbose')!
		assert toolcache_test_tree(directory) == before, dry
		// The summary of the folder around them names the tool cache folders that stay, and not
		// the one that the real run removes whole.
		assert dry.contains('Would wipe V tmp.c and tests folder ${vtmp}, except ${custom}.'), dry
		output := wipe_output('--verbose')!
		assert output.contains('V tmp.c and tests folder ${vtmp} was wiped, except the tool cache folders in it: ${custom}.'), output
		would_remove := toolcache_test_detail_paths(dry, '  would remove ')
		removed := toolcache_test_detail_paths(output, '  removing ')
		assert would_remove == removed, '${dry}\n---\n${output}'
		assert would_remove.len > 0
		for path in removed {
			assert !os.exists(path) && !os.is_link(path), path
		}
		// Whatever the real run says it keeps is there.
		for path in toolcache_test_detail_paths(output, '  keeping ') {
			assert os.exists(path), path
		}
		assert !os.exists(aside), output
	})!
}

// Several wipes at once all succeed: whatever another wipe removed first counts as removed.
fn test_wipes_that_run_at_the_same_time_all_succeed() ! {
	$if !linux && !windows {
		// What a system answers about a folder that another process removed a moment ago differs
		// from one system to the next, and this was measured on Linux and on Windows only.
		return
	}
	directory := toolcache_test_dir('wipe_cache_together')
	defer {
		os.rmdir_all(directory) or {}
	}
	start_path := os.join_path(directory, 'start')
	with_wipe_test_caches(directory, fn [directory, start_path] (cache_v_dir string) ! {
		tools := os.join_path(cache_v_dir, 'tools')
		vtmp := os.join_path(directory, 'vtmp')
		inner := os.join_path(vtmp, 'tools')
		aside := os.join_path(vtmp, 'tools.untrusted-1700000000-42')
		for round in 0 .. 3 {
			os.rm(start_path) or {}
			// Enough in every folder to keep each wipe at work while the others run.
			for folder in [os.getenv('VCACHE'), vtmp, os.getenv('V3CACHE')] {
				for i in 0 .. 12 {
					sub := os.join_path(folder, 'sub_${i}', 'deeper')
					os.mkdir_all(sub)!
					for j in 0 .. 8 {
						os.write_file(os.join_path(sub, 'file_${j}'), 'x')!
					}
				}
			}
			tool_cache_test_root(tools)!
			for i in 0 .. 6 {
				tool_cache_test_entry(tools, 'vtool${i}', 'a')!
			}
			// Tool cache folders inside VTMP as well, one of them moved aside: each wipe meets them
			// by themselves and again in the folder around them, where another wipe can have
			// removed them a moment before.
			for root in [inner, aside] {
				tool_cache_test_root(root)!
				for i in 0 .. 6 {
					tool_cache_test_entry(root, 'vtool${i}', 'b')!
				}
			}
			mut children := []&os.Process{}
			for _ in 0 .. 4 {
				mut child := os.new_process(os.executable())
				mut environment := os.environ()
				environment[toolcache_wipe_child_env] = start_path
				child.set_environment(environment)
				child.set_redirect_stdio()
				child.run()
				children << child
			}
			os.write_file(start_path, '')!
			for mut child in children {
				output := child.stdout_slurp() + child.stderr_slurp()
				child.wait()
				code := child.code
				child.close()
				assert code == 0, 'round ${round}: ${output}'
			}
			// Every entry went under one wipe or another; the lock files they made stay.
			for root in [tools, inner] {
				left := os.ls(root)!
				assert left.len == 6, left.str()
				assert left.all(it.ends_with(tool_cache_lock_ending)), left.str()
			}
			// The folder V moved aside went whole when one wipe held all its locks; when several
			// shared them, their lock files are what is left of it. Nothing else is left in VTMP.
			for name in os.ls(vtmp)! {
				assert name in [os.file_name(inner), os.file_name(aside)], name
			}
			if os.exists(aside) {
				left := os.ls(aside)!
				assert left.all(it.ends_with(tool_cache_lock_ending)), left.str()
			}
			assert os.ls(os.getenv('VCACHE'))!.len == 0
		}
	})!
}

// A tool cache folder that is a link is removed as a link; what it points to is not touched.
fn test_a_tool_cache_folder_that_is_a_link_is_removed_as_a_link() ! {
	$if windows {
		return
	}
	directory := toolcache_test_dir('wipe_cache_root_link')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (cache_v_dir string) ! {
		tools := os.join_path(cache_v_dir, 'tools')
		real := os.join_path(directory, 'real')
		os.mkdir_all(real)!
		os.write_file(os.join_path(real, 'keep'), 'x')!
		os.mkdir_all(cache_v_dir)!
		os.symlink(real, tools)!
		output := wipe_output('--tools')!
		assert output.contains('V tool cache folder ${tools} was wiped.'), output
		assert !os.is_link(tools)
		assert os.is_file(os.join_path(real, 'keep'))
	})!
}

// toolcache_test_junction makes `link` a junction to the folder `target`, the link to a folder
// that Windows lets any user make, and reports whether it did. Other systems get none.
fn toolcache_test_junction(link string, target string) bool {
	$if windows {
		return os.exec(['cmd', '/c', 'mklink', '/J', link, target]).exit_code == 0
	} $else {
		return false
	}
}

// The folder a wipe holds open is listed, inspected and cleared through that hold alone.
fn test_a_folder_is_listed_inspected_and_cleared_through_its_handle() ! {
	directory := toolcache_test_dir('wipe_dir_handle')
	defer {
		os.rmdir_all(directory) or {}
	}
	folder := os.join_path(directory, 'folder')
	outside := os.join_path(directory, 'outside')
	os.mkdir_all(os.join_path(folder, 'sub', 'deeper'))!
	os.write_file(os.join_path(folder, 'file'), 'x')!
	os.write_file(os.join_path(folder, 'sub', 'deeper', 'inner'), 'x')!
	os.mkdir_all(outside)!
	os.write_file(os.join_path(outside, 'keep'), 'x')!
	$if !windows {
		os.symlink(outside, os.join_path(folder, 'sub', 'folder-link'))!
		os.symlink(os.join_path(outside, 'keep'), os.join_path(folder, 'file-link'))!
	}
	has_junction := toolcache_test_junction(os.join_path(folder, 'junction'), outside)
	os.chmod(folder, 0o700)!
	working_folder := os.getwd()
	dir := open_tool_cache_wipe_dir(folder)!
	defer {
		dir.close()
	}
	// Each listing starts at the top again, and is sorted.
	first := dir.names()!
	assert dir.names()! == first
	assert 'file' in first
	assert 'sub' in first
	mut sorted := first.clone()
	sorted.sort()
	assert first == sorted
	assert dir.item('file').kind == .file
	assert dir.item('sub').kind == .folder
	assert dir.item('missing').kind == .gone
	// A name that is gone is told apart from one that cannot be opened as a folder.
	if other := dir.open_folder('missing') {
		other.close()
		assert false, 'a missing folder was opened'
	} else {
		assert err.code() == tool_cache_wipe_gone
	}
	if other := dir.open_folder('file') {
		other.close()
		assert false, 'a file was opened as a folder'
	} else {
		assert err.code() != tool_cache_wipe_gone
	}
	$if !windows {
		assert dir.item('file-link').kind == .link
		assert dir.item('file').owner == u32(os.getuid())
		assert dir.item('file').links == 1
		owner, mode := dir.owner_and_mode()
		assert owner == u32(os.getuid())
		assert mode == 0o700
	}
	// Looking at a name leaves the working folder where it was.
	assert os.getwd() == working_folder
	assert dir.is_at(folder)
	// A tree goes with all that is in it; a link in it is removed, and never followed.
	dir.remove_tree('sub')!
	assert !os.exists(os.join_path(folder, 'sub'))
	assert os.is_file(os.join_path(outside, 'keep'))
	// What is gone already counts as removed.
	dir.remove_tree('missing')!
	dir.remove_file('file')!
	dir.remove_file('file')!
	dir.remove_folder('missing')!
	assert !os.exists(os.join_path(folder, 'file'))
	if has_junction {
		// A link to a folder is a link, is not opened as a folder, and is removed itself.
		assert dir.item('junction').kind == .link
		if other := dir.open_folder('junction') {
			other.close()
			assert false, 'a junction was opened as a folder'
		}
		dir.remove_tree('junction')!
		assert !os.exists(os.join_path(folder, 'junction'))
		assert os.is_file(os.join_path(outside, 'keep'))
	}
	$if !windows {
		dir.remove_tree('file-link')!
		assert os.is_file(os.join_path(outside, 'keep'))
		// A link is not opened as a folder.
		link := os.join_path(directory, 'link-to-folder')
		os.symlink(outside, link)!
		if other := open_tool_cache_wipe_dir(link) {
			other.close()
			assert false, 'a link was opened as a folder'
		}
		// A folder that was renamed is still the folder that was opened.
		os.rename(folder, folder + '-moved')!
		assert !dir.is_at(folder)
		assert dir.is_at(folder + '-moved')
		assert dir.names()! == []string{}
		// A folder that is removed while it is held open lists nothing, and holds nothing.
		os.rmdir(folder + '-moved')!
		assert dir.names()! == []string{}
		assert dir.item('file').kind == .gone
		assert !dir.is_at(folder + '-moved')
	}
}

// A name is still described when the working folder cannot be opened to come back to: then the
// name is opened instead, which tells a file, a folder and a missing name apart, and nothing
// about a link.
fn test_a_name_is_described_when_the_working_folder_cannot_be_opened() ! {
	$if windows {
		return
	}
	if os.geteuid() == 0 {
		// The superuser opens a folder whatever its mode is.
		return
	}
	directory := toolcache_test_dir('wipe_dir_item_fallback')
	working_folder := os.getwd()
	defer {
		os.chdir(working_folder) or {}
		toolcache_test_unlock_tree(directory)
		os.rmdir_all(directory) or {}
	}
	folder := os.join_path(directory, 'folder')
	closed := os.join_path(directory, 'closed')
	os.mkdir_all(os.join_path(folder, 'sub'))!
	os.write_file(os.join_path(folder, 'file'), 'x')!
	os.symlink(os.join_path(folder, 'file'), os.join_path(folder, 'link'))!
	os.mkdir_all(closed)!
	dir := open_tool_cache_wipe_dir(folder)!
	// A folder that may be entered and not read cannot be opened.
	os.chdir(closed)!
	os.chmod(closed, 0o300)!
	file := dir.item('file')
	sub := dir.item('sub')
	missing := dir.item('missing')
	link := dir.item('link')
	os.chdir(working_folder)!
	dir.close()
	assert file.kind == .file
	assert file.owner == u32(os.getuid())
	assert sub.kind == .folder
	assert missing.kind == .gone
	assert link.kind == .other
}

// A build lock is taken through the open folder, with the very lock a build takes.
fn test_a_build_lock_is_taken_through_the_open_folder() ! {
	directory := toolcache_test_dir('wipe_dir_lock')
	defer {
		os.rmdir_all(directory) or {}
	}
	release_path := os.join_path(directory, 'lock.release')
	folder := os.join_path(directory, 'tools')
	os.mkdir_all(folder)!
	dir := open_tool_cache_wipe_dir(folder)!
	defer {
		dir.close()
	}
	name := tool_cache_lock_name('vfmt', u32(os.getuid()))
	assert dir.try_lock(name).state == .absent
	assert dir.create_lock_file(name, u32(os.getuid()))
	// A name that is taken is not made again.
	assert !dir.create_lock_file(name, u32(os.getuid()))
	assert dir.item(name).kind == .file
	mut held := dir.try_lock(name)
	assert held.state == .held
	held.release()
	// What a build holds is busy, and free again once the build lets go.
	mut holder := start_tool_cache_lock_holder(os.join_path(folder, name), release_path)!
	mut busy := dir.try_lock(name)
	assert busy.state == .busy
	busy.release()
	os.write_file(release_path, '')!
	holder.wait()
	holder.close()
	mut free := dir.try_lock(name)
	assert free.state == .held
	free.release()
	// A folder at the name is no lock file.
	other := tool_cache_lock_name('vdoc', u32(os.getuid()))
	os.mkdir(os.join_path(folder, other))!
	assert dir.try_lock(other).state == .unsafe_name
	$if !windows {
		// Neither is a link there, which is not followed, even to a lock file.
		linked := tool_cache_lock_name('vtest', u32(os.getuid()))
		os.symlink(os.join_path(folder, name), os.join_path(folder, linked))!
		assert dir.try_lock(linked).state == .unsafe_name
		assert !dir.create_lock_file(linked, u32(os.getuid()))
		assert os.is_link(os.join_path(folder, linked))
		// Nor is a FIFO, which opening it does not wait on.
		piped := tool_cache_lock_name('vpipe', u32(os.getuid()))
		fifo := cmdexec.run('mkfifo', [os.join_path(folder, piped)])
		assert fifo.exit_code == 0, fifo.output
		assert dir.try_lock(piped).state == .unsafe_name
		if os.geteuid() != 0 {
			// A lock file that cannot be handed to the user it is made for is not left behind.
			foreign := tool_cache_lock_name('vother', u32(os.getuid()) + 1)
			assert !dir.create_lock_file(foreign, u32(os.getuid()) + 1)
			assert dir.item(foreign).kind == .gone
		}
	}
}

// Whatever is gone already counts as removed, and a folder that cannot be emptied is named by the
// first item that stayed, after everything else in it was attempted.
fn test_a_cache_folder_is_removed_with_all_that_can_go() ! {
	directory := toolcache_test_dir('wipe_cache_folder_removal')
	defer {
		toolcache_test_unlock_tree(directory)
		os.rmdir_all(directory) or {}
	}
	remove_cache_folder(os.join_path(directory, 'missing'))!
	folder := os.join_path(directory, 'folder')
	os.mkdir_all(os.join_path(folder, 'sub', 'deeper'))!
	os.write_file(os.join_path(folder, 'sub', 'deeper', 'file'), 'x')!
	remove_cache_folder(folder)!
	assert !os.exists(folder)
	$if !windows {
		if os.geteuid() == 0 {
			// The superuser removes from a folder whatever its mode is.
			return
		}
		os.mkdir_all(folder)!
		stuck := toolcache_test_stuck_folder(folder)!
		mut message := ''
		remove_cache_folder(folder) or { message = err.msg() }
		assert message.starts_with(os.join_path(stuck, 'file') + ': '), message
		assert !os.exists(os.join_path(folder, 'z-after'))
		assert os.is_file(os.join_path(stuck, 'file'))
	}
}

// toolcache_test_stuck_folder puts the folder `a-stuck` into `parent`, with a file that this user
// cannot remove from it, and the file `z-after`, which a wipe meets later. It returns the folder.
fn toolcache_test_stuck_folder(parent string) !string {
	stuck := os.join_path(parent, 'a-stuck')
	os.mkdir_all(stuck)!
	os.write_file(os.join_path(stuck, 'file'), 'x')!
	os.write_file(os.join_path(parent, 'z-after'), 'x')!
	os.chmod(stuck, 0o500)!
	return stuck
}

// toolcache_test_unlock_tree gives this user full access again to `folder` and to every folder
// below it, so that a test which took access away can be cleaned up.
fn toolcache_test_unlock_tree(folder string) {
	os.chmod(folder, 0o700) or {}
	for name in os.ls(folder) or { []string{} } {
		path := os.join_path(folder, name)
		if os.is_dir(path) && !os.is_link(path) {
			toolcache_test_unlock_tree(path)
		}
	}
}

// wipe_failure runs `v wipe-cache` with `arguments` in this process, and returns the message of the
// error it ends with, or '' when it succeeds.
fn wipe_failure(arguments ...string) string {
	wipe_output(...arguments) or { return err.msg() }
	return ''
}

// A wipe that cannot remove something attempts all the rest, and then fails with one message that
// names the folder it was wiping and the first item that stayed.
fn test_a_wipe_that_leaves_something_behind_fails_and_names_it() ! {
	$if windows {
		return
	}
	if os.geteuid() == 0 {
		// The superuser removes from a folder whatever its mode is.
		return
	}
	directory := toolcache_test_dir('wipe_cache_failures')
	defer {
		toolcache_test_unlock_tree(directory)
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		vcache := os.join_path(directory, 'vcache')
		custom := os.join_path(vtmp, 'custom-tools')

		// In a cache folder that holds no tool cache folder.
		os.mkdir_all(vcache)!
		mut stuck := toolcache_test_stuck_folder(vcache)!
		mut message := wipe_failure('--vcache')
		assert message.starts_with('v wipe-cache: could not remove V cache folder ${vcache}: ${os.join_path(stuck, 'file')}: '), message
		assert !os.exists(os.join_path(vcache, 'z-after'))
		toolcache_test_unlock_tree(vcache)

		// Beside a tool cache folder, in the folder that holds it.
		os.setenv(tool_cache_dir_env, custom, true)
		tool_cache_test_root(custom)!
		stuck = toolcache_test_stuck_folder(vtmp)!
		message = wipe_failure('--vtmp')
		assert message.starts_with('v wipe-cache: could not remove V tmp.c and tests folder ${vtmp}: ${os.join_path(stuck, 'file')}: '), message
		assert message.count('v wipe-cache:') == 1, message
		assert !os.exists(os.join_path(vtmp, 'z-after'))
		toolcache_test_unlock_tree(vtmp)
		os.rmdir_all(stuck)!

		// In a tool cache folder inside that folder: the message is the tool cache folder's own.
		stuck = toolcache_test_stuck_folder(custom)!
		message = wipe_failure('--vtmp')
		assert message.starts_with('v wipe-cache: could not remove V tool cache folder ${custom}: ${os.join_path(stuck, 'file')}: '), message
		assert message.count('v wipe-cache:') == 1, message
		assert !os.exists(os.join_path(custom, 'z-after'))
		toolcache_test_unlock_tree(vtmp)
		os.rmdir_all(stuck)!

		// In a tool cache folder inside another one, which is wiped first.
		inner := os.join_path(vtmp, 'tools')
		os.setenv(tool_cache_dir_env, vtmp, true)
		tool_cache_test_root(inner)!
		stuck = toolcache_test_stuck_folder(inner)!
		message = wipe_failure('--tools')
		assert message.starts_with('v wipe-cache: could not remove V tool cache folder ${inner}: ${os.join_path(stuck, 'file')}: '), message
		assert message.count('v wipe-cache:') == 1, message
		assert !os.exists(os.join_path(inner, 'z-after'))
		toolcache_test_unlock_tree(vtmp)

		// A folder that cannot be listed says so when the wipe is asked to list what it removes,
		// in a dry run as well.
		os.chmod(vcache, 0o300)!
		for arguments in [['--vcache', '--verbose'], ['--vcache', '--verbose', '--dry-run']] {
			message = wipe_failure(...arguments)
			assert message.starts_with('v wipe-cache: could not list V cache folder ${vcache}: '), message
		}

		// A link or a file at the path of a cache folder that cannot be removed fails the wipe too:
		// only what is gone counts as removed.
		closed := os.join_path(directory, 'closed')
		os.mkdir_all(closed)!
		os.symlink(os.join_path(directory, 'nowhere'), os.join_path(closed, 'link'))!
		os.write_file(os.join_path(closed, 'file'), 'x')!
		os.chmod(closed, 0o500)!
		for name in ['link', 'file'] {
			stays := os.join_path(closed, name)
			os.setenv('VCACHE', stays, true)
			message = wipe_failure('--vcache')
			assert message.starts_with('v wipe-cache: could not remove V cache folder ${stays}: '), message
		}
	})!
}

// A folder beside a tool cache folder whose name only starts like the name of one V moved aside is
// wiped like any tool cache folder: its lock files stay, and so the folder does. So is a folder
// with such a name in which V builds tools.
fn test_only_a_folder_v_moved_aside_loses_its_lock_files() ! {
	directory := toolcache_test_dir('wipe_cache_almost_aside')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (cache_v_dir string) ! {
		almost := os.join_path(cache_v_dir, 'tools.untrusted-kept')
		in_use := os.join_path(directory, 'custom.untrusted-1700000000-42')
		os.setenv(tool_cache_dir_env, in_use, true)
		for root in [almost, in_use] {
			tool_cache_test_root(root)!
			assert !is_moved_aside_tool_cache_root(root), root
			entry := tool_cache_test_entry(root, 'vfmt', 'a')!
			lock_path := tool_cache_test_lock(root, 'vfmt')
			os.write_file(lock_path, '')!
			output := wipe_output('--tools')!
			assert output.contains('Moved-aside V tool cache folder ${root} was wiped.'), output
			assert !os.exists(entry), output
			assert os.is_file(lock_path), output
		}
	})!
}

// The lock files of a folder V moved aside go only when nothing in the folder is kept, and only
// those the wipe holds: a lock that a process holds, a lock file that cannot be opened and one of
// another user keep the others, and the folder, in place.
fn test_a_moved_aside_folder_keeps_its_lock_files_while_one_of_them_is_not_the_wipes() ! {
	directory := toolcache_test_dir('wipe_cache_aside_locks')
	defer {
		os.rmdir_all(directory) or {}
	}
	release_path := os.join_path(directory, 'lock.release')
	with_wipe_test_caches(directory, fn [directory, release_path] (cache_v_dir string) ! {
		me := u32(os.getuid())
		// A lock that a process holds, with no entry beside it.
		busy_aside := os.join_path(cache_v_dir, 'tools.untrusted-1700000000-42')
		tool_cache_test_root(busy_aside)!
		busy_lock := tool_cache_test_lock(busy_aside, 'vtest')
		beside_busy := tool_cache_test_lock(busy_aside, 'vfmt')
		os.write_file(busy_lock, '')!
		os.write_file(beside_busy, '')!
		// A lock file of another user, which this wipe does not take.
		foreign_aside := os.join_path(cache_v_dir, 'tools.untrusted-1700000000-43')
		tool_cache_test_root(foreign_aside)!
		foreign_lock := os.join_path(foreign_aside, tool_cache_lock_name('vfmt', me + 1))
		os.write_file(foreign_lock, '')!
		// A lock file that cannot be opened and, having a second name, is not repaired.
		stuck_aside := os.join_path(cache_v_dir, 'tools.untrusted-1700000000-44')
		tool_cache_test_root(stuck_aside)!
		stuck_lock := tool_cache_test_lock(stuck_aside, 'vtest')
		beside_stuck := tool_cache_test_lock(stuck_aside, 'vfmt')
		os.write_file(stuck_lock, '')!
		os.write_file(beside_stuck, '')!
		mut stuck_is_unopenable := false
		$if !windows {
			if os.geteuid() != 0 {
				os.chmod(stuck_lock, 0o400)!
				os.link(stuck_lock, os.join_path(directory, 'second-name'))!
				stuck_is_unopenable = true
			}
		}
		mut holder := start_tool_cache_lock_holder(busy_lock, release_path)!
		defer {
			os.write_file(release_path, '') or {}
			holder.wait()
			holder.close()
		}
		output := wipe_output('--tools', '--verbose')!
		assert os.is_file(busy_lock), output
		assert os.is_file(beside_busy), output
		assert output.contains('  keeping ${beside_busy}'), output
		if !tool_cache_wipe_is_superuser(me) {
			assert os.is_file(foreign_lock), output
			assert output.contains('  keeping ${foreign_lock}'), output
		}
		if stuck_is_unopenable {
			assert os.is_file(stuck_lock), output
			assert os.is_file(beside_stuck), output
			assert output.contains('  keeping ${beside_stuck}'), output
		} else {
			// Both locks were the wipe's to take, so the folder went whole.
			assert !os.exists(stuck_aside), output
		}
	})!
}

// What is no folder at the path of a tool cache folder, or of a folder that holds one, is removed
// as it is, like any other cache folder that is none.
fn test_what_is_no_folder_at_the_path_of_a_cache_folder_goes_as_it_is() ! {
	directory := toolcache_test_dir('wipe_cache_no_folder')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		custom := os.join_path(vtmp, 'custom-tools')
		os.setenv(tool_cache_dir_env, custom, true)
		// A file at the path of a tool cache folder, met in the folder around it and by itself.
		for arguments in [['--vtmp'], ['--tools']] {
			os.write_file(custom, 'no folder')!
			output := wipe_output(...arguments)!
			assert !os.exists(custom), output
		}
		// A file at the path of a cache folder that would hold a tool cache folder: the cache folder
		// is made anew. (`VTMP` cannot be tried this way: `os.vtmp_dir` panics on a file there.)
		vcache := os.join_path(directory, 'vcache')
		os.setenv(tool_cache_dir_env, os.join_path(vcache, 'custom-tools'), true)
		os.write_file(vcache, 'no folder')!
		assert 'custom-tools' in new_tool_cache_wipe_run().roots_below(vcache)
		file_output := wipe_output('--vcache')!
		assert os.is_dir(vcache), file_output
		os.setenv(tool_cache_dir_env, custom, true)
		$if !windows {
			// A link there, to a folder that holds a tool cache folder: the link goes, a real folder
			// takes its place, and what the link pointed to is not touched.
			real := os.join_path(directory, 'real-vtmp')
			tool_cache_test_root(os.join_path(real, 'custom-tools'))!
			entry := tool_cache_test_entry(os.join_path(real, 'custom-tools'), 'vfmt', 'a')!
			os.rmdir_all(vtmp)!
			os.symlink(real, vtmp)!
			assert 'custom-tools' in new_tool_cache_wipe_run().roots_below(vtmp)
			link_output := wipe_output('--vtmp')!
			assert !os.is_link(vtmp), link_output
			assert os.is_dir(vtmp), link_output
			assert os.is_dir(entry), link_output
		}
	})!
}
