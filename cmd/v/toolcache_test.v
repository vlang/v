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
const toolcache_wipe_ready_env = 'VTEST_TOOLCACHE_WIPE_READY'
const toolcache_lock_probe_env = 'VTEST_TOOLCACHE_LOCK_PROBE'

// Reuse the test executable for a bounded pruning probe. Reading a planted FIFO used to
// block indefinitely while holding the tool's lock, so run that work in a child process.
fn testsuite_begin() {
	if lock_path := os.getenv_opt(toolcache_lock_holder_env) {
		hold_tool_cache_lock_until_released(lock_path, os.getenv(toolcache_lock_release_env))
	}
	if lock_path := os.getenv_opt(toolcache_lock_probe_env) {
		// Tries a build lock once, without waiting, from a process of its own: a lock that the
		// test's own process holds does not keep that process from taking it again.
		mut probe := filelock.new_file(lock_path, mode: .exclusive)
		if probe.try_acquire() {
			probe.release()
			println('free')
		} else {
			println('busy')
		}
		exit(0)
	}
	if start_path := os.getenv_opt(toolcache_wipe_child_env) {
		// One of several wipes that start at the same moment, each in a process of its own. It
		// says that it runs, when asked to, so that a test can change its working folder first.
		if ready_path := os.getenv_opt(toolcache_wipe_ready_env) {
			os.write_file(ready_path, '') or { panic(err) }
		}
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

// ToolCacheTestStart is what the tests started with: the settings a wipe test changes, which of
// them were set, and the working folder.
struct ToolCacheTestStart {
	env map[string]string
	set []string
	wd  string
}

// toolcache_test_variables are the settings that wipe tests change.
const toolcache_test_variables = ['HOME', 'USERPROFILE', 'XDG_CACHE_HOME', 'VTMP', 'VCACHE', 'V3CACHE',
	'VMODULES', tool_cache_dir_env]

const toolcache_test_start = toolcache_test_capture()

fn toolcache_test_capture() ToolCacheTestStart {
	mut env := map[string]string{}
	mut set := []string{}
	for name in toolcache_test_variables {
		if value := os.getenv_opt(name) {
			env[name] = value
			set << name
		}
	}
	return ToolCacheTestStart{
		env: env
		set: set
		wd:  os.getwd()
	}
}

// toolcache_test_restore_start puts the settings that wipe tests change, and the working folder,
// back as the tests started with them. A failed assert ends a test without the deferred restores
// of the functions around it (`with_wipe_test_caches` and the like), so without this the next test
// would run with the cache folders of the failed one.
fn toolcache_test_restore_start() {
	for name in toolcache_test_variables {
		if name in toolcache_test_start.set {
			os.setenv(name, toolcache_test_start.env[name], true)
		} else {
			os.unsetenv(name)
		}
	}
	if toolcache_test_start.wd != '' {
		os.chdir(toolcache_test_start.wd) or {}
	}
}

// toolcache_test_finish waits for a child process of a test for `limit` at most, kills it if it
// still runs then, and returns what it printed and its exit status: a child that hangs fails the
// test instead of stopping the whole run. The children print little, so their pipes do not fill.
fn toolcache_test_finish(mut child os.Process, limit time.Duration) (string, int) {
	deadline := time.now().add(limit)
	for child.is_alive() && time.now() < deadline {
		time.sleep(5 * time.millisecond)
	}
	if child.is_alive() {
		child.signal_kill()
	}
	output := child.stdout_slurp() + child.stderr_slurp()
	child.wait()
	code := child.code
	child.close()
	return output, code
}

fn toolcache_test_dir(name string) string {
	toolcache_test_restore_start()
	directory := os.join_path(os.vtmp_dir(), 'v_toolcache_test', '${name}_${os.getpid()}')
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
		// A tool cache folder that builds use stays, emptied: a build may have just made it, and not
		// yet its lock file. `tools.untrusted-12345` is not named as V names a folder it moves aside;
		// it goes as any emptied tool cache folder that builds do not use goes.
		assert os.ls(cache_tools)! == []
		assert os.ls(home_tools)! == []
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
		assert os.ls(cache_tools)! == []
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
		// The tools pass, which comes first, wiped the updater's entry before the cache pass failed.
		assert !os.exists(updater)
		os.mkdir_all(os.dir(updater))!
		os.cp(os.executable(), updater)!
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

// The default tool cache folder `<VTMP>/tools` is wiped as a tool cache folder by the wipe of `VTMP`
// too, with the default options and with `--vtmp` alone: a build there keeps its entry and its lock,
// and the rest of `VTMP` goes.
fn test_wipe_cache_keeps_a_live_build_in_the_default_folder_inside_vtmp() ! {
	directory := toolcache_test_dir('wipe_cache_vtmp_default_root')
	defer {
		os.rmdir_all(directory) or {}
	}
	release_path := os.join_path(directory, 'lock.release')
	with_wipe_test_caches(directory, fn [directory, release_path] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		tools := os.join_path(vtmp, 'tools')
		tool_cache_test_root(tools)!
		busy_entry := tool_cache_test_entry(tools, 'vtest', 'a')!
		staged_binary := os.join_path(busy_entry, '${tool_cache_stage_prefix}0-1-2-0', 'vtest')
		os.mkdir_all(os.dir(staged_binary))!
		os.write_file(staged_binary, 'half built')!
		busy_lock := tool_cache_test_lock(tools, 'vtest')
		os.write_file(busy_lock, '')!
		mut holder := start_tool_cache_lock_holder(busy_lock, release_path)!
		defer {
			os.write_file(release_path, '') or {}
			holder.wait()
			holder.close()
		}
		for arguments in [[]string{}, ['--vtmp']] {
			idle_entry := tool_cache_test_entry(tools, 'vfmt', 'b')!
			temporary := os.join_path(vtmp, 'a.tmp.c')
			session := os.join_path(vtmp, 'tsession_1')
			os.write_file(temporary, 'c')!
			os.mkdir_all(os.join_path(session, 'deeper'))!
			os.write_file(os.join_path(session, 'deeper', 'file'), 'x')!
			output := wipe_output(...arguments)!
			context := '${arguments}\n${output}'
			assert os.is_file(staged_binary), context
			assert os.is_file(busy_lock), context
			assert !os.exists(idle_entry), context
			assert !os.exists(temporary), context
			assert !os.exists(session), context
			assert output.contains('V tool cache folder ${tools} was wiped, except for the tools another process has locked right now: vtest.'), context
			assert output.contains('V tmp.c and tests folder ${vtmp} was wiped, except the tool cache folders in it: ${tools}.'), context
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
		// A folder that a listing gives first or last is not mistaken for it.
		os.mkdir_all(os.join_path(vtmp, 'Deep', 'Another'))!
		os.mkdir_all(os.join_path(vtmp, 'Deep', 'Zeta'))!
		assert tool_cache_spelled_as_listed(vtmp, os.join_path('deep', 'custom-tools')) == os.join_path('Deep',
			'Custom-Tools')
		// Nor is another name of the same file, which differs from it in more than case.
		os.write_file(os.join_path(vtmp, 'Deep', 'File-B'), 'x')!
		os.link(os.join_path(vtmp, 'Deep', 'File-B'), os.join_path(vtmp, 'Deep', 'Aaa'))!
		assert tool_cache_spelled_as_listed(vtmp, os.join_path('deep', 'file-b')) == os.join_path('Deep',
			'File-B')
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
		// The wipe says the same when what stands at the path afterwards is a link that leads nowhere:
		// the path is taken, and the wipe did not empty what is there.
		custom := os.join_path(cache_v_dir, 'custom-tools')
		tool_cache_test_root(custom)!
		custom_dir := open_tool_cache_wipe_dir(custom)!
		defer {
			custom_dir.close()
		}
		os.rename(custom, custom + '.untrusted-1700000000-43')!
		os.symlink(os.join_path(cache_v_dir, 'nowhere'), custom)!
		mut custom_lines := []string{}
		run.wipe_open_tool_cache_directory(custom_dir, custom, opts, mut custom_lines)!
		said := custom_lines.join('\n')
		assert said.contains('V tool cache folder ${custom} was moved aside by a build during the wipe.'), said
		assert os.is_link(custom), said
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
		// Nor in a sticky folder that user can write to: there the user can still rename the file
		// and put a link in its place, which a change of mode by name would follow.
		assert !tool_cache_lock_is_repairable(0, 0, 0o1777, ToolCacheWipeItem{
			kind:  .file
			owner: 2000
			links: 1
		})
		assert tool_cache_lock_is_repairable(0, 0, 0o1777, ToolCacheWipeItem{
			kind:  .file
			owner: 0
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
// would reach a file outside the wipe. The entries of its tool stay, for no build, so the wipe
// fails and says why; a dry run says the same. The other tools' entries go all the same.
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
			message := wipe_failure(...arguments)
			assert message == 'v wipe-cache: could not remove V tool cache folder ${tools}: the entries of vfmt stay: the lock file ${lock_path} cannot be opened', message
			assert os.is_dir(entry), message
			assert !os.is_writable(lock_path), message
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
		// The file the link led to kept its mode: no repair went through the link. (Its mode, not
		// whether it can be written: the superuser may write any file.)
		assert os.stat(target)!.mode & 0o777 == 0o400
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
		assert new_tool_cache_wipe_run().is_moved_aside(aside)
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

// Several wipes at once all succeed: whatever another wipe removed first counts as removed. On
// Windows one of them can fail on a name that another one is removing at that moment, and the
// wipe run again then succeeds.
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
			mut failures := []string{}
			for mut child in children {
				output, code := toolcache_test_finish(mut child, 2 * time.minute)
				if code != 0 {
					failures << 'round ${round}: ${output}'
				}
			}
			$if windows {
				// On Windows a file or folder that one wipe is removing stays for a moment, and another
				// wipe can fail on it (see the docs); the wipe run again succeeds. This only checks
				// that no failure says a name was missing: that a name gone already never fails a wipe
				// is pinned, step by step, by
				// `test_what_another_wipe_removed_meanwhile_counts_as_removed_on_windows` and
				// `test_an_emptied_folder_gone_or_replaced_before_its_removal_does_not_fail_the_wipe`.
				for failure in failures {
					assert !failure.contains('No such file or directory'), failure
				}
				if failures.len > 0 {
					mut again := ''
					_ := wipe_output() or {
						again = err.msg()
						''
					}
					assert again == '', '${failures.join('\n')}\n--- the wipe run again:\n${again}'
				}
			} $else {
				assert failures.len == 0, failures.join('\n')
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
			assert os.ls(os.getenv('V3CACHE'))!.len == 0
		}
	})!
}

// On Windows a folder is removed item by item, and every item is attempted: one that cannot go (a
// file held open, which Windows does not delete) keeps the folder and is named, but not the items
// after it.
fn test_a_folder_removal_on_windows_attempts_every_item() ! {
	$if !windows {
		return
	}
	directory := toolcache_test_dir('wipe_remove_tree_every_item')
	defer {
		os.rmdir_all(directory) or {}
	}
	folder := os.join_path(directory, 'x')
	os.mkdir_all(os.join_path(folder, 'b', 'c'))!
	os.write_file(os.join_path(folder, 'b', 'c', 'file'), 'x')!
	os.write_file(os.join_path(folder, 'z'), 'x')!
	held_path := os.join_path(folder, 'a')
	os.write_file(held_path, 'held')!
	mut held := os.open(held_path)!
	dir := open_tool_cache_wipe_dir(directory)!
	mut failure := ''
	dir.remove_tree('x') or { failure = err.msg() }
	held.close()
	assert failure.starts_with('x${os.path_separator}a: '), failure
	assert os.is_file(held_path), failure
	assert !os.exists(os.join_path(folder, 'b')), failure
	assert !os.exists(os.join_path(folder, 'z')), failure
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

// A name is not looked at when the working folder cannot be opened to come back to: no other way of
// looking stands in for it, since each of them takes a link or an unreadable file for something
// else. The wipe leaves such a working folder before it starts (see the test below).
fn test_a_name_is_not_looked_at_without_a_way_back_to_the_working_folder() ! {
	$if windows {
		return
	}
	if os.geteuid() == 0 {
		// The superuser opens a folder whatever its mode is.
		return
	}
	directory := toolcache_test_dir('wipe_dir_item_no_way_back')
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
	mut kinds := []ToolCacheWipeKind{}
	for name in ['file', 'sub', 'missing', 'link'] {
		kinds << dir.item(name).kind
	}
	os.chdir(working_folder)!
	in_reach := dir.item('link').kind
	dir.close()
	assert kinds == [ToolCacheWipeKind.unknown, .unknown, .unknown, .unknown]
	assert in_reach == .link
}

// A wipe started from a folder it may not read (`sudo -u someone` from a folder that is not theirs)
// works as from any other folder: a link and an unreadable lock file in a tool cache folder are
// looked at and dealt with, and a setting given as a relative path still names the same folder.
fn test_a_wipe_started_from_a_folder_it_cannot_read_works() ! {
	$if windows {
		return
	}
	if os.geteuid() == 0 {
		return
	}
	directory := toolcache_test_dir('wipe_cache_unreadable_working_folder')
	defer {
		toolcache_test_unlock_tree(directory)
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (cache_v_dir string) ! {
		tools := os.join_path(cache_v_dir, 'tools')
		tool_cache_test_root(tools)!
		entry := tool_cache_test_entry(tools, 'vfmt', 'a')!
		lock_path := tool_cache_test_lock(tools, 'vfmt')
		os.write_file(lock_path, '')!
		os.chmod(lock_path, 0o000)!
		link := os.join_path(tools, 'stray-link')
		os.symlink(os.join_path(directory, 'nowhere'), link)!
		closed := os.join_path(directory, 'closed')
		os.mkdir_all(closed)!
		start := os.join_path(directory, 'start')
		os.write_file(start, '')!
		mut env := os.environ()
		env[toolcache_wipe_child_env] = start
		// Relative to the folder the wipe is started from.
		env['VCACHE'] = 'relative-cache'
		os.chmod(closed, 0o300)!
		mut child := os.new_process(os.executable())
		child.set_work_folder(closed)
		child.set_environment(env)
		child.set_redirect_stdio()
		child.run()
		output, code := toolcache_test_finish(mut child, 2 * time.minute)
		os.chmod(closed, 0o700)!
		assert code == 0, output
		assert !os.is_link(link), output
		assert !os.exists(entry), output
		assert os.is_file(lock_path), output
		assert os.is_dir(os.join_path(closed, 'relative-cache')), output
		assert !output.contains('could not look at'), output
	})!
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
	assert toolcache_test_made(dir, name, u32(os.getuid()))
	// A name that is taken is not made again.
	assert !toolcache_test_made(dir, name, u32(os.getuid()))
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
		assert !toolcache_test_made(dir, linked, u32(os.getuid()))
		assert os.is_link(os.join_path(folder, linked))
		// Nor is a FIFO, which opening it does not wait on.
		piped := tool_cache_lock_name('vpipe', u32(os.getuid()))
		fifo := cmdexec.run('mkfifo', [os.join_path(folder, piped)])
		assert fifo.exit_code == 0, fifo.output
		assert dir.try_lock(piped).state == .unsafe_name
		if os.geteuid() != 0 {
			// A lock file that cannot be handed to the user it is made for is not left behind.
			foreign := tool_cache_lock_name('vother', u32(os.getuid()) + 1)
			assert !toolcache_test_made(dir, foreign, u32(os.getuid()) + 1)
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
	run := ToolCacheWipeRun{}
	run.remove_cache_folder(os.join_path(directory, 'missing'))!
	folder := os.join_path(directory, 'folder')
	os.mkdir_all(os.join_path(folder, 'sub', 'deeper'))!
	os.write_file(os.join_path(folder, 'sub', 'deeper', 'file'), 'x')!
	run.remove_cache_folder(folder)!
	assert !os.exists(folder)
	$if !windows {
		if os.geteuid() == 0 {
			// The superuser removes from a folder whatever its mode is.
			return
		}
		os.mkdir_all(folder)!
		stuck := toolcache_test_stuck_folder(folder)!
		mut message := ''
		run.remove_cache_folder(folder) or { message = err.msg() }
		assert message.starts_with(os.join_path(stuck, 'file') + ': '), message
		assert !os.exists(os.join_path(folder, 'z-after'))
		assert os.is_file(os.join_path(stuck, 'file'))
	}
}

// A cache folder whose removal fails is tried again, since another wipe at work in the same folder
// can make one attempt fail; a folder that another wipe removed first, and outside Windows a folder
// that is no longer the one opened, do not fail the removal. Making a cache folder again is tried
// again too.
fn test_a_cache_folder_removal_and_making_it_again_are_tried_again() ! {
	directory := toolcache_test_dir('wipe_cache_tried_again')
	defer {
		os.rmdir_all(directory) or {}
	}
	folder := os.join_path(directory, 'cache')
	once := os.join_path(directory, 'once')
	os.mkdir_all(os.join_path(folder, 'sub'))!
	mut run := ToolCacheWipeRun{}
	// Something is put in the folder as the first attempt removes it, and is gone by the second.
	run.before_acting = fn [folder, once] (path string, step string) {
		if path != folder || step != 'rmdir' {
			return
		}
		if !os.exists(once) {
			os.write_file(once, '') or { panic(err) }
			os.write_file(os.join_path(folder, 'late'), 'x') or { panic(err) }
		} else {
			os.rm(os.join_path(folder, 'late')) or {}
		}
	}
	run.remove_cache_folder(folder)!
	assert !os.exists(folder)
	// Another wipe removes the folder as this one is about to.
	os.mkdir_all(os.join_path(folder, 'sub'))!
	run.before_acting = fn [folder] (path string, step string) {
		if path == folder && step == 'rmdir' {
			os.rmdir(folder) or {}
		}
	}
	run.remove_cache_folder(folder)!
	assert !os.exists(folder)
	$if !windows {
		// A build moves the folder aside and makes a new one, with something in it.
		os.mkdir_all(folder)!
		run.before_acting = fn [folder] (path string, step string) {
			if path == folder && step == 'rmdir' && !os.exists('${folder}-old') {
				os.rename(folder, '${folder}-old') or { panic(err) }
				os.mkdir_all(os.join_path(folder, 'new')) or { panic(err) }
			}
		}
		run.remove_cache_folder(folder)!
		assert os.is_dir(os.join_path(folder, 'new'))
		os.rmdir_all(folder)!
	}
	// Something is in the way as the first attempt makes the folder, and is gone by the second.
	os.rm(once)!
	run.before_acting = fn [folder, once] (path string, step string) {
		if path != folder || step != 'mkdir' {
			return
		}
		if !os.exists(once) {
			os.write_file(once, '') or { panic(err) }
			os.write_file(folder, 'in the way') or { panic(err) }
		} else {
			os.rm(folder) or {}
		}
	}
	run.make_cache_folder_again(folder, 'V cache')!
	assert os.is_dir(folder)
}

// A tool cache folder that the wipe emptied and is about to remove does not fail the wipe when it is
// not there any more at that moment, as another wipe removed it, or, outside Windows, when the folder
// at its path is another one, which a build made with something in it. Nor does it when a build put
// something in it meanwhile: the folder then stays, with what the build put there.
fn test_an_emptied_folder_gone_or_replaced_before_its_removal_does_not_fail_the_wipe() ! {
	directory := toolcache_test_dir('wipe_cache_rmdir_raced')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		aside := os.join_path(directory, 'vtmp', 'tools.untrusted-1700000000-7')
		late := os.join_path(aside, 'put-there-by-a-build')
		cases := $if windows { ['gone', 'filled'] } $else { ['gone', 'replaced', 'filled'] }
		for round, how in cases {
			tool_cache_test_root(aside)!
			tool_cache_test_entry(aside, 'vfmt', 'a')!
			mut run := new_tool_cache_wipe_run()
			run.before_acting = fn [aside, late, how, round] (path string, step string) {
				if path != aside || step != 'rmdir' {
					return
				}
				if how == 'gone' {
					os.rmdir(aside) or { panic(err) }
				} else if how == 'replaced' {
					os.rename(aside, '${aside}-old-${round}') or { panic(err) }
					os.mkdir_all(os.join_path(aside, 'new')) or { panic(err) }
				} else {
					os.write_file(late, 'made by the filled case') or { panic(err) }
				}
			}
			opts := parse_wipe_cache_options([], ['--tools'])!
			mut lines := []string{}
			mut failure := ''
			run.wipe_tool_cache_directory(aside, opts, mut lines) or { failure = err.msg() }
			assert failure == '', '${how}: ${failure}\n' + lines.join('\n')
			if how == 'filled' {
				assert os.read_file(late)! == 'made by the filled case', lines.join('\n')
			}
			os.rmdir_all(aside) or {}
		}
	})!
}

// On Windows the wipe counts what another wipe removed meanwhile as removed at every step: a name
// that went, a folder that went before it is listed, and a folder that another wipe removed while
// this one was emptying it, once the item that held it up could go.
fn test_what_another_wipe_removed_meanwhile_counts_as_removed_on_windows() ! {
	$if windows {
		directory := toolcache_test_dir('wipe_gone_meanwhile')
		defer {
			os.rmdir_all(directory) or {}
		}
		os.mkdir_all(directory)!
		mut dir := open_tool_cache_wipe_dir(directory)!
		dir.remove_file('missing')!
		dir.remove_folder('missing')!
		dir.remove_tree('missing')!
		listed := os.join_path(directory, 'listed')
		os.mkdir_all(listed)!
		inner := open_tool_cache_wipe_dir(listed)!
		os.rmdir(listed)!
		assert inner.names()! == []string{}
		folder := os.join_path(directory, 'x')
		os.mkdir_all(folder)!
		// A file held open, which Windows does not delete.
		stuck := os.join_path(folder, 'a')
		os.write_file(stuck, 'x')!
		held := os.open(stuck)!
		dir.before_acting = fn [folder, stuck, held] (path string, step string) {
			if path == folder && step == 'rmdir' {
				assert os.exists(stuck)
				mut file := held
				file.close()
				os.rmdir_all(folder) or { panic(err) }
			}
		}
		dir.remove_tree('x')!
		assert !os.exists(folder)
	}
}

// `remove_folder` removes only the empty folder it names: a folder that still holds something fails
// and stays. On Windows a name that no path reaches fails too, and the folder that a path with it
// reaches, `x` for `x.`, stays.
fn test_a_folder_removal_takes_only_the_empty_folder_it_names() ! {
	directory := toolcache_test_dir('wipe_remove_folder_named')
	defer {
		os.rmdir_all(directory) or {}
	}
	os.mkdir_all(os.join_path(directory, 'full'))!
	os.write_file(os.join_path(directory, 'full', 'kept'), 'kept')!
	os.mkdir_all(os.join_path(directory, 'x'))!
	dir := open_tool_cache_wipe_dir(directory)!
	defer {
		dir.close()
	}
	mut failure := ''
	dir.remove_folder('full') or { failure = err.msg() }
	assert failure != ''
	assert os.is_file(os.join_path(directory, 'full', 'kept'))
	$if windows {
		failure = ''
		dir.remove_folder('x.') or { failure = err.msg() }
		assert failure.contains('its name ends in a dot or a space'), failure
		assert os.is_dir(os.join_path(directory, 'x'))
	}
	dir.remove_folder('x')!
	assert !os.exists(os.join_path(directory, 'x'))
}

// An entry that another wipe removes, with the folder it is in, while this wipe takes its build lock
// stays for nobody: the lock file that cannot be made in the removed folder does not fail the wipe.
fn test_an_entry_removed_with_its_folder_while_its_lock_is_taken_does_not_fail_the_wipe() ! {
	directory := toolcache_test_dir('wipe_cache_claim_folder_gone')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		aside := os.join_path(directory, 'vtmp', 'tools.untrusted-1700000000-42')
		tool_cache_test_root(aside)!
		entry := tool_cache_test_entry(aside, 'vfmt', 'b')!
		mut run := new_tool_cache_wipe_run()
		run.before_acting = fn [aside, entry] (path string, step string) {
			if path == entry && step == 'claim' {
				os.rmdir_all(aside) or { panic(err) }
			}
		}
		opts := parse_wipe_cache_options([], ['--tools'])!
		mut lines := []string{}
		mut failure := ''
		run.wipe_tool_cache_directory(aside, opts, mut lines) or { failure = err.msg() }
		assert failure == '', '${failure}\n' + lines.join('\n')
		assert !os.exists(aside)
	})!
}

// A folder beside a tool cache folder that is named as V names one it moved aside, belongs to another
// account, and lies in a folder others can write to, is left as it is, and does not fail the wipe:
// V moves aside only folders in the account's own folders, and anyone who can write to the folder
// around can put one there. The wipe acts as another account here, since a test cannot make files
// of one; the folder around is then not the wipe's either.
fn test_a_moved_aside_name_of_another_account_is_left_and_does_not_fail_the_wipe() ! {
	$if windows {
		// Nothing counts as another account's there.
		return
	}
	directory := toolcache_test_dir('wipe_cache_foreign_moved_aside')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		shared := os.join_path(directory, 'shared')
		os.mkdir_all(shared)!
		// Others can write to it; only who owns an item decides for the wipe's own account.
		os.chmod(shared, 0o777)!
		configured := os.join_path(shared, 'vtools')
		os.setenv(tool_cache_dir_env, configured, true)
		// Every name the tools pass goes through: V's shape of a moved-aside folder, any other
		// `.untrusted-` name, as a folder, a link or a file, and the configured folder itself.
		planted := [os.join_path(shared, 'vtools.untrusted-1-2'),
			os.join_path(shared, 'vtools.untrusted-3-4'), os.join_path(shared, 'vtools.untrusted-x'),
			os.join_path(shared, 'vtools.untrusted-evil'), configured]
		os.mkdir_all(planted[0])!
		os.symlink(directory, planted[1])!
		os.mkdir_all(planted[2])!
		os.write_file(os.join_path(planted[2], 'inside'), 'x')!
		os.write_file(planted[3], 'x')!
		os.mkdir_all(configured)!
		os.write_file(os.join_path(configured, 'inside'), 'x')!
		for dry in [true, false] {
			mut run := new_tool_cache_wipe_run()
			run.user = i64(os.getuid()) + 1
			arguments := if dry { ['--tools', '--dry-run'] } else { ['--tools'] }
			opts := parse_wipe_cache_options([], arguments)!
			mut lines := []string{}
			mut failure := ''
			run.wipe_tool_places(opts, mut lines) or { failure = err.msg() }
			output := '${arguments}: ${failure}\n' + lines.join('\n')
			assert failure == '', output
			assert os.is_dir(planted[0]) && os.is_link(planted[1]), output
			assert os.is_file(os.join_path(planted[2], 'inside')), output
			assert os.is_file(planted[3]), output
			assert os.is_file(os.join_path(configured, 'inside')), output
			for place in planted {
				label := tool_cache_wipe_label(place)
				assert output.contains(if dry {
					'Would leave ${label} folder ${place} as it is: it belongs to another account, in a folder others can write to.'
				} else {
					'${label} folder ${place} was left as it is: it belongs to another account, in a folder others can write to.'
				}), output
			}
		}
		// The superuser takes what any account owns, as everywhere else in the wipe.
		mut run := new_tool_cache_wipe_run()
		run.user = 0
		mut lines := []string{}
		run.wipe_tool_places(parse_wipe_cache_options([], ['--tools'])!, mut lines)!
		output := lines.join('\n')
		assert !os.exists(planted[0]) && !os.is_link(planted[1]), output
		assert !os.exists(planted[2]) && !os.exists(planted[3]), output
		assert os.ls(configured)! == []string{}, output
		assert !output.contains('left as it is'), output
		// What the wipe's own account owns is its own, also in a folder others can write to.
		own := os.join_path(shared, 'vtools.untrusted-5-6')
		os.mkdir_all(own)!
		os.write_file(os.join_path(own, 'inside'), 'x')!
		lines.clear()
		mut own_run := new_tool_cache_wipe_run()
		own_run.wipe_tool_places(parse_wipe_cache_options([], ['--tools'])!, mut lines)!
		assert !os.exists(own), lines.join('\n')
	})!
}

// What another account owns in a folder that is not the wipe's own alone stays, in every pass, and
// does not fail the wipe; the summary counts it. Here the wipe acts as another account, so everything
// the test made belongs to someone else: in VTMP, which holds tool cache folders, and in VCACHE, which
// holds none and is otherwise removed whole and made again. Run as the superuser, the wipe takes it.
fn test_what_another_account_owns_in_a_cache_folder_stays_and_does_not_fail_the_wipe() ! {
	$if windows {
		// Nothing counts as another account's there.
		return
	}
	directory := toolcache_test_dir('wipe_cache_foreign_in_cache_folders')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		vcache := os.join_path(directory, 'vcache')
		os.mkdir_all(vcache)!
		for user in [i64(os.getuid()) + 1, 0] {
			for dry in [true, false] {
				if user == 0 && dry {
					continue
				}
				os.write_file(os.join_path(vtmp, 'junk'), 'x')!
				os.mkdir_all(os.join_path(vtmp, 'tools.untrusted-x'))!
				os.write_file(os.join_path(vcache, 'stray'), 'x')!
				mut run := new_tool_cache_wipe_run()
				run.user = user
				arguments := if dry {
					['--vtmp', '--vcache', '--dry-run']
				} else {
					['--vtmp', '--vcache']
				}
				opts := parse_wipe_cache_options([], arguments)!
				mut lines := []string{}
				mut failure := ''
				run.wipe_folder_around_tool_roots(vtmp, 'V tmp.c and tests', true, opts, mut
					lines) or { failure = err.msg() }
				run.wipe_folder_around_tool_roots(vcache, 'V cache', true, opts, mut lines) or {
					failure += err.msg()
				}
				output := '${user} ${arguments}: ${failure}\n' + lines.join('\n')
				assert failure == '', output
				if user == 0 {
					assert os.ls(vtmp)! == [], output
					assert os.ls(vcache)! == [], output
					continue
				}
				assert os.is_file(os.join_path(vtmp, 'junk')), output
				assert os.is_dir(os.join_path(vtmp, 'tools.untrusted-x')), output
				assert os.is_file(os.join_path(vcache, 'stray')), output
				if dry {
					assert output.contains('Would wipe V tmp.c and tests folder ${vtmp}, except for 2 files and folders of other users.'), output
					assert output.contains('Would wipe V cache folder ${vcache}, except for 1 files and folders of other users.'), output
				} else {
					assert output.contains('V tmp.c and tests folder ${vtmp} was wiped, except for 2 files and folders of other users.'), output
					assert output.contains('V cache folder ${vcache} was wiped, except for 1 files and folders of other users.'), output
				}
			}
		}
		// The V3 cache folders that VTMP holds, when V3CACHE names no folder of its own. A folder with
		// another name is none, whoever owns it, and one with a V3 name that is a link goes as a link.
		v3 := os.join_path(vtmp, 'v3_crun_cache')
		not_v3 := os.join_path(vtmp, 'not_v3')
		v3_link := os.join_path(vtmp, 'v3_fastc_unit_cache')
		os.mkdir_all(v3)!
		os.write_file(os.join_path(v3, 'object'), 'x')!
		os.mkdir_all(not_v3)!
		os.write_file(os.join_path(not_v3, 'kept'), 'made by the V3 case')!
		os.symlink(os.join_path(directory, 'nowhere'), v3_link)!
		for user in [i64(os.getuid()) + 1, 0] {
			mut run := new_tool_cache_wipe_run()
			run.user = user
			mut lines := []string{}
			run.wipe_v3_folders_in(vtmp, parse_wipe_cache_options([], ['--v3cache'])!, mut lines)!
			output := '${user}\n' + lines.join('\n')
			assert os.read_file(os.join_path(not_v3, 'kept'))! == 'made by the V3 case', output
			assert !output.contains('not_v3'), output
			// Some V3 cache folder was wiped or left, so the wipe does not say that none exist (in a
			// dry run) or that they were already clean.
			assert !output.contains('V3 cache folders in'), output
			if user == 0 {
				assert !os.exists(v3), output
				assert !os.is_link(v3_link), output
			} else {
				assert os.is_file(os.join_path(v3, 'object')), output
				assert os.is_link(v3_link), output
				assert output.contains('V3 cache folder ${v3} was left as it is: it belongs to another account, in a folder others can write to.'), output
			}
		}
	})!
}

// A name that went since the listing, or that cannot be looked at, is not counted as what another
// account owns: the wipe decides on it otherwise (see `tool_cache_wipe_leaves`).
fn test_a_name_that_went_or_cannot_be_looked_at_is_not_left_to_another_account() {
	me := u32(os.getuid())
	other := me + 1
	for kind in [ToolCacheWipeKind.gone, .unknown] {
		assert !tool_cache_wipe_leaves(me, other, 0o1777, ToolCacheWipeItem{ kind: kind, owner: other })
	}
	$if !windows {
		if me != 0 {
			// The same item that is there is left, in a folder others can write to.
			assert tool_cache_wipe_leaves(me, other, 0o1777, ToolCacheWipeItem{
				kind:  .file
				owner: other
			})
		}
	}
}

// Listing what other accounts own in a folder lets the folder go again (see `others_in`).
fn test_listing_what_another_account_owns_lets_the_folder_go() ! {
	$if !linux {
		return
	}
	directory := toolcache_test_dir('wipe_others_in_closes')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		folder := os.join_path(directory, 'listed')
		os.mkdir_all(folder)!
		os.write_file(os.join_path(folder, 'a'), 'x')!
		run := new_tool_cache_wipe_run()
		before := os.ls('/proc/self/fd')!.len
		for _ in 0 .. 5 {
			run.others_in(folder)
		}
		assert os.ls('/proc/self/fd')!.len == before
	})!
}

// The list of tool cache candidates that `--tools --dry-run` shows names every default folder with
// what a build does with it: one that is not there is made, a private one is used, and outside
// Windows a link is moved aside. A `VTOOLS_CACHE_DIR` that is a folder exists, and one that is a
// link leading nowhere is no folder.
fn test_the_candidate_list_names_every_default_folder_and_what_a_build_does_with_it() ! {
	directory := toolcache_test_dir('wipe_candidates')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (cache string) ! {
		cache_tools := os.join_path(cache, 'tools')
		vtmp_tools := os.join_path(directory, 'vtmp', 'tools')
		configured := os.join_path(directory, 'configured')
		os.rmdir_all(cache_tools) or {}
		tool_cache_test_root(vtmp_tools)!
		tool_cache_test_root(configured)!
		os.setenv(tool_cache_dir_env, configured, true)
		configured_output := wipe_output('--tools', '--dry-run')!
		os.unsetenv(tool_cache_dir_env)
		assert configured_output.contains('`${configured}` (${tool_cache_dir_env}): '), configured_output
		assert !configured_output.contains('`${configured}` (${tool_cache_dir_env}): does not exist'), configured_output
		mut output := wipe_output('--tools', '--dry-run')!
		assert output.contains('`${cache_tools}`: usable (will be created on demand)'), output
		$if !windows {
			// On Windows a folder is private by its access list, which `tool_cache_test_root` leaves
			// as the folder above has it.
			assert configured_output.contains('`${configured}` (${tool_cache_dir_env}): usable (exists)'), configured_output
			assert output.contains('`${vtmp_tools}`: usable (exists)'), output
			os.rmdir_all(vtmp_tools)!
			os.symlink(os.join_path(directory, 'elsewhere'), vtmp_tools)!
			dangling := os.join_path(directory, 'dangling')
			os.symlink(os.join_path(directory, 'nowhere'), dangling)!
			os.setenv(tool_cache_dir_env, dangling, true)
			defer {
				os.unsetenv(tool_cache_dir_env)
			}
			output = wipe_output('--tools', '--dry-run')!
			assert output.contains('`${vtmp_tools}`: untrusted symbolic link (will be moved aside on next tool build)'), output
			assert output.contains('`${dangling}` (${tool_cache_dir_env}): rejected: is not a directory'), output
		}
	})!
}

// A wipe of the tool caches alone that finds none says so; one that wipes some, that wipes other
// cache folders too, or that lists what it does, does not.
fn test_only_a_tools_wipe_that_finds_nothing_says_they_were_clean() ! {
	directory := toolcache_test_dir('wipe_cache_tools_clean')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn (cache string) ! {
		clean := 'V tool cache folders were already clean.'
		dry := wipe_output('--tools', '--dry-run')!
		assert dry.contains('No existing V tool cache folders to wipe.'), dry
		mut output := wipe_output('--tools')!
		assert output.contains(clean), output
		for more in ['--vcache', '--vtmp', '--v3cache', '--verbose'] {
			output = wipe_output('--tools', more)!
			assert !output.contains(clean), '${more}: ${output}'
		}
		tools := os.join_path(cache, 'tools')
		tool_cache_test_root(tools)!
		tool_cache_test_entry(tools, 'vfmt', 'a')!
		output = wipe_output('--tools')!
		assert !output.contains(clean), output
	})!
}

// A cache folder that is the tool cache folder too, which the tools pass dealt with first, is known
// as dealt with also when the setting of the cache folder reaches it through a link and the tool
// cache setting does not: its own pass leaves it as it is, and the wipe says so.
fn test_a_cache_folder_an_earlier_pass_dealt_with_is_known_through_a_link_in_its_path() ! {
	$if windows {
		// Making a symbolic link takes a privilege there.
		return
	}
	directory := toolcache_test_dir('wipe_cache_earlier_pass')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		real := os.join_path(directory, 'real')
		tool_cache_test_root(os.join_path(real, 'vtmp'))!
		os.symlink(real, os.join_path(directory, 'via'))!
		vtmp := os.join_path(directory, 'via', 'vtmp')
		os.setenv('VTMP', vtmp, true)
		os.setenv(tool_cache_dir_env, os.join_path(real, 'vtmp'), true)
		dry := wipe_output('--tools', '--vtmp', '--dry-run')!
		assert dry.contains('Would leave V tmp.c and tests folder ${vtmp} as it is: it is a tool cache folder too, which an earlier pass of this wipe dealt with.'), dry
		output := wipe_output('--tools', '--vtmp')!
		assert output.contains('V tmp.c and tests folder ${vtmp} was left as it is: it is a tool cache folder too, which an earlier pass of this wipe dealt with.'), output
		// It is left as it is indeed: a build that finishes between the passes keeps its entry.
		mut run := new_tool_cache_wipe_run()
		opts := parse_wipe_cache_options([], ['--tools', '--vtmp'])!
		mut lines := []string{}
		run.wipe_tool_places(opts, mut lines)!
		entry := tool_cache_test_entry(os.join_path(real, 'vtmp'), 'vfmt', 'b')!
		run.wipe_folder_around_tool_roots(vtmp, 'V tmp.c and tests', true, opts, mut lines)!
		assert os.is_dir(entry), lines.join('\n')
	})!
}

// A cache folder that is missing or a file when the wipe starts, with the tool cache folder below
// it, is made again by its pass, and is no tool cache folder for a later pass that holds it.
fn test_a_cache_folder_the_wipe_makes_again_is_no_tool_cache_folder_for_a_later_pass() ! {
	directory := toolcache_test_dir('wipe_cache_made_again')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		vcache := os.join_path(vtmp, 'vcache')
		os.setenv('VCACHE', vcache, true)
		os.setenv(tool_cache_dir_env, os.join_path(vcache, 'tools'), true)
		for how in ['missing', 'file'] {
			if how == 'file' {
				os.write_file(vcache, 'made by the file case')!
			}
			output := wipe_output('--vcache', '--vtmp')!
			context := '${how}: ${output}'
			assert output.contains('V cache folder ${vcache} was wiped.'), context
			assert output.contains('V tmp.c and tests folder ${vtmp} was wiped.'), context
			assert os.is_dir(vcache), context
			os.rmdir_all(vcache)!
		}
	})!
}

// On Windows a build refuses a default tool cache folder that is a link, and tries the next one; the
// list of candidates that `--dry-run` shows says so, and not that the folder will be moved aside.
fn test_the_candidate_list_says_what_a_windows_build_does_with_a_link() ! {
	$if !windows {
		return
	}
	directory := toolcache_test_dir('wipe_cache_candidate_link')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		root := os.join_path(directory, 'vtmp', 'tools')
		target := os.join_path(directory, 'elsewhere')
		os.mkdir_all(target)!
		if !toolcache_test_junction(root, target) {
			return
		}
		output := wipe_output('--tools', '--dry-run')!
		assert output.contains('`${root}`: rejected: it is a symbolic link'), output
		assert !output.contains('moved aside'), output
	})!
}

// The same for a default tool cache folder that is a real folder but not private: a Windows build
// refuses it too, and the list says why.
fn test_the_candidate_list_says_what_a_windows_build_does_with_a_folder_that_is_not_private() ! {
	$if !windows {
		return
	}
	directory := toolcache_test_dir('wipe_cache_candidate_not_private')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn (cache_v_dir string) ! {
		root := os.join_path(cache_v_dir, 'tools')
		tool_cache_test_root(root)!
		// Everyone may change it.
		system32 := os.join_path(os.getenv('SystemRoot'), 'System32')
		granted := os.exec([os.join_path(system32, 'icacls.exe'), root, '/grant', '*S-1-1-0:(OI)(CI)M'])
		assert granted.exit_code == 0, granted.output
		output := wipe_output('--tools', '--dry-run')!
		assert output.contains('`${root}`: rejected: '), output
		assert !output.contains('moved aside'), output
	})!
}

// Each setting the wipe reads fails the wipe, before anything is removed, when it is a relative path
// and the folder the wipe was started from has no name: `VTOOLS_CACHE_DIR`, `VCACHE`, `VTMP`,
// `V3CACHE`, `XDG_CACHE_HOME`, `HOME`, `USERPROFILE`, `TMPDIR`, and the first folder in `VMODULES`.
fn test_every_relative_setting_without_a_working_folder_fails_the_wipe() ! {
	$if windows {
		// The folder a process works in cannot be removed there.
		return
	}
	directory := toolcache_test_dir('wipe_cache_each_relative_setting')
	previous_dir := os.getwd()
	names := [tool_cache_dir_env, 'VCACHE', 'VTMP', 'V3CACHE', 'XDG_CACHE_HOME', 'HOME', 'USERPROFILE',
		'TMPDIR', 'VMODULES']
	mut previous := map[string]?string{}
	for name in names {
		previous[name] = os.getenv_opt(name)
	}
	defer {
		os.chdir(previous_dir) or {}
		for name, value in previous {
			if v := value {
				os.setenv(name, v, true)
			} else {
				os.unsetenv(name)
			}
		}
		os.rmdir_all(directory) or {}
	}
	gone := os.join_path(directory, 'gone')
	os.mkdir_all(gone)!
	os.chdir(gone)!
	os.rmdir(gone)!
	for name in names {
		for other in names {
			os.unsetenv(other)
		}
		os.setenv(name, 'relative', true)
		mut failure := ''
		tool_cache_wipe_check_settings() or { failure = err.msg() }
		assert failure.contains('${name} holds the relative path `relative`'), '${name}: ${failure}'
	}
	for other in names {
		os.unsetenv(other)
	}
	tool_cache_wipe_check_settings()!
}

// A dangling link at a default tool cache path is removed, as any link there.
fn test_a_dangling_link_at_a_default_tool_cache_path_is_removed() ! {
	$if windows {
		// A link to a folder needs rights a test may lack.
		return
	}
	directory := toolcache_test_dir('wipe_cache_dangling_default')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		root := os.join_path(directory, 'vtmp', 'tools')
		os.symlink(os.join_path(directory, 'nowhere'), root)!
		output := wipe_output('--tools')!
		assert !os.is_link(root), output
	})!
}

// A `VTOOLS_CACHE_DIR` link inside VTMP that leads nowhere stays, and the summary of VTMP names it
// among the tool cache folders it kept.
fn test_a_dangling_configured_link_in_vtmp_is_named_as_kept() ! {
	$if windows {
		// A link to a folder needs rights a test may lack.
		return
	}
	directory := toolcache_test_dir('wipe_cache_dangling_configured')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		link := os.join_path(vtmp, 'mytools')
		os.symlink(os.join_path(directory, 'nowhere'), link)!
		os.setenv(tool_cache_dir_env, link, true)
		output := wipe_output('--vtmp')!
		assert os.is_link(link), output
		assert output.contains('was wiped, except the tool cache folders in it: ${link}.'), output
	})!
}

// The wipe of a folder around tool cache folders attempts every item in it, and names the first one
// that stayed.
fn test_the_wipe_around_tool_cache_folders_names_the_first_item_that_stayed() ! {
	$if windows {
		return
	}
	if os.geteuid() == 0 {
		// The superuser removes from a folder whatever its mode is.
		return
	}
	directory := toolcache_test_dir('wipe_cache_first_failure_around')
	defer {
		toolcache_test_unlock_tree(directory)
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		tool_cache_test_root(os.join_path(vtmp, 'tools'))!
		for name in ['a-stuck', 'b-stuck'] {
			stuck := os.join_path(vtmp, name)
			os.mkdir_all(stuck)!
			os.write_file(os.join_path(stuck, 'file'), 'x')!
			os.chmod(stuck, 0o500)!
		}
		mut failure := ''
		wipe_output('--vtmp') or { failure = err.msg() }
		assert failure.contains(os.join_path(vtmp, 'a-stuck', 'file') + ': '), failure
	})!
}

// A dry run lets go of each build lock as soon as it has taken it: a build that starts while the dry
// run is still in the folder does not wait.
fn test_a_dry_run_lets_go_of_each_build_lock_at_once() ! {
	directory := toolcache_test_dir('wipe_cache_dry_run_locks')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (cache_v_dir string) ! {
		root := os.join_path(cache_v_dir, 'tools')
		tool_cache_test_root(root)!
		entry := tool_cache_test_entry(root, 'vfmt', 'a')!
		lock_path := tool_cache_test_lock(root, 'vfmt')
		os.write_file(lock_path, '')!
		free := os.join_path(directory, 'free')
		release := os.join_path(directory, 'release')
		mut run := new_tool_cache_wipe_run()
		run.before_acting = fn [entry, lock_path, free, release] (path string, step string) {
			if path == entry && step == 'claim' {
				mut holder := start_tool_cache_lock_holder(lock_path, release) or { return }
				os.write_file(release, '') or { panic(err) }
				holder.wait()
				holder.close()
				os.write_file(free, '') or { panic(err) }
			}
		}
		opts := parse_wipe_cache_options([], ['--tools', '--dry-run'])!
		mut lines := []string{}
		run.wipe_tool_cache_directory(root, opts, mut lines)!
		assert os.exists(free), lines.join('\n')
	})!
}

// A lock file that the wipe sets to mode 0600 and cannot, so that builds cannot lock it either,
// fails the wipe, which names it, also when no entry of its tool is there.
fn test_a_lock_file_that_cannot_be_repaired_fails_the_wipe() ! {
	$if windows {
		return
	}
	if os.geteuid() == 0 {
		// The superuser opens any file, so none needs to be repaired.
		return
	}
	directory := toolcache_test_dir('wipe_cache_repair_fails')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (cache_v_dir string) ! {
		root := os.join_path(cache_v_dir, 'tools')
		tool_cache_test_root(root)!
		lock_path := tool_cache_test_lock(root, 'vfmt')
		os.write_file(lock_path, '')!
		os.chmod(lock_path, 0o400)!
		nowhere := os.join_path(directory, 'nowhere')
		mut run := new_tool_cache_wipe_run()
		// What is there by the time the mode is set is a link that leads nowhere.
		run.before_acting = fn [lock_path, nowhere] (path string, step string) {
			if path == lock_path && step == 'repair' {
				os.rm(lock_path) or { panic(err) }
				os.symlink(nowhere, lock_path) or { panic(err) }
			}
		}
		opts := parse_wipe_cache_options([], ['--tools'])!
		mut lines := []string{}
		mut failure := ''
		run.wipe_tool_cache_directory(root, opts, mut lines) or { failure = err.msg() }
		assert failure.contains('cannot repair the lock file ${lock_path}'), '${failure}\n' + lines.join('\n')
	})!
}

// A `VTOOLS_CACHE_DIR` that is set but empty names no folder, and is no tool cache folder the wipe
// knows: not the folder the wipe was started from either, which the wipe of a cache folder that
// holds it removes.
fn test_an_empty_tool_cache_setting_names_no_folder() ! {
	directory := toolcache_test_dir('wipe_cache_empty_setting')
	previous := os.getwd()
	defer {
		os.chdir(previous) or {}
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory, previous] (_ string) ! {
		os.setenv(tool_cache_dir_env, ' ', true)
		assert tool_cache_wipe_targets().all(it != '' && it != os.getwd())
		output := wipe_output('--tools', '--verbose')!
		assert !output.contains('folder  does not exist'), output
		$if !windows {
			// Windows does not remove the folder a process works in.
			sub := os.join_path(directory, 'vtmp', 'sub')
			os.mkdir_all(sub)!
			os.chdir(sub)!
			around := wipe_output('--vtmp') or {
				os.chdir(previous)!
				return err
			}
			os.chdir(previous)!
			assert !os.exists(sub), around
		}
	})!
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

		// In a folder on the way to a tool cache folder, which is cleared around that one.
		way := os.join_path(vtmp, 'way')
		os.setenv(tool_cache_dir_env, os.join_path(way, 'tools'), true)
		os.mkdir_all(way)!
		stuck = toolcache_test_stuck_folder(way)!
		message = wipe_failure('--vtmp')
		assert message.starts_with('v wipe-cache: could not remove V tmp.c and tests folder ${vtmp}: ${os.join_path(stuck, 'file')}: '), message
		assert message.count('v wipe-cache:') == 1, message
		assert !os.exists(os.join_path(way, 'z-after'))
		toolcache_test_unlock_tree(vtmp)
		os.rmdir_all(way)!

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

		// The V3 cache folders in VTMP: a folder that cannot be listed is not taken for an empty one.
		os.setenv('V3CACHE', '', true)
		os.chmod(vtmp, 0o300)!
		message = wipe_failure('--v3cache')
		os.chmod(vtmp, 0o700)!
		assert message.starts_with('v wipe-cache: could not list V3 cache folders in ${vtmp}: '), message
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
			assert !new_tool_cache_wipe_run().is_moved_aside(root), root
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
// another user keep the others, and the folder, in place. A lock file of this user that cannot be
// opened keeps them for no build, so the wipe then fails, and names it.
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
		if stuck_is_unopenable {
			for arguments in [['--tools', '--dry-run'], ['--tools']] {
				message := wipe_failure(...arguments)
				assert message == 'v wipe-cache: could not remove Moved-aside V tool cache folder ${stuck_aside}: the lock file ${stuck_lock} cannot be opened', '${arguments}: ${message}'
				assert os.is_file(stuck_lock), message
				assert os.is_file(beside_stuck), message
			}
			os.chmod(stuck_lock, 0o600)!
		}
		// Both locks are the wipe's to take now, so the folder goes whole.
		again := wipe_output('--tools')!
		assert !os.exists(stuck_aside), again
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

// A folder named as V names one it moved aside is no such folder while a folder that V builds tools
// in lies in it, also when that is a link which leads elsewhere: a build replaces the link by a real
// folder, in which it builds.
fn test_a_folder_named_as_moved_aside_that_holds_a_used_link_is_not_one() ! {
	$if windows {
		// A link to a folder needs rights a test may lack.
		return
	}
	directory := toolcache_test_dir('moved_aside_holds_used_link')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		named := os.join_path(directory, 'tools.untrusted-1700000000-7')
		outside := os.join_path(directory, 'outside')
		tool_cache_test_root(named)!
		tool_cache_test_root(outside)!
		inner := os.join_path(named, 'inner')
		os.symlink(outside, inner)!
		assert new_tool_cache_wipe_run().is_moved_aside(named)
		os.setenv(tool_cache_dir_env, inner, true)
		assert !new_tool_cache_wipe_run().is_moved_aside(named)
	})!
}

// What is no folder at the path of a tool cache folder goes as it is also when the folder around it
// lets this user change and search it, but not list it.
fn test_what_is_no_folder_at_a_tool_cache_path_goes_in_a_folder_that_cannot_be_listed() ! {
	$if windows {
		return
	}
	if os.getuid() == 0 {
		// The superuser lists any folder.
		return
	}
	directory := toolcache_test_dir('wipe_cache_unlistable_parent')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		parent := os.join_path(directory, 'unlistable')
		os.mkdir_all(parent)!
		stand_in := os.join_path(parent, 'tools')
		os.write_file(stand_in, 'no folder')!
		os.setenv(tool_cache_dir_env, stand_in, true)
		os.chmod(parent, 0o300)!
		defer {
			os.chmod(parent, 0o700) or {}
		}
		output := wipe_output('--tools') or { err.msg() }
		assert !os.exists(stand_in), output
		assert output.contains('V tool cache folder ${stand_in} was wiped.'), output
	})!
}

// toolcache_test_pending_build makes the tool cache folder `pending` with an entry of vfmt, whose
// build lock another process holds until `release` exists. It returns that process and the names of
// the entry and of the lock. A test renames `pending` into place to play a build that makes a new
// tool cache folder and builds in it.
fn toolcache_test_pending_build(pending string, release string) !(&os.Process, string, string) {
	tool_cache_test_root(pending)!
	entry := tool_cache_test_entry(pending, 'vfmt', 'e')!
	lock_path := tool_cache_test_lock(pending, 'vfmt')
	os.write_file(lock_path, '')!
	holder := start_tool_cache_lock_holder(lock_path, release)!
	return holder, os.file_name(entry), os.file_name(lock_path)
}

// A tool cache folder the wipe knows is known by where it lies, not only by the folder it led to
// when the wipe started. A build moves a link at a default folder aside and makes a real folder there
// (see `prepare_default_tool_cache_root`); the build in that new folder keeps its entry and its
// lock, with `--vtmp` alone and with the tools pass run before it, as `wipe_caches` runs them.
fn test_a_known_folder_made_anew_after_the_wipe_started_keeps_its_build() ! {
	$if windows {
		// V moves no folder aside there, and refuses a link as a tool cache folder.
		return
	}
	directory := toolcache_test_dir('wipe_cache_link_root_replaced')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		root := os.join_path(vtmp, 'tools')
		outside := os.join_path(directory, 'outside-tools')
		tool_cache_test_root(outside)!
		os.write_file(os.join_path(outside, 'keep'), 'x')!
		for round, arguments in [['--vtmp'], []string{}] {
			os.symlink(outside, root)!
			mut run := new_tool_cache_wipe_run()
			// The build's own step, after the wipe has looked at the folders it knows.
			os.rename(root, '${root}.untrusted-1700000000-${round}')!
			release := os.join_path(directory, 'release-${round}')
			mut holder, entry_name, lock_name := toolcache_test_pending_build(os.join_path(directory,
				'pending-${round}'), release)!
			os.rename(os.join_path(directory, 'pending-${round}'), root)!
			other := os.join_path(vtmp, 'other.tmp.c')
			os.write_file(other, 'x')!
			opts := parse_wipe_cache_options([], arguments)!
			mut lines := []string{}
			mut failure := ''
			if opts.tools {
				run.wipe_tool_cache_directory(root, opts, mut lines) or { failure = err.msg() }
			}
			if failure == '' {
				run.wipe_folder_around_tool_roots(vtmp, 'V tmp.c and tests', true, opts, mut
					lines) or { failure = err.msg() }
			}
			os.write_file(release, '')!
			holder.wait()
			holder.close()
			output := '${arguments}: ${failure}\n' + lines.join('\n')
			assert failure == '', output
			assert os.is_dir(os.join_path(root, entry_name)), output
			assert os.is_file(os.join_path(root, lock_name)), output
			assert !os.exists(other), output
			assert os.is_file(os.join_path(outside, 'keep')), output
			assert output.contains('V tmp.c and tests folder ${vtmp} was wiped, except the tool cache folders in it: ${root}.'), output
			os.rmdir_all(root)!
		}
	})!
}

// A tool cache folder the wipe knows, or a folder that leads to one, is wiped as what opening it
// finds, whatever an earlier look saw: a build can move a tool cache folder aside and make a new one
// at any moment (see `prepare_default_tool_cache_root`). Here, in the folder around it, the wipe
// looks at the name while the build has moved the folder aside, and the build makes the new one, and
// builds in it, before the wipe opens the name. The build is kept.
fn test_a_known_folder_made_anew_before_the_wipe_opens_its_name_keeps_its_build() ! {
	$if windows {
		// V moves no folder aside there.
		return
	}
	directory := toolcache_test_dir('wipe_cache_root_made_anew')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		root := os.join_path(vtmp, 'tools')
		tool_cache_test_root(root)!
		idle := tool_cache_test_entry(root, 'vdoc', 'a')!
		os.write_file(os.join_path(vtmp, 'a-junk'), 'x')!
		release := os.join_path(directory, 'release')
		pending := os.join_path(directory, 'pending')
		mut holder, entry_name, lock_name := toolcache_test_pending_build(pending, release)!
		mut run := new_tool_cache_wipe_run()
		run.before_acting = fn [vtmp, root, pending] (path string, _ string) {
			if path == os.join_path(vtmp, 'a-junk') && os.is_dir(root) {
				os.rename(root, '${root}.untrusted-1700000000-1') or { panic(err) }
			} else if path == root && !os.exists(root) {
				os.rename(pending, root) or { panic(err) }
			}
		}
		opts := parse_wipe_cache_options([], ['--vtmp'])!
		mut lines := []string{}
		mut failure := ''
		run.wipe_folder_around_tool_roots(vtmp, 'V tmp.c and tests', true, opts, mut lines) or {
			failure = err.msg()
		}
		os.write_file(release, '')!
		holder.wait()
		holder.close()
		output := '${failure}\n' + lines.join('\n')
		assert !os.exists(pending), output
		assert failure == '', output
		assert os.is_dir(os.join_path(root, entry_name)), output
		assert os.is_file(os.join_path(root, lock_name)), output
		// The folder the build moved aside after the wipe listed VTMP is left for a later wipe, with
		// what is in it.
		assert os.is_dir('${root}.untrusted-1700000000-1'), output
		assert os.is_dir(os.join_path('${root}.untrusted-1700000000-1', os.file_name(idle))), output
	})!
}

// The same for a tool cache folder wiped by itself: the wipe sees a link at its path, and the build
// moves the link aside and makes the new folder, and builds in it, before the wipe opens the path.
// The build is kept, and what the link pointed to is not touched.
fn test_a_known_folder_made_anew_where_a_link_was_keeps_its_build() ! {
	$if windows {
		// V moves no folder aside there, and a link to a folder needs rights a test may lack.
		return
	}
	directory := toolcache_test_dir('wipe_cache_link_made_anew')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (cache_v_dir string) ! {
		root := os.join_path(cache_v_dir, 'tools')
		outside := os.join_path(directory, 'outside')
		tool_cache_test_root(outside)!
		os.write_file(os.join_path(outside, 'keep'), 'x')!
		os.mkdir_all(cache_v_dir)!
		os.symlink(outside, root)!
		release := os.join_path(directory, 'release')
		pending := os.join_path(directory, 'pending')
		mut holder, entry_name, lock_name := toolcache_test_pending_build(pending, release)!
		mut run := new_tool_cache_wipe_run()
		run.before_acting = fn [root, pending] (path string, _ string) {
			if path == root && os.is_link(root) {
				os.rename(root, '${root}.untrusted-1700000000-2') or { panic(err) }
				os.rename(pending, root) or { panic(err) }
			}
		}
		opts := parse_wipe_cache_options([], ['--tools'])!
		mut lines := []string{}
		mut failure := ''
		run.wipe_tool_cache_directory(root, opts, mut lines) or { failure = err.msg() }
		os.write_file(release, '')!
		holder.wait()
		holder.close()
		output := '${failure}\n' + lines.join('\n')
		assert !os.exists(pending), output
		assert failure == '', output
		assert os.is_dir(os.join_path(root, entry_name)), output
		assert os.is_file(os.join_path(root, lock_name)), output
		assert os.is_file(os.join_path(outside, 'keep')), output
	})!
}

// The same for a folder that holds a tool cache folder: the wipe finds it gone when it opens it, as
// when a wipe with other tool cache folders removed it, and a build makes it again, with a tool cache
// folder in which it builds, before the wipe goes on. The build is kept.
fn test_a_known_folder_made_anew_in_a_folder_made_anew_keeps_its_build() ! {
	$if windows {
		// The folder is renamed while a file in it is open, which Windows may refuse.
		return
	}
	directory := toolcache_test_dir('wipe_cache_vtmp_made_anew')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		tool_cache_test_root(os.join_path(vtmp, 'tools'))!
		aside := os.join_path(directory, 'vtmp-aside')
		release := os.join_path(directory, 'release')
		pending := os.join_path(directory, 'pending-vtmp')
		os.mkdir(pending, mode: 0o700)!
		mut holder, entry_name, lock_name := toolcache_test_pending_build(os.join_path(pending,
			'tools'), release)!
		mut run := new_tool_cache_wipe_run()
		run.before_acting = fn [vtmp, aside, pending] (path string, _ string) {
			if path != vtmp {
				return
			}
			if os.is_dir(vtmp) && !os.exists(aside) {
				os.rename(vtmp, aside) or { panic(err) }
			} else if !os.exists(vtmp) {
				os.rename(pending, vtmp) or { panic(err) }
			}
		}
		opts := parse_wipe_cache_options([], ['--vtmp'])!
		mut lines := []string{}
		mut failure := ''
		run.wipe_folder_around_tool_roots(vtmp, 'V tmp.c and tests', true, opts, mut lines) or {
			failure = err.msg()
		}
		os.write_file(release, '')!
		holder.wait()
		holder.close()
		output := '${failure}\n' + lines.join('\n')
		assert !os.exists(pending), output
		assert failure == '', output
		assert os.is_dir(os.join_path(vtmp, 'tools', entry_name)), output
		assert os.is_file(os.join_path(vtmp, 'tools', lock_name)), output
	})!
}

// The same when VTMP is spelled through a link: where a tool cache folder lies is taken from the
// real path of the folder around it, as the folder being wiped is.
fn test_a_known_folder_made_anew_under_a_linked_vtmp_keeps_its_build() ! {
	$if windows {
		// V moves no folder aside there, and a link to a folder needs rights a test may lack.
		return
	}
	directory := toolcache_test_dir('wipe_cache_linked_vtmp')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		base := os.join_path(directory, 'base')
		os.mkdir_all(os.join_path(base, 'vtmp'))!
		alias := os.join_path(directory, 'alias')
		os.symlink(base, alias)!
		vtmp := os.join_path(alias, 'vtmp')
		os.setenv('VTMP', vtmp, true)
		root := os.join_path(vtmp, 'tools')
		outside := os.join_path(directory, 'outside-tools')
		tool_cache_test_root(outside)!
		os.symlink(outside, root)!
		mut run := new_tool_cache_wipe_run()
		// The build's own step, after the wipe has looked at the folders it knows.
		os.rename(root, '${root}.untrusted-1700000000-5')!
		release := os.join_path(directory, 'release')
		pending := os.join_path(directory, 'pending')
		mut holder, entry_name, lock_name := toolcache_test_pending_build(pending, release)!
		os.rename(pending, root)!
		opts := parse_wipe_cache_options([], ['--vtmp'])!
		mut lines := []string{}
		mut failure := ''
		run.wipe_folder_around_tool_roots(vtmp, 'V tmp.c and tests', true, opts, mut lines) or {
			failure = err.msg()
		}
		os.write_file(release, '')!
		holder.wait()
		holder.close()
		output := '${failure}\n' + lines.join('\n')
		assert failure == '', output
		assert os.is_dir(os.join_path(root, entry_name)), output
		assert os.is_file(os.join_path(root, lock_name)), output
	})!
}

// Where a tool cache folder the wipe knows lies is worked out when a pass comes to it, also for one
// inside another tool cache folder: a build that moves a link at the outer folder aside and makes a
// real folder there (see `prepare_default_tool_cache_root`) after the wipe started puts the inner one
// at a new place. Here `VTOOLS_CACHE_DIR` names a folder inside `<VTMP>/tools`, or a link to one; the
// build in it keeps its entry and its lock in every pass that comes to it.
fn test_a_known_folder_inside_a_replaced_link_keeps_its_build() ! {
	$if windows {
		// V moves no folder aside there, and a link to a folder needs rights a test may lack.
		return
	}
	directory := toolcache_test_dir('wipe_cache_nested_link_replaced')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		root := os.join_path(vtmp, 'tools')
		inner := os.join_path(root, 'custom')
		outside := os.join_path(directory, 'outside-tools')
		linked := os.join_path(directory, 'my-tools')
		for round, arguments in [[]string{}, ['--tools'], ['--vtmp'], []string{}, ['--vtmp']] {
			// The last two rounds reach the inner folder through a link that `VTOOLS_CACHE_DIR` names.
			through_link := round >= 3
			tool_cache_test_root(os.join_path(outside, 'custom'))!
			os.symlink(outside, root)!
			if through_link {
				os.symlink(inner, linked)!
				os.setenv(tool_cache_dir_env, linked, true)
			} else {
				os.setenv(tool_cache_dir_env, inner, true)
			}
			mut run := new_tool_cache_wipe_run()
			// The steps of a build without that setting, after the wipe has looked at the folders it
			// knows; then a build with it.
			os.rename(root, '${root}.untrusted-1700000000-${round}')!
			os.mkdir(root, mode: 0o700)!
			release := os.join_path(directory, 'release-${round}')
			pending := os.join_path(directory, 'pending-${round}')
			mut holder, entry_name, lock_name := toolcache_test_pending_build(pending, release)!
			os.rename(pending, inner)!
			opts := parse_wipe_cache_options([], arguments)!
			mut lines := []string{}
			mut failure := ''
			if opts.tools {
				run.wipe_tool_places(opts, mut lines) or { failure = err.msg() }
			}
			if failure == '' && opts.vtmp {
				run.wipe_folder_around_tool_roots(vtmp, 'V tmp.c and tests', true, opts, mut
					lines) or { failure = err.msg() }
			}
			os.write_file(release, '')!
			holder.wait()
			holder.close()
			output := '${round} ${arguments}: ${failure}\n' + lines.join('\n')
			assert failure == '', output
			assert os.is_dir(os.join_path(inner, entry_name)), output
			assert os.is_file(os.join_path(inner, lock_name)), output
			os.rmdir_all(root)!
			os.rmdir_all(outside)!
			os.rm(linked) or {}
		}
	})!
}

// The maintainer's ordering on PR 31848 (review of 2026-11-20): the default `<VTMP>/tools` is a link
// to a folder outside VTMP; the builder's own preparation (`tool_cache_candidate_problems`) moves the
// link aside and makes the private folder; a staged `vfmt` entry is put there, and its lock held by
// another process; then the tools pass and the VTMP pass run in the default order, or the VTMP pass
// alone for `--vtmp`. The wipe takes its snapshot before the replacement, or after it. The staged
// entry and the lock file stay in every case, and the folder the link led to is not touched.
fn test_the_builders_own_replacement_of_a_linked_default_folder_keeps_its_build() ! {
	$if windows {
		// V moves no folder aside there, and a link to a folder needs rights a test may lack.
		return
	}
	directory := toolcache_test_dir('wipe_cache_builder_replacement')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		root := os.join_path(vtmp, 'tools')
		outside := os.join_path(directory, 'outside-tools')
		tool_cache_test_root(outside)!
		os.write_file(os.join_path(outside, 'keep'), 'x')!
		mut rows := []string{}
		for round, snapshot_first in [true, true, false, false] {
			arguments := if round % 2 == 0 { []string{} } else { ['--vtmp'] }
			os.rmdir_all(root) or {}
			os.symlink(outside, root)!
			early := new_tool_cache_wipe_run()
			problems := tool_cache_candidate_problems(root, true)
			assert problems.len == 0, problems.str()
			mut run := if snapshot_first { early } else { new_tool_cache_wipe_run() }
			entry := tool_cache_test_entry(root, 'vfmt', 'e')!
			lock_path := tool_cache_test_lock(root, 'vfmt')
			os.write_file(lock_path, '')!
			release := os.join_path(directory, 'release-${round}')
			mut holder := start_tool_cache_lock_holder(lock_path, release)!
			opts := parse_wipe_cache_options([], arguments)!
			mut lines := []string{}
			mut failure := ''
			if opts.tools {
				run.wipe_tool_cache_directory(root, opts, mut lines) or { failure = err.msg() }
			}
			if failure == '' {
				run.wipe_folder_around_tool_roots(vtmp, 'V tmp.c and tests', true, opts, mut
					lines) or { failure = err.msg() }
			}
			alive := holder.is_alive()
			os.write_file(release, '')!
			holder.wait()
			holder.close()
			snapshot := if snapshot_first { 'before' } else { 'after' }
			rows << '${snapshot} ${arguments}: failure=`${failure}` entry=${os.is_dir(entry)} lock=${os.is_file(lock_path)} holder=${alive} outside=${os.is_file(os.join_path(outside, 'keep'))}'
		}
		println(rows.join('\n'))
		for row in rows {
			assert row.ends_with('failure=`` entry=true lock=true holder=true outside=true'), rows.join('\n')
		}
	})!
}

// A tool cache folder the wipe knows, where one pass found a link or nothing, and which a build makes
// later in the wipe, is left as it is by the later passes too: the tools pass says the new folder was
// not touched, and the wipe of the folder around it does not wipe it after all. The build that made
// it keeps what it built there, also once it has let go of its lock.
fn test_a_known_folder_made_anew_during_the_tools_pass_is_left_by_later_passes() ! {
	$if windows {
		// V moves no folder aside there, and a link to a folder needs rights a test may lack.
		return
	}
	directory := toolcache_test_dir('wipe_cache_made_anew_between_passes')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		root := os.join_path(vtmp, 'tools')
		outside := os.join_path(directory, 'outside-tools')
		tool_cache_test_root(outside)!
		// The build moves a link aside as the tools pass is about to remove it, or about to open it,
		// or there was nothing at the path when the tools pass came to it.
		for round, step in ['remove', 'open', ''] {
			if step != '' {
				os.symlink(outside, root)!
			}
			pending := os.join_path(directory, 'pending-${round}')
			tool_cache_test_root(pending)!
			entry := tool_cache_test_entry(pending, 'vfmt', 'a')!
			os.write_file(tool_cache_test_lock(pending, 'vfmt'), '')!
			mut run := new_tool_cache_wipe_run()
			run.before_acting = fn [root, pending, step, round] (path string, at string) {
				if path == root && at == step && os.is_link(root) {
					os.rename(root, '${root}.untrusted-1700000000-${round}') or { panic(err) }
					if step == 'remove' {
						os.rename(pending, root) or { panic(err) }
					}
				}
			}
			opts := parse_wipe_cache_options([], [])!
			mut lines := []string{}
			mut failure := ''
			run.wipe_tool_places(opts, mut lines) or { failure = err.msg() }
			if os.exists(pending) {
				// The build makes its folder once the tools pass is past it.
				os.rename(pending, root)!
			}
			if failure == '' {
				run.wipe_folder_around_tool_roots(vtmp, 'V tmp.c and tests', true, opts, mut
					lines) or { failure = err.msg() }
			}
			output := '${round} ${step}: ${failure}\n' + lines.join('\n')
			assert failure == '', output
			assert os.is_dir(os.join_path(root, os.file_name(entry))), output
			assert !output.contains('V tool cache folder ${root} was wiped'), output
			assert output.contains('V tmp.c and tests folder ${vtmp} was wiped, except the tool cache folders in it: ${root}.'), output
			os.rmdir_all(root)!
		}
	})!
}

// A tool cache folder the wipe could not open, because a link was at its path, and that a build has
// made since (see `prepare_default_tool_cache_root`), is opened once more and wiped as the folder it
// now is: the build in it keeps its entry and its lock, and the wipe does not fail.
fn test_a_link_replaced_by_a_folder_while_the_wipe_opens_it_is_opened_again() ! {
	$if windows {
		// V moves no folder aside there, and a link to a folder needs rights a test may lack.
		return
	}
	directory := toolcache_test_dir('wipe_cache_open_again')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (cache_v_dir string) ! {
		root := os.join_path(cache_v_dir, 'tools')
		outside := os.join_path(directory, 'outside')
		tool_cache_test_root(outside)!
		os.mkdir_all(cache_v_dir)!
		os.symlink(outside, root)!
		release := os.join_path(directory, 'release')
		pending := os.join_path(directory, 'pending')
		mut holder, entry_name, lock_name := toolcache_test_pending_build(pending, release)!
		mut run := new_tool_cache_wipe_run()
		run.before_acting = fn [root, pending] (path string, step string) {
			if path == root && step == 'look' && os.is_link(root) {
				os.rename(root, '${root}.untrusted-1700000000-3') or { panic(err) }
				os.rename(pending, root) or { panic(err) }
			}
		}
		opts := parse_wipe_cache_options([], ['--tools'])!
		mut lines := []string{}
		mut failure := ''
		run.wipe_tool_cache_directory(root, opts, mut lines) or { failure = err.msg() }
		os.write_file(release, '')!
		holder.wait()
		holder.close()
		output := '${failure}\n' + lines.join('\n')
		assert !os.exists(pending), output
		assert failure == '', output
		assert os.is_dir(os.join_path(root, entry_name)), output
		assert os.is_file(os.join_path(root, lock_name)), output
	})!
}

// A tool cache folder that `VTOOLS_CACHE_DIR` names through a link is also known by the folder the
// link leads to, which is where builds go (`tool_cache_dir` returns the real path): a build there,
// inside VTMP, is kept by `--vtmp`.
fn test_a_build_behind_a_linked_tool_cache_folder_is_kept() ! {
	$if windows {
		// A link to a folder needs rights a test may lack.
		return
	}
	directory := toolcache_test_dir('wipe_cache_linked_configured')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		real_root := os.join_path(vtmp, 'real-tools')
		link := os.join_path(directory, 'linked-tools')
		os.setenv(tool_cache_dir_env, link, true)
		release := os.join_path(directory, 'release')
		pending := os.join_path(directory, 'pending')
		mut holder, entry_name, lock_name := toolcache_test_pending_build(pending, release)!
		os.rename(pending, real_root)!
		os.symlink(real_root, link)!
		other := os.join_path(vtmp, 'other.tmp.c')
		os.write_file(other, 'x')!
		output := wipe_output('--vtmp') or { err.msg() }
		os.write_file(release, '')!
		holder.wait()
		holder.close()
		assert os.is_dir(os.join_path(real_root, entry_name)), output
		assert os.is_file(os.join_path(real_root, lock_name)), output
		assert !os.exists(other), output
		assert output.contains('V tmp.c and tests folder ${vtmp} was wiped, except the tool cache folders in it: ${real_root}.'), output
	})!
}

// A tool cache folder the wipe knows, which is a folder but cannot be opened, fails the wipe, and
// the message names it: it is neither removed as something that is no folder nor kept in silence.
fn test_a_known_folder_that_cannot_be_opened_fails_the_wipe() ! {
	$if windows {
		return
	}
	if os.geteuid() == 0 {
		// The superuser opens a folder whatever its mode is.
		return
	}
	directory := toolcache_test_dir('wipe_cache_unopenable_root')
	defer {
		toolcache_test_unlock_tree(directory)
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		custom := os.join_path(vtmp, 'custom-tools')
		os.setenv(tool_cache_dir_env, custom, true)
		tool_cache_test_root(custom)!
		other := os.join_path(vtmp, 'other.tmp.c')
		os.write_file(other, 'x')!
		// It may be entered and changed, but not read.
		os.chmod(custom, 0o300)!
		message := wipe_failure('--vtmp')
		os.chmod(custom, 0o700)!
		assert message.starts_with('v wipe-cache: could not remove V tmp.c and tests folder ${vtmp}: could not open ${custom}: '), message
		assert os.is_dir(custom), message
		assert !os.exists(other), message
		// The folder around it may be read, but not entered: nothing in it can be reached, so the
		// tool cache folder stays, and the wipe fails and says why.
		os.chmod(vtmp, 0o600)!
		unlisted := wipe_failure('--vtmp')
		os.chmod(vtmp, 0o700)!
		assert unlisted.starts_with('v wipe-cache: could not remove V tmp.c and tests folder ${vtmp}: could not list ${vtmp}: '), unlisted
		assert os.is_dir(custom), unlisted
	})!
}

// A place the wipe knows that cannot be looked at stays, and the wipe fails and names it: it may be
// the link `VTOOLS_CACHE_DIR` names, which is not the wipe's to remove. Here the wipe has no way back
// to its working folder, so it looks at no name (see `ToolCacheWipeDir.item`); `v wipe-cache` itself
// leaves such a working folder before it starts.
fn test_a_known_place_that_cannot_be_looked_at_stays_and_fails_the_wipe() ! {
	$if windows {
		return
	}
	if os.geteuid() == 0 {
		// The superuser opens a folder whatever its mode is.
		return
	}
	directory := toolcache_test_dir('wipe_cache_unseen_place')
	working_folder := os.getwd()
	defer {
		os.chdir(working_folder) or {}
		toolcache_test_unlock_tree(directory)
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory, working_folder] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		target := os.join_path(directory, 'target-tools')
		tool_cache_test_root(target)!
		link := os.join_path(vtmp, 'my-tools')
		os.symlink(target, link)!
		os.setenv(tool_cache_dir_env, link, true)
		closed := os.join_path(directory, 'closed')
		os.mkdir_all(closed)!
		mut run := new_tool_cache_wipe_run()
		opts := parse_wipe_cache_options([], ['--vtmp'])!
		mut lines := []string{}
		mut failure := ''
		// A folder that may be entered and not read cannot be opened.
		os.chdir(closed)!
		os.chmod(closed, 0o300)!
		run.wipe_folder_around_tool_roots(vtmp, 'V tmp.c and tests', true, opts, mut lines) or {
			failure = err.msg()
		}
		os.chdir(working_folder)!
		assert os.is_link(link), failure
		assert failure == 'v wipe-cache: could not remove V tmp.c and tests folder ${vtmp}: could not look at ${link}', lines.join('\n')
	})!
}

// At the path of a cache folder that holds a tool cache folder, a dry run removes nothing that is
// no folder; and when nothing is there, a real run makes the folder and succeeds.
fn test_a_dry_run_keeps_what_is_no_folder_and_a_missing_folder_is_made() ! {
	directory := toolcache_test_dir('wipe_cache_stand_in_dry')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vcache := os.join_path(directory, 'vcache')
		custom := os.join_path(vcache, 'custom-tools')
		os.setenv(tool_cache_dir_env, custom, true)
		os.rmdir_all(vcache) or {}
		os.write_file(vcache, 'no folder')!
		dry := wipe_output('--vcache', '--dry-run')!
		assert os.read_file(vcache)! == 'no folder', dry
		assert dry.contains('Would wipe V cache folder ${vcache}.'), dry
		// The same for a tool cache folder by itself.
		os.rm(vcache)!
		os.mkdir_all(vcache)!
		os.write_file(custom, 'no folder')!
		tools_dry := wipe_output('--tools', '--dry-run')!
		assert os.read_file(custom)! == 'no folder', tools_dry
		assert tools_dry.contains('Would wipe V tool cache folder ${custom}.'), tools_dry
		// Nothing there: what is gone counts as removed, and the folder is made.
		os.rmdir_all(vcache)!
		output := wipe_output('--vcache')!
		assert os.is_dir(vcache), output
		assert output.contains('V cache folder ${vcache} was wiped.'), output
	})!
}

// What is no folder at the path of a tool cache folder and cannot be removed fails the wipe, which
// names it: a file, and a link to a folder, neither of which is taken for a folder made meanwhile.
fn test_what_is_no_folder_at_a_tool_cache_path_and_cannot_go_fails_the_wipe() ! {
	$if windows {
		return
	}
	if os.geteuid() == 0 {
		// The superuser removes from a folder whatever its mode is.
		return
	}
	directory := toolcache_test_dir('wipe_cache_stuck_stand_in')
	defer {
		toolcache_test_unlock_tree(directory)
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (cache_v_dir string) ! {
		target := os.join_path(directory, 'target')
		os.mkdir_all(target)!
		// A file where `VTOOLS_CACHE_DIR` points.
		closed := os.join_path(directory, 'closed')
		os.mkdir_all(closed)!
		file := os.join_path(closed, 'file')
		os.write_file(file, 'x')!
		os.chmod(closed, 0o500)!
		os.setenv(tool_cache_dir_env, file, true)
		message := wipe_failure('--tools')
		assert message.starts_with('v wipe-cache: could not remove V tool cache folder ${file}: '), message
		assert os.is_file(file)
		// A link to a folder at a default tool cache path. (A `VTOOLS_CACHE_DIR` that is a link stays,
		// see `test_a_linked_configured_tool_cache_folder_is_left_alone`.)
		os.unsetenv(tool_cache_dir_env)
		os.mkdir_all(cache_v_dir)!
		link := os.join_path(cache_v_dir, 'tools')
		os.symlink(target, link)!
		os.chmod(cache_v_dir, 0o500)!
		link_message := wipe_failure('--tools')
		os.chmod(cache_v_dir, 0o700)!
		assert link_message.starts_with('v wipe-cache: could not remove V tool cache folder ${link}: '), link_message
		assert os.is_link(link)
		os.chmod(closed, 0o700)!
		assert os.is_dir(target)
		// The same link in a cache folder that is being wiped: the message names the cache folder,
		// then the link.
		vtmp := os.join_path(directory, 'vtmp')
		inner := os.join_path(vtmp, 'tools')
		os.symlink(target, inner)!
		os.chmod(vtmp, 0o500)!
		around := wipe_failure('--vtmp')
		os.chmod(vtmp, 0o700)!
		assert around.starts_with('v wipe-cache: could not remove V tmp.c and tests folder ${vtmp}: ${inner}: '), around
		assert os.is_link(inner)
		assert os.is_dir(target)
	})!
}

// toolcache_test_made reports whether the wipe makes the lock file `name` in `dir` for `owner`.
fn toolcache_test_made(dir ToolCacheWipeDir, name string, owner u32) bool {
	dir.create_lock_file(name, owner) or { return false }
	return true
}

// toolcache_test_lock_state tries the build lock at `lock_path` once from another process, and
// returns 'free' or 'busy'.
fn toolcache_test_lock_state(lock_path string) string {
	mut p := os.new_process(os.executable())
	mut env := os.environ()
	env[toolcache_lock_probe_env] = lock_path
	p.set_environment(env)
	p.set_redirect_stdio()
	p.run()
	output, _ := toolcache_test_finish(mut p, 30 * time.second)
	return output.trim_space()
}

// toolcache_test_kept returns the tool cache folders that the summary line of the wipe of VTMP
// `folder` names as kept, in a dry run or a real run, sorted.
fn toolcache_test_kept(output string, folder string) []string {
	for line in output.split_into_lines() {
		for prefix in ['Would wipe V tmp.c and tests folder ${folder}, except ',
			'V tmp.c and tests folder ${folder} was wiped, except the tool cache folders in it: '] {
			if line.starts_with(prefix) {
				mut kept := line[prefix.len..].trim_right('.').split(', ')
				kept.sort()
				return kept
			}
		}
	}
	return []string{}
}

// A tool cache folder the wipe knows stays when the wipe leaves it empty, and so do the folders on
// the way to one: a build makes them before it makes its lock file, and fails to make that lock file
// in a folder that a wipe removed meanwhile. Only a folder V moved aside goes.
fn test_an_emptied_tool_cache_folder_and_the_folders_on_the_way_to_one_stay() ! {
	directory := toolcache_test_dir('wipe_cache_keep_empty')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (cache_v_dir string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		tools := os.join_path(cache_v_dir, 'tools')
		tool_cache_test_root(tools)!
		plain := os.join_path(tools, 'plain')
		inner := os.join_path(vtmp, 'tools')
		tool_cache_test_root(inner)!
		way := os.join_path(vtmp, 'a', 'b')
		os.mkdir_all(way)!
		os.setenv(tool_cache_dir_env, os.join_path(way, 'tools'), true)
		for arguments in [['--tools', '--dry-run'], ['--tools'], ['--vtmp', '--dry-run'], ['--vtmp'],
			[]string{}] {
			os.write_file(plain, 'x')!
			output := wipe_output(...arguments)!
			assert os.is_dir(tools), '${arguments}: ${output}'
			assert os.is_dir(inner), '${arguments}: ${output}'
			assert os.is_dir(way), '${arguments}: ${output}'
		}
		assert !os.exists(plain)
	})!
}

// A tool cache folder that holds where `VTOOLS_CACHE_DIR` points stays when the wipe leaves it
// empty, as any folder on the way to one does, even when it is a folder no build makes itself.
fn test_a_tool_cache_folder_that_holds_a_build_folder_stays_emptied() ! {
	directory := toolcache_test_dir('wipe_cache_holds_build_folder')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn (cache_v_dir string) ! {
		// Beside the default folder, so the wipe knows it; not named as V names a folder it moves aside.
		holder := os.join_path(cache_v_dir, 'tools.untrusted-kept')
		tool_cache_test_root(holder)!
		os.write_file(os.join_path(holder, 'plain'), 'x')!
		os.setenv(tool_cache_dir_env, os.join_path(holder, 'inner'), true)
		output := wipe_output('--tools')!
		assert os.is_dir(holder), output
		assert !os.exists(os.join_path(holder, 'plain')), output
	})!
}

// A wipe whose folder another wipe emptied and removed meanwhile says that the folder was wiped: no
// build moved it aside.
fn test_a_wipe_whose_folder_another_wipe_removed_says_it_was_wiped() ! {
	$if windows {
		// The folder is known by its path there, and is always where it was opened.
		return
	}
	directory := toolcache_test_dir('wipe_cache_removed_by_another')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn (cache_v_dir string) ! {
		tools := os.join_path(cache_v_dir, 'tools')
		tool_cache_test_root(tools)!
		opts := parse_wipe_cache_options([], ['--tools'])!
		mut run := new_tool_cache_wipe_run()
		dir := open_tool_cache_wipe_dir(tools)!
		defer {
			dir.close()
		}
		os.rmdir(tools)!
		mut lines := []string{}
		run.wipe_open_tool_cache_directory(dir, tools, opts, mut lines)!
		output := lines.join('\n')
		assert output.contains('V tool cache folder ${tools} was wiped.'), output
		assert !output.contains('moved aside'), output
	})!
}

// A dry run tells what the real run then does, in the summary of the folder around tool cache
// folders too: a link at a known tool cache path, which the real run removes; a tool without a lock
// file in a known folder, which keeps the folder, since the real run makes that lock file; and a
// folder V moved aside, which goes whole with the lock file the real run makes and removes.
fn test_a_dry_run_summary_matches_the_real_run() ! {
	$if windows {
		// A link to a folder needs rights a test may lack.
		return
	}
	directory := toolcache_test_dir('wipe_cache_dry_summary')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		outside := os.join_path(directory, 'outside')
		tool_cache_test_root(outside)!
		link := os.join_path(vtmp, 'tools')
		custom := os.join_path(vtmp, 'custom-tools')
		aside := os.join_path(vtmp, 'custom-tools.untrusted-1700000000-42')
		// Named like a folder V moved aside, but not as V names one: the real run makes its lock file
		// and keeps that, and so the folder.
		stranger := os.join_path(vtmp, 'custom-tools.untrusted-kept')
		os.setenv(tool_cache_dir_env, custom, true)
		for arguments in [['--vtmp'], []string{}] {
			os.symlink(outside, link)!
			tool_cache_test_root(custom)!
			tool_cache_test_entry(custom, 'vfmt', 'a')!
			tool_cache_test_root(aside)!
			tool_cache_test_entry(aside, 'vdoc', 'b')!
			tool_cache_test_root(stranger)!
			tool_cache_test_entry(stranger, 'vtest', 'c')!
			mut dry_arguments := arguments.clone()
			dry_arguments << '--dry-run'
			dry := wipe_output(...dry_arguments)!
			real := wipe_output(...arguments)!
			context := '${arguments}:\n${dry}\n---\n${real}'
			assert toolcache_test_kept(dry, vtmp) == toolcache_test_kept(real, vtmp), context
			mut expected := [custom, stranger]
			expected.sort()
			assert toolcache_test_kept(real, vtmp) == expected, context
			assert !os.is_link(link), context
			assert !os.exists(aside), context
		}
	})!
}

// Out of file descriptors, a wipe cannot look at the names in a tool cache folder: it keeps what it
// cannot look at, a lock file a build may hold included, and fails, instead of removing it.
fn test_a_wipe_that_cannot_look_at_a_name_keeps_it_and_fails() ! {
	$if windows {
		return
	}
	directory := toolcache_test_dir('wipe_cache_no_descriptors')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (cache_v_dir string) ! {
		tools := os.join_path(cache_v_dir, 'tools')
		tool_cache_test_root(tools)!
		me := u32(os.getuid())
		for i in 0 .. 120 {
			os.write_file(os.join_path(tools, tool_cache_lock_name('vt${i:03}', me)), '')!
		}
		start := os.join_path(directory, 'start')
		os.write_file(start, '')!
		mut env := os.environ()
		env[toolcache_wipe_child_env] = start
		mut child := os.new_process('/bin/sh')
		child.set_args(['-c', 'ulimit -n 60 && exec "\$0"', os.executable()])
		child.set_environment(env)
		child.set_redirect_stdio()
		child.run()
		output, code := toolcache_test_finish(mut child, 2 * time.minute)
		left := os.ls(tools)!.filter(it.ends_with(tool_cache_lock_ending))
		assert left.len == 120, 'exit ${code}: ${output}'
		assert code != 0, output
		assert output.contains('could not look at'), output
	})!
}

// A `VTOOLS_CACHE_DIR` that is a symbolic link is left as it is, and so is the folder it leads to:
// the wipe follows no link, since it cannot tell the user's own link from one that another account
// put there. Whichever pass comes to the link says so, a dry run too, and nothing in the folder
// behind it is touched, be it a tool entry, any other file, or the folder itself when it is empty.
fn test_a_linked_configured_tool_cache_folder_is_left_alone() ! {
	$if windows {
		// A link to a folder needs rights a test may lack.
		return
	}
	directory := toolcache_test_dir('wipe_cache_linked_configured_root')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		target := os.join_path(directory, 'target-tools')
		tool_cache_test_root(target)!
		idle := tool_cache_test_entry(target, 'vdoc', 'a')!
		plain := os.join_path(target, 'plain')
		os.write_file(plain, 'x')!
		empty_target := os.join_path(directory, 'empty-target')
		tool_cache_test_root(empty_target)!
		vtmp := os.join_path(directory, 'vtmp')
		for link in [os.join_path(vtmp, 'my-tools'), os.join_path(directory, 'my-tools')] {
			for leads_to in [target, empty_target] {
				os.symlink(leads_to, link)!
				os.setenv(tool_cache_dir_env, link, true)
				for arguments in [['--tools'], ['--vtmp'], []string{}, ['--tools', '--dry-run'],
					['--dry-run']] {
					output := wipe_output(...arguments)!
					context := '${link} -> ${leads_to}, ${arguments}:\n${output}'
					assert os.is_link(link), context
					assert os.is_dir(leads_to), context
					assert os.is_dir(idle), context
					assert os.is_file(plain), context
					assert toolcache_test_tree(leads_to).all(!it.ends_with(tool_cache_lock_ending)), context
					if '--vtmp' !in arguments {
						said := if '--dry-run' in arguments {
							'Would leave V tool cache folder ${link} as it is: it is a symbolic link'
						} else {
							'V tool cache folder ${link} was left as it is: it is a symbolic link'
						}
						assert output.contains(said), context
					}
					assert !output.contains(leads_to), context
				}
				os.rm(link)!
			}
		}
	})!
}

// The folder a `VTOOLS_CACHE_DIR` link leads to is where builds go. When it lies inside a cache
// folder that is being wiped, it is wiped as the tool cache folder it is, and stays even emptied,
// like every folder builds use: the wipe got there through the folder around it, not through the
// link, which stays too. So it is when the setting names the link with a separator at its end.
fn test_the_folder_a_configured_link_leads_to_stays_when_the_folder_around_it_is_wiped() ! {
	$if windows {
		// A link to a folder needs rights a test may lack.
		return
	}
	directory := toolcache_test_dir('wipe_cache_linked_configured_inside')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		target := os.join_path(vtmp, 'real-tools')
		link := os.join_path(directory, 'linked-tools')
		for setting in [link, '${link}/'] {
			os.setenv(tool_cache_dir_env, setting, true)
			for arguments in [['--vtmp'], []string{}] {
				tool_cache_test_root(target)!
				os.write_file(os.join_path(target, 'plain'), 'x')!
				if !os.is_link(link) {
					os.symlink(target, link)!
				}
				mut dry_arguments := arguments.clone()
				dry_arguments << '--dry-run'
				dry := wipe_output(...dry_arguments)!
				real := wipe_output(...arguments)!
				context := '${setting}, ${arguments}:\n${dry}\n---\n${real}'
				assert os.is_link(link), context
				assert os.is_dir(target), context
				assert os.ls(target)! == []string{}, context
				assert toolcache_test_kept(real, vtmp) == [target], context
				assert toolcache_test_kept(dry, vtmp) == [target], context
			}
		}
	})!
}

// A `VTOOLS_CACHE_DIR` link stays wherever the wipe meets it, also when it is the path of a cache
// folder the wipe was asked to wipe (`VTMP` set to the same link): each pass leaves it as it is.
fn test_a_configured_link_at_the_path_of_a_cache_folder_stays() ! {
	$if windows {
		// A link to a folder needs rights a test may lack.
		return
	}
	directory := toolcache_test_dir('wipe_cache_linked_configured_outer')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		behind := os.join_path(directory, 'vtmp-behind')
		os.mkdir(behind, mode: 0o700)!
		kept_file := os.join_path(behind, 'a.tmp.c')
		os.write_file(kept_file, 'x')!
		link := os.join_path(directory, 'vtmp-link')
		os.symlink(behind, link)!
		os.setenv('VTMP', link, true)
		os.setenv(tool_cache_dir_env, link, true)
		for arguments in [['--vtmp'], ['--tools'], []string{}, ['--dry-run']] {
			output := wipe_output(...arguments)!
			context := '${arguments}:\n${output}'
			assert os.is_link(link), context
			assert os.is_file(kept_file), context
			assert os.ls(behind)! == ['a.tmp.c'], context
			assert output.contains('it is a symbolic link, the one ${tool_cache_dir_env} names'), context
		}
	})!
}

// A cache folder the wipe was asked to wipe is there afterwards, also when it is a tool cache folder
// that would otherwise go once it is empty: here `VCACHE` names a folder that V moved aside from the
// tool cache folder beside it.
fn test_a_cache_folder_that_is_a_tool_cache_folder_too_is_there_afterwards() ! {
	directory := toolcache_test_dir('wipe_cache_outer_is_moved_aside')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		configured := os.join_path(directory, 'custom-tools')
		vcache := '${configured}.untrusted-1700000000-7'
		os.setenv(tool_cache_dir_env, configured, true)
		os.setenv('VCACHE', vcache, true)
		for arguments in [['--vcache', '--dry-run'], ['--vcache'], []string{}] {
			tool_cache_test_root(vcache)!
			os.write_file(os.join_path(vcache, 'x.o'), 'x')!
			output := wipe_output(...arguments)!
			context := '${arguments}:\n${output}'
			assert os.is_dir(vcache), context
			if '--dry-run' in arguments {
				assert output.contains('Would wipe V cache folder ${vcache}, which is a tool cache folder too.'), context
				assert os.ls(vcache)! == ['x.o'], context
			} else if arguments.len > 0 {
				assert output.contains('V cache folder ${vcache} was wiped, as the tool cache folder it also is.'), context
				assert os.ls(vcache)! == []string{}, context
			} else {
				// The tools pass, which comes first, wiped it as the folder V moved aside that its name
				// says it is, and removed it; the pass of `VCACHE` made it again.
				assert output.contains('V cache folder ${vcache} was wiped.'), context
				assert os.ls(vcache)! == []string{}, context
			}
		}
	})!
}

// A cache folder that is a tool cache folder too, where the tools pass found nothing and a build has
// made it since, is left as it is by the pass that wipes the cache folder, and the output says so,
// not that it was wiped: here `VTOOLS_CACHE_DIR` and `V3CACHE` name the same folder.
fn test_a_cache_folder_an_earlier_pass_dealt_with_is_said_to_be_left() ! {
	directory := toolcache_test_dir('wipe_cache_outer_dealt_with')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		v3cache := os.join_path(directory, 'v3cache')
		os.setenv(tool_cache_dir_env, v3cache, true)
		os.rmdir_all(v3cache) or {}
		for arguments in [['--dry-run'], []string{}] {
			mut run := new_tool_cache_wipe_run()
			opts := parse_wipe_cache_options([], arguments)!
			mut lines := []string{}
			run.wipe_tool_places(opts, mut lines)!
			// The build makes the folder once the tools pass is past it.
			tool_cache_test_root(v3cache)!
			entry := tool_cache_test_entry(v3cache, 'vfmt', 'a')!
			os.write_file(tool_cache_test_lock(v3cache, 'vfmt'), '')!
			run.wipe_folder_around_tool_roots(v3cache, 'V3 cache', true, opts, mut lines)!
			output := '${arguments}:\n' + lines.join('\n')
			assert os.is_dir(entry), output
			assert !output.contains('folder ${v3cache} was wiped'), output
			assert !output.contains('Would wipe V3 cache folder ${v3cache}'), output
			assert output.contains('V3 cache folder ${v3cache} as it is: it is a tool cache folder too')
				|| output.contains('V3 cache folder ${v3cache} was left as it is: it is a tool cache folder too'), output
			os.rmdir_all(v3cache)!
		}
	})!
}

// A tool cache folder is the same folder however its name is spelled where the file system ignores
// case: `VTOOLS_CACHE_DIR` written in another case than the folder on disk still names a folder
// builds use, which stays when the wipe of the folder around it leaves it empty.
fn test_a_tool_cache_folder_spelled_in_another_case_is_the_same_folder() ! {
	directory := toolcache_test_dir('wipe_cache_other_case')
	defer {
		os.rmdir_all(directory) or {}
	}
	os.mkdir(os.join_path(directory, 'CaseProbe'))!
	if !os.is_dir(os.join_path(directory, 'caseprobe')) {
		// This file system tells the two spellings apart, so they are two folders.
		return
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		on_disk := os.join_path(vtmp, 'mytools')
		os.setenv(tool_cache_dir_env, os.join_path(vtmp, 'MyTools'), true)
		for arguments in [['--vtmp'], ['--tools'], []string{}] {
			tool_cache_test_root(on_disk)!
			os.write_file(os.join_path(on_disk, 'plain'), 'x')!
			output := wipe_output(...arguments)!
			context := '${arguments}:\n${output}'
			assert os.is_dir(on_disk), context
			assert os.ls(on_disk)! == []string{}, context
		}
	})!
}

// The folders builds use are known by where they lie, with the links in the folders above them
// resolved (as on macOS, where `/tmp` is a link): a default tool cache folder and the one
// `VTOOLS_CACHE_DIR` names, both reached through a linked folder, stay when the wipe empties them.
fn test_build_folders_are_known_through_a_linked_parent() ! {
	$if windows {
		// A link to a folder needs rights a test may lack.
		return
	}
	directory := toolcache_test_dir('wipe_cache_linked_parent')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		base := os.join_path(directory, 'base')
		os.mkdir_all(os.join_path(base, 'vtmp'), mode: 0o700)!
		alias := os.join_path(directory, 'alias')
		os.symlink(base, alias)!
		vtmp := os.join_path(alias, 'vtmp')
		os.setenv('VTMP', vtmp, true)
		default_tools := os.join_path(vtmp, 'tools')
		configured := os.join_path(alias, 'my-tools')
		os.setenv(tool_cache_dir_env, configured, true)
		for arguments in [['--tools'], ['--vtmp'], []string{}] {
			for folder in [default_tools, configured] {
				tool_cache_test_root(folder)!
				os.write_file(os.join_path(folder, 'plain'), 'x')!
			}
			output := wipe_output(...arguments)!
			context := '${arguments}:\n${output}'
			assert os.is_dir(default_tools), context
			assert os.ls(default_tools)! == []string{}, context
			assert os.is_dir(configured), context
			if '--vtmp' !in arguments || arguments.len == 0 {
				assert os.ls(configured)! == []string{}, context
			}
		}
	})!
}

// A dry run says of a tool cache folder inside another one what the real run does with it: a folder
// V moved aside, which goes whole once it was wiped, is not listed as kept by the folder around it.
fn test_a_dry_run_says_of_a_nested_moved_aside_folder_what_the_real_run_does() ! {
	directory := toolcache_test_dir('wipe_cache_dry_nested')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn (cache_v_dir string) ! {
		tools := os.join_path(cache_v_dir, 'tools')
		custom := os.join_path(tools, 'custom')
		aside := os.join_path(tools, 'custom.untrusted-1700000000-9')
		os.setenv(tool_cache_dir_env, custom, true)
		tool_cache_test_root(tools)!
		tool_cache_test_root(custom)!
		tool_cache_test_root(aside)!
		tool_cache_test_entry(aside, 'vdoc', 'b')!
		dry := wipe_output('--tools', '--dry-run', '--verbose')!
		real := wipe_output('--tools', '--verbose')!
		context := '${dry}\n---\n${real}'
		assert !os.exists(aside), context
		assert os.is_dir(custom), context
		assert !dry.contains('would keep ${aside}'), context
		assert !real.contains('keeping ${aside}'), context
		assert dry.contains('would keep ${custom}'), context
		assert real.contains('keeping ${custom}'), context
	})!
}

// While the wipe removes the entries of a tool, it holds that tool's build lock, so a build that
// starts meanwhile waits until the wipe is done with the folder.
fn test_the_wipe_holds_a_tools_lock_while_it_removes_its_entries() ! {
	directory := toolcache_test_dir('wipe_cache_lock_held')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (cache_v_dir string) ! {
		tools := os.join_path(cache_v_dir, 'tools')
		tool_cache_test_root(tools)!
		lock_path := tool_cache_test_lock(tools, 'vfmt')
		seen := os.join_path(directory, 'seen')
		// The lock is held whichever way the wipe came by it: a lock file that was there, and one
		// the wipe made itself, for a tool that had none.
		for lock_file_was_there in [true, false] {
			entry := tool_cache_test_entry(tools, 'vfmt', 'a')!
			os.rm(lock_path) or {}
			os.rm(seen) or {}
			if lock_file_was_there {
				os.write_file(lock_path, '')!
			}
			mut run := new_tool_cache_wipe_run()
			run.before_acting = fn [entry, lock_path, seen] (path string, step string) {
				if path == entry && step == 'remove' {
					os.write_file(seen, toolcache_test_lock_state(lock_path)) or { panic(err) }
				}
			}
			opts := parse_wipe_cache_options([], ['--tools'])!
			mut lines := []string{}
			run.wipe_tool_cache_directory(tools, opts, mut lines)!
			output := 'lock file was there: ${lock_file_was_there}\n' + lines.join('\n')
			assert os.read_file(seen)! == 'busy', output
			assert !os.exists(entry), output
			assert os.is_file(lock_path), output
			assert toolcache_test_lock_state(lock_path) == 'free', output
		}
	})!
}

// In a tool cache folder that another account can write to, the wipe removes only what its own user
// owns. Run as another user, it keeps every file and folder of this one, lock files included, and
// counts them in its summary.
fn test_a_wipe_keeps_what_other_accounts_own_in_a_shared_folder() ! {
	$if windows {
		// Windows decides by the folder's ACL, which the tool cache checks when it picks a folder.
		return
	}
	directory := toolcache_test_dir('wipe_cache_others_shared')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn (cache_v_dir string) ! {
		tools := os.join_path(cache_v_dir, 'tools')
		tool_cache_test_root(tools)!
		os.chmod(tools, 0o1777)!
		entry := tool_cache_test_entry(tools, 'vfmt', 'a')!
		lock_path := tool_cache_test_lock(tools, 'vfmt')
		os.write_file(lock_path, '')!
		plain := os.join_path(tools, 'plain')
		os.write_file(plain, 'x')!
		mut run := new_tool_cache_wipe_run()
		run.user = i64(os.getuid()) + 1
		opts := parse_wipe_cache_options([], ['--tools'])!
		mut lines := []string{}
		run.wipe_tool_cache_directory(tools, opts, mut lines)!
		output := lines.join('\n')
		assert os.is_dir(entry), output
		assert os.is_file(plain), output
		assert os.is_file(lock_path), output
		assert output.contains('except for 2 files and folders of other users'), output
	})!
}

// What is no folder at a known tool cache path is removed with a call that cannot remove a folder:
// a build that puts its new folder there between the wipe's look and the removal keeps it, with the
// build in it, both where the folder around it is being wiped and where the tool cache folder is
// wiped by itself.
fn test_a_folder_made_between_the_look_and_the_removal_stays() ! {
	$if windows {
		// A link to a folder needs rights a test may lack.
		return
	}
	directory := toolcache_test_dir('wipe_cache_made_before_removal')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (cache_v_dir string) ! {
		outside := os.join_path(directory, 'outside')
		tool_cache_test_root(outside)!
		// In the folder around it: a new folder with a build in it.
		vtmp := os.join_path(directory, 'vtmp')
		root := os.join_path(vtmp, 'tools')
		os.symlink(outside, root)!
		release := os.join_path(directory, 'release')
		pending := os.join_path(directory, 'pending')
		mut holder, entry_name, lock_name := toolcache_test_pending_build(pending, release)!
		mut run := new_tool_cache_wipe_run()
		run.before_acting = fn [root, pending] (path string, step string) {
			if path == root && step == 'unlink' && os.is_link(root) {
				os.rename(root, '${root}.untrusted-1700000000-6') or { panic(err) }
				os.rename(pending, root) or { panic(err) }
			}
		}
		vtmp_opts := parse_wipe_cache_options([], ['--vtmp'])!
		mut lines := []string{}
		mut failure := ''
		run.wipe_folder_around_tool_roots(vtmp, 'V tmp.c and tests', true, vtmp_opts, mut
			lines) or { failure = err.msg() }
		os.write_file(release, '')!
		holder.wait()
		holder.close()
		around := '${failure}\n' + lines.join('\n')
		assert !os.exists(pending), around
		assert os.is_dir(os.join_path(root, entry_name)), around
		assert os.is_file(os.join_path(root, lock_name)), around
		// What the wipe says matches what is there: the folder is named as kept.
		assert failure == '', around
		assert around.contains('V tmp.c and tests folder ${vtmp} was wiped, except the tool cache folders in it: ${root}.'), around
		// By itself: the build's new folder, still empty.
		linked := os.join_path(cache_v_dir, 'tools')
		os.mkdir_all(cache_v_dir)!
		os.symlink(outside, linked)!
		mut link_run := new_tool_cache_wipe_run()
		link_run.before_acting = fn [linked] (path string, step string) {
			if path == linked && step == 'unlink' && os.is_link(linked) {
				os.rm(linked) or { panic(err) }
				os.mkdir(linked) or { panic(err) }
			}
		}
		tools_opts := parse_wipe_cache_options([], ['--tools'])!
		mut link_lines := []string{}
		link_run.wipe_tool_cache_directory(linked, tools_opts, mut link_lines)!
		by_itself := link_lines.join('\n')
		assert os.is_dir(linked) && !os.is_link(linked), by_itself
		assert by_itself.contains('V tool cache folder ${linked} was made anew during the wipe; the new folder was not touched.'), by_itself
	})!
}

// A place the wipe knows inside a folder it holds open is opened, looked at and removed through that
// folder, not by its path: when the folder is moved away during the wipe and another one stands at
// its path, the wipe goes on in the folder it opened, and nothing in the new one is touched.
fn test_a_known_place_in_an_open_folder_is_reached_through_that_folder() ! {
	$if windows {
		// A folder is known by its path there.
		return
	}
	directory := toolcache_test_dir('wipe_cache_through_open_folder')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		outside := os.join_path(directory, 'outside')
		tool_cache_test_root(outside)!
		vtmp := os.join_path(directory, 'vtmp')
		moved := os.join_path(directory, 'vtmp-moved')
		root := os.join_path(vtmp, 'tools')
		entry_name := 'vfmt-${'a'.repeat(64)}.${os.getuid()}'
		// A link at the path of the default tool cache folder in VTMP, which the wipe removes.
		os.symlink(outside, root)!
		mut run := new_tool_cache_wipe_run()
		run.before_acting = fn [vtmp, moved, root] (path string, step string) {
			if path == root && step == 'open' && !os.exists(moved) {
				os.rename(vtmp, moved) or { panic(err) }
				tool_cache_test_root(root) or { panic(err) }
				tool_cache_test_entry(root, 'vfmt', 'a') or { panic(err) }
			}
		}
		opts := parse_wipe_cache_options([], ['--vtmp'])!
		mut lines := []string{}
		mut failure := ''
		run.wipe_folder_around_tool_roots(vtmp, 'V tmp.c and tests', true, opts, mut lines) or {
			failure = err.msg()
		}
		output := '${failure}\n' + lines.join('\n')
		assert failure == '', output
		// The link went from the folder that was opened.
		assert os.ls(moved)! == []string{}, output
		// The folder now at the path has what it had, and got no lock file.
		assert os.ls(root)! == [entry_name], output
		assert os.is_file(os.join_path(root, entry_name, 'vfmt' + tool_exe_suffix())), output
		// What the wipe says matches what is there: the folder at the path is named as kept.
		assert output.contains('V tmp.c and tests folder ${vtmp} was wiped, except the tool cache folders in it: ${root}.'), output
	})!
}

// A tool cache folder that is gone when the wipe comes to open it (another wipe removed it, or a
// build moved it aside) is not reported as wiped or removed: each wipe says what it did itself.
fn test_a_known_folder_gone_before_the_wipe_opens_it_is_not_reported() ! {
	directory := toolcache_test_dir('wipe_cache_gone_before_open')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (cache_v_dir string) ! {
		// By itself.
		tools := os.join_path(cache_v_dir, 'tools')
		tool_cache_test_root(tools)!
		mut run := new_tool_cache_wipe_run()
		run.before_acting = fn [tools] (path string, step string) {
			if path == tools && step == 'open' {
				os.rmdir(tools) or { panic(err) }
			}
		}
		opts := parse_wipe_cache_options([], ['--tools', '--verbose'])!
		mut lines := []string{}
		run.wipe_tool_cache_directory(tools, opts, mut lines)!
		assert lines == []string{}
		// In the folder around it.
		vtmp := os.join_path(directory, 'vtmp')
		inner := os.join_path(vtmp, 'tools')
		tool_cache_test_root(inner)!
		mut around := new_tool_cache_wipe_run()
		around.before_acting = fn [inner] (path string, step string) {
			if path == inner && step == 'open' {
				os.rmdir(inner) or { panic(err) }
			}
		}
		vtmp_opts := parse_wipe_cache_options([], ['--vtmp', '--verbose'])!
		mut vtmp_lines := []string{}
		around.wipe_folder_around_tool_roots(vtmp, 'V tmp.c and tests', true, vtmp_opts, mut
			vtmp_lines)!
		output := vtmp_lines.join('\n')
		assert !output.contains(inner), output
		assert output.contains('V tmp.c and tests folder ${vtmp} was wiped.'), output
	})!
}

// What a dry run says of the places the wipe knows holds when the cache folders are named through a
// linked folder (as on macOS, where `/tmp` is a link): a place is the same place however the path
// to it is spelled. A link at a tool cache path and a folder V moved aside, both of which a real
// run removes, are not listed as kept, by the cache folder around them or by a tool cache folder
// they lie in. And a tool cache folder that two settings name in two spellings is wiped once.
fn test_a_dry_run_matches_the_real_run_where_folders_are_named_through_a_link() ! {
	$if windows {
		// A link to a folder needs rights a test may lack.
		return
	}
	directory := toolcache_test_dir('wipe_cache_dry_linked_parent')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		base := os.join_path(directory, 'base')
		real_vtmp := os.join_path(base, 'vtmp')
		os.mkdir_all(real_vtmp, mode: 0o700)!
		alias := os.join_path(directory, 'alias')
		os.symlink(base, alias)!
		vtmp := os.join_path(alias, 'vtmp')
		os.setenv('VTMP', vtmp, true)
		outside := os.join_path(directory, 'outside')
		tool_cache_test_root(outside)!
		link := os.join_path(vtmp, 'tools')
		aside := os.join_path(vtmp, 'tools.untrusted-1700000000-5')
		// First in the cache folder around them, then in a tool cache folder that holds them, which
		// `VTMP` is once `VTOOLS_CACHE_DIR` names it.
		for vtmp_is_a_tool_cache_folder in [false, true] {
			mut arguments := ['--vtmp', '--verbose']
			if vtmp_is_a_tool_cache_folder {
				os.setenv(tool_cache_dir_env, vtmp, true)
				arguments = ['--tools', '--verbose']
			}
			os.symlink(outside, link)!
			tool_cache_test_root(aside)!
			tool_cache_test_entry(aside, 'vdoc', 'b')!
			mut dry_arguments := arguments.clone()
			dry_arguments << '--dry-run'
			dry := wipe_output(...dry_arguments)!
			assert os.is_link(link) && os.is_dir(aside), dry
			real := wipe_output(...arguments)!
			context := '${arguments}:\n${dry}\n---\n${real}'
			assert !os.is_link(link), context
			assert !os.exists(aside), context
			assert os.is_dir(real_vtmp), context
			assert toolcache_test_kept(dry, vtmp) == []string{}, context
			assert toolcache_test_kept(real, vtmp) == []string{}, context
			assert !dry.contains('would keep ${aside}'), context
			assert !real.contains('keeping ${aside}'), context
			assert !dry.contains('would keep ${link}'), context
		}
		// Named in two spellings, by `VTOOLS_CACHE_DIR` above it and as the default folder in `VTMP`.
		os.setenv(tool_cache_dir_env, real_vtmp, true)
		inner := os.join_path(real_vtmp, 'tools')
		tool_cache_test_root(inner)!
		os.write_file(os.join_path(inner, 'plain'), 'x')!
		output := wipe_output('--tools')!
		assert os.ls(inner)! == []string{}, output
		assert output.count('V tool cache folder ${inner} was wiped.') == 1, output
		assert !output.contains(link), output
	})!
}

// The link `VTOOLS_CACHE_DIR` names is known by where it lies: it stays, and is not followed, when
// the wipe meets it in a cache folder whose own path goes through a linked folder.
fn test_a_configured_link_met_through_a_linked_folder_stays() ! {
	$if windows {
		// A link to a folder needs rights a test may lack.
		return
	}
	directory := toolcache_test_dir('wipe_cache_linked_configured_alias')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		base := os.join_path(directory, 'base')
		os.mkdir_all(os.join_path(base, 'vtmp'), mode: 0o700)!
		alias := os.join_path(directory, 'alias')
		os.symlink(base, alias)!
		vtmp := os.join_path(alias, 'vtmp')
		os.setenv('VTMP', vtmp, true)
		target := os.join_path(directory, 'target-tools')
		tool_cache_test_root(target)!
		idle := tool_cache_test_entry(target, 'vdoc', 'a')!
		// The setting names it by its real path; the wipe of `VTMP` meets it by the linked one.
		link := os.join_path(base, 'vtmp', 'my-tools')
		os.symlink(target, link)!
		os.setenv(tool_cache_dir_env, link, true)
		met := os.join_path(vtmp, 'my-tools')
		for arguments in [['--vtmp', '--verbose', '--dry-run'], ['--vtmp', '--verbose']] {
			output := wipe_output(...arguments)!
			context := '${arguments}:\n${output}'
			assert os.is_link(link), context
			assert os.is_dir(idle), context
			said := if '--dry-run' in arguments {
				'  would keep ${met}'
			} else {
				'  keeping ${met}'
			}
			assert output.contains(said), context
		}
	})!
}

// What is gone when the wipe comes to it (another wipe removed it since the folder was listed) is
// not reported as removed, and no lock file is made for a tool whose entry is gone: each wipe says
// what it did itself.
fn test_what_another_wipe_removed_meanwhile_is_not_reported() ! {
	directory := toolcache_test_dir('wipe_cache_gone_meanwhile')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn (cache_v_dir string) ! {
		tools := os.join_path(cache_v_dir, 'tools')
		tool_cache_test_root(tools)!
		plain := [os.join_path(tools, 'plain-a'), os.join_path(tools, 'plain-b')]
		for path in plain {
			os.write_file(path, 'x')!
		}
		entries := [tool_cache_test_entry(tools, 'vdoc', 'a')!,
			tool_cache_test_entry(tools, 'vfmt', 'b')!]
		mut run := new_tool_cache_wipe_run()
		// Whichever of two the wipe removes first, the other one is gone when the wipe comes to it.
		run.before_acting = fn [plain, entries] (path string, step string) {
			if step != 'remove' {
				return
			}
			for pair in [plain, entries] {
				at := pair.index(path)
				if at < 0 {
					continue
				}
				other := pair[1 - at]
				if os.is_dir(other) {
					os.rmdir_all(other) or { panic(err) }
				} else {
					os.rm(other) or {}
				}
			}
		}
		opts := parse_wipe_cache_options([], ['--tools', '--verbose'])!
		mut lines := []string{}
		run.wipe_tool_cache_directory(tools, opts, mut lines)!
		output := lines.join('\n')
		removed := toolcache_test_detail_paths(output, '  removing ')
		assert removed.filter(it in plain).len == 1, output
		assert removed.filter(it in entries).len == 1, output
		assert os.ls(tools)!.filter(it.ends_with(tool_cache_lock_ending)).len == 1, output
	})!
}

// Run as the superuser, the wipe takes the build lock of any user: the lock file of another user in
// a folder V moved aside is the wipe's to hold, so it goes, and the folder with it.
fn test_the_superuser_takes_the_lock_files_of_other_users_in_a_moved_aside_folder() ! {
	$if windows {
		// There is no superuser there (see `tool_cache_wipe_is_superuser`).
		return
	}
	directory := toolcache_test_dir('wipe_cache_superuser_aside')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn (cache_v_dir string) ! {
		aside := os.join_path(cache_v_dir, 'tools.untrusted-1700000000-45')
		tool_cache_test_root(aside)!
		foreign_lock := os.join_path(aside, tool_cache_lock_name('vfmt', u32(os.getuid()) + 1))
		os.write_file(foreign_lock, '')!
		mut run := new_tool_cache_wipe_run()
		run.user = 0
		opts := parse_wipe_cache_options([], ['--tools'])!
		mut lines := []string{}
		run.wipe_tool_cache_directory(aside, opts, mut lines)!
		assert !os.exists(aside), lines.join('\n')
	})!
}

// A folder V moved aside stays, with its lock files, while anything in it is kept: here a file of
// another account, in a folder that account can write to. A dry run says the same, with and without
// a lock file of the wipe's own user in the folder. The wipe acts as another user here, to whom VTMP
// itself then belongs to someone else, so the pass of VTMP would leave the folder whole (see
// `test_what_another_account_owns_in_a_cache_folder_stays_and_does_not_fail_the_wipe`): the wipe
// of the moved-aside folder itself is what runs, as the pass of a VTMP of the wipe's own reaches it.
fn test_a_moved_aside_folder_with_something_kept_stays_with_its_lock_files() ! {
	$if windows {
		// Windows decides by the folder's ACL, which the tool cache checks when it picks a folder.
		return
	}
	directory := toolcache_test_dir('wipe_cache_aside_kept')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		aside := os.join_path(vtmp, 'tools.untrusted-1700000000-46')
		tool_cache_test_root(aside)!
		os.chmod(aside, 0o1777)!
		plain := os.join_path(aside, 'plain')
		os.write_file(plain, 'x')!
		// The wipe runs as another user, to whom the file is somebody else's.
		wiper := u32(os.getuid()) + 1
		own_lock := os.join_path(aside, tool_cache_lock_name('vfmt', wiper))
		for with_lock in [false, true] {
			if with_lock {
				os.write_file(own_lock, '')!
			}
			for arguments in [['--vtmp', '--dry-run'], ['--vtmp']] {
				mut run := new_tool_cache_wipe_run()
				run.user = i64(wiper)
				opts := parse_wipe_cache_options([], arguments)!
				mut lines := []string{}
				run.wipe_tool_cache_directory(aside, opts, mut lines)!
				output := 'lock file: ${with_lock}, ${arguments}:\n' + lines.join('\n')
				assert os.is_file(plain), output
				assert os.is_file(own_lock) == with_lock, output
				assert os.is_dir(aside), output
				assert output.contains(if opts.dry_run {
					'Would wipe Moved-aside V tool cache folder ${aside}, except for 1 files and folders of other users.'
				} else {
					'Moved-aside V tool cache folder ${aside} was wiped, except for 1 files and folders of other users.'
				}), output
			}
		}
	})!
}

// A `VTOOLS_CACHE_DIR` link written with a separator or `.` at its end names the link itself: it
// stays, and nothing in the folder it leads to is touched, whichever pass comes to it. A `VTMP`,
// `VCACHE` or `V3CACHE` link written that way is the link too: its pass takes the link away and
// makes the folder, as it does with any link at the path of a cache folder, and leaves the folder
// that the link led to alone.
fn test_a_link_named_with_a_separator_at_its_end_is_the_link() ! {
	$if windows {
		// A link to a folder needs rights a test may lack.
		return
	}
	directory := toolcache_test_dir('wipe_cache_link_trailing')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		target := os.join_path(directory, 'target')
		tool_cache_test_root(target)!
		idle := tool_cache_test_entry(target, 'vdoc', 'a')!
		plain := os.join_path(target, 'plain')
		os.write_file(plain, 'x')!
		vtmp := os.join_path(directory, 'vtmp')
		for link in [os.join_path(directory, 'my-tools'), os.join_path(vtmp, 'my-tools')] {
			os.symlink(target, link)!
			for spelling in ['${link}/', '${link}/.', '${link}/./'] {
				os.setenv(tool_cache_dir_env, spelling, true)
				for arguments in [['--tools'], ['--vtmp'], []string{}, ['--tools', '--dry-run']] {
					output := wipe_output(...arguments)!
					context := '${spelling}, ${arguments}:\n${output}'
					assert os.is_link(link), context
					assert os.is_dir(idle), context
					assert os.is_file(plain), context
					if '--vtmp' !in arguments {
						said := if '--dry-run' in arguments {
							'Would leave V tool cache folder ${link} as it is: it is a symbolic link'
						} else {
							'V tool cache folder ${link} was left as it is: it is a symbolic link'
						}
						assert output.contains(said), context
					}
				}
			}
			os.rm(link)!
		}
		os.unsetenv(tool_cache_dir_env)
		for setting, option in {
			'VTMP':    '--vtmp'
			'VCACHE':  '--vcache'
			'V3CACHE': '--v3cache'
		} {
			behind := os.join_path(directory, '${setting}-behind')
			os.mkdir(behind, mode: 0o700)!
			kept := os.join_path(behind, 'kept.o')
			os.write_file(kept, 'x')!
			cache_link := os.join_path(directory, '${setting}-link')
			os.symlink(behind, cache_link)!
			os.setenv(setting, '${cache_link}/', true)
			output := wipe_output(option)!
			context := '${setting}:\n${output}'
			assert os.is_file(kept), context
			assert os.is_dir(cache_link) && !os.is_link(cache_link), context
		}
	})!
}

// An empty setting names no folder: taken as a path, it would be the folder the wipe was started
// from, which the wipe must never empty.
fn test_an_empty_setting_names_no_folder() ! {
	assert tool_cache_wipe_place('') == ''
	directory := toolcache_test_dir('wipe_cache_empty_setting')
	working_folder := os.getwd()
	defer {
		os.chdir(working_folder) or {}
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		started_in := os.join_path(directory, 'started-in')
		os.mkdir(started_in)!
		kept := os.join_path(started_in, 'kept.txt')
		os.write_file(kept, 'x')!
		os.setenv(tool_cache_dir_env, '', true)
		os.setenv('V3CACHE', '', true)
		os.chdir(started_in)!
		for arguments in [['--tools'], ['--v3cache'], ['--dry-run']] {
			output := wipe_output(...arguments)!
			assert os.is_file(kept), '${arguments}: ${output}'
		}
	})!
}

// A wipe comes back to the folder it was started from, which it leaves while it works. The test
// starts it from a folder of its own, so that a wipe which never came back cannot pass because an
// earlier one left the process where this one goes.
fn test_a_wipe_comes_back_to_the_folder_it_was_started_from() ! {
	directory := toolcache_test_dir('wipe_cache_comes_back')
	working_folder := os.getwd()
	defer {
		os.chdir(working_folder) or {}
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		started_in := os.join_path(directory, 'started-in')
		os.mkdir(started_in)!
		os.chdir(started_in)!
		before := os.getwd()
		output := wipe_output('--dry-run')!
		assert os.getwd() == before, output
		assert os.real_path(before) == os.real_path(started_in), output
	})!
}

// A setting given as a relative path names a folder below the one the wipe was started from. When
// that folder has no name, because it was removed, the wipe cannot tell which folder the setting
// names, and fails instead of taking it as a folder below `/`.
fn test_a_relative_setting_without_a_working_folder_fails_the_wipe() ! {
	$if windows {
		// A folder that is in use cannot be removed there.
		return
	}
	directory := toolcache_test_dir('wipe_cache_relative_no_folder')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		modules := os.join_path(directory, 'modules')
		// The setting, its value, and what a refusal says, or '' when the wipe goes ahead: VMODULES
		// lists several folders, and the wipe uses only the first (see `os.vmodules_dir`).
		cases := [
			['VCACHE', 'relative-cache', 'VCACHE holds the relative path `relative-cache`'],
			['VMODULES', 'relative-modules${os.path_delimiter}${modules}',
				'VMODULES holds the relative path `relative-modules`'],
			['VMODULES', '${modules}${os.path_delimiter}relative-modules', ''],
		]
		for i, c in cases {
			gone := os.join_path(directory, 'gone-${i}')
			os.mkdir(gone)!
			ready := os.join_path(directory, 'ready-${i}')
			start := os.join_path(directory, 'start-${i}')
			mut env := os.environ()
			env[toolcache_wipe_child_env] = start
			env[toolcache_wipe_ready_env] = ready
			env[c[0]] = c[1]
			mut child := os.new_process(os.executable())
			child.set_work_folder(gone)
			child.set_environment(env)
			child.set_redirect_stdio()
			child.run()
			deadline := time.now().add(10 * time.second)
			for time.now() < deadline && !os.exists(ready) {
				time.sleep(time.millisecond)
			}
			os.rmdir(gone)!
			os.write_file(start, '')!
			output, code := toolcache_test_finish(mut child, 2 * time.minute)
			if c[2] == '' {
				assert code == 0, '${c[0]}=${c[1]}: ${output}'
			} else {
				assert code != 0, output
				assert output.contains('v wipe-cache: ${c[2]}'), output
			}
		}
	})!
}

// A path with `..` after a link names what the system takes it to name: the folder above the one
// the link leads to. The tool cache folder that `VTOOLS_CACHE_DIR` names that way is the one
// builds use, and the wipe of the folder around it keeps the entry of a tool being built there.
fn test_a_tool_cache_folder_named_through_a_link_and_dot_dot_is_known() ! {
	$if windows {
		// The system takes `..` as text there.
		return
	}
	directory := toolcache_test_dir('wipe_cache_link_dot_dot')
	defer {
		os.rmdir_all(directory) or {}
	}
	release := os.join_path(directory, 'release')
	previous := os.getwd()
	defer {
		os.chdir(previous) or {}
	}
	with_wipe_test_caches(directory, fn [directory, release, previous] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		tools := os.join_path(vtmp, 'tc')
		sub := os.join_path(vtmp, 'sub')
		link := os.join_path(directory, 'ln')
		os.symlink(sub, link)!
		// The second setting is relative, to the folder the wipe is started from.
		for setting in ['${link}/../tc', 'ln/../tc'] {
			os.setenv(tool_cache_dir_env, setting, true)
			for arguments in [['--vtmp'], []string{}] {
				os.mkdir_all(sub)!
				tool_cache_test_root(tools)!
				entry := tool_cache_test_entry(tools, 'vfmt', 'a')!
				lock_path := tool_cache_test_lock(tools, 'vfmt')
				os.write_file(lock_path, '')!
				os.rm(release) or {}
				mut holder := start_tool_cache_lock_holder(lock_path, release)!
				mut failure := ''
				os.chdir(directory)!
				output := wipe_output(...arguments) or {
					failure = err.msg()
					''
				}
				os.chdir(previous)!
				os.write_file(release, '')!
				holder.wait()
				holder.close()
				context := '${setting} ${arguments}: ${failure}\n${output}'
				assert failure == '', context
				assert os.is_dir(entry), context
				assert os.is_file(lock_path), context
				assert output.contains('V tmp.c and tests folder ${vtmp} was wiped, except the tool cache folders in it: ${tools}.'), context
			}
		}
		// The folder the setting names is the user's link there: every pass leaves it as it is.
		mine := os.join_path(directory, 'mytools')
		tool_cache_test_root(mine)!
		os.write_file(os.join_path(mine, 'keep'), 'keep')!
		for setting in ['${link}/../tc', 'ln/../tc'] {
			os.setenv(tool_cache_dir_env, setting, true)
			for arguments in [['--tools'], ['--vtmp'], []string{}] {
				os.mkdir_all(sub)!
				if os.is_link(tools) {
					os.rm(tools)!
				} else {
					os.rmdir_all(tools) or {}
				}
				os.symlink(mine, tools)!
				mut failure := ''
				os.chdir(directory)!
				output := wipe_output(...arguments) or {
					failure = err.msg()
					''
				}
				os.chdir(previous)!
				context := 'link: ${setting} ${arguments}: ${failure}\n${output}'
				assert failure == '', context
				assert os.is_link(tools), context
				assert os.is_file(os.join_path(mine, 'keep')), context
			}
		}
	})!
}

// A tool cache folder the wipe knew stays known for the rest of the run, also when a link on the way
// to it goes. Here the tools pass itself removes the default `<VTMP>/tools`, a link into VTMP or into
// V3CACHE, behind which `VTOOLS_CACHE_DIR` names the folder builds use. The pass of the cache folder
// the link led into then keeps that folder, with the entry and the lock of the build in it.
fn test_a_folder_behind_a_link_the_wipe_removes_keeps_its_build() ! {
	$if windows {
		// A link to a folder needs rights a test may lack there.
		return
	}
	directory := toolcache_test_dir('wipe_cache_link_removed_by_wipe')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		v3cache := os.join_path(directory, 'v3cache')
		root := os.join_path(vtmp, 'tools')
		os.setenv(tool_cache_dir_env, os.join_path(root, 'custom'), true)
		for round, case in [['vtmp'], ['vtmp', '--tools', '--vtmp'], ['v3cache']] {
			target := os.join_path(if case[0] == 'vtmp' { vtmp } else { v3cache }, 'out-${round}')
			inner := os.join_path(target, 'custom')
			tool_cache_test_root(target)!
			os.symlink(target, root)!
			release := os.join_path(directory, 'release-${round}')
			pending := os.join_path(directory, 'pending-${round}')
			mut holder, entry_name, lock_name := toolcache_test_pending_build(pending, release)!
			os.rename(pending, inner)!
			mut failure := ''
			output := wipe_output(...case[1..]) or {
				failure = err.msg()
				''
			}
			os.write_file(release, '')!
			holder.wait()
			holder.close()
			context := '${case}: ${failure}\n${output}'
			assert failure == '', context
			assert os.is_dir(os.join_path(inner, entry_name)), context
			assert os.is_file(os.join_path(inner, lock_name)), context
			os.rm(root) or {}
			os.rmdir_all(root) or {}
			os.rmdir_all(target)!
		}
	})!
}

// The folder a `VTOOLS_CACHE_DIR` link leads to stays known for the whole run, also when a build
// without that setting moves the link aside and makes a folder there (see
// `prepare_default_tool_cache_root`) after the wipe started: builds that took the link before still
// work in the folder it led to, inside VTMP or VCACHE here.
fn test_the_folder_a_moved_aside_setting_link_led_to_keeps_its_build() ! {
	$if windows {
		// V moves no folder aside there, and a link to a folder needs rights a test may lack.
		return
	}
	directory := toolcache_test_dir('wipe_cache_setting_link_moved')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		vcache := os.join_path(directory, 'vcache')
		root := os.join_path(vtmp, 'tools')
		os.setenv(tool_cache_dir_env, root, true)
		for round, case in [['vtmp'], ['vtmp', '--vtmp'], ['vcache', '--vcache']] {
			target := os.join_path(if case[0] == 'vtmp' { vtmp } else { vcache }, 'real-tools-${round}')
			release := os.join_path(directory, 'release-${round}')
			pending := os.join_path(directory, 'pending-${round}')
			mut holder, entry_name, lock_name := toolcache_test_pending_build(pending, release)!
			os.mkdir_all(os.dir(target))!
			os.rename(pending, target)!
			os.symlink(target, root)!
			mut run := new_tool_cache_wipe_run()
			// A build without the setting moves the link aside and makes a private folder there.
			problems := tool_cache_candidate_problems(root, true)
			opts := parse_wipe_cache_options([], case[1..])!
			mut lines := []string{}
			mut failure := ''
			if opts.vcache {
				run.wipe_folder_around_tool_roots(vcache, 'V cache', true, opts, mut lines) or {
					failure = err.msg()
				}
			}
			if failure == '' && opts.tools {
				run.wipe_tool_places(opts, mut lines) or { failure = err.msg() }
			}
			if failure == '' && opts.vtmp {
				run.wipe_folder_around_tool_roots(vtmp, 'V tmp.c and tests', true, opts, mut
					lines) or { failure = err.msg() }
			}
			os.write_file(release, '')!
			holder.wait()
			holder.close()
			context := '${case}: ${problems} ${failure}\n' + lines.join('\n')
			assert problems == [], context
			assert failure == '', context
			assert os.is_dir(os.join_path(target, entry_name)), context
			assert os.is_file(os.join_path(target, lock_name)), context
			for name in os.ls(vtmp)! {
				if name.starts_with('tools') {
					os.rm(os.join_path(vtmp, name)) or {}
					os.rmdir_all(os.join_path(vtmp, name)) or {}
				}
			}
			os.rmdir_all(target)!
		}
	})!
}

// A `..` after a link is taken as the system takes it when the wipe starts, in `VTOOLS_CACHE_DIR` and
// in `XDG_CACHE_HOME`, which a default tool cache folder comes from, and it stays so when the wipe
// removes that link: the folder that the setting spells without the link, here `<VTMP>/tc`, is no
// tool cache folder, and goes with the rest of VTMP. The link is the default `<VTMP>/tools`, which
// the tools pass removes before the pass of VTMP works out which tool cache folders lie in it.
fn test_a_setting_through_a_link_the_wipe_removes_names_no_other_folder_afterwards() ! {
	$if windows {
		// The system takes `..` as text there.
		return
	}
	directory := toolcache_test_dir('wipe_cache_dot_dot_link_removed')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		deep := os.join_path(vtmp, 'sub', 'deep')
		link := os.join_path(vtmp, 'tools')
		named := os.join_path(vtmp, 'sub', 'tc')
		spelled := os.join_path(vtmp, 'tc')
		for name in [tool_cache_dir_env, 'XDG_CACHE_HOME'] {
			os.mkdir_all(deep)!
			if os.is_link(link) {
				os.rm(link)!
			} else {
				os.rmdir_all(link) or {}
			}
			os.symlink(deep, link)!
			os.setenv(name, '${link}/../tc', true)
			builds := if name == tool_cache_dir_env {
				named
			} else {
				os.join_path(named, 'v', 'tools')
			}
			tool_cache_test_root(builds)!
			os.mkdir_all(spelled)!
			os.write_file(os.join_path(spelled, 'junk'), 'x')!
			output := wipe_output()!
			context := '${name}:\n${output}'
			assert !os.is_link(link), context
			assert os.is_dir(builds), context
			assert !os.exists(spelled), context
			os.unsetenv(tool_cache_dir_env)
		}
	})!
}

// Every tool cache folder the run has dealt with is known by where it lies, however the path to it
// was spelled: the wipe of the folder around it, named without the link, finds it there. So is a
// folder V moved aside beside the one that `VTOOLS_CACHE_DIR` names through the link.
fn test_a_folder_the_run_dealt_with_is_known_by_where_it_lies() ! {
	$if windows {
		// A link to a folder needs rights a test may lack there.
		return
	}
	directory := toolcache_test_dir('wipe_cache_done_spelling')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		real := os.join_path(directory, 'real')
		os.mkdir_all(real)!
		link := os.join_path(directory, 'ln')
		os.symlink(real, link)!
		mut run := new_tool_cache_wipe_run()
		done := ['tools.untrusted-1700000000-7', 'tools.untrusted-1700000000-8',
			'tools.untrusted-1700000000-9']
		for name in done {
			run.mark_done(os.join_path(link, name))
		}
		below := run.roots_below(real)
		for name in done {
			assert name in below, below.str()
		}
		os.mkdir_all(os.join_path(real, 'tools.untrusted-1700000000-1'))!
		os.setenv(tool_cache_dir_env, os.join_path(link, 'tools'), true)
		assert 'tools.untrusted-1700000000-1' in new_tool_cache_wipe_run().roots_below(real)
	})!
}

// In a folder V moved aside, lock files that cannot be opened, and have a second name so that they
// are not repaired, stay for no build: the folder stays with them, and the wipe fails and names the
// first of them.
fn test_lock_files_no_build_can_use_in_a_moved_aside_folder_fail_the_wipe_on_the_first() ! {
	$if windows {
		return
	}
	if os.geteuid() == 0 {
		// The superuser opens any file.
		return
	}
	directory := toolcache_test_dir('wipe_cache_aside_unopenable')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (cache string) ! {
		aside := os.join_path(cache, 'tools.untrusted-1700000000-1')
		tool_cache_test_root(aside)!
		mut lock_paths := []string{}
		for tool in ['vdoc', 'vfmt'] {
			lock_path := tool_cache_test_lock(aside, tool)
			os.write_file(lock_path, '')!
			os.chmod(lock_path, 0o400)!
			os.link(lock_path, os.join_path(directory, 'second-name-${tool}'))!
			lock_paths << lock_path
		}
		lock_paths.sort()
		mut run := new_tool_cache_wipe_run()
		mut lines := []string{}
		mut failure := ''
		run.wipe_tool_cache_directory(aside, parse_wipe_cache_options([], ['--tools'])!, mut
			lines) or { failure = err.msg() }
		assert failure.ends_with(': the lock file ${lock_paths[0]} cannot be opened'), failure
		for lock_path in lock_paths {
			assert os.is_file(lock_path), failure
		}
	})!
}

// The wipe takes the build lock of a tool once, however many times it asks for it: a process gives
// up a POSIX lock when it closes any descriptor of the file, and Windows refuses a second lock.
fn test_a_build_lock_is_taken_once_however_often_it_is_asked_for() ! {
	directory := toolcache_test_dir('wipe_cache_lock_once')
	defer {
		os.rmdir_all(directory) or {}
	}
	tool_cache_test_root(directory)!
	os.write_file(tool_cache_test_lock(directory, 'vfmt'), '')!
	dir := open_tool_cache_wipe_dir(directory)!
	defer {
		dir.close()
	}
	uid := u32(os.getuid())
	mut locks := ToolCacheWipeLocks{
		root: directory
		me:   uid
	}
	assert locks.probe(dir, 'vfmt', uid) == .held
	assert locks.probe(dir, 'vfmt', uid) == .held
	assert locks.held.len == 1
	for mut held in locks.held {
		held.release()
	}
}

// A `VTOOLS_CACHE_DIR` that reaches its folder through `..` after a link names that folder for the
// whole run, also when the wipe removes the folder the link leads to before it comes to the tool
// cache folder: the user's link there stays, and so does a real folder there, emptied.
fn test_a_setting_through_a_link_and_dot_dot_stays_known_when_the_link_target_goes() ! {
	$if windows {
		// The system takes `..` as text there.
		return
	}
	directory := toolcache_test_dir('wipe_cache_dot_dot_target_removed')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		sub := os.join_path(vtmp, 'sub')
		tc := os.join_path(vtmp, 'tc')
		mytools := os.join_path(directory, 'mytools')
		link := os.join_path(directory, 'ln')
		os.symlink(sub, link)!
		os.setenv(tool_cache_dir_env, '${link}/../tc', true)
		for as_link in [true, false] {
			for arguments in [['--vtmp'], []string{}] {
				os.mkdir_all(sub)!
				if as_link {
					tool_cache_test_root(mytools)!
					os.write_file(os.join_path(mytools, 'keep'), 'x')!
					os.symlink(mytools, tc)!
				} else {
					tool_cache_test_root(tc)!
					os.write_file(os.join_path(tc, 'stray'), 'x')!
				}
				mut failure := ''
				output := wipe_output(...arguments) or {
					failure = err.msg()
					''
				}
				context := 'link=${as_link} ${arguments}: ${failure}\n${output}'
				assert failure == '', context
				if as_link {
					assert os.is_link(tc), context
					assert os.is_file(os.join_path(mytools, 'keep')), context
				} else {
					assert os.is_dir(tc), context
					assert !os.exists(os.join_path(tc, 'stray')), context
				}
				os.rm(tc) or {}
				os.rmdir_all(tc) or {}
				os.rmdir_all(mytools) or {}
			}
		}
	})!
}

// On Windows a folder can also have a short name, such as `TOOLCA~1` for `toolcachedir`: a
// `VTOOLS_CACHE_DIR` that uses it names the same folder, which stays when the wipe of the folder
// around it leaves it empty.
fn test_a_tool_cache_folder_named_by_its_short_name_is_known() ! {
	$if !windows {
		return
	}
	directory := toolcache_test_dir('wipe_cache_short_name')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		tools := os.join_path(vtmp, 'toolcachedir')
		tool_cache_test_root(tools)!
		// `dir /x` lists the short name in the column before the name, and leaves it empty when the
		// volume makes no short names.
		listing := cmdexec.run('cmd', ['/c', 'dir', '/x', vtmp])
		mut short := ''
		for line in listing.output.split_into_lines() {
			words := line.fields()
			if words.len >= 2 && words.last() == 'toolcachedir' && words[words.len - 2] != '<DIR>' {
				short = words[words.len - 2]
			}
		}
		if listing.exit_code != 0 || short == '' {
			// The volume makes no short names.
			return
		}
		os.setenv(tool_cache_dir_env, os.join_path(vtmp, short), true)
		os.write_file(os.join_path(tools, 'plain'), 'x')!
		output := wipe_output('--vtmp')!
		assert os.is_dir(tools), output
		assert os.ls(tools)! == []string{}, output
	})!
}

// The same for a `VTOOLS_CACHE_DIR` that is a link, a junction here, named by its short name: the
// wipe of the folder around it leaves the link as it is, as the tools pass says it does.
fn test_a_tool_cache_link_named_by_its_short_name_is_left_as_it_is() ! {
	$if !windows {
		return
	}
	directory := toolcache_test_dir('wipe_cache_short_link')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		target := os.join_path(directory, 'linked-tools')
		tool_cache_test_root(target)!
		os.write_file(os.join_path(target, 'keep'), 'x')!
		link := os.join_path(vtmp, 'linktothetoolcache')
		if !toolcache_test_junction(link, target) {
			return
		}
		// `dir /x` lists the short name in the column before the name, and leaves it empty when the
		// volume makes no short names.
		listing := cmdexec.run('cmd', ['/c', 'dir', '/x', vtmp])
		mut short := ''
		for line in listing.output.split_into_lines() {
			words := line.fields()
			at := words.index('linktothetoolcache')
			if at >= 1 && words[at - 1] != '<JUNCTION>' {
				short = words[at - 1]
			}
		}
		if listing.exit_code != 0 || short == '' {
			// The volume makes no short names.
			return
		}
		os.setenv(tool_cache_dir_env, os.join_path(vtmp, short), true)
		output := wipe_output()!
		assert os.is_link(link), output
		assert os.is_file(os.join_path(target, 'keep')), output
		assert output.contains('was left as it is'), output
	})!
}

// On Windows a path cannot reach an item whose name ends in a dot or a space, or is that of a
// device: `tools.` reaches `tools`, and `nul` the device. Such an item, which WSL makes for example,
// stays, and the wipe fails and names it; the item a path would reach instead, here a tool cache
// folder with a build in it, is not touched.
fn test_a_name_a_windows_path_cannot_reach_stays_and_fails_the_wipe() ! {
	$if !windows {
		return
	}
	directory := toolcache_test_dir('wipe_cache_unreachable_names')
	// A path that starts with `\\?\` is taken as it is written.
	long := '\\\\?\\' + os.join_path(directory, 'vtmp')
	odd := ['junk ', 'nul', 'tools.']
	defer {
		for name in odd {
			os.rm(long + '\\' + name) or {}
		}
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory, long, odd] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		root := os.join_path(vtmp, 'tools')
		release := os.join_path(directory, 'release')
		mut holder, entry_name, lock_name := toolcache_test_pending_build(root, release)!
		for name in odd {
			os.write_file(long + '\\' + name, 'x')!
		}
		mut failure := ''
		wipe_output('--vtmp') or { failure = err.msg() }
		os.write_file(release, '')!
		holder.wait()
		holder.close()
		listed := os.ls(vtmp)!
		assert failure.contains('junk : its name ends in a dot or a space'), failure
		for name in odd {
			assert name in listed, '${failure}\n${listed}'
		}
		assert os.is_dir(os.join_path(root, entry_name)), failure
		assert os.is_file(os.join_path(root, lock_name)), failure
		// The same inside a tool cache folder, where each name is looked at before it goes.
		for name in odd {
			os.rm(long + '\\' + name)!
		}
		os.write_file(long + '\\tools\\stray.', 'x')!
		failure = ''
		wipe_output('--tools') or { failure = err.msg() }
		os.rm(long + '\\tools\\stray.') or {}
		assert failure.contains('could not look at ${os.join_path(root, 'stray.')}'), failure
	})!
}

// On Windows a folder search takes `*`, `?`, `<`, `>` and `"` in the last name of a path as a
// pattern. A setting whose last name holds one of them names no folder that the pattern matches:
// the wipe leaves such a folder as it is.
fn test_a_setting_with_a_pattern_character_names_no_other_folder() ! {
	$if !windows {
		return
	}
	directory := toolcache_test_dir('wipe_cache_pattern_setting')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		projects := os.join_path(directory, 'data', 'projects')
		important := os.join_path(projects, 'important.txt')
		for name, variable in {
			'pro<':      tool_cache_dir_env
			'pr>jects':  tool_cache_dir_env
			'projects"': 'VCACHE'
		} {
			os.mkdir_all(os.join_path(projects, 'sub'))!
			os.write_file(important, 'x')!
			os.setenv(variable, os.join_path(directory, 'data', name), true)
			arguments := if variable == 'VCACHE' { ['--vcache'] } else { ['--tools'] }
			mut failure := ''
			output := wipe_output(...arguments) or {
				failure = err.msg()
				''
			}
			os.setenv('VCACHE', os.join_path(directory, 'vcache'), true)
			os.unsetenv(tool_cache_dir_env)
			context := '${name}: ${failure}\n${output}'
			assert os.is_file(important), context
			assert os.is_dir(os.join_path(projects, 'sub')), context
		}
	})!
}

// The folder search behind `tool_cache_listed_name` counts what it finds only when its own name is
// the one asked for, also for a name that a search takes as a pattern and that matches another
// folder; a name in another case, or the short name, counts.
fn test_the_folder_search_counts_only_the_name_asked_for() ! {
	// The function exists on Windows alone, so the whole test is in a block that only Windows builds.
	$if windows {
		directory := toolcache_test_dir('wipe_cache_searched_name')
		defer {
			os.rmdir_all(directory) or {}
		}
		os.mkdir_all(os.join_path(directory, 'projects'))!
		for pattern in ['pro<', 'pr>jects', 'projects"', 'pro*', 'proj?cts'] {
			if listed := tool_cache_searched_name(os.join_path(directory, pattern), pattern) {
				assert false, '${pattern} found ${listed}'
			}
		}
		found := tool_cache_searched_name(os.join_path(directory, 'PROJECTS'), 'PROJECTS') or { '' }
		assert found == 'projects'
	}
}

// `tool_cache_full_path` gives the path that Windows makes, and a path that Windows makes none of,
// such as one longer than the 32767 characters a path can have, as it is.
fn test_the_full_path_is_the_one_windows_makes_or_the_path_itself() {
	// The function exists on Windows alone, so the whole test is in a block that only Windows builds.
	$if windows {
		assert tool_cache_full_path('C:\\x\\tools.') == 'C:\\x\\tools'
		assert tool_cache_full_path('C:\\x\\nul') == '\\\\.\\nul'
		long := 'C:\\' + 'a'.repeat(40000)
		assert tool_cache_full_path(long) == long
	}
}

// On Windows a setting that ends in a dot or a space names the folder without them, as it does for
// every other program: the wipe of the folder around it keeps the build in it.
fn test_a_setting_that_ends_in_a_dot_or_a_space_names_the_folder_windows_reaches() ! {
	$if !windows {
		return
	}
	directory := toolcache_test_dir('wipe_cache_setting_trailing_dot')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		tools := os.join_path(directory, 'vtmp', 'tc')
		for round, ending in ['.', ' ', '. .'] {
			os.setenv(tool_cache_dir_env, tools + ending, true)
			release := os.join_path(directory, 'release-${round}')
			mut holder, entry_name, lock_name := toolcache_test_pending_build(tools, release)!
			mut failure := ''
			output := wipe_output('--vtmp') or {
				failure = err.msg()
				''
			}
			os.write_file(release, '')!
			holder.wait()
			holder.close()
			context := '`${ending}`: ${failure}\n${output}'
			assert failure == '', context
			assert os.is_dir(os.join_path(tools, entry_name)), context
			assert os.is_file(os.join_path(tools, lock_name)), context
			os.rmdir_all(tools)!
		}
	})!
}

// A name that looks like a device name but that a Windows path reaches is ordinary content: `com0`
// and `lpt0` name no device. The wipe removes it.
fn test_a_name_like_a_device_that_a_windows_path_reaches_is_removed() ! {
	$if !windows {
		return
	}
	directory := toolcache_test_dir('wipe_cache_device_like_names')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		names := ['com0', 'lpt0']
		for name in names {
			os.write_file(os.join_path(vtmp, name), 'x')!
		}
		for arguments in [['--vtmp', '--dry-run'], ['--vtmp']] {
			mut failure := ''
			output := wipe_output(...arguments) or {
				failure = err.msg()
				''
			}
			assert failure == '', '${arguments}: ${failure}\n${output}'
		}
		assert os.ls(vtmp)!.filter(it in names) == []string{}
	})!
}

// On Windows a folder beside a tool cache folder whose name ends in a dot or a space, which WSL makes,
// cannot be reached through a path: the tools pass leaves it, fails and names it, a dry run too,
// rather than saying that nothing is there. So does a dry run of a cache folder that holds such a
// name, or a name that Windows takes as a device, as the real run does; a link is not followed.
fn test_a_name_no_windows_path_reaches_fails_the_tools_pass_and_a_dry_run() ! {
	$if !windows {
		return
	}
	directory := toolcache_test_dir('wipe_cache_unreachable_sibling')
	// A path that starts with `\\?\` is taken as it is written; `os.mkdir` would drop the dot, so the
	// item is a file.
	long_root := '\\\\?\\' + directory
	odd := 'vtools.untrusted-1700000000-7.'
	defer {
		os.rm(long_root + '\\out\\' + odd) or {}
		os.rm(long_root + '\\vtmp\\junk.') or {}
		os.rm(long_root + '\\vtmp\\nul') or {}
		os.rm(long_root + '\\outside\\q.') or {}
		os.rm(long_root + '\\vtmp\\sub\\zz.') or {}
		os.rm(long_root + '\\vcache\\x.') or {}
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory, long_root, odd] (_ string) ! {
		out := os.join_path(directory, 'out')
		os.mkdir_all(out)!
		os.setenv(tool_cache_dir_env, os.join_path(out, 'vtools'), true)
		os.write_file(long_root + '\\out\\' + odd, 'x')!
		for arguments in [['--tools'], ['--tools', '--dry-run']] {
			mut failure := ''
			output := wipe_output(...arguments) or {
				failure = err.msg()
				''
			}
			context := '${arguments}: ${failure}\n${output}'
			assert failure.contains(os.join_path(out, odd)), context
			assert failure.contains('its name ends in a dot or a space'), context
			assert failure.contains('could not look at Moved-aside V tool cache folder'), context
			assert odd in os.ls(out)!, context
		}
		// Beside a folder with the name a path with it reaches: both are listed, and the pass fails.
		twin := os.join_path(out, odd.trim_right('.'))
		os.mkdir_all(twin)!
		for arguments in [['--tools', '--dry-run'], ['--tools']] {
			mut failure := ''
			output := wipe_output(...arguments) or {
				failure = err.msg()
				''
			}
			context := 'beside ${twin}, ${arguments}: ${failure}\n${output}'
			assert failure.contains(os.join_path(out, odd)), context
			assert odd in os.ls(out)!, context
		}
		os.rmdir_all(twin) or {}
		os.rm(long_root + '\\out\\' + odd)!
		os.unsetenv(tool_cache_dir_env)
		os.write_file(long_root + '\\vtmp\\junk.', 'x')!
		mut failure := ''
		output := wipe_output('--vtmp', '--dry-run') or {
			failure = err.msg()
			''
		}
		assert failure.contains('junk.: its name ends in a dot or a space'), '${failure}\n${output}'
		os.rm(long_root + '\\vtmp\\junk.')!
		// A name that this Windows takes as a device: a path with it reaches the device, not the file.
		vtmp := os.join_path(directory, 'vtmp')
		os.write_file(long_root + '\\vtmp\\nul', 'x')!
		for arguments in [['--vtmp', '--dry-run'], ['--vtmp']] {
			failure = ''
			got := wipe_output(...arguments) or {
				failure = err.msg()
				''
			}
			context := '${arguments}: ${failure}\n${got}'
			assert failure.contains('nul: its name is that of a device'), context
			assert 'nul' in os.ls(vtmp)!, context
		}
		os.rm(long_root + '\\vtmp\\nul')!
		// Behind a junction, which is removed itself, a name that no path reaches fails no run and stays.
		outside := os.join_path(directory, 'outside')
		os.mkdir_all(outside)!
		os.write_file(long_root + '\\outside\\q.', 'x')!
		assert toolcache_test_junction(os.join_path(vtmp, 'j'), outside)
		for arguments in [['--vtmp', '--dry-run'], ['--vtmp']] {
			failure = ''
			got := wipe_output(...arguments) or {
				failure = err.msg()
				''
			}
			assert failure == '', '${arguments}: ${failure}\n${got}'
		}
		assert !os.is_link(os.join_path(vtmp, 'j'))
		assert 'q.' in os.ls(outside)!
		os.rm(long_root + '\\outside\\q.')!
		// Below a folder the removal goes into, and in a cache folder that holds no tool cache folder,
		// between names it can reach: the dry run fails as the real run does, on the same name.
		sub := os.join_path(vtmp, 'sub')
		os.mkdir_all(sub)!
		os.write_file(os.join_path(sub, 'a'), 'x')!
		os.write_file(long_root + '\\vtmp\\sub\\zz.', 'x')!
		os.write_file(os.join_path(sub, 'zzz'), 'x')!
		vcache := os.join_path(directory, 'vcache')
		os.mkdir_all(vcache)!
		os.write_file(os.join_path(vcache, 'a'), 'x')!
		os.write_file(long_root + '\\vcache\\x.', 'x')!
		os.write_file(os.join_path(vcache, 'z'), 'x')!
		for arguments in [['--vtmp', '--dry-run'], ['--vtmp'], ['--vcache', '--dry-run'], ['--vcache']] {
			failure = ''
			got := wipe_output(...arguments) or {
				failure = err.msg()
				''
			}
			context := '${arguments}: ${failure}\n${got}'
			if arguments[0] == '--vtmp' {
				assert failure.contains(os.join_path('sub', 'zz.') +
					': its name ends in a dot or a space'), context
				assert 'zz.' in os.ls(sub)!, context
			} else {
				assert failure.contains('x.: its name ends in a dot or a space'), context
				assert 'x.' in os.ls(vcache)!, context
			}
		}
	})!
}

// A wipe started from a folder that it may not read, and that was removed since, works as from any
// other folder when no setting is a relative path: it needs no working folder.
fn test_a_wipe_started_from_a_removed_folder_it_cannot_read_works() ! {
	$if windows {
		return
	}
	if os.geteuid() == 0 {
		// The superuser reads any folder.
		return
	}
	directory := toolcache_test_dir('wipe_cache_removed_working_folder')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (cache_v_dir string) ! {
		tools := os.join_path(cache_v_dir, 'tools')
		tool_cache_test_root(tools)!
		entry := tool_cache_test_entry(tools, 'vfmt', 'a')!
		link := os.join_path(tools, 'stray-link')
		os.symlink(os.join_path(directory, 'nowhere'), link)!
		leftover := os.join_path(directory, 'vtmp', 'a.tmp.c')
		os.write_file(leftover, 'x')!
		closed := os.join_path(directory, 'closed')
		os.mkdir_all(closed)!
		ready := os.join_path(directory, 'ready')
		start := os.join_path(directory, 'start')
		mut env := os.environ()
		env[toolcache_wipe_child_env] = start
		env[toolcache_wipe_ready_env] = ready
		os.chmod(closed, 0o300)!
		mut child := os.new_process(os.executable())
		child.set_work_folder(closed)
		child.set_environment(env)
		child.set_redirect_stdio()
		child.run()
		deadline := time.now().add(10 * time.second)
		for time.now() < deadline && !os.exists(ready) {
			time.sleep(time.millisecond)
		}
		os.rmdir(closed)!
		os.write_file(start, '')!
		output, code := toolcache_test_finish(mut child, 2 * time.minute)
		assert code == 0, output
		assert !os.exists(entry), output
		assert !os.is_link(link), output
		assert !os.exists(leftover), output
	})!
}

// The entries of a tool whose lock file cannot be made stay, since nothing would guard them, and
// the wipe then fails and names the lock file: they stay for no build.
fn test_entries_whose_lock_file_cannot_be_made_stay_and_fail_the_wipe() ! {
	$if windows {
		// The mode of a folder does not keep files from being made in it there.
		return
	}
	if os.geteuid() == 0 {
		// The superuser makes files in a folder whatever its mode is.
		return
	}
	directory := toolcache_test_dir('wipe_cache_lock_not_made')
	defer {
		toolcache_test_unlock_tree(directory)
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn (cache_v_dir string) ! {
		tools := os.join_path(cache_v_dir, 'tools')
		tool_cache_test_root(tools)!
		entry := tool_cache_test_entry(tools, 'vls', 'a')!
		os.chmod(tools, 0o555)!
		message := wipe_failure('--tools')
		os.chmod(tools, 0o700)!
		assert message.starts_with('v wipe-cache: could not remove V tool cache folder ${tools}: the entries of vls stay: '), message
		assert message.contains(tool_cache_test_lock(tools, 'vls')), message
		assert os.is_dir(entry), message
	})!
}

// A dry run fails where the real run fails, with the same message: here a known tool cache folder
// that cannot be opened, inside the cache folder being wiped, or inside a tool cache folder.
fn test_a_dry_run_fails_where_the_real_run_fails() ! {
	$if windows {
		return
	}
	if os.geteuid() == 0 {
		// The superuser opens a folder whatever its mode is.
		return
	}
	directory := toolcache_test_dir('wipe_cache_dry_failure')
	defer {
		toolcache_test_unlock_tree(directory)
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		inner := os.join_path(vtmp, 'tools')
		for configured in ['', vtmp] {
			arguments := if configured == '' { ['--vtmp'] } else { ['--tools'] }
			if configured == '' {
				os.unsetenv(tool_cache_dir_env)
			} else {
				os.setenv(tool_cache_dir_env, configured, true)
			}
			tool_cache_test_root(inner)!
			os.write_file(os.join_path(vtmp, 'a.tmp.c'), 'x')!
			os.chmod(inner, 0o000)!
			mut dry_arguments := arguments.clone()
			dry_arguments << '--dry-run'
			dry := wipe_failure(...dry_arguments)
			real := wipe_failure(...arguments)
			os.chmod(inner, 0o700)!
			context := '${arguments}:\n${dry}\n---\n${real}'
			assert real != '', context
			assert dry == real, context
		}
	})!
}

// A folder V moved aside that holds nothing but lock files goes with them. When they cannot be
// removed, the wipe fails and names what stays, instead of saying that the folder was wiped.
fn test_a_moved_aside_folder_that_cannot_be_removed_fails_the_wipe() ! {
	$if windows {
		return
	}
	if os.geteuid() == 0 {
		// The superuser removes from a folder whatever its mode is.
		return
	}
	directory := toolcache_test_dir('wipe_cache_aside_stuck')
	defer {
		toolcache_test_unlock_tree(directory)
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn (cache_v_dir string) ! {
		aside := os.join_path(cache_v_dir, 'tools.untrusted-1700000000-4242')
		tool_cache_test_root(aside)!
		lock_path := tool_cache_test_lock(aside, 'vfmt')
		os.write_file(lock_path, '')!
		os.chmod(aside, 0o555)!
		message := wipe_failure('--tools')
		os.chmod(aside, 0o700)!
		assert message.starts_with('v wipe-cache: could not remove Moved-aside V tool cache folder ${aside}: '), message
		assert message.contains(lock_path), message
		assert os.is_file(lock_path), message
	})!
}

// A wipe that is not asked to wipe the tool caches neither makes nor needs the folders that builds
// would use: it makes no home cache folder and no `VTMP`, and works where neither can be made.
fn test_a_wipe_of_one_cache_folder_makes_and_needs_no_other_folder() ! {
	directory := toolcache_test_dir('wipe_cache_no_side_folders')
	defer {
		toolcache_test_unlock_tree(directory)
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		home := os.join_path(directory, 'nohome')
		vtmp := os.join_path(directory, 'novtmp')
		os.setenv('HOME', home, true)
		os.setenv('USERPROFILE', home, true)
		os.unsetenv('XDG_CACHE_HOME')
		os.setenv('VTMP', vtmp, true)
		vcache := os.join_path(directory, 'vcache')
		os.mkdir_all(vcache)!
		os.write_file(os.join_path(vcache, 'x.o'), 'x')!
		for arguments in [['--vcache', '--dry-run'], ['--vcache']] {
			output := wipe_output(...arguments)!
			assert !os.exists(home), '${arguments}: ${output}'
			assert !os.exists(vtmp), '${arguments}: ${output}'
		}
		$if !windows {
			if os.geteuid() != 0 {
				locked := os.join_path(directory, 'locked')
				os.mkdir(locked)!
				os.setenv('HOME', os.join_path(locked, 'home'), true)
				os.setenv('XDG_CACHE_HOME', os.join_path(locked, 'xdg'), true)
				os.mkdir_all(vtmp)!
				os.chmod(locked, 0o555)!
				for arguments in [['--vtmp'], ['--vcache'], ['--v3cache'], ['--vtmp', '--dry-run']] {
					wipe_output(...arguments) or {
						os.chmod(locked, 0o700) or {}
						return error('${arguments}: ${err.msg()}')
					}
				}
				os.chmod(locked, 0o700)!
			}
		}
	})!
}

// A wipe that cannot remove an entry, because a program in it runs, fails at once, as a wipe that
// cannot remove a file always did: it does not keep waiting while it holds the build locks of the
// folder.
fn test_a_wipe_does_not_wait_on_what_it_cannot_remove() ! {
	$if !windows {
		// A file that is open can be removed elsewhere.
		return
	}
	directory := toolcache_test_dir('wipe_cache_no_wait')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn (cache_v_dir string) ! {
		tools := os.join_path(cache_v_dir, 'tools')
		tool_cache_test_root(tools)!
		mut held := []os.File{}
		for tool in ['vfmt', 'vdoc', 'vvet'] {
			entry := tool_cache_test_entry(tools, tool, 'a')!
			held << os.open(os.join_path(entry, tool + tool_exe_suffix()))!
		}
		started := time.now()
		message := wipe_failure('--tools')
		elapsed := time.since(started)
		for mut file in held {
			file.close()
		}
		assert message != '', message
		assert elapsed < 3 * time.second, '${elapsed}: ${message}'
	})!
}

// A folder in a cache folder that cannot be opened fails the wipe with the reason that it cannot
// be opened, not with a later failure that hides it.
fn test_a_folder_that_cannot_be_opened_is_named_with_the_reason() ! {
	$if windows {
		return
	}
	if os.geteuid() == 0 {
		// The superuser opens a folder whatever its mode is.
		return
	}
	directory := toolcache_test_dir('wipe_cache_closed_folder')
	defer {
		toolcache_test_unlock_tree(directory)
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vcache := os.join_path(directory, 'vcache')
		closed := os.join_path(vcache, 'closed')
		os.mkdir_all(closed)!
		os.write_file(os.join_path(closed, 'file'), 'x')!
		os.chmod(closed, 0o000)!
		message := wipe_failure('--vcache')
		os.chmod(closed, 0o700)!
		// EACCES is 13 on every POSIX system V runs on.
		assert message == 'v wipe-cache: could not remove V cache folder ${vcache}: ${closed}: ${os.posix_get_error_msg(13)}', message
	})!
}

// The wipe holds the build lock of a tool until the last of its entries is gone, so that a build
// of that tool cannot start in an entry that the wipe removes after it.
fn test_the_wipe_holds_a_tools_lock_until_its_last_entry_is_gone() ! {
	directory := toolcache_test_dir('wipe_cache_lock_every_entry')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (cache_v_dir string) ! {
		tools := os.join_path(cache_v_dir, 'tools')
		tool_cache_test_root(tools)!
		lock_path := tool_cache_test_lock(tools, 'vfmt')
		os.write_file(lock_path, '')!
		mut entries := []string{}
		for key in ['a', 'b', 'c'] {
			entries << tool_cache_test_entry(tools, 'vfmt', key)!
		}
		seen := os.join_path(directory, 'seen')
		mut run := new_tool_cache_wipe_run()
		run.before_acting = fn [entries, lock_path, seen] (path string, step string) {
			if step == 'remove' && path in entries {
				before := os.read_file(seen) or { '' }
				os.write_file(seen, before + toolcache_test_lock_state(lock_path) + ' ') or {
					panic(err)
				}
			}
		}
		opts := parse_wipe_cache_options([], ['--tools'])!
		mut lines := []string{}
		run.wipe_tool_cache_directory(tools, opts, mut lines)!
		assert os.read_file(seen)! == 'busy busy busy ', lines.join('\n')
		assert entries.all(!os.exists(it)), lines.join('\n')
	})!
}

// What is no folder at a known tool cache path inside a folder being wiped is removed with a call
// that cannot remove a folder, not even an empty one: a build makes its new folder some steps
// before it puts anything in it.
fn test_an_empty_folder_made_at_a_known_place_inside_a_wiped_folder_stays() ! {
	$if windows {
		// A link to a folder needs rights a test may lack.
		return
	}
	directory := toolcache_test_dir('wipe_cache_empty_made_inside')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		outside := os.join_path(directory, 'outside')
		tool_cache_test_root(outside)!
		vtmp := os.join_path(directory, 'vtmp')
		root := os.join_path(vtmp, 'tools')
		os.symlink(outside, root)!
		mut run := new_tool_cache_wipe_run()
		run.before_acting = fn [root] (path string, step string) {
			if path == root && step == 'unlink' && os.is_link(root) {
				os.rm(root) or { panic(err) }
				os.mkdir(root) or { panic(err) }
			}
		}
		opts := parse_wipe_cache_options([], ['--vtmp'])!
		mut lines := []string{}
		run.wipe_folder_around_tool_roots(vtmp, 'V tmp.c and tests', true, opts, mut lines)!
		output := lines.join('\n')
		assert os.is_dir(root) && !os.is_link(root), output
		assert output.contains('V tmp.c and tests folder ${vtmp} was wiped, except the tool cache folders in it: ${root}.'), output
	})!
}

// A tool entry that cannot be removed fails the wipe, which names what stays.
fn test_a_tool_entry_that_cannot_be_removed_fails_the_wipe() ! {
	$if windows {
		return
	}
	if os.geteuid() == 0 {
		// The superuser removes from a folder whatever its mode is.
		return
	}
	directory := toolcache_test_dir('wipe_cache_stuck_entry')
	defer {
		toolcache_test_unlock_tree(directory)
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn (cache_v_dir string) ! {
		tools := os.join_path(cache_v_dir, 'tools')
		tool_cache_test_root(tools)!
		entry := tool_cache_test_entry(tools, 'vfmt', 'a')!
		stuck := toolcache_test_stuck_folder(entry)!
		message := wipe_failure('--tools')
		toolcache_test_unlock_tree(tools)
		assert message.starts_with('v wipe-cache: could not remove V tool cache folder ${tools}: ${os.join_path(stuck, 'file')}: '), message
		assert os.is_file(os.join_path(stuck, 'file')), message
	})!
}

// A tool cache folder inside `V3CACHE` keeps the entry and the lock file of a tool being built when
// the V3 cache folder is wiped, as one inside `VTMP` or `VCACHE` does.
fn test_a_tool_cache_folder_inside_v3cache_keeps_its_build() ! {
	directory := toolcache_test_dir('wipe_cache_inside_v3cache')
	defer {
		os.rmdir_all(directory) or {}
	}
	release := os.join_path(directory, 'release')
	with_wipe_test_caches(directory, fn [directory, release] (_ string) ! {
		v3cache := os.join_path(directory, 'v3cache')
		tools := os.join_path(v3cache, 'custom-tools')
		os.setenv(tool_cache_dir_env, tools, true)
		for arguments in [['--v3cache'], []string{}] {
			tool_cache_test_root(tools)!
			entry := tool_cache_test_entry(tools, 'vtest', 'a')!
			lock_path := tool_cache_test_lock(tools, 'vtest')
			os.write_file(lock_path, '')!
			other := os.join_path(v3cache, 'other.o')
			os.write_file(other, 'x')!
			os.rm(release) or {}
			mut holder := start_tool_cache_lock_holder(lock_path, release)!
			mut failure := ''
			output := wipe_output(...arguments) or {
				failure = err.msg()
				''
			}
			os.write_file(release, '')!
			holder.wait()
			holder.close()
			context := '${arguments}: ${failure}\n${output}'
			assert failure == '', context
			assert os.is_dir(entry), context
			assert os.is_file(lock_path), context
			assert !os.exists(other), context
			assert output.contains('V3 cache folder ${v3cache} was wiped, except the tool cache folders in it: ${tools}.'), context
		}
	})!
}

// Run as the superuser, the wipe takes an entry under the build lock of the user who built it, the
// one that user's builds take, not under a lock of its own.
fn test_the_superuser_takes_the_lock_of_the_user_who_built_an_entry() ! {
	$if windows {
		// There is no superuser there (see `tool_cache_wipe_is_superuser`).
		return
	}
	directory := toolcache_test_dir('wipe_cache_superuser_lock')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (cache_v_dir string) ! {
		tools := os.join_path(cache_v_dir, 'tools')
		tool_cache_test_root(tools)!
		// Not the superuser's own id, whoever runs the test.
		builder := if os.getuid() == 0 { u32(4242) } else { u32(os.getuid()) }
		entry := os.join_path(tools, 'vfmt-${'a'.repeat(64)}.${builder}')
		os.mkdir(entry)!
		builder_lock := os.join_path(tools, tool_cache_lock_name('vfmt', builder))
		seen := os.join_path(directory, 'seen')
		mut run := new_tool_cache_wipe_run()
		run.user = 0
		run.before_acting = fn [entry, builder_lock, seen] (path string, step string) {
			if path == entry && step == 'remove' {
				os.write_file(seen, toolcache_test_lock_state(builder_lock)) or { panic(err) }
			}
		}
		opts := parse_wipe_cache_options([], ['--tools'])!
		mut lines := []string{}
		run.wipe_tool_cache_directory(tools, opts, mut lines)!
		output := lines.join('\n')
		assert os.read_file(seen)! == 'busy', output
		assert !os.exists(entry), output
		assert os.ls(tools)! == [os.file_name(builder_lock)], output
		if os.getuid() == 0 {
			// The builds of that user accept only a lock file of their own.
			assert os.stat(builder_lock)!.uid == builder, output
		}
	})!
}

// Run as root for another user, in a folder that several accounts share: when another account has
// put a link at the name of that user's lock file, the entries of the tool stay, since nothing
// guards them, and the wipe fails and says why.
fn test_entries_whose_lock_name_another_account_took_stay_and_fail_the_wipe() ! {
	$if windows {
		return
	}
	if os.getuid() != 0 {
		// Only the superuser can make the files of other accounts.
		return
	}
	directory := toolcache_test_dir('wipe_cache_lock_name_taken')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		shared := os.join_path(directory, 'shared')
		os.mkdir(shared)!
		os.chmod(shared, 0o1777)!
		builder := u32(4242)
		entry := os.join_path(shared, 'vfmt-${'a'.repeat(64)}.${builder}')
		os.mkdir(entry)!
		os.chown(entry, int(builder), int(builder))!
		taken := os.join_path(shared, tool_cache_lock_name('vfmt', builder))
		os.symlink(os.join_path(directory, 'nowhere'), taken)!
		result := cmdexec.run('chown', ['-h', '4343', taken])
		assert result.exit_code == 0, result.output
		os.setenv(tool_cache_dir_env, shared, true)
		mut run := new_tool_cache_wipe_run()
		run.user = i64(builder)
		opts := parse_wipe_cache_options([], ['--tools'])!
		mut lines := []string{}
		mut failure := ''
		run.wipe_tool_cache_directory(shared, opts, mut lines) or { failure = err.msg() }
		assert failure.starts_with('v wipe-cache: could not remove V tool cache folder ${shared}: the entries of vfmt stay: '), failure
		assert failure.contains(taken), failure
		assert os.is_dir(entry), failure
		assert os.is_link(taken), failure
	})!
}

// The wipe gives back every descriptor it takes: with few to spare, it wipes a folder with many
// names in it, and many folders below one another.
fn test_a_wipe_gives_back_every_descriptor_it_takes() ! {
	$if windows {
		return
	}
	directory := toolcache_test_dir('wipe_cache_descriptors_back')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (cache_v_dir string) ! {
		tools := os.join_path(cache_v_dir, 'tools')
		tool_cache_test_root(tools)!
		for i in 0 .. 80 {
			os.write_file(os.join_path(tools, 'plain_${i}'), 'x')!
		}
		vtmp := os.join_path(directory, 'vtmp')
		for i in 0 .. 40 {
			deeper := os.join_path(vtmp, 'tsession_${i}', 'deeper')
			os.mkdir_all(deeper)!
			for j in 0 .. 3 {
				os.write_file(os.join_path(deeper, 'f_${j}'), 'x')!
			}
		}
		start := os.join_path(directory, 'start')
		os.write_file(start, '')!
		mut env := os.environ()
		env[toolcache_wipe_child_env] = start
		mut child := os.new_process('/bin/sh')
		child.set_args(['-c', 'ulimit -n 48 && exec "\$0"', os.executable()])
		child.set_environment(env)
		child.set_redirect_stdio()
		child.run()
		output, code := toolcache_test_finish(mut child, 2 * time.minute)
		assert code == 0, output
		assert os.ls(tools)! == []string{}, output
		assert os.ls(vtmp)!.filter(it.starts_with('tsession_')) == []string{}, output
	})!
}

// toolcache_test_unopenable_lock makes the lock file of `tool` for the user `uid` in `root`, one
// that cannot be opened for writing and that the wipe does not repair, since it has a second name,
// `second_name`. It returns the lock file.
fn toolcache_test_unopenable_lock(root string, tool string, uid u32, second_name string) !string {
	lock_path := os.join_path(root, tool_cache_lock_name(tool, uid))
	os.write_file(lock_path, '')!
	os.chmod(lock_path, 0o400)!
	os.link(lock_path, second_name)!
	return lock_path
}

// In a tool cache folder builds use, a lock file of this user that cannot be opened, and that
// guards no entry, stays like every lock file there: the wipe succeeds, and names the tool.
fn test_a_lock_file_that_cannot_be_opened_and_guards_nothing_does_not_fail_the_wipe() ! {
	$if windows {
		return
	}
	if os.geteuid() == 0 {
		// The superuser opens a read-only file for writing anyway.
		return
	}
	directory := toolcache_test_dir('wipe_cache_stuck_lock_alone')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (cache_v_dir string) ! {
		tools := os.join_path(cache_v_dir, 'tools')
		tool_cache_test_root(tools)!
		lock_path := toolcache_test_unopenable_lock(tools, 'vfmt', u32(os.getuid()), os.join_path(directory,
			'second-name'))!
		for arguments in [['--tools', '--dry-run'], ['--tools']] {
			output := wipe_output(...arguments)!
			assert output.contains('except for the tools whose lock file cannot be opened and was left as it is: vfmt.'), '${arguments}: ${output}'
			assert os.is_file(lock_path), output
		}
	})!
}

// A folder V moved aside stays while something in it is kept for a build or for another account.
// A lock file of this user that cannot be opened then stays with it, and does not fail the wipe:
// the folder could not go anyway.
fn test_a_moved_aside_folder_that_stays_for_a_build_or_an_account_keeps_a_stuck_lock_file_quietly() ! {
	$if windows {
		return
	}
	if os.geteuid() == 0 {
		// The superuser opens a read-only file for writing anyway, and takes every user's lock.
		return
	}
	directory := toolcache_test_dir('wipe_cache_aside_stays_quietly')
	defer {
		os.rmdir_all(directory) or {}
	}
	release_path := os.join_path(directory, 'lock.release')
	with_wipe_test_caches(directory, fn [directory, release_path] (cache_v_dir string) ! {
		me := u32(os.getuid())
		opts := parse_wipe_cache_options([], ['--tools'])!
		// A build holds a lock in it.
		busy_aside := os.join_path(cache_v_dir, 'tools.untrusted-1700000000-51')
		tool_cache_test_root(busy_aside)!
		busy_stuck := toolcache_test_unopenable_lock(busy_aside, 'vtest', me, os.join_path(directory,
			'second-busy'))!
		busy_lock := tool_cache_test_lock(busy_aside, 'vfmt')
		os.write_file(busy_lock, '')!
		mut holder := start_tool_cache_lock_holder(busy_lock, release_path)!
		mut busy_run := new_tool_cache_wipe_run()
		mut busy_lines := []string{}
		mut failure := ''
		busy_run.wipe_tool_cache_directory(busy_aside, opts, mut busy_lines) or { failure = err.msg() }
		os.write_file(release_path, '')!
		holder.wait()
		holder.close()
		assert failure == '', failure
		assert os.is_file(busy_stuck), busy_lines.join('\n')
		// Another user's lock file is in it.
		foreign_aside := os.join_path(cache_v_dir, 'tools.untrusted-1700000000-52')
		tool_cache_test_root(foreign_aside)!
		foreign_stuck := toolcache_test_unopenable_lock(foreign_aside, 'vtest', me, os.join_path(directory,
			'second-foreign'))!
		os.write_file(os.join_path(foreign_aside, tool_cache_lock_name('vfmt', me + 1)), '')!
		mut foreign_run := new_tool_cache_wipe_run()
		mut foreign_lines := []string{}
		foreign_run.wipe_tool_cache_directory(foreign_aside, opts, mut foreign_lines)!
		assert os.is_file(foreign_stuck), foreign_lines.join('\n')
		// Another account owns a file in it: the wipe acts as another user, to whom the folder and
		// what is in it belong to somebody else.
		other_aside := os.join_path(cache_v_dir, 'tools.untrusted-1700000000-53')
		tool_cache_test_root(other_aside)!
		other_stuck := toolcache_test_unopenable_lock(other_aside, 'vtest', me + 1, os.join_path(directory,
			'second-other'))!
		os.write_file(os.join_path(other_aside, 'plain'), 'x')!
		mut other_run := new_tool_cache_wipe_run()
		other_run.user = i64(me) + 1
		mut other_lines := []string{}
		other_run.wipe_tool_cache_directory(other_aside, opts, mut other_lines)!
		assert os.is_file(other_stuck), other_lines.join('\n')
	})!
}

// A folder V moved aside that the wipe empties but cannot remove fails the wipe: it should go.
fn test_an_emptied_moved_aside_folder_that_cannot_be_removed_fails_the_wipe() ! {
	$if windows {
		return
	}
	if os.geteuid() == 0 {
		// The superuser removes from a folder whatever its mode is.
		return
	}
	directory := toolcache_test_dir('wipe_cache_aside_not_removed')
	defer {
		toolcache_test_unlock_tree(directory)
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		holder := os.join_path(directory, 'holder')
		os.mkdir(holder, mode: 0o700)!
		// Beside the tool cache folder that `VTOOLS_CACHE_DIR` names, so the wipe knows it.
		os.setenv(tool_cache_dir_env, os.join_path(holder, 'tools'), true)
		aside := os.join_path(holder, 'tools.untrusted-1700000000-61')
		tool_cache_test_root(aside)!
		os.write_file(tool_cache_test_lock(aside, 'vfmt'), '')!
		os.chmod(holder, 0o555)!
		message := wipe_failure('--tools')
		os.chmod(holder, 0o700)!
		assert message.starts_with('v wipe-cache: could not remove Moved-aside V tool cache folder ${aside}: '), message
		assert os.is_dir(aside), message
	})!
}

// A tool cache folder that two settings name, in two spellings, is gone through once.
fn test_a_tool_cache_folder_named_twice_is_gone_through_once() ! {
	$if windows {
		// A link to a folder needs rights a test may lack.
		return
	}
	directory := toolcache_test_dir('wipe_cache_named_twice')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (cache_v_dir string) ! {
		// The default folder, which is not there, also named through a link to the folder above it.
		tools := os.join_path(cache_v_dir, 'tools')
		os.mkdir_all(cache_v_dir)!
		alias := os.join_path(directory, 'alias')
		os.symlink(cache_v_dir, alias)!
		os.setenv(tool_cache_dir_env, os.join_path(alias, 'tools'), true)
		output := wipe_output('--tools', '--verbose')!
		named := output.count('V tool cache folder ${os.join_path(alias, 'tools')} does not exist, skipping.')
		default := output.count('V tool cache folder ${tools} does not exist, skipping.')
		assert named + default == 1, output
	})!
}

// A `V3CACHE` that names `VTMP`, in whatever spelling, has the V3 pass remove only the V3 cache
// folders in `VTMP`, as when `V3CACHE` is not set.
fn test_a_v3cache_that_names_vtmp_leaves_the_rest_of_vtmp() ! {
	directory := toolcache_test_dir('wipe_cache_v3cache_is_vtmp')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		other := os.join_path(vtmp, 'a.tmp.c')
		v3 := os.join_path(vtmp, 'v3_thirdparty_objs')
		// Pairs of a `VTMP` and a `V3CACHE` that name the same folder.
		mut pairs := [[vtmp, vtmp], [vtmp, vtmp + os.path_separator]]
		$if !windows {
			// Through a link to the folder above it, on either side.
			alias := os.join_path(directory, 'alias')
			os.symlink(directory, alias)!
			pairs << [vtmp, os.join_path(alias, 'vtmp')]
			pairs << [os.join_path(alias, 'vtmp'), vtmp]
		}
		for pair in pairs {
			os.write_file(other, 'x')!
			os.mkdir_all(v3)!
			os.setenv('VTMP', pair[0], true)
			os.setenv('V3CACHE', pair[1], true)
			output := wipe_output('--v3cache')!
			assert os.is_file(other), '${pair}: ${output}'
			assert !os.exists(v3), '${pair}: ${output}'
		}
	})!
}

// A default tool cache folder is known by where it lies when `VTMP` is named through a link and
// `..`, which the system takes through the link: it stays when the wipe empties it, as every
// folder builds use does.
fn test_a_default_tool_cache_folder_in_a_vtmp_named_through_a_link_and_dot_dot_stays() ! {
	$if windows {
		// The system takes `..` as text there.
		return
	}
	directory := toolcache_test_dir('wipe_cache_vtmp_dot_dot')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		os.mkdir_all(os.join_path(directory, 'a', 'b'))!
		real_vtmp := os.join_path(directory, 'a', 'vt')
		os.mkdir(real_vtmp, mode: 0o700)!
		link := os.join_path(directory, 'ln')
		os.symlink(os.join_path(directory, 'a', 'b'), link)!
		os.setenv('VTMP', '${link}/../vt', true)
		tools := os.join_path(real_vtmp, 'tools')
		tool_cache_test_root(tools)!
		plain := os.join_path(tools, 'plain')
		os.write_file(plain, 'x')!
		output := wipe_output('--tools')!
		assert !os.exists(plain), output
		assert os.is_dir(tools), output
	})!
}

// The wipe holds the build lock of each tool whose entries it removes until it is done with the
// folder, and then lets go of every one of them, not only of one.
fn test_the_wipe_lets_go_of_every_build_lock_it_took() ! {
	directory := toolcache_test_dir('wipe_cache_releases_every_lock')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		root := os.join_path(directory, 'tools')
		tool_cache_test_root(root)!
		tools := ['vdoc', 'vfmt', 'vvet']
		for i, tool in tools {
			tool_cache_test_entry(root, tool, 'abc'[i..i + 1])!
		}
		mut run := new_tool_cache_wipe_run()
		mut lines := []string{}
		run.wipe_tool_cache_directory(root, parse_wipe_cache_options([], ['--tools'])!, mut lines)!
		for tool in tools {
			lock_path := tool_cache_test_lock(root, tool)
			assert os.is_file(lock_path), lines.join('\n')
			assert toolcache_test_lock_state(lock_path) == 'free', 'the build lock of ${tool} is still held'
		}
	})!
}

// A folder V moved aside goes once its entries are gone, also when the wipe had to make the lock
// files of several of its tools first: each of them goes with it.
fn test_a_moved_aside_folder_whose_lock_files_the_wipe_made_goes() ! {
	directory := toolcache_test_dir('wipe_cache_moved_aside_made_locks')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		moved := os.join_path(directory, 'tools.untrusted-1700000000-9')
		tool_cache_test_root(moved)!
		for i, tool in ['vdoc', 'vfmt', 'vvet'] {
			tool_cache_test_entry(moved, tool, 'abc'[i..i + 1])!
		}
		mut run := new_tool_cache_wipe_run()
		mut lines := []string{}
		run.wipe_tool_cache_directory(moved, parse_wipe_cache_options([], ['--tools'])!, mut lines)!
		left := os.ls(moved) or { []string{} }
		assert !os.exists(moved), lines.join('\n') + '\nleft: ' + left.join(', ')
	})!
}

// Without a V3CACHE of its own, `--v3cache` wipes every V3 cache folder in VTMP, wherever it is in
// the listing, and nothing else.
fn test_a_v3cache_wipe_goes_through_every_v3_cache_folder_in_vtmp() ! {
	directory := toolcache_test_dir('wipe_cache_v3_folders')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn (_ string) ! {
		os.setenv('V3CACHE', '', true)
		vtmp := os.vtmp_dir()
		os.mkdir_all(vtmp)!
		mut caches := []string{}
		for name in ['v3_module_cache_aa', 'v3_test_cache_bb'] {
			cache := os.join_path(vtmp, name)
			os.mkdir_all(cache)!
			os.write_file(os.join_path(cache, 'unit.o'), 'x')!
			caches << cache
		}
		first := os.join_path(vtmp, 'a-keep.txt')
		last := os.join_path(vtmp, 'zz-keep.txt')
		os.write_file(first, 'x')!
		os.write_file(last, 'x')!
		output := wipe_output('--v3cache')!
		for cache in caches {
			assert !os.exists(cache), output
		}
		assert os.is_file(first), output
		assert os.is_file(last), output
	})!
}

// With --verbose, the wipe of a cache folder lists every name it removes, and a dry run every name
// it would remove.
fn test_a_verbose_wipe_lists_every_name_in_a_cache_folder() ! {
	directory := toolcache_test_dir('wipe_cache_verbose_names')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn (_ string) ! {
		vcache := os.getenv('VCACHE')
		os.mkdir_all(vcache)!
		names := ['a.o', 'b.o', 'c.o']
		for name in names {
			os.write_file(os.join_path(vcache, name), 'x')!
		}
		dry := wipe_output('--vcache', '--verbose', '--dry-run')!
		for name in names {
			assert dry.contains('  would remove ${os.join_path(vcache, name)}'), dry
		}
		real := wipe_output('--vcache', '--verbose')!
		for name in names {
			assert real.contains('  removing ${os.join_path(vcache, name)}'), real
		}
	})!
}

// The default tool cache folder is wiped also when `VCACHE` is a link to V's own cache folder and
// `VTOOLS_CACHE_DIR` names the tool cache folder through that link: the pass of `VCACHE` removes the
// link, and the tools pass does not lose the folder for that. A dry run says the same.
fn test_a_tool_cache_folder_named_through_a_cache_folder_link_is_wiped() ! {
	$if windows {
		// Making a symbolic link takes a privilege there.
		return
	}
	directory := toolcache_test_dir('wipe_cache_tools_through_vcache_link')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (cache string) ! {
		tools := os.join_path(cache, 'tools')
		tool_cache_test_root(tools)!
		entry := tool_cache_test_entry(tools, 'vfmt', 'e')!
		link := os.join_path(directory, 'vmodules_cache')
		os.symlink(cache, link)!
		os.setenv('VCACHE', link, true)
		named := os.join_path(link, 'tools')
		os.setenv(tool_cache_dir_env, named, true)
		dry := wipe_output('--dry-run')!
		assert dry.contains('Would wipe V tool cache folder ${named}.'), dry
		output := wipe_output()!
		assert !os.exists(entry), output
		assert output.contains('V tool cache folder ${named} was wiped.'), output
		// The pass of `VCACHE` made a folder where the link was; the link goes back. The tools pass goes
		// through each place once, however many spellings name it.
		os.rmdir_all(link)!
		os.symlink(cache, link)!
		os.rmdir_all(tools)!
		verbose := wipe_output('--tools', '--verbose')!
		assert verbose.contains('V tool cache folder ${named} does not exist, skipping.'), verbose
		assert !verbose.contains('V tool cache folder ${tools} does not exist'), verbose
		// Another wipe at the same time removes the link `VCACHE` is, and makes the folder again, as
		// this tools pass comes to the folder named through it: the default spelling of that folder
		// still has its turn.
		tool_cache_test_root(tools)!
		later := tool_cache_test_entry(tools, 'vfmt', 'f')!
		mut run := new_tool_cache_wipe_run()
		run.before_acting = fn [link, named] (path string, step string) {
			if path == named && step == 'open' && os.is_link(link) {
				os.rm(link) or { panic(err) }
				os.mkdir(link) or { panic(err) }
			}
		}
		mut lines := []string{}
		run.wipe_tool_places(parse_wipe_cache_options([], ['--tools'])!, mut lines)!
		assert !os.exists(later), lines.join('\n')
	})!
}

// A cache folder that a build makes while the pass of that folder removes what it found there is
// left as it is for the rest of the wipe: the tools pass does not wipe the tool cache folder in it
// afterwards, and the pass of a cache folder that holds it does not empty it.
fn test_a_cache_folder_made_anew_during_its_pass_is_left_for_the_rest_of_the_wipe() ! {
	directory := toolcache_test_dir('wipe_cache_made_anew_left')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		for nested in [false, true] {
			vcache := if nested {
				os.join_path(vtmp, 'vcache')
			} else {
				os.join_path(directory, 'vcache_new')
			}
			tools := os.join_path(vcache, 'tools')
			// `V3CACHE` lies in it, and its pass comes after the pass of `VCACHE`.
			v3cache := os.join_path(vcache, 'v3')
			os.setenv('VCACHE', vcache, true)
			os.setenv('V3CACHE', v3cache, true)
			os.setenv(tool_cache_dir_env, tools, true)
			marker := os.join_path(vcache, 'made_by_a_build')
			v3_marker := os.join_path(v3cache, 'made_by_a_build_too')
			// What lies elsewhere in `VTMP` goes as ever.
			junk := os.join_path(vtmp, 'junk')
			os.write_file(junk, 'x')!
			mut run := new_tool_cache_wipe_run()
			// A build makes the folder, with its work in it, as the pass of `VCACHE` removes what it
			// found there, which is nothing.
			run.before_acting = fn [vcache, tools, marker, v3cache, v3_marker] (path string, step string) {
				if path != vcache || step != 'remove' || os.exists(vcache) {
					return
				}
				tool_cache_test_root(tools) or { panic(err) }
				tool_cache_test_entry(tools, 'vfmt', 'd') or { panic(err) }
				os.write_file(marker, 'x') or { panic(err) }
				os.mkdir_all(v3cache) or { panic(err) }
				os.write_file(v3_marker, 'x') or { panic(err) }
			}
			lines := run.wipe_passes(parse_wipe_cache_options([], [])!)!
			output := 'nested ${nested}:\n' + lines.join('\n')
			assert output.contains('V cache folder ${vcache} was made anew during the wipe; the new folder was not touched.'), output
			assert os.is_dir(os.join_path(tools, 'vfmt-${'d'.repeat(64)}.${os.getuid()}')), output
			assert !output.contains('V tool cache folder ${tools} was wiped'), output
			assert os.is_file(marker), output
			assert os.is_file(v3_marker), output
			assert output.contains('V3 cache folder ${v3cache} was left as it is: it lies in a folder that a build made anew during the wipe, which the wipe leaves to it.'), output
			assert !os.exists(junk), output
			os.rmdir_all(vcache)!
		}
	})!
}

// On a file system that ignores case, a setting that spells a folder above a tool cache folder in
// another case than the disk names the same folder: the pass of `VTMP` keeps the tool cache folder
// in it, lock file included, whether `VTMP` or `VTOOLS_CACHE_DIR` is spelled that way. Where the
// file system tells case apart, the names are other folders and there is nothing to test.
fn test_a_setting_that_spells_a_folder_in_another_case_names_the_same_folder() ! {
	$if windows {
		// Windows spells a real path as the disk lists it (see `os.real_path`).
		return
	}
	directory := toolcache_test_dir('wipe_cache_other_case')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		os.mkdir_all(os.join_path(directory, 'case_probe'))!
		if !os.exists(os.join_path(directory, 'CASE_PROBE')) {
			return
		}
		vtmp := os.join_path(directory, 'vtmp')
		upper := os.join_path(directory, 'VTMP')
		assert tool_cache_location(os.join_path(upper, 'tools')) == tool_cache_location(os.join_path(vtmp,
			'tools'))
		for how in ['vtmp', 'setting'] {
			tools := os.join_path(vtmp, if how == 'vtmp' { 'tools' } else { 'tools2' })
			tool_cache_test_root(tools)!
			lock_file := tool_cache_test_lock(tools, 'vfmt')
			os.write_file(lock_file, '')!
			tool_cache_test_entry(tools, 'vfmt', 'a')!
			if how == 'vtmp' {
				os.setenv('VTMP', upper, true)
				os.unsetenv(tool_cache_dir_env)
			} else {
				os.setenv('VTMP', vtmp, true)
				os.setenv(tool_cache_dir_env, os.join_path(upper, 'tools2'), true)
			}
			output := wipe_output('--vtmp')!
			assert os.is_file(lock_file), '${how}: ${output}'
		}
	})!
}

// When `VTMP` is a link, the V3 cache folders behind it are judged by who owns them as anywhere
// else: what another account owns in a folder others can write to stays. The wipe acts as another
// account here, since a test cannot make files of one.
fn test_v3_cache_folders_behind_a_vtmp_link_are_left_to_their_owner() ! {
	$if windows {
		// Nothing counts as another account's there.
		return
	}
	directory := toolcache_test_dir('wipe_cache_v3_behind_link')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		shared := os.join_path(directory, 'shared')
		os.mkdir_all(shared)!
		os.chmod(shared, 0o777)!
		v3 := os.join_path(shared, 'v3_module_cache_ab')
		os.mkdir_all(v3)!
		os.write_file(os.join_path(v3, 'owned_by_the_other_account'), 'x')!
		link := os.join_path(directory, 'vtmp_link')
		os.symlink(shared, link)!
		mut run := new_tool_cache_wipe_run()
		run.user = i64(os.getuid()) + 1
		mut lines := []string{}
		run.wipe_v3_folders_in(link, parse_wipe_cache_options([], ['--v3cache'])!, mut lines)!
		output := lines.join('\n')
		assert os.is_file(os.join_path(v3, 'owned_by_the_other_account')), output
		assert output.contains('V3 cache folder ${os.join_path(link, 'v3_module_cache_ab')} was left as it is: it belongs to another account, in a folder others can write to.'), output
	})!
}

// Every V3 cache folder in `VTMP` is attempted: one that cannot be removed fails the wipe, and the
// ones after it are wiped all the same.
fn test_a_v3_cache_folder_that_cannot_be_removed_does_not_stop_the_others() ! {
	$if windows {
		// A folder's mode does not keep its owner from removing what is in it there.
		return
	}
	if os.getuid() == 0 {
		// The superuser removes what is in a folder whatever its mode.
		return
	}
	directory := toolcache_test_dir('wipe_cache_v3_all_attempted')
	stuck := os.join_path(directory, 'vtmp', 'v3_module_cache_aa', 'sub')
	defer {
		os.chmod(stuck, 0o700) or {}
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory, stuck] (_ string) ! {
		vtmp := os.join_path(directory, 'vtmp')
		os.mkdir_all(stuck)!
		os.write_file(os.join_path(stuck, 'kept'), 'x')!
		os.chmod(stuck, 0o500)!
		later := os.join_path(vtmp, 'v3_module_cache_bb')
		os.mkdir_all(later)!
		os.write_file(os.join_path(later, 'goes'), 'x')!
		mut run := new_tool_cache_wipe_run()
		mut lines := []string{}
		mut failure := ''
		run.wipe_v3_folders_in(vtmp, parse_wipe_cache_options([], ['--v3cache'])!, mut lines) or {
			failure = err.msg()
		}
		output := '${failure}\n' + lines.join('\n')
		assert failure.contains('v3_module_cache_aa'), output
		assert !os.exists(later), output
	})!
}

// On Windows a setting that names a drive but no folder on it, such as `C:cache`, is taken as Windows
// takes it: from the folder that is current on that drive.
fn test_a_drive_relative_setting_is_taken_as_windows_takes_it() {
	$if !windows {
		return
	}
	wd := os.getwd()
	name := 'v_drive_relative_setting'
	expected := tool_cache_wipe_place(os.join_path(wd, name))
	assert tool_cache_wipe_place(tool_cache_wipe_absolute('${wd[..2]}${name}')) == expected
	// The list of folders the tools pass goes through takes the setting as it is written.
	assert tool_cache_wipe_place('${wd[..2]}${name}') == expected
	// `\cache` lies at the root of the current drive.
	rooted := '${wd[..2]}\\${name}'
	assert tool_cache_wipe_place(tool_cache_wipe_absolute('\\${name}')) == rooted
	assert tool_cache_wipe_place('\\${name}') == rooted
}

// On Windows the short name of a folder whose long name ends in a dot, which WSL makes, is kept as
// it is written: no path reaches the folder by its long name, which reaches the folder without the
// dot instead. So `--vtmp` wipes the folder the short name reaches, and leaves the other alone.
fn test_a_short_name_whose_long_name_no_path_reaches_is_kept() ! {
	$if !windows {
		return
	}
	directory := toolcache_test_dir('wipe_cache_short_dotted')
	area := os.join_path(directory, 'area')
	dotted := '\\\\?\\' + os.join_path(area, 'dotted.')
	defer {
		os.rm(dotted + '\\tmp_file.c') or {}
		os.rmdir(dotted) or {}
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [area, dotted] (_ string) ! {
		os.mkdir_all(os.join_path(area, 'dotted'))!
		precious := os.join_path(area, 'dotted', 'precious.txt')
		os.write_file(precious, 'x')!
		// `os.mkdir` takes the path as Windows takes it, which drops the dot; `cmd` does not.
		made := os.exec(['cmd', '/c', 'mkdir', dotted])
		assert made.exit_code == 0, made.output
		os.write_file(dotted + '\\tmp_file.c', 'x')!
		short := os.join_path(area, 'DOTTED~1')
		if !os.is_file(os.join_path(short, 'tmp_file.c')) {
			// This volume makes no short names, or gave the folder another one.
			return
		}
		os.setenv('VTMP', short, true)
		output := wipe_output('--vtmp')!
		assert os.is_file(precious), output
		assert !os.exists(dotted + '\\tmp_file.c'), output
	})!
}

// On Windows a lock file that setting its mode cannot make usable, such as one that another program
// holds open without letting others write to it, fails a dry run as it fails the real run. One that
// is only read-only does not: the real run clears that, and goes on.
fn test_a_dry_run_fails_on_a_lock_file_that_setting_its_mode_cannot_make_usable() ! {
	$if !windows {
		return
	}
	directory := toolcache_test_dir('wipe_cache_lock_held_open')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		root := os.join_path(directory, 'vtmp', 'tools')
		tool_cache_test_root(root)!
		entry := tool_cache_test_entry(root, 'vfmt', 'c')!
		lock_file := tool_cache_test_lock(root, 'vfmt')
		os.write_file(lock_file, '')!
		mut handle := voidptr(0)
		$if windows {
			handle = tool_cache_test_open_for_reading(lock_file)
		}
		mut dry_failure := ''
		mut failure := ''
		wipe_output('--tools', '--dry-run') or { dry_failure = err.msg() }
		wipe_output('--tools') or { failure = err.msg() }
		$if windows {
			tool_cache_test_close(handle)
		}
		assert failure.contains(lock_file), failure
		assert dry_failure.contains(lock_file), dry_failure
		assert os.is_dir(entry)
		os.chmod(lock_file, 0o400)!
		wipe_output('--tools', '--dry-run')!
		wipe_output('--tools')!
		assert !os.exists(entry)
	})!
}

// Something that is no folder and appears at a cache folder's place after the wipe found nothing
// there, but before it removes what it found, goes with that removal: the folder is made again,
// and the wipe says it was wiped.
fn test_a_file_that_appears_at_a_cache_folder_before_its_removal_goes_with_it() ! {
	directory := toolcache_test_dir('wipe_cache_file_before_removal')
	defer {
		os.rmdir_all(directory) or {}
	}
	with_wipe_test_caches(directory, fn [directory] (_ string) ! {
		vcache := os.join_path(directory, 'vcache_missing')
		os.setenv('VCACHE', vcache, true)
		os.setenv(tool_cache_dir_env, os.join_path(vcache, 'tools'), true)
		mut run := new_tool_cache_wipe_run()
		run.before_acting = fn [vcache] (path string, step string) {
			if path == vcache && step == 'remove' && !os.exists(vcache) {
				os.write_file(vcache, 'x') or { panic(err) }
			}
		}
		lines := run.wipe_passes(parse_wipe_cache_options([], ['--vcache'])!)!
		output := lines.join('\n')
		assert output.contains('V cache folder ${vcache} was wiped.'), output
		assert os.is_dir(vcache), output
	})!
}
