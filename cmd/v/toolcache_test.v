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
	binary := os.join_path(directory, 'vdemo-${key}')
	entry := ToolCacheEntry{
		name:     'vdemo'
		vroot:    directory
		binary:   binary
		manifest: binary + '.inputs'
	}
	os.write_file(binary, 'current') or { panic(err) }
	os.write_file(binary + '.inputs', 'manifest') or { panic(err) }
	displaced := '${binary}${tool_cache_replaced_marker}4242'
	os.write_file(displaced, 'previous') or { panic(err) }
	stale := os.join_path(directory, 'vdemo-' + 'b'.repeat(64))
	os.write_file(stale, 'older build') or { panic(err) }

	prune_stale_tool_binaries(entry)

	assert os.exists(binary), 'the current binary must be kept'
	assert os.exists(binary + '.inputs'), 'the current manifest must be kept'
	assert !os.exists(displaced), 'a binary replaced while in use must be collected'
	assert !os.exists(stale), 'a previous build must be collected'
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
