module driver

import os
import crypto.sha256
import time

fn age_unchanged_build_source(path string) ! {
	// Recent whole-second mtimes cannot safely identify an unchanged source.
	timestamp := time.utc().unix() - 10
	os.utime(path, timestamp, timestamp)!
}

struct ReceiptFixture {
	root        string
	source_dir  string
	cache_dir   string
	source      string
	header      string
	executable  string
	destination string
}

fn receipt_fixture(name string) !ReceiptFixture {
	root := os.join_path(os.vtmp_dir(), 'v3_unchanged_build_${name}_${os.getpid()}')
	source_dir := os.join_path(root, 'source')
	fixture := ReceiptFixture{
		root:        root
		source_dir:  source_dir
		cache_dir:   os.join_path(root, 'cache')
		source:      os.join_path(source_dir, 'main.v')
		header:      os.join_path(source_dir, 'native.h')
		executable:  os.join_path(root, 'app')
		destination: os.join_path(root, 'restored')
	}
	os.mkdir_all(source_dir)!
	os.write_file(fixture.source, 'fn main() {}\n')!
	os.write_file(fixture.header, 'static inline int native_value(void) { return 1; }\n')!
	age_unchanged_build_source(fixture.source)!
	age_unchanged_build_source(fixture.header)!
	age_unchanged_build_source(source_dir)!
	os.write_file(fixture.executable, 'executable bytes')!
	return fixture
}

fn (f ReceiptFixture) receipt() UnchangedBuildReceipt {
	return UnchangedBuildReceipt{
		sources: [f.source]
		files:   [f.header]
		dirs:    [f.source_dir]
	}
}

fn test_file_sha256_streams_large_inputs() {
	root := os.join_path(os.vtmp_dir(), 'v3_file_sha256_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	os.mkdir_all(root)!
	path := os.join_path(root, 'large.bin')
	content := []u8{len: 256 * 1024, init: u8(`v`)}
	os.write_file_array(path, content)!
	digest := v3_file_sha256_hex(path)?
	assert digest == sha256.sum(content).hex()
}

fn test_unchanged_build_cache_restores_and_invalidates_source_change() {
	f := receipt_fixture('source')!
	defer { os.rmdir_all(f.root) or {} }
	assert v3_publish_unchanged_build(f.cache_dir, 'test', f.executable, v3_unchanged_build_stamp(f.receipt(), f.cache_dir, ''))
	assert v3_restore_unchanged_build(f.cache_dir, 'test', f.destination, '')
	assert os.read_file(f.destination)! == 'executable bytes'
	// A new timestamp with the same content keeps the receipt.
	os.utime(f.source, time.utc().unix() - 5, time.utc().unix() - 5)!
	assert v3_restore_unchanged_build(f.cache_dir, 'test', f.destination, '')
	os.write_file(f.source, 'fn main() { println("changed") }\n')!
	assert !v3_restore_unchanged_build(f.cache_dir, 'test', f.destination, '')
}

fn test_unchanged_build_cache_invalidates_native_header_change() {
	f := receipt_fixture('header')!
	defer { os.rmdir_all(f.root) or {} }
	assert v3_publish_unchanged_build(f.cache_dir, 'test', f.executable, v3_unchanged_build_stamp(f.receipt(), f.cache_dir, ''))
	assert v3_restore_unchanged_build(f.cache_dir, 'test', f.destination, '')
	os.write_file(f.header, 'static inline int native_value(void) { return 2; }\n')!
	assert !v3_restore_unchanged_build(f.cache_dir, 'test', f.destination, '')
}

fn test_unchanged_build_cache_invalidates_new_source_file() {
	f := receipt_fixture('new_file')!
	defer { os.rmdir_all(f.root) or {} }
	assert v3_publish_unchanged_build(f.cache_dir, 'test', f.executable, v3_unchanged_build_stamp(f.receipt(), f.cache_dir, ''))
	os.write_file(os.join_path(f.source_dir, 'added.v'), 'fn added() {}\n')!
	assert !v3_restore_unchanged_build(f.cache_dir, 'test', f.destination, '')
}

fn test_unchanged_build_cache_checks_build_time_values() {
	f := receipt_fixture('build_time')!
	defer { os.rmdir_all(f.root) or {} }
	os.write_file(f.source, 'fn main() { println(@BUILD_DATE) }\n')!
	age_unchanged_build_source(f.source)!
	assert v3_publish_unchanged_build(f.cache_dir, 'test', f.executable, v3_unchanged_build_stamp(f.receipt(), f.cache_dir, '2026-11-08 1'))
	assert v3_restore_unchanged_build(f.cache_dir, 'test', f.destination, '2026-11-08 1')
	assert !v3_restore_unchanged_build(f.cache_dir, 'test', f.destination, '2026-11-09 2')
}

fn test_unchanged_build_cache_rejects_a_changed_cached_executable() {
	f := receipt_fixture('executable')!
	defer { os.rmdir_all(f.root) or {} }
	assert v3_publish_unchanged_build(f.cache_dir, 'test', f.executable, v3_unchanged_build_stamp(f.receipt(), f.cache_dir, ''))
	os.write_file(os.join_path(f.cache_dir, 'test.exe'), 'other bytes')!
	assert !v3_restore_unchanged_build(f.cache_dir, 'test', f.destination, '')
}

fn test_unchanged_build_cache_is_on_unless_the_user_turns_it_off() {
	old_disable := os.getenv('V3_DISABLE_UNCHANGED_BUILD_CACHE')
	defer {
		os.setenv('V3_DISABLE_UNCHANGED_BUILD_CACHE', old_disable, true)
	}
	os.unsetenv('V3_DISABLE_UNCHANGED_BUILD_CACHE')
	assert v3_unchanged_build_cache_opted_in(['-o', 'app', '.'], false)
	assert !v3_unchanged_build_cache_opted_in(['-nocache', '-o', 'app', '.'], false)
	// The compiler's own restart without the module cache keeps the receipt.
	assert v3_unchanged_build_cache_opted_in(['-nocache', '-o', 'app', '.'], true)
	os.setenv('V3_DISABLE_UNCHANGED_BUILD_CACHE', '1', true)
	assert !v3_unchanged_build_cache_opted_in(['-o', 'app', '.'], false)
}

fn test_parallel_monomorphize_requires_explicit_opt_in() {
	old := os.getenv_opt('V3_PARALLEL_MONOMORPHIZE')
	defer {
		if previous := old {
			os.setenv('V3_PARALLEL_MONOMORPHIZE', previous, true)
		} else {
			os.unsetenv('V3_PARALLEL_MONOMORPHIZE')
		}
	}
	os.unsetenv('V3_PARALLEL_MONOMORPHIZE')
	assert !should_parallel_monomorphize()
	os.setenv('V3_PARALLEL_MONOMORPHIZE', '0', true)
	assert !should_parallel_monomorphize()
	os.setenv('V3_PARALLEL_MONOMORPHIZE', '1', true)
	$if tinyc {
		$if linux && amd64 {
			assert should_parallel_monomorphize()
		} $else {
			assert !should_parallel_monomorphize()
		}
	} $else {
		assert should_parallel_monomorphize()
	}
}

fn test_default_build_rechecks_compile_time_environment() {
	root := os.join_path(os.vtmp_dir(), 'v3_receipt_env_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	os.mkdir_all(root)!
	source := os.join_path(root, 'main.v')
	executable := os.join_path(root, 'app')
	os.write_file(source, "fn main() { println(\$env('V3_RECEIPT_ENV_REGRESSION')) }\n")!
	for expected in ['first', 'second'] {
		mut environment := os.environ()
		environment.delete('VFLAGS')
		environment['V3CACHE'] = os.join_path(root, 'cache')
		environment['V3_RECEIPT_ENV_REGRESSION'] = expected
		environment['V_MACOS_V3_NO_FALLBACK'] = '1'
		mut compiler := os.new_process(@VEXE)
		compiler.set_environment(environment)
		compiler.set_args(['-cc', 'clang', '-o', executable, source])
		compiler.set_redirect_stdio_merged()
		compiler.run()
		output := compiler.stdout_slurp()
		compiler.wait()
		code := compiler.code
		compiler.close()
		assert code == 0, output
		mut program := os.new_process(executable)
		program.set_redirect_stdio()
		program.run()
		result := program.stdout_slurp()
		program.wait()
		program_code := program.code
		program.close()
		assert program_code == 0
		assert result.trim_space() == expected
	}
}

fn test_unchanged_build_cache_does_not_vouch_for_a_file_changed_during_the_build() {
	f := receipt_fixture('changed_during_build')!
	defer { os.rmdir_all(f.root) or {} }
	// Written after the build started: the build may have read the old text.
	time.sleep(20 * time.millisecond)
	os.write_file(f.header, 'static inline int native_value(void) { return 3; }\n')!
	assert !v3_unchanged_build_stamp(f.receipt(), f.cache_dir, '').ok
}

fn test_unchanged_build_entry_key_belongs_to_this_process() {
	old := os.getenv(v3_internal_receipt_entry_env)
	defer {
		os.setenv(v3_internal_receipt_entry_env, old, true)
	}
	os.unsetenv(v3_internal_receipt_entry_env)
	v3_note_unchanged_build_entry_key('first')
	// The key of the build the user started stays; a restart notes nothing new.
	v3_note_unchanged_build_entry_key('second')
	assert v3_unchanged_build_entry_key() == 'first'
	// A build of another program started by this one has another pid.
	os.setenv(v3_internal_receipt_entry_env, '${os.getpid() + 1}:other', true)
	assert v3_unchanged_build_entry_key() == ''
}

fn aged_write(path string, content string) ! {
	os.mkdir_all(os.dir(path))!
	os.write_file(path, content)!
	age_unchanged_build_source(path)!
}

// native_receipt_inputs is a receipt of a build of the fixture with `flags`.
fn native_receipt_inputs(f ReceiptFixture, flags []string, probe string) UnchangedBuildInputs {
	return UnchangedBuildInputs{
		sources:       [f.source]
		dirs:          [f.source_dir]
		output_dir:    f.root
		flags:         flags
		probe:         probe
		language:      'c'
		compiler:      'cc'
		cache_dir:     f.cache_dir
		target_os:     'linux'
		vroot:         os.join_path(f.root, 'no_vroot')
		project_roots: [f.root]
	}
}

// publish_native_receipt records a receipt prepared from `inputs` under `key`.
fn publish_native_receipt(f ReceiptFixture, key string, inputs UnchangedBuildInputs) {
	age_unchanged_build_source(f.source_dir) or { panic(err) }
	stamp := v3_unchanged_build_prepare(inputs)
	assert stamp.ok
	assert v3_publish_unchanged_build(f.cache_dir, key, f.executable, stamp)
	assert v3_restore_unchanged_build(f.cache_dir, key, f.destination, '')
}

fn test_receipt_covers_the_headers_of_external_c_sources() {
	f := receipt_fixture('external_c')!
	defer { os.rmdir_all(f.root) or {} }
	source := os.join_path(f.source_dir, 'native_unit.c')
	header := os.join_path(f.source_dir, 'unit_only.h')
	aged_write(header, '#define NATIVE_VALUE 1\n')!
	aged_write(source, '#include "unit_only.h"\nint native_value(void) { return NATIVE_VALUE; }\n')!
	publish_native_receipt(f, 'external', native_receipt_inputs(f, [source], ''))
	aged_write(header, '#define NATIVE_VALUE 2\n')!
	assert !v3_restore_unchanged_build(f.cache_dir, 'external', f.destination, '')
}

fn test_receipt_covers_the_source_of_an_object_it_compiles() {
	f := receipt_fixture('adjacent_c')!
	defer { os.rmdir_all(f.root) or {} }
	object := os.join_path(f.source_dir, 'built.o')
	source := os.join_path(f.source_dir, 'built.c')
	header := os.join_path(f.source_dir, 'built.h')
	aged_write(header, '#define NATIVE_VALUE 1\n')!
	aged_write(source, '#include "built.h"\nint native_value(void) { return NATIVE_VALUE; }\n')!
	publish_native_receipt(f, 'object', native_receipt_inputs(f, [object], ''))
	aged_write(header, '#define NATIVE_VALUE 2\n')!
	assert !v3_restore_unchanged_build(f.cache_dir, 'object', f.destination, '')
}

fn test_receipt_is_not_published_without_the_dependencies_of_a_c_source() {
	f := receipt_fixture('broken_c')!
	defer { os.rmdir_all(f.root) or {} }
	source := os.join_path(f.source_dir, 'broken.c')
	aged_write(source, '#include "does_not_exist.h"\n')!
	assert !v3_unchanged_build_prepare(native_receipt_inputs(f, [source], '')).ok
}

fn test_receipt_is_not_published_with_options_that_hide_inputs() {
	f := receipt_fixture('opaque')!
	defer { os.rmdir_all(f.root) or {} }
	assert !v3_unchanged_build_prepare(native_receipt_inputs(f, ['@response_file'], '')).ok
}

fn test_receipt_is_not_published_for_a_module_found_above_the_project() {
	f := receipt_fixture('above_project')!
	defer { os.rmdir_all(f.root) or {} }
	mut inputs := native_receipt_inputs(f, [], '')
	inputs = UnchangedBuildInputs{
		...inputs
		project_roots: [os.join_path(f.root, 'elsewhere')]
	}
	assert !v3_unchanged_build_prepare(inputs).ok
}

fn test_receipt_sees_a_header_added_earlier_in_the_search() {
	f := receipt_fixture('search_order')!
	defer { os.rmdir_all(f.root) or {} }
	earlier := os.join_path(f.root, 'earlier')
	later := os.join_path(f.root, 'later')
	os.mkdir_all(earlier)!
	aged_write(os.join_path(later, 'choice.h'), 'static int choice(void) { return 1; }\n')!
	age_unchanged_build_source(earlier)!
	publish_native_receipt(f, 'search', native_receipt_inputs(f, ['-I', earlier, '-I', later],
		'#include <choice.h>\n'))
	aged_write(os.join_path(earlier, 'choice.h'), 'static int choice(void) { return 2; }\n')!
	assert !v3_restore_unchanged_build(f.cache_dir, 'search', f.destination, '')
}

fn test_dependency_manifest_is_scanned_again_when_a_header_shadows_one_of_it() {
	root := os.join_path(os.vtmp_dir(), 'v3_dependency_shadow_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	earlier := os.join_path(root, 'earlier')
	later := os.join_path(root, 'later')
	os.mkdir_all(earlier)!
	aged_write(os.join_path(later, 'choice.h'), '#define CHOICE 1\n')!
	source := os.join_path(root, 'probe.c')
	aged_write(source, '#include <choice.h>\n')!
	args := ['-I', earlier, '-I', later, '-x', 'c', '-w']
	cache := os.join_path(root, 'cache')
	mut stats := CObjectCacheStats{}
	first := cached_c_object_dependencies(cache, 'cc', args, source, mut stats)
	assert !first.used_fallback
	assert os.join_path(earlier, 'choice.h') in first.missing
	assert stats.dependency_scans == 1
	again := cached_c_object_dependencies(cache, 'cc', args, source, mut stats)
	assert stats.dependency_scans == 1
	assert again.missing == first.missing
	aged_write(os.join_path(earlier, 'choice.h'), '#define CHOICE 2\n')!
	shadowed := cached_c_object_dependencies(cache, 'cc', args, source, mut stats)
	assert stats.dependency_scans == 2
	assert shadowed.files.any(it.ends_with('earlier/choice.h'))
	assert !shadowed.files.any(it.ends_with('later/choice.h'))
}

fn test_dependency_manifests_of_sources_in_other_directories_stay_apart() {
	root := os.join_path(os.vtmp_dir(), 'v3_dependency_dirs_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	mut sources := []string{}
	for name in ['one', 'two'] {
		aged_write(os.join_path(root, name, 'local.h'), '#define LOCAL 1\n')!
		source := os.join_path(root, name, 'unit.c')
		aged_write(source, '#include "local.h"\n')!
		sources << source
	}
	cache := os.join_path(root, 'cache')
	args := ['-x', 'c', '-w']
	mut stats := CObjectCacheStats{}
	assert !cached_c_object_dependencies(cache, 'cc', args, sources[0], mut stats).used_fallback
	second := cached_c_object_dependencies(cache, 'cc', args, sources[1], mut stats)
	assert second.files.any(it.ends_with('two/local.h'))
	assert !second.files.any(it.ends_with('one/local.h'))
}

fn test_c_object_manifest_is_invalid_when_a_file_appears_at_an_absent_path() {
	root := os.join_path(os.vtmp_dir(), 'v3_object_manifest_absent_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	header := os.join_path(root, 'header.h')
	object := os.join_path(root, 'cached.o')
	absent := os.join_path(root, 'earlier.h')
	aged_write(header, '#define A 1\n')!
	aged_write(object, 'object')!
	manifest := os.join_path(root, 'request.manifest')
	mut stats := CObjectCacheStats{}
	write_c_object_manifest(manifest, object, &CObjectDependencies{
		files:   [header]
		missing: [absent]
	}, mut stats)!
	assert valid_c_object_manifest(manifest, mut stats)? == object
	assert absent in stats.missing_candidates
	os.write_file(absent, '#define A 2\n')!
	if _ := valid_c_object_manifest(manifest, mut stats) {
		assert false, 'the manifest passed over a file at an absent path'
	}
}

fn test_missing_include_candidates_follow_the_order_of_the_search() {
	root := os.join_path(os.real_path(os.vtmp_dir()), 'v3_include_candidates_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	first := os.join_path(root, 'first')
	second := os.join_path(root, 'second')
	system := os.join_path(root, 'system')
	frameworks := os.join_path(root, 'frameworks')
	project := os.join_path(root, 'project')
	never := os.join_path(root, 'never')
	os.mkdir_all(first)!
	os.mkdir_all(os.join_path(frameworks, 'Kit.framework', 'Headers'))!
	aged_write(os.join_path(second, 'sub', 'found.h'), '')!
	aged_write(os.join_path(system, 'plain.h'), '')!
	aged_write(os.join_path(frameworks, 'Kit.framework', 'Headers', 'kit.h'), '')!
	aged_write(os.join_path(project, 'own.h'), '')!
	search := V3IncludeSearch{
		dirs:       [first, second, system, frameworks]
		frameworks: [frameworks]
		absent:     [never]
	}
	read := [os.join_path(second, 'sub', 'found.h'), os.join_path(system, 'plain.h'),
		os.join_path(frameworks, 'Kit.framework', 'Headers', 'kit.h'), os.join_path(project, 'own.h')]
	missing := v3_missing_include_candidates(search, [first, second], read, [], time.utc().unix())?
	assert never in missing
	// `sub` is not in `first`: that one path stands for all that would be below it.
	assert os.join_path(first, 'sub') in missing
	assert os.join_path(first, 'plain.h') in missing
	assert os.join_path(second, 'plain.h') in missing
	// The name of a file of a framework, in directories of plain headers.
	assert os.join_path(system, 'Kit') in missing
	// The directory of the program's own header is searched first for all of them.
	assert os.join_path(project, 'plain.h') in missing
	assert !missing.any(it.starts_with(frameworks))
}

fn test_missing_include_candidates_do_not_trust_a_file_newer_than_the_scan() {
	root := os.join_path(os.real_path(os.vtmp_dir()), 'v3_include_recent_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	first := os.join_path(root, 'first')
	second := os.join_path(root, 'second')
	aged_write(os.join_path(second, 'found.h'), '')!
	os.mkdir_all(first)!
	os.write_file(os.join_path(first, 'found.h'), '')!
	search := V3IncludeSearch{
		dirs: [first, second]
	}
	before := time.utc().unix() - 5
	if _ := v3_missing_include_candidates(search, [first, second], [os.join_path(second, 'found.h')],
		[], before)
	{
		assert false, 'a header that appeared during the scan was accepted'
	}
}

fn test_receipt_covers_every_form_of_a_library_however_it_is_linked() {
	root := os.join_path(os.vtmp_dir(), 'v3_receipt_libs_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	lib_dir := os.join_path(root, 'lib')
	earlier := os.join_path(root, 'earlier')
	framework_dir := os.join_path(root, 'frameworks', 'Native.framework')
	os.mkdir_all(earlier)!
	os.mkdir_all(framework_dir)!
	aged_write(os.join_path(lib_dir, 'libnative.a'), '!<arch>\n')!
	aged_write(os.join_path(lib_dir, 'libnative.so'), '\x7fELF\x02\x01\x01\x00\x00')!
	aged_write(os.join_path(framework_dir, 'Native'), '\xcf\xfa\xed\xfe\x00\x00')!
	found := v3_unchanged_build_link_inputs(['-L', earlier, '-L', lib_dir, '-Wl,-Bstatic', '-lnative',
		'-Wl,-Bdynamic', '-F${os.join_path(root, 'frameworks')}', '-framework', 'Native'],
		'cc', '', 'linux', os.join_path(root, 'cache'))
	assert found.ok, found.reason
	assert os.real_path(os.join_path(lib_dir, 'libnative.a')) in found.files
	assert os.real_path(os.join_path(lib_dir, 'libnative.so')) in found.files
	assert os.real_path(os.join_path(framework_dir, 'Native')) in found.files
	// A library added earlier in the search would be the one linked.
	assert os.join_path(earlier, 'libnative.a') in found.missing
	assert os.join_path(earlier, 'libnative.so') in found.missing
	// A library that no directory holds cannot be verified on Linux.
	assert !v3_unchanged_build_link_inputs(['-L', lib_dir, '-lmissing_receipt_lib'], 'cc', '',
		'linux', os.join_path(root, 'cache')).ok
	$if macos {
		// On macOS it may live in the dyld shared cache: the system version stands for it.
		missing := v3_unchanged_build_link_inputs(['-lmissing_receipt_lib'], 'cc', '', 'macos',
			os.join_path(root, 'cache'))
		assert missing.ok
		assert '/System/Library/CoreServices/SystemVersion.plist' in missing.files
	}
}

fn test_receipt_is_not_published_with_an_unknown_linker_option() {
	root := os.join_path(os.vtmp_dir(), 'v3_receipt_unknown_link_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	assert !v3_unchanged_build_link_inputs(['-Wl,--reads-a-file-we-do-not-know'], 'cc', '', 'linux',
		root).ok
}

fn test_receipt_covers_the_manifest_that_selects_the_sources() {
	f := receipt_fixture('manifest')!
	defer { os.rmdir_all(f.root) or {} }
	manifest := os.join_path(f.source_dir, 'v.mod')
	os.mkdir_all(os.join_path(f.source_dir, 'one'))!
	age_unchanged_build_source(os.join_path(f.source_dir, 'one'))!
	aged_write(manifest, "Module{ name: 'p' subdirs: ['one'] }\n")!
	publish_native_receipt(f, 'manifest', native_receipt_inputs(f, [], ''))
	aged_write(manifest, "Module{ name: 'p' subdirs: ['two'] }\n")!
	assert !v3_restore_unchanged_build(f.cache_dir, 'manifest', f.destination, '')
}

fn test_project_manifest_inputs_walk_up_to_the_nearest_manifest() {
	root := os.join_path(os.real_path(os.vtmp_dir()), 'v3_manifest_walk_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	project := os.join_path(root, 'project')
	source_dir := os.join_path(project, 'src', 'deep')
	selected := os.join_path(project, 'selected', 'below')
	os.mkdir_all(source_dir)!
	os.mkdir_all(selected)!
	aged_write(os.join_path(project, 'v.mod'), "Module{ name: 'p' subdirs: ['selected', 'absent'] }\n")!
	found := v3_project_manifest_inputs([source_dir], os.join_path(root, 'vroot'))
	assert os.real_path(os.join_path(project, 'v.mod')) in found.files
	assert project in found.dirs
	// A manifest created nearer to the sources would be the one that counts.
	assert os.join_path(source_dir, 'v.mod') in found.missing
	assert os.join_path(project, 'src', 'v.mod') in found.missing
	assert os.join_path(project, 'src', '.v.mod.stop') in found.missing
	assert !found.missing.any(it == os.join_path(root, 'v.mod'))
	// A directory that `subdirs` reaches can get sources without any being there yet.
	assert selected in found.dirs
	assert os.join_path(project, 'absent') in found.missing
}

fn test_receipt_covers_the_headers_that_only_tinycc_includes() {
	f := receipt_fixture('tinycc_view')!
	defer { os.rmdir_all(f.root) or {} }
	side := os.join_path(f.source_dir, 'tcc_side.h')
	wrapper := os.join_path(f.source_dir, 'wrapper.h')
	aged_write(side, '#define VALUE 1\n')!
	aged_write(wrapper, '#ifdef __TINYC__\n#include "tcc_side.h"\n#endif\n')!
	tcc_headers := os.join_path(f.root, 'tcc_headers')
	os.mkdir_all(tcc_headers)!
	publish_native_receipt(f, 'tinycc', UnchangedBuildInputs{
		...native_receipt_inputs(f, [], '#include "${wrapper}"\n')
		tcc_include_dir: tcc_headers
	})
	aged_write(side, '#define VALUE 2\n')!
	assert !v3_restore_unchanged_build(f.cache_dir, 'tinycc', f.destination, '')
}

fn test_dependency_key_sees_an_include_that_a_comment_hid() {
	commented := 'int a;\n/*\n#include "b.h"\n*/\n'
	assert c_preprocessor_lines(commented) != c_preprocessor_lines('int a;\n#include "b.h"\n')
}

fn test_receipt_sees_a_source_that_would_be_taken_first_for_an_object() {
	f := receipt_fixture('object_source_order')!
	defer { os.rmdir_all(f.root) or {} }
	object := os.join_path(f.source_dir, 'built.o')
	aged_write(os.join_path(f.source_dir, 'built.cpp'), 'int native_value(void) { return 1; }\n')!
	found := v3_external_native_sources([object])
	assert found.files == [os.join_path(f.source_dir, 'built.cpp')]
	assert object in found.missing
	assert os.join_path(f.source_dir, 'built.c') in found.missing
	assert os.join_path(f.source_dir, 'built.cc') in found.missing
	assert os.join_path(f.source_dir, 'built.m') !in found.missing
	publish_native_receipt(f, 'order', native_receipt_inputs(f, [object], ''))
	aged_write(os.join_path(f.source_dir, 'built.c'), 'int native_value(void) { return 2; }\n')!
	assert !v3_restore_unchanged_build(f.cache_dir, 'order', f.destination, '')
}

fn test_c_link_plan_is_invalid_when_a_file_appears_at_an_absent_path() {
	root := os.join_path(os.vtmp_dir(), 'v3_link_plan_absent_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	os.mkdir_all(root)!
	plan := os.join_path(root, 'link.manifest')
	absent := os.join_path(root, 'object.o')
	mut stats := CObjectCacheStats{}
	stats.missing_candidates[absent] = true
	write_c_link_plan(plan, []string{}, &stats)!
	mut read_stats := CObjectCacheStats{}
	assert valid_c_link_plan(plan, mut read_stats) != none
	os.write_file(absent, 'object')!
	assert valid_c_link_plan(plan, mut read_stats) == none
}

fn test_module_shadow_inputs_cover_nearer_copies_of_a_module() {
	root := os.join_path(os.real_path(os.vtmp_dir()), 'v3_module_shadow_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	global := os.join_path(root, 'global')
	other := os.join_path(root, 'other')
	project := os.join_path(root, 'project')
	os.mkdir_all(os.join_path(global, 'abc', 'inner'))!
	os.mkdir_all(other)!
	os.mkdir_all(os.join_path(project, 'abc'))!
	found := v3_module_shadow_inputs([project, os.join_path(global, 'abc'),
		os.join_path(global, 'abc', 'inner')], [global, other], [project])
	// An empty directory of that name in the project may get the sources of the module.
	assert os.join_path(project, 'abc') in found.dirs
	assert os.join_path(project, 'abc', 'inner') in found.missing
	assert os.join_path(other, 'abc') in found.missing
	assert !found.missing.any(it.starts_with(global))
}

fn test_probe_of_a_receipt_is_named_by_its_directives() {
	root := os.join_path(os.vtmp_dir(), 'v3_receipt_probe_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	first := v3_unchanged_build_probe_file(root, '#include <stdio.h>\n')?
	assert v3_unchanged_build_probe_file(root, '#include <stdio.h>\n')? == first
	assert v3_unchanged_build_probe_file(root, '#include <math.h>\n')? != first
	assert os.read_file(first)! == '#include <stdio.h>\n'
	assert os.ls(root)!.filter(it.ends_with('.tmp')).len == 0
}

fn test_unused_receipts_are_trimmed() {
	root := os.join_path(os.vtmp_dir(), 'v3_receipt_trim_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	os.mkdir_all(root)!
	old := time.utc().unix() - (c_dependency_manifest_unused_days + 1) * 24 * 3600
	for name in ['old.stamp', 'old.exe', 'orphan.exe', 'native_probe_old.c', 'kept.stamp', 'kept.exe'] {
		os.write_file(os.join_path(root, name), '')!
		if !name.starts_with('kept') {
			os.utime(os.join_path(root, name), old, old)!
		}
	}
	trim_unchanged_build_cache(root)
	mut left := os.ls(root)!.filter(it != 'receipts.trim')
	left.sort()
	assert left == ['kept.exe', 'kept.stamp']
}

fn test_pkgconfig_directive_packages_skip_options_and_versions() {
	assert v3_pkgconfig_packages('--cflags --libs gtk+-3.0 >= 3.24 glib-2.0') == [
		'gtk+-3.0',
		'glib-2.0',
	]
	assert v3_pkgconfig_packages('sdl2') == ['sdl2']
}

fn test_pkgconfig_files_follow_requirements() {
	if os.find_abs_path_of_executable('pkg-config') or { '' } == '' {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v3_receipt_pkgconfig_${os.getpid()}')
	old_path := os.getenv('PKG_CONFIG_PATH')
	defer {
		os.setenv('PKG_CONFIG_PATH', old_path, true)
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(root)!
	os.write_file(os.join_path(root, 'receiptbase.pc'), 'Name: receiptbase\nDescription: base\nVersion: 1\nCflags: -I/tmp/receiptbase\n')!
	os.write_file(os.join_path(root, 'receipttop.pc'), 'Name: receipttop\nDescription: top\nVersion: 1\nRequires: receiptbase\nLibs: -lreceipttop\n')!
	os.setenv('PKG_CONFIG_PATH', root, true)
	found := v3_pkgconfig_files(['receipttop'])
	assert found.ok
	assert os.real_path(os.join_path(root, 'receipttop.pc')) in found.files
	assert os.real_path(os.join_path(root, 'receiptbase.pc')) in found.files
	assert os.real_path(root) in found.dirs
	assert !v3_pkgconfig_files(['receipt_missing_package']).ok
}

fn test_tracked_dependencies_search_the_directory_of_the_unit_first() {
	root := os.join_path(os.real_path(os.vtmp_dir()), 'v3_unit_quote_dir_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	native := os.join_path(root, 'native')
	later := os.join_path(root, 'later')
	aged_write(os.join_path(later, 'choice.h'), '#define CHOICE 1\n')!
	source := os.join_path(native, 'native.c')
	aged_write(source, '#include "choice.h"\nint native_value(void) { return CHOICE; }\n')!
	deps := c_object_tracked_dependencies('cc', ['-I', later, '-x', 'c', '-w'], source,
		'', os.join_path(root, 'cache'))
	assert !deps.used_fallback
	assert deps.files.any(it.ends_with('later/choice.h'))
	// A `choice.h` beside the unit would be included instead.
	assert os.join_path(native, 'choice.h') in deps.missing
}

fn test_missing_include_candidates_start_in_the_directory_of_the_unit() {
	root := os.join_path(os.real_path(os.vtmp_dir()), 'v3_unit_candidates_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	later := os.join_path(root, 'later')
	native := os.join_path(root, 'native')
	aged_write(os.join_path(later, 'choice.h'), '')!
	os.mkdir_all(native)!
	search := V3IncludeSearch{
		dirs: [later]
	}
	read := [os.join_path(later, 'choice.h')]
	unit := os.join_path(native, 'native.c')
	missing := v3_missing_include_candidates(search, [later], read, [unit], time.utc().unix())?
	assert os.join_path(native, 'choice.h') in missing
	without_unit := v3_missing_include_candidates(search, [later], read, [], time.utc().unix())?
	assert os.join_path(native, 'choice.h') !in without_unit
}
