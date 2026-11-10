module driver

import os
import crypto.sha256
import time
import v.cmdexec
import v.modulecache
import v.pref

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
fn native_receipt_inputs(f ReceiptFixture, flags []string) UnchangedBuildInputs {
	return UnchangedBuildInputs{
		sources:       [f.source]
		dirs:          [f.source_dir]
		output_dir:    f.root
		flags:         flags
		cache_dir:     f.cache_dir
		project_roots: [f.root]
	}
}

// publish_native_receipt records a receipt prepared from `inputs` under `key`,
// with the native inputs `native` that the C compiler and the linker told.
fn publish_native_receipt(f ReceiptFixture, key string, inputs UnchangedBuildInputs, native UnchangedBuildNativeInputs) {
	age_unchanged_build_source(f.source_dir) or { panic(err) }
	stamp := v3_unchanged_build_with_native_inputs(v3_unchanged_build_prepare(inputs), native)
	assert stamp.ok
	assert v3_publish_unchanged_build(f.cache_dir, key, f.executable, stamp)
	assert v3_restore_unchanged_build(f.cache_dir, key, f.destination, '')
}

// compiled_unit_headers compiles the C `unit` with `args` as a build does, in a
// directory of its own below `root`, with the dependency file that the build asks
// for, and returns what v3_receipt_unit_headers takes from it.
fn compiled_unit_headers(root string, args []string, unit string) UnchangedBuildNativeInputs {
	build_dir := os.join_path(root, 'build')
	os.mkdir_all(build_dir) or { panic(err) }
	os.write_file(os.join_path(build_dir, 'src.c'), unit) or { panic(err) }
	before := time.utc().unix() - 1
	mut command := args.clone()
	command << ['-w', '-c', '-o', 'src.o', 'src.c', '-MD', '-MF', v3_program_dependency_file]
	result := cmdexec.run_in('cc', command, build_dir)
	assert result.exit_code == 0, result.output
	manager := modulecache.Manager{}
	return v3_receipt_unit_headers(&manager, 'cc', args, build_dir, 'src.c', before)
}

// told_headers is a V3ReceiptHeaders of a command that compiled the program unit.
fn told_headers(headers UnchangedBuildNativeInputs) V3ReceiptHeaders {
	return V3ReceiptHeaders{
		UnchangedBuildNativeInputs: headers
		told:                       true
	}
}

fn test_receipt_takes_the_headers_that_the_compiler_wrote_down() {
	f := receipt_fixture('told_headers')!
	defer { os.rmdir_all(f.root) or {} }
	include := os.join_path(f.root, 'include')
	aged_write(os.join_path(include, 'choice.h'), '#define CHOICE 1\n')!
	headers := compiled_unit_headers(f.root, ['-I', include], '#include <choice.h>\nint value(void) { return CHOICE; }\n')
	assert headers.unknown == ''
	assert os.join_path(include, 'choice.h') in headers.files
	// The unit is made by the build, beside its output.
	assert !headers.files.any(it.ends_with('src.c'))
	// A `choice.h` beside the unit would be included instead.
	assert os.join_path(f.root, 'build', 'choice.h') in headers.missing
	// The compiler did not tell: the receipt is not recorded.
	os.rm(os.join_path(f.root, 'build', v3_program_dependency_file))!
	manager := modulecache.Manager{}
	assert v3_receipt_unit_headers(&manager, 'cc', ['-I', include], os.join_path(f.root, 'build'),
		'src.c', time.utc().unix()).unknown != ''
}

fn test_receipt_is_not_recorded_for_a_unit_that_asks_for_the_time() {
	f := receipt_fixture('unit_time')!
	defer { os.rmdir_all(f.root) or {} }
	headers := compiled_unit_headers(f.root, [], 'const char *built = __DATE__;\n')
	assert headers.unknown.contains('time')
}

fn test_receipt_is_not_published_with_options_that_hide_inputs() {
	f := receipt_fixture('opaque')!
	defer { os.rmdir_all(f.root) or {} }
	assert !v3_unchanged_build_prepare(native_receipt_inputs(f, ['@response_file'])).ok
}

fn test_receipt_is_not_published_for_a_module_found_above_the_project() {
	f := receipt_fixture('above_project')!
	defer { os.rmdir_all(f.root) or {} }
	mut inputs := native_receipt_inputs(f, [])
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
	headers := compiled_unit_headers(f.root, ['-I', earlier, '-I', later], '#include <choice.h>\n')
	assert os.join_path(earlier, 'choice.h') in headers.missing
	publish_native_receipt(f, 'search', native_receipt_inputs(f, []), headers)
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

fn test_receipt_takes_every_form_of_a_library_from_the_link() {
	root := os.join_path(os.vtmp_dir(), 'v3_receipt_libs_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	lib_dir := os.join_path(root, 'lib')
	earlier := os.join_path(root, 'earlier')
	os.mkdir_all(earlier)!
	aged_write(os.join_path(lib_dir, 'libnative.a'), '!<arch>\n')!
	aged_write(os.join_path(lib_dir, 'libnative.so'), '\x7fELF\x02\x01\x01\x00\x00')!
	link := v3_program_link_inputs(['-o', 'out', 'src.c', '-L', earlier, '-L', lib_dir, '-Wl,-Bstatic',
		'-lnative', '-Wl,-Bdynamic'], '', []string{}, root)
	found := v3_unchanged_build_native_inputs(&link, &V3ReceiptHeaders{}, false, &CObjectCacheStats{},
		[]string{})
	assert found.unknown == ''
	// Each is recorded by the path that the linker looks up, not by where it leads.
	assert os.join_path(lib_dir, 'libnative.a') in found.files
	assert os.join_path(lib_dir, 'libnative.so') in found.files
	// A library added earlier in the search would be the one linked.
	assert os.join_path(earlier, 'libnative.a') in found.missing
	assert os.join_path(earlier, 'libnative.so') in found.missing
	unresolved := v3_program_link_inputs(['-o', 'out', 'src.c', '-L', lib_dir, '-lmissing_receipt_lib'],
		'', []string{}, root)
	missing := v3_unchanged_build_native_inputs(&unresolved, &V3ReceiptHeaders{}, false,
		&CObjectCacheStats{}, []string{})
	$if macos {
		// On macOS it may live in the dyld shared cache: the system version stands for it.
		assert missing.unknown == ''
		assert '/System/Library/CoreServices/SystemVersion.plist' in missing.files
	} $else {
		// A library that no directory holds cannot be verified elsewhere.
		assert missing.unknown != ''
	}
}

fn test_receipt_native_inputs_are_not_recorded_when_a_part_was_not_told() {
	untaken := V3ProgramLinkInputs{}
	assert v3_unchanged_build_native_inputs(&untaken, &V3ReceiptHeaders{}, false, &CObjectCacheStats{},
		[]string{}).unknown != ''
	link := V3ProgramLinkInputs{
		taken: true
	}
	// The command compiled the unit, and could not tell its headers.
	assert v3_unchanged_build_native_inputs(&link, &V3ReceiptHeaders{
		UnchangedBuildNativeInputs: UnchangedBuildNativeInputs{
			unknown: 'no headers'
		}
		told:                       true
	}, false, &CObjectCacheStats{}, []string{}).unknown == 'no headers'
	// It compiled no unit, and the program has native inputs of its own.
	assert v3_unchanged_build_native_inputs(&link, &V3ReceiptHeaders{}, true, &CObjectCacheStats{},
		[]string{}).unknown != ''
	// An object was built without the dependencies of its source.
	fallback := CObjectCacheStats{
		dependency_scan_fallbacks: 1
	}
	assert v3_unchanged_build_native_inputs(&link, &V3ReceiptHeaders{}, false, &fallback,
		[]string{}).unknown != ''
	// The reason of the link is taken from before the headers were added.
	blocked := V3ProgramLinkInputs{
		taken:   true
		unknown: 'native headers require C compiler processing'
	}
	assert v3_unchanged_build_native_inputs(&blocked, &V3ReceiptHeaders{
		told: true
	}, false, &CObjectCacheStats{}, []string{}).unknown == ''
}

fn test_receipt_native_inputs_take_the_objects_and_leave_out_what_the_build_made() {
	root := os.join_path(os.real_path(os.vtmp_dir()), 'v3_receipt_objects_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	made := os.join_path(root, 'cache')
	header := os.join_path(root, 'native.h')
	object := os.join_path(made, 'native.o')
	link := V3ProgramLinkInputs{
		taken: true
		files: [object, os.join_path(root, 'libx.a')]
	}
	mut objects := CObjectCacheStats{}
	objects.file_signatures[header] = 'signature'
	objects.file_signatures[os.join_path(made, 'stamp')] = 'signature'
	// A source that would be taken first for an object (c_object_absent_inputs).
	objects.missing_candidates[os.join_path(root, 'native.c')] = true
	found := v3_unchanged_build_native_inputs(&link, &V3ReceiptHeaders{}, false, &objects,
		[made])
	assert found.files == [os.join_path(root, 'libx.a'), header]
	assert found.missing == [os.join_path(root, 'native.c')]
}

// publish_receipt records `receipt` under `key` and checks that it restores.
fn publish_receipt(f ReceiptFixture, key string, receipt UnchangedBuildReceipt) {
	stamp := v3_unchanged_build_stamp(receipt, f.cache_dir, '')
	assert stamp.ok
	assert v3_publish_unchanged_build(f.cache_dir, key, f.executable, stamp)
	assert v3_restore_unchanged_build(f.cache_dir, key, f.destination, '')
}

fn test_receipt_sees_a_library_link_that_comes_to_point_elsewhere() {
	f := receipt_fixture('library_link')!
	defer { os.rmdir_all(f.root) or {} }
	lib_dir := os.join_path(f.root, 'lib')
	aged_write(os.join_path(lib_dir, 'one.a'), '!<arch>\none\n')!
	aged_write(os.join_path(lib_dir, 'two.a'), '!<arch>\ntwo\n')!
	link := os.join_path(lib_dir, 'libreceipt.a')
	os.symlink('one.a', link)!
	inputs := v3_program_link_inputs(['-o', 'out', 'src.c', '-L', lib_dir, '-Wl,-Bstatic', '-lreceipt',
		'-Wl,-Bdynamic'], '', []string{}, f.root)
	found := v3_unchanged_build_native_inputs(&inputs, &V3ReceiptHeaders{}, false, &CObjectCacheStats{},
		[]string{})
	assert found.unknown == ''
	// The linker opens the archive by the path that it looks up.
	assert link in found.files
	publish_native_receipt(f, 'library_link', native_receipt_inputs(f, []), found)
	// Both archives stay; only the link that the linker finds changes.
	os.rm(link)!
	os.symlink('two.a', link)!
	assert !v3_restore_unchanged_build(f.cache_dir, 'library_link', f.destination, '')
}

fn test_receipt_sees_a_module_directory_link_that_comes_to_point_elsewhere() {
	f := receipt_fixture('module_link')!
	defer { os.rmdir_all(f.root) or {} }
	first := os.join_path(f.root, 'store', 'one')
	second := os.join_path(f.root, 'store', 'two')
	aged_write(os.join_path(first, 'value.v'), 'module value\n\npub fn get() int {\n\treturn 1\n}\n')!
	aged_write(os.join_path(second, 'value.v'), 'module value\n\npub fn get() int {\n\treturn 2\n}\n')!
	age_unchanged_build_source(first)!
	age_unchanged_build_source(second)!
	os.mkdir_all(os.join_path(f.source_dir, 'deps'))!
	link := os.join_path(f.source_dir, 'deps', 'value')
	os.symlink(first, link)!
	found_as := os.join_path(link, 'value.v')
	real := os.real_path(found_as)
	assert v3_source_lookup_dir(found_as, real) == link
	assert v3_source_lookup_dir(f.source, f.source) == ''
	publish_native_receipt(f, 'module_link', UnchangedBuildInputs{
		...native_receipt_inputs(f, [])
		sources:       [f.source, real]
		dirs:          [f.source_dir, os.dir(real)]
		lookup_dirs:   [link]
		// The sources of a build are its resolved paths.
		project_roots: [f.root, os.real_path(f.root)]
	}, UnchangedBuildNativeInputs{})
	os.rm(link)!
	os.symlink(second, link)!
	assert !v3_restore_unchanged_build(f.cache_dir, 'module_link', f.destination, '')
}

fn test_receipt_sees_a_header_link_that_comes_to_point_elsewhere() {
	f := receipt_fixture('header_link')!
	defer { os.rmdir_all(f.root) or {} }
	include := os.join_path(f.root, 'include')
	aged_write(os.join_path(f.root, 'versions', 'one.h'), 'static int choice(void) { return 1; }\n')!
	aged_write(os.join_path(f.root, 'versions', 'two.h'), 'static int choice(void) { return 2; }\n')!
	os.mkdir_all(include)!
	link := os.join_path(include, 'choice.h')
	os.symlink(os.join_path(f.root, 'versions', 'one.h'), link)!
	age_unchanged_build_source(include)!
	headers := compiled_unit_headers(f.root, ['-I', include], '#include <choice.h>\n')
	// The header is recorded by the path that the compiler looked up.
	assert link in headers.files
	publish_native_receipt(f, 'header_link', native_receipt_inputs(f, []), headers)
	os.rm(link)!
	os.symlink(os.join_path(f.root, 'versions', 'two.h'), link)!
	assert !v3_restore_unchanged_build(f.cache_dir, 'header_link', f.destination, '')
}

fn test_receipt_is_not_published_with_an_unknown_linker_option() {
	link := v3_program_link_inputs(['-o', 'out', 'src.c', '-Wl,--reads-a-file-we-do-not-know'],
		'', []string{}, os.vtmp_dir())
	assert v3_unchanged_build_native_inputs(&link, &V3ReceiptHeaders{}, false, &CObjectCacheStats{},
		[]string{}).unknown != ''
}

fn test_receipt_covers_the_manifest_that_selects_the_sources() {
	f := receipt_fixture('manifest')!
	defer { os.rmdir_all(f.root) or {} }
	manifest := os.join_path(f.source_dir, 'v.mod')
	os.mkdir_all(os.join_path(f.source_dir, 'one'))!
	age_unchanged_build_source(os.join_path(f.source_dir, 'one'))!
	aged_write(manifest, "Module{ name: 'p' subdirs: ['one'] }\n")!
	publish_native_receipt(f, 'manifest', native_receipt_inputs(f, []), UnchangedBuildNativeInputs{})
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
	found := v3_project_manifest_inputs([source_dir])
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

fn test_project_manifest_inputs_cover_projects_inside_the_v_checkout() {
	vroot := os.real_path(@VEXEROOT)
	// A project or an example inside the checkout selects its sources with its own manifest.
	project := os.join_path(vroot, 'vlib', 'v', 'tests', 'project_with_c_code_2')
	found := v3_project_manifest_inputs([project])
	assert os.join_path(project, 'v.mod') in found.files
	assert project in found.dirs
	// A module of vlib reads the manifest of its own directory.
	x11 := os.join_path(vroot, 'vlib', 'x', 'x11')
	assert os.join_path(x11, 'v.mod') in v3_project_manifest_inputs([x11]).files
}

fn test_manifest_subdirs_are_recorded_by_the_path_that_leads_to_them() {
	root := os.join_path(os.vtmp_dir(), 'v3_manifest_link_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	project := os.join_path(root, 'project')
	os.mkdir_all(os.join_path(root, 'store', 'one'))!
	os.mkdir_all(project)!
	aged_write(os.join_path(project, 'v.mod'), "Module{ name: 'p' subdirs: ['selected'] }\n")!
	os.symlink(os.join_path(root, 'store', 'one'), os.join_path(project, 'selected'))!
	found := v3_project_manifest_inputs([project])
	assert os.join_path(project, 'v.mod') in found.files
	assert os.join_path(project, 'selected') in found.dirs
}

fn test_receipt_is_not_published_for_a_module_above_a_project_inside_the_v_checkout() {
	root := os.join_path(os.real_path(os.vtmp_dir()), 'v3_receipt_in_vroot_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	vroot := os.join_path(root, 'vroot')
	project := os.join_path(vroot, 'examples', 'a', 'b', 'project')
	above := os.join_path(vroot, 'examples', 'foo')
	os.mkdir_all(os.join_path(vroot, 'vlib', 'builtin'))!
	aged_write(os.join_path(project, 'v.mod'), "Module{ name: 'project' }\n")!
	aged_write(os.join_path(project, 'main.v'), 'import foo\n\nfn main() {}\n')!
	aged_write(os.join_path(above, 'foo.v'), 'module foo\n')!
	prefs := &pref.Preferences{
		vroot:                  vroot
		module_resolution_root: project
	}
	_, _, project_roots := v3_unchanged_build_roots(prefs)
	assert v3_path_is_below_one_of(os.join_path(project, 'main.v'), project_roots)
	assert v3_path_is_below_one_of(os.join_path(vroot, 'vlib', 'builtin', 'builtin.v'),
		project_roots)
	// A module found by walking up from the project is outside it, also in the checkout.
	assert !v3_path_is_below_one_of(os.join_path(above, 'foo.v'), project_roots)
}

fn test_dependency_key_sees_an_include_that_a_comment_hid() {
	commented := 'int a;\n/*\n#include "b.h"\n*/\n'
	assert c_preprocessor_lines(commented) != c_preprocessor_lines('int a;\n#include "b.h"\n')
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
	assert os.join_path(root, 'receipttop.pc') in found.files
	assert os.join_path(root, 'receiptbase.pc') in found.files
	assert root in found.dirs
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

fn test_receipt_knows_an_output_directory_of_sources_through_a_link() {
	f := receipt_fixture('output_source_link')!
	link := f.root + '_link'
	defer {
		os.rm(link) or {}
		os.rmdir_all(f.root) or {}
	}
	os.mkdir_all(f.cache_dir)!
	os.symlink(f.root, link)!
	age_unchanged_build_source(f.root)!
	receipt := UnchangedBuildReceipt{
		...f.receipt()
		dirs:       [f.source_dir, link]
		output_dir: os.real_path(f.root)
	}
	stamp := v3_unchanged_build_stamp(receipt, f.cache_dir, '')
	assert stamp.ok
	assert v3_publish_unchanged_build(f.cache_dir, 'output_link', f.executable, stamp)
	// The executable written into the directory is no new source there.
	assert v3_restore_unchanged_build(f.cache_dir, 'output_link', f.destination, '')
	assert v3_restore_unchanged_build(f.cache_dir, 'output_link', f.destination, '')
}

// The tests below build programs and take long: a test after them would find
// the files that it ages newer than the start of its process.

// age_tree gives every file and directory below `root`, and `root` itself, a time
// from before the build: a receipt vouches only for what did not change during it.
fn age_tree(root string) ! {
	mut dirs := [root]
	for dirs.len > 0 {
		dir := dirs.pop()
		for entry in os.ls(dir)! {
			path := os.join_path(dir, entry)
			if os.is_dir(path) {
				dirs << path
			} else {
				age_unchanged_build_source(path)!
			}
		}
		age_unchanged_build_source(dir)!
	}
}

// receipt_project_build builds the V project `project` into `executable` with the
// receipt cache in `cache`, and returns the output of the compiler and the line
// that the executable prints.
fn receipt_project_build(project string, executable string, cache string) (string, string) {
	mut environment := os.environ()
	environment.delete('VFLAGS')
	environment.delete('V3_DISABLE_UNCHANGED_BUILD_CACHE')
	environment['V3CACHE'] = cache
	environment['V3_CACHE_TRACE'] = '1'
	environment['V_MACOS_V3_NO_FALLBACK'] = '1'
	mut compiler := os.new_process(@VEXE)
	compiler.set_environment(environment)
	compiler.set_args(['-cc', 'clang', '-gc', 'none', '-o', executable, project])
	compiler.set_redirect_stdio_merged()
	compiler.run()
	output := compiler.stdout_slurp()
	compiler.wait()
	code := compiler.code
	compiler.close()
	assert code == 0, output
	result := os.exec([executable])
	assert result.exit_code == 0, result.output
	return output, result.output.trim_space()
}

// receipt_project_rebuilds builds `project`, which prints `1`, checks that a
// rebuild of it is restored from its receipt, calls `change`, and checks that the
// build after it prints `2`, as a build without the receipt does.
fn receipt_project_rebuilds(root string, project string, change fn () !) ! {
	if os.find_abs_path_of_executable('clang') or { '' } == '' {
		return
	}
	age_tree(project)!
	executable := os.join_path(root, 'out', 'app')
	os.mkdir_all(os.dir(executable))!
	age_unchanged_build_source(os.dir(executable))!
	cache := os.join_path(root, 'cache')
	first_output, first := receipt_project_build(project, executable, cache)
	assert first == '1'
	assert first_output.contains('unchanged build cache recorded'), first_output
	again_output, again := receipt_project_build(project, executable, cache)
	assert again == '1'
	assert again_output.contains('unchanged build cache hit'), again_output
	change()!
	changed_output, changed := receipt_project_build(project, executable, cache)
	assert changed == '2', changed_output
	assert !changed_output.contains('unchanged build cache hit'), changed_output
}

fn test_receipt_sees_a_header_of_an_object_that_the_build_compiles() {
	root := os.join_path(os.real_path(os.vtmp_dir()), 'v3_receipt_c_object_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	project := os.join_path(root, 'project')
	os.mkdir_all(project)!
	os.write_file(os.join_path(project, 'v.mod'), "Module { name: 'object' }\n")!
	os.write_file(os.join_path(project, 'main.v'), '#flag @VMODROOT/native.o\nfn C.native_value() int\n\nfn main() {\n\tprintln(C.native_value())\n}\n')!
	// The object cache compiles the source as C11; the C compiler has another
	// standard of its own.
	os.write_file(os.join_path(project, 'native.c'), '#if __STDC_VERSION__ == 201112L\n#include "c11_value.h"\n#else\n#include "other_value.h"\n#endif\nint native_value(void) { return NATIVE_VALUE; }\n')!
	os.write_file(os.join_path(project, 'c11_value.h'), '#define NATIVE_VALUE 1\n')!
	os.write_file(os.join_path(project, 'other_value.h'), '#define NATIVE_VALUE 9\n')!
	receipt_project_rebuilds(root, project, fn [project] () ! {
		os.write_file(os.join_path(project, 'c11_value.h'), '#define NATIVE_VALUE 2\n')!
	})!
}

fn test_receipt_sees_a_changed_header_of_the_program() {
	root := os.join_path(os.real_path(os.vtmp_dir()), 'v3_receipt_program_header_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	project := os.join_path(root, 'project')
	include := os.join_path(project, 'include')
	os.mkdir_all(include)!
	os.write_file(os.join_path(project, 'v.mod'), "Module { name: 'header' }\n")!
	os.write_file(os.join_path(project, 'main.v'), '#flag -I @VMODROOT/include\n#include <receipt.h>\nfn C.receipt_value() int\n\nfn main() {\n\tprintln(C.receipt_value())\n}\n')!
	os.write_file(os.join_path(include, 'receipt.h'), '#include "receipt_value.h"\nstatic int receipt_value(void) { return RECEIPT_VALUE; }\n')!
	os.write_file(os.join_path(include, 'receipt_value.h'), '#define RECEIPT_VALUE 1\n')!
	receipt_project_rebuilds(root, project, fn [include] () ! {
		os.write_file(os.join_path(include, 'receipt_value.h'), '#define RECEIPT_VALUE 2\n')!
	})!
}
