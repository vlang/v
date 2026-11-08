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
