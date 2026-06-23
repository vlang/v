module builder

import os
import v2.ast

fn test_parse_fn_signature_and_return_generic_params_with_space() {
	info := parse_fn_signature_and_return('pub fn run_at[A, X](mut global_app A, params RunParams) ! {') or {
		assert false
		return
	}
	assert info.signature == 'pub fn run_at[A, X](mut global_app A, params RunParams)'
	assert info.return_type == '!'
}

fn test_sanitize_staged_c_source_addresses_channel_semaphore_waits() {
	source := [
		'sync__Semaphore__wait(ch->writesem);',
		'sync__Semaphore__wait(&ch->readsem);',
	].join('\n')
	sanitized := sanitize_staged_c_source(source)
	assert sanitized.contains('sync__Semaphore__wait(&ch->writesem);')
	assert sanitized.contains('sync__Semaphore__wait(&ch->readsem);')
	assert !sanitized.contains('sync__Semaphore__wait(ch->writesem);')
	assert !sanitized.contains('sync__Semaphore__wait(&&ch->readsem);')
}

fn test_sanitize_cached_main_c_source_addresses_channel_semaphore_waits() {
	source := [
		'sync__Semaphore__wait(ch->writesem);',
		'sync__Semaphore__wait(&ch->readsem);',
	].join('\n')
	sanitized := sanitize_cached_main_c_source(source)
	assert sanitized.contains('sync__Semaphore__wait(&ch->writesem);')
	assert sanitized.contains('sync__Semaphore__wait(&ch->readsem);')
	assert !sanitized.contains('sync__Semaphore__wait(ch->writesem);')
	assert !sanitized.contains('sync__Semaphore__wait(&&ch->readsem);')
}

fn test_sanitize_cached_main_c_source_uses_c_typedef_names_for_sync_fields() {
	source := [
		'struct atomic_uintptr_t write_adr;',
		'struct pthread_rwlockattr_t attr;',
		'struct pthread_condattr_t attr;',
	].join('\n')
	sanitized := sanitize_cached_main_c_source(source)
	assert sanitized.contains('atomic_uintptr_t write_adr;')
	assert sanitized.contains('pthread_rwlockattr_t attr;')
	assert sanitized.contains('pthread_condattr_t attr;')
	assert !sanitized.contains('struct atomic_uintptr_t write_adr;')
	assert !sanitized.contains('struct pthread_rwlockattr_t attr;')
	assert !sanitized.contains('struct pthread_condattr_t attr;')
}

fn test_sanitize_cached_object_c_source_uses_c_typedef_names_for_sync_fields() {
	source := [
		'struct atomic_uintptr_t write_adr;',
		'struct atomic_uintptr_t read_adr;',
	].join('\n')
	sanitized := sanitize_cached_object_c_source(source)
	assert sanitized.contains('atomic_uintptr_t write_adr;')
	assert sanitized.contains('atomic_uintptr_t read_adr;')
	assert !sanitized.contains('struct atomic_uintptr_t write_adr;')
	assert !sanitized.contains('struct atomic_uintptr_t read_adr;')
}

fn test_sanitize_cached_source_guards_stdatomic_preamble_for_tcc_compat_header() {
	source := [
		'#include <stdint.h>',
		'#include <stdatomic.h>',
		'#include "/tmp/vroot/thirdparty/stdatomic/nix/atomic.h"',
	].join('\n')
	sanitized := sanitize_cached_object_c_source(source)
	assert sanitized.contains('#ifndef __TINYC__\n#include <stdatomic.h>\n#endif')
	assert sanitized.contains('#define extern static\n#endif\n#include "/tmp/vroot/thirdparty/stdatomic/nix/atomic.h"')
	assert sanitized.contains('#undef extern')
	assert sanitized.contains('#include "/tmp/vroot/thirdparty/stdatomic/nix/atomic.h"')
}

fn test_sanitize_cached_source_keeps_stdatomic_preamble_without_compat_header() {
	source := [
		'#include <stdint.h>',
		'#include <stdatomic.h>',
	].join('\n')
	sanitized := sanitize_cached_object_c_source(source)
	assert sanitized == source
}

fn test_join_flag_strings_deduplicates_partial_overlap() {
	flags := join_flag_strings('/tmp/sqlite3.c -lm -lpthread',
		'/tmp/sqlite3.c -lm -lssl -lcrypto -L/tmp/ssl -lpthread')
	assert flags == '/tmp/sqlite3.c -lm -lpthread -lssl -lcrypto -L/tmp/ssl'
}

fn test_join_flag_strings_keeps_distinct_two_token_flags() {
	flags := join_flag_strings('-I /tmp/a -framework Cocoa',
		'-I /tmp/a -I /tmp/b -framework Foundation')
	assert flags == '-I /tmp/a -framework Cocoa -I /tmp/b -framework Foundation'
}

fn test_user_entry_stamp_files_expands_directory_to_parsed_files() {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_cache_entry_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_file := os.join_path(tmp_dir, 'main.v')
	header_file := os.join_path(tmp_dir, 'main.vh')
	mut b := Builder{
		pref:       unsafe { nil }
		user_files: [tmp_dir]
		files:      [
			ast.File{
				name: source_file
			},
			ast.File{
				name: header_file
			},
			ast.File{},
		]
	}
	files := b.user_entry_stamp_files()
	assert files == [os.norm_path(source_file)]
	assert os.norm_path(tmp_dir) !in files
}
