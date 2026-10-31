module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_target_threads_keep_defaults_and_hosted_threads_keep_stack_attributes() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_target_spawn_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	header := os.join_path(root, 'thread_compat.h')
	os.write_file(header, r'#include <pthread.h>
#include <stdlib.h>
static int create_count = 0;
static int checked_pthread_create(pthread_t *thread, const pthread_attr_t *attributes,
	void *(*start)(void *), void *argument) {
	#if EXPECT_DEFAULT_ATTRIBUTES
	if (attributes != NULL) abort();
	#else
	size_t stack_size = 0;
	if (attributes == NULL || pthread_attr_getstacksize(attributes, &stack_size) != 0
		|| stack_size != 8 * 1024 * 1024) abort();
	#endif
	create_count++;
	return pthread_create(thread, attributes, start, argument);
}
static int thread_create_count(void) { return create_count; }
#define pthread_create checked_pthread_create
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.user_defines = ['v3_backend', 'nofloat', 'no_backtrace']
	prefs.exclude = [os.join_path(@VEXEROOT, 'vlib', 'builtin', 'backtraces_nix.c.v')]
	source_path := os.join_path(root, 'main.v')
	os.write_file(source_path, r'module main
#include "@DIR/thread_compat.h"
fn C.thread_create_count() int
fn work(value int) int { return value + 1 }
fn main() {
	worker := spawn work(41)
	assert worker.wait() == 42
	assert C.thread_create_count() == 1
}
') or { panic(err) }
	for target_libc in [true, false] {
		prefs.target = if target_libc {
			pref.target_from('vinix', pref.host_arch()) or { panic(err) }
		} else {
			pref.host_target()
		}
		prefs.target_libc_headers = target_libc
		c_source := generate_files([root], prefs) or { panic(err) }
		if target_libc {
			assert c_source.contains('pthread_create(&t.handle, NULL,'), c_source
			assert !c_source.contains('pthread_attr_init('), c_source
			assert !c_source.contains('pthread_attr_setstacksize('), c_source
			assert !c_source.contains('pthread_attr_destroy('), c_source
		} else {
			assert c_source.contains('pthread_create(&t.handle, &attributes,'), c_source
			assert c_source.contains('pthread_attr_init('), c_source
			assert c_source.contains('pthread_attr_setstacksize('), c_source
			assert c_source.contains('pthread_attr_destroy('), c_source
		}
		c_file := os.join_path(root, 'main.c')
		program := os.join_path(root, 'program')
		os.write_file(c_file, c_source) or { panic(err) }
		build := cmdexec.run('cc', ['-std=gnu11', '-Werror=implicit-function-declaration',
			'-DEXPECT_DEFAULT_ATTRIBUTES=${if target_libc { '1' } else { '0' }}', '-pthread', '-o',
			program, c_file])
		assert build.exit_code == 0, build.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
	}
}
