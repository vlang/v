module driver

import os
import time

fn fastc_host_support_test_root() !string {
	root := os.join_path(os.vtmp_dir(), 'fastc_host_support_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	return root
}

fn fastc_host_support_test_native_args(root string) []string {
	return [
		'vlib/builtin/segfault_handler_nix.c',
		'vlib/os/execute_capture_nix.c',
		'vlib/v/modulecache/file_metadata.c',
		'vlib/runtime/free_memory_darwin.c',
		'vlib/v/bench/peak_rss.c',
		'vlib/v/pthread_helper.c',
		'vlib/sync/thread_helper.c',
	].map(os.join_path(root, it))
}

fn test_fastc_host_support_preserves_owned_include_and_link_input_order() {
	$if !skip_fastc ? {
		root := fastc_host_support_test_root()!
		defer { os.rmdir_all(root) or { panic(err) } }
		mut args := fastc_host_support_test_native_args(root)
		tail := ['-L/sdk/usr/lib', '-lpthread', '-lm', '/temporary/atomic.o']
		args << tail
		flags := ['-std=gnu11', '-I', '/sdk/usr/include', '-I/tcc/lib/include', '-B', '/tcc/lib',
			'-DCUSTOM_DEFINE_no_backtrace', '-Werror=implicit-function-declaration']
		aggregated := v3_fastc_aggregate_host_support_args(root, root, flags, args, '/sdk', '-B/tcc/lib', '-I/tcc/lib/include')
		wrapper := os.join_path(root, 'host_support_darwin.c')
		assert aggregated == [wrapper, ...tail]
		source := os.read_file(wrapper)!
		mut previous := -1
		for path in args[..7] {
			index := source.index('#include "${path}"')!
			assert index > previous
			previous = index
		}
		assert source.contains('#undef V_PARALLEL_CC\n')
		assert source.contains('#undef V_OS_EXECUTE_SCOPE\n')
		assert source.contains('#undef V_PTHREAD_SCOPE\n')
		assert source.contains('#undef V_SYNC_THREAD_SCOPE\n')
	}
}

fn test_fastc_host_support_keeps_unknown_incomplete_and_interleaved_inputs_separate() {
	$if !skip_fastc ? {
		root := fastc_host_support_test_root()!
		defer { os.rmdir_all(root) or { panic(err) } }
		expected := fastc_host_support_test_native_args(root)
		mut unknown := expected.clone()
		unknown << os.join_path(root, 'other.c')
		mut interleaved := expected.clone()
		interleaved.insert(2, '-lpthread')
		mut duplicate := expected.clone()
		duplicate << expected[0]
		mut reordered := expected.clone()
		reordered[0], reordered[1] = reordered[1], reordered[0]
		for args in [unknown, duplicate, interleaved, reordered, expected[..6]] {
			assert v3_fastc_aggregate_host_support_args(root, root, ['-std=gnu11'], args, '/sdk', '-B/tcc/lib', '-I/tcc/lib/include') == args
		}
		assert !os.exists(os.join_path(root, 'host_support_darwin.c'))
	}
}

fn test_fastc_host_support_keeps_custom_preprocessing_flags_and_write_failures_separate() {
	$if !skip_fastc ? {
		root := fastc_host_support_test_root()!
		defer { os.rmdir_all(root) or { panic(err) } }
		args := fastc_host_support_test_native_args(root)
		for flags in [
			['-DV_GCBOEHM_NATIVE'],
			['-DUSER_NATIVE_MACRO=1'],
			['-U', 'NDEBUG'],
			['-include', '/custom/prefix.h'],
			['-imacros', '/custom/macros.h'],
			['-xc++'],
			['-g'],
			['-I'],
			['-I/custom'],
			['-I', '/custom'],
			['-B/custom'],
			['-B', '/custom'],
			['-I', '-DCUSTOM_DEFINE_no_backtrace'],
		] {
			assert v3_fastc_aggregate_host_support_args(root, root, flags, args, '/sdk', '-B/tcc/lib', '-I/tcc/lib/include') == args
		}
		assert !os.exists(os.join_path(root, 'host_support_darwin.c'))
		assert v3_fastc_aggregate_host_support_args(root, os.join_path(root, 'missing'),
			['-std=gnu11'], args, '/sdk', '-B/tcc/lib', '-I/tcc/lib/include') == args
		assert !os.exists(os.join_path(root, 'host_support_darwin.c'))
	}
}

fn test_fastc_host_support_keeps_paths_that_cannot_be_quoted_separate() {
	$if !skip_fastc ? {
		root := fastc_host_support_test_root()!
		defer { os.rmdir_all(root) or { panic(err) } }
		for suffix in ['"', '\\', '\r', '\n'] {
			unsafe_root := root + suffix
			args := fastc_host_support_test_native_args(unsafe_root)
			assert v3_fastc_aggregate_host_support_args(unsafe_root, root, ['-std=gnu11'], args,
				'/sdk', '-B/tcc/lib', '-I/tcc/lib/include') == args
		}
		assert !os.exists(os.join_path(root, 'host_support_darwin.c'))
	}
}
