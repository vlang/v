// vtest build: !windows
module driver

import os

fn test_compiler_cache_matches_a_clean_build_after_a_configuration_change() ! {
	compiler := os.find_abs_path_of_executable('cc')!
	root := os.join_path(os.vtmp_dir(), 'v3_native_wrapper_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	wrapper := os.join_path(root, 'wrapper-cc')
	os.write_file(wrapper, '#!/bin/sh
exec ${os.quoted_path(compiler)} "-DTEST_NATIVE_VALUE=\$V_TEST_NATIVE_VALUE" "$@"
')!
	os.chmod(wrapper, 0o755)!
	assert !v3_native_compiler_cacheable(wrapper)
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'native_wrapper' }\n")!
	os.write_file(os.join_path(root, 'native.c'), 'int native_value(void) { return TEST_NATIVE_VALUE; }\n')!
	source := os.join_path(root, 'main.v')
	os.write_file(source, '#flag @VMODROOT/native.o
fn C.native_value() int
fn main() { println(C.native_value()) }
')!
	program := os.join_path(root, 'program')
	for configured_cc in [wrapper, compiler] {
		is_wrapper := configured_cc == wrapper
		if !is_wrapper && !v3_native_compiler_cacheable(compiler) {
			continue
		}
		// With tracing disabled the build may reuse the saved link plan.
		for trace in ['1', ''] {
			cache_name := if trace == '1' { 'objects' } else { 'link_plan' }
			mode := if is_wrapper { 'wrapper' } else { 'direct' }
			warm := os.join_path(root, '${mode}_${cache_name}')
			cold := os.join_path(root, 'clean_${mode}_${cache_name}')
			for index, value in [1, 1, 2, 2] {
				scratch := if index == 3 { cold } else { warm }
				os.mkdir_all(scratch)!
				flags := if is_wrapper {
					[]string{}
				} else {
					['-cflags', '-DTEST_NATIVE_VALUE=${value}']
				}
				built := os.exec(['env', 'VTMP=${scratch}',
					'V3CACHE=${os.join_path(scratch, 'cache')}',
					'VMODULES=${os.join_path(scratch, 'modules')}', 'VFLAGS=', 'VOSARGS=', 'CFLAGS=',
					'LDFLAGS=', 'V3_ENABLE_UNCHANGED_BUILD_CACHE=1', 'V3_DISABLE_UNCHANGED_BUILD_CACHE=',
					'V3_CACHE_DISABLE_PROGRAM_EXECUTABLE=0', 'V3_CACHE_TRACE=${trace}',
					'V_MACOS_V3_NO_FALLBACK=1', 'V_TEST_NATIVE_VALUE=${value}', @VEXE, '-cc',
					configured_cc, '-gc', 'none', '-d', 'no_segfault_handler', ...flags, '-o',
					program, source])
				assert built.exit_code == 0, built.output
				run := os.exec([program])
				assert run.exit_code == 0, run.output
				assert run.output.trim_space() == value.str(), built.output
				if trace == '1' && is_wrapper {
					assert built.output.contains('outside the native cache contract'), built.output
					assert !built.output.contains('C object cache hit:'), built.output
					assert !built.output.contains('unchanged build cache hit'), built.output
					assert !built.output.contains('program executable restored'), built.output
				}
				if trace == '1' && !is_wrapper && index == 1 {
					assert built.output.contains('C object cache hit: key=native_o_'), built.output
				}
			}
		}
	}
}
