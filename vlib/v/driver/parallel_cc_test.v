module driver

import os
import v.cmdexec
import v.pref

fn v3_parallel_cc_test_compiler() string {
	// The V1-hosted CI test binary records v1_fallback as @VEXE; use the shared V3 compiler there.
	shared_v3 := os.getenv('V3_TEST_UNIT_SHARED_V3')
	if shared_v3.len > 0 {
		return shared_v3
	}
	return @VEXE
}

fn test_merge_v3_parallel_c_units_bounds_and_preserves_source() {
	parts := ['a', 'bb', 'ccc', 'dddd', 'eeeee', 'ffffff']
	merged := merge_v3_parallel_c_units(parts, 3)
	assert merged.len == 3
	assert merged.join('') == parts.join('')
	assert merge_v3_parallel_c_units(parts, 0) == parts
	assert merge_v3_parallel_c_units(parts[..2], 3) == parts[..2]
}

fn test_split_v3_parallel_c_source_uses_safe_unit_markers() {
	source := '#include <stdio.h>\n/* V3CACHE_BODY_BEGIN */\n\t/* V3PARALLEL_CC_UNIT */\n/* V3CACHE_MODULE first */\nvoid first(void) { puts("/* V3PARALLEL_CC_UNIT */"); }\n// /* V3PARALLEL_CC_UNIT */ inside a comment\n/* V3PARALLEL_CC_UNIT */\n/* V3CACHE_MODULE second */\nvoid second(void) {}\n/* V3CACHE_BODY_END */\n'
	prefix, units := split_v3_parallel_c_source(source, 8) or { panic(err) }
	assert prefix == '#include <stdio.h>\n'
	assert units.len == 2
	assert units[0].contains('void first(void) {')
	assert units[0].contains('puts("/* V3PARALLEL_CC_UNIT */")')
	assert units[0].contains('inside a comment')
	assert units[1].contains('void second(void) {}')
}

fn test_v3_parallel_c_cache_source_identity_tracks_generated_content() {
	cache_root := os.join_path(os.vtmp_dir(), 'parallel_cc_identity_test')
	body := 'void body(void) {}\n'
	first := v3_parallel_c_cached_source_path(cache_root, body, false)
	assert first == v3_parallel_c_cached_source_path(cache_root, body, false)
	assert first != v3_parallel_c_cached_source_path(cache_root, body + '// changed\n', false)
	assert first != v3_parallel_c_cached_source_path(cache_root, body, true)
}

fn test_v3_parallel_c_cached_unit_includes_stable_header_path() {
	header_path := '/cache/headers/shared.h'
	body := 'void body(void) {}\n'
	unit := v3_parallel_c_unit_source(header_path, body, false, false)
	assert unit.contains('#include "${header_path}"')
	assert unit.ends_with(body)
	owner := v3_parallel_c_unit_source(header_path, body, true, false)
	assert owner.contains('#define V_PARALLEL_CC_OUT_0 1')
	assert !owner.contains('#include "${header_path}"')
	assert !v3_parallel_c_unit_source('', body, false, false).contains('#include')
}

fn test_publish_v3_parallel_c_cache_object_copies_before_replacing() {
	root := os.join_path(os.vtmp_dir(), 'parallel_cc_publish_test_${os.getpid()}')
	os.rmdir_all(root) or {}
	build_dir := os.join_path_single(root, 'build')
	cache_dir := os.join_path_single(root, 'cache')
	os.mkdir_all(build_dir)!
	os.mkdir_all(cache_dir)!
	defer {
		os.rmdir_all(root) or {}
	}
	local_object := os.join_path_single(build_dir, 'unit.o')
	cache_object := os.join_path_single(cache_dir, 'cached.o')
	os.write_file(local_object, 'complete object')!
	os.write_file(cache_object, 'old object')!
	publish_v3_parallel_c_cache_object(local_object, cache_object)!
	assert os.read_file(cache_object)! == 'complete object'
	assert os.read_file(local_object)! == 'complete object'
	assert os.ls(cache_dir)! == ['cached.o']
}

fn test_v3_parallel_cc_compiles_and_runs_multiple_c_units() {
	$if bsd || linux {
		root := os.join_path(os.vtmp_dir(), 'v3_parallel_cc_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path_single(root, 'main.v')
		output := os.join_path_single(root, 'main')
		// The runtime headers shipped with V do not keep a program in one unit.
		os.write_file(source, 'fn twice(value int) int { return value * 2 }\nfn main() { println(twice(21)) }\n')!
		build := cmdexec.run(v3_parallel_cc_test_compiler(), ['-parallel-cc', '-cc', 'cc', '-nocache',
			'-showcc', '-o', output, source])
		assert build.exit_code == 0, build.output
		assert build.output.contains('unit_0.c'), build.output
		assert build.output.contains('unit_1.c'), build.output
		run_result := cmdexec.run(output, [])
		assert run_result.exit_code == 0, run_result.output
		assert run_result.output.trim_space() == '42'
	}
}

fn test_v3_parallel_cc_compiles_and_runs_a_split_c_fixture() {
	$if bsd || linux {
		root := os.join_path(os.vtmp_dir(), 'v3_parallel_cc_fixture_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path_single(root, 'main.c')
		build_dir := os.join_path_single(root, 'build')
		os.mkdir_all(build_dir)!
		output := os.join_path_single(root, 'main')
		// Keep this fixture independent of native includes in the V runtime.
		os.write_file(source, '#include <stdio.h>\nint value(void);\nint twice(int value);\n/* V3CACHE_BODY_BEGIN */\n/* V3PARALLEL_CC_UNIT */\nint value(void) { return 21; }\n/* V3PARALLEL_CC_UNIT */\nint twice(int value) { return value * 2; }\nint main(void) { printf("%d\\n", twice(value())); return 0; }\n/* V3CACHE_BODY_END */\n')!
		plan := V3CCompilerFlagPlan{
			before_inputs: ['-std=gnu11', '-fPIC']
		}
		mut stats := CObjectCacheStats{}
		build := compile_v3_parallel_c(source, 'cc', &plan, &plan, []string{}, []string{}, '',
			false, build_dir, output, false, 2, 2, false, false, pref.host_target(), @VEXEROOT, mut
			stats)
		assert build.exit_code == 0, build.output
		assert os.is_file(os.join_path(build_dir, 'unit_0.c'))
		assert os.is_file(os.join_path(build_dir, 'unit_1.c'))
		assert os.is_file(os.join_path(build_dir, 'unit_2.c'))
		run_result := cmdexec.run(output, [])
		assert run_result.exit_code == 0, run_result.output
		assert run_result.output.trim_space() == '42'
	}
}

fn test_v3_parallel_cc_falls_back_for_uninspected_insert() {
	$if bsd || linux {
		root := os.join_path(os.vtmp_dir(), 'v3_parallel_cc_insert_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path_single(root, 'main.v')
		header := os.join_path_single(root, 'implementation.h')
		output := os.join_path_single(root, 'main')
		os.write_file(header, 'int v3_parallel_implementation(void) { return 21; }\n')!
		os.write_file(source, '#insert "@DIR/implementation.h"\n\nfn C.v3_parallel_implementation() int\n\nfn twice(value int) int { return value * 2 }\nfn main() { println(twice(C.v3_parallel_implementation())) }\n')!
		build := cmdexec.run(v3_parallel_cc_test_compiler(), ['-parallel-cc', '-cc', 'cc', '-nocache',
			'-showcc', '-o', output, source])
		assert build.exit_code == 0, build.output
		assert !build.output.contains('unit_0.c'), build.output
		assert build.output.contains('src.c'), build.output
		run_result := cmdexec.run(output, [])
		assert run_result.exit_code == 0, run_result.output
		assert run_result.output.trim_space() == '42'
	}
}

fn test_v3_parallel_cc_keeps_test_binaries_in_one_unit() {
	$if bsd || linux {
		root := os.join_path(os.vtmp_dir(), 'v3_parallel_cc_test_harness_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path_single(root, 'harness_test.v')
		output := os.join_path_single(root, 'harness')
		os.write_file(source, 'fn twice(value int) int { return value * 2 }\n\nfn test_twice() { assert twice(21) == 42 }\n')!
		// The harness counters `assert` writes are `static` definitions inside the
		// program body, so a split unit that only references them would not
		// compile. The build has to stay in one translation unit.
		build := cmdexec.run(v3_parallel_cc_test_compiler(), ['-parallel-cc', '-cc', 'cc', '-nocache',
			'-showcc', '-o', output, source])
		assert build.exit_code == 0, build.output
		assert !build.output.contains('unit_1.c'), build.output
		run_result := cmdexec.run(output, [])
		assert run_result.exit_code == 0, run_result.output
	}
}

fn test_v3_parallel_cc_does_not_shadow_user_parallel_header() {
	$if bsd || linux {
		cc := os.find_abs_path_of_executable('clang') or { return }
		root := os.join_path(os.vtmp_dir(), 'v3_parallel_cc_header_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path_single(root, 'main.v')
		header := os.join_path_single(root, 'parallel.h')
		output := os.join_path_single(root, 'main')
		os.write_file(header, '#ifndef V3_USER_PARALLEL_H\n#define V3_USER_PARALLEL_H\nstatic inline int v3_user_parallel_header_value(void) { return 42; }\n#endif\n')!
		os.write_file(source, '#flag -I @DIR\n#include "parallel.h"\n\nfn C.v3_user_parallel_header_value() int\n\nfn main() { println(C.v3_user_parallel_header_value()) }\n')!
		build := cmdexec.run(v3_parallel_cc_test_compiler(), ['-parallel-cc', '-cc', cc, '-nocache',
			'-showcc', '-o', output, source])
		assert build.exit_code == 0, build.output
		// A header with only an inline function is the same in every unit, so the
		// program splits, and its units still find the user's `parallel.h`.
		assert build.output.contains('unit_1.c'), build.output
		run_result := cmdexec.run(output, [])
		assert run_result.exit_code == 0, run_result.output
		assert run_result.output.trim_space() == '42'
	}
}

fn test_v3_parallel_cc_falls_back_for_native_static_state() {
	$if bsd || linux {
		root := os.join_path(os.vtmp_dir(), 'v3_parallel_cc_static_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path_single(root, 'main.v')
		header := os.join_path_single(root, 'state.h')
		output := os.join_path_single(root, 'main')
		os.write_file(header, 'static int v3_parallel_state;\nstatic inline int v3_parallel_next(void) { return ++v3_parallel_state; }\n')!
		os.write_file(source, '#flag -I @DIR\n#include <state.h>\n\nfn C.v3_parallel_next() int\n\nfn first() int { return C.v3_parallel_next() }\nfn second() int { return C.v3_parallel_next() }\nfn main() { println(first())\nprintln(second()) }\n')!
		build := cmdexec.run(v3_parallel_cc_test_compiler(), ['-parallel-cc', '-cc', 'cc', '-nocache',
			'-showcc', '-o', output, source])
		assert build.exit_code == 0, build.output
		assert !build.output.contains('unit_0.c'), build.output
		assert build.output.contains('src.c'), build.output
		run_result := cmdexec.run(output, [])
		assert run_result.exit_code == 0, run_result.output
		assert run_result.output.trim_space() == '1\n2'
	}
}

fn test_v3_parallel_cc_require_split_refuses_instead_of_one_c_unit() {
	$if !windows {
		root := os.join_path(os.vtmp_dir(), 'v3_parallel_cc_require_split_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		old_value := os.getenv_opt(v3_parallel_cc_require_split_env)
		os.setenv(v3_parallel_cc_require_split_env, '1', true)
		defer {
			if value := old_value {
				os.setenv(v3_parallel_cc_require_split_env, value, true)
			} else {
				os.unsetenv(v3_parallel_cc_require_split_env)
			}
			os.rmdir_all(root) or {}
		}
		source := os.join_path_single(root, 'main.v')
		header := os.join_path_single(root, 'state.h')
		output := os.join_path_single(root, 'main')
		os.write_file(header, 'static int v3_parallel_state;\nstatic inline int v3_parallel_next(void) { return ++v3_parallel_state; }\n')!
		os.write_file(source, '#flag -I @DIR\n#include "state.h"\n\nfn C.v3_parallel_next() int\n\nfn first() int { return C.v3_parallel_next() }\nfn second() int { return C.v3_parallel_next() }\nfn main() { println(first())\nprintln(second()) }\n')!
		build := cmdexec.run(v3_parallel_cc_test_compiler(), ['-parallel-cc', '-cc', 'cc', '-nocache',
			'-o', output, source])
		assert build.exit_code == v3_parallel_cc_split_refused_exit_code, build.output
		assert build.output.contains('cannot split this program into C units'), build.output
		assert build.output.contains('state.h'), build.output
		assert !os.exists(output)
		object_build := cmdexec.run(v3_parallel_cc_test_compiler(), ['-parallel-cc', '-cc', 'cc',
			'-nocache', '-o', output + '.o', source])
		assert object_build.exit_code == v3_parallel_cc_split_refused_exit_code, object_build.output
		assert object_build.output.contains('selected compiler or build options'), object_build.output
		assert !os.exists(output + '.o')
	}
}

fn test_v3_parallel_cc_falls_back_for_native_function_local_static_state() {
	$if bsd || linux {
		root := os.join_path(os.vtmp_dir(), 'v3_parallel_cc_local_static_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path_single(root, 'main.v')
		header := os.join_path_single(root, 'state.h')
		output := os.join_path_single(root, 'main')
		os.write_file(header, 'static inline int v3_parallel_local_next(void) {\n\tstatic int state;\n\treturn ++state;\n}\n')!
		os.write_file(source, '#flag -I @DIR\n#include "state.h"\n\nfn C.v3_parallel_local_next() int\n\nfn first() int { return C.v3_parallel_local_next() }\nfn second() int { return C.v3_parallel_local_next() }\nfn main() { println(first())\nprintln(second()) }\n')!
		build := cmdexec.run(v3_parallel_cc_test_compiler(), ['-parallel-cc', '-cc', 'cc', '-nocache',
			'-showcc', '-o', output, source])
		assert build.exit_code == 0, build.output
		assert !build.output.contains('unit_0.c'), build.output
		assert build.output.contains('src.c'), build.output
		run_result := cmdexec.run(output, [])
		assert run_result.exit_code == 0, run_result.output
		assert run_result.output.trim_space() == '1\n2'
	}
}

fn test_v3_parallel_cc_falls_back_for_macro_generated_function_local_static_state() {
	$if bsd || linux {
		root := os.join_path(os.vtmp_dir(), 'v3_parallel_cc_macro_static_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path_single(root, 'main.v')
		header := os.join_path_single(root, 'state.h')
		output := os.join_path_single(root, 'main')
		os.write_file(header, '#define DEF(name) \\
	static inline int name(void) { \\
		static int state; \\
		return ++state; \\
	}
DEF(v3_parallel_macro_next)
')!
		os.write_file(source, '#flag -I @DIR\n#include "state.h"\n\nfn C.v3_parallel_macro_next() int\n\nfn first() int { return C.v3_parallel_macro_next() }\nfn second() int { return C.v3_parallel_macro_next() }\nfn main() { println(first())\nprintln(second()) }\n')!
		build := cmdexec.run(v3_parallel_cc_test_compiler(), ['-parallel-cc', '-cc', 'cc', '-nocache',
			'-showcc', '-o', output, source])
		assert build.exit_code == 0, build.output
		assert !build.output.contains('unit_0.c'), build.output
		assert build.output.contains('src.c'), build.output
		run_result := cmdexec.run(output, [])
		assert run_result.exit_code == 0, run_result.output
		assert run_result.output.trim_space() == '1\n2'
	}
}

fn test_v3_parallel_cc_falls_back_for_coverage_and_profile_state() {
	$if bsd || linux {
		root := os.join_path(os.vtmp_dir(), 'v3_parallel_cc_instrumentation_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path_single(root, 'main.v')
		os.write_file(source, "println('42')\n")!
		for mode in ['coverage', 'profile'] {
			output := os.join_path_single(root, 'main_${mode}')
			state_path := os.join_path_single(root, mode)
			option := if mode == 'coverage' { '-coverage' } else { '-profile' }
			build := cmdexec.run(v3_parallel_cc_test_compiler(), ['-parallel-cc', '-cc', 'cc',
				option, state_path, '-nocache', '-showcc', '-o', output, source])
			assert build.exit_code == 0, build.output
			assert !build.output.contains('unit_0.c'), build.output
			assert build.output.contains('src.c'), build.output
			run_result := cmdexec.run(output, [])
			assert run_result.exit_code == 0, run_result.output
			assert run_result.output.trim_space() == '42'
			if mode == 'coverage' {
				counter_files := os.walk_ext(state_path, '.csv')
				assert counter_files.len > 0, 'missing coverage counters in ${state_path}: ${os.walk_ext(root, '')}'
				counter_data := os.read_file(counter_files[0])!
				assert counter_data.split_into_lines().any(it.len > 0 && it[0].is_hex_digit())
			} else {
				assert os.is_file(state_path)
				assert os.file_size(state_path) > 0
			}
		}
	}
}

fn test_v3_parallel_cc_compiles_native_source_callers_in_the_first_unit() {
	// Objective-C sources are the native sources that only the first unit includes.
	$if macos {
		root := os.join_path(os.vtmp_dir(), 'v3_parallel_cc_native_source_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path_single(root, 'main.v')
		native := os.join_path_single(root, 'native.m')
		output := os.join_path_single(root, 'main')
		// Only the first unit compiles a native source: its static helper, its
		// external function and its macro are not declared anywhere else.
		os.write_file(native, '#define NATIVE_SCALE 3\nstatic int native_helper(int x) { return x * NATIVE_SCALE; }\nint native_triple(int x) { return native_helper(x); }\n')!
		os.write_file(source, '#include "@DIR/native.m"\n\nfn C.native_triple(x int) int\nfn C.native_helper(x int) int\n\nfn helper(x int) int { return C.native_helper(x) }\nfn triple(x int) int { return C.native_triple(x) }\nfn twice(x int) int { return x * 2 }\nfn main() { println(triple(7) + helper(2) + twice(3)) }\n')!
		build := cmdexec.run(v3_parallel_cc_test_compiler(), ['-parallel-cc', '-cc', 'cc', '-nocache',
			'-showcc', '-o', output, source])
		assert build.exit_code == 0, build.output
		assert build.output.contains('unit_0.c'), build.output
		assert build.output.contains('unit_1.c'), build.output
		run_result := cmdexec.run(output, [])
		assert run_result.exit_code == 0, run_result.output
		assert run_result.output.trim_space() == '33'
	}
}

fn test_v3_parallel_cc_defines_embedded_payloads_once() {
	$if bsd || linux || macos {
		root := os.join_path(os.vtmp_dir(), 'v3_parallel_cc_embed_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path_single(root, 'main.v')
		output := os.join_path_single(root, 'main')
		// One payload fits a single C object, the other is split into chunks.
		mut small := []u8{len: 10000}
		mut large := []u8{len: 200000}
		for i in 0 .. small.len {
			small[i] = u8(i * 7)
		}
		for i in 0 .. large.len {
			large[i] = u8(i * 13 + 1)
		}
		os.write_file_array(os.join_path_single(root, 'small.bin'), small)!
		os.write_file_array(os.join_path_single(root, 'large.bin'), large)!
		os.write_file(source, "fn sum(data []u8) u64 {\n\tmut total := u64(0)\n\tfor b in data {\n\t\ttotal += b\n\t}\n\treturn total\n}\n\nfn small_sum() u64 {\n\tfile := \$embed_file('small.bin')\n\treturn sum(file.to_bytes())\n}\n\nfn large_sum() u64 {\n\tfile := \$embed_file('large.bin')\n\treturn sum(file.to_bytes())\n}\n\nfn main() { println('\${small_sum()} \${large_sum()}') }\n")!
		mut expected_small := u64(0)
		mut expected_large := u64(0)
		for b in small {
			expected_small += b
		}
		for b in large {
			expected_large += b
		}
		build := cmdexec.run(v3_parallel_cc_test_compiler(), ['-parallel-cc', '-cc', 'cc', '-nocache',
			'-showcc', '-o', output, source])
		assert build.exit_code == 0, build.output
		assert build.output.contains('unit_1.c'), build.output
		run_result := cmdexec.run(output, [])
		assert run_result.exit_code == 0, run_result.output
		assert run_result.output.trim_space() == '${expected_small} ${expected_large}'
	}
}

// Check native ownership through both the ordinary and requested split build paths.
fn run_v3_parallel_cc_native_ownership_case(name string, header string, program string, expected string, expect_split bool) ! {
	$if bsd || linux || macos {
		cc := os.find_abs_path_of_executable('clang') or { return }
		root := os.join_path(os.vtmp_dir(), 'v3_parallel_cc_ownership_${name}_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		source := os.join_path(root, 'main.c.v')
		output := os.join_path(root, 'main')
		os.write_file(os.join_path(root, 'native.h'), header)!
		os.write_file(source, '#include "@DIR/native.h"\n' + program)!
		for parallel in [false, true] {
			mut args := ['-cc', cc, '-gc', 'none', '-nocache', '-no-retry-compilation', '-showcc',
				'-o', output]
			if parallel { args << '-parallel-cc' }
			args << source
			build := cmdexec.run(v3_parallel_cc_test_compiler(), args)
			assert build.exit_code == 0, '${name}, parallel=${parallel}: ${build.output}'
			if parallel {
				assert build.output.contains('unit_1.c') == expect_split, build.output
				if !expect_split {
					assert build.output.contains('src.c'), build.output
				}
			}
			run_result := cmdexec.run(output, []string{})
			assert run_result.exit_code == 0, '${name}, parallel=${parallel}: ${run_result.output}'
			assert run_result.output.trim_space() == expected, run_result.output
		}
	}
}

fn test_v3_parallel_cc_keeps_native_external_definitions_in_one_unit() {
	program := 'fn C.native_value() int\nfn main() { println(C.native_value()) }\n'
	run_v3_parallel_cc_native_ownership_case('extern_initializer',
		'extern int counter = 3;\nstatic inline int native_value(void) { return counter + 4; }\n',
		program, '7', false)!
	run_v3_parallel_cc_native_ownership_case('extern_inline',
		'extern inline int native_value(void) { return 7; }\n', program, '7', false)!
}

fn test_v3_parallel_cc_can_split_unused_native_constants() {
	run_v3_parallel_cc_native_ownership_case('unused_constant',
		'static const int sentinel = 7;\n#define UNUSED_ADDRESS() (&sentinel)\nstatic inline int native_value(void) { return 7; }\n',
		'fn C.native_value() int\nfn main() { println(C.native_value()) }\n', '7', true)!
}

fn test_v3_parallel_cc_preserves_native_constant_address_identity() {
	program := 'fn C.native_identity() voidptr
fn first() voidptr { return C.native_identity() }
fn second() voidptr { return C.native_identity() }
fn main() {
	assert first() == second()
	println("same identity")
}
'
	run_v3_parallel_cc_native_ownership_case('global_constant',
		'static const int sentinel = 7;\nstatic inline void *native_identity(void) { return (void*)&sentinel; }\n',
		program, 'same identity', false)!
	run_v3_parallel_cc_native_ownership_case('local_constant',
		'static inline void *native_identity(void) { static const int sentinel = 7; return (void*)&sentinel; }\n',
		program, 'same identity', false)!
	run_v3_parallel_cc_native_ownership_case('macro_constant',
		'#define NATIVE_SENTINEL(name) static const int name = 7;\nNATIVE_SENTINEL(sentinel)\nstatic inline void *native_identity(void) { return (void*)&sentinel; }\n',
		program, 'same identity', false)!
	run_v3_parallel_cc_native_ownership_case('macro_accessor',
		'static const int sentinel = 7;\n#define native_identity() ((void*)&sentinel)\n',
		program, 'same identity', false)!
	run_v3_parallel_cc_native_ownership_case('pasted_accessor',
		'static const int sentinel = 7;\n#define JOIN(a,b) a##b\n#define native_identity() ((void*)&JOIN(senti,nel))\n',
		program, 'same identity', false)!
}
