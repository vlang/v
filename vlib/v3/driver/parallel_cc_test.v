module driver

import os
import v3.cmdexec

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

fn test_v3_parallel_cc_compiles_and_runs_multiple_c_units() {
	$if macos || linux {
		root := os.join_path(os.vtmp_dir(), 'v3_parallel_cc_${os.getpid()}')
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
		build := cmdexec.run(@VEXE, ['-parallel-cc', '-nocache', '-showcc', '-o', output, source])
		assert build.exit_code == 0, build.output
		assert build.output.contains('unit_0.c')
		assert build.output.contains('unit_1.c')
		run_result := cmdexec.run(output, [])
		assert run_result.exit_code == 0, run_result.output
		assert run_result.output.trim_space() == '42'
	}
}

fn test_v3_parallel_cc_keeps_test_binaries_in_one_unit() {
	$if macos || linux {
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
		build := cmdexec.run(@VEXE, ['-parallel-cc', '-nocache', '-showcc', '-o', output,
			source])
		assert build.exit_code == 0, build.output
		assert !build.output.contains('unit_1.c'), build.output
		run_result := cmdexec.run(output, [])
		assert run_result.exit_code == 0, run_result.output
	}
}

fn test_v3_parallel_cc_does_not_shadow_user_parallel_header() {
	$if macos || linux {
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
		build := cmdexec.run(@VEXE, ['-parallel-cc', '-nocache', '-showcc', '-o', output, source])
		assert build.exit_code == 0, build.output
		assert build.output.contains('unit_0.c')
		run_result := cmdexec.run(output, [])
		assert run_result.exit_code == 0, run_result.output
		assert run_result.output.trim_space() == '42'
	}
}

fn test_v3_parallel_cc_falls_back_for_native_static_state() {
	$if macos || linux {
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
		os.write_file(source, '#flag -I @DIR\n#include "state.h"\n\nfn C.v3_parallel_next() int\n\nfn first() int { return C.v3_parallel_next() }\nfn second() int { return C.v3_parallel_next() }\nfn main() { println(first())\nprintln(second()) }\n')!
		build := cmdexec.run(@VEXE, ['-parallel-cc', '-nocache', '-showcc', '-o', output, source])
		assert build.exit_code == 0, build.output
		assert !build.output.contains('unit_0.c'), build.output
		assert build.output.contains('src.c'), build.output
		run_result := cmdexec.run(output, [])
		assert run_result.exit_code == 0, run_result.output
		assert run_result.output.trim_space() == '1\n2'
	}
}

fn test_v3_parallel_cc_falls_back_for_native_function_local_static_state() {
	$if macos || linux {
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
		build := cmdexec.run(@VEXE, ['-parallel-cc', '-nocache', '-showcc', '-o', output, source])
		assert build.exit_code == 0, build.output
		assert !build.output.contains('unit_0.c'), build.output
		assert build.output.contains('src.c'), build.output
		run_result := cmdexec.run(output, [])
		assert run_result.exit_code == 0, run_result.output
		assert run_result.output.trim_space() == '1\n2'
	}
}

fn test_v3_parallel_cc_falls_back_for_macro_generated_function_local_static_state() {
	$if macos || linux {
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
		build := cmdexec.run(@VEXE, ['-parallel-cc', '-nocache', '-showcc', '-o', output,
			source])
		assert build.exit_code == 0, build.output
		assert !build.output.contains('unit_0.c'), build.output
		assert build.output.contains('src.c'), build.output
		run_result := cmdexec.run(output, [])
		assert run_result.exit_code == 0, run_result.output
		assert run_result.output.trim_space() == '1\n2'
	}
}

fn test_v3_parallel_cc_falls_back_for_coverage_and_profile_state() {
	$if macos || linux {
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
			build := cmdexec.run(@VEXE, ['-parallel-cc', option, state_path, '-nocache', '-showcc',
				'-o', output, source])
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
				assert counter_data.split_into_lines().any(it.len > 0 && it[0].is_digit())
			} else {
				assert os.is_file(state_path)
				assert os.file_size(state_path) > 0
			}
		}
	}
}
