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
	source := '#include <stdio.h>\n/* V3CACHE_BODY_BEGIN */\n/* V3PARALLEL_CC_UNIT */\n/* V3CACHE_MODULE first */\nvoid first(void) {}\n/* V3PARALLEL_CC_UNIT */\n/* V3CACHE_MODULE second */\nvoid second(void) {}\n/* V3CACHE_BODY_END */\n'
	prefix, units := split_v3_parallel_c_source(source, 1) or { panic(err) }
	assert prefix == '#include <stdio.h>\n'
	assert units.len == 1
	assert units[0].contains('void first(void) {}')
	assert units[0].contains('void second(void) {}')
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
