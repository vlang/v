module driver

import os

fn test_cached_float_interpolation_preserves_its_allocation_budget() ! {
	$if !macos && !linux {
		return
	}
	if _ := os.find_abs_path_of_executable('clang') {
	} else {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'cached_float_interpolation_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	cache := os.join_path(root, 'cache')
	source := os.join_path(root, 'main.v')
	binary := os.join_path(root, 'probe')
	program := 'fn format_values(a f64, b f64, c f64, d f64) string {
	return "rect|\${a:.1f}|\${b:.1f}|\${c:.1f}|\${d:.1f}|\${a:.4f}|\${b:.4f}|\${c:.4f}|\${d:.4f}|\${a:.1f}|\${b:.1f}"
}
fn main() {
	before := gc_heap_usage().total_bytes
	mut length := 0
	for _ in 0 .. 1000 {
		text := format_values(10.3, 0.123456, 200.25, 7.0)
		length += text.len
	}
	after := gc_heap_usage().total_bytes
	println("\${length}:\${after - before}")
	println(format_values(10.3, 0.123456, 200.25, 7.0))
}'
	for phase in ['cold', 'warm'] {
		// Change a program symbol so the warm build generates C again while reusing
		// the same cached module interfaces and their pruned declarations.
		os.write_file(source, program.replace('format_values', 'format_values_${phase}'))!
		compiled := os.exec(['env', '-u', 'VFLAGS', '-u', 'V3_INTERNAL_CACHE_FORCE_SOURCE', '-u',
			'V3_CACHE_FORCE_SOURCE', '-u', 'V3_CACHE_ALL_DECLARATIONS', 'V3CACHE=' + cache,
			'V3_CACHE_TRACE=1', 'V3_CACHE_DISABLE_PROGRAM_EXECUTABLE=1',
			'V3_DISABLE_UNCHANGED_BUILD_CACHE=1', @VEXE, '-prod', '-cc', 'clang', '-gc', 'boehm',
			'-d', 'use_bundled_libgc', '-show-timings', '-o', binary, source])
		assert compiled.exit_code == 0, '${phase}: ${compiled.output}'
		if phase == 'warm' {
			assert compiled.output.contains('parsed .vh files'), compiled.output
			assert !compiled.output.contains('starting again with all of them'), compiled.output
			pruned := compiled.output.all_after('V3 cached declarations: left out ').all_before(' functions').int()
			assert pruned > 0, compiled.output
		}
		run := os.exec([binary])
		assert run.exit_code == 0, '${phase}: ${run.output}'
		lines := run.output.trim_space().split_into_lines()
		assert lines.len == 2, run.output
		assert lines[1] == 'rect|10.3|0.1|200.3|7.0|10.3000|0.1235|200.2500|7.0000|10.3|0.1', run.output
		counts := lines[0].split(':')
		assert counts.len == 2, run.output
		assert counts[0] == '63000', run.output
		// The result may allocate; ten per-hole temporary strings must not.
		assert counts[1].u64() <= 256000, '${phase}: ${run.output}'
	}
}
