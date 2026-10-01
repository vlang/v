// vtest vflags: -d v3_no_parallel
module types

import os
import strings
import v.parser
import v.pref

// In a `v3_no_parallel` build the checker must not run any of its work on the
// worker pool, even when the driver has started one, and the serial check must
// still reach the same result the parallel one does.
fn test_no_parallel_build_keeps_checker_work_off_a_live_pool() {
	old_vjobs := os.getenv_opt('VJOBS')
	os.setenv('VJOBS', '4', true)
	defer {
		if value := old_vjobs {
			os.setenv('VJOBS', value, true)
		} else {
			os.unsetenv('VJOBS')
		}
	}
	// Large enough to pass the checker's parallel thresholds for work items,
	// top-level declarations and interfaces.
	mut source := strings.new_builder(256_000)
	source.writeln('module main')
	for i in 0 .. 10 {
		source.writeln('interface Shape_${i} { area() int }')
		source.writeln('struct Box_${i} { side int }')
		source.writeln('fn (b Box_${i}) area() int { return b.side * ${i} }')
	}
	for i in 0 .. 1100 {
		source.writeln('fn dependency_${i}() int { return ${i} }')
		source.writeln('fn caller_${i}(flag bool) int { value := if flag { dependency_${i}() } else { ${i} }; return value }')
	}
	source.writeln('fn main() { println(caller_0(true)) }')
	path := os.join_path(os.vtmp_dir(), 'v3_serial_build_checker_${os.getpid()}.v')
	os.write_file(path, source.str()) or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	// The driver starts this pool for every build that is not `-no-parallel`.
	a.ensure_workers(3)
	defer {
		a.close_workers()
	}
	mut tc := TypeChecker.new(a)
	tc.building_v_fast = true
	tc.enable_scoped_parallel_workers()
	tc.collect(a)
	assert !tc.check_semantics_opt(true)
	assert a.worker_tasks_run() == 0
	assert tc.errors.len == 0, tc.errors.str()

	mut caller_ids := map[string]int{}
	for i, node in a.nodes {
		if node.kind == .fn_decl && node.value.starts_with('caller_') {
			caller_ids[node.value] = i
		}
	}
	for i in 0 .. 1100 {
		dependencies := tc.direct_dependencies(caller_ids['caller_${i}'])
		assert dependencies.any(it == 'dependency_${i}' || it.ends_with('.dependency_${i}')), 'caller_${i}: ${dependencies}'
	}
}
