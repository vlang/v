module c

import os
import strings
import v.cmdexec
import v.markused
import v.parser
import v.pref
import v.transform
import v.types
import v.workers

enum CgenModePool {
	lazy
	empty
	available
	failed
}

struct CgenModeOutput {
	code         string
	literals     []string
	fn_names     []string
	scoped       bool
	parallel     bool
	pool_present bool
	pool_size    int
	pool_tasks   u64
mut:
	launches      u64
	launch_errors u64
}

fn cgen_mode_restore_env(name string, previous ?string) {
	if value := previous {
		os.setenv(name, value, true)
	} else {
		os.unsetenv(name)
	}
}

fn cgen_mode_source() string {
	mut source := strings.new_builder(20_000)
	source.writeln('module main

const source_label = "constant literal"

__global runtime_label = initial_label()

struct Holder {
	label string = "field default literal"
}

type LabelFn = fn () string

fn initial_label() string { return "global initializer literal" }
fn fixed_row() [2]int { return [1, 2]! }
fn pair() (int, string) { return 3, "tuple literal" }
fn optional_value() ?int { return 4 }
')
	// Exceed the scoped-body threshold so this compares the serial preparation
	// path with the default dispatcher, including chunk publication and remapping.
	for i in 0 .. 130 {
		source.writeln('fn helper_${i}() string { return "body literal ${i}" }')
	}
	source.writeln('fn main() {
	holder := Holder{}
	row := fixed_row()
	value, label := pair()
	optional := optional_value() or { 0 }
	callback := LabelFn(helper_0)
	_ = callback()
	_ = holder.label
	_ = row[0] + value + optional
	_ = label
	_ = source_label
	_ = runtime_label
')
	for i in 0 .. 130 {
		source.writeln('\t_ = helper_${i}()')
	}
	source.writeln('}')
	return source.str()
}

fn cgen_mode_generate(path string, pool_mode CgenModePool, no_parallel bool) CgenModeOutput {
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.enable_globals = true
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	transform.transform(mut a, &tc)
	tc.annotate_types()
	used, _ := markused.mark_all_used_with_generic_usage(a, tc, [])
	match pool_mode {
		.lazy {}
		.empty { a.worker_pool = workers.new(0) }
		.available { a.worker_pool = workers.new(2) }
		.failed {
			previous := os.getenv_opt('V3_TEST_PTHREAD_CREATE_FAIL')
			os.setenv('V3_TEST_PTHREAD_CREATE_FAIL', 'pool:all', true)
			a.worker_pool = workers.new(2)
			cgen_mode_restore_env('V3_TEST_PTHREAD_CREATE_FAIL', previous)
		}
	}
	defer { a.close_workers() }
	mut g := FlatGen.new()
	g.set_skip_generics(true)
	g.set_scope_parallel_workers(true)
	code := g.gen_with_used_options(a, used, &tc, no_parallel)
	mut result := CgenModeOutput{
		code:         code
		literals:     g.str_lits.clone()
		fn_names:     g.used_fn_names.clone()
		scoped:       g.scope_parallel_workers
		parallel:     g.parallel_used
		pool_present: !isnil(a.worker_pool)
		pool_size:    a.worker_count()
		pool_tasks:   a.worker_tasks_run()
	}
	if !isnil(a.worker_pool) {
		stats := a.worker_pool.stats()
		result.launches = stats.launch_attempts
		result.launch_errors = stats.launch_failures
	}
	return result
}

fn test_empty_codegen_pool_preserves_serial_literals_and_scoped_bodies() {
	previous_jobs := os.getenv_opt('VJOBS')
	os.setenv('VJOBS', '4', true)
	defer { cgen_mode_restore_env('VJOBS', previous_jobs) }
	path := os.join_path(os.vtmp_dir(), 'cgen_empty_pool_${os.getpid()}.v')
	os.write_file(path, cgen_mode_source()) or { panic(err) }
	defer { os.rm(path) or {} }
	serial := cgen_mode_generate(path, .empty, true)
	default_mode := cgen_mode_generate(path, .empty, false)
	assert default_mode.code == serial.code
	assert default_mode.literals == serial.literals
	assert default_mode.fn_names == serial.fn_names
	assert default_mode.scoped
	assert !default_mode.parallel
	assert default_mode.pool_present
	assert default_mode.pool_size == 0
	assert default_mode.pool_tasks == 0
	$if !v3_no_parallel ? {
		// Runtime serial preparation interns the complete AST literal table.
		assert default_mode.literals.contains('constant literal')
	}
	// Constants use inline storage even when complete-AST interning is compiled out.
	assert default_mode.code.contains('source_label = (string){"constant literal", 16, 1};')
	field_literal_id := default_mode.literals.index('field default literal')
	assert field_literal_id >= 0
	assert default_mode.code.contains('.label = _str_${field_literal_id}')
	assert default_mode.literals.contains('field default literal')
	assert default_mode.literals.contains('body literal 129')
	assert default_mode.code.contains(' fixed_row(')
	assert default_mode.code.contains(' optional_value(')
	// Completed generation drains the scratch segments into the returned C.
	// Check that every published body still refers to its owned literal table.
	for i in 0 .. 130 {
		header := 'string helper_${i}(void) {'
		assert default_mode.code.contains(header), header
		body := default_mode.code.all_after_last(header).all_before('\n}')
		literal_id := default_mode.literals.index('body literal ${i}')
		assert literal_id >= 0
		assert body.contains('return _str_${literal_id};'), body
	}
}

fn test_codegen_pool_launch_failure_uses_the_same_serial_preparation() {
	previous_jobs := os.getenv_opt('VJOBS')
	os.setenv('VJOBS', '4', true)
	defer { cgen_mode_restore_env('VJOBS', previous_jobs) }
	path := os.join_path(os.vtmp_dir(), 'cgen_failed_pool_${os.getpid()}.v')
	os.write_file(path, cgen_mode_source()) or { panic(err) }
	defer { os.rm(path) or {} }
	serial := cgen_mode_generate(path, .empty, true)
	failed := cgen_mode_generate(path, .failed, false)
	assert failed.code == serial.code
	assert failed.literals == serial.literals
	assert failed.fn_names == serial.fn_names
	assert failed.launches == 2
	assert failed.launch_errors == 2
	assert failed.pool_size == 0
	assert failed.pool_tasks == 0
	assert failed.scoped
	assert !failed.parallel
}

fn test_one_job_codegen_keeps_a_lazy_pool_and_matches_explicit_serial() {
	previous_jobs := os.getenv_opt('VJOBS')
	os.setenv('VJOBS', '1', true)
	defer { cgen_mode_restore_env('VJOBS', previous_jobs) }
	path := os.join_path(os.vtmp_dir(), 'cgen_lazy_pool_${os.getpid()}.v')
	os.write_file(path, cgen_mode_source()) or { panic(err) }
	defer { os.rm(path) or {} }
	serial := cgen_mode_generate(path, .lazy, true)
	default_mode := cgen_mode_generate(path, .lazy, false)
	assert default_mode.code == serial.code
	assert default_mode.literals == serial.literals
	assert default_mode.fn_names == serial.fn_names
	assert !default_mode.pool_present
	assert !default_mode.parallel
	assert default_mode.scoped
}

fn test_codegen_keeps_parallel_dispatch_when_workers_are_available() {
	previous_jobs := os.getenv_opt('VJOBS')
	os.setenv('VJOBS', '4', true)
	defer { cgen_mode_restore_env('VJOBS', previous_jobs) }
	path := os.join_path(os.vtmp_dir(), 'cgen_available_pool_${os.getpid()}.v')
	os.write_file(path, cgen_mode_source()) or { panic(err) }
	defer { os.rm(path) or {} }
	serial := cgen_mode_generate(path, .empty, true)
	available := cgen_mode_generate(path, .available, false)
	assert available.code == serial.code
	assert available.literals == serial.literals
	assert available.fn_names == serial.fn_names
	assert available.scoped
	$if v3_no_parallel ? {
		assert !available.parallel
		assert available.pool_tasks == 0
	} $else {
		if available.pool_size > 0 {
			assert available.parallel
			assert available.pool_tasks > 0
		}
	}
}

fn test_multijob_codegen_preserves_lazy_pool_creation() {
	previous_jobs := os.getenv_opt('VJOBS')
	os.setenv('VJOBS', '4', true)
	defer { cgen_mode_restore_env('VJOBS', previous_jobs) }
	path := os.join_path(os.vtmp_dir(), 'cgen_multijob_lazy_pool_${os.getpid()}.v')
	os.write_file(path, cgen_mode_source()) or { panic(err) }
	defer { os.rm(path) or {} }
	serial := cgen_mode_generate(path, .lazy, true)
	default_mode := cgen_mode_generate(path, .lazy, false)
	assert default_mode.code == serial.code
	assert default_mode.literals == serial.literals
	assert !serial.pool_present
	assert default_mode.scoped
	$if v3_no_parallel ? {
		assert !default_mode.pool_present
		assert !default_mode.parallel
	} $else {
		assert default_mode.pool_present
		if default_mode.pool_size > 0 {
			assert default_mode.parallel
		}
	}
}

// The generated C must not depend on how many jobs the compiler uses. Later
// stages append synthesized nodes, here the `typeof(x).name` literals of the
// `name_of[T]` instances, in the order their parallel workers finish; only
// function bodies use the closures' `fn (i64) i64` type, which a declaration
// task running beside the body workers cannot see; and the serial transform
// reaches the `Shape` equality helper with a temporary counter that the
// consts advanced.
fn test_generated_c_does_not_depend_on_the_job_count() {
	test_dir := os.join_path(os.vtmp_dir(), 'cgen_job_count_independent_${os.getpid()}')
	os.mkdir_all(test_dir) or { panic(err) }
	defer {
		os.rmdir_all(test_dir) or {}
	}
	// Enough functions for the parallel transform and the parallel cgen dispatch.
	fn_count := 160
	mut source := strings.new_builder(60_000)
	source.writeln('module main

struct Point {
	x int
	y string
}

type Shape = Point | int

const points = [Point{
	x: 1
	y: "a"
}]
const lookup = {
	"a": 1
}

fn name_of[T](x T) string {
	return typeof(x).name
}

fn same(a Shape, b Shape) bool {
	return a == b
}
')
	for i in 0 .. fn_count {
		source.writeln('struct Left${i} {
	x int
}

struct Right${i} {
	y string
}

type Pick${i} = Left${i} | Right${i}

fn work_${i}(n i64) string {
	p := if n > ${i} { Pick${i}(Left${i}{ x: int(n) }) } else { Pick${i}(Right${i}{ y: "r" }) }
	add := fn [n] (x i64) i64 {
		return x + n + ${i}
	}
	return name_of(p) + "\${add(1)}"
}
')
	}
	source.writeln('fn main() {\n\tmut total := lookup["a"]')
	for i in 0 .. fn_count {
		source.writeln('\ttotal += work_${i}(${i % 7}).len')
	}
	source.writeln('\tprintln(same(Shape(points[0]), Shape(total)))\n\tprintln(total)\n}')
	v_file := os.join_path(test_dir, 'main.v')
	os.write_file(v_file, source.str()) or { panic(err) }
	mut first := ''
	for jobs in ['1', '2', '8'] {
		c_file := os.join_path(test_dir, 'main_${jobs}.c')
		generated := cmdexec.run_in_merged_with_env(@VEXE, ['-nocache', '-o', c_file, v_file],
			test_dir, {
				'VJOBS': jobs
			})
		assert generated.exit_code == 0, 'VJOBS=${jobs}: ${generated.output}'
		code := os.read_file(c_file) or { panic(err) }
		assert code.contains('"Pick${fn_count - 1}"'), 'VJOBS=${jobs}'
		assert code.contains('(*_fn_ptr_'), 'VJOBS=${jobs}'
		assert code.contains('__sum_eq_res_'), 'VJOBS=${jobs}'
		if first.len == 0 {
			first = code
			continue
		}
		assert code == first, 'VJOBS=${jobs} generated different C than VJOBS=1'
	}
}
