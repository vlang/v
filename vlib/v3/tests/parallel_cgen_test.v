import os
import strings

const parallel_tests_dir = os.dir(@FILE)
const parallel_v3_dir = os.dir(parallel_tests_dir)
const parallel_vlib_dir = os.dir(parallel_v3_dir)
const parallel_v3_src = os.join_path(parallel_v3_dir, 'v3.v')

// build_parallel_v3 builds parallel v3 data for v3 tests.
fn build_parallel_v3() string {
	vexe := @VEXE
	v3_bin := os.join_path(os.temp_dir(), 'v3_parallel_cgen_test')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${vexe} -d parallel -path "${parallel_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${parallel_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// write_parallel_module_init_project writes parallel module init project output for v3 tests.
fn write_parallel_module_init_project(name string) string {
	project_dir := os.join_path(os.temp_dir(), 'v3_${name}')
	os.rmdir_all(project_dir) or {}
	os.mkdir_all(os.join_path(project_dir, 'moda')) or { panic(err) }

	mut main_src := strings.new_builder(64_000)
	main_src.writeln('module main')
	main_src.writeln('')
	main_src.writeln('import moda')
	main_src.writeln('')
	for i in 0 .. 1050 {
		main_src.writeln('fn helper_${i}() int {')
		main_src.writeln('\treturn ${i}')
		main_src.writeln('}')
		main_src.writeln('')
	}
	main_src.writeln('fn main() {')
	main_src.writeln('\tmut total := 0')
	for i in 0 .. 1050 {
		main_src.writeln('\ttotal += helper_${i}()')
	}
	main_src.writeln('\tprintln(int_str(total + moda.value()))')
	main_src.writeln('}')
	os.write_file(os.join_path(project_dir, 'main.v'), main_src.str()) or { panic(err) }

	os.write_file(os.join_path(project_dir, 'moda', 'moda.v'), 'module moda

__global x int

fn init() {
	x = 7
}

pub fn value() int {
	return x
}
') or {
		panic(err)
	}
	return os.join_path(project_dir, 'main.v')
}

// test_parallel_cgen_main_emits_module_init_call validates this v3 regression case.
fn test_parallel_cgen_main_emits_module_init_call() {
	v3_bin := build_parallel_v3()
	main_path := write_parallel_module_init_project('parallel_module_init')
	c_out := os.join_path(os.temp_dir(), 'v3_parallel_module_init.c')
	compile := os.execute('VJOBS=2 ${v3_bin} ${main_path} -o ${c_out}')
	assert compile.exit_code == 0, compile.output
	assert compile.output.contains('cgen'), compile.output
	c_code := os.read_file(c_out) or { panic(err) }
	assert c_code.all_after('int main').contains('_vinit();')
}

fn write_parallel_gettid_project(name string) string {
	project_dir := os.join_path(os.temp_dir(), 'v3_${name}')
	os.rmdir_all(project_dir) or {}
	os.mkdir_all(project_dir) or { panic(err) }

	mut main_src := strings.new_builder(96_000)
	main_src.writeln('module main')
	main_src.writeln('')
	main_src.writeln('fn C.gettid() int')
	main_src.writeln('')
	for i in 0 .. 1050 {
		main_src.writeln('fn helper_${i}() int {')
		main_src.writeln('\treturn ${i}')
		main_src.writeln('}')
		main_src.writeln('')
	}
	main_src.writeln('fn worker_gettid_value() int {')
	main_src.writeln('\treturn C.gettid()')
	main_src.writeln('}')
	main_src.writeln('')
	main_src.writeln('fn main() {')
	main_src.writeln('\tmut total := 0')
	for i in 0 .. 1050 {
		main_src.writeln('\ttotal += helper_${i}()')
	}
	main_src.writeln('\tprintln(int_str(total + worker_gettid_value()))')
	main_src.writeln('}')
	os.write_file(os.join_path(project_dir, 'main.v'), main_src.str()) or { panic(err) }
	return os.join_path(project_dir, 'main.v')
}

fn test_parallel_cgen_merges_worker_gettid_compat() {
	v3_bin := build_parallel_v3()
	main_path := write_parallel_gettid_project('parallel_gettid_compat')
	c_out := os.join_path(os.temp_dir(), 'v3_parallel_gettid_compat.c')
	compile := os.execute('VJOBS=2 ${v3_bin} ${main_path} -o ${c_out}')
	assert compile.exit_code == 0, compile.output
	assert compile.output.contains('cgen'), compile.output
	c_code := os.read_file(c_out) or { panic(err) }
	assert c_code.contains('static inline u32 v3_gettid(void)'), c_code
	assert c_code.contains('return v3_gettid();'), c_code
}

fn write_parallel_top_level_no_main_project(name string) string {
	project_dir := os.join_path(os.temp_dir(), 'v3_${name}')
	os.rmdir_all(project_dir) or {}
	os.mkdir_all(project_dir) or { panic(err) }

	mut main_src := strings.new_builder(48_000)
	main_src.writeln('module main')
	main_src.writeln('')
	for i in 0 .. 320 {
		main_src.writeln('fn helper_${i}() int {')
		main_src.writeln('\treturn ${i}')
		main_src.writeln('}')
		main_src.writeln('')
	}
	main_src.writeln('mut values := map[string]int{}')
	main_src.writeln("mut total := values['missing'] or {")
	main_src.writeln('\t7')
	main_src.writeln('}')
	for i in 0 .. 320 {
		main_src.writeln('total += helper_${i}()')
	}
	main_src.writeln('println(int_str(total))')
	os.write_file(os.join_path(project_dir, 'main.v'), main_src.str()) or { panic(err) }
	return os.join_path(project_dir, 'main.v')
}

fn test_parallel_transform_lowers_top_level_stmts_without_main_once() {
	v3_bin := build_parallel_v3()
	main_path := write_parallel_top_level_no_main_project('parallel_top_level_no_main')
	bin_out := os.join_path(os.temp_dir(), 'v3_parallel_top_level_no_main_out')
	compile := os.execute('VJOBS=2 ${v3_bin} --parallel-transform ${main_path} -b c -o ${bin_out}')
	assert compile.exit_code == 0, compile.output
	assert compile.output.contains('transform (parallel)'), compile.output
	assert compile.output.contains('cgen'), compile.output
	run := os.execute(bin_out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '51047'
	c_code := os.read_file(bin_out + '.c') or { panic(err) }
	assert c_code.all_after('int main').count('map__get_check') == 1, c_code
}
