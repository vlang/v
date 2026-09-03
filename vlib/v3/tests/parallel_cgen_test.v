import os
import strings

const parallel_tests_dir = os.dir(@FILE)
const parallel_v3_dir = os.dir(parallel_tests_dir)
const parallel_vlib_dir = os.dir(parallel_v3_dir)
const parallel_v3_src = os.join_path(parallel_v3_dir, 'v3.v')

fn setup_parallel_v3_cache() {
	cache_dir := os.join_path(os.temp_dir(), 'v3_parallel_cgen_cache_${os.getpid()}')
	if os.getenv('V3CACHE') == cache_dir {
		return
	}
	os.rmdir_all(cache_dir) or {}
	os.setenv('V3CACHE', cache_dir, true)
}

// build_parallel_v3 builds parallel v3 data for v3 tests.
fn build_parallel_v3() string {
	setup_parallel_v3_cache()
	vexe := @VEXE
	v3_bin := os.join_path(os.temp_dir(), 'v3_parallel_cgen_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${vexe} -gc none -path "${parallel_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${parallel_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn build_parallel_prod_v3() string {
	setup_parallel_v3_cache()
	vexe := @VEXE
	v3_bin := os.join_path(os.temp_dir(), 'v3_parallel_prod_cgen_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${vexe} -gc none -prealloc -prod -path "${parallel_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${parallel_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn build_parallel_prealloc_prod_v3() string {
	setup_parallel_v3_cache()
	vexe := @VEXE
	v3_bin := os.join_path(os.temp_dir(), 'v3_parallel_prealloc_prod_cgen_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${vexe} -gc none -prod -prealloc -d parallel -path "${parallel_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${parallel_v3_src}')
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

const runtime_const = make_const()

__global (
	runtime_global = make_global()
	seen_const int
	seen_global int
)

fn make_const() int {
	return 5
}

fn make_global() int {
	return 7
}

fn init() {
	seen_const = runtime_const
	seen_global = runtime_global
}

pub fn value() int {
	return seen_const + seen_global
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

fn test_parallel_cgen_remaps_worker_string_ids() {
	v3_bin := build_parallel_v3()
	source := os.join_path(os.temp_dir(), 'v3_parallel_string_ids_${os.getpid()}.v')
	mut source_text := strings.new_builder(128_000)
	source_text.writeln('module main')
	source_text.writeln('')
	for i in 0 .. 1050 {
		source_text.writeln("fn helper_${i}() string { return 'parallel string ${i}' }")
	}
	source_text.writeln('')
	source_text.writeln('fn main() {')
	source_text.writeln('\tmut total := 0')
	for i in 0 .. 1050 {
		source_text.writeln('\ttotal += helper_${i}().len')
	}
	source_text.writeln('\tprintln(int_str(total))')
	source_text.writeln('}')
	os.write_file(source, source_text.str()) or { panic(err) }
	defer {
		os.rm(source) or {}
	}
	parallel_output := os.join_path(os.temp_dir(), 'v3_parallel_string_ids_${os.getpid()}.c')
	serial_output := os.join_path(os.temp_dir(), 'v3_serial_string_ids_${os.getpid()}.c')
	parallel_compile := os.execute('VJOBS=4 ${v3_bin} -nocache -o ${parallel_output} ${source}')
	assert parallel_compile.exit_code == 0, parallel_compile.output
	assert parallel_compile.output.contains('cgen (parallel)'), parallel_compile.output
	serial_compile :=
		os.execute('VJOBS=2 ${v3_bin} -no-parallel -nocache -o ${serial_output} ${source}')
	assert serial_compile.exit_code == 0, serial_compile.output
	parallel_c := os.read_file(parallel_output) or { panic(err) }
	serial_c := os.read_file(serial_output) or { panic(err) }
	assert parallel_c == serial_c
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

fn write_parallel_user_main_test_project(name string) string {
	project_dir := os.join_path(os.temp_dir(), 'v3_${name}')
	os.rmdir_all(project_dir) or {}
	os.mkdir_all(project_dir) or { panic(err) }

	mut main_src := strings.new_builder(96_000)
	main_src.writeln('module main')
	main_src.writeln('')
	for i in 0 .. 1050 {
		main_src.writeln('fn helper_${i}() int {')
		main_src.writeln('\treturn ${i}')
		main_src.writeln('}')
		main_src.writeln('')
	}
	main_src.writeln('fn main() {')
	main_src.writeln("\tprintln('user-main')")
	main_src.writeln('}')
	main_src.writeln('')
	main_src.writeln('fn test_call_user_main() {')
	main_src.writeln('\tmut total := 0')
	for i in 0 .. 1050 {
		main_src.writeln('\ttotal += helper_${i}()')
	}
	main_src.writeln('\tassert total == 550725')
	main_src.writeln('\tmain()')
	main_src.writeln('}')
	os.write_file(os.join_path(project_dir, 'main_test.v'), main_src.str()) or { panic(err) }
	return os.join_path(project_dir, 'main_test.v')
}

fn test_parallel_cgen_worker_keeps_test_user_main_renamed() {
	v3_bin := build_parallel_v3()
	main_path := write_parallel_user_main_test_project('parallel_user_main_test_file')
	bin_out := os.join_path(os.temp_dir(), 'v3_parallel_user_main_test_file_out')
	compile := os.execute('VJOBS=2 ${v3_bin} ${main_path} -b c -o ${bin_out}')
	assert compile.exit_code == 0, compile.output
	assert compile.output.contains('cgen'), compile.output
	run := os.execute(bin_out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'user-main'
	c_code := os.read_file(bin_out + '.c') or { panic(err) }
	assert c_code.count('int main(') == 1, c_code
	assert c_code.contains('main__user_main'), c_code
	assert c_code.contains('main__user_main();'), c_code
}

fn write_parallel_generic_struct_method_project(name string) string {
	project_dir := os.join_path(os.temp_dir(), 'v3_${name}')
	os.rmdir_all(project_dir) or {}
	os.mkdir_all(project_dir) or { panic(err) }

	mut main_src := strings.new_builder(96_000)
	main_src.writeln('module main')
	main_src.writeln('')
	main_src.writeln('struct Box[T] {')
	main_src.writeln('	value T')
	main_src.writeln('}')
	main_src.writeln('')
	main_src.writeln('fn (b Box[T]) accept(x ?T) T {')
	main_src.writeln('	value := x or {')
	main_src.writeln('		return b.value')
	main_src.writeln('	}')
	main_src.writeln('	return value')
	main_src.writeln('}')
	main_src.writeln('')
	main_src.writeln('fn use_box_accept() int {')
	main_src.writeln('	b := Box[int]{')
	main_src.writeln('		value: 5')
	main_src.writeln('	}')
	main_src.writeln('	return b.accept(7) + b.accept(8)')
	main_src.writeln('}')
	main_src.writeln('')
	for i in 0 .. 1050 {
		main_src.writeln('fn helper_${i}() int {')
		main_src.writeln('\treturn ${i}')
		main_src.writeln('}')
		main_src.writeln('')
	}
	main_src.writeln('fn main() {')
	main_src.writeln('\tmut total := use_box_accept()')
	for i in 0 .. 1050 {
		main_src.writeln('\ttotal += helper_${i}()')
	}
	main_src.writeln('\tprintln(int_str(total))')
	main_src.writeln('}')
	os.write_file(os.join_path(project_dir, 'main.v'), main_src.str()) or { panic(err) }
	return os.join_path(project_dir, 'main.v')
}

fn test_parallel_cgen_worker_resolves_generic_struct_method_signature() {
	v3_bin := build_parallel_v3()
	main_path := write_parallel_generic_struct_method_project('parallel_generic_struct_method')
	bin_out := os.join_path(os.temp_dir(), 'v3_parallel_generic_struct_method_out')
	compile := os.execute('VJOBS=2 ${v3_bin} -nocache ${main_path} -b c -o ${bin_out}')
	assert compile.exit_code == 0, compile.output
	assert compile.output.contains('cgen (parallel)'), compile.output
	run := os.execute(bin_out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '550740'
	c_code := os.read_file(bin_out + '.c') or { panic(err) }
	assert c_code.contains('Box_int__accept'), c_code
	assert c_code.contains('Optional_i64 x'), c_code
	assert !c_code.contains('?T'), c_code
}

fn test_parallel_cgen_generic_optional_method_uses_concrete_optional_abi_in_calls() {
	v3_bin := build_parallel_v3()
	main_path :=
		write_parallel_generic_struct_method_project('parallel_generic_struct_optional_abi')
	c_out := os.join_path(os.temp_dir(), 'v3_parallel_generic_struct_optional_abi.c')
	compile := os.execute('VJOBS=2 ${v3_bin} ${main_path} -o ${c_out}')
	assert compile.exit_code == 0, compile.output
	assert compile.output.contains('cgen (parallel)'), compile.output

	c_code := os.read_file(c_out) or { panic(err) }
	assert c_code.contains('Box_int__accept'), c_code
	assert c_code.contains('Optional_i64 x'), c_code
	assert c_code.contains('Box_int__accept(b, (Optional_i64){.ok = true, .value = 7})'), c_code
	assert c_code.contains('Box_int__accept(b, (Optional_i64){.ok = true, .value = 8})'), c_code
	assert !c_code.contains('Box_int__accept(b, (Optional){.ok = true, .value = 7})'), c_code
	assert !c_code.contains('Box_int__accept(b, (Optional){.ok = true, .value = 8})'), c_code
}

fn write_parallel_ierror_payload_success_project(name string) string {
	project_dir := os.join_path(os.temp_dir(), 'v3_${name}')
	os.rmdir_all(project_dir) or {}
	os.mkdir_all(os.join_path(project_dir, 'payloadmod')) or { panic(err) }

	mut main_src := strings.new_builder(128_000)
	main_src.writeln('module main')
	main_src.writeln('')
	main_src.writeln('import payloadmod')
	main_src.writeln('')
	for i in 0 .. 1300 {
		main_src.writeln('fn helper_${i}() int {')
		main_src.writeln('\treturn ${i}')
		main_src.writeln('}')
		main_src.writeln('')
	}
	main_src.writeln('fn main() {')
	main_src.writeln('\tmut total := 0')
	for i in 0 .. 1300 {
		main_src.writeln('\ttotal += helper_${i}()')
	}
	main_src.writeln('\tpayload := payloadmod.payload() or {')
	main_src.writeln("\t\tprintln('ERR:' + err.msg() + ':' + int_str(err.code()))")
	main_src.writeln('\t\treturn')
	main_src.writeln('\t}')
	main_src.writeln("\tprintln('OK:' + payload.msg() + ':' + int_str(payload.code()))")
	main_src.writeln('\tprintln(int_str(total))')
	main_src.writeln('}')
	os.write_file(os.join_path(project_dir, 'main.v'), main_src.str()) or { panic(err) }

	os.write_file(os.join_path(project_dir, 'payloadmod', 'payloadmod.v'), 'module payloadmod

pub struct WrappedBuiltinError {
	Error
}

pub fn payload() !IError {
	return WrappedBuiltinError{}
}
') or {
		panic(err)
	}
	return os.join_path(project_dir, 'main.v')
}

fn test_parallel_cgen_worker_preserves_ierror_payload_success_with_builtin_error_embed() {
	v3_bin := build_parallel_v3()
	main_path :=
		write_parallel_ierror_payload_success_project('parallel_ierror_payload_success_${os.getpid()}')
	bin_out := os.join_path(os.temp_dir(), 'v3_parallel_ierror_payload_success_out_${os.getpid()}')
	compile := os.execute('VJOBS=2 ${v3_bin} -nocache ${main_path} -b c -o ${bin_out}')
	assert compile.exit_code == 0, compile.output
	assert compile.output.contains('cgen (parallel)'), compile.output
	run := os.execute(bin_out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'OK::0\n844350'
	c_code := os.read_file(bin_out + '.c') or { panic(err) }
	assert c_code.contains('return (Optional_IError){.ok = true, .value = (IError){._typ ='), c_code
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
	compile := os.execute('VJOBS=2 ${v3_bin} ${main_path} -b c -o ${bin_out}')
	assert compile.exit_code == 0, compile.output
	assert compile.output.contains('transform'), compile.output
	assert compile.output.contains('cgen'), compile.output
	run := os.execute(bin_out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '51047'
	c_code := os.read_file(bin_out + '.c') or { panic(err) }
	assert c_code.all_after('int main').count('map__get_check') == 1, c_code
}

fn test_prealloc_keeps_parallel_transform_enabled() {
	v3_bin := build_parallel_prealloc_prod_v3()
	main_path := write_parallel_module_init_project('parallel_prealloc_cgen_${os.getpid()}')
	c_out := os.join_path(os.temp_dir(), 'v3_parallel_prealloc_transform_${os.getpid()}.c')
	compile := os.execute('VJOBS=2 ${v3_bin} -nocache -b c -o ${c_out} ${main_path}')
	assert compile.exit_code == 0, compile.output
	assert compile.output.contains('transform (parallel)'), compile.output
	assert compile.output.contains('cgen (parallel)'), compile.output
	c_code := os.read_file(c_out) or { panic(err) }
	vinit := c_code.all_after('void _vinit() {')
	const_init := vinit.index('moda__runtime_const =') or { -1 }
	global_init := vinit.index('moda__runtime_global =') or { -1 }
	module_init := vinit.index('moda__init();') or { -1 }
	assert const_init >= 0
	assert global_init > const_init
	assert module_init > global_init
}

fn test_parallel_transform_generates_v3_c_with_vjobs_4_and_12() {
	v3_bin := build_parallel_prod_v3()
	c_out_4 := os.join_path(os.temp_dir(), 'v3_parallel_selfhost_out_4_${os.getpid()}.c')
	os.rm(c_out_4) or {}
	cgen_4 :=
		os.execute('VJOBS=4 ${v3_bin} -nocache -building-v -b c -o ${c_out_4} ${parallel_v3_src}')
	assert cgen_4.exit_code == 0, cgen_4.output
	assert cgen_4.output.contains('transform (parallel)'), cgen_4.output
	assert cgen_4.output.contains('cgen (parallel)'), cgen_4.output
	assert os.exists(c_out_4), cgen_4.output
	c_code := os.read_file(c_out_4) or { panic(err) }
	assert c_code.contains('Array_u8__bytestr'), c_code
	assert c_code.contains('Array_u8__hex'), c_code
	assert c_code.contains('typedef void* pthread_t;'), c_code
	assert !c_code.contains('typedef struct pthread_t pthread_t;'), c_code
	assert c_code.contains('flat_cgen_chunk_thread'), c_code
	assert c_code.contains('run_parallel_transform'), c_code

	c_out_12 := os.join_path(os.temp_dir(), 'v3_parallel_selfhost_out_12_${os.getpid()}.c')
	os.rm(c_out_12) or {}
	cgen_12 :=
		os.execute('VJOBS=12 ${v3_bin} -nocache -building-v -b c -o ${c_out_12} ${parallel_v3_src}')
	assert cgen_12.exit_code == 0, cgen_12.output
	assert cgen_12.output.contains('transform (parallel)'), cgen_12.output
	assert cgen_12.output.contains('cgen (parallel)'), cgen_12.output
	assert os.exists(c_out_12), cgen_12.output
}

fn test_no_parallel_directory_selfhost_omits_parallel_support() {
	v3_bin := build_parallel_prod_v3()
	bin_out := os.join_path(os.temp_dir(), 'v3_no_parallel_dir_selfhost_out_${os.getpid()}')
	os.rm(bin_out) or {}
	os.rm(bin_out + '.c') or {}
	compile :=
		os.execute('VJOBS=2 ${v3_bin} --no-parallel -nocache -no-memory-limit -selfhost -b c -o ${bin_out} ${parallel_v3_dir}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('transform (parallel)'), compile.output
	assert !compile.output.contains('cgen (parallel)'), compile.output
	assert os.exists(bin_out), compile.output
	c_code := os.read_file(bin_out + '.c') or { panic(err) }
	assert !c_code.contains('flat_cgen_chunk_thread'), c_code
	assert !c_code.contains('flat_cgen_job_count'), c_code
	assert !c_code.contains('new_parallel_worker'), c_code
	assert !c_code.contains('merge_parallel_worker'), c_code
	assert !c_code.contains('transform_chunk_thread'), c_code
	assert !c_code.contains('transform_job_count'), c_code
}

fn write_no_parallel_user_define_project(name string) string {
	project_dir := os.join_path(os.temp_dir(), 'v3_${name}')
	os.rmdir_all(project_dir) or {}
	os.mkdir_all(project_dir) or { panic(err) }

	os.write_file(os.join_path(project_dir, 'main.v'), "module main

fn parallel_if_value() string {
\t\$if parallel ? {
\t\treturn 'if'
\t} \$else {
\t\treturn 'no-if'
\t}
}

fn main() {
\tprintln(parallel_file_value() + ':' + parallel_if_value())
}
") or {
		panic(err)
	}
	os.write_file(os.join_path(project_dir, 'feature_d_parallel.v'), "module main

fn parallel_file_value() string {
\treturn 'file'
}
") or {
		panic(err)
	}
	return project_dir
}

fn test_no_parallel_preserves_user_parallel_define_for_project() {
	v3_bin := build_parallel_v3()
	project_dir := write_no_parallel_user_define_project('no_parallel_user_define')
	bin_out := os.join_path(os.temp_dir(), 'v3_no_parallel_user_define_out_${os.getpid()}')
	os.rm(bin_out) or {}
	os.rm(bin_out + '.c') or {}
	compile :=
		os.execute('VJOBS=2 ${v3_bin} -nocache --no-parallel -d parallel -b c -o ${bin_out} ${project_dir}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('transform (parallel)'), compile.output
	assert !compile.output.contains('cgen (parallel)'), compile.output
	run := os.execute(bin_out)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'file:if'
}
