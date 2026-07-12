import os

const shared_flag_vexe = @VEXE
const shared_flag_tests_dir = os.dir(@FILE)
const shared_flag_v3_dir = os.dir(shared_flag_tests_dir)
const shared_flag_v3_src = os.join_path(shared_flag_v3_dir, 'v3.v')

// test_shared_flag_builds_no_main_module validates this v3 regression case.
fn test_shared_flag_builds_no_main_module() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_shared_flag_test_${os.getpid()}')
	build :=
		os.execute('${os.quoted_path(shared_flag_vexe)} -o ${os.quoted_path(v3_bin)} ${os.quoted_path(shared_flag_v3_src)}')
	assert build.exit_code == 0, build.output

	tmp_dir := os.join_path(os.temp_dir(), 'v3_shared_flag_module_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
		os.rm(v3_bin) or {}
	}

	os.write_file(os.join_path(tmp_dir, 'v.mod'), 'Module { name: "shared_flag_module" }\n')!
	os.write_file(os.join_path(tmp_dir, 'module.v'),
		'module shared_flag_module\n\npub fn answer() int {\n\treturn 42\n}\n')!

	out_lib := os.join_path(os.temp_dir(), 'v3_shared_flag_module_${os.getpid()}')
	out_path := out_lib + shared_flag_library_postfix()
	os.rm(out_path) or {}
	defer {
		os.rm(out_path) or {}
	}

	compile :=
		os.execute('${os.quoted_path(v3_bin)} -shared -o ${os.quoted_path(out_lib)} ${os.quoted_path(tmp_dir)}')
	assert compile.exit_code == 0, compile.output
	assert compile.output.contains('-shared'), compile.output
	assert !compile.output.contains('_main not defined'), compile.output
	assert os.exists(out_path)
	assert os.file_size(out_path) > 0
}

// test_shared_flag_builds_object_dependencies_as_pic validates that cached
// #flag .o dependencies are rebuilt with -fPIC for shared-library builds.
fn test_shared_flag_builds_object_dependencies_as_pic() {
	$if windows {
		return
	}
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_shared_flag_pic_test_${pid}')
	build :=
		os.execute('${os.quoted_path(shared_flag_vexe)} -o ${os.quoted_path(v3_bin)} ${os.quoted_path(shared_flag_v3_src)}')
	assert build.exit_code == 0, build.output

	tmp_dir := os.join_path(os.temp_dir(), 'v3_shared_flag_pic_${pid}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
		os.rm(v3_bin) or {}
	}

	native_dir := os.join_path(tmp_dir, 'native')
	os.mkdir_all(native_dir)!
	obj_path := os.join_path(native_dir, 'pic_dep.o')
	os.write_file(os.join_path(native_dir, 'pic_dep.c'), 'int v3_pic_dep_global = 41;
int v3_pic_dep_value(void) {
	return v3_pic_dep_global + 1;
}
')!
	shared_flag_remove_cached_objects(obj_path)
	defer {
		shared_flag_remove_cached_objects(obj_path)
	}

	app_dir := os.join_path(tmp_dir, 'app')
	os.mkdir_all(app_dir)!
	app_src := os.join_path(app_dir, 'main.v')
	os.write_file(app_src, '#flag ${obj_path}
fn C.v3_pic_dep_value() int

fn main() {
	println(C.v3_pic_dep_value().str())
}
')!
	app_bin := os.join_path(tmp_dir, 'app_bin')
	compile_app :=
		os.execute('${os.quoted_path(v3_bin)} -o ${os.quoted_path(app_bin)} ${os.quoted_path(app_src)}')
	assert compile_app.exit_code == 0, compile_app.output
	cached_after_app := shared_flag_cached_objects(obj_path)
	assert cached_after_app.len == 1, cached_after_app.str()
	run_app := os.execute(os.quoted_path(app_bin))
	assert run_app.exit_code == 0, run_app.output
	assert run_app.output.trim_space() == '42'

	lib_dir := os.join_path(tmp_dir, 'lib')
	os.mkdir_all(lib_dir)!
	os.write_file(os.join_path(lib_dir, 'v.mod'), 'Module { name: "shared_pic_dep" }\n')!
	os.write_file(os.join_path(lib_dir, 'module.v'), 'module shared_pic_dep

#flag ${obj_path}
fn C.v3_pic_dep_value() int

pub fn answer() int {
	return C.v3_pic_dep_value()
}
')!
	out_lib := os.join_path(tmp_dir, 'shared_pic_dep')
	out_path := out_lib + shared_flag_library_postfix()
	compile_shared :=
		os.execute('${os.quoted_path(v3_bin)} -shared -o ${os.quoted_path(out_lib)} ${os.quoted_path(lib_dir)}')
	assert compile_shared.exit_code == 0, compile_shared.output
	assert compile_shared.output.contains('-fPIC'), compile_shared.output
	cached_after_shared := shared_flag_cached_objects(obj_path)
	assert cached_after_shared.len == 2, '${compile_shared.output}\n${cached_after_shared}'
	assert os.exists(out_path)
	assert os.file_size(out_path) > 0
}

fn shared_flag_library_postfix() string {
	$if windows {
		return '.dll'
	} $else $if macos {
		return '.dylib'
	} $else {
		return '.so'
	}
}

fn shared_flag_cached_objects(obj_path string) []string {
	cache_dir := os.join_path(os.vtmp_dir(), 'v3_thirdparty_objs')
	if !os.exists(cache_dir) {
		return []
	}
	prefix := shared_flag_object_cache_prefix(obj_path)
	mut found := []string{}
	for path in os.walk_ext(cache_dir, '.o') {
		if os.file_name(path).starts_with(prefix) {
			found << path
		}
	}
	return found
}

fn shared_flag_remove_cached_objects(obj_path string) {
	for path in shared_flag_cached_objects(obj_path) {
		os.rm(path) or {}
	}
}

fn shared_flag_object_cache_prefix(path string) string {
	return path.replace_each(['/', '_', '\\', '_', ':', '_', '.', '_', ' ', '_']) + '_'
}
