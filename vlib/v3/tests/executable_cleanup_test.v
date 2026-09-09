import os

const executable_cleanup_vexe = @VEXE
const executable_cleanup_tests_dir = os.dir(@FILE)
const executable_cleanup_v3_dir = os.dir(executable_cleanup_tests_dir)
const executable_cleanup_v3_src = os.join_path(executable_cleanup_v3_dir, 'v3.v')

fn executable_cleanup_compile_and_run(v3_bin string, root string, name string, suffix string,
	source string) (os.Result, string) {
	generated_c := executable_cleanup_generate_c(v3_bin, root, name, suffix, '', source)
	source_path := os.join_path(root, '${name}${suffix}')
	output_path := os.join_path(root, name)
	compile :=
		os.execute('${os.quoted_path(v3_bin)} -nocache -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}')
	assert compile.exit_code == 0, compile.output
	return os.execute(os.quoted_path(output_path)), generated_c
}

fn executable_cleanup_generate_c(v3_bin string, root string, name string, suffix string,
	flags string, source string) string {
	source_path := os.join_path(root, '${name}${suffix}')
	c_path := os.join_path(root, '${name}.c')
	os.write_file(source_path, source) or { panic(err) }
	generate :=
		os.execute('${os.quoted_path(v3_bin)} -nocache ${flags} -o ${os.quoted_path(c_path)} ${os.quoted_path(source_path)}')
	assert generate.exit_code == 0, generate.output
	return os.read_file(c_path) or { panic(err) }
}

fn assert_cleanup_registered_after_init(c_code string) {
	init_index := c_code.index('\t_vinit();') or { -1 }
	cleanup_index := c_code.index('atexit(_vcleanup);') or { -1 }
	assert init_index >= 0, c_code
	assert cleanup_index > init_index, c_code
}

fn test_executable_mains_invoke_module_cleanup() {
	root := os.join_path(os.temp_dir(), 'v3_executable_cleanup_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	v3_bin := os.join_path(root, 'v3')
	defer {
		os.rmdir_all(root) or {}
	}
	build :=
		os.execute('${os.quoted_path(executable_cleanup_vexe)} -gc none -o ${os.quoted_path(v3_bin)} ${os.quoted_path(executable_cleanup_v3_src)}')
	assert build.exit_code == 0, build.output

	main_run, main_c := executable_cleanup_compile_and_run(v3_bin, root, 'user_main', '.v', "fn init() {
	println('init')
}

fn cleanup() {
	println('cleanup')
}

fn main() {
	println('main')
	return
}
")
	assert main_run.exit_code == 0, main_run.output
	assert main_run.output.trim_space() == 'init\nmain\ncleanup'
	assert main_c.contains('atexit(_vcleanup);'), main_c
	assert_cleanup_registered_after_init(main_c)

	init_exit_run, init_exit_c := executable_cleanup_compile_and_run(v3_bin, root, 'init_exit',
		'.v', "fn init() {
	println('init')
	exit(0)
}

fn cleanup() {
	println('cleanup')
}

fn main() {
	println('main')
}
")
	assert init_exit_run.exit_code == 0, init_exit_run.output
	assert init_exit_run.output.trim_space() == 'init'
	assert_cleanup_registered_after_init(init_exit_c)

	top_level_run, top_level_c := executable_cleanup_compile_and_run(v3_bin, root, 'top_level',
		'.vsh', "fn init() {
	println('init')
}

fn cleanup() {
	println('cleanup')
}

println('top level')
")
	assert top_level_run.exit_code == 0, top_level_run.output
	assert top_level_run.output.trim_space() == 'init\ntop level\ncleanup'
	assert top_level_c.contains('atexit(_vcleanup);'), top_level_c
	assert_cleanup_registered_after_init(top_level_c)

	test_run, test_c := executable_cleanup_compile_and_run(v3_bin, root, 'test_main', '_test.v', "fn init() {
	println('init')
}

fn cleanup() {
	println('cleanup')
}

fn test_one() {
	println('test')
}
")
	assert test_run.exit_code == 0, test_run.output
	assert test_run.output.trim_space() == 'init\ntest\ncleanup'
	assert test_c.contains('atexit(_vcleanup);'), test_c
	assert_cleanup_registered_after_init(test_c)

	function_only_run, function_only_c := executable_cleanup_compile_and_run(v3_bin, root,
		'function_only', '.v', 'module main

pub fn answer() int {
	return 42
}
')
	assert function_only_run.exit_code == 0, function_only_run.output
	assert function_only_c.contains('int main(int argc, char** argv) {'), function_only_c

	no_main_source := os.join_path(root, 'no_main.v')
	no_main_c_path := os.join_path(root, 'no_main.c')
	os.write_file(no_main_source, "module main

fn init() {
	println('init')
}

fn cleanup() {
	println('cleanup')
}

@[export: 'exported_answer']
pub fn answer() int {
	println('answer')
	return 42
}
")!
	generate_no_main :=
		os.execute('${os.quoted_path(v3_bin)} -nocache -o ${os.quoted_path(no_main_c_path)} ${os.quoted_path(no_main_source)}')
	assert generate_no_main.exit_code == 0, generate_no_main.output
	no_main_c := os.read_file(no_main_c_path)!
	assert no_main_c.contains('static void _vno_main_init_caller(void) {'), no_main_c
	assert no_main_c.contains('i64 exported_answer(void)'), no_main_c
	assert_cleanup_registered_after_init(no_main_c)

	// A natural-name export (export name equal to the C symbol) is emitted directly
	// with no wrapper, so its own body must run the guarded initializer; otherwise a
	// host calling it observes uninitialized globals and skipped module init().
	direct_export_source := os.join_path(root, 'direct_export.v')
	direct_export_c_path := os.join_path(root, 'direct_export.c')
	os.write_file(direct_export_source, "module main

__global (
	direct_export_counter int
)

fn init() {
	direct_export_counter = 100
}

@[export: 'start']
pub fn start() int {
	return direct_export_counter
}
")!
	generate_direct :=
		os.execute('${os.quoted_path(v3_bin)} -nocache -o ${os.quoted_path(direct_export_c_path)} ${os.quoted_path(direct_export_source)}')
	assert generate_direct.exit_code == 0, generate_direct.output
	direct_export_c := os.read_file(direct_export_c_path)!
	assert direct_export_c.contains('int start(void) {'), direct_export_c
	direct_start_body := direct_export_c.all_after('int start(void) {').all_before('}')
	assert direct_start_body.contains('_vno_main_init_caller();'), direct_export_c

	explicit_main_no_main_c := executable_cleanup_generate_c(v3_bin, root, 'explicit_main_no_main',
		'.v', '-d no_main', "fn main() {
	println('not called')
}
")
	assert !explicit_main_no_main_c.contains('int main(int argc, char** argv) {'), explicit_main_no_main_c
	assert !explicit_main_no_main_c.contains('void main(void) {'), explicit_main_no_main_c
	assert explicit_main_no_main_c.contains('main__main'), explicit_main_no_main_c

	top_level_no_main_c := executable_cleanup_generate_c(v3_bin, root, 'top_level_no_main', '.vsh',
		'-d no_main', "println('not called')
")
	assert !top_level_no_main_c.contains('int main(int argc, char** argv) {'), top_level_no_main_c
}
