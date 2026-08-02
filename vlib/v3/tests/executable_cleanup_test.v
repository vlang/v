import os

const executable_cleanup_vexe = @VEXE
const executable_cleanup_tests_dir = os.dir(@FILE)
const executable_cleanup_v3_dir = os.dir(executable_cleanup_tests_dir)
const executable_cleanup_v3_src = os.join_path(executable_cleanup_v3_dir, 'v3.v')

fn executable_cleanup_compile_and_run(v3_bin string, root string, name string, suffix string,
	source string) (os.Result, string) {
	source_path := os.join_path(root, '${name}${suffix}')
	output_path := os.join_path(root, name)
	c_path := output_path + '.c'
	os.write_file(source_path, source) or { panic(err) }
	generate :=
		os.execute('${os.quoted_path(v3_bin)} -nocache -o ${os.quoted_path(c_path)} ${os.quoted_path(source_path)}')
	assert generate.exit_code == 0, generate.output
	generated_c := os.read_file(c_path) or { panic(err) }
	compile :=
		os.execute('${os.quoted_path(v3_bin)} -nocache -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}')
	assert compile.exit_code == 0, compile.output
	return os.execute(os.quoted_path(output_path)), generated_c
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

	main_run, main_c := executable_cleanup_compile_and_run(v3_bin, root, 'user_main', '.v', "fn cleanup() {
	println('cleanup')
}

fn main() {
	println('main')
	return
}
")
	assert main_run.exit_code == 0, main_run.output
	assert main_run.output.trim_space() == 'main\ncleanup'
	assert main_c.contains('atexit(_vcleanup);'), main_c

	top_level_run, top_level_c := executable_cleanup_compile_and_run(v3_bin, root, 'top_level',
		'.vsh', "fn cleanup() {
	println('cleanup')
}

println('top level')
")
	assert top_level_run.exit_code == 0, top_level_run.output
	assert top_level_run.output.trim_space() == 'top level\ncleanup'
	assert top_level_c.contains('atexit(_vcleanup);'), top_level_c

	test_run, test_c := executable_cleanup_compile_and_run(v3_bin, root, 'test_main', '_test.v', "fn cleanup() {
	println('cleanup')
}

fn test_one() {
	println('test')
}
")
	assert test_run.exit_code == 0, test_run.output
	assert test_run.output.trim_space() == 'test\ncleanup'
	assert test_c.contains('atexit(_vcleanup);'), test_c
}
