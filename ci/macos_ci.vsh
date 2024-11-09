import os

enum Command {
	build_v
	test_symlink
	test_cross_compilation
	build_with_cstrict
	all_code_is_formatted
	run_sanitizers
	build_using_v
	verify_v_test_works
	install_iconv
	test_pure_v_math_module
	self_tests
	build_examples
	build_tetris_autofree
	build_blog_autofree
	build_examples_prod
	v_doctor
	v_self_compilation_usecache
	v_self_compilation_parallel_cc
	test_password_input
	test_readline
	test_vlib_skip_unused
}

fn main() {
	if os.args.len < 2 {
		println('Usage: v run macos_ci.vsh <step_name>')
		return
	}
	arg := os.args[1]
	if arg == 'all' {
		$for x in Command.values {
			println(get_step_name(x.value))
			run_step(x.value)
		}
		return
	}
	step := Command.from_string(arg) or {
		eprintln('Unknown step: ${arg}')
		exit(1)
	}
	run_step(step)
}

fn run_step(step Command) {
	println('Running ${step}...')
	match step {
		.build_v { build_v() }
		.test_symlink { test_symlink() }
		.test_cross_compilation { test_cross_compilation() }
		.build_with_cstrict { build_with_cstrict() }
		.all_code_is_formatted { all_code_is_formatted() }
		.run_sanitizers { run_sanitizers() }
		.build_using_v { build_using_v() }
		.verify_v_test_works { verify_v_test_works() }
		.install_iconv { install_iconv() }
		.test_pure_v_math_module { test_pure_v_math_module() }
		.self_tests { self_tests() }
		.build_examples { build_examples() }
		.build_tetris_autofree { build_tetris_autofree() }
		.build_blog_autofree { build_blog_autofree() }
		.build_examples_prod { build_examples_prod() }
		.v_doctor { v_doctor() }
		.v_self_compilation_usecache { v_self_compilation_usecache() }
		.v_self_compilation_parallel_cc { v_self_compilation_parallel_cc() }
		.test_password_input { test_password_input() }
		.test_readline { test_readline() }
		.test_vlib_skip_unused { test_vlib_skip_unused() }
	}
}

// Helper function to execute commands and exit if they fail
fn exec(command string) {
	result := os.system(command)
	// or {
	// eprintln('Command failed: $command\nError: $err')
	// exit(1)
	//}
	// if result.exit_code != 0 {
	if result != 0 {
		// eprintln('Command failed with code ${result.exit_code}: ${command}\nOutput: ${result.output}')
		exit(1)
	}
	// println(result.output)
}

// Map enum values to human readable step names
fn get_step_name(step Command) string {
	return match step {
		.build_v { 'Build V' }
		.test_symlink { 'Test symlink' }
		.test_cross_compilation { 'Test cross compilation to Linux' }
		.build_with_cstrict { 'Build V with -cstrict' }
		.all_code_is_formatted { 'All code is formatted' }
		.run_sanitizers { 'Run sanitizers' }
		.build_using_v { 'Build V using V' }
		.verify_v_test_works { 'Verify `v test` works' }
		.install_iconv { 'Install iconv for encoding.iconv' }
		.test_pure_v_math_module { 'Test pure V math module' }
		.self_tests { 'Self tests' }
		.build_examples { 'Build examples' }
		.build_tetris_autofree { 'Build tetris with -autofree' }
		.build_blog_autofree { 'Build blog tutorial with -autofree' }
		.build_examples_prod { 'Build examples with -prod' }
		.v_doctor { 'v doctor' }
		.v_self_compilation_usecache { 'V self compilation with -usecache' }
		.v_self_compilation_parallel_cc { 'V self compilation with -parallel-cc' }
		.test_password_input { 'Test password input' }
		.test_readline { 'Test readline' }
		.test_vlib_skip_unused { 'Test vlib modules with -skip-unused' }
	}
}

// Step functions
fn build_v() {
	exec('make -j4')
	exec('./v symlink')
}

fn test_symlink() {
	exec('v symlink')
}

fn test_cross_compilation() {
	exec('v -o hw -os linux examples/hello_world.v && ls -la hw && file hw')
	exec('v -d use_openssl -o ve -os linux examples/veb/veb_example.v && ls -la ve && file ve')
}

fn build_with_cstrict() {
	exec('v -cg -cstrict -o v cmd/v')
}

fn all_code_is_formatted() {
	exec('VJOBS=1 v test-cleancode')
}

fn run_sanitizers() {
	exec('v -o v2 cmd/v -cflags -fsanitize=undefined')
	exec('UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1 ./v2 -o v.c cmd/v')
}

fn build_using_v() {
	exec('v -o v2 cmd/v')
	exec('./v2 -o v3 cmd/v')
}

fn verify_v_test_works() {
	exec('echo \$VFLAGS')
	exec('v cmd/tools/test_if_v_test_system_works.v')
	exec('./cmd/tools/test_if_v_test_system_works')
}

fn install_iconv() {
	exec('brew install libiconv')
}

fn test_pure_v_math_module() {
	exec('v -exclude @vlib/math/*.c.v test vlib/math/')
}

fn self_tests() {
	exec('VJOBS=1 v test-self vlib')
}

fn build_examples() {
	exec('v build-examples')
}

fn build_tetris_autofree() {
	exec('v -autofree -o tetris examples/tetris/tetris.v')
}

fn build_blog_autofree() {
	exec('v -autofree -o blog tutorials/building_a_simple_web_blog_with_vweb/code/blog')
}

fn build_examples_prod() {
	exec('v -prod examples/news_fetcher.v')
}

fn v_doctor() {
	exec('v doctor')
}

fn v_self_compilation_usecache() {
	exec('unset VFLAGS')
	exec('v -usecache examples/hello_world.v && examples/hello_world')
	exec('v -o v2 -usecache cmd/v')
	exec('./v2 -o v3 -usecache cmd/v')
	exec('./v3 version')
	exec('./v3 -o tetris -usecache examples/tetris/tetris.v')
}

fn v_self_compilation_parallel_cc() {
	exec('v -o v2 -parallel-cc cmd/v')
}

fn test_password_input() {
	exec('v test examples/password/')
}

fn test_readline() {
	exec('v test examples/readline/')
}

fn test_vlib_skip_unused() {
	exec('v -skip-unused test vlib/builtin/ vlib/math vlib/flag/ vlib/os/ vlib/strconv/')
}
