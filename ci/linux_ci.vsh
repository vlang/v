import common { Task, exec }

//
// Shared tasks/helpers
//
fn all_code_is_formatted() {
	if common.is_github_job {
		exec('v -silent test-cleancode')
	} else {
		exec('v -progress test-cleancode')
	}
}

fn verify_v_test_works() {
	exec('echo \$VFLAGS')
	exec('v cmd/tools/test_if_v_test_system_works.v')
	exec('./cmd/tools/test_if_v_test_system_works')
}

fn test_pure_v_math_module() {
	exec('v -silent -exclude @vlib/math/*.c.v test vlib/math/')
}

fn self_tests() {
	if common.is_github_job {
		exec('v -silent test-self vlib')
	} else {
		exec('v -progress test-self vlib')
	}
}

fn build_examples() {
	if common.is_github_job {
		exec('v -silent build-examples')
	} else {
		exec('v -progress build-examples')
	}
}

fn v_doctor() {
	exec('v doctor')
}

fn build_v_with_prealloc() {
	exec('v -cg -cstrict -o vstrict1 cmd/v')
	exec('./vstrict1 -o vprealloc -prealloc cmd/v')
	exec('./vprealloc run examples/hello_world.v')
	exec('./vprealloc -o v3 cmd/v')
	exec('./v3 -o v4 cmd/v')
	exec('./v4 -d debug_malloc -d debug_realloc -o vdebug1 cmd/v')
}

//
// TCC job tasks
//

fn install_dependencies_for_examples_and_tools_tcc() {
	exec('v retry -- sudo apt update')
	exec('v retry -- sudo apt install --quiet -y libssl-dev sqlite3 libsqlite3-dev valgrind')
	exec('v retry -- sudo apt install --quiet -y libfreetype6-dev libxi-dev libxcursor-dev libgl-dev libxrandr-dev libasound2-dev')
	// The following is needed for examples/wkhtmltopdf.v
	exec('v retry -- sudo apt install --quiet -y xfonts-75dpi xfonts-base expect')
	exec('v retry -- wget --quiet https://github.com/wkhtmltopdf/packaging/releases/download/0.12.6.1-2/wkhtmltox_0.12.6.1-2.jammy_amd64.deb')
	exec('v retry -- sudo dpkg -i wkhtmltox_0.12.6.1-2.jammy_amd64.deb')
}

fn test_v_to_c_tcc() {
	exec('thirdparty/tcc/tcc.exe -version')
	exec('v -cg -o vtcc cmd/v') // ensure vtcc can build itself twice
}

fn v_self_compilation_tcc() {
	exec('v -o v2 cmd/v')
	exec('./v2 -o v3 cmd/v')
	exec('./v3 -o v4 cmd/v')
}

fn v_doctor_tcc() {
	v_doctor()
}

fn verify_v_test_works_tcc() {
	verify_v_test_works()
}

fn test_pure_v_math_module_tcc() {
	test_pure_v_math_module()
}

fn self_tests_tcc() {
	exec('v -keepc -cc tcc -g self')
	self_tests()
}

fn build_examples_tcc() {
	build_examples()
}

fn run_submodule_example_tcc() {
	exec('v -W run examples/submodule')
}

fn build_tools_tcc() {
	if common.is_github_job {
		exec('v -silent -N -W build-tools')
	} else {
		exec('v -progress -N -W build-tools')
	}
}

fn build_vbinaries_tcc() {
	exec('v -N -W build-vbinaries')
}

fn build_benches_tcc() {
	exec('v should-compile-all vlib/v/tests/bench/')
}

fn run_vsh_script_tcc() {
	exec('v run examples/v_script.vsh')
}

fn test_v_tutorials_tcc() {
	exec('v tutorials/building_a_simple_web_blog_with_veb/code/blog')
}

fn build_fast_tcc() {
	exec('cd cmd/tools/fast && v fast.v')
	exec('cd cmd/tools/fast && ./fast')
}

fn v_self_compilation_usecache_tcc() {
	exec('unset VFLAGS')
	exec('v -usecache examples/hello_world.v')
	exec('./examples/hello_world')
	exec('v -o v2 -usecache cmd/v')
	exec('./v2 -o v3 -usecache cmd/v')
	exec('./v3 version')
	exec('./v3 -o tetris -usecache examples/tetris/tetris.v')
}

fn test_password_input_tcc() {
	exec('v -silent test examples/password/')
}

fn test_readline_tcc() {
	exec('v -silent test examples/readline/')
}

fn test_leak_detector_tcc() {
	exec('v -gc boehm_leak -o testcase_leak vlib/v/tests/testcase_leak.vv')
	exec('./testcase_leak 2>leaks.txt')
	exec('grep "Found 1 leaked object" leaks.txt')
	exec('grep -P ", sz=\\s?1000," leaks.txt')
}

fn test_leak_detector_not_active_tcc() {
	exec('v -o testcase_leak vlib/v/tests/testcase_leak.vv')
	exec('./testcase_leak 2>leaks.txt')
	exec('[ "$(stat -c %s leaks.txt)" = "0" ]')
}

//
// GCC job tasks
//

fn all_code_is_formatted_gcc() {
	all_code_is_formatted()
}

fn install_dependencies_for_examples_and_tools_gcc() {
	exec('v retry -- sudo apt update')
	exec('v retry -- sudo apt install --quiet -y postgresql libpq-dev libssl-dev sqlite3 libsqlite3-dev valgrind')
	exec('v retry -- sudo apt install --quiet -y libfreetype6-dev libxi-dev libxcursor-dev libgl-dev libxrandr-dev libasound2-dev')
}

fn recompile_v_with_cstrict_gcc() {
	exec('v -cc gcc -cg -cstrict -o vstrict cmd/v')
}

fn valgrind_v_c_gcc() {
	exec('valgrind --error-exitcode=1 v -o v.c cmd/v')
}

fn run_sanitizers_gcc() {
	exec('v -o v2 cmd/v -cflags -fsanitize=thread')
	exec('v -o v3 cmd/v -cflags "-fsanitize=undefined -fno-sanitize=alignment"')
	exec('UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1 ./v2 -o v.c cmd/v')
	exec('UBSAN_OPTIONS=print_stacktrace=1:halt_on_error=1 ./v3 -o v.c cmd/v')
}

fn v_self_compilation_gcc() {
	exec('v -o v2 cmd/v')
	exec('./v2 -o v3 cmd/v')
	exec('./v3 -o v4 cmd/v')
}

fn v_self_compilation_usecache_gcc() {
	exec('unset VFLAGS')

	exec('v -usecache examples/hello_world.v')
	exec('examples/hello_world')

	exec('v -o v2 -usecache cmd/v')
	exec('./v2 -o v3 -usecache cmd/v')
	exec('./v3 version')
	exec('./v3 -o tetris -usecache examples/tetris/tetris.v')
}

fn verify_v_test_works_gcc() {
	verify_v_test_works()
}

fn test_pure_v_math_module_gcc() {
	test_pure_v_math_module()
}

fn self_tests_gcc() {
	self_tests()
}

fn self_tests_prod_gcc() {
	exec('v -o vprod -prod cmd/v')
	exec('./vprod -silent test-self vlib')
}

fn self_tests_cstrict_gcc() {
	exec('VTEST_JUST_ESSENTIAL=1 V_CI_CSTRICT=1 v -cc gcc -cstrict -silent test-self vlib')
}

fn build_examples_gcc() {
	build_examples()
}

fn build_tetris_autofree_gcc() {
	exec('v -autofree -o tetris examples/tetris/tetris.v')
	exec('rm -f tetris')
}

fn build_blog_autofree_gcc() {
	exec('v -autofree -o blog tutorials/building_a_simple_web_blog_with_veb/code/blog')
	exec('rm -f blog')
}

fn build_option_test_autofree_gcc() {
	exec('v -autofree vlib/v/tests/options/option_test.c.v')
}

fn v_self_compilation_parallel_cc_gcc() {
	exec('v -o v2 -parallel-cc cmd/v')
	exec('rm -f v2')
}

fn build_modules_gcc() {
	exec('v build-module vlib/os')
	exec('v build-module vlib/builtin')
	exec('v build-module vlib/strconv')
	exec('v build-module vlib/time')
	exec('v build-module vlib/term')
	exec('v build-module vlib/math')
	exec('v build-module vlib/strings')
	exec('v build-module vlib/v/token')
	exec('v build-module vlib/v/ast')
	exec('v build-module vlib/v/parser')
	exec('v build-module vlib/v/gen/c')
	exec('v build-module vlib/v/depgraph')
	exec('v build-module vlib/os/cmdline')
}

fn compile_vdoctor_prod_gcc() {
	exec('v -showcc -cc gcc -prod cmd/tools/vdoctor.v')
}

fn compile_vup_prod_gcc() {
	exec('v -showcc -cc gcc -prod cmd/tools/vup.v')
}

//
// Clang job tasks
//

fn all_code_is_formatted_clang() {
	all_code_is_formatted()
}

fn install_dependencies_for_examples_and_tools_clang() {
	exec('v retry -- sudo apt update')
	exec('v retry -- sudo apt install --quiet -y postgresql libpq-dev libssl-dev sqlite3 libsqlite3-dev valgrind')
	exec('v retry -- sudo apt install --quiet -y libfreetype6-dev libxi-dev libxcursor-dev libgl-dev libxrandr-dev libasound2-dev')
	exec('v retry -- sudo apt install --quiet -y clang')
}

fn recompile_v_with_cstrict_clang() {
	exec('v -cc clang -cg -cstrict -o vstrict cmd/v')
	exec('rm -f vstrict')
}

fn valgrind_clang() {
	exec('valgrind --error-exitcode=1 v -o v.c cmd/v')
	exec('rm -f v.c')
}

fn run_sanitizers_clang() {
	exec('.github/workflows/run_sanitizers.sh')
}

fn v_self_compilation_clang() {
	exec('v -o v2 cmd/v')
	exec('./v2 -o v3 cmd/v')
	exec('./v3 -o v4 cmd/v')
}

fn v_self_compilation_usecache_clang() {
	exec('unset VFLAGS')
	exec('v -usecache examples/hello_world.v')
	exec('./examples/hello_world')
	exec('v -o v2 -usecache cmd/v')
	exec('./v2 -o v3 -usecache cmd/v')
	exec('./v3 version')
	exec('./v3 -o tetris -usecache examples/tetris/tetris.v')
	exec('rm -f ./examples/hello_world v2 v3 tetris')
}

fn verify_v_test_works_clang() {
	verify_v_test_works()
}

fn test_pure_v_math_module_clang() {
	test_pure_v_math_module()
}

fn self_tests_clang() {
	self_tests()
}

fn self_tests_vprod_clang() {
	exec('v -o vprod -prod cmd/v')
	exec('./vprod -silent test-self vlib')
}

fn self_tests_cstrict_clang() {
	exec('VTEST_JUST_ESSENTIAL=1 V_CI_CSTRICT=1 ./vprod -cstrict -silent test-self vlib')
}

fn build_examples_clang() {
	build_examples()
}

fn build_examples_autofree_clang() {
	exec('v -autofree -experimental -o tetris examples/tetris/tetris.v')
	exec('rm -f tetris')
}

fn build_modules_clang() {
	exec('v build-module vlib/os')
	exec('v build-module vlib/builtin')
	exec('v build-module vlib/strconv')
	exec('v build-module vlib/time')
	exec('v build-module vlib/term')
	exec('v build-module vlib/math')
	exec('v build-module vlib/strings')
	exec('v build-module vlib/v/token')
	exec('v build-module vlib/v/ast')
	exec('v build-module vlib/v/parser')
	exec('v build-module vlib/v/gen/c')
	exec('v build-module vlib/v/depgraph')
	exec('v build-module vlib/os/cmdline')
}

fn native_machine_code_generation_common() {
	exec('cd cmd/tools && v gen1m.v')
	exec('cd cmd/tools && ./gen1m > 1m.v')
	exec('cd cmd/tools && v -backend native -o 1m 1m.v')
	exec('cd cmd/tools && ./1m && ls -larS 1m*')
	exec('cd cmd/tools && rm -f ./1m ./1m.v')
}

fn native_machine_code_generation_gcc() {
	native_machine_code_generation_common()
}

fn native_machine_code_generation_clang() {
	native_machine_code_generation_common()
}

fn native_cross_compilation_to_macos() {
	exec('v -os macos -experimental -b native -o hw.macos examples/hello_world.v')
	common.file_size_greater_than('hw.macos', 8000)
	exec('rm -f hw.macos')
}

//
// Collect all tasks
//
const all_tasks = {
	'build_v_with_prealloc':                             Task{build_v_with_prealloc, 'Build V with prealloc'}
	// tcc tasks
	'all_code_is_formatted_tcc':                         Task{all_code_is_formatted, 'All code is formatted (tcc)'}
	'install_dependencies_for_examples_and_tools_tcc':   Task{install_dependencies_for_examples_and_tools_tcc, 'Install deps for examples/tools (tcc)'}
	'test_v_to_c_tcc':                                   Task{test_v_to_c_tcc, 'Test v->c with tcc'}
	'v_self_compilation_tcc':                            Task{v_self_compilation_tcc, 'V self compilation (tcc)'}
	'v_doctor_tcc':                                      Task{v_doctor_tcc, 'v doctor (tcc)'}
	'verify_v_test_works_tcc':                           Task{verify_v_test_works_tcc, 'Verify `v test` works (tcc)'}
	'test_pure_v_math_module_tcc':                       Task{test_pure_v_math_module_tcc, 'Test pure V math module (tcc)'}
	'self_tests_tcc':                                    Task{self_tests_tcc, 'Self tests (tcc)'}
	'build_examples_tcc':                                Task{build_examples_tcc, 'Build examples (tcc)'}
	'run_submodule_example_tcc':                         Task{run_submodule_example_tcc, 'Run submodule example (tcc)'}
	'build_tools_tcc':                                   Task{build_tools_tcc, 'Build V tools (tcc)'}
	'build_vbinaries_tcc':                               Task{build_vbinaries_tcc, 'Build V binaries (tcc)'}
	'build_benches_tcc':                                 Task{build_benches_tcc, 'Build benches (tcc)'}
	'run_vsh_script_tcc':                                Task{run_vsh_script_tcc, 'Run a VSH script (tcc)'}
	'test_v_tutorials_tcc':                              Task{test_v_tutorials_tcc, 'Test V tutorials (tcc)'}
	'build_fast_tcc':                                    Task{build_fast_tcc, 'Build cmd/tools/fast (tcc)'}
	'v_self_compilation_usecache_tcc':                   Task{v_self_compilation_usecache_tcc, 'V self compilation with -usecache (tcc)'}
	'test_password_input_tcc':                           Task{test_password_input_tcc, 'Test password input (tcc)'}
	'test_readline_tcc':                                 Task{test_readline_tcc, 'Test readline (tcc)'}
	'test_leak_detector_tcc':                            Task{test_leak_detector_tcc, 'Test leak detector (tcc)'}
	'test_leak_detector_not_active_tcc':                 Task{test_leak_detector_not_active_tcc, 'Test leak detector not active (tcc)'}
	// gcc tasks
	'all_code_is_formatted_gcc':                         Task{all_code_is_formatted_gcc, 'All code is formatted (gcc)'}
	'install_dependencies_for_examples_and_tools_gcc':   Task{install_dependencies_for_examples_and_tools_gcc, 'Install deps for examples/tools (gcc)'}
	'recompile_v_with_cstrict_gcc':                      Task{recompile_v_with_cstrict_gcc, 'Recompile V with -cstrict and gcc'}
	'valgrind_v_c_gcc':                                  Task{valgrind_v_c_gcc, 'Valgrind v.c (gcc)'}
	'run_sanitizers_gcc':                                Task{run_sanitizers_gcc, 'Run sanitizers (gcc)'}
	'v_self_compilation_gcc':                            Task{v_self_compilation_gcc, 'V self compilation (gcc)'}
	'v_self_compilation_usecache_gcc':                   Task{v_self_compilation_usecache_gcc, 'V self compilation with -usecache (gcc)'}
	'verify_v_test_works_gcc':                           Task{verify_v_test_works_gcc, 'Verify `v test` works (gcc)'}
	'test_pure_v_math_module_gcc':                       Task{test_pure_v_math_module_gcc, 'Test pure V math module (gcc)'}
	'self_tests_gcc':                                    Task{self_tests_gcc, 'Self tests (gcc)'}
	'self_tests_prod_gcc':                               Task{self_tests_prod_gcc, 'Self tests (-prod) (gcc)'}
	'self_tests_cstrict_gcc':                            Task{self_tests_cstrict_gcc, 'Self tests (-cstrict) (gcc)'}
	'build_examples_gcc':                                Task{build_examples_gcc, 'Build examples (gcc)'}
	'build_tetris_autofree_gcc':                         Task{build_tetris_autofree_gcc, 'Build tetris with -autofree (gcc)'}
	'build_blog_autofree_gcc':                           Task{build_blog_autofree_gcc, 'Build blog tutorial with -autofree (gcc)'}
	'build_option_test_autofree_gcc':                    Task{build_option_test_autofree_gcc, 'Build option_test.c.v with -autofree (gcc)'}
	'v_self_compilation_parallel_cc_gcc':                Task{v_self_compilation_parallel_cc_gcc, 'V self compilation with -parallel-cc (gcc)'}
	'build_modules_gcc':                                 Task{build_modules_gcc, 'Build modules (gcc)'}
	'compile_vdoctor_prod_gcc':                          Task{compile_vdoctor_prod_gcc, 'compile vdoctor with -prod (gcc)'}
	'compile_vup_prod_gcc':                              Task{compile_vup_prod_gcc, 'compile vup with -prod (gcc)'}
	// clang tasks
	'all_code_is_formatted_clang':                       Task{all_code_is_formatted_clang, 'All code is formatted (clang)'}
	'install_dependencies_for_examples_and_tools_clang': Task{install_dependencies_for_examples_and_tools_clang, 'Install deps for examples/tools (clang)'}
	'recompile_v_with_cstrict_clang':                    Task{recompile_v_with_cstrict_clang, 'Recompile V with -cstrict and clang'}
	'valgrind_clang':                                    Task{valgrind_clang, 'Valgrind (clang)'}
	'run_sanitizers_clang':                              Task{run_sanitizers_clang, 'Run sanitizers (clang)'}
	'v_self_compilation_clang':                          Task{v_self_compilation_clang, 'V self compilation (clang)'}
	'v_self_compilation_usecache_clang':                 Task{v_self_compilation_usecache_clang, 'V self compilation with -usecache (clang)'}
	'verify_v_test_works_clang':                         Task{verify_v_test_works_clang, 'Verify `v test` works (clang)'}
	'test_pure_v_math_module_clang':                     Task{test_pure_v_math_module_clang, 'Test pure V math module (clang)'}
	'self_tests_clang':                                  Task{self_tests_clang, 'Self tests (clang)'}
	'self_tests_vprod_clang':                            Task{self_tests_vprod_clang, 'Self tests (vprod) (clang)'}
	'self_tests_cstrict_clang':                          Task{self_tests_cstrict_clang, 'Self tests (-cstrict) (clang)'}
	'build_examples_clang':                              Task{build_examples_clang, 'Build examples (clang)'}
	'build_examples_autofree_clang':                     Task{build_examples_autofree_clang, 'Build examples with -autofree (clang)'}
	'build_modules_clang':                               Task{build_modules_clang, 'Build modules (clang)'}
	'native_machine_code_generation_clang':              Task{native_machine_code_generation_clang, 'native machine code generation (clang)'}
	'native_machine_code_generation_gcc':                Task{native_machine_code_generation_gcc, 'native machine code generation (gcc)'}
	'native_cross_compilation_to_macos':                 Task{native_cross_compilation_to_macos, 'native cross compilation to macos'}
}

common.run(all_tasks)
