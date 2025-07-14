import common { Task, exec }

fn test_symlink() {
	exec('v symlink')
}

fn test_cross_compilation() {
	exec('v -o hw -os linux examples/hello_world.v && ls -la hw && file hw')
	exec('v -d use_openssl -o ve -os linux examples/veb/veb_example.v && ls -la ve && file ve')
}

fn build_with_cstrict() {
	exec('v -cg -cstrict -o vstrict1 cmd/v')
}

fn all_code_is_formatted() {
	if common.is_github_job {
		exec('VJOBS=1 v -silent test-cleancode')
	} else {
		exec('v -progress test-cleancode')
	}
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
	exec('v -silent -exclude @vlib/math/*.c.v test vlib/math/')
}

fn self_tests() {
	if common.is_github_job {
		exec('VJOBS=1 v -silent test-self vlib')
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

fn build_examples_v_compiled_with_tcc() {
	exec('v -o vtcc -cc tcc cmd/v')
	if common.is_github_job {
		// ensure that examples/veb/veb_example.v etc compiles
		exec('./vtcc -silent build-examples')
	} else {
		exec('./vtcc -progress build-examples')
	}
}

fn build_tetris_autofree() {
	exec('v -autofree -o tetris examples/tetris/tetris.v')
}

fn build_blog_autofree() {
	exec('v -autofree -o blog tutorials/building_a_simple_web_blog_with_veb/code/blog')
}

fn build_examples_prod() {
	exec('v -prod examples/news_fetcher.v')
}

fn v_doctor() {
	exec('v doctor')
}

fn build_v_with_prealloc() {
	exec('v -cg -cstrict -o vstrict1 cmd/v')
	exec('./vstrict1 -d debug_malloc -d debug_realloc -o vdebug1 cmd/v')
	exec('./vstrict1 -o vprealloc -prealloc cmd/v')
	// TODO: fix prealloc on macos (the rwmutex implementation for shared maps there seems to require that mutexes are allocated by C.malloc directly, and segfaults for arbitrary memory addresses)
	//	exec('./vprealloc run examples/hello_world.v')
	//	exec('./vprealloc -o v3 cmd/v')
	//	exec('./v3 -o v4 cmd/v')
}

fn v_self_compilation_usecache() {
	exec('unset VFLAGS')
	exec('v -usecache examples/hello_world.v')
	exec('./examples/hello_world')
	exec('v -o v2 -usecache cmd/v')
	exec('./v2 -o v3 -usecache cmd/v')
	exec('./v3 version')
	exec('./v3 -o tetris -usecache examples/tetris/tetris.v')
}

fn v_self_compilation_parallel_cc() {
	exec('v -o vp -parallel-cc cmd/v')
	// exec('./v2 -o v3 -usecache cmd/v')
	exec('./vp version')
	exec('./vp -o tetris examples/tetris/tetris.v')
}

fn test_password_input() {
	exec('v -silent test examples/password/')
}

fn test_readline() {
	exec('v -silent test examples/readline/')
}

const all_tasks = {
	'test_symlink':                       Task{test_symlink, 'Test symlink'}
	'test_cross_compilation':             Task{test_cross_compilation, 'Test cross compilation to Linux'}
	'build_with_cstrict':                 Task{build_with_cstrict, 'Build V with -cstrict'}
	'all_code_is_formatted':              Task{all_code_is_formatted, 'All code is formatted'}
	'run_sanitizers':                     Task{run_sanitizers, 'Run sanitizers'}
	'build_using_v':                      Task{build_using_v, 'Build V using V'}
	'verify_v_test_works':                Task{verify_v_test_works, 'Verify `v test` works'}
	'install_iconv':                      Task{install_iconv, 'Install iconv for encoding.iconv'}
	'test_pure_v_math_module':            Task{test_pure_v_math_module, 'Test pure V math module'}
	'self_tests':                         Task{self_tests, 'Self tests'}
	'build_examples':                     Task{build_examples, 'Build examples'}
	'build_tetris_autofree':              Task{build_tetris_autofree, 'Build tetris with -autofree'}
	'build_blog_autofree':                Task{build_blog_autofree, 'Build blog tutorial with -autofree'}
	'build_examples_prod':                Task{build_examples_prod, 'Build examples with -prod'}
	'build_examples_v_compiled_with_tcc': Task{build_examples_v_compiled_with_tcc, 'Build examples with V build with tcc'}
	'v_doctor':                           Task{v_doctor, 'v doctor'}
	'build_v_with_prealloc':              Task{build_v_with_prealloc, 'Build V with prealloc'}
	'v_self_compilation_usecache':        Task{v_self_compilation_usecache, 'V self compilation with -usecache'}
	'v_self_compilation_parallel_cc':     Task{v_self_compilation_parallel_cc, 'V self compilation with -parallel-cc'}
	'test_password_input':                Task{test_password_input, 'Test password input'}
	'test_readline':                      Task{test_readline, 'Test readline'}
}

common.run(all_tasks)
