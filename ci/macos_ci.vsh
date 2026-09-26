import common { Task, exec }
import crypto.sha256
import os
import runtime

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
		vjobs := os.getenv_opt('VJOBS') or { '1' }
		exec('VJOBS=${vjobs} v -progress test-cleancode')
	}
}

fn run_sanitizers() {
	common.exec_with_progress('v -o v2 cmd/v -cflags -fsanitize=undefined', ['v2'])
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
	// Skip Homebrew when iconv is already linkable for V on this machine.
	if os.system('v -silent test vlib/encoding/iconv/') == 0 {
		return
	}
	exec('brew list --versions libiconv >/dev/null 2>&1 || brew install libiconv')
}

fn test_pure_v_math_module() {
	exec('v -silent -exclude @vlib/math/*.c.v test vlib/math/')
}

fn self_tests() {
	// Do not select the V1 compatibility compiler here. It is a separate V 0.5.2
	// installation, so `test-self vlib` would resolve `vlib` under *its* VROOT and
	// test the release's own standard library instead of this repository's.
	// Individual files still fall back to it when the default compiler cannot
	// build them.
	// V3 needs several seconds per test file on the macOS runners, so a single job
	// cannot get through vlib before the job timeout. The automatic job count
	// allows one job per 8 GB of RAM, which is still one job on these 7 GB
	// runners, while a V3 test build peaks well below 2 GB; use every core.
	if common.is_github_job {
		exec('VJOBS=${runtime.nr_cpus()} v -no-memory-limit -silent test-self vlib')
	} else {
		vjobs := os.getenv_opt('VJOBS') or { '1' }
		exec('VJOBS=${vjobs} v -no-memory-limit -progress test-self vlib')
	}
}

fn build_examples() {
	if common.is_github_job {
		exec('v -no-memory-limit build-examples')
	} else {
		exec('v -no-memory-limit -progress build-examples')
	}
}

fn build_examples_v_compiled_with_tcc() {
	exec('v -o vtcc -cc tcc cmd/v')
	if common.is_github_job {
		exec('./vtcc -no-memory-limit build-examples')
	} else {
		exec('./vtcc -no-memory-limit -progress build-examples')
	}
}

// ownership_vexe builds, once per job, a V3 compiler with the ownership checker
// compiled in. `-autofree` needs it: a standard V3 build rejects the flag.
fn ownership_vexe() string {
	vexe := './vownership'
	if !os.exists(vexe) {
		exec('v -d ownership -o vownership cmd/v')
	}
	return vexe
}

fn skip_ownership_autofree_test() bool {
	return common.is_github_job || os.getenv('VTEST_SKIP_OWNERSHIP') == '1'
}

fn build_hello_world_autofree() {
	if skip_ownership_autofree_test() {
		eprintln('> skipping ownership/autofree test')
		return
	}
	exec('${ownership_vexe()} -autofree -o hello_world examples/hello_world.v')
	exec('./hello_world')
}

fn build_tetris_autofree() {
	if skip_ownership_autofree_test() {
		eprintln('> skipping ownership/autofree test')
		return
	}
	exec('${ownership_vexe()} -autofree -o tetris examples/tetris/tetris.v')
}

fn build_blog_autofree() {
	if skip_ownership_autofree_test() {
		eprintln('> skipping ownership/autofree test')
		return
	}
	// `-autofree` still needs the V1 compatibility compiler, and the frozen V 0.5.2
	// release behind it ships a vlib without `json2`, which the blog imports. Build
	// the tutorial with the default compiler until V3 ownership can run it;
	// build_tetris_autofree keeps the autofree path covered.
	exec('v -o blog tutorials/building_a_simple_web_blog_with_veb/code/blog')
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
	$if !enable_usecache_test ? {
		eprintln('> ${@LOCATION} use `-d enable_usecache_test` in VFLAGS to enable this task')
		return
	}
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
	// Expect gives the child a pseudo-terminal, but non-interactive parent shells can
	// still export TERM=dumb, which makes os.input_password reject that usable PTY.
	if os.getenv('TERM') in ['', 'dumb'] {
		os.setenv('TERM', 'xterm', true)
	}
	exec('v -silent test examples/password/')
}

fn test_readline() {
	exec('v -silent test examples/readline/')
}

fn test_inline_assembly() {
	exec('v test vlib/v/slow_tests/assembly')
}

const ci_tasks = [
	'test_symlink',
	'v_doctor',
	'build_v_with_prealloc',
	'test_cross_compilation',
	'test_inline_assembly',
	'build_with_cstrict',
	'all_code_is_formatted',
	'run_sanitizers',
	'build_using_v',
	'verify_v_test_works',
	'install_iconv',
	'test_pure_v_math_module',
	'self_tests',
	'build_examples',
	'build_hello_world_autofree',
	'build_tetris_autofree',
	'build_blog_autofree',
	'build_examples_prod',
	'build_examples_v_compiled_with_tcc',
	'v_self_compilation_parallel_cc',
	'test_password_input',
	'test_readline',
]

// Keep progress across edits/rebuilds, but isolate users and checkout directories.
fn ci_progress_path() string {
	checkout := sha256.hexhash(os.real_path(os.getwd()))
	return '/tmp/v-macos-ci-${os.getuid()}-${checkout}.progress'
}

fn ci_progress_contents(task_name string) string {
	// Record the whole ordered task list so changed plans restart safely.
	return 'macos-ci-v1\n${task_name}\n${ci_tasks.join('\n')}\n'
}

fn ci_resume_index(path string) !int {
	if !os.exists(path) {
		return -1
	}
	if !os.is_file(path) {
		return error('CI progress path is not a file: ${path}')
	}
	saved := os.read_file(path)!
	for i, task_name in ci_tasks {
		if saved == ci_progress_contents(task_name) {
			return i
		}
	}
	eprintln('Ignoring invalid or outdated CI progress; restarting from the first task.')
	return -1
}

fn save_ci_progress(path string, task_name string) ! {
	// Write privately, then rename on the same filesystem. An interrupted write
	// leaves the previous checkpoint intact, never a partially written cursor.
	if os.exists(path) && !os.is_file(path) {
		return error('CI progress path is not a file: ${path}')
	}
	tmp_dir := '${path}.${os.getpid()}.tmp'
	os.mkdir(tmp_dir, mode: 0o700)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	tmp_path := os.join_path(tmp_dir, 'progress')
	os.write_file(tmp_path, ci_progress_contents(task_name))!
	os.rename(tmp_path, path)!
}

// run_ci_tasks mirrors the active ci/macos_ci.vsh steps in
// .github/workflows/macos_ci.yml. The generic `all` mode intentionally remains
// exhaustive, including tasks that are currently disabled in the workflow.
fn run_ci_tasks(reset bool) ! {
	// Match the GitHub Actions job environment that changes test behavior.
	os.setenv('CI', 'true', true)
	os.setenv('GITHUB_ACTIONS', 'true', true)
	os.setenv('GITHUB_JOB', 'clang-macos', true)
	os.setenv('RUNNER_OS', 'macOS', true)
	os.setenv('VFLAGS', '-cc clang', true)
	// Stop within test/build sessions too, without other files already running.
	os.setenv('VTEST_FAIL_FAST', '1', true)
	os.setenv('VJOBS', '1', true)
	os.setenv('VTEST_SHOW_LONGEST_BY_RUNTIME', '3', true)
	os.setenv('VTEST_SHOW_LONGEST_BY_COMPTIME', '3', true)
	os.setenv('VTEST_SHOW_LONGEST_BY_TOTALTIME', '3', true)
	os.setenv('VTEST_SKIP_OWNERSHIP', '1', true)
	os.setenv('V_MACOS_V3_NO_FALLBACK', '1', true)
	os.setenv('V_MACOS_MULTIWINDOW_TESTS', '0', true)

	progress_path := ci_progress_path()
	progress_dir := '${progress_path}.d'
	saved_index := if reset { -1 } else { ci_resume_index(progress_path)! }
	// No valid cursor means none of its finer-grained records may be reused.
	if saved_index < 0 && os.exists(progress_dir) {
		os.rmdir_all(progress_dir)!
	}
	if !os.exists(progress_dir) {
		os.mkdir(progress_dir, mode: 0o700)!
	}
	start := if saved_index < 0 { 0 } else { saved_index }
	os.unsetenv('VTEST_RESUME_OWNER')
	eprintln('CI progress: ${progress_path}')
	eprintln('Use `v run ci/macos_ci.vsh ci --reset` to restart from the first task.')
	if start > 0 {
		eprintln('Resuming at ${ci_tasks[start]}; skipping ${start} completed CI tasks.')
	}
	for i in start .. ci_tasks.len {
		task_name := ci_tasks[i]
		// Save BEFORE execution: a failure or interruption must retry this task.
		save_ci_progress(progress_path, task_name)!
		os.setenv('V_MACOS_CI_TASK_PROGRESS', os.join_path(progress_dir, task_name), true)
		eprintln('CI task ${i + 1}/${ci_tasks.len}: ${task_name}')
		exec('v run ci/macos_ci.vsh ${task_name}')
	}
	os.rmdir_all(progress_dir)!
	os.rm(progress_path)!
	eprintln('CI tasks complete; progress cleared.')
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
	'build_hello_world_autofree':         Task{build_hello_world_autofree, 'Build hello_world with -autofree'}
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
	'test_inline_assembly':               Task{test_inline_assembly, 'Test inline assembly'}
}

if os.args.len > 1 && os.args[1] == 'ci' {
	if os.args.len > 3 || (os.args.len == 3 && os.args[2] != '--reset') {
		eprintln('Usage: v run ci/macos_ci.vsh ci [--reset]')
		exit(1)
	}
	run_ci_tasks(os.args.len == 3) or {
		eprintln('Could not update CI progress: ${err.msg()}')
		exit(1)
	}
	exit(0)
}

common.run(all_tasks)
