module testing

import os
import rand
import v.build_constraint

fn batch_test_root(name string) !string {
	root := os.join_path(os.temp_dir(), 'vtest-batch-${name}-${rand.ulid()}')
	os.mkdir_all(root)!
	return root
}

fn test_top_level_names_cover_declarations_methods_and_blocks() {
	names := test_file_top_level_names('module util

import os

pub fn helper[T](value T) T {
	return value
}

fn (mut s &State) reset() {}
fn C.puts(&char) int
const limit = 3
const (
	first  = 1
	second = 2
)
pub struct State {}
enum Kind {
	a
}
type Alias = int
__global counter int
fn test_one() {}
')
	for name in ['helper', 'State.reset', 'C.puts', 'limit', 'first', 'second', 'State', 'Kind',
		'Alias', 'counter', 'test_one'] {
		assert name in names, name
	}
	assert 'a' !in names
	assert test_file_module('// comment\n\nmodule util\n') == 'util'
	assert test_file_module('fn main() {}\n') == ''
	assert test_file_const_names('const a = 1\npub const (\n\tb = 2\n)\n\$if x {\n\tconst c = 3\n}\n') == [
		'a',
		'b',
		'c',
	]
	assert source_mentions_word('x := it + 1', 'it')
	assert !source_mentions_word('item := 1', 'it')
}

fn test_environment_changes_batch_only_when_they_are_restored() {
	restored := "fn test_a() {\n\told := os.getenv_opt('X')\n\tos.setenv('X', '1', true)\n\tdefer {\n\t\tif v := old { os.setenv('X', v, true) } else { os.unsetenv('X') }\n\t}\n}\n"
	assert test_restores_environment(restored)
	assert !test_restores_environment("fn test_a() {\n\tos.setenv('X', '1', true)\n}\n")
	assert !test_restores_environment('fn test_a() {\n\tdefer {}\n\tassert os.getenv_opt("X") == none\n}\n')
}

fn test_units_keep_environment_changing_files_apart() ! {
	root := batch_test_root('environment')!
	defer {
		os.rmdir_all(root) or {}
	}
	restored := "module util\nfn test_NAME() {\n\told := os.getenv_opt('X')\n\tos.setenv('X', '1', true)\n\tdefer {\n\t\tif v := old { os.setenv('X', v, true) } else { os.unsetenv('X') }\n\t}\n}\n"
	files := {
		'a_test.v': 'module util\nfn test_a() {}\n'
		'b_test.v': restored.replace('NAME', 'b')
		'c_test.v': restored.replace('NAME', 'c')
		'd_test.v': "module util\nfn test_d() {\n\tos.setenv('X', '1', true)\n}\n"
	}
	mut paths := []string{}
	for name, source in files {
		path := os.join_path(root, name)
		os.write_file(path, source)!
		paths << path
	}
	mut ts := TestSession{
		vexe:              os.executable()
		will_compile:      true
		exec_mode:         .compile_and_run
		build_environment: build_constraint.new_environment(['macos'], [])
	}
	assert ts.test_units(paths) == [paths[0], '${paths[1]}\n${paths[2]}', paths[3]]
}

fn test_units_batch_files_that_restore_the_working_directory_apart() ! {
	root := batch_test_root('cwd')!
	defer {
		os.rmdir_all(root) or {}
	}
	restored := 'module util\nfn test_NAME() {\n\told := os.getwd()\n\tos.chdir(os.temp_dir()) or {}\n\tdefer {\n\t\tos.chdir(old) or {}\n\t}\n}\n'
	files := {
		'a_test.v': 'module util\nfn test_a() {}\n'
		'b_test.v': restored.replace('NAME', 'b')
		'c_test.v': restored.replace('NAME', 'c')
		'd_test.v': 'module util\nfn test_d() {\n\tos.chdir(os.temp_dir()) or {}\n}\n'
	}
	mut paths := []string{}
	for name, source in files {
		path := os.join_path(root, name)
		os.write_file(path, source)!
		paths << path
	}
	mut ts := TestSession{
		vexe:              os.executable()
		will_compile:      true
		exec_mode:         .compile_and_run
		build_environment: build_constraint.new_environment(['macos'], [])
	}
	assert ts.test_units(paths) == [paths[0], '${paths[1]}\n${paths[2]}', paths[3]]
}

fn test_units_group_files_by_vflags_and_accept_ownership_markers() ! {
	root := batch_test_root('vflags')!
	defer {
		os.rmdir_all(root) or {}
	}
	files := {
		'a_test.v': 'module util\nfn test_a() {}\n'
		'b_test.v': 'module util\n// vtest ownership\nfn test_b() {}\n'
		'c_test.v': 'module util\n// vtest vflags: -w\nfn test_c() {}\n'
		'd_test.v': 'module util\n// vtest vflags: -w\nfn test_d() {}\n'
		'e_test.v': 'module util\n// vtest vflags: -d x\nfn test_e() {}\n'
	}
	mut paths := []string{}
	for name, source in files {
		path := os.join_path(root, name)
		os.write_file(path, source)!
		paths << path
	}
	mut ts := TestSession{
		vexe:              os.executable()
		will_compile:      true
		exec_mode:         .compile_and_run
		build_environment: build_constraint.new_environment(['macos'], [])
	}
	assert ts.test_units(paths) == ['${paths[0]}\n${paths[1]}', '${paths[2]}\n${paths[3]}', paths[4]]
}

fn test_vflags_are_part_of_the_batch_key() {
	assert test_file_vflags('fn test_a() {}\n') == ''
	assert test_file_vflags('// vtest vflags: -w\nfn test_a() {}\n') == '-w'
	assert test_file_vflags('// vtest vflags: -d a\n// vtest vflags:  -d b \nfn test_a() {}\n') == '-d b'
}

fn test_env_cleanup_hook_accepts_only_unsetenv_lines() {
	cleanup := "import os\n\nfn testsuite_begin() {\n\tos.unsetenv('VFLAGS')\n\tos.unsetenv('VOSARGS')\n}\n\nfn test_a() {}\n"
	hook := test_env_cleanup_hook(test_code_text(cleanup), cleanup) or { panic('no hook') }
	assert hook.text == "os.unsetenv('VFLAGS')\nos.unsetenv('VOSARGS')"
	assert cleanup[hook.start..hook.end].starts_with('fn testsuite_begin() {')
	assert cleanup[hook.start..hook.end].ends_with('\n}')
	for body in ["\tos.setenv('VFLAGS', '', true)", '\tos.unsetenv(name)', '\tos.mkdir_all(root) or {}',
		''] {
		other := 'import os\n\nfn testsuite_begin() {\n${body}\n}\n'
		assert test_env_cleanup_hook(test_code_text(other), other) == none, body
	}
}

fn test_excluded_markers_are_calls_not_name_endings() {
	assert source_contains_marker('fn f() {\n\texit(1)\n}', 'exit(')
	assert source_contains_marker('fn f() {\n\tos.exit(1)\n}', 'exit(')
	assert source_contains_marker('println(arguments())', 'arguments()')
	assert !source_contains_marker('fn test_defer_early_exit() {}', 'exit(')
	assert !source_contains_marker('fn test_literal_arguments() {}', 'arguments()')
	assert !source_contains_marker('fn test_x() {\n\tmy_os.args_len()\n}', 'os.args')
}

fn test_code_text_blanks_string_literals_but_keeps_code() {
	source := 'fn a() {\n\tprogram := \'\nfn main() {\n\texit(1)\n}\n\'\n\tb := "x\${it.name}y"\n\tr := r\'\\\'\n\tq := `\'`\n\t// it\'s a comment\n}\nfn c() {}\n'
	code := test_code_text(source)
	assert code.len == source.len
	assert code.count('\n') == source.count('\n')
	assert !code.contains('main') && !code.contains('exit(')
	// Interpolations, comments and rune literals stay; the code after them too.
	assert code.contains('\${it.name}')
	assert code.contains("// it's a comment")
	assert code.contains("`'`")
	assert test_file_top_level_names(code) == ['a', 'c']
}

fn test_units_batch_internal_tests_of_one_module_without_name_clashes() ! {
	root := batch_test_root('units')!
	defer {
		os.rmdir_all(root) or {}
	}
	files := {
		'a_test.v':      'module util\nfn helper() {}\nfn test_a() {}\n'
		'b_test.v':      'module util\nfn test_b() {}\n'
		'c_test.v':      'module util\nfn helper() {}\nfn test_c() {}\n'
		'd_test.v':      'module util\nfn testsuite_begin() {}\nfn test_d() {}\n'
		'e_test.v':      'fn test_e() {}\n'
		'f_test.v':      'module util\n// vtest build: present_python?\nfn test_f() {}\n'
		'g_test.c.v':    'module util\nfn test_g() {}\n'
		'h_test.v':      'module util\nfn test_h() { os.setenv("X", "1", true) }\n'
		'global_test.v': 'module util\nfn test_global() {}\n'
		'i_test.v':      'module main\nfn test_i() {}\n'
		'j_test.v':      '#flag -lm\nfn test_j() {}\n'
		'k_test.v':      'module util\n\$if linux {\n\tconst it = 1\n}\nfn test_k() {}\n'
		'l_test.v':      'module util\nfn test_l() {\n\tit := 2\n\tassert it == 2\n}\n'
		// Programs in string literals declare and call what they like.
		'm_test.v':      "fn test_m() {\n\tp := '\nfn main() {\n\texit(1)\n}\n'\n\tassert p != ''\n}\n"
		'n_test.v':      "fn test_n() {\n\tp := '\nfn main() {\n\texit(2)\n}\n'\n\tassert p != ''\n}\n"
		// A build constraint that holds lets a file share a binary; `retry:` does not.
		'o_test.v':      'module util\n// vtest build: macos || linux\nfn test_o() {}\n'
		'p_test.v':      'module util\n// vtest retry: 2\nfn test_p() {}\n'
		// Test functions of one name share a batch, unless a file calls its own.
		'q_test.v':      'module util\nfn test_shared() {}\n'
		'r_test.v':      'module util\nfn test_shared() {}\n'
		's_test.v':      'module util\nfn test_shared() {}\nfn run() {\n\ttest_shared()\n}\n'
	}
	mut paths := []string{}
	for name, source in files {
		path := os.join_path(root, name)
		os.write_file(path, source)!
		paths << path
	}
	mut ts := TestSession{
		vexe:              os.executable()
		will_compile:      true
		exec_mode:         .compile_and_run
		build_environment: build_constraint.new_environment(['macos'], [])
	}
	units := ts.test_units(paths)
	// `k_test.v` declares a constant `it`, which `l_test.v` uses as a local.
	// `g_test.c.v` is a C backend test, which a batch is too.
	batch := '${paths[0]}\n${paths[1]}\n${paths[6]}\n${paths[11]}\n${paths[15]}\n${paths[17]}\n${paths[18]}'
	// `main` tests share batches of their own; one with a `module main` clause is
	// compiled with its directory's other `main` sources, so it does not join those
	// without a clause. A file with C flags builds alone.
	main_batch := '${paths[4]}\n${paths[13]}\n${paths[14]}'
	// `c_test.v` clashes with `a_test.v` and starts a second batch of its own.
	assert units == [batch, '${paths[2]}\n${paths[12]}\n${paths[19]}', paths[3], main_batch, paths[5],
		paths[7], paths[8], paths[9], paths[10], paths[16]]
	ts.exec_mode = .compile
	assert ts.test_units(paths) == paths
	ts.exec_mode = .compile_and_run
	ts.vargs = '-b js'
	assert ts.test_units(paths) == paths
}

// `v vet` gets the same options for every file of a command (see tool_units).
fn test_tool_units_group_files_that_get_the_same_vet_options() ! {
	root := batch_test_root('tool-units')!
	defer {
		os.rmdir_all(root) or {}
	}
	mut paths := []string{}
	for name in ['a.v', 'global_b.v', 'c.v', 'global_d.v'] {
		path := os.join_path(root, name)
		os.write_file(path, 'module main\n')!
		paths << path
	}
	vet := TestSession{
		vargs: '-silent vet'
	}
	assert vet.can_batch_tool_runs()
	assert vet.tool_units(paths) == ['${paths[0]}\n${paths[2]}', '${paths[1]}\n${paths[3]}']
	fmt := TestSession{
		vargs: '-silent fmt -inprocess -verify'
	}
	assert fmt.can_batch_tool_runs()
	assert fmt.tool_units(paths) == [paths.join('\n')]
	assert TestSession{
		vargs: 'fmt -inprocess -l'
	}.can_batch_tool_runs()
	assert !TestSession{
		vargs: 'fmt -w'
	}.can_batch_tool_runs()
}

// A command over several files that rejects one of them names that file, and only that one.
fn test_tool_batch_that_fails_names_only_the_rejected_file() ! {
	root := batch_test_root('tool-batch')!
	defer {
		os.rmdir_all(root) or {}
	}
	mut paths := []string{}
	for name, source in {
		'a.v': 'module main\n\nfn main() {\n\tprintln(1)\n}\n'
		'b.v': 'module main\nfn main() {\n  x:=1\n println(x)\n}\n'
		'c.v': 'module main\n\nfn main() {\n\tprintln(3)\n}\n'
	} {
		path := os.join_path(root, name)
		os.write_file(path, source)!
		paths << path
	}
	mut ts := new_test_session('-silent fmt -inprocess -verify', false)
	// CI sets VTEST_FAIL_FAST=1; the counts below need every file of the session to run.
	ts.fail_fast = false
	ts.vexe = @VEXE
	ts.files << paths
	ts.test()
	assert ts.benchmark.nok == 2
	assert ts.benchmark.nfail == 1
	rlock ts.failed_files {
		assert ts.failed_files == [os.real_path(paths[1])]
	}
}

// `v test-fmt` fails only the files that cannot be formatted; one that formats differently passes.
fn test_fmt_list_batch_fails_only_the_file_that_cannot_be_formatted() ! {
	root := batch_test_root('fmt-list-batch')!
	defer {
		os.rmdir_all(root) or {}
	}
	mut paths := []string{}
	for name, source in {
		'a.v': 'module main\n\nfn main() {\n\tprintln(1)\n}\n'
		'b.v': 'module main\nfn main( {\n'
		'c.v': 'module main\nfn main() {\n  x:=1\n println(x)\n}\n'
	} {
		path := os.join_path(root, name)
		os.write_file(path, source)!
		paths << path
	}
	mut ts := new_test_session('-silent fmt -inprocess -l', false)
	// CI sets VTEST_FAIL_FAST=1; the counts below need every file of the session to run.
	ts.fail_fast = false
	ts.vexe = @VEXE
	ts.files << paths
	ts.test()
	assert ts.benchmark.nok == 2
	assert ts.benchmark.nfail == 1
	rlock ts.failed_files {
		assert ts.failed_files == [os.real_path(paths[1])]
	}
}
