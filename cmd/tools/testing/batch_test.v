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
	batch := '${paths[0]}\n${paths[1]}\n${paths[11]}\n${paths[15]}\n${paths[17]}\n${paths[18]}'
	// `main` tests share batches of their own; one with a `module main` clause is
	// compiled with its directory's other `main` sources, so it does not join those
	// without a clause. A file with C flags builds alone.
	main_batch := '${paths[4]}\n${paths[13]}\n${paths[14]}'
	// `c_test.v` clashes with `a_test.v` and starts a second batch of its own.
	assert units == [batch, '${paths[2]}\n${paths[12]}\n${paths[19]}', paths[3], main_batch, paths[5],
		paths[6], paths[7], paths[8], paths[9], paths[10], paths[16]]
	ts.exec_mode = .compile
	assert ts.test_units(paths) == paths
	ts.exec_mode = .compile_and_run
	ts.vargs = '-b js'
	assert ts.test_units(paths) == paths
}
