import os
import term
import v.util.vtest
import v.util.diff

struct FileOptions {
mut:
	vflags string
}

fn get_file_options(file string) FileOptions {
	mut res := FileOptions{}
	lines := os.read_lines(file) or { [] }
	for line in lines {
		if line.starts_with('// vtest vflags:') {
			res.vflags = line.all_after(':').trim_space()
		}
	}
	return res
}

fn test_vet() {
	vexe := os.getenv('VEXE')
	vroot := os.dir(vexe)
	os.chdir(vroot)!
	test_dir := 'cmd/tools/vvet/tests'
	tests := get_tests_in_dir(test_dir)
	fails := check_path(vexe, test_dir, tests)
	assert fails == 0
}

// test_builtin_inline_calls_from_other_modules verifies that unqualified calls
// to a builtin function made from another module still contribute to its inline
// threshold. Builtin declarations have a bare `fkey()` (e.g. `my_helper`), while
// the parser records such calls with the caller's module (e.g. `main.my_helper`),
// so they must be resolved back to the bare builtin key when counting.
fn test_builtin_inline_calls_from_other_modules() {
	vexe := os.getenv('VEXE')
	// Note: `os.temp_dir()` (not `os.vtmp_dir()`) is used on purpose, since `v vet`
	// filters out paths located inside `VTMP`.
	tmp := os.join_path(os.temp_dir(), 'vvet_builtin_inline_${os.getpid()}')
	os.rmdir_all(tmp) or {}
	os.mkdir_all(os.join_path(tmp, 'builtin'))!
	defer {
		os.rmdir_all(tmp) or {}
	}
	// A `v.mod` at the root makes the `builtin` sub folder resolve to `module builtin`.
	os.write_file(os.join_path(tmp, 'v.mod'), "Module {\n\tname: 'vvet_builtin_inline'\n}\n")!
	os.write_file(os.join_path(tmp, 'builtin', 'helper.v'),
		'module builtin\n\nfn my_helper(a int, b int) int {\n\treturn a + b\n}\n')!
	mut calls := []string{}
	for _ in 0 .. 10 {
		calls << '\tprintln(my_helper(1, 1))'
	}
	os.write_file(os.join_path(tmp, 'caller.v'),
		'module main\n\nfn main() {\n${calls.join('\n')}\n}\n')!
	res := os.execute('${os.quoted_path(vexe)} vet -nocolor -I ${os.quoted_path(tmp)}')
	assert res.exit_code >= 0, res.output
	assert res.output.contains('my_helper fn might be inlined'), res.output
}

// test_local_function_shadows_builtin_inline_calls verifies that unqualified calls
// are not credited to a same-named builtin function when the caller module declares
// its own function.
fn test_local_function_shadows_builtin_inline_calls() {
	vexe := os.getenv('VEXE')
	tmp := os.join_path(os.temp_dir(), 'vvet_builtin_shadow_${os.getpid()}')
	os.rmdir_all(tmp) or {}
	os.mkdir_all(os.join_path(tmp, 'builtin'))!
	defer {
		os.rmdir_all(tmp) or {}
	}
	os.write_file(os.join_path(tmp, 'v.mod'), "Module {\n\tname: 'vvet_builtin_shadow'\n}\n")!
	os.write_file(os.join_path(tmp, 'builtin', 'helper.v'),
		'module builtin\n\nfn my_helper(a int, b int) int {\n\treturn a + b\n}\n')!
	mut calls := []string{}
	for _ in 0 .. 10 {
		calls << '\tprintln(my_helper(1, 1))'
	}
	os.write_file(os.join_path(tmp, 'caller.v'),
		'module main\n\n@[inline]\nfn my_helper(a int, b int) int {\n\treturn a + b\n}\n\nfn main() {\n${calls.join('\n')}\n}\n')!
	res := os.execute('${os.quoted_path(vexe)} vet -nocolor -I ${os.quoted_path(tmp)}')
	assert res.exit_code >= 0, res.output
	assert !res.output.contains('my_helper fn might be inlined'), res.output
}

// test_for_c_body_is_analyzed_before_increment verifies that stateful vet checks
// observe a C-style loop body before an assignment in its increment clause.
fn test_for_c_body_is_analyzed_before_increment() {
	vexe := os.getenv('VEXE')
	path := os.join_path(os.temp_dir(), 'vvet_for_c_order_${os.getpid()}.v')
	os.rm(path) or {}
	defer {
		os.rm(path) or {}
	}
	os.write_file(path,
		"import regex\n\nfn main() {\n\tmut re := regex.new()\n\tfor ; false; re = 0 {\n\t\tre.compile_opt(r'foo|bar') or { panic(err) }\n\t}\n}\n")!
	res := os.execute('${os.quoted_path(vexe)} vet -nocolor ${os.quoted_path(path)}')
	assert res.exit_code >= 0, res.output
	assert res.output.contains('Confusing regex `|` in `foo|bar`'), res.output
}

fn get_tests_in_dir(dir string) []string {
	files := os.ls(dir) or { panic(err) }
	mut tests := files.filter(it.ends_with('.vv'))
	tests.sort()
	return tests
}

fn check_path(vexe string, dir string, tests []string) int {
	mut nb_fail := 0
	paths := vtest.filter_vtest_only(tests, basepath: dir)
	for path in paths {
		program := path
		print(path + ' ')
		file_options := get_file_options(path)
		res :=
			os.execute('${os.quoted_path(vexe)} vet -nocolor ${file_options.vflags} ${os.quoted_path(program)}')
		if res.exit_code < 0 {
			panic(res.output)
		}
		mut expected := os.read_file(program.replace('.vv', '') + '.out') or { panic(err) }
		expected = clean_line_endings(expected)
		found := clean_line_endings(res.output)
		if expected != found {
			println(term.red('FAIL'))
			println('============')
			if diff_ := diff.compare_text(expected, found) {
				println('diff:')
				println(diff_)
			} else {
				println('expected:')
				println(expected)
				println('============')
				println('found:')
				println(found)
			}
			println('============\n')
			nb_fail++
		} else {
			println(term.green('OK'))
		}
	}
	return nb_fail
}

fn clean_line_endings(s string) string {
	mut res := s.trim_space()
	res = res.replace(' \n', '\n')
	res = res.replace(' \r\n', '\n')
	res = res.replace('\r\n', '\n')
	res = res.trim('\n')
	return res
}
