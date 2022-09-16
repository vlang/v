import os
import rand
import term
import v.util.diff
import v.util.vtest

const vexe = @VEXE

const vroot = os.real_path(@VMODROOT)

const testdata_folder = os.join_path(vroot, 'vlib', 'v', 'gen', 'c', 'testdata')

const diff_cmd = diff.find_working_diff_command() or { '' }

const show_compilation_output = os.getenv('VTEST_SHOW_COMPILATION_OUTPUT').int() == 1

fn mm(s string) string {
	return term.colorize(term.magenta, s)
}

fn test_out_files() ? {
	println(term.colorize(term.green, '> testing whether .out files match:'))
	os.chdir(vroot) or {}
	output_path := os.join_path(os.temp_dir(), 'v', 'coutput', 'out')
	os.mkdir_all(output_path)?
	defer {
		os.rmdir_all(output_path) or {}
	}
	files := os.ls(testdata_folder) or { [] }
	tests := files.filter(it.ends_with('.out'))
	if tests.len == 0 {
		eprintln('no `.out` tests found in $testdata_folder')
		return
	}
	paths := vtest.filter_vtest_only(tests, basepath: testdata_folder)
	mut total_errors := 0
	for out_path in paths {
		basename, path, relpath, out_relpath := target2paths(out_path, '.out')
		pexe := os.join_path(output_path, '${basename}.exe')
		//
		file_options := get_file_options(path)
		alloptions := '-o ${os.quoted_path(pexe)} $file_options.vflags'
		print(mm('v $alloptions run $relpath') + ' == ${mm(out_relpath)} ')
		//
		compilation := os.execute('${os.quoted_path(vexe)} $alloptions ${os.quoted_path(path)}')
		ensure_compilation_succeeded(compilation)
		res := os.execute(os.quoted_path(pexe))
		if res.exit_code < 0 {
			println('nope')
			panic(res.output)
		}
		mut found := res.output.trim_right('\r\n').replace('\r\n', '\n')
		mut expected := os.read_file(out_path)?
		expected = expected.trim_right('\r\n').replace('\r\n', '\n')
		if expected.contains('================ V panic ================') {
			// panic include backtraces and absolute file paths, so can't do char by char comparison
			n_found := normalize_panic_message(found, vroot)
			n_expected := normalize_panic_message(expected, vroot)
			if found.contains('================ V panic ================') {
				if n_found.starts_with(n_expected) {
					println(term.green('OK (panic)'))
					continue
				} else {
					// Both have panics, but there was a difference...
					// Pass the normalized strings for further reporting.
					// There is no point in comparing the backtraces too.
					found = n_found
					expected = n_expected
				}
			}
		}
		if expected != found {
			println(term.red('FAIL'))
			println(term.header('expected:', '-'))
			println(expected)
			println(term.header('found:', '-'))
			println(found)
			if diff_cmd != '' {
				println(term.header('difference:', '-'))
				println(diff.color_compare_strings(diff_cmd, rand.ulid(), expected, found))
			} else {
				println(term.h_divider('-'))
			}
			total_errors++
		} else {
			println(term.green('OK'))
		}
	}
	assert total_errors == 0
}

fn test_c_must_have_files() ? {
	println(term.colorize(term.green, '> testing whether `.c.must_have` files match:'))
	os.chdir(vroot) or {}
	output_path := os.join_path(os.temp_dir(), 'v', 'coutput', 'c_must_have')
	os.mkdir_all(output_path)?
	defer {
		os.rmdir_all(output_path) or {}
	}
	files := os.ls(testdata_folder) or { [] }
	tests := files.filter(it.ends_with('.c.must_have'))
	if tests.len == 0 {
		eprintln('no `.c.must_have` files found in $testdata_folder')
		return
	}
	paths := vtest.filter_vtest_only(tests, basepath: testdata_folder)
	mut total_errors := 0
	mut failed_descriptions := []string{cap: paths.len}
	for must_have_path in paths {
		basename, path, relpath, must_have_relpath := target2paths(must_have_path, '.c.must_have')
		file_options := get_file_options(path)
		alloptions := '-o - $file_options.vflags'
		description := mm('v $alloptions $relpath') +
			' matches all line patterns in ${mm(must_have_relpath)} '
		print(description)
		cmd := '${os.quoted_path(vexe)} $alloptions ${os.quoted_path(path)}'
		compilation := os.execute(cmd)
		ensure_compilation_succeeded(compilation)
		expected_lines := os.read_lines(must_have_path) or { [] }
		generated_c_lines := compilation.output.split_into_lines()
		mut nmatches := 0
		mut failed_patterns := []string{}
		for idx_expected_line, eline in expected_lines {
			if does_line_match_one_of_generated_lines(eline, generated_c_lines) {
				nmatches++
				// eprintln('> testing: $must_have_path has line: $eline')
			} else {
				failed_patterns << eline
				println(term.red('FAIL'))
				eprintln('$must_have_path:${idx_expected_line + 1}: expected match error:')
				eprintln('`$cmd` did NOT produce expected line:')
				eprintln(term.colorize(term.red, eline))
				if description !in failed_descriptions {
					failed_descriptions << description
				}
				total_errors++
				continue
			}
		}
		if nmatches == expected_lines.len {
			println(term.green('OK'))
		} else {
			if show_compilation_output {
				eprintln('> ALL lines:')
				eprintln(compilation.output)
			}
			eprintln('--------- failed patterns: -------------------------------------------')
			for fpattern in failed_patterns {
				eprintln(fpattern)
			}
			eprintln('----------------------------------------------------------------------')
		}
	}
	if failed_descriptions.len > 0 {
		eprintln('--------- failed commands: -------------------------------------------')
		for fd in failed_descriptions {
			eprintln('  > $fd')
		}
		eprintln('----------------------------------------------------------------------')
	}
	assert total_errors == 0
}

fn does_line_match_one_of_generated_lines(line string, generated_c_lines []string) bool {
	for cline in generated_c_lines {
		if line == cline {
			return true
		}
		if cline.contains(line) {
			return true
		}
	}
	return false
}

fn normalize_panic_message(message string, vroot string) string {
	mut msg := message.all_before('=========================================')
	// change windows to nix path
	s := vroot.replace(os.path_separator, '/')
	msg = msg.replace(s + '/', '')
	msg = msg.trim_space()
	return msg
}

fn vroot_relative(opath string) string {
	nvroot := vroot.replace(os.path_separator, '/') + '/'
	npath := opath.replace(os.path_separator, '/')
	return npath.replace(nvroot, '')
}

fn ensure_compilation_succeeded(compilation os.Result) {
	if compilation.exit_code < 0 {
		panic(compilation.output)
	}
	if compilation.exit_code != 0 {
		panic('compilation failed: $compilation.output')
	}
}

fn target2paths(target_path string, postfix string) (string, string, string, string) {
	basename := os.file_name(target_path).replace(postfix, '')
	target_dir := os.dir(target_path)
	path := os.join_path(target_dir, '${basename}.vv')
	relpath := vroot_relative(path)
	target_relpath := vroot_relative(target_path)
	return basename, path, relpath, target_relpath
}

struct FileOptions {
mut:
	vflags string
}

pub fn get_file_options(file string) FileOptions {
	mut res := FileOptions{}
	lines := os.read_lines(file) or { [] }
	for line in lines {
		if line.starts_with('// vtest vflags:') {
			res.vflags = line.all_after(':').trim_space()
		}
	}
	return res
}
