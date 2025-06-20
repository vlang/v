// vtest build: !self_sandboxed_packaging?
import os
import time
import term
import v.util.diff
import v.util.vtest

const vexe = @VEXE

const vroot = os.real_path(@VMODROOT)

const testdata_folder = os.join_path(vroot, 'vlib', 'v', 'gen', 'c', 'testdata')

const show_compilation_output = os.getenv('VTEST_SHOW_COMPILATION_OUTPUT').int() == 1

const user_os = os.user_os()

const gcc_path = os.find_abs_path_of_executable('gcc') or { '' }

fn mm(s string) string {
	return term.colorize(term.magenta, s)
}

fn mj(input ...string) string {
	return mm(input.filter(it.len > 0).join(' '))
}

fn test_out_files() {
	println(term.colorize(term.green, '> testing whether .out files match:'))
	os.chdir(vroot) or {}
	output_path := os.join_path(os.vtmp_dir(), 'coutput_outs')
	os.mkdir_all(output_path)!
	defer {
		os.rmdir_all(output_path) or {}
	}
	files := os.ls(testdata_folder) or { [] }
	tests := files.filter(it.ends_with('.out'))
	if tests.len == 0 {
		eprintln('no `.out` tests found in ${testdata_folder}')
		return
	}
	paths := vtest.filter_vtest_only(tests, basepath: testdata_folder).sorted()
	mut total_errors := 0
	for out_path in paths {
		basename, path, relpath, out_relpath := target2paths(out_path, '.out')
		if should_skip(relpath) {
			continue
		}
		pexe := os.join_path(output_path, '${basename}.exe')
		//
		file_options := get_file_options(path)
		alloptions := '-o ${os.quoted_path(pexe)} ${file_options.vflags}'
		label := mj('v', file_options.vflags, 'run', relpath) + ' == ${mm(out_relpath)} '
		//
		compile_cmd := '${os.quoted_path(vexe)} ${alloptions} ${os.quoted_path(path)}'
		sw_compile := time.new_stopwatch()
		compilation := os.execute(compile_cmd)
		compile_ms := sw_compile.elapsed().milliseconds()
		ensure_compilation_succeeded(compilation, compile_cmd)
		//
		sw_run := time.new_stopwatch()
		res := os.execute(os.quoted_path(pexe))
		run_ms := sw_run.elapsed().milliseconds()
		//
		if res.exit_code < 0 {
			println('nope')
			panic(res.output)
		}
		mut found := res.output.trim_right('\r\n').replace('\r\n', '\n')
		mut expected := os.read_file(out_path)!
		expected = expected.trim_right('\r\n').replace('\r\n', '\n')
		if expected.contains('================ V panic ================') {
			// panic include backtraces and absolute file paths, so can't do char by char comparison
			n_found := normalize_panic_message(found, vroot)
			n_expected := normalize_panic_message(expected, vroot)
			if found.contains('================ V panic ================') {
				if n_found.starts_with(n_expected) {
					println('${term.green('OK (panic)')} C:${compile_ms:6}ms, R:${run_ms:2}ms ${label}')
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
			println('${term.red('FAIL')} C:${compile_ms:6}ms, R:${run_ms:2}ms ${label}')
			if diff_ := diff.compare_text(expected, found) {
				println(term.header('difference:', '-'))
				println(diff_)
			} else {
				println(term.header('expected:', '-'))
				println(expected)
				println(term.header('found:', '-'))
				println(found)
			}
			println(term.h_divider('-'))
			total_errors++
		} else {
			println('${term.green('OK  ')} C:${compile_ms:6}ms, R:${run_ms:2}ms ${label}')
		}
	}
	assert total_errors == 0
}

fn test_c_must_have_files() {
	println(term.colorize(term.green, '> testing whether all line patterns in `.c.must_have` files match:'))
	os.chdir(vroot) or {}
	output_path := os.join_path(os.vtmp_dir(), 'coutput_c_must_haves')
	os.mkdir_all(output_path)!
	defer {
		os.rmdir_all(output_path) or {}
	}
	files := os.ls(testdata_folder) or { [] }
	tests := files.filter(it.ends_with('.c.must_have'))
	if tests.len == 0 {
		eprintln('no `.c.must_have` files found in ${testdata_folder}')
		return
	}
	paths := vtest.filter_vtest_only(tests, basepath: testdata_folder).sorted()
	mut total_errors := 0
	mut failed_descriptions := []string{cap: paths.len}
	for must_have_path in paths {
		basename, path, relpath, must_have_relpath := target2paths(must_have_path, '.c.must_have')
		if should_skip(relpath) {
			continue
		}
		file_options := get_file_options(path)
		alloptions := '-o - ${file_options.vflags}'
		mut description := mj('v', alloptions, relpath) + ' matches ${mm(must_have_relpath)} '
		cmd := '${os.quoted_path(vexe)} ${alloptions} ${os.quoted_path(path)}'
		sw_compile := time.new_stopwatch()
		compilation := os.execute(cmd)
		compile_ms := sw_compile.elapsed().milliseconds()
		ensure_compilation_succeeded(compilation, cmd)
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
				description += '\n failed pattern: `${eline}`'
				println('${term.red('FAIL')} C:${compile_ms:5}ms ${description}')
				eprintln('${must_have_path}:${idx_expected_line + 1}: expected match error:')
				eprintln('`${cmd}` did NOT produce expected line:')
				eprintln(term.colorize(term.red, eline))
				if description !in failed_descriptions {
					failed_descriptions << description
				}
				total_errors++
				continue
			}
		}
		if nmatches == expected_lines.len {
			println('${term.green('OK  ')} C:${compile_ms:5}ms ${description}')
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
			eprintln('  > ${fd}')
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

fn ensure_compilation_succeeded(compilation os.Result, cmd string) {
	if compilation.exit_code < 0 {
		eprintln('> cmd exit_code < 0, cmd: ${cmd}')
		panic(compilation.output)
	}
	if compilation.exit_code != 0 {
		eprintln('> cmd exit_code != 0, cmd: ${cmd}')
		panic('compilation failed: ${compilation.output}')
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

fn should_skip(relpath string) bool {
	if user_os == 'windows' {
		if relpath.contains('_nix.vv') {
			eprintln('> skipping ${relpath} on windows')
			return true
		}
		$if !msvc {
			if relpath.contains('_msvc_windows.vv') {
				eprintln('> skipping ${relpath} on !msvc')
				return true
			}
		}
		$if !gcc {
			if relpath.contains('_gcc_windows.vv') {
				eprintln('> skipping ${relpath} on !gcc')
				return true
			}
		}
	} else {
		if relpath.contains('_windows.vv') {
			eprintln('> skipping ${relpath} on !windows')
			return true
		}
	}
	if relpath.contains('freestanding_module_import_') {
		$if !amd64 {
			// https://github.com/vlang/v/issues/23397
			eprintln('> skipping ${relpath} on != amd64')
			return true
		}
		if user_os != 'linux' {
			eprintln('> skipping ${relpath} on != linux')
			return true
		}
		if gcc_path == '' {
			eprintln('> skipping ${relpath} since it needs gcc, which is not detected')
			return true
		}
	}
	return false
}
