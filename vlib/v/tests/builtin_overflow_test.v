// vtest build: !self_sandboxed_packaging?
import os
import time
import term
import v.util.diff
import v.util.vtest

const vexe = @VEXE

const vroot = os.real_path(@VMODROOT)

const testdata_folder = os.join_path(vroot, 'vlib/v/tests/testdata/builtin_overflow')

fn mm(s string) string {
	return term.colorize(term.magenta, s)
}

fn mj(input ...string) string {
	return mm(input.filter(it.len > 0).join(' '))
}

fn test_out_files() {
	println(term.colorize(term.green, '> testing whether .out files match:'))
	os.chdir(vroot) or {}
	output_path := os.join_path(os.vtmp_dir(), 'overflow_outs')
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
		pexe := os.join_path(output_path, '${basename}.exe')
		//
		file_options := '-g -check-overflow'
		alloptions := '-o ${os.quoted_path(pexe)} ${file_options}'
		label := mj('v', file_options, 'run', relpath) + ' == ${mm(out_relpath)} '
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
