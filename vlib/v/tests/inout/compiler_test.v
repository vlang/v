// .out file:
// To test a panic, remove everything after the long `===` line
// You can also remove the line with 'line:' e.g. for a builtin fn

import os
import term
import v.util
import v.util.vtest

const turn_off_vcolors = os.setenv('VCOLORS', 'never', true)

fn test_all() {
	mut total_errors := 0
	vexe := os.getenv('VEXE')
	vroot := os.dir(vexe)
	os.chdir(vroot)
	diff_cmd := util.find_working_diff_command() or { '' }
	dir := 'vlib/v/tests/inout'
	files := os.ls(dir) or { panic(err) }
	tests := files.filter(it.ends_with('.vv'))
	if tests.len == 0 {
		println('no compiler tests found')
		assert false
	}
	paths := vtest.filter_vtest_only(tests,
		basepath: dir
	)
	for path in paths {
		print(path + ' ')
		program := path
		compilation := os.exec('$vexe -o test -cflags "-w" -cg $program') or { panic(err) }
		if compilation.exit_code != 0 {
			panic('compilation failed: $compilation.output')
		}
		res := os.exec('./test') or {
			println('nope')
			panic(err)
		}
		$if windows {
			os.rm('./test.exe') or { }
			$if msvc {
				os.rm('./test.ilk') or { }
				os.rm('./test.pdb') or { }
			}
		} $else {
			os.rm('./test') or { }
		}
		// println('============')
		// println(res.output)
		// println('============')
		mut found := res.output.trim_right('\r\n').replace('\r\n', '\n')
		mut expected := os.read_file(program.replace('.vv', '') + '.out') or { panic(err) }
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
				println(util.color_compare_strings(diff_cmd, expected, found))
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

fn normalize_panic_message(message string, vroot string) string {
	mut msg := message.all_before('=========================================')
	// change windows to nix path
	s := vroot.replace(os.path_separator, '/')
	// remove vroot
	msg = msg.replace(s + '/', '')
	msg = msg.trim_space()
	return msg
}
