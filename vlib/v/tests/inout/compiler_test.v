import os
import term
import v.util

fn test_all() {
	mut total_errors := 0
	vexe := os.getenv('VEXE')
	vroot := os.dir(vexe)
	diff_cmd := util.find_working_diff_command() or { '' }
	dir := 'vlib/v/tests/inout'
	files := os.ls(dir) or {
		panic(err)
	}
	tests := files.filter(it.ends_with('.vv'))
	if tests.len == 0 {
		println('no compiler tests found')
		assert false
	}
	for test in tests {
		path := os.join_path(dir, test).replace('\\', '/')
		print(path + ' ')
		program := path.replace('.vv', '.v')
		os.cp(path, program) or {
			panic(err)
		}
		_ := os.exec('$vexe -o exe -cflags "-w" -cg $program') or {
			panic(err)
		}
		// os.rm(program)
		res := os.exec('./exe') or {
			println('nope')
			panic(err)
		}
		os.rm('./exe')
		// println('============')
		// println(res.output)
		// println('============')
		mut found := res.output.trim_space().trim('\n').replace('\r\n', '\n')
		mut expected := os.read_file(program.replace('.v', '') + '.out') or {
			panic(err)
		}
		expected = expected.trim_space().trim('\n').replace('\r\n', '\n')
		if expected.contains('================ V panic ================') {
			// panic include backtraces and absolute file paths, so can't do char by char comparison
			n_found := normalize_panic_message( found, vroot )
			n_expected := normalize_panic_message( expected, vroot )
			if found.contains('================ V panic ================') {
				if n_found == n_expected {
					println(term.green('OK (panic)'))
					continue
				} else {
					found = n_found
					expected = n_expected
				}
			}
		}
		if expected != found {
			println(term.red('FAIL'))
			println(term.header('expected:','-'))
			println(expected)
			println(term.header('found:','-'))
			println(found)
			if diff_cmd != '' {
				println(term.header('difference:','-'))
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
	panic_line_before_backtrace := '========================================='
	// panic include backtraces and absolute file paths, so can't do char by char comparison
	mut msg := message.all_before(panic_line_before_backtrace) + panic_line_before_backtrace
	msg = msg.replace(vroot + os.path_separator, '')
	msg = msg.trim_space()
	return msg
}
