// .out file:
// To test a panic, remove everything after the long `===` line
// You can also remove the line with 'line:' e.g. for a builtin fn
import os
import rand
import term
import v.util.diff
import v.util.vtest

const turn_off_vcolors = os.setenv('VCOLORS', 'never', true)

const skip_files = [
	'do_not_remove_this',
	'tmpl_parse_html.vv', // skipped, due to a V template compilation problem after b42c824
]

fn test_all() {
	mut total_errors := 0
	vexe := os.getenv('VEXE')
	vroot := os.dir(vexe)
	os.chdir(vroot) or {}
	diff_cmd := diff.find_working_diff_command() or { '' }
	dir := 'vlib/v/tests/inout'
	files := os.ls(dir) or { panic(err) }
	tests := files.filter(it.ends_with('.vv') || it.ends_with('.vsh'))
	if tests.len == 0 {
		println('no compiler tests found')
		assert false
	}
	paths := vtest.filter_vtest_only(tests, basepath: dir)
	for path in paths {
		print(path + ' ')
		fname := os.file_name(path)
		if fname in skip_files {
			println(term.bright_yellow('SKIP'))
			continue
		}
		program := path
		tname := rand.ulid()
		compilation := os.execute('${os.quoted_path(vexe)} -o ${tname} -cflags "-w" -cg ${os.quoted_path(program)}')
		if compilation.exit_code < 0 {
			panic(compilation.output)
		}
		if compilation.exit_code != 0 {
			panic('compilation failed: ${compilation.output}')
		}
		res := os.execute('./${tname}')
		if res.exit_code < 0 {
			println('nope')
			panic(res.output)
		}
		$if windows {
			os.rm('./${tname}.exe') or {}
			$if msvc {
				os.rm('./${tname}.ilk') or {}
				os.rm('./${tname}.pdb') or {}
			}
		} $else {
			os.rm('./${tname}') or {}
		}
		// println('============')
		// println(res.output)
		// println('============')
		mut found := res.output.trim_right('\r\n').replace('\r\n', '\n')
		mut expected := os.read_file(program.replace('.vv', '').replace('.vsh', '') + '.out') or {
			panic(err)
		}
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

fn normalize_panic_message(message string, vroot string) string {
	mut msg := message.all_before('=========================================')
	// change windows to nix path
	s := vroot.replace(os.path_separator, '/')
	// remove vroot
	msg = msg.replace(s + '/', '')
	msg = msg.trim_space()
	return msg
}
