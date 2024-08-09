import os
import benchmark
import term

const is_verbose = os.getenv('VTEST_SHOW_CMD') != ''

// TODO: some logic copy pasted from valgrind_test.v and compiler_test.v, move to a module
fn test_wasm() {
	mut runtimes := ['wasmer', 'wasmtime', 'wavm', 'wasm3']
	mut runtime_found := false

	for runtime in runtimes {
		basename := $if windows { runtime + '.exe' } $else { runtime }

		if rf := os.find_abs_path_of_executable(basename) {
			runtime_found = true
			break
		}
	}

	if !runtime_found {
		eprintln('cannot find suitable wasm runtime, exiting...')
		if os.getenv('VTEST_ONLY') == 'wasm' {
			exit(1)
		}
		exit(0)
	}

	mut bench := benchmark.new_benchmark()
	vexe := os.getenv('VEXE')
	vroot := os.dir(vexe)
	dir := os.join_path(vroot, 'vlib/v/gen/wasm/tests')
	files := os.ls(dir)!

	wrkdir := os.join_path(os.vtmp_dir(), 'wasm_tests')
	os.mkdir_all(wrkdir)!
	defer {
		os.rmdir_all(wrkdir) or {}
	}
	os.chdir(wrkdir)!
	mut tests := files.filter(it.ends_with('.vv'))
	if tests.len == 0 {
		println('no wasm tests found')
		assert false
	}
	$if windows {
		// FIXME:
		if os.getenv('CI') == 'true' {
			tests = tests.filter(it !in ['arrays.vv', 'asm.vv', 'builtin.vv'])
		}
	}
	bench.set_total_expected_steps(tests.len)
	for test in tests {
		bench.step()
		full_test_path := os.real_path(os.join_path(dir, test))
		test_file_name := os.file_name(test)
		relative_test_path := full_test_path.replace(vroot + '/', '')
		work_test_path := '${wrkdir}/${test_file_name}'
		tmperrfile := '${dir}/${test}.tmperr'
		outfile := '${dir}/${test}.out'
		// force binaryen to print without colour
		cmd := '${os.quoted_path(vexe)} -b wasm run ${os.quoted_path(full_test_path)} 2> ${os.quoted_path(tmperrfile)}'
		if is_verbose {
			println(cmd)
		}
		res_wasm := os.execute(cmd)
		if res_wasm.exit_code != 0 {
			bench.fail()
			eprintln(bench.step_message_fail(cmd))

			if os.exists(tmperrfile) {
				err := os.read_file(tmperrfile)!
				eprintln(err)
			}

			continue
		}
		os.rm(tmperrfile) or {}
		if expected_ := os.read_file(outfile) {
			mut expected := expected_
			expected = expected.trim_right('\r\n').replace('\r\n', '\n')
			mut found := res_wasm.output.trim_right('\r\n').replace('\r\n', '\n')
			found = found.trim_space()
			if expected != found {
				println(term.red('FAIL'))
				println('============')
				println('expected: "${expected}" len=${expected.len}')
				println('============')
				println('found:"${found}" len=${found.len}')
				println('============\n')
				bench.fail()
				continue
			}
		} else {
			os.write_file(outfile, res_wasm.output.trim_right('\r\n').replace('\r\n',
				'\n'))!
		}
		bench.ok()
		eprintln(bench.step_message_ok(relative_test_path))
	}
	bench.stop()
	eprintln(term.h_divider('-'))
	eprintln(bench.total_message('wasm'))
	if bench.nfail > 0 {
		exit(1)
	}
}
