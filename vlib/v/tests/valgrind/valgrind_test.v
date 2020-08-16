import os
import term
import benchmark
import v.util
import v.util.vtest

const (
	skip_valgrind_files = [
		'vlib/v/tests/valgrind/struct_field.vv',
		'vlib/v/tests/valgrind/fn_returning_string_param.vv',
		'vlib/v/tests/valgrind/fn_with_return_should_free_local_vars.vv',
	]
)

fn vprintln(s string) {
	$if verbose ? {
		eprintln(s)
	}
}

fn test_all() {
	if os.user_os() != 'linux' && os.getenv('FORCE_VALGRIND_TEST').len == 0 {
		eprintln('Valgrind tests can only be run reliably on Linux for now.')
		eprintln('You can still do it by setting FORCE_VALGRIND_TEST=1 .')
		exit(0)
	}
	if os.getenv('V_CI_UBUNTU_MUSL').len > 0 {
		eprintln('This test is disabled for musl.')
		exit(0)
	}
	bench_message := 'memory leak checking with valgrind'
	mut bench := benchmark.new_benchmark()
	eprintln(term.header(bench_message, '-'))
	vexe := os.getenv('VEXE')
	vroot := os.dir(vexe)
	valgrind_test_path := 'vlib/v/tests/valgrind'
	dir := os.join_path(vroot, valgrind_test_path)
	files := os.ls(dir) or {
		panic(err)
	}
	//
	wrkdir := os.join_path(os.temp_dir(), 'vtests', 'valgrind')
	os.mkdir_all(wrkdir)
	os.chdir(wrkdir)
	//
	tests := vtest.filter_vtest_only(files.filter(it.ends_with('.vv')), {
		basepath: valgrind_test_path
	})
	bench.set_total_expected_steps(tests.len)
	for dir_test_path in tests {
		bench.step()
		test_basename := os.file_name(dir_test_path).replace('.vv', '')
		v_filename := '$wrkdir/${test_basename}.v'
		exe_filename := '$wrkdir/$test_basename'
		full_test_path := os.real_path(os.join_path(vroot, dir_test_path))
		//
		if dir_test_path in skip_valgrind_files {
			$if !noskip ? {
				bench.skip()
				eprintln(bench.step_message_skip(dir_test_path))
				continue
			}
		}
		vprintln('$dir_test_path => $v_filename')
		//
		vprintln('cp $full_test_path $v_filename')
		os.cp(full_test_path, v_filename)
		compile_cmd := '$vexe -cg -cflags "-w" -autofree $v_filename'
		vprintln('compile cmd: ${util.bold(compile_cmd)}')
		res := os.exec(compile_cmd) or {
			bench.fail()
			eprintln(bench.step_message_fail('valgrind $dir_test_path failed'))
			continue
		}
		if res.exit_code != 0 {
			bench.fail()
			eprintln(bench.step_message_fail('file: $dir_test_path could not be compiled.'))
			eprintln(res.output)
			continue
		}
		valgrind_cmd := 'valgrind --error-exitcode=1 --leak-check=full $exe_filename'
		vprintln('valgrind cmd: ${util.bold(valgrind_cmd)}')
		valgrind_res := os.exec(valgrind_cmd) or {
			bench.fail()
			eprintln(bench.step_message_fail('valgrind could not be executed'))
			continue
		}
		if valgrind_res.exit_code != 0 {
			bench.fail()
			eprintln(bench.step_message_fail('failed valgrind check for ${util.bold(dir_test_path)}'))
			eprintln(valgrind_res.output)
			continue
		}
		bench.ok()
		eprintln(bench.step_message_ok(dir_test_path))
	}
	bench.stop()
	eprintln(term.h_divider('-'))
	eprintln(bench.total_message(bench_message))
	if bench.nfail > 0 {
		exit(1)
	}
}
