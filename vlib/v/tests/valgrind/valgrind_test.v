import os
import term
import benchmark
import v.util
import v.util.vtest

const turn_off_vcolors = os.setenv('VCOLORS', 'never', true)

//
// NB: skip_compile_files can be used for totally skipping .v files temporarily.
// .v files in skip_valgrind_files will be compiled, but will not be run under
// valgrind. This ensures that at least the generated code does not have C syntax
// errors.
// Use: `./v -d noskipcompile vlib/v/tests/valgrind/valgrind_test.v` to ignore the
// skip_compile_files list.
// Use: `./v -d noskip vlib/v/tests/valgrind/valgrind_test.v` to ignore skip_valgrind_files
// Use: `./v -d noskipcompile -d noskip vlib/v/tests/valgrind/valgrind_test.v` to ignore both
//
const skip_compile_files = [
		'vlib/v/tests/valgrind/option_reassigned.v',
	]

const skip_valgrind_files = [
		'vlib/v/tests/valgrind/struct_field.v',
		'vlib/v/tests/valgrind/fn_returning_string_param.v',
		'vlib/v/tests/valgrind/fn_with_return_should_free_local_vars.v',
		'vlib/v/tests/valgrind/option_simple.v',
	]

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
	files := os.ls(dir) or { panic(err) }
	//
	wrkdir := os.join_path(os.temp_dir(), 'vtests', 'valgrind')
	os.mkdir_all(wrkdir) or { panic(err) }
	os.chdir(wrkdir)
	//
	only_ordinary_v_files := files.filter(it.ends_with('.v') && !it.ends_with('_test.v'))
	tests := vtest.filter_vtest_only(only_ordinary_v_files, basepath: valgrind_test_path)
	bench.set_total_expected_steps(tests.len)
	for test in tests {
		bench.step()
		if test in skip_compile_files {
			$if !noskipcompile ? {
				bench.skip()
				eprintln(bench.step_message_skip(test))
				continue
			}
		}
		//
		exe_filename := '$wrkdir/x'
		full_path_to_source_file := os.join_path(vroot, test)
		compile_cmd := '$vexe -o $exe_filename -cg -cflags "-w" -autofree "$full_path_to_source_file"'
		vprintln('compile cmd: ${util.bold(compile_cmd)}')
		res := os.execute(compile_cmd)
		if res.exit_code != 0 {
			bench.fail()
			eprintln(bench.step_message_fail('file: $test could not be compiled.'))
			eprintln(res.output)
			continue
		}
		if test in skip_valgrind_files {
			$if !noskip ? {
				bench.skip()
				eprintln(bench.step_message_skip(test))
				continue
			}
		}
		valgrind_cmd := 'valgrind --error-exitcode=1 --leak-check=full $exe_filename'
		vprintln('valgrind cmd: ${util.bold(valgrind_cmd)}')
		valgrind_res := os.execute(valgrind_cmd)
		if valgrind_res.exit_code != 0 {
			bench.fail()
			eprintln(bench.step_message_fail('failed valgrind check for ${util.bold(test)}'))
			eprintln(valgrind_res.output)
			continue
		}
		bench.ok()
		eprintln(bench.step_message_ok(test))
	}
	bench.stop()
	eprintln(term.h_divider('-'))
	eprintln(bench.total_message(bench_message))
	if bench.nfail > 0 {
		exit(1)
	}
}
