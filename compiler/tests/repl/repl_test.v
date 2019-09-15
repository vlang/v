import os
import compiler.tests.repl.runner

fn test_the_v_compiler_can_be_invoked() {
	vexec := runner.full_path_to_v()
	println('vexecutable: $vexec')
	assert vexec != ''

	vcmd := '$vexec --version'
	r := os.exec(vcmd) or { panic(err) }
	//println('"$vcmd" exit_code: $r.exit_code | output: $r.output')
	assert r.exit_code == 0

	vcmd_error := '$vexec nonexisting.v'
	r_error := os.exec(vcmd_error) or { panic(err) }
	//println('"$vcmd_error" exit_code: $r_error.exit_code | output: $r_error.output')
	assert r_error.exit_code == 1
	assert r_error.output == '`nonexisting.v` does not exist'
}

fn test_all_v_repl_files() {
	options := runner.new_options()
	global_start_time := runner.now()
	mut total_tests := 0
	mut ok_tests := 0
	mut failed_tests := 0
	for file in options.files {
		total_tests++
		sticks := runner.now()
		fres := runner.run_repl_file(options.wd, options.vexec, file) or {
			failed_tests++
			assert false
			eprintln( runner.tdiff_in_ms(err, sticks) )
			continue
		}
		assert true
		println( runner.tdiff_in_ms(fres, sticks) )
		ok_tests++
	}
	println( runner.tdiff_in_ms('<=== total time spent running REPL files', global_start_time) )
	println( '            ok, failed, total : ${ok_tests:5d}, ${failed_tests:5d}, ${total_tests:5d}' )
}

