import os
import compiler.tests.repl.runner
import benchmark

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
	mut bmark := benchmark.new_benchmark()
	for file in options.files {
		bmark.step()
		fres := runner.run_repl_file(options.wd, options.vexec, file) or {
			bmark.fail()
			eprintln( bmark.step_message(err) )
			assert false
			continue
		}
		bmark.ok()
		println( bmark.step_message(fres) )
		assert true
	}
	bmark.stop()
	println( bmark.total_message('total time spent running REPL files') )
}

