module main

import os
import compiler.tests.repl.runner
import benchmark
import runtime
import sync
import filepath

fn test_the_v_compiler_can_be_invoked() {
	vexec := runner.full_path_to_v(5)
	println('vexecutable: $vexec')
	assert vexec != ''
	vcmd := '"$vexec" --version'
	r := os.exec(vcmd) or {
		panic(err)
	}
	// println('"$vcmd" exit_code: $r.exit_code | output: $r.output')
	assert r.exit_code == 0
	vcmd_error := '"$vexec" nonexisting.v'
	r_error := os.exec(vcmd_error) or {
		panic(err)
	}
	// println('"$vcmd_error" exit_code: $r_error.exit_code | output: $r_error.output')
	assert r_error.exit_code == 1
	assert r_error.output == '`nonexisting.v` does not exist'
}

struct Session {
mut:
	options   runner.RunnerOptions
	bmark     benchmark.Benchmark
	ntask     int
	ntask_mtx &sync.Mutex
	waitgroup &sync.WaitGroup
}

fn test_all_v_repl_files() {
	mut session := &Session{
		options: runner.new_options()
		bmark: benchmark.new_benchmark()
		ntask: 0
		ntask_mtx: sync.new_mutex()
		waitgroup: sync.new_waitgroup()
	}

	// warmup, and ensure that the vrepl is compiled in single threaded mode if it does not exist
	runner.run_repl_file(os.cachedir(), session.options.vexec, 'vlib/compiler/tests/repl/nothing.repl') or {
		panic(err)
	}

	session.bmark.set_total_expected_steps( session.options.files.len )
	mut ncpus := runtime.nr_cpus()
	$if windows {
	// See: https://docs.microsoft.com/en-us/cpp/build/reference/fs-force-synchronous-pdb-writes?view=vs-2019
		ncpus = 1
	}
	session.waitgroup.add( ncpus )
	for i:=0; i < ncpus; i++ {
		go process_in_thread(session)
	}
	session.waitgroup.wait()
	session.bmark.stop()
	println(session.bmark.total_message('total time spent running REPL files'))
}

fn process_in_thread( session mut Session ){
	cdir := os.cachedir()
	mut tls_bench := benchmark.new_benchmark()
	tls_bench.set_total_expected_steps( session.bmark.nexpected_steps )
	for {
		session.ntask_mtx.lock()
		session.ntask++
		idx := session.ntask-1
		session.ntask_mtx.unlock()
		
		if idx >= session.options.files.len { break }
		tls_bench.cstep = idx

		tfolder := filepath.join( cdir, 'vrepl_tests_$idx')
		if os.is_dir( tfolder ) {
			os.rmdir_recursive( tfolder )
		}
		os.mkdir( tfolder ) or { panic(err) }
		
		file := os.realpath( filepath.join( session.options.wd, session.options.files[ idx ] ) )
		session.bmark.step()
		tls_bench.step()
		fres := runner.run_repl_file(tfolder, session.options.vexec, file) or {
			session.bmark.fail()
			tls_bench.fail()
			os.rmdir_recursive( tfolder )
			eprintln(tls_bench.step_message_fail(err))
			assert false
			continue
		}
		session.bmark.ok()
		tls_bench.ok()
		os.rmdir_recursive( tfolder )
		println(tls_bench.step_message_ok(fres))
		assert true
	}
	session.waitgroup.done()
}
