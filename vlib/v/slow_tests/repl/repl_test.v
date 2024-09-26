module main

import os
import v.slow_tests.repl.runner
import benchmark
import sync.pool

const turn_off_vcolors = os.setenv('VCOLORS', 'never', true)

fn test_the_v_compiler_can_be_invoked() {
	vexec := runner.full_path_to_v(5)
	println('vexecutable: ${vexec}')
	assert vexec != ''
	vcmd := '${os.quoted_path(vexec)} -version'
	r := os.execute_or_exit(vcmd)
	assert r.exit_code == 0
	// println('"$vcmd" exit_code: $r.exit_code | output: $r.output')
	vcmd_error := '${os.quoted_path(vexec)} nonexisting.v'
	r_error := os.execute(vcmd_error)
	if r_error.exit_code < 0 {
		panic(r_error.output)
	}
	// println('"$vcmd_error" exit_code: $r_error.exit_code | output: $r_error.output')
	assert r_error.exit_code == 1
	actual_error := r_error.output.trim_space()
	assert actual_error == "builder error: nonexisting.v doesn't exist"
}

struct Session {
mut:
	options runner.RunnerOptions
	bmark   benchmark.Benchmark
}

fn test_all_v_repl_files() {
	if os.user_os() == 'windows' {
		if os.getenv('VTEST_ENABLE_REPL') == '' {
			println('This test is disabled on windows temporarily')
			println('set VTEST_ENABLE_REPL=1')
			println('if you do want to run it anyway.')
			exit(0)
		}
	}
	mut session := &Session{
		options: runner.new_options()
		bmark:   benchmark.new_benchmark()
	}
	// warmup, and ensure that the vrepl is compiled in single threaded mode if it does not exist
	runner.run_repl_file(os.cache_dir(), session.options.vexec, 'vlib/v/slow_tests/repl/nothing.repl') or {
		panic(err)
	}
	session.bmark.set_total_expected_steps(session.options.files.len)
	mut pool_repl := pool.new_pool_processor(callback: worker_repl)
	pool_repl.set_shared_context(session)
	$if windows {
		// See: https://docs.microsoft.com/en-us/cpp/build/reference/fs-force-synchronous-pdb-writes?view=vs-2019
		pool_repl.set_max_jobs(1)
	}
	pool_repl.work_on_items[string](session.options.files)
	session.bmark.stop()
	println(session.bmark.total_message('total time spent running REPL files'))
}

fn worker_repl(mut p pool.PoolProcessor, idx int, thread_id int) voidptr {
	cdir := os.cache_dir()
	mut session := unsafe { &Session(p.get_shared_context()) }
	mut tls_bench := unsafe { &benchmark.Benchmark(p.get_thread_context(idx)) }
	if isnil(tls_bench) {
		tls_bench = benchmark.new_benchmark_pointer()
		tls_bench.set_total_expected_steps(session.bmark.nexpected_steps)
		p.set_thread_context(idx, tls_bench)
	}
	tls_bench.cstep = idx
	mut tfolder := os.join_path(cdir, 'vrepl_tests_${idx}')
	if os.is_dir(tfolder) {
		os.rmdir_all(tfolder) or { panic(err) }
	}
	os.mkdir(tfolder) or { panic(err) }
	tfolder = os.real_path(tfolder)
	file := p.get_item[string](idx)
	session.bmark.step()
	tls_bench.step()
	fres := runner.run_repl_file(tfolder, session.options.vexec, file) or {
		session.bmark.fail()
		tls_bench.fail()
		os.rmdir_all(tfolder) or { panic(err) }
		eprintln(tls_bench.step_message_fail(err.msg()))
		assert false
		return pool.no_result
	}
	session.bmark.ok()
	tls_bench.ok()
	os.rmdir_all(tfolder) or { panic(err) }
	println(tls_bench.step_message_ok(fres))
	assert true
	return pool.no_result
}
