// vtest flaky: true
// vtest retry: 3
import os
import time

const (
	vexe                   = os.getenv('VEXE')
	vroot                  = os.dir(vexe)
	tfolder                = os.join_path(os.vtmp_dir(), 'v', 'tests', 'os_process')
	test_os_process        = os.join_path(tfolder, 'test_os_process.exe')
	test_os_process_source = os.join_path(vroot, 'cmd/tools/test_os_process.v')
)

fn testsuite_begin() {
	os.rmdir_all(tfolder) or {}
	os.mkdir_all(tfolder)!
	if os.getenv('WINE_TEST_OS_PROCESS_EXE') != '' {
		// Make it easier to run the test under wine emulation, by just
		// prebuilding the executable with:
		//   v -os windows -o x.exe cmd/tools/test_os_process.v
		//   WINE_TEST_OS_PROCESS_EXE=x.exe ./v -os windows vlib/os/process_test.v
		os.cp(os.getenv('WINE_TEST_OS_PROCESS_EXE'), test_os_process)!
	} else {
		os.system('${os.quoted_path(vexe)} -o ${os.quoted_path(test_os_process)} ${os.quoted_path(test_os_process_source)}')
	}
	assert os.exists(test_os_process)
}

fn testsuite_end() {
	os.rmdir_all(tfolder) or {}
}

fn test_getpid() {
	pid := os.getpid()
	eprintln('current pid: ${pid}')
	assert pid != 0
}

fn test_run() {
	mut p := os.new_process(test_os_process)
	p.set_args(['-timeout_ms', '150', '-period_ms', '50'])
	p.run()
	assert p.status == .running
	assert p.pid > 0
	assert p.pid != os.getpid()
	mut i := 0
	for {
		if !p.is_alive() {
			break
		}
		$if trace_process_output ? {
			os.system('ps -opid= -oppid= -ouser= -onice= -of= -ovsz= -orss= -otime= -oargs= -p ${p.pid}')
		}
		time.sleep(50 * time.millisecond)
		i++
	}
	p.wait()
	assert p.code == 0
	assert p.status == .exited
	//
	eprintln('polling iterations: ${i}')
	assert i < 50
	p.close()
}

fn test_wait() {
	mut p := os.new_process(test_os_process)
	assert p.status != .exited
	p.wait()
	assert p.status == .exited
	assert p.code == 0
	assert p.pid != os.getpid()
	p.close()
}

fn test_slurping_output() {
	mut p := os.new_process(test_os_process)
	p.set_args(['-timeout_ms', '500', '-period_ms', '50'])
	p.set_redirect_stdio()
	assert p.status != .exited
	p.wait()
	assert p.status == .exited
	assert p.code == 0
	output := p.stdout_slurp().trim_space()
	errors := p.stderr_slurp().trim_space()
	p.close()
	$if trace_process_output ? {
		eprintln('---------------------------')
		eprintln('p output: "${output}"')
		eprintln('p errors: "${errors}"')
		eprintln('---------------------------')
	}
	// dump(output)
	assert output.contains('stdout, 1')
	assert output.contains('stdout, 2')
	assert output.contains('stdout, 3')
	assert output.contains('stdout, 4')
	//
	// dump(errors)
	assert errors.contains('stderr, 1')
	assert errors.contains('stderr, 2')
	assert errors.contains('stderr, 3')
	assert errors.contains('stderr, 4')
}
