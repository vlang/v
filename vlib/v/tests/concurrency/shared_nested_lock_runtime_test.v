import os
import time

const vexe = @VEXE

fn test_nested_shared_lock_fails_without_hanging() {
	$if windows {
		return
	}
	base_dir := os.join_path(os.vtmp_dir(), 'shared_nested_lock_runtime_${os.getpid()}')
	source_path := os.join_path(base_dir, 'shared_nested_lock_runtime.v')
	os.rmdir_all(base_dir) or {}
	os.mkdir_all(base_dir)!
	defer {
		os.rmdir_all(base_dir) or {}
	}
	source := [
		'module main',
		'',
		'fn main() {',
		'\tshared connections := []int{}',
		'\tlock_function(shared connections)',
		'\tlock connections {',
		"\t\tprintln('unreachable')",
		'\t}',
		'}',
		'',
		'fn lock_function(shared connections []int) {',
		'\tnested_lock_function := fn [shared connections] () {',
		'\t\tlock connections {',
		"\t\t\tprintln('unreachable nested')",
		'\t\t}',
		'\t}',
		'\tlock connections {',
		'\t\tnested_lock_function()',
		'\t}',
		'}',
	].join_lines()
	os.write_file(source_path, source)!
	mut p := os.new_process(vexe)
	p.set_args(['run', source_path])
	p.set_redirect_stdio()
	p.run()
	mut timeout_ticks := 0
	for p.is_alive() && timeout_ticks < 100 {
		time.sleep(50 * time.millisecond)
		timeout_ticks++
	}
	if p.is_alive() {
		p.signal_kill()
		p.wait()
		stdout := p.stdout_slurp()
		stderr := p.stderr_slurp()
		p.close()
		assert false, 'nested shared lock hung\nstdout:\n${stdout}\nstderr:\n${stderr}'
	}
	p.wait()
	stdout := p.stdout_slurp()
	stderr := p.stderr_slurp()
	p.close()
	output := stdout + stderr
	assert p.code != 0, 'expected nested shared lock failure, got output:\n${output}'
	assert output.contains('Resource deadlock avoided'), output
}
