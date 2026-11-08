import os
import time

const vexe = @VEXE
const source_run_poll_interval = 50 * time.millisecond
const source_run_timeout_ticks = 600

struct SourceRunResult {
	code   int
	output string
}

fn run_source_with_timeout(source_name string, source string) !SourceRunResult {
	base_dir := os.join_path(os.vtmp_dir(), '${source_name}_${os.getpid()}')
	source_path := os.join_path(base_dir, '${source_name}.v')
	executable_path := os.join_path(base_dir, source_name + $if windows { '.exe' } $else { '' })
	os.rmdir_all(base_dir) or {}
	os.mkdir_all(base_dir)!
	defer {
		os.rmdir_all(base_dir) or {}
	}
	os.write_file(source_path, source)!
	// The deadline checks runtime locking, not compilation by an instrumented compiler.
	compiled := os.exec([vexe, '-o', executable_path, source_path])
	if compiled.exit_code != 0 {
		return error('compilation failed\n${compiled.output}')
	}
	mut p := os.new_process(executable_path)
	p.set_redirect_stdio()
	// Kill the whole process group on timeout before draining its pipes, so a
	// descendant cannot keep stdout_slurp() waiting for EOF.
	p.use_pgroup = true
	p.run()
	mut timeout_ticks := 0
	for p.is_alive() && timeout_ticks < source_run_timeout_ticks {
		time.sleep(source_run_poll_interval)
		timeout_ticks++
	}
	if p.is_alive() {
		p.signal_pgkill()
		p.wait()
		stdout := p.stdout_slurp()
		stderr := p.stderr_slurp()
		p.close()
		return error('timed out\nstdout:\n${stdout}\nstderr:\n${stderr}')
	}
	p.wait()
	stdout := p.stdout_slurp()
	stderr := p.stderr_slurp()
	code := p.code
	p.close()
	return SourceRunResult{
		code:   code
		output: stdout + stderr
	}
}

fn test_nested_shared_lock_fails_without_hanging() {
	$if windows {
		return
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
	result := run_source_with_timeout('shared_nested_lock_runtime', source) or {
		assert false, 'nested shared lock probe failed\n${err}'
		return
	}
	assert result.code != 0, 'expected nested shared lock failure, got output:\n${result.output}'
	assert result.output.contains('Resource deadlock avoided'), result.output
}

fn test_shared_generic_heap_method_call_does_not_hang() {
	source := [
		'module main',
		'',
		'@[heap]',
		'struct Topic[T] {',
		'mut:',
		'\tlist List[T]',
		'}',
		'',
		'@[heap]',
		'struct List[T] {',
		'mut:',
		'\tnode ?&Node[T]',
		'}',
		'',
		'@[heap]',
		'struct Node[T] {',
		'mut:',
		'\tdata T',
		'\tnext ?&Node[T]',
		'}',
		'',
		'fn Topic.new[T]() Topic[T] {',
		'\treturn Topic[T]{}',
		'}',
		'',
		'fn (mut l List[T]) push(val T) {',
		'\tmut node := Node[T]{',
		'\t\tdata: val',
		'\t}',
		'\tl.node = &node',
		'}',
		'',
		'fn (shared t Topic[T]) add(mut data T) {',
		'\tlock t {',
		'\t\tt.list.push(data)',
		'\t}',
		'}',
		'',
		'fn main() {',
		'\tshared topic := Topic.new[string]()',
		"\tmut data := 'something'",
		'\ttopic.add(mut data)',
		"\tprintln('done')",
		'}',
	].join_lines()
	result := run_source_with_timeout('shared_generic_heap_method_call', source) or {
		assert false, 'shared generic heap method call probe failed\n${err}'
		return
	}
	assert result.code == 0, result.output
	assert result.output.contains('done'), result.output
}
