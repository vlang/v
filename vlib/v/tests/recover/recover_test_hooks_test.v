import os

const vexe = os.getenv('VEXE')

// A failed assert in a helper of `after_each` jumps out of the hook, past its
// deferred blocks. The panic frames that they linked must not outlive the hook,
// or the panic of the next test unwinds into the abandoned stack of the hook.
const hook_test_source = "fn check() {
	assert false, 'after_each fails'
}

fn after_each() {
	defer {
		println('after_each cleanup')
	}
	check()
}

fn test_first() {
	defer {
		recover()
	}
	println('first')
}

fn cleanup_then_panic() {
	defer {
		println('cleanup')
	}
	panic('boom')
}

fn test_second() {
	cleanup_then_panic()
}
"

fn test_failed_assert_in_after_each_leaves_no_panic_frames() {
	dir := os.join_path(os.vtmp_dir(), 'recover_after_each_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	src := os.join_path(dir, 'hook_test.v')
	os.write_file(src, hook_test_source)!
	exe := os.join_path(dir, 'hook_test')
	compile := os.exec([vexe, '-b', 'c', '-o', exe, '${src}'])
	assert compile.exit_code == 0, compile.output
	run := os.exec([exe])
	assert run.exit_code == 1, run.output
	assert run.output.contains('after_each fails'), run.output
	assert run.output.contains('cleanup'), run.output
	assert run.output.contains('V panic: boom'), run.output
	assert !run.output.contains('after_each cleanup'), run.output
}
