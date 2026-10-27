// Vlibc freestanding smoke test for BSDs and macOS. Runs only on a matching host.
import os

const vexe = @VEXE

const vlibc_bsd_program = "fn worker(arg u64) {
	bare_print(c'worker ran\\n', 11)
}

fn main() {
	mut tt, e3 := vlibc_thread_spawn(worker, 42)
	println('spawn err=' + int(e3).str() + ' tid=' + tt.tid.str())
	vlibc_thread_join(mut tt)
	println('joined')
	vlibc_write_file('/tmp/vlibc_t.txt', 'hello'.bytes())
	data, _ := vlibc_read_file('/tmp/vlibc_t.txt')
	println('file=' + data.bytestr())
	exit(0)
}
"

fn test_vlibc_freestanding_bsd() {
	$if !(freebsd || openbsd || netbsd || dragonfly || macos) {
		return
	}
	$if !amd64 && !arm64 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'vlibc_bsd_test_${os.getpid()}')
	os.mkdir_all(root) or { return }
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, vlibc_bsd_program) or { return }
	exe := os.join_path(root, 'main')
	mut args := [vexe, '-nocache', '-freestanding', '-d', 'vlibc', '-o', exe, source]
	build := os.exec(args)
	assert build.exit_code == 0, build.output
	run := os.exec([exe])
	assert run.exit_code == 0, run.output
	assert run.output.contains('worker ran'), run.output
	assert run.output.contains('joined'), run.output
	assert run.output.contains('file=hello'), run.output
}
