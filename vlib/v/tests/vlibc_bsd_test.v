// Vlibc freestanding regressions for BSDs and macOS on matching hosts.
import os

const vexe = @VEXE

const vlibc_bsd_program = "struct State {
mut:
	mutex VlibcMutex
	cond VlibcCond
	count u32
	done [2]u32
}

struct Work {
	index int
	delay u64
mut:
	state &State
}

fn worker(arg u64) {
	mut work := unsafe { &Work(arg) }
	if work.delay > 0 { vlibc_sleep_ns(work.delay) }
	for _ in 0 .. 10000 {
		vlibc_mutex_lock(mut work.state.mutex)
		work.state.count++
		vlibc_mutex_unlock(mut work.state.mutex)
		vlibc_cond_signal(mut work.state.cond)
	}
	vlibc_store_u32(&work.state.done[work.index], u32(work.index + 1))
}

fn check(e VlibcErrno) {
	if e != .enoerror { exit(80) }
}

fn main() {
	mut state := State{mutex: vlibc_mutex_init(), cond: vlibc_cond_init()}
	slow := Work{state: &state, index: 0, delay: 50000000}
	fast := Work{state: &state, index: 1, delay: 0}
	mut one, e1 := vlibc_thread_spawn(worker, u64(&slow))
	check(e1)
	mut two, e2 := vlibc_thread_spawn(worker, u64(&fast))
	check(e2)
	check(vlibc_thread_join(mut one))
	check(vlibc_thread_join(mut two))
	assert state.count == 20000
	assert state.done[0] == 1 && state.done[1] == 2
	println('threads and atomics ok')

	pipe, pe := vlibc_pipe()
	check(pe)
	assert pipe[0] != pipe[1] && pipe[0] > 2 && pipe[1] > 2
	n, we := vlibc_sys_write(i64(pipe[1]), c'hello', 5)
	check(we)
	assert n == 5
	mut bytes := [5]u8{}
	r, re := vlibc_sys_read(i64(pipe[0]), &bytes[0], 5)
	check(re)
	assert r == 5 && bytes[0] == 104 && bytes[4] == 111
	duplicated, de := vlibc_dup2(pipe[0], 100)
	check(de)
	assert duplicated == 100
	same, se := vlibc_dup2(100, 100)
	check(se)
	assert same == 100
	check(vlibc_close(100))
	_, invalid := vlibc_dup2(100, 100)
	assert invalid == .ebadf
	check(vlibc_close(i64(pipe[0])))
	check(vlibc_close(i64(pipe[1])))
	partial_pipe, ppe := vlibc_pipe()
	check(ppe)
	$if macos {
		_, fce := vlibc_sys_fcntl(i64(partial_pipe[0]), 4, voidptr(4))
		check(fce)
	} $else {
		_, fce := vlibc_sys_fcntl(i64(partial_pipe[0]), 4, 4)
		check(fce)
	}
	_, pwe := vlibc_sys_write(i64(partial_pipe[1]), c'partial', 7)
	check(pwe)
	partial, pre := vlibc_read_all(i64(partial_pipe[0]))
	assert pre == .eagain && partial.bytestr() == 'partial'
	check(vlibc_close(i64(partial_pipe[0])))
	check(vlibc_close(i64(partial_pipe[1])))
	pair, pse := vlibc_socketpair(1, 1, 0)
	check(pse)
	assert pair[0] != pair[1] && pair[0] > 2 && pair[1] > 2
	check(vlibc_close(i64(pair[0])))
	check(vlibc_close(i64(pair[1])))
	address := vlibc_parse_ipv4('127.0.0.1')
	assert unsafe { (&u8(&address))[0] } == 127
	println('descriptors and network order ok')

	child := vlibc_fork()
	assert child >= 0
	if child == 0 {
		vlibc_sleep_ns(50000000)
		exit(7)
	}
	mut status := i32(0)
	waited, ee := vlibc_sys_wait4(child, &status, 0, unsafe { nil })
	check(ee)
	assert waited == child && status == (7 << 8)
	other := vlibc_fork()
	assert other >= 0
	if other == 0 {
		vlibc_sleep_ns(50000000)
		exit(9)
	}
	wp, wpe := vlibc_waitpid(other)
	check(wpe)
	assert wp == other
	_, no_child := vlibc_sys_wait4(other, &status, 1, unsafe { nil })
	assert no_child == .echild
	println('waitpid and status ok')

	path := arguments()[1]
	check(vlibc_write_file(path, 'hello'.bytes()))
	data, fe := vlibc_read_file(path)
	check(fe)
	assert data.bytestr() == 'hello'
	names, le := vlibc_list_dir(arguments()[2])
	check(le)
	assert names.contains('fixture.txt')
	println('file=hello')
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
	root := os.join_path(os.vtmp_dir(), 'vlibc_bsd_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, vlibc_bsd_program)!
	for optimized in [false, true] {
		exe := os.join_path(root, 'main_${optimized}')
		mut args := [vexe, '-nocache', '-freestanding', '-d', 'vlibc']
		if optimized { args << '-prod' }
		args << ['-o', exe, source]
		build := os.exec(args)
		assert build.exit_code == 0, 'prod=${optimized}: ${build.output}'
		run := os.exec([exe, os.join_path(root, 'fixture.txt'), root])
		assert run.exit_code == 0, 'prod=${optimized}: ${run.output}'
		assert run.output == 'threads and atomics ok\ndescriptors and network order ok\nwaitpid and status ok\nfile=hello\n', run.output
	}
}
