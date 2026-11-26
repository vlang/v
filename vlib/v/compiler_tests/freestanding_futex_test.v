import os
import time

const freestanding_futex_source = '
#include <errno.h>

$if vlibc ? {
struct FutexProbe {
mut:
    ready u32
    word u32
    done u32
}

fn futex_worker(address u64) {
    probe := unsafe { &FutexProbe(voidptr(address)) }
    vlibc_store_u32(&probe.ready, 1)
    _, ready_error := bf_futex_wake(&probe.ready, 1)
    assert ready_error == 0
    for vlibc_load_u32(&probe.word) == 0 {
        code := bf_futex_wait(&probe.word, 0, 5_000_000_000)
        assert code in [0, 4, 11]
    }
    vlibc_store_u32(&probe.done, 1)
    _, done_error := bf_futex_wake(&probe.done, 1)
    assert done_error == 0
}
}

fn main() {
    mut word := u32(0)
    C.errno = 123
    before := bf_last_errno()
    assert bf_futex_wait(&word, 1, -1) == 11
    assert bf_futex_wait(&word, 0, 0) == 110
    woken, wake_error := bf_futex_wake(&word, 1)
    assert woken == 0 && wake_error == 0
    _, invalid := bf_futex_wake(&word, 0)
    assert invalid == 22
    _, too_many := bf_futex_wake(&word, int(2147483648))
    assert too_many == 22
    assert C.errno == 123 && bf_last_errno() == before
    $if vlibc ? {
        mut probe := FutexProbe{}
        mut worker, code := vlibc_thread_spawn(futex_worker, u64(voidptr(&probe)))
        assert code == .enoerror
        for vlibc_load_u32(&probe.ready) == 0 {
            ready_wait := bf_futex_wait(&probe.ready, 0, 5_000_000_000)
            assert ready_wait in [0, 4, 11]
        }
        vlibc_store_u32(&probe.word, 1)
        _, released := bf_futex_wake(&probe.word, 1)
        assert released == 0
        for vlibc_load_u32(&probe.done) == 0 {
            done_wait := bf_futex_wait(&probe.done, 0, 5_000_000_000)
            assert done_wait in [0, 4, 11]
        }
        joined := vlibc_thread_join(mut worker)
        assert joined == .enoerror
    }
    println("futex ok")
}
'

fn test_freestanding_linux_futex_wait_wake_and_errors() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'freestanding_futex_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.c.v')
	os.write_file(source, freestanding_futex_source)!
	for with_vlibc in [false, true] {
		output := os.join_path(root, if with_vlibc { 'vlibc' } else { 'plain' })
		mut command := [@VEXE, '-no-retry-compilation', '-b', 'c', '-cc', 'clang', '-freestanding',
			'-gc', 'none', '-cstrict', '-o', output]
		if with_vlibc { command << ['-d', 'vlibc'] }
		command << source
		built := os.exec(command)
		assert built.exit_code == 0, built.output
		ran := run_freestanding_futex_binary(output)
		assert ran.exit_code == 0, ran.output
		assert ran.output.trim_space() == 'futex ok', ran.output
	}
}

fn run_freestanding_futex_binary(binary string) os.Result {
	mut process := os.new_process(binary)
	process.set_redirect_stdio()
	process.run()
	defer { process.close() }
	elapsed := time.new_stopwatch()
	mut timed_out := false
	for process.is_alive() {
		if elapsed.elapsed() > 30 * time.second {
			timed_out = true
			process.signal_kill()
			break
		}
		time.sleep(time.millisecond)
	}
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	return os.Result{
		exit_code: if timed_out { -1 } else { process.code }
		output:    if timed_out { 'timed out after 30 seconds\n' + output } else { output }
	}
}
