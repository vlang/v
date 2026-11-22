module main

import os
import v.cmdexec

fn test_arm64_reports_unresolved_c_constants() {
	$if macos && arm64 {
		root := os.join_path(os.vtmp_dir(), 'arm64_c_constants_${os.getpid()}')
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'main.c.v')
		output := os.join_path(root, 'main')
		for name in ['THIS_MACRO_DOES_NOT_EXIST', 'HEADER_DEFINED_MACRO'] {
			for code in [
				'fn main() { println(int(C.${name})) }',
				'const number = int(C.${name})\nfn main() { println(number) }',
			] {
				os.write_file(source, code)!
				result := cmdexec.run(@VEXE, ['-b', 'arm64', '-o', output, source])
				assert result.exit_code == 1, result.output
				assert result.output.count('cannot resolve C constant `C.${name}`') == 1, result.output
				assert !os.exists(output)
			}
		}
		os.write_file(source, 'fn unused() { println(int(C.UNUSED_MACRO)) }
fn C.getpid() int
fn main() {
    println([int(C.F_SETFD), int(C.FD_CLOEXEC), int(C.SIGKILL), int(C.SIGTERM)])
    assert C.getpid() > 0
}
')!
		compiled := cmdexec.run(@VEXE, ['-b', 'arm64', '-o', output, source])
		assert compiled.exit_code == 0, compiled.output
		run := cmdexec.run(output, []string{})
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == '[2, 1, 9, 15]', run.output
	}
}

fn test_arm64_sync_constants_compile_and_run() {
	$if macos && arm64 {
		root := os.join_path(os.vtmp_dir(), 'arm64_sync_constants_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		source := os.join_path(root, 'main.c.v')
		output := os.join_path(root, 'main')
		os.write_file(source, 'import sync
pub const C.memory_order_relaxed i32
pub const C.memory_order_acq_rel i32
pub const C.memory_order_seq_cst i32
fn main() {
	assert int(C.memory_order_relaxed) == 0
	assert int(C.memory_order_acquire) == 2
	assert int(C.memory_order_release) == 3
	assert int(C.memory_order_acq_rel) == 4
	assert int(C.memory_order_seq_cst) == 5
	assert int(C.PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP) == 0
	mut spin := sync.new_spin_lock()
	assert spin.try_lock()
	spin.unlock()
	mut mutex := sync.new_rwmutex()
	mutex.lock()
	mutex.unlock()
	println("native sync constants")
}
')!
		compiled := cmdexec.run(@VEXE, ['-b', 'arm64', '-gc', 'none', '-nocache', '-o', output,
			source])
		assert compiled.exit_code == 0, compiled.output
		run := cmdexec.run_with_timeout(output, []string{}, 5000)
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == 'native sync constants', run.output
	}
}

fn test_arm64_c_pointer_sentinels_and_arithmetic_constants() {
	$if macos && arm64 {
		root := os.join_path(os.vtmp_dir(), 'arm64_pointer_constants_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		source := os.join_path(root, 'main.c.v')
		output := os.join_path(root, 'main')
		os.write_file(source, '#define POINTER_SENTINEL ((void*)-42)
#define FLOAT_CONSTANT 3.125
#define UNSIGNED_CONSTANT 18446744073709551615ULL
fn main() {
	assert voidptr(C.SIG_ERR) == voidptr(-1)
	assert voidptr(C.SIG_DFL) == voidptr(0)
	assert voidptr(C.SIG_IGN) == voidptr(1)
	assert voidptr(C.POINTER_SENTINEL) == voidptr(-42)
	assert f64(C.FLOAT_CONSTANT) == 3.125
	assert u64(C.UNSIGNED_CONSTANT) == u64(18446744073709551615)
	println("pointer and arithmetic constants")
}
')!
		compiled := cmdexec.run(@VEXE, ['-b', 'arm64', '-o', output, source])
		assert compiled.exit_code == 0, compiled.output
		run := cmdexec.run(output, []string{})
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == 'pointer and arithmetic constants', run.output
		for address in ['(&puts)', '"text"'] {
			os.write_file(source, '#define RELOCATED_POINTER ${address}
fn main() { println(u64(C.RELOCATED_POINTER)) }
')!
			unresolved := cmdexec.run(@VEXE, ['-b', 'arm64', '-o', output, source])
			assert unresolved.exit_code == 1, unresolved.output
			assert unresolved.output.contains('cannot resolve C constant `C.RELOCATED_POINTER`'), unresolved.output
		}
	}
}
