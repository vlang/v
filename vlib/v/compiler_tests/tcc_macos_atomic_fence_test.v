import os
import v.cmdexec

fn test_macos_tcc_links_atomic_thread_fences_without_fallback() ! {
	$if !macos {
		return
	}
	tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_file(tcc) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v_macos_tcc_atomic_fence_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.c.v')
	os.write_file(source, '
import sync
import sync.stdatomic as _

fn main() {
	spinlock := sync.new_spin_lock()
	spinlock.lock()
	spinlock.unlock()
	C.atomic_thread_fence(C.memory_order_seq_cst)
	println("fenced")
}
')!
	exe := os.join_path(root, 'main')
	for compiler_args in [([]string{}), ['-cc', 'tcc']] {
		for gc_mode in ['none', 'boehm'] {
			mut args := ['-new-compiler', '-nocache', '-no-retry-compilation', '-gc', gc_mode,
				...compiler_args, '-o', exe, source]
			$if arm64 {
				// macOS TCC's preprocessor exposes __arm64__ without __aarch64__.
				args.insert(0, ['-cflags', '-U__aarch64__'])
			}
			build := cmdexec.run(@VEXE, args)
			assert build.exit_code == 0, build.output
			assert !build.output.contains('falling back'), build.output
			assert !build.output.contains('implicit tcc could not be used'), build.output
			run := cmdexec.run(exe, [])
			assert run.exit_code == 0, run.output
			assert run.output.trim_space() == 'fenced', run.output
		}
	}
}
